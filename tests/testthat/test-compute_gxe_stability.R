# Source the module file to get function definitions
source(file.path(
  testthat::test_path(), "..", "..", "R", "mod_preProdAdvApp.R"
), local = TRUE)

# Helper to create predictions with CS/Diagonal model data (per-environment)
make_cs_predictions <- function(designations = c("G1", "G2", "G3"),
                                environments = c("Env_A", "Env_B", "Env_C"),
                                mta_stamp = "MTA_001",
                                trait_name = "Yield",
                                values_matrix = NULL,
                                reliabilities = NULL) {
  n_g <- length(designations)
  n_e <- length(environments)

  if (is.null(values_matrix)) {
    # Generate default values: genotype mean + environment offset
    set.seed(42)
    values_matrix <- matrix(rnorm(n_g * n_e, mean = 5, sd = 1), nrow = n_g, ncol = n_e)
  }

  if (is.null(reliabilities)) {
    reliabilities <- rep(0.5, n_g * n_e)
  }

  # Build per-environment rows
  env_rows <- data.frame(
    analysisId = rep(mta_stamp, n_g * n_e),
    designation = rep(designations, times = n_e),
    trait = rep(trait_name, n_g * n_e),
    environment = rep(environments, each = n_g),
    effectType = rep(environments, each = n_g),
    predictedValue = as.vector(values_matrix),
    stdError = 0.1,
    reliability = reliabilities,
    stringsAsFactors = FALSE
  )

  # Also include designation-level rows (overall mean from MTA)
  overall_means <- rowMeans(values_matrix)
  designation_rows <- data.frame(
    analysisId = rep(mta_stamp, n_g),
    designation = designations,
    trait = rep(trait_name, n_g),
    environment = NA_character_,
    effectType = "designation",
    predictedValue = overall_means,
    stdError = 0.1,
    reliability = rep(0.6, n_g),
    stringsAsFactors = FALSE
  )

  rbind(designation_rows, env_rows)
}

# Helper to create review_df
make_review_df <- function(designations = c("G1", "G2", "G3"),
                           statuses = c("SELECTED", "NOT SELECTED", "CHECK")) {
  data.frame(
    designation = designations,
    plot_status = statuses,
    stringsAsFactors = FALSE
  )
}

test_that("compute_gxe_stability returns correct columns", {
  preds <- make_cs_predictions()
  review <- make_review_df()

  result <- compute_gxe_stability(preds, review, "MTA_001", "Yield")

  expected_cols <- c("designation", "mean_value", "cv", "env_variance",
                     "n_environments", "plot_status", "opacity", "color", "shape")
  expect_equal(sort(colnames(result)), sort(expected_cols))
})

test_that("compute_gxe_stability computes CV correctly as sd/mean", {
  # Use known values to verify computation
  values_matrix <- matrix(c(
    4, 6, 8,  # G1: mean=6, sd=2, CV=2/6
    10, 10, 10  # G2: mean=10, sd=0, CV=0/10
  ), nrow = 2, byrow = TRUE)

  preds <- make_cs_predictions(
    designations = c("G1", "G2"),
    environments = c("Env_A", "Env_B", "Env_C"),
    values_matrix = values_matrix
  )
  review <- make_review_df(c("G1", "G2"), c("SELECTED", "SELECTED"))

  result <- compute_gxe_stability(preds, review, "MTA_001", "Yield")
  result <- result[order(result$designation), ]

  # G1: sd(c(4,6,8)) / mean(c(4,6,8)) = 2 / 6 = 0.3333
  expect_equal(result$cv[result$designation == "G1"], sd(c(4, 6, 8)) / mean(c(4, 6, 8)))
  # G2: sd(c(10,10,10)) / mean(c(10,10,10)) = 0 / 10 = 0
  expect_equal(result$cv[result$designation == "G2"], 0)
})

test_that("compute_gxe_stability computes env_variance correctly", {
  values_matrix <- matrix(c(
    4, 6, 8,   # G1: var = 4
    10, 10, 10  # G2: var = 0
  ), nrow = 2, byrow = TRUE)

  preds <- make_cs_predictions(
    designations = c("G1", "G2"),
    environments = c("Env_A", "Env_B", "Env_C"),
    values_matrix = values_matrix
  )
  review <- make_review_df(c("G1", "G2"), c("SELECTED", "SELECTED"))

  result <- compute_gxe_stability(preds, review, "MTA_001", "Yield")
  result <- result[order(result$designation), ]

  expect_equal(result$env_variance[result$designation == "G1"], var(c(4, 6, 8)))
  expect_equal(result$env_variance[result$designation == "G2"], var(c(10, 10, 10)))
})

test_that("compute_gxe_stability excludes genotypes with fewer than 2 environments", {
  # G3 appears in only 1 environment
  env_rows <- data.frame(
    analysisId = c("MTA_001", "MTA_001", "MTA_001", "MTA_001", "MTA_001"),
    designation = c("G1", "G1", "G2", "G2", "G3"),
    trait = "Yield",
    environment = c("Env_A", "Env_B", "Env_A", "Env_B", "Env_A"),
    effectType = c("Env_A", "Env_B", "Env_A", "Env_B", "Env_A"),
    predictedValue = c(5, 7, 8, 10, 6),
    stdError = 0.1,
    reliability = 0.5,
    stringsAsFactors = FALSE
  )
  review <- make_review_df(c("G1", "G2", "G3"), c("SELECTED", "SELECTED", "SELECTED"))

  result <- compute_gxe_stability(env_rows, review, "MTA_001", "Yield")

  # G3 should be excluded (only 1 environment)
  expect_false("G3" %in% result$designation)
  expect_true("G1" %in% result$designation)
  expect_true("G2" %in% result$designation)
})

test_that("compute_gxe_stability excludes genotypes with mean == 0", {
  # G2 has mean of 0 across environments (values sum to zero)
  values_matrix <- matrix(c(
    4, 6, 8,     # G1: mean=6 (included)
    -2, 0, 2     # G2: mean=0 (excluded — division by zero)
  ), nrow = 2, byrow = TRUE)

  preds <- make_cs_predictions(
    designations = c("G1", "G2"),
    environments = c("Env_A", "Env_B", "Env_C"),
    values_matrix = values_matrix
  )
  review <- make_review_df(c("G1", "G2"), c("SELECTED", "SELECTED"))

  result <- compute_gxe_stability(preds, review, "MTA_001", "Yield")

  expect_false("G2" %in% result$designation)
  expect_true("G1" %in% result$designation)
})

test_that("compute_gxe_stability assigns NOT SELECTED to designations absent from review_df", {
  preds <- make_cs_predictions(designations = c("G1", "G2", "G3"))
  review <- make_review_df(c("G1"), c("SELECTED"))

  result <- compute_gxe_stability(preds, review, "MTA_001", "Yield")

  expect_equal(result$plot_status[result$designation == "G1"], "SELECTED")
  expect_equal(result$plot_status[result$designation == "G2"], "NOT SELECTED")
  expect_equal(result$plot_status[result$designation == "G3"], "NOT SELECTED")
})

test_that("compute_gxe_stability applies overrides before computing visual properties", {
  preds <- make_cs_predictions(designations = c("G1", "G2"))
  review <- make_review_df(c("G1", "G2"), c("SELECTED", "NOT SELECTED"))
  overrides <- data.frame(
    designation = "G2",
    plot_status = "CHECK",
    stringsAsFactors = FALSE
  )

  result <- compute_gxe_stability(preds, review, "MTA_001", "Yield", overrides)

  expect_equal(result$plot_status[result$designation == "G2"], "CHECK")
  expect_equal(result$color[result$designation == "G2"], "#C2185B")
  expect_equal(result$opacity[result$designation == "G2"], 1.0)
  expect_equal(result$shape[result$designation == "G2"], "diamond")
})

test_that("compute_gxe_stability maps colors correctly from STATUS_COLORS", {
  preds <- make_cs_predictions(designations = c("G1", "G2", "G3"))
  review <- make_review_df(c("G1", "G2", "G3"), c("SELECTED", "NOT SELECTED", "CHECK"))

  result <- compute_gxe_stability(preds, review, "MTA_001", "Yield")

  expect_equal(result$color[result$designation == "G1"], "#0072B2")
  expect_equal(result$color[result$designation == "G2"], "#D55E00")
  expect_equal(result$color[result$designation == "G3"], "#C2185B")
})

test_that("compute_gxe_stability maps shapes correctly from STATUS_SHAPES", {
  preds <- make_cs_predictions(designations = c("G1", "G2", "G3"))
  review <- make_review_df(c("G1", "G2", "G3"), c("SELECTED", "NOT SELECTED", "CHECK"))

  result <- compute_gxe_stability(preds, review, "MTA_001", "Yield")

  expect_equal(result$shape[result$designation == "G1"], "circle")
  expect_equal(result$shape[result$designation == "G2"], "circle-open")
  expect_equal(result$shape[result$designation == "G3"], "diamond")
})

test_that("compute_gxe_stability sets opacity 1.0 for CHECK regardless of reliability", {
  preds <- make_cs_predictions(
    designations = c("G1"),
    reliabilities = rep(0.1, 3)  # low reliability
  )
  review <- make_review_df(c("G1"), c("CHECK"))

  result <- compute_gxe_stability(preds, review, "MTA_001", "Yield")

  expect_equal(result$opacity[1], 1.0)
})

test_that("compute_gxe_stability filters by mta_stamp and trait", {
  preds_a <- make_cs_predictions(designations = c("G1", "G2"), mta_stamp = "MTA_A", trait_name = "Yield")
  preds_b <- make_cs_predictions(designations = c("G3", "G4"), mta_stamp = "MTA_B", trait_name = "Yield")
  preds_c <- make_cs_predictions(designations = c("G5"), mta_stamp = "MTA_A", trait_name = "Height")
  preds <- rbind(preds_a, preds_b, preds_c)

  review <- make_review_df(
    c("G1", "G2", "G3", "G4", "G5"),
    rep("SELECTED", 5)
  )

  result <- compute_gxe_stability(preds, review, "MTA_A", "Yield")
  expect_equal(sort(result$designation), c("G1", "G2"))
})

test_that("compute_gxe_stability returns empty data.frame when no matching data", {
  preds <- make_cs_predictions(designations = c("G1"), mta_stamp = "MTA_001")
  review <- make_review_df(c("G1"), c("SELECTED"))

  result <- compute_gxe_stability(preds, review, "MTA_OTHER", "Yield")
  expect_equal(nrow(result), 0)
  expect_equal(sort(colnames(result)),
               sort(c("designation", "mean_value", "cv", "env_variance",
                      "n_environments", "plot_status", "opacity", "color", "shape")))
})

test_that("compute_gxe_stability excludes effectType 'designation', 'fw_slope', 'GenCorrMat'", {
  # Create predictions with mixed effectTypes
  env_rows <- data.frame(
    analysisId = rep("MTA_001", 8),
    designation = c("G1", "G1", "G1", "G1", "G1", "G1", "G1", "G1"),
    trait = "Yield",
    environment = c(NA, NA, NA, "Env_A", "Env_B", "Env_C", "Env_D", "Env_E"),
    effectType = c("designation", "fw_slope", "GenCorrMat",
                   "Env_A", "Env_B", "Env_C", "Env_D", "Env_E"),
    predictedValue = c(10, 1.1, 0.9, 4, 6, 8, 5, 7),
    stdError = 0.1,
    reliability = 0.5,
    stringsAsFactors = FALSE
  )
  review <- make_review_df(c("G1"), c("SELECTED"))

  result <- compute_gxe_stability(env_rows, review, "MTA_001", "Yield")

  # mean_value should be computed from Env_A..E only (not designation/fw_slope/GenCorrMat)
  expect_equal(result$mean_value[1], mean(c(4, 6, 8, 5, 7)))
  expect_equal(result$n_environments[1], 5)
})

test_that("compute_gxe_stability with NULL overrides works same as no overrides", {
  preds <- make_cs_predictions(designations = c("G1", "G2"))
  review <- make_review_df(c("G1", "G2"), c("SELECTED", "NOT SELECTED"))

  result_null <- compute_gxe_stability(preds, review, "MTA_001", "Yield", overrides = NULL)
  result_default <- compute_gxe_stability(preds, review, "MTA_001", "Yield")

  expect_equal(result_null, result_default)
})

test_that("compute_gxe_stability n_environments counts per-env predictions correctly", {
  values_matrix <- matrix(c(
    4, 6, 8, 10, 12   # G1 in 5 environments
  ), nrow = 1)

  preds <- make_cs_predictions(
    designations = c("G1"),
    environments = c("Env_A", "Env_B", "Env_C", "Env_D", "Env_E"),
    values_matrix = values_matrix
  )
  review <- make_review_df(c("G1"), c("SELECTED"))

  result <- compute_gxe_stability(preds, review, "MTA_001", "Yield")

  expect_equal(result$n_environments[1], 5)
})

test_that("compute_gxe_stability handles NULL review_df", {
  preds <- make_cs_predictions(designations = c("G1", "G2"))

  result <- compute_gxe_stability(preds, NULL, "MTA_001", "Yield")

  # All should default to "NOT SELECTED"
  expect_true(all(result$plot_status == "NOT SELECTED"))
  expect_true(all(result$color == "#D55E00"))
})

test_that("compute_gxe_stability handles empty review_df", {
  preds <- make_cs_predictions(designations = c("G1", "G2"))
  empty_review <- data.frame(designation = character(0), plot_status = character(0),
                             stringsAsFactors = FALSE)

  result <- compute_gxe_stability(preds, empty_review, "MTA_001", "Yield")

  expect_true(all(result$plot_status == "NOT SELECTED"))
})

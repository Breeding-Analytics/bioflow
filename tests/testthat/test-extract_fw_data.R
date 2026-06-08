# Source the module file to get function definitions
source(file.path(
  testthat::test_path(), "..", "..", "R", "mod_preProdAdvApp.R"
), local = TRUE)

# Helper to create predictions with FW model data
make_fw_predictions <- function(designations = c("G1", "G2", "G3"),
                                mta_stamp = "MTA_001",
                                trait_name = "Yield",
                                mean_values = NULL,
                                slope_values = NULL,
                                mean_reliabilities = NULL,
                                slope_reliabilities = NULL) {
  n <- length(designations)
  if (is.null(mean_values)) mean_values <- seq(5, by = 1, length.out = n)
  if (is.null(slope_values)) slope_values <- seq(0.8, by = 0.1, length.out = n)
  if (is.null(mean_reliabilities)) mean_reliabilities <- rep(0.5, n)
  if (is.null(slope_reliabilities)) slope_reliabilities <- rep(0.6, n)

  means_df <- data.frame(
    analysisId = mta_stamp,
    designation = designations,
    trait = trait_name,
    environment = NA_character_,
    effectType = "designation",
    predictedValue = mean_values,
    stdError = 0.1,
    reliability = mean_reliabilities,
    stringsAsFactors = FALSE
  )

  slopes_df <- data.frame(
    analysisId = mta_stamp,
    designation = designations,
    trait = trait_name,
    environment = NA_character_,
    effectType = "fw_slope",
    predictedValue = slope_values,
    stdError = 0.05,
    reliability = slope_reliabilities,
    stringsAsFactors = FALSE
  )

  rbind(means_df, slopes_df)
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

test_that("extract_fw_data returns correct columns", {
  preds <- make_fw_predictions()
  review <- make_review_df()

  result <- extract_fw_data(preds, review, "MTA_001", "Yield")

  expected_cols <- c("designation", "mean_value", "fw_slope",
                     "plot_status", "opacity", "color", "shape")
  expect_equal(sort(colnames(result)), sort(expected_cols))
})

test_that("extract_fw_data returns one row per designation", {
  preds <- make_fw_predictions(designations = c("G1", "G2", "G3", "G4", "G5"))
  review <- make_review_df(
    designations = c("G1", "G2", "G3", "G4", "G5"),
    statuses = c("SELECTED", "SELECTED", "NOT SELECTED", "CHECK", "NOT SELECTED")
  )

  result <- extract_fw_data(preds, review, "MTA_001", "Yield")
  expect_equal(nrow(result), 5)
  expect_equal(length(unique(result$designation)), 5)
})

test_that("extract_fw_data retrieves correct mean_value and fw_slope", {
  preds <- make_fw_predictions(
    designations = c("G1", "G2"),
    mean_values = c(10.5, 8.3),
    slope_values = c(1.1, 0.9)
  )
  review <- make_review_df(c("G1", "G2"), c("SELECTED", "CHECK"))

  result <- extract_fw_data(preds, review, "MTA_001", "Yield")
  result <- result[order(result$designation), ]

  expect_equal(result$mean_value[result$designation == "G1"], 10.5)
  expect_equal(result$mean_value[result$designation == "G2"], 8.3)
  expect_equal(result$fw_slope[result$designation == "G1"], 1.1)
  expect_equal(result$fw_slope[result$designation == "G2"], 0.9)
})

test_that("extract_fw_data assigns NOT SELECTED to designations absent from review_df", {
  preds <- make_fw_predictions(designations = c("G1", "G2", "G3"))
  review <- make_review_df(c("G1"), c("SELECTED"))

  result <- extract_fw_data(preds, review, "MTA_001", "Yield")

  expect_equal(result$plot_status[result$designation == "G1"], "SELECTED")
  expect_equal(result$plot_status[result$designation == "G2"], "NOT SELECTED")
  expect_equal(result$plot_status[result$designation == "G3"], "NOT SELECTED")
})

test_that("extract_fw_data applies overrides before computing visual properties", {
  preds <- make_fw_predictions(designations = c("G1", "G2"))
  review <- make_review_df(c("G1", "G2"), c("SELECTED", "NOT SELECTED"))
  overrides <- data.frame(
    designation = "G2",
    plot_decision = "CHECK",
    stringsAsFactors = FALSE
  )

  result <- extract_fw_data(preds, review, "MTA_001", "Yield", overrides)

  # G2 should have CHECK status, color, and opacity

  expect_equal(result$plot_status[result$designation == "G2"], "CHECK")
  expect_equal(result$color[result$designation == "G2"], "#C2185B")
  expect_equal(result$opacity[result$designation == "G2"], 1.0)
  expect_equal(result$shape[result$designation == "G2"], "diamond")
})

test_that("extract_fw_data computes correct opacity for non-CHECK genotypes", {
  preds <- make_fw_predictions(
    designations = c("G1", "G2", "G3"),
    mean_reliabilities = c(0.0, 0.35, 0.7)
  )
  review <- make_review_df(c("G1", "G2", "G3"), c("SELECTED", "SELECTED", "SELECTED"))

  result <- extract_fw_data(preds, review, "MTA_001", "Yield")
  result <- result[order(result$designation), ]

  # reliability = 0 → opacity = 0.15
  expect_equal(result$opacity[1], 0.15)
  # reliability = 0.35 → opacity = 0.15 + (0.35/0.7)*0.85 = 0.575
  expect_equal(result$opacity[2], 0.575)
  # reliability = 0.7 → opacity = 1.0
  expect_equal(result$opacity[3], 1.0)
})

test_that("extract_fw_data assigns minimum opacity 0.15 for NA reliability", {
  preds <- make_fw_predictions(
    designations = c("G1", "G2"),
    mean_reliabilities = c(NA_real_, 0.5)
  )
  review <- make_review_df(c("G1", "G2"), c("SELECTED", "SELECTED"))

  result <- extract_fw_data(preds, review, "MTA_001", "Yield")

  expect_equal(result$opacity[result$designation == "G1"], 0.15)
})

test_that("extract_fw_data assigns opacity 1.0 for CHECK regardless of reliability", {
  preds <- make_fw_predictions(
    designations = c("G1"),
    mean_reliabilities = c(0.1)
  )
  review <- make_review_df(c("G1"), c("CHECK"))

  result <- extract_fw_data(preds, review, "MTA_001", "Yield")

  expect_equal(result$opacity[1], 1.0)
})

test_that("extract_fw_data maps colors correctly from STATUS_COLORS", {
  preds <- make_fw_predictions(designations = c("G1", "G2", "G3"))
  review <- make_review_df(c("G1", "G2", "G3"), c("SELECTED", "NOT SELECTED", "CHECK"))

  result <- extract_fw_data(preds, review, "MTA_001", "Yield")

  expect_equal(result$color[result$designation == "G1"], "#0072B2")
  expect_equal(result$color[result$designation == "G2"], "#D55E00")
  expect_equal(result$color[result$designation == "G3"], "#C2185B")
})

test_that("extract_fw_data maps shapes correctly from STATUS_SHAPES", {
  preds <- make_fw_predictions(designations = c("G1", "G2", "G3"))
  review <- make_review_df(c("G1", "G2", "G3"), c("SELECTED", "NOT SELECTED", "CHECK"))

  result <- extract_fw_data(preds, review, "MTA_001", "Yield")

  expect_equal(result$shape[result$designation == "G1"], "circle")
  expect_equal(result$shape[result$designation == "G2"], "circle-open")
  expect_equal(result$shape[result$designation == "G3"], "diamond")
})

test_that("extract_fw_data filters by mta_stamp and trait", {
  # Create predictions with two different MTA stamps and traits
  preds_a <- make_fw_predictions(designations = c("G1", "G2"), mta_stamp = "MTA_A", trait_name = "Yield")
  preds_b <- make_fw_predictions(designations = c("G3", "G4"), mta_stamp = "MTA_B", trait_name = "Yield")
  preds_c <- make_fw_predictions(designations = c("G5"), mta_stamp = "MTA_A", trait_name = "Height")
  preds <- rbind(preds_a, preds_b, preds_c)

  review <- make_review_df(
    c("G1", "G2", "G3", "G4", "G5"),
    rep("SELECTED", 5)
  )

  result <- extract_fw_data(preds, review, "MTA_A", "Yield")
  expect_equal(sort(result$designation), c("G1", "G2"))
})

test_that("extract_fw_data returns empty data.frame when no matching data", {
  preds <- make_fw_predictions(designations = c("G1"), mta_stamp = "MTA_001")
  review <- make_review_df(c("G1"), c("SELECTED"))

  result <- extract_fw_data(preds, review, "MTA_OTHER", "Yield")
  expect_equal(nrow(result), 0)
})

test_that("extract_fw_data handles designations with only mean but no slope", {
  # G2 has a mean but no slope entry
  means_df <- data.frame(
    analysisId = "MTA_001",
    designation = c("G1", "G2"),
    trait = "Yield",
    environment = NA_character_,
    effectType = "designation",
    predictedValue = c(10, 12),
    stdError = 0.1,
    reliability = c(0.5, 0.5),
    stringsAsFactors = FALSE
  )
  slopes_df <- data.frame(
    analysisId = "MTA_001",
    designation = c("G1"),
    trait = "Yield",
    environment = NA_character_,
    effectType = "fw_slope",
    predictedValue = c(1.0),
    stdError = 0.05,
    reliability = c(0.6),
    stringsAsFactors = FALSE
  )
  preds <- rbind(means_df, slopes_df)
  review <- make_review_df(c("G1", "G2"), c("SELECTED", "SELECTED"))

  result <- extract_fw_data(preds, review, "MTA_001", "Yield")

  # Only G1 should be in result (inner join on means and slopes)
  expect_equal(nrow(result), 1)
  expect_equal(result$designation, "G1")
})

test_that("extract_fw_data with NULL overrides works same as no overrides", {
  preds <- make_fw_predictions(designations = c("G1", "G2"))
  review <- make_review_df(c("G1", "G2"), c("SELECTED", "NOT SELECTED"))

  result_null <- extract_fw_data(preds, review, "MTA_001", "Yield", overrides = NULL)
  result_default <- extract_fw_data(preds, review, "MTA_001", "Yield")

  expect_equal(result_null, result_default)
})

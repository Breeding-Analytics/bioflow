# Source the module file to get function definitions
source(file.path(
  testthat::test_path(), "..", "..", "R", "mod_preProdAdvApp.R"
), local = TRUE)

# Helper to create minimal sta_long test data
make_sta_long <- function(designations = c("G1", "G2", "G3"),
                          environments = c("ENV_A", "ENV_B"),
                          trait_name = "Yield",
                          pred_values = NULL,
                          reliabilities = NULL) {
  combos <- expand.grid(
    designation = designations,
    environment = environments,
    stringsAsFactors = FALSE
  )
  combos$trait <- trait_name
  n <- nrow(combos)
  if (is.null(pred_values)) {
    combos$predictedValue <- seq(1, n)
  } else {
    combos$predictedValue <- pred_values
  }
  if (is.null(reliabilities)) {
    combos$reliability <- rep(0.5, n)
  } else {
    combos$reliability <- reliabilities
  }
  combos
}

# Helper to create minimal review_df test data
make_review_df <- function(designations = c("G1", "G2", "G3"),
                           statuses = c("SELECTED", "NOT SELECTED", "CHECK")) {
  data.frame(
    designation = designations,
    plot_status = statuses,
    stringsAsFactors = FALSE
  )
}

test_that("prepare_beeswarm_data returns correct columns", {
  sta <- make_sta_long()
  review <- make_review_df()

  result <- prepare_beeswarm_data(sta, review, "Yield")

  expected_cols <- c("designation", "environment", "predictedValue",
                     "reliability", "plot_status", "opacity", "color")
  expect_equal(colnames(result), expected_cols)
})

test_that("prepare_beeswarm_data filters to selected trait only", {
  sta_yield <- make_sta_long(trait_name = "Yield")
  sta_dtf <- make_sta_long(trait_name = "DTF", pred_values = rep(60, 6))
  sta <- rbind(sta_yield, sta_dtf)
  review <- make_review_df()

  result <- prepare_beeswarm_data(sta, review, "Yield")

  # Should only contain Yield rows
  expect_equal(nrow(result), 6)  # 3 genotypes * 2 environments
  # All rows have predictedValue from the Yield data (1..6)
  expect_true(all(result$predictedValue %in% 1:6))
})

test_that("prepare_beeswarm_data removes non-finite predictedValue rows", {
  sta <- make_sta_long(
    designations = c("G1", "G2", "G3"),
    environments = c("ENV_A"),
    pred_values = c(1.5, NA, Inf)
  )
  review <- make_review_df()

  result <- prepare_beeswarm_data(sta, review, "Yield")

  # Only G1 has finite value

  expect_equal(nrow(result), 1)
  expect_equal(result$designation, "G1")
  expect_equal(result$predictedValue, 1.5)
})

test_that("prepare_beeswarm_data removes -Inf predictedValue rows", {
  sta <- make_sta_long(
    designations = c("G1", "G2"),
    environments = c("ENV_A"),
    pred_values = c(-Inf, 3.0)
  )
  review <- make_review_df(designations = c("G1", "G2"),
                           statuses = c("SELECTED", "NOT SELECTED"))

  result <- prepare_beeswarm_data(sta, review, "Yield")

  expect_equal(nrow(result), 1)
  expect_equal(result$designation, "G2")
})

test_that("prepare_beeswarm_data merges plot_status from review_df", {
  sta <- make_sta_long()
  review <- make_review_df()  # G1=SELECTED, G2=NOT SELECTED, G3=CHECK

  result <- prepare_beeswarm_data(sta, review, "Yield")

  g1_rows <- result[result$designation == "G1", ]
  g2_rows <- result[result$designation == "G2", ]
  g3_rows <- result[result$designation == "G3", ]

  expect_true(all(g1_rows$plot_status == "SELECTED"))
  expect_true(all(g2_rows$plot_status == "NOT SELECTED"))
  expect_true(all(g3_rows$plot_status == "CHECK"))
})

test_that("prepare_beeswarm_data applies overrides correctly", {
  sta <- make_sta_long()
  review <- make_review_df()  # G1=SELECTED, G2=NOT SELECTED, G3=CHECK

  overrides <- data.frame(
    designation = c("G1", "G2"),
    plot_decision = c("NOT SELECTED", "SELECTED"),
    stringsAsFactors = FALSE
  )

  result <- prepare_beeswarm_data(sta, review, "Yield", overrides = overrides)

  g1_rows <- result[result$designation == "G1", ]
  g2_rows <- result[result$designation == "G2", ]
  g3_rows <- result[result$designation == "G3", ]

  # G1 overridden from SELECTED to NOT SELECTED
  expect_true(all(g1_rows$plot_status == "NOT SELECTED"))
  # G2 overridden from NOT SELECTED to SELECTED
  expect_true(all(g2_rows$plot_status == "SELECTED"))
  # G3 not overridden, stays CHECK
  expect_true(all(g3_rows$plot_status == "CHECK"))
})

test_that("prepare_beeswarm_data assigns correct colors by status", {
  sta <- make_sta_long()
  review <- make_review_df()  # G1=SELECTED, G2=NOT SELECTED, G3=CHECK

  result <- prepare_beeswarm_data(sta, review, "Yield")

  g1_rows <- result[result$designation == "G1", ]
  g2_rows <- result[result$designation == "G2", ]
  g3_rows <- result[result$designation == "G3", ]

  expect_true(all(g1_rows$color == "#0072B2"))
  expect_true(all(g2_rows$color == "#D55E00"))
  expect_true(all(g3_rows$color == "#C2185B"))
})

test_that("prepare_beeswarm_data computes opacity from reliability", {
  sta <- make_sta_long(
    designations = c("G1", "G2"),
    environments = c("ENV_A"),
    reliabilities = c(0.0, 0.7)
  )
  review <- make_review_df(
    designations = c("G1", "G2"),
    statuses = c("SELECTED", "SELECTED")
  )

  result <- prepare_beeswarm_data(sta, review, "Yield")

  g1_row <- result[result$designation == "G1", ]
  g2_row <- result[result$designation == "G2", ]

  expect_equal(g1_row$opacity, 0.15)  # reliability 0 -> 0.15
  expect_equal(g2_row$opacity, 1.0)   # reliability 0.7 -> 1.0
})

test_that("prepare_beeswarm_data forces CHECK opacity to 1.0 regardless of reliability", {
  sta <- make_sta_long(
    designations = c("G1", "G2", "G3"),
    environments = c("ENV_A"),
    reliabilities = c(0.0, 0.3, NA)
  )
  review <- make_review_df(
    designations = c("G1", "G2", "G3"),
    statuses = c("CHECK", "CHECK", "CHECK")
  )

  result <- prepare_beeswarm_data(sta, review, "Yield")

  # All CHECK, so all opacity should be 1.0
  expect_true(all(result$opacity == 1.0))
})

test_that("prepare_beeswarm_data orders environments alphabetically", {
  sta <- make_sta_long(
    designations = c("G1"),
    environments = c("Zambia", "Argentina", "Mexico")
  )
  review <- make_review_df(designations = "G1", statuses = "SELECTED")

  result <- prepare_beeswarm_data(sta, review, "Yield")

  env_levels <- levels(result$environment)
  expect_equal(env_levels, c("Argentina", "Mexico", "Zambia"))
})

test_that("prepare_beeswarm_data returns empty data.frame when no rows match trait", {
  sta <- make_sta_long(trait_name = "DTF")
  review <- make_review_df()

  result <- prepare_beeswarm_data(sta, review, "Yield")

  expect_equal(nrow(result), 0)
  expected_cols <- c("designation", "environment", "predictedValue",
                     "reliability", "plot_status", "opacity", "color")
  expect_equal(colnames(result), expected_cols)
})

test_that("prepare_beeswarm_data handles NULL overrides (no change)", {
  sta <- make_sta_long()
  review <- make_review_df()

  result_null <- prepare_beeswarm_data(sta, review, "Yield", overrides = NULL)
  result_no_arg <- prepare_beeswarm_data(sta, review, "Yield")

  expect_equal(result_null, result_no_arg)
})

test_that("prepare_beeswarm_data handles empty overrides data.frame", {
  sta <- make_sta_long()
  review <- make_review_df()

  empty_overrides <- data.frame(
    designation = character(0),
    plot_decision = character(0),
    stringsAsFactors = FALSE
  )

  result_empty <- prepare_beeswarm_data(sta, review, "Yield", overrides = empty_overrides)
  result_null <- prepare_beeswarm_data(sta, review, "Yield", overrides = NULL)

  expect_equal(result_empty, result_null)
})

test_that("prepare_beeswarm_data produces one row per genotype-environment with finite value", {
  sta <- make_sta_long(
    designations = c("G1", "G2"),
    environments = c("ENV_A", "ENV_B", "ENV_C")
  )
  review <- make_review_df(
    designations = c("G1", "G2"),
    statuses = c("SELECTED", "NOT SELECTED")
  )

  result <- prepare_beeswarm_data(sta, review, "Yield")

  # 2 genotypes x 3 environments = 6 rows
  expect_equal(nrow(result), 6)

  # No duplicate genotype-environment combos
  combos <- paste(result$designation, result$environment, sep = "_")
  expect_equal(length(combos), length(unique(combos)))
})

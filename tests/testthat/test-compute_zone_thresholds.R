# Source the module file to get function definitions
source(file.path(
  testthat::test_path(), "..", "..", "R", "mod_preProdAdvApp.R"
), local = TRUE)

# Helper to create minimal prepared_data for testing
make_prepared_data <- function(designations = c("G1", "G2", "G3", "G4"),
                               cluster = "Cluster_A",
                               mean_values = c(5.0, 4.0, 3.0, 2.0),
                               statuses = c("SELECTED", "NOT SELECTED", "CHECK", "CHECK")) {
  n <- length(designations)
  data.frame(
    designation       = designations,
    cluster           = rep(cluster, n),
    mean_value        = mean_values,
    mean_reliability  = rep(0.5, n),
    plot_status       = statuses,
    marker_shape      = STATUS_SHAPES[statuses],
    marker_color      = STATUS_COLORS[statuses],
    rank_in_cluster   = seq_len(n),
    overall_rank      = seq_len(n),
    n_envs_in_cluster = rep(3L, n),
    stringsAsFactors  = FALSE
  )
}

# --- Test: cluster with CHECK designations ---

test_that("compute_zone_thresholds returns correct columns and one row per cluster", {
  pd <- make_prepared_data()
  result <- compute_zone_thresholds(pd)

  expected_cols <- c("cluster", "recommend_threshold", "avoid_threshold",
                     "check_mean", "pop_mean", "pop_sd")
  expect_equal(colnames(result), expected_cols)
  expect_equal(nrow(result), 1)
  expect_equal(result$cluster, "Cluster_A")
})

test_that("recommend_threshold equals check_mean when CHECKs exist", {
  # G3 (CHECK) = 3.0, G4 (CHECK) = 2.0 → check_mean = 2.5

  pd <- make_prepared_data(
    designations = c("G1", "G2", "G3", "G4"),
    mean_values  = c(5.0, 4.0, 3.0, 2.0),
    statuses     = c("SELECTED", "NOT SELECTED", "CHECK", "CHECK")
  )
  result <- compute_zone_thresholds(pd)

  expected_check_mean <- mean(c(3.0, 2.0))
  expect_equal(result$recommend_threshold, expected_check_mean)
  expect_equal(result$check_mean, expected_check_mean)
})

test_that("avoid_threshold is min(pop_mean - 1*pop_sd, min_check_value)", {
  pd <- make_prepared_data(
    designations = c("G1", "G2", "G3", "G4"),
    mean_values  = c(5.0, 4.0, 3.0, 2.0),
    statuses     = c("SELECTED", "NOT SELECTED", "CHECK", "CHECK")
  )
  result <- compute_zone_thresholds(pd)

  pop_mean <- mean(c(5.0, 4.0, 3.0, 2.0))
  pop_sd   <- sd(c(5.0, 4.0, 3.0, 2.0))
  min_check <- min(3.0, 2.0)
  expected_avoid <- min(pop_mean - 1 * pop_sd, min_check)

  expect_equal(result$avoid_threshold, expected_avoid)
})

# --- Test: cluster without CHECK designations ---

test_that("recommend_threshold falls back to pop_mean when no CHECKs", {

  pd <- make_prepared_data(
    designations = c("G1", "G2", "G3"),
    mean_values  = c(6.0, 4.0, 2.0),
    statuses     = c("SELECTED", "NOT SELECTED", "SELECTED")
  )
  result <- compute_zone_thresholds(pd)

  expected_pop_mean <- mean(c(6.0, 4.0, 2.0))
  expect_equal(result$recommend_threshold, expected_pop_mean)
  expect_true(is.na(result$check_mean))
})

test_that("avoid_threshold without CHECKs is pop_mean - pop_sd", {
  pd <- make_prepared_data(
    designations = c("G1", "G2", "G3"),
    mean_values  = c(6.0, 4.0, 2.0),
    statuses     = c("SELECTED", "NOT SELECTED", "SELECTED")
  )
  result <- compute_zone_thresholds(pd)

  pop_mean <- mean(c(6.0, 4.0, 2.0))
  pop_sd   <- sd(c(6.0, 4.0, 2.0))
  expected_avoid <- pop_mean - 1 * pop_sd

  expect_equal(result$avoid_threshold, expected_avoid)
})

# --- Test: single designation cluster ---

test_that("single designation cluster has pop_sd = 0 and avoid = recommend", {
  pd <- make_prepared_data(
    designations = c("G1"),
    mean_values  = c(3.5),
    statuses     = c("CHECK")
  )
  result <- compute_zone_thresholds(pd)

  expect_equal(result$pop_sd, 0)
  # With single value: sd() returns NA → forced to 0

  # check_mean = 3.5, pop_mean = 3.5
  expect_equal(result$recommend_threshold, 3.5)
  # avoid = min(pop_mean - 0, min_check_value) = min(3.5, 3.5) = 3.5
  # enforce avoid <= recommend → 3.5 <= 3.5 OK
  expect_equal(result$avoid_threshold, 3.5)
})

# --- Test: threshold ordering invariant ---

test_that("avoid_threshold is always <= recommend_threshold", {
  # Case where pop_mean - pop_sd could be higher than check_mean
  pd <- make_prepared_data(
    designations = c("G1", "G2", "G3"),
    mean_values  = c(10.0, 9.5, 1.0),
    statuses     = c("SELECTED", "SELECTED", "CHECK")
  )
  result <- compute_zone_thresholds(pd)

  expect_true(result$avoid_threshold <= result$recommend_threshold)
})

test_that("threshold ordering holds across multiple clusters", {
  pd1 <- make_prepared_data(
    designations = c("G1", "G2", "G3"),
    cluster      = "High rainfall environments",
    mean_values  = c(8.0, 6.0, 4.0),
    statuses     = c("SELECTED", "CHECK", "NOT SELECTED")
  )
  pd2 <- make_prepared_data(
    designations = c("G1", "G2", "G3"),
    cluster      = "Low rainfall environments",
    mean_values  = c(5.0, 3.0, 1.0),
    statuses     = c("CHECK", "NOT SELECTED", "NOT SELECTED")
  )
  pd <- rbind(pd1, pd2)
  result <- compute_zone_thresholds(pd)

  expect_equal(nrow(result), 2)
  expect_true(all(result$avoid_threshold <= result$recommend_threshold))
})

# --- Test: all thresholds are finite ---

test_that("all threshold values are finite", {
  pd <- make_prepared_data(
    designations = c("G1", "G2", "G3", "G4", "G5"),
    mean_values  = c(10.0, 7.5, 5.0, 2.5, 0.0),
    statuses     = c("SELECTED", "CHECK", "NOT SELECTED", "CHECK", "REVISE")
  )
  result <- compute_zone_thresholds(pd)

  expect_true(is.finite(result$recommend_threshold))
  expect_true(is.finite(result$avoid_threshold))
  expect_true(is.finite(result$pop_mean))
  expect_true(is.finite(result$pop_sd))
  # check_mean should also be finite when CHECKs exist
  expect_true(is.finite(result$check_mean))
})

# --- Test: multiple clusters produce one row each ---

test_that("multiple clusters produce correct number of rows", {
  pd1 <- make_prepared_data(cluster = "A", mean_values = c(4, 3, 2, 1))
  pd2 <- make_prepared_data(cluster = "B", mean_values = c(8, 7, 6, 5))
  pd3 <- make_prepared_data(cluster = "C", mean_values = c(12, 11, 10, 9))
  pd <- rbind(pd1, pd2, pd3)

  result <- compute_zone_thresholds(pd)

  expect_equal(nrow(result), 3)
  expect_equal(sort(result$cluster), c("A", "B", "C"))
})

# --- Test: pop_mean and pop_sd are computed correctly ---

test_that("pop_mean and pop_sd are computed correctly", {
  vals <- c(10.0, 8.0, 6.0, 4.0)
  pd <- make_prepared_data(mean_values = vals)
  result <- compute_zone_thresholds(pd)

  expect_equal(result$pop_mean, mean(vals))
  expect_equal(result$pop_sd, sd(vals))
})

# --- Test: all same mean_value (pop_sd = 0) ---

test_that("when all mean_values are identical pop_sd is 0 and avoid equals recommend", {
  pd <- make_prepared_data(
    designations = c("G1", "G2", "G3"),
    mean_values  = c(5.0, 5.0, 5.0),
    statuses     = c("SELECTED", "CHECK", "NOT SELECTED")
  )
  result <- compute_zone_thresholds(pd)

  expect_equal(result$pop_sd, 0)
  # check_mean = 5.0, pop_mean = 5.0
  expect_equal(result$recommend_threshold, 5.0)
  # avoid = min(5.0 - 0, 5.0) = 5.0, enforce <= 5.0 → OK
  expect_equal(result$avoid_threshold, 5.0)
})

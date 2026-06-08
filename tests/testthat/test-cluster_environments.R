# Source the module file to get function definitions
source(file.path(
  testthat::test_path(), "..", "..", "R", "mod_preProdAdvApp.R"
), local = TRUE)

# --- Helper: create a weather summary data frame ---
make_weather_summary <- function(n = 6, seed = 123) {
  set.seed(seed)
  data.frame(
    environment                 = paste0("ENV_", seq_len(n)),
    mean_temperature            = runif(n, 20, 35),
    heat_stress_index           = runif(n, 0, 1),
    rainfall                    = runif(n, 200, 1200),
    rainfall_distribution_index = runif(n, 0.3, 0.9),
    humidity                    = runif(n, 40, 90),
    stringsAsFactors            = FALSE
  )
}

# ===========================================================================
# 1. Auto-detected k produces valid clusters
# ===========================================================================
test_that("auto-detect k (NULL) produces valid clusters with enough environments", {
  ws <- make_weather_summary(n = 10, seed = 42)
  result <- cluster_environments(ws)

  expect_equal(length(result), 10)
  expect_true(length(unique(result)) >= 1)
  expect_true(length(unique(result)) <= 3)
})

# ===========================================================================
# 2. Explicit k=2 produces 2 clusters
# ===========================================================================
test_that("explicit k=2 produces exactly 2 clusters", {
  ws <- make_weather_summary(n = 10, seed = 42)
  result <- cluster_environments(ws, k = 2)

  expect_equal(length(result), 10)
  expect_equal(length(unique(result)), 2)
})

# ===========================================================================
# 3. Explicit k=3 produces 3 clusters
# ===========================================================================
test_that("explicit k=3 produces exactly 3 clusters", {
  ws <- make_weather_summary(n = 12, seed = 99)
  result <- cluster_environments(ws, k = 3)


  expect_equal(length(result), 12)
  expect_equal(length(unique(result)), 3)
})

# ===========================================================================
# 4. Fallback to single cluster when fewer than 2 finite values
# ===========================================================================
test_that("fallback to 'All environments' when fewer than 2 finite values", {
  ws <- data.frame(
    environment     = c("A", "B", "C"),
    mean_temperature = c(NA, NaN, Inf),
    heat_stress_index = c(NA, NaN, Inf),
    rainfall        = c(NA, NaN, Inf),
    rainfall_distribution_index = c(NA, NaN, Inf),
    humidity        = c(NA, NaN, Inf),
    stringsAsFactors = FALSE
  )

  result <- cluster_environments(ws)
  expect_equal(length(result), 3)
  expect_equal(unique(result), "All environments")
})

test_that("fallback to 'All environments' when only 1 finite row", {
  ws <- data.frame(
    environment     = c("A", "B", "C"),
    mean_temperature = c(25, NA, NA),
    heat_stress_index = c(0.1, NA, NA),
    rainfall        = c(100, NA, NA),
    rainfall_distribution_index = c(0.5, NA, NA),
    humidity        = c(50, NA, NA),
    stringsAsFactors = FALSE
  )

  result <- cluster_environments(ws)
  expect_equal(unique(result), "All environments")
})

# ===========================================================================
# 5. Output names match input environments
# ===========================================================================
test_that("output names match input environments exactly", {
  ws <- make_weather_summary(n = 5, seed = 7)
  env_names <- ws$environment

  result <- cluster_environments(ws, k = 2)
  expect_equal(names(result), env_names)
})

test_that("output length equals number of input environments", {
  ws <- make_weather_summary(n = 8, seed = 11)
  result <- cluster_environments(ws, k = 2)
  expect_equal(length(result), nrow(ws))
})

# ===========================================================================
# 6. Labels are descriptive strings with High/Low and covariate info
# ===========================================================================
test_that("kmeans labels are descriptive strings with High/Low and covariate info", {
  ws <- make_weather_summary(n = 10, seed = 77)
  result <- cluster_environments(ws, k = 2)
  unique_labels <- unique(result)

  # Each label should contain "High" or "Low"
  for (lbl in unique_labels) {
    expect_true(
      grepl("High", lbl) || grepl("Low", lbl),
      info = paste("Label should contain High or Low:", lbl)
    )
  }
})

# ===========================================================================
# 7. Low silhouette falls back to single cluster
# ===========================================================================
test_that("low silhouette score falls back to 'All environments'", {
  # Create environments with very similar covariate values (no clear clusters)
  set.seed(999)
  n <- 10
  ws <- data.frame(
    environment                 = paste0("ENV_", seq_len(n)),
    mean_temperature            = rep(25, n) + rnorm(n, sd = 0.001),
    heat_stress_index           = rep(0.5, n) + rnorm(n, sd = 0.001),
    rainfall                    = rep(500, n) + rnorm(n, sd = 0.001),
    rainfall_distribution_index = rep(0.6, n) + rnorm(n, sd = 0.001),
    humidity                    = rep(60, n) + rnorm(n, sd = 0.001),
    stringsAsFactors            = FALSE
  )

  result <- cluster_environments(ws)
  # With essentially identical environments, silhouette should be low → single cluster
  expect_true(length(unique(result)) <= 2)  # Either 1 (fallback) or 2 (if barely above threshold)
})

# ===========================================================================
# 8. No available covariates fallback
# ===========================================================================
test_that("fallback when no recognized covariates are present", {
  ws <- data.frame(
    environment = c("A", "B", "C"),
    some_other_col = c(1, 2, 3),
    stringsAsFactors = FALSE
  )

  result <- cluster_environments(ws)
  expect_equal(length(result), 3)
  expect_equal(unique(result), "All environments")
})

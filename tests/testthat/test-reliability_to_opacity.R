# Source the module file to get function definitions
source(file.path(
  testthat::test_path(), "..", "..", "R", "mod_preProdAdvApp.R"
), local = TRUE)

test_that("reliability_to_opacity returns correct length", {
  expect_length(reliability_to_opacity(c(0, 0.5, 1.0)), 3)
  expect_length(reliability_to_opacity(numeric(0)), 0)
  expect_length(reliability_to_opacity(c(NA, 0.3, NA, 0.7)), 4)
})

test_that("reliability_to_opacity maps boundary values correctly", {
  # reliability = 0 -> opacity = 0.15

  expect_equal(reliability_to_opacity(0), 0.15)

  # reliability = 0.7 -> opacity = 1.0
  expect_equal(reliability_to_opacity(0.7), 1.0)

  # reliability = 1.0 -> opacity = 1.0 (capped)
  expect_equal(reliability_to_opacity(1.0), 1.0)

  # reliability = 0.35 (midpoint) -> opacity = 0.15 + 0.5 * 0.85 = 0.575

  expect_equal(reliability_to_opacity(0.35), 0.575)
})

test_that("reliability_to_opacity treats NA as 0.7 (full opacity)", {
  expect_equal(reliability_to_opacity(NA_real_), 1.0)
  result <- reliability_to_opacity(c(0, NA, 0.35))
  expect_equal(result, c(0.15, 1.0, 0.575))
})

test_that("reliability_to_opacity caps values > 0.7 at opacity 1.0", {
  expect_equal(reliability_to_opacity(0.8), 1.0)
  expect_equal(reliability_to_opacity(0.9), 1.0)
  expect_equal(reliability_to_opacity(5.0), 1.0)
})

test_that("reliability_to_opacity floors negative values at 0.15", {
  expect_equal(reliability_to_opacity(-0.5), 0.15)
  expect_equal(reliability_to_opacity(-100), 0.15)
  expect_equal(reliability_to_opacity(-0.01), 0.15)
})

test_that("reliability_to_opacity output always in [0.15, 1.0]", {
  # Test with a variety of extreme inputs

  inputs <- c(-100, -1, -0.001, 0, 0.1, 0.35, 0.5, 0.7, 0.8, 1.0, 5.0, 100, NA)
  result <- reliability_to_opacity(inputs)
  expect_true(all(result >= 0.15))
  expect_true(all(result <= 1.0))
})

test_that("reliability_to_opacity is monotonically increasing in [0, 0.7]", {
  vals <- seq(0, 0.7, by = 0.01)
  result <- reliability_to_opacity(vals)
  # Each successive value should be >= previous (strictly increasing)
  diffs <- diff(result)
  expect_true(all(diffs > 0))
})

test_that("reliability_to_opacity is vectorized correctly", {
  input <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)
  result <- reliability_to_opacity(input)
  # Manually compute expected values
  expected <- 0.15 + (input / 0.7) * 0.85
  expect_equal(result, expected)
})

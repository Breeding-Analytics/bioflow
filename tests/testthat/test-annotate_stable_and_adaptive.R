# Source the module file to get function definitions
source(file.path(
  testthat::test_path(), "..", "..", "R", "mod_preProdAdvApp.R"
), local = TRUE)

# --- Helpers ---

#' Create a minimal FW plot_data data.frame
make_fw_plot_data <- function(designations = paste0("G", 1:10),
                              mean_values = NULL,
                              fw_slopes = NULL,
                              statuses = NULL) {
  n <- length(designations)
  if (is.null(mean_values)) mean_values <- seq(3, 7, length.out = n)
  if (is.null(fw_slopes)) fw_slopes <- seq(0.5, 1.5, length.out = n)

  if (is.null(statuses)) statuses <- rep("SELECTED", n)

  data.frame(
    designation = designations,
    mean_value = mean_values,
    fw_slope = fw_slopes,
    plot_status = statuses,
    opacity = rep(0.8, n),
    color = rep("#0072B2", n),
    shape = rep("circle", n),
    stringsAsFactors = FALSE
  )
}

#' Create a minimal CS/Diag plot_data data.frame
make_cs_plot_data <- function(designations = paste0("G", 1:10),
                              mean_values = NULL,
                              cvs = NULL,
                              statuses = NULL) {
  n <- length(designations)
  if (is.null(mean_values)) mean_values <- seq(3, 7, length.out = n)
  if (is.null(cvs)) cvs <- seq(0.05, 0.5, length.out = n)
  if (is.null(statuses)) statuses <- rep("SELECTED", n)

  data.frame(
    designation = designations,
    mean_value = mean_values,
    cv = cvs,
    plot_status = statuses,
    opacity = rep(0.8, n),
    color = rep("#0072B2", n),
    shape = rep("circle", n),
    stringsAsFactors = FALSE
  )
}

#' Create a minimal FA plot_data data.frame
make_fa_plot_data <- function(designations = paste0("G", 1:10),
                              mean_values = NULL,
                              pc1 = NULL,
                              pc2 = NULL,
                              statuses = NULL) {
  n <- length(designations)
  if (is.null(mean_values)) mean_values <- seq(3, 7, length.out = n)
  if (is.null(pc1)) pc1 <- seq(-2, 2, length.out = n)
  if (is.null(pc2)) pc2 <- seq(-1, 1, length.out = n)
  if (is.null(statuses)) statuses <- rep("SELECTED", n)

  data.frame(
    designation = designations,
    PC1 = pc1,
    PC2 = pc2,
    mean_value = mean_values,
    plot_status = statuses,
    opacity = rep(0.8, n),
    color = rep("#0072B2", n),
    shape = rep("circle", n),
    stringsAsFactors = FALSE
  )
}

#' Create a simple plotly scatter plot with customdata containing designations
make_test_plotly <- function(plot_data, x_col = "mean_value", y_col = "fw_slope") {
  plotly::plot_ly(
    data = plot_data,
    x = ~get(x_col),
    y = ~get(y_col),
    type = "scatter",
    mode = "markers",
    marker = list(size = 8),
    customdata = plot_data$designation,
    text = plot_data$designation
  )
}

# ===========================
# TESTS
# ===========================

test_that("annotate_stable_and_adaptive returns plot unchanged for unknown model_type", {
  pd <- make_fw_plot_data()
  p <- make_test_plotly(pd)
  result <- annotate_stable_and_adaptive(p, pd, "unknown_model")
  # Should return the original plot unchanged (as a plotly object)
  expect_true(inherits(result, "plotly") || inherits(result, "htmlwidget"))
})

test_that("annotate_stable_and_adaptive skips annotations when metrics within 1e-9", {
  # All fw_slopes identical → |slope - 1| all the same
  pd <- make_fw_plot_data(fw_slopes = rep(1.0, 10))
  p <- make_test_plotly(pd)
  result <- annotate_stable_and_adaptive(p, pd, "fw")

  # Should return unchanged (no annotations added)
  # Build the result to check for annotations
  built <- plotly::plotly_build(result)
  # If metrics are all 0, which has range 0 < 1e-9, no annotations should exist
  annotations <- built$x$layout$annotations
  # Annotations from annotate function should not be present
  # (some plotly defaults may exist, but no stable/adaptive ones)
  ann_texts <- vapply(annotations %||% list(), function(a) a$text %||% "", character(1))
  expect_false(any(grepl("Stable high-performers", ann_texts)))
  expect_false(any(grepl("Responsive", ann_texts)))
})

test_that("annotate_stable_and_adaptive classifies stable high-performers correctly for FW", {
  # Create clear separation: some genotypes with slope near 1 (low |slope-1|) and high mean
  # G1-G3: slope ~ 1.0 (metric ~ 0), high mean
  # G4-G10: slope far from 1 (metric high), various means
  designations <- paste0("G", 1:10)
  fw_slopes <- c(1.0, 1.01, 0.99, 1.8, 1.7, 1.6, 0.3, 0.2, 1.9, 2.0)
  mean_values <- c(8, 7.5, 7, 5, 4.5, 4, 3.5, 3, 2.5, 2)
  statuses <- c(rep("CHECK", 3), rep("SELECTED", 7))

  pd <- make_fw_plot_data(
    designations = designations,
    mean_values = mean_values,
    fw_slopes = fw_slopes,
    statuses = statuses
  )

  p <- make_test_plotly(pd)
  result <- annotate_stable_and_adaptive(p, pd, "fw")
  built <- plotly::plotly_build(result)

  # Check that annotations exist
  annotations <- built$x$layout$annotations
  ann_texts <- vapply(annotations %||% list(), function(a) a$text %||% "", character(1))

  # G1, G2, G3 have lowest |slope-1| (0, 0.01, 0.01) and highest mean values
  # Check_Mean = mean of CHECK genotypes mean_values = mean(8, 7.5, 7) = 7.5
  # So stable needs mean > 7.5 → only G1 qualifies (mean=8)
  # The stable annotation should be present
  expect_true(any(grepl("Stable high-performers", ann_texts)))
})

test_that("annotate_stable_and_adaptive classifies adaptive responders correctly for FW", {
  # G9, G10 should have high |slope - 1| (> 75th pctile) and mean > median
  designations <- paste0("G", 1:10)
  # Slopes: G1-G5 near 1, G6-G10 far from 1
  fw_slopes <- c(1.0, 1.02, 0.98, 1.05, 0.95, 1.5, 1.7, 1.8, 1.9, 2.0)
  # Means: spread, G9 and G10 have above-median means
  mean_values <- c(3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5)

  pd <- make_fw_plot_data(
    designations = designations,
    mean_values = mean_values,
    fw_slopes = fw_slopes
  )

  p <- make_test_plotly(pd)
  result <- annotate_stable_and_adaptive(p, pd, "fw")
  built <- plotly::plotly_build(result)

  annotations <- built$x$layout$annotations
  ann_texts <- vapply(annotations %||% list(), function(a) a$text %||% "", character(1))

  # G9, G10 have |slope-1| = 0.9, 1.0 → highest metrics (> 75th pctile)
  # Their means (7, 7.5) are above median(mean_values) = 5.25
  expect_true(any(grepl("Responsive", ann_texts)))
})

test_that("annotate_stable_and_adaptive uses 75th percentile fallback when no CHECK", {
  # No CHECK genotypes → threshold = 75th percentile of mean_value
  designations <- paste0("G", 1:8)
  fw_slopes <- c(1.0, 1.0, 1.01, 1.5, 1.6, 1.7, 0.2, 0.1)
  mean_values <- c(10, 9, 8, 7, 6, 5, 4, 3)
  statuses <- rep("SELECTED", 8)

  pd <- make_fw_plot_data(
    designations = designations,
    mean_values = mean_values,
    fw_slopes = fw_slopes,
    statuses = statuses
  )

  p <- make_test_plotly(pd)
  result <- annotate_stable_and_adaptive(p, pd, "fw")
  built <- plotly::plotly_build(result)

  annotations <- built$x$layout$annotations
  ann_texts <- vapply(annotations %||% list(), function(a) a$text %||% "", character(1))

  # 75th percentile of mean_values = quantile(c(3,4,5,6,7,8,9,10), 0.75) = 8.25
  # Stable needs metric ≤ 25th pctile AND mean > 8.25
  # G1 (slope=1, metric=0, mean=10) and G2 (slope=1, metric=0, mean=9) qualify
  expect_true(any(grepl("Stable high-performers", ann_texts)))
})

test_that("annotate_stable_and_adaptive shows limited data suffix when only 1 qualifies", {
  # Only 1 genotype will qualify as stable
  designations <- paste0("G", 1:8)
  fw_slopes <- c(1.0, 1.5, 1.6, 1.7, 1.8, 0.2, 0.1, 0.05)
  # Only G1 has low metric (0) AND high mean
  # Make G1 the only one with mean above the threshold
  mean_values <- c(10, 3, 3, 3, 3, 3, 3, 3)
  statuses <- rep("SELECTED", 8)

  pd <- make_fw_plot_data(
    designations = designations,
    mean_values = mean_values,
    fw_slopes = fw_slopes,
    statuses = statuses
  )

  p <- make_test_plotly(pd)
  result <- annotate_stable_and_adaptive(p, pd, "fw")
  built <- plotly::plotly_build(result)

  annotations <- built$x$layout$annotations
  ann_texts <- vapply(annotations %||% list(), function(a) a$text %||% "", character(1))

  # Should show "(limited data)" suffix
  stable_ann <- ann_texts[grepl("Stable high-performers", ann_texts)]
  expect_true(length(stable_ann) > 0)
  expect_true(grepl("limited data", stable_ann[1]))
})

test_that("annotate_stable_and_adaptive shows '... and N more' for > 5 stable", {
  # Many genotypes with low metric and high mean; use CHECK to set threshold low
  designations <- paste0("G", 1:20)
  # First 8 slopes near 1 (metric ~ 0), rest far from 1 (high metric)
  fw_slopes <- c(rep(1.0, 8), seq(1.5, 2.5, length.out = 12))
  # First 8 have high mean, rest have low mean
  mean_values <- c(rep(10, 8), rep(3, 12))
  # Use CHECK with low mean to set Check_Mean threshold low
  statuses <- c(rep("SELECTED", 8), "CHECK", rep("SELECTED", 11))
  # Move the CHECK to have a low mean value (it's already in the "low" group, mean=3)

  pd <- make_fw_plot_data(
    designations = designations,
    mean_values = mean_values,
    fw_slopes = fw_slopes,
    statuses = statuses
  )

  p <- make_test_plotly(pd)
  result <- annotate_stable_and_adaptive(p, pd, "fw")
  built <- plotly::plotly_build(result)

  annotations <- built$x$layout$annotations
  ann_texts <- vapply(annotations %||% list(), function(a) a$text %||% "", character(1))

  stable_ann <- ann_texts[grepl("Stable high-performers", ann_texts)]
  expect_true(length(stable_ann) > 0)
  # Should contain "and N more" (unicode ellipsis)
  expect_true(grepl("more", stable_ann[1]))
})

test_that("annotate_stable_and_adaptive works with cs_diag model", {
  designations <- paste0("G", 1:10)
  cvs <- c(0.05, 0.06, 0.07, 0.2, 0.25, 0.3, 0.4, 0.45, 0.5, 0.55)
  mean_values <- c(8, 7.5, 7, 6, 5.5, 5, 4.5, 4, 3.5, 3)
  statuses <- c("CHECK", "CHECK", rep("SELECTED", 8))

  pd <- make_cs_plot_data(
    designations = designations,
    mean_values = mean_values,
    cvs = cvs,
    statuses = statuses
  )

  p <- make_test_plotly(pd, x_col = "mean_value", y_col = "cv")
  result <- annotate_stable_and_adaptive(p, pd, "cs_diag")
  built <- plotly::plotly_build(result)

  annotations <- built$x$layout$annotations
  ann_texts <- vapply(annotations %||% list(), function(a) a$text %||% "", character(1))

  # Should produce annotations (CV is the stability metric for cs_diag)
  # At least one of stable or adaptive annotations should exist given clear separation
  has_annotation <- any(grepl("Stable high-performers|Responsive", ann_texts))
  expect_true(has_annotation)
})

test_that("annotate_stable_and_adaptive works with fa model", {
  designations <- paste0("G", 1:10)
  # G1-G3 near origin (low distance), G8-G10 far from origin (high distance)
  pc1 <- c(0.1, 0.05, -0.1, 1.0, 1.2, -1.0, -1.5, 2.0, 2.5, -3.0)
  pc2 <- c(0.05, -0.1, 0.1, 0.5, -0.5, 1.0, -1.0, 1.5, -2.0, 2.5)
  mean_values <- c(8, 7.5, 7, 6, 5.5, 5, 4.5, 6.5, 7, 6)
  statuses <- c("CHECK", "CHECK", rep("SELECTED", 8))

  pd <- make_fa_plot_data(
    designations = designations,
    mean_values = mean_values,
    pc1 = pc1,
    pc2 = pc2,
    statuses = statuses
  )

  p <- make_test_plotly(pd, x_col = "PC1", y_col = "PC2")
  result <- annotate_stable_and_adaptive(p, pd, "fa")
  built <- plotly::plotly_build(result)

  # FA metric = sqrt(PC1^2 + PC2^2); G1-G3 have smallest distance
  annotations <- built$x$layout$annotations
  ann_texts <- vapply(annotations %||% list(), function(a) a$text %||% "", character(1))

  has_annotation <- any(grepl("Stable high-performers|Responsive", ann_texts))
  expect_true(has_annotation)
})

test_that("annotate_stable_and_adaptive includes best environments for adaptive", {
  designations <- paste0("G", 1:8)
  # G7, G8 will be adaptive (high slope, high mean)
  fw_slopes <- c(1.0, 1.0, 1.0, 1.0, 1.1, 1.2, 2.0, 2.5)
  mean_values <- c(5, 5.5, 6, 6.5, 7, 7.5, 8, 9)
  statuses <- rep("SELECTED", 8)

  pd <- make_fw_plot_data(
    designations = designations,
    mean_values = mean_values,
    fw_slopes = fw_slopes,
    statuses = statuses
  )

  # env_predictions: G8 excels in Env_A (high value)
  env_predictions <- data.frame(
    designation = rep("G8", 4),
    environment = c("Env_A", "Env_B", "Env_C", "Env_D"),
    predictedValue = c(15, 5, 5, 5),  # mean=7.5, sd~4.33, threshold~11.83; Env_A > threshold
    stringsAsFactors = FALSE
  )

  p <- make_test_plotly(pd)
  result <- annotate_stable_and_adaptive(p, pd, "fw", env_predictions = env_predictions)
  built <- plotly::plotly_build(result)

  annotations <- built$x$layout$annotations
  ann_texts <- vapply(annotations %||% list(), function(a) a$text %||% "", character(1))

  adaptive_ann <- ann_texts[grepl("Responsive", ann_texts)]
  expect_true(length(adaptive_ann) > 0)
  # G8's best environment should be Env_A
  expect_true(any(grepl("Env_A", adaptive_ann)))
})

test_that("annotate_stable_and_adaptive handles NULL env_predictions gracefully", {
  designations <- paste0("G", 1:8)
  fw_slopes <- c(1.0, 1.0, 1.0, 1.0, 1.1, 1.2, 2.0, 2.5)
  mean_values <- c(5, 5.5, 6, 6.5, 7, 7.5, 8, 9)

  pd <- make_fw_plot_data(
    designations = designations,
    mean_values = mean_values,
    fw_slopes = fw_slopes
  )

  p <- make_test_plotly(pd)
  # No env_predictions → adaptive annotations without "best in"
  result <- annotate_stable_and_adaptive(p, pd, "fw", env_predictions = NULL)
  built <- plotly::plotly_build(result)

  annotations <- built$x$layout$annotations
  ann_texts <- vapply(annotations %||% list(), function(a) a$text %||% "", character(1))

  adaptive_ann <- ann_texts[grepl("Responsive", ann_texts)]
  # Should still show responsive annotation but without "best in"
  if (length(adaptive_ann) > 0) {
    expect_false(any(grepl("best in", adaptive_ann)))
  }
})

test_that("annotate_stable_and_adaptive returns unchanged plot when 0 qualify for both", {
  # All metrics are spread but means are all low → nobody qualifies for stable
  # and metrics are low → nobody qualifies for adaptive
  # This is tricky — let's make a case where metrics are in a narrow band
  # but > 1e-9 so it doesn't trigger the degenerate skip
  designations <- paste0("G", 1:8)
  # Slopes all similar (but not identical within 1e-9)
  fw_slopes <- c(1.001, 1.002, 1.003, 1.004, 1.005, 1.006, 1.007, 1.008)
  # All means below the 75th pctile threshold → none will qualify as stable
  # (since no CHECK, threshold = 75th pctile of means)
  mean_values <- rep(5, 8)  # All identical means → all below 75th pctile of itself

  pd <- make_fw_plot_data(
    designations = designations,
    mean_values = mean_values,
    fw_slopes = fw_slopes
  )

  p <- make_test_plotly(pd)
  result <- annotate_stable_and_adaptive(p, pd, "fw")
  built <- plotly::plotly_build(result)

  annotations <- built$x$layout$annotations
  ann_texts <- vapply(annotations %||% list(), function(a) a$text %||% "", character(1))

  # No stable or adaptive annotations should be present
  expect_false(any(grepl("Stable high-performers", ann_texts)))
  # When all means are identical, median == mean, so mean > median is FALSE
  expect_false(any(grepl("Responsive", ann_texts)))
})

test_that("annotate_stable_and_adaptive applies gold border to stable markers", {
  # Create data where G1, G2 should be stable
  designations <- paste0("G", 1:10)
  fw_slopes <- c(1.0, 1.0, 1.5, 1.6, 1.7, 1.8, 0.2, 0.1, 1.9, 2.0)
  mean_values <- c(10, 9, 5, 5, 5, 5, 5, 5, 5, 5)
  statuses <- rep("SELECTED", 10)

  pd <- make_fw_plot_data(
    designations = designations,
    mean_values = mean_values,
    fw_slopes = fw_slopes,
    statuses = statuses
  )

  p <- make_test_plotly(pd)
  result <- annotate_stable_and_adaptive(p, pd, "fw")

  # Result is already built (plotly_build returns the built structure)
  # Check that some traces have gold border
  found_gold <- FALSE
  for (trace in result$x$data) {
    if (!is.null(trace$marker$line$color)) {
      colors <- trace$marker$line$color
      if (any(colors == "#FFD700")) {
        found_gold <- TRUE
        break
      }
    }
  }
  expect_true(found_gold)
})

test_that("annotate_stable_and_adaptive applies 14px size to adaptive markers", {
  designations <- paste0("G", 1:10)
  fw_slopes <- c(1.0, 1.0, 1.0, 1.0, 1.0, 1.5, 1.7, 1.9, 2.0, 2.2)
  mean_values <- c(3, 3, 3, 3, 3, 6, 7, 8, 9, 10)

  pd <- make_fw_plot_data(
    designations = designations,
    mean_values = mean_values,
    fw_slopes = fw_slopes
  )

  p <- make_test_plotly(pd)
  result <- annotate_stable_and_adaptive(p, pd, "fw")

  # Check that some traces have size 14

  found_14 <- FALSE
  for (trace in result$x$data) {
    if (!is.null(trace$marker$size)) {
      sizes <- trace$marker$size
      if (any(sizes == 14)) {
        found_14 <- TRUE
        break
      }
    }
  }
  expect_true(found_14)
})

test_that("annotate_stable_and_adaptive handles less than 2 finite metrics", {
  # Only 1 genotype with finite metric
  pd <- make_fw_plot_data(
    designations = "G1",
    mean_values = 5,
    fw_slopes = 1.0
  )

  p <- make_test_plotly(pd)
  result <- annotate_stable_and_adaptive(p, pd, "fw")

  # Should return without adding annotations (less than 2 finite metrics)
  expect_true(inherits(result, "plotly") || inherits(result, "htmlwidget"))
})

# Source the module file to get function definitions
source(file.path(
  testthat::test_path(), "..", "..", "R", "mod_preProdAdvApp.R"
), local = TRUE)

# Helper to create minimal FW data (as produced by extract_fw_data)
make_fw_data <- function(designations = paste0("G", 1:10),
                         mean_values = NULL,
                         fw_slopes = NULL,
                         statuses = NULL) {
  n <- length(designations)
  if (is.null(mean_values)) mean_values <- seq(80, 120, length.out = n)
  if (is.null(fw_slopes)) fw_slopes <- seq(0.7, 1.5, length.out = n)
  if (is.null(statuses)) {
    statuses <- rep(c("SELECTED", "NOT SELECTED", "CHECK"), length.out = n)
  }

  status_colors <- c(
    "SELECTED"     = "#0072B2",
    "NOT SELECTED" = "#D55E00",
    "CHECK"        = "#C2185B"
  )

  df <- data.frame(
    designation = designations,
    mean_value = mean_values,
    fw_slope = fw_slopes,
    plot_status = statuses,
    stringsAsFactors = FALSE
  )
  df$opacity <- ifelse(df$plot_status == "CHECK", 1.0, 0.6)
  df$color <- status_colors[df$plot_status]
  df$shape <- "circle"

  df
}

test_that("build_fw_reaction_norm_plotly returns a valid plotly object", {
  fw_data <- make_fw_data()

  p <- build_fw_reaction_norm_plotly(
    fw_data = fw_data,
    highlighted = NULL,
    source_id = "test_fw"
  )

  expect_true(inherits(p, "plotly") || inherits(p, "htmlwidget"))
})

test_that("build_fw_reaction_norm_plotly creates scatter traces per status", {
  fw_data <- make_fw_data()

  p <- build_fw_reaction_norm_plotly(
    fw_data = fw_data,
    highlighted = NULL,
    source_id = "test_fw"
  )

  built <- plotly::plotly_build(p)

  # Should have at least scatter traces for the status categories present

  scatter_traces <- Filter(function(tr) {
    isTRUE(tr$type == "scatter") && isTRUE(tr$mode == "markers")
  }, built$x$data)

  # We have 3 statuses in our test data
  expect_gte(length(scatter_traces), 3)
})

test_that("build_fw_reaction_norm_plotly includes reference lines in shapes", {
  fw_data <- make_fw_data()

  p <- build_fw_reaction_norm_plotly(
    fw_data = fw_data,
    highlighted = NULL,
    source_id = "test_fw"
  )

  built <- plotly::plotly_build(p)
  shapes <- built$x$layout$shapes

  # Should have at least 3 shapes: hline, vline, and stable zone rect
  expect_gte(length(shapes), 3)

  # Find the horizontal line at y=1.0
  hline <- Filter(function(s) s$type == "line" && s$y0 == 1.0 && s$y1 == 1.0, shapes)
  expect_length(hline, 1)

  # Find the stable zone rectangle
  rects <- Filter(function(s) s$type == "rect", shapes)
  expect_gte(length(rects), 1)
  # Stable zone should have y0 = 0.8 and y1 = 1.2
  stable_rect <- Filter(function(s) s$y0 == 0.8 && s$y1 == 1.2, rects)
  expect_length(stable_rect, 1)
})

test_that("build_fw_reaction_norm_plotly uses Check_Mean for vertical line when CHECKs exist", {
  fw_data <- make_fw_data(
    designations = c("G1", "G2", "G3", "G4"),
    mean_values = c(80, 90, 100, 110),
    fw_slopes = c(0.9, 1.0, 1.1, 1.2),
    statuses = c("SELECTED", "CHECK", "SELECTED", "NOT SELECTED")
  )

  p <- build_fw_reaction_norm_plotly(
    fw_data = fw_data,
    highlighted = NULL,
    source_id = "test_fw"
  )

  built <- plotly::plotly_build(p)
  shapes <- built$x$layout$shapes

  # Check_Mean = mean of CHECK genotypes' mean_value = 90
  vlines <- Filter(function(s) s$type == "line" && s$x0 == s$x1, shapes)
  expect_length(vlines, 1)
  expect_equal(vlines[[1]]$x0, 90)
})

test_that("build_fw_reaction_norm_plotly uses 75th percentile fallback for vline when no CHECKs", {
  fw_data <- make_fw_data(
    designations = c("G1", "G2", "G3", "G4"),
    mean_values = c(80, 90, 100, 110),
    fw_slopes = c(0.9, 1.0, 1.1, 1.2),
    statuses = c("SELECTED", "NOT SELECTED", "SELECTED", "NOT SELECTED")
  )

  p <- build_fw_reaction_norm_plotly(
    fw_data = fw_data,
    highlighted = NULL,
    source_id = "test_fw"
  )

  built <- plotly::plotly_build(p)
  shapes <- built$x$layout$shapes

  # 75th percentile of c(80, 90, 100, 110) = 102.5
  expected_ref <- quantile(c(80, 90, 100, 110), 0.75, names = FALSE)
  vlines <- Filter(function(s) s$type == "line" && s$x0 == s$x1, shapes)
  expect_length(vlines, 1)
  expect_equal(vlines[[1]]$x0, expected_ref)
})

test_that("build_fw_reaction_norm_plotly applies highlighting correctly", {
  fw_data <- make_fw_data(
    designations = c("G1", "G2", "G3"),
    mean_values = c(80, 90, 100),
    fw_slopes = c(0.9, 1.0, 1.1),
    statuses = c("SELECTED", "SELECTED", "SELECTED")
  )

  p <- build_fw_reaction_norm_plotly(
    fw_data = fw_data,
    highlighted = "G2",
    source_id = "test_fw"
  )

  built <- plotly::plotly_build(p)

  # Find the scatter trace (should be 1 since all SELECTED)
  scatter_traces <- Filter(function(tr) {
    isTRUE(tr$type == "scatter") && isTRUE(tr$mode == "markers")
  }, built$x$data)

  expect_gte(length(scatter_traces), 1)

  # The highlighted point (G2) should have size 14
  trace <- scatter_traces[[1]]
  sizes <- trace$marker$size
  expect_true(14 %in% sizes)

  # White border on highlighted
  line_colors <- trace$marker$line$color
  expect_true("white" %in% line_colors)
})

test_that("build_fw_reaction_norm_plotly includes customdata for click events", {
  fw_data <- make_fw_data(
    designations = c("G1", "G2", "G3"),
    mean_values = c(80, 90, 100),
    fw_slopes = c(0.9, 1.0, 1.1),
    statuses = c("SELECTED", "SELECTED", "SELECTED")
  )

  p <- build_fw_reaction_norm_plotly(
    fw_data = fw_data,
    highlighted = NULL,
    source_id = "test_fw"
  )

  built <- plotly::plotly_build(p)

  # Find scatter traces and check customdata
  scatter_traces <- Filter(function(tr) {
    isTRUE(tr$type == "scatter") && isTRUE(tr$mode == "markers")
  }, built$x$data)

  # customdata should be designation values
  all_customdata <- unlist(lapply(scatter_traces, function(tr) tr$customdata))
  expect_true("G1" %in% all_customdata)
  expect_true("G2" %in% all_customdata)
  expect_true("G3" %in% all_customdata)
})

test_that("build_fw_reaction_norm_plotly calls annotate_stable_and_adaptive", {
  # Create data with clear stable/adaptive separation
  fw_data <- make_fw_data(
    designations = paste0("G", 1:10),
    mean_values = c(110, 115, 120, 100, 95, 90, 85, 80, 75, 70),
    fw_slopes = c(1.0, 1.0, 1.0, 1.2, 1.3, 1.5, 1.7, 2.0, 2.2, 2.5),
    statuses = c(rep("CHECK", 3), rep("SELECTED", 4), rep("NOT SELECTED", 3))
  )

  p <- build_fw_reaction_norm_plotly(
    fw_data = fw_data,
    highlighted = NULL,
    source_id = "test_fw"
  )

  built <- plotly::plotly_build(p)

  # The function result should be a built plotly (from annotate_stable_and_adaptive)
  expect_true(!is.null(built$x$layout))
  # If annotations are added by annotate_stable_and_adaptive, they'll be in layout
  # This is a basic smoke test that the function completes without error
  expect_true(!is.null(built$x))
})

test_that("build_fw_reaction_norm_plotly sets correct axis labels", {
  fw_data <- make_fw_data()

  p <- build_fw_reaction_norm_plotly(
    fw_data = fw_data,
    highlighted = NULL,
    source_id = "test_fw"
  )

  built <- plotly::plotly_build(p)

  expect_equal(built$x$layout$xaxis$title, "Mean Performance")
  expect_equal(built$x$layout$yaxis$title, "FW Regression Slope")
})

test_that("build_fw_reaction_norm_plotly dims non-highlighted markers", {
  fw_data <- make_fw_data(
    designations = c("G1", "G2", "G3"),
    mean_values = c(80, 90, 100),
    fw_slopes = c(0.9, 1.0, 1.1),
    statuses = c("SELECTED", "SELECTED", "SELECTED")
  )

  p <- build_fw_reaction_norm_plotly(
    fw_data = fw_data,
    highlighted = "G1",
    source_id = "test_fw"
  )

  built <- plotly::plotly_build(p)

  scatter_traces <- Filter(function(tr) {
    isTRUE(tr$type == "scatter") && isTRUE(tr$mode == "markers")
  }, built$x$data)

  trace <- scatter_traces[[1]]
  opacities <- trace$marker$opacity

  # Non-highlighted markers should have reduced opacity (original * 0.3)
  # G1 is highlighted (opacity 0.6), G2 and G3 are not (0.6 * 0.3 = 0.18)
  expect_true(any(opacities < 0.3))  # dimmed markers exist
  expect_true(any(opacities >= 0.5)) # highlighted marker is brighter
})

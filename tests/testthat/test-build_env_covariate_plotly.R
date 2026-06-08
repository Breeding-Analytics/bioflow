# Source the module file to get function definitions
source(file.path(
  testthat::test_path(), "..", "..", "R", "mod_preProdAdvApp.R"
), local = TRUE)

# Helper to create an env covariate data frame
make_env_df <- function(environments = c("ENV_A", "ENV_B", "ENV_C"),
                        values = NULL) {
  if (is.null(values)) {
    values <- seq_along(environments) * 10 + runif(length(environments), -1, 1)
  }

  data.frame(
    environment = environments,
    covariate_value = values,
    stringsAsFactors = FALSE
  )
}

test_that("build_env_covariate_plotly returns a valid plotly object", {
  env_df <- make_env_df()
  environments_order <- c("ENV_A", "ENV_B", "ENV_C")

  p <- build_env_covariate_plotly(
    env_df = env_df,
    covariate_label = "Mean Temperature",
    environments_order = environments_order
  )

  expect_s3_class(p, "plotly")
  expect_true("x" %in% names(p))
})

test_that("build_env_covariate_plotly creates bar trace", {
  env_df <- make_env_df()
  environments_order <- c("ENV_A", "ENV_B", "ENV_C")

  p <- build_env_covariate_plotly(
    env_df = env_df,
    covariate_label = "Mean Temperature",
    environments_order = environments_order
  )

  built <- plotly::plotly_build(p)
  # Should have exactly one bar trace
  expect_equal(length(built$x$data), 1)
  expect_equal(built$x$data[[1]]$type, "bar")
})

test_that("build_env_covariate_plotly x-axis matches environments_order", {
  env_df <- make_env_df(environments = c("ENV_C", "ENV_A", "ENV_B"),
                        values = c(30, 10, 20))
  environments_order <- c("ENV_A", "ENV_B", "ENV_C")

  p <- build_env_covariate_plotly(
    env_df = env_df,
    covariate_label = "Rainfall",
    environments_order = environments_order
  )

  built <- plotly::plotly_build(p)
  layout <- built$x$layout

  # tickvals should be seq_along(environments_order) = 1, 2, 3

  expect_equal(layout$xaxis$tickvals, c(1, 2, 3))
  # ticktext should match environments_order
  expect_equal(layout$xaxis$ticktext, c("ENV_A", "ENV_B", "ENV_C"))
})

test_that("build_env_covariate_plotly uses numeric x positions", {
  env_df <- make_env_df(environments = c("ENV_A", "ENV_B", "ENV_C"),
                        values = c(10, 20, 30))
  environments_order <- c("ENV_A", "ENV_B", "ENV_C")

  p <- build_env_covariate_plotly(
    env_df = env_df,
    covariate_label = "Temperature",
    environments_order = environments_order
  )

  built <- plotly::plotly_build(p)
  trace <- built$x$data[[1]]

  # x values should be numeric positions 1, 2, 3
  x_vals <- unlist(trace$x)
  expect_true(all(x_vals %in% c(1, 2, 3)))
})

test_that("build_env_covariate_plotly uses steel grey color for bars", {
  env_df <- make_env_df()
  environments_order <- c("ENV_A", "ENV_B", "ENV_C")

  p <- build_env_covariate_plotly(
    env_df = env_df,
    covariate_label = "Temperature",
    environments_order = environments_order
  )

  built <- plotly::plotly_build(p)
  trace <- built$x$data[[1]]

  expect_equal(trace$marker$color, "#7B8794")
})

test_that("build_env_covariate_plotly shows annotation when env_df is empty", {
  env_df <- data.frame(
    environment = character(0),
    covariate_value = numeric(0),
    stringsAsFactors = FALSE
  )
  environments_order <- c("ENV_A", "ENV_B", "ENV_C")

  p <- build_env_covariate_plotly(
    env_df = env_df,
    covariate_label = "Temperature",
    environments_order = environments_order
  )

  expect_s3_class(p, "plotly")

  built <- plotly::plotly_build(p)
  annotations <- built$x$layout$annotations

  # Should have the fallback annotation
  expect_true(length(annotations) >= 1)
  annotation_text <- annotations[[1]]$text
  expect_equal(
    annotation_text,
    "Environmental covariate data not available for these trials"
  )
})

test_that("build_env_covariate_plotly shows annotation when env_df is NULL", {
  environments_order <- c("ENV_A", "ENV_B", "ENV_C")

  p <- build_env_covariate_plotly(
    env_df = NULL,
    covariate_label = "Rainfall",
    environments_order = environments_order
  )

  expect_s3_class(p, "plotly")

  built <- plotly::plotly_build(p)
  annotations <- built$x$layout$annotations

  expect_true(length(annotations) >= 1)
  expect_equal(
    annotations[[1]]$text,
    "Environmental covariate data not available for these trials"
  )
})

test_that("build_env_covariate_plotly y-axis label uses covariate_label", {
  env_df <- make_env_df()
  environments_order <- c("ENV_A", "ENV_B", "ENV_C")

  p <- build_env_covariate_plotly(
    env_df = env_df,
    covariate_label = "Rainfall (mm)",
    environments_order = environments_order
  )

  built <- plotly::plotly_build(p)
  expect_equal(built$x$layout$yaxis$title, "Rainfall (mm)")
})

test_that("build_env_covariate_plotly excludes environments not in environments_order", {
  # env_df has an environment not in environments_order
  env_df <- make_env_df(environments = c("ENV_A", "ENV_B", "ENV_X"),
                        values = c(10, 20, 30))
  environments_order <- c("ENV_A", "ENV_B", "ENV_C")

  p <- build_env_covariate_plotly(
    env_df = env_df,
    covariate_label = "Temperature",
    environments_order = environments_order
  )

  built <- plotly::plotly_build(p)
  trace <- built$x$data[[1]]

  # Only ENV_A and ENV_B should be plotted (ENV_X not in order, ENV_C not in data)
  x_vals <- unlist(trace$x)
  expect_equal(length(x_vals), 2)
  # Positions should be 1 (ENV_A) and 2 (ENV_B)
  expect_equal(sort(x_vals), c(1, 2))
})

test_that("build_env_covariate_plotly has tooltip with environment and value", {
  env_df <- make_env_df(environments = c("ENV_A"),
                        values = c(25.5))
  environments_order <- c("ENV_A", "ENV_B")

  p <- build_env_covariate_plotly(
    env_df = env_df,
    covariate_label = "Temperature",
    environments_order = environments_order
  )

  built <- plotly::plotly_build(p)
  trace <- built$x$data[[1]]

  # Tooltip text should contain environment name and value
  hover <- trace$text
  expect_true(grepl("ENV_A", hover))
  expect_true(grepl("25.5", hover))
  expect_true(grepl("Temperature", hover))
})


# --- Tests for cluster_assignments parameter (Task 3.3) ---

test_that("build_env_covariate_plotly colors bars by cluster when cluster_assignments provided", {
  env_df <- make_env_df(environments = c("ENV_A", "ENV_B", "ENV_C"),
                        values = c(10, 20, 30))
  environments_order <- c("ENV_A", "ENV_B", "ENV_C")
  cluster_assignments <- c("ENV_A" = "High rainfall", "ENV_B" = "Low rainfall", "ENV_C" = "High rainfall")

  p <- build_env_covariate_plotly(
    env_df = env_df,
    covariate_label = "Rainfall",
    environments_order = environments_order,
    cluster_assignments = cluster_assignments
  )

  expect_s3_class(p, "plotly")
  built <- plotly::plotly_build(p)

  # Should have multiple traces (one per cluster)
  expect_true(length(built$x$data) >= 2)

  # All traces should be bar type
  trace_types <- vapply(built$x$data, function(t) t$type, character(1))
  expect_true(all(trace_types == "bar"))
})

test_that("build_env_covariate_plotly uses distinct colors per cluster", {
  env_df <- make_env_df(environments = c("ENV_A", "ENV_B", "ENV_C", "ENV_D"),
                        values = c(10, 20, 30, 40))
  environments_order <- c("ENV_A", "ENV_B", "ENV_C", "ENV_D")
  cluster_assignments <- c("ENV_A" = "Cluster 1", "ENV_B" = "Cluster 2",
                           "ENV_C" = "Cluster 1", "ENV_D" = "Cluster 2")

  p <- build_env_covariate_plotly(
    env_df = env_df,
    covariate_label = "Temperature",
    environments_order = environments_order,
    cluster_assignments = cluster_assignments
  )

  built <- plotly::plotly_build(p)
  # Extract colors from traces
  colors <- vapply(built$x$data, function(t) t$marker$color, character(1))
  # Should have exactly 2 unique colors (one per cluster)
  expect_equal(length(unique(colors)), 2)
  # Colors should be different
  expect_false(colors[1] == colors[2])
})

test_that("build_env_covariate_plotly shows legend when cluster_assignments provided", {
  env_df <- make_env_df(environments = c("ENV_A", "ENV_B", "ENV_C"),
                        values = c(10, 20, 30))
  environments_order <- c("ENV_A", "ENV_B", "ENV_C")
  cluster_assignments <- c("ENV_A" = "High temp", "ENV_B" = "Low temp", "ENV_C" = "High temp")

  p <- build_env_covariate_plotly(
    env_df = env_df,
    covariate_label = "Temperature",
    environments_order = environments_order,
    cluster_assignments = cluster_assignments
  )

  built <- plotly::plotly_build(p)
  # Each trace should have showlegend = TRUE
  for (trace in built$x$data) {
    expect_true(trace$showlegend)
  }
})

test_that("build_env_covariate_plotly trace names match cluster labels", {
  env_df <- make_env_df(environments = c("ENV_A", "ENV_B", "ENV_C"),
                        values = c(10, 20, 30))
  environments_order <- c("ENV_A", "ENV_B", "ENV_C")
  cluster_assignments <- c("ENV_A" = "High rainfall", "ENV_B" = "Low rainfall", "ENV_C" = "High rainfall")

  p <- build_env_covariate_plotly(
    env_df = env_df,
    covariate_label = "Rainfall",
    environments_order = environments_order,
    cluster_assignments = cluster_assignments
  )

  built <- plotly::plotly_build(p)
  trace_names <- vapply(built$x$data, function(t) t$name, character(1))
  # Trace names should be cluster labels

  expect_true("High rainfall" %in% trace_names)
  expect_true("Low rainfall" %in% trace_names)
})

test_that("build_env_covariate_plotly retains single-color when cluster_assignments is NULL", {
  env_df <- make_env_df()
  environments_order <- c("ENV_A", "ENV_B", "ENV_C")

  p <- build_env_covariate_plotly(
    env_df = env_df,
    covariate_label = "Temperature",
    environments_order = environments_order,
    cluster_assignments = NULL
  )

  built <- plotly::plotly_build(p)
  # Should have one trace with steel grey color
  expect_equal(length(built$x$data), 1)
  expect_equal(built$x$data[[1]]$marker$color, "#7B8794")
})

test_that("build_env_covariate_plotly backward compatible without cluster_assignments arg", {
  env_df <- make_env_df()
  environments_order <- c("ENV_A", "ENV_B", "ENV_C")

  # Calling without cluster_assignments should work (default NULL)
  p <- build_env_covariate_plotly(
    env_df = env_df,
    covariate_label = "Temperature",
    environments_order = environments_order
  )

  expect_s3_class(p, "plotly")
  built <- plotly::plotly_build(p)
  expect_equal(length(built$x$data), 1)
  expect_equal(built$x$data[[1]]$marker$color, "#7B8794")
})

test_that("build_env_covariate_plotly annotation still appears for NULL env_df with cluster_assignments", {
  environments_order <- c("ENV_A", "ENV_B", "ENV_C")
  cluster_assignments <- c("ENV_A" = "Cluster A", "ENV_B" = "Cluster B", "ENV_C" = "Cluster A")

  p <- build_env_covariate_plotly(
    env_df = NULL,
    covariate_label = "Rainfall",
    environments_order = environments_order,
    cluster_assignments = cluster_assignments
  )

  expect_s3_class(p, "plotly")
  built <- plotly::plotly_build(p)
  annotations <- built$x$layout$annotations
  expect_true(length(annotations) >= 1)
  expect_equal(
    annotations[[1]]$text,
    "Environmental covariate data not available for these trials"
  )
})

test_that("build_env_covariate_plotly handles 3 clusters correctly", {
  env_df <- make_env_df(environments = c("ENV_A", "ENV_B", "ENV_C", "ENV_D", "ENV_E", "ENV_F"),
                        values = c(10, 20, 30, 40, 50, 60))
  environments_order <- c("ENV_A", "ENV_B", "ENV_C", "ENV_D", "ENV_E", "ENV_F")
  cluster_assignments <- c("ENV_A" = "Hot", "ENV_B" = "Wet", "ENV_C" = "Cold",
                           "ENV_D" = "Hot", "ENV_E" = "Wet", "ENV_F" = "Cold")

  p <- build_env_covariate_plotly(
    env_df = env_df,
    covariate_label = "Temperature",
    environments_order = environments_order,
    cluster_assignments = cluster_assignments
  )

  built <- plotly::plotly_build(p)
  # Should have 3 traces (one per cluster)
  expect_equal(length(built$x$data), 3)
  # All colors should be distinct
  colors <- vapply(built$x$data, function(t) t$marker$color, character(1))
  expect_equal(length(unique(colors)), 3)
})

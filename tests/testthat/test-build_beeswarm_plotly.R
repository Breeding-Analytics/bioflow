# Source the module file to get function definitions
source(file.path(
  testthat::test_path(), "..", "..", "R", "mod_preProdAdvApp.R"
), local = TRUE)

# Helper to create a minimal beeswarm data frame (as produced by prepare_beeswarm_data)
make_beeswarm_df <- function(designations = c("G1", "G2", "G3"),
                             environments = c("ENV_A", "ENV_B"),
                             statuses = NULL,
                             reliabilities = NULL) {
  combos <- expand.grid(
    designation = designations,
    environment = environments,
    stringsAsFactors = FALSE
  )
  n <- nrow(combos)
  combos$predictedValue <- seq_len(n) + runif(n, -0.1, 0.1)
  combos$reliability <- if (is.null(reliabilities)) rep(0.5, n) else reliabilities

  # Assign statuses cyclically if not provided

  if (is.null(statuses)) {
    status_cycle <- c("SELECTED", "NOT SELECTED", "CHECK")
    combos$plot_status <- status_cycle[match(combos$designation, designations)]
  } else {
    combos$plot_status <- statuses[match(combos$designation, designations)]
  }

  status_colors <- c(
    "SELECTED"     = "#0072B2",
    "NOT SELECTED" = "#D55E00",
    "CHECK"        = "#C2185B"
  )
  combos$opacity <- reliability_to_opacity(combos$reliability)
  combos$opacity[combos$plot_status == "CHECK"] <- 1.0
  combos$color <- status_colors[combos$plot_status]
  combos$environment <- factor(combos$environment, levels = sort(unique(combos$environment)))

  combos[, c("designation", "environment", "predictedValue", "reliability",
             "plot_status", "opacity", "color"), drop = FALSE]
}

test_that("build_beeswarm_plotly returns a valid plotly object", {
  df <- make_beeswarm_df()

  p <- build_beeswarm_plotly(
    df = df,
    trait_name = "Yield",
    highlighted = NULL,
    connect_dots = FALSE,
    source_id = "test_source"
  )

  expect_s3_class(p, "plotly")
  # Should have data element

  expect_true("x" %in% names(p))
})

test_that("build_beeswarm_plotly creates one trace per status category present", {
  df <- make_beeswarm_df()  # 3 statuses -> 3 traces

  p <- build_beeswarm_plotly(
    df = df,
    trait_name = "Yield",
    highlighted = NULL,
    connect_dots = FALSE,
    source_id = "test_source"
  )

  # Extract plotly trace count from the plot data
  built <- plotly::plotly_build(p)
  trace_count <- length(built$x$data)

  # Should have exactly 3 traces (one per status)
  expect_equal(trace_count, 3)
})

test_that("build_beeswarm_plotly connect-dots adds a line trace", {
  df <- make_beeswarm_df(
    designations = c("G1", "G2"),
    environments = c("ENV_A", "ENV_B", "ENV_C")
  )

  # Without connect-dots
  p_no_connect <- build_beeswarm_plotly(
    df = df,
    trait_name = "Yield",
    highlighted = "G1",
    connect_dots = FALSE,
    source_id = "test_source"
  )
  built_no_connect <- plotly::plotly_build(p_no_connect)
  traces_no_connect <- length(built_no_connect$x$data)

  # With connect-dots
  p_connect <- build_beeswarm_plotly(
    df = df,
    trait_name = "Yield",
    highlighted = "G1",
    connect_dots = TRUE,
    source_id = "test_source"
  )
  built_connect <- plotly::plotly_build(p_connect)
  traces_connect <- length(built_connect$x$data)

  # Connect-dots should add exactly one extra trace (lines)
  expect_equal(traces_connect, traces_no_connect + 1)

  # The extra trace should be a lines trace
  last_trace <- built_connect$x$data[[traces_connect]]
  expect_equal(last_trace$mode, "lines")
})

test_that("build_beeswarm_plotly highlighted designation gets larger marker size", {
  df <- make_beeswarm_df(
    designations = c("G1", "G2"),
    environments = c("ENV_A", "ENV_B")
  )

  p <- build_beeswarm_plotly(
    df = df,
    trait_name = "Yield",
    highlighted = "G1",
    connect_dots = FALSE,
    source_id = "test_source"
  )

  built <- plotly::plotly_build(p)

  # Find trace containing G1 (SELECTED status -> first trace)
  # Check that G1's marker sizes are 12
  found_highlighted <- FALSE
  for (trace in built$x$data) {
    if (!is.null(trace$marker) && !is.null(trace$marker$size)) {
      sizes <- trace$marker$size
      if (any(sizes == 12)) {
        found_highlighted <- TRUE
        break
      }
    }
  }
  expect_true(found_highlighted)
})

test_that("build_beeswarm_plotly without highlight uses uniform marker size", {
  df <- make_beeswarm_df(
    designations = c("G1", "G2"),
    environments = c("ENV_A", "ENV_B")
  )

  p <- build_beeswarm_plotly(
    df = df,
    trait_name = "Yield",
    highlighted = NULL,
    connect_dots = FALSE,
    source_id = "test_source"
  )

  built <- plotly::plotly_build(p)

  # All markers should be size 7
  for (trace in built$x$data) {
    if (!is.null(trace$marker) && !is.null(trace$marker$size)) {
      sizes <- trace$marker$size
      expect_true(all(sizes == 7))
    }
  }
})

test_that("build_beeswarm_plotly connect-dots has correct number of points", {
  df <- make_beeswarm_df(
    designations = c("G1", "G2"),
    environments = c("ENV_A", "ENV_B", "ENV_C")
  )

  p <- build_beeswarm_plotly(
    df = df,
    trait_name = "Yield",
    highlighted = "G1",
    connect_dots = TRUE,
    source_id = "test_source"
  )

  built <- plotly::plotly_build(p)
  traces <- built$x$data

  # Find the line trace (last trace)
  line_trace <- traces[[length(traces)]]
  expect_equal(line_trace$mode, "lines")

  # G1 appears in 3 environments, so line should have 3 points
  expect_equal(length(line_trace$x), 3)
  expect_equal(length(line_trace$y), 3)
})

test_that("build_beeswarm_plotly uses source_id for click events", {
  df <- make_beeswarm_df()

  p <- build_beeswarm_plotly(
    df = df,
    trait_name = "Yield",
    highlighted = NULL,
    connect_dots = FALSE,
    source_id = "my_beeswarm_source"
  )

  # The source should be set in the plotly object
  expect_equal(p$x$source, "my_beeswarm_source")
})

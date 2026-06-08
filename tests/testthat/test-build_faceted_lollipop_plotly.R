# Source the module file to get function definitions
source(file.path(
  testthat::test_path(), "..", "..", "R", "mod_preProdAdvApp.R"
), local = TRUE)

# Helper to create prepared_data as produced by prepare_lollipop_data()
make_prepared_data <- function(n_designations = 6,
                               clusters = c("High rainfall", "Low rainfall")) {
  desigs <- paste0("G", seq_len(n_designations))
  rows <- expand.grid(
    designation = desigs,
    cluster = clusters,
    stringsAsFactors = FALSE
  )
  n <- nrow(rows)
  set.seed(42)
  rows$mean_value <- runif(n, 2, 10)
  rows$mean_reliability <- runif(n, 0.3, 0.8)

  statuses <- c("SELECTED", "NOT SELECTED", "CHECK", "REVISE")
  rows$plot_status <- statuses[(match(rows$designation, desigs) - 1) %% 4 + 1]
  rows$marker_shape <- STATUS_SHAPES[rows$plot_status]
  rows$marker_color <- STATUS_COLORS[rows$plot_status]

  # Rank within each cluster
  for (cl in clusters) {
    idx <- which(rows$cluster == cl)
    ranks <- rank(-rows$mean_value[idx], ties.method = "first")
    rows$rank_in_cluster[idx] <- ranks
  }
  rows$overall_rank <- rank(-rows$mean_value, ties.method = "first")
  rows$n_envs_in_cluster <- ifelse(rows$cluster == clusters[1], 3, 2)

  rows
}

# Helper to create thresholds as produced by compute_zone_thresholds()
make_thresholds <- function(prepared_data) {
  clusters <- unique(prepared_data$cluster)
  do.call(rbind, lapply(clusters, function(cl) {
    cl_data <- prepared_data[prepared_data$cluster == cl, ]
    pop_mean <- mean(cl_data$mean_value)
    pop_sd <- sd(cl_data$mean_value)
    check_rows <- cl_data[cl_data$plot_status == "CHECK", ]
    check_mean <- if (nrow(check_rows) > 0) mean(check_rows$mean_value) else pop_mean
    data.frame(
      cluster = cl,
      recommend_threshold = check_mean,
      avoid_threshold = pop_mean - pop_sd,
      check_mean = check_mean,
      pop_mean = pop_mean,
      pop_sd = pop_sd,
      stringsAsFactors = FALSE
    )
  }))
}

test_that("build_faceted_lollipop_plotly returns a valid plotly object", {
  pd <- make_prepared_data()
  th <- make_thresholds(pd)

  fig <- build_faceted_lollipop_plotly(
    prepared_data = pd,
    thresholds = th,
    highlighted = NULL,
    user_recommend_threshold = NULL,
    source_id = "test_lollipop"
  )

  expect_s3_class(fig, "plotly")
  expect_true("x" %in% names(fig))
})

test_that("build_faceted_lollipop_plotly creates traces for each status", {

  pd <- make_prepared_data()
  th <- make_thresholds(pd)

  fig <- build_faceted_lollipop_plotly(
    prepared_data = pd,
    thresholds = th,
    highlighted = NULL,
    user_recommend_threshold = NULL,
    source_id = "test_lollipop"
  )

  built <- plotly::plotly_build(fig)
  # We should have at least one trace per cluster (stems) plus marker traces

  # 2 clusters * (1 stem + up to 4 statuses) = at least 4 total traces
  trace_count <- length(built$x$data)
  expect_gte(trace_count, 4)
})

test_that("prescriptive header contains top picks format", {
  pd <- make_prepared_data()
  th <- make_thresholds(pd)

  fig <- build_faceted_lollipop_plotly(
    prepared_data = pd,
    thresholds = th,
    highlighted = NULL,
    user_recommend_threshold = NULL,
    source_id = "test_lollipop"
  )

  built <- plotly::plotly_build(fig)

  # Check annotations exist
  annotations <- built$x$layout$annotations
  expect_true(length(annotations) >= 2)  # One per cluster


  # At least one annotation should have "Top picks" or "No clear recommendations"
  annotation_texts <- vapply(annotations, function(a) a$text, character(1))
  has_prescriptive <- any(grepl("Top picks|No clear recommendations", annotation_texts))
  expect_true(has_prescriptive)
})

test_that("highlighting changes marker sizes", {
  pd <- make_prepared_data()
  th <- make_thresholds(pd)

  fig <- build_faceted_lollipop_plotly(
    prepared_data = pd,
    thresholds = th,
    highlighted = "G1",
    user_recommend_threshold = NULL,
    source_id = "test_lollipop"
  )

  built <- plotly::plotly_build(fig)

  # Look for a marker with size 14 (highlighted designation)
  found_size_14 <- FALSE
  for (trace in built$x$data) {
    if (!is.null(trace$marker) && !is.null(trace$marker$size)) {
      sizes <- trace$marker$size
      if (any(sizes == 14)) {
        found_size_14 <- TRUE
        break
      }
    }
  }
  expect_true(found_size_14)
})

test_that("recommendation zones are rendered as shapes", {
  pd <- make_prepared_data()
  th <- make_thresholds(pd)

  fig <- build_faceted_lollipop_plotly(
    prepared_data = pd,
    thresholds = th,
    highlighted = NULL,
    user_recommend_threshold = NULL,
    source_id = "test_lollipop"
  )

  # Check that shapes are present in the plotly object's layout attributes

  # The shapes are stored in layoutAttrs and rendered client-side
  # Extract shapes from the layout attributes (pre-build representation)
  all_shapes <- list()
  for (la in fig$x$layoutAttrs) {
    if (!is.null(la$shapes)) {
      all_shapes <- c(all_shapes, la$shapes)
    }
  }

  # Should have at least 4 shapes (2 per cluster: green + red)
  expect_gte(length(all_shapes), 4)

  # Verify colors
  fill_colors <- vapply(all_shapes, function(s) s$fillcolor, character(1))
  expect_true(any(grepl("76, 175, 80", fill_colors)))  # Green zone
  expect_true(any(grepl("244, 67, 54", fill_colors)))  # Red zone
})

test_that("user_recommend_threshold overrides default threshold", {
  pd <- make_prepared_data()
  th <- make_thresholds(pd)

  custom_thresh <- 8.0

  fig <- build_faceted_lollipop_plotly(
    prepared_data = pd,
    thresholds = th,
    highlighted = NULL,
    user_recommend_threshold = custom_thresh,
    source_id = "test_lollipop"
  )

  # Extract shapes from layout attributes
  all_shapes <- list()
  for (la in fig$x$layoutAttrs) {
    if (!is.null(la$shapes)) {
      all_shapes <- c(all_shapes, la$shapes)
    }
  }

  # All green zone shapes should start at custom_thresh
  green_shapes <- all_shapes[vapply(all_shapes, function(s) {
    grepl("76, 175, 80", s$fillcolor)
  }, logical(1))]

  expect_true(length(green_shapes) > 0)
  for (gs in green_shapes) {
    expect_equal(gs$x0, custom_thresh)
  }
})

test_that("single-environment cluster gets caution annotation", {
  pd <- make_prepared_data(n_designations = 4, clusters = c("Cluster A"))
  pd$n_envs_in_cluster <- 1
  th <- make_thresholds(pd)

  fig <- build_faceted_lollipop_plotly(
    prepared_data = pd,
    thresholds = th,
    highlighted = NULL,
    user_recommend_threshold = NULL,
    source_id = "test_lollipop"
  )

  built <- plotly::plotly_build(fig)
  annotations <- built$x$layout$annotations
  annotation_texts <- vapply(annotations, function(a) a$text, character(1))

  # Should contain caution text

  has_caution <- any(grepl("1 site.*interpret with caution", annotation_texts))
  expect_true(has_caution)
})

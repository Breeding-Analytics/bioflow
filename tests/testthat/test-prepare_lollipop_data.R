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

# Helper to create cluster_assignments named vector
make_clusters <- function(environments = c("ENV_A", "ENV_B"),
                          labels = c("High rainfall environments",
                                     "Low rainfall environments")) {
  stats::setNames(labels, environments)
}

test_that("prepare_lollipop_data returns correct columns", {
  sta <- make_sta_long()
  review <- make_review_df()
  clusters <- make_clusters()

  result <- prepare_lollipop_data(sta, review, "Yield", clusters)

  expected_cols <- c("designation", "cluster", "mean_value", "mean_reliability",
                     "plot_status", "marker_shape", "marker_color",
                     "rank_in_cluster", "overall_rank", "n_envs_in_cluster")
  expect_equal(colnames(result), expected_cols)
})

test_that("prepare_lollipop_data produces one row per (designation, cluster)", {
  sta <- make_sta_long(
    designations = c("G1", "G2", "G3"),
    environments = c("ENV_A", "ENV_B", "ENV_C", "ENV_D")
  )
  review <- make_review_df()
  clusters <- make_clusters(
    environments = c("ENV_A", "ENV_B", "ENV_C", "ENV_D"),
    labels = c("Cluster1", "Cluster1", "Cluster2", "Cluster2")
  )

  result <- prepare_lollipop_data(sta, review, "Yield", clusters)

  # 3 designations x 2 clusters = 6 rows
  expect_equal(nrow(result), 6)

  # No duplicate (designation, cluster) combos
  combos <- paste(result$designation, result$cluster, sep = "|")
  expect_equal(length(combos), length(unique(combos)))
})

test_that("prepare_lollipop_data filters to selected trait only", {
  sta_yield <- make_sta_long(trait_name = "Yield")
  sta_dtf <- make_sta_long(trait_name = "DTF", pred_values = rep(60, 6))
  sta <- rbind(sta_yield, sta_dtf)
  review <- make_review_df()
  clusters <- make_clusters()

  result <- prepare_lollipop_data(sta, review, "Yield", clusters)

  # 3 designations x 2 clusters = 6 rows (only Yield, each env in its own cluster)
  expect_equal(nrow(result), 6)
})

test_that("prepare_lollipop_data excludes non-finite predictedValue", {
  sta <- make_sta_long(
    designations = c("G1", "G2", "G3"),
    environments = c("ENV_A"),
    pred_values = c(5.0, NA, Inf)
  )
  review <- make_review_df()
  clusters <- make_clusters(environments = "ENV_A", labels = "All environments")

  result <- prepare_lollipop_data(sta, review, "Yield", clusters)

  # Only G1 has finite value
  expect_equal(nrow(result), 1)
  expect_equal(result$designation, "G1")
  expect_true(is.finite(result$mean_value))
})

test_that("prepare_lollipop_data excludes -Inf predictedValue", {
  sta <- make_sta_long(
    designations = c("G1", "G2"),
    environments = c("ENV_A"),
    pred_values = c(-Inf, 3.0)
  )
  review <- make_review_df(
    designations = c("G1", "G2"),
    statuses = c("SELECTED", "NOT SELECTED")
  )
  clusters <- make_clusters(environments = "ENV_A", labels = "All environments")

  result <- prepare_lollipop_data(sta, review, "Yield", clusters)

  expect_equal(nrow(result), 1)
  expect_equal(result$designation, "G2")
})

test_that("prepare_lollipop_data computes correct mean_value per cluster", {
  # G1 in ENV_A=10, ENV_B=20 -> mean in Cluster1 = 15
  # G1 in ENV_C=30 -> mean in Cluster2 = 30
  sta <- data.frame(
    designation = c("G1", "G1", "G1"),
    environment = c("ENV_A", "ENV_B", "ENV_C"),
    trait = "Yield",
    predictedValue = c(10, 20, 30),
    reliability = c(0.5, 0.6, 0.7),
    stringsAsFactors = FALSE
  )
  review <- make_review_df(designations = "G1", statuses = "SELECTED")
  clusters <- make_clusters(
    environments = c("ENV_A", "ENV_B", "ENV_C"),
    labels = c("Cluster1", "Cluster1", "Cluster2")
  )

  result <- prepare_lollipop_data(sta, review, "Yield", clusters)

  c1_row <- result[result$cluster == "Cluster1", ]
  c2_row <- result[result$cluster == "Cluster2", ]

  expect_equal(c1_row$mean_value, 15)  # (10 + 20) / 2
  expect_equal(c2_row$mean_value, 30)  # 30 / 1
  expect_equal(c1_row$mean_reliability, 0.55)  # (0.5 + 0.6) / 2
  expect_equal(c2_row$mean_reliability, 0.7)
})

test_that("prepare_lollipop_data merges plot_status from review_df", {
  sta <- make_sta_long()
  review <- make_review_df()  # G1=SELECTED, G2=NOT SELECTED, G3=CHECK
  clusters <- make_clusters()

  result <- prepare_lollipop_data(sta, review, "Yield", clusters)

  g1_rows <- result[result$designation == "G1", ]
  g2_rows <- result[result$designation == "G2", ]
  g3_rows <- result[result$designation == "G3", ]

  expect_true(all(g1_rows$plot_status == "SELECTED"))
  expect_true(all(g2_rows$plot_status == "NOT SELECTED"))
  expect_true(all(g3_rows$plot_status == "CHECK"))
})

test_that("prepare_lollipop_data assigns default status for missing designations", {
  sta <- make_sta_long(
    designations = c("G1", "G2"),
    environments = c("ENV_A")
  )
  # Only G1 in review_df
  review <- make_review_df(designations = "G1", statuses = "SELECTED")
  clusters <- make_clusters(environments = "ENV_A", labels = "All environments")

  result <- prepare_lollipop_data(sta, review, "Yield", clusters)

  g2_row <- result[result$designation == "G2", ]
  expect_equal(g2_row$plot_status, "NOT SELECTED")
})

test_that("prepare_lollipop_data applies overrides correctly", {
  sta <- make_sta_long()
  review <- make_review_df()  # G1=SELECTED, G2=NOT SELECTED, G3=CHECK
  clusters <- make_clusters()

  overrides <- data.frame(
    designation = c("G1", "G2"),
    plot_decision = c("NOT SELECTED", "SELECTED"),
    stringsAsFactors = FALSE
  )

  result <- prepare_lollipop_data(sta, review, "Yield", clusters, overrides = overrides)

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

test_that("prepare_lollipop_data maps marker_shape correctly from STATUS_SHAPES", {
  sta <- make_sta_long(
    designations = c("G1", "G2", "G3", "G4"),
    environments = c("ENV_A")
  )
  review <- make_review_df(
    designations = c("G1", "G2", "G3", "G4"),
    statuses = c("SELECTED", "NOT SELECTED", "CHECK", "REVISE")
  )
  clusters <- make_clusters(environments = "ENV_A", labels = "All environments")

  result <- prepare_lollipop_data(sta, review, "Yield", clusters)

  expect_equal(result$marker_shape[result$designation == "G1"], "circle")
  expect_equal(result$marker_shape[result$designation == "G2"], "circle-open")
  expect_equal(result$marker_shape[result$designation == "G3"], "diamond")
  expect_equal(result$marker_shape[result$designation == "G4"], "star")
})

test_that("prepare_lollipop_data maps marker_color correctly from STATUS_COLORS", {
  sta <- make_sta_long(
    designations = c("G1", "G2", "G3", "G4"),
    environments = c("ENV_A")
  )
  review <- make_review_df(
    designations = c("G1", "G2", "G3", "G4"),
    statuses = c("SELECTED", "NOT SELECTED", "CHECK", "REVISE")
  )
  clusters <- make_clusters(environments = "ENV_A", labels = "All environments")

  result <- prepare_lollipop_data(sta, review, "Yield", clusters)

  expect_equal(result$marker_color[result$designation == "G1"], "#0072B2")
  expect_equal(result$marker_color[result$designation == "G2"], "#D55E00")
  expect_equal(result$marker_color[result$designation == "G3"], "#C2185B")
  expect_equal(result$marker_color[result$designation == "G4"], "#F9A825")
})

test_that("prepare_lollipop_data assigns rank_in_cluster correctly", {
  # G1: mean=30, G2: mean=20, G3: mean=10 in same cluster
  sta <- data.frame(
    designation = c("G1", "G2", "G3"),
    environment = c("ENV_A", "ENV_A", "ENV_A"),
    trait = "Yield",
    predictedValue = c(30, 20, 10),
    reliability = c(0.5, 0.5, 0.5),
    stringsAsFactors = FALSE
  )
  review <- make_review_df()
  clusters <- make_clusters(environments = "ENV_A", labels = "All environments")

  result <- prepare_lollipop_data(sta, review, "Yield", clusters)
  result <- result[order(result$designation), ]

  expect_equal(result$rank_in_cluster[result$designation == "G1"], 1L)
  expect_equal(result$rank_in_cluster[result$designation == "G2"], 2L)
  expect_equal(result$rank_in_cluster[result$designation == "G3"], 3L)
})

test_that("prepare_lollipop_data handles rank ties with min method", {
  # G1=30, G2=30, G3=10 -> G1 and G2 should both be rank 1
  sta <- data.frame(
    designation = c("G1", "G2", "G3"),
    environment = c("ENV_A", "ENV_A", "ENV_A"),
    trait = "Yield",
    predictedValue = c(30, 30, 10),
    reliability = c(0.5, 0.5, 0.5),
    stringsAsFactors = FALSE
  )
  review <- make_review_df()
  clusters <- make_clusters(environments = "ENV_A", labels = "All environments")

  result <- prepare_lollipop_data(sta, review, "Yield", clusters)

  g1_rank <- result$rank_in_cluster[result$designation == "G1"]
  g2_rank <- result$rank_in_cluster[result$designation == "G2"]
  g3_rank <- result$rank_in_cluster[result$designation == "G3"]

  expect_equal(g1_rank, 1L)
  expect_equal(g2_rank, 1L)
  expect_equal(g3_rank, 3L)  # min method: next rank after ties = 3
})

test_that("prepare_lollipop_data computes overall_rank from grand mean across ALL environments", {
  # G1: ENV_A=10, ENV_B=20 -> grand mean = 15
  # G2: ENV_A=25, ENV_B=5  -> grand mean = 15  (tie)
  # G3: ENV_A=30, ENV_B=40 -> grand mean = 35
  sta <- data.frame(
    designation = c("G1", "G1", "G2", "G2", "G3", "G3"),
    environment = c("ENV_A", "ENV_B", "ENV_A", "ENV_B", "ENV_A", "ENV_B"),
    trait = "Yield",
    predictedValue = c(10, 20, 25, 5, 30, 40),
    reliability = rep(0.5, 6),
    stringsAsFactors = FALSE
  )
  review <- make_review_df()
  clusters <- make_clusters(
    environments = c("ENV_A", "ENV_B"),
    labels = c("Cluster1", "Cluster2")
  )

  result <- prepare_lollipop_data(sta, review, "Yield", clusters)

  # G3 grand mean = 35 -> rank 1
  # G1 and G2 grand mean = 15 -> rank 2 (min tie method)
  g3_rows <- result[result$designation == "G3", ]
  g1_rows <- result[result$designation == "G1", ]
  g2_rows <- result[result$designation == "G2", ]

  expect_true(all(g3_rows$overall_rank == 1L))
  expect_true(all(g1_rows$overall_rank == 2L))
  expect_true(all(g2_rows$overall_rank == 2L))
})

test_that("prepare_lollipop_data computes n_envs_in_cluster correctly", {
  sta <- data.frame(
    designation = c("G1", "G1", "G1"),
    environment = c("ENV_A", "ENV_B", "ENV_C"),
    trait = "Yield",
    predictedValue = c(10, 20, 30),
    reliability = rep(0.5, 3),
    stringsAsFactors = FALSE
  )
  review <- make_review_df(designations = "G1", statuses = "SELECTED")
  clusters <- make_clusters(
    environments = c("ENV_A", "ENV_B", "ENV_C"),
    labels = c("Cluster1", "Cluster1", "Cluster2")
  )

  result <- prepare_lollipop_data(sta, review, "Yield", clusters)

  c1_row <- result[result$cluster == "Cluster1", ]
  c2_row <- result[result$cluster == "Cluster2", ]

  expect_equal(c1_row$n_envs_in_cluster, 2)
  expect_equal(c2_row$n_envs_in_cluster, 1)
})

test_that("prepare_lollipop_data handles NULL overrides", {
  sta <- make_sta_long()
  review <- make_review_df()
  clusters <- make_clusters()

  result_null <- prepare_lollipop_data(sta, review, "Yield", clusters, overrides = NULL)
  result_no_arg <- prepare_lollipop_data(sta, review, "Yield", clusters)

  expect_equal(result_null, result_no_arg)
})

test_that("prepare_lollipop_data handles empty overrides data.frame", {
  sta <- make_sta_long()
  review <- make_review_df()
  clusters <- make_clusters()

  empty_overrides <- data.frame(
    designation = character(0),
    plot_decision = character(0),
    stringsAsFactors = FALSE
  )

  result_empty <- prepare_lollipop_data(sta, review, "Yield", clusters, overrides = empty_overrides)
  result_null <- prepare_lollipop_data(sta, review, "Yield", clusters, overrides = NULL)

  expect_equal(result_empty, result_null)
})

test_that("prepare_lollipop_data is idempotent (same inputs produce same output)", {
  sta <- make_sta_long()
  review <- make_review_df()
  clusters <- make_clusters()

  result1 <- prepare_lollipop_data(sta, review, "Yield", clusters)
  result2 <- prepare_lollipop_data(sta, review, "Yield", clusters)

  expect_equal(result1, result2)
})

test_that("prepare_lollipop_data returns no rows when trait has no data", {
  sta <- make_sta_long(trait_name = "DTF")
  review <- make_review_df()
  clusters <- make_clusters()

  result <- prepare_lollipop_data(sta, review, "Yield", clusters)

  expect_equal(nrow(result), 0)
})

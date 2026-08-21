#' Map reliability values to opacity
#'
#' Vectorized helper that converts prediction reliability values to
#' visual opacity on the scale [0.15, 1.0]. The mapping is linear from
#' reliability 0 (opacity 0.15) to reliability 0.7 (opacity 1.0), and
#' any reliability >= 0.7 saturates at full opacity.
#'
#' @param rel Numeric vector of reliability values (may contain NAs).
#'
#' @return Numeric vector of same length as \code{rel}, with all values
#'   in [0.15, 1.0].
#'
#' @details
#' - NA values are treated as 0.7 (full opacity).
#' - Negative values are floored: the formula yields 0.15 for rel = 0,
#'   and values below 0 are clamped to produce the minimum opacity of 0.15.
#' - Values above 0.7 are capped at 1.0 opacity.
#'
#' @noRd
reliability_to_opacity <- function(rel) {
  rel[is.na(rel)] <- 0.7
  # Clamp negative values to 0 before computing opacity

  rel_clamped <- pmax(rel, 0)
  pmin(1.0, 0.15 + (pmin(rel_clamped, 0.7) / 0.7) * 0.85)
}

# --------------------------------------------------------------------------
# Status encoding constants (used by lollipop and beeswarm pipelines)
# --------------------------------------------------------------------------

STATUS_SHAPES <- c(
  "SELECTED"     = "circle",
  "NOT SELECTED" = "circle",
  "REVISE"       = "star",
  "CHECK"        = "diamond"
)

STATUS_COLORS <- c(
  "SELECTED"     = "#0072B2",
  "NOT SELECTED" = "#D55E00",
  "REVISE"       = "#F9A825",
  "CHECK"        = "#C2185B"
)

# --------------------------------------------------------------------------
# Environment cluster palette (shared by the cluster map and the per-cluster
# "Performance across locations" tables so their colours always agree)
# --------------------------------------------------------------------------

CLUSTER_PALETTE <- c("#4CAF50", "#2196F3", "#FF9800", "#9C27B0", "#F44336")

#' Map environment cluster labels to palette colours
#'
#' Colours are assigned in the order clusters first appear in the location
#' rows of \code{weather_summary}, which is how the cluster map assigns them.
#' When no usable weather summary is supplied, the order of appearance in
#' \code{cluster_assignments} is used instead.
#'
#' @param cluster_assignments Named character vector mapping environment name
#'   to cluster label.
#' @param weather_summary Optional data.frame with environment, LON, LAT.
#' @return Named character vector of hex colours keyed by cluster label.
#' @noRd
cluster_palette_map <- function(cluster_assignments, weather_summary = NULL) {
  labels <- NULL

  if (!is.null(weather_summary) && is.data.frame(weather_summary) &&
      all(c("environment", "LON", "LAT") %in% colnames(weather_summary))) {
    loc <- unique(weather_summary[, c("environment", "LON", "LAT"), drop = FALSE])
    loc <- loc[is.finite(loc$LON) & is.finite(loc$LAT), , drop = FALSE]
    if (nrow(loc) > 0) {
      cl <- cluster_assignments[as.character(loc$environment)]
      cl[is.na(cl)] <- "Unknown"
      labels <- unique(as.character(cl))
    }
  }

  if (is.null(labels)) {
    labels <- unique(as.character(cluster_assignments))
  }
  labels <- labels[!is.na(labels)]
  if (length(labels) == 0) return(stats::setNames(character(0), character(0)))

  # Recycle the palette so a 6th+ cluster still gets a colour rather than NA
  idx <- (seq_along(labels) - 1L) %% length(CLUSTER_PALETTE) + 1L
  stats::setNames(CLUSTER_PALETTE[idx], labels)
}

# --------------------------------------------------------------------------
# Decision-table gradient cell styling (shared by the Decision table and the
# "Performance across locations" table so both render identically)
# --------------------------------------------------------------------------

GRADIENT_CELL_COLORS <- c(
  "SELECTED"     = "#85C1E9",
  "NOT SELECTED" = "#E8A87C",
  "REVISE"       = "#F7DC6F",
  "CHECK"        = "#C39BD3"
)

#' Base gradient colour for a decision status
#'
#' @param status Character vector of decision statuses.
#' @return Character vector of hex colours; unknown statuses fall back to the
#'   NOT SELECTED colour.
#' @noRd
gradient_status_color <- function(status) {
  out <- unname(GRADIENT_CELL_COLORS[as.character(status)])
  out[is.na(out)] <- GRADIENT_CELL_COLORS[["NOT SELECTED"]]
  out
}

#' Compute gradient intensity from quintile bins
#'
#' Returns an opacity multiplier in [0.15, 1.0] so that stronger performers get
#' more saturated cells. For SELECTED rows the best values are most saturated;
#' for NOT SELECTED rows the worst values are most saturated (highlighting the
#' reason they were dropped). CHECK and REVISE rows keep the mid default \u2014
#' REVISE is a deferred decision, so shading it like a rejection would imply it
#' was dropped for poor performance. REVISE values still contribute to the
#' quintile breaks, since they are part of the candidate population.
#'
#' @param values Numeric vector of trait values.
#' @param status Character vector of decision status per row.
#' @param higher_is_better Logical. If TRUE, higher values are better.
#' @return Numeric vector of opacity values in [0.15, 1.0].
#' @noRd
get_quintile_opacity <- function(values, status, higher_is_better = TRUE) {
  n <- length(values)
  opacity <- rep(0.5, n)  # default mid
  numeric_vals <- as.numeric(values)
  valid <- !is.na(numeric_vals) & status != "CHECK"
  if (sum(valid) < 5) return(opacity)
  quants <- stats::quantile(numeric_vals[valid], probs = c(0.2, 0.4, 0.6, 0.8), na.rm = TRUE)
  for (i in which(valid)) {
    # REVISE keeps the neutral mid default set above
    if (identical(status[i], "REVISE")) next
    v <- numeric_vals[i]
    if (status[i] == "SELECTED") {
      if (higher_is_better) {
        if (v >= quants[4]) opacity[i] <- 1.0
        else if (v >= quants[3]) opacity[i] <- 0.75
        else if (v >= quants[2]) opacity[i] <- 0.5
        else if (v >= quants[1]) opacity[i] <- 0.3
        else opacity[i] <- 0.15
      } else {
        if (v <= quants[1]) opacity[i] <- 1.0
        else if (v <= quants[2]) opacity[i] <- 0.75
        else if (v <= quants[3]) opacity[i] <- 0.5
        else if (v <= quants[4]) opacity[i] <- 0.3
        else opacity[i] <- 0.15
      }
    } else {
      if (higher_is_better) {
        if (v <= quants[1]) opacity[i] <- 1.0
        else if (v <= quants[2]) opacity[i] <- 0.75
        else if (v <= quants[3]) opacity[i] <- 0.5
        else if (v <= quants[4]) opacity[i] <- 0.3
        else opacity[i] <- 0.15
      } else {
        if (v >= quants[4]) opacity[i] <- 1.0
        else if (v >= quants[3]) opacity[i] <- 0.75
        else if (v >= quants[2]) opacity[i] <- 0.5
        else if (v >= quants[1]) opacity[i] <- 0.3
        else opacity[i] <- 0.15
      }
    }
  }
  opacity
}

#' Blend a hex colour toward white by an opacity factor
#'
#' @param hex_color Hex colour string, e.g. "#85C1E9".
#' @param opacity Numeric in [0, 1]; 1 keeps the colour, 0 returns white.
#' @return Hex colour string.
#' @noRd
blend_color <- function(hex_color, opacity) {
  r <- strtoi(substr(hex_color, 2, 3), 16)
  g <- strtoi(substr(hex_color, 4, 5), 16)
  b <- strtoi(substr(hex_color, 6, 7), 16)
  r2 <- as.integer(r * opacity + 255 * (1 - opacity))
  g2 <- as.integer(g * opacity + 255 * (1 - opacity))
  b2 <- as.integer(b * opacity + 255 * (1 - opacity))
  sprintf("#%02X%02X%02X", r2, g2, b2)
}

#' Decision-group colours for the Final Review statistics and plots
#'
#' Reuses the decision-table palette so the groups read consistently:
#' Selected = blue, All candidates = orange, Checks = pink/purple.
#' @noRd
REVIEW_GROUP_COLORS <- c(
  "Selected"       = "#85C1E9",
  "All candidates" = "#E8A87C",
  "Checks"         = "#C39BD3"
)

#' Faded variant of the decision-group colours, for table row backgrounds
#'
#' @param opacity Numeric in [0, 1]; lower is more washed out.
#' @return Named character vector of hex colours.
#' @noRd
review_group_colors_faded <- function(opacity = 0.35) {
  stats::setNames(
    vapply(REVIEW_GROUP_COLORS, blend_color, character(1), opacity),
    names(REVIEW_GROUP_COLORS)
  )
}

#' Build gradient-filled HTML cells for a numeric column
#'
#' @param values Numeric vector of cell values.
#' @param status Character vector of decision status per row.
#' @param higher_is_better Logical, passed to \code{get_quintile_opacity()}.
#' @param bold Logical, whether to bold the cell text.
#' @param digits Number of decimals to display.
#' @return Character vector of HTML div strings.
#' @noRd
gradient_cells <- function(values, status, higher_is_better = TRUE,
                           bold = FALSE, digits = 3) {
  op <- get_quintile_opacity(values, status, higher_is_better = higher_is_better)
  weight <- if (isTRUE(bold)) "font-weight:600;" else ""
  mapply(function(val, st, o) {
    bg <- blend_color(gradient_status_color(st), o)
    txt <- if (is.na(val)) "" else format(round(as.numeric(val), digits), nsmall = digits)
    sprintf(
      "<div style='background:%s; padding:6px; border-radius:4px; text-align:right; %s'>%s</div>",
      bg, weight, txt
    )
  }, values, status, op, SIMPLIFY = TRUE, USE.NAMES = FALSE)
}

# --------------------------------------------------------------------------
# Relatedness plot visual constants
# --------------------------------------------------------------------------
MARKER_SIZE_DEFAULT     <- 8
MARKER_SIZE_HIGHLIGHTED <- 14

DIVERSITY_BORDER_COLOR  <- "#FFD700"
DIVERSITY_BORDER_WIDTH  <- 3
DIVERSITY_BORDER_STYLE  <- "dash"

HIGHLIGHT_BORDER_COLOR  <- "white"
HIGHLIGHT_BORDER_WIDTH  <- 3

FAMILY_PALETTE <- c(
  "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
  "#FF7F00", "#A65628", "#F781BF", "#999999"
)

MISSING_STATUS_COLOR   <- "#999999"
MISSING_STATUS_OPACITY <- 0.15

# --------------------------------------------------------------------------
# Human-readable covariate name mapping
# --------------------------------------------------------------------------
.covariate_readable <- c(
  mean_temperature           = "temperature",
  heat_stress_index          = "heat stress",
  rainfall                   = "rainfall",

  rainfall_distribution_index = "rain distribution",
  humidity                   = "humidity"
)

#' Cluster environments by environmental covariates
#'
#' Groups trial environments into clusters based on weather covariates using
#' k-means with auto-detected optimal k (via silhouette scores).
#'
#' @param weather_summary A data.frame with columns: environment,
#'   mean_temperature, heat_stress_index, rainfall,
#'   rainfall_distribution_index, humidity (one row per environment).
#' @param k NULL (auto-detect) or integer â€” number of clusters to use.
#'
#' @return A named character vector mapping each environment to its cluster
#'   label.
#'
#' @noRd
cluster_environments <- function(weather_summary, k = NULL) {
  envs <- as.character(weather_summary$environment)

  # ---- Fallback: insufficient finite data ----
  all_covariates <- c("mean_temperature", "heat_stress_index", "rainfall",
                      "rainfall_distribution_index", "humidity")

  available_covs <- intersect(all_covariates, names(weather_summary))
  if (length(available_covs) == 0) {
    result <- rep("All environments", length(envs))
    names(result) <- envs
    return(result)
  }
  cov_mat <- as.matrix(weather_summary[, available_covs, drop = FALSE])
  finite_mask <- apply(cov_mat, 1, function(row) any(is.finite(row)))

  if (sum(finite_mask) < 2) {
    result <- rep("All environments", length(envs))
    names(result) <- envs
    return(result)
  }

  # ---- Prepare scaled covariate matrix ----
  cov_df <- weather_summary[, available_covs, drop = FALSE]

  # Replace non-finite values with column means for scaling
  for (col in available_covs) {
    col_vals <- cov_df[[col]]
    col_mean <- mean(col_vals[is.finite(col_vals)], na.rm = TRUE)
    col_vals[!is.finite(col_vals)] <- col_mean
    cov_df[[col]] <- col_vals
  }

  # Scale numeric covariates
  cov_scaled <- scale(cov_df)
  # Handle columns with zero variance (all same value after imputation)

  cov_scaled[is.nan(cov_scaled)] <- 0

  n_finite <- sum(finite_mask)

  # ---- Auto-detect optimal k if not specified ----
  if (is.null(k)) {
    if (n_finite < 4) {
      # Not enough environments to try k=2 meaningfully with silhouette
      k <- if (n_finite >= 2) 2L else 1L
    } else {
      # Try k from 2 to min(10, sqrt(n)) and pick by silhouette score
      has_cluster_pkg <- requireNamespace("cluster", quietly = TRUE)
      if (!has_cluster_pkg) {
        k <- 2L
      } else {
        max_k <- min(10L, as.integer(ceiling(sqrt(n_finite))), n_finite - 1L)
        max_k <- max(2L, max_k)
        candidates <- seq(2L, max_k)
        best_k <- 2L
        best_sil <- -1
        no_improve_count <- 0L

        set.seed(42)
        for (ck in candidates) {
          if (ck > n_finite) next
          km_try <- stats::kmeans(cov_scaled, centers = ck, nstart = 25)
          sil <- cluster::silhouette(km_try$cluster, stats::dist(cov_scaled))
          mean_sil <- mean(sil[, "sil_width"])
          if (mean_sil > best_sil) {
            best_sil <- mean_sil
            best_k <- ck
            no_improve_count <- 0L
          } else {
            no_improve_count <- no_improve_count + 1L
            # Stop after 2 consecutive non-improvements
            if (no_improve_count >= 2L) break
          }
        }
        # If best silhouette is very low, fall back to single cluster
        if (best_sil < 0.25) {
          result <- rep("All environments", length(envs))
          names(result) <- envs
          return(result)
        }
        k <- best_k
      }
    }
  }

  # ---- K-means clustering ----
  effective_k <- min(as.integer(k), n_finite)
  if (effective_k < 2) {
    result <- rep("All environments", length(envs))
    names(result) <- envs
    return(result)
  }

  # Minimum 3 environments per cluster constraint
  min_cluster_size <- 3L
  set.seed(42)
  km <- stats::kmeans(cov_scaled, centers = effective_k, nstart = 25)

  # If any cluster has fewer than min_cluster_size members, reduce k and retry

  while (min(table(km$cluster)) < min_cluster_size && effective_k > 2L) {
    effective_k <- effective_k - 1L
    set.seed(42)
    km <- stats::kmeans(cov_scaled, centers = effective_k, nstart = 25)
  }

  # Final fallback: if even k=2 produces a cluster too small, use single group
  if (effective_k < 2 || min(table(km$cluster)) < min_cluster_size) {
    if (effective_k >= 2 && min(table(km$cluster)) < min_cluster_size) {
      result <- rep("All environments", length(envs))
      names(result) <- envs
      return(result)
    }
  }

  # Generate descriptive labels per centroid
  # For each cluster center, find the covariate that deviates most from
  # the global mean (which is 0 in scaled space)
  centers <- km$centers  # matrix: k x ncol
  labels_vec <- character(effective_k)

  for (i in seq_len(effective_k)) {
    center_row <- centers[i, ]
    # Find top 2 most distinguishing covariates (largest absolute deviations)
    abs_devs <- abs(center_row)
    sorted_idx <- order(abs_devs, decreasing = TRUE)

    # Primary distinguishing covariate
    primary_idx <- sorted_idx[1]
    primary_cov <- available_covs[primary_idx]
    primary_label <- if (primary_cov %in% names(.covariate_readable)) {
      .covariate_readable[[primary_cov]]
    } else {
      primary_cov
    }
    primary_dir <- if (center_row[primary_idx] >= 0) "Warmer" else "Cooler"
    if (primary_cov != "mean_temperature") {
      primary_dir <- if (center_row[primary_idx] >= 0) "Higher" else "Lower"
    }

    # Check if second covariate is also strongly distinguishing (> 0.5 in absolute terms)
    if (length(sorted_idx) >= 2 && abs_devs[sorted_idx[2]] > 0.5) {
      secondary_idx <- sorted_idx[2]
      secondary_cov <- available_covs[secondary_idx]
      secondary_label <- if (secondary_cov %in% names(.covariate_readable)) {
        .covariate_readable[[secondary_cov]]
      } else {
        secondary_cov
      }
      secondary_dir <- if (center_row[secondary_idx] >= 0) "higher" else "lower"
      if (secondary_cov == "mean_temperature") {
        secondary_dir <- if (center_row[secondary_idx] >= 0) "warmer" else "cooler"
      }
      labels_vec[i] <- paste0(primary_dir, " ", primary_label, ", ", secondary_dir, " ", secondary_label)
    } else {
      labels_vec[i] <- paste0(primary_dir, " ", primary_label)
    }
  }

  # Handle potential duplicate labels by appending cluster number
  if (anyDuplicated(labels_vec)) {
    labels_vec <- paste0(labels_vec, " (", seq_len(effective_k), ")")
  }

  # Map cluster assignments to labels
  cluster_ids <- km$cluster
  result <- labels_vec[cluster_ids]
  names(result) <- envs
  result
}

#' Prepare lollipop chart data
#'
#' Transforms STA predictions into per-designation-per-cluster mean performance
#' data with status encoding, marker mapping, and ranking information.
#'
#' @param sta_long A data.frame with columns: designation, environment, trait,
#'   predictedValue, reliability.
#' @param review_df A data.frame with columns: designation, plot_status.
#' @param trait Character â€” selected trait to filter on.
#' @param cluster_assignments Named character vector mapping environment names
#'   to cluster labels.
#' @param overrides NULL or a data.frame with columns: designation,
#'   plot_decision. Override values take precedence over review_df status.
#'
#' @return A data.frame with columns: designation, cluster, mean_value,
#'   mean_reliability, plot_status, marker_shape, marker_color,
#'   rank_in_cluster, overall_rank, n_envs_in_cluster. Exactly one row per
#'   (designation, cluster) pair.
#'
#' @noRd
prepare_lollipop_data <- function(sta_long, review_df, trait, cluster_assignments, overrides = NULL) {

  # Empty result template
  empty_result <- data.frame(
    designation       = character(0),
    cluster           = character(0),
    mean_value        = numeric(0),
    mean_reliability  = numeric(0),
    plot_status       = character(0),
    marker_shape      = character(0),
    marker_color      = character(0),
    rank_in_cluster   = integer(0),
    overall_rank      = integer(0),
    n_envs_in_cluster = integer(0),
    stringsAsFactors  = FALSE
  )

  # Special case: index_value comes from review_df, not sta_long
  if (trait == "index_value") {
    if (!"index_value" %in% colnames(review_df)) {
      return(empty_result)
    }

    # Build one row per designation per cluster
    # index_value is the same across all clusters (it's an MTA-level metric)
    unique_clusters <- unique(cluster_assignments)
    designations_with_index <- review_df[is.finite(review_df$index_value),
                                          c("designation", "plot_status", "index_value"),
                                          drop = FALSE]

    if (nrow(designations_with_index) == 0) return(empty_result)

    # Expand: one row per designation per cluster
    agg <- expand.grid(
      designation = designations_with_index$designation,
      cluster = unique_clusters,
      stringsAsFactors = FALSE
    )
    agg <- merge(agg, designations_with_index, by = "designation")
    names(agg)[names(agg) == "index_value"] <- "mean_value"
    agg$mean_reliability <- NA_real_
    agg$n_envs_in_cluster <- NA_integer_

    # Apply overrides if provided
    if (!is.null(overrides) && nrow(overrides) > 0) {
      override_map <- stats::setNames(overrides$plot_decision, overrides$designation)
      match_idx <- match(agg$designation, names(override_map))
      has_override <- !is.na(match_idx)
      agg$plot_status[has_override] <- override_map[agg$designation[has_override]]
    }

    # Map plot_status to marker_shape and marker_color
    agg$marker_shape <- STATUS_SHAPES[agg$plot_status]
    agg$marker_color <- STATUS_COLORS[agg$plot_status]

    # Compute rank_in_cluster (rank by descending mean_value within cluster)
    agg$rank_in_cluster <- ave(
      agg$mean_value,
      agg$cluster,
      FUN = function(x) rank(-x, ties.method = "min")
    )
    agg$rank_in_cluster <- as.integer(agg$rank_in_cluster)

    # Compute overall_rank
    grand_means <- tapply(agg$mean_value, agg$designation, mean, na.rm = TRUE)
    unique_designations <- names(grand_means)
    designation_ranks <- as.integer(rank(-grand_means, ties.method = "min"))
    names(designation_ranks) <- unique_designations
    agg$overall_rank <- designation_ranks[agg$designation]

    agg <- agg[is.finite(agg$mean_value), , drop = FALSE]

    return(agg[, c("designation", "cluster", "mean_value", "mean_reliability",
                    "plot_status", "marker_shape", "marker_color",
                    "rank_in_cluster", "overall_rank", "n_envs_in_cluster")])
  }

  # Step 1: Filter to selected trait and finite predictedValue
  df <- sta_long[sta_long$trait == trait, , drop = FALSE]
  df <- df[is.finite(df$predictedValue), , drop = FALSE]

  # Step 2: Add cluster column via cluster_assignments lookup
  df$cluster <- cluster_assignments[as.character(df$environment)]

  # Remove rows whose environment isn't in cluster_assignments
  df <- df[!is.na(df$cluster), , drop = FALSE]

  # Early return if no data remains
 if (nrow(df) == 0) {
    return(empty_result)
  }

  # Step 3: Group by (designation, cluster) and compute aggregates
  groups <- split(df, list(df$designation, df$cluster), drop = TRUE)

  agg_list <- lapply(groups, function(grp) {
    data.frame(
      designation       = grp$designation[1],
      cluster           = grp$cluster[1],
      mean_value        = mean(grp$predictedValue, na.rm = TRUE),
      mean_reliability  = mean(grp$reliability, na.rm = TRUE),
      n_envs_in_cluster = nrow(grp),
      stringsAsFactors  = FALSE
    )
  })

  agg <- do.call(rbind, agg_list)
  rownames(agg) <- NULL

  # Step 4: Merge with review_df for plot_status
  agg <- merge(agg, review_df[, c("designation", "plot_status"), drop = FALSE],
               by = "designation", all.x = TRUE)
  # Default for any designation not found in review_df
  agg$plot_status[is.na(agg$plot_status)] <- "NOT SELECTED"

  # Step 5: Apply overrides if provided (override takes precedence)
  if (!is.null(overrides) && nrow(overrides) > 0) {
    override_map <- stats::setNames(overrides$plot_decision, overrides$designation)
    match_idx <- match(agg$designation, names(override_map))
    has_override <- !is.na(match_idx)
    agg$plot_status[has_override] <- override_map[agg$designation[has_override]]
  }

  # Step 6: Map plot_status to marker_shape and marker_color
  agg$marker_shape <- STATUS_SHAPES[agg$plot_status]
  agg$marker_color <- STATUS_COLORS[agg$plot_status]

  # Step 7: Compute rank_in_cluster (rank by descending mean_value within cluster)
  agg$rank_in_cluster <- ave(
    agg$mean_value,
    agg$cluster,
    FUN = function(x) rank(-x, ties.method = "min")
  )
  agg$rank_in_cluster <- as.integer(agg$rank_in_cluster)

  # Step 8: Compute overall_rank (rank by descending grand mean across ALL environments)
  # First compute grand mean per designation across ALL environments (not per cluster)
  grand_means <- tapply(
    df$predictedValue,
    df$designation,
    mean,
    na.rm = TRUE
  )
  # Build a designation-level rank lookup (one rank per unique designation)
  unique_designations <- names(grand_means)
  designation_ranks <- as.integer(rank(-grand_means, ties.method = "min"))
  names(designation_ranks) <- unique_designations

  # Map back to each row in agg
  agg$overall_rank <- designation_ranks[agg$designation]

  # Ensure no rows with NA or non-finite mean_value
  agg <- agg[is.finite(agg$mean_value), , drop = FALSE]

  # Return columns in specified order
  agg[, c("designation", "cluster", "mean_value", "mean_reliability",
           "plot_status", "marker_shape", "marker_color",
           "rank_in_cluster", "overall_rank", "n_envs_in_cluster")]
}

#' Compute recommendation zone thresholds per cluster
#'
#' Derives per-cluster recommend and avoid thresholds from prepared lollipop
#' data. The recommend threshold equals the mean of CHECK designations within
#' a cluster (falling back to the population mean if no CHECKs exist). The
#' avoid threshold is the lower of (population mean âˆ’ 1 SD) and the minimum
#' CHECK value, enforced to never exceed the recommend threshold.
#'
#' @param prepared_data A data.frame output of \code{prepare_lollipop_data()}
#'   with columns: designation, cluster, mean_value, plot_status (among others).
#'
#' @return A data.frame with columns: cluster, recommend_threshold,
#'   avoid_threshold, check_mean, pop_mean, pop_sd. One row per cluster.
#'
#' @noRd
compute_zone_thresholds <- function(prepared_data) {
  clusters <- unique(prepared_data$cluster)

  result_list <- lapply(clusters, function(cl) {
    cl_data <- prepared_data[prepared_data$cluster == cl, , drop = FALSE]

    # Population statistics
    pop_mean <- mean(cl_data$mean_value, na.rm = TRUE)
    pop_sd   <- stats::sd(cl_data$mean_value, na.rm = TRUE)

    # Handle edge case: single designation or all same value â†’ sd is NA or 0

    if (is.na(pop_sd) || !is.finite(pop_sd)) {
      pop_sd <- 0
    }

    # CHECK designations within this cluster
    check_rows <- cl_data[cl_data$plot_status == "CHECK", , drop = FALSE]

    if (nrow(check_rows) > 0) {
      check_mean      <- mean(check_rows$mean_value, na.rm = TRUE)
      min_check_value <- min(check_rows$mean_value, na.rm = TRUE)
    } else {
      check_mean      <- NA_real_
      min_check_value <- NA_real_
    }

    # Recommend threshold: check_mean if available, else pop_mean
    recommend_threshold <- if (!is.na(check_mean)) check_mean else pop_mean

    # Avoid threshold: min(pop_mean - 1*pop_sd, min_check_value)
    # If pop_sd is 0, pop_mean - 0 = pop_mean â†’ avoid = pop_mean (same as recommend)
    pop_lower <- pop_mean - 1 * pop_sd

    if (!is.na(min_check_value)) {
      avoid_threshold <- min(pop_lower, min_check_value)
    } else {
      avoid_threshold <- pop_lower
    }

    # Enforce: avoid_threshold <= recommend_threshold
    avoid_threshold <- min(avoid_threshold, recommend_threshold)

    data.frame(
      cluster             = cl,
      recommend_threshold = recommend_threshold,
      avoid_threshold     = avoid_threshold,
      check_mean          = check_mean,
      pop_mean            = pop_mean,
      pop_sd              = pop_sd,
      stringsAsFactors    = FALSE
    )
  })

  result <- do.call(rbind, result_list)
  rownames(result) <- NULL
  result
}

#' Build faceted lollipop plotly figure
#'
#' Constructs a complete plotly figure with vertically stacked subplot facets
#' (one per environment cluster), each containing a horizontal lollipop chart
#' with recommendation zones and prescriptive headers.
#'
#' @param prepared_data A data.frame from \code{prepare_lollipop_data()} with
#'   columns: designation, cluster, mean_value, mean_reliability, plot_status,
#'   marker_shape, marker_color, rank_in_cluster, overall_rank, n_envs_in_cluster.
#' @param thresholds A data.frame from \code{compute_zone_thresholds()} with
#'   columns: cluster, recommend_threshold, avoid_threshold, check_mean,
#'   pop_mean, pop_sd.
#' @param highlighted NULL or character â€” designation to highlight across facets.
#' @param user_recommend_threshold NULL or numeric â€” user override for
#'   recommend threshold (applies to all clusters).
#' @param source_id Character â€” plotly source ID for click events.
#'
#' @return A plotly object with vertically stacked subplot facets.
#'
#' @noRd
build_faceted_lollipop_plotly <- function(prepared_data, thresholds,
                                          highlighted = NULL,
                                          user_recommend_threshold = NULL,
                                          source_id) {

  clusters <- unique(prepared_data$cluster)
  n_clusters <- length(clusters)


  # ---- Global X-axis range (consistent across all facets) ----
  x_min_global <- min(prepared_data$mean_value, na.rm = TRUE)
  x_max_global <- max(prepared_data$mean_value, na.rm = TRUE)
  x_padding <- (x_max_global - x_min_global) * 0.05
  if (x_padding == 0) x_padding <- 0.5
  x_range <- c(x_min_global - x_padding, x_max_global + x_padding)

  # ---- Unique statuses for legend ----
  all_statuses <- unique(prepared_data$plot_status)
  legend_shown <- character(0)

  # ---- Build one subplot per cluster ----
  plot_list <- lapply(seq_along(clusters), function(i) {
    cl <- clusters[i]
    cl_data <- prepared_data[prepared_data$cluster == cl, , drop = FALSE]

    # Sort by descending mean_value (best at top)
    cl_data <- cl_data[order(cl_data$mean_value, decreasing = FALSE), , drop = FALSE]
    # Factor designations to maintain sort order on Y-axis (plotly reverses)
    cl_data$designation <- factor(cl_data$designation,
                                  levels = cl_data$designation)

    n_in_facet <- nrow(cl_data)

    # ---- Determine marker sizes and opacity ----
    marker_sizes <- rep(10, n_in_facet)
    marker_opacity <- rep(1, n_in_facet)
    marker_line_width <- rep(0, n_in_facet)
    marker_line_color <- rep("rgba(0,0,0,0)", n_in_facet)

    # REVISE markers get slightly larger
    revise_idx <- which(cl_data$plot_status == "REVISE")
    if (length(revise_idx) > 0) {
      marker_sizes[revise_idx] <- 12
    }

    # Highlighting logic
    if (!is.null(highlighted)) {
      hl_idx <- which(cl_data$designation == highlighted)
      non_hl_idx <- which(cl_data$designation != highlighted)
      if (length(hl_idx) > 0) {
        marker_sizes[hl_idx] <- 14
        marker_line_width[hl_idx] <- 2
        marker_line_color[hl_idx] <- "white"
      }
      if (length(non_hl_idx) > 0) {
        marker_opacity[non_hl_idx] <- 0.3
      }
    }

    # ---- Tooltip text ----
    tooltip_text <- mapply(function(desig, mv, rank, n_total, status, rel) {
      paste0(
        "<b>", desig, "</b><br>",
        "Mean value: ", sprintf("%.2f", mv), "<br>",
        "Rank in cluster: ", rank, " / ", n_total, "<br>",
        "Status: ", status, "<br>",
        "Reliability: ", sprintf("%.2f", rel)
      )
    }, cl_data$designation, cl_data$mean_value, cl_data$rank_in_cluster,
    n_in_facet, cl_data$plot_status, cl_data$mean_reliability,
    SIMPLIFY = TRUE, USE.NAMES = FALSE)

    # ---- Determine which statuses need legend entries ----
    show_legend_vec <- vapply(seq_len(n_in_facet), function(j) {
      st <- cl_data$plot_status[j]
      if (!(st %in% legend_shown)) {
        legend_shown <<- c(legend_shown, st)
        TRUE
      } else {
        FALSE
      }
    }, logical(1))

    # ---- Build the plot ----
    p <- plotly::plot_ly(source = source_id)

    # Add lollipop stems (horizontal segments from 0 or x_min to mean_value)
    stem_x <- list()
    stem_y <- list()
    for (j in seq_len(n_in_facet)) {
      stem_x <- c(stem_x, list(c(x_range[1], cl_data$mean_value[j], NA)))
      stem_y <- c(stem_y, list(c(as.character(cl_data$designation[j]),
                                  as.character(cl_data$designation[j]), NA)))
    }
    stem_x_flat <- unlist(stem_x)
    stem_y_flat <- unlist(stem_y)

    # Stem opacity matches highlighting
    stem_opacity <- if (!is.null(highlighted)) {
      opacity_vals <- vapply(seq_len(n_in_facet), function(j) {
        if (cl_data$designation[j] == highlighted) 1 else 0.3
      }, numeric(1))
      # Expand: each stem has 3 points (start, end, NA)
      rep(opacity_vals, each = 3)
    } else {
      1
    }

    p <- plotly::add_trace(
      p,
      x = stem_x_flat,
      y = stem_y_flat,
      type = "scatter",
      mode = "lines",
      line = list(color = "grey70", width = 1),
      opacity = if (is.numeric(stem_opacity) && length(stem_opacity) == 1) stem_opacity else 0.6,
      hoverinfo = "skip",
      showlegend = FALSE
    )

    # Add markers â€” one trace per status for legend grouping
    for (st in all_statuses) {
      st_idx <- which(cl_data$plot_status == st)
      if (length(st_idx) == 0) next

      show_leg <- any(show_legend_vec[st_idx])

      p <- plotly::add_trace(
        p,
        x = cl_data$mean_value[st_idx],
        y = cl_data$designation[st_idx],
        type = "scatter",
        mode = "markers",
        marker = list(
          symbol = cl_data$marker_shape[st_idx],
          color = cl_data$marker_color[st_idx],
          size = marker_sizes[st_idx],
          opacity = marker_opacity[st_idx],
          line = list(
            width = marker_line_width[st_idx],
            color = marker_line_color[st_idx]
          )
        ),
        text = tooltip_text[st_idx],
        hoverinfo = "text",
        key = cl_data$designation[st_idx],
        name = st,
        legendgroup = st,
        showlegend = show_leg
      )
    }

    # ---- Layout: Y-axis label with cluster name ----
    p <- plotly::layout(
      p,
      xaxis = list(range = x_range, title = ""),
      yaxis = list(title = "", categoryorder = "array",
                   categoryarray = levels(cl_data$designation))
    )

    p
  })

  # ---- Combine subplots vertically ----
  fig <- plotly::subplot(
    plot_list,
    nrows = n_clusters,
    shareX = TRUE,
    titleY = TRUE,
    margin = 0.08
  )

  # ---- Add recommendation zone shapes and annotations ----
  shapes_list <- list()
  annotations_list <- list()

  for (i in seq_along(clusters)) {
    cl <- clusters[i]
    cl_thresh <- thresholds[thresholds$cluster == cl, , drop = FALSE]
    cl_data <- prepared_data[prepared_data$cluster == cl, , drop = FALSE]

    # Determine effective recommend threshold
    rec_thresh <- if (!is.null(user_recommend_threshold)) {
      user_recommend_threshold
    } else {
      cl_thresh$recommend_threshold
    }
    avoid_thresh <- cl_thresh$avoid_threshold

    # Y-axis domain for this subplot panel
    # plotly subplot distributes panels evenly with margins
    panel_height <- (1 - 0.08 * (n_clusters - 1)) / n_clusters
    # Panels are arranged top-to-bottom (first cluster at top)
    y1 <- 1 - (i - 1) * (panel_height + 0.08)
    y0 <- y1 - panel_height

    # Green recommendation zone: from recommend_threshold to x_max
    shapes_list <- c(shapes_list, list(list(
      type = "rect",
      xref = "x",
      yref = "paper",
      x0 = rec_thresh,
      x1 = x_range[2],
      y0 = y0,
      y1 = y1,
      fillcolor = "rgba(76, 175, 80, 0.12)",
      line = list(width = 0),
      layer = "below"
    )))

    # Red/grey avoid zone: from x_min to avoid_threshold
    shapes_list <- c(shapes_list, list(list(
      type = "rect",
      xref = "x",
      yref = "paper",
      x0 = x_range[1],
      x1 = avoid_thresh,
      y0 = y0,
      y1 = y1,
      fillcolor = "rgba(244, 67, 54, 0.08)",
      line = list(width = 0),
      layer = "below"
    )))

    # ---- Prescriptive header annotation ----
    # Top picks: designations in green zone (up to 3)
    top_picks <- cl_data[cl_data$mean_value >= rec_thresh, , drop = FALSE]
    top_picks <- top_picks[order(top_picks$mean_value, decreasing = TRUE), , drop = FALSE]
    top_picks_names <- head(as.character(top_picks$designation), 3)

    # Build header text
    cluster_label <- cl
    # Single-environment cluster warning
    n_envs <- cl_data$n_envs_in_cluster[1]
    caution_text <- if (!is.na(n_envs) && n_envs == 1) {
      " (1 site \u2014 interpret with caution)"
    } else {
      ""
    }

    if (length(top_picks_names) > 0) {
      header_text <- paste0(
        "<b>", cluster_label, caution_text, ": Top picks \u2014 ",
        paste(top_picks_names, collapse = ", "), "</b>"
      )
    } else {
      header_text <- paste0(
        "<b>", cluster_label, caution_text, ": No clear recommendations</b>"
      )
    }

    annotations_list <- c(annotations_list, list(list(
      x = 0.5,
      y = y1 + 0.02,
      xref = "paper",
      yref = "paper",
      text = header_text,
      showarrow = FALSE,
      font = list(size = 12),
      xanchor = "center",
      yanchor = "bottom"
    )))
  }

  # ---- Apply layout-level shapes and annotations ----
  # Compute dynamic height: minimum 350px per cluster panel, scaled by max designations
  max_desigs_in_cluster <- max(vapply(clusters, function(cl) {
    nrow(prepared_data[prepared_data$cluster == cl, , drop = FALSE])
  }, integer(1)))
  panel_height_px <- max(200, min(500, max_desigs_in_cluster * 25))
  total_height <- n_clusters * panel_height_px + 100  # 100px for margins/title
  total_height <- max(500, total_height)

  fig <- plotly::layout(
    fig,
    shapes = shapes_list,
    annotations = annotations_list,
    xaxis = list(range = x_range, title = "Mean Performance"),
    showlegend = TRUE,
    legend = list(orientation = "v", x = 1.02, xanchor = "left", y = 0.5, yanchor = "middle"),
    margin = list(t = 60, b = 40, r = 120),
    height = total_height
  )

  fig
}

#' Build cluster-colored trial location map
#'
#' Creates a plotly scatter map of trial locations colored by environment cluster.
#' If multiple years/seasons share the same physical site, dots are jittered.
#'
#' @param weather_summary A data.frame with environment, LON, LAT columns.
#' @param cluster_assignments Named character vector (environment -> cluster label).
#' @param tpe_period_data NULL or data.frame with environment, tpe_period columns.
#'
#' @return A plotly object with a map of trial locations.
#'
#' @noRd
build_cluster_map_plotly <- function(weather_summary, cluster_assignments, tpe_period_data = NULL) {

  # Get unique (environment, LON, LAT) from weather_summary
  loc_df <- unique(weather_summary[, c("environment", "LON", "LAT"), drop = FALSE])
  loc_df <- loc_df[is.finite(loc_df$LON) & is.finite(loc_df$LAT), , drop = FALSE]

  if (nrow(loc_df) == 0) {
    p <- plotly::plot_ly() %>% plotly::layout(
      annotations = list(list(
        text = "No location data available",
        x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE
      ))
    )
    return(p)
  }

  # Add cluster label
  loc_df$cluster <- cluster_assignments[as.character(loc_df$environment)]
  loc_df$cluster[is.na(loc_df$cluster)] <- "Unknown"

  # Add period info if available
  loc_df$period <- NA_character_
  if (!is.null(tpe_period_data) && nrow(tpe_period_data) > 0) {
    period_lookup <- unique(tpe_period_data[, c("environment", "tpe_period"), drop = FALSE])
    m <- match(loc_df$environment, period_lookup$environment)
    loc_df$period[!is.na(m)] <- period_lookup$tpe_period[m[!is.na(m)]]
  }

  # Jitter dots sharing same physical location (same LON/LAT within ~1km)
  loc_df$loc_key <- paste(round(loc_df$LON, 2), round(loc_df$LAT, 2), sep = "_")
  dup_locs <- loc_df$loc_key[duplicated(loc_df$loc_key)]
  if (length(dup_locs) > 0) {
    set.seed(123)
    for (key in unique(dup_locs)) {
      idx <- which(loc_df$loc_key == key)
      n <- length(idx)
      # Jitter longitude by 0.4 degrees per step (clearly visible at country scale)
      jitter_offsets <- seq(-(n - 1) / 2, (n - 1) / 2) * 0.4
      loc_df$LON[idx] <- loc_df$LON[idx] + jitter_offsets
    }
  }

  # Assign color by cluster (shared helper keeps the per-cluster tables in sync)
  unique_clusters <- unique(loc_df$cluster)
  cluster_color_map <- cluster_palette_map(cluster_assignments, weather_summary)
  # Cover any cluster present in loc_df but absent from the map (e.g. "Unknown")
  missing_cl <- setdiff(unique_clusters, names(cluster_color_map))
  if (length(missing_cl) > 0) {
    extra_idx <- (length(cluster_color_map) + seq_along(missing_cl) - 1L) %%
      length(CLUSTER_PALETTE) + 1L
    cluster_color_map <- c(cluster_color_map,
                           stats::setNames(CLUSTER_PALETTE[extra_idx], missing_cl))
  }
  loc_df$color <- cluster_color_map[loc_df$cluster]

  # Tooltip
  loc_df$tooltip <- mapply(function(env, cl, period, lon, lat) {
    txt <- paste0("<b>", env, "</b><br>Cluster: ", cl)
    if (!is.na(period) && nzchar(period)) {
      txt <- paste0(txt, "<br>Period: ", period)
    }
    txt <- paste0(txt, "<br>LON: ", sprintf("%.3f", lon), ", LAT: ", sprintf("%.3f", lat))
    txt
  }, loc_df$environment, loc_df$cluster, loc_df$period, loc_df$LON, loc_df$LAT,
  SIMPLIFY = TRUE, USE.NAMES = FALSE)

  # Build plotly geo figure (actual map with country borders)
  p <- plotly::plot_geo()

  # Add one trace per cluster for legend
  for (cl in unique_clusters) {
    cl_data <- loc_df[loc_df$cluster == cl, , drop = FALSE]
    p <- plotly::add_trace(
      p,
      data = cl_data,
      lat = ~LAT,
      lon = ~LON,
      type = "scattergeo",
      mode = "markers",
      marker = list(
        size = 12,
        color = cluster_color_map[cl],
        line = list(width = 1, color = "white")
      ),
      text = ~tooltip,
      hoverinfo = "text",
      name = cl,
      legendgroup = cl,
      showlegend = TRUE
    )
  }

  # Compute map center and zoom from data extent
  lon_center <- mean(loc_df$LON, na.rm = TRUE)
  lat_center <- mean(loc_df$LAT, na.rm = TRUE)
  lon_range <- diff(range(loc_df$LON, na.rm = TRUE))
  lat_range <- diff(range(loc_df$LAT, na.rm = TRUE))

  # Ensure all points are visible: use the actual data extent plus a fixed margin
  lon_min <- min(loc_df$LON, na.rm = TRUE)
  lon_max <- max(loc_df$LON, na.rm = TRUE)
  lat_min <- min(loc_df$LAT, na.rm = TRUE)
  lat_max <- max(loc_df$LAT, na.rm = TRUE)

  # Add 15% padding on each side, minimum 2 degrees
  lon_pad <- max(2, lon_range * 0.15)
  lat_pad <- max(2, lat_range * 0.15)

  # Force square extent: use the larger span for both axes
  lon_span <- (lon_max - lon_min) + 2 * lon_pad
  lat_span <- (lat_max - lat_min) + 2 * lat_pad
  max_span <- max(lon_span, lat_span)

  lon_center <- (lon_min + lon_max) / 2
  lat_center <- (lat_min + lat_max) / 2

  p <- plotly::layout(
    p,
    geo = list(
      scope = "world",
      showland = TRUE,
      landcolor = "rgb(243, 243, 243)",
      showocean = TRUE,
      oceancolor = "rgb(204, 229, 255)",
      showcountries = TRUE,
      countrycolor = "rgb(180, 180, 180)",
      countrywidth = 0.5,
      showlakes = TRUE,
      lakecolor = "rgb(204, 229, 255)",
      projection = list(type = "natural earth"),
      lonaxis = list(
        range = c(lon_center - max_span / 2, lon_center + max_span / 2)
      ),
      lataxis = list(
        range = c(lat_center - max_span / 2, lat_center + max_span / 2)
      )
    ),
    showlegend = TRUE,
    legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.05),
    margin = list(t = 10, b = 30, l = 0, r = 0)
  )

  p
}

#' Build the relatedness plotly visualization
#'
#' Constructs an interactive plotly figure showing genetic relatedness
#' structure among breeding candidates, with dendrogram/cluster grouping,
#' trait performance overlays, status-colored markers, diversity candidate
#' highlighting, and click event capture.
#'
#' @param plot_data List with elements: individuals (data.frame), trait_values (data.frame),
#'   groups (data.frame), dendrogram (hclust or NULL), mode (character), summary (list).
#' @param highlighted Character or NULL â€” currently highlighted designation.
#' @param source_id Character â€” plotly source identifier for event capture.
#'
#' @return A plotly object.
#'
#' @noRd
build_relatedness_plotly <- function(plot_data, highlighted = NULL,
                                     source_id = "relatedness_plot") {

  individuals <- plot_data$individuals
  trait_values <- plot_data$trait_values
  groups <- plot_data$groups

  # ---- Validate minimal input ----
  if (is.null(individuals) || nrow(individuals) == 0) {
    p <- plotly::plot_ly(source = source_id) %>%
      plotly::layout(
        annotations = list(list(
          text = "No individuals to display",
          x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE
        ))
      )
    return(p)
  }

  # ---- Order individuals by group_id and order_within ----
  individuals <- individuals[order(individuals$group_id, individuals$order_within), , drop = FALSE]
  # Create ordered factor for Y-axis (bottom to top = first to last)
  individuals$y_factor <- factor(
    individuals$designation,
    levels = individuals$designation
  )
  n_indiv <- nrow(individuals)

  # ---- Apply highlighting logic ----
  if (!is.null(highlighted) && highlighted %in% individuals$designation) {
    hl_idx <- which(individuals$designation == highlighted)
    non_hl_idx <- which(individuals$designation != highlighted)

    # Highlighted individual: larger marker, white solid border
    individuals$marker_size[hl_idx] <- MARKER_SIZE_HIGHLIGHTED
    individuals$border_style[hl_idx] <- "white-solid"

    # Fade non-highlighted markers
    individuals$marker_opacity[non_hl_idx] <- individuals$marker_opacity[non_hl_idx] * 0.3
  }

  # ---- Compute marker border properties ----
  marker_line_width <- rep(0, n_indiv)
  marker_line_color <- rep("rgba(0,0,0,0)", n_indiv)
  marker_line_dash <- rep("solid", n_indiv)

  gold_idx <- which(individuals$border_style == "gold-dashed")
  if (length(gold_idx) > 0) {
    marker_line_width[gold_idx] <- DIVERSITY_BORDER_WIDTH
    marker_line_color[gold_idx] <- DIVERSITY_BORDER_COLOR
  }

  white_idx <- which(individuals$border_style == "white-solid")
  if (length(white_idx) > 0) {
    marker_line_width[white_idx] <- HIGHLIGHT_BORDER_WIDTH
    marker_line_color[white_idx] <- HIGHLIGHT_BORDER_COLOR
  }

  # ---- Build tooltip text inline ----
  tooltip_text <- mapply(function(desig, grp, status, idx_val) {
    paste0(
      "<b>", desig, "</b><br>",
      "Group: ", grp, "<br>",
      "Status: ", status, "<br>",
      "Index: ", if (is.finite(idx_val)) sprintf("%.2f", idx_val) else "N/A"
    )
  }, individuals$designation, individuals$group_label, individuals$plot_status,
  individuals$index_value, SIMPLIFY = TRUE, USE.NAMES = FALSE)

  # ---- Compute plot height (scale with number of individuals) ----
  # Minimum 600px, scale at 25px per individual for readability, cap at 4000px
  plot_height <- max(600, min(4000, n_indiv * 25))

  # ---- Initialize plotly figure ----
  fig <- plotly::plot_ly(source = source_id)

  # ---- Track legend entries for decision statuses ----
  legend_shown_statuses <- character(0)

  # ---- Add trait BLUP values with error bars (X-axis) ----
  # Split by status so error bars get the correct single color per trace
  if (!is.null(trait_values) && nrow(trait_values) > 0) {
    unique_traits <- unique(trait_values$trait)

    for (tr in unique_traits) {
      tr_data <- trait_values[trait_values$trait == tr, , drop = FALSE]
      # Match to individuals ordering
      tr_match <- match(tr_data$designation, individuals$designation)
      tr_data <- tr_data[!is.na(tr_match), , drop = FALSE]
      tr_match <- tr_match[!is.na(tr_match)]

      if (nrow(tr_data) == 0) next

      has_error <- "std_error" %in% colnames(tr_data) && any(!is.na(tr_data$std_error))

      # Get status for each matched individual
      statuses <- individuals$plot_status[tr_match]
      unique_st <- unique(statuses)

      for (st in unique_st) {
        st_mask <- which(statuses == st)
        if (length(st_mask) == 0) next

        st_color <- individuals$marker_color[tr_match[st_mask[1]]]
        st_opacity <- individuals$marker_opacity[tr_match[st_mask]]

        error_x_config <- if (has_error) {
          list(type = "data", array = tr_data$std_error[st_mask], visible = TRUE, color = st_color)
        } else {
          NULL
        }

        # Build rich tooltip: name, family, trait value, reliability (for non-index)
        indiv_subset <- individuals[tr_match[st_mask], , drop = FALSE]
        # Use per-trait reliability from trait_values data when available
        trait_rel <- tr_data$reliability[st_mask]
        tooltip_texts <- paste0(
          "<b>", indiv_subset$designation, "</b><br>",
          "Family: ", indiv_subset$group_label, "<br>",
          tr, ": ", sprintf("%.2f", tr_data$value[st_mask]),
          ifelse(
            tr != "Selection_Index" & !is.na(trait_rel),
            paste0("<br>Reliability: ", sprintf("%.2f", trait_rel)),
            ""
          )
        )

        # Show legend on first trace per status
        show_leg <- !(st %in% legend_shown_statuses)
        if (show_leg) legend_shown_statuses <<- c(legend_shown_statuses, st)

        # Get marker border properties for diversity candidate highlighting
        matched_indices <- tr_match[st_mask]
        st_line_width <- marker_line_width[matched_indices]
        st_line_color <- marker_line_color[matched_indices]

        fig <- plotly::add_trace(
          fig,
          x = tr_data$value[st_mask],
          y = individuals$y_factor[tr_match[st_mask]],
          type = "scatter",
          mode = "markers",
          marker = list(
            color = st_color,
            size = 8,
            opacity = st_opacity,
            symbol = STATUS_SHAPES[st],
            line = list(
              width = st_line_width,
              color = st_line_color
            )
          ),
          error_x = error_x_config,
          hoverinfo = "text",
          text = tooltip_texts,
          name = st,
          legendgroup = st,
          showlegend = show_leg
        )
      }
    }
  }

  # ---- Add horizontal separator lines between groups ----
  shapes_list <- list()
  if (!is.null(groups) && nrow(groups) > 1) {
    # Find boundaries between groups
    group_boundaries <- which(diff(individuals$group_id) != 0)
    for (boundary in group_boundaries) {
      shapes_list <- c(shapes_list, list(list(
        type = "line",
        x0 = 0, x1 = 1,
        xref = "paper",
        y0 = boundary - 0.5, y1 = boundary - 0.5,
        yref = "y",
        line = list(color = "grey60", width = 1.5, dash = "dot"),
        layer = "below"
      )))
    }
  }

  # ---- Add pale blue ribbon for selected individuals' trait value range ----
  if (!is.null(trait_values) && nrow(trait_values) > 0) {
    selected_desigs <- individuals$designation[individuals$plot_status == "SELECTED"]
    selected_trait_vals <- trait_values$value[trait_values$designation %in% selected_desigs]
    selected_trait_vals <- selected_trait_vals[is.finite(selected_trait_vals)]

    if (length(selected_trait_vals) > 0) {
      sel_min <- min(selected_trait_vals)
      sel_max <- max(selected_trait_vals)

      shapes_list <- c(shapes_list, list(list(
        type = "rect",
        x0 = sel_min, x1 = sel_max,
        xref = "x",
        y0 = 0, y1 = 1,
        yref = "paper",
        fillcolor = "rgba(173, 216, 230, 0.25)",
        line = list(width = 0),
        layer = "below"
      )))
    }
  }

  # ---- Layout ----
  layout_args <- list(
    fig,
    xaxis = list(title = "Trait Value"),
    yaxis = list(
      title = "",
      categoryorder = "array",
      categoryarray = levels(individuals$y_factor),
      tickfont = list(size = if (n_indiv > 80) 9 else 11)
    ),
    shapes = shapes_list,
    showlegend = TRUE,
    legend = list(orientation = "v", x = 1.02, xanchor = "left", y = 0.5, yanchor = "middle"),
    margin = list(t = 40, b = 40, l = 150, r = 120),
    hovermode = "closest",
    height = plot_height
  )

  fig <- do.call(plotly::layout, layout_args)

  fig
}

#' Prepare beeswarm plot data
#'
#' Transforms raw STA predictions into a plot-ready data frame with status
#' colors and opacity values for the beeswarm (strip) plot.
#'
#' @param sta_long A data.frame with columns: designation, environment, trait,
#'   predictedValue, reliability.
#' @param review_df A data.frame with columns: designation, plot_status.
#' @param trait A single character string indicating the trait to filter on.
#' @param overrides NULL or a data.frame with columns: designation,
#'   plot_decision (values "SELECTED" or "NOT SELECTED").
#'
#' @return A data.frame with columns: designation, environment, predictedValue,
#'   reliability, plot_status, opacity, color. Rows with non-finite
#'   predictedValue are excluded, environments are ordered alphabetically.
#'
#' @noRd
prepare_beeswarm_data <- function(sta_long, review_df, trait, overrides = NULL) {
  # Fixed status-to-color mapping
  status_colors <- c(
    "SELECTED"     = "#0072B2",
    "NOT SELECTED" = "#D55E00",
    "REVISE"       = "#F9A825",
    "CHECK"        = "#C2185B"
  )


  # 1. Filter sta_long to selected trait
  df <- sta_long[sta_long$trait == trait, , drop = FALSE]


  # 2. Remove rows with non-finite predictedValue
  df <- df[is.finite(df$predictedValue), , drop = FALSE]


  # 3. Merge with review_df to get plot_status per designation

  # Only keep columns we need from review_df
  review_cols <- review_df[, c("designation", "plot_status"), drop = FALSE]
  df <- merge(
    df[, c("designation", "environment", "predictedValue", "reliability"), drop = FALSE],
    review_cols,
    by = "designation",
    all.x = TRUE
  )

  # 4. Apply overrides (if present) to update plot_status
  if (!is.null(overrides) && nrow(overrides) > 0) {
    override_match <- match(df$designation, overrides$designation)
    has_override <- !is.na(override_match)
    df$plot_status[has_override] <- overrides$plot_decision[override_match[has_override]]
  }

  # 5. Compute opacity via reliability_to_opacity()
  df$opacity <- reliability_to_opacity(df$reliability)

  # CHECK designations always get opacity = 1.0
  df$opacity[df$plot_status == "CHECK"] <- 1.0

  # 6. Assign color from fixed status-color mapping
  df$color <- status_colors[df$plot_status]

  # 7. Order environments alphabetically
  df$environment <- factor(df$environment, levels = sort(unique(df$environment)))
  df <- df[order(df$environment, df$designation), , drop = FALSE]

  # Reset row names

  rownames(df) <- NULL


  # Return plot-ready data.frame with required columns
  df[, c("designation", "environment", "predictedValue", "reliability",
         "plot_status", "opacity", "color"), drop = FALSE]
}

#' Build beeswarm (strip) plotly figure
#'
#' Creates an interactive plotly scatter plot with jittered points showing
#' genotype performance across environments. Supports click-to-highlight and
#' optional connect-dots mode for tracing a single genotype across locations.
#'
#' @param df A data.frame from \code{prepare_beeswarm_data()} with columns:
#'   designation, environment, predictedValue, reliability, plot_status, opacity, color.
#' @param trait_name Character string used as y-axis label.
#' @param highlighted NULL or a character string naming the designation to highlight.
#' @param connect_dots Logical; if TRUE and \code{highlighted} is not NULL,
#'   draw lines connecting the highlighted genotype's points across environments.
#' @param source_id Character string used as the plotly source for event capture.
#'
#' @return A plotly object.
#'
#' @noRd
build_beeswarm_plotly <- function(df, trait_name, highlighted = NULL,
                                   connect_dots = FALSE, source_id) {

  # Status-to-color mapping for legend

  status_colors <- c(
    "SELECTED"     = "#0072B2",
    "NOT SELECTED" = "#D55E00",
    "REVISE"       = "#F9A825",
    "CHECK"        = "#C2185B"
  )

  # Ensure environment is a factor for categorical x-axis

  if (!is.factor(df$environment)) {
    df$environment <- factor(df$environment, levels = sort(unique(df$environment)))
  }
  env_levels <- levels(df$environment)


  # Compute deterministic jitter based on row index within each environment
  # Use a seeded approach for stable jitter across re-renders
  df$env_numeric <- as.numeric(df$environment)
  df$jitter_offset <- vapply(seq_len(nrow(df)), function(i) {
    # Deterministic pseudo-random offset based on row index and designation
    seed_val <- abs(digest::digest2int(paste0(df$designation[i], df$environment[i])))
    set.seed(seed_val)
    stats::runif(1, -0.25, 0.25)
  }, numeric(1))
  df$x_jittered <- df$env_numeric + df$jitter_offset

  # Adjust opacity when a genotype is highlighted
  if (!is.null(highlighted)) {
    is_highlighted <- df$designation == highlighted
    # Dim non-highlighted points
    df$display_opacity <- ifelse(is_highlighted, df$opacity, df$opacity * 0.3)
    # Marker sizes: highlighted gets larger markers
    df$marker_size <- ifelse(is_highlighted, 12, 7)
  } else {
    df$display_opacity <- df$opacity
    df$marker_size <- 7
  }

  # Build tooltip text
  df$hover_text <- paste0(
    "<b>", df$designation, "</b><br>",
    "Environment: ", df$environment, "<br>",
    "Value: ", round(df$predictedValue, 3), "<br>",
    "Reliability: ", round(df$reliability, 3), "<br>",
    "Status: ", df$plot_status
  )

  # Create traces per status category for legend
  p <- plotly::plot_ly(source = source_id)

  for (status in names(status_colors)) {
    sub_df <- df[df$plot_status == status, , drop = FALSE]
    if (nrow(sub_df) == 0) next

    # Configure marker border for highlighted points
    if (!is.null(highlighted)) {
      line_colors <- ifelse(sub_df$designation == highlighted, "white", "rgba(0,0,0,0)")
      line_widths <- ifelse(sub_df$designation == highlighted, 2, 0)
    } else {
      line_colors <- rep("rgba(0,0,0,0)", nrow(sub_df))
      line_widths <- rep(0, nrow(sub_df))
    }

    p <- plotly::add_trace(
      p,
      x = sub_df$x_jittered,
      y = sub_df$predictedValue,
      type = "scatter",
      mode = "markers",
      marker = list(
        color = sub_df$color,
        opacity = sub_df$display_opacity,
        size = sub_df$marker_size,
        line = list(color = line_colors, width = line_widths)
      ),
      key = sub_df$designation,
      text = sub_df$hover_text,
      hoverinfo = "text",
      name = status,
      legendgroup = status,
      showlegend = TRUE
    )
  }

  # Connect dots: add line trace for highlighted genotype
  if (connect_dots && !is.null(highlighted)) {
    hl_df <- df[df$designation == highlighted, , drop = FALSE]
    if (nrow(hl_df) > 0) {
      # Order by environment factor levels
      hl_df <- hl_df[order(as.numeric(hl_df$environment)), , drop = FALSE]
      hl_color <- hl_df$color[1]

      p <- plotly::add_trace(
        p,
        x = hl_df$x_jittered,
        y = hl_df$predictedValue,
        type = "scatter",
        mode = "lines",
        line = list(color = hl_color, width = 2),
        hoverinfo = "skip",
        showlegend = FALSE
      )
    }
  }

  # Configure layout with categorical x-axis
  p <- plotly::layout(
    p,
    xaxis = list(
      tickvals = seq_along(env_levels),
      ticktext = env_levels,
      title = "",
      tickangle = -45
    ),
    yaxis = list(
      title = trait_name
    ),
    hovermode = "closest",
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = 1.05
    )
  )

  # Configure click event emission
  p <- plotly::config(p, displayModeBar = TRUE)

  p
}

#' Build environmental covariate barplot (plotly)
#'
#' Creates a bar chart showing an environmental covariate for each trial
#' environment, aligned on the same x-axis positions as the beeswarm plot
#' above. Uses numeric x positions with tickvals/ticktext for alignment.
#'
#' @param env_df A data.frame with columns: environment, covariate_value.
#' @param covariate_label Character string used as the y-axis label.
#' @param environments_order Character vector defining x-axis category order
#'   (must match the beeswarm plot's environment ordering).
#' @param cluster_assignments Named character vector mapping environments to
#'   cluster labels (optional). When provided, bars are color-coded by cluster
#'   membership. When NULL, all bars are steel grey (#7B8794).
#'
#' @return A plotly object (bar chart or empty plot with annotation if no data).
#'
#' @noRd
build_env_covariate_plotly <- function(env_df, covariate_label, environments_order,
                                       cluster_assignments = NULL) {


  # Fallback: if env_df is empty, return empty plotly with informative annotation
  if (is.null(env_df) || nrow(env_df) == 0) {
    p <- plotly::plot_ly() %>%
      plotly::layout(
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        annotations = list(
          list(
            text = "Environmental covariate data not available for these trials",
            x = 0.5,
            y = 0.5,
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            font = list(size = 14, color = "#666666")
          )
        )
      )
    return(p)
  }

  # Map environments to numeric x positions matching beeswarm layout
  env_df$x_pos <- match(env_df$environment, environments_order)

  # Remove environments not found in environments_order
  env_df <- env_df[!is.na(env_df$x_pos), , drop = FALSE]

  # Build tooltip text
  env_df$hover_text <- paste0(
    "<b>", env_df$environment, "</b><br>",
    covariate_label, ": ", round(env_df$covariate_value, 2)
  )

  # Determine bar colors based on cluster_assignments
  if (!is.null(cluster_assignments)) {
    # Palette for up to 3 clusters (expandable if needed)
    cluster_palette <- c("#4CAF50", "#2196F3", "#FF9800", "#9C27B0", "#F44336")
    unique_labels <- unique(cluster_assignments)
    # Ensure consistent ordering of cluster labels
    unique_labels <- sort(unique_labels)
    color_map <- stats::setNames(
      cluster_palette[seq_along(unique_labels)],
      unique_labels
    )

    # Look up each environment's cluster and assign color
    env_df$cluster <- cluster_assignments[env_df$environment]
    env_df$bar_color <- color_map[env_df$cluster]
    # Environments not found in cluster_assignments get steel grey

    env_df$bar_color[is.na(env_df$bar_color)] <- "#7B8794"

    # Build bar chart with per-bar colors and legend via one trace per cluster
    p <- plotly::plot_ly(height = 200)
    for (cl in unique_labels) {
      cl_data <- env_df[env_df$cluster == cl & !is.na(env_df$cluster), , drop = FALSE]
      if (nrow(cl_data) > 0) {
        p <- plotly::add_trace(
          p,
          data = cl_data,
          x = ~x_pos,
          y = ~covariate_value,
          type = "bar",
          marker = list(color = color_map[cl]),
          text = ~hover_text,
          hoverinfo = "text",
          name = cl,
          showlegend = TRUE
        )
      }
    }
    # Add any unclustered environments as a separate grey trace
    unclustered <- env_df[is.na(env_df$cluster), , drop = FALSE]
    if (nrow(unclustered) > 0) {
      p <- plotly::add_trace(
        p,
        data = unclustered,
        x = ~x_pos,
        y = ~covariate_value,
        type = "bar",
        marker = list(color = "#7B8794"),
        text = ~hover_text,
        hoverinfo = "text",
        name = "Unclustered",
        showlegend = TRUE
      )
    }
  } else {
    # Original single-color behavior (no legend)
    p <- plotly::plot_ly(
      data = env_df,
      x = ~x_pos,
      y = ~covariate_value,
      type = "bar",
      marker = list(color = "#7B8794"),
      text = ~hover_text,
      hoverinfo = "text",
      showlegend = FALSE,
      height = 200
    )
  }

  # Configure layout to align with beeswarm x-axis
  p <- plotly::layout(
    p,
    xaxis = list(
      tickvals = seq_along(environments_order),
      ticktext = environments_order,
      title = "",
      tickangle = -45
    ),
    yaxis = list(
      title = covariate_label
    ),
    margin = list(t = 10, b = 60)
  )

  p <- plotly::config(p, displayModeBar = TRUE)

  p
}

#' product advancement UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_preProdAdvApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    tags$head(
      tags$style(HTML("
        table.decision-table thead th {
          background-color: #2C3E50 !important;
          color: white !important;
          font-weight: 700 !important;
          border-bottom: 2px solid #1B2631 !important;
          white-space: nowrap !important;
        }

        table.decision-table tbody td {
          vertical-align: middle !important;
        }

        div.dataTables_scrollHead table.decision-table,
        div.dataTables_scrollBody table.decision-table {
          margin-top: 0 !important;
          margin-bottom: 0 !important;
        }

        div.dataTables_scrollBody {
          border-bottom: 1px solid #ddd;
        }
      "))
    ),

    shiny::mainPanel(width = 12,
                     tabsetPanel( id=ns("tabsMain"),
                                  type = "tabs",

                                  tabPanel(div(icon("book"), "Information") ,
                                           br(),
                                           column(width = 6,
                                                  h1(strong(span("Pre-advancement Selection Module", style="color:darkcyan"))),
                                                  h2(strong("Data Status (wait to be displayed):")),
                                                  uiOutput(ns("warningMessage")),
                                                  tags$br(),
                                                  shinyWidgets::prettySwitch( inputId = ns('launch'), label = "Load example dataset", status = "success"),
                                                  tags$br(),
                                                  img(src = "www/PAM_image.png",style = "width: 500px; height: auto;")
                                           ),
                                           column(width = 6,
                                                  tags$body(
                                                    h2(strong("Details")),
                                                    p("The Pre-advancement module is the first step of the Product Advancement workflow. It is designed to be run ",
                                                      strong("individually by each breeder or stakeholder"), " before the advancement meeting."),
                                                    p("This module compiles key information from different sources to support advancement decisions across breeding stages,
                                                      combining phenotypic, genetic, and decision-support metrics into a single interface."),
                                                    p(strong("Workflow:")),
                                                    tags$ol(
                                                      tags$li("Define selection parameters (weights, thresholds, candidate set)"),
                                                      tags$li("Run the initial selection"),
                                                      tags$li("Review results in the decision table and visualisations"),
                                                      tags$li("Review previous selection decisions and save your analysis")
                                                    ),
                                                    p(strong("After this step:"), " Save your results and share the RData file with other stakeholders.
                                                      Multiple saved selections can then be compared in the ", strong("Advancement Meeting Dashboard"), " tab
                                                      to reach a joint consensus during the advancement meeting."),

                                                    tags$hr(),
                                                    h3(strong("Technical Methods & Data Requirements")),

                                                    p(strong("Environment Clustering:"), " Environments (trial locations) are grouped by similarity in climate covariates \u2014 ",
                                                      "temperature, rainfall, and humidity indices \u2014 to identify mega-environments. ",
                                                      "This clustering helps visualize how candidates perform across different environmental conditions."),

                                                    p(strong("Family Grouping:"), " Genotypes are grouped by pedigree-based kinship (A-matrix hierarchical clustering) ",
                                                      "or genomic relatedness (G-matrix) when available. This helps ensure selected candidates represent diverse genetic backgrounds."),

                                                    p(strong("Reliability-Weighted Index:"), " The selection index penalizes poorly-estimated individuals using ",
                                                      "sqrt(reliability) dampening. A candidate with high BLUP but low reliability contributes less to their index score. "),

                                                    tags$hr(),
                                                    h3(strong("Selection Quality Assessment")),

                                                    p("Before running the selection, the module performs an integrated quality assessment for each trait. ",
                                                      "This assessment combines two dimensions: (1) trait reliability and (2) boundary separability at the current selection intensity."),

                                                    h4(strong("Dimension 1: Trait Reliability")),

                                                    p(strong("RSR (Ranking Separability Ratio):"), " Computed as SD(BLUPs) / mean(SE) using only candidate designations. ",
                                                      "Because BLUPs are shrunk estimates (pulled toward the population mean), the standard deviation of BLUPs is ",
                                                      "intentionally conservative. An RSR below 1 does not necessarily mean a trait is unusable \u2014 it means that ",
                                                      "prediction uncertainty is as large as the estimated genetic differences."),

                                                    p(strong("Reliability Metrics:"), " Mean reliability across candidates and the percentage of candidates with ",
                                                      "reliability below 0.20 are also computed. Reliability is bounded between 0 and 1, where higher values indicate ",
                                                      "more accurate predictions."),

                                                    p(strong("Trait Reliability Classification:")),
                                                    tags$ul(
                                                      tags$li(strong("High reliability:"), " RSR \u2265 1 AND mean reliability \u2265 0.40 AND fewer than 30% of candidates have reliability below 0.20."),
                                                      tags$li(strong("Moderate reliability:"), " RSR < 1 OR mean reliability between 0.20 and 0.40 OR between 30% and 60% of candidates have low reliability."),
                                                      tags$li(strong("Low reliability:"), " (RSR < 1 AND mean reliability < 0.20) OR more than 60% of candidates have reliability below 0.20.")
                                                    ),

                                                    h4(strong("Dimension 2: Boundary Separability")),

                                                    p("The boundary separability assessment evaluates whether entries near the selection cutoff can be reliably distinguished."),

                                                    p(strong("Index Standard Error:"), " For each designation, the approximate index SE is computed as: ",
                                                      "SE(I", tags$sub("i"), ") = \u221A\u2211(w", tags$sub("k"), "\u00B2 \u00D7 SE", tags$sub("ik"), "\u00B2), ",
                                                      "assuming independence between traits. This is a conservative approximation since the full prediction error variance (PEV) matrix is not available from MTA."),

                                                    p(strong("Boundary Window:"), " The up to 5 selected entries immediately above the selection cutoff and ",
                                                      "up to 5 rejected entries immediately below form the boundary window. ",
                                                      "Pairwise z", tags$sub("ij"), " = (I", tags$sub("i"), " - I", tags$sub("j"), ") / SED", tags$sub("ij"),
                                                      " is computed for all cross-boundary pairs, where SED", tags$sub("ij"), " = \u221A(SE(I", tags$sub("i"), ")\u00B2 + SE(I", tags$sub("j"), ")\u00B2)."),

                                                    p(strong("Boundary Separability Classification:")),
                                                    tags$ul(
                                                      tags$li(strong("Separable boundary:"), " Median z", tags$sub("ij"), " \u2265 2 and at most 50% of comparisons below 2. ",
                                                              "Per trait: mean reliability of boundary-window designations on that trait \u2265 0.30."),
                                                      tags$li(strong("Non-separable boundary:"), " Median z", tags$sub("ij"), " < 2 or more than 50% below 2. ",
                                                              "Per trait: mean reliability of boundary-window designations on that trait < 0.30.")
                                                    ),

                                                    h4(strong("Integrated Recommendation Matrix")),

                                                    p("Each trait receives one combined recommendation based on both dimensions:"),

                                                    tags$table(
                                                      style = "border-collapse: collapse; width:100%; margin:10px 0;",
                                                      tags$thead(
                                                        tags$tr(style = "background:#f0f0f0;",
                                                          tags$th(style = "border:1px solid #ddd; padding:6px;", "Reliability \\ Boundary"),
                                                          tags$th(style = "border:1px solid #ddd; padding:6px;", "Separable"),
                                                          tags$th(style = "border:1px solid #ddd; padding:6px;", "Non-separable")
                                                        )
                                                      ),
                                                      tags$tbody(
                                                        tags$tr(
                                                          tags$td(style = "border:1px solid #ddd; padding:6px;", strong("High")),
                                                          tags$td(style = "border:1px solid #ddd; padding:6px; background:#d4edda;", "Recommended to keep"),
                                                          tags$td(style = "border:1px solid #ddd; padding:6px; background:#fff3cd;", "Use with caution")
                                                        ),
                                                        tags$tr(
                                                          tags$td(style = "border:1px solid #ddd; padding:6px;", strong("Moderate")),
                                                          tags$td(style = "border:1px solid #ddd; padding:6px; background:#fff3cd;", "Use with caution"),
                                                          tags$td(style = "border:1px solid #ddd; padding:6px; background:#f8d7da;", "Recommended to exclude")
                                                        ),
                                                        tags$tr(
                                                          tags$td(style = "border:1px solid #ddd; padding:6px;", strong("Low")),
                                                          tags$td(style = "border:1px solid #ddd; padding:6px; background:#f8d7da;", "Recommended to exclude"),
                                                          tags$td(style = "border:1px solid #ddd; padding:6px; background:#f8d7da;", "Recommended to exclude")
                                                        )
                                                      )
                                                    ),

                                                    p(strong("Selection Intensity Context:"), " The recommendations are contextualized by the current selection intensity ",
                                                      "(percentage of individuals selected). A trait may have poor overall reliability but still contribute to a separable ",
                                                      "boundary at a given selection pressure. The assessment accounts for this by evaluating boundary separability specifically ",
                                                      "at the actual decision point."),

                                                    p(style = "font-style:italic; color:#666;",
                                                      "Note: This assessment uses approximate standard error propagation (assuming trait independence). ",
                                                      "The full PEV matrix is not available from MTA. The breeder always makes the final decision \u2014 ",
                                                      "the assessment provides guidance, not binding constraints."),

                                                    tags$hr(),

                                                    h4(strong("Mandatory Inputs:")),
                                                    tags$ul(
                                                      tags$li(strong("MTA analysis stamp"), " \u2014 BLUPs (Best Linear Unbiased Predictions) and reliabilities from Multi-Trial Analysis"),
                                                      tags$li(strong("STA analysis stamp"), " \u2014 Per-environment predictions from Single-Trial Analysis")
                                                    ),

                                                    h4(strong("Optional Inputs:")),
                                                    tags$ul(
                                                      tags$li(strong("Pedigree data"), " \u2014 enables the relatedness plot and family grouping"),
                                                      tags$li(strong("Genomic marker data"), " \u2014 enables genomic relatedness (G-matrix) for family grouping"),
                                                      tags$li(strong("Environmental covariates"), " \u2014 enables environment clustering and TPE (Target Population of Environments) plots")
                                                    )
                                                  )
                                           ),
                                  ),
                                  tabPanel(div(icon("arrow-right-to-bracket"), "Input steps"),
                                           tabsetPanel(
                                             id = ns("inputStepsTabs"),
                                             tabPanel(
                                               value = "pick_stamps",
                                               div(icon("dice-one"), "Pick analysis stamps", icon("arrow-right")),
                                               br(),

                                               column(
                                                 width = 12,
                                                 style = "background-color:grey; color: #FFFFFF",

                                                 column(
                                                   width = 6,
                                                   selectInput(
                                                     ns("staStamp"),
                                                     label = tags$span(
                                                       "STA version to use",
                                                       tags$i(
                                                         class = "glyphicon glyphicon-info-sign",
                                                         style = "color:#FFFFFF",
                                                         title = "Select the Single Trial Analysis result stamp to use as input for this product advancement workflow."
                                                       )
                                                     ),
                                                     choices = NULL,
                                                     multiple = FALSE
                                                   )
                                                 ),

                                                 column(
                                                   width = 6,
                                                   selectInput(
                                                     ns("mtaStamp"),
                                                     label = tags$span(
                                                       "MTA version to use",
                                                       tags$i(
                                                         class = "glyphicon glyphicon-info-sign",
                                                         style = "color:#FFFFFF",
                                                         title = "Select the Multi Trial Analysis result stamp to use as input for this product advancement workflow."
                                                       )
                                                     ),
                                                     choices = NULL,
                                                     multiple = FALSE
                                                   )
                                                 )
                                               ),

                                               br(),

                                               column(
                                                 width = 12,
                                                 style = "background-color:grey; color: #FFFFFF",

                                                 column(
                                                   width = 6,
                                                   selectInput(
                                                     ns("tppIdInput"),
                                                     label = tags$span(
                                                       "TPP ID (optional)",
                                                       tags$i(
                                                         class = "glyphicon glyphicon-info-sign",
                                                         style = "color:#FFFFFF",
                                                         title = "Optionally select a Target Product Profile to drive automated trait selection, weight assignment, and threshold configuration."
                                                       )
                                                     ),
                                                     choices = NULL,
                                                     multiple = FALSE
                                                   )
                                                 ),

                                                 column(
                                                   width = 6,
                                                   uiOutput(ns("tppValidationMessage"))
                                                 )
                                               ),

                                               br(),
                                             ),

                                             tabPanel(
                                               value = "global_options",
                                               div(icon("dice-two"), "Global options", icon("arrow-right")),
                                               br(),

                                               column(
                                                 width = 12,
                                                 style = "background-color:grey; color: #FFFFFF",

                                                 column(
                                                   width = 8,
                                                   selectInput(
                                                     ns("traitsToEvaluate"),
                                                     label = tags$span(
                                                       "Traits to evaluate",
                                                       tags$i(
                                                         class = "glyphicon glyphicon-info-sign",
                                                         style = "color:#FFFFFF",
                                                         title = "Select the traits that will be considered in the advancement decision."
                                                       )
                                                     ),
                                                     choices = NULL,
                                                     multiple = TRUE
                                                   )
                                                 ),
                                               ),

                                               br(),

                                               column(
                                                 width = 12,
                                                 style = "background-color:grey; color: #FFFFFF",

                                                 column(
                                                   width = 12,
                                                   uiOutput(ns("tppWeightError")),
                                                   uiOutput(ns("weightExplanation")),
                                                   uiOutput(ns("customWeightsUI"))
                                                 )
                                               ),

                                               br(),

                                               column(
                                                 width = 12,
                                                 style = "background-color:grey; color: #FFFFFF",

                                                 column(
                                                   width = 4,
                                                   radioButtons(
                                                     ns("selectionMode"),
                                                     label = tags$span(
                                                       "Selection intensity",
                                                       tags$i(
                                                         class = "glyphicon glyphicon-info-sign",
                                                         style = "color:#FFFFFF",
                                                         title = "Choose whether to select a percentage of individuals or a specific number."
                                                       )
                                                     ),
                                                     choices = c(
                                                       "Percentage of individuals" = "pct",
                                                       "Specific number of individuals" = "num"
                                                     ),
                                                     selected = "pct"
                                                   )
                                                 ),

                                                 column(
                                                   width = 3,
                                                   conditionalPanel(
                                                     condition = "input.selectionMode == 'pct'",
                                                     ns = ns,
                                                     numericInput(
                                                       ns("topPctSelected"),
                                                       label = "% of top individuals",
                                                       value = 20,
                                                       min = 1,
                                                       max = 100,
                                                       step = 1
                                                     )
                                                   ),
                                                   conditionalPanel(
                                                     condition = "input.selectionMode == 'num'",
                                                     ns = ns,
                                                     numericInput(
                                                       ns("nSelected"),
                                                       label = "Number of individuals",
                                                       value = 10,
                                                       min = 1,
                                                       step = 1
                                                     )
                                                   )
                                                 )
                                               ),

                                               br(),

                                               column(
                                                 width = 12,
                                                 style = "background-color:grey; color: #FFFFFF",

                                                 column(
                                                   width = 4,
                                                   selectInput(
                                                     ns("checkEntryTypeValue"),
                                                     label = tags$span(
                                                       "Value in entryType that corresponds to checks",
                                                       tags$i(
                                                         class = "glyphicon glyphicon-info-sign",
                                                         style = "color:#FFFFFF",
                                                         title = "Select which value in the entryType column identifies check varieties."
                                                       )
                                                     ),
                                                     choices = NULL,
                                                     multiple = FALSE
                                                   )
                                                 )
                                               ),

                                               br(),
                                             ),

                                             tabPanel(
                                               value = "trait_rules",
                                               div(icon("dice-three"), "Trait-specific thresholds", icon("arrow-right")),
                                               br(),

                                               column(
                                                 width = 12,
                                                 style = "background-color:grey; color: #FFFFFF; padding-top:15px;",

                                                 column(
                                                   width = 12,
                                                   tags$span(
                                                     "Use this step to set minimum standards (thresholds) for each trait. ",
                                                     "Candidates that do not meet these thresholds will be flagged as 'NOT SELECTED' regardless of their index value. ",
                                                     "Traits that do not require a minimum standard should remain with the default values (no threshold).",
                                                     tags$i(
                                                       class = "glyphicon glyphicon-info-sign",
                                                       style = "color:#FFFFFF",
                                                       title = "Define one threshold per selected trait. Leave as 'None' if no minimum standard is needed."
                                                     )
                                                   ),
                                                   tags$br(),
                                                   tags$br(),
                                                   checkboxInput(
                                                     ns("applySameRuleToAllTraits"),
                                                     label = "Apply same thresholds to all traits",
                                                     value = FALSE
                                                   )
                                                 )
                                               ),

                                               br(),

                                               uiOutput(ns("indexOverrideWarning")),

                                               shinydashboard::box(
                                                 width = 12,
                                                 status = "success",
                                                 solidHeader = TRUE,
                                                 collapsible = TRUE,
                                                 collapsed = FALSE,
                                                 title = "Trait threshold cards",

                                                 uiOutput(ns("traitRuleCards"))
                                               )
                                             ),

                                             tabPanel(
                                               value = "preselection",
                                               div(icon("dice-four"), "Pre-selection", icon("arrow-right")),
                                               br(),

                                               column(
                                                 width = 12,
                                                 style = "background-color:grey; color: #FFFFFF; padding:15px 0;",

                                                 column(
                                                   width = 12,
                                                   tags$span(
                                                     "To restrict the candidate set before generating the initial recommendation, choose one of the options below."
                                                   ),
                                                   tags$br(),
                                                   tags$br(),

                                                   radioButtons(
                                                     ns("candidateSelectionMode"),
                                                     label = "Candidate set definition",
                                                     choices = c(
                                                       "Use all designations" = "all",
                                                       "Select all designations from a specific selection stage" = "stage",
                                                       "Select candidate designations manually" = "manual"
                                                     ),
                                                     selected = "all"
                                                   )
                                                 )
                                               ),

                                               br(),

                                               column(
                                                 width = 12,
                                                 style = "background-color:grey; color: #FFFFFF; padding:15px 0;",

                                                 column(
                                                   width = 12,

                                                   conditionalPanel(
                                                     condition = "input.candidateSelectionMode == 'stage'",
                                                     ns = ns,

                                                     selectInput(
                                                       ns("selectionStage"),
                                                       label = tags$span(
                                                         "Selection stage",
                                                         tags$i(
                                                           class = "glyphicon glyphicon-info-sign",
                                                           style = "color:#FFFFFF",
                                                           title = "All designations present in the selected stage will be used as candidates."
                                                         )
                                                       ),
                                                       choices = NULL,
                                                       multiple = FALSE
                                                     )
                                                   ),

                                                   conditionalPanel(
                                                     condition = "input.candidateSelectionMode == 'manual'",
                                                     ns = ns,

                                                     selectizeInput(
                                                       ns("candidateDesignations"),
                                                       label = tags$span(
                                                         "Candidate designations",
                                                         tags$i(
                                                           class = "glyphicon glyphicon-info-sign",
                                                           style = "color:#FFFFFF",
                                                           title = "Select the subset of candidate designations to include in the initial recommendation."
                                                         )
                                                       ),
                                                       choices = NULL,
                                                       multiple = TRUE,
                                                       options = list(placeholder = "Search designations...")
                                                     )
                                                   )
                                                 )
                                               )
                                             ),

                                             tabPanel(
                                               value = "run_initial_selection",
                                               div( icon("dice-five"), "Run selection" ),
                                               br(),
                                               column(width=12,style = "background-color:grey; color: #FFFFFF",
                                                      column(width=3,
                                                             tags$div(textInput(ns("initSelectionIdName"),
                                                                                label = tags$span("Analysis Name for initial selection (optional)",
                                                                                                  tags$i( class = "glyphicon glyphicon-info-sign", style = "color:#FFFFFF",
                                                                                                          title = "An optional name for the analysis besides the timestamp if desired.") ),
                                                                                placeholder = "(optional name)")
                                                             )
                                                      ),
                                                      column(width=3,
                                                             br(),
                                                             actionButton(ns("runInitProdAdv"), "Run initial selection", icon = icon("play-circle")),
                                                             br(),
                                                             br(),
                                                      ),
                                               ),

                                               # fluidRow(column(3, verbatimTextOutput(ns("value"))))
                                             ),
                                           ) # end of tabset
                                  ),# end of input panel

                                  tabPanel(
                                    value = "review_output",
                                    div(icon("search-plus",style = "color:#6C5B7B;"), "Review output"),
                                    tabsetPanel(
                                      id = ns("reviewOutputTabs"),
                                      tabPanel(
                                        value = "selection_stamps",
                                        div(icon("tags"), "Initial selection stamp", icon("arrow-right")),
                                        br(),
                                        uiOutput(ns("assessmentWarningsUI")),

                                        column(
                                          width = 12,
                                          style = "background-color:grey; color: #FFFFFF; padding:15px 0;",

                                          column(
                                            width = 12,
                                            tags$span(
                                              "Select the initial selection stamp to review. This loads the selection results computed in the Input steps.",
                                              tags$i(
                                                class = "glyphicon glyphicon-info-sign",
                                                style = "color:#FFFFFF",
                                                title = "Choose one previously saved initial selection stamp."
                                              )
                                            )
                                          )
                                        ),

                                        br(),

                                        column(
                                          width = 12,
                                          style = "background-color:grey; color: #FFFFFF; padding:15px 0;",

                                          column(
                                            width = 4,
                                            selectInput(
                                              ns("initialSelectionStamp"),
                                              label = tags$span(
                                                "Initial selection stamp",
                                                tags$i(
                                                  class = "glyphicon glyphicon-info-sign",
                                                  style = "color:#FFFFFF",
                                                  title = "Select one previously saved initial selection stamp. This field is mandatory."
                                                )
                                              ),
                                              choices = NULL,
                                              multiple = FALSE
                                            )
                                          )
                                        ),

                                        br(),

                                        uiOutput(ns("excludedTraitsWarningUI")),

                                        shinydashboard::box(
                                          width = 12,
                                          status = "success",
                                          solidHeader = TRUE,
                                          collapsible = TRUE,
                                          collapsed = TRUE,
                                          title = "Modeling table",

                                          DT::DTOutput(ns("selectionModelingTable"))
                                        )
                                      ),

                                      tabPanel(
                                        value = "decision_table",
                                        div(icon("table"), "Decision table", icon("arrow-right")),
                                        br(),

                                        column(
                                          width = 12,
                                          style = "background-color:grey; color: #FFFFFF; padding:15px 0;",

                                          column(
                                            width = 6,
                                            selectInput(
                                              ns("tableSelectionStampLoad"),
                                              label = tags$span(
                                                "Load previous table selection (optional)",
                                                tags$i(
                                                  class = "glyphicon glyphicon-info-sign",
                                                  style = "color:#FFFFFF",
                                                  title = "Load a previously saved table selection to restore manual overrides."
                                                )
                                              ),
                                              choices = c("Start from initial selection" = "__none__"),
                                              selected = "__none__",
                                              multiple = FALSE
                                            )
                                          )
                                        ),

                                        br(),

                                        column(
                                          width = 12,
                                          uiOutput(ns("tableDecisionSummary"))
                                        ),

                                        br(),

                                        # TPP controls: maintain traits checkbox and compliance count
                                        column(
                                          width = 12,
                                          uiOutput(ns("tppDecisionTableControls"))
                                        ),

                                        shinydashboard::box(
                                          width = 12,
                                          status = "success",
                                          solidHeader = TRUE,
                                          collapsible = TRUE,
                                          collapsed = FALSE,
                                          title = "Selection decision table",

                                          DT::DTOutput(ns("tableDecisionDT"))
                                        ),

                                        br(),

                                        column(
                                          width = 12,
                                          style = "background-color:grey; color: #FFFFFF; padding:15px 0;",

                                          column(
                                            width = 4,
                                            textInput(
                                              ns("tableSelectionId"),
                                              label = tags$span(
                                                "Table selection name",
                                                tags$i(
                                                  class = "glyphicon glyphicon-info-sign",
                                                  style = "color:#FFFFFF",
                                                  title = "Assign a name to save the current table selection (with any manual overrides)."
                                                )
                                              ),
                                              placeholder = "e.g. table_selection_v1"
                                            )
                                          ),

                                          column(
                                            width = 3,
                                            br(),
                                            actionButton(
                                              ns("saveTableSelection"),
                                              "Save table selection",
                                              icon = icon("save")
                                            )
                                          )
                                        )
                                      ),


                                      tabPanel(
                                        div(icon("chart-column"), "Visualisations", icon("arrow-right")),
                                        br(),

                                        column(
                                          width = 12,
                                          style = "background-color:grey; color: #FFFFFF; padding:15px 0;",

                                          column(
                                            width = 6,
                                            selectInput(
                                              ns("vizTableSelectionStamp"),
                                              label = tags$span(
                                                "Table selection stamp",
                                                tags$i(
                                                  class = "glyphicon glyphicon-info-sign",
                                                  style = "color:#FFFFFF",
                                                  title = "Optionally load a table selection stamp to use its decisions in visualisations."
                                                )
                                              ),
                                              choices = c("Use initial selection" = "__none__"),
                                              selected = "__none__",
                                              multiple = FALSE
                                            )
                                          ),

                                          column(
                                            width = 6,
                                            selectInput(
                                              ns("plotSelectionStamp"),
                                              label = tags$span(
                                                "Plot selection stamp",
                                                tags$i(
                                                  class = "glyphicon glyphicon-info-sign",
                                                  style = "color:#FFFFFF",
                                                  title = "Optionally load a plot selection stamp to restore previous plot-based decisions."
                                                )
                                              ),
                                              choices = c("No plot selection" = "__none__"),
                                              selected = "__none__",
                                              multiple = FALSE
                                            )
                                          )
                                        ),

                                        br(),

                                        column(
                                          width = 12,
                                          style = "background-color:grey; color: #FFFFFF; padding:15px 0;",

                                          column(
                                            width = 12,
                                            tags$span(
                                              "Select which visualisations to display. Candidate status (selected/not selected/check) is shown in all plots. Marker opacity reflects prediction reliability.",
                                              tags$i(
                                                class = "glyphicon glyphicon-info-sign",
                                                style = "color:#FFFFFF",
                                                title = "Choose which interactive plots to generate for candidate review."
                                              )
                                            ),
                                            tags$br(),
                                            tags$br(),

                                            checkboxGroupInput(
                                              ns("reviewPlots"),
                                              label = "Visualisations to display",
                                                choices = c(
                                                  "Pair-wise trait scatterplot",
                                                  "Performance across locations",
                                                  "Relatedness plot"
                                                ),
                                              selected = NULL
                                            )
                                          )
                                        ),

                                        br(),

#                                        conditionalPanel(
#                                          condition = "input.reviewPlots && input.reviewPlots.includes('Radar plot')",
#                                          ns = ns,
#
#                                          column(
#                                            width = 12,
#                                            style = "background-color:grey; color: #FFFFFF; padding:15px 0;",
#
#                                            column(
#                                              width = 6,
#                                              uiOutput(ns("radarDesignationUI"))
#                                            )
#                                          ),
#
#                                          br()
#                                        ),
#
                                        conditionalPanel(
                                          condition = "input.reviewPlots && input.reviewPlots.includes('Performance heatmap across TPE')",
                                          ns = ns,

                                          column(
                                            width = 12,
                                            style = "background-color:grey; color: #FFFFFF; padding:15px 0;",

                                            column(
                                              width = 2,
                                              uiOutput(ns("tpePeriodUI"))
                                            ),

                                            column(
                                              width = 2,
                                              selectInput(
                                                ns("tpePhase"),
                                                label = tags$span(
                                                  "Growing period phase",
                                                  tags$i(
                                                    class = "glyphicon glyphicon-info-sign",
                                                    style = "color:#FFFFFF",
                                                    title = "Split the selected growing period into an early phase (first 40%) and a late phase (remaining 60%)."
                                                  )
                                                ),
                                                choices = c("Early", "Late"),
                                                selected = "Early"
                                              )
                                            ),

                                            column(
                                              width = 2,
                                              selectInput(
                                                ns("tpeEnvCovariate"),
                                                label = tags$span(
                                                  "Environmental covariate",
                                                  tags$i(
                                                    class = "glyphicon glyphicon-info-sign",
                                                    style = "color:#FFFFFF",
                                                    title = "Choose which summarized environmental covariate to interpolate across the TPE."
                                                  )
                                                ),
                                                choices = c(
                                                  "Mean temperature" = "mean_temperature",
                                                  "Heat stress index" = "heat_stress_index",
                                                  "Rainfall" = "rainfall",
                                                  "Rainfall distribution index" = "rainfall_distribution_index",
                                                  "Humidity" = "humidity"
                                                ),
                                                selected = "rainfall"
                                              )
                                            ),

                                            column(
                                              width = 3,
                                              uiOutput(ns("tpeDesignationUI"))
                                            ),

                                            column(
                                              width = 3,
                                              uiOutput(ns("tpeTraitUI"))
                                            ),
                                          ),

                                          br()
                                        ),

                                        br(),

                                        conditionalPanel(
                                          condition = "input.reviewPlots && input.reviewPlots.includes('BLUP/BLUE reliability intervals')",
                                          ns = ns,

                                          column(
                                            width = 12,
                                            style = "background-color:grey; color: #FFFFFF; padding:15px 0;",

                                            column(
                                              width = 4,
                                              uiOutput(ns("reliabilityTraitUI"))
                                            )
                                          ),

                                          br()
                                        ),

                                        column(
                                          width = 12,
                                          style = "background-color:grey; color: #FFFFFF; padding:15px 0;",

                                          column(
                                            width = 12,
                                            actionButton(
                                              ns("renderReviewPlots"),
                                              "Render selected plots",
                                              icon = icon("play-circle")
                                            )
                                          )
                                        ),

                                        br(),

                                        shinydashboard::box(
                                          width = 12,
                                          status = "success",
                                          solidHeader = TRUE,
                                          collapsible = TRUE,
                                          collapsed = FALSE,
                                          title = "Interactive visualisations",

                                          uiOutput(ns("reviewPlotsUI"))
                                        ),

                                        br(),

                                        shinydashboard::box(
                                          width = 12,
                                          status = "warning",
                                          solidHeader = TRUE,
                                          collapsible = TRUE,
                                          collapsed = FALSE,
                                          title = "Manual designation selection",

                                          column(
                                            width = 6,
                                            uiOutput(ns("manualDesignationSelectionUI"))
                                          ),

                                          column(
                                            width = 3,
                                            selectInput(
                                              ns("manualDesignationDecision"),
                                              "Assign decision",
                                              choices = c("SELECTED", "NOT SELECTED", "REVISE"),
                                              selected = "SELECTED",
                                              multiple = FALSE
                                            )
                                          ),

                                          column(
                                            width = 3,
                                            br(),
                                            actionButton(
                                              ns("applyManualDesignationDecision"),
                                              "Apply decision",
                                              icon = icon("check")
                                            ),
                                          )
                                        ),

                                        br(),

                                        column(
                                          width = 12,
                                          style = "background-color:grey; color: #FFFFFF; padding:15px 0;",

                                          column(
                                            width = 4,
                                            textInput(
                                              ns("plotSelectionId"),
                                              label = tags$span(
                                                "Plot selection name",
                                                tags$i(
                                                  class = "glyphicon glyphicon-info-sign",
                                                  style = "color:#FFFFFF",
                                                  title = "Assign a name to the current plot-adjusted selection."
                                                )
                                              ),
                                              placeholder = "e.g. LS_review_plot_v1"
                                            )
                                          ),

                                          column(
                                            width = 3,
                                            br(),
                                            actionButton(
                                              ns("savePlotSelection"),
                                              "Save selection",
                                              icon = icon("save")
                                            )
                                          )
                                        )
                                      ),

                                      tabPanel(
                                        value = "final_review",
                                        div(icon("flag-checkered"), "Final Review and Run selection"),
                                        br(),

                                        column(
                                          width = 12,
                                          style = "background-color:grey; color: #FFFFFF; padding:15px 0;",

                                          column(
                                            width = 4,
                                            selectInput(
                                              ns("reportInitialSelectionStamp"),
                                              label = "Initial selection stamp",
                                              choices = NULL,
                                              multiple = FALSE
                                            )
                                          ),

                                          column(
                                            width = 4,
                                            selectInput(
                                              ns("reportPlotSelectionStamp"),
                                              label = "Table selection stamp",
                                              choices = c("No table selection" = "__none__"),
                                              selected = "__none__",
                                              multiple = FALSE
                                            )
                                          ),

                                          column(
                                            width = 4,
                                            selectInput(
                                              ns("reportFinalSelectionStamp"),
                                              label = "Plot selection stamp",
                                              choices = c("No plot selection" = "__none__"),
                                              selected = "__none__",
                                              multiple = FALSE
                                            )
                                          )
                                        ),

                                        br(),

                                        # --- Summary bar ---
                                        uiOutput(ns("finalSummaryBar")),

                                        # --- Final decision table ---
                                        shinydashboard::box(
                                          width = 12,
                                          title = "Final Decision Table",
                                          status = "primary",
                                          solidHeader = TRUE,
                                          collapsible = TRUE,

                                          fluidRow(
                                            column(
                                              width = 4,
                                              selectInput(
                                                ns("finalTablePortion"),
                                                label = tags$span(
                                                  "Rows to display (ranked by index)",
                                                  tags$i(
                                                    class = "glyphicon glyphicon-info-sign",
                                                    title = "Limits how many rows the table shows. Saving the final selection always uses every evaluated individual, not just the displayed rows."
                                                  )
                                                ),
                                                choices = c(
                                                  "Top 50" = 50,
                                                  "Top 100" = 100,
                                                  "Top 200" = 200,
                                                  "All" = -1
                                                ),
                                                selected = 100
                                              )
                                            ),
                                            column(
                                              width = 8,
                                              uiOutput(ns("finalTablePortionNote"))
                                            )
                                          ),

                                          DT::DTOutput(ns("finalDecisionDT"))
                                        ),

                                        hr(),

                                        # --- Final Review: Population Statistics Section (Req 9) ---
                                        shinydashboard::box(
                                          width = 12,
                                          title = "Final Review: Selection Statistics",
                                          status = "info",
                                          solidHeader = TRUE,
                                          collapsible = TRUE,

                                          fluidRow(
                                            column(
                                              width = 12,
                                              uiOutput(ns("finalReviewSelPct"))
                                            )
                                          ),

                                          hr(),

                                          # One statistics table per trait
                                          # (Selected / All candidates / Checks)
                                          uiOutput(ns("finalReviewStatsUI")),

                                          hr(),

                                          # Distribution of selected candidates vs all candidates, per trait
                                          plotly::plotlyOutput(ns("finalReviewHistogram"), height = "auto"),

                                          hr(),

                                          # TPP compliance percentages (only shown when TPP active)
                                          DT::DTOutput(ns("finalReviewTppPctsDT"))
                                        ),

                                        # --- TPP Breakdown (collapsed box, only shown when TPP active) ---
                                        uiOutput(ns("finalReviewTppBreakdownBox")),

                                        hr(),

                                        # --- Save section ---
                                        column(
                                          width = 12,
                                          style = "background-color:grey; color: #FFFFFF; padding:15px;",
                                          column(
                                            width = 5,
                                            textInput(
                                              ns("finalAnalysisName"),
                                              label = tags$span(style = "color:white;", "Decision name (required)"),
                                              placeholder = "e.g., 2024_maize_final_v1"
                                            )
                                          ),
                                          column(
                                            width = 4,
                                            br(),
                                            actionButton(
                                              ns("saveFinalSelection"),
                                              "Save decision & generate final report",
                                              icon = icon("save"),
                                              class = "btn-success"
                                            )
                                          )
                                        ),

                                        br(),
                                        textOutput(ns("outProdAdv_Raw"))
                                      ),
                                    )
                                  ),

                                  tabPanel(div(icon("arrow-right-from-bracket"), "Output tabs" ) , value = "outputTabs",
                                           tabsetPanel(
                                             tabPanel("Dashboard", icon = icon("file-image"),
                                                      br(),
                                                      textOutput(ns("outProdAdv_Raw2")),
                                                      br(),
                                                      actionButton(ns("renderReportProdAdv"), "Download dashboard", icon = icon("download")),
                                                      downloadButton(ns("downloadReportProdAdv"), "Download dashboard", style = "visibility:hidden;"),
                                                      br(),
                                                      uiOutput(ns('reportProdAdv')),
                                                      tags$hr(),
                                                      h3(strong("Population Statistics")),
                                                      # Same per-trait tables and distribution
                                                      # plot as the Final Review tab
                                                      uiOutput(ns("dashStatsUI")),
                                                      plotly::plotlyOutput(ns("dashStatsHistogram"), height = "auto"),
                                                      tags$hr(),
                                                      h3(strong("TPP Breakdown")),
                                                      uiOutput(ns("tppBreakdownIntro")),
                                                      DT::DTOutput(ns("tppDashboardBreakdown")),
                                                      uiOutput(ns("tppBreakdownEnvNotes")),
                                                      tags$hr(),
                                                      h3(strong("Global Options Summary")),
                                                      uiOutput(ns("tppOptionsIntro")),
                                                      DT::DTOutput(ns("tppDashboardOptionsTable")),
                                                      # Buffer so the dashboard doesn't end abruptly
                                                      tags$div(style = "height:120px;")
                                             ),
                                           ),
                                  ),
                     )) # end mainpanel

  )
}

#' qaRawApp Server Functions
#'
#' @noRd
mod_preProdAdvApp_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # ------------------------------------------------------------------
    # Pre-register the plotly event sources this module listens on.
    #
    # plotly::event_data() checks its registry from inside a
    # session$onFlushed() callback, i.e. after the calling frame has already
    # returned, so the "event ... is not registered" warning cannot be caught
    # with suppressWarnings() at the call site. event_register() on the plot
    # only populates the registry once that plot has actually rendered, which
    # is too late for the first flush: the listeners are created at session
    # start, long before the user opens the Visualisations tab.
    #
    # Seeding the registry here makes the deferred check pass from the outset.
    # eventID format mirrors plotly: paste(event, source, sep = "-").
    local({
      ids <- c(
        paste("plotly_click",   ns("pairwiseScatter"),   sep = "-"),
        paste("plotly_hover",   ns("tpePerformanceMap"), sep = "-"),
        paste("plotly_unhover", ns("tpePerformanceMap"), sep = "-"),
        paste("plotly_click",   "relatedness_plot",      sep = "-")
      )
      # Written on the root scope: a module's session is a session_proxy whose
      # `$` delegates unknown names to its parent, so plotly's
      # session$userData resolves to this same environment.
      root <- session$rootScope()
      root$userData$plotlyShinyEventIDs <- unique(
        c(root$userData$plotlyShinyEventIDs, ids)
      )
    })

    #############################################################################
    #Helper functions
    sanitize_trait_id <- function(x) {
      gsub("[^A-Za-z0-9_]", "_", x)
    }

    # Derive trait direction from weight sign
    #
    # Returns "Lower is better" for negative weights, "Higher is better" for
    # positive weights, and NULL for zero/NA/non-finite values.
    #
    # @param weight Numeric scalar weight value.
    # @return Character string or NULL.
    derive_direction_from_weight <- function(weight) {
      if (is.null(weight) || !is.finite(weight) || weight == 0) return(NULL)
      if (weight < 0) "Lower is better" else "Higher is better"
    }

    # Compute final decisions by applying the decision hierarchy
    #
    # Merge logic: plot_selection > table_selection > initial_selection.
    # For each designation, the most recent override wins.
    #
    # @param initial_decisions Data frame with columns: designation, initial_decision.
    # @param table_decisions Data frame with columns: designation, table_decision.
    #   May be NULL if no table stamp selected.
    # @param plot_decisions Data frame with columns: designation, plot_decision.
    #   May be NULL if no plot stamp selected.
    # @return Data frame with columns: designation, initial_decision,
    #   table_decision, plot_decision, final_decision.
    compute_final_decisions <- function(initial_decisions,
                                        table_decisions = NULL,
                                        plot_decisions = NULL) {
      result <- initial_decisions

      if (!is.null(table_decisions) && nrow(table_decisions) > 0) {
        result <- merge(result, table_decisions, by = "designation", all.x = TRUE)
      } else {
        result$table_decision <- NA_character_
      }

      if (!is.null(plot_decisions) && nrow(plot_decisions) > 0) {
        result <- merge(result, plot_decisions, by = "designation", all.x = TRUE)
      } else {
        result$plot_decision <- NA_character_
      }

      # Apply hierarchy: plot > table > initial
      result$final_decision <- ifelse(
        !is.na(result$plot_decision),
        result$plot_decision,
        ifelse(
          !is.na(result$table_decision),
          result$table_decision,
          result$initial_decision
        )
      )

      result
    }

    # Build trait distribution per status plot using plotly
    #
    # Creates a faceted strip/jitter plot with one horizontal panel per trait.
    # X-axis = trait value, markers = genotypes colored by status group.
    # Reference lines: check mean, selected mean, unselected mean, threshold.
    #
    # @param plot_df Data frame with columns: designation, trait, value, status.
    #   One row per designation x trait combination.
    # @param trait_directions Named list: trait_name -> "Higher is better" or "Lower is better".
    # @param thresholds Named list: trait_name -> numeric threshold value (or NULL).
    # @return A plotly object.
    build_trait_distribution_plotly <- function(plot_df, trait_directions, thresholds = list()) {
      req_cols <- c("designation", "trait", "value", "status")
      stopifnot(all(req_cols %in% colnames(plot_df)))

      traits <- unique(plot_df$trait)
      n_traits <- length(traits)

      # Create subplot list (one per trait)
      subplot_list <- lapply(seq_along(traits), function(i) {
        tr <- traits[i]
        tr_df <- plot_df[plot_df$trait == tr, , drop = FALSE]
        direction <- trait_directions[[tr]]
        hib <- is.null(direction) || identical(direction, "Higher is better")

        # Compute reference means
        selected_mean <- mean(tr_df$value[tr_df$status == "SELECTED"], na.rm = TRUE)
        not_sel_mean <- mean(tr_df$value[tr_df$status == "NOT SELECTED"], na.rm = TRUE)
        check_mean <- mean(tr_df$value[tr_df$status == "CHECK"], na.rm = TRUE)
        threshold_val <- thresholds[[tr]]

        # Build trace per status group
        p <- plotly::plot_ly()
        for (st in c("SELECTED", "NOT SELECTED", "REVISE", "CHECK")) {
          st_df <- tr_df[tr_df$status == st, , drop = FALSE]
          if (nrow(st_df) == 0) next
          p <- plotly::add_trace(p,
            x = st_df$value,
            y = jitter(rep(0, nrow(st_df)), amount = 0.3),
            type = "scatter", mode = "markers",
            marker = list(
              color = STATUS_COLORS[st],
              size = 8,
              symbol = STATUS_SHAPES[st]
            ),
            text = paste0(st_df$designation, ": ", round(st_df$value, 3)),
            hoverinfo = "text",
            name = st,
            legendgroup = st,
            showlegend = (i == 1)
          )
        }

        # Reference lines
        shapes <- list()
        if (is.finite(selected_mean)) {
          shapes <- c(shapes, list(list(
            type = "line", x0 = selected_mean, x1 = selected_mean,
            y0 = -0.5, y1 = 0.5, line = list(color = STATUS_COLORS["SELECTED"], dash = "dash")
          )))
        }
        if (is.finite(not_sel_mean)) {
          shapes <- c(shapes, list(list(
            type = "line", x0 = not_sel_mean, x1 = not_sel_mean,
            y0 = -0.5, y1 = 0.5, line = list(color = STATUS_COLORS["NOT SELECTED"], dash = "dash")
          )))
        }
        if (is.finite(check_mean)) {
          shapes <- c(shapes, list(list(
            type = "line", x0 = check_mean, x1 = check_mean,
            y0 = -0.5, y1 = 0.5, line = list(color = STATUS_COLORS["CHECK"], dash = "dot")
          )))
        }
        if (!is.null(threshold_val) && is.finite(threshold_val)) {
          shapes <- c(shapes, list(list(
            type = "line", x0 = threshold_val, x1 = threshold_val,
            y0 = -0.5, y1 = 0.5, line = list(color = "black", dash = "solid", width = 2)
          )))
        }

        # Direction annotation
        arrow_text <- if (hib) "\u2192 better" else "\u2190 better"

        p <- plotly::layout(p,
          xaxis = list(title = tpp_display_name(tr)),
          yaxis = list(visible = FALSE, range = c(-0.6, 0.6)),
          shapes = shapes,
          annotations = list(list(
            x = 1, y = 1, xref = "paper", yref = "paper",
            text = arrow_text, showarrow = FALSE,
            font = list(size = 11, color = "#666")
          ))
        )

        p
      })

      # Combine as vertical subplots
      plotly::subplot(subplot_list, nrows = n_traits, shareX = FALSE, titleY = FALSE)
    }

    parse_weather_time <- function(x, tz = "UTC") {
      x <- as.character(x)
      x <- trimws(x)
      x[x == ""] <- NA_character_

      x_clean <- sub(" GMT$", "", x)

      out <- rep(as.POSIXct(NA, tz = tz), length(x_clean))

      formats <- c(
        "%Y-%m-%d %H:%M:%S",     # after removing GMT
        "%Y-%m-%d %H:%M",
        "%Y-%m-%dT%H:%M:%S%z",
        "%Y-%m-%dT%H:%M:%SZ",
        "%Y-%m-%dT%H:%M:%S",
        "%m/%d/%Y %H:%M:%S",
        "%m/%d/%Y %H:%M",
        "%d/%m/%Y %H:%M:%S",
        "%d/%m/%Y %H:%M"
      )

      for (fmt in formats) {
        idx <- which(!is.na(x_clean) & is.na(out))
        if (length(idx) == 0) break

        parsed <- suppressWarnings(
          as.POSIXct(x_clean[idx], format = fmt, tz = tz)
        )

        ok <- !is.na(parsed)
        if (any(ok)) {
          out[idx[ok]] <- parsed[ok]
        }
      }

      out
    }

    parse_weather_date <- function(x) {
      x <- as.character(x)
      x <- trimws(x)
      x[x == ""] <- NA_character_

      parsed <- suppressWarnings(
        as.Date(
          x,
          tryFormats = c(
            "%Y-%m-%d",
            "%m/%d/%Y",
            "%d/%m/%Y",
            "%Y/%m/%d"
          )
        )
      )

      parsed
    }

    make_stamp_choices <- function(df) {
      stamps <- unique(df$analysisId)

      if (length(stamps) == 0) {
        return(character(0))
      }

      stamps_chr <- as.character(stamps)

      if ("analysisIdName" %in% colnames(df)) {
        lbl_df <- unique(df[, c("analysisId", "analysisIdName"), drop = FALSE])
        lbl_df$analysisId <- as.character(lbl_df$analysisId)

        labels <- paste(
          lbl_df$analysisIdName,
          as.POSIXct(as.numeric(lbl_df$analysisId), origin = "1970-01-01", tz = "GMT"),
          sep = "_"
        )

        out <- stamps_chr
        names(out) <- labels[match(stamps_chr, lbl_df$analysisId)]
        return(out)
      } else {
        out <- stamps_chr
        names(out) <- as.character(as.POSIXct(as.numeric(stamps_chr), origin = "1970-01-01", tz = "GMT"))
        return(out)
      }
    }


    build_decision_table_data <- function(dt,
                                          initial_stamp,
                                          table_stamp = "__none__",
                                          plot_stamp = "__none__",
                                          final_stamp = "__none__",
                                          final_overrides = NULL) {

      base_df <- cgiarPipeline::build_prodadv_decision_table_data(
        dt = dt,
        initial_stamp = initial_stamp,
        table_stamp = table_stamp,
        plot_stamp = plot_stamp,
        final_stamp = final_stamp,
        final_overrides = final_overrides
      )

      selected_traits <- grep("_trait_decision$", colnames(base_df), value = TRUE)
      selected_traits <- sub("_trait_decision$", "", selected_traits)

      display_df <- data.frame(
        designation = base_df$designation,
        stringsAsFactors = FALSE
      )

      for (t in selected_traits) {
        display_df[[t]] <- mapply(
          make_value_badge,
          base_df[[t]],
          base_df[[paste0(t, "_trait_decision")]],
          SIMPLIFY = TRUE,
          USE.NAMES = FALSE
        )
      }

      display_df$initial_decision <- sapply(
        base_df$initial_decision,
        make_decision_badge
      )

      display_df$table_decision <- sapply(
        base_df$table_decision,
        make_decision_badge
      )

      display_df$plot_decision <- sapply(
        base_df$plot_decision,
        make_decision_badge
      )

      display_df$final_decision <- mapply(
        make_final_cell,
        base_df$designation,
        base_df$final_decision_initial,
        base_df$initial_decision,
        SIMPLIFY = TRUE,
        USE.NAMES = FALSE
      )

      list(
        raw = base_df,
        display = display_df
      )
    }

    get_prodadv_period_cols <- function(dt) {
      mt_pheno <- dt$metadata$pheno

      year_col <- mt_pheno$value[
        mt_pheno$parameter == "year"
      ]

      season_col <- mt_pheno$value[
        mt_pheno$parameter == "season"
      ]

      list(
        year_col = if (length(year_col) > 0) year_col[1] else NULL,
        season_col = if (length(season_col) > 0) season_col[1] else NULL
      )
    }

    sample_reliability_plot_df <- function(df, max_per_status = 250, seed = 123) {
      if (!"plot_status" %in% names(df)) {
        return(df)
      }

      set.seed(seed)

      split_df <- split(df, df$plot_status, drop = TRUE)

      sampled <- lapply(split_df, function(x) {
        if (nrow(x) <= max_per_status) {
          return(x)
        }

        x[sample(seq_len(nrow(x)), max_per_status), , drop = FALSE]
      })

      do.call(rbind, sampled)
    }



    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })

    # Track which trait directions were auto-set by weight observer (trait_id -> TRUE/FALSE)
    direction_auto_set <- reactiveVal(list())

    # TPP Helper: Build tpp_criteria list from the tpp_filtered_traits data.frame
    # TPP Integration: Track selected TPP ID and filtered traits (Task 10.1)
    tpp_id_prepam <- reactiveVal(NULL)
    tpp_filtered_traits <- reactiveVal(NULL)

    # Helper: look up the TPP ID stored in the modeling table for a given
    # initial selection stamp. Returns NULL when none is recorded.
    # which() is used so NA values in the modeling table cannot produce
    # phantom all-NA rows from logical subsetting.
    tpp_stored_id_for_stamp <- function(dt, stamp) {
      if (is.null(dt) || is.null(dt$modeling)) return(NULL)
      if (is.null(stamp) || length(stamp) != 1 || is.na(stamp) || !nzchar(stamp)) return(NULL)
      m <- dt$modeling
      if (!all(c("analysisId", "module", "parameter", "value") %in% colnames(m))) return(NULL)
      idx <- which(
        as.character(m$analysisId) == as.character(stamp) &
          m$module == "Init_prodAdv" &
          m$parameter == "tpp_id"
      )
      if (length(idx) == 0) return(NULL)
      v <- m$value[idx[1]]
      if (is.na(v) || !nzchar(v)) return(NULL)
      v
    }

    # Helper: Get TPP display name for a trait.
    # If the trait matches a pheno_trait or tpp_trait in the filtered TPP data,
    # return the tpp_trait name (human-readable). Otherwise return the trait as-is.
    tpp_display_name <- function(trait) {
      filtered <- tpp_filtered_traits()
      if (is.null(filtered) || !is.data.frame(filtered) || nrow(filtered) == 0) {
        return(trait)
      }
      if (!"tpp_trait" %in% colnames(filtered) || !"pheno_trait" %in% colnames(filtered)) {
        return(trait)
      }
      # First check if trait IS already a tpp_trait name (e.g., "Virus resistance")
      idx <- which(filtered$tpp_trait == trait)
      if (length(idx) > 0) return(filtered$tpp_trait[idx[1]])
      # Otherwise look up by pheno_trait
      idx <- which(filtered$pheno_trait == trait)
      if (length(idx) > 0) return(filtered$tpp_trait[idx[1]])
      return(trait)
    }

    ############################################################################
    # show shinyWidgets until the user can use the module
    observeEvent(c(data(), input$staStamp, input$mtaStamp, input$traitsToEvaluate), {
      req(data())
      mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
      if(mappedColumns == 3 & length(input$staStamp)>0 & length(input$mtaStamp)>0 & length(input$traitsToEvaluate)){
        golem::invoke_js('showid', ns('holder1'))
      }else{
        golem::invoke_js('hideid', ns('holder1'))
      }
    })
    ############################################################################
    #Information Tab
    #################
    ## data example loading
    observeEvent(
      input$launch,
      if(length(input$launch) > 0){
        if (input$launch) {
          shinyWidgets::ask_confirmation(
            inputId = ns("myconfirmation"),
            text = "Are you sure you want to load the example data? This will delete any data currently in the environment.",
            title = "Data replacement warning"
          )
        }
      }
    )
    observeEvent(input$myconfirmation, {
      if (isTRUE(input$myconfirmation)) {
        shinybusy::show_modal_spinner('fading-circle', text = 'Loading example...')
        ## replace tables
        tmp <- data()
        data(cgiarBase::create_getData_object())
        utils::data(DT_example, package = "cgiarPipeline")
        if(!is.null(result$data)){tmp$data <- result$data}
        if(!is.null(result$metadata)){tmp$metadata <- result$metadata}
        if(!is.null(result$modifications)){tmp$modifications <- result$modifications}
        if(!is.null(result$predictions)){tmp$predictions <- result$predictions}
        if(!is.null(result$metrics)){tmp$metrics <- result$metrics}
        if(!is.null(result$modeling)){tmp$modeling <- result$modeling}
        if(!is.null(result$status)){tmp$status <- result$status}
        data(tmp) # update data with results
        shinybusy::remove_modal_spinner()
      }else{
        shinyWidgets::updatePrettySwitch(session, "launch", value = FALSE)
      }
    }, ignoreNULL = TRUE)

    # warning message
    output$warningMessage <- renderUI(
      if(is.null(data())){
        HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your phenotypic data using the 'Data Retrieval' tab.")) )
      }else{ # data is there
        mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
        if(mappedColumns == 3){
          if( any( c("mta","mtaAsr","mtaFlex","mtaLmms","mas") %in% data()$status$module ) ){
            HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to the Input tabs.")) )
          }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please perform a Multi-Trial Analysis before Product Advancement")) ) }
        }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please make sure that you have computed the 'environment' column, and that column 'designation' and \n at least one trait have been mapped using the 'Data Retrieval' tab.")) )}
      }
    )

    ############################################################################
    #Input Tabs
    #################


    ########################################
    #Select stamps (idx1)
    ########################################

    observeEvent(c(data()), {
      req(data())
      dt <- data()
      dt <- dt$status

      dtSta <- dt[which(dt$module == "sta"),]
      stampsSta <- unique(dtSta$analysisId)

      dtMta <- dt[which(dt$module %in% c("mta","mtaAsr","mtaFlex","mtaLmms","mas") ),]
      stampsMta <- unique(dtMta$analysisId)

      dtIdxD <- dt[which(dt$module %in% c("indexD")),]
      stampsIdxD <- unique(dtIdxD$analysisId)

      if(length(stampsSta) > 0){
        if("analysisIdName" %in% colnames(dtSta)){
          names(stampsSta) <- paste(dtSta$analysisIdName, as.POSIXct(stampsSta, origin="1970-01-01", tz="GMT"), sep = "_")
        }else{
          names(stampsSta) <- as.character(as.POSIXct(stampsSta, origin="1970-01-01", tz="GMT"))
        }
      }

      if(length(stampsMta) > 0){
        if("analysisIdName" %in% colnames(dtMta)){
          names(stampsMta) <- paste(dtMta$analysisIdName, as.POSIXct(stampsMta, origin="1970-01-01", tz="GMT"), sep = "_")
        }else{
          names(stampsMta) <- as.character(as.POSIXct(stampsMta, origin="1970-01-01", tz="GMT"))
        }
      }

      if(length(stampsIdxD) > 0){
        if("analysisIdName" %in% colnames(dtIdxD)){
          names(stampsIdxD) <- paste(dtIdxD$analysisIdName, as.POSIXct(stampsIdxD, origin="1970-01-01", tz="GMT"), sep = "_")
        }else{
          names(stampsIdxD) <- as.character(as.POSIXct(stampsIdxD, origin="1970-01-01", tz="GMT"))
        }
      }

      updateSelectInput(session, "staStamp", choices = stampsSta)
      updateSelectInput(session, "mtaStamp", choices = stampsMta)
      # indexStamp removed in Phase 2 redesign
    })

    ########################################
    # TPP ID selection and validation (Task 10.1)
    ########################################

    # Populate TPP ID dropdown when data is loaded
    observeEvent(c(data()), {
      req(data())
      dt <- data()
      tpp_ids <- tpp_get_tpp_ids(dt)
      # Add empty option at beginning so it's optional
      tpp_choices <- c("(none)" = "", stats::setNames(tpp_ids, tpp_ids))
      updateSelectInput(session, "tppIdInput", choices = tpp_choices, selected = "")
    })

    # Validate TPP ID and filter traits when TPP ID or stamps change
    observeEvent(c(input$tppIdInput, input$mtaStamp, input$staStamp), {
      req(data())
      dt <- data()
      selected_tpp <- input$tppIdInput

      # If TPP ID is empty, clear TPP state and allow progression.
      # Exception: when the selected initial selection stamp has a TPP recorded
      # in the modeling table, the Review-output recovery observer owns the TPP
      # state — don't wipe it just because this dropdown hasn't been touched.
      if (is.null(selected_tpp) || selected_tpp == "") {
        if (!is.null(tpp_stored_id_for_stamp(dt, isolate(input$initialSelectionStamp)))) {
          return()
        }
        tpp_id_prepam(NULL)
        tpp_filtered_traits(NULL)
        return()
      }

      # Validate TPP ID exists in metadata
      tpp_ids <- tpp_get_tpp_ids(dt)
      if (!selected_tpp %in% tpp_ids) {
        tpp_id_prepam(NULL)
        tpp_filtered_traits(NULL)
        return()
      }

      # Get TPP traits data
      validation <- tpp_validate_metadata(dt, selected_tpp)
      if (!validation$valid) {
        tpp_id_prepam(NULL)
        tpp_filtered_traits(NULL)
        return()
      }

      tpp_entry <- dt$metadata$TPP[[selected_tpp]]
      tpp_traits_df <- tpp_entry$traits

      # Get available phenotypic columns from the selected MTA stamp
      available_traits <- character(0)
      if (!is.null(input$mtaStamp) && nchar(input$mtaStamp) > 0) {
        dtPred <- dt$predictions
        dtPred <- dtPred[which(dtPred$analysisId %in% input$mtaStamp), ]
        available_traits <- unique(dtPred$trait)
      }

      # Filter TPP traits to those whose pheno_trait is in available columns
      if (nrow(tpp_traits_df) > 0 && "pheno_trait" %in% colnames(tpp_traits_df)) {
        mapped_mask <- !is.na(tpp_traits_df$pheno_trait) &
          tpp_traits_df$pheno_trait %in% available_traits
        filtered_df <- tpp_traits_df[mapped_mask, , drop = FALSE]
      } else {
        filtered_df <- tpp_traits_df[0, , drop = FALSE]
      }

      # Enrich filtered_df with category and desired_direction from raw TPP data
      # (data()$data$TPP[[tpp_id]]) since metadata$TPP$traits only stores
      # tpp_trait and pheno_trait columns.
      if (nrow(filtered_df) > 0 && !"category" %in% colnames(filtered_df)) {
        raw_tpp <- dt$data$TPP[[selected_tpp]]
        if (!is.null(raw_tpp) && is.data.frame(raw_tpp)) {
          # Map "Trait Requirement" to category
          if ("Trait Requirement" %in% colnames(raw_tpp) && "Trait Name" %in% colnames(raw_tpp)) {
            # Match by tpp_trait name to raw TPP "Trait Name"
            match_idx <- match(filtered_df$tpp_trait, raw_tpp[["Trait Name"]])

            # Convert requirement text to standardized category
            raw_req <- raw_tpp[["Trait Requirement"]][match_idx]
            filtered_df$category <- ifelse(
              grepl("Improve", raw_req, ignore.case = TRUE), "Essential_Improve",
              ifelse(grepl("Maintain", raw_req, ignore.case = TRUE), "Essential_Maintain",
                     ifelse(grepl("Nice", raw_req, ignore.case = TRUE), "Nice_To_Have", NA_character_))
            )

            # Parse "Desired Score" text into structured threshold data
            if ("Desired Score" %in% colnames(raw_tpp)) {
              raw_score <- raw_tpp[["Desired Score"]][match_idx]

              # Parse each desired score text
              parsed_scores <- lapply(raw_score, tpp_parse_desired_score)

              filtered_df$desired_direction <- vapply(parsed_scores, function(p) {
                if (!is.na(p$direction)) p$direction else NA_character_
              }, character(1))

              filtered_df$score_type <- vapply(parsed_scores, function(p) {
                if (!is.na(p$score_type)) p$score_type else NA_character_
              }, character(1))

              filtered_df$desired_lower <- vapply(parsed_scores, function(p) {
                if (!is.na(p$lower_bound)) p$lower_bound else NA_real_
              }, numeric(1))

              filtered_df$desired_upper <- vapply(parsed_scores, function(p) {
                if (!is.na(p$upper_bound)) p$upper_bound else NA_real_
              }, numeric(1))

              filtered_df$pct_above_check <- vapply(parsed_scores, function(p) {
                if (!is.na(p$pct_above_check)) p$pct_above_check else NA_real_
              }, numeric(1))

              filtered_df$pct_of_check <- vapply(parsed_scores, function(p) {
                if (!is.na(p$pct_of_check)) p$pct_of_check else NA_real_
              }, numeric(1))

              filtered_df$desired_score_text <- raw_score
            } else {
              filtered_df$desired_direction <- NA_character_
              filtered_df$score_type <- NA_character_
              filtered_df$desired_lower <- NA_real_
              filtered_df$desired_upper <- NA_real_
              filtered_df$pct_above_check <- NA_real_
              filtered_df$pct_of_check <- NA_real_
              filtered_df$desired_score_text <- NA_character_
            }

            # Also parse and store scale bounds for out-of-range validation
            if ("Scale Option" %in% colnames(raw_tpp)) {
              raw_scales <- raw_tpp[["Scale Option"]][match_idx]
              parsed_scales <- lapply(raw_scales, tpp_parse_scale_option)
              filtered_df$scale_min <- vapply(parsed_scales, function(p) {
                if (!is.null(p) && !is.null(p$min)) p$min else NA_real_
              }, numeric(1))
              filtered_df$scale_max <- vapply(parsed_scales, function(p) {
                if (!is.null(p) && !is.null(p$max)) p$max else NA_real_
              }, numeric(1))
            } else {
              filtered_df$scale_min <- NA_real_
              filtered_df$scale_max <- NA_real_
            }
          }
        }
      }

      # Store validated TPP ID and filtered traits
      if (nrow(filtered_df) > 0) {
        tpp_id_prepam(selected_tpp)
        tpp_filtered_traits(filtered_df)
      } else {
        # TPP ID is valid but no traits map to available columns
        tpp_id_prepam(selected_tpp)
        tpp_filtered_traits(NULL)
      }
    })

    # Render TPP validation message
    output$tppValidationMessage <- renderUI({
      selected_tpp <- input$tppIdInput

      # No message needed when field is empty
      if (is.null(selected_tpp) || selected_tpp == "") {
        return(NULL)
      }

      req(data())
      dt <- data()

      # Check if TPP ID is recognized
      tpp_ids <- tpp_get_tpp_ids(dt)
      if (!selected_tpp %in% tpp_ids) {
        return(
          tags$div(
            style = "color: #FF6B6B; font-weight: bold; margin-top: 25px;",
            icon("exclamation-triangle"),
            "TPP ID not recognized"
          )
        )
      }

      # Check if TPP has mapped traits
      filtered <- tpp_filtered_traits()
      if (is.null(filtered) || (is.data.frame(filtered) && nrow(filtered) == 0)) {
        return(
          tags$div(
            style = "color: #FF6B6B; font-weight: bold; margin-top: 25px;",
            icon("exclamation-triangle"),
            "No mapped traits found"
          )
        )
      }

      # Valid TPP with mapped traits - show success message
      n_traits <- nrow(filtered)
      tags$div(
        style = "color: #90EE90; font-weight: bold; margin-top: 25px;",
        icon("check-circle"),
        paste0("TPP loaded: ", n_traits, " mapped trait(s) available")
      )
    })

    ########################################
    #Global options (idx2)
    ########################################

    # Track whether TPP auto-assignment has been applied to avoid re-applying on every change
    tpp_auto_applied <- reactiveVal(FALSE)
    # Track the TPP ID that was used for the last auto-assignment
    tpp_auto_applied_id <- reactiveVal(NULL)

    # Helper: Build named choices vector for traitsToEvaluate selectInput.
    # Names = TPP display names, Values = internal trait identifiers.
    # When no TPP is active, names == values.
    tpp_build_named_choices <- function(traitsProdAdv) {
      filtered <- tpp_filtered_traits()
      display_names <- vapply(traitsProdAdv, function(t) tpp_display_name(t), character(1))
      stats::setNames(traitsProdAdv, display_names)
    }

    observeEvent(c(data(), input$mtaStamp), {
      req(data())
      req(input$mtaStamp)
      dt <- data()
      dtPred <- dt$predictions
      dtPred <- dtPred[which(dtPred$analysisId %in% input$mtaStamp),]
      traitsProdAdv <- unique(dtPred$trait)
      named_choices <- tpp_build_named_choices(traitsProdAdv)

      # If TPP auto-selection has already been applied, preserve the current selection
      # to avoid a race condition where this observer wipes out the TPP-driven selection.
      if (isTRUE(tpp_auto_applied())) {
        current_selected <- input$traitsToEvaluate
        # Keep only traits that are still valid choices
        valid_selected <- intersect(current_selected, traitsProdAdv)
        updateSelectInput(session, "traitsToEvaluate",
                          choices = named_choices,
                          selected = valid_selected)
      } else {
        updateSelectInput(session, "traitsToEvaluate", choices = named_choices)
      }
    })

    # TPP auto-select traits observer (Req 4.1): Triggered when the user navigates to the
    # Global Options tab OR when tpp_filtered_traits changes while already on that tab.
    # The core issue is that updateSelectInput is silently ignored if the target input
    # hasn't been rendered yet (i.e., the tab hasn't been visited). So we must wait until
    # the inputStepsTabs value is "global_options" before applying the auto-selection.
    observeEvent(c(tpp_filtered_traits(), input$inputStepsTabs), {
      # Only proceed when we're on the global_options tab (input exists at that point)
      req(input$inputStepsTabs == "global_options")

      filtered <- tpp_filtered_traits()
      current_tpp <- tpp_id_prepam()

      # If TPP is cleared or has no mapped traits, reset auto-applied flag
      if (is.null(current_tpp) || is.null(filtered) ||
          !is.data.frame(filtered) || nrow(filtered) == 0) {
        tpp_auto_applied(FALSE)
        tpp_auto_applied_id(NULL)
        return()
      }

      # Only auto-select if we haven't already applied for this TPP ID
      if (isTRUE(tpp_auto_applied()) && identical(tpp_auto_applied_id(), current_tpp)) {
        return()
      }

      # Use tpp_select_traits to determine which traits to auto-select
      if (!"category" %in% colnames(filtered)) {
        return()
      }

      # Rebuild the full choices list from the MTA predictions so we can pass
      # choices + selected atomically in a single updateSelectInput call.
      req(data())
      req(input$mtaStamp)
      dt <- data()
      dtPred <- dt$predictions
      dtPred <- dtPred[which(dtPred$analysisId %in% input$mtaStamp), ]
      traitsProdAdv <- unique(dtPred$trait)

      selection_mask <- tpp_select_traits(filtered)
      selected_pheno_traits <- filtered$pheno_trait[selection_mask]
      selected_pheno_traits <- selected_pheno_traits[!is.na(selected_pheno_traits)]

      # Also include MTA traits that match TPP trait names directly
      # (e.g., MTA has a trait "Virus resistance" and TPP also has "Virus resistance")
      selected_tpp_names <- filtered$tpp_trait[selection_mask]
      selected_tpp_names <- selected_tpp_names[!is.na(selected_tpp_names)]
      # Combine pheno_trait values and any tpp_trait names that are actual MTA traits
      all_selected <- unique(c(selected_pheno_traits, selected_tpp_names))

      # Ensure selected traits are actually in the available choices
      selected_pheno_traits <- intersect(all_selected, traitsProdAdv)
      named_choices <- tpp_build_named_choices(traitsProdAdv)

      if (length(selected_pheno_traits) > 0) {
        updateSelectInput(session, "traitsToEvaluate",
                          choices = named_choices,
                          selected = selected_pheno_traits)
      }

      # Mark that auto-assignment has been applied for this TPP ID
      tpp_auto_applied(TRUE)
      tpp_auto_applied_id(current_tpp)
    }, ignoreNULL = FALSE)

    # Reset auto-applied state when TPP ID input is cleared
    observeEvent(input$tppIdInput, {
      selected_tpp <- input$tppIdInput
      if (is.null(selected_tpp) || selected_tpp == "") {
        tpp_auto_applied(FALSE)
        tpp_auto_applied_id(NULL)
      }
    }, ignoreInit = TRUE)

    # Helper: get the TPP-assigned weight for a given trait (pheno_trait or tpp_trait name)
    # Returns NULL if no TPP or trait not found in filtered traits
    tpp_get_weight_for_trait <- function(pheno_trait) {
      filtered <- tpp_filtered_traits()
      if (is.null(filtered) || !is.data.frame(filtered) || nrow(filtered) == 0) {
        return(NULL)
      }
      if (!all(c("pheno_trait", "category", "desired_direction") %in% colnames(filtered))) {
        return(NULL)
      }
      # Try matching by pheno_trait first
      match_idx <- which(filtered$pheno_trait == pheno_trait)

      # If not found by pheno_trait, try matching by tpp_trait name
      # (handles cases like MTA trait "Virus resistance" matching TPP trait name directly)
      if (length(match_idx) == 0 && "tpp_trait" %in% colnames(filtered)) {
        match_idx <- which(filtered$tpp_trait == pheno_trait)
      }

      if (length(match_idx) == 0) return(NULL)

      row <- filtered[match_idx[1], , drop = FALSE]
      # Use tpp_assign_weights on the single-row data.frame
      row_with_weight <- tpp_assign_weights(row)
      return(row_with_weight$weight)
    }

    output$customWeightsUI <- renderUI({
      req(input$traitsToEvaluate)
      req(length(input$traitsToEvaluate) > 0)

      trait_list <- input$traitsToEvaluate
      current_tpp <- tpp_id_prepam()
      filtered <- tpp_filtered_traits()
      has_tpp <- !is.null(current_tpp) && !is.null(filtered) &&
        is.data.frame(filtered) && nrow(filtered) > 0

      tagList(
        tags$div(
          style = "margin-top: 5px;",
          lapply(trait_list, function(trait) {
            safe_trait <- gsub("[^A-Za-z0-9_]", "_", trait)
            display_label <- tpp_display_name(trait)

            # Determine initial weight value: use TPP weight if available, else default to 1
            initial_weight <- 1
            if (has_tpp) {
              tpp_weight <- tpp_get_weight_for_trait(trait)
              if (!is.null(tpp_weight)) {
                initial_weight <- tpp_weight
              }
            }

            numericInput(
              inputId = ns(paste0("weight_", safe_trait)),
              label = display_label,
              value = initial_weight,
              step = 0.1
            )
          })
        )
      )
    })

    # TPP auto-weight error display: show error when TPP ID is set but no traits mapped (Req 4.5)
    output$weightExplanation <- renderUI({
      current_tpp <- tpp_id_prepam()
      filtered <- tpp_filtered_traits()
      has_tpp <- !is.null(current_tpp) && !is.null(filtered) &&
        is.data.frame(filtered) && nrow(filtered) > 0

      if (has_tpp) {
        tags$div(
          style = "background-color: #1a5276; border-left: 4px solid #3498db; padding: 10px 14px; margin-bottom: 10px; border-radius: 4px;",
          tags$span(
            style = "color: #85c1e9; font-weight:600;",
            icon("magic"),
            " Weights auto-assigned from TPP"
          ),
          tags$p(
            style = "color: #D6EAF8; margin-top: 6px; margin-bottom: 0; font-size: 0.92em; line-height: 1.5;",
            "Essential: Improve traits receive +1 (higher is better) or \u22121 (lower is better) ",
            "depending on the desired direction defined in the TPP. ",
            "Essential: Maintain and Nice-to-Have traits receive 0 (they are evaluated via thresholds, not the index). ",
            "You can manually override any weight."
          )
        )
      } else {
        tagList(
          tags$p(style = "color:#FFFFFF; font-weight:bold;",
                 "Trait weights for selection index"),
          tags$p(style = "color:#FFFFFF;",
                 "Assign a weight to each trait to compute a custom selection index. ",
                 "Higher absolute values indicate greater importance. ",
                 "Use negative weights for traits where lower values are preferred. ",
                 "Default weight is 1 for all traits.")
        )
      }
    })

    # TPP auto-weight error display: show error when TPP ID is set but no traits mapped (Req 4.5)
    output$tppWeightError <- renderUI({
      current_tpp <- tpp_id_prepam()
      selected_tpp_input <- input$tppIdInput

      # Only show error when a TPP ID is selected but has no mapped traits
      if (is.null(selected_tpp_input) || selected_tpp_input == "") {
        return(NULL)
      }

      filtered <- tpp_filtered_traits()
      if (!is.null(filtered) && is.data.frame(filtered) && nrow(filtered) > 0) {
        return(NULL)
      }

      # TPP ID is set but no mapped traits - display error
      tags$div(
        style = "background-color: #641E16; border-left: 4px solid #E74C3C; padding: 8px 12px; margin-bottom: 10px; border-radius: 4px;",
        tags$span(
          style = "color: #F5B7B1;",
          icon("exclamation-triangle"),
          " TPP has no mapped traits available. Auto-assignment cannot proceed. Please clear the TPP ID or map traits in the TPP module."
        )
      )
    })

    observe({
      req(data())
      req(input$mtaStamp)

      dt <- data()
      dtPred <- dt$predictions

      dtPred <- dtPred[dtPred$analysisId %in% input$mtaStamp, ]

      entryType_column <- dt$metadata$pheno
      entryType_column <- entryType_column[entryType_column$parameter == "entryType","value"]

      entry_type_values <- dt$data$pheno[,entryType_column]
      entry_type_values <- unique(entry_type_values[!is.na(entry_type_values)])
      entry_type_values <- toupper(trimws(as.character(entry_type_values)))

      updateSelectInput(
        session,
        "checkEntryTypeValue",
        choices = c("No checks / not applicable" = "", entry_type_values),
        selected = ""
      )
    })

    # minTraitsPass observer removed in Phase 2 (decision logic always uses weighted index)

    ########################################
    #Trait-specific thresholds (idx3)
    ########################################

    output$indexOverrideWarning <- renderUI({
      div(
        style = "
        background-color: #fff3cd;
        color: #856404;
        border-left: 5px solid #ffeeba;
        padding: 12px;
        margin-bottom: 15px;
        border-radius: 4px;
      ",

        tags$strong("Note: "),
        "Trait-specific thresholds override the selection index. ",
        "If you define a threshold (e.g. minimum yield), candidates failing that threshold will be marked as 'NOT SELECTED' ",
        "even if they have high index values. ",
        tags$br(),
        tags$br(),
        "Only set thresholds when you need to enforce hard constraints. Traits without thresholds will only be used in the index calculation."
      )
    })

    # TPP Integration: Track auto-fill state for Essential_Maintain traits (Task 10.3)
    tpp_autofill_state <- reactiveVal(list())

    output$traitRuleCards <- renderUI({
      req(input$traitsToEvaluate)
      req(length(input$traitsToEvaluate) > 0)

      trait_list <- input$traitsToEvaluate

      # Get TPP data if available
      tpp_data <- tpp_filtered_traits()
      has_tpp <- !is.null(tpp_data) && is.data.frame(tpp_data) && nrow(tpp_data) > 0

      tagList(
        lapply(trait_list, function(trait_name) {
          safe_trait <- gsub("[^A-Za-z0-9_]", "_", trait_name)

          # --- TPP category lookup ---
          tpp_category <- NULL
          tpp_score_type <- NULL
          if (has_tpp && "pheno_trait" %in% colnames(tpp_data)) {
            trait_row <- tpp_data[tpp_data$pheno_trait == trait_name, , drop = FALSE]
            # Also try matching by tpp_trait name if pheno_trait doesn't match
            if (nrow(trait_row) == 0 && "tpp_trait" %in% colnames(tpp_data)) {
              trait_row <- tpp_data[tpp_data$tpp_trait == trait_name, , drop = FALSE]
            }
            if (nrow(trait_row) > 0) {
              if ("category" %in% colnames(tpp_data)) {
                tpp_category <- trait_row$category[1]
              }
              if ("score_type" %in% colnames(tpp_data)) {
                tpp_score_type <- trait_row$score_type[1]
              }
            }
          }

          # --- Build TPP category badge ---
          tpp_badge_ui <- NULL
          if (!is.null(tpp_category) && !is.na(tpp_category)) {
            badge_color <- switch(
              tpp_category,
              "Essential_Improve" = "background-color:#337ab7; color:white;",
              "Essential_Maintain" = "background-color:#5cb85c; color:white;",
              "Nice_To_Have" = "background-color:#999999; color:white;",
              "background-color:#999999; color:white;"
            )
            badge_label <- gsub("_", " ", tpp_category)
            tpp_badge_ui <- tags$span(
              style = paste0(
                badge_color,
                " padding: 3px 8px; border-radius: 4px; font-size: 11px;",
                " font-weight: bold; margin-left: 10px; display: inline-block;"
              ),
              badge_label
            )
          }

          # --- "Use TPP desired scores" checkbox (Essential_Maintain only) ---
          tpp_checkbox_ui <- NULL
          tpp_scale_warning_ui <- NULL

          # Check if desired score is out of scale range (applies to all categories)
          out_of_scale <- FALSE
          if (has_tpp && exists("trait_row") && nrow(trait_row) > 0) {
            s_min <- if ("scale_min" %in% colnames(trait_row)) trait_row$scale_min[1] else NA_real_
            s_max <- if ("scale_max" %in% colnames(trait_row)) trait_row$scale_max[1] else NA_real_
            s_type <- if ("score_type" %in% colnames(trait_row)) trait_row$score_type[1] else NA_character_
            d_lower <- if ("desired_lower" %in% colnames(trait_row)) trait_row$desired_lower[1] else NA_real_
            d_upper <- if ("desired_upper" %in% colnames(trait_row)) trait_row$desired_upper[1] else NA_real_
            d_text <- if ("desired_score_text" %in% colnames(trait_row)) trait_row$desired_score_text[1] else ""

            if (!is.na(s_min) && !is.na(s_max) && !is.na(s_type) && s_type == "absolute") {
              if ((!is.na(d_lower) && is.finite(d_lower) && d_lower > s_max) ||
                  (!is.na(d_upper) && is.finite(d_upper) && d_upper < s_min)) {
                out_of_scale <- TRUE
                tpp_scale_warning_ui <- tags$div(
                  style = "color:#721c24; background-color:#f8d7da; border-left:5px solid #f5c6cb; padding:10px; margin-bottom:10px; border-radius:4px;",
                  tags$strong(icon("exclamation-triangle"), " Out of scale: "),
                  sprintf(
                    "The TPP desired score (\"%s\") is outside the trait's scale (%s to %s) and cannot be used as a threshold.",
                    d_text, s_min, s_max
                  )
                )
              }
            }
          }

          if (!is.null(tpp_category) && !is.na(tpp_category) &&
              tpp_category == "Essential_Maintain" && !out_of_scale) {

            # For relative desired scores, don't show checkbox if no checks are mapped
            can_autofill <- TRUE
            if (has_tpp && exists("trait_row") && nrow(trait_row) > 0) {
              tr_score_type <- if ("score_type" %in% colnames(trait_row)) trait_row$score_type[1] else NA_character_
              if (!is.na(tr_score_type) && tr_score_type == "relative") {
                checks_available <- NULL
                current_tpp_id <- tpp_id_prepam()
                if (!is.null(current_tpp_id)) {
                  dt_meta <- data()$metadata$TPP[[current_tpp_id]]
                  if (!is.null(dt_meta) && !is.null(dt_meta$checks_per_trait)) {
                    tpp_trait_name <- trait_row$tpp_trait[1]
                    checks_available <- dt_meta$checks_per_trait[[tpp_trait_name]]
                  }
                }
                if (is.null(checks_available) || length(checks_available) == 0) {
                  can_autofill <- FALSE
                  d_text_rel <- if ("desired_score_text" %in% colnames(trait_row)) trait_row$desired_score_text[1] else ""
                  tpp_scale_warning_ui <- tags$div(
                    style = "color:#856404; background-color:#fff3cd; border-left:5px solid #ffeeba; padding:10px; margin-bottom:10px; border-radius:4px;",
                    tags$strong(icon("exclamation-triangle"), " No checks mapped: "),
                    sprintf(
                      "The TPP desired score (\"%s\") is relative to a check, but no checks are mapped for this trait. Map checks in the TPP module to enable auto-fill.",
                      d_text_rel
                    )
                  )
                }
              }
            }

            if (can_autofill) {
              tpp_checkbox_ui <- checkboxInput(
                ns(paste0("tppAutoFill_", safe_trait)),
                label = "Use TPP desired scores to set trait thresholds",
                value = FALSE
              )
            }
          }

          # --- Multi-select check dropdown for relative score_type traits ---
          tpp_checks_ui <- NULL
          if (!is.null(tpp_score_type) && !is.na(tpp_score_type) &&
              tpp_score_type == "relative" && has_tpp) {
            trait_row <- tpp_data[tpp_data$pheno_trait == trait_name, , drop = FALSE]
            check_designations <- NULL
            if (nrow(trait_row) > 0 && "checks" %in% colnames(tpp_data)) {
              check_designations <- trait_row$checks[[1]]
            }
            if (!is.null(check_designations) && length(check_designations) > 0) {
              tpp_checks_ui <- tagList(
                selectInput(
                  ns(paste0("tppChecks_", safe_trait)),
                  label = "Reference checks (TPP)",
                  choices = check_designations,
                  selected = NULL,
                  multiple = TRUE
                ),
                uiOutput(ns(paste0("tppRefValue_", safe_trait)))
              )
            }
          }

          # --- Warning placeholder for auto-fill issues ---
          tpp_warning_ui <- uiOutput(ns(paste0("tppAutoFillWarning_", safe_trait)))

          shinydashboard::box(
            width = 12,
            title = tagList(
              tags$span(tpp_display_name(trait_name)),
              tpp_badge_ui
            ),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,

            # TPP checkbox (only for Essential_Maintain)
            tpp_checkbox_ui,

            # TPP out-of-scale warning (shown when desired score is outside scale range)
            tpp_scale_warning_ui,

            # TPP auto-fill warning
            tpp_warning_ui,

            # TPP multi-select checks (only for relative score_type traits)
            tpp_checks_ui,

            selectInput(
              ns(paste0("ruleType_", safe_trait)),
              "Rule type",
              choices = c(
                "Threshold",
                "Acceptable range",
                "% over check",
                "% over mean"
              ),
              selected = "Threshold"
            ),

            conditionalPanel(
              condition = sprintf(
                "input['%s'] != 'Acceptable range'",
                paste0("ruleType_", safe_trait)
              ),
              ns = ns,
              selectInput(
                ns(paste0("direction_", safe_trait)),
                "Direction",
                choices = c("Higher is better", "Lower is better"),
                selected = {
                  # Derive initial direction from weight if available
                  weight_val <- input[[paste0("weight_", safe_trait)]]
                  derived <- derive_direction_from_weight(weight_val)
                  if (!is.null(derived)) derived else "Higher is better"
                }
              )
            ),

            uiOutput(ns(paste0("ruleInputs_", safe_trait)))
          )
        })
      )
    })

    get_digits_from_range <- function(rng) {
      range_width <- diff(rng)

      if (!is.finite(range_width) || range_width <= 0) {
        return(3)
      }

      e <- floor(log10(range_width))

      if (e < 0) return(3)   # range < 1
      if (e == 0) return(2)  # range on 10^0 scale
      if (e == 1) return(1)  # range on 10^1 scale
      return(0)              # range on 10^2 or larger
    }

    get_step_from_digits <- function(digits) {
      switch(
        as.character(digits),
        "3" = 0.001,
        "2" = 0.01,
        "1" = 0.1,
        "0" = 1,
        0.001
      )
    }

    ########################################
    # TPP Auto-fill observers (Task 10.3)
    ########################################

    # Observer: "Use TPP desired scores" checkbox toggle for Essential_Maintain traits
    observe({
      req(input$traitsToEvaluate)

      tpp_data <- tpp_filtered_traits()
      has_tpp <- !is.null(tpp_data) && is.data.frame(tpp_data) && nrow(tpp_data) > 0
      if (!has_tpp) return()

      trait_list <- input$traitsToEvaluate

      lapply(trait_list, function(trait_name) {
        safe_trait <- sanitize_trait_id(trait_name)
        checkbox_id <- paste0("tppAutoFill_", safe_trait)

        # Only observe for Essential_Maintain traits
        trait_row <- tpp_data[tpp_data$pheno_trait == trait_name, , drop = FALSE]
        # Also try matching by tpp_trait name
        if (nrow(trait_row) == 0 && "tpp_trait" %in% colnames(tpp_data)) {
          trait_row <- tpp_data[tpp_data$tpp_trait == trait_name, , drop = FALSE]
        }
        if (nrow(trait_row) == 0) return()
        if (!"category" %in% colnames(tpp_data)) return()
        if (is.na(trait_row$category[1]) || trait_row$category[1] != "Essential_Maintain") return()

        observeEvent(input[[checkbox_id]], {
          checked <- isTRUE(input[[checkbox_id]])
          autofill_state <- tpp_autofill_state()

          if (checked) {
            # --- Auto-fill threshold from TPP Desired_Score ---
            # Check if the required columns exist in tpp_data
            score_type <- if ("score_type" %in% colnames(trait_row)) trait_row$score_type[1] else NA_character_
            desired_lower <- if ("desired_lower" %in% colnames(trait_row)) trait_row$desired_lower[1] else NA_real_
            desired_upper <- if ("desired_upper" %in% colnames(trait_row)) trait_row$desired_upper[1] else NA_real_
            desired_direction <- if ("desired_direction" %in% colnames(trait_row)) trait_row$desired_direction[1] else NA_character_

            # If score_type is not available, try to infer from desired_direction
            if (is.na(score_type) && (!is.na(desired_lower) || !is.na(desired_upper))) {
              score_type <- "absolute"
            }

            # If no structured threshold data is available, show info message and return
            if (is.na(score_type) && is.na(desired_lower) && is.na(desired_upper)) {
              output[[paste0("tppAutoFillWarning_", safe_trait)]] <- renderUI({
                tags$div(
                  style = "color:#856404; background-color:#fff3cd; border-left:5px solid #ffeeba; padding:10px; margin-bottom:10px;",
                  tags$strong("Info: "),
                  "No structured desired score data is available for this trait. ",
                  "Please configure the threshold manually."
                )
              })
              return()
            }

            if (!is.na(score_type) && score_type == "relative") {
              # Check if checks are mapped — look in metadata$TPP checks_per_trait
              checks_mapped <- NULL
              current_tpp_id <- tpp_id_prepam()
              if (!is.null(current_tpp_id)) {
                dt <- data()
                dt_meta <- dt$metadata$TPP[[current_tpp_id]]
                if (!is.null(dt_meta) && !is.null(dt_meta$checks_per_trait)) {
                  tpp_trait_name <- trait_row$tpp_trait[1]
                  checks_mapped <- dt_meta$checks_per_trait[[tpp_trait_name]]
                }
              }
              if (is.null(checks_mapped) || length(checks_mapped) == 0) {
                # Show warning: no checks mapped for relative trait
                output[[paste0("tppAutoFillWarning_", safe_trait)]] <- renderUI({
                  tags$div(
                    style = "color:#856404; background-color:#fff3cd; border-left:5px solid #ffeeba; padding:10px; margin-bottom:10px;",
                    tags$strong("Warning: "),
                    "No checks are mapped for this trait in the TPP metadata. ",
                    "The check must be mapped before auto-fill can proceed for relative desired scores."
                  )
                })
                return()
              }
            }

            # Clear any previous warning
            output[[paste0("tppAutoFillWarning_", safe_trait)]] <- renderUI({ NULL })

            # --- Validate: check if desired score is within the trait's scale ---
            scale_min_val <- if ("scale_min" %in% colnames(trait_row)) trait_row$scale_min[1] else NA_real_
            scale_max_val <- if ("scale_max" %in% colnames(trait_row)) trait_row$scale_max[1] else NA_real_

            if (!is.na(scale_min_val) && !is.na(scale_max_val) && score_type == "absolute") {
              # Check if any absolute threshold is outside scale bounds
              out_of_scale <- FALSE
              if (!is.na(desired_lower) && is.finite(desired_lower) && desired_lower > scale_max_val) {
                out_of_scale <- TRUE
              }
              if (!is.na(desired_upper) && is.finite(desired_upper) && desired_upper < scale_min_val) {
                out_of_scale <- TRUE
              }
              if (out_of_scale) {
                score_text <- if ("desired_score_text" %in% colnames(trait_row)) trait_row$desired_score_text[1] else ""
                output[[paste0("tppAutoFillWarning_", safe_trait)]] <- renderUI({
                  tags$div(
                    style = "color:#721c24; background-color:#f8d7da; border-left:5px solid #f5c6cb; padding:10px; margin-bottom:10px;",
                    tags$strong("Warning: "),
                    sprintf(
                      "The TPP desired score (\"%s\") is outside the trait's scale range (%s to %s). This threshold cannot be used.",
                      score_text, scale_min_val, scale_max_val
                    )
                  )
                })
                return()
              }
            }

            # Determine threshold value and direction based on score_type
            if (!is.na(score_type) && score_type == "absolute") {

              # Check if both lower and upper are defined → use "Acceptable range"
              has_lower <- !is.na(desired_lower) && is.finite(desired_lower)
              has_upper <- !is.na(desired_upper) && is.finite(desired_upper)

              if (has_lower && has_upper) {
                # Range: use "Acceptable range" rule type
                updateSelectInput(session, paste0("ruleType_", safe_trait), selected = "Acceptable range")
                # Defer range slider update to after UI re-renders
                session$onFlushed(function() {
                  updateSliderInput(session, paste0("rangeSlider_", safe_trait),
                                    value = c(desired_lower, desired_upper))
                  updateNumericInput(session, paste0("rangeMin_", safe_trait), value = desired_lower)
                  updateNumericInput(session, paste0("rangeMax_", safe_trait), value = desired_upper)
                }, once = TRUE)

              } else if (has_lower) {
                # Single lower bound: "Higher is better" threshold
                updateSelectInput(session, paste0("ruleType_", safe_trait), selected = "Threshold")
                updateSelectInput(session, paste0("direction_", safe_trait), selected = "Higher is better")
                # Defer threshold update to after UI re-renders
                session$onFlushed(function() {
                  updateSliderInput(session, paste0("minThresholdSlider_", safe_trait), value = desired_lower)
                  updateNumericInput(session, paste0("minThreshold_", safe_trait), value = desired_lower)
                }, once = TRUE)

              } else if (has_upper) {
                # Single upper bound: "Lower is better" threshold
                updateSelectInput(session, paste0("ruleType_", safe_trait), selected = "Threshold")
                updateSelectInput(session, paste0("direction_", safe_trait), selected = "Lower is better")
                # Defer threshold update to after UI re-renders
                session$onFlushed(function() {
                  updateSliderInput(session, paste0("minThresholdSlider_", safe_trait), value = desired_upper)
                  updateNumericInput(session, paste0("minThreshold_", safe_trait), value = desired_upper)
                }, once = TRUE)

              } else {
                return()
              }

            } else if (!is.na(score_type) && score_type == "relative") {
              # For relative: use "% over check" rule type with percentage from TPP
              pct_above <- if ("pct_above_check" %in% colnames(trait_row)) trait_row$pct_above_check[1] else NA_real_
              pct_of <- if ("pct_of_check" %in% colnames(trait_row)) trait_row$pct_of_check[1] else NA_real_

              # Determine the percentage threshold and direction
              if (!is.na(pct_above) && is.finite(pct_above)) {
                if (pct_above < 0) {
                  # Negative pct means "allowed up to X% below check" → Lower is better
                  threshold_val <- abs(pct_above)
                  direction_val <- "Lower is better"
                } else {
                  # Positive pct means "must be X% above check" → Higher is better
                  threshold_val <- pct_above
                  direction_val <- "Higher is better"
                }
              } else if (!is.na(pct_of) && is.finite(pct_of)) {
                # "Equal to at least X% of check" → must be at least X% of check
                # Convert: being at X% of check means (100 - X)% below → threshold = 100 - X, Lower is better
                if (pct_of < 100) {
                  threshold_val <- 100 - pct_of
                  direction_val <- "Lower is better"
                } else {
                  threshold_val <- pct_of - 100
                  direction_val <- "Higher is better"
                }
              } else {
                threshold_val <- 0
                direction_val <- "Higher is better"
              }

              updateSelectInput(session, paste0("ruleType_", safe_trait), selected = "% over check")
              updateSelectInput(session, paste0("direction_", safe_trait), selected = direction_val)

              # Auto-select checks from metadata$TPP$checks_per_trait
              if (!is.null(checks_mapped) && length(checks_mapped) > 0) {
                session$onFlushed(function() {
                  updateSelectInput(session, paste0("checkVar_", safe_trait), selected = checks_mapped)
                }, once = TRUE)
              }

              # Defer % slider update to after UI re-renders
              local_threshold_val <- threshold_val
              session$onFlushed(function() {
                updateSliderInput(session, paste0("pctOverCheckSlider_", safe_trait), value = max(0, min(100, local_threshold_val)))
                updateNumericInput(session, paste0("pctOverCheck_", safe_trait), value = local_threshold_val)
              }, once = TRUE)
            }

            # Track that this trait was auto-filled
            autofill_state[[safe_trait]] <- TRUE
            tpp_autofill_state(autofill_state)

          } else {
            # --- Uncheck: Clear auto-filled fields, restore defaults ---
            output[[paste0("tppAutoFillWarning_", safe_trait)]] <- renderUI({ NULL })

            # Reset to defaults
            req(data())
            req(input$mtaStamp)
            dt <- data()
            dtPred <- dt$predictions
            dtPred <- dtPred[which(dtPred$analysisId %in% input$mtaStamp), ]
            dtPred_trait <- dtPred[dtPred$trait == trait_name & dtPred$effectType == "designation", ]
            predictedValue <- dtPred_trait$predictedValue[!is.na(dtPred_trait$predictedValue)]

            if (length(predictedValue) > 0) {
              rng <- range(predictedValue, na.rm = TRUE)
              if (all(is.finite(rng))) {
                # Reset to min (default for "Higher is better")
                updateSliderInput(session, paste0("minThresholdSlider_", safe_trait), value = rng[1])
                updateNumericInput(session, paste0("minThreshold_", safe_trait), value = rng[1])
              }
            }
            updateSelectInput(session, paste0("direction_", safe_trait), selected = "Higher is better")

            # Clear auto-fill tracking
            autofill_state[[safe_trait]] <- FALSE
            tpp_autofill_state(autofill_state)
          }
        }, ignoreInit = TRUE)
      })
    })

    # Observer: Multi-select check dropdown for relative traits - compute reference value
    observe({
      req(input$traitsToEvaluate)

      tpp_data <- tpp_filtered_traits()
      has_tpp <- !is.null(tpp_data) && is.data.frame(tpp_data) && nrow(tpp_data) > 0
      if (!has_tpp) return()

      trait_list <- input$traitsToEvaluate

      lapply(trait_list, function(trait_name) {
        safe_trait <- sanitize_trait_id(trait_name)
        checks_input_id <- paste0("tppChecks_", safe_trait)

        # Only for relative score_type traits
        trait_row <- tpp_data[tpp_data$pheno_trait == trait_name, , drop = FALSE]
        if (nrow(trait_row) == 0) return()
        if (!"score_type" %in% colnames(tpp_data)) return()
        if (is.na(trait_row$score_type[1]) || trait_row$score_type[1] != "relative") return()

        # Render reference value based on selected checks
        output[[paste0("tppRefValue_", safe_trait)]] <- renderUI({
          selected_checks <- input[[checks_input_id]]
          if (is.null(selected_checks) || length(selected_checks) == 0) {
            return(NULL)
          }

          # Compute mean of selected checks' predicted values for this trait
          req(data())
          req(input$mtaStamp)
          dt <- data()
          dtPred <- dt$predictions
          dtPred <- dtPred[which(dtPred$analysisId %in% input$mtaStamp), ]
          dtPred_trait <- dtPred[
            dtPred$trait == trait_name &
              dtPred$effectType == "designation" &
              dtPred$designation %in% selected_checks,
            , drop = FALSE
          ]

          pred_values <- dtPred_trait$predictedValue[!is.na(dtPred_trait$predictedValue)]

          if (length(pred_values) == 0) {
            return(
              tags$div(
                style = "color:#856404; background-color:#fff3cd; border-left:5px solid #ffeeba; padding:8px; margin-top:5px;",
                tags$strong("Warning: "),
                "Cannot compute reference value - no predicted values available for selected checks."
              )
            )
          }

          ref_value <- mean(pred_values, na.rm = TRUE)
          tags$div(
            style = "background-color:#d4edda; border-left:5px solid #28a745; padding:8px; margin-top:5px; border-radius:3px;",
            tags$strong("Reference value: "),
            tags$span(sprintf("%.3f", ref_value)),
            tags$br(),
            tags$small(
              style = "color:#666;",
              paste0("(Mean of ", length(pred_values), " check prediction(s))")
            )
          )
        })
      })
    })

    observe({
      req(input$traitsToEvaluate)
      trait_list <- input$traitsToEvaluate

      get_digits_from_range <- function(rng) {
        range_width <- diff(rng)

        if (!is.finite(range_width) || range_width <= 0) {
          return(3)
        }

        e <- floor(log10(range_width))

        if (e < 0) return(3)
        if (e == 0) return(2)
        if (e == 1) return(1)
        return(0)
      }

      get_step_from_digits <- function(digits) {
        switch(
          as.character(digits),
          "3" = 0.001,
          "2" = 0.01,
          "1" = 0.1,
          "0" = 1,
          0.001
        )
      }

      lapply(trait_list, function(trait_name) {
        safe_trait <- sanitize_trait_id(trait_name)

        output[[paste0("ruleInputs_", safe_trait)]] <- renderUI({
          rule_type <- input[[paste0("ruleType_", safe_trait)]]
          req(rule_type)

          req(data())
          req(input$mtaStamp)

          dt <- data()
          dtPred <- dt$predictions
          dtPred <- dtPred[which(dtPred$analysisId %in% input$mtaStamp), ]
          dtPred_trait <- dtPred[dtPred$trait == trait_name & dtPred$effectType == "designation", ]
          predictedValue <- dtPred_trait$predictedValue[!is.na(dtPred_trait$predictedValue)]

          req(length(predictedValue) > 0)

          rng <- range(predictedValue, na.rm = TRUE)
          req(all(is.finite(rng)))

          digits <- get_digits_from_range(rng)
          slider_step <- get_step_from_digits(digits)

          rng_min <- round(rng[1], digits)
          rng_max <- round(rng[2], digits)

          if (!is.finite(slider_step) || slider_step <= 0) {
            slider_step <- get_step_from_digits(3)
          }

          if (rule_type == "Threshold") {
            # Default threshold based on direction:
            # "Higher is better" -> min (everything above min passes)
            # "Lower is better" -> max (everything below max passes)
            direction_val <- input[[paste0("direction_", safe_trait)]]
            # Also check weight sign as fallback (direction input may not exist yet)
            if (is.null(direction_val)) {
              weight_val <- input[[paste0("weight_", safe_trait)]]
              direction_val <- derive_direction_from_weight(weight_val)
            }
            threshold_default <- if (!is.null(direction_val) && identical(direction_val, "Lower is better")) rng_max else rng_min

            # If TPP auto-fill is active, use TPP value (isolate to avoid reactive dependency)
            if (isTRUE(isolate(tpp_autofill_state())[[safe_trait]])) {
              tpp_data <- isolate(tpp_filtered_traits())
              if (!is.null(tpp_data) && is.data.frame(tpp_data) && nrow(tpp_data) > 0) {
                t_row <- tpp_data[tpp_data$pheno_trait == trait_name, , drop = FALSE]
                if (nrow(t_row) == 0 && "tpp_trait" %in% colnames(tpp_data)) {
                  t_row <- tpp_data[tpp_data$tpp_trait == trait_name, , drop = FALSE]
                }
                if (nrow(t_row) > 0) {
                  d_lower <- if ("desired_lower" %in% colnames(t_row)) t_row$desired_lower[1] else NA_real_
                  d_upper <- if ("desired_upper" %in% colnames(t_row)) t_row$desired_upper[1] else NA_real_
                  if (!is.na(d_lower) && is.finite(d_lower)) {
                    threshold_default <- d_lower
                  } else if (!is.na(d_upper) && is.finite(d_upper)) {
                    threshold_default <- d_upper
                  }
                }
              }
            }

            tagList(
              sliderInput(
                ns(paste0("minThresholdSlider_", safe_trait)),
                "Threshold value",
                min = rng_min,
                max = rng_max,
                value = max(rng_min, min(rng_max, threshold_default)),
                step = slider_step
              ),
              numericInput(
                ns(paste0("minThreshold_", safe_trait)),
                "Threshold value",
                value = threshold_default,
                min = rng_min,
                max = rng_max,
                step = slider_step
              )
            )

          } else if (rule_type == "Acceptable range") {

            range_min_default <- rng_min
            range_max_default <- rng_max

            # If TPP auto-fill is active, use TPP values (isolate to avoid reactive dependency)
            if (isTRUE(isolate(tpp_autofill_state())[[safe_trait]])) {
              tpp_data_local <- isolate(tpp_filtered_traits())
              if (!is.null(tpp_data_local) && is.data.frame(tpp_data_local) && nrow(tpp_data_local) > 0) {
                t_row <- tpp_data_local[tpp_data_local$pheno_trait == trait_name, , drop = FALSE]
                if (nrow(t_row) == 0 && "tpp_trait" %in% colnames(tpp_data_local)) {
                  t_row <- tpp_data_local[tpp_data_local$tpp_trait == trait_name, , drop = FALSE]
                }
                if (nrow(t_row) > 0) {
                  d_lower <- if ("desired_lower" %in% colnames(t_row)) t_row$desired_lower[1] else NA_real_
                  d_upper <- if ("desired_upper" %in% colnames(t_row)) t_row$desired_upper[1] else NA_real_
                  if (!is.na(d_lower) && is.finite(d_lower)) range_min_default <- d_lower
                  if (!is.na(d_upper) && is.finite(d_upper)) range_max_default <- d_upper
                }
              }
            }

            tagList(
              sliderInput(
                ns(paste0("rangeSlider_", safe_trait)),
                "Acceptable range",
                min = rng_min,
                max = rng_max,
                value = c(max(rng_min, min(rng_max, range_min_default)),
                          max(rng_min, min(rng_max, range_max_default))),
                step = slider_step
              ),
              fluidRow(
                column(
                  width = 6,
                  numericInput(
                    ns(paste0("rangeMin_", safe_trait)),
                    "Minimum value",
                    value = range_min_default,
                    min = rng_min,
                    max = rng_max,
                    step = slider_step
                  )
                ),
                column(
                  width = 6,
                  numericInput(
                    ns(paste0("rangeMax_", safe_trait)),
                    "Maximum value",
                    value = range_max_default,
                    min = rng_min,
                    max = rng_max,
                    step = slider_step
                  )
                )
              )
            )

          } else if (rule_type == "% over check") {

            check_entry_type_value <- input$checkEntryTypeValue
            if (is.null(check_entry_type_value) || !nzchar(check_entry_type_value)) {
              check_entry_type_value <- NULL
            }

            if (is.null(check_entry_type_value)) {
              return(
                div(
                  style = "color:#856404; background-color:#fff3cd; border-left:5px solid #ffeeba; padding:10px;",
                  tags$strong("Checks required: "),
                  "Select a valid check entry type before using a '% over check' rule."
                )
              )
            }

            check_designations <- dtPred[
              dtPred$entryType == check_entry_type_value,
              "designation"
            ]
            check_designations <- unique(check_designations[!is.na(check_designations)])

            # Default % value
            pct_default <- 0

            # If TPP auto-fill is active, use TPP value (isolate to avoid reactive dependency)
            if (isTRUE(isolate(tpp_autofill_state())[[safe_trait]])) {
              tpp_data_local <- isolate(tpp_filtered_traits())
              if (!is.null(tpp_data_local) && is.data.frame(tpp_data_local) && nrow(tpp_data_local) > 0) {
                t_row <- tpp_data_local[tpp_data_local$pheno_trait == trait_name, , drop = FALSE]
                if (nrow(t_row) == 0 && "tpp_trait" %in% colnames(tpp_data_local)) {
                  t_row <- tpp_data_local[tpp_data_local$tpp_trait == trait_name, , drop = FALSE]
                }
                if (nrow(t_row) > 0) {
                  p_above <- if ("pct_above_check" %in% colnames(t_row)) t_row$pct_above_check[1] else NA_real_
                  p_of <- if ("pct_of_check" %in% colnames(t_row)) t_row$pct_of_check[1] else NA_real_
                  if (!is.na(p_above) && is.finite(p_above)) {
                    pct_default <- abs(p_above)
                  } else if (!is.na(p_of) && is.finite(p_of)) {
                    pct_default <- if (p_of < 100) (100 - p_of) else (p_of - 100)
                  }
                }
              }
            }

            tagList(
              selectInput(
                ns(paste0("checkVar_", safe_trait)),
                "Reference check(s)",
                choices = check_designations,
                multiple = TRUE
              ),
              sliderInput(
                ns(paste0("pctOverCheckSlider_", safe_trait)),
                "% over check",
                min = 0,
                max = 100,
                value = max(0, min(100, pct_default)),
                step = 1
              ),
              numericInput(
                ns(paste0("pctOverCheck_", safe_trait)),
                "% over check",
                value = pct_default,
                min = 0,
                max = 100,
                step = 1
              )
            )

          } else if (rule_type == "% over mean") {
            tagList(
              sliderInput(
                ns(paste0("pctOverMeanSlider_", safe_trait)),
                "% over mean",
                min = 0,
                max = 100,
                value = 0,
                step = 1
              ),
              numericInput(
                ns(paste0("pctOverMean_", safe_trait)),
                "% over mean",
                value = 0,
                min = 0,
                max = 100,
                step = 1
              )
            )
          }
        })

        observeEvent(input[[paste0("minThresholdSlider_", safe_trait)]], ignoreInit = TRUE, {
          val <- input[[paste0("minThresholdSlider_", safe_trait)]]
          updateNumericInput(
            session,
            paste0("minThreshold_", safe_trait),
            value = val
          )
        })

        observeEvent(input[[paste0("minThreshold_", safe_trait)]], ignoreInit = TRUE, {
          val <- input[[paste0("minThreshold_", safe_trait)]]
          updateSliderInput(
            session,
            paste0("minThresholdSlider_", safe_trait),
            value = val
          )
        })

        observeEvent(input[[paste0("rangeSlider_", safe_trait)]], ignoreInit = TRUE, {
          val <- input[[paste0("rangeSlider_", safe_trait)]]
          updateNumericInput(
            session,
            paste0("rangeMin_", safe_trait),
            value = val[1]
          )
          updateNumericInput(
            session,
            paste0("rangeMax_", safe_trait),
            value = val[2]
          )
        })

        observeEvent(input[[paste0("rangeMin_", safe_trait)]], ignoreInit = TRUE, {
          min_val <- input[[paste0("rangeMin_", safe_trait)]]
          max_val <- input[[paste0("rangeMax_", safe_trait)]]

          req(!is.null(min_val), !is.null(max_val))

          updateSliderInput(
            session,
            paste0("rangeSlider_", safe_trait),
            value = c(min(min_val, max_val), max(min_val, max_val))
          )
        })

        observeEvent(input[[paste0("rangeMax_", safe_trait)]], ignoreInit = TRUE, {
          min_val <- input[[paste0("rangeMin_", safe_trait)]]
          max_val <- input[[paste0("rangeMax_", safe_trait)]]

          req(!is.null(min_val), !is.null(max_val))

          updateSliderInput(
            session,
            paste0("rangeSlider_", safe_trait),
            value = c(min(min_val, max_val), max(min_val, max_val))
          )
        })

        observeEvent(input[[paste0("pctOverCheckSlider_", safe_trait)]], ignoreInit = TRUE, {
          val <- input[[paste0("pctOverCheckSlider_", safe_trait)]]
          updateNumericInput(
            session,
            paste0("pctOverCheck_", safe_trait),
            value = val
          )
        })

        observeEvent(input[[paste0("pctOverCheck_", safe_trait)]], ignoreInit = TRUE, {
          val <- input[[paste0("pctOverCheck_", safe_trait)]]
          updateSliderInput(
            session,
            paste0("pctOverCheckSlider_", safe_trait),
            value = val
          )
        })

        observeEvent(input[[paste0("pctOverMeanSlider_", safe_trait)]], ignoreInit = TRUE, {
          val <- input[[paste0("pctOverMeanSlider_", safe_trait)]]
          updateNumericInput(
            session,
            paste0("pctOverMean_", safe_trait),
            value = val
          )
        })

        observeEvent(input[[paste0("pctOverMean_", safe_trait)]], ignoreInit = TRUE, {
          val <- input[[paste0("pctOverMean_", safe_trait)]]
          updateSliderInput(
            session,
            paste0("pctOverMeanSlider_", safe_trait),
            value = val
          )
        })

        observeEvent(input[[paste0("direction_", safe_trait)]], ignoreInit = TRUE, {
          req(input[[paste0("ruleType_", safe_trait)]] == "Threshold")

          # Skip reset if TPP auto-fill is active — the onFlushed callback
          # will set the correct value after this observer fires.
          autofill_state <- tpp_autofill_state()
          if (isTRUE(autofill_state[[safe_trait]])) return()

          direction <- input[[paste0("direction_", safe_trait)]]
          req(direction)

          req(data())
          req(input$mtaStamp)

          dt <- data()
          dtPred <- dt$predictions
          dtPred <- dtPred[which(dtPred$analysisId %in% input$mtaStamp), ]
          dtPred_trait <- dtPred[
            dtPred$trait == trait_name & dtPred$effectType == "designation",
          ]
          predictedValue <- dtPred_trait$predictedValue[!is.na(dtPred_trait$predictedValue)]

          req(length(predictedValue) > 0)

          rng <- range(predictedValue, na.rm = TRUE)
          req(all(is.finite(rng)))

          digits <- get_digits_from_range(rng)
          slider_step <- get_step_from_digits(digits)

          rng_min <- round(rng[1], digits)
          rng_max <- round(rng[2], digits)

          new_value <- if (direction == "Lower is better") rng_max else rng_min

          updateSliderInput(
            session,
            paste0("minThresholdSlider_", safe_trait),
            value = new_value
          )

          updateNumericInput(
            session,
            paste0("minThreshold_", safe_trait),
            value = new_value
          )
        })

      })
    })

    ########################################
    # Direction auto-sync: weight sign → direction dropdown
    ########################################

    observe({
      req(input$traitsToEvaluate)

      lapply(input$traitsToEvaluate, function(trait_name) {
        safe_trait <- sanitize_trait_id(trait_name)
        weight_id <- paste0("weight_", safe_trait)
        direction_id <- paste0("direction_", safe_trait)

        # Observer: when weight changes, auto-update direction
        observeEvent(input[[weight_id]], {
          weight_val <- input[[weight_id]]
          new_direction <- derive_direction_from_weight(weight_val)

          if (is.null(new_direction)) {
            return()
          }

          current_direction <- input[[direction_id]]
          if (!identical(current_direction, new_direction)) {
            updateSelectInput(session, direction_id, selected = new_direction)
            # Mark as auto-set
            auto_map <- direction_auto_set()
            auto_map[[safe_trait]] <- TRUE
            direction_auto_set(auto_map)
          } else {
          }
        }, ignoreInit = TRUE)

        # Observer: detect manual direction override
        observeEvent(input[[direction_id]], {
          auto_map <- direction_auto_set()
          if (isTRUE(auto_map[[safe_trait]])) {
            # This change was triggered by auto-sync; clear the flag
            auto_map[[safe_trait]] <- FALSE
            direction_auto_set(auto_map)
          }
          # If flag was already FALSE, this is a manual override — preserved until next weight sign change
        }, ignoreInit = TRUE)
      })
    })

    ########################################
    #Pre-selection (idx4)
    ########################################

    observe({
      req(data())
      req(input$mtaStamp)

      dt <- data()

      #Get unique stages in data
      dt_pheno <- dt$data$pheno
      mt_dt <- dt$metadata$pheno
      stage_col <- mt_dt$value[mt_dt$parameter == "stage"]
      stages <- unique(dt_pheno[,stage_col])

      #Get unique designations available
      dtPred <- dt$predictions
      dtPred <- dtPred[which(dtPred$analysisId %in% input$mtaStamp), ]

      check_entry_type_value <- input$checkEntryTypeValue
      if (is.null(check_entry_type_value) || !nzchar(check_entry_type_value)) {
        check_entry_type_value <- NULL
      }

      if (is.null(check_entry_type_value)) {
        mta_candidates <- dtPred[
          dtPred$effectType == "designation",
          "designation"
        ]
      } else {
        mta_candidates <- dtPred[
          dtPred$effectType == "designation" &
            dtPred$entryType != check_entry_type_value,
          "designation"
        ]
      }

      mta_candidates <- unique(mta_candidates)


      updateSelectInput(
        session = session,
        inputId = "selectionStage",
        choices = stages
      )

      updateSelectizeInput(
        session = session,
        inputId = "candidateDesignations",
        choices = mta_candidates,
        server = TRUE
      )
    })


    selected_candidates <- reactive({
      req(data())

      dt <- data()
      mode <- input$candidateSelectionMode

      #Candidates when selecting all
      dtPred <- dt$predictions
      dtPred <- dtPred[which(dtPred$analysisId %in% input$mtaStamp), ]

      check_entry_type_value <- input$checkEntryTypeValue
      if (is.null(check_entry_type_value) || !nzchar(check_entry_type_value)) {
        check_entry_type_value <- NULL
      }

      if (is.null(check_entry_type_value)) {
        mta_candidates <- dtPred[
          dtPred$effectType == "designation",
          "designation"
        ]
      } else {
        mta_candidates <- dtPred[
          dtPred$effectType == "designation" &
            dtPred$entryType != check_entry_type_value,
          "designation"
        ]
      }

      mta_candidates <- unique(mta_candidates)

      if (mode == "all") {
        return(mta_candidates)
      }

      if (mode == "stage") {
        req(input$selectionStage)

        dt_pheno <- dt$data$pheno
        mt_dt <- dt$metadata$pheno
        stage_col <- mt_dt$value[mt_dt$parameter == "stage"]
        designation_col <- mt_dt$value[mt_dt$parameter == "designation"]

        stage_candidates <- dt_pheno[dt_pheno[,stage_col]==input$selectionStage,designation_col]
        stage_candidates <- unique(stage_candidates)
        stage_candidates <- stage_candidates[stage_candidates %in% mta_candidates]

        return(stage_candidates)
      }

      if (mode == "manual") {
        req(input$candidateDesignations)
        return(input$candidateDesignations)
      }
    })

    ###########################################
    #Run initial selection (idx5)
    ###########################################

    trait_rules_input <- reactive({
      req(input$traitsToEvaluate)
      req(length(input$traitsToEvaluate) > 0)
      req(data())
      req(input$mtaStamp)

      dt <- data()
      dtPred <- dt$predictions
      dtPred <- dtPred[dtPred$analysisId %in% input$mtaStamp, , drop = FALSE]

      out <- lapply(input$traitsToEvaluate, function(trait_name) {
        safe_trait <- sanitize_trait_id(trait_name)

        dtPred_trait <- dtPred[
          dtPred$trait == trait_name &
            dtPred$effectType == "designation",
          ,
          drop = FALSE
        ]

        predictedValue <- dtPred_trait$predictedValue[
          !is.na(dtPred_trait$predictedValue)
        ]

        rng <- range(predictedValue, na.rm = TRUE)

        rule_type <- input[[paste0("ruleType_", safe_trait)]]
        if (is.null(rule_type) || !nzchar(rule_type)) {
          rule_type <- "Threshold"
        }

        direction <- input[[paste0("direction_", safe_trait)]]
        if (is.null(direction) || !nzchar(direction)) {
          # Fallback: derive from weight sign if direction UI not rendered yet
          weight_val <- input[[paste0("weight_", safe_trait)]]
          direction <- derive_direction_from_weight(weight_val)
          if (is.null(direction)) direction <- "Higher is better"
        }

        rule_list <- list(
          trait = trait_name,
          ruleType = rule_type
        )

        if (!identical(rule_type, "Acceptable range")) {
          rule_list$direction <- direction
        } else {
          rule_list$direction <- NULL
        }

        if (identical(rule_type, "Threshold")) {
          threshold <- input[[paste0("minThreshold_", safe_trait)]]

          if (is.null(threshold) || !is.finite(threshold)) {
            # Default: min for "Higher is better", max for "Lower is better"
            threshold <- if (identical(direction, "Lower is better")) rng[2] else rng[1]
          }

          rule_list$threshold <- threshold
        }

        if (identical(rule_type, "Acceptable range")) {
          min_value <- input[[paste0("rangeMin_", safe_trait)]]
          max_value <- input[[paste0("rangeMax_", safe_trait)]]

          if (is.null(min_value) || !is.finite(min_value)) {
            min_value <- rng[1]
          }

          if (is.null(max_value) || !is.finite(max_value)) {
            max_value <- rng[2]
          }

          rule_list$minValue <- min_value
          rule_list$maxValue <- max_value
        }

        if (identical(rule_type, "% over check")) {
          reference_check <- input[[paste0("checkVar_", safe_trait)]]

          check_entry_type_value <- input$checkEntryTypeValue
          if (is.null(check_entry_type_value) || !nzchar(check_entry_type_value)) {
            check_entry_type_value <- NULL
          }

          if (is.null(reference_check) || length(reference_check) == 0 ||
              (length(reference_check) == 1 && !nzchar(reference_check))) {
            if (!is.null(check_entry_type_value)) {
              check_designations <- unique(
                dtPred_trait$designation[
                  dtPred_trait$entryType == check_entry_type_value &
                    !is.na(dtPred_trait$designation)
                ]
              )

              reference_check <- if (length(check_designations) > 0) {
                check_designations
              } else {
                NULL
              }
            } else {
              reference_check <- NULL
            }
          }

          threshold <- input[[paste0("pctOverCheck_", safe_trait)]]
          if (is.null(threshold) || !is.finite(threshold)) {
            threshold <- 0
          }

          rule_list$referenceCheck <- reference_check
          rule_list$threshold <- threshold
        }

        if (identical(rule_type, "% over mean")) {
          threshold <- input[[paste0("pctOverMean_", safe_trait)]]

          if (is.null(threshold) || !is.finite(threshold)) {
            threshold <- 0
          }

          rule_list$threshold <- threshold
        }

        rule_list
      })

      names(out) <- input$traitsToEvaluate
      out
    })

    custom_weights_input <- reactive({
      req(input$traitsToEvaluate)

      out <- sapply(input$traitsToEvaluate, function(trait_name) {
        safe_trait <- sanitize_trait_id(trait_name)
        w <- input[[paste0("weight_", safe_trait)]]
        if (is.null(w)) 1 else w
      }, simplify = TRUE, USE.NAMES = TRUE)

      out
    })

    initial_selection_args <- reactive({
      req(input$staStamp)
      req(input$mtaStamp)
      req(input$traitsToEvaluate)
      req(length(input$traitsToEvaluate) > 0)

      check_entry_type_value <- input$checkEntryTypeValue
      if (is.null(check_entry_type_value) || !nzchar(check_entry_type_value)) {
        check_entry_type_value <- NULL
      }

      trait_rules <- trait_rules_input()

      uses_check_rule <- any(vapply(
        trait_rules,
        function(x) identical(x$ruleType, "% over check"),
        logical(1)
      ))

      validate(
        need(
          !uses_check_rule || !is.null(check_entry_type_value),
          "Checks are required when using a '% over check' threshold. Select a valid check entry type or choose another threshold type."
        )
      )

      # Get selection intensity
      top_pct <- NULL
      n_sel <- NULL
      if (identical(input$selectionMode, "pct")) {
        top_pct <- input$topPctSelected
      } else {
        n_sel <- input$nSelected
      }

      list(
        analysisIdName = if (nzchar(trimws(input$initSelectionIdName))) trimws(input$initSelectionIdName) else NULL,
        staStamp = input$staStamp,
        mtaStamp = input$mtaStamp,
        traitsToEvaluate = input$traitsToEvaluate,
        decisionLogic = "Weighted index",
        topPctSelected = top_pct,
        nSelected = n_sel,
        customWeights = custom_weights_input(),
        checkEntryTypeValue = check_entry_type_value,
        applySameRuleToAllTraits = isTRUE(input$applySameRuleToAllTraits),
        traitRules = trait_rules,
        candidateSelectionMode = input$candidateSelectionMode,
        selectionStage = if (identical(input$candidateSelectionMode, "stage")) input$selectionStage else NULL,
        candidateDesignations = if (identical(input$candidateSelectionMode, "manual")) input$candidateDesignations else NULL,
        selectedCandidates = selected_candidates(),
        tppId = tpp_id_prepam()
      )
    })

    observeEvent(input$runInitProdAdv, {

      args <- initial_selection_args()

      # Step 1: Run assessment
      assessment_result <- tryCatch({
        cgiarPipeline::assessSelectionQuality(args = args, dt_object = data())
      }, error = function(e) {
        message("[PAM Assessment] Error: ", e$message)
        NULL
      })

      # Store assessment result for the modal handlers
      assessment_cache(assessment_result)

      # Step 2: Show Assessment Modal (always)
      if (is.null(assessment_result)) {
        # Graceful degradation: simplified modal
        showModal(modalDialog(
          title = "Selection Quality Assessment",
          tags$p(style = "color:#856404; background:#fff3cd; padding:12px; border-radius:4px;",
                 icon("triangle-exclamation"),
                 " Assessment could not be completed. Proceed with caution."),
          tags$p("All traits will be included by default."),
          footer = tagList(
            actionButton(ns("proceedSelection"), "Proceed with selection", icon = icon("play-circle"), class = "btn-success"),
            modalButton("Go back")
          ),
          size = "l",
          easyClose = FALSE
        ))
      } else {
        # Full assessment modal (Tasks 3.2 & 3.3)
        ta <- assessment_result$trait_assessments

        # Build per-trait rows with color-coded badges and keep/exclude toggles
        trait_rows <- lapply(seq_len(nrow(ta)), function(i) {
          t_name <- ta$trait[i]
          rec <- ta$integrated_recommendation[i]
          just <- ta$justification[i]
          keep_default <- ta$default_keep[i]
          safe_name <- gsub("[^A-Za-z0-9_]", "_", t_name)

          # Color-coded badge based on recommendation
          badge_color <- if (rec == "Recommended to keep") "#28a745"
                         else if (grepl("Use with caution", rec)) "#ffc107"
                         else "#dc3545"
          badge_text_color <- if (grepl("Use with caution", rec)) "#000" else "#fff"

          tags$div(
            style = "margin-bottom:10px; padding:10px; border:1px solid #dee2e6; border-radius:6px; background:#f8f9fa;",
            fluidRow(
              column(8,
                tags$div(
                  tags$strong(t_name),
                  tags$span(
                    style = paste0("display:inline-block; margin-left:8px; padding:2px 8px; border-radius:4px; font-size:12px; background:", badge_color, "; color:", badge_text_color, ";"),
                    rec
                  )
                ),
                tags$p(style = "margin-top:4px; color:#555; font-size:13px;", just)
              ),
              column(4, style = "text-align:right; padding-top:10px;",
                checkboxInput(
                  inputId = ns(paste0("assessKeep_", safe_name)),
                  label = "Keep in index",
                  value = keep_default
                )
              )
            )
          )
        })

        # Summary sentence (Requirement 5.7)
        n_keep <- sum(ta$integrated_recommendation == "Recommended to keep")
        n_total <- nrow(ta)
        boundary_status <- if (!is.null(assessment_result$boundary_stats$median_z) &&
                               !is.na(assessment_result$boundary_stats$median_z) &&
                               assessment_result$boundary_stats$median_z >= 2) "adequate" else "limited"
        summary_text <- paste0("Overall: ", n_keep, " of ", n_total,
                               " traits recommended to keep. Boundary separability is ",
                               boundary_status, " at current selection intensity.")

        showModal(modalDialog(
          title = "Selection Quality Assessment",
          # Header line (Requirement 5.1)
          tags$p(strong(paste0("Selection intensity: ", assessment_result$selection_intensity, "% (",
                               assessment_result$n_selected, " of ",
                               assessment_result$n_total_candidates, " candidates selected)"))),
          tags$hr(),
          # Integrated trait table (Requirements 5.2, 5.3, 5.8, 5.9)
          do.call(tagList, trait_rows),
          tags$hr(),
          # Overall summary (Requirement 5.7)
          tags$p(style = "font-style:italic; color:#666;", summary_text),
          footer = tagList(
            actionButton(ns("proceedSelection"), "Proceed with selection", icon = icon("play-circle"), class = "btn-success"),
            modalButton("Go back")
          ),
          size = "l",
          easyClose = FALSE
        ))
      }
    })

    # Store the assessment result for use by the modal handlers
    assessment_cache <- reactiveVal(NULL)

    # Store persistent post-modal warnings for Review Output tab (Task 5.2)
    assessment_warnings_cache <- reactiveVal(NULL)

    # Task 3.4: "Proceed with selection" handler for the Assessment Modal
    observeEvent(input$proceedSelection, {
      removeModal()

      args <- initial_selection_args()
      assessment <- assessment_cache()

      if (is.null(assessment)) {
        # Graceful degradation: no assessment available, proceed with all traits
        run_initial_selection(args, exclude_traits = NULL)
        return()
      }

      ta <- assessment$trait_assessments

      # Collect keep/exclude states from checkbox inputs
      exclude_traits <- character(0)
      for (i in seq_len(nrow(ta))) {
        t_name <- ta$trait[i]
        safe_name <- gsub("[^A-Za-z0-9_]", "_", t_name)
        keep_value <- input[[paste0("assessKeep_", safe_name)]]

        # If checkbox is unchecked (FALSE or NULL), trait is excluded
        if (!isTRUE(keep_value)) {
          exclude_traits <- c(exclude_traits, t_name)
        }
      }

      # Validate: at least one trait must be kept
      if (length(exclude_traits) == length(ta$trait)) {
        showNotification(
          "At least one trait must be kept in the selection index.",
          type = "error",
          duration = 5
        )
        return()
      }

      if (length(exclude_traits) == 0) exclude_traits <- NULL

      run_initial_selection(args, exclude_traits = exclude_traits)
    })



    run_initial_selection <- function(args, exclude_traits) {
      args$excludeTraits <- exclude_traits

      shinybusy::show_modal_spinner(
        spin = "fading-circle",
        text = "Running initial product advancement selection..."
      )

      dt_object <- data()

      result <- tryCatch({
        cgiarPipeline::runInitialProdAdv(analysisId = as.numeric(Sys.time()),
                          analysisIdName = args$analysisIdName,
                          args = args,
                          dt_object = dt_object)
      }, error = function(e) {
        shinybusy::remove_modal_spinner()
        showNotification(
          paste("Initial selection failed:", e$message),
          type = "error",
          duration = NULL
        )
        return(NULL)
      })

      shinybusy::remove_modal_spinner()

      req(result)
      data(result)

      # Store assessment metadata in the data object (Task 5.3)
      assessment <- assessment_cache()
      if (!is.null(assessment)) {
        ta <- assessment$trait_assessments
        kept_traits <- setdiff(ta$trait, if (!is.null(exclude_traits)) exclude_traits else character(0))

        metadata <- data.frame(
          trait = ta$trait,
          integrated_recommendation = ta$integrated_recommendation,
          breeder_override = ta$trait %in% kept_traits & ta$integrated_recommendation != "Recommended to keep",
          selection_intensity = assessment$selection_intensity,
          stringsAsFactors = FALSE
        )

        # Attach to the result object
        result_with_meta <- data()
        attr(result_with_meta, "pam_assessment_metadata") <- metadata
        data(result_with_meta)
      }

      showNotification(
        "Initial selection completed successfully.",
        type = "message"
      )

      # Post-modal warning: overridden traits notification (Task 5.1)
      assessment <- assessment_cache()
      if (!is.null(assessment)) {
        ta <- assessment$trait_assessments
        # Find traits that are KEPT but had a non-"Recommended to keep" recommendation
        kept_traits <- setdiff(ta$trait, if (!is.null(exclude_traits)) exclude_traits else character(0))
        overridden <- ta[ta$trait %in% kept_traits & ta$integrated_recommendation != "Recommended to keep", ]

        if (nrow(overridden) > 0) {
          showNotification(
            paste0("Selection executed. ", nrow(overridden), " trait(s) kept with caution \u2014 see Review Output for guidance."),
            type = "warning",
            duration = 10
          )
        }

        # Store persistent warnings for Review Output tab (Task 5.2)
        if (nrow(overridden) > 0) {
          si <- assessment$selection_intensity
          warnings_list <- lapply(seq_len(nrow(overridden)), function(i) {
            t_name <- overridden$trait[i]
            rec <- overridden$integrated_recommendation[i]

            if (rec == "Use with caution (boundary separable)") {
              paste0("Trait '", t_name, "' \u2014 Use with caution (boundary separable) at ", si,
                     "% selection intensity. Do not override the index-based ranking using this trait alone. Trust the composite index.")
            } else if (rec == "Use with caution (boundary non-separable)") {
              paste0("Trait '", t_name, "' \u2014 Use with caution (boundary non-separable) at ", si,
                     "% selection intensity. Entries near the selection boundary may not be reliably distinguished on this trait. Review boundary candidates manually before finalizing advancement.")
            } else if (rec == "Recommended to exclude") {
              paste0("Trait '", t_name, "' \u2014 Recommended to exclude (kept by breeder override) at ", si,
                     "% selection intensity. This trait adds substantial uncertainty to the index. Boundary decisions involving this trait should be treated as provisional. Strongly consider reviewing all entries within 5 ranks of the cutoff.")
            } else {
              NULL
            }
          })
          assessment_warnings_cache(Filter(Negate(is.null), warnings_list))
        } else {
          assessment_warnings_cache(NULL)
        }
      } else {
        assessment_warnings_cache(NULL)
      }

      # Switch top-level tab to Review output
      updateTabsetPanel(
        session = session,
        inputId = "tabsMain",
        selected = "review_output"
      )

      # Switch inner Review output tab to Selection stamps
      updateTabsetPanel(
        session = session,
        inputId = "reviewOutputTabs",
        selected = "selection_stamps"
      )
    }

    # Render persistent assessment warnings in Review Output tab (Task 5.2)
    output$assessmentWarningsUI <- renderUI({
      warnings <- assessment_warnings_cache()
      if (is.null(warnings) || length(warnings) == 0) return(NULL)

      warning_divs <- lapply(warnings, function(w) {
        tags$div(
          style = "margin-bottom:8px; padding:10px; background:#fff3cd; border:1px solid #ffc107; border-radius:4px; color:#856404;",
          icon("triangle-exclamation"),
          tags$span(w)
        )
      })

      tagList(
        tags$h5(style = "color:#856404; margin-top:15px;", icon("triangle-exclamation"), " Assessment Warnings"),
        do.call(tagList, warning_divs)
      )
    })

    ############################################################################
    #Review Tabs
    #################

    ###########################################
    #Selection stamps (idx6)
    ###########################################

    observeEvent(c(data()), {
      req(data())
      dt <- data()$status

      dtSta <- dt[dt$module == "sta", , drop = FALSE]
      dtMta <- dt[dt$module %in% c("mta","mtaAsr","mtaFlex","mtaLmms","mas"), , drop = FALSE]
      dtIdxD <- dt[dt$module %in% c("indexD"), , drop = FALSE]

      dtInitSel  <- dt[dt$module == "Init_prodAdv",  , drop = FALSE]
      dtTableSel <- dt[dt$module == "Table_prodAdv", , drop = FALSE]
      dtPlotSel  <- dt[dt$module == "Plot_prodAdv",  , drop = FALSE]
      dtFinalSel <- dt[dt$module == "Final_prodAdv", , drop = FALSE]

      stampsSta      <- make_stamp_choices(dtSta)
      stampsMta      <- make_stamp_choices(dtMta)
      stampsIdxD     <- make_stamp_choices(dtIdxD)
      stampsInitSel  <- make_stamp_choices(dtInitSel)
      stampsTableSel <- make_stamp_choices(dtTableSel)
      stampsPlotSel  <- make_stamp_choices(dtPlotSel)
      stampsFinalSel <- make_stamp_choices(dtFinalSel)

      current_init  <- isolate(input$initialSelectionStamp)
      current_plot  <- isolate(input$plotSelectionStamp)
      current_final <- isolate(input$finalSelectionStamp)

      if (is.null(current_init) || !current_init %in% unname(stampsInitSel)) {
        current_init <- if (length(stampsInitSel) > 0) unname(stampsInitSel)[1] else NULL
      }

      if (is.null(current_plot) || !nzchar(current_plot) || !current_plot %in% unname(stampsPlotSel)) {
        current_plot <- "__none__"
      }

      if (is.null(current_final) ||
          !nzchar(current_final) ||
          current_final == "__none__" ||
          !current_final %in% unname(stampsFinalSel)) {
        current_final <- "__none__"
      }

      updateSelectInput(session, "staStamp", choices = stampsSta)
      updateSelectInput(session, "mtaStamp", choices = stampsMta)
      # indexStamp removed in Phase 2 redesign

      updateSelectInput(
        session,
        "initialSelectionStamp",
        choices = stampsInitSel,
        selected = current_init
      )

      updateSelectInput(
        session,
        "plotSelectionStamp",
        choices = c("No plot selection" = "__none__", stampsPlotSel),
        selected = current_plot
      )

      # Table selection stamp uses Table_prodAdv stamps
      updateSelectInput(
        session,
        "tableSelectionStampLoad",
        choices = c("Start from initial selection" = "__none__", stampsTableSel)
      )

      # Viz stamp dropdowns
      updateSelectInput(
        session,
        "vizTableSelectionStamp",
        choices = c("Use initial selection" = "__none__", stampsTableSel)
      )

      updateSelectInput(
        session,
        "finalSelectionStamp",
        choices = c("No final selection" = "__none__", stampsFinalSel),
        selected = current_final
      )

      # (finalInitialStamp/finalTableStamp/finalPlotStamp removed - now using report stamp inputs)
    })

    ########################################
    # TPP ID recovery from initial selection stamp (Review Output tab)
    # When a user selects an initial selection stamp, auto-recover the TPP ID
    # that was used during that run (stored as parameter "tpp_id" in modeling).
    ########################################
    observeEvent(input$initialSelectionStamp, {
      req(data())
      req(input$initialSelectionStamp)

      dt <- data()
      init_stamp <- input$initialSelectionStamp

      # Look up tpp_id from modeling table for this stamp
      stored_tpp_id <- tpp_stored_id_for_stamp(dt, init_stamp)

      if (is.null(stored_tpp_id)) {
        # No TPP was stored for this stamp.
        # Only clear TPP state if it wasn't already set via the TPP dropdown
        # (i.e., don't override a live UI selection during the Input steps workflow).
        if (is.null(tpp_id_prepam())) {
          tpp_filtered_traits(NULL)
        }
        return()
      }

      # Validate the stored TPP ID still exists in the data
      tpp_ids <- tpp_get_tpp_ids(dt)
      if (!stored_tpp_id %in% tpp_ids) {
        tpp_id_prepam(NULL)
        tpp_filtered_traits(NULL)
        return()
      }

      validation <- tpp_validate_metadata(dt, stored_tpp_id)
      if (!validation$valid) {
        tpp_id_prepam(NULL)
        tpp_filtered_traits(NULL)
        return()
      }

      # Reflect the recovered TPP in the Input-steps dropdown so the UI is
      # consistent and the input-driven observer agrees with the recovered state.
      if (!identical(isolate(input$tppIdInput), stored_tpp_id)) {
        updateSelectInput(session, "tppIdInput", selected = stored_tpp_id)
      }

      # Get the MTA stamp used for this initial selection (which() = NA-safe)
      mta_idx <- which(
        as.character(dt$modeling$analysisId) == as.character(init_stamp) &
          dt$modeling$module == "Init_prodAdv" &
          dt$modeling$parameter == "mta_stamp"
      )
      mta_stamp <- if (length(mta_idx) > 0) dt$modeling$value[mta_idx[1]] else NULL

      # Build filtered traits using the same enrichment logic
      tpp_entry <- dt$metadata$TPP[[stored_tpp_id]]
      tpp_traits_df <- tpp_entry$traits

      # Get available traits from MTA predictions
      available_traits <- character(0)
      if (!is.null(mta_stamp) && nzchar(mta_stamp)) {
        dtPred <- dt$predictions
        dtPred <- dtPred[which(dtPred$analysisId %in% mta_stamp), ]
        available_traits <- unique(dtPred$trait)
      }

      # Filter TPP traits to those whose pheno_trait is in available columns
      if (nrow(tpp_traits_df) > 0 && "pheno_trait" %in% colnames(tpp_traits_df)) {
        mapped_mask <- !is.na(tpp_traits_df$pheno_trait) &
          tpp_traits_df$pheno_trait %in% available_traits
        filtered_df <- tpp_traits_df[mapped_mask, , drop = FALSE]
      } else {
        filtered_df <- tpp_traits_df[0, , drop = FALSE]
      }

      # Enrich with category and desired scores from raw TPP data
      if (nrow(filtered_df) > 0 && !"category" %in% colnames(filtered_df)) {
        raw_tpp <- dt$data$TPP[[stored_tpp_id]]
        if (!is.null(raw_tpp) && is.data.frame(raw_tpp)) {
          if ("Trait Requirement" %in% colnames(raw_tpp) && "Trait Name" %in% colnames(raw_tpp)) {
            match_idx <- match(filtered_df$tpp_trait, raw_tpp[["Trait Name"]])
            raw_req <- raw_tpp[["Trait Requirement"]][match_idx]
            filtered_df$category <- ifelse(
              grepl("Improve", raw_req, ignore.case = TRUE), "Essential_Improve",
              ifelse(grepl("Maintain", raw_req, ignore.case = TRUE), "Essential_Maintain",
                     ifelse(grepl("Nice", raw_req, ignore.case = TRUE), "Nice_To_Have", NA_character_))
            )

            if ("Desired Score" %in% colnames(raw_tpp)) {
              raw_score <- raw_tpp[["Desired Score"]][match_idx]
              parsed_scores <- lapply(raw_score, tpp_parse_desired_score)

              filtered_df$desired_direction <- vapply(parsed_scores, function(p) {
                if (!is.na(p$direction)) p$direction else NA_character_
              }, character(1))
              filtered_df$score_type <- vapply(parsed_scores, function(p) {
                if (!is.na(p$score_type)) p$score_type else NA_character_
              }, character(1))
              filtered_df$desired_lower <- vapply(parsed_scores, function(p) {
                if (!is.na(p$lower_bound)) p$lower_bound else NA_real_
              }, numeric(1))
              filtered_df$desired_upper <- vapply(parsed_scores, function(p) {
                if (!is.na(p$upper_bound)) p$upper_bound else NA_real_
              }, numeric(1))
              filtered_df$pct_above_check <- vapply(parsed_scores, function(p) {
                if (!is.na(p$pct_above_check)) p$pct_above_check else NA_real_
              }, numeric(1))
              filtered_df$pct_of_check <- vapply(parsed_scores, function(p) {
                if (!is.na(p$pct_of_check)) p$pct_of_check else NA_real_
              }, numeric(1))
              filtered_df$desired_score_text <- raw_score
            } else {
              filtered_df$desired_direction <- NA_character_
              filtered_df$score_type <- NA_character_
              filtered_df$desired_lower <- NA_real_
              filtered_df$desired_upper <- NA_real_
              filtered_df$pct_above_check <- NA_real_
              filtered_df$pct_of_check <- NA_real_
              filtered_df$desired_score_text <- NA_character_
            }

            if ("Scale Option" %in% colnames(raw_tpp)) {
              raw_scales <- raw_tpp[["Scale Option"]][match_idx]
              parsed_scales <- lapply(raw_scales, tpp_parse_scale_option)
              filtered_df$scale_min <- vapply(parsed_scales, function(p) {
                if (!is.null(p) && !is.null(p$min)) p$min else NA_real_
              }, numeric(1))
              filtered_df$scale_max <- vapply(parsed_scales, function(p) {
                if (!is.null(p) && !is.null(p$max)) p$max else NA_real_
              }, numeric(1))
            } else {
              filtered_df$scale_min <- NA_real_
              filtered_df$scale_max <- NA_real_
            }
          }
        }
      }

      # Set TPP state
      if (nrow(filtered_df) > 0) {
        tpp_id_prepam(stored_tpp_id)
        tpp_filtered_traits(filtered_df)
      } else {
        tpp_id_prepam(stored_tpp_id)
        tpp_filtered_traits(NULL)
      }
    })

    output$selectionModelingTable <- DT::renderDT({
      req(data())
      req(input$initialSelectionStamp)

      dt <- data()
      req(!is.null(dt$modeling))
      req("analysisId" %in% colnames(dt$modeling))

      modeling_df <- dt$modeling
      modeling_df <- modeling_df[
        modeling_df$analysisId %in% input$initialSelectionStamp &
          modeling_df$module == "Init_prodAdv",
        ,
        drop = FALSE
      ]

      validate(
        need(nrow(modeling_df) > 0, "No modeling records found for the selected initial selection stamp.")
      )

      DT::datatable(
        modeling_df,
        rownames = FALSE,
        filter = "top",
        options = list(
          scrollX = TRUE,
          pageLength = 10
        )
      )
    })

    ###########################################
    # Excluded traits warning (for initial stamp)
    ###########################################

    output$excludedTraitsWarningUI <- renderUI({
      req(data())
      req(input$initialSelectionStamp)

      dt <- data()

      # Check for user-excluded traits
      excl_df <- dt$modeling[
        dt$modeling$analysisId %in% input$initialSelectionStamp &
          dt$modeling$module == "Init_prodAdv" &
          dt$modeling$parameter == "user_excluded_trait",
        , drop = FALSE
      ]

      ui_parts <- list()

      if (nrow(excl_df) > 0) {
        ui_parts <- c(ui_parts, list(
          div(
            style = "background-color:#f8d7da; color:#721c24; border:1px solid #f5c6cb; padding:12px; border-radius:6px; margin-bottom:12px;",
            tags$strong(icon("circle-xmark"), " Traits excluded by user:"),
            tags$p(paste(excl_df$trait, collapse = ", ")),
            tags$p("These traits were removed from the selection index at your request.")
          )
        ))
      }

      if (length(ui_parts) > 0) do.call(tagList, ui_parts) else NULL
    })

    ###########################################
    # Decision table (Phase 3)
    ###########################################

    # Reactive: build the decision table data
    table_decision_data <- reactive({
      req(data())
      req(input$initialSelectionStamp)

      dt <- data()
      initial_stamp <- input$initialSelectionStamp

      # Load table selection stamp if user chose one
      table_stamp <- input$tableSelectionStampLoad
      if (is.null(table_stamp) || table_stamp == "__none__") {
        table_stamp <- NULL
      }

      tryCatch({
        result <- cgiarPipeline::build_prodadv_decision_table_data(
          dt = dt,
          initial_stamp = initial_stamp,
          table_stamp = if (!is.null(table_stamp)) table_stamp else "__none__",
          plot_stamp = "__none__",
          final_stamp = "__none__"
        )
        result
      }, error = function(e) {
        showNotification(paste("Decision table error:", e$message), type = "error", duration = 10)
        NULL
      })
    })

    # Summary above decision table
    output$tableDecisionSummary <- renderUI({
      tbl <- table_decision_data()
      req(tbl)

      n_total <- nrow(tbl)
      n_selected <- sum(tbl$initial_decision == "SELECTED", na.rm = TRUE)
      n_not_selected <- sum(tbl$initial_decision == "NOT SELECTED", na.rm = TRUE)
      n_checks <- sum(tbl$initial_decision == "CHECK", na.rm = TRUE)

      div(
        style = "background-color:#2C3E50; color:white; padding:15px; border-radius:8px; margin-bottom:15px;",
        fluidRow(
          column(3, tags$h4(style = "margin:0;", paste0(n_total, " total candidates"))),
          column(3, tags$h4(style = "margin:0; color:#27ae60;", paste0(n_selected, " selected"))),
          column(3, tags$h4(style = "margin:0; color:#e74c3c;", paste0(n_not_selected, " not selected"))),
          column(3, tags$h4(style = "margin:0; color:#3498db;", paste0(n_checks, " checks")))
        )
      )
    })

    # TPP Decision Table controls: checkbox for maintain traits + compliance count (Task 11.1)
    output$tppDecisionTableControls <- renderUI({
      # Only show TPP controls when a TPP ID is active
      current_tpp <- tpp_id_prepam()
      filtered <- tpp_filtered_traits()
      has_tpp <- !is.null(current_tpp) && !is.null(filtered) &&
        is.data.frame(filtered) && nrow(filtered) > 0

      if (!has_tpp) return(NULL)

      # Compute compliance count
      tbl <- table_decision_data()
      if (is.null(tbl) || nrow(tbl) == 0) return(NULL)

      # Build tpp_criteria list from filtered TPP traits
      checks_info <- NULL
      if (!is.null(current_tpp)) {
        dt_local <- data()
        if (!is.null(dt_local$metadata$TPP[[current_tpp]]$checks_per_trait)) {
          checks_info <- dt_local$metadata$TPP[[current_tpp]]$checks_per_trait
        }
      }
      tpp_criteria <- tpp_build_criteria_list_from_filtered(filtered, tbl, checks_per_trait = checks_info)

      # Evaluate Essential_Improve criteria
      ei_matrix <- tpp_evaluate_all_criteria(tbl, tpp_criteria, category_filter = "Essential_Improve")
      compliance <- tpp_count_fully_compliant(ei_matrix)

      tagList(
        fluidRow(
          column(
            width = 6,
            checkboxInput(
              ns("showMaintainTraits"),
              "Show essential: maintain traits",
              value = FALSE
            )
          ),
          column(
            width = 6,
            tags$div(
              style = "padding-top: 8px; font-size: 1.1em; font-weight: 600; color: #2C3E50;",
              sprintf("%d out of %d individuals meet all Essential Improve TPP criteria",
                      compliance$n_meeting, compliance$n_total)
            )
          )
        )
      )
    })

    # Render decision table with gradient coloring
    output$tableDecisionDT <- DT::renderDT({
      tbl <- table_decision_data()
      req(tbl)

      tryCatch({

      # Get trait columns from modeling
      dt <- data()
      modeling_init <- dt$modeling[
        dt$modeling$analysisId %in% input$initialSelectionStamp &
          dt$modeling$module == "Init_prodAdv", , drop = FALSE
      ]
      selected_traits <- unique(modeling_init$trait[!is.na(modeling_init$trait) & nzchar(modeling_init$trait)])
      selected_traits <- selected_traits[!selected_traits %in%
        modeling_init$trait[modeling_init$parameter == "user_excluded_trait"]]
      selected_traits <- intersect(selected_traits, colnames(tbl))

      # --- TPP Integration (Task 11.1) ---
      # Determine if TPP is active and build criteria for border formatting
      current_tpp <- tpp_id_prepam()
      filtered_tpp <- tpp_filtered_traits()
      has_tpp <- !is.null(current_tpp) && !is.null(filtered_tpp) &&
        is.data.frame(filtered_tpp) && nrow(filtered_tpp) > 0

      tpp_criteria_list <- NULL
      ei_traits <- character(0)
      em_traits <- character(0)
      show_maintain <- FALSE

      if (has_tpp) {
        # Build criteria list for evaluation
        checks_info <- NULL
        if (!is.null(current_tpp)) {
          if (!is.null(dt$metadata$TPP[[current_tpp]]$checks_per_trait)) {
            checks_info <- dt$metadata$TPP[[current_tpp]]$checks_per_trait
          }
        }
        tpp_criteria_list <- tpp_build_criteria_list_from_filtered(filtered_tpp, tbl, checks_per_trait = checks_info)

        # Identify Essential_Improve and Essential_Maintain traits (by pheno_trait name)
        # For env-filtered traits, the column in tbl is the tpp_trait name (not pheno_trait),
        # so we also check tpp_trait names when building the trait lists.
        if ("category" %in% colnames(filtered_tpp) && "pheno_trait" %in% colnames(filtered_tpp)) {
          ei_mask <- !is.na(filtered_tpp$category) & filtered_tpp$category == "Essential_Improve"
          ei_pheno <- filtered_tpp$pheno_trait[ei_mask]
          ei_traits <- intersect(ei_pheno, colnames(tbl))
          # Also include tpp_trait names that exist as columns (env-filtered traits)
          if ("tpp_trait" %in% colnames(filtered_tpp)) {
            ei_tpp_names <- filtered_tpp$tpp_trait[ei_mask]
            ei_tpp_names <- ei_tpp_names[!is.na(ei_tpp_names)]
            ei_traits <- unique(c(ei_traits, intersect(ei_tpp_names, colnames(tbl))))
          }

          em_mask <- !is.na(filtered_tpp$category) & filtered_tpp$category == "Essential_Maintain"
          em_pheno <- filtered_tpp$pheno_trait[em_mask]
          em_traits <- intersect(em_pheno, colnames(tbl))
          # Also include tpp_trait names that exist as columns (env-filtered traits)
          if ("tpp_trait" %in% colnames(filtered_tpp)) {
            em_tpp_names <- filtered_tpp$tpp_trait[em_mask]
            em_tpp_names <- em_tpp_names[!is.na(em_tpp_names)]
            em_traits <- unique(c(em_traits, intersect(em_tpp_names, colnames(tbl))))
          }
        }

        # Filter selected_traits to show only EI (+ optionally EM) by default
        show_maintain <- isTRUE(input$showMaintainTraits)
        if (length(ei_traits) > 0 || length(em_traits) > 0) {
          tpp_display_traits <- ei_traits
          if (show_maintain) {
            tpp_display_traits <- c(tpp_display_traits, em_traits)
          }
          # Only filter if we have TPP traits to show; keep non-TPP traits too
          selected_traits <- intersect(tpp_display_traits, selected_traits)
        }
      }

      # Sort by index_value descending (checks participate in ordering)
      if ("index_value" %in% colnames(tbl)) {
        tbl <- tbl[order(-as.numeric(tbl$index_value)), , drop = FALSE]
      }

      # Original color scheme (colorblind-friendly), more saturated for gradient visibility:
      # SELECTED = blue, NOT SELECTED = orange, REVISE = yellow, CHECK = pink
      col_selected <- "#85C1E9"
      col_not_selected <- "#E8A87C"
      col_check <- "#C39BD3"
      col_revise <- "#F7DC6F"

      # Gradient helpers (get_quintile_opacity / blend_color) are defined at
      # file scope and shared with the "Performance across locations" table.

      # Build HTML display table
      display_df <- data.frame(designation = tbl$designation, stringsAsFactors = FALSE)

      # Get trait rules for direction-aware opacity
      # First try live UI rules, fallback to stored directions in modeling table
      trait_rules <- tryCatch(trait_rules_input(), error = function(e) {
        list()
      })

      # If trait_rules is empty (UI not available), get directions from modeling table
      if (length(trait_rules) == 0) {
        direction_rows <- modeling_init[modeling_init$parameter == "direction", , drop = FALSE]
        for (i in seq_len(nrow(direction_rows))) {
          tr_name <- direction_rows$trait[i]
          if (!is.null(tr_name) && nzchar(tr_name)) {
            trait_rules[[tr_name]] <- list(direction = direction_rows$value[i])
          }
        }
      }


      # Index value column with gradient (always higher_is_better = TRUE for index)
      if ("index_value" %in% colnames(tbl)) {
        idx_status <- tbl$initial_decision
        idx_status[is.na(idx_status)] <- "NOT SELECTED"
        idx_opacity <- get_quintile_opacity(tbl$index_value, idx_status, higher_is_better = TRUE)
        display_df$index_value <- mapply(function(val, st, op) {
          base_col <- if (st == "SELECTED") col_selected
                      else if (st == "CHECK") col_check
                      else if (st == "REVISE") col_revise
                      else col_not_selected
          bg <- blend_color(base_col, op)
          sprintf("<div style='background:%s; padding:6px; border-radius:4px; text-align:right; font-weight:600;'>%s</div>",
                  bg, format(round(as.numeric(val), 3), nsmall = 3))
        }, tbl$index_value, idx_status, idx_opacity, SIMPLIFY = TRUE)
      }

      # Trait columns with gradient
      # Get available trait decision columns from the data
      avail_trait_decisions <- grep("_trait_decision$", colnames(tbl), value = TRUE)

      for (tr in selected_traits) {
        trait_decision_col <- paste0(tr, "_trait_decision")
        if (trait_decision_col %in% avail_trait_decisions) {
          trait_status <- tbl[[trait_decision_col]]
          # Replace NA decisions with "NOT SELECTED" to avoid
          # "missing value where TRUE/FALSE needed" when traits have NA predictions
          trait_status[is.na(trait_status)] <- "NOT SELECTED"
        } else {
          # Trait decision not available â€” default all to SELECTED (no threshold applied)
          trait_status <- rep("SELECTED", nrow(tbl))
        }
        trait_vals <- tbl[[tr]]
        # Look up direction from trait_rules_input() for direction-aware gradient
        trait_direction <- trait_rules[[tr]]$direction
        hib <- is.null(trait_direction) || identical(trait_direction, "Higher is better")
        tr_opacity <- get_quintile_opacity(trait_vals, trait_status, higher_is_better = hib)
        display_df[[tr]] <- mapply(function(val, st, op) {
          base_col <- if (st == "SELECTED") col_selected
                      else if (st == "CHECK") col_check
                      else if (st == "REVISE") col_revise
                      else col_not_selected
          bg <- blend_color(base_col, op)
          txt <- if (is.na(val)) "" else format(round(as.numeric(val), 3), nsmall = 3)
          sprintf("<div style='background:%s; padding:6px; border-radius:4px; text-align:right;'>%s</div>", bg, txt)
        }, trait_vals, trait_status, tr_opacity, SIMPLIFY = TRUE)
      }

      # Index selection column (badge)
      display_df$index_selection <- sapply(tbl$initial_decision, function(st) {
        if (is.na(st)) st <- "NOT SELECTED"
        bg <- if (st == "SELECTED") col_selected
              else if (st == "CHECK") col_check
              else if (st == "REVISE") col_revise
              else col_not_selected
        sprintf("<div style='background:%s; padding:6px; border-radius:4px; text-align:center; font-weight:600;'>%s</div>", bg, st)
      })

      # Table selection column (dropdown, same as old final_decision)
      display_df$table_selection <- mapply(function(designation, initial_decision, table_decision) {
        if (identical(initial_decision, "CHECK")) {
          bg <- col_check
          return(sprintf("<div style='background:%s; padding:6px; border-radius:4px; text-align:center; font-weight:600;'>CHECK</div>", bg))
        }
        # Use table_decision if available, otherwise fall back to initial
        selected_value <- if (!is.na(table_decision) && nzchar(table_decision)) table_decision else initial_decision
        choices <- c("SELECTED", "NOT SELECTED", "REVISE")
        options <- vapply(choices, function(ch) {
          sel <- if (identical(ch, selected_value)) " selected" else ""
          sprintf("<option value='%s'%s>%s</option>", ch, sel, ch)
        }, character(1))
        sprintf("<select class='form-control table-decision-select' data-designation='%s' style='width:140px;'>%s</select>",
                htmltools::htmlEscape(designation), paste(options, collapse = ""))
      }, tbl$designation, tbl$initial_decision,
         if ("table_decision" %in% colnames(tbl)) tbl$table_decision else rep(NA_character_, nrow(tbl)),
         SIMPLIFY = TRUE)


      # --- TPP: Build criteria matrix for border formatting ---
      tpp_row_callback_js <- ""
      tpp_caption <- NULL

      if (has_tpp && !is.null(tpp_criteria_list) && length(tpp_criteria_list) > 0) {
        # Evaluate criteria for all individuals
        ei_em_filter <- c("Essential_Improve", "Essential_Maintain")
        criteria_matrix <- tpp_evaluate_all_criteria(tbl, tpp_criteria_list, category_filter = ei_em_filter)

        # Determine which displayed trait columns have criteria
        trait_cols_in_display <- intersect(selected_traits, names(tpp_criteria_list))

        if (length(trait_cols_in_display) > 0 && nrow(criteria_matrix) > 0) {
          # Build boolean matrix aligned with display_df rows
          # Column order in display_df: designation, index_value, traits..., index_selection, table_selection
          # We need the column indices (0-based) for the trait columns in display_df
          display_col_names <- colnames(display_df)

          # Build criteria JSON for each row
          n_rows <- nrow(display_df)
          n_trait_cols <- length(trait_cols_in_display)
          criteria_js_rows <- vector("list", n_rows)
          all_ei_met <- rep(0L, n_rows)

          # Build lookup by designation
          criteria_lookup <- list()
          if ("designation" %in% colnames(criteria_matrix)) {
            for (ci in seq_len(nrow(criteria_matrix))) {
              criteria_lookup[[as.character(criteria_matrix$designation[ci])]] <- criteria_matrix[ci, , drop = FALSE]
            }
          }

          for (i in seq_len(n_rows)) {
            desig <- as.character(display_df$designation[i])
            row_crit <- criteria_lookup[[desig]]
            trait_met <- rep(0L, n_trait_cols)

            ei_met_all <- TRUE
            for (j in seq_along(trait_cols_in_display)) {
              tr_name <- trait_cols_in_display[j]
              if (!is.null(row_crit) && tr_name %in% colnames(row_crit)) {
                val <- row_crit[[tr_name]]
                if (!is.null(val) && length(val) == 1 && !is.na(val) && isTRUE(val)) {
                  trait_met[j] <- 1L
                }
              }
            }

            # Check if ALL Essential_Improve traits are met (for Index border)
            for (ei_tr in ei_traits) {
              if (!is.null(row_crit) && ei_tr %in% colnames(row_crit)) {
                val <- row_crit[[ei_tr]]
                if (is.null(val) || length(val) != 1 || is.na(val) || !isTRUE(val)) {
                  ei_met_all <- FALSE
                }
              } else {
                ei_met_all <- FALSE
              }
            }
            if (length(ei_traits) == 0) ei_met_all <- FALSE
            all_ei_met[i] <- as.integer(ei_met_all)
            criteria_js_rows[[i]] <- trait_met
          }

          # Build JSON arrays
          criteria_json <- paste0(
            "[",
            paste(vapply(criteria_js_rows, function(row) {
              paste0("[", paste(row, collapse = ","), "]")
            }, character(1)), collapse = ","),
            "]"
          )
          index_border_json <- paste0("[", paste(all_ei_met, collapse = ","), "]")

          # Compute column indices (0-based) for trait columns in display_df
          trait_col_indices <- vapply(trait_cols_in_display, function(tc) {
            which(display_col_names == tc) - 1L
          }, integer(1))
          trait_col_indices_json <- paste0("[", paste(trait_col_indices, collapse = ","), "]")

          # Index column position (0-based)
          index_col_idx <- which(display_col_names == "index_value") - 1L
          if (length(index_col_idx) == 0) index_col_idx <- 1L

          tpp_row_callback_js <- sprintf(
            "
            var critMatrix = %s;
            var indexBorder = %s;
            var traitColIndices = %s;
            var indexColIdx = %d;

            if (dataIndex < critMatrix.length) {
              var rowCrit = critMatrix[dataIndex];
              for (var j = 0; j < traitColIndices.length; j++) {
                if (rowCrit[j] === 1) {
                  $('td:eq(' + traitColIndices[j] + ') div', row).css({'box-shadow': 'inset 0 0 0 1.5px #333333', 'border-radius': '4px', 'font-weight': '700'});
                }
              }
              if (indexBorder[dataIndex] === 1) {
                $('td:eq(' + indexColIdx + ') div', row).css({'box-shadow': 'inset 0 0 0 1.5px #333333', 'border-radius': '4px'});
              }
            }
            ",
            criteria_json, index_border_json, trait_col_indices_json, index_col_idx
          )
        }

        # Legend caption
        tpp_caption <- htmltools::tags$caption(
          style = "caption-side: bottom; text-align: left; font-size: 0.9em; padding-top: 8px;",
          htmltools::tags$span(
            style = "box-shadow: inset 0 0 0 1.5px #333333; border-radius: 4px; padding: 2px 6px; margin-right: 6px; display: inline-block;",
            "Cell"
          ),
          " = TPP criteria met (dark border indicates the individual meets the TPP desired score for that trait)"
        )
      }

      # Build the combined JS callback
      dropdown_js <- sprintf("
        table.on('change', 'select.table-decision-select', function() {
          var designation = $(this).data('designation');
          var value = $(this).val();
          Shiny.setInputValue('%s', {
            designation: designation,
            value: value,
            nonce: Math.random()
          }, {priority: 'event'});
        });
      ", ns("tableDecisionChange"))

      # Combine rowCallback if TPP is active
      row_callback_option <- NULL
      if (nzchar(tpp_row_callback_js)) {
        row_callback_option <- DT::JS(paste0(
          "function(row, data, displayNum, displayIndex, dataIndex) {",
          tpp_row_callback_js,
          "}"
        ))
      }

      dt_options <- list(
        scrollX = TRUE,
        scrollY = "500px",
        paging = FALSE,
        searching = TRUE,
        ordering = TRUE,
        order = list(),
        autoWidth = FALSE
      )

      # --- Numeric sorting fix: HTML-rendered cells sort lexicographically by
      # default. We append hidden columns with the raw numeric values and use
      # columnDefs.orderData so the visible columns sort numerically. ---
      numeric_cols <- c("index_value", selected_traits)
      numeric_cols <- intersect(numeric_cols, colnames(display_df))
      sort_col_defs <- list()

      if (length(numeric_cols) > 0) {
        n_visible <- ncol(display_df)
        for (k in seq_along(numeric_cols)) {
          col_nm <- numeric_cols[k]
          # Append a hidden column with the raw numeric values
          sort_col_name <- paste0(".sort_", col_nm)
          display_df[[sort_col_name]] <- as.numeric(tbl[[col_nm]])
          # visible column index (0-based)
          vis_idx <- which(colnames(display_df) == col_nm) - 1L
          # hidden sort column index (0-based)
          sort_idx <- ncol(display_df) - 1L
          sort_col_defs <- c(sort_col_defs, list(
            list(targets = vis_idx, orderData = sort_idx),
            list(targets = sort_idx, visible = FALSE)
          ))
        }
        dt_options$columnDefs <- sort_col_defs
      }

      if (!is.null(row_callback_option)) {
        dt_options$rowCallback <- row_callback_option
      }

      # Rename trait columns to TPP display names for the table headers
      # (skip hidden sort columns that start with ".sort_")
      col_names <- colnames(display_df)
      display_col_names <- vapply(col_names, function(cn) {
        if (startsWith(cn, ".sort_")) return(cn)
        if (cn %in% selected_traits) tpp_display_name(cn) else cn
      }, character(1))
      colnames(display_df) <- display_col_names

      DT::datatable(
        display_df,
        escape = FALSE,
        rownames = FALSE,
        selection = "none",
        class = "compact stripe hover nowrap decision-table",
        caption = tpp_caption,
        options = dt_options,
        callback = htmlwidgets::JS(dropdown_js)
      )
      }, error = function(e) {
        DT::datatable(data.frame(error = e$message))
      })
    })

    # Handle table selection changes from dropdown
    table_selection_overrides <- reactiveVal(data.frame(
      designation = character(),
      table_decision = character(),
      stringsAsFactors = FALSE
    ))

    observeEvent(input$tableDecisionChange, {
      change <- input$tableDecisionChange
      req(change$designation, change$value)

      overrides <- table_selection_overrides()
      if (change$designation %in% overrides$designation) {
        overrides$table_decision[overrides$designation == change$designation] <- change$value
      } else {
        overrides <- rbind(overrides, data.frame(
          designation = change$designation,
          table_decision = change$value,
          stringsAsFactors = FALSE
        ))
      }
      table_selection_overrides(overrides)
    }, ignoreInit = TRUE)
    # Save table selection
    observeEvent(input$saveTableSelection, {
      req(data())
      req(input$initialSelectionStamp)

      tbl <- table_decision_data()
      req(tbl)

      # Start from the base decisions (initial or previously loaded table stamp)
      # If table_decision exists and has non-NA values, use it; otherwise fall back to initial_decision
      base_decisions <- if ("table_decision" %in% colnames(tbl) && any(!is.na(tbl$table_decision))) {
        df <- tbl[, c("designation", "table_decision"), drop = FALSE]
        # Fill NAs with initial_decision where available
        if ("initial_decision" %in% colnames(tbl)) {
          df$table_decision <- ifelse(is.na(df$table_decision), tbl$initial_decision, df$table_decision)
        }
        df
      } else if ("initial_decision" %in% colnames(tbl)) {
        data.frame(designation = tbl$designation, table_decision = tbl$initial_decision, stringsAsFactors = FALSE)
      } else {
        data.frame(designation = tbl$designation, table_decision = "NOT SELECTED", stringsAsFactors = FALSE)
      }

      # Apply user overrides from dropdown changes
      overrides <- table_selection_overrides()
      if (nrow(overrides) > 0) {
        for (i in seq_len(nrow(overrides))) {
          idx <- which(base_decisions$designation == overrides$designation[i])
          if (length(idx) > 0) {
            base_decisions$table_decision[idx[1]] <- overrides$table_decision[i]
          }
        }
      }

      table_decisions <- base_decisions

      analysis_name <- if (nzchar(trimws(input$tableSelectionId))) trimws(input$tableSelectionId) else NULL

      # Determine if there is a previous table selection stamp to chain from
      table_stamp_load <- input$tableSelectionStampLoad
      prev_table_stamp <- if (!is.null(table_stamp_load) && nzchar(table_stamp_load) && table_stamp_load != "__none__") {
        table_stamp_load
      } else {
        NULL
      }

      dt_object <- data()
      result <- tryCatch({
        cgiarPipeline::saveTableProdAdvSelection(
          analysisId = as.numeric(Sys.time()),
          analysisIdName = analysis_name,
          initialSelectionStamp = input$initialSelectionStamp,
          tableSelectionStamp = prev_table_stamp,
          manual_decisions = table_decisions,
          dt_object = dt_object
        )
      }, error = function(e) {
        showNotification(paste("Save failed:", e$message), type = "error")
        return(NULL)
      })

      req(result)
      data(result)
      # Reset table overrides after successful save
      table_selection_overrides(data.frame(
        designation = character(),
        table_decision = character(),
        stringsAsFactors = FALSE
      ))
      showNotification("Table selection saved successfully.", type = "message")
    })

    ###########################################
    #Visualization (idx7)
    ###########################################


    review_plot_data <- eventReactive(input$renderReviewPlots, {
      req(data())
      req(input$initialSelectionStamp)
      req(input$reviewPlots)
      req(length(input$reviewPlots) > 0)

      out <- cgiarPipeline::build_prodadv_review_plot_data(
        dt = data(),
        initial_stamp = input$initialSelectionStamp,
        table_stamp = if (!is.null(input$vizTableSelectionStamp) && input$vizTableSelectionStamp != "__none__") input$vizTableSelectionStamp else "__none__",
        plot_stamp = input$plotSelectionStamp,
        final_stamp = input$finalSelectionStamp
      )

      out$selected_plots <- input$reviewPlots
      out
    }, ignoreInit = TRUE)

    # --- Performance across locations: rank-by trait selector (trait Y) ---
    output$perfTableTraitUI <- renderUI({
      plot_obj <- review_plot_data()
      req(plot_obj)

      trait_choices <- plot_obj$traits
      # Always add index_value as an option — it's computed during initial selection
      # and will be fetched from decision table data when selected
      trait_choices <- c("index_value", trait_choices)
      req(length(trait_choices) >= 1)

      # Use TPP display names as labels (index_value stays as "Index Value")
      display_names <- vapply(trait_choices, function(t) {
        if (t == "index_value") "Index Value" else tpp_display_name(t)
      }, character(1))
      named_choices <- stats::setNames(trait_choices, display_names)

      selectInput(
        ns("perfTableTrait"),
        label = tags$span(
          "Rank individuals by trait",
          tags$i(
            class = "glyphicon glyphicon-info-sign",
            title = "The top X% of individuals is determined by ranking on this trait."
          )
        ),
        choices = named_choices,
        selected = trait_choices[1]
      )
    })

    # --- Cluster assignments reactive ---
    lollipop_cluster_assignments <- reactive({
      weather_df <- tryCatch(tpe_weather_summary(), error = function(e) NULL)

      if (is.null(weather_df) || nrow(weather_df) == 0) {
        # Fallback: single cluster with all environments from STA data
        plot_obj <- review_plot_data()
        req(plot_obj)
        all_envs <- unique(as.character(plot_obj$sta_long$environment))
        result <- rep("All environments", length(all_envs))
        names(result) <- all_envs
        return(result)
      }

      # Aggregate across phases per environment
      env_summary <- aggregate(
        weather_df[, c("mean_temperature", "heat_stress_index", "rainfall", "rainfall_distribution_index", "humidity")],
        by = list(environment = weather_df$environment),
        FUN = mean,
        na.rm = TRUE
      )

      cluster_environments(env_summary)
    })

    # --- Trait directions (higher/lower is better) for gradient + ranking ---
    # Prefers the live trait rule cards; falls back to the stored directions in
    # the modeling table so the table also works when only a stamp is loaded.
    perf_trait_directions <- reactive({
      out <- tryCatch(trait_rules_input(), error = function(e) list())
      dirs <- list()
      for (nm in names(out)) {
        d <- out[[nm]]$direction
        if (!is.null(d) && !is.na(d)) dirs[[nm]] <- d
      }
      if (length(dirs) == 0) {
        req(data())
        req(input$initialSelectionStamp)
        m <- data()$modeling
        idx <- which(
          as.character(m$analysisId) == as.character(input$initialSelectionStamp) &
            m$module == "Init_prodAdv" &
            m$parameter == "direction"
        )
        for (i in idx) {
          tr <- m$trait[i]
          if (!is.na(tr) && nzchar(tr)) dirs[[tr]] <- m$value[i]
        }
      }
      dirs
    })

    # --- Performance across locations: per-cluster table data ---
    # Mirrors the old lollipop facets: one table per environment cluster, each
    # showing the top X% individuals (ranked on trait Y) with traits as columns.
    # Values are means across the environments belonging to that cluster.
    performance_table_data <- reactive({
      plot_obj <- review_plot_data()
      req(plot_obj)
      req(input$perfTableTrait)

      rank_trait <- input$perfTableTrait
      top_pct <- input$perfTablePct
      if (is.null(top_pct) || !is.finite(top_pct)) top_pct <- 20

      sta_long <- plot_obj$sta_long
      validate(need(!is.null(sta_long) && nrow(sta_long) > 0,
                    "No environment-level (STA) data available."))

      review_df <- plot_obj$review_df

      # Apply any manual plot overrides so status colouring stays in sync
      overrides <- plot_selection_overrides()
      if (!is.null(overrides) && nrow(overrides) > 0) {
        omap <- stats::setNames(overrides$plot_decision, overrides$designation)
        hit <- review_df$designation %in% names(omap)
        review_df$plot_status[hit] <- omap[review_df$designation[hit]]
      }

      # --- Resolve the ranking metric per designation ---
      if (identical(rank_trait, "index_value")) {
        if (!"index_value" %in% colnames(review_df)) {
          tbl <- tryCatch(
            cgiarPipeline::build_prodadv_decision_table_data(
              dt = data(),
              initial_stamp = input$initialSelectionStamp,
              plot_stamp = "__none__",
              final_stamp = "__none__"
            ),
            error = function(e) NULL
          )
          if (!is.null(tbl) && "index_value" %in% colnames(tbl)) {
            review_df <- merge(review_df, tbl[, c("designation", "index_value"), drop = FALSE],
                               by = "designation", all.x = TRUE)
          }
        }
        validate(need("index_value" %in% colnames(review_df),
                      "Index value is not available for this selection stamp."))
        rank_values <- stats::setNames(as.numeric(review_df$index_value), review_df$designation)
        rank_higher_is_better <- TRUE
      } else {
        validate(need(rank_trait %in% colnames(review_df),
                      paste("Trait not available:", rank_trait)))
        rank_values <- stats::setNames(as.numeric(review_df[[rank_trait]]), review_df$designation)
        d <- perf_trait_directions()[[rank_trait]]
        rank_higher_is_better <- is.null(d) || identical(d, "Higher is better")
      }
      rank_values <- rank_values[is.finite(rank_values)]
      validate(need(length(rank_values) > 0,
                    "No finite values available for the selected ranking trait."))

      # --- Pick the top X% of candidates; checks are always kept for reference ---
      status_map <- stats::setNames(as.character(review_df$plot_status), review_df$designation)
      is_check <- function(d) identical(unname(status_map[d]), "CHECK")
      check_desigs <- names(rank_values)[vapply(names(rank_values), is_check, logical(1))]
      cand_values <- rank_values[setdiff(names(rank_values), check_desigs)]

      n_top <- max(1L, ceiling(length(cand_values) * (top_pct / 100)))
      ordered_cands <- names(sort(cand_values, decreasing = rank_higher_is_better))
      top_desigs <- utils::head(ordered_cands, n_top)
      keep_desigs <- unique(c(top_desigs, check_desigs))

      # --- Restrict to the kept individuals ---
      sl <- sta_long[
        sta_long$designation %in% keep_desigs & is.finite(sta_long$predictedValue),
        , drop = FALSE
      ]
      validate(need(nrow(sl) > 0,
                    "No environment-level data available for the selected individuals."))

      # --- Assign each environment to its cluster (same source as the lollipop) ---
      clusters <- tryCatch(lollipop_cluster_assignments(), error = function(e) NULL)
      if (!is.null(clusters) && length(clusters) > 0) {
        lbl <- unname(clusters[as.character(sl$environment)])
        lbl[is.na(lbl)] <- "Unassigned"
        sl$cluster <- lbl
      } else {
        sl$cluster <- "All environments"
      }

      # Number of environments contributing to each cluster
      envs_per_cluster <- tapply(
        as.character(sl$environment), sl$cluster,
        function(x) length(unique(x))
      )

      # --- Mean per designation x cluster x trait, then traits as columns ---
      agg <- stats::aggregate(
        predictedValue ~ designation + cluster + trait,
        data = sl, FUN = mean, na.rm = TRUE
      )

      trait_levels <- unique(as.character(agg$trait))
      cluster_levels <- unique(as.character(agg$cluster))

      per_cluster <- list()
      for (cl in cluster_levels) {
        sub <- agg[agg$cluster == cl, , drop = FALSE]
        wide <- stats::reshape(
          sub[, c("designation", "trait", "predictedValue"), drop = FALSE],
          idvar = "designation", timevar = "trait", direction = "wide"
        )
        names(wide) <- sub("^predictedValue\\.", "", names(wide))

        wide$plot_status <- unname(status_map[wide$designation])
        wide$plot_status[is.na(wide$plot_status)] <- "NOT SELECTED"
        wide$rank_value <- unname(rank_values[wide$designation])

        # Best individuals first by default
        ord_key <- if (rank_higher_is_better) -wide$rank_value else wide$rank_value
        wide <- wide[order(ord_key, wide$designation), , drop = FALSE]

        per_cluster[[cl]] <- wide
      }

      list(
        clusters = cluster_levels,
        per_cluster = per_cluster,
        envs_per_cluster = envs_per_cluster,
        traits = trait_levels,
        rank_trait = rank_trait,
        rank_higher_is_better = rank_higher_is_better,
        n_top = length(top_desigs),
        n_candidates = length(cand_values),
        n_checks = length(check_desigs)
      )
    })

    # --- Performance across locations: summary line ---
    output$perfTableSummary <- renderUI({
      obj <- performance_table_data()
      req(obj)
      label <- if (identical(obj$rank_trait, "index_value")) "Index Value" else tpp_display_name(obj$rank_trait)
      div(
        style = "background-color:#2C3E50; color:white; padding:12px; border-radius:8px; margin-bottom:12px;",
        sprintf(
          "Top %d of %d candidates by %s%s — shown across %d environment cluster%s",
          obj$n_top, obj$n_candidates, label,
          if (obj$n_checks > 0) sprintf(" (plus %d checks)", obj$n_checks) else "",
          length(obj$clusters), if (length(obj$clusters) == 1) "" else "s"
        )
      )
    })

    # Stable output id / CSS class per cluster label
    perf_cluster_output_id <- function(cluster_label) {
      paste0("perfClusterDT_", gsub("[^A-Za-z0-9]", "_", cluster_label))
    }
    perf_cluster_css_class <- function(cluster_label) {
      paste0("perfclus-", gsub("[^A-Za-z0-9]", "-", cluster_label))
    }

    # Cluster -> colour, matching the "Trial locations by environment cluster" map
    perf_cluster_colors <- reactive({
      clusters <- tryCatch(lollipop_cluster_assignments(), error = function(e) NULL)
      if (is.null(clusters) || length(clusters) == 0) {
        return(stats::setNames(character(0), character(0)))
      }
      weather_df <- tryCatch(tpe_weather_summary(), error = function(e) NULL)
      cluster_palette_map(clusters, weather_df)
    })

    # --- One table per environment cluster (mirrors the old lollipop facets) ---
    output$perfClusterTablesUI <- renderUI({
      obj <- performance_table_data()
      req(obj)

      colmap <- tryCatch(perf_cluster_colors(), error = function(e) NULL)

      tagList(lapply(obj$clusters, function(cl) {
        n_env <- obj$envs_per_cluster[[cl]]

        # Cluster colour from the map; fall back to the default box blue
        col <- if (!is.null(colmap) && cl %in% names(colmap)) unname(colmap[[cl]]) else "#3C8DBC"
        tint <- blend_color(col, 0.08)   # very light wash of the cluster colour
        cls <- perf_cluster_css_class(cl)
        table_id <- ns(perf_cluster_output_id(cl))

        # Scope every rule to this cluster's wrapper so tables don't affect
        # each other or the decision table elsewhere in the app.
        # NOTE: descendant selectors (not "> .box") are required because
        # shinydashboard::box(width = ) nests .box inside a col-sm-* div.
        # !important is required on the header because the app-level
        # "table.decision-table thead th" rule uses !important, which beats
        # plain declarations regardless of selector specificity.
        css <- sprintf(paste0(
          # Card body: light wash of the cluster colour, clearly separated from
          # the table above so the coloured header reads as this table's title.
          ".%1$s { display: block; margin-top: 26px; }",
          ".%1$s .box { background-color: %3$s; border: 1px solid %2$s;",
          " border-top: none; box-shadow: 0 1px 2px rgba(0,0,0,0.08);",
          " margin-bottom: 6px; }",
          # Cluster name styled as a solid title bar
          ".%1$s .box-header { background-color: %2$s !important;",
          " padding: 12px 14px !important; border-radius: 2px 2px 0 0; }",
          ".%1$s .box-header .box-title { color: #ffffff !important;",
          " font-weight: 700 !important; font-size: 15px !important;",
          " letter-spacing: 0.2px; }",
          ".%1$s .box-header .btn-box-tool,",
          ".%1$s .box-header .btn-box-tool:hover { color: #ffffff !important;",
          " opacity: 0.95; }",
          ".%1$s .box-body { padding-top: 8px; }",
          # Table header row in the same cluster colour
          "#%4$s table.decision-table thead th,",
          "#%4$s table.dataTable thead th {",
          " background-color: %2$s !important;",
          " color: #ffffff !important;",
          " border-bottom: 2px solid %2$s !important; }",
          "#%4$s table.dataTable thead th.sorting:after,",
          "#%4$s table.dataTable thead th.sorting_asc:after,",
          "#%4$s table.dataTable thead th.sorting_desc:after { color: #ffffff !important; }",
          "#%4$s .dataTables_wrapper { background-color: transparent; }"
        ), cls, col, tint, table_id)

        div(
          class = cls,
          tags$style(HTML(css)),
          shinydashboard::box(
            width = 12,
            title = sprintf("%s (%d environment%s)", cl, n_env,
                            if (identical(as.integer(n_env), 1L)) "" else "s"),
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            DT::DTOutput(ns(perf_cluster_output_id(cl)))
          )
        )
      }))
    })

    # Register a DT renderer for each cluster table
    observe({
      obj <- performance_table_data()
      req(obj)

      dirs <- tryCatch(perf_trait_directions(), error = function(e) list())
      rank_label <- if (identical(obj$rank_trait, "index_value")) {
        "Index Value"
      } else {
        tpp_display_name(obj$rank_trait)
      }

      lapply(obj$clusters, function(cl) {
        local({
          cluster_label <- cl
          out_id <- perf_cluster_output_id(cluster_label)
          tbl <- obj$per_cluster[[cluster_label]]

          output[[out_id]] <- DT::renderDT({
            tryCatch({
              traits <- intersect(obj$traits, colnames(tbl))

              display_df <- data.frame(
                designation = tbl$designation,
                stringsAsFactors = FALSE
              )

              # Status badge (no manual decision control on this table)
              display_df$status <- vapply(tbl$plot_status, function(st) {
                sprintf(
                  "<div style='background:%s; padding:6px; border-radius:4px; text-align:center; font-weight:600;'>%s</div>",
                  gradient_status_color(st), st
                )
              }, character(1), USE.NAMES = FALSE)

              # Ranking metric column, gradient-filled
              display_df$rank_value <- gradient_cells(
                tbl$rank_value, tbl$plot_status,
                higher_is_better = obj$rank_higher_is_better, bold = TRUE
              )

              # One gradient column per trait, direction-aware
              for (tr in traits) {
                d <- dirs[[tr]]
                hib <- is.null(d) || identical(d, "Higher is better")
                display_df[[tr]] <- gradient_cells(tbl[[tr]], tbl$plot_status,
                                                   higher_is_better = hib)
              }

              # --- Numeric sorting fix: hidden sort columns ---
              numeric_perf_cols <- c("rank_value", traits)
              numeric_perf_cols <- intersect(numeric_perf_cols, colnames(display_df))
              sort_col_defs_perf <- list()
              if (length(numeric_perf_cols) > 0) {
                for (k in seq_along(numeric_perf_cols)) {
                  col_nm <- numeric_perf_cols[k]
                  sort_col_name <- paste0(".sort_", col_nm)
                  display_df[[sort_col_name]] <- as.numeric(tbl[[col_nm]])
                  vis_idx <- which(colnames(display_df) == col_nm) - 1L
                  sort_idx <- ncol(display_df) - 1L
                  sort_col_defs_perf <- c(sort_col_defs_perf, list(
                    list(targets = vis_idx, orderData = sort_idx),
                    list(targets = sort_idx, visible = FALSE)
                  ))
                }
              }

              # unname() is required: a named colnames vector makes DT treat it
              # as a new = old rename map and then fail resolving 'escape'.
              col_labels <- unname(c(
                "Designation", "Status", rank_label,
                vapply(traits, tpp_display_name, character(1)),
                rep("", length(numeric_perf_cols))  # hidden cols get empty labels
              ))
              # trim col_labels to match display_df width
              col_labels <- col_labels[seq_len(ncol(display_df))]

              DT::datatable(
                display_df,
                escape = FALSE,
                rownames = FALSE,
                selection = "none",
                colnames = col_labels,
                class = "compact stripe hover nowrap decision-table",
                options = list(
                  scrollX = TRUE,
                  paging = FALSE,
                  searching = TRUE,
                  ordering = TRUE,
                  order = list(),
                  autoWidth = FALSE,
                  dom = "ft",
                  columnDefs = sort_col_defs_perf
                )
              )
            }, error = function(e) {
              DT::datatable(data.frame(error = conditionMessage(e)))
            })
          })
        })
      })
    })

    # --- Cluster map output ---
    output$clusterMapPlot <- plotly::renderPlotly({
      clusters <- lollipop_cluster_assignments()
      req(length(clusters) > 0)

      weather_df <- tryCatch(tpe_weather_summary(), error = function(e) NULL)
      if (is.null(weather_df) || nrow(weather_df) == 0) {
        # Fallback: empty plot with message
        p <- plotly::plot_ly() %>% plotly::layout(
          annotations = list(list(
            text = "Weather/location data not available",
            x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE
          ))
        )
        return(p)
      }

      # Get period data if available
      period_data <- tryCatch(tpe_period_lookup(), error = function(e) NULL)

      build_cluster_map_plotly(
        weather_summary = weather_df,
        cluster_assignments = clusters,
        tpe_period_data = period_data
      )
    })


    output$scatterXTraitUI <- renderUI({
      plot_obj <- review_plot_data()
      req(plot_obj)

      trait_choices <- plot_obj$traits
      req(length(trait_choices) >= 2)

      # Use TPP display names as labels
      named_choices <- stats::setNames(trait_choices, vapply(trait_choices, tpp_display_name, character(1)))

      selectInput(
        ns("scatterXTrait"),
        "X-axis trait",
        choices = named_choices,
        selected = trait_choices[1]
      )
    })

    output$scatterYTraitUI <- renderUI({
      plot_obj <- review_plot_data()
      req(plot_obj)

      trait_choices <- plot_obj$traits
      req(length(trait_choices) >= 2)

      default_y <- if (length(trait_choices) >= 2) trait_choices[2] else trait_choices[1]

      # Use TPP display names as labels
      named_choices <- stats::setNames(trait_choices, vapply(trait_choices, tpp_display_name, character(1)))

      selectInput(
        ns("scatterYTrait"),
        "Y-axis trait",
        choices = named_choices,
        selected = default_y
      )
    })

    observeEvent(
      list(review_plot_data(), input$scatterXTrait),
      {
        plot_obj <- review_plot_data()
        req(plot_obj)

        trait_choices <- plot_obj$traits
        req(length(trait_choices) >= 2)

        y_choices <- trait_choices[trait_choices != input$scatterXTrait]
        named_y_choices <- stats::setNames(y_choices, vapply(y_choices, tpp_display_name, character(1)))

        updateSelectInput(
          session,
          "scatterYTrait",
          choices = named_y_choices,
          selected = y_choices[1]
        )
      },
      ignoreInit = TRUE
    )

    plot_selection_overrides <- reactiveVal(data.frame(
      designation = character(),
      plot_decision = character(),
      stringsAsFactors = FALSE
    ))

    tpe_weather_warning_cache <- reactiveVal(NULL)

    output$pairwiseScatterPlot <- plotly::renderPlotly({
      plot_obj <- review_plot_data()
      req(plot_obj)

      df <- plot_obj$review_df
      req(nrow(df) > 0)

      req(input$scatterXTrait, input$scatterYTrait)

      x_trait <- input$scatterXTrait
      y_trait <- input$scatterYTrait

      validate(
        need(x_trait %in% colnames(df), paste("Trait not found in data:", x_trait)),
        need(y_trait %in% colnames(df), paste("Trait not found in data:", y_trait)),
        need(x_trait != y_trait, "Please select two different traits.")
      )

      x_rel_col <- paste0("reliability_", x_trait)
      y_rel_col <- paste0("reliability_", y_trait)

      if (!x_rel_col %in% colnames(df)) df[[x_rel_col]] <- NA_real_
      if (!y_rel_col %in% colnames(df)) df[[y_rel_col]] <- NA_real_

      df_plot <- df[, c("designation", "plot_status", x_trait, y_trait, x_rel_col, y_rel_col), drop = FALSE]
      names(df_plot)[names(df_plot) == x_trait] <- "x_value"
      names(df_plot)[names(df_plot) == y_trait] <- "y_value"
      names(df_plot)[names(df_plot) == x_rel_col] <- "x_reliability"
      names(df_plot)[names(df_plot) == y_rel_col] <- "y_reliability"

      df_plot <- df_plot[stats::complete.cases(df_plot[, c("x_value", "y_value")]), , drop = FALSE]

      overrides <- plot_selection_overrides()
      if (nrow(overrides) > 0) {
        df_plot <- merge(df_plot, overrides, by = "designation", all.x = TRUE)
        df_plot$plot_status <- ifelse(!is.na(df_plot$plot_decision),
                                     df_plot$plot_decision, as.character(df_plot$plot_status))
        df_plot$plot_decision <- NULL
      }

      validate(need(nrow(df_plot) > 0, "No complete observations available for the selected trait pair."))

      # --- Determine trait directions for axis inversion ---
      # "Lower is better" traits get a reversed axis so upper-right is always ideal.
      # "Acceptable range" traits (both lower and upper bounds) are NOT inverted;
      # their two TPP lines visually mark the desired band instead.
      get_trait_direction <- function(trait_name) {
        # Try from TPP data first
        tpp_df_local <- tryCatch(tpp_filtered_traits(), error = function(e) NULL)
        if (!is.null(tpp_df_local) && is.data.frame(tpp_df_local) && nrow(tpp_df_local) > 0) {
          if ("pheno_trait" %in% colnames(tpp_df_local) && "desired_direction" %in% colnames(tpp_df_local)) {
            t_row <- tpp_df_local[tpp_df_local$pheno_trait == trait_name, , drop = FALSE]
            if (nrow(t_row) == 0 && "tpp_trait" %in% colnames(tpp_df_local)) {
              t_row <- tpp_df_local[tpp_df_local$tpp_trait == trait_name, , drop = FALSE]
            }
            if (nrow(t_row) > 0) {
              dir_val <- t_row$desired_direction[1]
              # Check if this is a range trait (both bounds present) — do not invert
              has_both <- FALSE
              if ("desired_lower" %in% colnames(t_row) && "desired_upper" %in% colnames(t_row)) {
                has_both <- !is.na(t_row$desired_lower[1]) && !is.na(t_row$desired_upper[1])
              }
              if (has_both) return("range")
              if (!is.na(dir_val) && dir_val == "lower") return("lower")
              return("higher")
            }
          }
        }
        # Fallback to trait_rules_input()
        rules <- tryCatch(trait_rules_input(), error = function(e) list())
        if (trait_name %in% names(rules)) {
          rule_dir <- rules[[trait_name]]$direction
          if (!is.null(rule_dir) && identical(rule_dir, "Lower is better")) return("lower")
        }
        "higher"
      }

      x_direction <- get_trait_direction(x_trait)
      y_direction <- get_trait_direction(y_trait)
      x_invert <- identical(x_direction, "lower")
      y_invert <- identical(y_direction, "lower")

      # Compute absolute opacity based on reliability (NOT relative to population)
      # reliability >= 0.7 â†’ opacity 1.0; reliability = 0 â†’ opacity 0.15
      reliability_to_opacity <- function(rel) {
        rel[is.na(rel)] <- 0.7  # default to full opacity if NA
        pmin(1.0, 0.15 + (pmin(rel, 0.7) / 0.7) * 0.85)
      }

      df_plot$opacity <- reliability_to_opacity(
        pmin(df_plot$x_reliability, df_plot$y_reliability)
      )
      # Checks always full opacity
      df_plot$opacity[df_plot$plot_status == "CHECK"] <- 1.0

      df_plot$plot_status <- factor(df_plot$plot_status, levels = c("SELECTED", "NOT SELECTED", "REVISE", "CHECK"))

      # Colors: original scheme
      status_colors <- c("SELECTED" = "#0072B2", "NOT SELECTED" = "#D55E00", "REVISE" = "#F9A825", "CHECK" = "#C2185B")

      p <- ggplot2::ggplot(df_plot, ggplot2::aes(
        x = x_value, y = y_value, key = designation,
        text = paste0("Designation: ", designation,
                      "<br>", x_trait, ": ", signif(x_value, 4),
                      "<br>", x_trait, " reliability: ", signif(x_reliability, 3),
                      "<br>", y_trait, ": ", signif(y_value, 4),
                      "<br>", y_trait, " reliability: ", signif(y_reliability, 3),
                      "<br>Status: ", plot_status)
      ))

      # Add regression lines if requested
      show_reg <- isTRUE(input$scatterShowRegression)
      if (show_reg) {
        # Regression lines per status group (using geom_abline for plotly compatibility)
        for (status_level in c("SELECTED", "CHECK")) {
          sub_df <- df_plot[df_plot$plot_status == status_level, ]
          if (nrow(sub_df) >= 3) {
            fit <- tryCatch(lm(y_value ~ x_value, data = sub_df), error = function(e) NULL)
            if (!is.null(fit)) {
              p <- p + ggplot2::geom_abline(
                intercept = coef(fit)[1], slope = coef(fit)[2],
                color = status_colors[status_level], linetype = "dashed", linewidth = 0.8
              )
            }
          }
        }

        # Reference mean lines
        for (status_level in c("SELECTED", "NOT SELECTED", "CHECK")) {
          sub_df <- df_plot[df_plot$plot_status == status_level, ]
          if (nrow(sub_df) > 0) {
            p <- p +
              ggplot2::geom_vline(xintercept = mean(sub_df$x_value, na.rm = TRUE),
                                  color = status_colors[status_level], linetype = "dotted", linewidth = 0.5) +
              ggplot2::geom_hline(yintercept = mean(sub_df$y_value, na.rm = TRUE),
                                  color = status_colors[status_level], linetype = "dotted", linewidth = 0.5)
          }
        }
      }

      # Points with reliability-based opacity
      # Separate layers: REVISE gets star shape, checks get diamond, others get circle
      df_normal <- df_plot[df_plot$plot_status %in% c("SELECTED", "NOT SELECTED"), , drop = FALSE]
      df_revise <- df_plot[df_plot$plot_status == "REVISE", , drop = FALSE]
      df_checks <- df_plot[df_plot$plot_status == "CHECK", , drop = FALSE]

      if (nrow(df_normal) > 0) {
        p <- p +
          ggplot2::geom_point(
            data = df_normal,
            ggplot2::aes(fill = plot_status, alpha = opacity),
            shape = 21, stroke = 0.5, color = "grey40", size = 2.8
          )
      }

      if (nrow(df_revise) > 0) {
        p <- p +
          ggplot2::geom_point(
            data = df_revise,
            ggplot2::aes(fill = plot_status, alpha = opacity),
            shape = 24, stroke = 0.8, color = "grey20", size = 3.5
          )
      }

      if (nrow(df_checks) > 0) {
        p <- p +
          ggplot2::geom_point(
            data = df_checks,
            ggplot2::aes(fill = plot_status),
            shape = 23, stroke = 1, color = "white", size = 5, alpha = 1
          )
      }

      p <- p +
        ggplot2::scale_fill_manual(values = status_colors) +
        ggplot2::scale_alpha_identity() +
        ggplot2::labs(
          x = paste0(tpp_display_name(x_trait), if (x_invert) " (inverted: lower is better)" else ""),
          y = paste0(tpp_display_name(y_trait), if (y_invert) " (inverted: lower is better)" else ""),
          fill = "Status"
        ) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(legend.position = "bottom")

      # Invert axes for "lower is better" traits so upper-right is always ideal
      if (x_invert) p <- p + ggplot2::scale_x_reverse()
      if (y_invert) p <- p + ggplot2::scale_y_reverse()

      fig <- plotly::ggplotly(p, tooltip = "text", source = ns("pairwiseScatter"))

      # TPP desired score reference lines (Task 11.2, Requirements 7.1-7.5)
      if (isTRUE(input$scatterShowTppLines) && !is.null(tpp_id_prepam()) && !is.null(tpp_filtered_traits())) {
        tpp_df <- tpp_filtered_traits()

        # Helper: resolve desired score bounds for a trait, handling relative types
        resolve_tpp_bounds <- function(trait_name) {
          tryCatch({
            if (!("pheno_trait" %in% colnames(tpp_df))) return(NULL)

            # Match by pheno_trait or tpp_trait
            t_match <- tpp_df[tpp_df$pheno_trait == trait_name, , drop = FALSE]
            if (nrow(t_match) == 0 && "tpp_trait" %in% colnames(tpp_df)) {
              t_match <- tpp_df[tpp_df$tpp_trait == trait_name, , drop = FALSE]
            }
            if (nrow(t_match) == 0) return(NULL)

            score_type <- if ("score_type" %in% colnames(t_match)) t_match$score_type[1] else NA_character_
            d_lower <- if ("desired_lower" %in% colnames(t_match)) t_match$desired_lower[1] else NA_real_
            d_upper <- if ("desired_upper" %in% colnames(t_match)) t_match$desired_upper[1] else NA_real_

            if (!is.na(score_type) && score_type == "relative") {
              # For relative: compute absolute threshold from check mean
              pct_above <- if ("pct_above_check" %in% colnames(t_match)) t_match$pct_above_check[1] else NA_real_
              pct_of <- if ("pct_of_check" %in% colnames(t_match)) t_match$pct_of_check[1] else NA_real_

              # Get check designations from metadata
              check_desigs <- NULL
              current_tpp <- isolate(tpp_id_prepam())
              if (!is.null(current_tpp)) {
                dt_local <- isolate(data())
                checks_info <- dt_local$metadata$TPP[[current_tpp]]$checks_per_trait
                tpp_trait_nm <- if ("tpp_trait" %in% colnames(t_match)) t_match$tpp_trait[1] else trait_name
                check_desigs <- checks_info[[tpp_trait_nm]]
              }

              if (!is.null(check_desigs) && length(check_desigs) > 0 &&
                  trait_name %in% colnames(df) && "designation" %in% colnames(df)) {
                check_vals <- as.numeric(df[[trait_name]][df$designation %in% check_desigs])
                check_vals <- check_vals[!is.na(check_vals)]
                if (length(check_vals) > 0) {
                  check_mean <- mean(check_vals)
                  if (!is.na(pct_above) && is.finite(pct_above)) {
                    target <- check_mean * (1 + pct_above / 100)
                    return(list(lower = target, upper = NULL))
                  } else if (!is.na(pct_of) && is.finite(pct_of)) {
                    target <- check_mean * (pct_of / 100)
                    return(list(lower = target, upper = NULL))
                  }
                }
              }
              return(NULL)
            }

            # Absolute type
            has_lower <- !is.na(d_lower) && is.finite(d_lower)
            has_upper <- !is.na(d_upper) && is.finite(d_upper)
            if (!has_lower && !has_upper) return(NULL)
            list(
              lower = if (has_lower) d_lower else NULL,
              upper = if (has_upper) d_upper else NULL
            )
          }, error = function(e) NULL)
        }

        x_trait_desired <- resolve_tpp_bounds(x_trait)
        y_trait_desired <- resolve_tpp_bounds(y_trait)

        # Apply TPP reference lines (omits lines when both are NULL)
        fig <- tpp_add_scatterplot_lines(fig, x_trait_desired, y_trait_desired)
      }

      fig <- plotly::event_register(fig, "plotly_click")
      fig
    })

    observeEvent(plotly::event_data("plotly_click", source = ns("pairwiseScatter")), {
      click <- plotly::event_data("plotly_click", source = ns("pairwiseScatter"))
      req(click)

      clicked_designation <- click$key
      req(clicked_designation)

      base_df <- review_plot_data()$review_df
      base_status <- base_df$plot_status[base_df$designation == clicked_designation][1]

      if (is.na(base_status) || base_status == "CHECK") return()

      overrides <- plot_selection_overrides()

      if (clicked_designation %in% overrides$designation) {
        current_status <- overrides$plot_decision[overrides$designation == clicked_designation][1]
      } else {
        current_status <- base_status
      }

      new_status <- if (current_status == "SELECTED") "NOT SELECTED" else if (current_status == "NOT SELECTED") "REVISE" else "SELECTED"

      if (new_status == base_status) {
        overrides <- overrides[overrides$designation != clicked_designation, , drop = FALSE]
      } else if (clicked_designation %in% overrides$designation) {
        overrides$plot_decision[overrides$designation == clicked_designation] <- new_status
      } else {
        overrides <- rbind(
          overrides,
          data.frame(
            designation = clicked_designation,
            plot_decision = new_status,
            stringsAsFactors = FALSE
          )
        )
      }

      plot_selection_overrides(overrides)
    })

    # output$radarDesignationUI <- renderUI({
    # plot_obj <- review_plot_data()
    # req(plot_obj)
    #
    # df <- plot_obj$review_df
    # req(nrow(df) > 0)
    #
    # df <- df[, c("designation", "plot_status"), drop = FALSE]
    # df <- df[!is.na(df$designation), , drop = FALSE]
    #
    # df$plot_status[is.na(df$plot_status) | df$plot_status == ""] <- "NOT SELECTED"
    #
    # df$label <- paste0(
    # df$plot_status,
    # " - ",
    # df$designation
    # )
    #
    # choices <- df$designation
    # names(choices) <- df$label
    #
    # selected <- df$designation[df$plot_status == "CHECK"]
    # selected <- selected[1]
    #
    # selectizeInput(
    # ns("radarDesignations"),
    # label = "Designations to display in radar plot",
    # choices = choices,
    # selected = selected,
    # multiple = TRUE,
    # options = list(
    # maxItems = 4,
    # placeholder = "Select up to 4 designations"
    # )
    # )
    # })

    # build_radar_plot <- function(df_panel, trait_cols, trait_labels = NULL, title_text = NULL) {
    # req(nrow(df_panel) > 0)
    # if (is.null(trait_labels)) trait_labels <- trait_cols
    #
    # designation_cols <- grDevices::hcl.colors(
    # n = nrow(df_panel),
    # palette = "Dark 3"
    # )
    #
    # p <- plotly::plot_ly()
    #
    # for (i in seq_len(nrow(df_panel))) {
    # values <- as.numeric(df_panel[i, trait_cols, drop = TRUE])
    #
    # status_i <- df_panel$plot_status[i]
    # if (is.na(status_i) || !nzchar(status_i)) {
    # status_i <- "NOT SELECTED"
    # }
    #
    # color_i <- designation_cols[i]
    #
    # trace_name <- paste0(status_i, " - ", df_panel$designation[i])
    #
    # p <- p %>%
    # plotly::add_trace(
    # type = "scatterpolar",
    # mode = "lines+markers",
    # r = c(values, values[1]),
    # theta = c(trait_labels, trait_labels[1]),
    # name = trace_name,
    # hovertemplate = paste0(
    # "Designation: ", df_panel$designation[i],
    # "<br>Decision: ", status_i,
    # "<br>Trait: %{theta}",
    # "<br>Scaled value: %{r:.3f}<extra></extra>"
    # ),
    # line = list(
    # width = 2,
    # color = color_i
    # ),
    # marker = list(
    # size = 5,
    # color = color_i
    # )
    # )
    # }
    #
    # p %>%
    # plotly::layout(
    # title = title_text,
    # polar = list(
    # radialaxis = list(
    # visible = TRUE,
    # range = c(0, 1)
    # )
    # ),
    # showlegend = TRUE
    # )
    # }

    # output$radarPlot <- plotly::renderPlotly({
    # plot_obj <- review_plot_data()
    # req(plot_obj)
    #
    # req(input$radarDesignations)
    # req(length(input$radarDesignations) > 0)
    #
    # validate(
    # need(length(input$radarDesignations) <= 4, "Please select a maximum of 4 designations.")
    # )
    #
    # df <- plot_obj$review_df
    # trait_cols <- plot_obj$traits
    #
    # req(length(trait_cols) > 0)
    #
    # df_radar <- df[
    # df$designation %in% input$radarDesignations,
    # c("designation", "plot_status", trait_cols),
    # drop = FALSE
    # ]
    #
    # validate(
    # need(nrow(df_radar) > 0, "No selected designations available for radar plot."),
    # need(nrow(df_radar) <= 4, "Radar plot supports a maximum of 4 designations.")
    # )
    #
    # df_radar <- df_radar[stats::complete.cases(df_radar[, trait_cols, drop = FALSE]), , drop = FALSE]
    #
    # validate(
    # need(nrow(df_radar) > 0, "Selected designations do not have complete trait values for the radar plot.")
    # )
    #
    # # Get trait directions from modeling table (to invert "lower is better" traits)
    # dt <- data()
    # modeling_init <- dt$modeling[
    # dt$modeling$analysisId %in% input$initialSelectionStamp &
    # dt$modeling$module == "Init_prodAdv", , drop = FALSE
    # ]
    # trait_directions <- list()
    # for (tr in trait_cols) {
    # dir_row <- modeling_init[modeling_init$trait == tr & modeling_init$parameter == "direction", , drop = FALSE]
    # if (nrow(dir_row) > 0) {
    # trait_directions[[tr]] <- dir_row$value[1]
    # } else {
    # trait_directions[[tr]] <- "Higher is better"  # default
    # }
    # }
    #
    # for (tr in trait_cols) {
    # x <- df[[tr]]
    # rng <- range(x, na.rm = TRUE)
    #
    # if (all(is.finite(rng)) && diff(rng) > 0) {
    # scaled <- (df_radar[[tr]] - rng[1]) / diff(rng)
    # # Invert "lower is better" traits so better always points outward
    # if (identical(trait_directions[[tr]], "Lower is better")) {
    # scaled <- 1 - scaled
    # }
    # df_radar[[tr]] <- scaled
    # } else {
    # df_radar[[tr]] <- 0.5
    # }
    # }
    #
    # # Update trait labels to indicate direction
    # radar_labels <- sapply(trait_cols, function(tr) {
    # if (identical(trait_directions[[tr]], "Lower is better")) {
    # paste0(tr, " (â†“)")
    # } else {
    # tr
    # }
    # })
    #
    # build_radar_plot(
    # df_panel = df_radar,
    # trait_cols = trait_cols,
    # trait_labels = radar_labels,
    # title_text = "Radar plot (outward = better)"
    # )
    # })

    tpe_period_lookup <- reactive({
      req(data())

      dt <- data()
      dt_pheno <- dt$data$pheno
      mt_pheno <- dt$metadata$pheno

      period_cols <- get_prodadv_period_cols(dt)

      if (is.null(period_cols$year_col) && is.null(period_cols$season_col)) {
        return(NULL)
      }

      designation_col <- mt_pheno$value[mt_pheno$parameter == "designation"]
      environment_col <- mt_pheno$value[mt_pheno$parameter == "environment"]

      validate(
        need(length(designation_col) > 0, "Could not identify designation column."),
        need(length(environment_col) > 0, "Could not identify environment column.")
      )

      out <- data.frame(
        designation = dt_pheno[[designation_col[1]]],
        environment = dt_pheno[[environment_col[1]]],
        stringsAsFactors = FALSE
      )

      if (!is.null(period_cols$year_col)) {
        out$year <- as.character(dt_pheno[[period_cols$year_col]])
      } else {
        out$year <- NA_character_
      }

      if (!is.null(period_cols$season_col)) {
        out$season <- as.character(dt_pheno[[period_cols$season_col]])
      } else {
        out$season <- NA_character_
      }

      if (!is.null(period_cols$year_col) && !is.null(period_cols$season_col)) {
        out$tpe_period <- paste(out$year, out$season, sep = " - ")
      } else if (!is.null(period_cols$year_col)) {
        out$tpe_period <- out$year
      } else {
        out$tpe_period <- out$season
      }

      out <- out[
        !is.na(out$designation) &
          !is.na(out$environment) &
          !is.na(out$tpe_period) &
          nzchar(out$tpe_period),
        ,
        drop = FALSE
      ]

      unique(out)
    })

    output$tpePeriodUI <- renderUI({
      lookup <- tpe_period_lookup()

      if (is.null(lookup)) {
        return(NULL)
      }

      plot_obj <- review_plot_data()
      req(plot_obj)

      candidate_designations <- unique(as.character(plot_obj$sta_long$designation))

      lookup <- lookup[
        lookup$designation %in% candidate_designations,
        ,
        drop = FALSE
      ]

      period_choices <- sort(unique(lookup$tpe_period))
      period_choices <- period_choices[!is.na(period_choices) & nzchar(period_choices)]

      validate(
        need(length(period_choices) > 0, "No year/season values available for the selected candidates.")
      )

      selectInput(
        ns("tpePeriod"),
        label = tags$span(
          "Year / season",
          tags$i(
            class = "glyphicon glyphicon-info-sign",
            style = "color:#FFFFFF",
            title = "Restrict the TPE performance and environmental surface to the selected year/season."
          )
        ),
        choices = period_choices,
        selected = period_choices[1],
        multiple = FALSE
      )
    })

    hovered_tpe_environment <- reactiveVal(NULL)

    get_weather_phase_label <- function(date_vec, planting_date, harvest_date) {
      total_days <- as.numeric(harvest_date - planting_date) + 1

      if (!is.finite(total_days) || total_days <= 1) {
        return(rep(NA_character_, length(date_vec)))
      }

      split_day <- planting_date + floor(total_days * 0.40) - 1

      ifelse(date_vec <= split_day, "Early", "Late")
    }

    compute_rainfall_distribution_index <- function(rain_vec) {
      rain_vec <- rain_vec[is.finite(rain_vec)]
      if (length(rain_vec) == 0) return(NA_real_)

      rainy_days <- sum(rain_vec >= 1, na.rm = TRUE)
      total_rain <- sum(rain_vec, na.rm = TRUE)

      if (total_rain <= 0) return(0)

      rainy_days / length(rain_vec)
    }

    compute_heat_stress_index <- function(temp_vec, threshold = 32) {
      temp_vec <- temp_vec[is.finite(temp_vec)]
      if (length(temp_vec) == 0) return(NA_real_)

      mean(temp_vec > threshold, na.rm = TRUE) * 100
    }

    tpe_weather_resolution_info <- reactive({
      req(data())
      req(!is.null(data()$data))
      req(!is.null(data()$data$weather))

      weather_df <- data()$data$weather

      validate(
        need(nrow(weather_df) > 0, "Weather data is empty."),
        need(
          all(c("environment", "LON", "LAT", "T2M", "PRECTOTCORR", "RH2M") %in% colnames(weather_df)),
          "Weather data must contain environment, LON, LAT, T2M, PRECTOTCORR, and RH2M."
        )
      )

      time_mode <- NULL

      if ("datetime" %in% colnames(weather_df) && any(!is.na(weather_df$datetime))) {
        weather_df$time_index <- parse_weather_time(weather_df$datetime)
        time_mode <- "datetime"

        validate(
          need(
            any(!is.na(weather_df$time_index)),
            "Weather 'datetime' column exists but could not be parsed into valid timestamps."
          )
        )

      } else if ("date" %in% colnames(weather_df) && any(!is.na(weather_df$date))) {
        weather_df$time_index <- parse_weather_date(weather_df$date)
        time_mode <- "date"

        validate(
          need(
            any(!is.na(weather_df$time_index)),
            "Weather 'date' column exists but could not be parsed into valid dates."
          )
        )

      } else {
        validate(
          need(FALSE, "Weather data must contain either 'datetime' or 'date' to build the TPE environmental plot.")
        )
      }

      weather_df <- weather_df[!is.na(weather_df$time_index), , drop = FALSE]
      weather_df <- weather_df[!is.na(weather_df$environment), , drop = FALSE]

      validate(
        need(nrow(weather_df) > 0, "Weather data has no valid time values.")
      )

      infer_weather_resolution <- function(x) {
        x <- sort(unique(x))
        if (length(x) < 2) return(NA_character_)

        if (inherits(x, "Date")) {
          x_num <- as.numeric(x) * 24 * 3600
        } else if (inherits(x, "POSIXt")) {
          x_num <- as.numeric(x)
        } else {
          return(NA_character_)
        }

        diffs <- diff(x_num)
        med_diff <- stats::median(diffs, na.rm = TRUE)

        if (!is.finite(med_diff)) return(NA_character_)

        if (med_diff <= 3 * 3600) return("hourly")
        if (med_diff <= 36 * 3600) return("daily")
        if (med_diff >= 20 * 24 * 3600) return("monthly")

        "unsupported"
      }

      resolution_by_env <- vapply(
        split(weather_df$time_index, weather_df$environment),
        infer_weather_resolution,
        character(1)
      )

      list(
        weather_df = weather_df,
        time_mode = time_mode,
        resolution_by_env = resolution_by_env,
        supported_envs = names(resolution_by_env)[resolution_by_env %in% c("daily", "hourly")],
        monthly_envs = names(resolution_by_env)[resolution_by_env == "monthly"],
        bad_envs = names(resolution_by_env)[!resolution_by_env %in% c("daily", "hourly", "monthly")]
      )
    })

    observe({
      info <- tpe_weather_resolution_info()

      unsupported_envs <- sort(unique(c(info$monthly_envs, info$bad_envs)))
      current_key <- paste(unsupported_envs, collapse = "||")
      last_key <- tpe_weather_warning_cache()

      if (length(unsupported_envs) == 0) {
        tpe_weather_warning_cache(NULL)
        return()
      }

      if (!identical(current_key, last_key)) {
        shiny::showNotification(
          paste0(
            "Some environments were excluded from the TPE environmental plot because their weather resolution is not supported. ",
            "Supported resolutions: daily and hourly. ",
            if (length(info$monthly_envs) > 0) {
              paste0("Monthly: ", paste(info$monthly_envs, collapse = ", "), ". ")
            } else {
              ""
            },
            if (length(info$bad_envs) > 0) {
              paste0("Other unsupported: ", paste(info$bad_envs, collapse = ", "), ".")
            } else {
              ""
            }
          ),
          type = "warning",
          duration = 8
        )

        tpe_weather_warning_cache(current_key)
      }
    })

    tpe_weather_summary <- reactive({
      info <- tpe_weather_resolution_info()

      validate(
        need(length(info$supported_envs) > 0,
             "TPE environmental visualisation currently supports only daily or hourly weather data. Monthly weather data is not supported.")
      )

      df <- info$weather_df
      df <- df[df$environment %in% info$supported_envs, , drop = FALSE]
      df <- df[order(df$environment, df$time_index), , drop = FALSE]

      compute_heat_stress_index <- function(temp_vec, threshold = 32) {
        temp_vec <- temp_vec[is.finite(temp_vec)]
        if (length(temp_vec) == 0) return(NA_real_)
        mean(temp_vec > threshold, na.rm = TRUE) * 100
      }

      compute_rainfall_distribution_index_daily <- function(rain_vec) {
        rain_vec <- rain_vec[is.finite(rain_vec)]
        if (length(rain_vec) == 0) return(NA_real_)

        total_rain <- sum(rain_vec, na.rm = TRUE)
        if (total_rain <= 0) return(0)

        rainy_days <- sum(rain_vec >= 1, na.rm = TRUE)
        rainy_days / length(rain_vec)
      }

      compute_rainfall_distribution_index_hourly <- function(rain_vec, time_vec) {
        keep <- is.finite(rain_vec) & !is.na(time_vec)
        rain_vec <- rain_vec[keep]
        time_vec <- time_vec[keep]

        if (length(rain_vec) == 0) return(NA_real_)

        day_vec <- as.Date(time_vec)
        daily_rain <- tapply(rain_vec, day_vec, sum, na.rm = TRUE)

        total_rain <- sum(daily_rain, na.rm = TRUE)
        if (length(daily_rain) == 0) return(NA_real_)
        if (total_rain <= 0) return(0)

        rainy_days <- sum(daily_rain >= 1, na.rm = TRUE)
        rainy_days / length(daily_rain)
      }

      env_list <- split(df, df$environment)

      out_list <- lapply(env_list, function(env_df) {
        env_df <- env_df[order(env_df$time_index), , drop = FALSE]

        env_resolution <- info$resolution_by_env[unique(env_df$environment)]

        unique_time <- sort(unique(env_df$time_index))
        n_time <- length(unique_time)

        if (n_time < 2) return(NULL)

        split_idx <- max(1, floor(n_time * 0.40))
        early_time <- unique_time[seq_len(split_idx)]

        env_df$phase <- ifelse(env_df$time_index %in% early_time, "Early", "Late")

        phase_list <- split(env_df, env_df$phase)

        do.call(rbind, lapply(phase_list, function(z) {
          rain_vec <- z$PRECTOTCORR[is.finite(z$PRECTOTCORR)]
          temp_vec <- z$T2M[is.finite(z$T2M)]
          hum_vec  <- z$RH2M[is.finite(z$RH2M)]

          rainfall_distribution_index <- if (identical(env_resolution, "hourly")) {
            compute_rainfall_distribution_index_hourly(z$PRECTOTCORR, z$time_index)
          } else {
            compute_rainfall_distribution_index_daily(z$PRECTOTCORR)
          }

          data.frame(
            environment = z$environment[1],
            phase = z$phase[1],
            LON = z$LON[1],
            LAT = z$LAT[1],
            weather_resolution = env_resolution,
            mean_temperature = if (length(temp_vec) == 0) NA_real_ else mean(temp_vec, na.rm = TRUE),
            heat_stress_index = compute_heat_stress_index(temp_vec, threshold = 32),
            rainfall = if (length(rain_vec) == 0) NA_real_ else sum(rain_vec, na.rm = TRUE),
            rainfall_distribution_index = rainfall_distribution_index,
            humidity = if (length(hum_vec) == 0) NA_real_ else mean(hum_vec, na.rm = TRUE),
            stringsAsFactors = FALSE
          )
        }))
      })

      out <- do.call(rbind, out_list)
      rownames(out) <- NULL

      validate(
        need(!is.null(out) && nrow(out) > 0, "No summarized weather data available for supported environments.")
      )

      out
    })

    selected_env_surface_data <- reactive({
      df <- tpe_weather_summary()
      req(df)

      req(input$tpePhase)
      req(input$tpeEnvCovariate)

      validate(
        need("phase" %in% colnames(df), "Summarized weather data must contain a 'phase' column."),
        need(input$tpeEnvCovariate %in% colnames(df),
             paste("Selected environmental covariate not found:", input$tpeEnvCovariate))
      )

      df_phase <- df[df$phase == input$tpePhase, , drop = FALSE]

      lookup <- tpe_period_lookup()

      if (!is.null(lookup)) {
        req(input$tpePeriod)

        env_period <- unique(
          lookup[, c("environment", "tpe_period"), drop = FALSE]
        )

        df_phase <- merge(
          df_phase,
          env_period,
          by = "environment",
          all.x = TRUE
        )

        df_phase <- df_phase[
          df_phase$tpe_period == input$tpePeriod,
          ,
          drop = FALSE
        ]

        validate(
          need(nrow(df_phase) > 0,
               paste("No environmental summaries available for year/season:", input$tpePeriod))
        )
      }

      validate(
        need(nrow(df_phase) > 0, paste("No weather summaries available for phase:", input$tpePhase))
      )

      df_phase$env_value <- df_phase[[input$tpeEnvCovariate]]

      df_phase <- df_phase[
        stats::complete.cases(df_phase[, c("environment", "LON", "LAT", "env_value")]),
        ,
        drop = FALSE
      ]

      validate(
        need(nrow(df_phase) > 0, "No complete environmental covariate values available for the selected phase."),
        need(any(is.finite(df_phase$env_value)), "Selected environmental covariate has no finite values.")
      )

      df_phase
    })

    output$tpeDesignationUI <- renderUI({
      plot_obj <- review_plot_data()
      req(plot_obj)

      df <- plot_obj$sta_long
      req(!is.null(df))
      req(nrow(df) > 0)

      validate(
        need("designation" %in% colnames(df), "STA predictions must contain a 'designation' column.")
      )

      designation_choices <- sort(unique(as.character(df$designation)))
      designation_choices <- designation_choices[!is.na(designation_choices) & nzchar(designation_choices)]

      validate(
        need(length(designation_choices) > 0, "No designations available for the TPE performance plot.")
      )

      selectInput(
        ns("tpeDesignation"),
        label = tags$span(
          "Variety",
          tags$i(
            class = "glyphicon glyphicon-info-sign",
            style = "color:#FFFFFF",
            title = "Choose the designation whose environment-specific STA predictions will be shown across the TPE."
          )
        ),
        choices = designation_choices,
        selected = designation_choices[1]
      )
    })

    output$tpeTraitUI <- renderUI({
      plot_obj <- review_plot_data()
      req(plot_obj)

      df <- plot_obj$sta_long
      req(!is.null(df))
      req(nrow(df) > 0)

      validate(
        need("trait" %in% colnames(df), "STA predictions must contain a 'trait' column.")
      )

      trait_choices <- sort(unique(as.character(df$trait)))
      trait_choices <- trait_choices[!is.na(trait_choices) & nzchar(trait_choices)]

      validate(
        need(length(trait_choices) > 0, "No traits available for the TPE performance plot.")
      )

      selectInput(
        ns("tpeTrait"),
        label = tags$span(
          "Trait",
          tags$i(
            class = "glyphicon glyphicon-info-sign",
            style = "color:#FFFFFF",
            title = "Choose the trait whose environment-specific performance will be displayed."
          )
        ),
        choices = trait_choices,
        selected = trait_choices[1]
      )
    })

    selected_tpe_performance <- reactive({
      plot_obj <- review_plot_data()
      req(plot_obj)

      df <- plot_obj$sta_long
      req(!is.null(df))
      req(nrow(df) > 0)

      req(input$tpeDesignation)
      req(input$tpeTrait)

      validate(
        need(
          all(c("designation", "environment", "trait", "predictedValue") %in% colnames(df)),
          "STA predictions must contain designation, environment, trait, and predictedValue."
        )
      )

      df_sel <- df[
        df$designation == input$tpeDesignation &
          df$trait == input$tpeTrait,
        ,
        drop = FALSE
      ]

      lookup <- tpe_period_lookup()

      if (!is.null(lookup)) {
        req(input$tpePeriod)

        perf_period <- unique(
          lookup[, c("designation", "environment", "tpe_period"), drop = FALSE]
        )

        df_sel <- merge(
          df_sel,
          perf_period,
          by = c("designation", "environment"),
          all.x = TRUE
        )

        df_sel <- df_sel[
          df_sel$tpe_period == input$tpePeriod,
          ,
          drop = FALSE
        ]

        validate(
          need(nrow(df_sel) > 0,
               paste("No STA predictions found for the selected designation, trait, and year/season:", input$tpePeriod))
        )
      }

      validate(
        need(nrow(df_sel) > 0,
             paste("No STA predictions found for designation", input$tpeDesignation,
                   "and trait", input$tpeTrait, ".")),
        need("environment" %in% colnames(df_sel),
             "STA predictions must contain an environment column.")
      )

      # Join coordinates from summarized weather data
      env_coords <- unique(
        selected_env_surface_data()[, c("environment", "LON", "LAT"), drop = FALSE]
      )

      df_sel <- merge(
        df_sel,
        env_coords,
        by = "environment",
        all.x = TRUE
      )

      df_sel <- df_sel[
        stats::complete.cases(df_sel[, c("environment", "designation", "trait", "predictedValue", "LON", "LAT")]),
        ,
        drop = FALSE
      ]

      validate(
        need(nrow(df_sel) > 0,
             "No complete environment-level performance records with coordinates are available for the selected designation and trait.")
      )

      df_sel
    })

    tpe_trait_performance_range <- reactive({
      plot_obj <- review_plot_data()
      req(plot_obj)

      df <- plot_obj$sta_long
      req(!is.null(df))
      req(nrow(df) > 0)

      req(input$tpeTrait)

      validate(
        need(
          all(c("trait", "predictedValue") %in% colnames(df)),
          "STA predictions must contain trait and predictedValue."
        )
      )

      df_trait <- df[
        df$trait == input$tpeTrait &
          is.finite(df$predictedValue),
        ,
        drop = FALSE
      ]

      validate(
        need(nrow(df_trait) > 0,
             paste("No finite STA predicted values found for trait", input$tpeTrait, "."))
      )

      range(df_trait$predictedValue, na.rm = TRUE)
    })

    output$tpePerformanceMap <- plotly::renderPlotly({
      df <- selected_tpe_performance()
      req(df)

      validate(
        need(nrow(df) > 0, "No environment-level performance data available for the selected designation and trait."),
        need(all(c("environment", "designation", "trait", "predictedValue", "LON", "LAT") %in% colnames(df)),
             "Performance data must contain environment, designation, trait, predictedValue, LON, and LAT.")
      )

      map_obj <- tryCatch(
        {
          cgiarPipeline::get_tpe_basemap(
            df_points = df,
            buffer_deg = 3,
            square_expand_factor = 1.05
          )
        },
        error = function(e) {
          validate(need(FALSE, e$message))
        }
      )

      basemap <- map_obj$basemap
      bbox <- map_obj$bbox
      basemap_df <- cgiarPipeline::sf_to_plotly_polygons(basemap)

      rng <- tpe_trait_performance_range()

      validate(
        need(all(is.finite(rng)), "Predicted values are not finite for the selected trait.")
      )

      if (diff(rng) > 0) {
        df$halo_size <- 18 + 22 * (df$predictedValue - rng[1]) / diff(rng)
      } else {
        df$halo_size <- 28
      }

      p <- plotly::plot_ly()

      if (nrow(basemap_df) > 0) {
        split_polys <- split(basemap_df, basemap_df$group)

        for (poly_df in split_polys) {
          p <- p %>%
            plotly::add_trace(
              data = poly_df,
              x = ~LON,
              y = ~LAT,
              type = "scatter",
              mode = "lines",
              line = list(color = "black", width = 1),
              hoverinfo = "skip",
              showlegend = FALSE
            )
        }
      }

      p <- p %>%
        plotly::add_markers(
          data = df,
          x = ~LON,
          y = ~LAT,
          key = ~environment,
          text = ~paste0(
            "Designation: ", designation,
            "<br>Environment: ", environment,
            "<br>Trait: ", trait,
            "<br>Predicted value: ", signif(predictedValue, 4),
            "<br>Reliability: ", signif(reliability, 4)
          ),
          hoverinfo = "text",
          marker = list(
            sizemode = "diameter",
            opacity = 0.22,
            color = "#5B8FF9",
            line = list(width = 0)
          ),
          size = ~halo_size,
          showlegend = FALSE,
          source = ns("tpePerformanceMap")
        )

      p <- p %>%
        plotly::add_markers(
          data = df,
          x = ~LON,
          y = ~LAT,
          key = ~environment,
          text = ~paste0(
            "Designation: ", designation,
            "<br>Environment: ", environment,
            "<br>Trait: ", trait,
            "<br>Predicted value: ", signif(predictedValue, 4),
            "<br>Reliability: ", signif(reliability, 4)
          ),
          hoverinfo = "text",
          marker = list(
            size = 7,
            color = "#1F1F1F",
            line = list(color = "white", width = 1)
          ),
          showlegend = FALSE,
          source = ns("tpePerformanceMap")
        )

      p <- p %>%
        plotly::layout(
          title = paste("Performance across TPE -", input$tpeDesignation, "-", input$tpeTrait),
          xaxis = list(
            visible = FALSE,
            range = c(unname(bbox["xmin"]), unname(bbox["xmax"])),
            fixedrange = TRUE
          ),
          yaxis = list(
            visible = FALSE,
            range = c(unname(bbox["ymin"]), unname(bbox["ymax"])),
            scaleanchor = "x",
            scaleratio = 1,
            fixedrange = TRUE
          ),
          margin = list(l = 0, r = 0, b = 0, t = 50, pad = 0),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )

      p <- plotly::event_register(p, "plotly_hover")
      p <- plotly::event_register(p, "plotly_unhover")
      p
    })

    observeEvent(
      plotly::event_data("plotly_hover", source = ns("tpePerformanceMap")),
      {
        hover <- plotly::event_data("plotly_hover", source = ns("tpePerformanceMap"))
        req(hover)

        if (!is.null(hover$key) && length(hover$key) > 0) {
          hovered_tpe_environment(as.character(hover$key[[1]]))
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      plotly::event_data("plotly_unhover", source = ns("tpePerformanceMap")),
      {
        hovered_tpe_environment(NULL)
      },
      ignoreInit = TRUE
    )

    output$tpeEnvCovHeatmap <- plotly::renderPlotly({
      df_points <- selected_env_surface_data()
      req(df_points)

      validate(
        need(nrow(df_points) > 0, "No environmental summary data available for the selected phase and covariate."),
        need(all(c("environment", "LON", "LAT", "env_value") %in% colnames(df_points)),
             "Environmental surface data must contain environment, LON, LAT, and env_value.")
      )

      map_obj <- tryCatch(
        {
          cgiarPipeline::get_tpe_basemap(
            df_points = df_points,
            buffer_deg = 3,
            square_expand_factor = 1.05
          )
        },
        error = function(e) {
          validate(need(FALSE, e$message))
        }
      )

      basemap <- map_obj$basemap
      bbox <- map_obj$bbox
      basemap_df <- cgiarPipeline::sf_to_plotly_polygons(basemap)

      grid_df <- tryCatch(
        {
          cgiarPipeline::idw_surface(
            df_points = df_points,
            value_col = "env_value",
            lon_col = "LON",
            lat_col = "LAT",
            n_grid = 140,
            power = 2,
            lon_min = unname(bbox["xmin"]),
            lon_max = unname(bbox["xmax"]),
            lat_min = unname(bbox["ymin"]),
            lat_max = unname(bbox["ymax"])
          )
        },
        error = function(e) {
          validate(need(FALSE, e$message))
        }
      )

      mask_sf <- tryCatch(
        {
          cgiarPipeline::build_tpe_mask(df_points, buffer_km = 220)
        },
        error = function(e) {
          NULL
        }
      )

      #grid_df <- tryCatch(
      #  {
      #    mask_grid_to_polygon(grid_df, mask_sf)
      #  },
      #  error = function(e) {
      #    grid_df
      #  }
      #)

      highlighted_env <- hovered_tpe_environment()

      covariate_label <- switch(
        input$tpeEnvCovariate,
        "mean_temperature" = "Mean temperature",
        "heat_stress_index" = "Heat stress index",
        "rainfall" = "Rainfall",
        "rainfall_distribution_index" = "Rainfall distribution index",
        "humidity" = "Humidity",
        input$tpeEnvCovariate
      )

      p <- plotly::plot_ly()

      if (nrow(grid_df) > 0) {
        p <- p %>%
          plotly::add_trace(
            data = grid_df,
            x = ~LON,
            y = ~LAT,
            z = ~z,
            type = "heatmap",
            colorscale = "Viridis",
            showscale = TRUE,
            hoverinfo = "skip"
          )
      }

      if (nrow(basemap_df) > 0) {
        split_polys <- split(basemap_df, basemap_df$group)

        for (poly_df in split_polys) {
          p <- p %>%
            plotly::add_trace(
              data = poly_df,
              x = ~LON,
              y = ~LAT,
              type = "scatter",
              mode = "lines",
              line = list(color = "black", width = 1),
              hoverinfo = "skip",
              showlegend = FALSE
            )
        }
      }

      p <- p %>%
        plotly::add_markers(
          data = df_points,
          x = ~LON,
          y = ~LAT,
          key = ~environment,
          text = ~paste0(
            "Environment: ", environment,
            "<br>Phase: ", phase,
            "<br>", covariate_label, ": ", signif(env_value, 4),
            "<br>Weather resolution: ", weather_resolution
          ),
          hoverinfo = "text",
          marker = list(
            size = 7,
            color = "white",
            line = list(color = "black", width = 1)
          ),
          showlegend = FALSE
        )

      if (!is.null(highlighted_env)) {
        df_highlight <- df_points[df_points$environment == highlighted_env, , drop = FALSE]

        if (nrow(df_highlight) > 0) {
          p <- p %>%
            plotly::add_markers(
              data = df_highlight,
              x = ~LON,
              y = ~LAT,
              text = ~paste0(
                "Highlighted environment: ", environment,
                "<br>", covariate_label, ": ", signif(env_value, 4)
              ),
              hoverinfo = "text",
              marker = list(
                size = 18,
                color = "rgba(255,255,255,0)",
                line = list(color = "#FF4D4F", width = 3),
                symbol = "circle"
              ),
              showlegend = FALSE
            )
        }
      }

      period_label <- if (!is.null(tpe_period_lookup())) {
        paste("-", input$tpePeriod)
      } else {
        ""
      }

      p %>%
        plotly::layout(
          title = paste("Environmental surface", period_label, "-", input$tpePhase, "-", covariate_label),
          xaxis = list(
            visible = FALSE,
            range = c(unname(bbox["xmin"]), unname(bbox["xmax"])),
            fixedrange = TRUE
          ),
          yaxis = list(
            visible = FALSE,
            range = c(unname(bbox["ymin"]), unname(bbox["ymax"])),
            scaleanchor = "x",
            scaleratio = 1,
            fixedrange = TRUE
          ),
          margin = list(l = 0, r = 0, b = 0, t = 50, pad = 0),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )
    })

    output$reliabilityTraitUI <- renderUI({
      plot_obj <- review_plot_data()
      req(plot_obj)

      trait_choices <- plot_obj$traits
      req(length(trait_choices) > 0)

      selectInput(
        ns("reliabilityTrait"),
        "Trait for reliability intervals",
        choices = trait_choices,
        selected = trait_choices[1]
      )
    })

    output$reliabilityIntervalPlot <- plotly::renderPlotly({
      plot_obj <- review_plot_data()
      req(plot_obj)

      req(input$reliabilityTrait)

      df <- plot_obj$mta_long
      req(nrow(df) > 0)


      df <- df[
        df$trait == input$reliabilityTrait,
        ,
        drop = FALSE
      ]

      validate(
        need(nrow(df) > 0, "No predictions found for the selected trait."),
        need("stdError" %in% colnames(df), "Standard errors are not available for this trait.")
      )

      if (!"reliability" %in% colnames(df)) {
        df$reliability <- NA_real_
      }

      if (!"plot_status" %in% colnames(df)) {
        status_df <- plot_obj$review_df[, c("designation", "plot_status"), drop = FALSE]
        df <- merge(df, status_df, by = "designation", all.x = TRUE)
      }

      df$lower_2se <- df$predictedValue - df$stdError
      df$upper_2se <- df$predictedValue + df$stdError

      df <- df[order(df$predictedValue, decreasing = TRUE), , drop = FALSE]
      df$designation <- factor(df$designation, levels = df$designation)

      df <- sample_reliability_plot_df(
        df,
        max_per_status = 100
      )

      p <- ggplot2::ggplot(
        df,
        ggplot2::aes(
          x = designation,
          y = predictedValue,
          color = plot_status,
          alpha = plot_status,
          text = paste0(
            "Designation: ", designation,
            "<br>Trait: ", trait,
            "<br>Predicted value: ", signif(predictedValue, 4),
            "<br>Std. error: ", signif(stdError, 4),
            "<br>Approx. interval: [",
            signif(lower_2se, 4), ", ",
            signif(upper_2se, 4), "]",
            "<br>Reliability: ", signif(reliability, 4),
            "<br>Status: ", plot_status
          )
        )
      ) +
        ggplot2::geom_errorbar(
          ggplot2::aes(
            ymin = lower_2se,
            ymax = upper_2se
          ),
          width = 0.2
        ) +
        ggplot2::geom_point(size = 2.5) +
        ggplot2::coord_flip() +
        ggplot2::scale_color_manual(
          values = c(
            "SELECTED" = "#0072B2",
            "NOT SELECTED" = "#D55E00",
            "REVISE" = "#F9A825",
            "CHECK" = "#C2185B"
          ),
          drop = FALSE
        ) +
        ggplot2::scale_alpha_manual(
          values = c(
            "SELECTED" = 0.6,
            "NOT SELECTED" = 0.6,
            "REVISE" = 0.8,
            "CHECK" = 1.0
          )
        )+
        ggplot2::guides(alpha = "none")+
        ggplot2::labs(
          title = paste("BLUP/BLUE reliability intervals -", input$reliabilityTrait),
          x = "Designation",
          y = "Predicted value",
          color = "Decision"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          legend.position = "bottom"
        )

      plotly::ggplotly(p, tooltip = "text")
    })

    output$reviewPlotsUI <- renderUI({
      plot_obj <- review_plot_data()
      req(plot_obj)

      selected_plots <- plot_obj$selected_plots

      ui_list <- list()

      if ("Pair-wise trait scatterplot" %in% selected_plots) {
        # Determine if TPP checkbox should be shown
        show_tpp_checkbox <- !is.null(tpp_id_prepam()) && !is.null(tpp_filtered_traits())

        tpp_checkbox_ui <- if (show_tpp_checkbox) {
          checkboxInput(
            ns("scatterShowTppLines"),
            label = "Show TPP desired scores",
            value = FALSE
          )
        } else {
          NULL
        }

        ui_list <- c(
          ui_list,
          list(
            shinydashboard::box(
              width = 12,
              title = "Pair-wise trait scatterplot",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE,
              fluidRow(
                column(4, uiOutput(ns("scatterXTraitUI"))),
                column(4, uiOutput(ns("scatterYTraitUI"))),
                column(4,
                  checkboxInput(
                    ns("scatterShowRegression"),
                    label = "Show regression lines & means",
                    value = TRUE
                  ),
                  tpp_checkbox_ui
                )
              ),
              tags$p(
                style = "color: #555; font-size: 0.9em; margin: 8px 0 4px 0; padding: 0 5px;",
                "Axes for traits where lower values are desirable are inverted so that the",
                tags$b("upper-right quadrant always represents the ideal."),
                "For traits with an acceptable range, two TPP reference lines mark the desired band."
              ),
              plotly::plotlyOutput(ns("pairwiseScatterPlot"), height = "650px")
            )
          )
        )
      }

      # if ("Radar plot" %in% selected_plots) {
      # ui_list <- c(
      # ui_list,
      # list(
      # shinydashboard::box(
      # width = 12,
      # title = "Radar plot",
      # status = "primary",
      # solidHeader = TRUE,
      # collapsible = TRUE,
      # collapsed = FALSE,
      # plotly::plotlyOutput(ns("radarPlot"), height = "700px")
      # )
      # )
      # )
      # }

      if ("Performance across locations" %in% selected_plots) {
        ui_list <- c(
          ui_list,
          list(
            # Map first: it defines the cluster colours the tables below reuse
            fluidRow(
              column(
                width = 12,
                shinydashboard::box(
                  width = 12,
                  title = "Trial locations by environment cluster",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  plotly::plotlyOutput(ns("clusterMapPlot"), height = "400px")
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                shinydashboard::box(
                  width = 12,
                  title = "Performance across locations",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  fluidRow(
                    column(6, uiOutput(ns("perfTableTraitUI"))),
                    column(
                      6,
                      numericInput(
                        ns("perfTablePct"),
                        label = tags$span(
                          "Top % of individuals",
                          tags$i(
                            class = "glyphicon glyphicon-info-sign",
                            title = "Show only the best-performing X% of candidates for the selected trait. Checks are always shown for reference."
                          )
                        ),
                        value = 20,
                        min = 1,
                        max = 100,
                        step = 1
                      )
                    )
                  ),
                  uiOutput(ns("perfTableSummary")),
                  uiOutput(ns("perfClusterTablesUI"))
                )
              )
            )
          )
        )
      }

      if ("Relatedness plot" %in% selected_plots) {
        ui_list <- c(
          ui_list,
          list(
            shinydashboard::box(
              width = 12,
              title = "Relatedness / family representation",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE,
              fluidRow(
                column(4,
                  uiOutput(ns("relatednessGenoStampUI"))
                ),
                column(4,
                  numericInput(
                    ns("relatednessTopX"),
                    label = "Top non-selected per unrepresented family",
                    value = 1, min = 0, max = 20, step = 1
                  )
                ),
                column(4,
                  selectInput(
                    ns("relatednessTraits"),
                    label = "Trait to display",
                    choices = NULL,
                    selected = NULL
                  )
                )
              ),
              uiOutput(ns("relatednessSummary")),
              uiOutput(ns("relatednessMessages")),
              tags$p(style = "color: #666; font-size: 12px; font-style: italic; margin: 4px 0;",
                     icon("info-circle"),
                     " Individuals and families are ordered by genetic similarity. Dotted lines separate families.",
                     tags$span(style = "margin-left: 12px; color: #B8860B;", "\u2B24"),
                     tags$span(style = "color: #666; margin-left: 4px;", "= diversity candidate")),
              plotly::plotlyOutput(ns("relatednessPlot"), height = "auto")
            )
          )
        )
      }

      do.call(tagList, ui_list)
    })

    # ---- Relatedness plot reactives ----
    relatedness_mode <- reactive({
      dt_obj <- data()
      req(dt_obj)
      result <- cgiarPipeline::detect_relatedness_mode(dt_obj)
      result
    })

    relatedness_geno_stamp <- reactive({
      dt_obj <- data()
      req(dt_obj)
      mode_info <- relatedness_mode()

      if (!mode_info$has_genomic) {
        return(NULL)
      }

      # Use geno_imp keys directly as the available stamps
      available_stamps <- names(dt_obj$data$geno_imp)
      if (length(available_stamps) == 0) return(NULL)

      # Use user selection — only use genomic data if explicitly selected
      user_selection <- input$relatednessGenoStamp
      if (is.null(user_selection) || !nzchar(user_selection) || user_selection == "__none__") {
        return(NULL)
      }

      selected_stamp <- user_selection

      # Validate the stamp exists in geno_imp
      if (!(selected_stamp %in% available_stamps)) {
        return(NULL)
      }

      selected_stamp
    })

    output$relatednessGenoStampUI <- renderUI({
      dt_obj <- data()
      req(dt_obj)
      mode_info <- relatedness_mode()

      if (!mode_info$has_genomic) return(NULL)

      # Use geno_imp keys directly
      available_stamps <- names(dt_obj$data$geno_imp)
      if (length(available_stamps) == 0) return(NULL)

      # Format choices as timestamps
      labels <- tryCatch(
        format(as.POSIXct(as.numeric(available_stamps), origin = "1970-01-01"), "%Y-%m-%d %H:%M:%S"),
        error = function(e) available_stamps
      )
      choices <- stats::setNames(available_stamps, labels)

      selectInput(
        ns("relatednessGenoStamp"),
        label = "Genomic QA stamp",
        choices = c("None (pedigree only)" = "__none__", choices),
        selected = "__none__"
      )
    })

    # Populate trait picker for relatedness plot
    observeEvent(review_plot_data(), {
      dt_obj <- data()
      if (is.null(dt_obj)) return()

      # Get traits from MTA predictions (same traits used in the analysis)
      preds <- dt_obj$predictions
      if (is.null(preds) || nrow(preds) == 0) return()

      # Use traits from the MTA predictions only (unique trait values)
      mta_traits <- unique(preds$trait[preds$trait != "" & !is.na(preds$trait)])

      choices <- c("Selection_Index" = "Selection_Index", stats::setNames(mta_traits, mta_traits))

      updateSelectInput(
        session,
        "relatednessTraits",
        choices = choices,
        selected = "Selection_Index"
      )
    })

    relatedness_matrix <- reactive({
      dt_obj <- data()
      req(dt_obj)
      mode_info <- relatedness_mode()

      if (mode_info$mode == "none") {
        return(NULL)
      }

      a_mat <- NULL
      grm <- NULL
      warnings <- character(0)

      # Compute A-matrix if pedigree is available
      if (mode_info$has_pedigree) {
        a_mat <- tryCatch(
          cgiarPipeline::compute_a_matrix(dt_obj),
          error = function(e) { NULL }
        )
      }

      # Compute GRM only if user explicitly selected a genomic QA stamp
      if (mode_info$has_genomic) {
        stamp <- relatedness_geno_stamp()
        if (!is.null(stamp)) {
          genlight_obj <- tryCatch(dt_obj$data$geno_imp[[stamp]], error = function(e) NULL)
          if (!is.null(genlight_obj) && inherits(genlight_obj, "genlight")) {
            # Check marker count (safely)
            n_markers <- tryCatch(adegenet::nLoc(genlight_obj), error = function(e) NA_integer_)
            if (!is.na(n_markers) && n_markers < 10) {
              warnings <- c(warnings, sprintf("Genomic data contains very few markers (%d). Relatedness estimates may be unreliable.", n_markers))
            }
            grm <- tryCatch(
              cgiarPipeline::compute_grm(genlight_obj),
              error = function(e) { NULL }
            )
          }
        }
      }

      # Determine effective mode based on what was actually computed
      effective_mode <- if (!is.null(a_mat) && !is.null(grm)) {
        "pedigree-genomic"
      } else if (!is.null(a_mat) && is.null(grm)) {
        "pedigree-only"
      } else if (is.null(a_mat) && !is.null(grm)) {
        "genomic-only"
      } else {
        "none"
      }

      # Choose the similarity matrix based on effective mode
      sim_matrix <- if (effective_mode == "pedigree-only") {
        a_mat
      } else if (effective_mode == "pedigree-genomic") {
        # Prefer GRM, fall back to A-matrix if GRM failed
        if (!is.null(grm)) grm else a_mat
      } else if (effective_mode == "genomic-only") {
        grm
      } else {
        NULL
      }

      if (is.null(sim_matrix)) return(NULL)

      # Handle NA/non-finite values
      na_count <- sum(!is.finite(sim_matrix))
      total_entries <- length(sim_matrix)
      na_pct <- na_count / total_entries * 100

      if (na_pct > 50) {
        return(list(matrix = NULL, a_mat = a_mat, grm = grm, warnings = warnings,
                    effective_mode = effective_mode,
                    error = sprintf("Relatedness matrix contains too many missing values (%.0f%% of pairs). Cannot compute reliable clustering.", na_pct)))
      }

      if (na_count > 0) {
        sim_matrix[!is.finite(sim_matrix)] <- 0.0
        warnings <- c(warnings, sprintf("%d pairs excluded from relatedness metrics (replaced with 0.0).", na_count))
      }

      list(matrix = sim_matrix, a_mat = a_mat, grm = grm, warnings = warnings, effective_mode = effective_mode, error = NULL)
    })

    relatedness_plot_data <- reactive({
      dt_obj <- data()
      req(dt_obj)
      mode_info <- relatedness_mode()
      mat_result <- relatedness_matrix()

      if (is.null(mat_result) || !is.null(mat_result$error)) {
        return(NULL)
      }

      sim_matrix <- mat_result$matrix
      if (is.null(sim_matrix)) return(NULL)

      # Get review data
      plot_obj <- review_plot_data()
      req(plot_obj)
      review_df <- plot_obj$review_df
      req(nrow(review_df) > 0)

      # Apply overrides
      overrides <- plot_selection_overrides()
      if (!is.null(overrides) && nrow(overrides) > 0) {
        override_map <- stats::setNames(overrides$plot_decision, overrides$designation)
        match_idx <- match(review_df$designation, names(override_map))
        has_override <- !is.na(match_idx)
        review_df$plot_status[has_override] <- override_map[review_df$designation[has_override]]
      }

      # Get selected set
      selected_set <- review_df$designation[review_df$plot_status == "SELECTED"]
      if (length(selected_set) < 2) {
        return(NULL)
      }

      # Get index values â€” index_value may not be in review_df, need to fetch from decision table
      if (!"index_value" %in% colnames(review_df)) {
        # Get the initial stamp from the review plot data
        init_stamp <- plot_obj$initialSelectionStamp
        tbl <- tryCatch(
          cgiarPipeline::build_prodadv_decision_table_data(dt_obj, init_stamp),
          error = function(e) { NULL }
        )
        if (!is.null(tbl) && "index_value" %in% colnames(tbl)) {
          idx_df <- tbl[, c("designation", "index_value"), drop = FALSE]
          review_df <- merge(review_df, idx_df, by = "designation", all.x = TRUE)
        } else {
          # Fallback: use NA for index_value
          review_df$index_value <- NA_real_
        }
      }

      index_vals <- stats::setNames(review_df$index_value, review_df$designation)

      # Determine families or clusters
      groups_df <- data.frame(group_id = integer(0), group_label = character(0),
                              branch_color = character(0), n_members = integer(0),
                              stringsAsFactors = FALSE)
      individuals_df <- NULL
      hclust_obj <- NULL

      # Use effective_mode (accounts for user's genomic stamp selection)
      effective_mode <- mat_result$effective_mode

      if (effective_mode %in% c("pedigree-only", "pedigree-genomic")) {
        # Assign families
        paramsPed <- dt_obj$metadata$pedigree
        desig_col <- paramsPed[paramsPed$parameter == "designation", "value"]
        mother_col <- paramsPed[paramsPed$parameter == "mother", "value"]
        father_col <- paramsPed[paramsPed$parameter == "father", "value"]

        # Guard against missing metadata
        if (length(desig_col) == 0 || length(mother_col) == 0 || length(father_col) == 0) {
          return(NULL)
        }
        desig_col <- desig_col[1]
        mother_col <- mother_col[1]
        father_col <- father_col[1]

        families <- tryCatch(
          cgiarPipeline::assign_families(dt_obj$data$pedigree, desig_col, mother_col, father_col),
          error = function(e) { NULL }
        )
        if (is.null(families)) {
          return(NULL)
        }

        # Build candidates_df with family column
        candidates_df <- data.frame(
          designation = review_df$designation,
          index_value = review_df$index_value,
          family = as.character(families[as.character(review_df$designation)]),
          stringsAsFactors = FALSE
        )
        candidates_df$family[is.na(candidates_df$family)] <- "Founders / Unknown Family"

        # Top X from unrepresented families (X per family, not X total)
        top_x <- input$relatednessTopX
        if (is.null(top_x)) top_x <- 1
        top_x <- max(0L, min(20L, as.integer(top_x)))

        top_x_df <- tryCatch(
          cgiarPipeline::find_top_x_unrelated(candidates_df, sim_matrix, selected_set, top_x, mode = "pedigree"),
          error = function(e) { data.frame(designation = character(0), index_value = numeric(0), family = character(0), stringsAsFactors = FALSE) }
        )

        # Individuals to display: selected + checks + top_x
        check_set <- review_df$designation[review_df$plot_status == "CHECK"]
        display_desigs <- unique(c(selected_set, check_set, top_x_df$designation))
        display_df <- review_df[review_df$designation %in% display_desigs, , drop = FALSE]
        display_df$family <- candidates_df$family[match(display_df$designation, candidates_df$designation)]

        # Order families by genetic similarity (GRM > pedigree fallback)
        unique_families <- tryCatch(
          cgiarPipeline::order_families_by_similarity(
            family_labels = families,
            display_designations = display_df$designation,
            similarity_matrix = sim_matrix,
            pedigree_df = dt_obj$data$pedigree,
            designation_col = desig_col,
            mother_col = mother_col,
            father_col = father_col
          ),
          error = function(e) unique(display_df$family)
        )

        # Assign group IDs based on similarity order
        family_to_id <- stats::setNames(seq_along(unique_families), unique_families)
        display_df$group_id <- family_to_id[display_df$family]
        display_df$group_label <- display_df$family

        # Order within groups by index_value descending
        display_df <- display_df[order(display_df$group_id, -display_df$index_value), , drop = FALSE]
        display_df$order_within <- ave(seq_len(nrow(display_df)), display_df$group_id, FUN = seq_along)

        # Build groups_df
        groups_df <- data.frame(
          group_id = seq_along(unique_families),
          group_label = unique_families,
          branch_color = FAMILY_PALETTE[(seq_along(unique_families) - 1) %% length(FAMILY_PALETTE) + 1],
          n_members = as.integer(table(display_df$group_id)[as.character(seq_along(unique_families))]),
          stringsAsFactors = FALSE
        )

        individuals_df <- display_df

      } else {
        # Genomic-only mode: cluster
        cluster_result <- cgiarPipeline::cluster_genomic_only(sim_matrix, selected_set, index_vals)
        hclust_obj <- cluster_result$dendrogram

        # Build candidates_df for top_x
        candidates_df <- data.frame(
          designation = review_df$designation,
          index_value = review_df$index_value,
          stringsAsFactors = FALSE
        )

        top_x <- input$relatednessTopX
        if (is.null(top_x)) top_x <- 1
        top_x <- max(0L, min(20L, as.integer(top_x)))

        top_x_df <- cgiarPipeline::find_top_x_unrelated(
          candidates_df, sim_matrix, selected_set, top_x, mode = "genomic"
        )

        # Display: selected (with cluster assignments) + top_x
        display_desigs <- c(names(cluster_result$clusters), top_x_df$designation)
        display_df <- review_df[review_df$designation %in% display_desigs, , drop = FALSE]

        # Assign cluster labels
        display_df$group_id <- as.integer(cluster_result$clusters[as.character(display_df$designation)])
        # Top X individuals get assigned to a special group
        max_cluster <- max(c(0L, cluster_result$clusters), na.rm = TRUE)
        display_df$group_id[is.na(display_df$group_id)] <- max_cluster + 1L
        display_df$group_label <- paste("Cluster", display_df$group_id)
        display_df$group_label[display_df$group_id == max_cluster + 1L] <- "Top non-selected"

        display_df <- display_df[order(display_df$group_id, -display_df$index_value), , drop = FALSE]
        display_df$order_within <- ave(seq_len(nrow(display_df)), display_df$group_id, FUN = seq_along)

        unique_groups <- sort(unique(display_df$group_id))
        groups_df <- data.frame(
          group_id = unique_groups,
          group_label = paste("Cluster", unique_groups),
          branch_color = FAMILY_PALETTE[(seq_along(unique_groups) - 1) %% length(FAMILY_PALETTE) + 1],
          n_members = as.integer(table(display_df$group_id)[as.character(unique_groups)]),
          stringsAsFactors = FALSE
        )
        groups_df$group_label[groups_df$group_id == max_cluster + 1L] <- "Top non-selected"

        individuals_df <- display_df
      }

      # Compute marker visual properties
      individuals_df$is_selected <- individuals_df$plot_status == "SELECTED"
      individuals_df$marker_color <- STATUS_COLORS[individuals_df$plot_status]
      individuals_df$marker_color[is.na(individuals_df$marker_color)] <- MISSING_STATUS_COLOR

      # Compute reliability: use minimum across available reliability columns
      rel_cols <- grep("^reliability_", colnames(individuals_df), value = TRUE)
      if (length(rel_cols) > 0) {
        rel_matrix <- as.matrix(individuals_df[, rel_cols, drop = FALSE])
        individuals_df$reliability <- apply(rel_matrix, 1, function(x) min(x, na.rm = TRUE))
        individuals_df$reliability[!is.finite(individuals_df$reliability)] <- NA_real_
      } else {
        individuals_df$reliability <- NA_real_
      }

      individuals_df$marker_opacity <- rep(1.0, nrow(individuals_df))
      individuals_df$marker_size <- MARKER_SIZE_DEFAULT

      # Diversity candidates
      div_candidates <- cgiarPipeline::classify_diversity_candidates(
        data.frame(designation = review_df$designation, index_value = review_df$index_value, stringsAsFactors = FALSE),
        sim_matrix,
        selected_set
      )
      individuals_df$is_diversity <- individuals_df$designation %in% div_candidates
      individuals_df$border_style <- "none"
      individuals_df$border_style[individuals_df$is_diversity] <- "gold-dashed"

      # Avg relatedness to selected
      individuals_df$avg_relatedness <- sapply(individuals_df$designation, function(d) {
        if (d %in% rownames(sim_matrix) && length(intersect(selected_set, colnames(sim_matrix))) > 0) {
          sel_in_mat <- intersect(selected_set, colnames(sim_matrix))
          mean(sim_matrix[d, sel_in_mat], na.rm = TRUE)
        } else {
          NA_real_
        }
      })

      individuals_df$has_genomic <- individuals_df$designation %in% rownames(sim_matrix)

      # Trait values — use MTA predictions from plot_obj for consistency with other plots
      selected_traits <- input$relatednessTraits
      trait_values_df <- data.frame(designation = character(0), trait = character(0),
                                     value = numeric(0), std_error = numeric(0),
                                     reliability = numeric(0),
                                     direction = character(0),
                                     stringsAsFactors = FALSE)
      if (!is.null(selected_traits) && length(selected_traits) > 0) {
        # Use MTA predictions from the review_plot_data (same source as scatterplot)
        mta_preds <- plot_obj$mta_long

        for (tr in selected_traits) {
          if (tr == "Selection_Index") {
            tr_vals <- data.frame(
              designation = individuals_df$designation,
              trait = "Selection_Index",
              value = individuals_df$index_value,
              std_error = NA_real_,
              reliability = NA_real_,
              direction = "increase",
              stringsAsFactors = FALSE
            )
          } else {
            # Get from MTA predictions (same analysisId as scatterplot)
            tr_preds <- mta_preds[mta_preds$trait == tr & mta_preds$designation %in% individuals_df$designation, , drop = FALSE]
            if (nrow(tr_preds) == 0) next
            tr_preds <- tr_preds[!duplicated(tr_preds$designation), , drop = FALSE]

            # Get direction from metadata
            pheno_meta <- dt_obj$metadata$pheno
            direction <- "increase"
            if (!is.null(pheno_meta)) {
              dir_row <- pheno_meta[pheno_meta$trait == tr & pheno_meta$parameter == "direction", , drop = FALSE]
              if (nrow(dir_row) > 0) direction <- dir_row$value[1]
            }

            # Get stdError if available
            se_values <- if ("stdError" %in% colnames(tr_preds)) tr_preds$stdError else NA_real_

            # Get per-trait reliability
            rel_values <- if ("reliability" %in% colnames(tr_preds)) tr_preds$reliability else NA_real_

            tr_vals <- data.frame(
              designation = tr_preds$designation,
              trait = tr,
              value = cgiarPipeline::normalize_trait_direction(tr_preds$predictedValue, direction),
              std_error = se_values,
              reliability = rel_values,
              direction = direction,
              stringsAsFactors = FALSE
            )
          }
          trait_values_df <- rbind(trait_values_df, tr_vals)
        }
      }

      # Summary
      # Count families in the selected set specifically
      selected_individuals <- individuals_df[individuals_df$is_selected, , drop = FALSE]
      selected_family_count <- length(unique(selected_individuals$group_label))

      summary_text <- cgiarPipeline::compute_summary_bar(
        selected_count = sum(individuals_df$is_selected),
        selected_family_count = selected_family_count,
        group_count = nrow(groups_df),
        diversity_count = sum(individuals_df$is_diversity),
        mode_string = effective_mode
      )

      list(
        individuals = individuals_df,
        trait_values = trait_values_df,
        groups = groups_df,
        dendrogram = hclust_obj,
        mode = effective_mode,
        summary = list(
          selected_count = sum(individuals_df$is_selected),
          group_count = nrow(groups_df),
          diversity_count = sum(individuals_df$is_diversity),
          mode_label = effective_mode,
          text = summary_text
        ),
        warnings = mat_result$warnings
      )
    })

    output$relatednessSummary <- renderUI({
      plot_data <- relatedness_plot_data()
      if (is.null(plot_data)) return(NULL)

      summary_text <- plot_data$summary$text
      tags$div(
        style = "background-color: #f0f0f0; padding: 8px 12px; border-radius: 4px; margin-bottom: 10px; font-weight: bold;",
        icon("chart-pie"),
        summary_text
      )
    })

    output$relatednessMessages <- renderUI({
      # Check for validation messages
      mode_info <- relatedness_mode()

      if (mode_info$mode == "none") {
        return(tags$div(
          style = "color:orange; padding:10px;",
          icon("triangle-exclamation"),
          mode_info$message
        ))
      }

      # Check matrix errors
      mat_result <- relatedness_matrix()
      if (!is.null(mat_result) && is.list(mat_result) && !is.null(mat_result$error)) {
        return(tags$div(
          style = "color:red; padding:10px;",
          icon("circle-xmark"),
          mat_result$error
        ))
      }

      # Check < 2 selected
      plot_obj <- review_plot_data()
      if (!is.null(plot_obj)) {
        review_df <- plot_obj$review_df
        overrides <- plot_selection_overrides()
        if (!is.null(overrides) && nrow(overrides) > 0) {
          override_map <- stats::setNames(overrides$plot_decision, overrides$designation)
          match_idx <- match(review_df$designation, names(override_map))
          has_override <- !is.na(match_idx)
          review_df$plot_status[has_override] <- override_map[review_df$designation[has_override]]
        }
        selected_count <- sum(review_df$plot_status == "SELECTED", na.rm = TRUE)
        if (selected_count < 2) {
          return(tags$div(
            style = "color:orange; padding:10px;",
            icon("triangle-exclamation"),
            "At least 2 selected individuals are required for relatedness analysis."
          ))
        }
      }

      # Show warnings from the matrix computation
      plot_data <- relatedness_plot_data()
      if (!is.null(plot_data) && length(plot_data$warnings) > 0) {
        warning_tags <- lapply(plot_data$warnings, function(w) {
          tags$div(
            style = "color:orange; padding:4px 10px;",
            icon("triangle-exclamation"), w
          )
        })
        return(do.call(tagList, warning_tags))
      }

      NULL
    })

    # ---- Relatedness plot: shared cross-plot highlight state ----
    highlighted_designation <- reactiveVal(NULL)

    output$relatednessPlot <- plotly::renderPlotly({
      plot_data <- relatedness_plot_data()
      validate(
        need(!is.null(plot_data), "Relatedness data not available. Check data inputs.")
      )

      hl <- highlighted_designation()
      result <- tryCatch(
        build_relatedness_plotly(plot_data, highlighted = hl, source_id = "relatedness_plot"),
        error = function(e) { NULL }
      )
      validate(need(!is.null(result), "Error building relatedness plot."))
      plotly::event_register(result, "plotly_click")
    })

    # Click-to-highlight handler for relatedness plot
    observeEvent(plotly::event_data("plotly_click", source = "relatedness_plot"), {
      click_data <- plotly::event_data("plotly_click", source = "relatedness_plot")
      if (is.null(click_data)) return()

      clicked_desig <- click_data$key
      if (is.null(clicked_desig) || !nzchar(clicked_desig)) return()

      current_hl <- highlighted_designation()

      if (!is.null(current_hl) && current_hl == clicked_desig) {
        # Toggle off: clicking the same designation removes highlight
        highlighted_designation(NULL)
      } else {
        # Set or transfer highlight to the new designation
        highlighted_designation(clicked_desig)
      }
    })

    manual_designation_choices <- reactive({
      plot_obj <- review_plot_data()
      req(plot_obj)

      df <- plot_obj$review_df
      req(nrow(df) > 0)

      req(all(c("designation", "plot_status") %in% colnames(df)))

      df_manual <- df[, c("designation", "plot_status"), drop = FALSE]
      df_manual$designation <- as.character(df_manual$designation)
      df_manual$plot_status <- as.character(df_manual$plot_status)

      df_manual <- df_manual[
        !is.na(df_manual$designation) &
          nzchar(df_manual$designation) &
          !is.na(df_manual$plot_status) &
          df_manual$plot_status != "CHECK",
        ,
        drop = FALSE
      ]

      overrides <- plot_selection_overrides()

      if (nrow(overrides) > 0) {
        df_manual <- merge(
          df_manual,
          overrides,
          by = "designation",
          all.x = TRUE
        )

        df_manual$effective_status <- ifelse(
          !is.na(df_manual$plot_decision),
          as.character(df_manual$plot_decision),
          as.character(df_manual$plot_status)
        )
      } else {
        df_manual$effective_status <- df_manual$plot_status
      }

      df_manual <- unique(df_manual[, c("designation", "effective_status"), drop = FALSE])
      df_manual <- df_manual[order(df_manual$designation), , drop = FALSE]

      choice_values <- df_manual$designation
      names(choice_values) <- paste0(
        df_manual$designation,
        " [",
        df_manual$effective_status,
        "]"
      )

      choice_values
    })

    output$manualDesignationSelectionUI <- renderUI({
      choices <- manual_designation_choices()

      selectizeInput(
        ns("manualDesignationSelection"),
        label = "Manual designation selection",
        choices = choices,
        selected = character(0),
        multiple = TRUE,
        options = list(
          placeholder = "Search designations...",
          plugins = list("remove_button"),
          maxOptions = 1000
        )
      )
    })

    observeEvent(manual_designation_choices(), {
      choices <- manual_designation_choices()

      current_selection <- isolate(input$manualDesignationSelection)
      current_selection <- current_selection[current_selection %in% unname(choices)]

      updateSelectizeInput(
        session,
        "manualDesignationSelection",
        choices = choices,
        selected = current_selection,
        server = TRUE
      )
    }, ignoreInit = TRUE)

    observeEvent(input$applyManualDesignationDecision, {
      req(input$manualDesignationSelection)
      req(length(input$manualDesignationSelection) > 0)
      req(input$manualDesignationDecision)

      selected_designations <- input$manualDesignationSelection
      new_status <- input$manualDesignationDecision

      base_df <- review_plot_data()$review_df
      req(base_df)

      overrides <- plot_selection_overrides()

      for (des in selected_designations) {
        base_status <- base_df$plot_status[base_df$designation == des][1]

        if (is.na(base_status) || base_status == "CHECK") {
          next
        }

        if (des %in% overrides$designation) {
          overrides$plot_decision[overrides$designation == des] <- new_status
        } else {
          overrides <- rbind(
            overrides,
            data.frame(
              designation = des,
              plot_decision = new_status,
              stringsAsFactors = FALSE
            )
          )
        }

        if (!is.na(base_status) && identical(base_status, new_status)) {
          overrides <- overrides[overrides$designation != des, , drop = FALSE]
        }
      }

      plot_selection_overrides(overrides)

      showNotification(
        paste(length(selected_designations), "designation(s) updated."),
        type = "message"
      )
    })

    observeEvent(input$savePlotSelection, {
      req(data())
      req(input$initialSelectionStamp)

      manual_decisions <- plot_selection_overrides()
      req(nrow(manual_decisions) > 0)

      analysis_id <- as.numeric(Sys.time())

      dt_object <- cgiarPipeline::savePlotProdAdvSelection(
        analysisId = analysis_id,
        analysisIdName = if (nzchar(trimws(input$plotSelectionId))) trimws(input$plotSelectionId) else NULL,
        initialSelectionStamp = input$initialSelectionStamp,
        plotSelectionStamp = if (!is.null(input$plotSelectionStamp) &&
                                 nzchar(input$plotSelectionStamp) &&
                                 input$plotSelectionStamp != "__none__") input$plotSelectionStamp else NULL,
        manual_decisions = manual_decisions,
        dt_object = data()
      )

      data(dt_object)

      new_plot_stamp <- as.character(analysis_id)

      dt_status <- dt_object$status
      dtPlotSel <- dt_status[dt_status$module == "Plot_prodAdv", , drop = FALSE]
      stampsPlotSel <- make_stamp_choices(dtPlotSel)

      session$onFlushed(function() {

        updateSelectInput(
          session,
          "plotSelectionStamp",
          choices = c("No plot selection" = "__none__", stampsPlotSel),
          selected = new_plot_stamp
        )

        updateTabsetPanel(session, "reviewOutputTabs", selected = "decision_table")

      }, once = TRUE)

      showNotification(
        "Plot selection saved successfully.",
        type = "message"
      )
    }, ignoreInit = TRUE)

    ###########################################
    # Decision table (idx8)
    ###########################################

    final_decision_overrides <- reactiveVal(
      data.frame(
        designation = character(),
        final_decision = character(),
        stringsAsFactors = FALSE
      )
    )

    `%||%` <- function(x, y) {
      if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
    }

    make_decision_badge <- function(value) {
      bg <- dplyr::case_when(
        is.na(value) | !nzchar(value) ~ "#F5F5F5",
        value == "SELECTED" ~ "#D6EAF8",
        value == "NOT SELECTED" ~ "#F5C4A5",
        value == "CHECK" ~ "#E3A9C4",
        value == "REVISE" ~ "#FFF3CD",
        TRUE ~ "#F5F5F5"
      )

      txt <- ifelse(is.na(value), "", as.character(value))

      sprintf(
        "<div style='width:100%%; background:%s; padding:6px; border-radius:4px; text-align:center; font-weight:600;'>%s</div>",
        bg, txt
      )
    }

    make_value_badge <- function(value, decision) {
      bg <- dplyr::case_when(
        decision == "SELECTED" ~ "#D6EAF8",
        decision == "NOT SELECTED" ~ "#F5C4A5",
        decision == "REVISE" ~ "#FFF3CD",
        decision == "CHECK" ~ "#E3A9C4",
        TRUE ~ "#FFFFFF"
      )

      txt <- ifelse(is.na(value), "", format(round(as.numeric(value), 3), nsmall = 3))

      sprintf(
        "<div style='width:100%%; background:%s; padding:6px; border-radius:4px; text-align:right;'>%s</div>",
        bg, txt
      )
    }

    make_final_cell <- function(designation, selected_value, initial_decision) {

      if (identical(initial_decision, "CHECK")) {
        return(make_decision_badge("CHECK"))
      }

      selected_value <- selected_value %||% "REVISE"

      choices <- c("SELECTED", "NOT SELECTED", "REVISE")

      options <- vapply(
        choices,
        function(ch) {
          sel <- if (identical(ch, selected_value)) " selected" else ""
          sprintf("<option value='%s'%s>%s</option>", ch, sel, ch)
        },
        character(1)
      )

      sprintf(
        "<select class='form-control final-decision-select' data-designation='%s' style='width:140px;'>%s</select>",
        htmltools::htmlEscape(designation),
        paste(options, collapse = "")
      )
    }

    decision_table_data <- reactive({
      build_decision_table_data(
        dt = data(),
        initial_stamp = input$initialSelectionStamp,
        plot_stamp = input$plotSelectionStamp,
        final_stamp = input$finalSelectionStamp,
        final_overrides = final_decision_overrides()
      )
    })

    output$decisionTable <- DT::renderDT({

      obj <- decision_table_data()
      req(obj)

      DT::datatable(
        obj$display,
        escape = FALSE,
        rownames = FALSE,
        selection = "none",
        class = "compact stripe hover nowrap decision-table",
        options = list(
          scrollX = TRUE,
          scrollY = "500px",
          paging = FALSE,
          searching = FALSE,
          ordering = FALSE,
          autoWidth = FALSE
        ),
        callback = htmlwidgets::JS(sprintf("
      table.on('change', 'select.final-decision-select', function() {
        var designation = $(this).data('designation');
        var value = $(this).val();
        Shiny.setInputValue('%s', {
          designation: designation,
          value: value,
          nonce: Math.random()
        }, {priority: 'event'});
      });
    ", ns("finalDecisionChange")))
      )
    })

    observeEvent(input$finalDecisionChange, {

      current <- final_decision_overrides()

      desig <- input$finalDecisionChange$designation
      val <- input$finalDecisionChange$value

      if (!desig %in% current$designation) {
        current <- rbind(
          current,
          data.frame(
            designation = desig,
            final_decision = val,
            stringsAsFactors = FALSE
          )
        )
      } else {
        current$final_decision[current$designation == desig] <- val
      }

      final_decision_overrides(current)

    }, ignoreInit = TRUE)

    final_decision_table_raw <- reactive({
      obj <- decision_table_data()
      req(obj)

      out <- obj$raw

      trait_cols <- grep("_trait_decision$", colnames(out), invert = TRUE, value = TRUE)
      drop_cols <- c(
        "entryType",
        "is_check_sort",
        "final_decision",
        "final_manual_decision"
      )
      keep_cols <- setdiff(trait_cols, drop_cols)

      out <- out[, keep_cols, drop = FALSE]

      names(out)[names(out) == "final_decision_initial"] <- "final_decision"

      out
    })

    final_report_table <- reactive({
      tbl <- final_decision_table_raw()
      req(tbl)
      req(nrow(tbl) > 0)

      tbl
    })

    final_decision_summary <- reactive({
      tbl <- final_decision_table_raw()
      req(tbl)
      req("final_decision" %in% colnames(tbl))

      x <- toupper(trimws(as.character(tbl$final_decision)))

      data.frame(
        selected = sum(x == "SELECTED", na.rm = TRUE),
        not_selected = sum(x == "NOT SELECTED", na.rm = TRUE),
        revise = sum(x == "REVISE", na.rm = TRUE)
      )
    })

    output$finalDecisionSummary <- renderUI({
      s <- final_decision_summary()

      tags$div(
        style = "margin-top:10px;",
        tags$strong("Current final decision summary:"),
        tags$br(),
        tags$span(paste0("Selected: ", s$selected, " individuals")),
        tags$br(),
        tags$span(paste0("Non-selected: ", s$not_selected, " individuals")),
        tags$br(),
        tags$span(paste0("Revise: ", s$revise, " individuals"))
      )
    })

    ###########################################
    # Final Review & Output tab server logic
    ###########################################

    # Reactive: compute final decisions by merging initial, table, and plot stamps
    final_decision_data <- reactive({
      req(data())
      req(input$reportInitialSelectionStamp)

      dt <- data()

      # --- Load initial decisions ---
      init_stamp <- input$reportInitialSelectionStamp
      init_mods <- dt$modifications$selection[
        as.character(dt$modifications$selection$analysisId) %in% init_stamp &
          dt$modifications$selection$module == "Init_prodAdv" &
          dt$modifications$selection$reason == "initial_selection",
        , drop = FALSE
      ]
      req(nrow(init_mods) > 0)

      initial_decisions <- unique(init_mods[, c("designation", "value"), drop = FALSE])
      names(initial_decisions)[names(initial_decisions) == "value"] <- "initial_decision"

      # --- Load table decisions (if stamp selected) ---
      table_stamp <- input$reportPlotSelectionStamp
      table_decisions <- NULL
      if (!is.null(table_stamp) && nzchar(table_stamp) && table_stamp != "__none__" && table_stamp != "None") {
        table_mods <- dt$modifications$selection[
          as.character(dt$modifications$selection$analysisId) %in% table_stamp &
            dt$modifications$selection$module == "Table_prodAdv" &
            dt$modifications$selection$reason == "manual_table_selection",
          , drop = FALSE
        ]
        if (nrow(table_mods) > 0) {
          table_decisions <- unique(table_mods[, c("designation", "value"), drop = FALSE])
          names(table_decisions)[names(table_decisions) == "value"] <- "table_decision"
        }
      }

      # --- Load plot decisions (if stamp selected) ---
      plot_stamp <- input$reportFinalSelectionStamp
      plot_decisions <- NULL
      if (!is.null(plot_stamp) && nzchar(plot_stamp) && plot_stamp != "__none__" && plot_stamp != "None") {
        plot_mods <- dt$modifications$selection[
          as.character(dt$modifications$selection$analysisId) %in% plot_stamp &
            dt$modifications$selection$module == "Plot_prodAdv" &
            dt$modifications$selection$reason == "manual_plot_selection",
          , drop = FALSE
        ]
        if (nrow(plot_mods) > 0) {
          plot_decisions <- unique(plot_mods[, c("designation", "value"), drop = FALSE])
          names(plot_decisions)[names(plot_decisions) == "value"] <- "plot_decision"
        }
      }

      # --- Call compute_final_decisions ---
      merged <- compute_final_decisions(
        initial_decisions = initial_decisions,
        table_decisions = table_decisions,
        plot_decisions = plot_decisions
      )

      # --- Join trait BLUPs from predictions for display ---
      modeling_init <- dt$modeling[
        dt$modeling$analysisId %in% init_stamp &
          dt$modeling$module == "Init_prodAdv",
        , drop = FALSE
      ]
      selected_traits <- unique(modeling_init$trait[!is.na(modeling_init$trait) & nzchar(modeling_init$trait)])
      selected_traits <- selected_traits[!selected_traits %in%
        modeling_init$trait[modeling_init$parameter == "user_excluded_trait"]]

      mta_stamp <- modeling_init$value[modeling_init$parameter == "mta_stamp"][1]

      if (!is.na(mta_stamp) && nzchar(mta_stamp)) {
        preds <- dt$predictions[
          dt$predictions$analysisId %in% mta_stamp &
            dt$predictions$trait %in% selected_traits &
            dt$predictions$effectType == "designation" &
            dt$predictions$designation %in% merged$designation,
          , drop = FALSE
        ]

        if (nrow(preds) > 0) {
          # Deduplicate: keep one row per designation × trait (average if duplicated)
          dup_key <- paste(preds$designation, preds$trait, sep = "|||")
          if (any(duplicated(dup_key))) {
            preds <- do.call(rbind, lapply(split(preds, dup_key), function(x) {
              row1 <- x[1, , drop = FALSE]
              row1$predictedValue <- mean(x$predictedValue, na.rm = TRUE)
              row1
            }))
            rownames(preds) <- NULL
          }

          pred_wide <- reshape(
            preds[, c("designation", "trait", "predictedValue"), drop = FALSE],
            idvar = "designation",
            timevar = "trait",
            direction = "wide"
          )
          names(pred_wide) <- sub("^predictedValue\\.", "", names(pred_wide))
          merged <- merge(merged, pred_wide, by = "designation", all.x = TRUE)
        }

        # Compute index_value from stored weights
        weight_rows <- modeling_init[modeling_init$parameter == "index_weight", , drop = FALSE]
        if (nrow(weight_rows) > 0 && length(selected_traits) > 0) {
          index_weights <- as.numeric(weight_rows$value)
          names(index_weights) <- weight_rows$trait
          avail_traits <- intersect(names(index_weights), colnames(merged))
          if (length(avail_traits) > 0) {
            trait_matrix <- as.matrix(merged[, avail_traits, drop = FALSE])
            # Replace NA with column means for scaling
            for (col_idx in seq_len(ncol(trait_matrix))) {
              na_mask <- is.na(trait_matrix[, col_idx])
              if (any(na_mask)) {
                trait_matrix[na_mask, col_idx] <- mean(trait_matrix[!na_mask, col_idx], na.rm = TRUE)
              }
            }
            scaled_matrix <- scale(trait_matrix)
            scaled_matrix[is.nan(scaled_matrix)] <- 0
            w <- index_weights[avail_traits]

            # Replace NAs in zero-weight columns to prevent NA*0=NA propagation in index
            zero_weight_cols <- which(w == 0)
            if (length(zero_weight_cols) > 0) {
              scaled_matrix[, zero_weight_cols][is.na(scaled_matrix[, zero_weight_cols])] <- 0
            }

            # Apply reliability weighting (same as build_prodadv_decision_table_data)
            if ("reliability" %in% colnames(preds)) {
              rel_data <- reshape(
                preds[, c("designation", "trait", "reliability"), drop = FALSE],
                idvar = "designation", timevar = "trait", direction = "wide"
              )
              names(rel_data) <- sub("^reliability\\.", "", names(rel_data))
              rel_avail <- intersect(avail_traits, colnames(rel_data))
              if (length(rel_avail) == length(avail_traits)) {
                rel_matrix <- as.matrix(rel_data[match(merged$designation, rel_data$designation), avail_traits, drop = FALSE])
                rel_matrix[is.na(rel_matrix)] <- 0
                rel_matrix <- pmax(0, pmin(1, rel_matrix))
                reliability_penalized <- scaled_matrix * sqrt(rel_matrix)
                merged$index_value <- as.numeric(reliability_penalized %*% w)
              } else {
                merged$index_value <- as.numeric(scaled_matrix %*% w)
              }
            } else {
              merged$index_value <- as.numeric(scaled_matrix %*% w)
            }
          } else {
            merged$index_value <- NA_real_
          }
        } else {
          merged$index_value <- NA_real_
        }
      }

      # Sort by index_value descending
      if ("index_value" %in% colnames(merged)) {
        merged <- merged[order(-as.numeric(merged$index_value)), , drop = FALSE]
      }

      merged
    })

    # Render finalSummaryBar showing counts
    output$finalSummaryBar <- renderUI({
      tbl <- final_decision_data()
      req(tbl)
      req("final_decision" %in% colnames(tbl))

      fd <- toupper(trimws(as.character(tbl$final_decision)))
      n_total <- length(fd)
      n_selected <- sum(fd == "SELECTED", na.rm = TRUE)
      n_not_selected <- sum(fd == "NOT SELECTED", na.rm = TRUE)
      n_revise <- sum(fd == "REVISE", na.rm = TRUE)
      n_check <- sum(fd == "CHECK", na.rm = TRUE)

      div(
        style = "background-color:#2C3E50; color:white; padding:15px; border-radius:8px; margin-bottom:15px;",
        fluidRow(
          column(2, tags$h4(style = "margin:0;", paste0(n_total, " evaluated"))),
          column(3, tags$h4(style = "margin:0; color:#0072B2;", paste0(n_selected, " SELECTED"))),
          column(3, tags$h4(style = "margin:0; color:#D55E00;", paste0(n_not_selected, " NOT SELECTED"))),
          column(2, tags$h4(style = "margin:0; color:#F9A825;", paste0(n_revise, " REVISE"))),
          column(2, tags$h4(style = "margin:0; color:#C2185B;", paste0(n_check, " CHECK")))
        )
      )
    })

    # Resolve the requested display portion for the Final Decision Table.
    # Returns -1 for "All". Display-only: saving uses the full table.
    final_table_portion <- reactive({
      p <- suppressWarnings(as.integer(input$finalTablePortion))
      if (length(p) != 1 || is.na(p)) return(-1L)
      p
    })

    # Note beside the selector: how many of the total rows are on screen
    output$finalTablePortionNote <- renderUI({
      tbl <- final_decision_data()
      req(tbl)
      n_total <- nrow(tbl)
      portion <- final_table_portion()
      n_shown <- if (portion > 0) min(portion, n_total) else n_total

      msg <- if (n_shown < n_total) {
        sprintf("Showing top %d of %d evaluated individuals (ranked by index).", n_shown, n_total)
      } else {
        sprintf("Showing all %d evaluated individuals.", n_total)
      }

      tags$div(
        style = "padding-top: 32px; color:#2C3E50;",
        msg,
        tags$span(
          style = "color:#777; font-style:italic;",
          " Saving the final selection always includes every individual."
        )
      )
    })

    # Render finalDecisionDT with gradient coloring (same pattern as tableDecisionDT)
    output$finalDecisionDT <- DT::renderDT({
      tbl <- final_decision_data()
      req(tbl)

      # Subset to the requested portion. final_decision_data() is already sorted
      # by index_value descending, so this yields the top N.
      portion <- final_table_portion()
      if (portion > 0 && portion < nrow(tbl)) {
        tbl <- tbl[seq_len(portion), , drop = FALSE]
      }

      # Get trait columns from modeling
      dt <- data()
      init_stamp <- input$reportInitialSelectionStamp
      modeling_init <- dt$modeling[
        dt$modeling$analysisId %in% init_stamp &
          dt$modeling$module == "Init_prodAdv", , drop = FALSE
      ]
      selected_traits <- unique(modeling_init$trait[!is.na(modeling_init$trait) & nzchar(modeling_init$trait)])
      selected_traits <- selected_traits[!selected_traits %in%
        modeling_init$trait[modeling_init$parameter == "user_excluded_trait"]]
      selected_traits <- intersect(selected_traits, colnames(tbl))

      # Colors (same as tableDecisionDT)
      col_selected <- "#85C1E9"
      col_not_selected <- "#E8A87C"
      col_check <- "#C39BD3"
      col_revise <- "#FFF3CD"

      # get_quintile_opacity (same logic as tableDecisionDT)
      get_quintile_opacity <- function(values, status, higher_is_better = TRUE) {
        n <- length(values)
        opacity <- rep(0.5, n)
        numeric_vals <- as.numeric(values)
        valid <- !is.na(numeric_vals) & status != "CHECK"
        if (sum(valid) < 5) return(opacity)
        quants <- quantile(numeric_vals[valid], probs = c(0.2, 0.4, 0.6, 0.8), na.rm = TRUE)
        for (i in which(valid)) {
          v <- numeric_vals[i]
          if (status[i] == "SELECTED") {
            if (higher_is_better) {
              if (v >= quants[4]) opacity[i] <- 1.0
              else if (v >= quants[3]) opacity[i] <- 0.75
              else if (v >= quants[2]) opacity[i] <- 0.5
              else if (v >= quants[1]) opacity[i] <- 0.3
              else opacity[i] <- 0.15
            } else {
              if (v <= quants[1]) opacity[i] <- 1.0
              else if (v <= quants[2]) opacity[i] <- 0.75
              else if (v <= quants[3]) opacity[i] <- 0.5
              else if (v <= quants[4]) opacity[i] <- 0.3
              else opacity[i] <- 0.15
            }
          } else {
            if (higher_is_better) {
              if (v <= quants[1]) opacity[i] <- 1.0
              else if (v <= quants[2]) opacity[i] <- 0.75
              else if (v <= quants[3]) opacity[i] <- 0.5
              else if (v <= quants[4]) opacity[i] <- 0.3
              else opacity[i] <- 0.15
            } else {
              if (v >= quants[4]) opacity[i] <- 1.0
              else if (v >= quants[3]) opacity[i] <- 0.75
              else if (v >= quants[2]) opacity[i] <- 0.5
              else if (v >= quants[1]) opacity[i] <- 0.3
              else opacity[i] <- 0.15
            }
          }
        }
        opacity
      }

      # blend_color helper
      blend_color <- function(hex_color, opacity) {
        r <- strtoi(substr(hex_color, 2, 3), 16)
        g <- strtoi(substr(hex_color, 4, 5), 16)
        b <- strtoi(substr(hex_color, 6, 7), 16)
        r2 <- as.integer(r * opacity + 255 * (1 - opacity))
        g2 <- as.integer(g * opacity + 255 * (1 - opacity))
        b2 <- as.integer(b * opacity + 255 * (1 - opacity))
        sprintf("#%02X%02X%02X", r2, g2, b2)
      }

      # Build HTML display table
      display_df <- data.frame(designation = tbl$designation, stringsAsFactors = FALSE)

      # Get trait rules for direction-aware opacity
      # First try live UI rules, fallback to stored directions in modeling table
      trait_rules <- tryCatch(trait_rules_input(), error = function(e) list())

      # If trait_rules is empty (UI not available), get directions from modeling table
      if (length(trait_rules) == 0) {
        direction_rows <- modeling_init[modeling_init$parameter == "direction", , drop = FALSE]
        for (i in seq_len(nrow(direction_rows))) {
          tr_name <- direction_rows$trait[i]
          if (!is.null(tr_name) && nzchar(tr_name)) {
            trait_rules[[tr_name]] <- list(direction = direction_rows$value[i])
          }
        }
      }

      # Index value column with gradient (always higher_is_better = TRUE for index)
      if ("index_value" %in% colnames(tbl)) {
        idx_status <- tbl$final_decision
        idx_opacity <- get_quintile_opacity(tbl$index_value, idx_status, higher_is_better = TRUE)
        display_df$index_value <- mapply(function(val, st, op) {
          base_col <- if (st == "SELECTED") col_selected
                      else if (st == "CHECK") col_check
                      else if (st == "REVISE") col_revise
                      else col_not_selected
          bg <- blend_color(base_col, op)
          sprintf("<div style='background:%s; padding:6px; border-radius:4px; text-align:right; font-weight:600;'>%s</div>",
                  bg, format(round(as.numeric(val), 3), nsmall = 3))
        }, tbl$index_value, idx_status, idx_opacity, SIMPLIFY = TRUE)
      }

      # Trait columns with gradient
      for (tr in selected_traits) {
        trait_vals <- tbl[[tr]]
        trait_status <- tbl$final_decision
        # Look up direction from trait_rules_input() for direction-aware gradient
        trait_direction <- trait_rules[[tr]]$direction
        hib <- is.null(trait_direction) || identical(trait_direction, "Higher is better")
        tr_opacity <- get_quintile_opacity(trait_vals, trait_status, higher_is_better = hib)
        display_df[[tr]] <- mapply(function(val, st, op) {
          base_col <- if (st == "SELECTED") col_selected
                      else if (st == "CHECK") col_check
                      else if (st == "REVISE") col_revise
                      else col_not_selected
          bg <- blend_color(base_col, op)
          txt <- if (is.na(val)) "" else format(round(as.numeric(val), 3), nsmall = 3)
          sprintf("<div style='background:%s; padding:6px; border-radius:4px; text-align:right;'>%s</div>", bg, txt)
        }, trait_vals, trait_status, tr_opacity, SIMPLIFY = TRUE)
      }

      # plot_selection column (badge, read-only)
      if ("plot_decision" %in% colnames(tbl)) {
        display_df$plot_selection <- sapply(tbl$plot_decision, function(st) {
          if (is.na(st) || !nzchar(st)) {
            return("<div style='background:#F5F5F5; padding:6px; border-radius:4px; text-align:center;'>\u2014</div>")
          }
          bg <- if (st == "SELECTED") "#D6EAF8"
                else if (st == "NOT SELECTED") "#F5C4A5"
                else if (st == "REVISE") "#FFF3CD"
                else if (st == "CHECK") "#E3A9C4"
                else "#F5F5F5"
          sprintf("<div style='background:%s; padding:6px; border-radius:4px; text-align:center; font-weight:600;'>%s</div>", bg, st)
        })
      }

      # final_decision column (read-only badge)
      display_df$final_decision <- sapply(tbl$final_decision, function(st) {
        if (is.na(st) || !nzchar(st)) {
          return("<div style='background:#F5F5F5; padding:6px; border-radius:4px; text-align:center;'>\u2014</div>")
        }
        bg <- if (st == "SELECTED") "#D6EAF8"
              else if (st == "NOT SELECTED") "#F5C4A5"
              else if (st == "REVISE") "#FFF3CD"
              else if (st == "CHECK") "#E3A9C4"
              else "#F5F5F5"
        sprintf("<div style='background:%s; padding:6px; border-radius:4px; text-align:center; font-weight:600;'>%s</div>", bg, st)
      })

      # --- Numeric sorting fix: hidden sort columns ---
      numeric_final_cols <- c("index_value", selected_traits)
      numeric_final_cols <- intersect(numeric_final_cols, colnames(display_df))
      sort_col_defs_final <- list()
      if (length(numeric_final_cols) > 0) {
        for (k in seq_along(numeric_final_cols)) {
          col_nm <- numeric_final_cols[k]
          sort_col_name <- paste0(".sort_", col_nm)
          display_df[[sort_col_name]] <- as.numeric(tbl[[col_nm]])
          vis_idx <- which(colnames(display_df) == col_nm) - 1L
          sort_idx <- ncol(display_df) - 1L
          sort_col_defs_final <- c(sort_col_defs_final, list(
            list(targets = vis_idx, orderData = sort_idx),
            list(targets = sort_idx, visible = FALSE)
          ))
        }
      }

      DT::datatable(
        display_df,
        escape = FALSE,
        rownames = FALSE,
        selection = "none",
        class = "compact stripe hover nowrap decision-table",
        options = list(
          scrollX = TRUE,
          scrollY = "500px",
          paging = FALSE,
          searching = TRUE,
          ordering = TRUE,
          autoWidth = FALSE,
          dom = "ft",
          columnDefs = sort_col_defs_final
        )
      )
    })

    ###########################################
    # Final Review: Selection Statistics (Req 9)
    ###########################################

    # Metrics shown in the Final Review statistics/plots: the selection index
    # plus every trait used in the initial selection.
    final_review_metrics <- reactive({
      tbl <- tryCatch(final_decision_data(), error = function(e) NULL)
      if (is.null(tbl) || !is.data.frame(tbl) || nrow(tbl) == 0) return(character(0))

      dt <- data()
      init_stamp <- input$reportInitialSelectionStamp
      modeling_init <- dt$modeling[
        dt$modeling$analysisId %in% init_stamp &
          dt$modeling$module == "Init_prodAdv", , drop = FALSE
      ]
      traits <- unique(modeling_init$trait[!is.na(modeling_init$trait) & nzchar(modeling_init$trait)])
      traits <- traits[!traits %in%
        modeling_init$trait[modeling_init$parameter == "user_excluded_trait"]]
      traits <- intersect(traits, colnames(tbl))

      metrics <- character(0)
      if ("index_value" %in% colnames(tbl)) metrics <- "index_value"
      metrics <- c(metrics, traits)

      # Keep only metrics that actually have finite values to summarise
      metrics <- metrics[vapply(metrics, function(m) {
        any(is.finite(suppressWarnings(as.numeric(tbl[[m]]))))
      }, logical(1))]

      metrics
    })

    # Human-readable label for a metric column
    final_review_metric_label <- function(m) {
      if (identical(m, "index_value")) "Index Value" else tpp_display_name(m)
    }

    # Long-format values per metric and group (Selected / All candidates / Checks).
    # Groups come from the final decision, not from a display portion.
    # Returns NULL (rather than req()/validate()) when there is nothing to
    # summarise, so the observers that register the per-trait tables can bail
    # out cleanly instead of raising errors.
    final_review_long <- reactive({
      tbl <- tryCatch(final_decision_data(), error = function(e) NULL)
      if (is.null(tbl) || !is.data.frame(tbl) || nrow(tbl) == 0) return(NULL)
      if (!("final_decision" %in% colnames(tbl))) return(NULL)

      metrics <- tryCatch(final_review_metrics(), error = function(e) character(0))
      if (length(metrics) == 0) return(NULL)

      decisions <- toupper(trimws(as.character(tbl$final_decision)))
      is_check <- decisions == "CHECK"
      is_selected <- decisions == "SELECTED" & !is_check

      # A group can legitimately be empty — e.g. no individual met a trait
      # threshold, so nothing is SELECTED. data.frame() cannot recycle a
      # length-1 column against a length-0 one ("arguments imply differing
      # number of rows: 1, 0"), so skip empty groups instead of constructing
      # them.
      make_piece <- function(metric, group, values) {
        values <- values[is.finite(values)]
        if (length(values) == 0) return(NULL)
        data.frame(
          metric = rep(metric, length(values)),
          group  = rep(group, length(values)),
          value  = values,
          stringsAsFactors = FALSE
        )
      }

      out <- do.call(rbind, lapply(metrics, function(m) {
        vals <- suppressWarnings(as.numeric(tbl[[m]]))
        pieces <- list(
          make_piece(m, "All candidates", vals[!is_check]),
          make_piece(m, "Selected",       vals[is_selected]),
          make_piece(m, "Checks",         vals[is_check])
        )
        pieces <- pieces[!vapply(pieces, is.null, logical(1))]
        if (length(pieces) == 0) return(NULL)
        do.call(rbind, pieces)
      }))

      if (is.null(out) || nrow(out) == 0) return(NULL)
      out$label <- vapply(out$metric, final_review_metric_label, character(1))
      out
    })

    # Render: Selection percentage (SELECTED / total candidates excl checks)
    output$finalReviewSelPct <- renderUI({
      tbl <- final_decision_data()
      req(tbl)
      req("final_decision" %in% colnames(tbl))

      decisions <- toupper(trimws(as.character(tbl$final_decision)))
      # Exclude checks from denominator
      candidates <- decisions[decisions != "CHECK"]
      n_candidates <- length(candidates)
      n_selected <- sum(candidates == "SELECTED", na.rm = TRUE)

      if (n_candidates > 0) {
        pct <- round((n_selected / n_candidates) * 100, 1)
      } else {
        pct <- 0
      }

      tags$div(
        style = "padding-top: 30px;",
        tags$h4(
          style = "color: #2C3E50; font-weight: 600;",
          paste0("Final Selection: ", n_selected, " / ", n_candidates,
                 " candidates SELECTED (", pct, "%)")
        )
      )
    })

    # Stable output id per metric for the per-trait statistics tables.
    # `prefix` lets the Output-tab dashboard render its own copies of the same
    # tables without clashing with the Final Review tab's output ids.
    final_review_stats_output_id <- function(metric, prefix = "finalStatsDT_") {
      paste0(prefix, gsub("[^A-Za-z0-9]", "_", metric))
    }

    # Per-metric statistics data.frame (Selected / All candidates / Checks)
    final_review_stats_for <- function(long, metric) {
      sub <- long[long$metric == metric, , drop = FALSE]
      s <- tpp_compute_dashboard_stats(list(
        selected = sub$value[sub$group == "Selected"],
        overall  = sub$value[sub$group == "All candidates"],
        checks   = sub$value[sub$group == "Checks"]
      ))
      if (!all(c("Group", "Min") %in% colnames(s))) return(NULL)
      # Match the plot legend wording
      s$Group[s$Group == "Overall"] <- "All candidates"
      s$Group <- factor(s$Group, levels = names(REVIEW_GROUP_COLORS))
      s <- s[order(s$Group), , drop = FALSE]
      s$Group <- as.character(s$Group)
      for (col in intersect(c("Min", "Max", "Mean", "Median", "SD"), colnames(s))) {
        s[[col]] <- round(as.numeric(s[[col]]), 3)
      }
      s
    }

    # Build the "one table per trait" UI for a given output-id prefix
    final_review_stats_ui <- function(metrics, prefix) {
      tagList(lapply(metrics, function(m) {
        tags$div(
          style = "margin-bottom: 18px;",
          tags$div(
            style = paste0(
              "font-weight:700; font-size:14px; color:#2C3E50;",
              " padding:6px 0 4px 2px; border-bottom:2px solid #2C3E50;",
              " margin-bottom:6px;"
            ),
            final_review_metric_label(m)
          ),
          DT::DTOutput(ns(final_review_stats_output_id(m, prefix)))
        )
      }))
    }

    # Register one DT renderer per metric for a given output-id prefix
    final_review_register_stats <- function(long, metrics, prefix) {
      faded <- review_group_colors_faded(0.35)

      lapply(metrics, function(m) {
        local({
          metric_name <- m
          out_id <- final_review_stats_output_id(metric_name, prefix)

          output[[out_id]] <- DT::renderDT({
            stats_df <- final_review_stats_for(long, metric_name)
            validate(need(!is.null(stats_df) && nrow(stats_df) > 0,
                          "No trait distribution data available."))

            dt_obj <- DT::datatable(
              stats_df,
              rownames = FALSE,
              selection = "none",
              options = list(
                dom = "t",
                paging = FALSE,
                searching = FALSE,
                ordering = FALSE,
                scrollX = TRUE
              )
            )

            # Faded decision-table colours per group row
            DT::formatStyle(
              dt_obj,
              "Group",
              target = "row",
              backgroundColor = DT::styleEqual(names(faded), unname(faded))
            )
          })
        })
      })
      invisible(NULL)
    }

    # Faceted distribution plot: SELECTED vs ALL candidates, one panel per metric
    final_review_build_plot <- function(long) {
      plot_df <- long[long$group %in% c("All candidates", "Selected"), , drop = FALSE]
      if (nrow(plot_df) == 0) return(NULL)

      metric_order <- unique(long$metric)
      label_order <- vapply(metric_order, final_review_metric_label, character(1))
      plot_df$label <- factor(plot_df$label, levels = unname(label_order))
      plot_df$group <- factor(plot_df$group, levels = c("All candidates", "Selected"))

      check_df <- long[long$group == "Checks", , drop = FALSE]
      check_means <- NULL
      if (nrow(check_df) > 0) {
        check_means <- stats::aggregate(value ~ label, data = check_df, FUN = mean, na.rm = TRUE)
        check_means$label <- factor(check_means$label, levels = unname(label_order))
      }

      p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = value, fill = group)) +
        ggplot2::geom_histogram(position = "identity", alpha = 0.55,
                                bins = 30, colour = NA) +
        ggplot2::facet_wrap(~ label, scales = "free", ncol = 2) +
        ggplot2::scale_fill_manual(
          values = c(
            "All candidates" = unname(REVIEW_GROUP_COLORS[["All candidates"]]),
            "Selected"       = unname(REVIEW_GROUP_COLORS[["Selected"]])
          ),
          name = NULL
        ) +
        ggplot2::labs(x = "Predicted value", y = "Count") +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          legend.position = "top",
          panel.spacing = grid::unit(1.1, "lines"),
          strip.text = ggplot2::element_text(face = "bold")
        )

      if (!is.null(check_means) && nrow(check_means) > 0) {
        p <- p + ggplot2::geom_vline(
          data = check_means,
          ggplot2::aes(xintercept = value),
          colour = unname(REVIEW_GROUP_COLORS[["Checks"]]),
          linetype = "dashed", linewidth = 0.9
        )
      }

      # Mean lines for Selected and All candidates
      sel_df <- plot_df[plot_df$group == "Selected", , drop = FALSE]
      sel_means <- if (nrow(sel_df) > 0) stats::aggregate(value ~ label, data = sel_df, FUN = mean, na.rm = TRUE) else data.frame(label = character(0), value = numeric(0))
      all_df <- plot_df[plot_df$group == "All candidates", , drop = FALSE]
      all_means <- if (nrow(all_df) > 0) stats::aggregate(value ~ label, data = all_df, FUN = mean, na.rm = TRUE) else data.frame(label = character(0), value = numeric(0))
      if (nrow(sel_means) > 0) {
        sel_means$label <- factor(sel_means$label, levels = levels(plot_df$label))
        p <- p + ggplot2::geom_vline(
          data = sel_means,
          ggplot2::aes(xintercept = value),
          colour = unname(REVIEW_GROUP_COLORS[["Selected"]]),
          linetype = "dashed", linewidth = 0.7
        )
      }
      if (nrow(all_means) > 0) {
        all_means$label <- factor(all_means$label, levels = levels(plot_df$label))
        p <- p + ggplot2::geom_vline(
          data = all_means,
          ggplot2::aes(xintercept = value),
          colour = unname(REVIEW_GROUP_COLORS[["All candidates"]]),
          linetype = "dashed", linewidth = 0.7
        )
      }

      n_panels <- length(unique(plot_df$label))
      n_rows <- ceiling(n_panels / 2)

      fig <- plotly::ggplotly(p, height = max(320, 240 * n_rows))

      # Legend above the panels so it cannot overlap the x-axis title
      plotly::layout(
        fig,
        legend = list(
          orientation = "h",
          x = 0.5, xanchor = "center",
          y = 1.06, yanchor = "bottom"
        ),
        margin = list(t = 70, b = 70)
      )
    }

    # Shown when there is nothing to summarise (e.g. no individual met a trait
    # threshold, so no trait values fall into any group)
    final_review_empty_note <- function() {
      tags$div(
        style = paste0(
          "padding:12px 14px; background-color:#FCF3CF; border-left:4px solid #B7950B;",
          " border-radius:3px; color:#7D6608;"
        ),
        tags$b("No statistics to display."),
        " No trait values are available to summarise for this selection \u2014 this",
        " happens when no individual met the configured thresholds, so the selected",
        " set is empty. Revisit the trait thresholds in the Input steps if this is",
        " unexpected."
      )
    }

    # --- Final Review tab: one statistics table per trait ---
    output$finalReviewStatsUI <- renderUI({
      long <- final_review_long()
      if (is.null(long)) return(final_review_empty_note())
      final_review_stats_ui(unique(long$metric), "finalStatsDT_")
    })

    observe({
      long <- final_review_long()
      if (is.null(long)) return()
      final_review_register_stats(long, unique(long$metric), "finalStatsDT_")
    })

    # --- Output dashboard: the same tables, separate output ids ---
    output$dashStatsUI <- renderUI({
      long <- final_review_long()
      if (is.null(long)) return(final_review_empty_note())
      final_review_stats_ui(unique(long$metric), "dashStatsDT_")
    })

    observe({
      long <- final_review_long()
      if (is.null(long)) return()
      final_review_register_stats(long, unique(long$metric), "dashStatsDT_")
    })

    output$dashStatsHistogram <- plotly::renderPlotly({
      long <- final_review_long()
      validate(need(!is.null(long), "No trait distribution data available."))
      fig <- final_review_build_plot(long)
      validate(need(!is.null(fig), "No trait distribution data available."))
      fig
    })

    # Render: distribution of SELECTED candidates against ALL candidates, one
    # panel per metric (selection index + every trait). Checks are drawn as a
    # dashed reference line rather than bars so they don't distort the counts.
    output$finalReviewHistogram <- plotly::renderPlotly({
      long <- final_review_long()
      validate(need(!is.null(long), "No trait distribution data available."))
      fig <- final_review_build_plot(long)
      validate(need(!is.null(fig), "No trait distribution data available."))
      fig
    })

    # Render: TPP compliance percentages (only when TPP is active)
    output$finalReviewTppPctsDT <- DT::renderDT({
      tbl <- final_decision_data()
      req(tbl)
      req("final_decision" %in% colnames(tbl))

      # Check TPP is active
      current_tpp <- tpp_id_prepam()
      filtered <- tpp_filtered_traits()
      has_tpp <- !is.null(current_tpp) && !is.null(filtered) &&
        is.data.frame(filtered) && nrow(filtered) > 0

      req(has_tpp)

      # Build criteria list
      checks_info <- if (!is.null(current_tpp) && !is.null(data()$metadata$TPP[[current_tpp]]$checks_per_trait)) {
        data()$metadata$TPP[[current_tpp]]$checks_per_trait
      } else { NULL }
      tpp_criteria <- tpp_build_criteria_list_from_filtered(filtered, tbl, checks_per_trait = checks_info)
      req(length(tpp_criteria) > 0)

      # Evaluate criteria matrices for both category filters
      criteria_matrix_all <- tpp_evaluate_all_criteria(
        tbl, tpp_criteria,
        category_filter = c("Essential_Improve", "Essential_Maintain")
      )
      criteria_matrix_improve <- tpp_evaluate_all_criteria(
        tbl, tpp_criteria,
        category_filter = "Essential_Improve"
      )

      # Get selected designations (SELECTED status)
      decisions <- toupper(trimws(as.character(tbl$final_decision)))
      selected_designations <- tbl$designation[decisions == "SELECTED"]

      # Compute compliance percentages
      compliance_df <- tpp_compute_compliance_pcts(
        criteria_matrix_all,
        criteria_matrix_improve,
        subset_designations = selected_designations
      )

      # If it returned a message (no data), return NULL
      if ("Message" %in% colnames(compliance_df)) {
        return(NULL)
      }

      # Format percentages
      compliance_df$Selected_Pct <- paste0(round(compliance_df$Selected_Pct, 1), "%")
      compliance_df$Overall_Pct <- paste0(round(compliance_df$Overall_Pct, 1), "%")

      colnames(compliance_df) <- c("TPP Criteria", "Selected (%)", "Overall (%)")

      DT::datatable(
        compliance_df,
        rownames = FALSE,
        selection = "none",
        options = list(
          dom = "t",
          paging = FALSE,
          searching = FALSE,
          ordering = FALSE
        ),
        caption = "TPP Compliance: % of individuals meeting all criteria"
      )
    })

    # --- TPP Breakdown box on the Final Review tab (collapsed, only when TPP is active) ---
    output$finalReviewTppBreakdownBox <- renderUI({
      current_tpp <- tpp_id_prepam()
      if (is.null(current_tpp)) return(NULL)

      dt <- tryCatch(data(), error = function(e) NULL)
      if (is.null(dt)) return(NULL)
      tpp_meta_all <- dt$metadata$TPP
      if (is.null(tpp_meta_all) || !(current_tpp %in% names(tpp_meta_all))) return(NULL)

      ns <- session$ns
      shinydashboard::box(
        width = 12,
        title = paste0("TPP Breakdown (", current_tpp, ")"),
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        DT::DTOutput(ns("finalReviewTppBreakdownDT"))
      )
    })

    output$finalReviewTppBreakdownDT <- DT::renderDT({
      res <- tpp_breakdown_detail()

      if (is.null(res)) {
        msg_df <- data.frame(Message = "No TPP information available",
                             stringsAsFactors = FALSE)
        return(DT::datatable(msg_df, options = list(dom = "t", paging = FALSE),
                             rownames = FALSE))
      }

      tpp_style_breakdown_proximity(
        DT::datatable(
          res$table,
          rownames = FALSE,
          selection = "none",
          options = list(dom = "tp", pageLength = 20, scrollX = TRUE, ordering = TRUE)
        )
      )
    })

    # --- Trait distribution per status plot render ---
    output$pianoPlot <- plotly::renderPlotly({
      tbl <- final_decision_data()
      req(tbl)
      req("final_decision" %in% colnames(tbl))

      # Get traits
      dt <- data()
      init_stamp <- input$reportInitialSelectionStamp
      modeling_init <- dt$modeling[
        dt$modeling$analysisId %in% init_stamp & dt$modeling$module == "Init_prodAdv", , drop = FALSE
      ]
      selected_traits <- unique(modeling_init$trait[!is.na(modeling_init$trait) & nzchar(modeling_init$trait)])
      selected_traits <- selected_traits[!selected_traits %in%
        modeling_init$trait[modeling_init$parameter == "user_excluded_trait"]]
      selected_traits <- intersect(selected_traits, colnames(tbl))
      req(length(selected_traits) > 0)

      # Build long-format plot_df
      plot_df <- do.call(rbind, lapply(selected_traits, function(tr) {
        data.frame(
          designation = tbl$designation,
          trait = tr,
          value = as.numeric(tbl[[tr]]),
          status = tbl$final_decision,
          stringsAsFactors = FALSE
        )
      }))
      plot_df <- plot_df[!is.na(plot_df$value), , drop = FALSE]

      # Get trait directions and thresholds
      trait_rules <- tryCatch(trait_rules_input(), error = function(e) list())
      # Fallback to modeling table if trait_rules_input() is empty
      if (length(trait_rules) == 0) {
        direction_rows <- modeling_init[modeling_init$parameter == "direction", , drop = FALSE]
        for (i in seq_len(nrow(direction_rows))) {
          tr_name <- direction_rows$trait[i]
          if (!is.null(tr_name) && nzchar(tr_name)) {
            trait_rules[[tr_name]] <- list(direction = direction_rows$value[i])
          }
        }
      }
      trait_directions <- lapply(selected_traits, function(tr) {
        d <- trait_rules[[tr]]$direction
        if (is.null(d)) "Higher is better" else d
      })
      names(trait_directions) <- selected_traits

      thresholds <- lapply(selected_traits, function(tr) trait_rules[[tr]]$threshold)
      names(thresholds) <- selected_traits

      build_trait_distribution_plotly(plot_df, trait_directions, thresholds)
    })

    observeEvent(input$saveFinalSelection, {
      # 1. Validate analysis name is non-empty (trimmed)
      analysis_name <- trimws(input$finalAnalysisName)
      if (!nzchar(analysis_name)) {
        showNotification(
          "Please provide an analysis name before saving.",
          type = "error",
          duration = 5
        )
        return()
      }

      req(data())
      req(input$reportInitialSelectionStamp)

      # 2. Get the final decisions from the reactive
      final_tbl <- final_decision_data()
      req(nrow(final_tbl) > 0)

      # Build final_decisions data frame with designation and final_decision columns
      final_decisions <- final_tbl[, c("designation", "final_decision"), drop = FALSE]

      # 3. Generate analysisId as timestamp (pattern used elsewhere)
      analysis_id <- as.numeric(Sys.time())

      # 4. Determine table and plot stamps (NULL if "None" or empty)
      table_stamp <- input$reportPlotSelectionStamp
      if (is.null(table_stamp) || !nzchar(table_stamp) || table_stamp == "__none__" || table_stamp == "None") {
        table_stamp <- NULL
      }

      plot_stamp <- input$reportFinalSelectionStamp
      if (is.null(plot_stamp) || !nzchar(plot_stamp) || plot_stamp == "__none__" || plot_stamp == "None") {
        plot_stamp <- NULL
      }

      # 5. Call cgiarPipeline::saveFinalProdAdvSelection() with assembled parameters
      dt_object <- cgiarPipeline::saveFinalProdAdvSelection(
        analysisId = analysis_id,
        analysisIdName = analysis_name,
        initialSelectionStamp = input$reportInitialSelectionStamp,
        tableSelectionStamp = table_stamp,
        plotSelectionStamp = plot_stamp,
        final_decisions = final_decisions,
        dt_object = data()
      )

      # Update the data reactive with the returned dt_object
      data(dt_object)

      # Update final_decision_overrides with saved decisions
      final_decision_overrides(final_decisions)

      # Rebuild final stamp choices and select the newly saved stamp
      dt_status <- dt_object$status
      dtFinalSel <- dt_status[dt_status$module == "Final_prodAdv", , drop = FALSE]
      stampsFinalSel <- unique(dtFinalSel$analysisId)

      if (length(stampsFinalSel) > 0) {
        if ("analysisIdName" %in% colnames(dtFinalSel)) {
          names(stampsFinalSel) <- paste(
            dtFinalSel$analysisIdName,
            as.POSIXct(stampsFinalSel, origin = "1970-01-01", tz = "GMT"),
            sep = "_"
          )
        } else {
          names(stampsFinalSel) <- as.character(
            as.POSIXct(stampsFinalSel, origin = "1970-01-01", tz = "GMT")
          )
        }
      }

      updateSelectInput(
        session,
        "finalSelectionStamp",
        choices = c(" " = "", stampsFinalSel),
        selected = as.character(analysis_id)
      )

      # 6. Generate final report and switch to Output tab
      shinybusy::show_modal_spinner(spin = "fading-circle", text = "Generating Report...")

      result <- data()
      final_table_export <- final_report_table_raw()

      # Gather trait directions and thresholds for the trait distribution plot
      trait_directions <- tryCatch({
        rules <- trait_rules_input()
        dirs <- lapply(rules, function(r) if (!is.null(r)) r$direction else NULL)
        names(dirs) <- vapply(rules, function(r) if (!is.null(r)) r$trait else NA_character_, character(1))
        dirs[!is.na(names(dirs))]
      }, error = function(e) list())

      # Fallback to modeling table if trait_rules_input() is not available
      if (length(trait_directions) == 0) {
        init_mod <- result$modeling[
          result$modeling$analysisId %in% input$reportInitialSelectionStamp &
            result$modeling$module == "Init_prodAdv" &
            result$modeling$parameter == "direction", , drop = FALSE
        ]
        for (i in seq_len(nrow(init_mod))) {
          tr_name <- init_mod$trait[i]
          if (!is.null(tr_name) && nzchar(tr_name)) {
            trait_directions[[tr_name]] <- init_mod$value[i]
          }
        }
      }

      trait_thresholds <- tryCatch({
        rules <- trait_rules_input()
        ths <- lapply(rules, function(r) if (!is.null(r) && !is.null(r$threshold)) r$threshold else NULL)
        names(ths) <- vapply(rules, function(r) if (!is.null(r)) r$trait else NA_character_, character(1))
        ths[!is.na(names(ths))]
      }, error = function(e) list())

      src <- normalizePath(system.file("rmd","reportProdAdv.Rmd", package = "bioflow"))

      tmp_report <- file.path(tempdir(), "reportProdAdv_tmp.Rmd")
      tmp_rdata  <- file.path(tempdir(), "resultProdAdv.RData")

      .rx <- report_extra_payload()
      breakdown_detail <- .rx$breakdown_detail
      options_table    <- .rx$options_table

      # Build review_long directly from the decision table to ensure all traits
      # are included (avoids reactive timing issues with final_review_long())
      review_long <- tryCatch({
        tbl <- final_decision_data()
        if (is.null(tbl) || !is.data.frame(tbl) || nrow(tbl) == 0) return(NULL)
        if (!("final_decision" %in% colnames(tbl))) return(NULL)

        init_stamp <- input$reportInitialSelectionStamp
        modeling_init_local <- result$modeling[
          result$modeling$analysisId %in% init_stamp &
            result$modeling$module == "Init_prodAdv", , drop = FALSE
        ]
        traits <- unique(modeling_init_local$trait[!is.na(modeling_init_local$trait) & nzchar(modeling_init_local$trait)])
        traits <- traits[!traits %in%
          modeling_init_local$trait[modeling_init_local$parameter == "user_excluded_trait"]]
        traits <- intersect(traits, colnames(tbl))

        metrics <- character(0)
        if ("index_value" %in% colnames(tbl)) metrics <- "index_value"
        metrics <- c(metrics, traits)
        metrics <- metrics[vapply(metrics, function(m) {
          any(is.finite(suppressWarnings(as.numeric(tbl[[m]]))))
        }, logical(1))]

        if (length(metrics) == 0) return(NULL)

        decisions <- toupper(trimws(as.character(tbl$final_decision)))
        is_check <- decisions == "CHECK"
        is_selected <- decisions == "SELECTED" & !is_check

        make_piece <- function(metric, group, values) {
          values <- values[is.finite(values)]
          if (length(values) == 0) return(NULL)
          data.frame(metric = rep(metric, length(values)),
                     group = rep(group, length(values)),
                     value = values, stringsAsFactors = FALSE)
        }

        out <- do.call(rbind, lapply(metrics, function(m) {
          vals <- suppressWarnings(as.numeric(tbl[[m]]))
          pieces <- list(
            make_piece(m, "All candidates", vals[!is_check]),
            make_piece(m, "Selected", vals[is_selected]),
            make_piece(m, "Checks", vals[is_check])
          )
          pieces <- pieces[!vapply(pieces, is.null, logical(1))]
          if (length(pieces) == 0) return(NULL)
          do.call(rbind, pieces)
        }))

        if (is.null(out) || nrow(out) == 0) return(NULL)
        out$label <- vapply(out$metric, function(m) {
          if (identical(m, "index_value")) "Index Value" else tpp_display_name(m)
        }, character(1))
        out
      }, error = function(e) .rx$review_long)

      save(result, final_table_export, trait_directions, trait_thresholds,
           STATUS_COLORS, STATUS_SHAPES, REVIEW_GROUP_COLORS, analysis_name,
           review_long, breakdown_detail, options_table, file = tmp_rdata)
      file.copy(src, tmp_report, overwrite = TRUE)

      updateTabsetPanel(session, "tabsMain", selected = "outputTabs")

      output$reportProdAdv <- renderUI({
        old <- setwd(tempdir())
        on.exit(setwd(old), add = TRUE)

        HTML(
          markdown::markdownToHTML(
            knitr::knit(tmp_report, quiet = TRUE),
            fragment.only = TRUE
          )
        )
      })

      shinybusy::remove_modal_spinner()

      showNotification(
        paste0("Final selection saved successfully as: ", analysis_name),
        type = "message",
        duration = 5
      )
    }, ignoreInit = TRUE)

    ###########################################
    # Final report (idx7)
    ###########################################

    observeEvent(data(), {
      req(data())

      dt <- data()$status

      dtInitSel  <- dt[dt$module == "Init_prodAdv",  , drop = FALSE]
      dtTableSel <- dt[dt$module == "Table_prodAdv", , drop = FALSE]
      dtPlotSel  <- dt[dt$module == "Plot_prodAdv",  , drop = FALSE]

      stampsInitSel  <- make_stamp_choices(dtInitSel)
      stampsTableSel <- make_stamp_choices(dtTableSel)
      stampsPlotSel  <- make_stamp_choices(dtPlotSel)

      # This observer re-fires on every data() change, which includes saving a
      # final decision. Keep whatever the user already picked so the selection
      # isn't silently moved off their stamp; only fall back to a default when
      # the current value is unset or no longer a valid choice. The default is
      # the MOST RECENT stamp, matching the two dropdowns below.
      keep_or_default <- function(current, choices, default_none = NULL) {
        vals <- unname(choices)
        if (!is.null(current) && length(current) == 1 && !is.na(current) &&
            nzchar(current) && current %in% vals) {
          return(current)
        }
        if (length(vals) > 0) vals[length(vals)] else default_none
      }

      cur_init  <- isolate(input$reportInitialSelectionStamp)
      cur_table <- isolate(input$reportPlotSelectionStamp)
      cur_plot  <- isolate(input$reportFinalSelectionStamp)

      updateSelectInput(
        session,
        "reportInitialSelectionStamp",
        choices = stampsInitSel,
        selected = keep_or_default(cur_init, stampsInitSel, NULL)
      )

      updateSelectInput(
        session,
        "reportPlotSelectionStamp",
        choices = c("No table selection" = "__none__", stampsTableSel),
        selected = if (!is.null(cur_table) && length(cur_table) == 1 &&
                       identical(cur_table, "__none__")) {
          "__none__"
        } else {
          keep_or_default(cur_table, stampsTableSel, "__none__")
        }
      )

      updateSelectInput(
        session,
        "reportFinalSelectionStamp",
        choices = c("No plot selection" = "__none__", stampsPlotSel),
        selected = if (!is.null(cur_plot) && length(cur_plot) == 1 &&
                       identical(cur_plot, "__none__")) {
          "__none__"
        } else {
          keep_or_default(cur_plot, stampsPlotSel, "__none__")
        }
      )
    })

    final_report_table_data <- reactive({
      build_decision_table_data(
        dt = data(),
        initial_stamp = input$reportInitialSelectionStamp,
        table_stamp = input$reportPlotSelectionStamp,
        plot_stamp = input$reportFinalSelectionStamp,
        final_overrides = NULL
      )
    })

    final_report_table_raw <- reactive({
      tbl <- final_report_table_data()$raw

      drop_cols <- c(
        "entryType",
        "is_check_sort",
        "final_decision",
        "final_manual_decision",
        "final_decision_initial"
      )

      tbl <- tbl[, setdiff(colnames(tbl), drop_cols), drop = FALSE]

      if ("final_decision_initial" %in% colnames(final_report_table_data()$raw)) {
        tbl$final_decision <- final_report_table_data()$raw$final_decision_initial
      }

      tbl
    })

    report <- reactiveVal(NULL)

    observeEvent(input$renderReportProdAdv, {

      req(input$reportInitialSelectionStamp)
      req(input$reportPlotSelectionStamp)
      req(input$reportFinalSelectionStamp)

      shinybusy::show_modal_spinner(spin = "fading-circle", text = "Generating Report...")

      result <- data()
      final_table_export <- final_report_table_raw()

      # Gather trait directions and thresholds for the trait distribution plot
      trait_directions <- tryCatch({
        rules <- trait_rules_input()
        dirs <- lapply(rules, function(r) if (!is.null(r)) r$direction else NULL)
        names(dirs) <- vapply(rules, function(r) if (!is.null(r)) r$trait else NA_character_, character(1))
        dirs[!is.na(names(dirs))]
      }, error = function(e) list())

      # Fallback to modeling table if trait_rules_input() is not available
      if (length(trait_directions) == 0) {
        init_mod <- result$modeling[
          result$modeling$analysisId %in% input$reportInitialSelectionStamp &
            result$modeling$module == "Init_prodAdv" &
            result$modeling$parameter == "direction", , drop = FALSE
        ]
        for (i in seq_len(nrow(init_mod))) {
          tr_name <- init_mod$trait[i]
          if (!is.null(tr_name) && nzchar(tr_name)) {
            trait_directions[[tr_name]] <- init_mod$value[i]
          }
        }
      }

      trait_thresholds <- tryCatch({
        rules <- trait_rules_input()
        ths <- lapply(rules, function(r) if (!is.null(r) && !is.null(r$threshold)) r$threshold else NULL)
        names(ths) <- vapply(rules, function(r) if (!is.null(r)) r$trait else NA_character_, character(1))
        ths[!is.na(names(ths))]
      }, error = function(e) list())

      analysis_name <- tryCatch(input$finalAnalysisName, error = function(e) "")

      src <- normalizePath(system.file("rmd","reportProdAdv.Rmd", package = "bioflow"))

      tmp_report <- file.path(tempdir(), "reportProdAdv_download.Rmd")
      tmp_rdata  <- file.path(tempdir(), "resultProdAdv.RData")

      .rx <- report_extra_payload()
      breakdown_detail <- .rx$breakdown_detail
      options_table    <- .rx$options_table

      # Build review_long directly from the decision table to ensure all traits
      # are included (avoids reactive timing issues with final_review_long())
      review_long <- tryCatch({
        tbl <- final_decision_data()
        if (is.null(tbl) || !is.data.frame(tbl) || nrow(tbl) == 0) return(NULL)
        if (!("final_decision" %in% colnames(tbl))) return(NULL)

        # Determine metrics: index_value + all trait columns from modeling
        init_stamp <- input$reportInitialSelectionStamp
        modeling_init <- result$modeling[
          result$modeling$analysisId %in% init_stamp &
            result$modeling$module == "Init_prodAdv", , drop = FALSE
        ]
        traits <- unique(modeling_init$trait[!is.na(modeling_init$trait) & nzchar(modeling_init$trait)])
        traits <- traits[!traits %in%
          modeling_init$trait[modeling_init$parameter == "user_excluded_trait"]]
        traits <- intersect(traits, colnames(tbl))

        metrics <- character(0)
        if ("index_value" %in% colnames(tbl)) metrics <- "index_value"
        metrics <- c(metrics, traits)
        metrics <- metrics[vapply(metrics, function(m) {
          any(is.finite(suppressWarnings(as.numeric(tbl[[m]]))))
        }, logical(1))]

        if (length(metrics) == 0) return(NULL)

        decisions <- toupper(trimws(as.character(tbl$final_decision)))
        is_check <- decisions == "CHECK"
        is_selected <- decisions == "SELECTED" & !is_check

        make_piece <- function(metric, group, values) {
          values <- values[is.finite(values)]
          if (length(values) == 0) return(NULL)
          data.frame(metric = rep(metric, length(values)),
                     group = rep(group, length(values)),
                     value = values, stringsAsFactors = FALSE)
        }

        out <- do.call(rbind, lapply(metrics, function(m) {
          vals <- suppressWarnings(as.numeric(tbl[[m]]))
          pieces <- list(
            make_piece(m, "All candidates", vals[!is_check]),
            make_piece(m, "Selected", vals[is_selected]),
            make_piece(m, "Checks", vals[is_check])
          )
          pieces <- pieces[!vapply(pieces, is.null, logical(1))]
          if (length(pieces) == 0) return(NULL)
          do.call(rbind, pieces)
        }))

        if (is.null(out) || nrow(out) == 0) return(NULL)
        out$label <- vapply(out$metric, function(m) {
          if (identical(m, "index_value")) "Index Value" else tpp_display_name(m)
        }, character(1))
        out
      }, error = function(e) .rx$review_long)

      save(result, final_table_export, trait_directions, trait_thresholds,
           STATUS_COLORS, STATUS_SHAPES, REVIEW_GROUP_COLORS, analysis_name,
           review_long, breakdown_detail, options_table, file = tmp_rdata)
      file.copy(src, tmp_report, overwrite = TRUE)

      old <- setwd(tempdir())
      on.exit(setwd(old), add = TRUE)

      outReport <- rmarkdown::render(
        input = basename(tmp_report),
        params = list(toDownload = TRUE),
        output_format = rmdformats::robobook(toc_depth = 4)
      )

      report(outReport)

      shinybusy::remove_modal_spinner()
      shinyjs::click("downloadReportProdAdv")
    })

    # Serves the standalone HTML rendered by the observer above. Without this
    # handler the hidden downloadButton has no server binding, so the browser
    # follows the download URL and saves the Bioflow app page itself.
    output$downloadReportProdAdv <- downloadHandler(
      filename = function() {
        paste(paste0('preadvancement_dashboard_', gsub("-", "", as.integer(Sys.time()))),
              sep = '.', switch("HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'))
      },
      content = function(file) {
        out <- report()
        req(out)
        # file.rename fails across devices; fall back to it only if copy fails
        if (!file.copy(out, file, overwrite = TRUE)) {
          file.rename(out, file)
        }
      }
    )

    observeEvent(input$runFinalProdAdv, {

      req(input$reportInitialSelectionStamp)
      req(input$reportPlotSelectionStamp)
      req(input$reportFinalSelectionStamp)

      shinybusy::show_modal_spinner(spin = "fading-circle", text = "Generating Report...")

      result <- data()
      final_table_export <- final_report_table_raw()

      # Gather trait directions and thresholds for the trait distribution plot
      trait_directions <- tryCatch({
        rules <- trait_rules_input()
        dirs <- lapply(rules, function(r) if (!is.null(r)) r$direction else NULL)
        names(dirs) <- vapply(rules, function(r) if (!is.null(r)) r$trait else NA_character_, character(1))
        dirs[!is.na(names(dirs))]
      }, error = function(e) list())

      # Fallback to modeling table if trait_rules_input() is not available
      if (length(trait_directions) == 0) {
        init_mod <- result$modeling[
          result$modeling$analysisId %in% input$reportInitialSelectionStamp &
            result$modeling$module == "Init_prodAdv" &
            result$modeling$parameter == "direction", , drop = FALSE
        ]
        for (i in seq_len(nrow(init_mod))) {
          tr_name <- init_mod$trait[i]
          if (!is.null(tr_name) && nzchar(tr_name)) {
            trait_directions[[tr_name]] <- init_mod$value[i]
          }
        }
      }

      trait_thresholds <- tryCatch({
        rules <- trait_rules_input()
        ths <- lapply(rules, function(r) if (!is.null(r) && !is.null(r$threshold)) r$threshold else NULL)
        names(ths) <- vapply(rules, function(r) if (!is.null(r)) r$trait else NA_character_, character(1))
        ths[!is.na(names(ths))]
      }, error = function(e) list())

      analysis_name <- tryCatch(input$finalAnalysisName, error = function(e) "")

      src <- normalizePath(system.file("rmd","reportProdAdv.Rmd", package = "bioflow"))

      tmp_report <- file.path(tempdir(), "reportProdAdv_tmp.Rmd")
      tmp_rdata  <- file.path(tempdir(), "resultProdAdv.RData")

      .rx <- report_extra_payload()
      breakdown_detail <- .rx$breakdown_detail
      options_table    <- .rx$options_table

      # Build review_long directly from the decision table to ensure all traits
      # are included (avoids reactive timing issues with final_review_long())
      review_long <- tryCatch({
        tbl <- final_decision_data()
        if (is.null(tbl) || !is.data.frame(tbl) || nrow(tbl) == 0) return(NULL)
        if (!("final_decision" %in% colnames(tbl))) return(NULL)

        init_stamp <- input$reportInitialSelectionStamp
        modeling_init_local <- result$modeling[
          result$modeling$analysisId %in% init_stamp &
            result$modeling$module == "Init_prodAdv", , drop = FALSE
        ]
        traits <- unique(modeling_init_local$trait[!is.na(modeling_init_local$trait) & nzchar(modeling_init_local$trait)])
        traits <- traits[!traits %in%
          modeling_init_local$trait[modeling_init_local$parameter == "user_excluded_trait"]]
        traits <- intersect(traits, colnames(tbl))

        metrics <- character(0)
        if ("index_value" %in% colnames(tbl)) metrics <- "index_value"
        metrics <- c(metrics, traits)
        metrics <- metrics[vapply(metrics, function(m) {
          any(is.finite(suppressWarnings(as.numeric(tbl[[m]]))))
        }, logical(1))]

        if (length(metrics) == 0) return(NULL)

        decisions <- toupper(trimws(as.character(tbl$final_decision)))
        is_check <- decisions == "CHECK"
        is_selected <- decisions == "SELECTED" & !is_check

        make_piece <- function(metric, group, values) {
          values <- values[is.finite(values)]
          if (length(values) == 0) return(NULL)
          data.frame(metric = rep(metric, length(values)),
                     group = rep(group, length(values)),
                     value = values, stringsAsFactors = FALSE)
        }

        out <- do.call(rbind, lapply(metrics, function(m) {
          vals <- suppressWarnings(as.numeric(tbl[[m]]))
          pieces <- list(
            make_piece(m, "All candidates", vals[!is_check]),
            make_piece(m, "Selected", vals[is_selected]),
            make_piece(m, "Checks", vals[is_check])
          )
          pieces <- pieces[!vapply(pieces, is.null, logical(1))]
          if (length(pieces) == 0) return(NULL)
          do.call(rbind, pieces)
        }))

        if (is.null(out) || nrow(out) == 0) return(NULL)
        out$label <- vapply(out$metric, function(m) {
          if (identical(m, "index_value")) "Index Value" else tpp_display_name(m)
        }, character(1))
        out
      }, error = function(e) .rx$review_long)

      save(result, final_table_export, trait_directions, trait_thresholds,
           STATUS_COLORS, STATUS_SHAPES, REVIEW_GROUP_COLORS, analysis_name,
           review_long, breakdown_detail, options_table, file = tmp_rdata)
      file.copy(src, tmp_report, overwrite = TRUE)

      updateTabsetPanel(session, "tabsMain", selected = "outputTabs")

      output$reportProdAdv <- renderUI({
        old <- setwd(tempdir())
        on.exit(setwd(old), add = TRUE)

        HTML(
          markdown::markdownToHTML(
            knitr::knit(tmp_report, quiet = TRUE),
            fragment.only = TRUE
          )
        )
      })

      shinybusy::remove_modal_spinner()
    })

    ###########################################
    # TPP Dashboard Tables (Task 13.1)
    # Requirements: 10.1-10.10, 14.1-14.4
    ###########################################

    # Population Statistics on the Output tab is rendered by dashStatsUI /
    # dashStatsHistogram, which reuse the Final Review tab's per-trait tables
    # and distribution plot so both views stay identical.

    # --- TPP Breakdown: explanation, detailed table, environment notes ---
    # Contrasts what the TPP asks for (verbatim) against what was actually
    # used on the phenotypic data, including the percentage of the SELECTED
    # individuals meeting each criterion.

    tpp_breakdown_detail <- reactive({
      current_tpp <- tpp_id_prepam()
      if (is.null(current_tpp)) return(NULL)

      dt <- tryCatch(data(), error = function(e) NULL)
      if (is.null(dt)) return(NULL)

      tpp_meta <- dt$metadata$TPP[[current_tpp]]
      raw_tpp <- tryCatch(dt$data$TPP[[current_tpp]], error = function(e) NULL)
      if (is.null(tpp_meta) || is.null(raw_tpp)) return(NULL)

      # Use final_decision_data() — the same authoritative table behind the
      # Final Decision Table and the statistics panels. final_report_table_data()
      # was yielding no SELECTED rows here, which made every percentage read
      # "no selection".
      predictions_df <- tryCatch(final_decision_data(), error = function(e) NULL)
      if (is.null(predictions_df) || !is.data.frame(predictions_df) ||
          nrow(predictions_df) == 0) {
        predictions_df <- tryCatch({
          tb <- final_report_table_data()$raw
          if (!is.null(tb) && is.data.frame(tb) && nrow(tb) > 0) tb else NULL
        }, error = function(e) NULL)
      }
      if (is.null(predictions_df)) return(NULL)

      status_col <- if ("final_decision" %in% colnames(predictions_df)) "final_decision"
                    else if ("plot_status" %in% colnames(predictions_df)) "plot_status"
                    else NULL

      selected_designations <- character(0)
      candidate_designations <- unique(as.character(predictions_df$designation))
      if (!is.null(status_col)) {
        st <- toupper(trimws(as.character(predictions_df[[status_col]])))
        selected_designations <- predictions_df$designation[st == "SELECTED"]
        # Candidate set excludes checks, matching the statistics panel wording
        candidate_designations <- predictions_df$designation[st != "CHECK"]
      }

      # Total environments, for phrasing the environment-subset notes, plus the
      # traits actually carried into the selection (excluding any the user
      # dropped, e.g. after a low-reliability flag).
      n_env <- NA_integer_
      used_traits <- NULL
      init_stamp <- tryCatch(input$reportInitialSelectionStamp, error = function(e) NULL)
      if (!is.null(init_stamp) && length(init_stamp) == 1 && nzchar(init_stamp)) {
        srow <- which(
          as.character(dt$modeling$analysisId) == as.character(init_stamp) &
            dt$modeling$module == "Init_prodAdv" &
            dt$modeling$parameter == "sta_stamp"
        )
        if (length(srow) > 0) {
          sta_stamp <- dt$modeling$value[srow[1]]
          envs <- dt$predictions$environment[
            dt$predictions$analysisId %in% sta_stamp]
          envs <- unique(envs[!is.na(envs) & nzchar(envs) & envs != "across"])
          if (length(envs) > 0) n_env <- length(envs)
        }

        mrows <- which(
          as.character(dt$modeling$analysisId) == as.character(init_stamp) &
            dt$modeling$module == "Init_prodAdv"
        )
        if (length(mrows) > 0) {
          mi <- dt$modeling[mrows, , drop = FALSE]
          all_tr <- unique(mi$trait[!is.na(mi$trait) & nzchar(mi$trait)])
          dropped <- unique(mi$trait[mi$parameter == "user_excluded_trait"])
          dropped <- dropped[!is.na(dropped) & nzchar(dropped)]
          used_traits <- setdiff(all_tr, dropped)
        }
      }

      tpp_build_breakdown_detail(
        tpp_id = current_tpp,
        raw_tpp = raw_tpp,
        traits_df = tpp_meta$traits,
        env_filters = tpp_meta$env_filters,
        checks_per_trait = tpp_meta$checks_per_trait,
        predictions_df = predictions_df,
        selected_designations = selected_designations,
        candidate_designations = candidate_designations,
        n_total_environments = n_env,
        used_traits = used_traits
      )
    })

    output$tppBreakdownIntro <- renderUI({
      tags$p(
        style = "color:#2C3E50; line-height:1.55; margin-bottom:12px;",
        "This table compares what the Target Product Profile asks for against",
        "what was actually evaluated on your phenotypic data. For each TPP trait it",
        "shows whether the trait was mapped to a column in the phenotypic data",
        tags$b("(Mapped, Trait_Name_data)"), ", whether that mapped trait was actually",
        "carried into the selection", tags$b("(Used_for_selection)"), "\u2014 a trait",
        "you dropped, for instance after a low-reliability flag, is mapped but not",
        "used, and its percentage reads \"trait not used\" \u2014 the requirement",
        "category, and two",
        "versions of both the desired score and the checks: the",
        tags$b("_tpp"), " columns are reproduced verbatim from the TPP, while the",
        tags$b("_data"), " columns show what was resolved against your data \u2014",
        "relative specifications such as \"Percentage above check 5\" are converted",
        "into the equivalent absolute threshold (for example \"Higher than 6.72\")",
        "using the mean of the checks you mapped, and the mapped check",
        "designations replace the TPP check labels. The",
        tags$b("mean_score_sel_data"), "and", tags$b("mean_score_all_data"),
        "columns give the observed mean of the trait in the selected set and",
        "across the whole candidate set (checks excluded), so you can see the shift",
        "selection produced. The two compliance columns pair with them:",
        tags$b("Sel_pct_meeting_criteria"), "is the share of the",
        tags$b("selected"), "individuals meeting the resolved threshold, while",
        tags$b("All_pct_meeting_criteria"), "is the share across all candidates \u2014",
        "comparing the two shows how much the criterion was enriched by selection.",
        "Traits with no phenotypic mapping are reported as \"not evaluated\", traits",
        "you dropped read \"trait not used\", and traits whose desired score is",
        "relative but have no mapped checks cannot be resolved to an absolute value."
      )
    })

    output$tppDashboardBreakdown <- DT::renderDT({
      res <- tpp_breakdown_detail()

      if (is.null(res)) {
        msg_df <- data.frame(Message = "No TPP information available",
                             stringsAsFactors = FALSE)
        return(DT::datatable(msg_df, options = list(dom = "t", paging = FALSE),
                             rownames = FALSE))
      }

      tpp_style_breakdown_proximity(
        DT::datatable(
          res$table,
          rownames = FALSE,
          selection = "none",
          options = list(dom = "tp", pageLength = 20, scrollX = TRUE, ordering = TRUE)
        )
      )
    })

    output$tppBreakdownEnvNotes <- renderUI({
      res <- tpp_breakdown_detail()
       if (is.null(res) || length(res$env_notes) == 0) return(NULL)

      tags$div(
        style = paste0(
          "margin-top:12px; padding:12px 14px; background-color:#F4F6F7;",
          " border-left:4px solid #2C3E50; border-radius:3px;"
        ),
        tags$div(
          style = "font-weight:700; color:#2C3E50; margin-bottom:6px;",
          "Traits evaluated on a subset of environments"
        ),
        tags$ul(
          style = "margin-bottom:0; padding-left:18px; color:#2C3E50;",
          lapply(res$env_notes, function(n) tags$li(style = "margin-bottom:4px;", n))
        )
      )
    })


    # --- Global Options Summary Table ---
    output$tppOptionsIntro <- renderUI({
      tags$p(
        style = "color:#2C3E50; line-height:1.55; margin-bottom:12px;",
        "This table records the", tags$b("actual selection criteria you chose"),
        "for Bioflow\u0027s initial selection \u2014 not what the TPP asks for, but the",
        "configuration that was applied when the selection was run. For each trait it",
        "shows the index weight it carried, the threshold value and direction taken",
        "from its trait rule card, and the reference check(s) used where the rule was",
        "defined relative to a check. Traits with a weight of 0 did not contribute to",
        "the selection index, and traits showing \"no threshold\" were not filtered on",
        "a cut-off. Use this table to document, or reproduce, the decisions behind the",
        "initial selection."
      )
    })

    # Builds the Global Options Summary data.frame. Kept as a reactive so the
    # Output-tab table and the downloadable report render exactly the same data.
    global_options_df <- reactive({
      # The Global Options table on the Output tab shows the configuration that
      # was ACTUALLY USED when the selection ran, so it always reads from the
      # stored modeling table rather than from live UI controls which may have
      # changed since. The live path (trait_rules_input) is only used as a
      # fallback when no modeling-table data exists yet (e.g. before a run).
      trait_rules <- list()
      weights <- numeric(0)

      dt_fb <- tryCatch(data(), error = function(e) NULL)
      init_stamp <- tryCatch(input$reportInitialSelectionStamp, error = function(e) NULL)

      if (!is.null(dt_fb) && !is.null(dt_fb$modeling) &&
          !is.null(init_stamp) && length(init_stamp) == 1 && nzchar(init_stamp)) {
        m <- dt_fb$modeling
        idx <- which(
          as.character(m$analysisId) == as.character(init_stamp) &
            m$module == "Init_prodAdv"
        )
          mi <- m[idx, , drop = FALSE]

          if (nrow(mi) > 0) {
            excluded <- mi$trait[mi$parameter == "user_excluded_trait"]
            traits_fb <- unique(mi$trait[!is.na(mi$trait) & nzchar(mi$trait)])
            traits_fb <- setdiff(traits_fb, excluded)

            get_param <- function(tr, param) {
              v <- mi$value[!is.na(mi$trait) & mi$trait == tr &
                            !is.na(mi$parameter) & mi$parameter == param]
              v <- v[!is.na(v)]
              if (length(v) == 0) NULL else v[1]
            }
            as_num <- function(x) {
              if (is.null(x)) return(NULL)
              n <- suppressWarnings(as.numeric(x))
              if (is.na(n)) NULL else n
            }

            trait_rules <- stats::setNames(lapply(traits_fb, function(tr) {
              rt <- get_param(tr, "trait_rule_type")
              list(
                trait = tr,
                ruleType = if (is.null(rt)) "None" else rt,
                direction = get_param(tr, "direction"),
                threshold = as_num(get_param(tr, "threshold")),
                minValue = as_num(get_param(tr, "min_value")),
                maxValue = as_num(get_param(tr, "max_value")),
                referenceCheck = {
                  rc <- get_param(tr, "reference_check")
                  if (is.null(rc)) NULL else trimws(strsplit(rc, ",", fixed = TRUE)[[1]])
                }
              )
            }), traits_fb)

            if (length(weights) == 0) {
              weights <- vapply(traits_fb, function(tr) {
                w <- as_num(get_param(tr, "index_weight"))
                if (is.null(w)) 0 else w
              }, numeric(1))
              names(weights) <- traits_fb
            }
          }
      }

      if (length(trait_rules) == 0) {
        return(NULL)
      }

      # Get predictions for computing reference values.
      # input$mtaStamp is empty when the Input steps were never visited, so fall
      # back to the mta_stamp recorded for the selected initial stamp.
      dt <- tryCatch(data(), error = function(e) NULL)
      mta_stamp_used <- input$mtaStamp
      if ((is.null(mta_stamp_used) || !nzchar(paste0(mta_stamp_used))) && !is.null(dt)) {
        init_stamp <- tryCatch(input$reportInitialSelectionStamp, error = function(e) NULL)
        if (!is.null(init_stamp) && length(init_stamp) == 1 && nzchar(init_stamp)) {
          mrow <- which(
            as.character(dt$modeling$analysisId) == as.character(init_stamp) &
              dt$modeling$module == "Init_prodAdv" &
              dt$modeling$parameter == "mta_stamp"
          )
          if (length(mrow) > 0) mta_stamp_used <- dt$modeling$value[mrow[1]]
        }
      }

      dtPred <- NULL
      if (!is.null(dt) && !is.null(mta_stamp_used) && nzchar(paste0(mta_stamp_used))) {
        dtPred <- dt$predictions
        dtPred <- dtPred[dtPred$analysisId %in% mta_stamp_used &
                         dtPred$effectType == "designation", , drop = FALSE]
      }

      # Build trait_configs list for tpp_build_global_options_table
      trait_configs <- lapply(names(trait_rules), function(trait_name) {
        rule <- trait_rules[[trait_name]]
        if (is.null(rule)) return(NULL)

        weight_val <- if (trait_name %in% names(weights)) weights[[trait_name]] else 0

        # Determine threshold value and direction
        threshold_value <- NA_real_
        threshold_direction <- NA_character_
        reference_checks <- NULL
        reference_value <- NA_real_

        if (!is.null(rule$ruleType)) {
          if (identical(rule$ruleType, "Threshold") && !is.null(rule$threshold)) {
            threshold_value <- rule$threshold
            if (!is.null(rule$direction)) {
              threshold_direction <- if (identical(rule$direction, "Higher is better")) "greater" else "less"
            }
          } else if (identical(rule$ruleType, "% over check") && !is.null(rule$referenceCheck)) {
            threshold_value <- if (!is.null(rule$threshold)) rule$threshold else NA_real_
            threshold_direction <- "greater"
            reference_checks <- rule$referenceCheck

            # Compute reference value from predictions
            if (!is.null(dtPred) && nrow(dtPred) > 0 && !is.null(reference_checks)) {
              ref_vals <- dtPred$predictedValue[
                dtPred$trait == trait_name &
                  dtPred$designation %in% reference_checks &
                  !is.na(dtPred$predictedValue)
              ]
              if (length(ref_vals) > 0) {
                reference_value <- mean(ref_vals)
              }
            }
          } else if (identical(rule$ruleType, "Acceptable range")) {
            threshold_value <- NA_real_
            threshold_direction <- NA_character_
          }
        }

        # Also check for TPP-based check references
        filtered <- tpp_filtered_traits()
        if (!is.null(filtered) && is.data.frame(filtered) && nrow(filtered) > 0 &&
            "pheno_trait" %in% colnames(filtered) && "checks" %in% colnames(filtered)) {
          trait_row <- filtered[filtered$pheno_trait == trait_name, , drop = FALSE]
          if (nrow(trait_row) > 0 && !is.null(trait_row$checks[[1]])) {
            safe_trait <- sanitize_trait_id(trait_name)
            tpp_checks_selected <- tryCatch(
              input[[paste0("tppChecks_", safe_trait)]],
              error = function(e) NULL
            )
            if (!is.null(tpp_checks_selected) && length(tpp_checks_selected) > 0) {
              reference_checks <- tpp_checks_selected
              # Compute reference value for TPP checks
              if (!is.null(dtPred) && nrow(dtPred) > 0) {
                ref_vals <- dtPred$predictedValue[
                  dtPred$trait == trait_name &
                    dtPred$designation %in% reference_checks &
                    !is.na(dtPred$predictedValue)
                ]
                if (length(ref_vals) > 0) {
                  reference_value <- mean(ref_vals)
                }
              }
            }
          }
        }

        list(
          trait_name = tpp_display_name(trait_name),
          weight = weight_val,
          threshold_value = threshold_value,
          threshold_direction = threshold_direction,
          reference_checks = reference_checks,
          reference_value = reference_value
        )
      })

      # Remove NULLs
      trait_configs <- trait_configs[!vapply(trait_configs, is.null, logical(1))]

      options_df <- tpp_build_global_options_table(trait_configs)

      if (nrow(options_df) == 0) {
        return(NULL)
      }

      # Make an absent reference check explicit rather than leaving a blank cell
      if ("Reference_Check" %in% colnames(options_df)) {
        rc <- as.character(options_df$Reference_Check)
        rc[is.na(rc) | !nzchar(trimws(rc)) | trimws(rc) %in% c("-", "NA", "none")] <-
          "none used for initial selection"
        options_df$Reference_Check <- rc
      }

      # Round numeric columns to 3 decimal places
      for (col in colnames(options_df)) {
        vals <- options_df[[col]]
        if (is.numeric(vals)) {
          options_df[[col]] <- round(vals, 3)
        } else {
          # Try to detect numeric-looking character columns (e.g. Threshold_Value)
          num_vals <- suppressWarnings(as.numeric(vals))
          if (!all(is.na(num_vals)) && sum(!is.na(num_vals)) > 0) {
            options_df[[col]] <- ifelse(
              is.na(num_vals),
              vals,
              format(round(num_vals, 3), nsmall = 3)
            )
          }
        }
      }

      options_df
    })

    output$tppDashboardOptionsTable <- DT::renderDT({
      options_df <- global_options_df()

      if (is.null(options_df) || nrow(options_df) == 0) {
        empty_df <- data.frame(Message = "No options configured yet",
                               stringsAsFactors = FALSE)
        return(DT::datatable(empty_df, options = list(dom = "t", paging = FALSE),
                             rownames = FALSE))
      }

      DT::datatable(options_df, options = list(dom = "t", paging = FALSE,
                                               scrollX = TRUE, ordering = FALSE),
                    rownames = FALSE, caption = "Global Options Summary")
    })

    # Everything the Rmd needs beyond the decision table, so the downloaded
    # dashboard matches the Output tab section for section. Computed here rather
    # than in the template because the reactives live in this module.
    report_extra_payload <- function() {
      list(
        review_long      = tryCatch(final_review_long(),   error = function(e) NULL),
        breakdown_detail = tryCatch(tpp_breakdown_detail(), error = function(e) NULL),
        options_table    = tryCatch(global_options_df(),   error = function(e) NULL)
      )
    }


  })
}

## To be copied in the UI
# mod_qaRawApp_ui("qaPhenoApp_1")

## To be copied in the server
# mod_qaRawApp_server("qaPhenoApp_1")

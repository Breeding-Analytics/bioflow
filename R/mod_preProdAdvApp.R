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
  "NOT SELECTED" = "circle-open",
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
# GxE model detection
# --------------------------------------------------------------------------

#' Detect the GxE model type used in an MTA analysis
#'
#' Uses a two-source approach: first checks the predictions table for
#' unambiguous GxE indicators (fw_slope, GenCorrMat), then cross-references the
#' modeling table to confirm that GxE interaction terms were actually fitted.
#' This prevents false positives from main-effects models that still store
#' per-environment BLUPs in the predictions table.
#'
#' @param predictions A data.frame with at least columns \code{analysisId}
#'   and \code{effectType}.
#' @param mta_stamp Character scalar — the analysisId to filter on.
#' @param modeling Optional data.frame (the modeling table) with columns:
#'   analysisId, parameter, value. When provided, used to verify that a GxE
#'   interaction was actually fitted (avoids false "cs_diag" detection).
#'
#' @return Character scalar: one of \code{"fw"}, \code{"fa"},
#'   \code{"cs_diag"}, or \code{"none"}.
#'
#' @details
#' Detection priority:
#' \enumerate{
#'   \item If \code{"fw_slope"} is present among effectTypes → \code{"fw"}
#'   \item Else if \code{"GenCorrMat"} is present → \code{"fa"}
#'   \item Else if the modeling table confirms GxE interaction was fitted
#'         (kernels contain "environment" or randomFormula references
#'         environment interaction) AND per-environment effectTypes exist →
#'         \code{"cs_diag"}
#'   \item Otherwise → \code{"none"}
#' }
#'
#' @noRd
detect_gxe_model <- function(predictions, mta_stamp, modeling = NULL) {
  mta_preds <- predictions[predictions$analysisId == mta_stamp, ]
  effect_types <- unique(mta_preds$effectType)

  # Priority 1: FW model — unambiguous indicator

  if ("fw_slope" %in% effect_types) return("fw")

  # Priority 2: FA model — unambiguous indicator
  if ("GenCorrMat" %in% effect_types) return("fa")

  # Priority 3: CS/Diagonal — requires confirmation from modeling table
  # Per-environment effectTypes exist (not just "designation")
  gxe_types <- setdiff(effect_types, c("designation", "fw_slope", "GenCorrMat"))

  if (length(gxe_types) > 0) {
    # If modeling table is provided, verify GxE was actually fitted
    if (!is.null(modeling) && nrow(modeling) > 0) {
      mta_modeling <- modeling[modeling$analysisId == mta_stamp, , drop = FALSE]

      # Check 1: kernels parameter contains "environment" (e.g., "designation:environment")
      kernels_rows <- mta_modeling[mta_modeling$parameter == "kernels", , drop = FALSE]
      has_gxe_kernel <- any(grepl("environment", kernels_rows$value, ignore.case = TRUE))

      # Check 2: randomFormula contains "environment" interaction term
      formula_rows <- mta_modeling[mta_modeling$parameter == "randomFormula", , drop = FALSE]
      has_gxe_formula <- any(grepl("environment", formula_rows$value, ignore.case = TRUE))

      if (has_gxe_kernel || has_gxe_formula) {
        return("cs_diag")
      } else {
        # Main-effects model with per-environment predictions — NOT a GxE model
        return("none")
      }
    } else {
      # No modeling table available — fall back to original behavior (assume GxE)
      return("cs_diag")
    }
  }

  return("none")
}

# --------------------------------------------------------------------------
# FW data extraction
# --------------------------------------------------------------------------

#' Extract Finlay-Wilkinson data for the stability/adaptability plot
#'
#' Pure function that retrieves mean performance and FW regression slope from
#' MTA predictions, merges with selection status from review_df, applies any
#' user overrides, and computes visual encoding properties (color, opacity,
#' shape) for each designation.
#'
#' @param predictions A data.frame with columns: analysisId, designation,
#'   trait, effectType, predictedValue, reliability.
#' @param review_df A data.frame with columns: designation, plot_status.
#' @param mta_stamp Character scalar — the analysisId to filter on.
#' @param trait Character scalar — the trait to filter on.
#' @param overrides A data.frame with columns: designation, plot_decision
#'   (or NULL if no overrides).
#'
#' @return A data.frame with one row per designation containing columns:
#'   designation, mean_value, fw_slope, plot_status, opacity, color, shape.
#'
#' @noRd
extract_fw_data <- function(predictions, review_df, mta_stamp, trait, overrides = NULL) {
  # Filter predictions to matching analysisId and trait

  preds <- predictions[predictions$analysisId == mta_stamp & predictions$trait == trait, , drop = FALSE]

  # Extract mean_value from effectType == "designation"
  means <- preds[preds$effectType == "designation", , drop = FALSE]
  means <- means[, c("designation", "predictedValue", "reliability"), drop = FALSE]
  names(means)[names(means) == "predictedValue"] <- "mean_value"
  names(means)[names(means) == "reliability"] <- "mean_reliability"

  # Extract fw_slope from effectType == "fw_slope"
  slopes <- preds[preds$effectType == "fw_slope", , drop = FALSE]
  slopes <- slopes[, c("designation", "predictedValue", "reliability"), drop = FALSE]
  names(slopes)[names(slopes) == "predictedValue"] <- "fw_slope"
  names(slopes)[names(slopes) == "reliability"] <- "slope_reliability"

  # Merge means and slopes by designation (inner join — only keep designations with both)
  df <- merge(means, slopes, by = "designation")

  if (nrow(df) == 0) return(df)

  # Use mean_reliability as the reliability for opacity computation
  df$reliability <- df$mean_reliability
  df$mean_reliability <- NULL
  df$slope_reliability <- NULL

  # Left-join with review_df on designation
  review_cols <- review_df[, c("designation", "plot_status"), drop = FALSE]
  df <- merge(df, review_cols, by = "designation", all.x = TRUE)

  # Assign "NOT SELECTED" to designations absent from review_df
  df$plot_status[is.na(df$plot_status)] <- "NOT SELECTED"

  # Apply overrides (if present) to update plot_status
  if (!is.null(overrides) && nrow(overrides) > 0) {
    override_match <- match(df$designation, overrides$designation)
    has_override <- !is.na(override_match)
    df$plot_status[has_override] <- overrides$plot_decision[override_match[has_override]]
  }

  # Compute opacity: NA reliability → minimum opacity (0.15)
  rel_for_opacity <- df$reliability
  rel_for_opacity[is.na(rel_for_opacity)] <- 0
  df$opacity <- reliability_to_opacity(rel_for_opacity)

  # CHECK designations always get opacity = 1.0
  df$opacity[df$plot_status == "CHECK"] <- 1.0

  # Assign color from STATUS_COLORS
  df$color <- STATUS_COLORS[df$plot_status]

  # Assign shape from STATUS_SHAPES
  df$shape <- STATUS_SHAPES[df$plot_status]

  # Return only required columns, one row per designation
  df[, c("designation", "mean_value", "fw_slope", "plot_status", "opacity", "color", "shape"), drop = FALSE]
}

# --------------------------------------------------------------------------
# CS/Diagonal GxE stability computation
# --------------------------------------------------------------------------

#' Compute GxE stability metrics for CS/Diagonal models
#'
#' For Compound Symmetry or Diagonal models, extracts per-environment GxE
#' predictions, computes the coefficient of variation (CV) and variance per
#' genotype, and merges with review status / overrides to produce a
#' plot-ready data frame.
#'
#' @param predictions Data.frame with columns: analysisId, designation, trait,
#'   environment, effectType, predictedValue, stdError, reliability.
#' @param review_df Data.frame with columns: designation, plot_status.
#' @param mta_stamp Character scalar — analysisId to filter on.
#' @param trait Character scalar — trait name to filter on.
#' @param overrides Data.frame with columns: designation, plot_status
#'   (or NULL if no overrides).
#'
#' @return A data.frame with one row per qualifying genotype and columns:
#'   designation, mean_value, cv, env_variance, n_environments, plot_status,
#'   opacity, color, shape.
#'
#' @details
#' - Filters predictions on \code{analysisId == mta_stamp} AND
#'   \code{trait == trait}.
#' - Extracts per-environment rows (effectType NOT in
#'   \{"designation", "fw_slope", "GenCorrMat"\}).
#' - Computes per-genotype: mean, sd, CV (sd/mean), variance, and count of
#'   environments.
#' - Excludes genotypes with fewer than 2 environments.
#' - Excludes genotypes with mean == 0 (division by zero protection).
#' - Left-joins with \code{review_df}; unmatched designations get
#'   "NOT SELECTED".
#' - Applies overrides (if present) before computing visual properties.
#' - Computes color, opacity, and shape from the effective plot_status.
#'
#' @noRd
compute_gxe_stability <- function(predictions, review_df, mta_stamp, trait, overrides = NULL) {
  # Step 1: Filter to MTA stamp and trait
  mta_preds <- predictions[predictions$analysisId == mta_stamp &
                             predictions$trait == trait, , drop = FALSE]

  # Step 2: Keep only per-environment predictions
  #   (effectType NOT in {"designation", "fw_slope", "GenCorrMat"})
  excluded_types <- c("designation", "fw_slope", "GenCorrMat")
  env_preds <- mta_preds[!(mta_preds$effectType %in% excluded_types), , drop = FALSE]

  # Early return if no per-environment data
  if (nrow(env_preds) == 0) {
    return(data.frame(
      designation    = character(0),
      mean_value     = numeric(0),
      cv             = numeric(0),
      env_variance   = numeric(0),
      n_environments = integer(0),
      plot_status    = character(0),
      opacity        = numeric(0),
      color          = character(0),
      shape          = character(0),
      stringsAsFactors = FALSE
    ))
  }

  # Step 3: Aggregate per genotype — mean, sd, var, n_environments
  agg_mean <- aggregate(predictedValue ~ designation, data = env_preds, FUN = mean)
  names(agg_mean)[2] <- "mean_value"

  agg_sd <- aggregate(predictedValue ~ designation, data = env_preds, FUN = sd)
  names(agg_sd)[2] <- "sd_value"

  agg_var <- aggregate(predictedValue ~ designation, data = env_preds, FUN = var)
  names(agg_var)[2] <- "env_variance"

  agg_n <- aggregate(predictedValue ~ designation, data = env_preds, FUN = length)
  names(agg_n)[2] <- "n_environments"

  # Merge aggregates
  result <- merge(agg_mean, agg_sd, by = "designation")
  result <- merge(result, agg_var, by = "designation")
  result <- merge(result, agg_n, by = "designation")

  # Step 4: Exclude genotypes with fewer than 2 environments
  result <- result[result$n_environments >= 2, , drop = FALSE]

  # Step 5: Exclude genotypes with mean == 0 (division by zero protection)
  result <- result[result$mean_value != 0, , drop = FALSE]

  # Early return if no genotypes remain
  if (nrow(result) == 0) {
    return(data.frame(
      designation    = character(0),
      mean_value     = numeric(0),
      cv             = numeric(0),
      env_variance   = numeric(0),
      n_environments = integer(0),
      plot_status    = character(0),
      opacity        = numeric(0),
      color          = character(0),
      shape          = character(0),
      stringsAsFactors = FALSE
    ))
  }

  # Step 6: Compute CV = sd / mean
  result$cv <- result$sd_value / result$mean_value

  # Remove temporary sd column
  result$sd_value <- NULL

  # Step 7: Left-join with review_df on designation
  if (!is.null(review_df) && nrow(review_df) > 0) {
    result <- merge(result, review_df[, c("designation", "plot_status"), drop = FALSE],
                    by = "designation", all.x = TRUE)
  } else {
    result$plot_status <- NA_character_
  }

  # Assign "NOT SELECTED" to unmatched designations
  result$plot_status[is.na(result$plot_status)] <- "NOT SELECTED"

  # Step 8: Apply overrides (if present)
  if (!is.null(overrides) && nrow(overrides) > 0) {
    override_match <- match(result$designation, overrides$designation)
    has_override <- !is.na(override_match)
    if (any(has_override)) {
      result$plot_status[has_override] <- overrides$plot_status[override_match[has_override]]
    }
  }

  # Step 9: Compute visual properties from effective plot_status
  # Get mean reliability per genotype for opacity computation
  rel_agg <- aggregate(reliability ~ designation, data = env_preds, FUN = function(x) mean(x, na.rm = TRUE))
  names(rel_agg)[2] <- "reliability"
  result <- merge(result, rel_agg, by = "designation", all.x = TRUE)

  # Opacity: reliability_to_opacity() for non-CHECK, 1.0 for CHECK
  result$opacity <- reliability_to_opacity(result$reliability)
  result$opacity[result$plot_status == "CHECK"] <- 1.0

  # Color from status mapping
  result$color <- STATUS_COLORS[result$plot_status]

  # Shape from status mapping
  result$shape <- STATUS_SHAPES[result$plot_status]

  # Remove temporary reliability column
  result$reliability <- NULL

  # Step 10: Return final data.frame with required columns in order
  result[, c("designation", "mean_value", "cv", "env_variance",
             "n_environments", "plot_status", "opacity", "color", "shape"),
         drop = FALSE]
}

# --------------------------------------------------------------------------
# Factor Analytic data extraction
# --------------------------------------------------------------------------

#' Extract Factor Analytic biplot data from MTA predictions
#'
#' Pure function that extracts the genetic correlation matrix from GenCorrMat
#' predictions, performs eigen-decomposition to derive environment loadings,
#' projects per-environment genotype BLUPs onto the first two eigenvectors to
#' compute genotype scores, and merges with selection status.
#'
#' Falls back to \code{compute_gxe_stability()} when the GenCorrMat contains
#' fewer than 3 environments.
#'
#' @param predictions A data.frame with columns: analysisId, designation,
#'   trait, environment, effectType, predictedValue, reliability.
#' @param review_df A data.frame with columns: designation, plot_status.
#' @param mta_stamp Character scalar — the analysisId to filter on.
#' @param trait Character scalar — the trait to filter on.
#' @param overrides A data.frame with columns: designation, plot_status
#'   (or NULL if no overrides).
#'
#' @return A list with components:
#'   \describe{
#'     \item{genotype_scores}{data.frame with columns: designation, PC1, PC2,
#'       mean_value, plot_status, color, shape, opacity}
#'     \item{env_loadings}{data.frame with columns: environment, PC1_loading,
#'       PC2_loading}
#'     \item{variance_explained}{numeric vector of length 2 — proportion of
#'       variance explained by PC1 and PC2}
#'     \item{fallback}{logical — TRUE if fell back to compute_gxe_stability()}
#'     \item{fallback_data}{data.frame or NULL — the CS/Diag stability data
#'       if fallback was triggered}
#'   }
#'
#' @noRd
extract_fa_data <- function(predictions, review_df, mta_stamp, trait, overrides = NULL) {
  # Step 1: Filter predictions to matching analysisId and trait
  preds <- predictions[predictions$analysisId == mta_stamp &
                         predictions$trait == trait, , drop = FALSE]

  # Step 2: Extract GenCorrMat rows
  gencorr_rows <- preds[preds$effectType == "GenCorrMat", , drop = FALSE]

  # In GenCorrMat rows, 'designation' and 'environment' columns encode the

  # row/column of the correlation matrix and 'predictedValue' holds the value
  envs_from_designation <- unique(gencorr_rows$designation)
  envs_from_environment <- unique(gencorr_rows$environment)
  all_envs <- unique(c(envs_from_designation, envs_from_environment))
  n_envs <- length(all_envs)

  # Step 3: Fallback — fewer than 3 environments → use CS/Diag stability
  if (n_envs < 3) {
    fallback_result <- compute_gxe_stability(predictions, review_df, mta_stamp, trait, overrides)
    return(list(
      genotype_scores = NULL,
      env_loadings = NULL,
      variance_explained = NULL,
      fallback = TRUE,
      fallback_data = fallback_result
    ))
  }

  # Step 4: Reconstruct the correlation matrix
  corr_mat <- matrix(0, nrow = n_envs, ncol = n_envs,
                     dimnames = list(all_envs, all_envs))

  for (i in seq_len(nrow(gencorr_rows))) {
    row_env <- gencorr_rows$designation[i]
    col_env <- gencorr_rows$environment[i]
    val <- gencorr_rows$predictedValue[i]
    corr_mat[row_env, col_env] <- val
    corr_mat[col_env, row_env] <- val
  }

  # Ensure diagonal is 1 (correlation matrix property)
  diag(corr_mat) <- 1

  # Step 5: Eigen-decomposition
  eig <- eigen(corr_mat, symmetric = TRUE)
  eigenvalues <- eig$values

  # First 2 eigenvectors → environment loadings
  loadings_mat <- eig$vectors[, 1:2, drop = FALSE]

  # Variance explained by first 2 PCs
  total_var <- sum(pmax(eigenvalues, 0))
  variance_explained <- pmax(eigenvalues[1:2], 0) / total_var

  # Step 6: Build environment loadings data.frame
  env_loadings <- data.frame(
    environment = all_envs,
    PC1_loading = loadings_mat[, 1],
    PC2_loading = loadings_mat[, 2],
    stringsAsFactors = FALSE
  )

  # Step 7: Get per-environment genotype BLUPs
  non_env_types <- c("designation", "fw_slope", "GenCorrMat")
  env_preds <- preds[!(preds$effectType %in% non_env_types), , drop = FALSE]

  if (nrow(env_preds) == 0) {
    # No per-environment BLUPs → return empty genotype scores
    return(list(
      genotype_scores = data.frame(
        designation = character(0), PC1 = numeric(0), PC2 = numeric(0),
        mean_value = numeric(0), plot_status = character(0),
        color = character(0), shape = character(0), opacity = numeric(0),
        stringsAsFactors = FALSE
      ),
      env_loadings = env_loadings,
      variance_explained = variance_explained,
      fallback = FALSE,
      fallback_data = NULL
    ))
  }

  # Only use environments present in the correlation matrix
  env_preds_filtered <- env_preds[env_preds$environment %in% all_envs, , drop = FALSE]
  genotypes <- unique(env_preds_filtered$designation)

  # Step 8: Build genotype-by-environment BLUP matrix
  blup_mat <- matrix(0, nrow = length(genotypes), ncol = n_envs,
                     dimnames = list(genotypes, all_envs))

  for (i in seq_len(nrow(env_preds_filtered))) {
    g <- env_preds_filtered$designation[i]
    e <- env_preds_filtered$environment[i]
    blup_mat[g, e] <- env_preds_filtered$predictedValue[i]
  }

  # Step 9: Project BLUPs onto eigenvectors → genotype scores
  scores_mat <- blup_mat %*% loadings_mat

  # Step 10: Build genotype_scores data.frame
  genotype_scores <- data.frame(
    designation = genotypes,
    PC1 = scores_mat[, 1],
    PC2 = scores_mat[, 2],
    stringsAsFactors = FALSE
  )

  # Merge mean_value from "designation" effectType predictions
  overall_preds <- preds[preds$effectType == "designation", , drop = FALSE]
  if (nrow(overall_preds) > 0) {
    mean_df <- overall_preds[, c("designation", "predictedValue", "reliability"), drop = FALSE]
    names(mean_df)[names(mean_df) == "predictedValue"] <- "mean_value"
    genotype_scores <- merge(genotype_scores, mean_df, by = "designation", all.x = TRUE)
  } else {
    genotype_scores$mean_value <- rowMeans(blup_mat[genotype_scores$designation, , drop = FALSE], na.rm = TRUE)
    genotype_scores$reliability <- NA_real_
  }

  # Step 11: Left-join with review_df on designation
  if (!is.null(review_df) && nrow(review_df) > 0) {
    review_cols <- review_df[, c("designation", "plot_status"), drop = FALSE]
    genotype_scores <- merge(genotype_scores, review_cols, by = "designation", all.x = TRUE)
  } else {
    genotype_scores$plot_status <- NA_character_
  }

  # Assign "NOT SELECTED" to designations absent from review_df
  genotype_scores$plot_status[is.na(genotype_scores$plot_status)] <- "NOT SELECTED"

  # Step 12: Apply overrides (if present)
  if (!is.null(overrides) && nrow(overrides) > 0) {
    override_match <- match(genotype_scores$designation, overrides$designation)
    has_override <- !is.na(override_match)
    if (any(has_override)) {
      genotype_scores$plot_status[has_override] <- overrides$plot_status[override_match[has_override]]
    }
  }

  # Step 13: Compute visual properties
  rel_for_opacity <- genotype_scores$reliability
  rel_for_opacity[is.na(rel_for_opacity)] <- 0
  genotype_scores$opacity <- reliability_to_opacity(rel_for_opacity)

  # CHECK designations always get opacity = 1.0
  genotype_scores$opacity[genotype_scores$plot_status == "CHECK"] <- 1.0

  # Assign color from STATUS_COLORS
  genotype_scores$color <- STATUS_COLORS[genotype_scores$plot_status]

  # Assign shape from STATUS_SHAPES
  genotype_scores$shape <- STATUS_SHAPES[genotype_scores$plot_status]

  # Remove temporary reliability column
  genotype_scores$reliability <- NULL

  # Step 14: Return final structure
  genotype_scores <- genotype_scores[, c("designation", "PC1", "PC2", "mean_value",
                                          "plot_status", "color", "shape", "opacity"),
                                      drop = FALSE]

  list(
    genotype_scores = genotype_scores,
    env_loadings = env_loadings,
    variance_explained = variance_explained,
    fallback = FALSE,
    fallback_data = NULL
  )
}

# --------------------------------------------------------------------------
# Prescriptive annotation layer
# --------------------------------------------------------------------------

#' Annotate stable high-performers and adaptive responders on a stability plot
#'
#' Applies the prescriptive highlighting layer to any of the three GxE plot
#' types. Computes classification thresholds, identifies qualifying genotypes,
#' and adds gold border rings, enlarged markers, and text annotations.
#'
#' @param plot A plotly object (the base scatter/biplot).
#' @param plot_data A data.frame with stability metrics. Expected columns vary
#'   by model: FW requires designation, mean_value, fw_slope, plot_status;
#'   cs_diag requires designation, mean_value, cv, plot_status; FA requires
#'   designation, PC1, PC2, mean_value, plot_status.
#' @param model_type Character: one of "fw", "cs_diag", or "fa".
#' @param env_predictions Optional data.frame with columns designation,
#'   environment, predictedValue. Used for identifying best environments
#'   for adaptive responders. If NULL, adaptive annotations omit "best in".
#'
#' @return A plotly object with prescriptive annotations added.
#'
#' @noRd
annotate_stable_and_adaptive <- function(plot, plot_data, model_type,
                                          env_predictions = NULL) {
  # --- Step 1: Compute Stability_Metric per model type ---
  if (model_type == "fw") {
    plot_data$stability_metric <- abs(plot_data$fw_slope - 1)
  } else if (model_type == "cs_diag") {
    plot_data$stability_metric <- plot_data$cv
  } else if (model_type == "fa") {
    plot_data$stability_metric <- sqrt(plot_data$PC1^2 + plot_data$PC2^2)
  } else {
    return(plot)
  }

  # --- Step 2: Check for degenerate case — all metrics within 1e-9 ---
  finite_metrics <- plot_data$stability_metric[is.finite(plot_data$stability_metric)]
  if (length(finite_metrics) < 2) return(plot)

  metric_range <- max(finite_metrics) - min(finite_metrics)
  if (metric_range <= 1e-9) return(plot)

  # --- Step 3: Compute thresholds ---
  q25 <- quantile(finite_metrics, 0.25, na.rm = TRUE, names = FALSE)
  q75 <- quantile(finite_metrics, 0.75, na.rm = TRUE, names = FALSE)
  median_mean <- median(plot_data$mean_value, na.rm = TRUE)


  # Check_Mean: mean of mean_value where plot_status == "CHECK"
  check_rows <- plot_data[plot_data$plot_status == "CHECK", , drop = FALSE]
  if (nrow(check_rows) > 0) {
    check_mean <- mean(check_rows$mean_value, na.rm = TRUE)
  } else {
    # Fallback: 75th percentile of mean_value
    check_mean <- quantile(plot_data$mean_value, 0.75, na.rm = TRUE, names = FALSE)
  }

  # --- Step 4: Classify genotypes ---
  plot_data$is_stable <- (plot_data$stability_metric <= q25) &
    (plot_data$mean_value > check_mean) &
    is.finite(plot_data$stability_metric) &
    is.finite(plot_data$mean_value)

  plot_data$is_adaptive <- (plot_data$stability_metric > q75) &
    (plot_data$mean_value > median_mean) &
    is.finite(plot_data$stability_metric) &
    is.finite(plot_data$mean_value)

  stable_genotypes <- plot_data$designation[plot_data$is_stable]
  adaptive_genotypes <- plot_data$designation[plot_data$is_adaptive]

  n_stable <- length(stable_genotypes)
  n_adaptive <- length(adaptive_genotypes)

  # --- Step 5: Identify best environments for adaptive genotypes ---
  adaptive_best_envs <- list()
  if (n_adaptive > 0 && !is.null(env_predictions) && nrow(env_predictions) > 0) {
    for (geno in adaptive_genotypes) {
      geno_envs <- env_predictions[env_predictions$designation == geno, , drop = FALSE]
      if (nrow(geno_envs) < 2) {
        adaptive_best_envs[[geno]] <- character(0)
        next
      }
      geno_mean <- mean(geno_envs$predictedValue, na.rm = TRUE)
      geno_sd <- sd(geno_envs$predictedValue, na.rm = TRUE)
      if (!is.finite(geno_sd) || geno_sd == 0) {
        adaptive_best_envs[[geno]] <- character(0)
        next
      }
      threshold <- geno_mean + geno_sd
      best <- geno_envs$environment[geno_envs$predictedValue > threshold]
      adaptive_best_envs[[geno]] <- as.character(best)
    }
  }

  # --- Step 6: Apply visual modifications via plotly_build ---
  p_built <- plotly::plotly_build(plot)

  # Find marker indices per designation in the trace data
  # We iterate through traces and modify marker properties

  for (trace_idx in seq_along(p_built$x$data)) {
    trace <- p_built$x$data[[trace_idx]]
    if (is.null(trace$text) && is.null(trace$customdata)) next

    # Try to get designation info from customdata or text
    trace_desigs <- NULL
    if (!is.null(trace$customdata)) {
      # customdata may be a list or matrix
      if (is.list(trace$customdata)) {
        trace_desigs <- vapply(trace$customdata, function(x) {
          if (is.character(x)) x[1] else as.character(x[1])
        }, character(1))
      } else if (is.matrix(trace$customdata) || is.data.frame(trace$customdata)) {
        trace_desigs <- as.character(trace$customdata[, 1])
      } else if (is.character(trace$customdata)) {
        trace_desigs <- trace$customdata
      }
    }

    if (is.null(trace_desigs) || length(trace_desigs) == 0) next

    n_points <- length(trace_desigs)

    # --- Gold border for stable markers ---
    stable_mask <- trace_desigs %in% stable_genotypes
    if (any(stable_mask)) {
      # Initialize marker.line if not present
      if (is.null(p_built$x$data[[trace_idx]]$marker$line)) {
        p_built$x$data[[trace_idx]]$marker$line <- list(
          width = rep(0, n_points),
          color = rep("rgba(0,0,0,0)", n_points)
        )
      }
      # Ensure line properties are per-point vectors
      current_width <- p_built$x$data[[trace_idx]]$marker$line$width
      current_color <- p_built$x$data[[trace_idx]]$marker$line$color

      if (length(current_width) == 1) {
        current_width <- rep(current_width, n_points)
      }
      if (length(current_color) == 1) {
        current_color <- rep(current_color, n_points)
      }

      current_width[stable_mask] <- 4
      current_color[stable_mask] <- "#FFD700"

      p_built$x$data[[trace_idx]]$marker$line$width <- current_width
      p_built$x$data[[trace_idx]]$marker$line$color <- current_color
    }

    # --- 14px size for adaptive markers ---
    adaptive_mask <- trace_desigs %in% adaptive_genotypes
    if (any(adaptive_mask)) {
      current_size <- p_built$x$data[[trace_idx]]$marker$size
      if (is.null(current_size)) {
        current_size <- rep(8, n_points)
      }
      if (length(current_size) == 1) {
        current_size <- rep(current_size, n_points)
      }
      current_size[adaptive_mask] <- 14
      p_built$x$data[[trace_idx]]$marker$size <- current_size
    }
  }

  # --- Step 7: Build text annotations ---
  annotations <- list()

  # Stable high-performers annotation
  if (n_stable > 0) {
    if (n_stable <= 5) {
      stable_text <- paste0("\u2605 Stable high-performers: ",
                            paste(stable_genotypes, collapse = ", "))
    } else {
      stable_text <- paste0("\u2605 Stable high-performers: ",
                            paste(stable_genotypes[1:5], collapse = ", "),
                            " \u2026 and ", n_stable - 5, " more")
    }
    if (n_stable == 1) {
      stable_text <- paste0(stable_text, " (limited data)")
    }

    annotations[[length(annotations) + 1]] <- list(
      text = paste0("<b>", stable_text, "</b>"),
      xref = "paper", yref = "paper",
      x = 0.02, y = 0.98,
      xanchor = "left", yanchor = "top",
      showarrow = FALSE,
      font = list(size = 11, color = "#333333")
    )
  }

  # Adaptive responders annotations
  if (n_adaptive > 0) {
    adaptive_texts <- character(0)
    for (geno in adaptive_genotypes) {
      best <- adaptive_best_envs[[geno]]
      if (!is.null(best) && length(best) > 0) {
        geno_text <- paste0("\u2197 Responsive: ", geno,
                            " (best in: ", paste(best, collapse = ", "), ")")
      } else if (!is.null(env_predictions) && nrow(env_predictions) > 0) {
        # env_predictions provided but no clear best environment
        geno_text <- paste0("\u2197 Responsive: ", geno,
                            " (no clear best environment)")
      } else {
        # No env_predictions at all — omit "best in"
        geno_text <- paste0("\u2197 Responsive: ", geno)
      }
      adaptive_texts <- c(adaptive_texts, geno_text)
    }

    # Combine all adaptive annotations into one block
    combined_adaptive_text <- paste(adaptive_texts, collapse = "<br>")

    if (n_adaptive == 1) {
      combined_adaptive_text <- paste0(combined_adaptive_text, " (limited data)")
    }

    annotations[[length(annotations) + 1]] <- list(
      text = paste0("<i>", combined_adaptive_text, "</i>"),
      xref = "paper", yref = "paper",
      x = 0.02, y = 0.02,
      xanchor = "left", yanchor = "bottom",
      showarrow = FALSE,
      font = list(size = 10, color = "#555555")
    )
  }

  # --- Step 8: Apply annotations to the built plot ---
  if (length(annotations) > 0) {
    existing_annotations <- p_built$x$layout$annotations
    if (is.null(existing_annotations)) {
      existing_annotations <- list()
    }
    p_built$x$layout$annotations <- c(existing_annotations, annotations)
  }

  # Convert back to plotly object
  p_built
}

# --------------------------------------------------------------------------
# Finlay-Wilkinson Reaction Norm Plot Builder
# --------------------------------------------------------------------------

#' Build Finlay-Wilkinson reaction norm scatter plot
#'
#' Creates a plotly scatter plot of mean_value vs. fw_slope with reference lines,
#' a shaded stable zone, and prescriptive annotations.
#'
#' @param fw_data A data.frame from \code{extract_fw_data()} with columns:
#'   designation, mean_value, fw_slope, plot_status, opacity, color, shape.
#' @param highlighted Character or NULL — designation to highlight with larger
#'   marker and white border. All other points are dimmed.
#' @param source_id Character — plotly source identifier for click event capture.
#' @param env_predictions Data.frame or NULL — per-environment predictions with
#'   columns designation, environment, predictedValue. Passed through to
#'   \code{annotate_stable_and_adaptive()} for best-environment identification.
#'
#' @return A plotly object.
#'
#' @noRd
build_fw_reaction_norm_plotly <- function(fw_data, highlighted = NULL,
                                          source_id, env_predictions = NULL) {

  # --- Compute reference thresholds ---
  check_rows <- fw_data[fw_data$plot_status == "CHECK", , drop = FALSE]
  if (nrow(check_rows) > 0) {
    check_mean <- mean(check_rows$mean_value, na.rm = TRUE)
  } else {
    check_mean <- quantile(fw_data$mean_value, 0.75, na.rm = TRUE, names = FALSE)
  }

  # --- Highlighting logic ---
  if (!is.null(highlighted)) {
    is_highlighted <- fw_data$designation == highlighted
    fw_data$display_opacity <- ifelse(is_highlighted, fw_data$opacity, fw_data$opacity * 0.3)
    fw_data$marker_size <- ifelse(is_highlighted, 14, 8)
    fw_data$line_color <- ifelse(is_highlighted, "white", "rgba(0,0,0,0)")
    fw_data$line_width <- ifelse(is_highlighted, 3, 0)
  } else {
    fw_data$display_opacity <- fw_data$opacity
    fw_data$marker_size <- 8
    fw_data$line_color <- rep("rgba(0,0,0,0)", nrow(fw_data))
    fw_data$line_width <- rep(0, nrow(fw_data))
  }

  # --- Classify for tooltip ---
  # Quick classification for display purposes
  finite_metrics <- abs(fw_data$fw_slope - 1)
  q25 <- quantile(finite_metrics[is.finite(finite_metrics)], 0.25, na.rm = TRUE, names = FALSE)
  q75 <- quantile(finite_metrics[is.finite(finite_metrics)], 0.75, na.rm = TRUE, names = FALSE)
  median_mean <- median(fw_data$mean_value, na.rm = TRUE)

  fw_data$classification <- vapply(seq_len(nrow(fw_data)), function(i) {
    metric <- abs(fw_data$fw_slope[i] - 1)
    mv <- fw_data$mean_value[i]
    if (!is.finite(metric) || !is.finite(mv)) return("none")
    if (metric <= q25 && mv > check_mean) return("stable")
    if (metric > q75 && mv > median_mean) return("adaptive")
    return("none")
  }, character(1))

  # --- Build tooltip ---
  fw_data$hover_text <- paste0(
    "<b>", fw_data$designation, "</b><br>",
    "Mean value: ", sprintf("%.3f", fw_data$mean_value), "<br>",
    "FW slope: ", sprintf("%.3f", fw_data$fw_slope), "<br>",
    "Classification: ", fw_data$classification, "<br>",
    "Status: ", fw_data$plot_status
  )

  # --- Build scatter plot ---
  p <- plotly::plot_ly(source = source_id)

  # Add one trace per status category for legend
  status_colors <- c(
    "SELECTED"     = "#0072B2",
    "NOT SELECTED" = "#D55E00",
    "CHECK"        = "#C2185B"
  )

  for (status in names(status_colors)) {
    sub_df <- fw_data[fw_data$plot_status == status, , drop = FALSE]
    if (nrow(sub_df) == 0) next

    p <- plotly::add_trace(
      p,
      x = sub_df$mean_value,
      y = sub_df$fw_slope,
      type = "scatter",
      mode = "markers",
      marker = list(
        color = sub_df$color,
        opacity = sub_df$display_opacity,
        size = sub_df$marker_size,
        line = list(
          color = sub_df$line_color,
          width = sub_df$line_width
        )
      ),
      customdata = sub_df$designation,
      text = sub_df$hover_text,
      hoverinfo = "text",
      name = status,
      legendgroup = status,
      showlegend = TRUE
    )
  }

  # --- Compute axis ranges for shapes ---
  x_min <- min(fw_data$mean_value, na.rm = TRUE)
  x_max <- max(fw_data$mean_value, na.rm = TRUE)
  x_pad <- (x_max - x_min) * 0.05
  y_min <- min(fw_data$fw_slope, na.rm = TRUE)
  y_max <- max(fw_data$fw_slope, na.rm = TRUE)
  y_pad <- (y_max - y_min) * 0.05

  # --- Layout with reference lines and stable zone ---
  shapes_list <- list(
    # Horizontal reference line at slope = 1.0
    list(
      type = "line",
      x0 = x_min - x_pad, x1 = x_max + x_pad,
      y0 = 1.0, y1 = 1.0,
      xref = "x", yref = "y",
      line = list(color = "grey50", width = 1.5, dash = "dash")
    ),
    # Vertical reference line at Check_Mean
    list(
      type = "line",
      x0 = check_mean, x1 = check_mean,
      y0 = y_min - y_pad, y1 = y_max + y_pad,
      xref = "x", yref = "y",
      line = list(color = "grey50", width = 1.5, dash = "dash")
    ),
    # Shaded stable zone: slope in [0.8, 1.2] AND mean_value above check_mean
    list(
      type = "rect",
      x0 = check_mean, x1 = x_max + x_pad,
      y0 = 0.8, y1 = 1.2,
      xref = "x", yref = "y",
      fillcolor = "rgba(76, 175, 80, 0.10)",
      line = list(width = 0),
      layer = "below"
    )
  )

  p <- plotly::layout(
    p,
    xaxis = list(
      title = "Mean Performance",
      range = c(x_min - x_pad, x_max + x_pad)
    ),
    yaxis = list(
      title = "FW Regression Slope",
      range = c(y_min - y_pad, y_max + y_pad)
    ),
    shapes = shapes_list,
    hovermode = "closest",
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = 1.05
    )
  )

  # --- Apply prescriptive annotations ---
  p <- annotate_stable_and_adaptive(p, fw_data, "fw", env_predictions = env_predictions)

  p
}

# --------------------------------------------------------------------------
# CS/Diagonal Stability Biplot (Mean vs. CV)
# --------------------------------------------------------------------------

#' Build the CS/Diagonal stability biplot (mean vs. CV)
#'
#' Renders a scatter plot with mean_value on x-axis and CV on y-axis.
#' Bottom-right quadrant represents the ideal region (high mean, low CV).
#' A shaded rectangle marks the "stable zone" using CHECK-based thresholds
#' (or quantile-based fallbacks when no CHECK genotypes exist).
#'
#' @param stability_data data.frame from \code{compute_gxe_stability()} with
#'   columns: designation, mean_value, cv, env_variance, n_environments,
#'   plot_status, opacity, color, shape.
#' @param highlighted Character scalar or NULL — if provided, this designation
#'   is rendered at 14px with a 3px white border and other markers are dimmed.
#' @param source_id Character string — the plotly source id for click events.
#' @param env_predictions data.frame or NULL — per-environment predictions used
#'   by \code{annotate_stable_and_adaptive()} to identify best environments for
#'   adaptive responders.
#'
#' @return A plotly object.
#'
#' @noRd
build_stability_biplot_plotly <- function(stability_data, highlighted = NULL,
                                          source_id, env_predictions = NULL) {

  # --- Step 1: Compute stable zone thresholds ---
  check_rows <- stability_data[stability_data$plot_status == "CHECK", , drop = FALSE]

  if (nrow(check_rows) > 0) {
    check_cv <- mean(check_rows$cv, na.rm = TRUE)
    check_mean <- mean(check_rows$mean_value, na.rm = TRUE)
  } else {
    # Fallback per Req 4.4: 25th percentile of CV, 75th percentile of mean_value
    check_cv <- quantile(stability_data$cv, 0.25, na.rm = TRUE, names = FALSE)
    check_mean <- quantile(stability_data$mean_value, 0.75, na.rm = TRUE, names = FALSE)
  }

  # --- Step 2: Adjust opacity and size for highlighting ---
  if (!is.null(highlighted)) {
    is_highlighted <- stability_data$designation == highlighted
    display_opacity <- ifelse(is_highlighted, stability_data$opacity,
                              stability_data$opacity * 0.3)
    marker_size <- ifelse(is_highlighted, 14, 8)
    line_colors <- ifelse(is_highlighted, "white", "rgba(0,0,0,0)")
    line_widths <- ifelse(is_highlighted, 3, 0)
  } else {
    display_opacity <- stability_data$opacity
    marker_size <- rep(8, nrow(stability_data))
    line_colors <- rep("rgba(0,0,0,0)", nrow(stability_data))
    line_widths <- rep(0, nrow(stability_data))
  }

  # --- Step 3: Build classification labels for tooltip ---
  classification <- rep("", nrow(stability_data))
  finite_metrics <- stability_data$cv[is.finite(stability_data$cv)]
  if (length(finite_metrics) >= 2) {
    metric_range <- max(finite_metrics) - min(finite_metrics)
    if (metric_range > 1e-9) {
      q25 <- quantile(finite_metrics, 0.25, na.rm = TRUE, names = FALSE)
      q75 <- quantile(finite_metrics, 0.75, na.rm = TRUE, names = FALSE)
      median_mean <- median(stability_data$mean_value, na.rm = TRUE)

      for (i in seq_len(nrow(stability_data))) {
        cv_i <- stability_data$cv[i]
        mean_i <- stability_data$mean_value[i]
        if (is.finite(cv_i) && is.finite(mean_i)) {
          if (cv_i <= q25 && mean_i > check_mean) {
            classification[i] <- "Stable High-Performer"
          } else if (cv_i > q75 && mean_i > median_mean) {
            classification[i] <- "Adaptive Responder"
          }
        }
      }
    }
  }

  # --- Step 4: Build tooltip text ---
  hover_text <- paste0(
    "<b>", stability_data$designation, "</b><br>",
    "Mean Performance: ", sprintf("%.3f", stability_data$mean_value), "<br>",
    "CV: ", sprintf("%.4f", stability_data$cv), "<br>",
    "Classification: ", ifelse(classification == "", "None", classification), "<br>",
    "Status: ", stability_data$plot_status
  )

  # --- Step 5: Create scatter plot ---
  p <- plotly::plot_ly(source = source_id)

  # Compute axis range for stable zone shape
  x_max <- max(stability_data$mean_value, na.rm = TRUE)
  x_pad <- (x_max - min(stability_data$mean_value, na.rm = TRUE)) * 0.05

  # Add shaded stable zone (bottom-right quadrant: high mean, low CV)
  shapes_list <- list(
    list(
      type = "rect",
      x0 = check_mean, x1 = x_max + x_pad,
      y0 = 0, y1 = check_cv,
      xref = "x", yref = "y",
      fillcolor = "rgba(144, 238, 144, 0.15)",
      line = list(color = "rgba(144, 238, 144, 0.4)", width = 1),
      layer = "below"
    )
  )

  # Add markers — one trace per status for legend
  status_colors <- c(
    "SELECTED"     = "#0072B2",
    "NOT SELECTED" = "#D55E00",
    "CHECK"        = "#C2185B"
  )

  for (status in names(status_colors)) {
    mask <- stability_data$plot_status == status
    sub_df <- stability_data[mask, , drop = FALSE]
    if (nrow(sub_df) == 0) next

    sub_opacity <- display_opacity[mask]
    sub_size <- marker_size[mask]
    sub_line_colors <- line_colors[mask]
    sub_line_widths <- line_widths[mask]
    sub_hover <- hover_text[mask]

    p <- plotly::add_trace(
      p,
      x = sub_df$mean_value,
      y = sub_df$cv,
      type = "scatter",
      mode = "markers",
      marker = list(
        color = sub_df$color,
        opacity = sub_opacity,
        size = sub_size,
        line = list(color = sub_line_colors, width = sub_line_widths)
      ),
      customdata = sub_df$designation,
      text = sub_hover,
      hoverinfo = "text",
      name = status,
      legendgroup = status,
      showlegend = TRUE
    )
  }

  # --- Step 6: Configure layout ---
  p <- plotly::layout(
    p,
    xaxis = list(title = "Mean Performance"),
    yaxis = list(title = "Coefficient of Variation (CV)"),
    shapes = shapes_list,
    hovermode = "closest",
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = 1.05
    )
  )

  # --- Step 7: Apply prescriptive annotations ---
  p <- annotate_stable_and_adaptive(p, stability_data, "cs_diag",
                                     env_predictions = env_predictions)

  p
}

# --------------------------------------------------------------------------
# Factor Analytic Biplot Builder
# --------------------------------------------------------------------------

#' Build Factor Analytic biplot (plotly)
#'
#' Renders a biplot of genotype scores on PC1 vs PC2 axes with environment
#' loadings displayed as arrows from the origin. Axis labels include the
#' percentage of variance explained by each component.
#'
#' @param fa_data A list with components:
#'   \itemize{
#'     \item genotype_scores: data.frame with columns designation, PC1, PC2,
#'       mean_value, plot_status, color, shape, opacity
#'     \item env_loadings: data.frame with columns environment, PC1_loading,
#'       PC2_loading
#'     \item variance_explained: numeric vector of length 2
#'   }
#' @param highlighted Character or NULL — designation to highlight with larger
#'   marker (14px) and 3px white border. All other points are dimmed.
#' @param source_id Character — plotly source identifier for click event capture.
#' @param env_predictions Data.frame or NULL — per-environment predictions with
#'   columns designation, environment, predictedValue. Passed through to
#'   \code{annotate_stable_and_adaptive()} for best-environment identification.
#'
#' @return A plotly object.
#'
#' @noRd
build_fa_biplot_plotly <- function(fa_data, highlighted = NULL, source_id,
                                   env_predictions = NULL) {

  scores <- fa_data$genotype_scores
  loadings <- fa_data$env_loadings
  var_expl <- fa_data$variance_explained

  # --- Axis labels with % variance explained ---
  xlab <- paste0("PC1 (", round(var_expl[1] * 100, 1), "%)")
  ylab <- paste0("PC2 (", round(var_expl[2] * 100, 1), "%)")

  # --- Highlighting logic ---
  if (!is.null(highlighted)) {
    is_highlighted <- scores$designation == highlighted
    scores$display_opacity <- ifelse(is_highlighted, scores$opacity,
                                     scores$opacity * 0.3)
    scores$marker_size <- ifelse(is_highlighted, 14, 8)
    scores$border_color <- ifelse(is_highlighted, "white", "rgba(0,0,0,0)")
    scores$border_width <- ifelse(is_highlighted, 3, 0)
  } else {
    scores$display_opacity <- scores$opacity
    scores$marker_size <- rep(8, nrow(scores))
    scores$border_color <- rep("rgba(0,0,0,0)", nrow(scores))
    scores$border_width <- rep(0, nrow(scores))
  }

  # --- Classification for tooltip ---
  stability_metric <- sqrt(scores$PC1^2 + scores$PC2^2)
  finite_metrics <- stability_metric[is.finite(stability_metric)]
  check_rows <- scores[scores$plot_status == "CHECK", , drop = FALSE]
  if (nrow(check_rows) > 0) {
    check_mean <- mean(check_rows$mean_value, na.rm = TRUE)
  } else {
    check_mean <- quantile(scores$mean_value, 0.75, na.rm = TRUE, names = FALSE)
  }
  q25 <- quantile(finite_metrics, 0.25, na.rm = TRUE, names = FALSE)
  q75 <- quantile(finite_metrics, 0.75, na.rm = TRUE, names = FALSE)
  median_mean <- median(scores$mean_value, na.rm = TRUE)

  scores$classification <- vapply(seq_len(nrow(scores)), function(i) {
    metric <- stability_metric[i]
    mv <- scores$mean_value[i]
    if (!is.finite(metric) || !is.finite(mv)) return("none")
    if (metric <= q25 && mv > check_mean) return("stable")
    if (metric > q75 && mv > median_mean) return("adaptive")
    return("none")
  }, character(1))

  # --- Tooltip ---
  scores$hover_text <- paste0(
    "<b>", scores$designation, "</b><br>",
    "PC1: ", sprintf("%.3f", scores$PC1), "<br>",
    "PC2: ", sprintf("%.3f", scores$PC2), "<br>",
    "Mean: ", sprintf("%.3f", scores$mean_value), "<br>",
    "Classification: ", scores$classification, "<br>",
    "Status: ", scores$plot_status
  )

  # --- Build genotype scatter ---
  p <- plotly::plot_ly(source = source_id)

  p <- plotly::add_trace(
    p,
    x = scores$PC1,
    y = scores$PC2,
    type = "scatter",
    mode = "markers",
    marker = list(
      color = scores$color,
      opacity = scores$display_opacity,
      size = scores$marker_size,
      line = list(
        color = scores$border_color,
        width = scores$border_width
      )
    ),
    customdata = scores$designation,
    text = scores$hover_text,
    hoverinfo = "text",
    showlegend = FALSE
  )

  # --- Scale environment loadings for display ---
  # Scale so arrows are visible relative to genotype score range
  score_range_x <- diff(range(scores$PC1, na.rm = TRUE))
  score_range_y <- diff(range(scores$PC2, na.rm = TRUE))
  score_range <- max(score_range_x, score_range_y, 1)

  loading_max <- max(
    abs(loadings$PC1_loading), abs(loadings$PC2_loading), na.rm = TRUE
  )
  if (loading_max > 0) {
    scale_factor <- (score_range * 0.4) / loading_max
  } else {
    scale_factor <- 1
  }

  scaled_pc1 <- loadings$PC1_loading * scale_factor
  scaled_pc2 <- loadings$PC2_loading * scale_factor

  # --- Add environment arrows as annotations ---
  arrow_annotations <- list()
  for (i in seq_len(nrow(loadings))) {
    # Arrow from origin to loading endpoint
    arrow_annotations[[length(arrow_annotations) + 1]] <- list(
      x = scaled_pc1[i],
      y = scaled_pc2[i],
      ax = 0,
      ay = 0,
      xref = "x",
      yref = "y",
      axref = "x",
      ayref = "y",
      showarrow = TRUE,
      arrowhead = 2,
      arrowsize = 1,
      arrowwidth = 1.5,
      arrowcolor = "#666666"
    )
    # Environment label at arrow endpoint
    arrow_annotations[[length(arrow_annotations) + 1]] <- list(
      x = scaled_pc1[i],
      y = scaled_pc2[i],
      text = loadings$environment[i],
      showarrow = FALSE,
      xref = "x",
      yref = "y",
      font = list(size = 9, color = "#444444"),
      xanchor = if (scaled_pc1[i] >= 0) "left" else "right",
      yanchor = "bottom"
    )
  }

  # --- Layout ---
  p <- plotly::layout(
    p,
    xaxis = list(title = xlab, zeroline = TRUE, zerolinecolor = "#CCCCCC"),
    yaxis = list(title = ylab, zeroline = TRUE, zerolinecolor = "#CCCCCC"),
    hovermode = "closest",
    annotations = arrow_annotations
  )

  # --- Prescriptive layer ---
  p <- annotate_stable_and_adaptive(p, scores, "fa", env_predictions)

  p
}

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
#' @param k NULL (auto-detect) or integer — number of clusters to use.
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
      # Try k=2 and k=3, pick by silhouette score
      has_cluster_pkg <- requireNamespace("cluster", quietly = TRUE)
      if (!has_cluster_pkg) {
        k <- 2L
      } else {
        candidates <- c(2L, 3L)
        # Only try k=3 if enough environments
        if (n_finite < 6) candidates <- 2L
        best_k <- 2L
        best_sil <- -1

        set.seed(42)
        for (ck in candidates) {
          if (ck > n_finite) next
          km_try <- stats::kmeans(cov_scaled, centers = ck, nstart = 25)
          sil <- cluster::silhouette(km_try$cluster, stats::dist(cov_scaled))
          mean_sil <- mean(sil[, "sil_width"])
          if (mean_sil > best_sil) {
            best_sil <- mean_sil
            best_k <- ck
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

  set.seed(42)
  km <- stats::kmeans(cov_scaled, centers = effective_k, nstart = 25)

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
    primary_dir <- if (center_row[primary_idx] >= 0) "High" else "Low"

    # Check if second covariate is also strongly distinguishing (> 0.5 in absolute terms)
    if (length(sorted_idx) >= 2 && abs_devs[sorted_idx[2]] > 0.5) {
      secondary_idx <- sorted_idx[2]
      secondary_cov <- available_covs[secondary_idx]
      secondary_label <- if (secondary_cov %in% names(.covariate_readable)) {
        .covariate_readable[[secondary_cov]]
      } else {
        secondary_cov
      }
      secondary_dir <- if (center_row[secondary_idx] >= 0) "high" else "low"
      labels_vec[i] <- paste0(primary_dir, " ", primary_label, " + ", secondary_dir, " ", secondary_label)
    } else {
      labels_vec[i] <- paste0(primary_dir, " ", primary_label, " environments")
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
#' @param trait Character — selected trait to filter on.
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
#' avoid threshold is the lower of (population mean − 1 SD) and the minimum
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

    # Handle edge case: single designation or all same value → sd is NA or 0

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
    # If pop_sd is 0, pop_mean - 0 = pop_mean → avoid = pop_mean (same as recommend)
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
#' @param highlighted NULL or character — designation to highlight across facets.
#' @param user_recommend_threshold NULL or numeric — user override for
#'   recommend threshold (applies to all clusters).
#' @param source_id Character — plotly source ID for click events.
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

    # Add markers — one trace per status for legend grouping
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
  fig <- plotly::layout(
    fig,
    shapes = shapes_list,
    annotations = annotations_list,
    xaxis = list(range = x_range, title = "Mean Performance"),
    showlegend = TRUE,
    legend = list(orientation = "v", x = 1.02, xanchor = "left", y = 0.5, yanchor = "middle"),
    margin = list(t = 60, b = 40, r = 120)
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

  # Cluster palette
  cluster_palette <- c("#4CAF50", "#2196F3", "#FF9800", "#9C27B0", "#F44336")

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

  # Assign color by cluster
  unique_clusters <- unique(loc_df$cluster)
  cluster_color_map <- stats::setNames(
    cluster_palette[seq_along(unique_clusters)],
    unique_clusters
  )
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
  buffer <- max(lon_range, lat_range) * 0.3 + 2

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
        range = c(lon_center - buffer, lon_center + buffer)
      ),
      lataxis = list(
        range = c(lat_center - buffer, lat_center + buffer)
      )
    ),
    showlegend = TRUE,
    legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.05),
    margin = list(t = 10, b = 30, l = 0, r = 0)
  )

  p
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
                                                  # column(width=4, tags$br(),
                                                  shinyWidgets::prettySwitch( inputId = ns('launch'), label = "Load example dataset", status = "success"),
                                                  # ),
                                                  tags$br(),
                                                  img(src = "www/qaRaw.png", height = 200, width = 470), # add an image
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
                                                      tags$li("Make final selection decisions and save your analysis")
                                                    ),
                                                    p(strong("After this step:"), " Save your results and share the RData file with other stakeholders.
                                                      Multiple saved selections can then be compared in the ", strong("Advancement Meeting Dashboard"), " tab
                                                      to reach a joint consensus during the advancement meeting."),
                                                    # column(width = 12, shiny::plotOutput(ns("plotDataDependencies")), ),
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
                                                   tags$p(style = "color:#FFFFFF; font-weight:bold;",
                                                          "Trait weights for selection index"),
                                                   tags$p(style = "color:#FFFFFF;",
                                                          "Assign a weight to each trait to compute a custom selection index. ",
                                                          "Higher absolute values indicate greater importance. ",
                                                          "Use negative weights for traits where lower values are preferred. ",
                                                          "Default weight is 1 for all traits."),
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

                                                     selectInput(
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
                                                       multiple = TRUE
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
                                                "Stability/adaptability plot",
                                                "Radar plot",
                                                "Performance across locations",
                                                "Per-variety trait performance profile",
                                                "Relatedness plot"
                                              ),
                                              selected = NULL
                                            )
                                          )
                                        ),

                                        br(),

                                        conditionalPanel(
                                          condition = "input.reviewPlots && input.reviewPlots.includes('Pair-wise trait scatterplot')",
                                          ns = ns,

                                          column(
                                            width = 12,
                                            style = "background-color:grey; color: #FFFFFF; padding:15px 0;",

                                            column(
                                              width = 4,
                                              uiOutput(ns("scatterXTraitUI"))
                                            ),

                                            column(
                                              width = 4,
                                              uiOutput(ns("scatterYTraitUI"))
                                            ),

                                            column(
                                              width = 4,
                                              checkboxInput(
                                                ns("scatterShowRegression"),
                                                label = "Show regression lines & means",
                                                value = TRUE
                                              )
                                            )
                                          ),

                                          br()
                                        ),

                                        conditionalPanel(
                                          condition = "input.reviewPlots && input.reviewPlots.includes('Stability/adaptability plot')",
                                          ns = ns,

                                          column(
                                            width = 12,
                                            style = "background-color:grey; color: #FFFFFF; padding:15px 0;",

                                            column(
                                              width = 6,
                                              uiOutput(ns("stabilityTraitUI"))
                                            ),

                                            column(
                                              width = 6,
                                              tags$div(
                                                style = "padding-top: 25px;",
                                                uiOutput(ns("stabilityModelLabel"))
                                              )
                                            )
                                          ),

                                          br()
                                        ),

                                        conditionalPanel(
                                          condition = "input.reviewPlots && input.reviewPlots.includes('Radar plot')",
                                          ns = ns,

                                          column(
                                            width = 12,
                                            style = "background-color:grey; color: #FFFFFF; padding:15px 0;",

                                            column(
                                              width = 6,
                                              uiOutput(ns("radarDesignationUI"))
                                            )
                                          ),

                                          br()
                                        ),

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

                                        conditionalPanel(
                                          condition = "input.reviewPlots && input.reviewPlots.includes('Performance across locations')",
                                          ns = ns,

                                          column(
                                            width = 12,
                                            style = "background-color:grey; color: #FFFFFF; padding:15px 0;",

                                            column(
                                              width = 12,
                                              uiOutput(ns("lollipopTraitUI"))
                                            )
                                          ),

                                          br()
                                        ),

                                        conditionalPanel(
                                          condition = "input.reviewPlots && input.reviewPlots.includes('Per-variety trait performance profile')",
                                          ns = ns,
                                          column(
                                            width = 12,
                                            style = "background-color:grey; color: #FFFFFF; padding:15px 0;",

                                            column(
                                              width = 4,
                                              selectInput(
                                                ns("performanceProfileScale"),
                                                label = tags$span(
                                                  "Per-variety profile scale",
                                                  tags$i(
                                                    class = "glyphicon glyphicon-info-sign",
                                                    style = "color:#FFFFFF",
                                                    title = "Choose how performance profiles should be displayed when the per-variety performance plot is selected."
                                                  )
                                                ),
                                                choices = c("% over check", "% over mean"),
                                                selected = "% over check"
                                              )
                                            ),
                                            column(
                                              width = 4,
                                              numericInput(
                                                ns("performanceProfilePage"),
                                                label = tags$span(
                                                  "Page",
                                                  tags$i(
                                                    class = "glyphicon glyphicon-info-sign",
                                                    style = "color:#FFFFFF",
                                                    title = "Each page shows up to 10 designations."
                                                  )
                                                ),
                                                value = 1,
                                                min = 1,
                                                step = 1
                                              )
                                            ),
                                            column(
                                              width = 4,
                                              uiOutput(ns("performanceProfilePageInfo"))
                                            )
                                          )
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
                                              choices = c("SELECTED", "NOT SELECTED"),
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
                                              label = "Initial selection stamp for report",
                                              choices = NULL,
                                              multiple = FALSE
                                            )
                                          ),

                                          column(
                                            width = 4,
                                            selectInput(
                                              ns("reportPlotSelectionStamp"),
                                              label = "Plot selection stamp for report",
                                              choices = c("No plot selection" = "__none__"),
                                              selected = "__none__",
                                              multiple = FALSE
                                            )
                                          ),

                                          column(
                                            width = 4,
                                            selectInput(
                                              ns("reportFinalSelectionStamp"),
                                              label = "Final selection stamp for report",
                                              choices = c("No final selection" = "__none__"),
                                              selected = "__none__",
                                              multiple = FALSE
                                            )
                                          )
                                        ),

                                        br(),

                                        column(
                                          width = 12,
                                          style = "background-color:grey; color: #FFFFFF",
                                          column(
                                            width = 3,
                                            br(),
                                            actionButton(
                                              ns("runFinalProdAdv"),
                                              "Generate final report",
                                              icon = icon("file-lines")
                                            ),
                                            br(),
                                            br()
                                          )
                                        ),

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
                                                      uiOutput(ns('reportProdAdv'))
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

    #############################################################################
    #Helper functions
    sanitize_trait_id <- function(x) {
      gsub("[^A-Za-z0-9_]", "_", x)
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
                                          plot_stamp = "__none__",
                                          final_stamp = "__none__",
                                          final_overrides = NULL) {

      base_df <- cgiarPipeline::build_prodadv_decision_table_data(
        dt = dt,
        initial_stamp = initial_stamp,
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
    #Global options (idx2)
    ########################################

    observeEvent(c(data(), input$mtaStamp), {
      req(data())
      req(input$mtaStamp)
      dt <- data()
      dtPred <- dt$predictions
      dtPred <- dtPred[which(dtPred$analysisId %in% input$mtaStamp),]
      traitsProdAdv <- unique(dtPred$trait)
      updateSelectInput(session, "traitsToEvaluate", choices = traitsProdAdv)
    })

    # --- Observer: Stability/adaptability checkbox availability (Task 8.1) ---
    observe({
      req(data())
      req(input$mtaStamp)

      dt <- data()
      model_type <- detect_gxe_model(dt$predictions, input$mtaStamp, dt$modeling)

      # Treat unexpected model values as "none" (Req 2.5)
      valid_models <- c("fw", "fa", "cs_diag", "none")
      if (!(model_type %in% valid_models)) {
        model_type <- "none"
      }

      # Define all possible choices in the desired order
      all_choices <- c(
        "Pair-wise trait scatterplot",
        "Stability/adaptability plot",
        "Radar plot",
        "Performance across locations",
        "Per-variety trait performance profile",
        "Relatedness plot"
      )

      if (model_type == "none") {
        # Remove "Stability/adaptability plot" from choices
        current_choices <- all_choices[all_choices != "Stability/adaptability plot"]
      } else {
        # Include "Stability/adaptability plot" in choices
        current_choices <- all_choices
      }

      # Preserve existing selections but remove stability if model is "none"
      current_selected <- input$reviewPlots
      if (model_type == "none") {
        current_selected <- setdiff(current_selected, "Stability/adaptability plot")
      }

      updateCheckboxGroupInput(
        session,
        "reviewPlots",
        choices = current_choices,
        selected = current_selected
      )
    })

    # --- Stability trait selector (Task 8.2 server) ---
    output$stabilityTraitUI <- renderUI({
      req(data())
      req(input$mtaStamp)

      dt <- data()
      dtPred <- dt$predictions
      dtPred <- dtPred[dtPred$analysisId %in% input$mtaStamp, , drop = FALSE]
      trait_choices <- unique(dtPred$trait)
      req(length(trait_choices) >= 1)

      selectInput(
        ns("stabilityTrait"),
        label = tags$span(
          "Trait for stability plot",
          tags$i(
            class = "glyphicon glyphicon-info-sign",
            style = "color:#FFFFFF",
            title = "Select which trait to visualize in the stability/adaptability plot."
          )
        ),
        choices = trait_choices,
        selected = trait_choices[1]
      )
    })

    # --- Stability model type label (Task 8.2 server) ---
    output$stabilityModelLabel <- renderUI({
      req(data())
      req(input$mtaStamp)

      model_type <- detect_gxe_model(data()$predictions, input$mtaStamp, data()$modeling)

      model_label <- switch(
        model_type,
        "fw" = "Finlay-Wilkinson (reaction norm)",
        "fa" = "Factor Analytic (biplot)",
        "cs_diag" = "Compound Symmetry / Diagonal (mean vs. CV)",
        "Model not detected"
      )

      tags$span(
        style = "color: #FFFFFF; font-style: italic;",
        icon("chart-line"),
        paste("Detected model:", model_label)
      )
    })

    # --- Stability plot informational message (Task 8.3 server) ---
    output$stabilityPlotMessage <- renderUI({
      req(data())
      req(input$mtaStamp)

      model_type <- detect_gxe_model(data()$predictions, input$mtaStamp, data()$modeling)

      valid_models <- c("fw", "fa", "cs_diag")
      if (!(model_type %in% valid_models)) {
        return(tags$p(
          style = "color: #999; padding: 10px;",
          "No GxE model detected for this MTA stamp. Stability plot is not available."
        ))
      }

      model_desc <- switch(
        model_type,
        "fw" = "Showing Finlay-Wilkinson reaction norm plot (mean performance vs. FW regression slope).",
        "fa" = "Showing Factor Analytic biplot (genotype scores on PC1 vs. PC2 with environment loadings).",
        "cs_diag" = "Showing mean vs. coefficient of variation (CV) stability plot."
      )

      tags$p(
        style = "color: #555; padding: 10px;",
        icon("info-circle"),
        model_desc
      )
    })

    # --- GxE model type reactive (Task 9.1) ---
    gxe_model_type <- reactive({
      req(data())
      req(input$mtaStamp)
      detect_gxe_model(data()$predictions, input$mtaStamp, data()$modeling)
    })

    # --- Stability plot data reactive (Task 9.2) ---
    stability_plot_data <- reactive({
      req(input$stabilityTrait)
      model_type <- gxe_model_type()
      req(model_type)
      validate(need(model_type %in% c("fw", "fa", "cs_diag"), "No GxE model detected"))

      dt <- data()
      preds <- dt$predictions
      trait <- input$stabilityTrait
      mta_stamp <- input$mtaStamp
      overrides <- plot_selection_overrides()
      review_df <- review_plot_data()$review_df

      # Check for sufficient data (Req 11.6)
      validate(need(
        nrow(preds[preds$analysisId == mta_stamp & preds$trait == trait, ]) > 0,
        "No prediction data available for the selected analysis and trait"
      ))

      result <- switch(model_type,
        "fw" = {
          fw_result <- extract_fw_data(preds, review_df, mta_stamp, trait, overrides)
          # Validate FW slope availability (Req 3.6, 11.2)
          validate(need(nrow(fw_result) > 0, "FW slopes not available for this trait"))
          # Validate minimum genotype count (Req 11.5)
          validate(need(
            nrow(fw_result) >= 4,
            "Insufficient data for stability visualization (minimum 4 genotypes required)"
          ))
          list(data = fw_result, model_type = "fw")
        },
        "cs_diag" = {
          cs_result <- compute_gxe_stability(preds, review_df, mta_stamp, trait, overrides)
          # Validate minimum genotype count (Req 11.5)
          validate(need(
            nrow(cs_result) >= 4,
            "Insufficient data for stability visualization (minimum 4 genotypes required)"
          ))
          list(data = cs_result, model_type = "cs_diag")
        },
        "fa" = {
          fa_result <- extract_fa_data(preds, review_df, mta_stamp, trait, overrides)
          if (fa_result$fallback) {
            # FA fallback to CS/Diag (Req 5.4, 11.1)
            validate(need(
              nrow(fa_result$fallback_data) >= 4,
              "Insufficient data for stability visualization (minimum 4 genotypes required)"
            ))
            list(data = fa_result$fallback_data, model_type = "cs_diag", is_fallback = TRUE)
          } else {
            validate(need(
              nrow(fa_result$genotype_scores) >= 4,
              "Insufficient data for stability visualization (minimum 4 genotypes required)"
            ))
            list(data = fa_result, model_type = "fa")
          }
        }
      )

      result
    })

    # --- Stability plot highlighted designation reactiveVal (shared) ---
    highlighted_stability_designation <- reactiveVal(NULL)

    # --- Stability plot informational message (Task 9.3 - fallback) ---
    output$stabilityPlotMessage <- renderUI({
      plot_info <- stability_plot_data()
      if (isTRUE(plot_info$is_fallback)) {
        tags$p(
          style = "color: #856404; background-color: #fff3cd; padding: 10px; border-radius: 4px;",
          icon("info-circle"),
          "FA biplot requires at least 3 environments; showing mean vs. stability plot instead"
        )
      } else {
        NULL
      }
    })

    # --- Stability plot renderPlotly (Task 9.3) ---
    output$stabilityPlot <- plotly::renderPlotly({
      plot_info <- stability_plot_data()
      req(plot_info)

      highlighted <- highlighted_stability_designation()
      source_id <- ns("stabilityPlot")

      # Get per-env predictions for adaptive responder annotations
      dt <- data()
      preds <- dt$predictions
      mta_stamp <- input$mtaStamp
      trait <- input$stabilityTrait
      mta_preds <- preds[preds$analysisId == mta_stamp & preds$trait == trait, ]
      excluded_types <- c("designation", "fw_slope", "GenCorrMat")
      env_preds <- mta_preds[!(mta_preds$effectType %in% excluded_types),
                             c("designation", "environment", "predictedValue")]

      switch(plot_info$model_type,
        "fw" = build_fw_reaction_norm_plotly(plot_info$data, highlighted, source_id, env_preds),
        "cs_diag" = build_stability_biplot_plotly(plot_info$data, highlighted, source_id, env_preds),
        "fa" = build_fa_biplot_plotly(plot_info$data, highlighted, source_id, env_preds)
      )
    })

    # --- Click-to-highlight event handler for stability plot (Task 9.4) ---
    observeEvent(
      plotly::event_data("plotly_click", source = ns("stabilityPlot")),
      {
        click_data <- plotly::event_data("plotly_click", source = ns("stabilityPlot"))
        req(click_data)

        clicked_designation <- click_data$customdata
        if (is.null(clicked_designation) || length(clicked_designation) == 0) return()
        clicked_designation <- clicked_designation[1]

        # Toggle: if same designation clicked again, clear highlight; otherwise set new
        current <- highlighted_stability_designation()
        if (!is.null(current) && current == clicked_designation) {
          highlighted_stability_designation(NULL)
        } else {
          highlighted_stability_designation(clicked_designation)
        }
      },
      ignoreInit = TRUE
    )

    output$customWeightsUI <- renderUI({
      req(input$traitsToEvaluate)
      req(length(input$traitsToEvaluate) > 0)

      trait_list <- input$traitsToEvaluate

      tagList(
        tags$div(
          style = "margin-top: 5px;",
          lapply(trait_list, function(trait) {
            safe_trait <- gsub("[^A-Za-z0-9_]", "_", trait)

            numericInput(
              inputId = ns(paste0("weight_", safe_trait)),
              label = trait,
              value = 1,
              step = 0.1
            )
          })
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

    output$traitRuleCards <- renderUI({
      req(input$traitsToEvaluate)
      req(length(input$traitsToEvaluate) > 0)

      trait_list <- input$traitsToEvaluate

      tagList(
        lapply(trait_list, function(trait_name) {
          safe_trait <- gsub("[^A-Za-z0-9_]", "_", trait_name)

          shinydashboard::box(
            width = 12,
            title = trait_name,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,

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
                selected = "Higher is better"
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
            tagList(
              sliderInput(
                ns(paste0("minThresholdSlider_", safe_trait)),
                "Threshold value",
                min = rng_min,
                max = rng_max,
                value = rng_min,
                step = slider_step
              ),
              numericInput(
                ns(paste0("minThreshold_", safe_trait)),
                "Threshold value",
                value = rng_min,
                min = rng_min,
                max = rng_max,
                step = slider_step
              )
            )

          } else if (rule_type == "Acceptable range") {
            tagList(
              sliderInput(
                ns(paste0("rangeSlider_", safe_trait)),
                "Acceptable range",
                min = rng_min,
                max = rng_max,
                value = c(rng_min, rng_max),
                step = slider_step
              ),
              fluidRow(
                column(
                  width = 6,
                  numericInput(
                    ns(paste0("rangeMin_", safe_trait)),
                    "Minimum value",
                    value = rng_min,
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
                    value = rng_max,
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

            tagList(
              selectInput(
                ns(paste0("checkVar_", safe_trait)),
                "Reference check",
                choices = check_designations,
                multiple = FALSE
              ),
              sliderInput(
                ns(paste0("pctOverCheckSlider_", safe_trait)),
                "% over check",
                min = 0,
                max = 100,
                value = 0,
                step = 1
              ),
              numericInput(
                ns(paste0("pctOverCheck_", safe_trait)),
                "% over check",
                value = 0,
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
    #Pre-selection (idx4)
    ########################################

    observe({
      req(data())

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

      updateSelectInput(
        session = session,
        inputId = "candidateDesignations",
        choices = mta_candidates
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
          direction <- "Higher is better"
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
            threshold <- rng[1]
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

          if (is.null(reference_check) || !nzchar(reference_check)) {
            if (!is.null(check_entry_type_value)) {
              check_designations <- unique(
                dtPred_trait$designation[
                  dtPred_trait$entryType == check_entry_type_value &
                    !is.na(dtPred_trait$designation)
                ]
              )

              reference_check <- if (length(check_designations) > 0) {
                check_designations[1]
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
        selectedCandidates = selected_candidates()
      )
    })

    observeEvent(input$runInitProdAdv, {

      args <- initial_selection_args()

      # Step 1: Check trait quality first
      quality_check <- tryCatch({
        cgiarPipeline::checkTraitQuality(args = args, dt_object = data())
      }, error = function(e) {
        list(has_issues = FALSE, flagged_traits = character(0), flag_reasons = list())
      })

      # Cache flagged traits for use by the confirm handler
      flagged_traits_cache(quality_check$flagged_traits)

      # If traits are flagged, show modal with per-trait checkboxes
      if (quality_check$has_issues) {

        # Build checkbox UI for each flagged trait
        checkbox_ui <- lapply(quality_check$flagged_traits, function(t) {
          tags$div(
            style = "margin-bottom:8px; padding:8px; background:#fff3cd; border-radius:4px;",
            checkboxInput(
              inputId = ns(paste0("excludeTrait_", gsub("[^A-Za-z0-9_]", "_", t))),
              label = tags$span(
                tags$strong(t), " — ", quality_check$flag_reasons[[t]]
              ),
              value = TRUE  # default: exclude flagged traits
            )
          )
        })

        showModal(modalDialog(
          title = tags$span(icon("triangle-exclamation"), " Trait quality warning"),
          tags$p("The following traits have low prediction quality for ranking candidates."),
          tags$p("Checked traits will be ", tags$strong("excluded"), " from the selection index."),
          tags$p("Uncheck a trait to ", tags$strong("keep it"), " in the analysis despite the warning."),
          tags$hr(),
          do.call(tagList, checkbox_ui),
          tags$hr(),
          tags$p(style = "color:#666; font-size:12px;",
                 "Traits with low RSR or reliability may produce unreliable rankings. ",
                 "Including them will still apply the reliability-weighted penalty in the index."),
          footer = tagList(
            actionButton(ns("confirmTraitSelection"), "Proceed", icon = icon("play-circle"), class = "btn-success"),
            modalButton("Cancel")
          ),
          size = "m",
          easyClose = FALSE
        ))
      } else {
        # No issues — run directly
        run_initial_selection(args, exclude_traits = NULL)
      }
    })

    # Store the flagged traits for use in the confirm handler
    flagged_traits_cache <- reactiveVal(character(0))

    observeEvent(input$confirmTraitSelection, {
      removeModal()

      args <- initial_selection_args()
      flagged <- flagged_traits_cache()

      # Read which traits the user chose to exclude (checked = exclude)
      exclude_traits <- character(0)
      for (t in flagged) {
        safe_t <- gsub("[^A-Za-z0-9_]", "_", t)
        if (isTRUE(input[[paste0("excludeTrait_", safe_t)]])) {
          exclude_traits <- c(exclude_traits, t)
        }
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

      # Show info about flagged traits that were kept
      flagged <- attr(result, "pam_flagged_traits")
      if (!is.null(flagged) && length(flagged) > 0) {
        kept <- setdiff(flagged, exclude_traits)
        if (length(kept) > 0) {
          showNotification(
            paste0("Note: Traits with low ranking quality included in analysis: ",
                   paste(kept, collapse = ", "), ". Reliability weighting was applied."),
            type = "warning",
            duration = 10
          )
        }
      }

      showNotification(
        "Initial selection completed successfully.",
        type = "message"
      )

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
      dtPlotSel  <- dt[dt$module == "Plot_prodAdv",  , drop = FALSE]
      dtFinalSel <- dt[dt$module == "Final_prodAdv", , drop = FALSE]

      stampsSta      <- make_stamp_choices(dtSta)
      stampsMta      <- make_stamp_choices(dtMta)
      stampsIdxD     <- make_stamp_choices(dtIdxD)
      stampsInitSel  <- make_stamp_choices(dtInitSel)
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

      # Table selection stamp uses same Plot_prodAdv stamps (table selections are stored as Plot_prodAdv)
      updateSelectInput(
        session,
        "tableSelectionStampLoad",
        choices = c("Start from initial selection" = "__none__", stampsPlotSel)
      )

      # Viz stamp dropdowns
      updateSelectInput(
        session,
        "vizTableSelectionStamp",
        choices = c("Use initial selection" = "__none__", stampsPlotSel)
      )

      updateSelectInput(
        session,
        "finalSelectionStamp",
        choices = c("No final selection" = "__none__", stampsFinalSel),
        selected = current_final
      )
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

      # Check for flagged (but kept) traits
      flag_df <- dt$modeling[
        dt$modeling$analysisId %in% input$initialSelectionStamp &
          dt$modeling$module == "Init_prodAdv" &
          dt$modeling$parameter == "flagged_low_quality",
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

      if (nrow(flag_df) > 0) {
        flag_text <- paste(sapply(seq_len(nrow(flag_df)), function(i) {
          paste0(flag_df$trait[i], ": ", flag_df$value[i])
        }), collapse = "; ")
        ui_parts <- c(ui_parts, list(
          div(
            style = "background-color:#fff3cd; color:#856404; border:1px solid #ffeeba; padding:12px; border-radius:6px; margin-bottom:12px;",
            tags$strong(icon("triangle-exclamation"), " Traits with low ranking quality (kept in analysis):"),
            tags$p(flag_text),
            tags$p("These traits were flagged but included in the selection. Consider their reliability when interpreting results.")
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
        cgiarPipeline::build_prodadv_decision_table_data(
          dt = dt,
          initial_stamp = initial_stamp,
          plot_stamp = if (!is.null(table_stamp)) table_stamp else "__none__",
          final_stamp = "__none__"
        )
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

    # Render decision table with gradient coloring
    output$tableDecisionDT <- DT::renderDT({
      tbl <- table_decision_data()
      req(tbl)

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

      # Sort by index_value descending (checks participate in ordering)
      if ("index_value" %in% colnames(tbl)) {
        tbl <- tbl[order(-as.numeric(tbl$index_value)), , drop = FALSE]
      }

      # Original color scheme (colorblind-friendly), more saturated for gradient visibility:
      # SELECTED = blue, NOT SELECTED = orange, CHECK = pink
      col_selected <- "#85C1E9"
      col_not_selected <- "#E8A87C"
      col_check <- "#C39BD3"

      # Helper: compute gradient intensity (5 levels based on 20th percentile bins)
      # Returns opacity multiplier 0.15-1.0 for pronounced gradient
      get_quintile_opacity <- function(values, status, higher_is_better = TRUE) {
        n <- length(values)
        opacity <- rep(0.5, n)  # default mid
        numeric_vals <- as.numeric(values)
        valid <- !is.na(numeric_vals) & status != "CHECK"
        if (sum(valid) < 5) return(opacity)
        quants <- quantile(numeric_vals[valid], probs = c(0.2, 0.4, 0.6, 0.8), na.rm = TRUE)
        for (i in which(valid)) {
          v <- numeric_vals[i]
          if (status[i] == "SELECTED") {
            # For selected: darker = higher value
            if (v >= quants[4]) opacity[i] <- 1.0
            else if (v >= quants[3]) opacity[i] <- 0.75
            else if (v >= quants[2]) opacity[i] <- 0.5
            else if (v >= quants[1]) opacity[i] <- 0.3
            else opacity[i] <- 0.15
          } else {
            # For not selected: darker = lower value
            if (v <= quants[1]) opacity[i] <- 1.0
            else if (v <= quants[2]) opacity[i] <- 0.75
            else if (v <= quants[3]) opacity[i] <- 0.5
            else if (v <= quants[4]) opacity[i] <- 0.3
            else opacity[i] <- 0.15
          }
        }
        opacity
      }

      # Helper: blend color with white based on opacity
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

      # Index value column with gradient
      if ("index_value" %in% colnames(tbl)) {
        idx_status <- tbl$initial_decision
        idx_opacity <- get_quintile_opacity(tbl$index_value, idx_status)
        display_df$index_value <- mapply(function(val, st, op) {
          base_col <- if (st == "SELECTED") col_selected else if (st == "CHECK") col_check else col_not_selected
          bg <- blend_color(base_col, op)
          sprintf("<div style='background:%s; padding:6px; border-radius:4px; text-align:right; font-weight:600;'>%s</div>",
                  bg, format(round(as.numeric(val), 3), nsmall = 3))
        }, tbl$index_value, idx_status, idx_opacity, SIMPLIFY = TRUE)
      }

      # Trait columns with gradient
      # Get available trait decision columns from the data
      avail_trait_decisions <- grep("_trait_decision$", colnames(tbl), value = TRUE)
      
      # Debug: show what columns are available
      if (length(avail_trait_decisions) == 0) {
        showNotification(
          paste("Debug: No _trait_decision columns found in tbl. Columns:", paste(colnames(tbl), collapse=", ")),
          type = "warning", duration = 15
        )
      }
      
      for (tr in selected_traits) {
        trait_decision_col <- paste0(tr, "_trait_decision")
        if (trait_decision_col %in% avail_trait_decisions) {
          trait_status <- tbl[[trait_decision_col]]
        } else {
          # Trait decision not available — default all to SELECTED (no threshold applied)
          trait_status <- rep("SELECTED", nrow(tbl))
        }
        trait_vals <- tbl[[tr]]
        tr_opacity <- get_quintile_opacity(trait_vals, trait_status)
        display_df[[tr]] <- mapply(function(val, st, op) {
          base_col <- if (st == "SELECTED") col_selected else if (st == "CHECK") col_check else col_not_selected
          bg <- blend_color(base_col, op)
          txt <- if (is.na(val)) "" else format(round(as.numeric(val), 3), nsmall = 3)
          sprintf("<div style='background:%s; padding:6px; border-radius:4px; text-align:right;'>%s</div>", bg, txt)
        }, trait_vals, trait_status, tr_opacity, SIMPLIFY = TRUE)
      }

      # Index selection column (badge)
      display_df$index_selection <- sapply(tbl$initial_decision, function(st) {
        bg <- if (st == "SELECTED") col_selected else if (st == "CHECK") col_check else col_not_selected
        sprintf("<div style='background:%s; padding:6px; border-radius:4px; text-align:center; font-weight:600;'>%s</div>", bg, st)
      })

      # Table selection column (dropdown, same as old final_decision)
      display_df$table_selection <- mapply(function(designation, initial_decision) {
        if (identical(initial_decision, "CHECK")) {
          bg <- col_check
          return(sprintf("<div style='background:%s; padding:6px; border-radius:4px; text-align:center; font-weight:600;'>CHECK</div>", bg))
        }
        selected_value <- initial_decision
        choices <- c("SELECTED", "NOT SELECTED", "REVISE")
        options <- vapply(choices, function(ch) {
          sel <- if (identical(ch, selected_value)) " selected" else ""
          sprintf("<option value='%s'%s>%s</option>", ch, sel, ch)
        }, character(1))
        sprintf("<select class='form-control table-decision-select' data-designation='%s' style='width:140px;'>%s</select>",
                htmltools::htmlEscape(designation), paste(options, collapse = ""))
      }, tbl$designation, tbl$initial_decision, SIMPLIFY = TRUE)

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
          ordering = FALSE,
          autoWidth = FALSE
        ),
        callback = htmlwidgets::JS(sprintf("
          table.on('change', 'select.table-decision-select', function() {
            var designation = $(this).data('designation');
            var value = $(this).val();
            Shiny.setInputValue('%s', {
              designation: designation,
              value: value,
              nonce: Math.random()
            }, {priority: 'event'});
          });
        ", ns("tableDecisionChange")))
      )
    })

    # Handle table selection changes from dropdown
    observeEvent(input$tableDecisionChange, {
      # Store overrides for saving later
      showNotification(
        paste0("Updated: ", input$tableDecisionChange$designation, " → ", input$tableDecisionChange$value),
        type = "message", duration = 3
      )
    }, ignoreInit = TRUE)

    # Save table selection
    observeEvent(input$saveTableSelection, {
      req(data())
      req(input$initialSelectionStamp)

      tbl <- table_decision_data()
      req(tbl)

      # Get table selection values (from the editable table or initial decision)
      table_decisions <- if ("table_selection" %in% colnames(tbl)) {
        tbl[, c("designation", "table_selection"), drop = FALSE]
      } else {
        data.frame(designation = tbl$designation, table_selection = tbl$initial_decision, stringsAsFactors = FALSE)
      }
      names(table_decisions)[2] <- "plot_decision"

      analysis_name <- if (nzchar(trimws(input$tableSelectionId))) trimws(input$tableSelectionId) else NULL

      dt_object <- data()
      result <- tryCatch({
        cgiarPipeline::savePlotProdAdvSelection(
          analysisId = as.numeric(Sys.time()),
          analysisIdName = analysis_name,
          initialSelectionStamp = input$initialSelectionStamp,
          plotSelectionStamp = NULL,
          manual_decisions = table_decisions,
          dt_object = dt_object
        )
      }, error = function(e) {
        showNotification(paste("Save failed:", e$message), type = "error")
        return(NULL)
      })

      req(result)
      data(result)
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
        plot_stamp = input$plotSelectionStamp,
        final_stamp = input$finalSelectionStamp
      )

      out$selected_plots <- input$reviewPlots
      out$performanceProfileScale <- input$performanceProfileScale

      out
    }, ignoreInit = TRUE)

    # --- Lollipop trait selector ---
    output$lollipopTraitUI <- renderUI({
      plot_obj <- review_plot_data()
      req(plot_obj)

      trait_choices <- plot_obj$traits
      # Always add index_value as an option — it's computed during initial selection
      # and will be fetched from decision table data when selected
      trait_choices <- c("index_value", trait_choices)
      req(length(trait_choices) >= 1)

      selectInput(
        ns("lollipopTrait"),
        label = tags$span(
          "Trait to rank by",
          tags$i(
            class = "glyphicon glyphicon-info-sign",
            style = "color:#FFFFFF",
            title = "Select which trait to use for ranking designations across environment types."
          )
        ),
        choices = trait_choices,
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

    # --- Lollipop plot data reactive ---
    lollipop_plot_data <- reactive({
      plot_obj <- review_plot_data()
      req(plot_obj)
      req(input$lollipopTrait)

      clusters <- lollipop_cluster_assignments()
      req(length(clusters) > 0)

      # If trait is index_value, compute it and inject into review_df
      review_df_enriched <- plot_obj$review_df
      if (input$lollipopTrait == "index_value" && !"index_value" %in% colnames(review_df_enriched)) {
        # Fetch index_value from decision table data
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
          idx_df <- tbl[, c("designation", "index_value"), drop = FALSE]
          review_df_enriched <- merge(review_df_enriched, idx_df, by = "designation", all.x = TRUE)
        }
      }

      df <- prepare_lollipop_data(
        sta_long = plot_obj$sta_long,
        review_df = review_df_enriched,
        trait = input$lollipopTrait,
        cluster_assignments = clusters,
        overrides = plot_selection_overrides()
      )

      validate(
        need(nrow(df) > 0, "No environment-level data available for the selected trait.")
      )

      df
    })

    # --- Lollipop plot output ---
    output$lollipopPlot <- plotly::renderPlotly({
      data <- lollipop_plot_data()
      req(nrow(data) > 0)

      thresholds <- compute_zone_thresholds(data)

      build_faceted_lollipop_plotly(
        prepared_data = data,
        thresholds = thresholds,
        highlighted = highlighted_lollipop_designation(),
        user_recommend_threshold = NULL,
        source_id = ns("lollipopPlot")
      )
    })

    # --- Click-to-highlight event handler ---
    highlighted_lollipop_designation <- reactiveVal(NULL)

    observeEvent(
      plotly::event_data("plotly_click", source = ns("lollipopPlot")),
      {
        click <- plotly::event_data("plotly_click", source = ns("lollipopPlot"))
        req(click)

        clicked_designation <- click$key
        req(clicked_designation)

        current <- highlighted_lollipop_designation()
        if (!is.null(current) && current == clicked_designation) {
          highlighted_lollipop_designation(NULL)
        } else {
          highlighted_lollipop_designation(clicked_designation)
        }
      },
      ignoreInit = TRUE
    )

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

      selectInput(
        ns("scatterXTrait"),
        "X-axis trait",
        choices = trait_choices,
        selected = trait_choices[1]
      )
    })

    output$scatterYTraitUI <- renderUI({
      plot_obj <- review_plot_data()
      req(plot_obj)

      trait_choices <- plot_obj$traits
      req(length(trait_choices) >= 2)

      default_y <- if (length(trait_choices) >= 2) trait_choices[2] else trait_choices[1]

      selectInput(
        ns("scatterYTrait"),
        "Y-axis trait",
        choices = trait_choices,
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

        updateSelectInput(
          session,
          "scatterYTrait",
          choices = y_choices,
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

      # Compute absolute opacity based on reliability (NOT relative to population)
      # reliability >= 0.7 → opacity 1.0; reliability = 0 → opacity 0.15
      reliability_to_opacity <- function(rel) {
        rel[is.na(rel)] <- 0.7  # default to full opacity if NA
        pmin(1.0, 0.15 + (pmin(rel, 0.7) / 0.7) * 0.85)
      }

      df_plot$opacity <- reliability_to_opacity(
        pmin(df_plot$x_reliability, df_plot$y_reliability)
      )
      # Checks always full opacity
      df_plot$opacity[df_plot$plot_status == "CHECK"] <- 1.0

      df_plot$plot_status <- factor(df_plot$plot_status, levels = c("SELECTED", "NOT SELECTED", "CHECK"))

      # Colors: original scheme
      status_colors <- c("SELECTED" = "#0072B2", "NOT SELECTED" = "#D55E00", "CHECK" = "#C2185B")

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
      # Separate layers for checks (larger) and candidates (smaller)
      df_candidates <- df_plot[df_plot$plot_status != "CHECK", , drop = FALSE]
      df_checks <- df_plot[df_plot$plot_status == "CHECK", , drop = FALSE]

      if (nrow(df_candidates) > 0) {
        p <- p +
          ggplot2::geom_point(
            data = df_candidates,
            ggplot2::aes(fill = plot_status, alpha = opacity),
            shape = 21, stroke = 0.5, color = "grey40", size = 2.8
          )
      }

      if (nrow(df_checks) > 0) {
        p <- p +
          ggplot2::geom_point(
            data = df_checks,
            ggplot2::aes(fill = plot_status),
            shape = 21, stroke = 1, color = "white", size = 5, alpha = 1
          )
      }

      p <- p +
        ggplot2::scale_fill_manual(values = status_colors) +
        ggplot2::scale_alpha_identity() +
        ggplot2::labs(x = x_trait, y = y_trait, fill = "Status") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(legend.position = "bottom")

      plotly::ggplotly(p, tooltip = "text", source = ns("pairwiseScatter"))
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

      new_status <- if (current_status == "SELECTED") "NOT SELECTED" else "SELECTED"

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

    output$radarDesignationUI <- renderUI({
      plot_obj <- review_plot_data()
      req(plot_obj)

      df <- plot_obj$review_df
      req(nrow(df) > 0)

      df <- df[, c("designation", "plot_status"), drop = FALSE]
      df <- df[!is.na(df$designation), , drop = FALSE]

      df$plot_status[is.na(df$plot_status) | df$plot_status == ""] <- "NOT SELECTED"

      df$label <- paste0(
        df$plot_status,
        " - ",
        df$designation
      )

      choices <- df$designation
      names(choices) <- df$label

      selected <- df$designation[df$plot_status == "CHECK"]
      selected <- selected[1]

      selectizeInput(
        ns("radarDesignations"),
        label = "Designations to display in radar plot",
        choices = choices,
        selected = selected,
        multiple = TRUE,
        options = list(
          maxItems = 4,
          placeholder = "Select up to 4 designations"
        )
      )
    })

    build_radar_plot <- function(df_panel, trait_cols, trait_labels = NULL, title_text = NULL) {
      req(nrow(df_panel) > 0)
      if (is.null(trait_labels)) trait_labels <- trait_cols

      designation_cols <- grDevices::hcl.colors(
        n = nrow(df_panel),
        palette = "Dark 3"
      )

      p <- plotly::plot_ly()

      for (i in seq_len(nrow(df_panel))) {
        values <- as.numeric(df_panel[i, trait_cols, drop = TRUE])

        status_i <- df_panel$plot_status[i]
        if (is.na(status_i) || !nzchar(status_i)) {
          status_i <- "NOT SELECTED"
        }

        color_i <- designation_cols[i]

        trace_name <- paste0(status_i, " - ", df_panel$designation[i])

        p <- p %>%
          plotly::add_trace(
            type = "scatterpolar",
            mode = "lines+markers",
            r = c(values, values[1]),
            theta = c(trait_labels, trait_labels[1]),
            name = trace_name,
            hovertemplate = paste0(
              "Designation: ", df_panel$designation[i],
              "<br>Decision: ", status_i,
              "<br>Trait: %{theta}",
              "<br>Scaled value: %{r:.3f}<extra></extra>"
            ),
            line = list(
              width = 2,
              color = color_i
            ),
            marker = list(
              size = 5,
              color = color_i
            )
          )
      }

      p %>%
        plotly::layout(
          title = title_text,
          polar = list(
            radialaxis = list(
              visible = TRUE,
              range = c(0, 1)
            )
          ),
          showlegend = TRUE
        )
    }

    output$radarPlot <- plotly::renderPlotly({
      plot_obj <- review_plot_data()
      req(plot_obj)

      req(input$radarDesignations)
      req(length(input$radarDesignations) > 0)

      validate(
        need(length(input$radarDesignations) <= 4, "Please select a maximum of 4 designations.")
      )

      df <- plot_obj$review_df
      trait_cols <- plot_obj$traits

      req(length(trait_cols) > 0)

      df_radar <- df[
        df$designation %in% input$radarDesignations,
        c("designation", "plot_status", trait_cols),
        drop = FALSE
      ]

      validate(
        need(nrow(df_radar) > 0, "No selected designations available for radar plot."),
        need(nrow(df_radar) <= 4, "Radar plot supports a maximum of 4 designations.")
      )

      df_radar <- df_radar[stats::complete.cases(df_radar[, trait_cols, drop = FALSE]), , drop = FALSE]

      validate(
        need(nrow(df_radar) > 0, "Selected designations do not have complete trait values for the radar plot.")
      )

      # Get trait directions from modeling table (to invert "lower is better" traits)
      dt <- data()
      modeling_init <- dt$modeling[
        dt$modeling$analysisId %in% input$initialSelectionStamp &
          dt$modeling$module == "Init_prodAdv", , drop = FALSE
      ]
      trait_directions <- list()
      for (tr in trait_cols) {
        dir_row <- modeling_init[modeling_init$trait == tr & modeling_init$parameter == "direction", , drop = FALSE]
        if (nrow(dir_row) > 0) {
          trait_directions[[tr]] <- dir_row$value[1]
        } else {
          trait_directions[[tr]] <- "Higher is better"  # default
        }
      }

      for (tr in trait_cols) {
        x <- df[[tr]]
        rng <- range(x, na.rm = TRUE)

        if (all(is.finite(rng)) && diff(rng) > 0) {
          scaled <- (df_radar[[tr]] - rng[1]) / diff(rng)
          # Invert "lower is better" traits so better always points outward
          if (identical(trait_directions[[tr]], "Lower is better")) {
            scaled <- 1 - scaled
          }
          df_radar[[tr]] <- scaled
        } else {
          df_radar[[tr]] <- 0.5
        }
      }

      # Update trait labels to indicate direction
      radar_labels <- sapply(trait_cols, function(tr) {
        if (identical(trait_directions[[tr]], "Lower is better")) {
          paste0(tr, " (↓)")
        } else {
          tr
        }
      })

      build_radar_plot(
        df_panel = df_radar,
        trait_cols = trait_cols,
        trait_labels = radar_labels,
        title_text = "Radar plot (outward = better)"
      )
    })

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

      p %>%
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

    performance_profile_data <- reactive({
      req(review_plot_data())
      req(data())
      req(input$initialSelectionStamp)

      dt <- data()

      modeling_init <- dt$modeling
      modeling_init <- modeling_init[
        modeling_init$analysisId %in% input$initialSelectionStamp &
          modeling_init$module == "Init_prodAdv",
        ,
        drop = FALSE
      ]

      validate(
        need(nrow(modeling_init) > 0, "No modeling records found for the selected initial selection stamp.")
      )

      check_entry_type_value <- input$checkEntryTypeValue
      if (is.null(check_entry_type_value) || !nzchar(check_entry_type_value)) {
        check_entry_type_value <- NULL
      }

      validate(
        need(
          !identical(input$performanceProfileScale, "% over check") ||
            !is.null(check_entry_type_value),
          "Checks are required to display the performance profile as '% over check'. Use '% over mean' or select a valid check entry type."
        )
      )

      tryCatch(
        {
          cgiarPipeline::buildProdAdvPerformanceProfile(
            mta_long = review_plot_data()$mta_long,
            review_df = review_plot_data()$review_df,
            modeling_df = modeling_init,
            performance_profile_scale = input$performanceProfileScale,
            plot_selection_overrides = plot_selection_overrides()
          )
        },
        error = function(e) {
          validate(need(FALSE, e$message))
        }
      )
    })

    get_performance_profile_page_data <- function(df_plot, page, page_size = 10) {
      if (!is.data.frame(df_plot)) {
        stop("'df_plot' must be a data.frame.")
      }

      required_cols <- c("designation")
      missing_cols <- setdiff(required_cols, colnames(df_plot))
      if (length(missing_cols) > 0) {
        stop(
          "Missing required columns in 'df_plot': ",
          paste(missing_cols, collapse = ", ")
        )
      }

      designation_order <- unique(as.character(df_plot$designation))
      n_designations <- length(designation_order)

      if (n_designations == 0) {
        stop("No designations available for paging.")
      }

      n_pages <- max(1, ceiling(n_designations / page_size))

      if (is.null(page) || !is.finite(page)) {
        page <- 1
      }

      page <- max(1, min(as.integer(page), n_pages))

      start_idx <- ((page - 1) * page_size) + 1
      end_idx <- min(page * page_size, n_designations)

      keep_designations <- designation_order[start_idx:end_idx]

      df_page <- df_plot[df_plot$designation %in% keep_designations, , drop = FALSE]
      df_page$designation <- factor(df_page$designation, levels = keep_designations)

      list(
        df = df_page,
        page = page,
        n_pages = n_pages,
        n_designations = n_designations,
        start_idx = start_idx,
        end_idx = end_idx
      )
    }

    output$performanceProfilePageInfo <- renderUI({
      prof_obj <- performance_profile_data()
      req(prof_obj)

      page_obj <- get_performance_profile_page_data(
        df_plot = prof_obj$plot_df,
        page = input$performanceProfilePage,
        page_size = 10
      )

      tags$div(
        style = "padding-top: 25px;",
        tags$strong(
          paste0(
            "Showing plots ",
            page_obj$start_idx,
            "-",
            page_obj$end_idx,
            " of ",
            page_obj$n_designations
          )
        ),
        if (page_obj$page < page_obj$n_pages) {
          tags$span(paste0("  |  Next page: ", page_obj$page + 1))
        }
      )
    })

    observe({
      prof_obj <- performance_profile_data()
      req(prof_obj)

      n_pages <- max(1, ceiling(length(unique(prof_obj$plot_df$designation)) / 10))

      current_page <- input$performanceProfilePage
      if (is.null(current_page) || !is.finite(current_page)) {
        current_page <- 1
      }

      updateNumericInput(
        session = session,
        inputId = "performanceProfilePage",
        min = 1,
        max = n_pages,
        value = max(1, min(current_page, n_pages))
      )
    })

    output$performanceProfilePlot <- plotly::renderPlotly({
      prof_obj <- performance_profile_data()
      req(prof_obj)

      df_plot <- prof_obj$plot_df
      x_lab <- prof_obj$x_lab

      validate(
        need(length(unique(df_plot$designation)) > 0, "No designations available for performance profile plot."),
        need(length(unique(df_plot$designation)) <= 50, "Per-variety performance profile supports up to 50 designations.")
      )

      page_obj <- get_performance_profile_page_data(
        df_plot = df_plot,
        page = input$performanceProfilePage,
        page_size = 10
      )

      df_page <- page_obj$df
      df_page$trait <- factor(df_page$trait, levels = rev(unique(df_page$trait)))

      df_page$sign_class <- ifelse(
        df_page$profile_value_clipped > 0, "POS",
        ifelse(df_page$profile_value_clipped < 0, "NEG", "ZERO")
      )

      p <- ggplot2::ggplot(
        df_page,
        ggplot2::aes(
          x = profile_value_clipped,
          y = trait,
          fill = sign_class,
          text = paste0(
            "Designation: ", designation,
            "<br>Trait: ", trait,
            "<br>", x_lab, ": ", round(profile_value, 1), "%",
            ifelse(
              x_lab == "% over check",
              paste0("<br>Reference check: ", reference_check_used),
              ""
            ),
            "<br>Status: ", plot_status
          )
        )
      ) +
        ggplot2::geom_col(width = 0.75, alpha = 1) +
        ggplot2::geom_vline(
          xintercept = 0,
          linetype = "dashed",
          linewidth = 0.5,
          color = "black"
        ) +
        ggplot2::facet_wrap(~ designation, nrow = 2, ncol = 5, scales = "fixed") +
        ggplot2::scale_x_continuous(
          limits = c(-50, 50),
          breaks = c(-50, -25, 0, 25, 50),
          oob = scales::squish
        ) +
        ggplot2::scale_fill_manual(
          values = c(
            "POS" = "#1B9E77",
            "NEG" = "#D95F02",
            "ZERO" = "#BDBDBD"
          ),
          drop = FALSE,
          guide = "none"
        ) +
        ggplot2::labs(
          x = x_lab,
          y = NULL
        ) +
        ggplot2::theme_bw(base_size = 12) +
        ggplot2::theme(
          panel.border = ggplot2::element_rect(
            colour = "black",
            fill = NA,
            linewidth = 0.8
          ),
          panel.background = ggplot2::element_rect(
            fill = "white",
            colour = NA
          ),
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_line(
            colour = "#D9D9D9",
            linewidth = 0.3
          ),
          strip.background = ggplot2::element_rect(
            fill = "#D9D9D9",
            colour = "black",
            linewidth = 0.8
          ),
          strip.text = ggplot2::element_text(
            face = "bold",
            size = 11,
            colour = "black"
          ),
          axis.text.y = ggplot2::element_text(
            colour = "black",
            size = 10
          ),
          axis.text.x = ggplot2::element_text(
            colour = "black",
            size = 9
          ),
          axis.title.x = ggplot2::element_text(
            face = "bold",
            colour = "black"
          ),
          axis.title.y = ggplot2::element_blank(),
          legend.position = "none",
          panel.spacing = grid::unit(0.9, "lines")
        )

      plotly::ggplotly(p, tooltip = "text")
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
            "CHECK" = "#C2185B"
          ),
          drop = FALSE
        ) +
        ggplot2::scale_alpha_manual(
          values = c(
            "SELECTED" = 0.6,
            "NOT SELECTED" = 0.6,
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
              plotly::plotlyOutput(ns("pairwiseScatterPlot"), height = "650px")
            )
          )
        )
      }

      if ("Radar plot" %in% selected_plots) {
        ui_list <- c(
          ui_list,
          list(
            shinydashboard::box(
              width = 12,
              title = "Radar plot",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE,
              plotly::plotlyOutput(ns("radarPlot"), height = "700px")
            )
          )
        )
      }

      if ("Performance across locations" %in% selected_plots) {
        ui_list <- c(
          ui_list,
          list(
            fluidRow(
              column(
                width = 12,
                shinydashboard::box(
                  width = 12,
                  title = "Performance across locations (lollipop)",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  plotly::plotlyOutput(ns("lollipopPlot"), height = "700px")
                )
              )
            ),
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
            )
          )
        )
      }

      if ("Per-variety trait performance profile" %in% selected_plots) {
        ui_list <- c(
          ui_list,
          list(
            shinydashboard::box(
              width = 12,
              title = "Per-variety trait performance profile",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE,
              plotly::plotlyOutput(ns("performanceProfilePlot"), height = "900px")
            )
          )
        )
      }

      if ("Stability/adaptability plot" %in% selected_plots) {
        ui_list <- c(
          ui_list,
          list(
            shinydashboard::box(
              width = 12,
              title = "Stability/adaptability plot",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE,
              plotly::plotlyOutput(ns("stabilityPlot"), height = "650px")
            ),
            shinydashboard::box(
              width = 12,
              status = "info",
              solidHeader = FALSE,
              collapsible = TRUE,
              collapsed = TRUE,
              title = "Stability plot information",
              uiOutput(ns("stabilityPlotMessage"))
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
              tags$p(style = "color:orange; padding:10px;",
                     icon("triangle-exclamation"),
                     " This plot requires pedigree information. Full implementation in Phase 4 completion."),
              tags$p("Will show family clustering with over-represented families highlighted among selected candidates.")
            )
          )
        )
      }

      do.call(tagList, ui_list)
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

    observeEvent(input$saveFinalSelection, {
      req(data())
      req(input$initialSelectionStamp)

      final_tbl <- final_decision_table_raw()
      req(nrow(final_tbl) > 0)

      analysis_id <- as.numeric(Sys.time())

      final_modifications <- data.frame(
        module = "Final_prodAdv",
        analysisId = analysis_id,
        designation = final_tbl$designation,
        reason = "final_manual_decision",
        value = final_tbl$final_decision,
        stringsAsFactors = FALSE
      )

      final_status <- data.frame(
        module = "Final_prodAdv",
        analysisId = analysis_id,
        analysisIdName = if (nzchar(trimws(input$finalSelectionId))) trimws(input$finalSelectionId) else NA_character_,
        stringsAsFactors = FALSE
      )

      dt_object <- data()

      if (is.null(dt_object$modifications$selection)) {
        dt_object$modifications$selection <- final_modifications
      } else {
        dt_object$modifications$selection <- rbind(
          dt_object$modifications$selection,
          final_modifications
        )
      }

      if (is.null(dt_object$status)) {
        dt_object$status <- final_status
      } else {
        dt_object$status <- rbind(
          dt_object$status,
          final_status
        )
      }

      data(dt_object)

      final_decision_overrides(
        final_tbl[, c("designation", "final_decision"), drop = FALSE]
      )

      # rebuild final stamp choices and select the newly saved final stamp
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

      # intentionally do NOT touch plotSelectionStamp here

      showNotification(
        "Final decision table saved successfully.",
        type = "message"
      )
    }, ignoreInit = TRUE)

    ###########################################
    # Final report (idx7)
    ###########################################

    observeEvent(data(), {
      req(data())

      dt <- data()$status

      dtInitSel  <- dt[dt$module == "Init_prodAdv",  , drop = FALSE]
      dtPlotSel  <- dt[dt$module == "Plot_prodAdv",  , drop = FALSE]
      dtFinalSel <- dt[dt$module == "Final_prodAdv", , drop = FALSE]

      stampsInitSel  <- make_stamp_choices(dtInitSel)
      stampsPlotSel  <- make_stamp_choices(dtPlotSel)
      stampsFinalSel <- make_stamp_choices(dtFinalSel)

      updateSelectInput(
        session,
        "reportInitialSelectionStamp",
        choices = stampsInitSel,
        selected = if (length(stampsInitSel) > 0) unname(stampsInitSel)[1] else NULL
      )

      updateSelectInput(
        session,
        "reportPlotSelectionStamp",
        choices = c("No plot selection" = "__none__", stampsPlotSel),
        selected = "__none__"
      )

      updateSelectInput(
        session,
        "reportFinalSelectionStamp",
        choices = c("No final selection" = "__none__", stampsFinalSel),
        selected = "__none__"
      )
    })

    final_report_table_data <- reactive({
      build_decision_table_data(
        dt = data(),
        initial_stamp = input$reportInitialSelectionStamp,
        plot_stamp = input$reportPlotSelectionStamp,
        final_stamp = input$reportFinalSelectionStamp,
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

      src <- normalizePath(system.file("rmd","reportProdAdv.Rmd", package = "bioflow"))

      tmp_report <- file.path(tempdir(), "reportProdAdv_download.Rmd")
      tmp_rdata  <- file.path(tempdir(), "resultProdAdv.RData")

      save(result, final_table_export, file = tmp_rdata)
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

    observeEvent(input$runFinalProdAdv, {

      req(input$reportInitialSelectionStamp)
      req(input$reportPlotSelectionStamp)
      req(input$reportFinalSelectionStamp)

      shinybusy::show_modal_spinner(spin = "fading-circle", text = "Generating Report...")

      result <- data()
      final_table_export <- final_report_table_raw()

      src <- normalizePath(system.file("rmd","reportProdAdv.Rmd", package = "bioflow"))

      tmp_report <- file.path(tempdir(), "reportProdAdv_tmp.Rmd")
      tmp_rdata  <- file.path(tempdir(), "resultProdAdv.RData")

      save(result, final_table_export, file = tmp_rdata)
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





  })
}

## To be copied in the UI
# mod_qaRawApp_ui("qaPhenoApp_1")

## To be copied in the server
# mod_qaRawApp_server("qaPhenoApp_1")

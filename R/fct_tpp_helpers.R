# =============================================================================
# fct_tpp_helpers.R
#
# Consolidated TPP (Target Product Profile) helper functions for the bioflow
# package. Contains pure utility functions used by the Pre-PAM, Advancement
# Meeting, and TPP data retrieval modules.
#
# Sections:
#   1. Desired Score Parser
#   2. Criteria Evaluation
#   3. Criteria List Builder
#   4. Scale Validation
#   5. Weight Assignment
#   6. Breakdown Table (population statistics)
#   7. Breakdown Detail Table (compliance reporting)
#   8. Population Context (Advancement Meeting)
# =============================================================================


# =============================================================================
# SECTION 1: Desired Score Parser
# =============================================================================

#' Parse a free-text "Desired Score" string into a structured threshold spec
#'
#' Recognized patterns:
#' - "Lower than X" / "Less than X" -> absolute, upper_bound = X, direction = "lower"
#' - "Higher than X" / "Greater than X" / "More than X" -> absolute, lower_bound = X, direction = "higher"
#' - "Within range (... to...) X to Y" -> absolute, lower_bound = X, upper_bound = Y
#' - "Percentage above check X" -> relative, pct_above_check = X
#' - "Equal to at least X% of check" -> relative, pct_of_check = X
#' - "Equal to -<X% commercial check" / "Equal to -<X% check" -> relative, pct_above_check = -X
#' - "Same or better than check" -> relative, pct_above_check = 0
#' - "Equal to <qualitative>" (no number) -> qualitative, not parseable
#'
#' @param desired_score_text Character string with the desired score description
#' @return A list with:
#'   \itemize{
#'     \item \code{score_type}: "absolute", "relative", or NA (unparseable)
#'     \item \code{lower_bound}: numeric lower bound (or NA)
#'     \item \code{upper_bound}: numeric upper bound (or NA)
#'     \item \code{direction}: "higher" or "lower" (or NA)
#'     \item \code{pct_above_check}: numeric percentage above check (for relative; or NA)
#'     \item \code{pct_of_check}: numeric percentage of check value (for relative; or NA)
#'     \item \code{original_text}: the original text for reference
#'   }
#' @noRd
tpp_parse_desired_score <- function(desired_score_text) {
  result <- list(
    score_type = NA_character_,
    lower_bound = NA_real_,
    upper_bound = NA_real_,
    direction = NA_character_,
    pct_above_check = NA_real_,
    pct_of_check = NA_real_,
    original_text = desired_score_text
  )

  if (is.null(desired_score_text) || is.na(desired_score_text) ||
      !is.character(desired_score_text) || !nzchar(trimws(desired_score_text))) {
    return(result)
  }

  txt <- trimws(desired_score_text)
  txt_lower <- tolower(txt)

  # --- Pattern: "Within range (... to...) X to Y" ---
  range_match <- regmatches(txt, regexec(
    "(?i)within\\s+range.*?([\\-]?[0-9]*\\.?[0-9]+)\\s*to\\s*([\\-]?[0-9]*\\.?[0-9]+)",
    txt, perl = TRUE
  ))[[1]]
  if (length(range_match) == 3) {
    result$score_type <- "absolute"
    result$lower_bound <- as.numeric(range_match[2])
    result$upper_bound <- as.numeric(range_match[3])
    result$direction <- "higher"
    return(result)
  }

  # --- Pattern: "Percentage above check X" ---
  pct_above_match <- regmatches(txt, regexec(
    "(?i)percentage\\s+above\\s+check\\s+([\\-]?[0-9]*\\.?[0-9]+)",
    txt, perl = TRUE
  ))[[1]]
  if (length(pct_above_match) == 2) {
    result$score_type <- "relative"
    result$pct_above_check <- as.numeric(pct_above_match[2])
    result$direction <- "higher"
    return(result)
  }

  # --- Pattern: "Equal to at least X% of check" ---
  at_least_pct_match <- regmatches(txt, regexec(
    "(?i)(?:equal\\s+to\\s+)?at\\s+least\\s+([0-9]*\\.?[0-9]+)\\s*%\\s*(?:of\\s+)?check",
    txt, perl = TRUE
  ))[[1]]
  if (length(at_least_pct_match) == 2) {
    result$score_type <- "relative"
    result$pct_of_check <- as.numeric(at_least_pct_match[2])
    result$direction <- "higher"
    return(result)
  }

  # --- Pattern: "Equal to -<X% commercial check" or "Equal to -<X% check" ---
  neg_pct_match <- regmatches(txt, regexec(
    "(?i)equal\\s+to\\s+\\-?<?\\s*([0-9]*\\.?[0-9]+)\\s*%\\s*(?:commercial\\s+)?check",
    txt, perl = TRUE
  ))[[1]]
  if (length(neg_pct_match) == 2) {
    result$score_type <- "relative"
    result$pct_above_check <- -as.numeric(neg_pct_match[2])
    result$direction <- "higher"
    return(result)
  }

  # --- Pattern: "Same or better than check" ---
  if (grepl("(?i)same\\s+(or\\s+)?better\\s+than\\s+check", txt, perl = TRUE)) {
    result$score_type <- "relative"
    result$pct_above_check <- 0
    result$direction <- "higher"
    return(result)
  }

  # --- Pattern: "Lower than X" / "Less than X" / "Below X" ---
  lower_match <- regmatches(txt, regexec(
    "(?i)(?:lower|less|below)\\s+than\\s+([\\-]?[0-9]*\\.?[0-9]+)",
    txt, perl = TRUE
  ))[[1]]
  if (length(lower_match) == 2) {
    result$score_type <- "absolute"
    result$upper_bound <- as.numeric(lower_match[2])
    result$direction <- "lower"
    return(result)
  }

  # --- Pattern: "Higher than X" / "Greater than X" / "More than X" / "Above X" ---
  higher_match <- regmatches(txt, regexec(
    "(?i)(?:higher|greater|more|above)\\s+than\\s+([\\-]?[0-9]*\\.?[0-9]+)",
    txt, perl = TRUE
  ))[[1]]
  if (length(higher_match) == 2) {
    result$score_type <- "absolute"
    result$lower_bound <- as.numeric(higher_match[2])
    result$direction <- "higher"
    return(result)
  }

  # --- Pattern: "Equal to X" (numeric only) ---
  equal_numeric_match <- regmatches(txt, regexec(
    "(?i)equal\\s+to\\s+([\\-]?[0-9]*\\.?[0-9]+)$",
    txt, perl = TRUE
  ))[[1]]
  if (length(equal_numeric_match) == 2) {
    val <- as.numeric(equal_numeric_match[2])
    result$score_type <- "absolute"
    result$lower_bound <- val
    result$upper_bound <- val
    result$direction <- "higher"
    return(result)
  }

  # --- If no pattern matched, return as unparseable ---
  return(result)
}


# =============================================================================
# SECTION 2: Criteria Evaluation
# =============================================================================

#' Evaluate whether an individual meets TPP criteria for a single trait
#'
#' Determines if a trait value meets the TPP desired score specification.
#' For absolute type: value >= lower AND value <= upper.
#' For relative type: (value - check_mean) is evaluated against lower/upper bounds.
#' NA values are treated as NOT meeting criteria (returns FALSE).
#'
#' @param value Numeric trait value for the individual.
#' @param desired_score List with elements:
#'   \itemize{
#'     \item type: Character, either "absolute" or "relative"
#'     \item lower: Numeric lower bound (inclusive)
#'     \item upper: Numeric upper bound (inclusive)
#'     \item check_mean: Numeric mean of check values (required for relative type)
#'   }
#' @return Logical TRUE if criteria met, FALSE otherwise (including for NA values).
#' @noRd
tpp_meets_criteria <- function(value, desired_score) {
  tryCatch(
    {
      if (is.null(value) || length(value) != 1 || is.na(value)) {
        return(FALSE)
      }

      if (is.null(desired_score) || !is.list(desired_score)) {
        return(FALSE)
      }

      score_type <- desired_score$type
      lower <- desired_score$lower
      upper <- desired_score$upper

      if (is.null(score_type) || !is.character(score_type) ||
          length(score_type) != 1 || is.na(score_type)) {
        return(FALSE)
      }

      if (is.null(lower) || is.null(upper) ||
          !is.numeric(lower) || !is.numeric(upper) ||
          length(lower) != 1 || length(upper) != 1 ||
          is.na(lower) || is.na(upper)) {
        return(FALSE)
      }

      if (score_type == "absolute") {
        return(value >= lower && value <= upper)

      } else if (score_type == "relative") {
        check_mean <- desired_score$check_mean

        if (is.null(check_mean) || !is.numeric(check_mean) ||
            length(check_mean) != 1 || is.na(check_mean)) {
          return(FALSE)
        }

        relative_value <- value - check_mean
        return(relative_value >= lower && relative_value <= upper)

      } else {
        return(FALSE)
      }
    },
    error = function(e) {
      warning(paste("tpp_meets_criteria: error evaluating criteria -", e$message))
      return(FALSE)
    }
  )
}

#' Evaluate TPP criteria across all traits for a matrix of individuals
#'
#' Evaluates whether each individual meets TPP criteria for each trait that
#' matches the specified category filter. Returns a data.frame with a
#' designation column and one logical column per evaluated trait.
#'
#' @param predictions_df Data.frame with columns: designation, plus one or more
#'   numeric trait columns containing predicted values.
#' @param tpp_criteria Named list of desired_score specs keyed by trait name.
#'   Each element is a list with fields: type, lower, upper, check_mean (for
#'   relative), and category.
#' @param category_filter Character vector of categories to evaluate
#'   (default: "Essential_Improve"). Only traits whose category field is in
#'   this vector will be evaluated.
#' @return Data.frame with designation column and one logical column per
#'   evaluated trait. Returns a data.frame with only the designation column if
#'   no traits match the category filter. Returns an empty data.frame (0 rows)
#'   if predictions_df is empty or NULL.
#' @noRd
tpp_evaluate_all_criteria <- function(predictions_df, tpp_criteria,
                                       category_filter = "Essential_Improve") {
  tryCatch(
    {
      if (is.null(predictions_df) || !is.data.frame(predictions_df)) {
        return(data.frame(designation = character(0), stringsAsFactors = FALSE))
      }

      if (nrow(predictions_df) == 0) {
        return(data.frame(designation = character(0), stringsAsFactors = FALSE))
      }

      if (!"designation" %in% colnames(predictions_df)) {
        warning("tpp_evaluate_all_criteria: predictions_df must have a 'designation' column")
        return(data.frame(designation = character(0), stringsAsFactors = FALSE))
      }

      if (is.null(tpp_criteria) || !is.list(tpp_criteria) || length(tpp_criteria) == 0) {
        result <- data.frame(
          designation = predictions_df$designation,
          stringsAsFactors = FALSE
        )
        return(result)
      }

      # Filter traits by category
      trait_names <- names(tpp_criteria)
      matching_traits <- character(0)

      for (trait_name in trait_names) {
        spec <- tpp_criteria[[trait_name]]
        trait_category <- spec$category
        if (!is.null(trait_category) && !is.na(trait_category) &&
            trait_category %in% category_filter) {
          if (trait_name %in% colnames(predictions_df)) {
            matching_traits <- c(matching_traits, trait_name)
          }
        }
      }

      result <- data.frame(
        designation = predictions_df$designation,
        stringsAsFactors = FALSE
      )

      if (length(matching_traits) == 0) {
        return(result)
      }

      for (trait_name in matching_traits) {
        spec <- tpp_criteria[[trait_name]]
        trait_values <- predictions_df[[trait_name]]

        meets <- vapply(trait_values, function(val) {
          tpp_meets_criteria(val, spec)
        }, logical(1))

        result[[trait_name]] <- meets
      }

      return(result)
    },
    error = function(e) {
      warning(paste("tpp_evaluate_all_criteria: error evaluating criteria -", e$message))
      return(data.frame(designation = character(0), stringsAsFactors = FALSE))
    }
  )
}

#' Count individuals meeting all criteria for a given category
#'
#' Takes a criteria matrix (output from tpp_evaluate_all_criteria) and counts
#' how many individuals meet ALL evaluated trait criteria simultaneously.
#'
#' @param criteria_matrix Data.frame from tpp_evaluate_all_criteria with a
#'   designation column and one or more logical trait columns.
#' @return List with:
#'   \itemize{
#'     \item n_meeting: Integer count of individuals meeting all criteria
#'     \item n_total: Integer total number of individuals
#'     \item pct: Numeric percentage (0-100) of individuals meeting all criteria
#'   }
#' @noRd
tpp_count_fully_compliant <- function(criteria_matrix) {
  tryCatch(
    {
      if (is.null(criteria_matrix) || !is.data.frame(criteria_matrix)) {
        return(list(n_meeting = 0L, n_total = 0L, pct = 0))
      }

      if (nrow(criteria_matrix) == 0) {
        return(list(n_meeting = 0L, n_total = 0L, pct = 0))
      }

      n_total <- nrow(criteria_matrix)

      all_cols <- colnames(criteria_matrix)
      trait_cols <- setdiff(all_cols, "designation")

      if (length(trait_cols) == 0) {
        return(list(
          n_meeting = as.integer(n_total),
          n_total = as.integer(n_total),
          pct = 100
        ))
      }

      all_met <- vapply(seq_len(n_total), function(i) {
        row_vals <- criteria_matrix[i, trait_cols, drop = FALSE]
        all(unlist(row_vals) == TRUE)
      }, logical(1))

      n_meeting <- sum(all_met)
      pct <- (n_meeting / n_total) * 100

      return(list(
        n_meeting = as.integer(n_meeting),
        n_total = as.integer(n_total),
        pct = pct
      ))
    },
    error = function(e) {
      warning(paste("tpp_count_fully_compliant: error counting compliant individuals -", e$message))
      return(list(n_meeting = 0L, n_total = 0L, pct = 0))
    }
  )
}


# =============================================================================
# SECTION 3: Criteria List Builder
# =============================================================================

#' Build TPP Criteria List from Filtered Traits Data Frame
#'
#' Shared utility function that converts the flat tpp_filtered_traits
#' data.frame into the named list format expected by tpp_evaluate_all_criteria.
#'
#' @param filtered_df Data.frame of mapped TPP traits with columns:
#'   pheno_trait, category, score_type, desired_lower, desired_upper,
#'   and optionally tpp_trait, pct_above_check, pct_of_check.
#' @param predictions_tbl Data.frame of predictions (wide format with designation
#'   column and trait columns). Used to compute check_mean for relative traits.
#' @param checks_per_trait Named list where keys are tpp_trait names and values
#'   are character vectors of check designation names.
#' @return Named list of desired_score specifications keyed by trait column name.
#' @noRd
tpp_build_criteria_list_from_filtered <- function(filtered_df, predictions_tbl = NULL,
                                                   checks_per_trait = NULL) {
  if (is.null(filtered_df) || !is.data.frame(filtered_df) || nrow(filtered_df) == 0) {
    return(list())
  }

  required_cols <- c("pheno_trait", "category", "score_type", "desired_lower", "desired_upper")
  if (!all(required_cols %in% colnames(filtered_df))) {
    return(list())
  }

  pred_cols <- if (!is.null(predictions_tbl) && is.data.frame(predictions_tbl)) {
    colnames(predictions_tbl)
  } else {
    character(0)
  }

  has_tpp_trait_col <- "tpp_trait" %in% colnames(filtered_df)
  has_pct_above <- "pct_above_check" %in% colnames(filtered_df)
  has_pct_of <- "pct_of_check" %in% colnames(filtered_df)

  criteria_list <- list()
  for (i in seq_len(nrow(filtered_df))) {
    row <- filtered_df[i, , drop = FALSE]
    pheno_trait <- row$pheno_trait
    if (is.na(pheno_trait) || !nzchar(pheno_trait)) next

    score_type <- if (!is.na(row$score_type)) row$score_type else "absolute"
    lower <- if (!is.na(row$desired_lower)) row$desired_lower else -Inf
    upper <- if (!is.na(row$desired_upper)) row$desired_upper else Inf
    category <- if (!is.na(row$category)) row$category else "Essential_Improve"

    trait_key <- pheno_trait
    tpp_name <- NA_character_
    if (has_tpp_trait_col) {
      tpp_name <- row$tpp_trait
      if (!is.na(tpp_name) && nzchar(tpp_name) && tpp_name != pheno_trait) {
        if (tpp_name %in% pred_cols) {
          trait_key <- tpp_name
        }
      }
    }

    spec <- list(
      type = score_type,
      lower = lower,
      upper = upper,
      category = category
    )

    # For relative score types, convert to absolute bounds using check mean
    if (score_type == "relative" && !is.null(predictions_tbl) && trait_key %in% pred_cols) {
      pct_above <- if (has_pct_above && !is.na(row$pct_above_check)) row$pct_above_check else NA_real_
      pct_of <- if (has_pct_of && !is.na(row$pct_of_check)) row$pct_of_check else NA_real_

      check_desigs <- NULL
      if (!is.null(checks_per_trait)) {
        lookup_name <- if (!is.na(tpp_name) && nzchar(tpp_name)) tpp_name else pheno_trait
        check_desigs <- checks_per_trait[[lookup_name]]
      }
      if (is.null(check_desigs) && "checks" %in% colnames(filtered_df)) {
        check_desigs <- row$checks
        if (is.list(check_desigs)) check_desigs <- check_desigs[[1]]
      }

      check_mean <- NA_real_
      if (!is.null(check_desigs) && length(check_desigs) > 0) {
        check_vals <- predictions_tbl[[trait_key]][
          predictions_tbl$designation %in% check_desigs
        ]
        check_vals <- as.numeric(check_vals[!is.na(check_vals)])
        if (length(check_vals) > 0) {
          check_mean <- mean(check_vals)
        }
      }

      if (!is.na(check_mean) && is.finite(check_mean)) {
        if (!is.na(pct_above) && is.finite(pct_above)) {
          target <- check_mean * (1 + pct_above / 100)
          spec <- list(type = "absolute", lower = target, upper = Inf, category = category)
        } else if (!is.na(pct_of) && is.finite(pct_of)) {
          target <- check_mean * (pct_of / 100)
          spec <- list(type = "absolute", lower = target, upper = Inf, category = category)
        } else if (is.finite(lower) || is.finite(upper)) {
          spec$check_mean <- check_mean
        } else {
          spec <- list(type = "absolute", lower = -Inf, upper = Inf, category = category)
        }
      }
    }

    criteria_list[[trait_key]] <- spec
  }
  return(criteria_list)
}


# =============================================================================
# SECTION 4: Scale Validation
# =============================================================================

#' Parse a "Scale Option" text value into numeric min/max bounds
#'
#' Extracts numeric lower and upper bounds from a text string representing a
#' scale range. Supports formats like "1 to 5", "1-5", "1 - 9".
#'
#' @param scale_option Character string from the "Scale Option" column.
#' @return A list with elements \code{min} and \code{max}, or NULL if unparseable.
#' @noRd
tpp_parse_scale_option <- function(scale_option) {
  if (is.null(scale_option) || length(scale_option) != 1 || is.na(scale_option)) {
    return(NULL)
  }

  scale_option <- trimws(as.character(scale_option))

  if (nchar(scale_option) == 0) {
    return(NULL)
  }

  # Try pattern: "<number> to <number>"
  match_to <- regmatches(scale_option,
    regexec("^\\s*([0-9]*\\.?[0-9]+)\\s+to\\s+([0-9]*\\.?[0-9]+)\\s*$",
            scale_option, ignore.case = TRUE))[[1]]

  if (length(match_to) == 3) {
    min_val <- suppressWarnings(as.numeric(match_to[2]))
    max_val <- suppressWarnings(as.numeric(match_to[3]))
    if (!is.na(min_val) && !is.na(max_val)) {
      return(list(min = min(min_val, max_val), max = max(min_val, max_val)))
    }
  }

  # Try pattern: "<number> - <number>" or "<number>-<number>"
  match_dash <- regmatches(scale_option,
    regexec("^\\s*([0-9]*\\.?[0-9]+)\\s*-\\s*([0-9]*\\.?[0-9]+)\\s*$",
            scale_option))[[1]]

  if (length(match_dash) == 3) {
    min_val <- suppressWarnings(as.numeric(match_dash[2]))
    max_val <- suppressWarnings(as.numeric(match_dash[3]))
    if (!is.na(min_val) && !is.na(max_val)) {
      return(list(min = min(min_val, max_val), max = max(min_val, max_val)))
    }
  }

  return(NULL)
}

#' Compute percentage of values outside TPP scale range
#'
#' @param values Numeric vector of phenotypic values (may contain NAs).
#' @param scale_min Numeric lower bound of TPP scale (inclusive).
#' @param scale_max Numeric upper bound of TPP scale (inclusive).
#' @return Numeric percentage (0-100) of non-missing values outside range.
#' @noRd
tpp_compute_scale_violation_pct <- function(values, scale_min, scale_max) {
  if (!is.numeric(values)) {
    values <- suppressWarnings(as.numeric(values))
  }

  if (!is.numeric(scale_min) || !is.numeric(scale_max) ||
      length(scale_min) != 1 || length(scale_max) != 1 ||
      is.na(scale_min) || is.na(scale_max)) {
    stop("scale_min and scale_max must be single non-NA numeric values")
  }

  if (scale_min > scale_max) {
    stop(paste0(
      "Invalid scale configuration: scale_min (", scale_min,
      ") is greater than scale_max (", scale_max, ")"
    ))
  }

  non_na_values <- values[!is.na(values)]

  if (length(non_na_values) == 0) {
    return(0)
  }

  n_outside <- sum(non_na_values < scale_min | non_na_values > scale_max)
  n_total <- length(non_na_values)

  pct <- (n_outside / n_total) * 100
  return(pct)
}

#' Check if a trait mapping has a scale violation
#'
#' @param values Numeric vector of phenotypic values (may contain NAs).
#' @param scale_min Numeric lower bound of TPP scale (inclusive).
#' @param scale_max Numeric upper bound of TPP scale (inclusive).
#' @param threshold Numeric violation threshold percentage (default 10.0).
#' @return Logical TRUE if violation exists (percentage > threshold), FALSE otherwise.
#' @noRd
tpp_has_scale_violation <- function(values, scale_min, scale_max, threshold = 10.0) {
  pct <- tpp_compute_scale_violation_pct(values, scale_min, scale_max)
  return(pct > threshold)
}

#' Format scale violation warning message
#'
#' @param trait_name Character string with the trait name.
#' @param scale_min Numeric lower bound of TPP scale.
#' @param scale_max Numeric upper bound of TPP scale.
#' @param pct_outside Numeric percentage of values outside the scale range.
#' @return Character string warning message.
#' @noRd
tpp_format_scale_warning <- function(trait_name, scale_min, scale_max, pct_outside) {
  if (is.null(trait_name) || !is.character(trait_name) || length(trait_name) != 1 || is.na(trait_name)) {
    trait_name <- "Unknown trait"
  }

  pct_formatted <- sprintf("%.1f", pct_outside)

  msg <- paste0(
    "Scale violation for '", trait_name, "': ",
    pct_formatted, "% of values fall outside the TPP scale range [",
    scale_min, ", ", scale_max, "]."
  )

  return(msg)
}


# =============================================================================
# SECTION 5: Weight Assignment
# =============================================================================

#' Assign weights based on TPP categories and desired score direction
#'
#' - Essential_Improve with direction "lower": weight = -1
#' - Essential_Improve with any other direction: weight = +1
#' - Essential_Maintain: weight = 0
#' - Nice_To_Have: weight = 0
#'
#' @param tpp_traits_df Data.frame with columns: trait_name, category, desired_direction
#' @return Data.frame with all original columns plus: weight (numeric)
#' @noRd
tpp_assign_weights <- function(tpp_traits_df) {
  tryCatch(
    {
      if (is.null(tpp_traits_df) || !is.data.frame(tpp_traits_df)) {
        warning("tpp_assign_weights: tpp_traits_df must be a data.frame")
        return(data.frame(
          trait_name = character(0),
          category = character(0),
          desired_direction = character(0),
          weight = numeric(0),
          stringsAsFactors = FALSE
        ))
      }

      if (nrow(tpp_traits_df) == 0) {
        tpp_traits_df$weight <- numeric(0)
        return(tpp_traits_df)
      }

      required_cols <- c("category", "desired_direction")
      missing_cols <- setdiff(required_cols, colnames(tpp_traits_df))
      if (length(missing_cols) > 0) {
        warning(paste("tpp_assign_weights: missing required columns:",
                      paste(missing_cols, collapse = ", ")))
        tpp_traits_df$weight <- rep(0, nrow(tpp_traits_df))
        return(tpp_traits_df)
      }

      weight <- vapply(seq_len(nrow(tpp_traits_df)), function(i) {
        cat_val <- tpp_traits_df$category[i]
        dir_val <- tpp_traits_df$desired_direction[i]

        if (is.na(cat_val)) {
          return(0)
        }

        if (cat_val == "Essential_Improve") {
          if (!is.na(dir_val) && dir_val == "lower") {
            return(-1)
          } else {
            return(1)
          }
        } else if (cat_val == "Essential_Maintain") {
          return(0)
        } else {
          return(0)
        }
      }, numeric(1))

      tpp_traits_df$weight <- weight
      return(tpp_traits_df)
    },
    error = function(e) {
      warning(paste("tpp_assign_weights: error assigning weights -", e$message))
      if (is.data.frame(tpp_traits_df)) {
        tpp_traits_df$weight <- rep(0, nrow(tpp_traits_df))
        return(tpp_traits_df)
      }
      return(data.frame(
        trait_name = character(0),
        category = character(0),
        desired_direction = character(0),
        weight = numeric(0),
        stringsAsFactors = FALSE
      ))
    }
  )
}

#' Determine trait selection based on TPP categories
#'
#' - Essential_Improve: TRUE (selected)
#' - Essential_Maintain: TRUE (selected)
#' - Nice_To_Have: FALSE (not selected)
#'
#' @param tpp_traits_df Data.frame with column: category
#' @return Logical vector (TRUE for Essential_Improve and Essential_Maintain)
#' @noRd
tpp_select_traits <- function(tpp_traits_df) {
  tryCatch(
    {
      if (is.null(tpp_traits_df) || !is.data.frame(tpp_traits_df)) {
        warning("tpp_select_traits: tpp_traits_df must be a data.frame")
        return(logical(0))
      }

      if (nrow(tpp_traits_df) == 0) {
        return(logical(0))
      }

      if (!"category" %in% colnames(tpp_traits_df)) {
        warning("tpp_select_traits: missing required column 'category'")
        return(rep(FALSE, nrow(tpp_traits_df)))
      }

      selected <- vapply(tpp_traits_df$category, function(cat_val) {
        if (is.na(cat_val)) {
          return(FALSE)
        }
        return(cat_val %in% c("Essential_Improve", "Essential_Maintain"))
      }, logical(1))

      return(selected)
    },
    error = function(e) {
      warning(paste("tpp_select_traits: error determining selection -", e$message))
      if (is.data.frame(tpp_traits_df)) {
        return(rep(FALSE, nrow(tpp_traits_df)))
      }
      return(logical(0))
    }
  )
}


# =============================================================================
# SECTION 6: Breakdown Table (population statistics)
# =============================================================================

#' Build the full TPP breakdown table for dashboard display
#'
#' Constructs a data.frame summarizing all TPP traits (mapped and unmapped) with
#' population statistics, selected population statistics, and per-trait criteria
#' percentages.
#'
#' @param tpp_data Data.frame of all TPP traits with columns:
#'   tpp_id, trait_id, tpp_trait, category, pheno_trait (NA for unmapped)
#' @param predictions_df Data.frame of individual predictions with columns:
#'   designation, plus numeric trait columns
#' @param selected_designations Character vector of selected/advanced designations
#' @param check_designations Character vector of check entry designations
#' @param tpp_criteria Named list of desired_score specs keyed by trait name.
#' @param trait_map Named character vector mapping TPP trait names to pheno trait names
#' @return Data.frame with columns: TPP_ID, Trait_ID, Trait_Name, Trait_Requirement,
#'   Mapped, Pop_Min, Pop_Max, Pop_Mean, Sel_Min, Sel_Max, Sel_Mean, Pct_Meeting_Criteria
#' @noRd
tpp_build_breakdown_table <- function(tpp_data, predictions_df, selected_designations,
                                       check_designations, tpp_criteria, trait_map) {
  tryCatch(
    {
      if (is.null(tpp_data) || !is.data.frame(tpp_data) || nrow(tpp_data) == 0) {
        return(.tpp_empty_breakdown())
      }

      required_cols <- c("tpp_id", "trait_id", "tpp_trait", "category")
      if (!all(required_cols %in% colnames(tpp_data))) {
        warning("tpp_build_breakdown_table: tpp_data missing required columns")
        return(.tpp_empty_breakdown())
      }

      if (is.null(predictions_df) || !is.data.frame(predictions_df)) {
        predictions_df <- data.frame(designation = character(0), stringsAsFactors = FALSE)
      }
      if (is.null(selected_designations)) selected_designations <- character(0)
      if (is.null(check_designations)) check_designations <- character(0)
      if (is.null(tpp_criteria)) tpp_criteria <- list()
      if (is.null(trait_map)) trait_map <- character(0)

      # Overall population: all rows NOT in check_designations
      if ("designation" %in% colnames(predictions_df) && nrow(predictions_df) > 0) {
        pop_df <- predictions_df[!predictions_df$designation %in% check_designations, , drop = FALSE]
      } else {
        pop_df <- data.frame(designation = character(0), stringsAsFactors = FALSE)
      }

      # Selected population
      has_selection <- length(selected_designations) > 0
      if (has_selection && "designation" %in% colnames(predictions_df) && nrow(predictions_df) > 0) {
        sel_df <- predictions_df[predictions_df$designation %in% selected_designations, , drop = FALSE]
      } else {
        sel_df <- data.frame(designation = character(0), stringsAsFactors = FALSE)
      }

      n_traits <- nrow(tpp_data)
      results <- vector("list", n_traits)

      for (i in seq_len(n_traits)) {
        row <- tpp_data[i, , drop = FALSE]
        tpp_trait_name <- as.character(row$tpp_trait)

        is_mapped <- tpp_trait_name %in% names(trait_map)
        mapped_str <- ifelse(is_mapped, "Yes", "No")

        if (is_mapped) {
          pheno_col <- trait_map[[tpp_trait_name]]

          pop_stats <- .tpp_compute_stats(pop_df, pheno_col)

          if (!has_selection) {
            sel_stats <- list(min = "no selection", max = "no selection", mean = "no selection")
          } else {
            sel_stats <- .tpp_compute_stats(sel_df, pheno_col)
          }

          has_threshold <- tpp_trait_name %in% names(tpp_criteria) &&
            !is.null(tpp_criteria[[tpp_trait_name]])

          if (has_threshold) {
            pct_meeting <- .tpp_compute_criteria_pct(
              pop_df, pheno_col, tpp_criteria[[tpp_trait_name]]
            )
          } else {
            pct_meeting <- "no threshold"
          }

        } else {
          pop_stats <- list(min = "not evaluated", max = "not evaluated", mean = "not evaluated")
          sel_stats <- list(min = "not evaluated", max = "not evaluated", mean = "not evaluated")
          pct_meeting <- "not evaluated"
        }

        results[[i]] <- data.frame(
          TPP_ID              = as.character(row$tpp_id),
          Trait_ID            = as.character(row$trait_id),
          Trait_Name          = tpp_trait_name,
          Trait_Requirement   = as.character(row$category),
          Mapped              = mapped_str,
          Pop_Min             = pop_stats$min,
          Pop_Max             = pop_stats$max,
          Pop_Mean            = pop_stats$mean,
          Sel_Min             = sel_stats$min,
          Sel_Max             = sel_stats$max,
          Sel_Mean            = sel_stats$mean,
          Pct_Meeting_Criteria = pct_meeting,
          stringsAsFactors    = FALSE
        )
      }

      breakdown_df <- do.call(rbind, results)

      # Order by category priority then Trait_ID
      category_order <- c("Essential_Improve", "Essential_Maintain", "Nice_To_Have")
      breakdown_df$cat_rank <- match(breakdown_df$Trait_Requirement, category_order)
      breakdown_df$cat_rank[is.na(breakdown_df$cat_rank)] <- length(category_order) + 1

      breakdown_df <- breakdown_df[order(breakdown_df$cat_rank, breakdown_df$Trait_ID), , drop = FALSE]
      breakdown_df$cat_rank <- NULL
      rownames(breakdown_df) <- NULL

      return(breakdown_df)
    },
    error = function(e) {
      warning(paste("tpp_build_breakdown_table: error building breakdown table -", e$message))
      return(.tpp_empty_breakdown())
    }
  )
}

#' Create an empty breakdown data.frame with correct structure
#' @noRd
.tpp_empty_breakdown <- function() {
  data.frame(
    TPP_ID              = character(0),
    Trait_ID            = character(0),
    Trait_Name          = character(0),
    Trait_Requirement   = character(0),
    Mapped              = character(0),
    Pop_Min             = character(0),
    Pop_Max             = character(0),
    Pop_Mean            = character(0),
    Sel_Min             = character(0),
    Sel_Max             = character(0),
    Sel_Mean            = character(0),
    Pct_Meeting_Criteria = character(0),
    stringsAsFactors    = FALSE
  )
}

#' Compute min, max, mean for a trait column within a subset data.frame
#' @noRd
.tpp_compute_stats <- function(df, col_name) {
  if (is.null(df) || nrow(df) == 0 || !col_name %in% colnames(df)) {
    return(list(min = "NA", max = "NA", mean = "NA"))
  }

  values <- df[[col_name]]
  values <- suppressWarnings(as.numeric(values))
  values <- values[!is.na(values)]

  if (length(values) == 0) {
    return(list(min = "NA", max = "NA", mean = "NA"))
  }

  list(
    min  = format(round(min(values), 2), nsmall = 2),
    max  = format(round(max(values), 2), nsmall = 2),
    mean = format(round(mean(values), 2), nsmall = 2)
  )
}

#' Compute percentage of candidates meeting TPP criteria for a single trait
#' @noRd
.tpp_compute_criteria_pct <- function(pop_df, pheno_col, desired_score) {
  if (is.null(pop_df) || nrow(pop_df) == 0 || !pheno_col %in% colnames(pop_df)) {
    return(format(round(0, 2), nsmall = 2))
  }

  values <- pop_df[[pheno_col]]
  values <- suppressWarnings(as.numeric(values))

  n_total <- length(values[!is.na(values)])
  if (n_total == 0) {
    return(format(round(0, 2), nsmall = 2))
  }

  meets <- vapply(values, function(val) {
    tpp_meets_criteria(val, desired_score)
  }, logical(1))

  n_meeting <- sum(meets)
  pct <- (n_meeting / n_total) * 100

  return(format(round(pct, 2), nsmall = 2))
}


# =============================================================================
# SECTION 7: Breakdown Detail Table (compliance reporting)
# =============================================================================

#' Render a desired-score specification as human-readable text
#' @noRd
tpp_format_bounds <- function(lower, upper, digits = 3) {
  has_lower <- !is.null(lower) && length(lower) == 1 && !is.na(lower) && is.finite(lower)
  has_upper <- !is.null(upper) && length(upper) == 1 && !is.na(upper) && is.finite(upper)

  if (has_lower && has_upper) {
    return(sprintf("Within range %s to %s", round(lower, digits), round(upper, digits)))
  }
  if (has_lower) return(sprintf("Higher than %s", round(lower, digits)))
  if (has_upper) return(sprintf("Lower than %s", round(upper, digits)))
  NA_character_
}

#' Resolve a TPP desired score to absolute bounds against the data
#' @noRd
tpp_resolve_desired_bounds <- function(parsed, check_values = NULL) {
  out <- list(lower = NA_real_, upper = NA_real_, basis = "unresolved")
  if (is.null(parsed) || !is.list(parsed)) return(out)

  score_type <- parsed$score_type
  lower <- parsed$lower_bound
  upper <- parsed$upper_bound

  is_relative <- !is.null(score_type) && !is.na(score_type) && score_type == "relative"

  if (!is_relative) {
    out$lower <- if (!is.null(lower) && !is.na(lower)) lower else NA_real_
    out$upper <- if (!is.null(upper) && !is.na(upper)) upper else NA_real_
    out$basis <- if (is.na(out$lower) && is.na(out$upper)) "unresolved" else "absolute"
    return(out)
  }

  cv <- suppressWarnings(as.numeric(check_values))
  cv <- cv[is.finite(cv)]
  if (length(cv) == 0) return(out)
  check_mean <- mean(cv)

  pct_above <- parsed$pct_above_check
  pct_of <- parsed$pct_of_check

  if (!is.null(pct_above) && !is.na(pct_above) && is.finite(pct_above)) {
    out$lower <- check_mean * (1 + pct_above / 100)
    out$basis <- "relative"
    return(out)
  }
  if (!is.null(pct_of) && !is.na(pct_of) && is.finite(pct_of)) {
    out$lower <- check_mean * (pct_of / 100)
    out$basis <- "relative"
    return(out)
  }
  if (!is.null(lower) && !is.na(lower) && is.finite(lower)) {
    out$lower <- check_mean + lower
    out$basis <- "relative"
  }
  if (!is.null(upper) && !is.na(upper) && is.finite(upper)) {
    out$upper <- check_mean + upper
    out$basis <- "relative"
  }
  out
}

#' Ordered set of target-proximity labels
#' @noRd
TPP_PROXIMITY_LEVELS <- c(
  "Comfortably meeting",
  "Moderately meeting",
  "Barely meeting",
  "Barely failing",
  "Moderately failing",
  "Substantially failing"
)

#' Non-numeric placeholders that can appear in a Target_proximity column
#' @noRd
TPP_PROXIMITY_PLACEHOLDERS <- c(
  "not evaluated", "no threshold", "trait not used", "no data", "no variance"
)

#' Signed distance from a trait mean to its TPP target, in SD units
#' @noRd
tpp_target_gap_sd <- function(mean_val, lower, upper, sd_ref) {
  if (length(mean_val) != 1 || !is.finite(mean_val)) return(NA_real_)
  if (length(sd_ref) != 1 || !is.finite(sd_ref) || sd_ref <= 0) return(NA_real_)

  has_lower <- length(lower) == 1 && !is.na(lower) && is.finite(lower)
  has_upper <- length(upper) == 1 && !is.na(upper) && is.finite(upper)
  if (!has_lower && !has_upper) return(NA_real_)

  raw <- if (has_lower && has_upper) {
    if (mean_val < lower) {
      mean_val - lower
    } else if (mean_val > upper) {
      upper - mean_val
    } else {
      min(mean_val - lower, upper - mean_val)
    }
  } else if (has_lower) {
    mean_val - lower
  } else {
    upper - mean_val
  }

  raw / sd_ref
}

#' Map a signed SD gap to a target-proximity label
#' @noRd
tpp_target_proximity_label <- function(gap) {
  if (length(gap) != 1 || !is.finite(gap)) return(NA_character_)
  if (gap >= 0.5)  return("Comfortably meeting")
  if (gap >= 0.2)  return("Moderately meeting")
  if (gap >= 0)    return("Barely meeting")
  if (gap >= -0.2) return("Barely failing")
  if (gap >= -0.5) return("Moderately failing")
  "Substantially failing"
}

#' Colour-code the Target_proximity columns of a breakdown datatable
#' @noRd
tpp_style_breakdown_proximity <- function(dt_obj,
                                         columns = c("Target_proximity_sel",
                                                     "Target_proximity_all")) {
  bg <- c(
    "Comfortably meeting"   = "#D5F5E3",
    "Moderately meeting"    = "#E8F8F5",
    "Barely meeting"        = "#FCF3CF",
    "Barely failing"        = "#FDEBD0",
    "Moderately failing"    = "#FADBD8",
    "Substantially failing" = "#F5B7B1"
  )
  fg <- c(
    "Comfortably meeting"   = "#186A3B",
    "Moderately meeting"    = "#148F77",
    "Barely meeting"        = "#7D6608",
    "Barely failing"        = "#935116",
    "Moderately failing"    = "#943126",
    "Substantially failing" = "#78281F"
  )

  out <- dt_obj
  for (cl in columns) {
    out <- DT::formatStyle(
      out,
      columns = cl,
      backgroundColor = DT::styleEqual(names(bg), unname(bg)),
      color = DT::styleEqual(names(fg), unname(fg)),
      fontWeight = DT::styleEqual(names(bg), rep("600", length(bg)))
    )
  }
  out
}

#' Build the detailed TPP breakdown table and environment-subset notes
#'
#' @param tpp_id Character TPP identifier.
#' @param raw_tpp Data.frame of the raw TPP sheet.
#' @param traits_df Data.frame with tpp_trait and pheno_trait.
#' @param env_filters Named list keyed by Trait ID.
#' @param checks_per_trait Named list keyed by tpp_trait.
#' @param predictions_df Wide predictions/decision table with designation column.
#' @param selected_designations Character vector of selected designations.
#' @param candidate_designations Character vector of the whole candidate set.
#' @param n_total_environments Optional integer.
#' @param used_traits Character vector of trait columns actually used in selection.
#' @return List with \code{table} (data.frame) and \code{env_notes} (character).
#' @noRd
tpp_build_breakdown_detail <- function(tpp_id,
                                       raw_tpp,
                                       traits_df,
                                       env_filters = NULL,
                                       checks_per_trait = NULL,
                                       predictions_df = NULL,
                                       selected_designations = character(0),
                                       candidate_designations = NULL,
                                       n_total_environments = NA_integer_,
                                       used_traits = NULL) {

  empty <- list(
    table = data.frame(Message = "No TPP information available", stringsAsFactors = FALSE),
    env_notes = character(0)
  )

  if (is.null(raw_tpp) || !is.data.frame(raw_tpp) || nrow(raw_tpp) == 0) return(empty)

  col_or_na <- function(df, nm) {
    if (nm %in% colnames(df)) as.character(df[[nm]]) else rep(NA_character_, nrow(df))
  }

  trait_ids   <- col_or_na(raw_tpp, "Trait ID")
  trait_names <- col_or_na(raw_tpp, "Trait Name")
  requirement <- col_or_na(raw_tpp, "Trait Requirement")
  desired_tpp <- col_or_na(raw_tpp, "Desired Score")
  checks_tpp  <- col_or_na(raw_tpp, "Best Checks")

  if (all(is.na(trait_names))) return(empty)
  if (any(is.na(trait_ids))) {
    trait_ids[is.na(trait_ids)] <- sprintf("TR-%05d", which(is.na(trait_ids)))
  }

  pheno_of <- rep(NA_character_, length(trait_names))
  if (!is.null(traits_df) && is.data.frame(traits_df) &&
      all(c("tpp_trait", "pheno_trait") %in% colnames(traits_df))) {
    mi <- match(trait_names, as.character(traits_df$tpp_trait))
    pheno_of <- as.character(traits_df$pheno_trait)[mi]
  }
  mapped <- !is.na(pheno_of) & nzchar(pheno_of)

  pred_cols <- if (!is.null(predictions_df) && is.data.frame(predictions_df)) {
    colnames(predictions_df)
  } else {
    character(0)
  }

  n_rows <- length(trait_names)
  desired_data  <- rep(NA_character_, n_rows)
  checks_data   <- rep(NA_character_, n_rows)
  sel_pct       <- rep(NA_character_, n_rows)
  all_pct       <- rep(NA_character_, n_rows)
  mean_sel      <- rep(NA_character_, n_rows)
  mean_all      <- rep(NA_character_, n_rows)
  prox_sel      <- rep(NA_character_, n_rows)
  prox_all      <- rep(NA_character_, n_rows)
  used_flag     <- rep(NA_character_, n_rows)

  have_pred <- !is.null(predictions_df) && "designation" %in% pred_cols
  has_sel <- length(selected_designations) > 0 && have_pred
  know_used <- !is.null(used_traits)

  if (is.null(candidate_designations) && have_pred) {
    candidate_designations <- unique(as.character(predictions_df$designation))
  }
  has_all <- !is.null(candidate_designations) &&
    length(candidate_designations) > 0 && have_pred

  vals_for <- function(col, desigs) {
    v <- suppressWarnings(as.numeric(
      predictions_df[[col]][predictions_df$designation %in% desigs]
    ))
    v[is.finite(v)]
  }
  fmt_mean <- function(v, digits = 3) {
    if (length(v) == 0) "no data" else as.character(round(mean(v), digits))
  }
  fmt_pct <- function(v, lo, up) {
    if (length(v) == 0) return("no data")
    n_meet <- sum(v >= lo & v <= up)
    sprintf("%.1f%% (%d/%d)", 100 * n_meet / length(v), n_meet, length(v))
  }

  fmt_prox <- function(v, lo, up, sd_ref) {
    if (length(v) == 0) return("no data")
    if (!is.finite(sd_ref) || sd_ref <= 0) return("no variance")
    lbl <- tpp_target_proximity_label(
      tpp_target_gap_sd(mean(v), lo, up, sd_ref)
    )
    if (is.na(lbl)) "no data" else lbl
  }

  for (i in seq_len(n_rows)) {
    tpp_trait <- trait_names[i]

    mapped_checks <- if (!is.null(checks_per_trait)) checks_per_trait[[tpp_trait]] else NULL
    checks_data[i] <- if (!is.null(mapped_checks) && length(mapped_checks) > 0) {
      paste(mapped_checks, collapse = ", ")
    } else {
      "none mapped"
    }

    if (!mapped[i]) {
      desired_data[i] <- "not evaluated"
      sel_pct[i]      <- "not evaluated"
      all_pct[i]      <- "not evaluated"
      mean_sel[i]     <- "not evaluated"
      mean_all[i]     <- "not evaluated"
      prox_sel[i]     <- "not evaluated"
      prox_all[i]     <- "not evaluated"
      used_flag[i]    <- "No"
      next
    }

    value_col <- if (tpp_trait %in% pred_cols) tpp_trait else pheno_of[i]

    is_used <- TRUE
    if (know_used) {
      is_used <- value_col %in% used_traits || tpp_trait %in% used_traits
    }
    used_flag[i] <- if (!know_used) "not recorded" else if (is_used) "Yes" else "No"

    parsed <- tryCatch(tpp_parse_desired_score(desired_tpp[i]), error = function(e) NULL)

    check_values <- NULL
    if (!is.null(mapped_checks) && length(mapped_checks) > 0 &&
        value_col %in% pred_cols && "designation" %in% pred_cols) {
      check_values <- suppressWarnings(as.numeric(
        predictions_df[[value_col]][predictions_df$designation %in% mapped_checks]
      ))
    }

    bounds <- tpp_resolve_desired_bounds(parsed, check_values)
    txt <- tpp_format_bounds(bounds$lower, bounds$upper)

    col_ok <- value_col %in% pred_cols
    mean_sel[i] <- if (col_ok && has_sel) {
      fmt_mean(vals_for(value_col, selected_designations))
    } else "no data"
    mean_all[i] <- if (col_ok && has_all) {
      fmt_mean(vals_for(value_col, candidate_designations))
    } else "no data"

    if (is.na(txt)) {
      is_rel <- !is.null(parsed$score_type) && !is.na(parsed$score_type) &&
        parsed$score_type == "relative"
      desired_data[i] <- if (is_rel) "unresolved (no checks mapped)" else "no threshold"
      lbl <- if (know_used && !is_used) "trait not used" else "no threshold"
      sel_pct[i] <- lbl
      all_pct[i] <- lbl
      prox_sel[i] <- lbl
      prox_all[i] <- lbl
      next
    }
    desired_data[i] <- txt

    if (know_used && !is_used) {
      sel_pct[i] <- "trait not used"
      all_pct[i] <- "trait not used"
      prox_sel[i] <- "trait not used"
      prox_all[i] <- "trait not used"
      next
    }

    lo <- if (is.na(bounds$lower)) -Inf else bounds$lower
    up <- if (is.na(bounds$upper)) Inf else bounds$upper

    sel_pct[i] <- if (!col_ok) "no data"
                  else if (!has_sel) "no selection"
                  else fmt_pct(vals_for(value_col, selected_designations), lo, up)

    all_pct[i] <- if (!col_ok) "no data"
                  else if (!has_all) "no data"
                  else fmt_pct(vals_for(value_col, candidate_designations), lo, up)

    sd_ref <- NA_real_
    if (col_ok && has_all) {
      all_vals <- vals_for(value_col, candidate_designations)
      if (length(all_vals) > 1) sd_ref <- stats::sd(all_vals)
    }

    prox_sel[i] <- if (!col_ok) "no data"
                   else if (!has_sel) "no selection"
                   else fmt_prox(vals_for(value_col, selected_designations),
                                 bounds$lower, bounds$upper, sd_ref)

    prox_all[i] <- if (!col_ok) "no data"
                   else if (!has_all) "no data"
                   else fmt_prox(vals_for(value_col, candidate_designations),
                                 bounds$lower, bounds$upper, sd_ref)
  }

  out_tbl <- data.frame(
    TPP_ID               = rep(as.character(tpp_id), n_rows),
    Trait_ID             = trait_ids,
    Trait_Name_tpp       = trait_names,
    Mapped               = ifelse(mapped, "Yes", "No"),
    Used_for_selection   = used_flag,
    Trait_Name_data      = ifelse(mapped, pheno_of, "not mapped"),
    Trait_Requirement    = ifelse(is.na(requirement), "not specified", requirement),
    Desired_score_tpp    = ifelse(is.na(desired_tpp) | !nzchar(desired_tpp),
                                  "not specified", desired_tpp),
    Desired_score_data   = desired_data,
    mean_score_sel_data  = mean_sel,
    Target_proximity_sel = prox_sel,
    mean_score_all_data  = mean_all,
    Target_proximity_all = prox_all,
    Best_checks_tpp      = ifelse(is.na(checks_tpp) | !nzchar(checks_tpp),
                                  "not specified", checks_tpp),
    Best_checks_data     = checks_data,
    Sel_pct_meeting_criteria = sel_pct,
    All_pct_meeting_criteria = all_pct,
    stringsAsFactors     = FALSE
  )

  # Environment-subset notes
  env_notes <- character(0)
  if (!is.null(env_filters) && is.list(env_filters) && length(env_filters) > 0) {
    for (key in names(env_filters)) {
      envs <- env_filters[[key]]
      if (is.null(envs) || length(envs) == 0) next

      idx <- match(key, trait_ids)
      if (is.na(idx)) next

      tpp_trait <- trait_names[idx]
      pheno <- if (mapped[idx]) pheno_of[idx] else NA_character_

      subject <- if (!is.na(pheno)) {
        sprintf("%s corresponds to the trait %s", tpp_trait, pheno)
      } else {
        sprintf("%s (not mapped to a phenotypic trait)", tpp_trait)
      }

      count_txt <- if (!is.na(n_total_environments) &&
                       n_total_environments >= length(envs)) {
        sprintf(" (%d of %d environments)", length(envs), n_total_environments)
      } else {
        sprintf(" (%d environments)", length(envs))
      }

      env_notes <- c(env_notes, sprintf(
        "%s evaluated in environments %s%s.",
        subject, paste(envs, collapse = ", "), count_txt
      ))
    }
  }

  list(table = out_tbl, env_notes = env_notes)
}


# =============================================================================
# SECTION 8: Population Context (Advancement Meeting)
# =============================================================================

#' Compute the truncation limit for the population context table
#'
#' Determines how many rows (top-ranked designations) to display in the
#' Advancement Meeting population context table.
#'
#' @param rankings Data.frame with columns: designation, rank, is_controversial.
#' @param currently_discussed Character designation currently under discussion, or NULL.
#' @return Integer number of rows to display.
#' @noRd
tpp_compute_context_limit <- function(rankings, currently_discussed = NULL) {
  if (is.null(rankings) || !is.data.frame(rankings) || nrow(rankings) == 0) {
    return(0L)
  }

  required_cols <- c("designation", "rank", "is_controversial")
  if (!all(required_cols %in% colnames(rankings))) {
    warning("tpp_compute_context_limit: rankings missing required columns (designation, rank, is_controversial)")
    return(nrow(rankings))
  }

  total_population <- nrow(rankings)

  controversial_rows <- rankings[rankings$is_controversial == TRUE, , drop = FALSE]

  if (nrow(controversial_rows) == 0) {
    return(as.integer(total_population))
  }

  max_controversial_rank <- max(controversial_rows$rank, na.rm = TRUE)

  truncation_limit <- max_controversial_rank + 5L

  truncation_limit <- min(truncation_limit, total_population)

  if (!is.null(currently_discussed) && length(currently_discussed) == 1 &&
      nchar(currently_discussed) > 0) {
    discussed_row <- rankings[rankings$designation == currently_discussed, , drop = FALSE]
    if (nrow(discussed_row) > 0) {
      discussed_rank <- discussed_row$rank[1]
      if (discussed_rank > truncation_limit) {
        truncation_limit <- discussed_rank
      }
    }
  }

  truncation_limit <- min(truncation_limit, total_population)

  return(as.integer(truncation_limit))
}

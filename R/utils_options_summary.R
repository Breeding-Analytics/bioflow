#' Options Summary Table Builders
#'
#' @description Utility functions for constructing the Global Options Summary
#'   table and Stakeholder Options Summary table displayed in the Pre-PAM and
#'   Advancement Meeting dashboards. These pure functions encapsulate all data
#'   transformation logic for presenting configured weights, thresholds,
#'   directions, and reference checks per trait.
#'
#'

#' Build global options summary table
#'
#' Constructs a data.frame summarizing each selected trait's configured weight,
#' threshold value, threshold direction, and reference check designation. One row
#' per trait, ordered consistently with the input list order.
#'
#' @param trait_configs List of trait configuration objects. Each element is a
#'   list with fields:
#'   \itemize{
#'     \item \code{trait_name} Character trait name
#'     \item \code{weight} Numeric weight value (+1, -1, or 0)
#'     \item \code{threshold_value} Numeric threshold or NA
#'     \item \code{threshold_direction} Character "greater" or "less", or NA
#'     \item \code{reference_checks} Character vector of check names, or NULL
#'     \item \code{reference_value} Numeric computed mean of checks, or NA
#'   }
#' @return A data.frame with columns: Trait, Weight, Threshold_Value,
#'   Threshold_Direction, Reference_Check. Returns an empty data.frame with
#'   the correct column structure if \code{trait_configs} is empty or NULL.
#' @noRd
tpp_build_global_options_table <- function(trait_configs) {
  # Define output column names
  col_names <- c("Trait", "Weight", "Threshold_Value",
                 "Threshold_Direction", "Reference_Check")


  # Handle NULL or empty input

  if (is.null(trait_configs) || length(trait_configs) == 0) {
    empty_df <- data.frame(
      Trait = character(0),
      Weight = numeric(0),
      Threshold_Value = character(0),
      Threshold_Direction = character(0),
      Reference_Check = character(0),
      stringsAsFactors = FALSE
    )
    return(empty_df)
  }

  n <- length(trait_configs)

  traits <- character(n)
  weights <- numeric(n)
  threshold_values <- character(n)
  threshold_directions <- character(n)
  reference_checks <- character(n)

  for (i in seq_len(n)) {
    cfg <- trait_configs[[i]]

    # Trait name
    traits[i] <- if (!is.null(cfg$trait_name)) as.character(cfg$trait_name) else ""

    # Weight
    weights[i] <- if (!is.null(cfg$weight) && !is.na(cfg$weight)) {
      as.numeric(cfg$weight)
    } else {
      0
    }

    # Threshold value: display "no threshold" if NA or NULL
    if (is.null(cfg$threshold_value) || is.na(cfg$threshold_value)) {
      threshold_values[i] <- "no threshold"
    } else {
      threshold_values[i] <- as.character(cfg$threshold_value)
    }

    # Threshold direction: display "no threshold" if NA or NULL
    if (is.null(cfg$threshold_direction) || is.na(cfg$threshold_direction)) {
      threshold_directions[i] <- "no threshold"
    } else {
      threshold_directions[i] <- as.character(cfg$threshold_direction)
    }

    # Reference check: display check names + computed reference value, or empty
    if (is.null(cfg$reference_checks) || length(cfg$reference_checks) == 0) {
      reference_checks[i] <- ""
    } else {
      checks_str <- paste(cfg$reference_checks, collapse = ", ")
      if (!is.null(cfg$reference_value) && !is.na(cfg$reference_value)) {
        reference_checks[i] <- paste0(checks_str, " (ref: ",
                                      round(cfg$reference_value, 4), ")")
      } else {
        reference_checks[i] <- checks_str
      }
    }
  }

  result <- data.frame(
    Trait = traits,
    Weight = weights,
    Threshold_Value = threshold_values,
    Threshold_Direction = threshold_directions,
    Reference_Check = reference_checks,
    stringsAsFactors = FALSE
  )

  return(result)
}


#' Build stakeholder options summary table
#'
#' Constructs a named list of data.frames, one per stakeholder, summarizing each
#' stakeholder's per-trait configured weight, threshold value, threshold direction,
#' and reference check designation. Each sub-table follows the same formatting
#' rules as the global options table.
#'
#' @param stakeholder_configs Named list (by stakeholder display name) of trait
#'   config lists. Each value is a list of trait configuration objects (same
#'   structure as \code{trait_configs} in \code{tpp_build_global_options_table}).
#' @return A named list of data.frames (one per stakeholder) for rendering.
#'   Each data.frame has columns: Trait, Weight, Threshold_Value,
#'   Threshold_Direction, Reference_Check. If \code{stakeholder_configs} is NULL,
#'   empty, or not a list, returns a list with a single element containing
#'   the message data.frame.
#' @noRd
tpp_build_stakeholder_options_table <- function(stakeholder_configs) {
  # Handle NULL, empty, or non-list input
  if (is.null(stakeholder_configs) || !is.list(stakeholder_configs) ||
      length(stakeholder_configs) == 0) {
    message_df <- data.frame(
      Message = "No stakeholder configuration data available",
      stringsAsFactors = FALSE
    )
    return(list("No Data" = message_df))
  }

  stakeholder_names <- names(stakeholder_configs)

  # If no names, generate fallback names

  if (is.null(stakeholder_names) || all(stakeholder_names == "")) {
    stakeholder_names <- paste("Stakeholder", seq_along(stakeholder_configs))
  }

  result <- vector("list", length(stakeholder_configs))
  names(result) <- stakeholder_names

  for (i in seq_along(stakeholder_configs)) {
    trait_cfgs <- stakeholder_configs[[i]]
    result[[i]] <- tpp_build_global_options_table(trait_cfgs)
  }

  return(result)
}

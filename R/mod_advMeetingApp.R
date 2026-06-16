# ============================================================================
# mod_advMeetingApp_helpers.R (merged into main module file)
#
# Internal helper functions for the Advancement Meeting Dashboard module.
# These handle comparison matrix construction, controversy scoring, and sorting.
# ============================================================================

#' Build comparison matrix from stakeholder selection decisions
#'
#' Extracts decisions from the modifications table for each selected stamp,
#' pivots to wide format (one row per designation, one column per stamp),
#' and attaches controversy scores.
#'
#' @param dt_object The data object list with `$modifications$selection` and `$status`.
#' @param selected_stamps Character vector of analysisId values for the stamps to compare.
#'
#' @return A data.frame with columns: designation, one column per stamp
#'   (named by analysisIdName from status table), and controversy_score.
#'
#' @noRd
build_comparison_matrix <- function(dt_object, selected_stamps) {
  mods <- dt_object$modifications$selection

  # Filter to relevant rows: matching stamps, module, and reason

  filtered <- mods[
    mods$analysisId %in% selected_stamps &
      mods$module == "Final_prodAdv" &
      mods$reason == "final_selection",
    ,
    drop = FALSE
  ]

  # Build stamp name lookup from status table
  status <- dt_object$status
  stamp_names <- setNames(
    status$analysisIdName[match(selected_stamps, status$analysisId)],
    selected_stamps
  )

  # Create long-format data frame with readable stamp names
  long_df <- data.frame(
    designation = filtered$designation,
    stamp_name  = stamp_names[as.character(filtered$analysisId)],
    decision    = filtered$value,
    stringsAsFactors = FALSE
  )

  # Pivot to wide format
  designations <- sort(unique(long_df$designation))
  stakeholder_names <- as.character(stamp_names[selected_stamps])

  # Initialize wide matrix
  wide <- data.frame(designation = designations, stringsAsFactors = FALSE)

  for (sn in stakeholder_names) {
    subset_df <- long_df[long_df$stamp_name == sn, , drop = FALSE]
    matched_decisions <- subset_df$decision[match(designations, subset_df$designation)]
    wide[[sn]] <- matched_decisions
  }

  # Compute and attach controversy scores
  scores <- compute_controversy_scores(wide)
  wide$controversy_score <- scores[wide$designation]

  wide
}

#' Compute controversy scores for each designation
#'
#' Calculates the ratio of SELECTED decisions to total stakeholders for each
#' designation row in the comparison matrix.
#'
#' @param comparison_matrix A wide-format data.frame with a `designation` column
#'   and one column per stakeholder containing decision values.
#'   May or may not already contain a `controversy_score` column.
#'
#' @return A named numeric vector of scores (keyed by designation), where each
#'   score = (count of "SELECTED") / N, and N is the number of stakeholder columns.
#'
#' @noRd
compute_controversy_scores <- function(comparison_matrix) {
  # Identify stakeholder columns (exclude designation and controversy_score)
  all_cols <- colnames(comparison_matrix)
  stakeholder_cols <- setdiff(all_cols, c("designation", "controversy_score"))
  n_stakeholders <- length(stakeholder_cols)

  if (n_stakeholders == 0) {
    return(setNames(rep(NA_real_, nrow(comparison_matrix)), comparison_matrix$designation))
  }

  scores <- vapply(seq_len(nrow(comparison_matrix)), function(i) {
    decisions <- as.character(unlist(comparison_matrix[i, stakeholder_cols]))
    sum(decisions == "SELECTED", na.rm = TRUE) / n_stakeholders
  }, numeric(1))

  setNames(scores, comparison_matrix$designation)
}

#' Sort comparison matrix by controversy level
#'
#' Sorts rows with consensus REVISE at top, then by |controversy_score - 0.5|
#' ascending, placing the most controversial candidates near the top.
#'
#' @param comparison_matrix A wide-format data.frame with a `controversy_score` column.
#'
#' @return The same data.frame sorted by controversy (REVISE first, then most controversial).
#'
#' @noRd
sort_by_controversy <- function(comparison_matrix) {
  stakeholder_cols <- setdiff(colnames(comparison_matrix), c("designation", "controversy_score"))

  # Identify unanimous REVISE rows
  is_revise <- vapply(seq_len(nrow(comparison_matrix)), function(i) {
    decisions <- as.character(unlist(comparison_matrix[i, stakeholder_cols]))
    all(decisions == "REVISE", na.rm = TRUE)
  }, logical(1))

  distance_from_half <- abs(comparison_matrix$controversy_score - 0.5)

  # Sort: REVISE first (0), then by distance ascending
  sort_key <- ifelse(is_revise, -1, distance_from_half)
  comparison_matrix[order(sort_key), , drop = FALSE]
}

#' Classify controversy scores into categories
#'
#' Maps numeric controversy scores to human-readable classification labels.
#'
#' @param scores A numeric vector of controversy scores (values in [0, 1]).
#'
#' @return A character vector of same length: "controversial" for 0 < score < 1,
#'   "consensus_selected" for score == 1.0, "consensus_not_selected" for score == 0.0.
#'
#' @noRd
classify_controversy <- function(scores) {
  vapply(scores, function(s) {
    if (is.na(s)) return(NA_character_)
    if (s == 1.0) return("consensus_selected")
    if (s == 0.0) return("consensus_not_selected")
    "controversial"
  }, character(1))
}

#' Validate MTA compatibility across selected Final_Selection_Stamps
#'
#' Traces each selected stamp through the modeling table to extract the
#' underlying MTA_Stamp and verifies all stamps share the same one.
#'
#' @param dt_object The data object list with `$modeling` data frame.
#' @param selected_stamps Character vector of Final_Selection_Stamp analysisId values.
#'
#' @return A list with:
#'   \item{compatible}{Logical; TRUE if all stamps share the same MTA stamp.}
#'   \item{mta_stamp}{Character; the shared MTA stamp ID, or NULL if incompatible.}
#'   \item{incompatible_stamps}{Character vector of stamp IDs that don't match
#'     the most common MTA stamp (empty character vector if compatible).}
#'
#' @noRd
validate_mta_compatibility <- function(dt_object, selected_stamps) {
  modeling <- dt_object$modeling

  # For each selected stamp, trace: Final_prodAdv -> Init_prodAdv -> mta_stamp
  mta_stamps <- vapply(selected_stamps, function(stamp_id) {
    # Step 1: Find the Init_prodAdv stamp ID from Final_prodAdv inputObject
    init_rows <- modeling[
      modeling$module == "Final_prodAdv" &
        modeling$analysisId == stamp_id &
        modeling$parameter == "inputObject",
      ,
      drop = FALSE
    ]

    if (nrow(init_rows) == 0) return(NA_character_)
    init_stamp <- init_rows$value[1]

    # Step 2: From Init_prodAdv, extract the mta_stamp parameter value
    mta_rows <- modeling[
      modeling$module == "Init_prodAdv" &
        modeling$analysisId == init_stamp &
        modeling$parameter == "mta_stamp",
      ,
      drop = FALSE
    ]

    if (nrow(mta_rows) == 0) return(NA_character_)
    mta_rows$value[1]
  }, character(1))

  # Check if all MTA stamps are the same
  unique_mta <- unique(mta_stamps[!is.na(mta_stamps)])

  if (length(unique_mta) == 1 && !any(is.na(mta_stamps))) {
    # All compatible
    list(
      compatible = TRUE,
      mta_stamp = unique_mta,
      incompatible_stamps = character(0)
    )
  } else {
    # Incompatible: identify which stamps don't match
    # Use the most common MTA stamp as the reference
    if (length(unique_mta) == 0) {
      # No MTA stamps could be resolved
      incompatible <- selected_stamps
    } else {
      mta_counts <- table(mta_stamps[!is.na(mta_stamps)])
      reference_mta <- names(which.max(mta_counts))
      incompatible <- selected_stamps[is.na(mta_stamps) | mta_stamps != reference_mta]
    }

    list(
      compatible = FALSE,
      mta_stamp = NULL,
      incompatible_stamps = incompatible
    )
  }
}

#' Compute the intersection of designation sets across selected stamps
#'
#' Extracts the designation list for each selected stamp from the modifications
#' table and computes their intersection. Produces warning messages when any two
#' stamps have different designation sets.
#'
#' @param dt_object The data object list with `$modifications$selection`.
#' @param selected_stamps Character vector of analysisId values for the stamps to compare.
#'
#' @return A list with:
#'   \item{designations}{Character vector: the intersection of all designation sets.}
#'   \item{warnings}{Character vector: messages describing which stamps have
#'     different sets (empty character vector if all identical).}
#'
#' @noRd
intersect_designation_sets <- function(dt_object, selected_stamps) {
  mods <- dt_object$modifications$selection

  # Extract designation set for each stamp
  stamp_sets <- lapply(selected_stamps, function(stamp_id) {
    rows <- mods[
      mods$analysisId == stamp_id &
        mods$module == "Final_prodAdv" &
        mods$reason == "final_selection",
      ,
      drop = FALSE
    ]
    unique(rows$designation)
  })
  names(stamp_sets) <- selected_stamps

  # Compute intersection across all sets
  designations <- Reduce(intersect, stamp_sets)

  # Flag differences: compare each pair of stamps

  warnings <- character(0)

  stamp_ids <- names(stamp_sets)
  n <- length(stamp_ids)

  if (n >= 2) {
    for (i in seq_len(n - 1)) {
      for (j in (i + 1):n) {
        set_i <- stamp_sets[[stamp_ids[i]]]
        set_j <- stamp_sets[[stamp_ids[j]]]
        if (!setequal(set_i, set_j)) {
          only_in_i <- setdiff(set_i, set_j)
          only_in_j <- setdiff(set_j, set_i)
          msg <- paste0(
            "Stamps '", stamp_ids[i], "' and '", stamp_ids[j],
            "' have different designation sets."
          )
          if (length(only_in_i) > 0) {
            msg <- paste0(msg, " Only in '", stamp_ids[i], "': ",
                          paste(only_in_i, collapse = ", "), ".")
          }
          if (length(only_in_j) > 0) {
            msg <- paste0(msg, " Only in '", stamp_ids[j], "': ",
                          paste(only_in_j, collapse = ", "), ".")
          }
          warnings <- c(warnings, msg)
        }
      }
    }
  }

  list(
    designations = designations,
    warnings     = warnings
  )
}


# ============================================================================
# Discussion status, color mapping, and filtering helpers
# ============================================================================

#' Named vector of status colors for decision visualization
#' @noRd
MEETING_STATUS_COLORS <- c(
  "SELECTED"     = "#0072B2",
  "NOT SELECTED" = "#D55E00",
  "REVISE"       = "#F9A825",
  "CHECK"        = "#C2185B"
)

#' Toggle discussion status between "Discuss" and "Resolved"
#'
#' Switches the current discussion status to its opposite value.
#' "Discuss" becomes "Resolved" and "Resolved" becomes "Discuss".
#'
#' @param current_status Character scalar, either "Discuss" or "Resolved".
#'
#' @return Character scalar: the opposite status.
#'
#' @noRd
toggle_discussion_status <- function(current_status) {
  if (current_status == "Discuss") {
    return("Resolved")
  }
  "Discuss"
}

#' Map decision values to hex colors
#'
#' Returns the corresponding STATUS_COLORS hex value for each decision.
#' Unrecognized values are mapped to gray ("#999999").
#'
#' @param decision Character vector of decision values
#'   (e.g., "SELECTED", "NOT SELECTED", "REVISE", "CHECK").
#'
#' @return Character vector of hex color strings, same length as input.
#'
#' @noRd
get_status_color <- function(decision) {
  colors <- MEETING_STATUS_COLORS[decision]
  colors[is.na(colors)] <- "#999999"
  unname(colors)
}

#' Filter comparison matrix to controversial candidates only
#'
#' Returns only rows where controversy_score is strictly between 0 and 1.
#'
#' @param comparison_matrix Wide-format data.frame with a `controversy_score` column.
#'
#' @return Filtered data.frame containing only controversial rows.
#'
#' @noRd
filter_controversial_only <- function(comparison_matrix) {
  comparison_matrix[
    comparison_matrix$controversy_score > 0 &
      comparison_matrix$controversy_score < 1,
    ,
    drop = FALSE
  ]
}


# ============================================================================
# Majority Voting and Decision Assembly Helpers
# ============================================================================

#' Compute majority vote from a vector of stakeholder decisions
#'
#' Returns the decision with the highest count. In case of a tie
#' (two or more decisions share the maximum count), returns "NOT SELECTED".
#'
#' @param decisions_vector Character vector of decisions from multiple
#'   stakeholders for one candidate (e.g., c("SELECTED", "NOT SELECTED", "SELECTED")).
#'
#' @return Character scalar: the majority decision, or "NOT SELECTED" on tie.
#'
#' @noRd
compute_majority_vote <- function(decisions_vector) {
  if (length(decisions_vector) == 0) return("NOT SELECTED")

  counts <- table(decisions_vector)
  max_count <- max(counts)
  winners <- names(counts)[counts == max_count]

  if (length(winners) == 1) {
    return(winners)
  }
  # Tie: return "NOT SELECTED"
  "NOT SELECTED"
}

#' Apply manual override to a majority vote result
#'
#' If an override value is provided (not NULL and not NA), it takes precedence
#' over the majority result.
#'
#' @param majority_result Character scalar: the computed majority vote.
#' @param override_value Character scalar or NULL: the manual override decision.
#'
#' @return Character scalar: override_value if provided, otherwise majority_result.
#'
#' @noRd
apply_override <- function(majority_result, override_value) {
  if (!is.null(override_value) && !is.na(override_value)) {
    return(override_value)
  }
  majority_result
}

#' Auto-assign decisions for non-controversial designations
#'
#' Identifies designations with unanimous agreement and assigns their decision
#' automatically. Unanimous REVISE is NOT auto-assigned (requires discussion).
#'
#' Rules:
#' - Unanimous SELECTED (controversy_score = 1.0) -> "SELECTED"
#' - Unanimous NOT SELECTED (all stakeholder columns = "NOT SELECTED") -> "NOT SELECTED"
#' - Unanimous CHECK (any stakeholder assigned "CHECK") -> "CHECK"
#' - Unanimous REVISE (all stakeholder columns = "REVISE") -> NOT auto-assigned
#'
#' @param comparison_matrix Wide-format data.frame with columns: designation,
#'   one column per stakeholder, and controversy_score.
#'
#' @return A data.frame with columns: designation, decision (only rows for
#'   auto-assigned designations).
#'
#' @noRd
auto_assign_non_controversial <- function(comparison_matrix) {
  # Identify stakeholder columns
  all_cols <- colnames(comparison_matrix)
  stakeholder_cols <- setdiff(all_cols, c("designation", "controversy_score"))

  result_designation <- character(0)
  result_decision <- character(0)


  for (i in seq_len(nrow(comparison_matrix))) {
    decisions <- as.character(unlist(comparison_matrix[i, stakeholder_cols]))
    score <- comparison_matrix$controversy_score[i]
    desig <- comparison_matrix$designation[i]

    # CHECK: any stakeholder assigned CHECK -> auto-assign CHECK
    if (any(decisions == "CHECK", na.rm = TRUE)) {
      result_designation <- c(result_designation, desig)
      result_decision <- c(result_decision, "CHECK")
      next
    }

    # Unanimous SELECTED: controversy_score = 1.0
    if (!is.na(score) && score == 1.0) {
      result_designation <- c(result_designation, desig)
      result_decision <- c(result_decision, "SELECTED")
      next
    }

    # Unanimous NOT SELECTED: all stakeholders = "NOT SELECTED"
    if (all(decisions == "NOT SELECTED", na.rm = TRUE)) {
      result_designation <- c(result_designation, desig)
      result_decision <- c(result_decision, "NOT SELECTED")
      next
    }

    # Unanimous REVISE: all stakeholders = "REVISE" -> NOT auto-assigned
    # (falls through, not added to result)
  }

  data.frame(
    designation = result_designation,
    decision    = result_decision,
    stringsAsFactors = FALSE
  )
}

#' Identify designations that require discussion in the Joint Decision Workflow
#'
#' Returns designation names that need the card-based review:
#' - Controversial: 0 < controversy_score < 1 (among non-CHECK designations)
#' - Unanimous REVISE: all stakeholders assigned "REVISE"
#'
#' @param comparison_matrix Wide-format data.frame with columns: designation,
#'   one column per stakeholder, and controversy_score.
#'
#' @return Character vector of designation names requiring discussion.
#'
#' @noRd
requires_discussion <- function(comparison_matrix) {
  # Identify stakeholder columns
  all_cols <- colnames(comparison_matrix)
  stakeholder_cols <- setdiff(all_cols, c("designation", "controversy_score"))

  discussion_designations <- character(0)

  for (i in seq_len(nrow(comparison_matrix))) {
    decisions <- as.character(unlist(comparison_matrix[i, stakeholder_cols]))
    score <- comparison_matrix$controversy_score[i]
    desig <- comparison_matrix$designation[i]

    # Skip CHECK designations (any stakeholder assigned CHECK)
    if (any(decisions == "CHECK", na.rm = TRUE)) {
      next
    }

    # Controversial: 0 < score < 1
    if (!is.na(score) && score > 0 && score < 1) {
      discussion_designations <- c(discussion_designations, desig)
      next
    }

    # Unanimous REVISE: all stakeholders assigned "REVISE"
    if (all(decisions == "REVISE", na.rm = TRUE)) {
      discussion_designations <- c(discussion_designations, desig)
      next
    }
  }

  discussion_designations
}

#' Assemble final decisions from discussion results and auto-assigned decisions
#'
#' Combines decisions from the Joint Decision Workflow (for controversial
#' candidates) with auto-assigned decisions (for non-controversial candidates)
#' into a single complete data.frame with exactly one row per designation.
#'
#' @param discussion_decisions Data.frame with columns: designation, decision
#'   (from the Joint Decision Workflow).
#' @param auto_assigned Data.frame with columns: designation, decision
#'   (from auto_assign_non_controversial).
#'
#' @return Data.frame with columns: designation, decision — one row per
#'   designation, no duplicates.
#'
#' @noRd
assemble_final_decisions <- function(discussion_decisions, auto_assigned) {
  combined <- rbind(discussion_decisions, auto_assigned)

  # Ensure no duplicates — keep first occurrence (discussion takes precedence)
  combined <- combined[!duplicated(combined$designation), , drop = FALSE]

  # Reset row names

  rownames(combined) <- NULL
  combined
}


# ============================================================================
# Direction-aware normalization and threshold helpers
# ============================================================================

#' Normalize trait values to [0, 1] respecting optimization direction
#'
#' Normalizes a numeric vector so that 1 = best and 0 = worst, respecting
#' the trait direction. For "Higher is better", higher raw values map to
#' values closer to 1. For "Lower is better", lower raw values map closer to 1.
#'
#' @param values Numeric vector of trait values.
#' @param direction Character; either "Higher is better" or "Lower is better".
#'
#' @return Numeric vector of same length as `values`, with all values in [0, 1].
#'   Returns 0.5 for all values if max == min (no variation).
#'
#' @noRd
normalize_trait_values <- function(values, direction) {
  v_min <- min(values, na.rm = TRUE)
  v_max <- max(values, na.rm = TRUE)

  # Edge case: no variation

  if (v_max == v_min) {
    return(rep(0.5, length(values)))
  }

  if (direction == "Higher is better") {
    (values - v_min) / (v_max - v_min)
  } else {
    # "Lower is better": lower values are better (closer to 1)
    (v_max - values) / (v_max - v_min)
  }
}

#' Detect whether a candidate fails a threshold
#'
#' Returns TRUE if the candidate FAILS the threshold given the trait direction.
#' "Higher is better" fails when value < threshold.
#' "Lower is better" fails when value > threshold.
#' If threshold is NA, no failure is flagged (returns FALSE).
#'
#' @param value Numeric scalar or vector of trait values.
#' @param threshold Numeric threshold value (may be NA for no threshold).
#' @param direction Character; either "Higher is better" or "Lower is better".
#'
#' @return Logical vector same length as `value`. TRUE indicates threshold failure.
#'
#' @noRd
detect_threshold_failure <- function(value, threshold, direction) {
  # If threshold is NA, no failure possible
  if (is.na(threshold)) {
    return(rep(FALSE, length(value)))
  }

  if (direction == "Higher is better") {
    value < threshold
  } else {
    # "Lower is better": fails if value exceeds (is higher than) threshold
    value > threshold
  }
}

#' Extract trait directions and thresholds from a dt_object
#'
#' Extracts the direction and threshold for each trait from the modeling table,
#' filtering by the given Init_prodAdv stamp. Traits with trait_rule_type = "None"
#' are assigned threshold = NA.
#'
#' @param dt_object The data object list with `$modeling` data frame.
#' @param init_stamp Character; the Init_prodAdv analysisId to filter on.
#'
#' @return A data.frame with columns: trait, direction, threshold.
#'   Threshold is NA when trait_rule_type = "None" or no threshold is defined.
#'
#' @noRd
extract_trait_directions <- function(dt_object, init_stamp) {
  modeling <- dt_object$modeling

  # Extract direction rows
  dir_rows <- modeling[
    modeling$analysisId == init_stamp &
      modeling$parameter == "direction",
    ,
    drop = FALSE
  ]

  # Extract threshold rows
  thresh_rows <- modeling[
    modeling$analysisId == init_stamp &
      modeling$parameter == "threshold",
    ,
    drop = FALSE
  ]

  # Extract trait_rule_type rows
  rule_rows <- modeling[
    modeling$analysisId == init_stamp &
      modeling$parameter == "trait_rule_type",
    ,
    drop = FALSE
  ]

  # Build result from direction rows (one row per trait)
  traits <- dir_rows$trait
  directions <- dir_rows$value

  # Match thresholds and rule types by trait
  thresholds <- vapply(traits, function(tr) {
    # Check if trait_rule_type is "None"
    rule_idx <- which(rule_rows$trait == tr)
    if (length(rule_idx) > 0 && rule_rows$value[rule_idx[1]] == "None") {
      return(NA_real_)
    }

    # Get threshold value
    thresh_idx <- which(thresh_rows$trait == tr)
    if (length(thresh_idx) == 0) {
      return(NA_real_)
    }
    as.numeric(thresh_rows$value[thresh_idx[1]])
  }, numeric(1))

  data.frame(
    trait     = traits,
    direction = directions,
    threshold = thresholds,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

#' Compute opacity values for direction-aware gradient coloring
#'
#' Maps trait values to opacity values in [0.2, 1.0] for gradient coloring.
#' "Higher is better": higher values get higher opacity.
#' "Lower is better": lower values get higher opacity.
#'
#' @param values Numeric vector of trait values.
#' @param direction Character; either "Higher is better" or "Lower is better".
#'
#' @return Numeric vector of opacities in [0.2, 1.0], same length as `values`.
#'   Returns 0.6 for all values if all values are the same (no variation).
#'
#' @noRd
apply_direction_gradient <- function(values, direction) {
  v_min <- min(values, na.rm = TRUE)
  v_max <- max(values, na.rm = TRUE)

  # Edge case: no variation
  if (v_max == v_min) {
    return(rep(0.6, length(values)))
  }

  # Normalize to [0, 1] first
  if (direction == "Higher is better") {
    normalized <- (values - v_min) / (v_max - v_min)
  } else {
    # "Lower is better": lower values should have higher opacity
    normalized <- (v_max - values) / (v_max - v_min)
  }

  # Map [0, 1] to [0.2, 1.0]
  0.2 + normalized * 0.8
}


#' Advancement Meeting Dashboard UI Function
#'
#' @description A shiny Module for multi-stakeholder advancement meeting decisions.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_advMeetingApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::mainPanel(width = 12,
                     tabsetPanel( id=ns("tabsMain"),
                                  type = "tabs",

                                  tabPanel(div(icon("book"), "Information"), value = "Information",
                                           br(),
                                           column(width = 6,
                                                  h1(strong(span("Advancement Meeting Dashboard", style="color:darkcyan"))),
                                                  h2(strong("Status:")),
                                                  uiOutput(ns("warningMessage")),
                                                  tags$br(),
                                           ),
                                           column(width = 6,
                                                  tags$body(
                                                    h2(strong("Details")),
                                                    p("The Advancement Meeting Dashboard enables multi-stakeholder consensus on product advancement decisions.
                                                      It reads from the merged data() reactive and does ", strong("NOT"), " load files directly."),
                                                    p(strong("Meeting Workflow (4 steps):")),
                                                    tags$ol(
                                                      tags$li("Each stakeholder runs the Pre-advancement module independently and saves their selection as an RData file."),
                                                      tags$li("The meeting facilitator uses the Data Management module to load and merge all stakeholder RData files into a single dataset."),
                                                      tags$li("In this module, the facilitator selects which stamps to compare and the group reviews disagreements."),
                                                      tags$li("The group records consensus decisions and generates a final meeting report.")
                                                    ),
                                                    p(strong("Note:"), " This module reads from the merged data() reactive provided by the data management module.
                                                      It does NOT load files itself."),

                                                    tags$hr(),
                                                    h3(strong("Prerequisites")),
                                                    tags$ul(
                                                      tags$li("All stakeholders must have completed the Pre-advancement workflow on the ", strong("same MTA analysis"), "."),
                                                      tags$li("All stakeholder RData files must be merged using the ", strong("Data Management module"), " (mod_bindObjectApp) before opening this tab."),
                                                      tags$li("At least two Final Selection Stamps must be present in the merged dataset.")
                                                    ),

                                                    tags$hr(),
                                                    h3(strong("Controversy Score")),
                                                    p("The ", strong("Controversy Score"), " measures agreement across stakeholders for each candidate:"),
                                                    p(em("Controversy Score = (number of SELECTED votes) / (total number of stakeholders)")),
                                                    tags$ul(
                                                      tags$li("Score = 1.0: full consensus SELECTED (all stakeholders agree to advance)"),
                                                      tags$li("Score = 0.0: full consensus NOT SELECTED (all stakeholders agree to drop)"),
                                                      tags$li("Score near 0.5: maximum disagreement (requires group discussion)"),
                                                      tags$li("0 < Score < 1: controversial candidate (needs discussion in meeting)")
                                                    )
                                                  )
                                           ),
                                  ),

                                  tabPanel(div(icon("arrow-right-to-bracket"), "Input steps"), value = "Input steps",
                                           br(),
                                           column(width = 12,
                                                  h4(strong("Select Final Selection Stamps to compare")),
                                                  hr(),
                                                  p("Select at least two stakeholder stamps from the merged dataset to compare their decisions in the meeting."),
                                                  tags$br(),
                                                  selectizeInput(
                                                    ns("stamp_select"),
                                                    label = "Final Selection Stamps:",
                                                    choices = NULL,
                                                    multiple = TRUE,
                                                    options = list(placeholder = "Select stamps to compare...")
                                                  ),
                                                  tags$br(),
                                                  column(width = 12, style = "background-color:grey; color: #FFFFFF",
                                                         br(),
                                                         actionButton(ns("merge_btn"), "Merge and Compare Decisions", icon = icon("play-circle")),
                                                         br(), br(),
                                                  ),
                                           ),
                                  ),

                                  tabPanel(div(icon("people-group"), "Review output"), value = "Review output",
                                           br(),
                                           column(width = 12,

                                                  # --- SECTION: Agreement Heatmap ---
                                                  h3(strong("Agreement Heatmap")),
                                                  p("Visual comparison of stakeholder decisions sorted by controversy level."),
                                                  checkboxInput(ns("filter_controversial"),
                                                                label = "Show only controversial candidates",
                                                                value = FALSE),
                                                  plotly::plotlyOutput(ns("heatmap"), height = "600px"),
                                                  tags$hr(),

                                                  # --- SECTION: Controversy Summary ---
                                                  h3(strong("Controversy Summary")),
                                                  plotOutput(ns("controversy_chart"), height = "300px"),
                                                  uiOutput(ns("consensus_summary_text")),
                                                  tags$hr(),

                                                  # --- SECTION: Candidate Detail Panel ---
                                                  h3(strong("Candidate Detail Comparison")),
                                                  selectizeInput(
                                                    ns("detail_candidates"),
                                                    label = "Select designations to compare (up to 5, including CHECK references):",
                                                    choices = NULL,
                                                    multiple = TRUE,
                                                    options = list(maxItems = 5,
                                                                   placeholder = "Select up to 5 designations...")
                                                  ),
                                                  fluidRow(
                                                    column(6, plotly::plotlyOutput(ns("radar_plot"), height = "450px")),
                                                    column(6, DT::dataTableOutput(ns("detail_table")))
                                                  ),
                                                  uiOutput(ns("stakeholder_breakdown")),
                                                  tags$hr(),

                                                  # --- SECTION: Joint Decision Workflow ---
                                                  h3(strong("Joint Decision Workflow")),
                                                  textOutput(ns("progress_indicator")),
                                                  uiOutput(ns("selection_tally")),
                                                  tags$br(),
                                                  fluidRow(
                                                    column(8,
                                                      wellPanel(
                                                        style = "border: 1px solid #ddd; border-radius: 8px; padding: 20px; background-color: #fafafa;",
                                                        uiOutput(ns("candidate_card")),
                                                        tags$br(),
                                                        fluidRow(
                                                          column(6, align = "left",
                                                                 actionButton(ns("prev_btn"), "Previous", icon = icon("arrow-left"))
                                                          ),
                                                          column(6, align = "right",
                                                                 actionButton(ns("next_btn"), "Next", icon = icon("arrow-right"))
                                                          )
                                                        )
                                                      )
                                                    ),
                                                    column(4,
                                                      wellPanel(
                                                        style = "border: 1px solid #ddd; border-radius: 8px; padding: 20px; background-color: #f0f8ff;",
                                                        radioButtons(ns("vote_decision"),
                                                                     label = "Meeting Decision:",
                                                                     choices = c("SELECTED", "NOT SELECTED"),
                                                                     inline = TRUE),
                                                        tags$hr(),
                                                        actionButton(ns("majority_btn"), "Simple Majority", icon = icon("users"), style = "width:100%; margin-bottom:10px;"),
                                                        actionButton(ns("toggle_status_btn"), "Mark as Resolved", icon = icon("check"), class = "btn-success", style = "width:100%;")
                                                      )
                                                    )
                                                  ),
                                                  # Population context table for current candidate
                                                  shinydashboard::box(
                                                    width = 12,
                                                    title = "Population Context",
                                                    status = "info",
                                                    solidHeader = TRUE,
                                                    collapsible = TRUE,
                                                    DT::DTOutput(ns("candidate_context_table"))
                                                  ),
                                                  tags$br(),

                                                  # --- SECTION: Save Meeting Decisions ---
                                                  tags$hr(),
                                                  h3(strong("Save Meeting Decisions")),
                                                  textOutput(ns("auto_assigned_summary")),
                                                  fluidRow(
                                                    column(6,
                                                           textInput(ns("meeting_name"),
                                                                     "Meeting Name:",
                                                                     placeholder = "Enter a name for this meeting session")
                                                    ),
                                                    column(6,
                                                           br(),
                                                           actionButton(ns("save_btn"),
                                                                        "Save Meeting Decisions",
                                                                        icon = icon("floppy-disk"),
                                                                        class = "btn-success btn-lg")
                                                    )
                                                  )

                                           ),
                                  ),

                                  tabPanel(div(icon("file-lines"), "Output"), value = "Output",
                                           br(),
                                           column(width = 12,
                                                  downloadButton(ns("download_report"),
                                                                 "Download dashboard",
                                                                 icon = icon("download"),
                                                                 class = "btn-primary"),
                                                  tags$br(), tags$br(),
                                                  uiOutput(ns("rmd_report")),
                                           ),
                                  ),

                     ) # end tabsetPanel
    ) # end mainPanel

  )
}

#' Advancement Meeting Dashboard Server Functions
#'
#' @noRd
mod_advMeetingApp_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # =========================================================================
    # Reactive Values
    # =========================================================================
    comparison_matrix   <- reactiveVal(NULL)
    controversy_scores  <- reactiveVal(NULL)
    trait_directions    <- reactiveVal(NULL)
    trait_thresholds    <- reactiveVal(NULL)
    meeting_decisions   <- reactiveVal(NULL)
    current_candidate_idx <- reactiveVal(1)

    # =========================================================================
    # available_stamps() reactive — filter status for Final_prodAdv stamps
    # Requirement 1.6, 2.1
    # =========================================================================
    available_stamps <- reactive({
      req(data())
      dt_obj <- data()
      if (is.null(dt_obj$status)) return(data.frame())
      stamps <- dt_obj$status[dt_obj$status$module == "Final_prodAdv", , drop = FALSE]
      stamps
    })

    # =========================================================================
    # Update stamp_select choices when stamps are available
    # Requirement 2.1, 2.2
    # =========================================================================
    observe({
      stamps <- available_stamps()
      if (nrow(stamps) == 0) {
        updateSelectizeInput(session, "stamp_select",
                            choices = character(0),
                            selected = character(0))
      } else {
        choices <- setNames(stamps$analysisId, stamps$analysisIdName)
        updateSelectizeInput(session, "stamp_select",
                            choices = choices,
                            selected = character(0))
      }
    })

    # =========================================================================
    # output$warningMessage — status indicator for detected stamps
    # Requirement 1.6
    # =========================================================================
    output$warningMessage <- renderUI({
      if (is.null(data())) {
        return(HTML(as.character(div(style = "color: orange; font-size: 20px;",
                                     "No data loaded. Please load and merge stakeholder files using the Data Management module."))))
      }
      stamps <- available_stamps()
      if (nrow(stamps) > 0) {
        HTML(as.character(div(style = "color: green; font-size: 20px;",
                              paste0("Data loaded. ", nrow(stamps),
                                     " Final Selection Stamp(s) detected: ",
                                     paste(stamps$analysisIdName, collapse = ", "), "."))))
      } else {
        HTML(as.character(div(style = "color: orange; font-size: 20px;",
                              "No Final Selection Stamps (Final_prodAdv) found in the current dataset. Please complete the Pre-advancement workflow and merge files first.")))
      }
    })

    # =========================================================================
    # observeEvent(input$merge_btn) — Validate, build comparison matrix, store results
    # Requirements 2.3, 2.4, 2.5, 2.6, 3.1, 3.2, 3.3, 3.4, 3.5
    # =========================================================================
    observeEvent(input$merge_btn, {
      selected_stamps <- input$stamp_select

      # Validate at least 2 stamps selected (Requirement 2.3, 2.5)
      if (is.null(selected_stamps) || length(selected_stamps) < 2) {
        shiny::showNotification(
          "Please select at least 2 stamps to compare.",
          type = "warning",
          duration = 5
        )
        return()
      }

      dt_obj <- data()

      # Validate MTA compatibility (Requirement 3.1, 3.2, 3.3)
      mta_result <- validate_mta_compatibility(dt_obj, selected_stamps)
      if (!mta_result$compatible) {
        shiny::showNotification(
          paste0("Incompatible MTA stamps detected. The following stamps do not share the same MTA analysis: ",
                 paste(mta_result$incompatible_stamps, collapse = ", "),
                 ". Please select stamps from the same MTA."),
          type = "error",
          duration = 10
        )
        return()
      }

      # Intersect designation sets (Requirement 3.4, 3.5)
      intersection_result <- intersect_designation_sets(dt_obj, selected_stamps)
      if (length(intersection_result$warnings) > 0) {
        shiny::showNotification(
          paste0("Designation sets differ across stamps. Proceeding with the intersection (",
                 length(intersection_result$designations), " common designations). ",
                 intersection_result$warnings[1]),
          type = "warning",
          duration = 8
        )
      }

      # Build comparison matrix (Requirement 2.6, 12.1-12.5)
      cm <- build_comparison_matrix(dt_obj, selected_stamps)

      # Sort by controversy
      cm <- sort_by_controversy(cm)

      # Store comparison matrix and controversy scores
      comparison_matrix(cm)
      controversy_scores(cm$controversy_score)

      # Extract trait directions and thresholds (Requirement 10.1, 10.2)
      # Trace Final_prodAdv -> Init_prodAdv stamp to get directions/thresholds
      modeling <- dt_obj$modeling

      # Get the Init_prodAdv stamp from the first selected stamp
      init_rows <- modeling[
        modeling$module == "Final_prodAdv" &
          modeling$analysisId == selected_stamps[1] &
          modeling$parameter == "inputObject",
        ,
        drop = FALSE
      ]

      if (nrow(init_rows) > 0) {
        init_stamp <- init_rows$value[1]
        trait_info <- extract_trait_directions(dt_obj, init_stamp)
        if (nrow(trait_info) > 0) {
          dirs <- setNames(trait_info$direction, trait_info$trait)
          thresh <- setNames(trait_info$threshold, trait_info$trait)
          trait_directions(dirs)
          trait_thresholds(thresh)
        }
      }

      # Switch to Review Output tab (Requirement 2.6)
      updateTabsetPanel(session, "tabsMain", selected = "Review output")

      shiny::showNotification(
        paste0("Comparison matrix built successfully with ",
               nrow(cm), " designations and ",
               length(selected_stamps), " stakeholders."),
        type = "message",
        duration = 5
      )
    })

    # =========================================================================
    # output$auto_assigned_summary — count of auto-assigned decisions
    # Requirement 13.4
    # =========================================================================
    output$auto_assigned_summary <- renderText({
      cm <- comparison_matrix()
      if (is.null(cm)) return("0 candidates auto-assigned by full consensus.")
      auto <- auto_assign_non_controversial(cm)
      paste0(nrow(auto), " candidates auto-assigned by full consensus.")
    })

    # =========================================================================
    # Reactive: heatmap_data — filters comparison matrix based on toggle
    # Requirement 4.6
    # =========================================================================
    heatmap_data <- reactive({
      cm <- comparison_matrix()
      if (is.null(cm)) return(NULL)
      if (isTRUE(input$filter_controversial)) {
        cm <- filter_controversial_only(cm)
      }
      cm
    })

    # =========================================================================
    # output$heatmap — Agreement Heatmap via plotly
    # Requirements 4.1, 4.2, 4.3, 4.4, 4.5, 4.6
    # =========================================================================
    output$heatmap <- plotly::renderPlotly({
      cm <- heatmap_data()
      if (is.null(cm) || nrow(cm) == 0) {
        p <- plotly::plot_ly() %>%
          plotly::layout(
            title = "No data to display",
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          )
        return(p)
      }

      # Identify stakeholder columns
      all_cols <- colnames(cm)
      stakeholder_cols <- setdiff(all_cols, c("designation", "controversy_score"))
      n_designations <- nrow(cm)

      # Convert to long format for ggplot
      hm_long <- do.call(rbind, lapply(stakeholder_cols, function(st) {
        data.frame(
          designation = cm$designation,
          stakeholder = st,
          decision = as.character(cm[[st]]),
          stringsAsFactors = FALSE
        )
      }))

      # Preserve row order: top = most controversial (first in cm)
      hm_long$designation <- factor(
        hm_long$designation,
        levels = rev(cm$designation)
      )

      # Ensure decision is a factor with known levels
      hm_long$decision[is.na(hm_long$decision)] <- "NA"
      hm_long$decision <- factor(
        hm_long$decision,
        levels = c("SELECTED", "NOT SELECTED", "REVISE", "CHECK", "NA")
      )

      # Colors
      hm_colors <- c(
        "SELECTED"     = "#0072B2",
        "NOT SELECTED" = "#D55E00",
        "REVISE"       = "#F9A825",
        "CHECK"        = "#C2185B",
        "NA"           = "#CCCCCC"
      )

      # Dynamic plot height
      plot_height <- max(400, n_designations * 14)

      # Build ggplot tile heatmap
      p <- ggplot2::ggplot(hm_long, ggplot2::aes(
        x = stakeholder, y = designation, fill = decision,
        text = paste0("Designation: ", designation,
                      "\nStakeholder: ", stakeholder,
                      "\nDecision: ", decision)
      )) +
        ggplot2::geom_tile(color = "white", linewidth = 0.3) +
        ggplot2::scale_fill_manual(
          values = hm_colors,
          name = "Decision",
          drop = FALSE
        ) +
        ggplot2::labs(
          title = "Agreement Heatmap (sorted by controversy)",
          subtitle = "Most controversial candidates at top"
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10),
          axis.text.y = ggplot2::element_text(size = 7),
          axis.title = ggplot2::element_blank(),
          legend.position = "bottom",
          legend.text = ggplot2::element_text(size = 10),
          panel.grid = ggplot2::element_blank(),
          plot.title = ggplot2::element_text(size = 13, face = "bold"),
          plot.subtitle = ggplot2::element_text(size = 10, color = "grey50")
        )

      plotly::ggplotly(p, tooltip = "text", height = plot_height) %>%
        plotly::layout(legend = list(orientation = "h", x = 0.2, y = -0.15))
    })

    # =========================================================================
    # output$controversy_chart — Bar chart with 3 categories
    # Requirements 5.1, 5.2, 5.4
    # =========================================================================
    output$controversy_chart <- renderPlot({
      cm <- comparison_matrix()
      if (is.null(cm) || nrow(cm) == 0) {
        plot.new()
        text(0.5, 0.5, "No data to display", cex = 1.5, col = "gray50")
        return()
      }

      # Identify stakeholder columns
      all_cols <- colnames(cm)
      stakeholder_cols <- setdiff(all_cols, c("designation", "controversy_score"))

      # Classify each designation (excluding CHECK varieties)
      n_selected <- 0
      n_not_selected <- 0
      n_revise <- 0
      n_controversial <- 0

      for (i in seq_len(nrow(cm))) {
        decisions <- as.character(unlist(cm[i, stakeholder_cols]))
        score <- cm$controversy_score[i]

        # Skip check varieties
        if (any(decisions == "CHECK", na.rm = TRUE)) next

        if (all(decisions == "REVISE", na.rm = TRUE)) {
          n_revise <- n_revise + 1
        } else if (!is.na(score) && score == 1.0) {
          n_selected <- n_selected + 1
        } else if (!is.na(score) && score == 0.0) {
          n_not_selected <- n_not_selected + 1
        } else {
          n_controversial <- n_controversial + 1
        }
      }

      # Bar data
      counts <- c(n_selected, n_not_selected, n_revise, n_controversial)
      labels <- c("Consensus\nSELECTED", "Consensus\nNOT SELECTED", "Consensus\nREVISE", "Controversial")
      bar_colors <- c(
        MEETING_STATUS_COLORS["SELECTED"],
        MEETING_STATUS_COLORS["NOT SELECTED"],
        MEETING_STATUS_COLORS["REVISE"],
        "#999999"
      )

      # Only show bars with counts > 0
      show_mask <- counts > 0
      counts <- counts[show_mask]
      labels <- labels[show_mask]
      bar_colors <- bar_colors[show_mask]

      # Draw bar chart
      par(mar = c(5, 4, 3, 2))
      bp <- barplot(
        counts,
        names.arg = labels,
        col = bar_colors,
        border = NA,
        main = "Controversy Summary",
        ylab = "Number of Candidates",
        ylim = c(0, max(counts) * 1.2 + 1),
        cex.names = 0.9,
        las = 1
      )

      # Add count labels above bars
      text(bp, counts, labels = counts, pos = 3, cex = 1.2, font = 2)
    })

    # =========================================================================
    # output$consensus_summary_text — Summary text
    # Requirement 5.3
    # =========================================================================
    output$consensus_summary_text <- renderUI({
      cm <- comparison_matrix()
      if (is.null(cm) || nrow(cm) == 0) {
        return(tags$p(style = "color: gray;", "No comparison data available."))
      }

      # Count excluding CHECK varieties
      stakeholder_cols <- setdiff(colnames(cm), c("designation", "controversy_score"))
      n_agreement <- 0
      n_discussion <- 0

      for (i in seq_len(nrow(cm))) {
        decisions <- as.character(unlist(cm[i, stakeholder_cols]))
        score <- cm$controversy_score[i]

        # Skip check varieties
        if (any(decisions == "CHECK", na.rm = TRUE)) next

        if (all(decisions == "REVISE", na.rm = TRUE)) {
          n_discussion <- n_discussion + 1
        } else if (!is.na(score) && (score == 1.0 || score == 0.0)) {
          n_agreement <- n_agreement + 1
        } else {
          n_discussion <- n_discussion + 1
        }
      }

      tags$p(
        style = "font-size: 16px; font-weight: bold; margin-top: 10px;",
        paste0(n_agreement, " candidates with full agreement, ",
               n_discussion, " candidates requiring discussion.")
      )
    })

    # =========================================================================
    # Update detail_candidates selectizeInput choices from comparison_matrix
    # Requirements 6.1, 6.8
    # =========================================================================
    observe({
      cm <- comparison_matrix()
      if (is.null(cm) || nrow(cm) == 0) {
        updateSelectizeInput(session, "detail_candidates",
                            choices = character(0),
                            selected = character(0))
        return()
      }

      # Build labelled choices with consensus status
      stakeholder_cols <- setdiff(colnames(cm), c("designation", "controversy_score"))

      status_labels <- vapply(seq_len(nrow(cm)), function(i) {
        decisions <- as.character(unlist(cm[i, stakeholder_cols]))
        score <- cm$controversy_score[i]

        if (any(decisions == "CHECK", na.rm = TRUE)) {
          "CHECK"
        } else if (all(decisions == "REVISE", na.rm = TRUE)) {
          "CONSENSUS - REVISE"
        } else if (!is.na(score) && score == 1.0) {
          "CONSENSUS - SELECTED"
        } else if (!is.na(score) && score == 0.0) {
          "CONSENSUS - NOT SELECTED"
        } else {
          "CONTROVERSIAL"
        }
      }, character(1))

      # Sort order: Controversial, Check, Consensus Selected, Consensus Not Selected
      sort_priority <- c(
        "CONTROVERSIAL" = 1,
        "CONSENSUS - REVISE" = 2,
        "CHECK" = 3,
        "CONSENSUS - SELECTED" = 4,
        "CONSENSUS - NOT SELECTED" = 5
      )
      sort_order <- order(sort_priority[status_labels], cm$designation)

      desigs <- cm$designation[sort_order]
      labels <- paste0(cm$designation[sort_order], " [", status_labels[sort_order], "]")

      choices <- setNames(desigs, labels)
      updateSelectizeInput(session, "detail_candidates",
                          choices = choices,
                          selected = character(0))
    })

    # =========================================================================
    # output$radar_plot — Radar/spider plot for up to 5 candidates
    # Requirements 6.3, 6.4, 6.5, 6.9, 10.1, 10.3, 10.4
    # =========================================================================
    output$radar_plot <- plotly::renderPlotly({
      req(input$detail_candidates)

      selected_desigs <- input$detail_candidates
      dt_obj <- data()
      cm <- comparison_matrix()
      dirs <- trait_directions()

      req(dt_obj, cm, dirs)

      # Colorblind-friendly palette (up to 5 candidates)
      # Use STATUS_COLORS for CHECK designations, distinct palette for others
      candidate_palette <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e")

      # Determine the MTA stamp for predictions
      selected_stamps <- input$stamp_select
      modeling <- dt_obj$modeling
      init_rows <- modeling[
        modeling$module == "Final_prodAdv" &
          modeling$analysisId == selected_stamps[1] &
          modeling$parameter == "inputObject",
        ,
        drop = FALSE
      ]
      if (nrow(init_rows) == 0) return(plotly::plot_ly())
      init_stamp <- init_rows$value[1]

      mta_rows <- modeling[
        modeling$module == "Init_prodAdv" &
          modeling$analysisId == init_stamp &
          modeling$parameter == "mta_stamp",
        ,
        drop = FALSE
      ]
      if (nrow(mta_rows) == 0) return(plotly::plot_ly())
      mta_stamp <- mta_rows$value[1]

      # Get predictions for the MTA stamp
      preds <- dt_obj$predictions
      mta_preds <- preds[
        preds$analysisId == mta_stamp &
          preds$effectType == "designation",
        ,
        drop = FALSE
      ]

      if (nrow(mta_preds) == 0) return(plotly::plot_ly())

      # Get traits from directions
      traits <- names(dirs)

      # Filter predictions to selected designations and available traits
      plot_preds <- mta_preds[
        mta_preds$designation %in% selected_desigs &
          mta_preds$trait %in% traits,
        ,
        drop = FALSE
      ]

      if (nrow(plot_preds) == 0) return(plotly::plot_ly())

      # Build normalized values per trait
      p <- plotly::plot_ly(type = "scatterpolar", mode = "lines+markers")

      # Determine color for each candidate
      # CHECK designations get STATUS_COLORS["CHECK"], others get palette colors
      stakeholder_cols <- setdiff(colnames(cm), c("designation", "controversy_score"))
      non_check_idx <- 0

      for (idx in seq_along(selected_desigs)) {
        desig <- selected_desigs[idx]

        # Determine if this is a CHECK designation
        desig_row <- cm[cm$designation == desig, , drop = FALSE]
        is_check <- FALSE
        if (nrow(desig_row) > 0) {
          desig_decisions <- as.character(unlist(desig_row[1, stakeholder_cols]))
          is_check <- any(desig_decisions == "CHECK", na.rm = TRUE)
        }

        if (is_check) {
          color <- MEETING_STATUS_COLORS["CHECK"]
        } else {
          non_check_idx <- non_check_idx + 1
          color <- candidate_palette[((non_check_idx - 1) %% length(candidate_palette)) + 1]
        }

        # Get normalized values for this candidate across traits
        norm_vals <- numeric(length(traits))
        for (t_idx in seq_along(traits)) {
          tr <- traits[t_idx]
          # All values for this trait (for normalization context)
          all_trait_vals <- mta_preds$predictedValue[mta_preds$trait == tr]
          # This candidate's value
          cand_val <- plot_preds$predictedValue[
            plot_preds$designation == desig & plot_preds$trait == tr
          ]
          if (length(cand_val) == 0 || is.na(cand_val[1])) {
            norm_vals[t_idx] <- NA
          } else {
            norm_vals[t_idx] <- normalize_trait_values(
              c(all_trait_vals, cand_val[1]),
              dirs[tr]
            )[length(all_trait_vals) + 1]
          }
        }

        # Close the polygon (repeat first value)
        theta_vals <- c(traits, traits[1])
        r_vals <- c(norm_vals, norm_vals[1])

        # Determine consensus label for this designation
        consensus_label <- if (is_check) {
          "CHECK"
        } else if (nrow(desig_row) > 0) {
          score <- desig_row$controversy_score[1]
          if (!is.na(score) && score == 1.0) "CONSENSUS - SELECTED"
          else if (!is.na(score) && score == 0.0) "CONSENSUS - NOT SELECTED"
          else "CONTROVERSIAL"
        } else {
          ""
        }
        desig_label <- paste0(desig, " [", consensus_label, "]")

        p <- plotly::add_trace(
          p,
          theta = theta_vals,
          r = r_vals,
          name = desig_label,
          line = list(color = color),
          marker = list(color = color),
          fill = "toself",
          fillcolor = paste0(color, "33"),
          opacity = 0.7
        )
      }

      p <- plotly::layout(
        p,
        polar = list(
          radialaxis = list(visible = TRUE, range = c(0, 1)),
          angularaxis = list(direction = "clockwise")
        ),
        showlegend = TRUE,
        legend = list(x = 1.05, y = 1),
        title = "Trait Performance Comparison (Normalized)"
      )

      p
    })

    # =========================================================================
    # output$detail_table — Decision table with direction-aware gradient coloring
    # Requirements 6.2, 6.6, 10.2, 10.5
    # =========================================================================
    output$detail_table <- DT::renderDataTable({
      req(input$detail_candidates)

      selected_desigs <- input$detail_candidates
      dt_obj <- data()
      dirs <- trait_directions()
      thresholds <- trait_thresholds()

      req(dt_obj, dirs)

      # Determine the MTA stamp for predictions
      selected_stamps <- input$stamp_select
      modeling <- dt_obj$modeling
      init_rows <- modeling[
        modeling$module == "Final_prodAdv" &
          modeling$analysisId == selected_stamps[1] &
          modeling$parameter == "inputObject",
        ,
        drop = FALSE
      ]
      if (nrow(init_rows) == 0) return(DT::datatable(data.frame()))
      init_stamp <- init_rows$value[1]

      mta_rows <- modeling[
        modeling$module == "Init_prodAdv" &
          modeling$analysisId == init_stamp &
          modeling$parameter == "mta_stamp",
        ,
        drop = FALSE
      ]
      if (nrow(mta_rows) == 0) return(DT::datatable(data.frame()))
      mta_stamp <- mta_rows$value[1]

      # Get predictions
      preds <- dt_obj$predictions
      mta_preds <- preds[
        preds$analysisId == mta_stamp &
          preds$effectType == "designation",
        ,
        drop = FALSE
      ]

      traits <- names(dirs)

      # Build wide table: designations as rows, traits as columns
      wide_df <- data.frame(Designation = selected_desigs, stringsAsFactors = FALSE)

      for (tr in traits) {
        vals <- vapply(selected_desigs, function(d) {
          v <- mta_preds$predictedValue[
            mta_preds$designation == d & mta_preds$trait == tr
          ]
          if (length(v) == 0) NA_real_ else v[1]
        }, numeric(1))
        wide_df[[tr]] <- round(vals, 3)
      }

      # Annotate threshold failures with a marker
      # Create a separate annotation data frame
      annotations <- wide_df
      for (tr in traits) {
        thresh <- if (!is.null(thresholds) && tr %in% names(thresholds)) thresholds[tr] else NA_real_
        dir <- dirs[tr]
        if (!is.na(thresh)) {
          failures <- detect_threshold_failure(wide_df[[tr]], thresh, dir)
          # Mark failures with red triangle indicator
          annotations[[tr]] <- ifelse(
            failures & !is.na(wide_df[[tr]]),
            paste0(wide_df[[tr]], " \u26A0"),
            as.character(wide_df[[tr]])
          )
        } else {
          annotations[[tr]] <- as.character(wide_df[[tr]])
        }
      }

      # Build display data frame with annotations
      display_df <- data.frame(Designation = selected_desigs, stringsAsFactors = FALSE)
      for (tr in traits) {
        display_df[[tr]] <- annotations[[tr]]
      }

      # Create DT datatable with direction-aware gradient coloring
      dt <- DT::datatable(
        display_df,
        options = list(
          dom = "t",
          paging = FALSE,
          scrollX = TRUE,
          columnDefs = list(
            list(className = "dt-center", targets = "_all")
          )
        ),
        rownames = FALSE,
        escape = FALSE,
        caption = "Trait values with direction-aware coloring. \u26A0 = threshold failure."
      )

      # Apply direction-aware gradient styling to each trait column
      for (col_idx in seq_along(traits)) {
        tr <- traits[col_idx]
        col_vals <- wide_df[[tr]]
        col_vals <- col_vals[!is.na(col_vals)]

        if (length(col_vals) < 2) next

        dir <- dirs[tr]
        # Compute gradient colors: green for better, white for worse
        if (dir == "Higher is better") {
          # Higher values -> darker green
          dt <- DT::formatStyle(
            dt,
            columns = tr,
            backgroundColor = DT::styleInterval(
              sort(col_vals[-length(col_vals)]),
              paste0("rgba(76, 175, 80, ",
                     seq(0.1, 0.8, length.out = length(col_vals)), ")")
            )
          )
        } else {
          # Lower values -> darker green (reverse)
          dt <- DT::formatStyle(
            dt,
            columns = tr,
            backgroundColor = DT::styleInterval(
              sort(col_vals[-length(col_vals)]),
              paste0("rgba(76, 175, 80, ",
                     seq(0.8, 0.1, length.out = length(col_vals)), ")")
            )
          )
        }
      }

      dt
    })

    # =========================================================================
    # output$stakeholder_breakdown — Per-candidate stakeholder decision breakdown
    # Requirements 6.7
    # =========================================================================
    output$stakeholder_breakdown <- renderUI({
      req(input$detail_candidates)

      selected_desigs <- input$detail_candidates
      cm <- comparison_matrix()

      req(cm)

      stakeholder_cols <- setdiff(colnames(cm), c("designation", "controversy_score"))

      # Build UI for each selected designation
      breakdown_panels <- lapply(selected_desigs, function(desig) {
        row <- cm[cm$designation == desig, , drop = FALSE]
        if (nrow(row) == 0) return(NULL)

        # Group stakeholders by decision
        decisions <- as.character(unlist(row[1, stakeholder_cols]))
        names(decisions) <- stakeholder_cols

        # Group by decision category
        decision_groups <- split(names(decisions), decisions)

        group_tags <- lapply(names(decision_groups), function(dec) {
          color <- get_status_color(dec)
          stakeholders_in_group <- decision_groups[[dec]]
          tags$div(
            style = paste0(
              "display: inline-block; margin: 4px 8px; padding: 4px 10px; ",
              "border-left: 4px solid ", color, "; ",
              "background-color: ", paste0(color, "1A"), ";"
            ),
            tags$strong(style = paste0("color: ", color, ";"), dec),
            tags$span(": ", paste(stakeholders_in_group, collapse = ", "))
          )
        })

        tags$div(
          style = "margin-bottom: 12px; padding: 8px; border: 1px solid #eee; border-radius: 4px;",
          tags$strong(desig),
          tags$div(group_tags)
        )
      })

      tags$div(
        tags$h4(strong("Stakeholder Decision Breakdown")),
        breakdown_panels
      )
    })

    # =========================================================================
    # discussion_candidates() reactive — designations needing discussion
    # Requirements 7.1, 13.6, 13.7
    # =========================================================================
    discussion_candidates <- reactive({
      cm <- comparison_matrix()
      req(cm)
      requires_discussion(cm)
    })

    # =========================================================================
    # discussion_status reactiveVal — named character vector (designation -> status)
    # Requirement 7.6, 7.7
    # =========================================================================
    discussion_status <- reactiveVal(NULL)

    # Initialize discussion_status when comparison_matrix changes
    observeEvent(comparison_matrix(), {
      candidates <- requires_discussion(comparison_matrix())
      if (length(candidates) > 0) {
        statuses <- setNames(rep("Discuss", length(candidates)), candidates)
        discussion_status(statuses)
      } else {
        discussion_status(character(0))
      }
      # Reset navigation index
      current_candidate_idx(1)
      # Initialize meeting_decisions as empty named list
      meeting_decisions(list())
    })

    # =========================================================================
    # output$candidate_card — renderUI for Joint Decision Workflow card
    # Requirement 7.1, 7.2
    # =========================================================================
    output$candidate_card <- renderUI({
      candidates <- discussion_candidates()
      req(candidates)
      idx <- current_candidate_idx()
      req(idx >= 1 && idx <= length(candidates))

      cm <- comparison_matrix()
      req(cm)

      current_designation <- candidates[idx]

      # Get the row for this designation
      row_idx <- which(cm$designation == current_designation)
      if (length(row_idx) == 0) return(NULL)

      row_data <- cm[row_idx, , drop = FALSE]

      # Get controversy score
      score <- row_data$controversy_score

      # Get stakeholder columns
      all_cols <- colnames(cm)
      stakeholder_cols <- setdiff(all_cols, c("designation", "controversy_score"))

      # Build stakeholder decision spans (color-coded)
      stakeholder_spans <- lapply(stakeholder_cols, function(sn) {
        decision <- as.character(row_data[[sn]])
        color <- get_status_color(decision)
        tags$span(
          style = paste0("display: inline-block; margin: 3px 6px; padding: 4px 10px; ",
                         "border-radius: 4px; color: white; font-weight: bold; ",
                         "background-color: ", color, ";"),
          paste0(sn, ": ", decision)
        )
      })

      # Brief trait summary from predictions (if available)
      trait_summary_ui <- NULL
      trait_dirs <- trait_directions()
      mta_stamp <- NULL

      # Try to get MTA stamp for predictions lookup
      dt_obj <- data()
      selected_stamps <- input$stamp_select
      if (!is.null(dt_obj) && !is.null(selected_stamps) && length(selected_stamps) > 0) {
        modeling <- dt_obj$modeling
        init_rows <- modeling[
          modeling$module == "Final_prodAdv" &
            modeling$analysisId == selected_stamps[1] &
            modeling$parameter == "inputObject",
          , drop = FALSE
        ]
        if (nrow(init_rows) > 0) {
          init_stamp <- init_rows$value[1]
          mta_rows <- modeling[
            modeling$module == "Init_prodAdv" &
              modeling$analysisId == init_stamp &
              modeling$parameter == "mta_stamp",
            , drop = FALSE
          ]
          if (nrow(mta_rows) > 0) {
            mta_stamp <- mta_rows$value[1]
          }
        }
      }

      if (!is.null(mta_stamp) && !is.null(dt_obj$predictions)) {
        preds <- dt_obj$predictions
        cand_preds <- preds[
          preds$analysisId == mta_stamp &
            preds$designation == current_designation &
            preds$effectType == "designation",
          , drop = FALSE
        ]
        if (nrow(cand_preds) > 0) {
          trait_items <- lapply(seq_len(nrow(cand_preds)), function(j) {
            tags$li(paste0(cand_preds$trait[j], ": ",
                           round(cand_preds$predictedValue[j], 3)))
          })
          trait_summary_ui <- tags$div(
            tags$strong("Trait Summary:"),
            tags$ul(trait_items)
          )
        }
      }

      # Get discussion status for this candidate
      ds <- discussion_status()
      status_label <- if (!is.null(ds) && current_designation %in% names(ds)) {
        ds[[current_designation]]
      } else {
        "Discuss"
      }
      status_color <- if (status_label == "Resolved") "#28a745" else "#ffc107"

      # Build card UI
      tags$div(
        tags$h4(
          style = "margin-bottom: 5px;",
          paste0("Candidate ", idx, " of ", length(candidates), ": "),
          tags$strong(current_designation)
        ),
        tags$span(
          style = paste0("display: inline-block; padding: 2px 8px; border-radius: 4px; ",
                         "color: white; background-color: ", status_color, "; font-size: 12px;"),
          status_label
        ),
        tags$p(
          style = "margin-top: 8px;",
          tags$strong("Controversy Score: "),
          round(score, 3)
        ),
        tags$div(
          style = "margin-top: 8px;",
          tags$strong("Stakeholder Decisions:"),
          tags$div(style = "margin-top: 4px;", stakeholder_spans)
        ),
        if (!is.null(trait_summary_ui)) trait_summary_ui
      )
    })

    # =========================================================================
    # output$progress_indicator — "X of Y controversial candidates resolved."
    # Requirement 7.7
    # =========================================================================
    output$progress_indicator <- renderText({
      candidates <- discussion_candidates()
      ds <- discussion_status()
      if (is.null(candidates) || length(candidates) == 0) {
        return("No controversial candidates to resolve.")
      }
      if (is.null(ds)) {
        return(paste0("0 of ", length(candidates), " controversial candidates resolved."))
      }
      resolved_count <- sum(ds == "Resolved", na.rm = TRUE)
      paste0(resolved_count, " of ", length(candidates), " controversial candidates resolved.")
    })

    # =========================================================================
    # output$selection_tally — Running count of selected vs total (excl. checks)
    # =========================================================================
    output$selection_tally <- renderUI({
      cm <- comparison_matrix()
      if (is.null(cm) || nrow(cm) == 0) return(NULL)

      # Reactive dependencies for update triggers
      input$vote_decision
      input$majority_btn
      input$toggle_status_btn
      ds <- discussion_status()
      md <- meeting_decisions()

      # Get all decisions: auto-assigned + discussed
      auto <- auto_assign_non_controversial(cm)
      current_decisions <- if (is.null(md)) list() else md

      # Combine into a single decision per designation
      all_decisions <- auto
      if (length(current_decisions) > 0) {
        disc_df <- data.frame(
          designation = names(current_decisions),
          decision = as.character(unlist(current_decisions)),
          stringsAsFactors = FALSE
        )
        all_decisions <- rbind(disc_df, all_decisions)
        all_decisions <- all_decisions[!duplicated(all_decisions$designation), , drop = FALSE]
      }

      # Exclude CHECK designations from the tally
      stakeholder_cols <- setdiff(colnames(cm), c("designation", "controversy_score"))
      check_desigs <- character(0)
      for (i in seq_len(nrow(cm))) {
        decisions <- as.character(unlist(cm[i, stakeholder_cols]))
        if (any(decisions == "CHECK", na.rm = TRUE)) {
          check_desigs <- c(check_desigs, cm$designation[i])
        }
      }

      total_candidates <- nrow(cm) - length(check_desigs)
      selected_count <- sum(
        all_decisions$decision == "SELECTED" & !all_decisions$designation %in% check_desigs,
        na.rm = TRUE
      )

      tags$p(
        style = "font-size: 14px;",
        paste0("Selection: ", selected_count, " selected / ", total_candidates, " total candidates (excluding checks)")
      )
    })

    # =========================================================================
    # output$candidate_context_table — Color-coded trait table for population context
    # =========================================================================
    output$candidate_context_table <- DT::renderDT({
      candidates <- discussion_candidates()
      req(candidates)
      idx <- current_candidate_idx()
      req(idx >= 1 && idx <= length(candidates))

      cm <- comparison_matrix()
      req(cm)
      dt_obj <- data()
      req(dt_obj)

      current_designation <- candidates[idx]

      # Resolve MTA stamp
      selected_stamps <- input$stamp_select
      req(selected_stamps)
      modeling <- dt_obj$modeling
      init_rows <- modeling[
        modeling$module == "Final_prodAdv" &
          modeling$analysisId == selected_stamps[1] &
          modeling$parameter == "inputObject",
        , drop = FALSE
      ]
      req(nrow(init_rows) > 0)
      init_stamp <- init_rows$value[1]

      mta_rows <- modeling[
        modeling$module == "Init_prodAdv" &
          modeling$analysisId == init_stamp &
          modeling$parameter == "mta_stamp",
        , drop = FALSE
      ]
      req(nrow(mta_rows) > 0)
      mta_stamp <- mta_rows$value[1]

      # Get predictions
      preds <- dt_obj$predictions
      mta_preds <- preds[
        preds$analysisId == mta_stamp &
          preds$effectType == "designation" &
          preds$designation %in% cm$designation,
        , drop = FALSE
      ]
      req(nrow(mta_preds) > 0)

      # Get traits
      dirs <- trait_directions()
      traits <- if (!is.null(dirs)) names(dirs) else unique(mta_preds$trait)

      # Build wide-format table: designation + traits + index
      pred_wide <- reshape(
        mta_preds[mta_preds$trait %in% traits, c("designation", "trait", "predictedValue"), drop = FALSE],
        idvar = "designation", timevar = "trait", direction = "wide"
      )
      names(pred_wide) <- sub("^predictedValue\\.", "", names(pred_wide))

      # Compute index
      weight_rows <- modeling[
        modeling$analysisId == init_stamp & modeling$parameter == "index_weight",
        , drop = FALSE
      ]
      if (nrow(weight_rows) > 0) {
        idx_weights <- as.numeric(weight_rows$value)
        names(idx_weights) <- weight_rows$trait
        avail_traits <- intersect(names(idx_weights), colnames(pred_wide))
        if (length(avail_traits) > 0) {
          trait_mat <- as.matrix(pred_wide[, avail_traits, drop = FALSE])
          scaled_mat <- scale(trait_mat)
          scaled_mat[is.nan(scaled_mat)] <- 0
          w <- idx_weights[avail_traits]

          # Apply reliability weighting
          if ("reliability" %in% colnames(mta_preds)) {
            rel_data <- reshape(
              mta_preds[mta_preds$trait %in% avail_traits, c("designation", "trait", "reliability"), drop = FALSE],
              idvar = "designation", timevar = "trait", direction = "wide"
            )
            names(rel_data) <- sub("^reliability\\.", "", names(rel_data))
            rel_avail <- intersect(avail_traits, colnames(rel_data))
            if (length(rel_avail) == length(avail_traits)) {
              rel_matrix <- as.matrix(rel_data[match(pred_wide$designation, rel_data$designation), avail_traits, drop = FALSE])
              rel_matrix[is.na(rel_matrix)] <- 0
              rel_matrix <- pmax(0, pmin(1, rel_matrix))
              reliability_penalized <- scaled_mat * sqrt(rel_matrix)
              pred_wide$index_value <- as.numeric(reliability_penalized %*% w)
            } else {
              pred_wide$index_value <- as.numeric(scaled_mat %*% w)
            }
          } else {
            pred_wide$index_value <- as.numeric(scaled_mat %*% w)
          }
        } else {
          pred_wide$index_value <- NA_real_
        }
      } else {
        pred_wide$index_value <- NA_real_
      }

      # Classify each designation's consensus status
      stakeholder_cols <- setdiff(colnames(cm), c("designation", "controversy_score"))
      desig_status <- vapply(pred_wide$designation, function(d) {
        row <- cm[cm$designation == d, , drop = FALSE]
        if (nrow(row) == 0) return("CONTROVERSIAL")
        decisions <- as.character(unlist(row[1, stakeholder_cols]))
        if (any(decisions == "CHECK", na.rm = TRUE)) return("CHECK")
        if (all(decisions == "REVISE", na.rm = TRUE)) return("REVISE")
        score <- row$controversy_score[1]
        if (!is.na(score) && score == 1.0) return("SELECTED")
        if (!is.na(score) && score == 0.0) return("NOT SELECTED")
        "CONTROVERSIAL"
      }, character(1))

      pred_wide$status <- desig_status

      # Sort by index descending
      pred_wide <- pred_wide[order(-pred_wide$index_value), , drop = FALSE]

      # Determine which designations need discussion (grey)
      needs_discussion <- candidates  # all discussion candidates

      # Build color-coded HTML table
      # Colors: SELECTED=#85C1E9, NOT SELECTED=#E8A87C, CHECK=#C39BD3, grey for controversial
      col_selected <- "#85C1E9"
      col_not_selected <- "#E8A87C"
      col_check <- "#C39BD3"
      col_controversial <- "#FFF8DC"  # Muted pale yellow for discussion candidates
      col_highlight <- "#FFD700"  # Bright gold for current candidate

      # Columns to display: designation, index_value, traits
      display_cols <- c("index_value", intersect(traits, colnames(pred_wide)))

      # Build quintile opacity per column
      get_quintile_bg <- function(values, status_vec, base_col) {
        n <- length(values)
        bg <- rep("#FFFFFF", n)
        numeric_vals <- as.numeric(values)
        valid <- !is.na(numeric_vals)
        if (sum(valid) < 5) {
          bg[valid] <- base_col
          return(bg)
        }
        quants <- quantile(numeric_vals[valid], probs = c(0.2, 0.4, 0.6, 0.8), na.rm = TRUE)
        for (i in which(valid)) {
          v <- numeric_vals[i]
          if (v >= quants[4]) op <- 1.0
          else if (v >= quants[3]) op <- 0.75
          else if (v >= quants[2]) op <- 0.5
          else if (v >= quants[1]) op <- 0.3
          else op <- 0.15
          # Blend base_col with white at given opacity
          r <- strtoi(substr(base_col, 2, 3), 16)
          g <- strtoi(substr(base_col, 4, 5), 16)
          b <- strtoi(substr(base_col, 6, 7), 16)
          r2 <- as.integer(r * op + 255 * (1 - op))
          g2 <- as.integer(g * op + 255 * (1 - op))
          b2 <- as.integer(b * op + 255 * (1 - op))
          bg[i] <- sprintf("#%02X%02X%02X", r2, g2, b2)
        }
        bg
      }

      # Build display HTML
      display_df <- data.frame(designation = pred_wide$designation, stringsAsFactors = FALSE)

      for (col in display_cols) {
        vals <- pred_wide[[col]]
        # Determine base color per row based on status
        cell_html <- vapply(seq_len(nrow(pred_wide)), function(i) {
          d <- pred_wide$designation[i]
          st <- pred_wide$status[i]
          v <- vals[i]
          val_str <- if (is.na(v)) "" else format(round(as.numeric(v), 3), nsmall = 3)

          # Determine background color
          if (d == current_designation) {
            bg <- col_highlight
          } else if (d %in% needs_discussion) {
            bg <- col_controversial
          } else if (st == "SELECTED") {
            # Use quantile-based intensity
            bg <- col_selected
          } else if (st == "NOT SELECTED") {
            bg <- col_not_selected
          } else if (st == "CHECK") {
            bg <- col_check
          } else {
            bg <- col_controversial
          }

          # For non-grey/non-highlight, apply opacity based on quantile rank
          if (!d %in% c(current_designation, needs_discussion) && st %in% c("SELECTED", "NOT SELECTED")) {
            valid_vals <- as.numeric(vals[!is.na(vals)])
            if (length(valid_vals) >= 5 && !is.na(v)) {
              quants <- quantile(valid_vals, probs = c(0.2, 0.4, 0.6, 0.8))
              nv <- as.numeric(v)
              op <- if (nv >= quants[4]) 1.0 else if (nv >= quants[3]) 0.75 else if (nv >= quants[2]) 0.5 else if (nv >= quants[1]) 0.3 else 0.15
              base <- if (st == "SELECTED") col_selected else col_not_selected
              r <- strtoi(substr(base, 2, 3), 16); g <- strtoi(substr(base, 4, 5), 16); b <- strtoi(substr(base, 6, 7), 16)
              r2 <- as.integer(r * op + 255 * (1 - op)); g2 <- as.integer(g * op + 255 * (1 - op)); b2 <- as.integer(b * op + 255 * (1 - op))
              bg <- sprintf("#%02X%02X%02X", r2, g2, b2)
            }
          }

          sprintf("<div style='background:%s; padding:4px; border-radius:3px; text-align:right;'>%s</div>", bg, val_str)
        }, character(1))

        display_df[[col]] <- cell_html
      }

      # Style the designation column: bold + highlight for current
      display_df$designation <- vapply(seq_len(nrow(pred_wide)), function(i) {
        d <- pred_wide$designation[i]
        if (d == current_designation) {
          sprintf("<div style='background:%s; padding:4px; border-radius:3px; font-weight:bold;'>%s &#9654;</div>", col_highlight, d)
        } else {
          d
        }
      }, character(1))

      DT::datatable(
        display_df,
        escape = FALSE,
        rownames = FALSE,
        selection = "none",
        options = list(
          scrollX = TRUE,
          scrollY = "400px",
          paging = FALSE,
          searching = FALSE,
          ordering = FALSE,
          autoWidth = FALSE
        )
      )
    })

    # =========================================================================
    # observeEvent(input$prev_btn) — Navigate to previous candidate
    # Requirement 7.1
    # =========================================================================
    observeEvent(input$prev_btn, {
      idx <- current_candidate_idx()
      if (idx > 1) {
        current_candidate_idx(idx - 1)
      }
    })

    # =========================================================================
    # observeEvent(input$next_btn) — Navigate to next candidate
    # Requirement 7.1
    # =========================================================================
    observeEvent(input$next_btn, {
      idx <- current_candidate_idx()
      candidates <- discussion_candidates()
      if (!is.null(candidates) && idx < length(candidates)) {
        current_candidate_idx(idx + 1)
      }
    })

    # =========================================================================
    # observeEvent(input$majority_btn) — Compute and apply majority vote
    # Requirement 7.4
    # =========================================================================
    observeEvent(input$majority_btn, {
      candidates <- discussion_candidates()
      req(candidates)
      idx <- current_candidate_idx()
      req(idx >= 1 && idx <= length(candidates))

      cm <- comparison_matrix()
      req(cm)

      current_designation <- candidates[idx]

      # Get stakeholder decisions for this designation
      row_idx <- which(cm$designation == current_designation)
      if (length(row_idx) == 0) return()

      all_cols <- colnames(cm)
      stakeholder_cols <- setdiff(all_cols, c("designation", "controversy_score"))

      decisions_vector <- as.character(unlist(cm[row_idx, stakeholder_cols]))

      # Compute majority vote using helper
      majority_decision <- compute_majority_vote(decisions_vector)

      # Store in meeting_decisions
      current_decisions <- meeting_decisions()
      if (is.null(current_decisions)) current_decisions <- list()
      current_decisions[[current_designation]] <- majority_decision
      meeting_decisions(current_decisions)

      # Update the radio button to reflect the majority vote
      updateRadioButtons(session, "vote_decision", selected = majority_decision)

      shiny::showNotification(
        paste0("Majority vote applied for '", current_designation, "': ", majority_decision),
        type = "message",
        duration = 3
      )
    })

    # =========================================================================
    # observeEvent(input$vote_decision) — Record manual override
    # Requirement 7.5
    # =========================================================================
    observeEvent(input$vote_decision, {
      # Guard: only act when comparison_matrix is available
      req(comparison_matrix())
      candidates <- discussion_candidates()
      req(candidates)
      idx <- current_candidate_idx()
      req(idx >= 1 && idx <= length(candidates))

      current_designation <- candidates[idx]
      override_value <- input$vote_decision

      # Apply override using helper
      current_decisions <- meeting_decisions()
      if (is.null(current_decisions)) current_decisions <- list()

      existing <- current_decisions[[current_designation]]
      if (is.null(existing)) existing <- "NOT SELECTED"
      final_decision <- apply_override(existing, override_value)

      current_decisions[[current_designation]] <- final_decision
      meeting_decisions(current_decisions)
    })

    # =========================================================================
    # observeEvent(input$toggle_status_btn) — Toggle discussion status
    # Requirement 7.6, 7.8
    # =========================================================================
    observeEvent(input$toggle_status_btn, {
      candidates <- discussion_candidates()
      req(candidates)
      idx <- current_candidate_idx()
      req(idx >= 1 && idx <= length(candidates))

      current_designation <- candidates[idx]
      ds <- discussion_status()
      req(ds)

      # Toggle using helper
      current_status <- ds[[current_designation]]
      new_status <- toggle_discussion_status(current_status)
      ds[[current_designation]] <- new_status
      discussion_status(ds)

      # Check if all are resolved (Requirement 7.8)
      if (all(ds == "Resolved")) {
        shiny::showNotification(
          "All controversial candidates have been resolved! You can now save meeting decisions.",
          type = "message",
          duration = 8
        )
        # Highlight save button by adding a CSS class via JavaScript
        shinyjs_available <- requireNamespace("shinyjs", quietly = TRUE)
        if (shinyjs_available) {
          # Fallback: use inline script to highlight save button
        }
        # Use insertUI to inject a style for highlighting the save button
        shiny::insertUI(
          selector = paste0("#", ns("save_btn")),
          where = "afterEnd",
          ui = tags$script(HTML(paste0(
            "document.getElementById('", ns("save_btn"), "').style.boxShadow = '0 0 15px 5px #28a745';",
            "document.getElementById('", ns("save_btn"), "').style.border = '2px solid #28a745';"
          ))),
          immediate = TRUE
        )
      }
    })

    # =========================================================================
    # observeEvent(input$save_btn) — Save meeting decisions
    # Requirements 8.1, 8.2, 8.3, 8.4, 8.5, 8.6, 8.7, 13.1, 13.2, 13.3, 13.5, 13.6, 13.7
    # =========================================================================
    observeEvent(input$save_btn, {
      # Validate Meeting_Name is not empty (Requirement 8.2)
      meeting_name <- trimws(input$meeting_name)
      if (is.null(meeting_name) || nchar(meeting_name) == 0) {
        shiny::showNotification(
          "Please enter a Meeting Name before saving.",
          type = "error",
          duration = 5
        )
        return()
      }

      cm <- comparison_matrix()
      if (is.null(cm) || nrow(cm) == 0) {
        shiny::showNotification(
          "No comparison matrix available. Please merge stamps first.",
          type = "error",
          duration = 5
        )
        return()
      }

      # Build auto-assigned decisions (Requirement 13.1, 13.2, 13.3, 13.5)
      auto_assigned <- auto_assign_non_controversial(cm)

      # Build discussion decisions from meeting_decisions() reactiveVal (Requirement 13.6, 13.7)
      current_decisions <- meeting_decisions()
      if (is.null(current_decisions)) current_decisions <- list()

      # Convert named list to data.frame
      if (length(current_decisions) > 0) {
        discussion_df <- data.frame(
          designation = names(current_decisions),
          decision    = as.character(unlist(current_decisions)),
          stringsAsFactors = FALSE
        )
      } else {
        discussion_df <- data.frame(
          designation = character(0),
          decision    = character(0),
          stringsAsFactors = FALSE
        )
      }

      # Assemble final decisions combining discussed + auto-assigned (Requirement 8.3)
      final_decisions <- assemble_final_decisions(discussion_df, auto_assigned)

      # Generate analysisId as numeric timestamp (Requirement 8.4)
      new_analysis_id <- as.numeric(Sys.time())

      # Get selected stamp IDs
      selected_stamps <- input$stamp_select

      # Get participant names from stamp analysisIdName in status table
      dt_obj <- data()
      status <- dt_obj$status
      participants <- status$analysisIdName[match(selected_stamps, status$analysisId)]
      participants <- participants[!is.na(participants)]

      # Call saveMeetingProdAdvSelection wrapped in tryCatch (Requirement 8.4, 8.5, 8.6, 8.7)
      tryCatch({
        # Resolve MTA stamp from selected stakeholder stamps
        mta_result <- validate_mta_compatibility(dt_obj, selected_stamps)
        mta_stamp_value <- if (mta_result$compatible) mta_result$mta_stamp else NA_character_

        if (is.na(mta_stamp_value) || !nzchar(mta_stamp_value)) {
          shiny::showNotification(
            "Could not determine the shared MTA stamp from selected stakeholder stamps.",
            type = "error",
            duration = 8
          )
          return()
        }

        updated_dt <- cgiarPipeline::saveMeetingProdAdvSelection(
          analysisId        = new_analysis_id,
          analysisIdName    = meeting_name,
          stakeholderStamps = selected_stamps,
          mtaStamp          = mta_stamp_value,
          meetingDecisions  = final_decisions,
          participants      = participants,
          dt_object         = dt_obj
        )

        # On success: update data() reactive (Requirement 8.5)
        data(updated_dt)

        # Show success notification (Requirement 8.6)
        shiny::showNotification(
          paste0("Meeting decisions saved successfully as '", meeting_name, "'."),
          type = "message",
          duration = 8
        )

        # Switch to Output tab (Requirement 8.6)
        updateTabsetPanel(session, "tabsMain", selected = "Output")

      }, error = function(e) {
        # On error: show error notification (Requirement 8.7)
        shiny::showNotification(
          paste0("Error saving meeting decisions: ", conditionMessage(e)),
          type = "error",
          duration = 10
        )
      })
    })

    # =========================================================================
    # Output tab: rendered Rmd report or informational message
    # Requirements 11.1, 11.2, 11.7, 11.8
    # =========================================================================
    output$rmd_report <- renderUI({
      # Check if a Meeting_prodAdv stamp exists in the data
      dt_obj <- data()
      has_meeting_stamp <- FALSE
      if (!is.null(dt_obj) && !is.null(dt_obj$status)) {
        has_meeting_stamp <- any(dt_obj$status$module == "Meeting_prodAdv")
      }

      if (!has_meeting_stamp) {
        return(tags$div(
          style = "color: #31708f; background-color: #d9edf7; border: 1px solid #bce8f1; border-radius: 4px; padding: 15px; margin-top: 10px;",
          icon("info-circle"),
          tags$strong(" No meeting report available."),
          tags$p("Please complete the meeting workflow in the 'Review output' tab and save your decisions first. ",
                 "Once meeting decisions are saved, the dashboard report will be rendered here.")
        ))
      }

      # --- Gather all required data for the Rmd template ---
      # 1. comparison_matrix (wide-format comparison matrix)
      cm <- comparison_matrix()
      if (is.null(cm)) cm <- data.frame()

      # 2. meeting_decisions_df (designation, decision, controversy_score)
      meeting_decisions_df <- tryCatch({
        resolve_decisions <- function() {
          # Get the meeting stamp from status
          meeting_status <- dt_obj$status[dt_obj$status$module == "Meeting_prodAdv", , drop = FALSE]
          if (nrow(meeting_status) == 0) return(data.frame())
          # Use the most recent meeting stamp
          meeting_stamp_id <- meeting_status$analysisId[nrow(meeting_status)]

          # Get decisions from modifications table
          mods <- dt_obj$modifications$selection
          meeting_mods <- mods[
            mods$analysisId == meeting_stamp_id &
              mods$module == "Meeting_prodAdv" &
              mods$reason == "meeting_decision",
            ,
            drop = FALSE
          ]

          if (nrow(meeting_mods) == 0) return(data.frame())

          decisions_df <- data.frame(
            designation = meeting_mods$designation,
            decision = meeting_mods$value,
            stringsAsFactors = FALSE
          )

          # Add controversy_score from comparison_matrix if available
          if (nrow(cm) > 0 && "controversy_score" %in% colnames(cm)) {
            decisions_df$controversy_score <- cm$controversy_score[
              match(decisions_df$designation, cm$designation)
            ]
          } else {
            decisions_df$controversy_score <- NA_real_
          }

          decisions_df
        }
        resolve_decisions()
      }, error = function(e) data.frame())

      # 3. mta_stamp (shared MTA analysis ID)
      mta_stamp <- tryCatch({
        resolve_mta <- function() {
          selected_stamps <- input$stamp_select
          if (is.null(selected_stamps) || length(selected_stamps) == 0) {
            # Try to extract from the meeting modeling rows
            meeting_status <- dt_obj$status[dt_obj$status$module == "Meeting_prodAdv", , drop = FALSE]
            if (nrow(meeting_status) == 0) return(NA_character_)
            meeting_stamp_id <- meeting_status$analysisId[nrow(meeting_status)]
            # Get inputObject rows from modeling for this meeting stamp
            input_rows <- dt_obj$modeling[
              dt_obj$modeling$module == "Meeting_prodAdv" &
                dt_obj$modeling$analysisId == meeting_stamp_id &
                dt_obj$modeling$parameter == "inputObject",
              ,
              drop = FALSE
            ]
            if (nrow(input_rows) == 0) return(NA_character_)
            # The MTA stamp is the inputObject that matches an MTA module
            for (val in input_rows$value) {
              is_mta <- any(dt_obj$status$analysisId == val & dt_obj$status$module %in% c("mta", "mtaLmms", "mtaAsr", "mtaFlex", "mas"))
              if (is_mta) return(val)
            }
            # Fallback: trace through one of the stakeholder stamps
            stakeholder_ids <- input_rows$value
            for (sid in stakeholder_ids) {
              mta_res <- validate_mta_compatibility(dt_obj, sid)
              if (mta_res$compatible) return(mta_res$mta_stamp)
            }
            NA_character_
          } else {
            mta_res <- validate_mta_compatibility(dt_obj, selected_stamps)
            if (mta_res$compatible) mta_res$mta_stamp else NA_character_
          }
        }
        resolve_mta()
      }, error = function(e) NA_character_)

      # 4. participants (character vector of participant names)
      participants <- tryCatch({
        resolve_participants <- function() {
          meeting_status <- dt_obj$status[dt_obj$status$module == "Meeting_prodAdv", , drop = FALSE]
          if (nrow(meeting_status) == 0) return(character(0))
          meeting_stamp_id <- meeting_status$analysisId[nrow(meeting_status)]
          part_rows <- dt_obj$modeling[
            dt_obj$modeling$module == "Meeting_prodAdv" &
              dt_obj$modeling$analysisId == meeting_stamp_id &
              dt_obj$modeling$parameter == "participants",
            ,
            drop = FALSE
          ]
          if (nrow(part_rows) == 0) return(character(0))
          unlist(strsplit(part_rows$value[1], ","))
        }
        resolve_participants()
      }, error = function(e) character(0))

      # 5. meeting_name (character string)
      meeting_name <- tryCatch({
        resolve_name <- function() {
          meeting_status <- dt_obj$status[dt_obj$status$module == "Meeting_prodAdv", , drop = FALSE]
          if (nrow(meeting_status) == 0) return("Unnamed meeting")
          meeting_status$analysisIdName[nrow(meeting_status)]
        }
        resolve_name()
      }, error = function(e) "Unnamed meeting")

      # 6. trait_directions (named list: trait -> direction string)
      td <- trait_directions()
      if (is.null(td)) td <- list()
      trait_directions_export <- as.list(td)

      # 7. predictions_data (data.frame with columns: designation, trait, predictedValue)
      predictions_data <- tryCatch({
        if (is.na(mta_stamp) || is.null(dt_obj$predictions)) return(data.frame())
        preds <- dt_obj$predictions
        mta_preds <- preds[
          preds$analysisId == mta_stamp & preds$effectType == "designation",
          c("designation", "trait", "predictedValue"),
          drop = FALSE
        ]
        mta_preds
      }, error = function(e) data.frame())

      # --- Save .RData and render the Rmd ---
      tryCatch({
        # Prepare variables with the correct names expected by the Rmd template
        comparison_matrix_out <- cm
        trait_directions_out <- trait_directions_export

        # Save all required variables to temp RData using the names the Rmd expects
        tmp_rdata <- file.path(tempdir(), "resultAdvMeeting.RData")

        # The Rmd loads: comparison_matrix, meeting_decisions_df, mta_stamp,
        #                participants, meeting_name, trait_directions, predictions_data
        comparison_matrix <- comparison_matrix_out
        trait_directions <- trait_directions_out
        save(
          comparison_matrix, meeting_decisions_df, mta_stamp, participants,
          meeting_name, trait_directions, predictions_data,
          file = tmp_rdata,
          envir = environment()
        )

        # Copy Rmd template to tempdir
        src <- system.file("rmd", "reportAdvMeeting.Rmd", package = "bioflow")
        if (!nzchar(src) || !file.exists(src)) {
          # Fallback for dev mode (golem::run_dev)
          src <- file.path(system.file(package = "bioflow"), "..", "inst", "rmd", "reportAdvMeeting.Rmd")
          if (!file.exists(src)) {
            src <- file.path(getwd(), "inst", "rmd", "reportAdvMeeting.Rmd")
          }
        }
        if (!file.exists(src)) {
          return(tags$div(
            style = "color: #a94442; background-color: #f2dede; border: 1px solid #ebccd1; border-radius: 4px; padding: 15px; margin-top: 10px;",
            icon("exclamation-triangle"),
            tags$strong(" Report template not found."),
            tags$p("The reportAdvMeeting.Rmd template could not be located in the package.")
          ))
        }

        tmp_report <- file.path(tempdir(), "reportAdvMeeting.Rmd")
        file.copy(src, tmp_report, overwrite = TRUE)

        # Render the Rmd report
        old <- setwd(tempdir())
        on.exit(setwd(old), add = TRUE)

        HTML(
          markdown::markdownToHTML(
            knitr::knit(basename(tmp_report), quiet = TRUE),
            fragment.only = TRUE
          )
        )
      }, error = function(e) {
        tags$div(
          style = "color: #a94442; background-color: #f2dede; border: 1px solid #ebccd1; border-radius: 4px; padding: 15px; margin-top: 10px;",
          icon("exclamation-triangle"),
          tags$strong(" Error rendering report."),
          tags$p(paste0("An error occurred while generating the meeting dashboard: ", conditionMessage(e)))
        )
      })
    })

    # Download handler for the dashboard report (Requirements 11.7)
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("advancement_meeting_dashboard_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
      },
      content = function(file) {
        # Gather data (same logic as renderUI above)
        dt_obj <- data()
        cm <- comparison_matrix()
        if (is.null(cm)) cm <- data.frame()

        # meeting_decisions_df
        meeting_decisions_df <- tryCatch({
          meeting_status <- dt_obj$status[dt_obj$status$module == "Meeting_prodAdv", , drop = FALSE]
          if (nrow(meeting_status) == 0) return(data.frame())
          meeting_stamp_id <- meeting_status$analysisId[nrow(meeting_status)]
          mods <- dt_obj$modifications$selection
          meeting_mods <- mods[
            mods$analysisId == meeting_stamp_id &
              mods$module == "Meeting_prodAdv" &
              mods$reason == "meeting_decision",
            ,
            drop = FALSE
          ]
          if (nrow(meeting_mods) == 0) return(data.frame())
          decisions_df <- data.frame(
            designation = meeting_mods$designation,
            decision = meeting_mods$value,
            stringsAsFactors = FALSE
          )
          if (nrow(cm) > 0 && "controversy_score" %in% colnames(cm)) {
            decisions_df$controversy_score <- cm$controversy_score[
              match(decisions_df$designation, cm$designation)
            ]
          } else {
            decisions_df$controversy_score <- NA_real_
          }
          decisions_df
        }, error = function(e) data.frame())

        # mta_stamp
        mta_stamp <- tryCatch({
          selected_stamps <- input$stamp_select
          if (is.null(selected_stamps) || length(selected_stamps) == 0) {
            meeting_status <- dt_obj$status[dt_obj$status$module == "Meeting_prodAdv", , drop = FALSE]
            if (nrow(meeting_status) == 0) return(NA_character_)
            meeting_stamp_id <- meeting_status$analysisId[nrow(meeting_status)]
            input_rows <- dt_obj$modeling[
              dt_obj$modeling$module == "Meeting_prodAdv" &
                dt_obj$modeling$analysisId == meeting_stamp_id &
                dt_obj$modeling$parameter == "inputObject",
              ,
              drop = FALSE
            ]
            if (nrow(input_rows) == 0) return(NA_character_)
            for (val in input_rows$value) {
              is_mta <- any(dt_obj$status$analysisId == val & dt_obj$status$module == "mta")
              if (is_mta) return(val)
            }
            stakeholder_ids <- input_rows$value
            for (sid in stakeholder_ids) {
              mta_res <- validate_mta_compatibility(dt_obj, sid)
              if (mta_res$compatible) return(mta_res$mta_stamp)
            }
            NA_character_
          } else {
            mta_res <- validate_mta_compatibility(dt_obj, selected_stamps)
            if (mta_res$compatible) mta_res$mta_stamp else NA_character_
          }
        }, error = function(e) NA_character_)

        # participants
        participants <- tryCatch({
          meeting_status <- dt_obj$status[dt_obj$status$module == "Meeting_prodAdv", , drop = FALSE]
          if (nrow(meeting_status) == 0) return(character(0))
          meeting_stamp_id <- meeting_status$analysisId[nrow(meeting_status)]
          part_rows <- dt_obj$modeling[
            dt_obj$modeling$module == "Meeting_prodAdv" &
              dt_obj$modeling$analysisId == meeting_stamp_id &
              dt_obj$modeling$parameter == "participants",
            ,
            drop = FALSE
          ]
          if (nrow(part_rows) == 0) return(character(0))
          unlist(strsplit(part_rows$value[1], ","))
        }, error = function(e) character(0))

        # meeting_name
        meeting_name <- tryCatch({
          meeting_status <- dt_obj$status[dt_obj$status$module == "Meeting_prodAdv", , drop = FALSE]
          if (nrow(meeting_status) == 0) return("Unnamed meeting")
          meeting_status$analysisIdName[nrow(meeting_status)]
        }, error = function(e) "Unnamed meeting")

        # trait_directions
        td <- trait_directions()
        if (is.null(td)) td <- list()
        trait_directions <- as.list(td)

        # predictions_data
        predictions_data <- tryCatch({
          if (is.na(mta_stamp) || is.null(dt_obj$predictions)) return(data.frame())
          preds <- dt_obj$predictions
          mta_preds <- preds[
            preds$analysisId == mta_stamp & preds$effectType == "designation",
            c("designation", "trait", "predictedValue"),
            drop = FALSE
          ]
          mta_preds
        }, error = function(e) data.frame())

        # Use cm for comparison_matrix variable in RData
        comparison_matrix <- cm

        # Save .RData
        tmp_rdata <- file.path(tempdir(), "resultAdvMeeting.RData")
        save(
          comparison_matrix, meeting_decisions_df, mta_stamp, participants,
          meeting_name, trait_directions, predictions_data,
          file = tmp_rdata,
          envir = environment()
        )

        # Copy Rmd template
        src <- system.file("rmd", "reportAdvMeeting.Rmd", package = "bioflow")
        if (!nzchar(src) || !file.exists(src)) {
          src <- file.path(system.file(package = "bioflow"), "..", "inst", "rmd", "reportAdvMeeting.Rmd")
          if (!file.exists(src)) {
            src <- file.path(getwd(), "inst", "rmd", "reportAdvMeeting.Rmd")
          }
        }
        if (!file.exists(src)) {
          # Write error HTML if template not found
          writeLines(
            "<html><body><h1>Error</h1><p>Report template not found.</p></body></html>",
            file
          )
          return()
        }

        tmp_report <- file.path(tempdir(), "reportAdvMeeting_download.Rmd")
        file.copy(src, tmp_report, overwrite = TRUE)

        # Render
        tryCatch({
          old <- setwd(tempdir())
          on.exit(setwd(old), add = TRUE)

          outReport <- rmarkdown::render(
            input = basename(tmp_report),
            params = list(toDownload = TRUE),
            output_format = rmarkdown::html_document(
              toc = TRUE,
              toc_depth = 3,
              self_contained = TRUE
            )
          )

          file.copy(outReport, file, overwrite = TRUE)
        }, error = function(e) {
          # On rendering failure, write an error HTML
          writeLines(
            paste0(
              "<html><body><h1>Report Rendering Error</h1>",
              "<p>An error occurred while rendering the meeting dashboard:</p>",
              "<pre>", htmltools::htmlEscape(conditionMessage(e)), "</pre>",
              "</body></html>"
            ),
            file
          )
        })
      }
    )

  })
}

## To be copied in the UI
# mod_advMeetingApp_ui("advMeetingApp_1")

## To be copied in the server
# mod_advMeetingApp_server("advMeetingApp_1")

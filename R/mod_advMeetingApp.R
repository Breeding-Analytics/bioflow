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
    desig <- comparison_matrix$designation[i]

    # Remove NAs for reliable comparison
    decisions <- decisions[!is.na(decisions)]
    if (length(decisions) == 0) next

    # CHECK: any stakeholder assigned CHECK -> auto-assign CHECK
    if (any(decisions == "CHECK")) {
      result_designation <- c(result_designation, desig)
      result_decision <- c(result_decision, "CHECK")
      next
    }

    # Unanimous SELECTED: all stakeholders said SELECTED
    if (all(decisions == "SELECTED")) {
      result_designation <- c(result_designation, desig)
      result_decision <- c(result_decision, "SELECTED")
      next
    }

    # Unanimous NOT SELECTED: all stakeholders said NOT SELECTED
    if (all(decisions == "NOT SELECTED")) {
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
    desig <- comparison_matrix$designation[i]

    # Remove NAs
    decisions <- decisions[!is.na(decisions)]
    if (length(decisions) == 0) next

    # Skip CHECK designations (any stakeholder assigned CHECK)
    if (any(decisions == "CHECK")) next

    # Consensus cases â€” do NOT need discussion
    if (all(decisions == "SELECTED")) next
    if (all(decisions == "NOT SELECTED")) next

    # Everything else needs discussion (controversial + unanimous REVISE)
    discussion_designations <- c(discussion_designations, desig)
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
#' @return Data.frame with columns: designation, decision â€” one row per
#'   designation, no duplicates.
#'
#' @noRd
assemble_final_decisions <- function(discussion_decisions, auto_assigned) {
  combined <- rbind(discussion_decisions, auto_assigned)

  # Ensure no duplicates â€” keep first occurrence (discussion takes precedence)
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

                                  tabPanel(div(icon("people-group"), "Conflict resolution"), value = "Conflict resolution",
                                           br(),
                                           column(width = 12,

                                                  # --- No-conflict message (shown dynamically) ---
                                                  uiOutput(ns("noConflictMessage")),

                                                  # --- SECTION: Agreement Heatmap ---
                                                  uiOutput(ns("conflictResolutionUI"))

                                           ),
                                  ),

                                  tabPanel(div(icon("flag-checkered"), "Review meeting consensus"), value = "Review meeting consensus",
                                           br(),
                                           uiOutput(ns("advMeetingReviewUI"))
                                  ),

                                  tabPanel(div(icon("arrow-right-from-bracket"), "Output tabs"), value = "Output",
                                           tabsetPanel(
                                             tabPanel("Dashboard", icon = icon("file-image"),
                                                      br(),
                                                      actionButton(ns("renderReportAdvMeeting"), "Download dashboard", icon = icon("download")),
                                                      downloadButton(ns("download_report"), "Download dashboard", style = "visibility:hidden;"),
                                                      br(),
                                                      # Rmd report: About this analysis, Summary,
                                                      # Final Meeting Decisions Table (per-stakeholder
                                                      # decisions) and the Agreement Heatmap
                                                      uiOutput(ns("rmd_report")),
                                                      tags$hr(),
                                                      h3(strong("Population Statistics")),
                                                      # Same per-trait tables and distribution plot
                                                      # as the Review meeting consensus tab
                                                      uiOutput(ns("dashStatsUI")),
                                                      plotly::plotlyOutput(ns("dashStatsHistogram"), height = "auto"),
                                                      tags$hr(),
                                                      h3(strong("TPP Breakdown")),
                                                      uiOutput(ns("tppBreakdownIntro")),
                                                      DT::DTOutput(ns("advDashboardBreakdown")),
                                                      uiOutput(ns("tppBreakdownEnvNotes")),
                                                      tags$hr(),
                                                      h3(strong("Global Options Summary")),
                                                      uiOutput(ns("advDashboardStakeholderOptions")),
                                                      # Buffer so the dashboard doesn't end abruptly
                                                      tags$div(style = "height:120px;")
                                             ),
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
    # available_stamps() reactive â€” filter status for Final_prodAdv stamps
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
    # meeting_context() â€” resolve the stamps/MTA/TPP driving the dashboard.
    #
    # Prefers the live session state (stamps picked in "Input steps"), and falls
    # back to the most recent saved Meeting_prodAdv stamp. The fallback is what
    # makes the Output dashboard work after loading an RData file, where
    # input$stamp_select and comparison_matrix() are both empty.
    # =========================================================================
    meeting_context <- reactive({
      dt_obj <- data()
      if (is.null(dt_obj) || is.null(dt_obj$modeling) || is.null(dt_obj$status)) {
        return(NULL)
      }

      modeling <- dt_obj$modeling
      status <- dt_obj$status

      # --- Resolve the stakeholder (Final_prodAdv) stamps ---
      stakeholder_stamps <- input$stamp_select
      source <- "live"

      if (is.null(stakeholder_stamps) || length(stakeholder_stamps) == 0) {
        # Fall back to the latest saved meeting stamp
        meeting_status <- status[status$module == "Meeting_prodAdv", , drop = FALSE]
        if (nrow(meeting_status) == 0) return(NULL)
        meeting_stamp <- meeting_status$analysisId[nrow(meeting_status)]

        input_rows <- modeling[
          modeling$module == "Meeting_prodAdv" &
            as.character(modeling$analysisId) == as.character(meeting_stamp) &
            modeling$parameter == "inputObject",
          , drop = FALSE
        ]
        if (nrow(input_rows) == 0) return(NULL)

        # The inputObject rows mix stakeholder stamps and the MTA stamp; keep
        # only those that resolve to Final_prodAdv in the status table.
        candidates <- input_rows$value
        is_final <- vapply(candidates, function(v) {
          any(as.character(status$analysisId) == as.character(v) &
                status$module == "Final_prodAdv")
        }, logical(1))
        stakeholder_stamps <- candidates[is_final]
        if (length(stakeholder_stamps) == 0) return(NULL)
        source <- "saved"
      }

      # --- Trace each stakeholder stamp to its Init_prodAdv stamp ---
      init_stamps <- vapply(stakeholder_stamps, function(stamp_id) {
        rows <- modeling[
          modeling$module == "Final_prodAdv" &
            as.character(modeling$analysisId) == as.character(stamp_id) &
            modeling$parameter == "inputObject",
          , drop = FALSE
        ]
        if (nrow(rows) == 0) NA_character_ else as.character(rows$value[1])
      }, character(1))
      init_stamps <- init_stamps[!is.na(init_stamps)]
      if (length(init_stamps) == 0) return(NULL)

      # --- MTA stamp (shared across stakeholders) ---
      mta_stamp <- NA_character_
      for (istamp in init_stamps) {
        rows <- modeling[
          modeling$module == "Init_prodAdv" &
            as.character(modeling$analysisId) == as.character(istamp) &
            modeling$parameter == "mta_stamp",
          , drop = FALSE
        ]
        if (nrow(rows) > 0 && !is.na(rows$value[1]) && nzchar(rows$value[1])) {
          mta_stamp <- as.character(rows$value[1])
          break
        }
      }

      # --- TPP id (recorded per Init_prodAdv run) ---
      tpp_id <- NULL
      for (istamp in init_stamps) {
        rows <- modeling[
          as.character(modeling$analysisId) == as.character(istamp) &
            modeling$parameter == "tpp_id",
          , drop = FALSE
        ]
        if (nrow(rows) > 0 && !is.na(rows$value[1]) && nzchar(rows$value[1])) {
          tpp_id <- as.character(rows$value[1])
          break
        }
      }

      # --- Participant labels ---
      participants <- status$analysisIdName[
        match(as.character(stakeholder_stamps), as.character(status$analysisId))
      ]
      participants <- participants[!is.na(participants)]

      list(
        stakeholder_stamps = as.character(stakeholder_stamps),
        init_stamps        = as.character(init_stamps),
        mta_stamp          = mta_stamp,
        tpp_id             = tpp_id,
        participants       = participants,
        source             = source
      )
    })

    # =========================================================================
    # dash_comparison_matrix() â€” comparison matrix for the dashboard.
    # Uses the live matrix when present, otherwise rebuilds it from the
    # stakeholder stamps recorded in the saved meeting stamp.
    # =========================================================================
    dash_comparison_matrix <- reactive({
      cm <- comparison_matrix()
      if (!is.null(cm) && nrow(cm) > 0) return(cm)

      ctx <- meeting_context()
      if (is.null(ctx) || length(ctx$stakeholder_stamps) == 0) return(NULL)

      out <- tryCatch(
        build_comparison_matrix(data(), ctx$stakeholder_stamps),
        error = function(e) NULL
      )
      if (is.null(out) || nrow(out) == 0) return(NULL)
      tryCatch(sort_by_controversy(out), error = function(e) out)
    })

    # =========================================================================
    # dash_trait_info() â€” trait directions/thresholds for the dashboard,
    # resolved from the Init_prodAdv stamp when the live values are unset.
    # =========================================================================
    dash_trait_info <- reactive({
      dirs <- trait_directions()
      if (!is.null(dirs) && length(dirs) > 0) return(dirs)

      ctx <- meeting_context()
      if (is.null(ctx) || length(ctx$init_stamps) == 0) return(NULL)

      info <- tryCatch(
        extract_trait_directions(data(), ctx$init_stamps[1]),
        error = function(e) NULL
      )
      if (is.null(info) || nrow(info) == 0) return(NULL)
      stats::setNames(info$direction, info$trait)
    })

    # =========================================================================
    # dash_tpp_traits() â€” TPP trait table enriched with category / desired
    # score / scale columns.
    #
    # metadata$TPP$traits only carries the tpp_trait -> pheno_trait mapping;
    # the requirement category and desired bounds live in the raw sheet at
    # data$TPP. tpp_build_criteria_list_from_filtered() returns an empty list
    # without those columns, which is why the TPP breakdown and compliance
    # tables came up empty.
    # =========================================================================
    dash_tpp_traits <- reactive({
      ctx <- meeting_context()
      if (is.null(ctx) || is.null(ctx$tpp_id)) return(NULL)
      tryCatch(
        tpp_enrich_traits_from_raw(data(), ctx$tpp_id),
        error = function(e) NULL
      )
    })

    # Checks mapped per TPP trait, used when resolving check-relative criteria
    dash_tpp_checks <- reactive({
      ctx <- meeting_context()
      if (is.null(ctx) || is.null(ctx$tpp_id)) return(NULL)
      dt_obj <- data()
      if (is.null(dt_obj)) return(NULL)
      meta <- dt_obj$metadata$TPP[[ctx$tpp_id]]
      if (is.null(meta)) return(NULL)
      meta$checks_per_trait
    })

    # =========================================================================
    # dash_final_decisions() â€” one decision per designation for the dashboard.
    # Live meeting decisions take precedence; otherwise the decisions saved on
    # the Meeting_prodAdv stamp are used.
    # =========================================================================
    dash_final_decisions <- reactive({
      cm <- dash_comparison_matrix()
      if (is.null(cm) || nrow(cm) == 0) return(NULL)

      md <- meeting_decisions()
      auto <- tryCatch(auto_assign_non_controversial(cm),
                       error = function(e) NULL)
      if (is.null(auto)) {
        auto <- data.frame(designation = character(0), decision = character(0),
                           stringsAsFactors = FALSE)
      }

      if (!is.null(md) && length(md) > 0) {
        disc_df <- data.frame(
          designation = names(md),
          decision    = as.character(unlist(md)),
          stringsAsFactors = FALSE
        )
        out <- rbind(disc_df, auto)
        return(out[!duplicated(out$designation), , drop = FALSE])
      }

      # No live decisions â€” read them off the saved meeting stamp
      dt_obj <- data()
      saved <- NULL
      if (!is.null(dt_obj$status) && !is.null(dt_obj$modifications$selection)) {
        meeting_status <- dt_obj$status[dt_obj$status$module == "Meeting_prodAdv", , drop = FALSE]
        if (nrow(meeting_status) > 0) {
          meeting_stamp <- meeting_status$analysisId[nrow(meeting_status)]
          mods <- dt_obj$modifications$selection
          rows <- mods[
            as.character(mods$analysisId) == as.character(meeting_stamp) &
              mods$module == "Meeting_prodAdv" &
              mods$reason == "meeting_decision",
            , drop = FALSE
          ]
          if (nrow(rows) > 0) {
            saved <- data.frame(
              designation = rows$designation,
              decision    = rows$value,
              stringsAsFactors = FALSE
            )
            saved <- saved[!duplicated(saved$designation), , drop = FALSE]
          }
        }
      }

      if (!is.null(saved) && nrow(saved) > 0) {
        out <- rbind(saved, auto)
        return(out[!duplicated(out$designation), , drop = FALSE])
      }

      auto
    })

    # =========================================================================
    # output$warningMessage â€” status indicator for detected stamps
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
    # observeEvent(input$merge_btn) â€” Validate, build comparison matrix, store results
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

      # Switch to Conflict resolution tab (Requirement 2.6)
      updateTabsetPanel(session, "tabsMain", selected = "Conflict resolution")

      shiny::showNotification(
        paste0("Comparison matrix built successfully with ",
               nrow(cm), " designations and ",
               length(selected_stamps), " stakeholders."),
        type = "message",
        duration = 5
      )
    })

    # =========================================================================
    # output$auto_assigned_summary â€” count of auto-assigned decisions
    # Requirement 13.4
    # =========================================================================
    output$auto_assigned_summary <- renderText({
      cm <- comparison_matrix()
      if (is.null(cm)) return("0 candidates auto-assigned by full consensus.")
      auto <- auto_assign_non_controversial(cm)
      paste0(nrow(auto), " candidates auto-assigned by full consensus.")
    })

    # =========================================================================
    # output$noConflictMessage â€” shown when there is no controversy
    # =========================================================================
    output$noConflictMessage <- renderUI({
      cm <- comparison_matrix()
      if (is.null(cm) || nrow(cm) == 0) return(NULL)

      candidates <- requires_discussion(cm)
      if (length(candidates) == 0) {
        return(tags$div(
          style = "padding: 40px; text-align: center;",
          tags$div(
            style = "background-color: #d4edda; border: 1px solid #c3e6cb; border-radius: 8px; padding: 30px; max-width: 700px; margin: auto;",
            icon("check-circle", style = "font-size: 48px; color: #155724; margin-bottom: 15px;"),
            tags$h3(style = "color: #155724;", "No Conflicts to Resolve"),
            tags$p(style = "color: #155724; font-size: 14px;",
                   "All stakeholders are in full agreement on every candidate. ",
                   "There are no controversial designations requiring discussion."),
            tags$p(style = "color: #155724; font-size: 13px; margin-top: 10px;",
                   "You can proceed directly to the 'Review meeting consensus' tab to review the consensus selection and save meeting decisions.")
          )
        ))
      }
      NULL
    })

    # =========================================================================
    # output$conflictResolutionUI â€” Full conflict resolution content (hidden when no conflicts)
    # =========================================================================
    output$conflictResolutionUI <- renderUI({
      cm <- comparison_matrix()
      if (is.null(cm) || nrow(cm) == 0) {
        return(tags$div(
          style = "padding: 20px; color: #666;",
          tags$p("No comparison data available. Please go to 'Input steps' and merge stakeholder stamps first.")
        ))
      }

      # Check if there are candidates needing discussion
      candidates <- requires_discussion(cm)
      if (length(candidates) == 0) {
        # No conflicts â€” the noConflictMessage above handles the display
        return(NULL)
      }

      ns <- session$ns
      # isolate(): detail_choice_info() also tracks meeting_decisions(), and
      # without this the whole tab would re-render every time a decision is
      # recorded, resetting the card and the radio. The UI still rebuilds when
      # comparison_matrix() changes, which is read reactively above.
      detail_info <- isolate(detail_choice_info())
      tagList(
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
        tags$hr(),
        # --- SECTION: Radar plot + detail table ---
        h3(strong("Candidate Detail Comparison")),
        # Choices and the default selection are supplied at creation time so the
        # radar plot opens on a controversial candidate plus a check reference.
        selectizeInput(
          ns("detail_candidates"),
          label = "Select designations to compare (up to 5, including CHECK references):",
          choices = detail_info$choices,
          selected = detail_info$default,
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
        # Population context table for current candidate
        shinydashboard::box(
          width = 12,
          title = "Population Context",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          tags$p(
            style = "font-size: 12px; color: #555; margin-bottom: 10px;",
            "Only traits used by the majority of stakeholders are included (ties excluded). ",
            "Weights are the median across stakeholders (a weight of 0 is used if a stakeholder did not include a given trait). ",
            "Individual BLUPs are penalized by the square root of their reliability before index computation."
          ),
          DT::DTOutput(ns("candidate_context_table"))
        )
      )
    })

    # =========================================================================
    # Reactive: heatmap_data â€” filters comparison matrix based on toggle
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
    # output$heatmap â€” Agreement Heatmap via plotly
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
    # output$controversy_chart â€” Bar chart with 3 categories
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
        # Remove NAs for comparison
        decisions <- decisions[!is.na(decisions)]

        # Skip check varieties
        if (any(decisions == "CHECK", na.rm = TRUE)) next

        # Skip if no valid decisions
        if (length(decisions) == 0) next

        # Consensus REVISE: ALL stakeholders said REVISE
        if (all(decisions == "REVISE")) {
          n_revise <- n_revise + 1
        # Consensus SELECTED: ALL stakeholders said SELECTED
        } else if (all(decisions == "SELECTED")) {
          n_selected <- n_selected + 1
        # Consensus NOT SELECTED: ALL stakeholders said NOT SELECTED
        } else if (all(decisions == "NOT SELECTED")) {
          n_not_selected <- n_not_selected + 1
        # Everything else is controversial
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
    # output$consensus_summary_text â€” Summary text
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
      n_checks <- 0

      for (i in seq_len(nrow(cm))) {
        decisions <- as.character(unlist(cm[i, stakeholder_cols]))
        decisions <- decisions[!is.na(decisions)]

        # Skip check varieties
        if (any(decisions == "CHECK", na.rm = TRUE)) {
          n_checks <- n_checks + 1
          next
        }

        if (length(decisions) == 0) next

        # Unanimous REVISE requires discussion
        if (all(decisions == "REVISE")) {
          n_discussion <- n_discussion + 1
        # Unanimous SELECTED or NOT SELECTED = full agreement
        } else if (all(decisions == "SELECTED") || all(decisions == "NOT SELECTED")) {
          n_agreement <- n_agreement + 1
        } else {
          n_discussion <- n_discussion + 1
        }
      }

      total_candidates <- nrow(cm) - n_checks

      tags$p(
        style = "font-size: 16px; font-weight: bold; margin-top: 10px;",
        paste0(total_candidates, " candidates: ",
               n_agreement, " with full agreement, ",
               n_discussion, " requiring discussion.")
      )
    })

    # =========================================================================
    # detail_choice_info() — labelled choices for the radar-plot picker plus the
    # default selection (first controversial candidate + first check).
    #
    # Shared by conflictResolutionUI, which creates the selectizeInput with
    # these values already in place, and by the observer below that refreshes
    # the labels as decisions are recorded. Setting them at creation time is
    # what makes the default selection stick: the widget lives inside a
    # renderUI, so an updateSelectizeInput issued before the browser has
    # instantiated it is silently dropped.
    # =========================================================================
    detail_choice_info <- reactive({
      cm <- comparison_matrix()
      md <- meeting_decisions()
      if (is.null(cm) || nrow(cm) == 0) {
        return(list(choices = character(0), default = character(0)))
      }

      stakeholder_cols <- setdiff(colnames(cm), c("designation", "controversy_score"))

      status_labels <- vapply(seq_len(nrow(cm)), function(i) {
        desig <- cm$designation[i]
        if (!is.null(md) && desig %in% names(md)) {
          return(paste0("RESOLVED - ", md[[desig]]))
        }
        decisions <- as.character(unlist(cm[i, stakeholder_cols]))
        decisions <- decisions[!is.na(decisions)]
        if (any(decisions == "CHECK")) {
          "CHECK"
        } else if (all(decisions == "SELECTED")) {
          "CONSENSUS - SELECTED"
        } else if (all(decisions == "NOT SELECTED")) {
          "CONSENSUS - NOT SELECTED"
        } else if (all(decisions == "REVISE")) {
          "CONSENSUS - REVISE"
        } else {
          "CONTROVERSIAL"
        }
      }, character(1))

      sort_priority <- c("CONTROVERSIAL" = 1, "CONSENSUS - REVISE" = 2)
      priority_vals <- vapply(status_labels, function(lbl) {
        if (lbl %in% names(sort_priority)) return(sort_priority[[lbl]])
        if (grepl("^RESOLVED", lbl)) return(3)
        if (lbl == "CHECK") return(4)
        if (lbl == "CONSENSUS - SELECTED") return(5)
        if (lbl == "CONSENSUS - NOT SELECTED") return(6)
        return(3)
      }, numeric(1))
      sort_order <- order(priority_vals, cm$designation)

      desigs <- cm$designation[sort_order]
      sorted_labels <- status_labels[sort_order]
      labels <- paste0(desigs, " [", sorted_labels, "]")

      # Default: one candidate that still needs discussion + one check, so the
      # radar opens on a meaningful comparison instead of empty.
      needs_disc <- requires_discussion(cm)
      cand <- desigs[desigs %in% needs_disc]
      if (length(cand) == 0) cand <- desigs[sorted_labels == "CONTROVERSIAL"]
      chk <- desigs[sorted_labels == "CHECK"]

      default <- character(0)
      if (length(cand) > 0) default <- c(default, cand[1])
      if (length(chk) > 0) default <- c(default, chk[1])

      list(choices = stats::setNames(desigs, labels), default = default)
    })

    # =========================================================================
    # Update detail_candidates selectizeInput choices from comparison_matrix
    # Requirements 6.1, 6.8
    # =========================================================================
    observe({
      cm <- comparison_matrix()
      md <- meeting_decisions()
      if (is.null(cm) || nrow(cm) == 0) {
        updateSelectizeInput(session, "detail_candidates",
                            choices = character(0),
                            selected = character(0))
        return()
      }

      # Build labelled choices with consensus status (incorporating meeting decisions)
      stakeholder_cols <- setdiff(colnames(cm), c("designation", "controversy_score"))

      status_labels <- vapply(seq_len(nrow(cm)), function(i) {
        desig <- cm$designation[i]
        # If resolved in meeting, show the meeting decision
        if (!is.null(md) && desig %in% names(md)) {
          return(paste0("RESOLVED - ", md[[desig]]))
        }
        decisions <- as.character(unlist(cm[i, stakeholder_cols]))
        decisions <- decisions[!is.na(decisions)]

        if (any(decisions == "CHECK")) {
          "CHECK"
        } else if (all(decisions == "SELECTED")) {
          "CONSENSUS - SELECTED"
        } else if (all(decisions == "NOT SELECTED")) {
          "CONSENSUS - NOT SELECTED"
        } else if (all(decisions == "REVISE")) {
          "CONSENSUS - REVISE"
        } else {
          "CONTROVERSIAL"
        }
      }, character(1))

      # Sort order: Controversial first, then resolved, then consensus
      sort_priority <- c(
        "CONTROVERSIAL" = 1,
        "CONSENSUS - REVISE" = 2
      )
      # Assign priority: resolved and consensus go after controversial
      priority_vals <- vapply(status_labels, function(lbl) {
        if (lbl %in% names(sort_priority)) return(sort_priority[[lbl]])
        if (grepl("^RESOLVED", lbl)) return(3)
        if (lbl == "CHECK") return(4)
        if (lbl == "CONSENSUS - SELECTED") return(5)
        if (lbl == "CONSENSUS - NOT SELECTED") return(6)
        return(3)
      }, numeric(1))
      sort_order <- order(priority_vals, cm$designation)

      desigs <- cm$designation[sort_order]
      labels <- paste0(cm$designation[sort_order], " [", status_labels[sort_order], "]")

      choices <- setNames(desigs, labels)

      # Refresh the labels (they carry RESOLVED status) while keeping whatever
      # the user has picked. The default selection is applied when the widget is
      # created in conflictResolutionUI, so nothing is forced here.
      current_selected <- isolate(input$detail_candidates)
      current_selected <- current_selected[current_selected %in% desigs]

      updateSelectizeInput(session, "detail_candidates",
                          choices = choices,
                          selected = current_selected)
    })

    # =========================================================================
    # output$radar_plot â€” Radar/spider plot for up to 5 candidates
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
          # Leave headroom at the top so the 12 o'clock trait label doesn't
          # collide with the title
          domain = list(y = c(0, 0.88)),
          radialaxis = list(visible = TRUE, range = c(0, 1)),
          angularaxis = list(direction = "clockwise")
        ),
        showlegend = TRUE,
        legend = list(x = 1.05, y = 1),
        title = list(
          text = "Trait Performance Comparison (Normalized)",
          y = 0.97,
          yanchor = "top",
          x = 0.5,
          xanchor = "center"
        ),
        margin = list(t = 70, b = 40)
      )

      p
    })

    # =========================================================================
    # output$detail_table â€” Decision table with direction-aware gradient coloring
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
    # output$stakeholder_breakdown â€” Per-candidate stakeholder decision breakdown
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
    # discussion_candidates() reactive â€” designations needing discussion
    # Requirements 7.1, 13.6, 13.7
    # =========================================================================
    discussion_candidates <- reactive({
      cm <- comparison_matrix()
      req(cm)
      requires_discussion(cm)
    })

    # =========================================================================
    # discussion_status reactiveVal â€” named character vector (designation -> status)
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
    # output$candidate_card â€” renderUI for Joint Decision Workflow card
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

      # Get current meeting decision for card coloring
      md <- meeting_decisions()
      current_decision <- if (!is.null(md) && current_designation %in% names(md)) {
        md[[current_designation]]
      } else {
        NULL
      }

      # Card background color based on meeting decision
      card_bg <- if (!is.null(current_decision) && current_decision == "SELECTED") {
        "#D6EAF8"   # light blue
      } else if (!is.null(current_decision) && current_decision == "NOT SELECTED") {
        "#FDEBD0"   # light orange
      } else {
        "#FFFFFF"   # white (no decision yet)
      }

      # Decision tag (shown before Resolved tag)
      decision_tag <- if (!is.null(current_decision)) {
        dec_color <- if (current_decision == "SELECTED") "#0072B2" else "#D55E00"
        tags$span(
          style = paste0("display: inline-block; padding: 2px 8px; border-radius: 4px; ",
                         "color: white; background-color: ", dec_color, "; font-size: 12px; margin-right: 6px;"),
          current_decision
        )
      } else {
        NULL
      }

      # Build card UI
      tags$div(
        style = paste0("background-color: ", card_bg, "; padding: 15px; border-radius: 8px; border: 1px solid #ddd;"),
        tags$h4(
          style = "margin-bottom: 5px;",
          paste0("Candidate ", idx, " of ", length(candidates), ": "),
          tags$strong(current_designation)
        ),
        tags$div(
          style = "margin-bottom: 8px;",
          decision_tag,
          tags$span(
            style = paste0("display: inline-block; padding: 2px 8px; border-radius: 4px; ",
                           "color: white; background-color: ", status_color, "; font-size: 12px;"),
            status_label
          )
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
    # output$progress_indicator â€” "X of Y controversial candidates resolved."
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
    # output$selection_tally â€” Running count of selected vs total (excl. checks)
    # =========================================================================
    output$selection_tally <- renderUI({
      cm <- comparison_matrix()
      if (is.null(cm) || nrow(cm) == 0) return(NULL)

      # Primary reactive dependencies: these reactiveVals drive the tally
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
    # output$candidate_context_table â€” Color-coded trait table for population context
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

      # Compute index: averaged weights across ALL stakeholders
      all_weight_lists <- list()
      all_trait_sets <- list()
      for (stamp_id in selected_stamps) {
        s_init_rows <- modeling[
          modeling$module == "Final_prodAdv" &
            modeling$analysisId == stamp_id &
            modeling$parameter == "inputObject",
          , drop = FALSE
        ]
        if (nrow(s_init_rows) == 0) next
        s_init_stamp <- s_init_rows$value[1]

        s_weight_rows <- modeling[
          modeling$analysisId == s_init_stamp & modeling$parameter == "index_weight",
          , drop = FALSE
        ]
        if (nrow(s_weight_rows) > 0) {
          w <- as.numeric(s_weight_rows$value)
          names(w) <- s_weight_rows$trait
          all_weight_lists[[stamp_id]] <- w
        }

        s_all_tr <- unique(modeling$trait[
          modeling$analysisId == s_init_stamp &
            modeling$module == "Init_prodAdv" &
            !is.na(modeling$trait) & nzchar(modeling$trait)
        ])
        s_excluded_tr <- modeling$trait[
          modeling$analysisId == s_init_stamp &
            modeling$parameter == "user_excluded_trait"
        ]
        all_trait_sets[[stamp_id]] <- setdiff(s_all_tr, s_excluded_tr)
      }

      # Majority rule: only include traits used by more than half the stakeholders (ties excluded)
      all_traits_union <- unique(unlist(all_trait_sets))
      n_stakeholders <- length(all_trait_sets)
      union_traits <- all_traits_union[vapply(all_traits_union, function(tr) {
        n_using <- sum(vapply(all_trait_sets, function(ts) tr %in% ts, logical(1)))
        n_using > n_stakeholders / 2
      }, logical(1))]

      if (length(union_traits) > 0 && length(all_weight_lists) > 0) {
        idx_weights <- vapply(union_traits, function(tr) {
          trait_weights <- vapply(all_weight_lists, function(wl) {
            if (tr %in% names(wl)) wl[[tr]] else 0
          }, numeric(1))
          median(trait_weights)
        }, numeric(1))
        names(idx_weights) <- union_traits
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

      # Classify each designation's consensus status â€” incorporate meeting decisions
      stakeholder_cols <- setdiff(colnames(cm), c("designation", "controversy_score"))
      md <- meeting_decisions()
      desig_status <- vapply(pred_wide$designation, function(d) {
        # If already resolved in meeting, use the meeting decision
        if (!is.null(md) && d %in% names(md)) {
          return(md[[d]])
        }
        row <- cm[cm$designation == d, , drop = FALSE]
        if (nrow(row) == 0) return("CONTROVERSIAL")
        decisions <- as.character(unlist(row[1, stakeholder_cols]))
        decisions <- decisions[!is.na(decisions)]
        if (length(decisions) == 0) return("CONTROVERSIAL")
        if (any(decisions == "CHECK")) return("CHECK")
        if (all(decisions == "SELECTED")) return("SELECTED")
        if (all(decisions == "NOT SELECTED")) return("NOT SELECTED")
        if (all(decisions == "REVISE")) return("REVISE")
        "CONTROVERSIAL"
      }, character(1))

      pred_wide$status <- desig_status

      # Sort by index descending
      pred_wide <- pred_wide[order(-pred_wide$index_value), , drop = FALSE]

      # Determine which designations still need discussion (unresolved only)
      ds <- discussion_status()
      needs_discussion <- if (!is.null(ds)) {
        names(ds)[ds != "Resolved"]
      } else {
        candidates
      }

      # --- Population Context Truncation (Requirements 11.1, 11.2, 11.3, 11.4) ---
      # Build rankings data.frame for tpp_compute_context_limit
      rankings_df <- data.frame(
        designation = pred_wide$designation,
        rank = seq_len(nrow(pred_wide)),
        is_controversial = pred_wide$designation %in% needs_discussion,
        stringsAsFactors = FALSE
      )

      # Compute truncation limit (N+5 beyond lowest controversial candidate)
      display_limit <- tpp_compute_context_limit(rankings_df, current_designation)

      # Truncate to display_limit rows
      if (display_limit > 0 && display_limit < nrow(pred_wide)) {
        display_data <- pred_wide[seq_len(display_limit), , drop = FALSE]

        # Ensure currently discussed candidate is included even if beyond truncation
        if (!is.null(current_designation) &&
            !current_designation %in% display_data$designation) {
          discussed_row <- pred_wide[pred_wide$designation == current_designation, , drop = FALSE]
          if (nrow(discussed_row) > 0) {
            display_data <- rbind(display_data, discussed_row)
          }
        }

        pred_wide <- display_data
      }
      # If display_limit >= nrow(pred_wide) or == 0 (empty rankings), show all rows
      # --- End Population Context Truncation ---

      # Build color-coded HTML table
      # Colors: SELECTED=#85C1E9, NOT SELECTED=#E8A87C, CHECK=#C39BD3, yellow for unresolved
      col_selected <- "#85C1E9"
      col_not_selected <- "#E8A87C"
      col_check <- "#C39BD3"
      col_controversial <- "#FFF8DC"  # Muted pale yellow for unresolved discussion candidates
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
    # observeEvent(input$prev_btn) â€” Navigate to previous candidate
    # Requirement 7.1
    # =========================================================================
    observeEvent(input$prev_btn, {
      idx <- current_candidate_idx()
      if (idx > 1) {
        current_candidate_idx(idx - 1)
      }
    })

    # =========================================================================
    # observeEvent(input$next_btn) â€” Navigate to next candidate
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
    # observeEvent(input$majority_btn) â€” Compute and apply majority vote
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
    # Keep the "Meeting Decision" radio in sync with the candidate on screen.
    #
    # The radio is only a pending choice; it must NOT write into
    # meeting_decisions(). renderUI recreates the radio, so input$vote_decision
    # goes NULL -> "SELECTED" (its first choice) as a genuine change event.
    # A previous version recorded that as a decision, which made the card show
    # SELECTED and the card background turn blue while the progress counter
    # still read "0 of N resolved". Decisions are now committed only by
    # "Simple Majority" or "Mark as Resolved".
    # =========================================================================
    observeEvent(list(current_candidate_idx(), comparison_matrix()), {
      cm <- comparison_matrix()
      req(cm)
      candidates <- discussion_candidates()
      req(candidates)
      idx <- current_candidate_idx()
      req(idx >= 1 && idx <= length(candidates))

      current_designation <- candidates[idx]

      # Already-recorded decision wins; otherwise suggest the majority vote so
      # the radio starts somewhere sensible without implying a decision.
      md <- meeting_decisions()
      shown <- if (!is.null(md) && current_designation %in% names(md)) {
        md[[current_designation]]
      } else {
        row_idx <- which(cm$designation == current_designation)
        if (length(row_idx) == 0) {
          "NOT SELECTED"
        } else {
          stakeholder_cols <- setdiff(colnames(cm), c("designation", "controversy_score"))
          dv <- as.character(unlist(cm[row_idx, stakeholder_cols]))
          dv <- dv[!is.na(dv)]
          if (length(dv) == 0) "NOT SELECTED" else compute_majority_vote(dv)
        }
      }

      if (!identical(isolate(input$vote_decision), shown)) {
        updateRadioButtons(session, "vote_decision", selected = shown)
      }
    }, ignoreNULL = FALSE)

    # =========================================================================
    # observeEvent(input$toggle_status_btn) â€” Toggle discussion status
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

      # Commit or clear the decision BEFORE updating discussion_status so the
      # tally sees the change when it re-renders. Resolving always records the
      # radio value currently on screen (so re-resolving after changing the
      # radio updates the decision); un-resolving clears it again, keeping the
      # card badge and the tally in step with the progress counter.
      md <- meeting_decisions()
      if (is.null(md)) md <- list()

      if (new_status == "Resolved") {
        current_vote <- input$vote_decision
        if (!is.null(current_vote) && nzchar(current_vote)) {
          md[[current_designation]] <- current_vote
          meeting_decisions(md)
        }
      } else {
        # Back to "Discuss": drop the recorded decision
        if (current_designation %in% names(md)) {
          md[[current_designation]] <- NULL
          meeting_decisions(md)
        }
      }

      # Now update discussion_status (this triggers tally re-render)
      ds[[current_designation]] <- new_status
      discussion_status(ds)

      # Check if all are resolved
      if (all(ds == "Resolved")) {
        shiny::showNotification(
          "All controversial candidates have been resolved! You can proceed to 'Review meeting consensus' to review and save.",
          type = "message",
          duration = 8
        )
      }
    })

    # =========================================================================
    # observeEvent(input$save_btn) â€” Save meeting decisions
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
          tags$p("Please complete the meeting workflow in the 'Review meeting consensus' tab and save your decisions first. ",
                 "Once meeting decisions are saved, the dashboard report will be rendered here.")
        ))
      }

      # --- Gather all required data for the Rmd template ---
      # 1. comparison_matrix (wide-format comparison matrix)
      cm <- dash_comparison_matrix()
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
      td <- dash_trait_info()
      if (is.null(td)) td <- list()
      trait_directions_export <- as.list(td)

      # 7. predictions_data (data.frame with columns: designation, trait, predictedValue, reliability)
      predictions_data <- tryCatch({
        if (is.na(mta_stamp) || is.null(dt_obj$predictions)) return(data.frame())
        preds <- dt_obj$predictions
        cols_to_keep <- intersect(c("designation", "trait", "predictedValue", "reliability"), colnames(preds))
        mta_preds <- preds[
          preds$analysisId == mta_stamp & preds$effectType == "designation",
          cols_to_keep,
          drop = FALSE
        ]
        mta_preds
      }, error = function(e) data.frame())

      # 8. index_weights and selected_traits: averaged across ALL stakeholders
      # Union of traits across stakeholders, weights averaged (0 if a stakeholder didn't use a trait)
      index_weights <- NULL
      selected_traits <- NULL
      tryCatch({
        ctx_rep <- meeting_context()
        selected_stamps_val <- if (!is.null(ctx_rep)) ctx_rep$stakeholder_stamps else input$stamp_select
        if (!is.null(selected_stamps_val) && length(selected_stamps_val) > 0) {
          modeling <- dt_obj$modeling

          # Collect weights and traits from each stakeholder's Init_prodAdv
          all_weight_lists <- list()
          all_trait_sets <- list()

          for (stamp_id in selected_stamps_val) {
            init_rows <- modeling[
              modeling$module == "Final_prodAdv" &
                modeling$analysisId == stamp_id &
                modeling$parameter == "inputObject",
              , drop = FALSE
            ]
            if (nrow(init_rows) == 0) next
            init_stamp_val <- init_rows$value[1]

            weight_rows <- modeling[
              modeling$analysisId == init_stamp_val & modeling$parameter == "index_weight",
              , drop = FALSE
            ]
            if (nrow(weight_rows) > 0) {
              w <- as.numeric(weight_rows$value)
              names(w) <- weight_rows$trait
              all_weight_lists[[stamp_id]] <- w
            }

            # Get selected traits for this stakeholder
            all_tr <- unique(modeling$trait[
              modeling$analysisId == init_stamp_val &
                modeling$module == "Init_prodAdv" &
                !is.na(modeling$trait) & nzchar(modeling$trait)
            ])
            excluded_tr <- modeling$trait[
              modeling$analysisId == init_stamp_val &
                modeling$parameter == "user_excluded_trait"
            ]
            all_trait_sets[[stamp_id]] <- setdiff(all_tr, excluded_tr)
          }

          # Majority rule: only include traits used by more than half the stakeholders (ties excluded)
          all_traits_union <- unique(unlist(all_trait_sets))
          n_stk <- length(all_trait_sets)
          selected_traits <- all_traits_union[vapply(all_traits_union, function(tr) {
            n_using <- sum(vapply(all_trait_sets, function(ts) tr %in% ts, logical(1)))
            n_using > n_stk / 2
          }, logical(1))]

          # Average weights: for each trait, average across stakeholders (0 if not used)
          if (length(selected_traits) > 0 && length(all_weight_lists) > 0) {
            n_stakeholders <- length(all_weight_lists)
            index_weights <- vapply(selected_traits, function(tr) {
              trait_weights <- vapply(all_weight_lists, function(wl) {
                if (tr %in% names(wl)) wl[[tr]] else 0
              }, numeric(1))
              median(trait_weights)
            }, numeric(1))
            names(index_weights) <- selected_traits
          }
        }
      }, error = function(e) NULL)

      # --- Save .RData and render the Rmd ---
      tryCatch({
        # Prepare variables with the correct names expected by the Rmd template
        comparison_matrix_out <- cm
        trait_directions_out <- trait_directions_export

        # Save all required variables to temp RData using the names the Rmd expects
        tmp_rdata <- file.path(tempdir(), "resultAdvMeeting.RData")

        # The Rmd loads: comparison_matrix, meeting_decisions_df, mta_stamp,
        #                participants, meeting_name, trait_directions, predictions_data,
        #                index_weights, selected_traits
        comparison_matrix <- comparison_matrix_out
        trait_directions <- trait_directions_out
        review_long        <- tryCatch(adv_review_long(), error = function(e) NULL)
        breakdown_detail   <- tryCatch(tpp_breakdown_detail(), error = function(e) NULL)
        stakeholder_tables <- tryCatch(stakeholder_options_tables(), error = function(e) NULL)

        save(
          comparison_matrix, meeting_decisions_df, mta_stamp, participants,
          meeting_name, trait_directions, predictions_data,
          index_weights, selected_traits,
          review_long, breakdown_detail, stakeholder_tables, REVIEW_GROUP_COLORS,
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

    # =========================================================================
    # DASHBOARD TAB: Population Statistics, TPP Breakdown, Stakeholder Options
    # Requirements 13.1, 13.2, 13.3, 13.4, 13.5, 14.5, 14.6, 14.7, 14.8, 14.9
    # =========================================================================

    # --- Population Statistics Panel ---
    output$advDashboardStats <- DT::renderDT({
      cm <- comparison_matrix()
      dt_obj <- data()

      if (is.null(cm) || nrow(cm) == 0 || is.null(dt_obj)) {
        msg_df <- data.frame(Message = "No trait distribution data available",
                             stringsAsFactors = FALSE)
        return(DT::datatable(msg_df, options = list(dom = "t", paging = FALSE),
                             rownames = FALSE))
      }

      # Resolve MTA stamp for predictions
      selected_stamps <- input$stamp_select
      if (is.null(selected_stamps) || length(selected_stamps) == 0) {
        msg_df <- data.frame(Message = "No trait distribution data available",
                             stringsAsFactors = FALSE)
        return(DT::datatable(msg_df, options = list(dom = "t", paging = FALSE),
                             rownames = FALSE))
      }

      modeling <- dt_obj$modeling
      init_rows <- modeling[
        modeling$module == "Final_prodAdv" &
          modeling$analysisId == selected_stamps[1] &
          modeling$parameter == "inputObject",
        , drop = FALSE
      ]
      if (nrow(init_rows) == 0) {
        msg_df <- data.frame(Message = "No trait distribution data available",
                             stringsAsFactors = FALSE)
        return(DT::datatable(msg_df, options = list(dom = "t", paging = FALSE),
                             rownames = FALSE))
      }
      init_stamp <- init_rows$value[1]

      mta_rows <- modeling[
        modeling$module == "Init_prodAdv" &
          modeling$analysisId == init_stamp &
          modeling$parameter == "mta_stamp",
        , drop = FALSE
      ]
      if (nrow(mta_rows) == 0) {
        msg_df <- data.frame(Message = "No trait distribution data available",
                             stringsAsFactors = FALSE)
        return(DT::datatable(msg_df, options = list(dom = "t", paging = FALSE),
                             rownames = FALSE))
      }
      mta_stamp <- mta_rows$value[1]

      # Get predictions and compute index values
      preds <- dt_obj$predictions
      mta_preds <- preds[
        preds$analysisId == mta_stamp &
          preds$effectType == "designation" &
          preds$designation %in% cm$designation,
        , drop = FALSE
      ]

      if (nrow(mta_preds) == 0) {
        msg_df <- data.frame(Message = "No trait distribution data available",
                             stringsAsFactors = FALSE)
        return(DT::datatable(msg_df, options = list(dom = "t", paging = FALSE),
                             rownames = FALSE))
      }

      # Build wide format and compute index same as candidate_context_table
      traits_used <- names(trait_directions())
      if (is.null(traits_used) || length(traits_used) == 0) {
        traits_used <- unique(mta_preds$trait)
      }

      pred_wide <- tryCatch({
        reshape(
          mta_preds[mta_preds$trait %in% traits_used,
                    c("designation", "trait", "predictedValue"), drop = FALSE],
          idvar = "designation", timevar = "trait", direction = "wide"
        )
      }, error = function(e) NULL)

      if (is.null(pred_wide) || nrow(pred_wide) == 0) {
        msg_df <- data.frame(Message = "No trait distribution data available",
                             stringsAsFactors = FALSE)
        return(DT::datatable(msg_df, options = list(dom = "t", paging = FALSE),
                             rownames = FALSE))
      }
      names(pred_wide) <- sub("^predictedValue\\.", "", names(pred_wide))

      # Compute index using median weights across stakeholders
      all_weight_lists <- list()
      all_trait_sets <- list()
      for (stamp_id in selected_stamps) {
        s_init_rows <- modeling[
          modeling$module == "Final_prodAdv" &
            modeling$analysisId == stamp_id &
            modeling$parameter == "inputObject",
          , drop = FALSE
        ]
        if (nrow(s_init_rows) == 0) next
        s_init_stamp <- s_init_rows$value[1]
        s_weight_rows <- modeling[
          modeling$analysisId == s_init_stamp & modeling$parameter == "index_weight",
          , drop = FALSE
        ]
        if (nrow(s_weight_rows) > 0) {
          w <- as.numeric(s_weight_rows$value)
          names(w) <- s_weight_rows$trait
          all_weight_lists[[stamp_id]] <- w
        }
        s_all_tr <- unique(modeling$trait[
          modeling$analysisId == s_init_stamp &
            modeling$module == "Init_prodAdv" &
            !is.na(modeling$trait) & nzchar(modeling$trait)
        ])
        s_excluded_tr <- modeling$trait[
          modeling$analysisId == s_init_stamp &
            modeling$parameter == "user_excluded_trait"
        ]
        all_trait_sets[[stamp_id]] <- setdiff(s_all_tr, s_excluded_tr)
      }

      # Majority rule: traits used by more than half the stakeholders
      all_traits_union <- unique(unlist(all_trait_sets))
      n_stakeholders <- length(all_trait_sets)
      union_traits <- all_traits_union[vapply(all_traits_union, function(tr) {
        n_using <- sum(vapply(all_trait_sets, function(ts) tr %in% ts, logical(1)))
        n_using > n_stakeholders / 2
      }, logical(1))]

      if (length(union_traits) > 0 && length(all_weight_lists) > 0) {
        idx_weights <- vapply(union_traits, function(tr) {
          trait_weights <- vapply(all_weight_lists, function(wl) {
            if (tr %in% names(wl)) wl[[tr]] else 0
          }, numeric(1))
          median(trait_weights)
        }, numeric(1))
        names(idx_weights) <- union_traits
        avail_traits <- intersect(names(idx_weights), colnames(pred_wide))
        if (length(avail_traits) > 0) {
          trait_mat <- as.matrix(pred_wide[, avail_traits, drop = FALSE])
          scaled_mat <- scale(trait_mat)
          scaled_mat[is.nan(scaled_mat)] <- 0
          w <- idx_weights[avail_traits]
          pred_wide$index_value <- as.numeric(scaled_mat %*% w)
        } else {
          pred_wide$index_value <- NA_real_
        }
      } else {
        pred_wide$index_value <- NA_real_
      }

      # Classify designations using meeting decisions + auto-assigned
      stakeholder_cols <- setdiff(colnames(cm), c("designation", "controversy_score"))
      md <- meeting_decisions()
      auto <- auto_assign_non_controversial(cm)

      # Build combined decisions
      all_decisions <- auto
      if (!is.null(md) && length(md) > 0) {
        disc_df <- data.frame(
          designation = names(md),
          decision = as.character(unlist(md)),
          stringsAsFactors = FALSE
        )
        all_decisions <- rbind(disc_df, all_decisions)
        all_decisions <- all_decisions[!duplicated(all_decisions$designation), , drop = FALSE]
      }

      # Identify checks
      check_desigs <- character(0)
      for (i in seq_len(nrow(cm))) {
        decisions <- as.character(unlist(cm[i, stakeholder_cols]))
        if (any(decisions == "CHECK", na.rm = TRUE)) {
          check_desigs <- c(check_desigs, cm$designation[i])
        }
      }

      # Build index value groups
      selected_desigs <- all_decisions$designation[all_decisions$decision == "SELECTED"]
      overall_desigs <- setdiff(pred_wide$designation, check_desigs)

      selected_idx <- pred_wide$index_value[pred_wide$designation %in% selected_desigs &
                                              !is.na(pred_wide$index_value)]
      overall_idx <- pred_wide$index_value[pred_wide$designation %in% overall_desigs &
                                             !is.na(pred_wide$index_value)]
      check_idx <- pred_wide$index_value[pred_wide$designation %in% check_desigs &
                                           !is.na(pred_wide$index_value)]

      index_values <- list(
        selected = selected_idx,
        overall = overall_idx,
        checks = check_idx
      )

      stats_df <- tpp_compute_dashboard_stats(index_values)

      DT::datatable(stats_df, options = list(dom = "t", paging = FALSE, ordering = FALSE),
                    rownames = FALSE, caption = "Population-level index statistics")
    })

    # --- TPP Breakdown: explanation, detailed table, environment notes ---
    # Uses the same builder as the Pre-advancement dashboard so the table is
    # identical; the only difference is that the selected/candidate sets come
    # from the meeting consensus decisions rather than a single stakeholder's.
    tpp_breakdown_detail <- reactive({
      ctx <- meeting_context()
      if (is.null(ctx) || is.null(ctx$tpp_id)) return(NULL)

      dt <- tryCatch(data(), error = function(e) NULL)
      if (is.null(dt)) return(NULL)

      current_tpp <- ctx$tpp_id
      tpp_meta <- dt$metadata$TPP[[current_tpp]]
      raw_tpp <- tryCatch(dt$data$TPP[[current_tpp]], error = function(e) NULL)
      if (is.null(tpp_meta) || is.null(raw_tpp)) return(NULL)

      # The authoritative table behind the Final Decision Table and the
      # statistics panels on the Review meeting consensus tab.
      predictions_df <- tryCatch(adv_review_decision_data(), error = function(e) NULL)
      if (is.null(predictions_df) || !is.data.frame(predictions_df) ||
          nrow(predictions_df) == 0) {
        return(NULL)
      }

      status_col <- if ("final_decision" %in% colnames(predictions_df)) "final_decision" else NULL

      selected_designations <- character(0)
      candidate_designations <- unique(as.character(predictions_df$designation))
      if (!is.null(status_col)) {
        st <- toupper(trimws(as.character(predictions_df[[status_col]])))
        selected_designations <- predictions_df$designation[st == "SELECTED"]
        # Candidate set excludes checks, matching the statistics panel wording
        candidate_designations <- predictions_df$designation[st != "CHECK"]
      }

      # Total environments, for phrasing the environment-subset notes, plus the
      # traits actually carried into the selection.
      n_env <- NA_integer_
      used_traits <- NULL
      init_stamp <- if (length(ctx$init_stamps) > 0) ctx$init_stamps[1] else NULL
      if (!is.null(init_stamp) && nzchar(init_stamp)) {
        srow <- which(
          as.character(dt$modeling$analysisId) == as.character(init_stamp) &
            dt$modeling$module == "Init_prodAdv" &
            dt$modeling$parameter == "sta_stamp"
        )
        if (length(srow) > 0) {
          sta_stamp <- dt$modeling$value[srow[1]]
          envs <- dt$predictions$environment[
            as.character(dt$predictions$analysisId) %in% as.character(sta_stamp)]
          envs <- unique(envs[!is.na(envs) & nzchar(envs) & envs != "across"])
          if (length(envs) > 0) n_env <- length(envs)
        }

        # Traits used by the meeting: union across stakeholders, minus any a
        # stakeholder explicitly dropped.
        all_tr <- character(0)
        dropped <- character(0)
        for (istamp in ctx$init_stamps) {
          mrows <- which(
            as.character(dt$modeling$analysisId) == as.character(istamp) &
              dt$modeling$module == "Init_prodAdv"
          )
          if (length(mrows) == 0) next
          mi <- dt$modeling[mrows, , drop = FALSE]
          all_tr <- c(all_tr, mi$trait[!is.na(mi$trait) & nzchar(mi$trait)])
          dropped <- c(dropped, mi$trait[mi$parameter == "user_excluded_trait"])
        }
        all_tr <- unique(all_tr)
        dropped <- unique(dropped[!is.na(dropped) & nzchar(dropped)])
        used_traits <- setdiff(all_tr, dropped)
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
        "dropped by the stakeholders, for instance after a low-reliability flag, is",
        "mapped but not used, and its percentage reads \"trait not used\" \u2014 the",
        "requirement category, and two",
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
        "the meeting consensus produced. The two compliance columns pair with them:",
        tags$b("Sel_pct_meeting_criteria"), "is the share of the",
        tags$b("selected"), "individuals meeting the resolved threshold, while",
        tags$b("All_pct_meeting_criteria"), "is the share across all candidates \u2014",
        "comparing the two shows how much the criterion was enriched by selection.",
        "Traits with no phenotypic mapping are reported as \"not evaluated\", traits",
        "that were dropped read \"trait not used\", and traits whose desired score is",
        "relative but have no mapped checks cannot be resolved to an absolute value."
      )
    })

    output$advDashboardBreakdown <- DT::renderDT({
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

    # --- Stakeholder Options Summary (Global Options Summary) ---
    # Kept as a reactive returning the named list of per-stakeholder tables so
    # the Output tab and the downloadable report render the same data.
    stakeholder_options_tables <- reactive({
      dt_obj <- data()
      if (is.null(dt_obj)) return(NULL)

      ctx <- meeting_context()
      if (is.null(ctx) || length(ctx$stakeholder_stamps) == 0) return(NULL)
      selected_stamps <- ctx$stakeholder_stamps

      modeling <- dt_obj$modeling
      status <- dt_obj$status

      # Build stakeholder_configs: one entry per stakeholder with their trait configs
      stakeholder_configs <- list()

      for (stamp_id in selected_stamps) {
        # Get stakeholder display name from status
        stamp_name <- status$analysisIdName[
          match(as.character(stamp_id), as.character(status$analysisId))
        ]
        if (is.na(stamp_name)) stamp_name <- stamp_id

        # Trace to Init_prodAdv stamp
        init_rows <- modeling[
          modeling$module == "Final_prodAdv" &
            as.character(modeling$analysisId) == as.character(stamp_id) &
            modeling$parameter == "inputObject",
          , drop = FALSE
        ]
        if (nrow(init_rows) == 0) next
        init_stamp <- as.character(init_rows$value[1])

        is_init <- as.character(modeling$analysisId) == init_stamp

        # Get weight and threshold info for this stakeholder's Init_prodAdv
        weight_rows    <- modeling[is_init & modeling$parameter == "index_weight", , drop = FALSE]
        direction_rows <- modeling[is_init & modeling$parameter == "direction", , drop = FALSE]
        threshold_rows <- modeling[is_init & modeling$parameter == "threshold", , drop = FALSE]
        rule_type_rows <- modeling[is_init & modeling$parameter == "trait_rule_type", , drop = FALSE]
        ref_check_rows <- modeling[is_init & modeling$parameter == "reference_check", , drop = FALSE]

        # Get all selected traits (exclude user-excluded)
        all_tr <- unique(modeling$trait[
          is_init &
            modeling$module == "Init_prodAdv" &
            !is.na(modeling$trait) & nzchar(modeling$trait)
        ])
        excluded_tr <- modeling$trait[
          is_init & modeling$parameter == "user_excluded_trait"
        ]
        selected_traits <- setdiff(all_tr, excluded_tr)

        if (length(selected_traits) == 0) next

        # Build trait_configs for this stakeholder
        trait_cfgs <- lapply(selected_traits, function(trait_name) {
          # Weight
          w_row <- weight_rows[weight_rows$trait == trait_name, , drop = FALSE]
          weight_val <- if (nrow(w_row) > 0) as.numeric(w_row$value[1]) else 0

          # Threshold
          threshold_value <- NA_real_
          threshold_direction <- NA_character_
          reference_checks <- NULL
          reference_value <- NA_real_

          rule_row <- rule_type_rows[rule_type_rows$trait == trait_name, , drop = FALSE]
          rule_type <- if (nrow(rule_row) > 0) rule_row$value[1] else "None"

          if (!is.null(rule_type) && rule_type != "None") {
            t_row <- threshold_rows[threshold_rows$trait == trait_name, , drop = FALSE]
            if (nrow(t_row) > 0) {
              threshold_value <- as.numeric(t_row$value[1])
            }

            dir_row <- direction_rows[direction_rows$trait == trait_name, , drop = FALSE]
            if (nrow(dir_row) > 0) {
              dir_val <- dir_row$value[1]
              threshold_direction <- if (identical(dir_val, "Higher is better")) "greater" else "less"
            }

            rc_row <- ref_check_rows[ref_check_rows$trait == trait_name, , drop = FALSE]
            if (nrow(rc_row) > 0 && nzchar(rc_row$value[1])) {
              reference_checks <- unlist(strsplit(rc_row$value[1], ","))

              # Compute reference value from predictions
              mta_rows <- modeling[
                modeling$module == "Init_prodAdv" &
                  as.character(modeling$analysisId) == init_stamp &
                  modeling$parameter == "mta_stamp",
                , drop = FALSE
              ]
              if (nrow(mta_rows) > 0) {
                mta_stamp_val <- as.character(mta_rows$value[1])
                preds <- dt_obj$predictions
                ref_vals <- preds$predictedValue[
                  as.character(preds$analysisId) == mta_stamp_val &
                    preds$effectType == "designation" &
                    preds$trait == trait_name &
                    preds$designation %in% reference_checks &
                    !is.na(preds$predictedValue)
                ]
                if (length(ref_vals) > 0) {
                  reference_value <- mean(ref_vals)
                }
              }
            }
          }

          list(
            trait_name = trait_name,
            weight = weight_val,
            threshold_value = threshold_value,
            threshold_direction = threshold_direction,
            reference_checks = reference_checks,
            reference_value = reference_value
          )
        })

        stakeholder_configs[[stamp_name]] <- trait_cfgs
      }

      tpp_build_stakeholder_options_table(stakeholder_configs)
    })

    output$advDashboardStakeholderOptions <- renderUI({
      stakeholder_tables <- stakeholder_options_tables()

      if (is.null(stakeholder_tables) || length(stakeholder_tables) == 0) {
        return(tags$div(
          tags$p(style = "color: #666; font-style: italic;",
                 "No stakeholder configuration data available")
        ))
      }

      # Check for "no data" message
      if (length(stakeholder_tables) == 1 && "No Data" %in% names(stakeholder_tables)) {
        first_table <- stakeholder_tables[[1]]
        if ("Message" %in% colnames(first_table)) {
          return(tags$div(
            tags$p(style = "color: #666; font-style: italic;",
                   first_table$Message[1])
          ))
        }
      }

      # Render each stakeholder's table as a separate section
      table_panels <- lapply(names(stakeholder_tables), function(sname) {
        tbl_df <- stakeholder_tables[[sname]]
        if (is.null(tbl_df) || nrow(tbl_df) == 0) {
          return(tags$div(
            tags$h4(sname),
            tags$p(style = "color: #666; font-style: italic;", "No configuration data")
          ))
        }

        # Build HTML table manually for embedding in renderUI
        header_row <- tags$tr(
          lapply(colnames(tbl_df), function(col) tags$th(style = "padding: 8px; border-bottom: 2px solid #ddd; text-align: left;", col))
        )

        body_rows <- lapply(seq_len(nrow(tbl_df)), function(i) {
          tags$tr(
            lapply(seq_len(ncol(tbl_df)), function(j) {
              tags$td(style = "padding: 6px 8px; border-bottom: 1px solid #eee;",
                      as.character(tbl_df[i, j]))
            })
          )
        })

        tags$div(
          style = "margin-bottom: 20px;",
          tags$h4(style = "color: darkcyan;", sname),
          tags$table(
            style = "width: 100%; border-collapse: collapse; font-size: 13px;",
            tags$thead(header_row),
            tags$tbody(body_rows)
          )
        )
      })

      tags$div(table_panels)
    })

    # =========================================================================
    # DASHBOARD SUBTAB: Additional outputs (heatmap, decisions table, stats, distribution)
    # =========================================================================

    # --- Dashboard: Population Statistics (per-trait tables with colored rows) ---
    output$dashStatsUI <- renderUI({
      long <- adv_review_long()
      if (is.null(long)) {
        return(tags$div(
          style = "padding:12px 14px; background-color:#FCF3CF; border-left:4px solid #B7950B; border-radius:3px; color:#7D6608;",
          tags$b("No statistics to display.")
        ))
      }

      ns <- session$ns
      metrics <- unique(long$metric)

      tagList(lapply(metrics, function(m) {
        label <- if (identical(m, "index_value")) "Index Value" else m
        out_id <- paste0("dashMeetStatsDT_", gsub("[^A-Za-z0-9]", "_", m))
        tags$div(
          style = "margin-bottom: 18px;",
          tags$div(
            style = paste0(
              "font-weight:700; font-size:14px; color:#2C3E50;",
              " padding:6px 0 4px 2px; border-bottom:2px solid #2C3E50;",
              " margin-bottom:6px;"
            ),
            label
          ),
          DT::DTOutput(ns(out_id))
        )
      }))
    })

    # Register DT renderers for dashboard stats tables
    observe({
      long <- adv_review_long()
      if (is.null(long)) return()

      faded <- review_group_colors_faded(0.35)
      metrics <- unique(long$metric)

      lapply(metrics, function(m) {
        local({
          metric_name <- m
          out_id <- paste0("dashMeetStatsDT_", gsub("[^A-Za-z0-9]", "_", metric_name))

          output[[out_id]] <- DT::renderDT({
            sub <- long[long$metric == metric_name, , drop = FALSE]
            stats_df <- tpp_compute_dashboard_stats(list(
              selected = sub$value[sub$group == "Selected"],
              overall  = sub$value[sub$group == "All candidates"],
              checks   = sub$value[sub$group == "Checks"]
            ))
            validate(need(!is.null(stats_df) && nrow(stats_df) > 0,
                          "No trait distribution data available."))
            stats_df$Group[stats_df$Group == "Overall"] <- "All candidates"
            stats_df$Group <- factor(stats_df$Group, levels = names(REVIEW_GROUP_COLORS))
            stats_df <- stats_df[order(stats_df$Group), , drop = FALSE]
            stats_df$Group <- as.character(stats_df$Group)
            for (col in intersect(c("Min", "Max", "Mean", "Median", "SD"), colnames(stats_df))) {
              stats_df[[col]] <- round(as.numeric(stats_df[[col]]), 3)
            }

            dt_obj <- DT::datatable(
              stats_df,
              rownames = FALSE,
              selection = "none",
              options = list(dom = "t", paging = FALSE, searching = FALSE, ordering = FALSE, scrollX = TRUE)
            )

            DT::formatStyle(dt_obj, "Group", target = "row",
              backgroundColor = DT::styleEqual(names(faded), unname(faded)))
          })
        })
      })
    })

    # --- Dashboard: Distribution histogram (faceted, same as Review tab) ---
    output$dashStatsHistogram <- plotly::renderPlotly({
      long <- adv_review_long()
      validate(need(!is.null(long), "No trait distribution data available."))

      plot_df <- long[long$group %in% c("All candidates", "Selected"), , drop = FALSE]
      validate(need(nrow(plot_df) > 0, "No data to plot."))

      metric_order <- unique(long$metric)
      label_order <- vapply(metric_order, function(m) {
        if (identical(m, "index_value")) "Index Value" else m
      }, character(1))
      plot_df$label <- factor(plot_df$label, levels = unname(label_order))
      plot_df$group <- factor(plot_df$group, levels = c("All candidates", "Selected"))

      check_df <- long[long$group == "Checks", , drop = FALSE]
      check_means <- NULL
      if (nrow(check_df) > 0) {
        check_means <- stats::aggregate(value ~ label, data = check_df, FUN = mean, na.rm = TRUE)
        check_means$label <- factor(check_means$label, levels = unname(label_order))
      }

      p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = value, fill = group)) +
        ggplot2::geom_histogram(position = "identity", alpha = 0.55, bins = 30, colour = NA) +
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
      sel_means <- stats::aggregate(value ~ label, data = plot_df[plot_df$group == "Selected", , drop = FALSE], FUN = mean, na.rm = TRUE)
      all_means <- stats::aggregate(value ~ label, data = plot_df[plot_df$group == "All candidates", , drop = FALSE], FUN = mean, na.rm = TRUE)
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
      plotly::layout(fig,
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.06, yanchor = "bottom"),
        margin = list(t = 70, b = 70)
      )
    })

    # --- Dashboard: Render button handler ---
    observeEvent(input$renderReportAdvMeeting, {
      shinyjs_available <- requireNamespace("shinyjs", quietly = TRUE)
      # Trigger the hidden download button
      shiny::insertUI(
        selector = paste0("#", ns("download_report")),
        where = "afterEnd",
        ui = tags$script(HTML(paste0(
          "document.getElementById('", ns("download_report"), "').click();"
        ))),
        immediate = TRUE
      )
    })

    # =========================================================================
    # REVIEW MEETING CONSENSUS TAB: Final Review content (no lock)
    # =========================================================================

    # Reactive: check if all controversial candidates are resolved
    all_controversial_resolved <- reactive({
      ds <- discussion_status()
      candidates <- discussion_candidates()
      # No comparison matrix yet = not resolved
      if (is.null(candidates) || length(candidates) == 0) return(FALSE)
      if (is.null(ds)) return(FALSE)
      all(ds == "Resolved")
    })

    # Render the Review meeting consensus tab UI (no lock â€” users can navigate freely)
    output$advMeetingReviewUI <- renderUI({
      cm <- dash_comparison_matrix()

      # If no comparison data yet, show informational message
      if (is.null(cm) || nrow(cm) == 0) {
        return(tags$div(
          style = "padding: 40px; text-align: center;",
          tags$div(
            style = "background-color: #d9edf7; border: 1px solid #bce8f1; border-radius: 4px; padding: 30px; max-width: 600px; margin: auto;",
            icon("info-circle", style = "font-size: 48px; color: #31708f; margin-bottom: 15px;"),
            tags$h3(style = "color: #31708f;", "No Meeting Data Available"),
            tags$p(style = "color: #31708f; font-size: 14px;",
                   "Please go to 'Input steps' and merge stakeholder stamps first.")
          )
        ))
      }

      ns <- session$ns

      # Mirror the Pre-advancement "Final Review and Run selection" tab structure
      tagList(

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
                ns("advReviewPortion"),
                label = tags$span(
                  "Rows to display (ranked by index)",
                  tags$i(
                    class = "glyphicon glyphicon-info-sign",
                    title = "Limits how many rows the table shows. Saving the meeting decisions always uses every evaluated individual, not just the displayed rows."
                  )
                ),
                choices = c(
                  "Top 50"  = 50,
                  "Top 100" = 100,
                  "Top 200" = 200,
                  "All"     = -1
                ),
                selected = -1
              )
            ),
            column(
              width = 8,
              uiOutput(ns("advReviewPortionNote"))
            )
          ),

          DT::DTOutput(ns("finalDecisionDT"))
        ),

        hr(),

        # --- Selection Statistics ---
        shinydashboard::box(
          width = 12,
          title = "Review Meeting Consensus: Selection Statistics",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,

          fluidRow(
            column(
              width = 12,
              uiOutput(ns("advReviewSelPct"))
            )
          ),

          hr(),

          # One statistics table per trait (Selected / All candidates / Checks)
          uiOutput(ns("advReviewStatsTablesUI")),

          hr(),

          # Distribution of selected candidates vs all candidates, per trait
          plotly::plotlyOutput(ns("advReviewHistogram"), height = "auto"),

          hr(),

          # TPP compliance percentages (only shown when TPP active)
          DT::DTOutput(ns("advReviewTppPctsDT"))
        ),

        hr(),

        # --- TPP Breakdown (only shown when a TPP is attached) ---
        uiOutput(ns("advReviewTppBreakdownBox")),

        # --- Save section ---
        column(
          width = 12,
          style = "background-color:grey; color: #FFFFFF; padding:15px;",
          column(
            width = 5,
            textInput(
              ns("meeting_name"),
              label = tags$span(style = "color:white;", "Meeting name (required)"),
              placeholder = "e.g., 2024_maize_advancement_meeting"
            )
          ),
          column(
            width = 4,
            br(),
            actionButton(
              ns("save_btn"),
              "Save meeting decisions & generate report",
              icon = icon("save"),
              class = "btn-success"
            )
          ),
          column(
            width = 3,
            br(),
            actionButton(
              ns("advReviewBackBtn"),
              "Back to conflict resolution",
              icon = icon("arrow-left")
            )
          )
        ),

        br(),
        textOutput(ns("auto_assigned_summary"))
      )
    })

    # ---- Reactive: Build meeting-based decision data for Review Selection ----
    adv_review_decision_data <- reactive({
      cm <- dash_comparison_matrix()
      req(cm)
      dt_obj <- data()
      req(dt_obj)

      ctx <- meeting_context()
      req(ctx)
      selected_stamps <- ctx$stakeholder_stamps
      req(length(selected_stamps) > 0)
      modeling <- dt_obj$modeling
      mta_stamp <- ctx$mta_stamp
      req(!is.na(mta_stamp), nzchar(mta_stamp))

      # Get predictions
      preds <- dt_obj$predictions
      mta_preds <- preds[
        as.character(preds$analysisId) == as.character(mta_stamp) &
          preds$effectType == "designation" &
          preds$designation %in% cm$designation,
        , drop = FALSE
      ]
      req(nrow(mta_preds) > 0)

      # Traits
      dirs <- dash_trait_info()
      traits <- if (!is.null(dirs)) names(dirs) else unique(mta_preds$trait)

      # Build wide-format predictions
      pred_wide <- reshape(
        mta_preds[mta_preds$trait %in% traits, c("designation", "trait", "predictedValue"), drop = FALSE],
        idvar = "designation", timevar = "trait", direction = "wide"
      )
      names(pred_wide) <- sub("^predictedValue\\.", "", names(pred_wide))

      # Compute index using median weights across ALL stakeholders
      all_weight_lists <- list()
      all_trait_sets <- list()
      for (stamp_id in selected_stamps) {
        s_init_rows <- modeling[
          modeling$module == "Final_prodAdv" &
            as.character(modeling$analysisId) == as.character(stamp_id) &
            modeling$parameter == "inputObject",
          , drop = FALSE
        ]
        if (nrow(s_init_rows) == 0) next
        s_init_stamp <- s_init_rows$value[1]

        s_weight_rows <- modeling[
          as.character(modeling$analysisId) == as.character(s_init_stamp) &
            modeling$parameter == "index_weight",
          , drop = FALSE
        ]
        if (nrow(s_weight_rows) > 0) {
          w <- as.numeric(s_weight_rows$value)
          names(w) <- s_weight_rows$trait
          all_weight_lists[[stamp_id]] <- w
        }

        s_all_tr <- unique(modeling$trait[
          as.character(modeling$analysisId) == as.character(s_init_stamp) &
            modeling$module == "Init_prodAdv" &
            !is.na(modeling$trait) & nzchar(modeling$trait)
        ])
        s_excluded_tr <- modeling$trait[
          as.character(modeling$analysisId) == as.character(s_init_stamp) &
            modeling$parameter == "user_excluded_trait"
        ]
        all_trait_sets[[stamp_id]] <- setdiff(s_all_tr, s_excluded_tr)
      }

      # Majority rule: only include traits used by more than half the stakeholders
      all_traits_union <- unique(unlist(all_trait_sets))
      n_stakeholders <- length(all_trait_sets)
      union_traits <- all_traits_union[vapply(all_traits_union, function(tr) {
        n_using <- sum(vapply(all_trait_sets, function(ts) tr %in% ts, logical(1)))
        n_using > n_stakeholders / 2
      }, logical(1))]

      if (length(union_traits) > 0 && length(all_weight_lists) > 0) {
        idx_weights <- vapply(union_traits, function(tr) {
          trait_weights <- vapply(all_weight_lists, function(wl) {
            if (tr %in% names(wl)) wl[[tr]] else 0
          }, numeric(1))
          median(trait_weights)
        }, numeric(1))
        names(idx_weights) <- union_traits
        avail_traits <- intersect(names(idx_weights), colnames(pred_wide))
        if (length(avail_traits) > 0) {
          trait_mat <- as.matrix(pred_wide[, avail_traits, drop = FALSE])
          scaled_mat <- scale(trait_mat)
          scaled_mat[is.nan(scaled_mat)] <- 0
          w <- idx_weights[avail_traits]

          # Apply reliability weighting if available
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

      # Build final decision column from meeting decisions + auto-assigned
      # (falls back to the decisions saved on the Meeting_prodAdv stamp)
      all_decisions <- dash_final_decisions()
      if (is.null(all_decisions)) {
        all_decisions <- data.frame(designation = character(0), decision = character(0),
                                    stringsAsFactors = FALSE)
      }

      # Merge decisions into pred_wide
      pred_wide$final_decision <- all_decisions$decision[
        match(pred_wide$designation, all_decisions$designation)
      ]
      # Default unmatched to "NOT SELECTED"
      pred_wide$final_decision[is.na(pred_wide$final_decision)] <- "NOT SELECTED"

      # Sort by index descending
      pred_wide <- pred_wide[order(-pred_wide$index_value), , drop = FALSE]
      pred_wide
    })

    # ---- Reactive: Portion data for Review Selection ----
    adv_review_portion_data <- reactive({
      tbl <- adv_review_decision_data()
      req(tbl)
      req("index_value" %in% colnames(tbl))

      portion <- as.integer(input$advReviewPortion)
      if (is.na(portion) || portion == -1) {
        portion_tbl <- tbl
      } else {
        n_display <- min(portion, nrow(tbl))
        portion_tbl <- tbl[seq_len(n_display), , drop = FALSE]
      }
      portion_tbl
    })

    # ---- Render: Summary bar (mirrors Pre-advancement finalSummaryBar) ----
    output$finalSummaryBar <- renderUI({
      tbl <- adv_review_decision_data()
      if (is.null(tbl) || nrow(tbl) == 0) return(NULL)
      if (!("final_decision" %in% colnames(tbl))) return(NULL)

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

    # ---- Render: Final Decision Table (gradient-colored, mirrors Pre-advancement) ----
    output$finalDecisionDT <- DT::renderDT({
      tbl <- adv_review_decision_data()
      req(tbl)
      req("final_decision" %in% colnames(tbl))

      # Apply portion filter
      portion <- as.integer(input$advReviewPortion)
      if (!is.na(portion) && portion > 0 && portion < nrow(tbl)) {
        tbl <- tbl[seq_len(portion), , drop = FALSE]
      }

      # Get traits
      dirs <- dash_trait_info()
      traits <- if (!is.null(dirs)) names(dirs) else character(0)
      traits <- intersect(traits, colnames(tbl))

      # Colors
      col_selected <- "#85C1E9"
      col_not_selected <- "#E8A87C"
      col_check <- "#C39BD3"
      col_revise <- "#FFF3CD"

      # Build HTML display table
      display_df <- data.frame(designation = tbl$designation, stringsAsFactors = FALSE)

      # Index value column with gradient
      if ("index_value" %in% colnames(tbl)) {
        idx_status <- tbl$final_decision
        display_df$index_value <- gradient_cells(
          tbl$index_value, idx_status, higher_is_better = TRUE, bold = TRUE
        )
      }

      # Trait columns with gradient
      for (tr in traits) {
        trait_vals <- tbl[[tr]]
        trait_status <- tbl$final_decision
        d <- if (!is.null(dirs)) dirs[[tr]] else "Higher is better"
        hib <- is.null(d) || identical(d, "Higher is better")
        display_df[[tr]] <- gradient_cells(trait_vals, trait_status, higher_is_better = hib)
      }

      # Final decision column (badge)
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
          dom = "ft"
        )
      )
    })

    # ---- Reactive: Long-format values per metric and group (Selected / All candidates / Checks) ----
    adv_review_long <- reactive({
      tbl <- adv_review_decision_data()
      if (is.null(tbl) || nrow(tbl) == 0) return(NULL)
      if (!("final_decision" %in% colnames(tbl))) return(NULL)

      dirs <- dash_trait_info()
      traits <- if (!is.null(dirs)) names(dirs) else character(0)
      traits <- intersect(traits, colnames(tbl))
      metrics <- character(0)
      if ("index_value" %in% colnames(tbl)) metrics <- "index_value"
      metrics <- c(metrics, traits)

      # Keep only metrics that have finite values
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
      out$label <- vapply(out$metric, function(m) {
        if (identical(m, "index_value")) "Index Value" else m
      }, character(1))
      out
    })

    # ---- Render: Per-trait statistics tables UI (mirrors Pre-advancement) ----
    output$advReviewStatsTablesUI <- renderUI({
      long <- adv_review_long()
      if (is.null(long)) {
        return(tags$div(
          style = "padding:12px 14px; background-color:#FCF3CF; border-left:4px solid #B7950B; border-radius:3px; color:#7D6608;",
          tags$b("No statistics to display."),
          " No trait values are available to summarise for this selection."
        ))
      }

      ns <- session$ns
      metrics <- unique(long$metric)

      tagList(lapply(metrics, function(m) {
        label <- if (identical(m, "index_value")) "Index Value" else m
        out_id <- paste0("advMeetStatsDT_", gsub("[^A-Za-z0-9]", "_", m))
        tags$div(
          style = "margin-bottom: 18px;",
          tags$div(
            style = paste0(
              "font-weight:700; font-size:14px; color:#2C3E50;",
              " padding:6px 0 4px 2px; border-bottom:2px solid #2C3E50;",
              " margin-bottom:6px;"
            ),
            label
          ),
          DT::DTOutput(ns(out_id))
        )
      }))
    })

    # Register DT renderers for the per-trait stats tables (with row coloring)
    observe({
      long <- adv_review_long()
      if (is.null(long)) return()

      faded <- review_group_colors_faded(0.35)
      metrics <- unique(long$metric)

      lapply(metrics, function(m) {
        local({
          metric_name <- m
          out_id <- paste0("advMeetStatsDT_", gsub("[^A-Za-z0-9]", "_", metric_name))

          output[[out_id]] <- DT::renderDT({
            sub <- long[long$metric == metric_name, , drop = FALSE]
            stats_df <- tpp_compute_dashboard_stats(list(
              selected = sub$value[sub$group == "Selected"],
              overall  = sub$value[sub$group == "All candidates"],
              checks   = sub$value[sub$group == "Checks"]
            ))
            validate(need(!is.null(stats_df) && nrow(stats_df) > 0,
                          "No trait distribution data available."))
            stats_df$Group[stats_df$Group == "Overall"] <- "All candidates"
            stats_df$Group <- factor(stats_df$Group, levels = names(REVIEW_GROUP_COLORS))
            stats_df <- stats_df[order(stats_df$Group), , drop = FALSE]
            stats_df$Group <- as.character(stats_df$Group)
            for (col in intersect(c("Min", "Max", "Mean", "Median", "SD"), colnames(stats_df))) {
              stats_df[[col]] <- round(as.numeric(stats_df[[col]]), 3)
            }

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
    })

    # ---- Render: Portion note beside the selector ----
    output$advReviewPortionNote <- renderUI({
      tbl <- adv_review_decision_data()
      req(tbl)
      n_total <- nrow(tbl)
      portion <- suppressWarnings(as.integer(input$advReviewPortion))
      if (length(portion) != 1 || is.na(portion)) portion <- -1L
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
          " Saving the meeting decisions always includes every individual."
        )
      )
    })

    # ---- Render: Selection percentage ----
    output$advReviewSelPct <- renderUI({
      tbl <- adv_review_decision_data()
      req(tbl)
      req("final_decision" %in% colnames(tbl))

      decisions <- toupper(trimws(as.character(tbl$final_decision)))
      candidates <- decisions[decisions != "CHECK"]
      n_candidates <- length(candidates)
      n_selected <- sum(candidates == "SELECTED", na.rm = TRUE)

      pct <- if (n_candidates > 0) round((n_selected / n_candidates) * 100, 1) else 0

      tags$div(
        style = "padding-top: 30px;",
        tags$h4(
          style = "color: #2C3E50; font-weight: 600;",
          paste0("Final Selection: ", n_selected, " / ", n_candidates,
                 " candidates SELECTED (", pct, "%)")
        )
      )
    })

    # ---- Render: Faceted distribution plot (mirrors Pre-advancement) ----
    output$advReviewHistogram <- plotly::renderPlotly({
      long <- adv_review_long()
      validate(need(!is.null(long), "No trait distribution data available."))

      # Build faceted histogram: SELECTED vs ALL candidates, checks as dashed line
      plot_df <- long[long$group %in% c("All candidates", "Selected"), , drop = FALSE]
      validate(need(nrow(plot_df) > 0, "No data to plot."))

      metric_order <- unique(long$metric)
      label_order <- vapply(metric_order, function(m) {
        if (identical(m, "index_value")) "Index Value" else m
      }, character(1))
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
      sel_means <- stats::aggregate(value ~ label, data = plot_df[plot_df$group == "Selected", , drop = FALSE], FUN = mean, na.rm = TRUE)
      all_means <- stats::aggregate(value ~ label, data = plot_df[plot_df$group == "All candidates", , drop = FALSE], FUN = mean, na.rm = TRUE)
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

      plotly::layout(
        fig,
        legend = list(
          orientation = "h",
          x = 0.5, xanchor = "center",
          y = 1.06, yanchor = "bottom"
        ),
        margin = list(t = 70, b = 70)
      )
    })

    # ---- Render: TPP Breakdown box on the Review meeting consensus tab ----
    # Shown only when the meeting has a TPP attached; reuses the same breakdown
    # table that the Output dashboard renders.
    output$advReviewTppBreakdownBox <- renderUI({
      ctx <- meeting_context()
      if (is.null(ctx) || is.null(ctx$tpp_id)) return(NULL)

      dt_obj <- data()
      if (is.null(dt_obj)) return(NULL)
      tpp_meta_all <- dt_obj$metadata$TPP
      if (is.null(tpp_meta_all) || !(ctx$tpp_id %in% names(tpp_meta_all))) return(NULL)

      ns <- session$ns
      tagList(
        shinydashboard::box(
          width = 12,
          title = paste0("TPP Breakdown (", ctx$tpp_id, ")"),
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          DT::DTOutput(ns("advReviewTppBreakdownDT"))
        ),
        hr()
      )
    })

    output$advReviewTppBreakdownDT <- DT::renderDT({
      # Same content as the dashboard breakdown table
      res <- tpp_breakdown_detail()
      validate(need(!is.null(res), "No TPP information available."))
      tpp_style_breakdown_proximity(
        DT::datatable(
          res$table,
          rownames = FALSE,
          selection = "none",
          options = list(dom = "tp", pageLength = 20, scrollX = TRUE, ordering = TRUE)
        )
      )
    })

    # ---- Render: TPP compliance percentages ----
    output$advReviewTppPctsDT <- DT::renderDT({
      tbl <- adv_review_decision_data()
      req(tbl)
      req("final_decision" %in% colnames(tbl))

      # Check if TPP data is available
      dt_obj <- data()
      req(dt_obj)
      tpp_meta_all <- dt_obj$metadata$TPP
      req(!is.null(tpp_meta_all) && length(tpp_meta_all) > 0)

      # Resolve TPP ID via the shared meeting context (live stamps or saved stamp)
      ctx <- meeting_context()
      req(ctx)
      modeling <- dt_obj$modeling
      current_tpp <- ctx$tpp_id

      req(!is.null(current_tpp) && current_tpp %in% names(tpp_meta_all))

      # Enriched trait table (category / desired score columns joined from the
      # raw TPP sheet) â€” the criteria builder needs those columns
      tpp_all_traits <- dash_tpp_traits()
      req(!is.null(tpp_all_traits) && nrow(tpp_all_traits) > 0)

      # Filter to mapped traits
      filtered <- tpp_all_traits[!is.na(tpp_all_traits$pheno_trait) &
                                   nzchar(tpp_all_traits$pheno_trait), , drop = FALSE]
      req(nrow(filtered) > 0)

      # Build criteria list
      tpp_criteria <- tryCatch(
        tpp_build_criteria_list_from_filtered(
          filtered, tbl,
          checks_per_trait = dash_tpp_checks()
        ),
        error = function(e) list()
      )
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

      # Compute compliance percentages (Requirement 12.5)
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
        options = list(dom = "t", paging = FALSE, searching = FALSE, ordering = FALSE),
        caption = "TPP compliance: percentage of individuals meeting all criteria"
      )
    })

    # ---- observeEvent: "Back to Discussion" button (Requirement 12.7) ----
    observeEvent(input$advReviewBackBtn, {
      # Navigate back to conflict resolution tab, preserving all current decisions/statuses
      updateTabsetPanel(session, "tabsMain", selected = "Conflict resolution")
    })

    # Download handler for the dashboard report (Requirements 11.7)
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("advancement_meeting_dashboard_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
      },
      content = function(file) {
        tryCatch({
          # Gather data (same logic as renderUI above)
          dt_obj <- data()
          cm <- dash_comparison_matrix()
          if (is.null(cm)) cm <- data.frame()

          # meeting_decisions_df
          meeting_decisions_df <- tryCatch({
            meeting_status <- dt_obj$status[dt_obj$status$module == "Meeting_prodAdv", , drop = FALSE]
            if (nrow(meeting_status) == 0) stop("no meeting")
            meeting_stamp_id <- meeting_status$analysisId[nrow(meeting_status)]
            mods <- dt_obj$modifications$selection
            meeting_mods <- mods[
              mods$analysisId == meeting_stamp_id &
                mods$module == "Meeting_prodAdv" &
                mods$reason == "meeting_decision",
              , drop = FALSE
            ]
            if (nrow(meeting_mods) == 0) stop("no mods")
            df <- data.frame(
              designation = meeting_mods$designation,
              decision = meeting_mods$value,
              stringsAsFactors = FALSE
            )
            if (nrow(cm) > 0 && "controversy_score" %in% colnames(cm)) {
              df$controversy_score <- cm$controversy_score[match(df$designation, cm$designation)]
            } else {
              df$controversy_score <- NA_real_
            }
            df
          }, error = function(e) data.frame())

          # mta_stamp
          mta_stamp <- tryCatch({
            ctx_dl <- meeting_context()
            selected_stamps <- if (!is.null(ctx_dl)) ctx_dl$stakeholder_stamps else input$stamp_select
            if (!is.null(selected_stamps) && length(selected_stamps) > 0) {
              mta_res <- validate_mta_compatibility(dt_obj, selected_stamps)
              if (mta_res$compatible) mta_res$mta_stamp else NA_character_
            } else {
              meeting_status <- dt_obj$status[dt_obj$status$module == "Meeting_prodAdv", , drop = FALSE]
              if (nrow(meeting_status) == 0) stop("no meeting")
              meeting_stamp_id <- meeting_status$analysisId[nrow(meeting_status)]
              input_rows <- dt_obj$modeling[
                dt_obj$modeling$module == "Meeting_prodAdv" &
                  dt_obj$modeling$analysisId == meeting_stamp_id &
                  dt_obj$modeling$parameter == "inputObject",
                , drop = FALSE
              ]
              if (nrow(input_rows) == 0) stop("no inputs")
              found_stamp <- NA_character_
              for (val in input_rows$value) {
                is_mta <- any(dt_obj$status$analysisId == val &
                                dt_obj$status$module %in% c("mta", "mtaLmms", "mtaAsr", "mtaFlex", "mas"))
                if (is_mta) { found_stamp <- val; break }
              }
              if (is.na(found_stamp)) {
                for (sid in input_rows$value) {
                  mta_res <- validate_mta_compatibility(dt_obj, sid)
                  if (mta_res$compatible) { found_stamp <- mta_res$mta_stamp; break }
                }
              }
              found_stamp
            }
          }, error = function(e) NA_character_)

          # participants
          participants <- tryCatch({
            meeting_status <- dt_obj$status[dt_obj$status$module == "Meeting_prodAdv", , drop = FALSE]
            if (nrow(meeting_status) == 0) stop("no meeting")
            meeting_stamp_id <- meeting_status$analysisId[nrow(meeting_status)]
            part_rows <- dt_obj$modeling[
              dt_obj$modeling$module == "Meeting_prodAdv" &
                dt_obj$modeling$analysisId == meeting_stamp_id &
                dt_obj$modeling$parameter == "participants",
              , drop = FALSE
            ]
            if (nrow(part_rows) == 0) character(0)
            else unlist(strsplit(part_rows$value[1], ","))
          }, error = function(e) character(0))

          # meeting_name
          meeting_name <- tryCatch({
            meeting_status <- dt_obj$status[dt_obj$status$module == "Meeting_prodAdv", , drop = FALSE]
            if (nrow(meeting_status) == 0) "Unnamed meeting"
            else meeting_status$analysisIdName[nrow(meeting_status)]
          }, error = function(e) "Unnamed meeting")

          # trait_directions
          td <- dash_trait_info()
          if (is.null(td)) td <- list()
          trait_directions <- as.list(td)

          # predictions_data
          predictions_data <- tryCatch({
            if (is.na(mta_stamp) || is.null(dt_obj$predictions)) stop("no data")
            preds <- dt_obj$predictions
            cols_to_keep <- intersect(c("designation", "trait", "predictedValue", "reliability"), colnames(preds))
            preds[preds$analysisId == mta_stamp & preds$effectType == "designation", cols_to_keep, drop = FALSE]
          }, error = function(e) data.frame())

          # index_weights and selected_traits
          wt_result <- tryCatch({
            ctx_wt <- meeting_context()
            selected_stamps_val <- if (!is.null(ctx_wt)) ctx_wt$stakeholder_stamps else input$stamp_select
            if (is.null(selected_stamps_val) || length(selected_stamps_val) == 0) stop("no stamps")
            modeling <- dt_obj$modeling
            all_weight_lists <- list()
            all_trait_sets <- list()

            for (stamp_id in selected_stamps_val) {
              init_rows <- modeling[
                modeling$module == "Final_prodAdv" &
                  modeling$analysisId == stamp_id &
                  modeling$parameter == "inputObject",
                , drop = FALSE
              ]
              if (nrow(init_rows) == 0) next
              init_stamp_val <- init_rows$value[1]

              weight_rows <- modeling[
                modeling$analysisId == init_stamp_val & modeling$parameter == "index_weight",
                , drop = FALSE
              ]
              if (nrow(weight_rows) > 0) {
                w <- as.numeric(weight_rows$value)
                names(w) <- weight_rows$trait
                all_weight_lists[[stamp_id]] <- w
              }

              all_tr <- unique(modeling$trait[
                modeling$analysisId == init_stamp_val &
                  modeling$module == "Init_prodAdv" &
                  !is.na(modeling$trait) & nzchar(modeling$trait)
              ])
              excluded_tr <- modeling$trait[
                modeling$analysisId == init_stamp_val &
                  modeling$parameter == "user_excluded_trait"
              ]
              all_trait_sets[[stamp_id]] <- setdiff(all_tr, excluded_tr)
            }

            all_traits_union <- unique(unlist(all_trait_sets))
            n_stk <- length(all_trait_sets)
            sel_traits <- all_traits_union[vapply(all_traits_union, function(tr) {
              n_using <- sum(vapply(all_trait_sets, function(ts) tr %in% ts, logical(1)))
              n_using > n_stk / 2
            }, logical(1))]

            idx_weights <- NULL
            if (length(sel_traits) > 0 && length(all_weight_lists) > 0) {
              idx_weights <- vapply(sel_traits, function(tr) {
                trait_weights <- vapply(all_weight_lists, function(wl) {
                  if (tr %in% names(wl)) wl[[tr]] else 0
                }, numeric(1))
                median(trait_weights)
              }, numeric(1))
              names(idx_weights) <- sel_traits
            }

            list(index_weights = idx_weights, selected_traits = sel_traits)
          }, error = function(e) list(index_weights = NULL, selected_traits = NULL))

          index_weights <- wt_result$index_weights
          selected_traits <- wt_result$selected_traits

          # Use cm for comparison_matrix variable in RData
          comparison_matrix <- cm

          # Save .RData
          tmp_rdata <- file.path(tempdir(), "resultAdvMeeting.RData")
          review_long        <- tryCatch(adv_review_long(), error = function(e) NULL)
          breakdown_detail   <- tryCatch(tpp_breakdown_detail(), error = function(e) NULL)
          stakeholder_tables <- tryCatch(stakeholder_options_tables(), error = function(e) NULL)

          save(
            comparison_matrix, meeting_decisions_df, mta_stamp, participants,
            meeting_name, trait_directions, predictions_data,
            index_weights, selected_traits,
            review_long, breakdown_detail, stakeholder_tables, REVIEW_GROUP_COLORS,
            file = tmp_rdata
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
            writeLines("<html><body><h1>Error</h1><p>Report template not found.</p></body></html>", file)
            return()
          }

          tmp_report <- file.path(tempdir(), "reportAdvMeeting_download.Rmd")
          file.copy(src, tmp_report, overwrite = TRUE)

          # Render
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
          # On any failure, write an error HTML so downloadHandler doesn't get "No file"
          writeLines(
            paste0(
              "<html><body><h1>Report Rendering Error</h1>",
              "<p>An error occurred while generating the meeting dashboard:</p>",
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

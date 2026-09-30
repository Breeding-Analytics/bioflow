#' Decision Table TPP Formatting Utilities
#'
#' @description Utility function for applying TPP border formatting to a DT
#'   datatable. Applies a green inset glow (box-shadow) to cells where
#'   individuals meet TPP criteria, and to the Index cell when all
#'   Essential_Improve criteria are met. Supports toggling Essential_Maintain
#'   columns via the \code{show_maintain} parameter.
#'
#'

#' Apply TPP border formatting to a DT datatable
#'
#' Creates a DT::datatable object from decision table data with conditional
#' border formatting based on the TPP criteria evaluation matrix. Cells where
#' criteria are met receive a green inset glow. The Index
#' cell also receives the border when all Essential_Improve criteria are met
#' for that individual.
#'
#' @param dt_data Data.frame of decision table data with columns: designation,
#'   Index, and trait columns (both Essential_Improve and Essential_Maintain).
#' @param criteria_matrix Logical data.frame from \code{tpp_evaluate_all_criteria}
#'   with columns: designation, then logical columns per evaluated trait.
#' @param essential_improve_traits Character vector of Essential_Improve trait
#'   column names present in \code{dt_data}.
#' @param essential_maintain_traits Character vector of Essential_Maintain trait
#'   column names present in \code{dt_data}.
#' @param show_maintain Logical whether to show Essential_Maintain columns
#'   (default FALSE). When FALSE, Essential_Maintain columns are hidden.
#' @return DT::datatable object with TPP border formatting applied. The table
#'   is sorted by Index descending by default and includes a caption explaining
#'   the bold black border meaning.
#' @noRd
tpp_format_decision_table <- function(dt_data, criteria_matrix,
                                       essential_improve_traits,
                                       essential_maintain_traits,
                                       show_maintain = FALSE) {
  # --- Input validation ---
  if (is.null(dt_data) || !is.data.frame(dt_data) || nrow(dt_data) == 0) {
    # Return a minimal empty datatable
    empty_df <- data.frame(designation = character(0), Index = numeric(0),
                           stringsAsFactors = FALSE)
    return(DT::datatable(empty_df, caption = "No data available."))
  }

  if (is.null(criteria_matrix) || !is.data.frame(criteria_matrix)) {
    criteria_matrix <- data.frame(designation = dt_data$designation,
                                  stringsAsFactors = FALSE)
  }

  if (is.null(essential_improve_traits)) essential_improve_traits <- character(0)
  if (is.null(essential_maintain_traits)) essential_maintain_traits <- character(0)

  # --- Step 1: Determine which columns to display ---
  # Always show: designation, Index, and Essential_Improve trait columns
  base_cols <- c("designation", "Index")
  display_cols <- base_cols

  # Add Essential_Improve traits that exist in dt_data

  ei_cols <- intersect(essential_improve_traits, colnames(dt_data))
  display_cols <- c(display_cols, ei_cols)

  # Conditionally add Essential_Maintain traits
  em_cols <- character(0)
  if (isTRUE(show_maintain)) {
    em_cols <- intersect(essential_maintain_traits, colnames(dt_data))
    display_cols <- c(display_cols, em_cols)
  }

  # --- Step 2: Filter dt_data columns ---
  # Keep only display columns that exist in dt_data
  display_cols <- intersect(display_cols, colnames(dt_data))
  filtered_data <- dt_data[, display_cols, drop = FALSE]

  # --- Step 3: Build the criteria matrix for JavaScript ---
  # Create a boolean matrix aligned with filtered_data rows and columns
  # Rows match by designation; columns match trait names in display order
  trait_cols_in_display <- setdiff(display_cols, c("designation", "Index"))

  # Build a matrix of 0/1 for criteria met
  n_rows <- nrow(filtered_data)
  n_trait_cols <- length(trait_cols_in_display)

  # Also determine which individuals meet ALL Essential_Improve criteria (for Index border)
  all_ei_met <- rep(FALSE, n_rows)

  # Build criteria lookup by designation
  criteria_lookup <- list()
  if (nrow(criteria_matrix) > 0 && "designation" %in% colnames(criteria_matrix)) {
    for (i in seq_len(nrow(criteria_matrix))) {
      desig <- as.character(criteria_matrix$designation[i])
      criteria_lookup[[desig]] <- criteria_matrix[i, , drop = FALSE]
    }
  }

  # Build the boolean matrix as a JSON-compatible structure for JS callback
  # criteria_js_matrix[row][col] = 1 or 0
  # Column order in JS: col 0 = designation, col 1 = Index, col 2.. = traits
  criteria_js_rows <- vector("list", n_rows)

  for (i in seq_len(n_rows)) {
    desig <- as.character(filtered_data$designation[i])
    row_criteria <- criteria_lookup[[desig]]

    # For each trait column in the display, check if criteria is met
    trait_met <- rep(0L, n_trait_cols)
    ei_met_for_individual <- TRUE

    for (j in seq_along(trait_cols_in_display)) {
      trait_name <- trait_cols_in_display[j]
      if (!is.null(row_criteria) && trait_name %in% colnames(row_criteria)) {
        val <- row_criteria[[trait_name]]
        if (!is.null(val) && length(val) == 1 && !is.na(val) && isTRUE(val)) {
          trait_met[j] <- 1L
        }
      }
    }

    # Check if ALL Essential_Improve traits are met for Index border
    for (ei_trait in ei_cols) {
      if (!is.null(row_criteria) && ei_trait %in% colnames(row_criteria)) {
        val <- row_criteria[[ei_trait]]
        if (is.null(val) || length(val) != 1 || is.na(val) || !isTRUE(val)) {
          ei_met_for_individual <- FALSE
        }
      } else {
        # If trait not in criteria_matrix, treat as not met
        ei_met_for_individual <- FALSE
      }
    }

    # Special case: if there are no Essential_Improve traits, no individual
    # gets the Index border (need at least one trait to evaluate)
    if (length(ei_cols) == 0) {
      ei_met_for_individual <- FALSE
    }

    all_ei_met[i] <- ei_met_for_individual
    criteria_js_rows[[i]] <- trait_met
  }

  # Convert to JSON arrays for the JavaScript callback
  criteria_json <- paste0(
    "[",
    paste(vapply(criteria_js_rows, function(row) {
      paste0("[", paste(row, collapse = ","), "]")
    }, character(1)), collapse = ","),
    "]"
  )

  # Index border array (1 per row: 1 if Index should get border, 0 otherwise)
  index_border_json <- paste0("[", paste(as.integer(all_ei_met), collapse = ","), "]")

  # --- Step 4: Build the JavaScript rowCallback ---
  # Column indices in DT (0-based): col 0 = designation, col 1 = Index,
  # col 2 onwards = trait columns
  # The first trait column starts at index 2
  js_callback <- sprintf(
    "function(row, data, displayNum, displayIndex, dataIndex) {
      var critMatrix = %s;
      var indexBorder = %s;
      var firstTraitCol = 2;
      var numTraitCols = %d;

      // Apply border to trait cells where criteria met
      if (dataIndex < critMatrix.length) {
        var rowCrit = critMatrix[dataIndex];
        for (var j = 0; j < numTraitCols; j++) {
          if (rowCrit[j] === 1) {
            $('td:eq(' + (firstTraitCol + j) + ')', row).css({'box-shadow': 'inset 0 0 0 1.5px #333333', 'border-radius': '4px', 'font-weight': '700'});
          }
        }
        // Apply glow to Index cell (col index 1) if all EI criteria met
        if (indexBorder[dataIndex] === 1) {
          $('td:eq(1)', row).css({'box-shadow': 'inset 0 0 0 1.5px #333333', 'border-radius': '4px'});
        }
      }
    }",
    criteria_json, index_border_json, n_trait_cols
  )

  # --- Step 5: Determine default sort order (Index descending) ---
  # Index is at column position 1 (0-based)
  order_option <- list(list(1, "desc"))

  # --- Step 6: Build DT::datatable ---
  # Caption/legend explaining bold black borders
  caption_text <- htmltools::tags$caption(
    style = "caption-side: bottom; text-align: left; font-size: 0.9em; padding-top: 8px;",
    htmltools::tags$span(
      style = "box-shadow: inset 0 0 0 1.5px #333333; border-radius: 4px; padding: 2px 6px; margin-right: 6px;",
      "Cell"
    ),
    " = TPP criteria met (dark border indicates the individual meets the TPP desired score for that trait)"
  )

  dt_obj <- DT::datatable(
    filtered_data,
    caption = caption_text,
    rownames = FALSE,
    options = list(
      order = order_option,
      rowCallback = DT::JS(js_callback),
      pageLength = 25,
      scrollX = TRUE
    ),
    class = "display compact"
  )

  return(dt_obj)
}

#' Validate a TPP ID string
#'
#' Trims leading and trailing whitespace, then validates that the input matches
#' the pattern TPP followed by exactly 5 digits (e.g., TPP00123).
#'
#' @param id_string A character string to validate as a TPP ID.
#'
#' @return TRUE if the trimmed string matches `^TPP[0-9]{5}$`, FALSE otherwise.
#'
#' @noRd
validate_tpp_id <- function(id_string) {
  if (!is.character(id_string) || length(id_string) != 1) {
    return(FALSE)
  }
  trimmed <- trimws(id_string)
  grepl("^TPP[0-9]{5}$", trimmed)
}

#' Standardize a checks column vector
#'
#' Replaces common alternative delimiters (semicolons, forward slashes, pipes,
#' and plus signs) with commas, then trims whitespace around each entry and
#' removes any resulting empty entries. Returns the standardized character vector.
#'
#' @param col_vector A character vector representing the Best_Checks_Column values
#'   from a TPP data.frame.
#'
#' @return A character vector of the same length as `col_vector`, where each element
#'   has been standardized: alternative delimiters replaced with commas, entries
#'   trimmed, and empty entries removed within each cell.
#'
#' @noRd
standardize_checks_column <- function(col_vector) {
  if (!is.character(col_vector)) {
    col_vector <- as.character(col_vector)
  }

  vapply(col_vector, function(cell) {
    # Preserve NA values
    if (is.na(cell) || cell == "NA") {
      return(NA_character_)
    }
    # Replace alternative delimiters (;, /, |, +) with commas
    standardized <- gsub("[;/|\\+]", ",", cell)
    # Split by comma
    entries <- strsplit(standardized, ",", fixed = TRUE)[[1]]
    # Trim whitespace from each entry
    entries <- trimws(entries)
    # Remove empty entries
    entries <- entries[nchar(entries) > 0]
    # If nothing remains, return NA
    if (length(entries) == 0) {
      return(NA_character_)
    }
    # Rejoin with comma separator
    paste(entries, collapse = ", ")
  }, character(1), USE.NAMES = FALSE)
}

#' Parse a TPP file (CSV or Excel)
#'
#' Dispatches to \code{utils::read.csv} for CSV files or
#' \code{readxl::read_excel} for Excel files based on the file extension.
#' Returns a data.frame on success or NULL on any error.
#'
#' @param file_path Character string. Path to the file to parse.
#' @param ext Character string. File extension (e.g., "csv", "xlsx", "xls").
#' @param sep Character string. Field separator for CSV parsing (default ",").
#' @param quote Character string. Quoting character for CSV parsing (default "\"").
#' @param dec Character string. Decimal point character for CSV parsing (default ".").
#'
#' @return A data.frame containing the parsed file contents, or NULL if parsing fails.
#'
#' @noRd
parse_tpp_file <- function(file_path, ext, sep = ",", quote = "\"", dec = ".") {
  tryCatch({
    ext_lower <- tolower(ext)
    if (ext_lower == "csv") {
      df <- utils::read.csv(file_path, sep = sep, quote = quote, dec = dec,
                            stringsAsFactors = FALSE)
    } else if (ext_lower %in% c("xlsx", "xls")) {
      df <- as.data.frame(readxl::read_excel(file_path, sheet = 1))
    } else {
      return(NULL)
    }
    df
  }, error = function(e) {
    NULL
  })
}

#' Get the default checks column name
#'
#' Returns the first column name containing "check" (case-insensitive) from
#' a vector of column names. Returns NULL if no match is found.
#'
#' @param col_names Character vector of column names to search.
#'
#' @return The first column name containing "check" (case-insensitive),
#'   or NULL if no match is found.
#'
#' @noRd
get_default_checks_col <- function(col_names) {
  matches <- grepl("check", col_names, ignore.case = TRUE)
  if (any(matches)) {
    return(col_names[which(matches)[1]])
  }
  NULL
}

#' Check if a single TPP trait is successfully mapped
#'
#' A trait is considered successfully mapped when it has a non-NA pheno_trait
#' AND either: (a) its score_type is "absolute", OR (b) its score_type is
#' "relative" and at least one check for that trait has a non-NA
#' pheno_designation in the checks mapping.
#' Additionally, if a scale_validations list is provided and the trait has
#' an active scale violation, it is NOT considered successfully mapped.
#'
#' @param trait_row A single-row data.frame (or named list) from the traits
#'   mapping, containing at minimum columns: \code{tpp_trait}, \code{pheno_trait},
#'   and \code{score_type}.
#' @param checks_mapping A data.frame of check mappings with columns:
#'   \code{tpp_check}, \code{tpp_trait}, and \code{pheno_designation}.
#' @param scale_validations Optional named list of scale validation results,
#'   keyed by trait name. Each element is a list with at least
#'   \code{is_violation} (logical). If a trait has \code{is_violation = TRUE},
#'   it cannot be successfully mapped.
#'
#' @return TRUE if the trait is successfully mapped, FALSE otherwise.
#'
#' @noRd
is_successfully_mapped_trait <- function(trait_row, checks_mapping, scale_validations = NULL) {
  # Trait must have a non-NA pheno_trait mapping
  if (is.na(trait_row$pheno_trait) || is.null(trait_row$pheno_trait)) {
    return(FALSE)
  }

  # Check for active scale violation

  if (!is.null(scale_validations)) {
    trait_name <- trait_row$tpp_trait
    if (!is.null(scale_validations[[trait_name]]) &&
        isTRUE(scale_validations[[trait_name]]$is_violation)) {
      return(FALSE)
    }
  }

  # If score_type is absolute, no check mapping needed
  if (!is.na(trait_row$score_type) && trait_row$score_type == "absolute") {
    return(TRUE)
  }

  # For relative scores, at least one check must be mapped for this trait
  if (!is.null(checks_mapping) && nrow(checks_mapping) > 0) {
    trait_checks <- checks_mapping[checks_mapping$tpp_trait == trait_row$tpp_trait, , drop = FALSE]
    if (nrow(trait_checks) > 0) {
      has_mapped_check <- any(!is.na(trait_checks$pheno_designation))
      return(has_mapped_check)
    }
  }

  return(FALSE)
}

#' Evaluate overall TPP mapping status
#'
#' Determines whether the TPP mapping is complete by checking that both
#' mapping steps (traits and checks) have been triggered and that at least
#' one trait is successfully mapped. Returns a list with status information
#' suitable for display in the status step UI.
#'
#' @param traits_mapping A data.frame of trait mappings (columns: tpp_trait,
#'   pheno_trait), or NULL if trait mapping step has not been
#'   triggered.
#' @param checks_mapping A data.frame of check mappings (columns: tpp_check,
#'   tpp_trait, pheno_designation), or NULL if check mapping step has not been
#'   triggered.
#' @param tpp_df The stored TPP data.frame, used to retrieve score_type
#'   information for each trait.
#' @param scale_validations Optional named list of scale validation results
#'   (keyed by trait name). Passed through to is_successfully_mapped_trait.
#'
#' @return A list with three elements:
#'   \describe{
#'     \item{complete}{Logical. TRUE if mapping is complete and usable.}
#'     \item{message}{Character. Human-readable status message.}
#'     \item{color}{Character. Either "green" (ready) or "red" (incomplete).}
#'   }
#'
#' @noRd
evaluate_mapping_status <- function(traits_mapping, checks_mapping, tpp_df, scale_validations = NULL) {
  # Check if trait mapping step has been triggered
  if (is.null(traits_mapping)) {
    return(list(
      complete = FALSE,
      message = "Trait mapping not completed",
      color = "red"
    ))
  }

  # Check if check mapping step has been triggered
  if (is.null(checks_mapping)) {
    return(list(
      complete = FALSE,
      message = "Check mapping not completed",
      color = "red"
    ))
  }

  # Both steps triggered — check if at least one trait is successfully mapped
  any_mapped <- FALSE

  if (nrow(traits_mapping) > 0) {
    for (i in seq_len(nrow(traits_mapping))) {
      trait_row <- traits_mapping[i, , drop = FALSE]

      # Determine score_type for this trait from tpp_df
      if (!"score_type" %in% names(trait_row)) {
        # Look up from tpp_df: check "Desired Score" column for "check" keyword
        # If Desired Score text contains "check", it's relative; otherwise absolute
        score_type <- "absolute"  # default
        if (!is.null(tpp_df)) {
          # Find the trait name column
          trait_name_col <- if ("Trait Name" %in% names(tpp_df)) "Trait Name" else names(tpp_df)[1]
          tpp_match <- tpp_df[tpp_df[[trait_name_col]] == trait_row$tpp_trait, , drop = FALSE]
          if (nrow(tpp_match) > 0) {
            # Check if there's a score_type column explicitly
            if ("score_type" %in% names(tpp_df)) {
              score_type <- tpp_match$score_type[1]
            } else if ("Desired Score" %in% names(tpp_df)) {
              # Infer from Desired Score text: contains "check" => relative
              desired <- tolower(as.character(tpp_match[["Desired Score"]][1]))
              if (!is.na(desired) && grepl("check", desired)) {
                score_type <- "relative"
              }
            }
          }
        }
        trait_row$score_type <- score_type
      }

      if (is_successfully_mapped_trait(trait_row, checks_mapping, scale_validations)) {
        any_mapped <- TRUE
        break
      }
    }
  }

  if (any_mapped) {
    return(list(
      complete = TRUE,
      message = "Data is complete. You can proceed to other modules.",
      color = "green"
    ))
  } else {
    return(list(
      complete = FALSE,
      message = "No traits are successfully mapped. At least one trait must be mapped with its required checks (if applicable) for TPP information to be usable.",
      color = "red"
    ))
  }
}

#' getDataTPP UI Function
#'
#' @description A shiny Module for uploading and managing Target Product Profile
#'   (TPP) data. Provides a navlistPanel-based step workflow for loading TPP files,
#'   mapping traits and checks to phenotypic data, and validating mapping completeness.
#'
#' @param id Internal parameter for {shiny}. The module namespace ID.
#'
#' @return A tagList containing the full TPP module UI with a navlistPanel of 4 steps.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_getDataTPP_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    tags$br(),

    navlistPanel("Steps:", widths = c(2, 10),

                 # Step 1: Load data
                 tabPanel(div("1. Load data"),

                          column(width = 8,

                                 # TPP_ID text input
                                 textInput(
                                   inputId = ns('tpp_id'),
                                   label = 'TPP ID*:',
                                   value = '',
                                   width = '300px',
                                   placeholder = 'e.g., TPP00123'
                                 ),

                                 # Inline error message for invalid TPP_ID
                                 tags$div(
                                   id = ns('tpp_id_error'),
                                   style = "display: none;",
                                   tags$span(
                                     style = "color: red; font-size: 12px;",
                                     "Invalid TPP ID. Must be format TPPXXXXX (e.g., TPP00123)"
                                   )
                                 ),

                                 # File input accepting .xlsx, .xls
                                 fileInput(
                                   inputId = ns('tpp_file'),
                                   label = 'Upload TPP file (.xlsx):',
                                   width = '400px',
                                   accept = c('.xlsx', '.xls')
                                 ),

                          ),

                          # Collapsible DT preview box
                          shinydashboard::box(
                            width = 12,
                            status = 'success',
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            collapsed = TRUE,
                            title = "Preview of uploaded data (click on the '+' symbol on the right to view)",
                            hr(),
                            DT::DTOutput(ns('tpp_preview_table'))
                          ),

                 ),

                 # Step 2: Map traits
                 tabPanel(div("2. Map traits"),

                          uiOutput(ns('trait_mapping_ui'))

                 ),

                 # Step 3: Map checks
                 tabPanel(div("3. Map checks"),

                          selectInput(
                            inputId = ns('check_entry_type'),
                            label = 'Check Entry Type:',
                            choices = NULL,
                            width = '300px'
                          ),

                          uiOutput(ns('check_mapping_ui'))

                 ),

                 # Step 4: Status
                 tabPanel(div("4. Status"),

                          uiOutput(ns('status_display'))

                 )

    )
  )
}


#' getDataTPP Server Function
#'
#' Server logic for the TPP data integration module. Handles file upload,
#' parsing, standardization, and storage of Target Product Profile data.
#' Receives the shared \code{data} reactiveVal from the main application.
#'
#' @param id Internal parameter for {shiny}. The module namespace ID.
#' @param data A reactiveVal containing the Bioflow data object (list with
#'   \code{data}, \code{metadata}, \code{modifications}, \code{predictions},
#'   \code{metrics}, \code{modeling}, and \code{status} slots).
#'
#' @noRd
mod_getDataTPP_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Internal reactive state ──────────────────────────────────────────────
    uploaded_df <- reactiveVal(NULL)
    tpp_id_valid <- reactiveVal(FALSE)
    trait_mapping_triggered <- reactiveVal(FALSE)
    check_mapping_triggered <- reactiveVal(FALSE)
    load_validated <- reactiveVal(FALSE)
    scale_violations <- reactiveVal(list())  # named list: trait_name -> list(is_violation, pct, scale_min, scale_max)

    # ── Load handler: validate TPP entries on data object load ────────────────
    observe({
      req(data())
      # Only run once per session to avoid infinite loops
      if (isTRUE(load_validated())) return()

      tpp_data <- data()$data$TPP

      # If no TPP data, nothing to validate
      if (is.null(tpp_data) || length(tpp_data) == 0) {
        load_validated(TRUE)
        return()
      }

      # Validate each TPP entry: must be a valid data.frame with rows
      valid_ids <- character(0)
      invalid_ids <- character(0)

      for (tpp_id in names(tpp_data)) {
        entry <- tpp_data[[tpp_id]]
        if (is.data.frame(entry) && nrow(entry) > 0) {
          valid_ids <- c(valid_ids, tpp_id)
        } else {
          invalid_ids <- c(invalid_ids, tpp_id)
        }
      }

      # Enforce 50 TPP limit: keep only the first 50 valid entries
      if (length(valid_ids) > 50) {
        excess_ids <- valid_ids[51:length(valid_ids)]
        invalid_ids <- c(invalid_ids, excess_ids)
        valid_ids <- valid_ids[1:50]
        shinyWidgets::show_alert(
          title = "TPP limit exceeded",
          text = paste0("Maximum of 50 TPPs supported. The following excess TPP IDs were not loaded: ",
                        paste(excess_ids, collapse = ", ")),
          type = "warning"
        )
      }

      # Alert about invalid entries
      if (length(invalid_ids) > 0) {
        shinyWidgets::show_alert(
          title = "Invalid TPP entries skipped",
          text = paste0("The following TPP IDs could not be loaded (invalid structure): ",
                        paste(invalid_ids, collapse = ", ")),
          type = "warning"
        )

        # Remove invalid entries from data object
        temp <- data()
        for (id in invalid_ids) {
          temp$data$TPP[[id]] <- NULL
        }
        data(temp)
      }

      # If loaded metadata exists for valid TPPs, mark mapping steps as triggered
      if (length(valid_ids) > 0) {
        tpp_meta <- data()$metadata$TPP
        if (!is.null(tpp_meta)) {
          for (vid in valid_ids) {
            if (!is.null(tpp_meta[[vid]]$traits)) {
              trait_mapping_triggered(TRUE)
            }
            if (!is.null(tpp_meta[[vid]]$checks) || !is.null(tpp_meta[[vid]]$checks_per_trait)) {
              check_mapping_triggered(TRUE)
            }
          }
        }
      }

      load_validated(TRUE)
    })

    # ── Step 1: Load Data ────────────────────────────────────────────────────

    # --- TPP_ID validation reactive ---
    observeEvent(input$tpp_id, {
      id_value <- input$tpp_id
      is_valid <- validate_tpp_id(id_value)
      tpp_id_valid(is_valid)

      if (!is_valid && nchar(trimws(id_value)) > 0) {
        # Show inline error
        shinyjs::show(id = "tpp_id_error")
      } else {
        shinyjs::hide(id = "tpp_id_error")
      }

      # Enable/disable file upload based on valid ID
      if (is_valid) {
        shinyjs::enable("tpp_file")
      } else {
        shinyjs::disable("tpp_file")
      }
    }, ignoreNULL = FALSE)

    # --- Duplicate TPP_ID check ---
    observeEvent(input$tpp_file, {
      req(input$tpp_file)
      req(tpp_id_valid())

      tpp_id <- trimws(input$tpp_id)

      # Check for duplicate
      if (!is.null(data()) && !is.null(data()$data$TPP[[tpp_id]])) {
        shinyWidgets::ask_confirmation(
          inputId = ns("confirm_overwrite"),
          title = "TPP already exists",
          text = paste0("A TPP with ID '", tpp_id, "' already exists. Do you want to overwrite it?"),
          type = "warning",
          btn_labels = c("Cancel", "Overwrite"),
          btn_colors = c("#6e7d88", "#3085d6")
        )
      } else {
        # No duplicate — proceed directly with parsing
        process_tpp_file()
      }
    })

    # Handle overwrite confirmation response
    observeEvent(input$confirm_overwrite, {
      if (isTRUE(input$confirm_overwrite)) {
        process_tpp_file()
      }
      # If FALSE (cancelled), do nothing — retain existing data
    })

    # --- File upload: validate extension, parse file ---
    process_tpp_file <- function() {
      req(input$tpp_file)

      file_path <- input$tpp_file$datapath
      file_name <- input$tpp_file$name

      # Get extension
      ext <- tools::file_ext(file_name)
      ext_lower <- tolower(ext)

      # Validate extension (xlsx/xls only)
      if (!ext_lower %in% c("xlsx", "xls")) {
        shinyWidgets::show_alert(
          title = "Unsupported file format",
          text = paste0("The file '", file_name, "' has an unsupported extension '.",
                        ext, "'. Accepted formats: .xlsx, .xls"),
          type = "error"
        )
        return(NULL)
      }

      # Parse the Excel file
      parsed_df <- parse_tpp_file(
        file_path = file_path,
        ext = ext_lower
      )

      # Error handling: parse failure
      if (is.null(parsed_df)) {
        shinyWidgets::show_alert(
          title = "File parsing error",
          text = "The file could not be parsed. Please verify it is a valid Excel file.",
          type = "error"
        )
        return(NULL)
      }

      # Check for empty data (header only, no data rows)
      if (nrow(parsed_df) == 0) {
        shinyWidgets::show_alert(
          title = "Empty file",
          text = "The file contains only a header row with no data rows.",
          type = "error"
        )
        return(NULL)
      }

      # Store parsed data in reactive
      uploaded_df(parsed_df)

      # Auto-detect Best Checks column and standardize
      col_names <- names(parsed_df)
      checks_col <- get_default_checks_col(col_names)

      # Standardize and store
      tpp_id <- trimws(input$tpp_id)
      standardized_df <- parsed_df
      if (!is.null(checks_col)) {
        standardized_df[[checks_col]] <- standardize_checks_column(standardized_df[[checks_col]])
      }

      # Store standardized data at data()$data$TPP$TPPXXXXX
      temp <- data()
      if (is.null(temp)) {
        temp <- list(data = list(), metadata = list())
      }
      if (is.null(temp$data$TPP)) {
        temp$data$TPP <- list()
      }

      # Enforce 50 TPP limit
      existing_ids <- names(temp$data$TPP)
      if (!tpp_id %in% existing_ids && length(existing_ids) >= 50) {
        shinyWidgets::show_alert(
          title = "TPP limit reached",
          text = "Maximum of 50 TPPs supported.",
          type = "warning"
        )
        return(NULL)
      }

      temp$data$TPP[[tpp_id]] <- standardized_df
      data(temp)
    }

    # --- DT preview: render uploaded data in collapsible box ---
    output$tpp_preview_table <- DT::renderDT({
      req(uploaded_df())
      DT::datatable(
        uploaded_df(),
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    }, server = FALSE)

    # ── Step 2: Map Traits ───────────────────────────────────────────────────

    # Track which TPP_ID has been rendered to avoid re-rendering on data() writes
    rendered_tpp_id <- reactiveVal(NULL)

    # Only re-render trait mapping UI when a NEW TPP is stored (not on mapping changes)
    observe({
      req(data())
      tpp_id <- trimws(input$tpp_id)
      req(tpp_id)
      req(data()$data$TPP[[tpp_id]])
      # Only trigger if this is a different TPP than what was last rendered
      if (!identical(rendered_tpp_id(), tpp_id)) {
        rendered_tpp_id(tpp_id)
      }
    })

    output$trait_mapping_ui <- renderUI({
      # Only re-render when rendered_tpp_id changes (not on every data() write)
      tpp_id <- rendered_tpp_id()
      req(tpp_id)

      # Use isolate to read data without creating a reactive dependency
      tpp_data <- isolate(data()$data$TPP[[tpp_id]])
      req(tpp_data)

      # Get trait names from "Trait Name" column (column 3 in standard TPP format)
      trait_name_col <- if ("Trait Name" %in% names(tpp_data)) "Trait Name" else names(tpp_data)[1]
      trait_names <- tpp_data[[trait_name_col]]

      # Get phenotypic metadata (isolate to avoid re-render on mapping storage)
      pheno_meta <- isolate(data()$metadata$pheno)
      if (is.null(pheno_meta) || nrow(pheno_meta) == 0) {
        return(tagList(
          tags$div(class = "alert alert-info",
            icon("info-circle"),
            "Phenotypic data with mapped traits must be loaded before trait mapping can proceed."
          )
        ))
      }

      # Filter for traits only (parameter == "trait")
      trait_cols <- pheno_meta[pheno_meta$parameter == "trait", "value"]

      if (length(trait_cols) == 0) {
        return(tagList(
          tags$div(class = "alert alert-info",
            icon("info-circle"),
            "No traits have been mapped in the phenotypic data. Please map traits in the Phenotypic module first."
          )
        ))
      }

      # Get environment factor columns — these are the columns that compose the environment
      # Exclude: trait, designation, entryType, environment (the combined one), and non-factor cols
      env_forming_params <- c("pipeline", "stage", "year", "season", "timepoint",
                              "country", "location", "trial", "study", "management")
      factor_params <- intersect(env_forming_params, pheno_meta$parameter)
      env_factor_rows <- pheno_meta[pheno_meta$parameter %in% factor_params, , drop = FALSE]
      # If none of those exist, fall back to any column that's not trait/designation/entryType/environment
      if (nrow(env_factor_rows) == 0) {
        excluded_params <- c("trait", "designation", "entryType", "environment",
                             "rep", "row", "col", "latitude", "longitude",
                             "plantingDate", "harvestingDate")
        env_factor_rows <- pheno_meta[!pheno_meta$parameter %in% excluded_params, , drop = FALSE]
      }

      # Get Trait IDs if available (for display alongside trait name)
      trait_id_col <- if ("Trait ID" %in% names(tpp_data)) "Trait ID" else NULL
      trait_ids <- if (!is.null(trait_id_col)) tpp_data[[trait_id_col]] else rep("", length(trait_names))

      # Get phenotypic data for environment values (isolate)
      pheno_data <- isolate(data()$data$pheno)

      # Build UI for each trait
      trait_ui_list <- lapply(seq_along(trait_names), function(i) {
        trait_name <- trait_names[i]
        trait_id <- trait_ids[i]

        # Display label: "TR-00001 — Grain Yield"
        display_label <- if (nchar(trait_id) > 0) {
          paste0(trait_id, " \u2014 ", trait_name)
        } else {
          trait_name
        }

        # Trait mapping row: Trait ID + Name label + dropdown to select matching phenotypic trait
        trait_row <- fluidRow(
          column(4,
            tags$label(
              style = "font-weight: bold; font-size: 14px; padding-top: 8px;",
              display_label
            )
          ),
          column(4,
            selectInput(
              inputId = ns(paste0('trait_map_', i)),
              label = NULL,
              choices = c("-- Select matching phenotypic trait --" = "", trait_cols),
              selected = "",
              width = "100%"
            )
          ),
          column(4,
            # Checkbox to reveal environment filters (hidden by default)
            checkboxInput(
              inputId = ns(paste0('env_filter_toggle_', i)),
              label = "Filter environments",
              value = FALSE
            )
          )
        )

        # Environment filter panel (only visible when checkbox is checked)
        env_filter_panel <- conditionalPanel(
          condition = paste0("input['", ns(paste0('env_filter_toggle_', i)), "']"),
          tags$div(
            style = "background: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 10px; margin-left: 15px;",
            tags$p(style = "font-size: 12px; color: #555;",
              "Filter by environment factors. Deselect values to exclude environments containing them."
            ),
            fluidRow(
              lapply(seq_len(nrow(env_factor_rows)), function(j) {
                factor_col <- env_factor_rows$value[j]
                factor_label <- env_factor_rows$parameter[j]
                # Get unique values for this factor from the phenotypic data
                factor_values <- if (!is.null(pheno_data) && factor_col %in% names(pheno_data)) {
                  sort(unique(as.character(pheno_data[[factor_col]])))
                } else {
                  character(0)
                }

                column(4,
                  selectizeInput(
                    inputId = ns(paste0('env_', i, '_', j)),
                    label = paste0(factor_label, " (", factor_col, ")"),
                    choices = factor_values,
                    selected = factor_values,  # All selected by default
                    multiple = TRUE,
                    width = "100%",
                    options = list(plugins = list("remove_button"))
                  )
                )
              })
            ),
            tags$hr(style = "margin-top: 10px; margin-bottom: 10px;"),
            tags$p(style = "font-size: 12px; color: #555; font-weight: bold;",
              "Environments kept (after subfactor filtering). You can further exclude specific environments below."
            ),
            uiOutput(ns(paste0('env_kept_', i)))
          )
        )

        # Scale violation warning output (rendered dynamically)
        scale_warning_ui <- uiOutput(ns(paste0('scale_warning_', i)))

        tagList(trait_row, scale_warning_ui, env_filter_panel, tags$hr())
      })

      tagList(
        tags$div(class = "alert alert-info", style = "margin-bottom: 15px;",
          icon("info-circle"),
          " For each TPP trait below, select the corresponding trait column from your phenotypic data."
        ),
        do.call(tagList, trait_ui_list)
      )
    })

    # --- Dynamic "Environments kept" renderers for each trait ---
    observe({
      tpp_id <- rendered_tpp_id()
      req(tpp_id)

      tpp_data <- isolate(data()$data$TPP[[tpp_id]])
      req(tpp_data)

      trait_name_col <- if ("Trait Name" %in% names(tpp_data)) "Trait Name" else names(tpp_data)[1]
      n_traits <- length(tpp_data[[trait_name_col]])

      pheno_meta <- isolate(data()$metadata$pheno)
      pheno_data <- isolate(data()$data$pheno)

      # Get environment column info
      env_col_name <- NULL
      all_env_levels <- character(0)
      if (!is.null(pheno_meta)) {
        env_row <- pheno_meta[pheno_meta$parameter == "environment", , drop = FALSE]
        if (nrow(env_row) > 0) {
          env_col_name <- env_row$value[1]
          if (!is.null(pheno_data) && env_col_name %in% names(pheno_data)) {
            all_env_levels <- sort(unique(as.character(pheno_data[[env_col_name]])))
          }
        }
      }

      # Get environment factor rows
      env_forming_params <- c("pipeline", "stage", "year", "season", "timepoint",
                              "country", "location", "trial", "study", "management")
      factor_params <- intersect(env_forming_params, pheno_meta$parameter)
      env_factor_rows <- if (!is.null(pheno_meta)) {
        rows <- pheno_meta[pheno_meta$parameter %in% factor_params, , drop = FALSE]
        if (nrow(rows) == 0) {
          excluded_params <- c("trait", "designation", "entryType", "environment",
                               "rep", "row", "col", "latitude", "longitude",
                               "plantingDate", "harvestingDate")
          pheno_meta[!pheno_meta$parameter %in% excluded_params, , drop = FALSE]
        } else {
          rows
        }
      } else {
        data.frame(parameter = character(0), value = character(0), stringsAsFactors = FALSE)
      }

      # Create a renderer for each trait's env_kept output
      lapply(seq_len(n_traits), function(i) {
        local({
          local_i <- i
          output[[paste0('env_kept_', local_i)]] <- renderUI({
            # Depend on subfactor inputs to reactively update
            subfactor_selected <- lapply(seq_len(nrow(env_factor_rows)), function(j) {
              input[[paste0('env_', local_i, '_', j)]]
            })

            # Filter phenotypic data by selected subfactor levels
            filtered_data <- pheno_data
            if (!is.null(filtered_data) && nrow(env_factor_rows) > 0) {
              for (j in seq_len(nrow(env_factor_rows))) {
                factor_col <- env_factor_rows$value[j]
                selected_levels <- subfactor_selected[[j]]
                if (!is.null(selected_levels) && factor_col %in% names(filtered_data)) {
                  filtered_data <- filtered_data[
                    as.character(filtered_data[[factor_col]]) %in% selected_levels, , drop = FALSE
                  ]
                }
              }
            }

            # Get the remaining environment levels after subfactor filtering
            remaining_envs <- if (!is.null(env_col_name) && !is.null(filtered_data) &&
                                  env_col_name %in% names(filtered_data)) {
              sort(unique(as.character(filtered_data[[env_col_name]])))
            } else {
              all_env_levels
            }

            if (length(remaining_envs) == 0) {
              return(tags$div(
                style = "color: #dc3545; font-style: italic;",
                "No environments remain after subfactor filtering."
              ))
            }

            tagList(
              selectizeInput(
                inputId = ns(paste0('env_kept_select_', local_i)),
                label = paste0("Environments (", length(remaining_envs), " available)"),
                choices = remaining_envs,
                selected = remaining_envs,
                multiple = TRUE,
                width = "100%",
                options = list(plugins = list("remove_button"))
              ),
              tags$p(style = "font-size: 11px; color: #777;",
                paste0(length(remaining_envs), " environment(s) available. Deselect to exclude specific environments.")
              )
            )
          })
        })
      })
    })

    # --- Scale violation warning renderers for each trait ---
    observe({
      tpp_id <- rendered_tpp_id()
      req(tpp_id)

      tpp_data <- isolate(data()$data$TPP[[tpp_id]])
      req(tpp_data)

      trait_name_col <- if ("Trait Name" %in% names(tpp_data)) "Trait Name" else names(tpp_data)[1]
      n_traits <- length(tpp_data[[trait_name_col]])
      trait_names_local <- tpp_data[[trait_name_col]]

      # Create a renderer for each trait's scale_warning output
      lapply(seq_len(n_traits), function(i) {
        local({
          local_i <- i
          local_trait_name <- trait_names_local[local_i]
          output[[paste0('scale_warning_', local_i)]] <- renderUI({
            # React to scale_violations changes
            violations <- scale_violations()

            violation_info <- violations[[local_trait_name]]
            if (!is.null(violation_info) && isTRUE(violation_info$is_violation)) {
              # Format warning message using the helper
              warning_msg <- tpp_format_scale_warning(
                trait_name = local_trait_name,
                scale_min = violation_info$scale_min,
                scale_max = violation_info$scale_max,
                pct_outside = violation_info$pct
              )

              tags$div(
                class = "alert alert-warning",
                style = "margin-top: 5px; margin-bottom: 5px; padding: 8px 12px; font-size: 13px;",
                icon("exclamation-triangle"),
                tags$strong(" Scale Warning: "),
                warning_msg,
                tags$br(),
                tags$span(
                  style = "font-size: 12px; color: #856404;",
                  "You must choose another phenotypic column, leave the trait unmapped, or update the TPP information in the breeding portal."
                )
              )
            } else {
              NULL
            }
          })
        })
      })
    })

    # --- Trait mapping storage reactive ---
    observe({
      # This observer fires when any trait_map_* input changes
      tpp_id <- rendered_tpp_id()
      req(tpp_id)

      # Use isolate to read TPP data without creating circular dependency
      tpp_data <- isolate(data()$data$TPP[[tpp_id]])
      req(tpp_data)

      trait_name_col <- if ("Trait Name" %in% names(tpp_data)) "Trait Name" else names(tpp_data)[1]
      trait_names <- tpp_data[[trait_name_col]]
      n_traits <- length(trait_names)

      # Get environment factors info (isolate)
      pheno_meta <- isolate(data()$metadata$pheno)
      env_forming_params <- c("pipeline", "stage", "year", "season", "timepoint",
                              "country", "location", "trial", "study", "management")
      factor_params <- intersect(env_forming_params, pheno_meta$parameter)
      env_factor_rows <- if (!is.null(pheno_meta)) {
        rows <- pheno_meta[pheno_meta$parameter %in% factor_params, , drop = FALSE]
        if (nrow(rows) == 0) {
          excluded_params <- c("trait", "designation", "entryType", "environment",
                               "rep", "row", "col", "latitude", "longitude",
                               "plantingDate", "harvestingDate")
          pheno_meta[!pheno_meta$parameter %in% excluded_params, , drop = FALSE]
        } else {
          rows
        }
      } else {
        data.frame(parameter = character(0), value = character(0), stringsAsFactors = FALSE)
      }

      # Get all environment levels and the environment column name
      env_col_name <- NULL
      all_env_levels <- character(0)
      pheno_data <- isolate(data()$data$pheno)
      if (!is.null(pheno_meta)) {
        env_row <- pheno_meta[pheno_meta$parameter == "environment", , drop = FALSE]
        if (nrow(env_row) > 0) {
          env_col_name <- env_row$value[1]
          if (!is.null(pheno_data) && env_col_name %in% names(pheno_data)) {
            all_env_levels <- sort(unique(as.character(pheno_data[[env_col_name]])))
          }
        }
      }

      # Read all trait mapping inputs
      traits_list <- lapply(seq_len(n_traits), function(i) {
        pheno_trait <- input[[paste0('trait_map_', i)]]
        if (is.null(pheno_trait) || pheno_trait == "") {
          pheno_trait <- NA_character_
        }

        # Determine environments kept for this trait
        env_toggle <- input[[paste0('env_filter_toggle_', i)]]
        envs_kept <- all_env_levels  # default: all environments

        if (isTRUE(env_toggle) && !is.null(pheno_data) && nrow(env_factor_rows) > 0) {
          # First: apply subfactor filtering to narrow down environments
          subfactor_filtered <- pheno_data

          for (j in seq_len(nrow(env_factor_rows))) {
            factor_col <- env_factor_rows$value[j]
            selected_levels <- input[[paste0('env_', i, '_', j)]]
            if (!is.null(selected_levels) && factor_col %in% names(subfactor_filtered)) {
              subfactor_filtered <- subfactor_filtered[
                as.character(subfactor_filtered[[factor_col]]) %in% selected_levels, , drop = FALSE
              ]
            }
          }

          # Get environment levels surviving subfactor filtering
          if (!is.null(env_col_name) && env_col_name %in% names(subfactor_filtered)) {
            subfactor_envs <- sort(unique(as.character(subfactor_filtered[[env_col_name]])))
          } else {
            subfactor_envs <- all_env_levels
          }

          # Then: apply the explicit env_kept selection (user can further exclude)
          env_kept_selection <- input[[paste0('env_kept_select_', i)]]
          if (!is.null(env_kept_selection)) {
            # The user's selection within the subfactor-filtered list
            envs_kept <- intersect(env_kept_selection, subfactor_envs)
          } else {
            envs_kept <- subfactor_envs
          }
        }

        list(
          tpp_trait = trait_names[i],
          pheno_trait = pheno_trait,
          environments_kept = envs_kept
        )
      })

      # Only proceed if the dynamic inputs have been rendered
      # (input$trait_map_1 will be NULL until the renderUI flushes)
      if (is.null(input[[paste0('trait_map_', 1)]])) return(NULL)

      # ── Scale validation logic ──────────────────────────────────────────────
      # Parse "Scale Option" column for numeric ranges (e.g. "1 to 5", "1-9")
      new_violations <- list()

      has_scale_option <- "Scale Option" %in% names(tpp_data)

      if (has_scale_option) {
        for (i in seq_len(n_traits)) {
          trait_name <- trait_names[i]
          pheno_trait <- traits_list[[i]]$pheno_trait

          if (!is.na(pheno_trait) && !is.null(pheno_data) && pheno_trait %in% names(pheno_data)) {
            scale_text <- tpp_data[["Scale Option"]][i]
            parsed <- tpp_parse_scale_option(scale_text)

            if (!is.null(parsed)) {
              scale_min_val <- parsed$min
              scale_max_val <- parsed$max

              pheno_values <- pheno_data[[pheno_trait]]

              # Compute violation percentage using the helper
              violation_pct <- tryCatch(
                tpp_compute_scale_violation_pct(pheno_values, scale_min_val, scale_max_val),
                error = function(e) 0
              )

              has_violation <- tpp_has_scale_violation(pheno_values, scale_min_val, scale_max_val)

              new_violations[[trait_name]] <- list(
                is_violation = has_violation,
                pct = violation_pct,
                scale_min = scale_min_val,
                scale_max = scale_max_val
              )
            }
          }
        }
      }

      # Update the scale_violations reactiveVal (only if changed to avoid loops)
      if (!identical(isolate(scale_violations()), new_violations)) {
        scale_violations(new_violations)
      }

      # Store scale validations in metadata
      # ── End scale validation logic ──────────────────────────────────────────

      # Mark trait mapping as triggered
      trait_mapping_triggered(TRUE)

      # Build traits data.frame (without environment info — that goes in env_filters)
      traits_df <- data.frame(
        tpp_trait = sapply(traits_list, `[[`, "tpp_trait"),
        pheno_trait = sapply(traits_list, `[[`, "pheno_trait"),
        stringsAsFactors = FALSE
      )

      # Build env_filters: a named list keyed by Trait ID, only for traits
      # where the user has actually filtered something out (not all envs kept)
      trait_id_col <- if ("Trait ID" %in% names(tpp_data)) "Trait ID" else NULL
      trait_ids <- if (!is.null(trait_id_col)) tpp_data[[trait_id_col]] else trait_names

      env_filters <- list()
      for (i in seq_len(n_traits)) {
        envs_kept <- traits_list[[i]]$environments_kept
        # Only store if something was actually filtered out
        if (!setequal(envs_kept, all_env_levels)) {
          env_filters[[trait_ids[i]]] <- envs_kept
        }
      }
      # If no trait has filtering, store NULL (no unnecessary data)
      if (length(env_filters) == 0) env_filters <- NULL

      # Store at data()$metadata$TPP$TPPXXXXX$traits and $env_filters (only if changed)
      temp <- isolate(data())
      if (is.null(temp$metadata$TPP)) {
        temp$metadata$TPP <- list()
      }
      if (is.null(temp$metadata$TPP[[tpp_id]])) {
        temp$metadata$TPP[[tpp_id]] <- list()
      }
      # Only write if mapping has actually changed (avoid triggering other observers)
      existing_traits <- temp$metadata$TPP[[tpp_id]]$traits
      existing_env_filters <- temp$metadata$TPP[[tpp_id]]$env_filters
      existing_scale_validations <- temp$metadata$TPP[[tpp_id]]$scale_validations
      traits_changed <- !identical(existing_traits, traits_df)
      env_filters_changed <- !identical(existing_env_filters, env_filters)
      scale_validations_changed <- !identical(existing_scale_validations, new_violations)
      if (traits_changed || env_filters_changed || scale_validations_changed) {
        temp$metadata$TPP[[tpp_id]]$traits <- traits_df
        temp$metadata$TPP[[tpp_id]]$env_filters <- env_filters
        temp$metadata$TPP[[tpp_id]]$scale_validations <- new_violations
        data(temp)
      }
    })

    # ── Step 3: Map Checks (Multi-Check Selection) ──────────────────────────

    # Reactive to store traits requiring checks (for use in storage logic)
    traits_requiring_checks_rv <- reactiveVal(NULL)

    # Populate check entry type dropdown
    # Track last known entry types to avoid redundant updates
    last_entry_types <- reactiveVal(NULL)

    observe({
      req(data())
      pheno_meta <- data()$metadata$pheno

      # Find entry type column (parameter == "entryType")
      entry_type_row <- pheno_meta[pheno_meta$parameter == "entryType", ]

      if (is.null(entry_type_row) || nrow(entry_type_row) == 0) {
        # Entry_Type_Column not mapped — disable controls
        if (!is.null(isolate(last_entry_types()))) {
          shinyjs::disable("check_entry_type")
          last_entry_types(NULL)
        }
        return()
      }

      entry_type_col <- entry_type_row$value[1]
      pheno_data <- data()$data$pheno

      if (!is.null(pheno_data) && entry_type_col %in% names(pheno_data)) {
        entry_types <- sort(unique(as.character(pheno_data[[entry_type_col]])))
        # Only update if entry types actually changed
        if (!identical(entry_types, isolate(last_entry_types()))) {
          updateSelectInput(session, 'check_entry_type', choices = c("Select..." = "", entry_types))
          shinyjs::enable("check_entry_type")
          last_entry_types(entry_types)
        }
      } else {
        if (!is.null(isolate(last_entry_types()))) {
          shinyjs::disable("check_entry_type")
          last_entry_types(NULL)
        }
      }
    })

    # Render multi-check selection UI (one multi-select per trait)
    output$check_mapping_ui <- renderUI({
      # Only depend on input changes, not on data() writes from this module
      req(input$check_entry_type)
      req(input$check_entry_type != "")

      # Re-render when trait mapping changes (so unmapped traits are excluded)
      trait_mapping_triggered()

      tpp_id <- trimws(input$tpp_id)
      req(tpp_id)

      # Use isolate for data() reads to avoid re-render on every data() write
      pheno_meta <- isolate(data()$metadata$pheno)
      entry_type_row <- if (!is.null(pheno_meta)) {
        pheno_meta[pheno_meta$parameter == "entryType", ]
      } else {
        data.frame()
      }

      if (is.null(entry_type_row) || nrow(entry_type_row) == 0) {
        return(tags$div(class = "alert alert-info",
          icon("info-circle"),
          "The entry type column must be mapped in the phenotypic data before check mapping can proceed."
        ))
      }

      tpp_data <- isolate(data()$data$TPP[[tpp_id]])
      req(tpp_data)

      # Get the Best Checks column (auto-detected)
      checks_col_name <- get_default_checks_col(names(tpp_data))
      if (is.null(checks_col_name)) {
        return(tags$div(class = "alert alert-warning",
          icon("exclamation-triangle"),
          "No 'Best Checks' column found in the TPP data."
        ))
      }

      # Get trait names from "Trait Name" column
      trait_name_col <- if ("Trait Name" %in% names(tpp_data)) "Trait Name" else names(tpp_data)[1]
      trait_names <- tpp_data[[trait_name_col]]
      checks_values <- tpp_data[[checks_col_name]]

      # Get Desired Score column for inferring relative score type
      desired_score_values <- if ("Desired Score" %in% names(tpp_data)) {
        tpp_data[["Desired Score"]]
      } else {
        rep(NA_character_, length(trait_names))
      }

      # Get current trait mapping to filter out unmapped traits
      tpp_meta <- isolate(data()$metadata$TPP[[tpp_id]])
      traits_mapping <- if (!is.null(tpp_meta$traits)) tpp_meta$traits else NULL

      # Identify traits that require check references:
      # A trait needs checks if EITHER:
      #   (a) its Best Checks column is non-empty (TPP suggests reference genotypes), OR
      #   (b) its Desired Score text contains "check" (relative score type)
      # Additionally, the trait must be mapped to a phenotypic column.
      traits_with_checks <- list()

      for (i in seq_along(trait_names)) {
        trait_name <- trait_names[i]

        # Check if this trait is mapped to a phenotypic column
        is_mapped <- FALSE
        if (!is.null(traits_mapping) && nrow(traits_mapping) > 0) {
          match_row <- traits_mapping[traits_mapping$tpp_trait == trait_name, , drop = FALSE]
          if (nrow(match_row) > 0 && !is.na(match_row$pheno_trait[1])) {
            is_mapped <- TRUE
          }
        }

        if (!is_mapped) next

        # Determine if trait requires checks: Best Checks filled OR Desired Score mentions "check"
        has_checks_col <- !is.na(checks_values[i]) && nchar(trimws(checks_values[i])) > 0
        desired_mentions_check <- !is.na(desired_score_values[i]) &&
          grepl("check", desired_score_values[i], ignore.case = TRUE)

        if (has_checks_col || desired_mentions_check) {
          # Parse suggested check names from the Best Checks column (if available)
          checks <- character(0)
          if (has_checks_col) {
            checks <- trimws(strsplit(checks_values[i], ",")[[1]])
            checks <- checks[nchar(checks) > 0 & checks != "NA"]
          }
          traits_with_checks[[trait_name]] <- unique(checks)
        }
      }

      # Store for downstream use in storage observer
      traits_requiring_checks_rv(traits_with_checks)

      if (length(traits_with_checks) == 0) {
        return(tags$div(class = "alert alert-info", "No traits requiring check references found in the TPP data."))
      }

      # Get designation values matching the selected entry type
      entry_type_col <- entry_type_row$value[1]

      desig_row <- pheno_meta[pheno_meta$parameter == "designation", ]
      desig_col <- if (nrow(desig_row) > 0) desig_row$value[1] else NULL

      pheno_data <- isolate(data()$data$pheno)

      if (is.null(desig_col) || !desig_col %in% names(pheno_data)) {
        return(tags$div(class = "alert alert-warning",
          icon("exclamation-triangle"),
          "Designation column not mapped in phenotypic data."
        ))
      }

      # Filter pheno data for the selected entry type
      matching_entries <- pheno_data[pheno_data[[entry_type_col]] == input$check_entry_type, ]
      designations <- sort(unique(as.character(matching_entries[[desig_col]])))

      # Requirement 2.5: If no designations found, show warning and disable dropdowns
      no_designations <- length(designations) == 0

      if (no_designations) {
        warning_msg <- tags$div(class = "alert alert-warning",
          icon("exclamation-triangle"),
          paste0("No check entries were found for entry type '", input$check_entry_type,
                 "'. Check selection dropdowns are disabled until a valid entry type is selected.")
        )
      } else {
        warning_msg <- NULL
      }

      # Retrieve stored checks_per_trait from metadata (if previously saved)
      stored_checks_per_trait <- NULL
      tpp_meta <- isolate(data()$metadata$TPP[[tpp_id]])
      if (!is.null(tpp_meta$checks_per_trait)) {
        stored_checks_per_trait <- tpp_meta$checks_per_trait
      }

      # Build UI: one multi-select dropdown PER TRAIT
      trait_names_with_checks <- names(traits_with_checks)
      check_ui_list <- lapply(seq_along(trait_names_with_checks), function(i) {
        trait_name <- trait_names_with_checks[i]
        associated_checks <- traits_with_checks[[trait_name]]

        # Determine pre-selected values from stored metadata
        stored_selections <- NULL
        if (!is.null(stored_checks_per_trait) && trait_name %in% names(stored_checks_per_trait)) {
          stored_selections <- stored_checks_per_trait[[trait_name]]
          # Filter out stale selections not in current designations
          if (!no_designations) {
            stored_selections <- intersect(stored_selections, designations)
          }
        }

        # Build the trait info display
        checks_label <- if (length(associated_checks) > 0) {
          paste0("TPP checks: ", paste(associated_checks, collapse = ", "))
        } else {
          "Desired score references a check (select below)"
        }

        trait_info <- tags$div(
          style = "margin-bottom: 5px;",
          tags$span(style = "font-weight: bold; font-size: 14px;", trait_name),
          tags$br(),
          tags$span(style = "font-size: 12px; color: #666;", checks_label)
        )

        # Multi-select dropdown
        if (no_designations) {
          # Disabled state when no designations available
          dropdown <- tags$div(
            selectizeInput(
              inputId = ns(paste0('check_multi_', i)),
              label = NULL,
              choices = character(0),
              selected = NULL,
              multiple = TRUE,
              width = "100%",
              options = list(
                placeholder = "No designations available",
                plugins = list("remove_button")
              )
            ),
            tags$script(
              sprintf("$('#%s').prop('disabled', true);",
                      ns(paste0('check_multi_', i)))
            )
          )
        } else {
          dropdown <- selectizeInput(
            inputId = ns(paste0('check_multi_', i)),
            label = NULL,
            choices = designations,
            selected = stored_selections,
            multiple = TRUE,
            width = "100%",
            options = list(
              placeholder = "Select 1\u201320 check designations...",
              maxItems = 20,
              plugins = list("remove_button")
            )
          )
        }

        # Validation message placeholder
        validation_msg <- uiOutput(ns(paste0('check_validation_', i)))

        fluidRow(
          column(4, trait_info),
          column(6, dropdown),
          column(2, validation_msg)
        )
      })

      tagList(
        tags$div(class = "alert alert-info", style = "margin-bottom: 15px;",
          icon("info-circle"),
          " For each trait requiring check references, select 1\u201320 check designations from the phenotypic data. ",
          "The available designations are filtered by the selected check entry type."
        ),
        warning_msg,
        # Column headers
        fluidRow(
          column(4, tags$label(style = "font-weight: bold; color: #555;", "TPP Trait & Associated Checks")),
          column(6, tags$label(style = "font-weight: bold; color: #555;", "Selected Check Designations (1\u201320)")),
          column(2, tags$label(style = "font-weight: bold; color: #555;", "Status"))
        ),
        tags$hr(style = "margin-top: 5px; margin-bottom: 10px;"),
        do.call(tagList, check_ui_list)
      )
    })

    # --- Validation message renderers for each trait's check count ---
    observe({
      traits_with_checks <- traits_requiring_checks_rv()
      req(traits_with_checks)
      req(length(traits_with_checks) > 0)

      n_traits_checks <- length(traits_with_checks)

      lapply(seq_len(n_traits_checks), function(i) {
        local({
          local_i <- i
          output[[paste0('check_validation_', local_i)]] <- renderUI({
            selected <- input[[paste0('check_multi_', local_i)]]
            n_selected <- length(selected)

            if (n_selected == 0) {
              return(tags$span(
                style = "color: #dc3545; font-size: 12px;",
                icon("exclamation-circle"),
                "Min 1 required"
              ))
            } else if (n_selected > 20) {
              return(tags$span(
                style = "color: #dc3545; font-size: 12px;",
                icon("exclamation-circle"),
                "Max 20 allowed"
              ))
            } else {
              return(tags$span(
                style = "color: #28a745; font-size: 12px;",
                icon("check-circle"),
                paste0(n_selected, " selected")
              ))
            }
          })
        })
      })
    })

    # --- Multi-check storage reactive ---
    observe({
      traits_with_checks <- traits_requiring_checks_rv()
      req(traits_with_checks)
      req(length(traits_with_checks) > 0)

      tpp_id <- trimws(input$tpp_id)
      req(tpp_id)

      n_traits_checks <- length(traits_with_checks)
      trait_names_with_checks <- names(traits_with_checks)

      # Only proceed if at least one input has been rendered
      any_input_exists <- any(sapply(seq_len(n_traits_checks), function(i) {
        !is.null(input[[paste0('check_multi_', i)]])
      }))

      if (!any_input_exists) return(NULL)

      # Mark check mapping as triggered
      check_mapping_triggered(TRUE)

      # Build checks_per_trait: named list keyed by trait name,
      # value is character vector of selected designations
      checks_per_trait <- list()
      for (i in seq_len(n_traits_checks)) {
        selected <- input[[paste0('check_multi_', i)]]
        trait_name <- trait_names_with_checks[i]
        if (!is.null(selected) && length(selected) > 0) {
          # Enforce 1-20 range: only store valid selections
          if (length(selected) >= 1 && length(selected) <= 20) {
            checks_per_trait[[trait_name]] <- selected
          }
        }
      }

      # Also build backward-compatible $checks data.frame
      # Each selected designation for a trait generates rows for all
      # associated check names in that trait
      checks_rows <- list()
      for (i in seq_len(n_traits_checks)) {
        trait_name <- trait_names_with_checks[i]
        associated_check_names <- traits_with_checks[[trait_name]]
        selected <- input[[paste0('check_multi_', i)]]
        if (!is.null(selected) && length(selected) > 0) {
          for (check_name in associated_check_names) {
            for (desig in selected) {
              checks_rows <- c(checks_rows, list(data.frame(
                tpp_check = check_name,
                tpp_trait = trait_name,
                pheno_designation = desig,
                stringsAsFactors = FALSE
              )))
            }
          }
        } else {
          # Add unmapped entries for this trait's checks
          for (check_name in associated_check_names) {
            checks_rows <- c(checks_rows, list(data.frame(
              tpp_check = check_name,
              tpp_trait = trait_name,
              pheno_designation = NA_character_,
              stringsAsFactors = FALSE
            )))
          }
        }
      }

      checks_df <- if (length(checks_rows) > 0) {
        do.call(rbind, checks_rows)
      } else {
        data.frame(
          tpp_check = character(0),
          tpp_trait = character(0),
          pheno_designation = character(0),
          stringsAsFactors = FALSE
        )
      }

      # Store at data()$metadata$TPP$TPPXXXXX$checks_per_trait and $checks (only if changed)
      temp <- isolate(data())
      if (is.null(temp$metadata$TPP)) {
        temp$metadata$TPP <- list()
      }
      if (is.null(temp$metadata$TPP[[tpp_id]])) {
        temp$metadata$TPP[[tpp_id]] <- list()
      }

      existing_checks_per_trait <- temp$metadata$TPP[[tpp_id]]$checks_per_trait
      existing_checks <- temp$metadata$TPP[[tpp_id]]$checks

      checks_per_trait_changed <- !identical(existing_checks_per_trait, checks_per_trait)
      checks_changed <- !identical(existing_checks, checks_df)

      if (checks_per_trait_changed || checks_changed) {
        temp$metadata$TPP[[tpp_id]]$checks_per_trait <- checks_per_trait
        temp$metadata$TPP[[tpp_id]]$checks <- checks_df
        data(temp)
      }
    })

    # ── Step 4: Status ───────────────────────────────────────────────────────
    output$status_display <- renderUI({
      tpp_id <- trimws(input$tpp_id)
      req(tpp_id)
      req(data())

      # Get stored mappings
      tpp_meta <- data()$metadata$TPP[[tpp_id]]
      tpp_data <- data()$data$TPP[[tpp_id]]

      # Simple logic: check what's stored
      traits_mapping <- tpp_meta$traits
      checks_mapping <- tpp_meta$checks
      stored_scale_validations <- tpp_meta$scale_validations

      # Evaluate status (pass scale_validations to prevent mapping while violations exist)
      status <- evaluate_mapping_status(traits_mapping, checks_mapping, tpp_data, stored_scale_validations)

      # Build colored status display
      color_style <- if (status$color == "green") {
        "color: #28a745; font-weight: bold; font-size: 1.2em; padding: 15px; border: 2px solid #28a745; border-radius: 5px; background-color: #d4edda;"
      } else {
        "color: #dc3545; font-weight: bold; font-size: 1.2em; padding: 15px; border: 2px solid #dc3545; border-radius: 5px; background-color: #f8d7da;"
      }

      icon_name <- if (status$color == "green") "check-circle" else "exclamation-triangle"

      tags$div(
        style = color_style,
        icon(icon_name),
        tags$span(status$message)
      )
    })

  })
}

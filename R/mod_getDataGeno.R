geno_example  <- 'www/example/geno.hmp.txt'

# Currently polyploid supported formats
polyploid_support <- c("vcf", 'dartag', 'hapmap')
dartseq_formats <- c("dartseqsnp")
#' getDataGenoCustom UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_getDataGeno_ui <- function(id) {
  ns <- NS(id)
  tagList(tags$br(),
          navlistPanel("Steps:", id = ns("geno_load_navpanel"), widths = c(2, 10),
            tabPanel("1. Load data",
              column(width = 8,
                     selectInput(inputId = ns('custom_geno_input'),
                                 label   = 'Biallelic SNP format*:',
                                 choices = list('HapMap' = 'hapmap',
                                                'VCF' = 'vcf',
                                                'DartSeq SNP' = 'dartseqsnp',
                                                'DarTag SNP' = 'dartag'
                                                ),
                                 width   = '200px'
                                 ),

                     selectInput(inputId = ns('geno_input'),
                                 label   = 'Data Source*: ',
                                 choices = list('Upload File' = 'file',
                                                'Copy URL' = 'url'),
                                 width   = '200px'
                                 ),

                     tags$span(id = ns('adegeno_file_holder'),
                               fileInput(inputId = ns('adegeno_file'),
                                         label   = NULL,
                                         width   = '400px',
                                         accept  = c('application/gzip', '.gz',
                                                     '.txt', '.hmp', '.csv', '.vcf')
                               )
                     ),
                     tags$span(id = ns('adegeno_url_holder'),
                               textInput(inputId = ns("adegeno_url"),
                                         label = NULL,
                                         width   = '400px',
                                         placeholder = "https://website.com/genotype_file.txt"
                               )
                     ),
                     tags$span(id = ns('adegeno_file_holder2'),
                               fileInput(inputId = ns('darttag_counts_file'),
                                         label   = "Counts:",
                                         width   = '400px',
                                         accept  = c('.csv')
                               ),
                               fileInput(inputId = ns('darttag_dosage_file'),
                                         label   = "Dosage:",
                                         width   = '400px',
                                         accept  = c('.csv')
                               )
                     ),
                     tags$span(id = ns('adegeno_url_holder2'),
                               textInput(inputId = ns("darttag_counts_url"),
                                         label = "Counts:",
                                         width   = '400px',
                                         placeholder = "https://website.com/dartag_counts.csv"
                               ),
                               textInput(inputId = ns("darttag_dosage_url"),
                                         label = "Dosage:",
                                         width   = '400px',
                                         placeholder = "https://website.com/dartag_dosage.csv"
                               ),
                     ),
                     if (!is.null(geno_example)) {
                       shinyWidgets::prettySwitch( inputId = ns('geno_example'), label = "Load example", status = "success")
                     },

                     actionButton(ns("load_geno_btn"), "Load"),
              ),
              column(width = 4,
                shinydashboard::box(width = 12, title = span(icon('youtube'), ' Tutorial'), status = "success", collapsible = TRUE, collapsed = TRUE, solidHeader=FALSE, style = "color: #000000",
                  h4(strong(span("", tags$a(href = "https://www.youtube.com/watch?v=gFYGIb9fBLQ&list=PLZ0lafzH_UmclOPifjCntlMzysEB2_2wX&index=2", icon("youtube") , target ="_blank"), style = "color:darkcyan"))),
                ),
                tags$div(id = ns('geno_table_options'),
                  shinydashboard::box(width = 12,title = span(icon('screwdriver-wrench'), ' Options'), status = "success", collapsible = TRUE, collapsed = FALSE, solidHeader=FALSE, style = "color: #000000",
                    uiOutput(ns("ploidity_params")),
                    uiOutput(ns("dartseq_params"))
                  ),
                ),
              )
            ),
            tabPanel("2. Genotype data Summary",
                     column(width = 8,
                            hr(),
                            uiOutput(ns("summary_warning")),
                            # Outputs
                            tableOutput(ns('summary_by_chrom')),
                            DT::DTOutput(ns('ind_summary')),
                            div(
                              id = ns("geno_summary_container"),
                              shinydashboard::box(
                                width = 12,
                                title = tags$span("Data summary", style = "font-weight:600;"),  # only title bold
                                status = NULL,
                                solidHeader = FALSE,
                                collapsible = FALSE,

                                tags$style(HTML(sprintf(
                                  "#%s .box-body{padding-top:8px;padding-bottom:8px;}",
                                  ns("geno_summary_container")
                                ))),

                                tags$div(
                                  style = "background:#f5f5f5; border:1px solid #e0e0e0; border-radius:6px;
               padding:8px 12px; margin-top:4px; line-height:1.6;",
                                  uiOutput(ns("geno_summary"))
                                )
                              )
                            ),
                            actionButton(ns("ind_management_btn"), "Accept and proceed"),
                            div(style = "height: 32px;")
                     )),
            tabPanel("3. Individual Management",
                     uiOutput(ns("indManagementUI"))),
            tabPanel("4. Check status",
                     uiOutput(ns("warningMessage")),
            )))}


mod_getDataGeno_server <-
  function(id, data = NULL, res_auth = NULL) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      library(adegenet)

      dup_values <- reactiveValues(
        load_geno_data = FALSE,
        run_det_dups = FALSE,
        dup_df = NULL
      )

      recalc_if_nonempty <- function(gl) {
        if (adegenet::nInd(gl) > 0) {
          tryCatch(cgiarGenomics::recalc_metrics(gl), error = function(e) gl)
        } else {
          gl
        }
      }

      rv <- reactiveValues(is_merging = FALSE)

      output$warningMessage <- renderUI(
        if(is.null(data())){
          HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your genotype data using the 'Data Retrieval' tab. Make sure you use the right data format (e.g., single header, etc.).")) )
        }else{ # data is there
          if(!is.null(data()$data$geno)){
            HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, you can proceed to use the other modules.")) )
          }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your genotype data using the 'Data Retrieval' tab. Make sure you use the right data format (e.g., single header, etc.). ")) )}
        }
      )

      observeEvent(c(input$custom_geno_input, input$geno_input),{
        if(input$custom_geno_input != "dartag"){
          if(length(input$geno_input) >0){
            if (input$geno_input == 'file') {
              golem::invoke_js('showid', ns('adegeno_file_holder'))
              golem::invoke_js('hideid', ns('adegeno_url_holder'))
              golem::invoke_js('hideid', ns('adegeno_file_holder2'))
              golem::invoke_js('hideid', ns('adegeno_url_holder2'))
              updateCheckboxInput(session, 'geno_example', value = FALSE)
            } else if (input$geno_input == 'url') {
              golem::invoke_js('hideid', ns('adegeno_file_holder'))
              golem::invoke_js('showid', ns('adegeno_url_holder'))
              golem::invoke_js('hideid', ns('adegeno_file_holder2'))
              golem::invoke_js('hideid', ns('adegeno_url_holder2'))
            }
          }
        } else{
          if(length(input$geno_input) >0){
            if (input$geno_input == 'file') {
              golem::invoke_js('hideid', ns('adegeno_file_holder'))
              golem::invoke_js('hideid', ns('adegeno_url_holder'))
              golem::invoke_js('showid', ns('adegeno_file_holder2'))
              golem::invoke_js('hideid', ns('adegeno_url_holder2'))
              updateCheckboxInput(session, 'geno_example', value = FALSE)
            } else if (input$geno_input == 'url') {
              golem::invoke_js('hideid', ns('adegeno_file_holder'))
              golem::invoke_js('hideid', ns('adegeno_url_holder'))
              golem::invoke_js('hideid', ns('adegeno_file_holder2'))
                golem::invoke_js('showid', ns('adegeno_url_holder2'))
                updateCheckboxInput(session, 'geno_example', value = FALSE)
              }
            }
          }
        }
        )

        output$ploidity_params <- renderUI({
          req(input$custom_geno_input)

          if (input$custom_geno_input %in% polyploid_support) {
            print('Polyploid supported format')
            ploidity_list <- list("Diploid" = 2, "Tetraploid" = 4, "Hexaploid" = 6)
          } else {
            # ADD should limit the obtions of poly lvl to only diploid
            print('Polyploid unsupported format')
            ploidity_list <- list("Diploid" = 2)
          }

          selectInput(inputId = ns("ploidlvl_input"),
                      label = "Ploidity level:",
                      choices = ploidity_list,
                      )
          })

        output$dartseq_params <- renderUI({
          req(input$custom_geno_input)
          req(input$adegeno_file)

          if (input$custom_geno_input %in% dartseq_formats) {
            print('dartseq format')
            dart_cols <- dart_getCols(input$adegeno_file$datapath)
            tags$span(
              selectInput(inputId = ns("dartseq_markerid"),
                          label = "Marker id:",
                          choices = dart_cols
                          ),
              selectInput(inputId = ns("dartseq_chrom"),
                          label = "Chromosome",
                          choices = dart_cols
                          ),
              selectInput(inputId = ns("dartseq_position"),
                          label = "Variant position",
                          choices = dart_cols
                          )
            )
          }
        })

        ## data example loading
        observeEvent(input$geno_example,{
          if(length(input$geno_example) > 0){
            if (input$geno_example) {
              shinyWidgets::ask_confirmation(
                inputId = ns("myconfirmation"),
                text = "Are you sure you want to load the example genotypes? This will delete any genotypic data currently in the environment.",
                title = "Data replacement warning"
              )
            }
          }
        })

        observeEvent(input$myconfirmation, {
          if (isTRUE(input$myconfirmation)) {
            if(length(input$geno_example) > 0){
              if (input$geno_example) {
                updateSelectInput(session, 'custom_geno_input', selected = 'hapmap')
                updateSelectInput(session, 'geno_input', selected = 'url')

                geno_example_url <- 'https://raw.githubusercontent.com/Breeding-Analytics/bioflow/main/inst/app/www/example/geno.hmp.txt'

                updateTextInput(session, 'adegeno_url', value = geno_example_url)
              } else {
                updateSelectInput(session, 'custom_geno_input', selected = 'hapmap')
                updateSelectInput(session, 'geno_input', selected = 'file')
                updateTextInput(session, 'adegeno_url', value = '')
              }
            }
          }else{
            shinyWidgets::updatePrettySwitch(session, "geno_example", value = FALSE)
          }
        }, ignoreNULL = TRUE)

        # Reactive function where input functions are called
        get_geno_data <- reactive({
          print("Geno load btn clicked")
          if(input$custom_geno_input != "dartag"){
            if (input$geno_input == 'file') {
              genotype_file <- input$adegeno_file$datapath
            } else if (input$geno_input == 'url') {
              temp_genofile <- tempfile(
                tmpdir = tempdir(),
                fileext = sub(".*\\.([a-zA-Z0-9]+)$", ".\\1", input$adegeno_url))
              # Download file
              utils::download.file(input$adegeno_url, temp_genofile)
              genotype_file <- temp_genofile
              }
            } else {
              if (input$geno_input == 'file') {
                dosage_file <- input$darttag_dosage_file$datapath
                counts_file <- input$darttag_counts_file$datapath
              } else if (input$geno_input == 'url') {
                # Dosage file download
                temp_dosagefile <- tempfile(
                  tmpdir = tempdir(),
                  fileext = sub(".*\\.([a-zA-Z0-9]+)$", ".\\1", input$darttag_dosage_url))
                utils::download.file(input$darttag_dosage_url, temp_dosagefile)
                dosage_file <- temp_dosagefile
                # Counts file download
                temp_countsfile <- tempfile(
                  tmpdir = tempdir(),
                  fileext = sub(".*\\.([a-zA-Z0-9]+)$", ".\\1", input$darttag_counts_url))
                utils::download.file(input$darttag_counts_url, temp_countsfile)
                counts_file <- temp_countsfile
              }
            }

            print(as.numeric(input$ploidlvl_input))
            print(typeof(as.numeric(input$ploidlvl_input)))
            switch(input$custom_geno_input,
                   hapmap = {
                     shinybusy::show_modal_spinner('fading-circle', text = 'Loading...')
                     tryCatch({
                       geno_data = cgiarGenomics::read_hapmap(path = genotype_file,
                                                              ploidity = as.numeric(input$ploidlvl_input))
                       }, error = function(e) {
                         print(e)
                         shinyWidgets::show_alert(title = 'Error !!',
                                                  text = 'Not a valid file format :-(',
                                                  type = 'error')
                         })
                     shinybusy::remove_modal_spinner()
                     },
                   vcf = {
                     shinybusy::show_modal_spinner('fading-circle', text = 'Loading...')
                     tryCatch({
                       geno_data = cgiarGenomics::read_vcf(path = genotype_file,
                                                           ploidity = as.numeric(input$ploidlvl_input))
                       }, error = function(e) {
                         print(e)
                         shinyWidgets::show_alert(title = 'Error !!',
                                                  text = 'Not a valid file format :-(',
                                                  type = 'error')
                         })
                     shinybusy::remove_modal_spinner()
                     },
                   dartseqsnp = {
                     shinybusy::show_modal_spinner('fading-circle', text = 'Loading...')
                     tryCatch({
                       geno_data = cgiarGenomics::read_DArTSeq_SNP(path = genotype_file,
                                                                   snp_id = input$dartseq_markerid,
                                                                   chr_name = input$dartseq_chrom,
                                                                   pos_name = input$dartseq_position)
                       }, error = function(e) {
                         print(e)
                         shinyWidgets::show_alert(title = 'Error !!',
                                                  text = 'Not a valid file format :-(',
                                                  type = 'error')
                         })
                     shinybusy::remove_modal_spinner()
                     },
                   dartag = {
                     shinybusy::show_modal_spinner('fading-circle', text = 'Loading...')
                     tryCatch({
                       geno_data = cgiarGenomics::read_DArTag_count_dosage(dosage_path = dosage_file,
                                                                           counts_path = counts_file,
                                                                           ploidity = as.numeric(input$ploidlvl_input))
                       }, error = function(e) {
                         print(e)
                         shinyWidgets::show_alert(title = 'Error !!',
                                                  text = 'Not a valid file format :-(',
                                                  type = 'error')
                         })
                     shinybusy::remove_modal_spinner()
                     },
                   {
                     print("This should not execute D;")
                     }
                   )
            dup_values$load_geno_data <- TRUE
            return(geno_data)
            })

        # just to make button reactive
        observeEvent(input$load_geno_btn,{
          geno_data <- get_geno_data()
          add_data()
          updateNavlistPanel(session = session,
                             inputId = "geno_load_navpanel",
                             selected = "2. Genotype data Summary")

          # Summary of genotypic data
          output$summary_by_chrom <- renderTable({
            # geno_data <- get_geno_data()
            print("Processsing...")
            geno_metadata <- data.frame(CHROM = geno_data@chromosome,
                                        POS = geno_data@position)
            data.frame(
              chrom = unique(geno_metadata$CHROM),
              min_pos = aggregate(POS ~ CHROM, data = geno_metadata, FUN = min)[, 2],
              max_pos = aggregate(POS ~ CHROM, data = geno_metadata, FUN = max)[, 2],
              snps_count = aggregate(POS ~ CHROM, data = geno_metadata, FUN = length)[, 2]
            )
          })

          output$geno_summary <- renderUI({
            tmp <- data()
            gl  <- if (!is.null(tmp) && !is.null(tmp$data)) tmp$data$geno else NULL
            if (is.null(gl)) return(NULL)

            total <- tryCatch(adegenet::nInd(gl), error = function(e) NA_integer_)

            # Duplicate-accession count (among non-F1s when applicable)
            dups_tbl <- dup_groups()
            dup_accessions <- if (!is.null(dups_tbl) && nrow(dups_tbl) > 0) {
              sum(dups_tbl$count, na.rm = TRUE)
            } else 0

            # F1/parental breakdown: support either sample_id OR designation matching
            et <- crosstype_info()
            f1_count <- NA_integer_
            parent_count <- NA_integer_

            if (!is.null(tmp) &&
                !is.null(tmp$data$pedigree) &&
                !is.null(tmp$metadata) &&
                !is.null(tmp$metadata$pedigree) &&
                ("crossType" %in% tmp$metadata$pedigree$parameter)) {

              ped <- tmp$data$pedigree
              md  <- tmp$metadata$pedigree

              et_col  <- md$value[md$parameter == "crossType"]
              sid_col <- if ("sample_id" %in% md$parameter) md$value[md$parameter == "sample_id"] else NA_character_

              # 1) prefer sample_id if present and valid
              # 2) otherwise fall back to 'designation' if it exists in pedigree
              key_col <- NULL
              if (!is.na(sid_col) && nzchar(sid_col) && sid_col %in% names(ped)) {
                key_col <- sid_col
              } else if ("designation" %in% names(ped)) {
                key_col <- "designation"
              }

              if (!is.null(key_col) &&
                  !is.na(et_col) && nzchar(et_col) && et_col %in% names(ped)) {

                inds <- adegenet::indNames(gl)
                cross_vec <- ped[[et_col]][match(inds, ped[[key_col]])]

                is_f1  <- !is.na(cross_vec) & toupper(as.character(cross_vec)) == "F1"
                f1_count     <- sum(is_f1, na.rm = TRUE)
                parent_count <- sum(!is_f1 & !is.na(cross_vec), na.rm = TRUE)

              }
            }

            fmt <- function(x) if (is.na(x)) "NA" else format(x, big.mark = ",", trim = TRUE)

            if (et$status == "has_f1" && !is.na(f1_count) && !is.na(parent_count)) {
              tags$ul(
                style = "margin-top: 2px;",
                tags$li(sprintf("%s total accessions", fmt(total))),
                tags$li(sprintf("%s parental accessions", fmt(parent_count))),
                tags$ul(
                  style = "margin-top: 4px; margin-bottom: 6px;",
                  tags$li(sprintf(
                    "%s parental accessions have duplicated designations and will go through Individual Management",
                    fmt(dup_accessions)
                  ))
                ),
                tags$li(sprintf("%s F1 accessions", fmt(f1_count)))
              )
            } else {
                tags$ul(
                style = "margin-top: 2px;",
                tags$li(sprintf("%s total accessions", fmt(total))),
                tags$li(sprintf(
                  "%s accessions have duplicated designations and will go through Individual Management",
                  fmt(dup_accessions)
                ))
              )
            }
          })


        output$summary_warning <- renderUI({
          temp <- data()

          # Default: no warning
          warn_needed <- FALSE
          msg <- NULL

          # 1) Check that data()$data$pedigree exists
          if (is.null(temp) || is.null(temp$data) || is.null(temp$data$pedigree)) {
            warn_needed <- TRUE
          } else {
            ped <- temp$data$pedigree

            # 2) Check that data()$metadata$pedigree has parameter == "sample_id"
            if (is.null(temp$metadata) || is.null(temp$metadata$pedigree)) {
              warn_needed <- TRUE
            } else {
              md_ped <- temp$metadata$pedigree
              if (!("sample_id" %in% md_ped$parameter)) {
                warn_needed <- TRUE
              } else {
                # 3) Get the column name for sample_id from metadata$pedigree$value
                col_name <- md_ped$value[md_ped$parameter == "sample_id"]
                # sanity checks on column name & values
                if (length(col_name) != 1 || is.na(col_name) || is.null(col_name) || identical(col_name, "")) {
                  warn_needed <- TRUE
                } else if (!(col_name %in% colnames(ped))) {
                  warn_needed <- TRUE
                } else {
                  vec <- ped[[col_name]]
                  # non-NA / non-NULL values required
                  if (is.null(vec) || all(is.na(vec))) {
                    warn_needed <- TRUE
                  }
                }
              }
            }
          }

          if (!warn_needed) return(NULL)

          # Warning UI
          HTML(as.character(
            div(
              style = "background:#fff3cd;border:1px solid #ffeeba;color:#856404;padding:12px;border-radius:6px;margin-bottom:12px;",
              tags$b("Warning: sample_id → designation mapping is missing."),
              tags$p(
                "Bioflow will assume the genotypic data already uses the correct designation
                names. If your genotyping was done using sample_id codes,
                please provide a mapping through the Pedigree & Geno metadata tab and return to this step before
                proceeding"
              ),
            )
          ))
        })


        #Summary table
        output$ind_summary <- DT::renderDT({
          # Order of individuals as they appear in the genotypic file
          inds <- adegenet::indNames(geno_data)

          df_out <- data.frame(
            designation = inds,
            check.names = FALSE,
            stringsAsFactors = FALSE
          )

          # Try to switch to a 2-column mapping if pedigree sample_id info is present
          temp <- data()
          if (!is.null(temp) &&
              !is.null(temp$data) &&
              !is.null(temp$data$pedigree) &&
              !is.null(temp$metadata) &&
              !is.null(temp$metadata$pedigree) &&
              ("sample_id" %in% temp$metadata$pedigree$parameter)) {

            ped <- temp$data$pedigree
            md_ped <- temp$metadata$pedigree
            col_name <- md_ped$value[md_ped$parameter == "sample_id"]

            # Validate the mapping column name and values
            if (length(col_name) == 1 &&
                !is.na(col_name) &&
                !is.null(col_name) &&
                !identical(col_name, "") &&
                (col_name %in% colnames(ped))) {

              # Build mapping: sample_id -> designation (pedigree must have 'designation' column)
              if ("designation" %in% colnames(ped)) {
                idx <- match(inds, ped[[col_name]])
                mapped_designation <- ped$designation[idx]

                df_out <- data.frame(
                  sample_id   = inds,
                  designation = mapped_designation,
                  check.names = FALSE,
                  stringsAsFactors = FALSE
                )
              }
            }
          }

          DT::datatable(
            df_out,
            options = list(
              dom = "lfrtip",
              pageLength = 10,
              lengthMenu = c(5, 10, 25, 50)
            ),
            rownames = FALSE
          )
        })

        })

        # Store the genotype data into the data structure and update the metadata
        add_data <- reactive({
            temp <- data()
            if(!is.null(temp$metadata$geno)){temp$metadata$geno <- temp$metadata$geno[0,]} # make sure if an user uploads a new dataset the metadata starts empty
            if(!is.null(temp$modifications$geno)){temp$modifications$geno <- temp$modifications$geno[0,]} # make sure if an user uploads a new dataset the modifications starts empty
            if(!is.null(temp$status)){
              toRemove <- which(temp$status$module == "qaGeno")
              if(length(toRemove) > 0){temp$status <- temp$status[-toRemove,, drop=FALSE]}
            } # make sure if an user uploads a new dataset the qaGeno starts empty

            # Add the gl object in results$data$geno slot
            temp$data$geno <- get_geno_data()

            {
              mp <- ped_mapping()
              et <- crosstype_info()
              dups <- dup_groups()

              # crossType absent or all NA
              if (et$status %in% c("absent", "all_na")) {
                if (is.null(dups) || nrow(dups) == 0) {
                  # No duplicated designations -> translate all sample_ids to designation
                  temp$data$geno <- translate_ind_names(temp$data$geno, mp, et)
                }
                # If duplicates exist: do nothing here; translation happens AFTER consensus (below)
              } else if (et$status == "present_no_f1") {
                if (is.null(dups) || nrow(dups) == 0) {
                  temp$data$geno <- translate_ind_names(temp$data$geno, mp, et)
                }
              } else if (et$status == "has_f1") {
                # With F1 present and no duplicates among non-F1 -> translate parents only (F1s retained)
                if (is.null(dups) || nrow(dups) == 0) {
                  temp$data$geno <- translate_ind_names(temp$data$geno, mp, et)
                }
                # If duplicates among non-F1 exist: handle after consensus
              }
            }

            # Add metadata
            parameter_names <- c(
              'input_format',
              'ploidity')
            parameter_values <- c(input$custom_geno_input,
                                  as.numeric(input$ploidlvl_input))

            tmp_metadata <- data.frame(parameter = parameter_names,
                                       value = parameter_values)

            # If the input file is a dartseqsnp also store in metadata the
            # column names of marker_id, chromosome and position in the csv
            if(input$custom_geno_input == 'dartseqsnp'){
              parameter_names <- c('dartseq_marker_id',
                                   'dartseq_chr',
                                   'dartseq_pos')

              parameter_values <- c(input$dartseq_markerid,
                                    input$dartseq_chrom,
                                    pos_name = input$dartseq_position)

              dartseq_metadata <- data.frame(parameter = parameter_names,
                                             value = parameter_values)

              tmp_metadata <- rbind(tmp_metadata, dartseq_metadata)

            }

            temp$metadata$geno <- tmp_metadata

            data(temp)
          }
        )


  # Ind Management server ---------------------------------------------------
        # pedigree mapping helpers
        ped_mapping <- reactive({
          temp <- data()
          if (is.null(temp) || is.null(temp$data$pedigree) ||
              is.null(temp$metadata) || is.null(temp$metadata$pedigree) ||
              !("sample_id" %in% temp$metadata$pedigree$parameter)) {
            return(NULL)
          }
          md_ped   <- temp$metadata$pedigree
          ped      <- temp$data$pedigree
          col_name <- md_ped$value[md_ped$parameter == "sample_id"]

          if (length(col_name) != 1 || is.na(col_name) || identical(col_name, "") ||
              !(col_name %in% colnames(ped)) || !("designation" %in% colnames(ped))) {
            return(NULL)
          }

          out <- ped[, c(col_name, "designation"), drop = FALSE]
          colnames(out) <- c("sample_id", "designation_id")
          # keep only non-empty sample_ids
          out <- out[!is.na(out$sample_id) & out$sample_id != "", , drop = FALSE]
          if (!nrow(out)) return(NULL)
          out
        })

        dup_groups <- reactive({
          mp <- ped_mapping()
          if (is.null(mp)) return(NULL)

          temp   <- data()
          ped    <- temp$data$pedigree
          md_ped <- temp$metadata$pedigree

          et <- crosstype_info()

          # If crossType has F1, exclude F1 rows from duplicate checking
          if (!is.null(et$col) && et$status == "has_f1") {
            # columns from metadata
            sid_col <- md_ped$value[md_ped$parameter == "sample_id"]  # sample_id column name in pedigree
            et_col  <- et$col                                         # crossType column name in pedigree

            # Map each mp$sample_id to its crossType
            ped_idx   <- match(mp$sample_id, ped[[sid_col]])
            cross_vec <- ped[[et_col]][ped_idx]

            is_f1 <- !is.na(cross_vec) & toupper(as.character(cross_vec)) == "F1"
            mp    <- mp[!is_f1, , drop = FALSE]
          }

          if (!nrow(mp)) return(NULL)

          dplyr::count(mp, designation_id, name = "count") |>
            dplyr::filter(count > 1)
        })

        can_manage_inds <- reactive({
          !is.null(ped_mapping()) && !is.null(dup_groups()) && nrow(dup_groups()) > 0
        })

        observeEvent(input$ind_management_btn, {
          if (isTRUE(can_manage_inds())) {

            df <- ped_mapping() |>
              dplyr::filter(.data$designation_id %in% dup_groups()$designation_id)

            et <- crosstype_info()
            if (!is.null(et$col) && et$status == "has_f1") {
              ped <- data()$data$pedigree
              md  <- data()$metadata$pedigree
              sid_col <- md$value[md$parameter == "sample_id"]
              # exclude F1 rows
              cross_vec <- ped[[et$col]][match(df$sample_id, ped[[sid_col]])]
              df <- df[!(toupper(as.character(cross_vec)) == "F1"), , drop = FALSE]
            }

            dup_values$dup_df <- dplyr::mutate(df, selected = FALSE)
            updateNavlistPanel(session = session,
                               inputId = "geno_load_navpanel",
                               selected = "3. Individual Management")
          } else {
            shinyWidgets::show_alert(
              title = "Individual management will be skipped",
              text = paste0(
                "This section is enabled only when (1) a pedigree mapping ",
                "sample_id → designation is provided, and (2) at least one ",
                "designation maps to multiple sample_ids."
              ),
              type = "warning"
            )

            updateNavlistPanel(session = session,
                               inputId = "geno_load_navpanel",
                               selected = "4. Check status")
          }
        })

        # crossType helpers
        crosstype_info <- reactive({
          temp <- data()
          out <- list(status = "absent", col = NULL, vec = NULL)  # statuses: absent, all_na, present_no_f1, has_f1

          if (is.null(temp) || is.null(temp$metadata) || is.null(temp$metadata$pedigree)) return(out)
          md_ped <- temp$metadata$pedigree
          if (!("crossType" %in% md_ped$parameter)) return(out)

          ped <- temp$data$pedigree
          col_name <- md_ped$value[md_ped$parameter == "crossType"]
          if (length(col_name) != 1 || is.na(col_name) || identical(col_name, "") || !(col_name %in% colnames(ped))) {
            return(out)  # treat as absent
          }
          vec <- ped[[col_name]]
          if (length(vec) == 0 || all(is.na(vec))) {
            out$status <- "all_na"
          } else if (any(toupper(as.character(vec)) == "F1", na.rm = TRUE)) {
            out$status <- "has_f1"
          } else {
            out$status <- "present_no_f1"
          }
          out$col <- col_name
          out$vec <- vec
          out
        })

        output$indManagementUI <- renderUI({
          if (isTRUE(rv$is_merging)) return(NULL)  # avoid mid-merge redraws
          req(dup_values$load_geno_data)
          req(can_manage_inds())


          gl <- data()$data$geno

          nloc <- adegenet::nLoc(gl)
          prev <- isolate(input$n_loc_dup_slider)
          default_val <- if (!is.null(prev) && is.finite(prev) && prev >= 1 && prev <= nloc) prev else min(100,nloc)

          tags$div(
            shinydashboard::box(width = 12, title = "Subset genotype matrix",
                                fluidRow(
                                  column(
                                    width = 6,
                                    shinydashboard::box(
                                      title = "Parameters", solidHeader = TRUE, status = "primary",
                                      "Subsample the genotypic matrix to detect duplicates based on IBS",
                                      br(), "Samples with the same designation will be compared using this submatrix.",
                                      sliderInput(
                                        ns("n_loc_dup_slider"),
                                        "Select number of markers to consider:",
                                        min = 1,
                                        max = nloc,
                                        value = default_val,   # defaults to, at most, 100 markers; keeps user's last value if still valid
                                        step = 1
                                      )
                                      ,
                                      sliderInput(ns("loc_miss_dup_slider"), "Set min locus missingness:",
                                                  min = 0, max = 1, value = 0.2, step = 0.01),
                                      sliderInput(ns("maf_dup_slider"), "Set min MAF:",
                                                  min = 0, max = 1, value = 0.05, step = 0.01),
                                      numericInput(ns("seed_dup"), "Random seed", value = 7, min = 0, step = 1)
                                    )
                                  )
                                )
            ),

            shinydashboard::box(width = 12, title = "Merge Individuals",
                                # group picker fed from pedigree duplicates
                                uiOutput(ns("dup_group_picker")),
                                uiOutput(ns("dup_group_notice")),
                                fluidRow(
                                  column(width = 6, plotly::plotlyOutput(ns("dup_group_heatmap"))),
                                  column(
                                    width = 6,
                                    tags$h4("Actions"),
                                    checkboxGroupInput(
                                      ns("dup_merge_samples"),
                                      label   = "Select samples to merge into this designation (consensus):",
                                      choices = NULL
                                    ),
                                    #checkboxGroupInput(
                                    #  ns("dup_remove_samples"),
                                    #  label   = "Select samples to remove:",
                                    #  choices = NULL
                                    #),
                                    div(
                                      style = "display:flex; gap:8px; flex-wrap:wrap;",
                                      actionButton(ns("btn_apply_actions"), "Run consensus")
                                    )
                                  )
                                )
            ),
          )
        })

        output$dup_group_picker <- renderUI({
          if (isTRUE(rv$is_merging)) {
            return(NULL)
          }

          dgs <- dup_groups()
          if (is.null(dgs) || nrow(dgs) == 0) {
            return(span("No duplicate groups remaining."))
          }

          ch <- dgs$designation_id
          prev <- isolate(input$dup_group_pick)
          sel  <- if (length(prev) == 1 && !is.null(prev) && prev %in% ch) prev else ch[[1]]

          selectInput(ns("dup_group_pick"),
                      "Select a duplicate group (designation)",
                      choices = ch,
                      selected = sel,
                      multiple = FALSE)
        })

        picked_dup_samples <- reactive({
          if (isTRUE(rv$is_merging)) {
            return(data.frame(sample_id=character(), designation_id=character(), selected=logical()))
          }
          df <- dup_values$dup_df
          if (!is.data.frame(df) || !"designation_id" %in% names(df) || is.null(input$dup_group_pick)) {
            return(data.frame(sample_id=character(), designation_id=character(), selected=logical()))
          }

          out <- df[df$designation_id == input$dup_group_pick, , drop = FALSE]

          if (!nrow(out)) {
            return(data.frame(sample_id=character(), designation_id=character(), selected=logical()))
          }

          existing <- tryCatch(as.character(adegenet::indNames(data()$data$geno)), error = function(e) character(0))
          out <- out[out$sample_id %in% existing, , drop = FALSE]

          if (!nrow(out)) {
            return(data.frame(sample_id=character(), designation_id=character(), selected=logical()))
          }
          out
        })

        observe({
          if (isTRUE(rv$is_merging)) return()
          p <- picked_dup_samples()
          choices <- if (nrow(p)) p$sample_id else character(0)

          gl <- data()$data$geno
          cur_grp <- isolate(input$dup_group_pick)

          merge_choices  <- choices
          #remove_choices <- choices[]

          if (!is.null(gl) && !is.null(cur_grp) && cur_grp %in% adegenet::indNames(gl)) {
            # add reference to both lists
            merge_choices  <- c(merge_choices,  cur_grp)
            #remove_choices <- c(remove_choices, cur_grp)
          }

          keep <- function(x, pool) if (length(x)) intersect(x, pool) else character(0)
          updateCheckboxGroupInput(session, "dup_merge_samples",
                                   choices  = merge_choices,
                                   selected = keep(isolate(input$dup_merge_samples), merge_choices))
          #updateCheckboxGroupInput(session, "dup_remove_samples",
          #                         choices  = remove_choices,
          #                         selected = keep(isolate(input$dup_remove_samples), remove_choices))
        })

        output$dup_group_notice <- renderUI({
          cur <- input$dup_group_pick
          tmp <- data()
          gl  <- if (!is.null(tmp) && !is.null(tmp$data)) tmp$data$geno else NULL
          if (is.null(cur) || is.null(gl)) return(NULL)

          # If the current designation is itself present as an individual in the matrix
          if (cur %in% adegenet::indNames(gl)) {
            # Neutral grey note, plain text
            div(
              style = "background:#f5f5f5;border:1px solid #e0e0e0;border-radius:6px;padding:8px 10px;margin:6px 0;font-size:13px;",
              # Feel free to tweak phrasing:
              sprintf(
                "Note: A genotype already named “%s” is present in the genotype matrix. It will appear in the heatmap and in the checkboxes",
                cur
              )
            )
          } else {
            NULL
          }
        })



        #observeEvent(input$dup_merge_samples, ignoreInit = TRUE, {
        #  # remove overlap from the remove list
        #  overlap <- intersect(input$dup_merge_samples, isolate(input$dup_remove_samples))
        #  if (length(overlap)) {
        #    updateCheckboxGroupInput(session, "dup_remove_samples",
        #                             selected = setdiff(isolate(input$dup_remove_samples), overlap))
        #  }
        #})

        #observeEvent(input$dup_remove_samples, ignoreInit = TRUE, {
        #  # remove overlap from the merge list
        #  overlap <- intersect(input$dup_remove_samples, isolate(input$dup_merge_samples))
        #  if (length(overlap)) {
        #    updateCheckboxGroupInput(session, "dup_merge_samples",
        #                             selected = setdiff(isolate(input$dup_merge_samples), overlap))
        #  }
        #})


        filter_dup_det_gl <- reactive({
          req(dup_values$load_geno_data, input$n_loc_dup_slider, input$loc_miss_dup_slider, input$maf_dup_slider, input$seed_dup)

          gl <- data()$data$geno

          shinybusy::show_modal_spinner('fading-circle', text = 'Randomly filtering genotype matrix...')
          on.exit(shinybusy::remove_modal_spinner(), add = TRUE)

          size_req <- input$n_loc_dup_slider
          size_use <- max(1, min(size_req, adegenet::nLoc(gl)))

          rand_gl <- cgiarGenomics::random_select_loci(
            gl,
            ind_miss = 0.2,
            loc_miss = input$loc_miss_dup_slider,
            maf      = input$maf_dup_slider,
            size     = size_use,
            seed     = input$seed_dup
          )
          rand_gl
        })


        observeEvent(input$dup_update_dup_df, {
          req(dup_values$dup_df)

          choices <- dup_values$dup_df %>%
            dplyr::filter(designation_id == input$dup_group_pick) %>%
            dplyr::pull(sample_id)

          choices_idx <- which(dup_values$dup_df$sample_id %in% choices)
          selected <- which(choices %in% input$dup_group_select_samples)
          not_selected <- which(!choices %in% input$dup_group_select_samples)

          if (length(selected) > 0 ){
            dup_values$dup_df[choices_idx[selected], "selected"] <- TRUE
          }
          if (length(not_selected) > 0 ){
            dup_values$dup_df[choices_idx[not_selected], "selected"] <- FALSE
          }
        })

        filt_random_gl <- reactive({
          req(filter_dup_det_gl())
          sel <- picked_dup_samples()
          if (!is.data.frame(sel) || nrow(sel) == 0) {
            return(NULL)
          }

          target_samples <- sel$sample_id
          rand_gl <- filter_dup_det_gl()
          idx <- match(target_samples, adegenet::indNames(rand_gl))
          ok  <- !is.na(idx)

          if (!any(ok)) return(NULL)
          rand_gl[idx[ok], ]
        })


        get_ibs <- reactive({
          req(filt_random_gl())
          gl <- filt_random_gl()
          cgiarGenomics::ibs_matrix_purrr(gl, as.numeric(input$ploidlvl_input))
        })

        output$dup_group_heatmap <- plotly::renderPlotly({
          if (isTRUE(rv$is_merging)) {return(NULL)}
          if (nrow(picked_dup_samples()) < 2) {return(NULL)}

          tg_rand_gl <- filt_random_gl()
          if (is.null(tg_rand_gl)) {return(NULL) }

          ibs <- get_ibs()
          if (is.null(ibs) || is.null(ibs$ibs)) {return(NULL) }
          m <- tryCatch(as.matrix(ibs$ibs), error = function(e) NULL)
          if (is.null(m) || any(dim(m) < 2)) {return(NULL) }

          plotly::plot_ly(
            z = m, x = colnames(m), y = rownames(m),
            type = "heatmap", colorscale = "Viridis",
            zmin = 0, zmax = 1, colorbar = list(title = "IBS"),
            hovertemplate = "<b>%{y}</b> × <b>%{x}</b><br>IBS: %{z:.3f}<extra></extra>"
          ) |>
            plotly::layout(
              title = list(text = "IBS Heatmap (no clustering)", x = 0.5),
              xaxis = list(title = "", tickangle = 45, automargin = TRUE, constrain = "domain"),
              yaxis = list(title = "", autorange = "reversed", automargin = TRUE, scaleanchor = "x", scaleratio = 1),
              margin = list(l = 60, r = 20, t = 60, b = 80)
            )
        })

        observeEvent(input$btn_apply_actions, {
          rv$is_merging <- TRUE
          shinybusy::show_modal_spinner('fading-circle', text = 'Applying actions...')
          on.exit({ rv$is_merging <- FALSE; shinybusy::remove_modal_spinner() }, add = TRUE)

          cur_grp    <- isolate(input$dup_group_pick)
          merge_ids  <- isolate(input$dup_merge_samples)

          if (!length(merge_ids)) {
            shinyWidgets::show_alert(
              title = "Nothing to do",
              text  = "Select at least one sample to keep / use as consensus in this group.",
              type  = "info"
            )
            return(invisible(NULL))
          }

          group_ids <- dup_values$dup_df$sample_id[dup_values$dup_df$designation_id == cur_grp]
          remove_ids <- setdiff(group_ids, merge_ids)

          tmp <- data()
          if (is.null(tmp) || is.null(tmp$data) || is.null(tmp$data$geno)) {
            shinyWidgets::show_alert(title="Aborted", text="Genotype object not found.", type="error")
            return(invisible(NULL))
          }
          gl_src <- tmp$data$geno
          gl     <- gl_src

          # helper: copy marker slots (if lengths match) and recalc metrics only when non-empty
          apply_postprocess <- function(gl_new, gl_ref) {
            if (!is.null(gl_ref@position)   && length(gl_ref@position)   == adegenet::nLoc(gl_new)) gl_new@position   <- gl_ref@position
            if (!is.null(gl_ref@chromosome) && length(gl_ref@chromosome) == adegenet::nLoc(gl_new)) gl_new@chromosome <- gl_ref@chromosome
            if (!is.null(gl_ref@loc.all)    && length(gl_ref@loc.all)    == adegenet::nLoc(gl_new)) gl_new@loc.all    <- gl_ref@loc.all
            if (adegenet::nInd(gl_new) > 0) {
              tryCatch(cgiarGenomics::recalc_metrics(gl_new), error=function(e) gl_new)
            } else {
              gl_new
            }
          }

          # exactly one kept sample, no consensus needed
          if (length(merge_ids) == 1) {
            keep_id <- merge_ids[[1]]
            current_names <- as.character(adegenet::indNames(gl))

            # try to rename kept sample to designation (cur_grp) if doing so won't collide
            if (!identical(keep_id, cur_grp) && nzchar(cur_grp)) {
              cur_exists <- cur_grp %in% current_names
              cur_marked_for_removal <- cur_grp %in% remove_ids
              if (!cur_exists || cur_marked_for_removal) {
                idx <- match(keep_id, current_names)
                if (!is.na(idx)) {
                  current_names[idx] <- cur_grp
                  adegenet::indNames(gl) <- current_names
                }
              }
              # else: keep the original keep_id to avoid a duplicate name
            }

            # remove any not selected for merge (allow empty)
            if (length(remove_ids)) {
              keep_idx <- which(!adegenet::indNames(gl) %in% remove_ids)
              if (!length(keep_idx)) {
                gl <- gl[0, ]  # allow empty
              } else {
                gl <- gl[keep_idx, ]
              }
            }

            gl <- apply_postprocess(gl, gl_src)

            # save back
            tmp$data$geno <- gl
            data(tmp)

            # refresh dup table to only existing sample_ids; clear selections
            existing <- tryCatch(as.character(adegenet::indNames(gl)), error=function(e) character(0))
            if (is.data.frame(dup_values$dup_df) && nrow(dup_values$dup_df)) {
              dup_values$dup_df <- dup_values$dup_df[dup_values$dup_df$sample_id %in% existing, , drop=FALSE]
              if (nrow(dup_values$dup_df)) dup_values$dup_df$selected <- FALSE
            }
            updateCheckboxGroupInput(session, "dup_merge_samples",  selected = character(0))
            #updateCheckboxGroupInput(session, "dup_remove_samples", selected = character(0))

            # keep current group if still present; otherwise fallback or advance
            dgs <- dup_groups()
            if (is.null(dgs) || !nrow(dgs)) {
              updateNavlistPanel(session, "geno_load_navpanel", selected = "4. Check status")
            } else {
              new_choices <- dgs$designation_id
              new_sel <- if (!is.null(cur_grp) && nzchar(cur_grp) && cur_grp %in% new_choices) cur_grp else new_choices[[1]]
              updateSelectInput(session, "dup_group_pick", choices = new_choices, selected = new_sel)
            }

            if (adegenet::nInd(gl) == 0) {
              shinyWidgets::show_alert(title="All samples excluded",
                                       text="The genotype matrix is now empty. Load new data or go back.",
                                       type="warning")
            } else {
              shinyWidgets::show_alert(title="Done",
                                       text="Kept the selected sample and removed the rest.",
                                       type="success")
            }
            return(invisible(NULL))
          }

          #Regular input
          did_merge   <- FALSE
          did_remove  <- FALSE

          # 1) Merge by consensus only if ≥2 samples selected
          if (length(merge_ids) >= 2) {
            all_samples   <- as.character(adegenet::indNames(gl))
            sample_id_vec <- all_samples
            target_vec    <- all_samples  # identity map
            idx <- match(merge_ids, sample_id_vec)
            idx <- idx[!is.na(idx)]
            if (length(idx)) target_vec[idx] <- cur_grp

            full_map <- data.frame(
              sample_id      = sample_id_vec,
              designation_id = target_vec,
              check.names    = FALSE, stringsAsFactors = FALSE
            )

            gl <- cgiarGenomics::merge_duplicate_inds(gl, full_map)
            did_merge <- TRUE
          }

          # 2) Remove selected (after merge); allow empty
          if (length(remove_ids)) {
            keep_idx <- which(!adegenet::indNames(gl) %in% remove_ids)
            if (!length(keep_idx)) {
              gl <- gl[0, ]
            } else {
              gl <- gl[keep_idx, ]
            }
            did_remove <- TRUE
          }

          # 3) Postprocess (copy marker slots & recalc metrics if non-empty), save back
          gl <- apply_postprocess(gl, gl_src)
          tmp$data$geno <- gl
          data(tmp)

          # 4) Refresh dup_df: drop rows for samples that no longer exist; clear selections
          existing <- tryCatch(as.character(adegenet::indNames(gl)), error=function(e) character(0))
          if (is.data.frame(dup_values$dup_df) && nrow(dup_values$dup_df)) {
            dup_values$dup_df <- dup_values$dup_df[dup_values$dup_df$sample_id %in% existing, , drop=FALSE]
            if (nrow(dup_values$dup_df)) dup_values$dup_df$selected <- FALSE
          }
          updateCheckboxGroupInput(session, "dup_merge_samples",  selected = character(0))
          updateCheckboxGroupInput(session, "dup_remove_samples", selected = character(0))

          # 5) Keep current group if still present; otherwise fallback or advance
          dgs <- dup_groups()
          if (is.null(dgs) || !nrow(dgs)) {
            updateNavlistPanel(session, "geno_load_navpanel", selected = "4. Check status")
          } else {
            new_choices <- dgs$designation_id
            new_sel <- if (!is.null(cur_grp) && nzchar(cur_grp) && cur_grp %in% new_choices) cur_grp else new_choices[[1]]
            updateSelectInput(session, "dup_group_pick", choices = new_choices, selected = new_sel)
          }

          # 6) Feedback
          if (adegenet::nInd(gl) == 0) {
            shinyWidgets::show_alert(title="All samples excluded",
                                     text="The genotype matrix is now empty. Load new data or go back.",
                                     type="warning")
          } else if (did_merge && did_remove) {
            shinyWidgets::show_alert(title="Done", text="Merged selected samples and removed the rest", type="success")
          } else if (did_merge) {
            shinyWidgets::show_alert(title="Done", text="Merged selected samples.", type="success")
          } else if (did_remove) {
            shinyWidgets::show_alert(title="Done", text="Removed non-selected samples.", type="success")
          } else {
            shinyWidgets::show_alert(title="Nothing to do",
                                     text="Select at least one sample to keep", type="info")
          }
        })


        # name translation (sample_id -> designation, with F1 exceptions)
        translate_ind_names <- function(gl, ped_map, crosstype) {
          # ped_map: data.frame(sample_id, designation_id)
          if (is.null(ped_map) || !inherits(gl, "genlight")) return(gl)

          inds <- adegenet::indNames(gl)
          # final name defaults to current sample_id
          final <- inds

          # build mapping vectors aligned to inds
          m_idx <- match(inds, ped_map$sample_id)
          desig <- ped_map$designation_id[m_idx]

          et_status <- crosstype$status
          if (et_status == "has_f1") {
            ped <- data()$data$pedigree
            md  <- data()$metadata$pedigree
            sid_col <- md$value[md$parameter == "sample_id"]
            et_col  <- crosstype$col
            # cross type for each sample_id in inds
            et_vec <- ped[[et_col]][match(inds, ped[[sid_col]])]
            is_f1  <- toupper(as.character(et_vec)) == "F1"
            # non-F1: use designation if available
            use_desig <- !is_f1 & !is.na(desig) & nzchar(as.character(desig))
            final[use_desig] <- as.character(desig[use_desig])
            # F1 rows keep sample_id (no translation)
          } else {
            # no crossType or all NA or present without F1 -> translate all with available designation
            use_desig <- !is.na(desig) & nzchar(as.character(desig))
            final[use_desig] <- as.character(desig[use_desig])
          }

          adegenet::indNames(gl) <- final
          gl
        }


    })
  }
# Util functions pending to move ------------------------------------------
# get the column names of dart csv files
dart_getCols <- function(path) {
  # remove all rows staring with * character
  top_rows <- read.csv(path,
                       comment.char = "*",
                       nrows = 20)
  return(colnames(top_rows))
}

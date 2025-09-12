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
                            # Outputs
                            tableOutput(ns('summary_by_chrom')),
                            DT::DTOutput(ns('ind_summary')),
                            verbatimTextOutput(ns('geno_summary')),
                            actionButton(ns("ind_management_btn"), "Next")
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
        run_det_dups = FALSE
      )

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
                     geno_data = cgiarGenomics::read_DArTSeq_SNP(art_path = genotype_file,
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

      # just for make button reactive
      observeEvent(input$load_geno_btn,{
        geno_data <- get_geno_data()
        #add_data() Commented this during duplicate handelling dev REMOVE
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

        output$geno_summary <- renderText({
            temp <- data()
            ind_names <- adegenet::indNames(geno_data)
            if (!is.null(ind_names)) {
              glue::glue("Accessions in genotypic data:\n{length(ind_names)}")
            }
          })

        output$ind_summary <- DT::renderDT({
            DT::datatable(
              data.frame(sample_id = adegenet::indNames(geno_data)),
              options = list(
                dom = "lfrtip",          # 'f' = global search box
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

      observeEvent(input$ind_management_btn, {
        updateNavlistPanel(session = session,
                           inputId = "geno_load_navpanel",
                           selected = "3. Individual Management")
      })

      output$indManagementUI <- renderUI({
        req(dup_values$load_geno_data)
        gl <- get_geno_data()
        tags$div(
          shinydashboard::box(width = 12, title = "Duplicate Detection",
            fluidRow(column(width = 6,
                shinydashboard::box(title = "Parameters", solidHeader = TRUE,
                    status = "primary",
                    "Subsample the genotypic matrix to detect duplicates based on IBS",
                    br(), "This is an all vs all process, select a fraction of loci",
                    sliderInput(ns("n_loc_dup_slider"), "Select a fraction of markers:", min = 0, max = adegenet::nLoc(gl), value = 1),
                    sliderInput(ns("loc_miss_dup_slider"), "Set min locus missingness:", min = 0, max = 1, value = 0.2, step = 0.01),
                    sliderInput(ns("maf_dup_slider"), "Set min MAF:", min = 0, max = 1, value = 0.05, step = 0.01),
                    numericInput(ns("seed_dup"), "Random seed", value = 7, min = 0, step = 1),
                    actionButton(ns("run_dup_detect_btn"), "Run", status = "primary"))),
              column(width = 6,
                      uiOutput(ns("dup_detect_panel"))
                     ))

              ),
          shinydashboard::box(width = 12, title = "Duplicate Definition",
            fluidRow(column(width = 6,
              shinydashboard::box(title = "Duplicate Mapping", solidHeader = TRUE,
                status = "primary",
                "Upload a tab-separated file with sample_id and designation_id columns",
                br(),
                "Samples with same designation_id will be merged.",
                br(),
                downloadButton(ns("dup_det_download_template_btn"),
                                  "Download the template to map the duplicate samples"),
                fileInput(ns("dup_det_upl_template"), "Upload the modified template")
                )),
                column(width = 6,
                uiOutput(ns("dup_det_preview_panel"))))),
          shinydashboard::box(width = 12, title = "Merge Individuals",
            uiOutput(ns("dup_det_merge_panel"))),
          shinydashboard::box(width = 12, title = "Remove Individuals")
        )
      })

      observeEvent(input$run_dup_detect_btn, {
          shinyWidgets::ask_confirmation(
              inputId = ns("dup_det_conf"),
              text = "Detect duplicates is a computational demanding process. If you are confident you don't have duplicates avoid this step",
              title = "Duplicate detection warning"
            )
      })

      observeEvent(input$dup_det_conf, {
        if(isTRUE(input$dup_det_conf)){
          get_general_ibs()
        }
      })


      get_general_ibs <- reactive({
        req(dup_values$load_geno_data)
        req(input$n_loc_dup_slider)
        req(input$loc_miss_dup_slider)
        req(input$maf_dup_slider)
        req(input$seed_dup)

        gl <- get_geno_data()

        shinybusy::show_modal_spinner('fading-circle', text = 'Loading...')
        ibs_out <- cgiarGenomics::get_paired_IBS(gl, as.numeric(input$ploidlvl_input),
                       n_loci = input$n_loc_dup_slider,
                       seed = input$seed_dup,
                       maf = input$maf_dup_slider,
                       ind_miss = 0.2, loc_miss = input$loc_miss_dup_slider)
        output$dup_detect_panel <- renderUI({
          tags$div(
            tabsetPanel(type = "tabs",
                        tabPanel("Histogram",
                                 plotOutput(ns('dup_det_hist'))),
                        tabPanel("Heatmap",
                                 plotly::plotlyOutput(ns("dup_det_heatmap"))),
                        tabPanel("Log",
                                 verbatimTextOutput(ns("dup_det_log")))),
            DT::DTOutput(ns("dup_det_comps_tbl"))
          )
        })
        shinybusy::remove_modal_spinner()
        ibs_out
      })

      output$dup_det_hist <- renderPlot({
        ibs <- get_general_ibs()
        values <- as.vector(ibs$score)
        x <- values[is.finite(values)]
        n <- length(values)
        m <- mean(x)

        ggplot2::ggplot(data.frame(IBS = as.vector(ibs$score)), ggplot2::aes(IBS)) +
          ggplot2::geom_histogram(bins = 40, linewidth = 0.2) +
          ggplot2::geom_vline(xintercept = m, linetype = "dashed", linewidth = 0.6) +
          ggplot2::coord_cartesian(xlim = c(0, 1)) +
          ggplot2::labs(
            title = "IBS distribution (values near 1 ⇒ more related)",
            subtitle = sprintf("n = %d   mean = %.3f", n, m),
            x = "IBS", y = "Count of pairwise comparisons"
          ) +
          ggplot2::theme_minimal(base_size = 13) +
          ggplot2::theme(
            plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
            plot.subtitle = ggplot2::element_text(margin = ggplot2::margin(t = 4, b = 6))
          )
      }, res = 120)

      output$dup_det_comps_tbl <- DT::renderDT({
        ibs <- get_general_ibs()
        ibs_df <- as.data.frame(as.table(ibs$score)) %>%
          dplyr::filter(Var1 != Var2)

        DT::datatable(ibs_df,
          options = list(
            dom = "lfrtip",          # 'f' = global search box
            pageLength = 10,
            lengthMenu = c(5, 10, 25, 50)
          ),
          rownames = FALSE
        )
      })

      output$dup_det_heatmap <- plotly::renderPlotly({

        ibs <- get_general_ibs()
        m <- ibs$score

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

      output$dup_det_download_template_btn <- downloadHandler(
        filename = function() {
          paste0("dup_template-", Sys.Date(), ".csv")
        },
        content = function(file) {
          gl <- get_geno_data()
          template <- data.frame(sample_id = adegenet::indNames(gl), designation_id = NA)
          write.csv(template, file, row.names = FALSE, quote = F)
        }
      )

      output$dup_det_preview_panel <- renderUI({
        if (is.null(input$dup_det_upl_template)) {
          tags$div(
            style = "padding: 1rem; border: 1px dashed #bbb; border-radius: 8px;",
            h4("No file uploaded"),
            p("Please upload a .csv file to see a quick preview.")
          )
        } else {
          DT::DTOutput(ns("dup_det_preview_table"))
        }
      })


      read_dup_template <- reactive({
        req(input$dup_det_upl_template)
        df <- read.csv(input$dup_det_upl_template$datapath,
                       check.names = FALSE, stringsAsFactors = FALSE)
        if(sum(names(df) %in% c("sample_id", "designation_id")) != 2){
          validate("Uploaded file should have two columns named sample_id and designation_id separated by commas.")
        } else {
          gl <- get_geno_data()
          sample_ids <- adegenet::indNames(gl)
          if(sum(df$sample_id %in% sample_ids) != length(sample_ids)){
            validate("sample_ids provided in the template don't match with provided genotypic file")
          }
          return(df)
      }})

      output$dup_det_preview_table <- DT::renderDT({
        df <- read_dup_template()
        DT::datatable(df,
                      options = list(
                        dom = "lfrtip",          # 'f' = global search box
                        pageLength = 10,
                        lengthMenu = c(5, 10, 25, 50)
                      ),
                      rownames = FALSE
        )
      })

      output$dup_det_merge_panel <- renderUI({
        req(read_dup_template())
        duplicates <- read_dup_template()
        dup_groups <- duplicates %>%
          dplyr::group_by(designation_id) %>%
          dplyr::summarise(count = dplyr::n()) %>%
          dplyr::filter(count > 1)

        tags$div(
          selectInput(ns("dup_group_pick"), "Select a duplicate group",
                      choices = dup_groups$designation_id, multiple = FALSE),
          fluidRow(column(width = 6,
                          plotly::plotlyOutput(ns("dup_group_heatmap"))),
                   column(width = 6,
                          checkboxGroupInput(ns("dup_group_select_samples"),
                                             "Select samples to merge:", choices = NULL)))
        )
      })

      observe({
        req(read_dup_template())
        req(input$dup_group_pick)

        df <- read_dup_template()
        dup_samples <- df %>%
          dplyr::filter(designation_id == input$dup_group_pick) %>%
          dplyr::pull(sample_id)
        updateCheckboxGroupInput(session = session, "dup_group_select_samples",
                                 choices = dup_samples)
      })

      subset_IBS_matrix <- reactive({
        req(get_general_ibs())
        req(read_dup_template())
        req(input$dup_group_pick)

        ibs_mt <- get_general_ibs()
        df <- read_dup_template()
        dup_group <- input$dup_group_pick
        dup_samples <- df %>%
          dplyr::filter(designation_id == dup_group) %>%
          dplyr::pull(sample_id)

        sub_ibs <- ibs_mt$score[dup_samples, dup_samples]
        sub_ibs
      })

      output$dup_group_heatmap <- plotly::renderPlotly({

        m <- subset_IBS_matrix()

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

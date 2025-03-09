mod_qaGenoApp_ui <- function(id) {
  ns <- NS(id)
  tagList(shiny::mainPanel(
    width = 12,
    tabsetPanel(
      id = ns("tabsMain"),
      type = "tabs",
      tabPanel(
        "Information-QA-Geno",
        icon = icon("book"),
        br(),
        tags$body(uiOutput(ns("warningMessage")), )
      ),
      tabPanel(
        "Input Steps",
        icon = icon("book"),
        # Parameter table and histogram tabset
        fluidRow(# Parameter table
          column(
            width = 5,
            tags$div(
              uiOutput(ns('filt_seq_params')),
              inputPanel(
                selectInput(ns('filt_param'), 'Parameter:', choices = NULL),
                selectInput(ns('filt_op'), 'Filter:', choices = c(">", ">=", "<", "<=")),
                numericInput(
                  ns('filt_tresh'),
                  'Threshold:',
                  value = NULL,
                  min = 0,
                  max = 1,
                  step = 0.01
                )
              ),
              actionButton(ns('add_filt'), 'Add'),
              actionButton(ns('del_filt'), 'Delete'),
              actionButton(ns('preview_filt'), 'Preview Filters'),
              actionButton(ns('reset_filt'), 'Reset'),
              verbatimTextOutput(ns("filt_seq_error"))
            )
          ), # Histogram tabset
          column(
            width = 7, tabsetPanel(
              type = "tabs",
              tabPanel(
                "Loci",
                tabsetPanel(
                  id = ns('loc_metric_panel'),
                  type = 'pills',
                  tabPanel(title = "MAF", plotly::plotlyOutput(ns("hist_maf"))),
                  tabPanel(title = "Missingness", plotly::plotlyOutput(ns("hist_loc_miss"))),
                  tabPanel(title = "Heterozygosity", plotly::plotlyOutput(ns("hist_loc_het"))),
                  tabPanel(title = "Inbreeding", plotly::plotlyOutput(ns("hist_loc_Fis")))
                )
              ),
              tabPanel(
                "Ind",
                tabsetPanel(
                  id = ns('ind_metric_panel'),
                  type = 'pills',
                  tabPanel(title = "Missingness", plotly::plotlyOutput(ns("hist_ind_miss"))),
                  tabPanel(title = "Heterozygosity", plotly::plotlyOutput(ns("hist_ind_het")))
                )
              )
            )
          ), ),
        # Summary table
        fluidRow(
          br(),
          column(width = 4, DT::DTOutput(
            ns('ov_summary_tab')
        ))),
        fluidRow(
          br(),
          column(width = 8, DT::DTOutput(
            ns('filter_log_tab')
          ))
        )
      ),
      tabPanel("Imputation", icon = icon("gears"), br(),
               fluidRow(
                 column(width = 4,
                        verbatimTextOutput(ns('pre_imp_metrics'))
                        ),
                 column(width = 8,
                        inputPanel(
                          selectInput(
                            ns("imputationMethod"),
                            "Imputation method",
                            choices = c("frequency"),
                            multiple = FALSE
                          ),
                          actionButton(ns('run_imputation'),
                                  'Apply Imputation')
                          ),
                        )
               )),
      tabPanel("Output", icon = icon("book"), br(), fluidRow(
        style = "background-color:grey; color: #FFFFFF",
        column(
          width = 3,
            textInput(
              ns("analysisIdName"),
              label = tags$i(
                class = "glyphicon glyphicon-info-sign",
                style = "color:#FFFFFF; float:left",
                title = "An optional name for the analysis besides the timestamp if desired."
              ))),
        column(width = 2,
               actionButton(
                 ns("runQaMb"),
                 "Identify & store modifications",
                 icon = icon("play-circle")
               )),
        column(width = 1),
        column(
          width = 6,
          shinydashboard::box(
            width = 12,
            status = "success",
            background = "green",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            title = "Additional run settings...",
          ),
        ),
      ),
      textOutput(ns("outQaMb")),
      downloadButton(ns("downloadReportQaGeno"), "Download dashboard"),
      br(),
      uiOutput(ns('reportQaGeno'))
      )
    )))
}

mod_qaGenoApp_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # output$plotDataDependencies <- shiny::renderPlot({ dependencyPlot() })
    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })
    ############################################################################
    # show shinyWidgets until the user can use the module
    observeEvent(c(data()), {
      req(data())
      if(!is.null(data()$data$geno)){
        golem::invoke_js('showid', ns('holder'))
      }else{
        golem::invoke_js('hideid', ns('holder'))
      }
    })
    # warning message
    output$warningMessage <- renderUI(if (is.null(data())) {
      HTML(as.character(
        div(
          style = "color: red; font-size: 20px;",
          "Please retrieve or load your data using the 'Data' tab."
        )
      ))
    } else{
      # data is there
      if (!is.null(data()$data$geno)) {
        HTML(as.character(
          div(
            style = "color: green; font-size: 20px;",
            "Data is complete, please proceed to perform the marker QA specifying your input parameters under the Input tabs."
          )
        ))
      } else{
        HTML(as.character(
          div(
            style = "color: red; font-size: 20px;",
            "Please retrieve or load your genotype data using the 'Data' tab. "
          )
        ))
      }
    })

    #  this reactive object will store all parameters for the QA module

    geno_qa_data <- reactiveValues(geno = NULL,
                                   filter = FALSE,
                                   preview_geno = NULL,
                                   filter_log = NULL,
                                   imputation_log = NULL,
                                   metric_mappings = list(
                                     "MAF" = 'maf',
                                     "Missingness by locus" = 'loc_miss',
                                     "Heterozygosity by locus" ='loc_het',
                                     "Inbreeding by locus" = 'loc_Fis',
                                     "Missingness by individual" = 'ind_miss',
                                     "Heterozygosity by individual" = 'ind_het'
                                   ))
    geno_qa_data$filt_seq <- data.frame()
    geno_qa_data$overall_summary <- data.frame()


    # Recipe to specify the filtering sequence table output
    output$filt_seq_params <- renderUI({
      helper_message <- tags$h4(
        "Add filters using the add button. For remove click the filter \n
              on the table and after click delete."
      )
      if (is.data.frame(geno_qa_data$filt_seq)) {
        if (nrow(geno_qa_data$filt_seq) > 0) {
          DT::DTOutput(ns('filt_seq_tab'))
        } else {
          helper_message
        }
      } else {
        helper_message
      }
    })

    proxy_seq_tab <- DT::dataTableProxy(ns("filt_seq_tab"))

    observeEvent(input$filt_seq_tab_cell_edit, {
      info <- input$filt_seq_tab_cell_edit  # Get edit info
      i <- info$row                         # Row index
      j <- info$col                         # Column index
      newValue <- as.numeric(info$value)    # New value (as numeric for this example)
      # Validate the new value (example: age must be between 18 and 65)
      if (newValue %% 1 == 0) {
        # Show validation error
        output$filt_seq_error <- renderText("Invalid treshold. Must be between [0-1]")
        # Revert change by resetting the table
        DT::replaceData(proxy_seq_tab, geno_qa_data$filt_seq, resetPaging = FALSE)
      } else {
        # Update data if validation passes
        updatedData <- geno_qa_data$filt_seq
        updatedData[i, j] <- info$value
        geno_qa_data$filt_seq <- updatedData
      }
    })

    output$filt_seq_tab <- DT::renderDT({
      DT = geno_qa_data$filt_seq
    }, selection = "single", editable = list(target = "cell", disable = list(columns = c(1, 2))), options = list(
      paging = FALSE,
      searching = FALSE,
      info = FALSE
    ))

    output$ov_summary_tab <- DT::renderDataTable({
      summary_table = geno_qa_data$overall_summary
      DT::datatable(summary_table,
                    colnames = c("Dataset", "No. Loci", "No. Inds", "Overall Miss", 'Mean Ind. Het', "Mean MAF"),
                    options = list(escape = F,
                                   searching = F,
                                   info = F,
                                   filter = F,
                                   padding = F))
    })


    output$filter_log_tab <- DT::renderDT({
      DT = geno_qa_data$filter_log
    })



    observeEvent(data()$data$geno,{

      # requered the unfiltered Data
      req(data()$data$geno)
      # Store in the reactive obj the available ind and loc metrics
      geno_qa_data$geno <- isolate(data()$data$geno)
      geno_qa_data$loc_metrics <- colnames(geno_qa_data$geno@other$loc.metrics)
      geno_qa_data$ind_metrics <- colnames(geno_qa_data$geno@other$ind.metrics)

      updateSelectInput(
        inputId = 'filt_param',
        choices = geno_qa_data$metric_mappings
      )

      plot_metrics()

      ov_summary <- cgiarGenomics::get_overall_summary(geno_qa_data$geno)

      added_row <- data.frame(
        dataset = 'Original',
        nloc = ov_summary$nloc,
        nind = ov_summary$nind,
        ov_miss = round(ov_summary$ov_miss, 2),
        ov_het = round(ov_summary$ov_het, 2),
        ov_maf = round(ov_summary$ov_maf, 2)
      )

      geno_qa_data$overall_summary <- rbind(geno_qa_data$overall_summary, added_row)

    })

    plot_metrics <- reactive({
      # Merge all metrics into single vector
      metrics <- c(geno_qa_data$loc_metrics, geno_qa_data$ind_metrics)
      # Inside this anonymous func for each input metric:
      # - create the output_id for the histogram
      # - get the metric values in the raw and filtered dataset
      # - plot the data
      lapply(metrics, function(i_metric) {
        output_plot_id <- glue::glue("hist_{i_metric}")
        margin <- cgiarGenomics::get_parameter_margin(geno_qa_data$geno, i_metric)

        if (margin == 'loc') {
          unfilt_metric <- c(geno_qa_data$geno@other$loc.metrics[, i_metric])
        } else {
          unfilt_metric <- c(geno_qa_data$geno@other$ind.metrics[, i_metric])
        }

        output[[output_plot_id]] <- plotly::renderPlotly({
          plot <- plotly::plot_ly(x = unfilt_metric,
                                  type = "histogram",
                                  name = "Original Data")

          if (geno_qa_data$filter) {
            if (margin == 'loc') {
              filt_metric <- c(geno_qa_data$preview_geno$gl@other$loc.metrics[, i_metric])

            } else {
              filt_metric <- c(geno_qa_data$preview_geno$gl@other$ind.metrics[, i_metric])

            }


            plot <- plot %>%
              plotly::add_trace(x = filt_metric,
                                type = "histogram",
                                name = "Filtered Data")
          }
          return(plot)
        })
      })
    })

    add_filt_seq_row <- observeEvent(input$add_filt, {
      req(input$filt_param)
      req(input$filt_op)
      req(input$filt_tresh)

      if (input$filt_tresh <= 0 || input$filt_tresh %% 1 == 0) {
        output$filt_seq_error <- renderText("Threshold needs to be greather than zero and decimal")
      } else {
        added_row <- data.frame(
          Parameter = isolate(input$filt_param),
          Filter = isolate(input$filt_op),
          Threshold = isolate(input$filt_tresh)
        )

        geno_qa_data$filt_seq <- rbind(geno_qa_data$filt_seq, added_row)
      }
    })

    observeEvent(input$del_filt, {
      if (!is.null(input$filt_seq_tab_rows_selected)) {
        geno_qa_data$filt_seq <- geno_qa_data$filt_seq[-as.numeric(input$filt_seq_tab_rows_selected), ]
      }
    })

    observeEvent(input$preview_filt, {
      print("filter original gl")

      if (is.null(nrow(geno_qa_data$filt_seq))) {
        output$filt_seq_error <- renderText("Please first indicate a filtering sequence!")
      } else {
        output$filt_seq_error <- renderText("")
        gl <- isolate(geno_qa_data$geno)

        filt_seq <- get_filtering_sequence(geno_qa_data$filt_seq)
        preview_gl <- cgiarGenomics::apply_sequence_filtering(gl, filt_seq)

        geno_qa_data$preview_geno <- preview_gl
        preview_gl_summary <- cgiarGenomics::get_overall_summary(preview_gl$gl)

        added_row <- data.frame(
          dataset = 'Filtered',
          nloc = preview_gl_summary$nloc,
          nind = preview_gl_summary$nind,
          ov_miss = round(preview_gl_summary$ov_miss, 2),
          ov_het = round(preview_gl_summary$ov_het, 2),
          ov_maf = round(preview_gl_summary$ov_maf, 2)
        )
        if (!geno_qa_data$filter) {
          geno_qa_data$overall_summary <- rbind(geno_qa_data$overall_summary, added_row)
        } else {
          geno_qa_data$overall_summary <- rbind(geno_qa_data$overall_summary[1, ], added_row)

        }

        geno_qa_data$filter_log <- get_filter_log(preview_gl$filt_log)
      }

      if (!geno_qa_data$filter) {
        geno_qa_data$filter <- TRUE
      }



    })

    observeEvent(input$reset_filt, {
      geno_qa_data$filter <- FALSE
      geno_qa_data$filt_seq <- data.frame()
      geno_qa_data$overall_summary <- geno_qa_data$overall_summary[1, ]
      geno_qa_data$filter_log <- data.frame()
    })

    observeEvent(input$runQaMb,{
      req(geno_qa_data$preview_geno)
      req(geno_qa_data$imputation_log)
      print('identify mods')
      shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')


      filter_mods <- isolate(geno_qa_data$filter_log)
      filter_mods$analysisId <- as.numeric(Sys.time())
      filter_mods$analysisIdName <- input$analysisIdName
      filter_mods$module <- "qaGeno"

      up_analysis_id <- as.character(filter_mods$analysisId[nrow(filter_mods)])

      results <- data()

      # Filter modifications
      if(!is.null(results$modifications$geno)){
         results$modifications$geno <- rbind(results$modifications$geno, filter_mods)
      } else {
         results$modifications$geno <- filter_mods
      }

      # Imputation modifications
      if(!is.null(results$modifications$geno_imp)){
         results$modifications$geno_imp[[up_analysis_id]] <- geno_qa_data$imputation_log$log
      } else {
         results$modifications$geno_imp <- list()
         results$modifications$geno_imp[[up_analysis_id]] <- geno_qa_data$imputation_log$log
      }

      # Output gl object
      if(!is.null(results$data$geno_imp)){
         results$data$geno_imp[[up_analysis_id]] <- geno_qa_data$imputation_log$gl
      } else {
         results$data$geno_imp <- list()
         results$data$geno_imp[[up_analysis_id]] = geno_qa_data$imputation_log$gl
      }

      newStatus <- data.frame(module="qaGeno", analysisId=filter_mods$analysisId[nrow(filter_mods)], analysisIdName=input$analysisIdName)
      print(up_analysis_id)
      if(!is.null(results$status)){
        results$status <- rbind(results$status, newStatus)
      }else{results$status <- newStatus}
      data(results)
      output$outQaMb <- renderPrint({
        cat(paste("Modifications to genotype information saved with id:",as.POSIXct(newStatus$analysisId[1], origin="1970-01-01", tz="GMT")))
      })

      shinybusy::remove_modal_spinner()
    })

    # Imputation reactive expressions
    output$pre_imp_metrics <- renderPrint(

      if(geno_qa_data$filter){
        filt_data <- geno_qa_data$overall_summary[2,]

        # Always the second row will be poidity
        ploidity <- as.numeric(data()$metadata$geno[2,]$value)
        nas_number <- sum(adegenet::glNA(geno_qa_data$preview_geno$gl)/ploidity)
        info_text <- glue::glue("General Information
                                Ploidity level: {ploidity}
                                Number of loci: {filt_data$nloc}
                                Number of individuals: {filt_data$nind}
                                Number of imputations: {nas_number}
                                Overall Missing rate: {filt_data$ov_miss}
                                Minimum MAF: {round(min(geno_qa_data$preview_geno$gl@other$loc.metrics$maf), 4)}")
        cat(info_text)
      } else {
        helper_message <- "To use the imputation module first filter the genotypic matrix"
        cat(helper_message)
      }
    )


    observeEvent(input$run_imputation, {
      req(geno_qa_data$preview_geno$gl)
      ploidity <- as.numeric(data()$metadata$geno[2,]$value)
      # Imputation
      shinybusy::show_modal_spinner('fading-circle', text = 'Imputing filtered genotyope matrix...')
      geno_qa_data$imputation_log <- cgiarGenomics::impute_gl(gl = geno_qa_data$preview_geno$gl,
                                           ploidity = ploidity,
                                           method = input$imputationMethod)

      shinybusy::remove_modal_spinner()
    })

    # output$reportQaGeno <- renderUI({
    #   HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportQaGeno.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
    # })

    # output$downloadReportQaGeno <- downloadHandler(
    #   filename = function() {
    #     paste(paste0('qaGeno_dashboard_',gsub("-", "", Sys.Date())), sep = '.', switch(
    #       "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
    #     ))
    #   },
    #   content = function(file) {
    #     src <- normalizePath(system.file("rmd","reportQaGeno.Rmd",package="bioflow"))
    #     src2 <- normalizePath('data/resultQaGeno.RData')
    #     # temporarily switch to the temp dir, in case you do not have write
    #     # permission to the current working directory
    #     owd <- setwd(tempdir())
    #     on.exit(setwd(owd))
    #     file.copy(src, 'report.Rmd', overwrite = TRUE)
    #     file.copy(src2, 'resultQaGeno.RData', overwrite = TRUE)
    #     shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
    #     out <- rmarkdown::render('report.Rmd', params = list(toDownload=TRUE),switch(
    #       "HTML",
    #       HTML = rmdformats::robobook(toc_depth = 4)
    #       # HTML = rmarkdown::html_document()
    #     ))
    #     shinybusy::remove_modal_spinner()
    #     file.rename(out, file)
    #   }
    # )

    get_filtering_sequence <- function(filt_seq_df) {
      print("is getting the indicated filterting seq")
      if (!is.data.frame(filt_seq_df)) {
        cli::cli_abort("`filt_seq_df` is not a data.frame object is: {class(filt_seq_df)}")
      }
      filt_seq <- apply(filt_seq_df,
                        1,
                        \(.x) list(
                          param = .x[1],
                          operator = .x[2],
                          threshold = as.numeric(.x[3])
                        ))
      return(filt_seq)
    }

    get_filter_log <- function(filter_step_log){
      print("filter_processing...")
      base_loc_names <- adegenet::locNames(data()$data$geno)
      base_ind_names <- adegenet::indNames(data()$data$geno)

      out <- purrr::map_df(filter_step_log, function(filter_step){

        if(length(filter_step$filter_out) > 0){

          reason <- paste(filter_step$filter_margin,
                          filter_step$param,
                          filter_step$operator,
                          filter_step$threshold,
                          sep = '_')

          if(filter_step$filter_margin == 'loc'){
            loc_idx <- which(filter_step$filter_out %in% base_loc_names)
            col_data <- loc_idx
            row_data <- rep(NA, length(loc_idx))

          } else {
            ind_idx <- which(filter_step$filter_out %in% base_ind_names)
            col_data <- rep(NA, length(ind_idx))
            row_data <- ind_idx
          }
        }

          filt_step_log <- data.frame(
            reason = rep(reason, length(filter_step$filter_out)),
            row = row_data,
            col = col_data,
            value = rep(NA, length(filter_step$filter_out))
          )
          return(filt_step_log)
      })
     return(out)
    }
  })
}

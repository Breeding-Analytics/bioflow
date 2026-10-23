#' staApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_staApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    tags$br(),

    mainPanel( width = 12,
               tabsetPanel(id=ns("tabsMain"),
                           type = "tabs",

                           tabPanel(div(icon("book"), "Information") ,
                                    br(),
                                    column(width = 6,
                                           h1(strong(span("Single Occurrence Analysis Module", tags$a(href="https://www.youtube.com/watch?v=fqUTdvbzNSE&list=PLZ0lafzH_UmclOPifjCntlMzysEB2_2wX&index=6", icon("youtube") , target="_blank"), style="color:darkcyan"))),
                                           h2(strong("Data Status (wait to be displayed):")),
                                           uiOutput(ns("warningMessage")),
                                           tags$br(),
                                           img(src = "www/geospatial.jpg", height = 250, width = 500), # add an image
                                    ),

                                    column(width = 6,
                                           h2(strong("Details")),
                                           p("The genetic evaluation approach we use known as 'two-step' first analyze trait by trait and trial by trial
                                          to remove the spatial noise from experiments using experimental factors like blocking and spatial coordinates.
                                          Each trial is one level of the environment
                              column (defined when the user matches the expected columns to columns present in the initial phenotypic input file).
                              Genotype is fitted as both, fixed and random. The user defines which should be returned in the predictions table.
                              By default genotype (designation column) predictions and their standard errors are returned."),
                                           h2(strong("References")),
                                           p("Velazco, J. G., Rodriguez-Alvarez, M. X., Boer, M. P., Jordan, D. R., Eilers, P. H., Malosetti, M., & Van Eeuwijk, F. A. (2017).
                                Modelling spatial trends in sorghum breeding field trials using a two-dimensional P-spline mixed model. Theoretical and Applied
                                Genetics, 130, 1375-1392."),
                                           p("Rodriguez-Alvarez, M. X., Boer, M. P., van Eeuwijk, F. A., & Eilers, P. H. (2018). Correcting for spatial heterogeneity in plant
                                breeding experiments with P-splines. Spatial Statistics, 23, 52-71."),
                                           h2(strong("Software used")),
                                           p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
                                Vienna, Austria. URL https://www.R-project.org/."),
                                           p("Boer M, van Rossum B (2022). _LMMsolver: Linear Mixed Model Solver_. R package version 1.0.4.9000."),
                                           # column(width = 12, shiny::plotOutput(ns("plotDataDependencies")), ),
                                    ),
                           ),
                           
                           tabPanel(div(icon("arrow-right-from-bracket"), "Output tabs" ) , value = "outputTabs",
                                    tabsetPanel(
                                      tabPanel("Dashboard", icon = icon("file-image"),
                                               br(),
                                               textOutput(ns("outSta2")),
                                               br(),
                                               downloadButton(ns("downloadReportSta"), "Download dashboard"),
                                               uiOutput(ns('ebsReportButton')),
                                               br(),
                                               uiOutput(ns('reportSta')),
                                      ),
                                      tabPanel("Predictions", icon = icon("table"),
                                               br(),
                                               DT::DTOutput(ns("predictionsSta")),
                                      ),
                                      tabPanel("Metrics", icon = icon("table"),
                                               br(),
                                               DT::DTOutput(ns("metricsSta")),
                                      ),
                                      tabPanel("Modeling", icon = icon("table"),
                                               br(),
                                               DT::DTOutput(ns("modelingSta")),
                                      ),
                                    ) # of of tabsetPanel
                           )# end of output panel
               )) # end mainpanel
  )
}

#' staApp Server Functions
#'
#' @noRd
mod_staApp_server <- function(id,data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })

    ############################################################################
    # warning message
    output$warningMessage <- renderUI(
      if(is.null(data())){
        HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your phenotypic data using the 'Data Retrieval' tab.")) )
      }else{
        if("sta" %in% data()$status$module){
          HTML( as.character(div(style="color: green; font-size: 20px;", "Single Occurrence Analysis results are ready. See the report under the 'Output tabs'.")) )
        }else{
          HTML( as.character(div(style="color: red; font-size: 20px;", "Please run the Model-Based QA/QC module first. The Single Occurrence Analysis is computed there and its report is shown here automatically.")) )
        }
      }
    )

    ## latest Single-Trial Analysis (sta) stamp available in the object
    latestStaId <- reactive({
      req(data())
      ids <- data()$status$analysisId[which(data()$status$module == "sta")]
      if(length(ids) == 0) return(NULL)
      ids[length(ids)]
    })

    ## When a new STA result appears (produced by the Model-Based QA module right
    ## after tagging outliers and running staLMM) switch to the Output tab so the
    ## report shows up automatically. The analysis is NOT re-run here.
    lastStaShown <- reactiveVal(NULL)
    observe({
      id <- latestStaId()
      req(id)
      if(!is.null(lastStaShown()) && isTRUE(as.character(lastStaShown()) == as.character(id))) return()
      lastStaShown(id)
      updateTabsetPanel(session, "tabsMain", selected = "outputTabs")
    })

    output$outSta2 <- renderPrint({
      id <- latestStaId()
      if(is.null(id)){ cat("") }
      else { cat(paste("Single Occurrence Analysis with id:", as.POSIXct(as.numeric(id), origin = "1970-01-01", tz = "GMT"), "is ready.")) }
    })

    ## Predictions / Metrics / Modeling of the latest STA result
    output$predictionsSta <- DT::renderDT({
      req(latestStaId())
      result <- data()
      ids <- data()$status$analysisId[which(data()$status$module == "sta")]
      ids<-ids[length(ids)]
      predictions <- result$predictions
      predictions <- predictions[predictions$module == "sta", ]
      predictions$analysisId <- as.numeric(predictions$analysisId)
      predictions <- predictions[!is.na(predictions$analysisId), ]
      current.predictions <- predictions[predictions$analysisId == ids, ]
      current.predictions <- subset(current.predictions, select = -c(module, analysisId))
      numeric.output <- c("predictedValue", "stdError", "reliability")
      DT::formatRound(DT::datatable(current.predictions, extensions = 'Buttons',
                                    options = list(dom = 'Blfrtip', scrollX = TRUE, buttons = c('copy','csv','excel','pdf','print'),
                                                   lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
      ), numeric.output)
    }, server = FALSE)

    output$metricsSta <- DT::renderDT({
      req(latestStaId())
      result <- data()
      ids <- data()$status$analysisId[which(data()$status$module == "sta")]
      ids<-ids[length(ids)]
      metrics <- result$metrics
      metrics <- metrics[metrics$module == "sta", ]
      metrics$analysisId <- as.numeric(metrics$analysisId)
      metrics <- metrics[!is.na(metrics$analysisId), ]
      current.metrics <- metrics[metrics$analysisId == ids, ]
      current.metrics <- subset(current.metrics, select = -c(module, analysisId))
      numeric.output <- c("value", "stdError")
      DT::formatRound(DT::datatable(current.metrics, extensions = 'Buttons',
                                    options = list(dom = 'Blfrtip', scrollX = TRUE, buttons = c('copy','csv','excel','pdf','print'),
                                                   lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
      ), numeric.output)
    }, server = FALSE)

    output$modelingSta <- DT::renderDT({
      req(latestStaId())
      result <- data()
      ids <- data()$status$analysisId[which(data()$status$module == "sta")]
      ids<-ids[length(ids)]
      modeling <- result$modeling
      modeling <- modeling[modeling$module == "sta", ]
      modeling$analysisId <- as.numeric(modeling$analysisId)
      modeling <- modeling[!is.na(modeling$analysisId), ]
      current.modeling <- modeling[modeling$analysisId == ids, ]
      current.modeling <- subset(current.modeling, select = -c(module, analysisId))
      DT::datatable(current.modeling, extensions = 'Buttons',
                    options = list(dom = 'Blfrtip', scrollX = TRUE, buttons = c('copy','csv','excel','pdf','print'),
                                   lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
      )
    }, server = FALSE)

    ## STA report (shown automatically once an STA result exists in the object)
    output$reportSta <- renderUI({
      req(lastStaShown())
      HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportSta.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
    })

    output$downloadReportSta <- downloadHandler(
      filename = function() {
        paste(paste0('sta_dashboard_',gsub("-", "", as.integer(Sys.time()))), sep = '.', switch(
          "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
        ))
      },
      content = function(file) {
        shinybusy::show_modal_spinner(spin = "fading-circle", text = "Generating Report...")
        src <- normalizePath(system.file("rmd","reportSta.Rmd",package="bioflow"))
        # the data object was already saved to tempdir() by the run step, and the
        # report looks it up there, so no extra copy of the .RData is needed.
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'report.Rmd', overwrite = TRUE)
        out <- rmarkdown::render('report.Rmd', params = list(toDownload=TRUE), switch(
          "HTML",
          HTML = rmdformats::robobook(toc_depth = 4)
        ))
        wait.time <- 0
        while (!file.exists(out) && wait.time < 60) { Sys.sleep(1); wait.time <- wait.time + 1 }
        file.rename(out, file)
        shinybusy::remove_modal_spinner()
      }
    )

    output$ebsReportButton <- renderUI({
      query <- parseQueryString(session$clientData$url_search)

      # check if task id exists, and verify (sanitize) this md5 parameter ;-)
      if (is.character(query$task) &&
          is.character(query$domain) &&
          grepl("^[a-f0-9]{32}$", query$task) &&
          grepl("^[a-z_0-9\\.\\-]+$", query$domain)) {

        s3 <- paws::s3()

        bucket_name <- "ebs-bioflow"
        s3_object_path <- paste0(query$domain, "/", query$task, ".RData")

        tryCatch({
          s3_object_head <- s3$head_object(Bucket = bucket_name, Key = s3_object_path)
          actionButton(ns("ebsStaReport"), "Submit Results to EBS")
        }, error = function(e) {
          # no such file on the S3 bucket clipboard
          NULL
        })
      } else {
        # parameters does not match the expected format
        NULL
      }
    })

    observeEvent(input$ebsStaReport, {
      req(data())

      query <- parseQueryString(session$clientData$url_search)

      # check if task id exists, and verify (sanitize) this md5 parameter ;-)
      if (is.character(query$task) &&
          is.character(query$domain) &&
          grepl("^[a-f0-9]{32}$", query$task) &&
          grepl("^[a-z_0-9\\.\\-]+$", query$domain)) {

        shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')

        result <- data()
        s3 <- paws::s3()

        # set S3 bucket parameters
        bucket_name <- "ebs-bioflow"
        s3_object_path <- paste0(query$domain, "/", query$task, ".RData")

        tryCatch({
          # update S3 metadata
          s3_object_head <- s3$head_object(Bucket = bucket_name, Key = s3_object_path)
          s3_object_metadata <- s3_object_head$Metadata
          s3_object_metadata[["Upload-Origin"]] <- "bioflow"

          temp_file <- paste0(tempdir(), "/", query$task, ".RData")
          save(result, file = temp_file)

          # upload data object to S3 bucket
          s3$put_object(
            Body = temp_file,
            Bucket = bucket_name,
            Key = s3_object_path,
            Metadata = s3_object_metadata
          )

          shinybusy::remove_modal_spinner()
          shinyalert::shinyalert(title = "Success!", text = "Analysis results successfully submitted to EBS.", type = "success")
        }, error = function(e) {
          shinybusy::remove_modal_spinner()
          shinyalert::shinyalert(title = "Failed!", text = e$message, type = "error")
        })
      }
    })

  }) ## end moduleserver
}

## To be copied in the UI
# mod_staApp_ui("staApp_1")

## To be copied in the server
# mod_staApp_server("staApp_1")

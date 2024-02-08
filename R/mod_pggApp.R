#' pggApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pggApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    sidebarPanel(
      # input <- list(version2Pgg="1699508839.68847",trait2Pgg="desireIndex",entryType2Pgg= "TGV_EST004D#TEST_tested",nCrossPgg="20", targetAnglePgg="30",maxRun=40, relType="grm", env2Pgg="across",verbosePgg=TRUE )
      tags$style(".well {background-color:grey; color: #FFFFFF;}"),
      HTML("<img src='www/cgiar3.png' width='42' vspace='10' hspace='10' height='46' align='top'>
                  <font size='5'>Predicted Genetic Gain</font>"),
      # div(tags$p(h4(strong("Predicted Genetic Gain")))),#, style = "color: #817e7e"
      hr(style = "border-top: 1px solid #4c4c4c;"),
      selectInput(ns("version2Pgg"), "Index or MTA version to analyze", choices = NULL, multiple = FALSE),
      selectInput(ns("trait2Pgg"), "Trait(s) to use", choices = NULL, multiple = TRUE),
      selectInput(ns("environmentToUse"), "Environments to use", choices = NULL, multiple = TRUE),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      shinydashboard::box(width = 12, status = "success", background="green",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Settings...",
                          numericInput(ns("proportion"), label = "Proportion selected (%)", value = 10, step = 10, max = 100, min = 1),
                          selectInput(ns("verbose"), label = "Print logs?", choices = list(TRUE,FALSE), selected = FALSE, multiple=FALSE)
      ),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      actionButton(ns("runPgg"), "Run", icon = icon("play-circle")),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      uiOutput(ns("qaQcPggInfo")),
      textOutput(ns("outPgg"))
    ), # end sidebarpanel
    mainPanel(tabsetPanel(
      type = "tabs",
      tabPanel(p("Information",class="info-p"),  icon = icon("book"),
               br(),
               shinydashboard::box(status="success",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,   style = "height:800px; overflow-y: scroll;overflow-x: scroll;",
                                          h2(strong("Status:")),
                                          uiOutput(ns("warningMessage")),
                                          h2(strong("Details")),
                                          p("In order to monitor the efficacy of genetic evaluation in the current cycle of selection the predicted genetic gain formula is used.
                                          This option aims to calculate the predicted genetic gain from the classical breeders' equation
                              R = i*r*s being R the response to selection, i the selection intensity, r the selection accuracy, s the genetic standard deviation.
                                The way the options are used is the following:"),
                                          img(src = "www/pgg.png", height = 250, width = 500), # add an image
                                          p(strong("Trait for cross prediction.-")," Trait to be be used for predicting all possible crosses (an index is suggested)."),
                                          p(strong("Year to analyze.-")," Year of data to be used from the input file."),
                                          p(strong("Fieldinst.-")," Field instance of data to be used from the input file."),
                                          p(strong("Percentage selected.-")," Percentage of parents to be selected to be used to calculate the selection intensity."),
                                          p(strong("Cycle time (idealized).-")," Idealized cycle time to be used as denominator to calculate the rate of response to selection."),
                                          h2(strong("References:")),
                                          p("Lush, J. L. (2013). Animal breeding plans. Read Books Ltd."),
                                          p("Mrode, R. A. (2014). Linear models for the prediction of animal breeding values. Cabi."),
                                          h2(strong("Software used:")),
                                          p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
                                Vienna, Austria. URL https://www.R-project.org/.")
                                   )
               )
      ),
      tabPanel(p("Input",class="input-p"), icon = icon("arrow-right-to-bracket"),
               tabsetPanel(
                 tabPanel("Metrics view", icon = icon("table"),
                          br(),
                          shinydashboard::box(status="success",width = 12,solidHeader = TRUE,
                                              column(width=6,selectInput(ns("traitMetrics"), "Trait to visualize", choices = NULL, multiple = TRUE) ),
                                              column(width=6,selectInput(ns("parameterMetrics"), "Parameter to visualize", choices = NULL, multiple = FALSE) ),
                                              column(width=12, plotly::plotlyOutput(ns("barplotPredictionsMetrics")) )
                          )
                 ),
                 tabPanel("Data", icon = icon("table"),
                          br(),
                          shinydashboard::box(status="success",width = 12,
                                              solidHeader = TRUE,
                                              column(width=12,DT::DTOutput(ns("phenoPgg")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
                          )
                 )
               )
      ),
      tabPanel(p("Output",class="output-p"), icon = icon("arrow-right-from-bracket"),
               tabsetPanel(
                 tabPanel("Metrics", icon = icon("table"),
                          br(),
                          shinydashboard::box(status="success",width = 12,
                                              solidHeader = TRUE,
                                              column(width=12,br(),DT::DTOutput(ns("metricsPgg")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
                          )
                 ),
                 tabPanel("Modeling", icon = icon("table"),
                          br(),
                          shinydashboard::box(status="success",width = 12,
                                              solidHeader = TRUE,
                                              column(width=12,DT::DTOutput(ns("modelingPgg")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
                          )
                 ),
                 tabPanel("Report", icon = icon("file-image"),
                          br(),
                          div(tags$p("Please download the report below:") ),
                          downloadButton(ns("downloadReportPgg"), "Download report"),
                          br(),
                          uiOutput(ns('reportPgg'))
                 )
               )
      )# end of output panel
    )) # end mainpanel

  )
}

#' pggApp Server Functions
#'
#' @noRd
mod_pggApp_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })
    ############################################################################
    # data = reactive({
    #   load("~/Documents/bioflow/dataStr0.RData")
    #   data <- res
    #   return(data)
    # })
    ## warning message
    output$warningMessage <- renderUI(
      if(is.null(data())){
        HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your phenotypic data using the 'Data Retrieval' tab.")) )
      }else{ # data is there
        if("mta" %in% data()$status$module){
          HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to perform the predicted genetic gain inspecting the other tabs.")) )
        }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please perform MTA before performing a predicted genetic gain analysis.")) ) }
      }
    )
    #################
    ## version
    observeEvent(c(data()), {
      req(data())
      dtPgg <- data()
      dtPgg <- dtPgg$status
      dtPgg <- dtPgg[which(dtPgg$module %in% c("mta","indexD")),]
      traitsPgg <- unique(dtPgg$analysisId)
      if(length(traitsPgg) > 0){names(traitsPgg) <- as.POSIXct(traitsPgg, origin="1970-01-01", tz="GMT")}
      updateSelectInput(session, "version2Pgg", choices = traitsPgg)
    })
    #################
    ## traits
    observeEvent(c(data(), input$version2Pgg), {
      req(data())
      req(input$version2Pgg)
      dtPgg <- data()
      dtPgg <- dtPgg$predictions
      dtPgg <- dtPgg[which(dtPgg$analysisId == input$version2Pgg),]
      traitsPgg <- unique(dtPgg$trait)
      updateSelectInput(session, "trait2Pgg", choices = traitsPgg)
    })
    # trait for report tab
    observeEvent(c(data(), input$version2Pgg), {
      req(data())
      req(input$version2Pgg)
      dtPgg <- data()
      dtPgg <- dtPgg$predictions
      dtPgg <- dtPgg[which(dtPgg$analysisId == input$version2Pgg),]
      traitsPgg <- unique(dtPgg$trait)
      updateSelectInput(session, "traitFilterPredictions2D2", choices = traitsPgg)
    })
    # environment for report tab
    observeEvent(c(data(), input$version2Pgg), {
      req(data())
      req(input$version2Pgg)
      dtPgg <- data()
      dtPgg <- dtPgg$predictions
      dtPgg <- dtPgg[which(dtPgg$analysisId == input$version2Pgg),]
      traitsPgg <- unique(dtPgg$environment)
      updateSelectInput(session, "environment", choices = traitsPgg)
    })
    ##############
    ## entry type
    observeEvent(c(data(), input$version2Pgg, input$trait2Pgg), {
      req(data())
      req(input$version2Pgg)
      req(input$trait2Pgg)
      dtPgg <- data()$predictions
      dtPgg <- dtPgg[which(dtPgg$analysisId == input$version2Pgg),]
      traitsPgg <- unique(dtPgg$environment)
      updateSelectInput(session, "environmentToUse", choices = traitsPgg, selected =traitsPgg )
    })
    ##############################################################################################
    ##############################################################################################
    ##############################################################################################
    ## render metrics barplot
    observeEvent(c(data(),input$version2Pgg), { # update trait
      req(data())
      req(input$version2Pgg)
      dtPgg <- data()
      dtPgg <- dtPgg$metrics
      dtPgg <- dtPgg[which(dtPgg$analysisId %in% input$version2Pgg),] # only traits that have been QA
      traitPggInput <- unique(dtPgg$trait)
      updateSelectInput(session, "traitMetrics", choices = traitPggInput, selected = traitPggInput)
    })
    observeEvent(c(data(),input$version2Pgg), { # update parameter
      req(data())
      req(input$version2Pgg)
      dtPgg <- data()
      dtPgg <- dtPgg$metrics
      dtPgg <- dtPgg[which(dtPgg$analysisId %in% input$version2Pgg),] # only traits that have been QA
      metricsPggInput <- unique(dtPgg$parameter)
      updateSelectInput(session, "parameterMetrics", choices = metricsPggInput)
    })
    output$barplotPredictionsMetrics <- plotly::renderPlotly({
      req(data())
      req(input$version2Pgg)
      dtPgg <- data()
      mydata <- dtPgg$metrics
      mydata <- mydata[which(mydata$analysisId %in% input$version2Pgg),]
      mydata = mydata[which(mydata$parameter %in% input$parameterMetrics),]
      mydata = mydata[which(mydata$trait %in% input$traitMetrics),]
      res = plotly::plot_ly(data = mydata, x = mydata[,"environment"], y = mydata[,"value"],
                            color=mydata[,"trait"]
                            # size=mydata[,input$sizeMetrics2D], text=mydata[,"environment"]
      )   # , type="scatter", mode   = "markers")
      res = res %>% plotly::add_bars()
      res
    })
    ## render the data to be analyzed
    output$phenoPgg <-  DT::renderDT({
      req(data())
      req(input$version2Pgg)
      dtPgg <- data()
      dtPgg <- dtPgg$predictions
      current.predictions <- dtPgg[which(dtPgg$analysisId == input$version2Pgg),setdiff(colnames(dtPgg),c("module","analysisId"))]
      # current.predictions <- subset(dtPgg, select = -c(module,analysisId))
      numeric.output <- c("predictedValue", "stdError", "reliability")
      DT::formatRound(DT::datatable(current.predictions, extensions = 'Buttons',
                                    options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                   lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
      ), numeric.output)
    })
    ## render result of "run" button click
    outPgg <- eventReactive(input$runPgg, {
      req(data())
      req(input$version2Pgg)
      req(input$trait2Pgg)
      req(input$environmentToUse)
      shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
      dtPgg <- data()
      # run the modeling, but before test if mta was done
      if(sum(dtPgg$status$module %in% c("mta","indexD")) == 0) {
        output$qaQcPggInfo <- renderUI({
          if (hideAll$clearAll){
            return()
          }else{
            req(dtPgg)
            HTML(as.character(div(style="color: brown;",
                                  "Please perform Multi-Trial-Analysis or Selection Index before conducting Optimal Cross Selection."))
            )
          }
        })
      }else{
        output$qaQcPggInfo <- renderUI({return(NULL)})
        result <- try(cgiarPipeline::pgg(
          phenoDTfile= dtPgg,
          analysisId=input$version2Pgg,
          trait=input$trait2Pgg, # per trait
          environment=input$environmentToUse,
          proportion=input$proportion,
          verbose=input$verbose
        ),
        silent=TRUE
        )
        if(!inherits(result,"try-error")) {
          data(result) # update data with results
          # save(result, file = "./R/outputs/resultPgg.RData")
          cat(paste("Predicted genetic gain step with id:",as.POSIXct( result$status$analysisId[length(result$status$analysisId)], origin="1970-01-01", tz="GMT") ,"saved."))
        }else{
          cat(paste("Analysis failed with the following error message: \n\n",result[[1]]))
        }
      }
      shinybusy::remove_modal_spinner()

      if(!inherits(result,"try-error")) {
        # view metrics
        output$metricsPgg <-  DT::renderDT({
          # if ( hideAll$clearAll){
          #   return()
          # }else{
          metrics <- result$metrics
          metrics <- metrics[metrics$module=="pgg",]
          metrics$analysisId <- as.numeric(metrics$analysisId)
          metrics <- metrics[!is.na(metrics$analysisId),]
          current.metrics <- metrics[metrics$analysisId==max(metrics$analysisId),]
          current.metrics <- subset(current.metrics, select = -c(module,analysisId))
          numeric.output <- c("value", "stdError")
          DT::formatRound(DT::datatable(current.metrics, extensions = 'Buttons',
                                        options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                       lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
          ), numeric.output)
          # }
        })
        # view modeling
        output$modelingPgg <-  DT::renderDT({
          # if ( hideAll$clearAll){
          #   return()
          # }else{
          modeling <- result$modeling
          modeling <- modeling[modeling$module=="pgg",]
          modeling$analysisId <- as.numeric(modeling$analysisId)
          modeling <- modeling[!is.na(modeling$analysisId),]
          current.modeling <- modeling[modeling$analysisId==max(modeling$analysisId),]
          current.modeling <- subset(current.modeling, select = -c(module,analysisId))
          DT::datatable(current.modeling, extensions = 'Buttons',
                        options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                       lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
          )
          # }
        })
        # Report tab
        output$reportPgg <- renderUI({
          HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportPgg.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
        })


      } else {
        output$predictionsPgg <- DT::renderDT({DT::datatable(NULL)})
        output$metricsPgg <- DT::renderDT({DT::datatable(NULL)})
        output$modelingPgg <- DT::renderDT({DT::datatable(NULL)})
      }

      hideAll$clearAll <- FALSE

    }) ## end eventReactive

    output$downloadReportPgg <- downloadHandler(
      filename = function() {
        paste('my-report', sep = '.', switch(
          "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
        ))
      },
      content = function(file) {
        src <- normalizePath(system.file("rmd","reportPgg.Rmd",package="bioflow"))
        src2 <- normalizePath('data/resultPgg.RData')
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'report.Rmd', overwrite = TRUE)
        file.copy(src2, 'resultPgg.RData', overwrite = TRUE)
        out <- rmarkdown::render('report.Rmd', params = list(toDownload=TRUE),switch(
          "HTML",
          HTML = rmarkdown::html_document()
        ))
        file.rename(out, file)
      }
    )

    output$outPgg <- renderPrint({
      outPgg()
    })


  })
}

## To be copied in the UI
# mod_pggApp_ui("pggApp_1")

## To be copied in the server
# mod_pggApp_server("pggApp_1")

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
      div(tags$p(h4(strong("Predicted Genetic Gain")))),#, style = "color: #817e7e"
      hr(style = "border-top: 1px solid #4c4c4c;"),
      selectInput(ns("version2Pgg"), "Version to analyze", choices = NULL, multiple = FALSE),
      selectInput(ns("trait2Pgg"), "Trait(s) to use", choices = NULL, multiple = TRUE),
      selectInput(ns("environmentToUse"), "Environments to use", choices = NULL, multiple = TRUE),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      shinydashboard::box(width = 12, status = "primary", background="light-blue",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Settings...",
                          numericInput(ns("proportion"), label = "Proportion selected (%)", value = 10, step = 10, max = 100, min = 1),
                          selectInput(ns("verbose"), label = "Print logs?", choices = list(TRUE,FALSE), selected = FALSE, multiple=FALSE)
      ),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      actionButton(ns("runPgg"), "Run", icon = icon("play-circle")),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      uiOutput(ns("qaQcPggInfo")),
      shinycssloaders::withSpinner(textOutput(ns("outPgg")),type=8)
    ), # end sidebarpanel
    mainPanel(tabsetPanel(
      type = "tabs",

      tabPanel("Data",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,DT::DTOutput(ns("phenoPgg")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;"),
                                   downloadButton(ns('downloadData'), 'Download data')
               )
      ),
      # tabPanel("Predictions",
      #          br(),
      #          shinydashboard::box(status="primary",width = 12,
      #                              solidHeader = TRUE,
      #                              column(width=12,DT::DTOutput(ns("predictionsPgg")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
      #          )
      # ),
      tabPanel("Metrics",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,br(),DT::DTOutput(ns("metricsPgg")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;"),
                                   downloadButton(ns('downloadMetrics'), 'Download metrics')
               )
      ),
      tabPanel("Modeling",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,DT::DTOutput(ns("modelingPgg")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;"),
                                   downloadButton(ns('downloadModeling'), 'Download modeling')
               )
      ),
      tabPanel("Report",
               br(),
               # shinydashboard::box(width = 6, status = "success",# background="light-blue",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Settings...",
               #                     selectInput(ns("traitFilterPredictions2D2"), "Trait(s) to visualize", choices = NULL, multiple = FALSE),
               #                     selectInput(ns("environment"), "Treatment to view", choices = NULL, multiple = FALSE)
               # ),
               # shinydashboard::box(status="success",width = 12,# solidHeader = TRUE, title = "Desired change and expected response",
               #                     plotly::plotlyOutput(ns("plotPredictionsScatter"))
               # )
      ),
      tabPanel("Documentation",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,   style = "height:800px; overflow-y: scroll;overflow-x: scroll;",
                                          h1(strong("Details")),
                                          p("This option aims to calculate the predicted genetic gain from the classical breeders' equation
                              R = i*r*s being R the response to selection, i the selection intensity, r the selection accuracy, s the genetic standard deviation.
                                The way the options are used is the following:"),
                                          p(strong("Trait for cross prediction.-")," Trait to be be used for predicting all possible crosses (an index is suggested)."),
                                          p(strong("Year to analyze.-")," Year of data to be used from the input file."),
                                          p(strong("Fieldinst.-")," Field instance of data to be used from the input file."),
                                          p(strong("Percentage selected.-")," Percentage of parents to be selected to be used to calculate the selection intensity."),
                                          p(strong("Cycle time (idealized).-")," Idealized cycle time to be used as denominator to calculate the rate of response to selection."),
                                          h2(strong("References:")),
                                          p("Lush, J. L. (2013). Animal breeding plans. Read Books Ltd."),
                                          p("Mrode, R. A. (2014). Linear models for the prediction of animal breeding values. Cabi."),
                                          h3(strong("Software used:")),
                                          p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
                                Vienna, Austria. URL https://www.R-project.org/.")
                                   )
               )
      )
    )) # end mainpanel

  )
}

#' pggApp Server Functions
#'
#' @noRd
mod_pggApp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })
    ############################################################################

    data = reactive({ # provisional dataset for testing
      load("dataStr0.RData")
      data <- zz
      return(data)
    })
    #################
    ## version
    observeEvent(c(data()), {
      req(data())
      dtPgg <- data()
      dtPgg <- dtPgg$status
      dtPgg <- dtPgg[which(dtPgg$module %in% c("mta","indexD")),]
      traitsPgg <- unique(dtPgg$analysisId)
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
    # observeEvent(c(data(), input$version2Pgg), {
    #   req(data())
    #   req(input$version2Pgg)
    #   dtPgg <- data()
    #   dtPgg <- dtPgg$predictions
    #   dtPgg <- dtPgg[which(dtPgg$analysisId == input$version2Pgg),]
    #   traitsPgg <- unique(dtPgg$trait)
    #   updateSelectInput(session, "traitFilterPredictions2D2", choices = traitsPgg)
    # })
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
    ## render the data to be analyzed
    output$phenoPgg <-  DT::renderDT({
      req(data())
      req(input$version2Pgg)
      dtPgg <- data()
      dtPgg <- dtPgg$predictions
      dtPgg <- dtPgg[which(dtPgg$analysisId == input$version2Pgg),setdiff(colnames(dtPgg),c("module","analysisId"))]
      DT::datatable(dtPgg,options = list(autoWidth = TRUE),filter = "top")
    })
    # download button for input data
    output$downloadData <- downloadHandler(
      filename = function() { paste("tableDataInputPgg-", Sys.Date(), ".csv", sep="") },
      content = function(file) {
        dtPgg <- data();  dtPgg <- dtPgg$predictions
        dtPgg <- dtPgg[which(dtPgg$analysisId == input$version2Pgg),setdiff(colnames(dtPgg),c("module","analysisId"))]
        utils::write.csv(dtPgg, file)
      })
    ## render result of "run" button click
    outPgg <- eventReactive(input$runPgg, {
      req(data())
      req(input$version2Pgg)
      req(input$trait2Pgg)
      req(input$environmentToUse)
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
          print("Realized genetic gain calculated")
          # data(result) # update data with results
          print(data()$status)
        }else{
          print(result)
        }
      }

      if(!inherits(result,"try-error")) {

        # output$predictionsPgg <-  DT::renderDT({
        #   # if ( hideAll$clearAll){
        #   #   return()
        #   # }else{
        #   predictions <- result$predictions
        #   predictions <- predictions[predictions$module=="pgg",]
        #   predictions$analysisId <- as.numeric(predictions$analysisId)
        #   predictions <- predictions[!is.na(predictions$analysisId),]
        #   current.predictions <- predictions[predictions$analysisId==max(predictions$analysisId),]
        #   current.predictions <- subset(current.predictions, select = -c(module,analysisId))
        #   numeric.output <- c("predictedValue", "stdError", "reliability")
        #   DT::formatRound(DT::datatable(current.predictions,
        #                                 options = list(autoWidth = TRUE),
        #                                 filter = "top"
        #   ), numeric.output)
        #   # }
        # })
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
          DT::formatRound(DT::datatable(current.metrics,
                                        options = list(autoWidth = TRUE),
                                        filter = "top"
          ), numeric.output)
          # }
        })
        # download metrics
        output$downloadMetrics <- downloadHandler(
          filename = function() { paste("tableMetricstPgg-", Sys.Date(), ".csv", sep="")},
          content = function(file) {
            # if ( hideAll$clearAll){ return()
            # }else{
            metrics <- result$metrics
            mtas <- result$status[which(result$status$module == "pgg"),"analysisId"]; mtaId <- mtas[length(mtas)]
            metrics <- metrics[which(metrics$analysisId == mtaId),]
            utils::write.csv(metrics, file)
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
          DT::datatable(current.modeling,
                        options = list(autoWidth = TRUE),
                        filter = "top"
          )
          # }
        })
        # download modeling table
        output$downloadModeling <- downloadHandler(
          filename = function() { paste("tableModelingtPgg-", Sys.Date(), ".csv", sep="")},
          content = function(file) {
            # if ( hideAll$clearAll){ return()
            # }else{
            modeling <- result$modeling
            mtas <- result$status[which(result$status$module == "pgg"),"analysisId"]; mtaId <- mtas[length(mtas)]
            modeling <- modeling[which(modeling$analysisId == mtaId),]
            utils::write.csv(modeling, file)
            # }
          })
        ## Report tab
        # plot scatter of predictions and color by pedigree
        # output$plotPredictionsScatter <-  plotly::renderPlotly({
        #   # input <- list(traitFilterPredictions2D2="desireIndex", environment="desireIndex ~ 20 crosses * 30 degrees", checkboxBoxplotPredictions2D=TRUE,
        #   #               xAxisPredictions2D="designation",yAxisPredictions2D="predictedValue",colorPredictions2D="mother", sizePredictions2D="stdError", textPredictions2D="designation")
        #   mydata = result$predictions
        #   mtas <- result$status[which(result$status$module == "pgg"),"analysisId"];mtaId <- mtas[length(mtas)]
        #   mydata <- mydata[which(mydata$analysisId == mtaId),]
        #   mydata = mydata[which(mydata$trait == input$traitFilterPredictions2D2),]
        #   mydata = mydata[which(mydata$environment == input$environment),]
        #   res = plotly::plot_ly(data = mydata, x = mydata[,input$xAxisPredictions2D], y = mydata[,input$yAxisPredictions2D],
        #                         color=mydata[,input$colorPredictions2D], size=mydata[,input$sizePredictions2D], #boxpoints = "all",
        #                         text=mydata[,input$textPredictions2D],  type="box")
        #   # res = res %>% plotly::layout(showlegend = FALSE)
        #   res
        # })

      } else {
        output$predictionsPgg <- DT::renderDT({DT::datatable(NULL)})
        output$metricsPgg <- DT::renderDT({DT::datatable(NULL)})
        output$modelingPgg <- DT::renderDT({DT::datatable(NULL)})
      }

      hideAll$clearAll <- FALSE

    }) ## end eventReactive

    output$outPgg <- renderPrint({
      outPgg()
    })

    # output$reportPgg <- renderUI({
    #   HTML(markdown::markdownToHTML(knitr::knit("./R/testing_sta.Rmd", quiet = TRUE), fragment.only=TRUE))
    # })


  })
}

## To be copied in the UI
# mod_pggApp_ui("pggApp_1")

## To be copied in the server
# mod_pggApp_server("pggApp_1")

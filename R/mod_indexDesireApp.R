#' indexDesireApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_indexDesireApp_ui <- function(id){
  ns <- NS(id)
  tagList(


    sidebarPanel(

      tags$style(".well {background-color:grey; color: #FFFFFF;}"),
      div(tags$p( h4(strong("Desire selection index")))),#, style = "color: #817e7e"
      hr(style = "border-top: 1px solid #4c4c4c;"),
      selectInput(ns("version2IdxD"), "Version to analyze", choices = NULL, multiple = FALSE),
      selectInput(ns("trait2IdxD"), "Trait(s) to analyze", choices = NULL, multiple = TRUE),
      textInput(ns("desirev"), label = "Desired change in traits [Enter a numeric vector (comma delimited): e.g: 0,100,2 ]", value=NULL),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      shinydashboard::box(width = 12, status = "primary", background="light-blue",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Settings...",
                          numericInput(ns("proportion"), label = "Selected proportion", value = 0.1, min=0.001,max=1, step=0.05),
                          selectInput(ns("scaledIndex"), label = "Scale traits? (only if desire is scaled)", choices = list(TRUE,FALSE), selected = FALSE, multiple=FALSE),
                          numericInput(ns("fontSizeRadar"), label = "Font size", value = 12),
                          selectInput(ns("verboseIndex"), label = "Print logs?", choices = list(TRUE,FALSE), selected = FALSE, multiple=FALSE)
      ),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      actionButton(ns("runIdxD"), "Run", icon = icon("play-circle")),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      uiOutput(ns("qaQcIdxDInfo")),
      shinycssloaders::withSpinner(textOutput(ns("outIdxD")),type=8)
    ), # end sidebarpanel
    mainPanel(tabsetPanel(
      type = "tabs",

      tabPanel("Input",
               br(),
               shinydashboard::box(status="success",width = 12,
                                   solidHeader = TRUE, title = "Desired change and expected response",
                                   plotly::plotlyOutput(ns("plotPredictionsRadar")),
                                   plotly::plotlyOutput(ns("plotPotentialResponse"))
               ),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,DT::DTOutput(ns("tablePredictionsTraitsWide")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;"),
                                   downloadButton(ns('downloadPredictionsWide'), 'Download input traits')
               )
      ),
      tabPanel("Predictions",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,DT::DTOutput(ns("predictionsIdxD")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;"),
                                   downloadButton(ns('downloadPredictions'), 'Download predictions')
               )
      ),
      tabPanel("Modeling",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,DT::DTOutput(ns("modelingIdxD")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;"),
                                   downloadButton(ns('downloadModeling'), 'Download modeling')
               )
      ),
      tabPanel("Report",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,DT::DTOutput(ns("tablePredictionsTraitsWide2")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;"),
                                   downloadButton(ns('downloadPredictionsWide2'), 'Download predictions')
               )
      ),
      tabPanel("Documentation",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,   style = "height:800px; overflow-y: scroll;overflow-x: scroll;",
                                          h1(strong("Details")),
                                          p("This option aims to calculate a selection index using across-environment predictions from multiple
                              traits based on user's weights and return a table of predictions with the index and the traits used
                              for the index calculation.
                                The way the options are used is the following:"),
                                          p(strong("Traits to analyze.-")," Traits to be considered in the index."),
                                          p(strong("Desire vector.-")," Vector of values indicating the desired change in traits."),
                                          p(strong("Scale predictions.-")," A TRUE or FALSE value indicating if the table of traits should be
                                scaled or not. If TRUE is selected, the values of the desire vector are expected to be expressed in
                                standard deviations. If FALSE, the values of the desire vector are expected to be expressed in
                                original units."),
                                          h2(strong("References:")),
                                          p("Pesek, J., & Baker, R. J. (1969). Desired improvement in relation to selection indices. Canadian journal of plant science, 49(6), 803-804."),
                                          p("Ceron-Rojas, J. J., & Crossa, J. (2018). Linear selection indices in modern plant breeding (p. 256). Springer Nature."),
                                          h3(strong("Software used:")),
                                          p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
                                Vienna, Austria. URL https://www.R-project.org/.")
                                   )
               )
      )
    )) # end mainpanel


  )
}

#' indexDesireApp Server Functions
#'
#' @noRd
mod_indexDesireApp_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })
    ############################################################################

    # data = reactive({
    #   load("dataStr0.RData")
    #   data <- yy
    #   return(data)
    # })
    #################
    ## version
    observeEvent(c(data()), {
      req(data())
      dtIdxD <- data()
      dtIdxD <- dtIdxD$status
      dtIdxD <- dtIdxD[which(dtIdxD$module == "mta"),]
      traitsIdxD <- unique(dtIdxD$analysisId)
      updateSelectInput(session, "version2IdxD", choices = traitsIdxD)
    })
    #################
    ## traits
    observeEvent(c(data(), input$version2IdxD), {
      req(data())
      req(input$version2IdxD)
      dtIdxD <- data()
      dtIdxD <- dtIdxD$predictions
      dtIdxD <- dtIdxD[which(dtIdxD$analysisId == input$version2IdxD),]
      traitsIdxD <- unique(dtIdxD$trait)
      updateSelectInput(session, "trait2IdxD", choices = traitsIdxD)
    })

    ##############################################################################################
    ##############################################################################################
    ##############################################################################################
    ## render the data to be analyzed (wide format)
    output$tablePredictionsTraitsWide <-  DT::renderDT({
      req(data())
      req(input$version2IdxD)
      dtIdxD <- data(); dtIdxD <- dtIdxD$predictions
      dtIdxD <- dtIdxD[which(dtIdxD$analysisId == input$version2IdxD),setdiff(colnames(dtIdxD),c("module","analysisId"))]
      wide <- stats::reshape(dtIdxD[,c(c("designation"),"trait",c("predictedValue"))], direction = "wide", idvar = c("designation"),
                             timevar = "trait", v.names = c("predictedValue"), sep= "_")
      colnames(wide) <- gsub("predictedValue_","",colnames(wide))
      numeric.output <- colnames(wide)[-c(1)]
      DT::formatRound(DT::datatable(wide, options = list(autoWidth = TRUE),filter = "top"), numeric.output)
    })
    # render plot for initial values
    output$plotPredictionsRadar <-  plotly::renderPlotly({
      req(data())
      req(input$version2IdxD)
      dtIdxD <- data(); dtIdxD <- dtIdxD$predictions
      mydata <- dtIdxD[which(dtIdxD$analysisId == input$version2IdxD),setdiff(colnames(dtIdxD),c("module","analysisId"))]
      radarPlot(mydata, environmentPredictionsRadar2="across",traitFilterPredictionsRadar2=input$trait2IdxD,proportion=input$proportion,meanGroupPredictionsRadar=input$desirev,
                             fontSizeRadar=input$fontSizeRadar, r0Radar=NULL, neRadar=NULL, plotSdRadar=FALSE) # send to setting plotSdRadar # send to argument meanGroupPredictionsRadar
    })
    # render plot for potential responses
    output$plotPotentialResponse <-  plotly::renderPlotly({
      req(data())
      req(input$version2IdxD)
      req(input$trait2IdxD)
      dtIdxD <- data();
      plotDensitySelected(object=dtIdxD,environmentPredictionsRadar2="across", traitFilterPredictionsRadar2=input$trait2IdxD, meanGroupPredictionsRadar=input$desirev, proportion=input$proportion,
                                       analysisId=input$version2IdxD, trait=input$trait2IdxD, desirev=input$desirev, scaled=input$scaledIndex)
    })
    # download predictions in wide format
    output$downloadPredictionsWide <- downloadHandler(
      filename = function() { paste("tableInputWide-", Sys.Date(), ".csv", sep="")},
      content = function(file) {
        req(data())
        req(input$version2IdxD)
        dtIdxD <- data(); dtIdxD <- dtIdxD$predictions
        dtIdxD <- dtIdxD[which(dtIdxD$analysisId == input$version2IdxD),setdiff(colnames(dtIdxD),c("module","analysisId"))]
        wide <- stats::reshape(dtIdxD[,c(c("designation"),"trait",c("predictedValue"))], direction = "wide", idvar = c("designation"),
                               timevar = "trait", v.names = c("predictedValue"), sep= "_")
        colnames(wide) <- gsub("predictedValue_","",colnames(wide))
        utils::write.csv(wide, file)
      })
    ## render result of "run" button click
    outIdxD <- eventReactive(input$runIdxD, {
      req(data())
      req(input$trait2IdxD)
      dtIdxD <- data()
      # run the modeling, but before test if mta was done
      if(sum(dtIdxD$status$module %in% "mta") == 0) {
        output$qaQcIdxDInfo <- renderUI({
          if (hideAll$clearAll)
            return()
          else
            req(dtIdxD)
          HTML(as.character(div(style="color: brown;",
                                "Please perform Multi-Trial-Analysis before conducting a Selection index."))
          )
        })
      }else{
        output$qaQcIdxDInfo <- renderUI({return(NULL)})
        result <- try(cgiarPipeline::indexDesire(
          phenoDTfile= dtIdxD, # input data structure
          analysisId=input$version2IdxD, # analysis to be picked from predictions database
          trait= input$trait2IdxD, # traits to include in the index
          desirev = as.numeric(unlist(strsplit(input$desirev,","))), # vector of desired values
          scaled=input$scaledIndex, # whether predicted values should be scaled or not
          verbose=input$verboseIndex # should we print logs or not
        ),
        silent=TRUE
        )
        if(!inherits(result,"try-error")) {
          # save(result, file="toTest.RData")
          print("Index calculated")
          data(result) # update data with results
          print(data()$status)
        }else{
          print(result)
        }
      }

      if(!inherits(result,"try-error")) {
        # display table of predictions
        output$predictionsIdxD <-  DT::renderDT({
          # if ( hideAll$clearAll){
          #   return()
          # }else{
            predictions <- data()$predictions
            predictions <- predictions[predictions$module=="indexD",]
            predictions$analysisId <- as.numeric(predictions$analysisId)
            predictions <- predictions[!is.na(predictions$analysisId),]
            current.predictions <- predictions[predictions$analysisId==max(predictions$analysisId),]
            current.predictions <- subset(current.predictions, select = -c(module,analysisId))
            numeric.output <- c("predictedValue", "stdError", "reliability")
            DT::formatRound(DT::datatable(current.predictions, options = list(autoWidth = TRUE),filter = "top"), numeric.output)
          # }
        })
        # download predictions
        output$downloadPredictions <- downloadHandler(
          filename = function() { paste("tablePredictionstMta-", Sys.Date(), ".csv", sep="")},
          content = function(file) {
            # if ( hideAll$clearAll){
            #   return()
            # }else{
              predictions <- data()$predictions
              mtas <- data()$status[which(data()$status$module == "indexD"),"analysisId"]; mtaId <- mtas[length(mtas)]
              predictions <- predictions[which(predictions$analysisId == mtaId),]
              utils::write.csv(predictions, file)
            # }
          })
        # display table of modeling
        output$modelingIdxD <-  DT::renderDT({
          # if ( hideAll$clearAll){
          #   return()
          # }else{
            modeling <- data()$modeling
            mtas <- data()$status[which(data()$status$module == "indexD"),"analysisId"]; mtaId <- mtas[length(mtas)]
            modeling <- modeling[which(modeling$analysisId == mtaId),]
            modeling <- subset(modeling, select = -c(module,analysisId))
            DT::datatable(modeling, options = list(autoWidth = TRUE),filter = "top")
          # }
        })
        # download table of modelng
        output$downloadModeling <- downloadHandler(
          filename = function() { paste("tableModelingtMta-", Sys.Date(), ".csv", sep="")},
          content = function(file) {
            # if ( hideAll$clearAll){
            #   return()
            # }else{
              modeling <- data()$modeling
              mtas <- data()$status[which(data()$status$module == "indexD"),"analysisId"]; mtaId <- mtas[length(mtas)]
              modeling <- modeling[which(modeling$analysisId == mtaId),]
              utils::write.csv(modeling, file)
            # }
          })
        ## Report tab
        # table of predictions in wide format
        output$tablePredictionsTraitsWide2 <-  DT::renderDT({
          # if ( hideAll$clearAll){
          #   return()
          # }else{
            mydata = data()$predictions
            mtas <- data()$status[which(data()$status$module == "indexD"),"analysisId"];
            mtaId <- c(mtas[length(mtas)],input$version2IdxD)
            mydata <- mydata[which(mydata$analysisId %in% mtaId),]
            wide <- stats::reshape(mydata[,c(c("designation"),"trait",c("predictedValue"))], direction = "wide", idvar = c("designation"),
                                   timevar = "trait", v.names = c("predictedValue"), sep= "_")
            colnames(wide) <- gsub("predictedValue_","",colnames(wide))
            numeric.output <- colnames(wide)[-c(1)]
            DT::formatRound(DT::datatable(wide, options = list(autoWidth = TRUE),filter = "top"), numeric.output)
          # }
        })
        # download predictions in wide format
        output$downloadPredictionsWide2 <- downloadHandler(
          filename = function() { paste("tablePredictionstIndexWide-", Sys.Date(), ".csv", sep="")},
          content = function(file) {
            # if ( hideAll$clearAll){
            #   return()
            # }else{
              mydata = data()$predictions
              mtas <- data()$status[which(data()$status$module == "indexD"),"analysisId"];
              mtaId <- c(mtas[length(mtas)], input$version2IdxD)
              mydata <- mydata[which(mydata$analysisId %in% mtaId),]
              wide <- stats::reshape(mydata[,c(c("designation"),"trait",c("predictedValue"))], direction = "wide", idvar = c("designation"),
                                     timevar = "trait", v.names = c("predictedValue"), sep= "_")
              colnames(wide) <- gsub("predictedValue_","",colnames(wide))
              utils::write.csv(wide, file)
            # }
          })

      } else {
        output$predictionsIdxD <- DT::renderDT({DT::datatable(NULL)})
        output$metricsIdxD <- DT::renderDT({DT::datatable(NULL)})
        output$modelingIdxD <- DT::renderDT({DT::datatable(NULL)})
        hideAll$clearAll <- TRUE
      }

      hideAll$clearAll <- FALSE

    }) ## end eventReactive

    output$outIdxD <- renderPrint({
      outIdxD()
    })


  })
}

## To be copied in the UI
# mod_indexDesireApp_ui("indexDesireApp_1")

## To be copied in the server
# mod_indexDesireApp_server("indexDesireApp_1")

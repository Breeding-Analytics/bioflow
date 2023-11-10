#' ocsApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ocsApp_ui <- function(id){
  ns <- NS(id)
  tagList(


    sidebarPanel(
      # input <- list(version2Ocs="1699508839.68847",trait2Ocs="desireIndex",entryType2Ocs= "TGV_EST004D#TEST_tested",nCrossOcs="20", targetAngleOcs="30",maxRun=40, relType="grm", env2Ocs="across",verboseOcs=TRUE )
      tags$style(".well {background-color:grey; color: #FFFFFF;}"),
      div(tags$p( "Optimal Cross Selection")),#, style = "color: #817e7e"
      hr(style = "border-top: 1px solid #4c4c4c;"),
      selectInput(ns("version2Ocs"), "Version to analyze", choices = NULL, multiple = FALSE),
      selectInput(ns("trait2Ocs"), "Trait(s) to use", choices = NULL, multiple = FALSE),
      selectInput(ns("entryType2Ocs"), "Entry types to use", choices = NULL, multiple = TRUE),
      textInput(ns("nCrossOcs"), label = "Number of crosses [Enter a numeric vector (comma delimited): e.g: 20,30,40 ]", value="20"),
      textInput(ns("targetAngleOcs"), label = "Target angle [Enter a numeric vector (comma delimited): e.g: 30,60,90 ]", value="30"),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      shinydashboard::box(width = 12, status = "primary", background="light-blue",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Settings...",
                          numericInput(ns("maxRun"), label = "Stopping criteria (#of iterations without change)", value = 40),
                          selectInput(ns("relType"), "Relationship to use", choices = list(GRM="grm",NRM="nrm", BOTH="both"), multiple = FALSE),
                          selectInput(ns("env2Ocs"), "Environment to use", choices = NULL, multiple = FALSE),
                          selectInput(ns("verboseOcs"), label = "Print logs?", choices = list(TRUE,FALSE), selected = FALSE, multiple=FALSE)
      ),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      actionButton(ns("runOcs"), "Run", icon = icon("play-circle")),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      uiOutput(ns("qaQcOcsInfo")),
      shinycssloaders::withSpinner(textOutput(ns("outOcs")),type=8)
    ), # end sidebarpanel
    mainPanel(tabsetPanel(
      type = "tabs",

      tabPanel("Data",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,DT::DTOutput(ns("phenoOcs")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
               )
      ),
      tabPanel("Predictions",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,DT::DTOutput(ns("predictionsOcs")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
               )
      ),
      tabPanel("Metrics",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,br(),DT::DTOutput(ns("metricsOcs")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
               )
      ),
      tabPanel("Modeling",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,DT::DTOutput(ns("modelingOcs")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
               )
      ),
      tabPanel("Report",
               # br(),
               # div(tags$p("Please download the report below:") ),
               # br(),
               # uiOutput(ns('reportOcs')),
               # includeHTML("/Users/idieng/Library/CloudStorage/OneDrive-CGIAR/_IITA/_Shiny/lmm_golem/bioflow/R/report_STA2.html")
      ),
      tabPanel("Documentation",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,   style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
               )
      ),
      tabPanel("References",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,    style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
               )
      )
    )) # end mainpanel


  )
}

#' ocsApp Server Functions
#'
#' @noRd
mod_ocsApp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })
    ############################################################################

    data = reactive({
      load("~/Documents/bioflow/dataStr0.RData")
      data <- zz
      return(data)
    })
    #################
    ## version
    observeEvent(c(data()), {
      req(data())
      dtOcs <- data()
      dtOcs <- dtOcs$status
      dtOcs <- dtOcs[which(dtOcs$module %in% c("mta","indexD")),]
      traitsOcs <- unique(dtOcs$analysisId)
      updateSelectInput(session, "version2Ocs", choices = traitsOcs)
    })
    #################
    ## traits
    observeEvent(c(data(), input$version2Ocs), {
      req(data())
      req(input$version2Ocs)
      dtOcs <- data()
      dtOcs <- dtOcs$predictions
      dtOcs <- dtOcs[which(dtOcs$analysisId == input$version2Ocs),]
      traitsOcs <- unique(dtOcs$trait)
      updateSelectInput(session, "trait2Ocs", choices = traitsOcs)
    })
    ##############
    ## entry type
    observeEvent(c(data(), input$version2Ocs, input$trait2Ocs), {
      req(data())
      req(input$version2Ocs)
      req(input$trait2Ocs)
      dtOcs <- data()
      dtOcs <- dtOcs$predictions
      dtOcs <- dtOcs[which(dtOcs$analysisId == input$version2Ocs),]
      dtOcs <- dtOcs[which(dtOcs$trait == input$trait2Ocs),]
      traitsOcs <- unique(dtOcs$entryType)
      updateSelectInput(session, "entryType2Ocs", choices = traitsOcs)
    })
    ##############
    ## environment
    observeEvent(c(data(), input$version2Ocs, input$trait2Ocs), {
      req(data())
      req(input$version2Ocs)
      req(input$trait2Ocs)
      dtOcs <- data()
      dtOcs <- dtOcs$predictions
      dtOcs <- dtOcs[which(dtOcs$analysisId == input$version2Ocs),]
      dtOcs <- dtOcs[which(dtOcs$trait == input$trait2Ocs),]
      traitsOcs <- unique(dtOcs$environment)
      updateSelectInput(session, "env2Ocs", choices = traitsOcs)
    })

    ##############################################################################################
    ##############################################################################################
    ##############################################################################################
    ## render the data to be analyzed
    output$phenoOcs <-  DT::renderDT({
      req(data())
      req(input$version2Ocs)
      dtOcs <- data()
      dtOcs <- dtOcs$predictions
      dtOcs <- dtOcs[which(dtOcs$analysisId == input$version2Ocs),setdiff(colnames(dtOcs),c("module","analysisId"))]
      DT::datatable(dtOcs,
                    options = list(autoWidth = TRUE),
                    filter = "top"
      )
    })

    ## render result of "run" button click
    outOcs <- eventReactive(input$runOcs, {
      req(data())
      req(input$version2Ocs)
      req(input$trait2Ocs)
      req(input$env2Ocs)
      dtOcs <- data()

      # run the modeling, but before test if mta was done
      if(sum(dtOcs$status$module %in% "mta") == 0) {
        output$qaQcOcsInfo <- renderUI({
          if (hideAll$clearAll)
            return()
          else
            req(dtOcs)
          HTML(as.character(div(style="color: brown;",
                                "Please perform Multi-Trial-Analysis or Selection Index before conducting Optimal Cross Selection."))
          )
        })
      }else{
        output$qaQcOcsInfo <- renderUI({return(NULL)})
        result <- try(cgiarPipeline::ocs(
          phenoDTfile= dtOcs, # analysis to be picked from predictions database
          analysisId=input$version2Ocs,
          relDTfile= input$relType,
          trait= input$trait2Ocs, # per trait
          environment=input$env2Ocs,
          nCross=as.numeric(gsub(" ","",unlist(strsplit(input$nCrossOcs,",")))),
          targetAngle=as.numeric(gsub(" ","",unlist(strsplit(input$targetAngleOcs,",")))), # in radians
          verbose=input$verboseOcs, maxRun = input$maxRun,
          entryType=input$entryType2Ocs
        ),
        silent=TRUE
        )
        if(!inherits(result,"try-error")) {
          print("Ocs calculated")
        }else{print("There was an issue. Please contact your administrator.")}
        # data(result) # update data with results
      }

      if(!inherits(result,"try-error")) {

        output$predictionsOcs <-  DT::renderDT({
          if(!inherits(result,"try-error") ){
            if ( hideAll$clearAll)
              return()
            else
              predictions <- result$predictions
            predictions <- predictions[predictions$module=="ocs",]

            predictions$analysisId <- as.numeric(predictions$analysisId)
            predictions <- predictions[!is.na(predictions$analysisId),]


            current.predictions <- predictions[predictions$analysisId==max(predictions$analysisId),]
            current.predictions <- subset(current.predictions, select = -c(module,analysisId))
            numeric.output <- c("predictedValue", "stdError", "reliability")
            DT::formatRound(DT::datatable(current.predictions,
                                          options = list(autoWidth = TRUE),
                                          filter = "top"
            ), numeric.output)
          }
        })

        output$metricsOcs <-  DT::renderDT({
          if(!inherits(result,"try-error") ){
            if ( hideAll$clearAll)
              return()
            else
              metrics <- result$metrics
            metrics <- metrics[metrics$module=="ocs",]
            metrics$analysisId <- as.numeric(metrics$analysisId)
            metrics <- metrics[!is.na(metrics$analysisId),]
            current.metrics <- metrics[metrics$analysisId==max(metrics$analysisId),]
            current.metrics <- subset(current.metrics, select = -c(module,analysisId))
            numeric.output <- c("value", "stdError")
            DT::formatRound(DT::datatable(current.metrics,
                                          options = list(autoWidth = TRUE),
                                          filter = "top"
            ), numeric.output)
          }
        })

        output$modelingOcs <-  DT::renderDT({
          if(!inherits(result,"try-error") ){
            if ( hideAll$clearAll)
              return()
            else
              modeling <- result$modeling
            modeling <- modeling[modeling$module=="ocs",]

            modeling$analysisId <- as.numeric(modeling$analysisId)
            modeling <- modeling[!is.na(modeling$analysisId),]

            current.modeling <- modeling[modeling$analysisId==max(modeling$analysisId),]
            current.modeling <- subset(current.modeling, select = -c(module,analysisId))
            DT::datatable(current.modeling,
                          options = list(autoWidth = TRUE),
                          filter = "top"
            )
          }
        })
      } else {
        output$predictionsOcs <- DT::renderDT({DT::datatable(NULL)})
        output$metricsOcs <- DT::renderDT({DT::datatable(NULL)})
        output$modelingOcs <- DT::renderDT({DT::datatable(NULL)})
      }

      hideAll$clearAll <- FALSE

    }) ## end eventReactive

    output$outOcs <- renderPrint({
      outOcs()
    })

    output$reportOcs <- renderUI({
      HTML(markdown::markdownToHTML(knitr::knit("./R/testing_sta.Rmd", quiet = TRUE), fragment.only=TRUE))
    })



  })
}

## To be copied in the UI
# mod_ocsApp_ui("ocsApp_1")

## To be copied in the server
# mod_ocsApp_server("ocsApp_1")

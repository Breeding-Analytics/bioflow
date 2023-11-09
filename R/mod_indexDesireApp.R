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
      div(tags$p( "Desire selection index")),#, style = "color: #817e7e"
      hr(style = "border-top: 1px solid #4c4c4c;"),
      selectInput(ns("version2IdxD"), "Version to analyze", choices = NULL, multiple = FALSE),
      selectInput(ns("trait2IdxD"), "Trait(s) to analyze", choices = NULL, multiple = TRUE),
      textInput(ns("desirev"), label = "Desire vector [Enter a numeric vector (comma delimited): e.g: 0,1,2 ]", value=NULL),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      shinydashboard::box(width = 12, status = "primary", background="light-blue",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Settings...",
                          selectInput(ns("scaledIndex"), label = "Scale traits? (only if desire is scaled)", choices = list(TRUE,FALSE), selected = TRUE, multiple=FALSE),
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

      tabPanel("Data",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,DT::DTOutput(ns("phenoIdxD")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
               )
      ),
      tabPanel("Predictions",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,DT::DTOutput(ns("predictionsIdxD")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
               )
      ),
      tabPanel("Modeling",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,DT::DTOutput(ns("modelingIdxD")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
               )
      ),
      tabPanel("Report",
               # br(),
               # div(tags$p("Please download the report below:") ),
               # br(),
               # uiOutput(ns('reportIdxD')),
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

#' indexDesireApp Server Functions
#'
#' @noRd
mod_indexDesireApp_server <- function(id){
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
      data <- yy
      return(data)
    })
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
    ## render the data to be analyzed
    output$phenoIdxD <-  DT::renderDT({
      req(data())
      req(input$version2IdxD)
      dtIdxD <- data()
      dtIdxD <- dtIdxD$predictions
      dtIdxD <- dtIdxD[which(dtIdxD$analysisId == input$version2IdxD),setdiff(colnames(dtIdxD),c("module","analysisId"))]
      DT::datatable(dtIdxD,
                    options = list(autoWidth = TRUE),
                    filter = "top"
      )
    })

    ## render result of "run" button click
    outIdxD <- eventReactive(input$runIdxD, {
      req(data())
      req(input$trait2IdxD)
      dtIdxD <- data()

      # run the modeling, but before test if sta was done
      if(sum(dtIdxD$status$module %in% "sta") == 0) {
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
        print("Index calculated")
        # data(result) # update data with results
      }

      if(!inherits(result,"try-error")) {

        output$predictionsIdxD <-  DT::renderDT({
          if(!inherits(result,"try-error") ){
            if ( hideAll$clearAll)
              return()
            else
              predictions <- result$predictions
            predictions <- predictions[predictions$module=="indexD",]

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

        output$modelingIdxD <-  DT::renderDT({
          if(!inherits(result,"try-error") ){
            if ( hideAll$clearAll)
              return()
            else
              modeling <- result$modeling
            modeling <- modeling[modeling$module=="indexD",]

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
        output$predictionsIdxD <- DT::renderDT({DT::datatable(NULL)})
        output$metricsIdxD <- DT::renderDT({DT::datatable(NULL)})
        output$modelingIdxD <- DT::renderDT({DT::datatable(NULL)})
      }

      hideAll$clearAll <- FALSE

    }) ## end eventReactive

    output$outIdxD <- renderPrint({
      outIdxD()
    })

    output$reportIdxD <- renderUI({
      HTML(markdown::markdownToHTML(knitr::knit("./R/testing_sta.Rmd", quiet = TRUE), fragment.only=TRUE))
    })



  })
}

## To be copied in the UI
# mod_indexDesireApp_ui("indexDesireApp_1")

## To be copied in the server
# mod_indexDesireApp_server("indexDesireApp_1")

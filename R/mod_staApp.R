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
    sidebarPanel(
      tags$style(".well {background-color:grey; color: #FFFFFF;}"),
      div(tags$p( "Single Trial Analysis")),#, style = "color: #817e7e"
      hr(style = "border-top: 1px solid #4c4c4c;"),
      selectInput(ns("genoUnitSta"), "Genetic evaluation unit(s)", choices = NULL, multiple = TRUE),
      selectInput(ns("fixedTermSta2"), "Covariable(s)", choices = NULL, multiple = TRUE),
      selectInput(ns("trait2Sta"), "Trait(s) to analyze", choices = NULL, multiple = TRUE),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      shinydashboard::box(width = 12, status = "primary", background =  "light-blue", solidHeader = TRUE,collapsible = TRUE, collapsed = TRUE, title = "Settings...",
                          selectInput(ns("genoAsFixedSta"), "Return BLUEs", choices = list(TRUE,FALSE), selected = TRUE),
                          numericInput(ns("maxitSta"), "Number of iterations", value = 35),
                          selectInput(ns("verboseSta"), "Print logs", choices = list(TRUE,FALSE), selected = FALSE)
      ),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      actionButton(ns("runSta"), "Run", icon = icon("play-circle")),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      shinycssloaders::withSpinner(textOutput(ns("outSta")),type=8)
    ), # end sidebarpanel
    mainPanel(tabsetPanel(
      type = "tabs",

      tabPanel("Data",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE, #collapsible = TRUE, collapsed = FALSE,
                                   column(width=12,shinycssloaders::withSpinner(DT::DTOutput(ns("phenoSta"))),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
               )
      ),
      tabPanel("Predictions",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE, #collapsible = TRUE, collapsed = FALSE,
                                   column(width=12,DT::DTOutput(ns("predictionsSta")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
               )
      ),
      tabPanel("Metrics",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE, #collapsible = TRUE, collapsed = FALSE,
                                   column(width=12,DT::DTOutput(ns("metricsSta")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
               )
      ),
      tabPanel("Modeling",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE, #collapsible = TRUE, collapsed = FALSE,
                                   column(width=12,DT::DTOutput(ns("modelingSta")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
               )
      ),
      tabPanel("Documentation",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE, #collapsible = TRUE, collapsed = FALSE,
                                   column(width=12,style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
               )
      ),
      tabPanel("References",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE, #collapsible = TRUE, collapsed = FALSE,
                                   column(width=12,style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
               )
      )
    )) # end mainpanel

  )
}

#' staApp Server Functions
#'
#' @noRd
mod_staApp_server <- function(id,data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Create the fields
    observe({
      req(data())
      genetic.evaluation <- c("designation", "mother","father")
      updateSelectInput(session, "genoUnitSta",choices = genetic.evaluation)
    })
    observeEvent(input$genoUnitSta, {
      req(data())
      dtSta <- data()$data$pheno
      dNames <- names(dtSta)
      updateSelectInput(session, "fixedTermSta2",choices = dNames[dNames!=input$genoUnitSta])
    })
    observeEvent(c(input$genoUnitSta,input$fixedTermSta2), {
      req(data())
      dtTraitsSta <- data()$metadata$pheno
      traitsSta <- dtTraitsSta[dtTraitsSta$parameter=="trait","value"]
      updateSelectInput(session, "trait2Sta",choices = traitsSta)
    })

    # reactive table for trait family distributions
    dtDistTrait = reactive({
      req(data())
      dtSta <- data()$data$pheno
      req(input$trait2Sta)
      traitNames = input$trait2Sta
      mm = matrix(0,nrow = 8, ncol = length(traitNames));
      rownames(mm) <- c(
        "gaussian(link = 'identity')",
        "quasi(link = 'identity', variance = 'constant')",
        "binomial(link = 'logit')",
        "Gamma(link = 'inverse')",
        "inverse.gaussian(link = '1/mu^2')",
        "poisson(link = 'log')",
        "quasibinomial(link = 'logit')",
        "quasipoisson(link = 'log')"
      );
      colnames(mm) <- traitNames
      dtProvTable = as.data.frame(mm);  colnames(dtProvTable) <- traitNames
      return(dtProvTable)
    })
    xx = reactiveValues(df = NULL)
    observe({
      df <- dtDistTrait()
      xx$df <- df
    })
    output$traitDistSta = DT::renderDT(xx$df,
                                       selection = 'none',
                                       editable = TRUE,
                                       options = list(paging=FALSE,
                                                      searching=FALSE,
                                                      initComplete = I("function(settings, json) {alert('Done.');}")
                                       )
    )

    proxy = DT::dataTableProxy('traitDistSta')

    observeEvent(input$traitDistSta_cell_edit, {
      info = input$traitDistSta_cell_edit
      utils::str(info)
      i = info$row
      j = info$col
      v = info$value
      xx$df[i, j] <- isolate(DT::coerceValue(v, xx$df[i, j]))
    })
    ## render the data to be analyzed
    output$phenoSta <-  DT::renderDT({
      req(data())
      dtSta <- data()$data$pheno
        DT::datatable(dtSta,
                      options = list(autoWidth = TRUE),
                      filter = "top" # This will position the column filters at the top
        )


    })

    ## render result of "run" button click
    # outSta <- observeEvent(input$runSta, {
    outSta <- eventReactive(input$runSta, {
      req(data())
      req(input$trait2Sta)
      req(input$genoUnitSta)
      myFamily = apply(xx$df,2,function(y){rownames(xx$df)[which(y > 0)[1]]})
      dontHaveDist <- which(is.na(myFamily))
      if(length(dontHaveDist) > 0){myFamily[dontHaveDist] <- "gaussian(link = 'identity')"}
      # run the modeling
      result <- try(cgiarPipe::staLMM(phenoDTfile = data(),
                                      trait=input$trait2Sta, traitFamily = myFamily,
                                      fixedTerm = input$fixedTermSta2,
                                      returnFixedGeno=input$genoAsFixedSta, genoUnit = input$genoUnitSta,
                                      verbose = input$verboseSta, maxit = input$maxitSta),
                    silent=TRUE
      )
      output$predictionsSta <-  DT::renderDT({
        if(!inherits(result,"try-error") ){
          predictions <- result$predictions
          predictions <- predictions[predictions$module=="sta",]
          current.predictions <- predictions[predictions$analysisId==max(predictions$analysisId),]
          current.predictions <- subset(current.predictions, select = -c(module,analysisId))
          numeric.output <- c("predictedValue", "stdError", "reliability")
          DT::formatRound(DT::datatable(current.predictions,
                        options = list(autoWidth = TRUE),
                        filter = "top"
          ), numeric.output)
        }
      })

      output$metricsSta <-  DT::renderDT({
        if(!inherits(result,"try-error") ){
          metrics <- result$metrics
          metrics <- metrics[metrics$module=="sta",]
          current.metrics <- metrics[metrics$analysisId==max(metrics$analysisId),]
          current.metrics <- subset(current.metrics, select = -c(module,analysisId))
          numeric.output <- c("value", "stdError")
          DT::formatRound(DT::datatable(current.metrics,
                        options = list(autoWidth = TRUE),
                        filter = "top"
          ), numeric.output)
        }
      })

      output$modelingSta <-  DT::renderDT({
        if(!inherits(result,"try-error") ){
          modeling <- result$modeling
          modeling <- modeling[modeling$module=="sta",]
          current.modeling <- modeling[modeling$analysisId==max(modeling$analysisId),]
          DT::datatable(current.modeling,
                        options = list(autoWidth = TRUE),
                        filter = "top"
          )
        }
      })


    })

    output$outSta <- renderPrint({
      outSta()
    })


## end
  })
}

## To be copied in the UI
# mod_staApp_ui("staApp_1")

## To be copied in the server
# mod_staApp_server("staApp_1")

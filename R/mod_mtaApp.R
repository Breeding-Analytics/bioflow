#' mtaApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mtaApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    sidebarPanel(

      tags$style(".well {background-color:grey; color: #FFFFFF;}"),
      div(tags$p( "Multi Trial Analysis")),#, style = "color: #817e7e"
      hr(style = "border-top: 1px solid #4c4c4c;"),
      selectInput(ns("version2Mta"), "Version to analyze", choices = NULL, multiple = FALSE),
      selectInput(ns("trait2Mta"), "Trait(s) to analyze", choices = NULL, multiple = TRUE),
      selectInput(ns("fixedTermMta2"), "Fixed effect(s)", choices = NULL, multiple = TRUE),
      selectInput(ns("randomTermMta2"), "Random effect(s)", choices = NULL, multiple = TRUE),
      selectInput(ns("interactionTermMta2"), "GxE term(s)", choices = NULL, multiple = TRUE),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      shinydashboard::box(width = 12, status = "primary", background="light-blue",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Fields to include...",
                          column(width = 12,DT::dataTableOutput(ns("fieldsMet")), style = "height:400px; overflow-y: scroll;overflow-x: scroll;")
      ),
      shinydashboard::box(width = 12, status = "primary", background="light-blue",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Trait distributions...",
                          column(width = 12,DT::DTOutput(ns("traitDistMet")), style = "height:400px; overflow-y: scroll;overflow-x: scroll;")
      ),
      shinydashboard::box(width = 12, status = "primary", background="light-blue",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Settings...",
                          selectInput(ns("deregressMet"), label = "Deregress Predictions?",  choices = list(TRUE,FALSE), selected = FALSE, multiple=FALSE),
                          textInput(ns("heritLBMet"), label = "Lower H2&R2 bound per trait (separate by commas) or single value across", value="0.2"),
                          textInput(ns("heritUBMet"), label = "Upper H2&R2 bound per trait (separate by commas) or single value across", value="0.95"),
                          numericInput(ns("maxitMet"), label = "Number of iterations", value = 70),
                          selectInput(ns("scaledDesireMet"), label = "Scale desire file", choices = list(TRUE,FALSE), selected = TRUE, multiple=FALSE),
                          selectInput(ns("useWeights"), label = "Use weights?", choices = list(TRUE,FALSE), selected = TRUE, multiple=FALSE),
                          selectInput(ns("modelMet"), label = "Method", choices = list(BLUP="blup",pBLUP="pblup",gBLUP="gblup",ssGBLUP="ssgblup",rrBLUP="rrblup"), selected = "blup", multiple=FALSE),
                          numericInput(ns("nPC"), label = "Number of PCs if method is rrBLUP", value = 0)
      ),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      actionButton(ns("runMta"), "Run", icon = icon("play-circle")),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      uiOutput(ns("qaQcMtaInfo")),
      shinycssloaders::withSpinner(textOutput(ns("outMta")),type=8)
    ), # end sidebarpanel
    mainPanel(tabsetPanel(
      type = "tabs",

      tabPanel("Data",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,DT::DTOutput(ns("phenoMta")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
               )
      ),
      tabPanel("Predictions",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,DT::DTOutput(ns("predictionsMta")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
               )
      ),
      tabPanel("Metrics",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,br(),DT::DTOutput(ns("metricsMta")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
               )
      ),
      tabPanel("Modeling",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,DT::DTOutput(ns("modelingMta")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
               )
      ),
      tabPanel("Report",
               # br(),
               # div(tags$p("Please download the report below:") ),
               # br(),
               # uiOutput(ns('reportMta')),
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

#' mtaApp Server Functions
#'
#' @noRd
mod_mtaApp_server <- function(id){
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
      data <- xx
      return(data)
    })
    #################
    ## version
    observeEvent(c(data()), {
      req(data())
      dtMta <- data()
      dtMta <- dtMta$status
      dtMta <- dtMta[which(dtMta$module == "sta"),]
      traitsMta <- unique(dtMta$analysisId)
      updateSelectInput(session, "version2Mta", choices = traitsMta)
    })
    #################
    ## traits
    observeEvent(c(data(), input$version2Mta), {
      req(data())
      req(input$version2Mta)
      dtMta <- data()
      dtMta <- dtMta$predictions
      dtMta <- dtMta[which(dtMta$analysisId == input$version2Mta),]
      traitsMta <- unique(dtMta$trait)
      updateSelectInput(session, "trait2Mta", choices = traitsMta)
    })
    #################
    ## fixed effects
    observeEvent(c(data(),input$version2Mta, input$trait2Mta), {
      req(data())
      req(input$version2Mta)
      req(input$trait2Mta)
      dtMta <- data()
      dtMta <- dtMta$predictions
      dtMta <- dtMta[which(dtMta$analysisId == input$version2Mta),]
      traitsMta <- apply(dtMta[,c("environment","designation","entryType","pipeline")],2,function(x){length(unique(x))})
      traitsMta <- names(traitsMta)[which(traitsMta > 1)]
      updateSelectInput(session, "fixedTermMta2", choices = traitsMta)
    })
    #################
    ## random effects
    observeEvent(c(data(), input$version2Mta, input$trait2Mta, input$fixedTermMta2 ), {
      req(data())
      req(input$version2Mta)
      req(input$trait2Mta)
      req(input$fixedTermMta2)
      dtMta <- data()
      dtMta <- dtMta$predictions
      dtMta <- dtMta[which(dtMta$analysisId == input$version2Mta),]
      traitsMta <- apply(dtMta[,c("environment","designation","entryType","pipeline")],2,function(x){length(unique(x))})
      traitsMta <- names(traitsMta)[which(traitsMta > 1)]
      traitsMta <- setdiff(traitsMta, input$fixedTermMta2)
      updateSelectInput(session, "randomTermMta2", choices = traitsMta)
    })
    #################
    ## gXe interactions
    observeEvent(c(data(), input$trait2Mta), {
      req(data())
      req(input$trait2Mta)
      dtMta <- data()
      dtMta <- dtMta$metadata$weather
      traitsMta <- unique(c("envIndex",unique(dtMta$parameter)))
      updateSelectInput(session, "interactionTermMta2", choices = traitsMta)
    })
    #################
    # reactive table for environments to be included
    dtFieldMet = reactive({
      req(data())
      req(input$version2Mta)
      dtMta <- data()
      dtProv = dtMta$predictions
      dtProv <- dtProv[which(dtProv$analysisId == input$version2Mta),]
      dtProvTable=  as.data.frame( do.call( rbind, list (with(dtProv, table(environment,trait)) ) ) )
      bad <- which(dtProvTable <= 1, arr.ind = TRUE)
      if(nrow(bad) > 0){dtProvTable[bad] = 0}
      dtProvTable[which(dtProvTable > 1, arr.ind = TRUE)] = 1
      return(dtProvTable)
    })
    x = reactiveValues(df = NULL)
    observe({
      df <- dtFieldMet()
      x$df <- df
    })
    output$fieldsMet = DT::renderDT(x$df, selection = 'none', editable = TRUE)
    proxy = DT::dataTableProxy('fieldsMet')
    observeEvent(input$fieldsMet_cell_edit, {
      info = input$fieldsMet_cell_edit
      utils::str(info)
      i = info$row
      j = info$col
      v = info$value
      x$df[i, j] <- isolate(DT::coerceValue(v, x$df[i, j]))
    })
    #################
    # reactive table for trait family distributions
    dtDistTrait = reactive({
      traitNames = input$trait2Mta
      mm = matrix(0,nrow = 8, ncol = length(traitNames));
      rownames(mm) <- c(
        "quasi(link = 'identity', variance = 'constant')", "gaussian(link = 'identity')", "binomial(link = 'logit')",  "Gamma(link = 'inverse')",
        "inverse.gaussian(link = '1/mu^2')", "poisson(link = 'log')", "quasibinomial(link = 'logit')", "quasipoisson(link = 'log')"
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
    output$traitDistMet = DT::renderDT(xx$df,
                                       selection = 'none',
                                       editable = TRUE,
                                       options = list(paging=FALSE,
                                                      searching=FALSE,
                                                      initComplete = I("function(settings, json) {alert('Done.');}")
                                       )
    )
    proxy = DT::dataTableProxy('traitDistMet')
    observeEvent(input$traitDistMet_cell_edit, {
      info = input$traitDistMet_cell_edit
      utils::str(info)
      i = info$row
      j = info$col
      v = info$value
      xx$df[i, j] <- isolate(DT::coerceValue(v, xx$df[i, j]))
    })

    ##############################################################################################
    ##############################################################################################
    ##############################################################################################
    ## render the data to be analyzed
    output$phenoMta <-  DT::renderDT({
        req(data())
        req(input$version2Mta)
        dtMta <- data()
        dtMta <- dtMta$predictions
        dtMta <- dtMta[which(dtMta$analysisId == input$version2Mta),setdiff(colnames(dtMta),c("module","analysisId"))]
        DT::datatable(dtMta,
                      options = list(autoWidth = TRUE),
                      filter = "top"
        )
    })

    ## render result of "run" button click
    outMta <- eventReactive(input$runMta, {
      req(data())
      req(input$trait2Mta)
      dtMta <- data()

      # family distributions input
      myFamily = apply(xx$df,2,function(y){rownames(xx$df)[which(y > 0)[1]]})
      dontHaveDist <- which(is.na(myFamily))
      if(length(dontHaveDist) > 0){myFamily[dontHaveDist] <- "quasi(link = 'identity', variance = 'constant')"}

      # run the modeling, but before test if sta was done
      if(sum(dtMta$status$module %in% "sta") == 0) {
        output$qaQcMtaInfo <- renderUI({
          if (hideAll$clearAll)
            return()
          else
            req(dtMta)
          HTML(as.character(div(style="color: brown;",
                                "Please perform Single-Trial-Analysis before conducting a Multi-Trial Analysis when using a two-stage analysis."))
          )
        })
      }else{
        output$qaQcMtaInfo <- renderUI({return(NULL)})
        result <- try(cgiarPipeline::metLMM(
          phenoDTfile= dtMta, # analysis to be picked from predictions database
          analysisId=input$version2Mta,
          fixedTerm= input$fixedTermMta2,  randomTerm=input$randomTermMta2,  residualBy=NULL,
          interactionsWithGeno=input$interactionTermMta2, envsToInclude=x$df,
          trait= input$trait2Mta, traitFamily=myFamily, useWeights=input$useWeights,
          heritLB= as.numeric(unlist(strsplit(input$heritLBMet,","))),
          heritUB= as.numeric(unlist(strsplit(input$heritUBMet,","))),
          modelType=input$modelMet, # either "grm", "nrm", or both
          deregress=input$deregressMet,  nPC=input$nPC,
          maxIters=input$maxitMet, batchSizeToPredict=500, tolParInv=1e-4,
          verbose=TRUE
        ),
        silent=TRUE
        )
        # data(result) # update data with results
      }

      if(!inherits(result,"try-error")) {

        output$predictionsMta <-  DT::renderDT({
          if(!inherits(result,"try-error") ){
            if ( hideAll$clearAll)
              return()
            else
              predictions <- result$predictions
            predictions <- predictions[predictions$module=="mta",]

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

        output$metricsMta <-  DT::renderDT({
          if(!inherits(result,"try-error") ){
            if ( hideAll$clearAll)
              return()
            else
              metrics <- result$metrics
            metrics <- metrics[metrics$module=="mta",]
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

        output$modelingMta <-  DT::renderDT({
          if(!inherits(result,"try-error") ){
            if ( hideAll$clearAll)
              return()
            else
              modeling <- result$modeling
            modeling <- modeling[modeling$module=="mta",]

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
        output$predictionsMta <- DT::renderDT({DT::datatable(NULL)})
        output$metricsMta <- DT::renderDT({DT::datatable(NULL)})
        output$modelingMta <- DT::renderDT({DT::datatable(NULL)})
      }

      hideAll$clearAll <- FALSE

    }) ## end eventReactive

    output$outMta <- renderPrint({
      outMta()
    })

    output$reportMta <- renderUI({
      HTML(markdown::markdownToHTML(knitr::knit("./R/testing_sta.Rmd", quiet = TRUE), fragment.only=TRUE))
    })



  })
}

## To be copied in the UI
# mod_mtaApp_ui("mtaApp_1")

## To be copied in the server
# mod_mtaApp_server("mtaApp_1")

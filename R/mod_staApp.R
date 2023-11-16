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

      # tags$head(tags$style(HTML("
      #                          body {
      #                             width: 100% !important;
      #                             max-width: 100% !important;
      #                          }
      #
      #                          "))),


      tags$style(".well {background-color:grey; color: #FFFFFF;}"),
      div(tags$p( "Single Trial Analysis")),#, style = "color: #817e7e"
      hr(style = "border-top: 1px solid #4c4c4c;"),
      selectInput(ns("genoUnitSta"), "Genetic evaluation unit(s)", choices = NULL, multiple = TRUE),
      selectInput(ns("trait2Sta"), "Trait(s) to analyze", choices = NULL, multiple = TRUE),
      selectInput(ns("fixedTermSta2"), "Covariable(s)", choices = NULL, multiple = TRUE),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      shinydashboard::box(width = 12, status = "primary", background="light-blue",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Settings...",
                          selectInput(ns("genoAsFixedSta"),"Predictions",choices=list("BLUEs"=TRUE,"BLUPs"=FALSE),selected=TRUE),
                          numericInput(ns("maxitSta"),"Number of iterations",value=35),
                          selectInput(ns("verboseSta"),"Print logs",choices=list("Yes"=TRUE,"No"=FALSE),selected=FALSE)
      ),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      actionButton(ns("runSta"), "Run", icon = icon("play-circle")),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      uiOutput(ns("qaQcStaInfo")),
      shinycssloaders::withSpinner(textOutput(ns("outSta")),type=8)
    ), # end sidebarpanel
    mainPanel(tabsetPanel(
      type = "tabs",

      tabPanel("Data",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,DT::DTOutput(ns("phenoSta")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
               )
      ),
      tabPanel("Predictions",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,DT::DTOutput(ns("predictionsSta")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
               )
      ),
      tabPanel("Metrics",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,br(),DT::DTOutput(ns("metricsSta")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
               )
      ),
      tabPanel("Modeling",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,DT::DTOutput(ns("modelingSta")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
               )
      ),
      # tabPanel("Report",
      #          br(),
      #          shinydashboard::box(status="primary",width = 12,
      #                              solidHeader = TRUE,
      #                              column(width=12,
      #                                     div(tags$p("Please download the report below:") ),
      #                                     br(),
      #                                     uiOutput(ns('reportSta')),
      #                                     includeHTML("/Users/idieng/Library/CloudStorage/OneDrive-CGIAR/_IITA/_Shiny/lmm_golem/bioflow/R/report_STA2.html"),
      #                                     style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
      #          )
      # ),
      tabPanel("Report",
               br(),
               div(tags$p("Please download the report below:") ),
               downloadButton(ns("downloadReportSta"), "Download report"),
               br(),
               uiOutput(ns('reportSta')),
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

    # Create the fields
    observe({
      req(data())
      genetic.evaluation <- c("designation", "mother","father")
      updateSelectInput(session, "genoUnitSta",choices = genetic.evaluation)
    })

    observeEvent(c(data(),input$genoUnitSta), {
      req(data())
      req(input$genoUnitSta)
      dtSta <- data()
      dtSta <- dtSta$metadata$pheno
      traitsSta <- dtSta[dtSta$parameter=="trait","value"]
      updateSelectInput(session, "trait2Sta", choices = traitsSta)
    })

    observeEvent(c(data(),input$genoUnitSta, input$trait2Sta), {
      req(data())
      req(input$genoUnitSta)
      req(input$trait2Sta)
      dtSta <- data()
      dtSta <- dtSta$metadata$pheno
      traitsSta <- dtSta[dtSta$parameter=="trait","value"]
      updateSelectInput(session, "fixedTermSta2", choices = traitsSta[traitsSta!=input$trait2Sta])
    })

    # reactive table for trait family distributions
    dtDistTrait = reactive({
      req(data())
      dtSta <- data()
      dtSta <- dtSta$data$pheno
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
    observeEvent(data(),{
      if(sum(data()$status$module %in% "qa") != 0) {
        output$phenoSta <-  DT::renderDT({
          # if ( hideAll$clearAll){
          #   return()
          # }else{
            req(data())
            dtSta <- data()
            dtSta <- dtSta$data$pheno
            DT::datatable(dtSta,
                          options = list(autoWidth = TRUE),
                          filter = "top"
            )
          # }
        })
      } else {
        output$phenoSta <- DT::renderDT({DT::datatable(NULL)})
      }
    })

    ## render result of "run" button click
    outSta <- eventReactive(input$runSta, {
      req(data())
      req(input$trait2Sta)
      req(input$genoUnitSta)
      dtSta <- data()

      myFamily = apply(xx$df,2,function(y){rownames(xx$df)[which(y > 0)[1]]})
      dontHaveDist <- which(is.na(myFamily))
      if(length(dontHaveDist) > 0){myFamily[dontHaveDist] <- "gaussian(link = 'identity')"}

      # run the modeling, but before test if qa/qc done
      if(sum(dtSta$status$module %in% c("qa","qaRaw","qaRes")) == 0) {
        output$qaQcStaInfo <- renderUI({
          if (hideAll$clearAll)
            return()
          else
            req(dtSta)
            HTML(as.character(div(style="color: brown;",
                                "Please perform QA/QC before conducting a Single-Trial Analysis."))
            )
        })
      } else {
        output$qaQcStaInfo <- renderUI({return(NULL)})
        result <- try(cgiarPipeline::staLMM(phenoDTfile = dtSta,
                                        trait=input$trait2Sta, traitFamily = NULL,
                                        fixedTerm = input$fixedTermSta2,
                                        returnFixedGeno=input$genoAsFixedSta, genoUnit = input$genoUnitSta,
                                        verbose = input$verboseSta, maxit = input$maxitSta),
                      silent=TRUE
        )
        if(!inherits(result,"try-error") ){
          data(result)
          print("Sta finished succesfully")
          print(data()$status)
        }else{
          print(result)
        }
      }

      if(sum(dtSta$status$module %in% "qa") != 0) {

        output$predictionsSta <-  DT::renderDT({
          if(!inherits(result,"try-error") ){
            # if ( hideAll$clearAll){
            #   return()
            # }else{
              predictions <- data()$predictions
              predictions <- predictions[predictions$module=="sta",]
              predictions$analysisId <- as.numeric(predictions$analysisId)
              predictions <- predictions[!is.na(predictions$analysisId),]
              current.predictions <- predictions[predictions$analysisId==max(predictions$analysisId),]
              current.predictions <- subset(current.predictions, select = -c(module,analysisId))
              numeric.output <- c("predictedValue", "stdError", "reliability")
              DT::formatRound(DT::datatable(current.predictions, options = list(autoWidth = TRUE),filter = "top"), numeric.output)
            # }
          }
        })

        output$metricsSta <-  DT::renderDT({
          if(!inherits(result,"try-error") ){
            # if ( hideAll$clearAll){
            #   return()
            # }else{
              metrics <- data()$metrics
              metrics <- metrics[metrics$module=="sta",]
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
          }
        })

        output$modelingSta <-  DT::renderDT({
          if(!inherits(result,"try-error") ){
            # if ( hideAll$clearAll){
            #   return()
            # }else{
              modeling <- data()$modeling
              modeling <- modeling[modeling$module=="sta",]

              modeling$analysisId <- as.numeric(modeling$analysisId)
              modeling <- modeling[!is.na(modeling$analysisId),]

              current.modeling <- modeling[modeling$analysisId==max(modeling$analysisId),]
              current.modeling <- subset(current.modeling, select = -c(module,analysisId))
              DT::datatable(current.modeling,
                            options = list(autoWidth = TRUE),
                            filter = "top"
              )
            # }
          }
        })
        data(result)
      } else {
        output$predictionsSta <- DT::renderDT({DT::datatable(NULL)})
        output$metricsSta <- DT::renderDT({DT::datatable(NULL)})
        output$modelingSta <- DT::renderDT({DT::datatable(NULL)})
      }

      hideAll$clearAll <- FALSE

    }) ## end eventReactive

    output$outSta <- renderPrint({
      outSta()
    })

    # output$reportSta <- renderUI({
    #   rmarkdown::render(
    #     input = ("/Users/idieng/Library/CloudStorage/OneDrive-CGIAR/_IITA/_Shiny/lmm_golem/bioflow/R/report_STA2.Rmd"),
    #     params = list(
    #       data_rds_sta = "/Users/idieng/Library/CloudStorage/OneDrive-CGIAR/_IITA/_Shiny/lmm_golem/bioflow/R/result.rds"
    #     )
    #   )
    # })

    output$reportSta <- renderUI({
      HTML(markdown::markdownToHTML(knitr::knit("./R/testing_sta.Rmd", quiet = TRUE), fragment.only=TRUE))
    })

    # return(data)

    #### for downloading report STA
    # cleaned data reactive object
    phenoSta <-  reactive({
      if(sum(data()$status$module %in% "qa") != 0) {
        if (hideAll$clearAll){
          return(NULL)
        }
        else{
          req(data())
          dtSta <- data()
          dtSta <- dtSta$data$pheno
          return(dtSta)
        }
      } else {
        return(NULL)
      }
    })

    # metrics reactive object
    # metricsSta <-  reactive({
    #   if(!inherits(result,"try-error") ){
    #     if ( hideAll$clearAll){
    #       return(NULL)
    #     }
    #     else{
    #       metrics <- result$metrics
    #       metrics <- metrics[metrics$module=="sta",]
    #       metrics$analysisId <- as.numeric(metrics$analysisId)
    #       metrics <- metrics[!is.na(metrics$analysisId),]
    #       current.metrics <- metrics[metrics$analysisId==max(metrics$analysisId),]
    #       current.metrics <- subset(current.metrics, select = -c(module,analysisId))
    #       return(current.metrics)
    #     }
    #   }
    # })

    output$downloadReportSta <- downloadHandler(
      filename = function() {
        paste0("reportSTA-",gsub("-|:| ", "", Sys.time()),".html")
      },
      content = function(file) {
        shinybusy::show_modal_spinner(spin = "fading-circle",
                                      color = "#F39C12",
                                      text = "Generating Report...")

        rmarkdown::render(
          # input RMD file
          input = ("R/report_STA.Rmd"),

          # input RMD parameters ----
          params = list(
            ## phenotypic data (required) ----
            data_rds_clp = phenoSta(),
            traits = input$trait2Sta,
            #data_rds_sta = metricsSta(),

            ## file path and report parameters ----
            doc_title = "Single-Trial Analysis",
            doc_subtitle = "<subtitle>"
          ), output_file = file)
        shinybusy::remove_modal_spinner()
      }, contentType = "html"
    )

  }) ## end moduleserver
}

## To be copied in the UI
# mod_staApp_ui("staApp_1")

## To be copied in the server
# mod_staApp_server("staApp_1")

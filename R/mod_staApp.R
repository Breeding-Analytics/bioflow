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
      div(tags$p( h4(strong("Single Trial Analysis")))),#, style = "color: #817e7e"
      hr(style = "border-top: 1px solid #4c4c4c;"),
      # input <- list(version2Sta=)
      selectInput(ns("version2Sta"), "Data QA version(s) to consider", choices = NULL, multiple = TRUE),
      selectInput(ns("genoUnitSta"), "Genetic evaluation unit(s)", choices = NULL, multiple = TRUE),
      selectInput(ns("trait2Sta"), "Trait(s) to analyze", choices = NULL, multiple = TRUE),
      selectInput(ns("fixedTermSta2"), "Covariable(s)", choices = NULL, multiple = TRUE),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      shinydashboard::box(width = 12, status = "primary", background="light-blue",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Settings...",
                          selectInput(ns("genoAsFixedSta"),"Predictions",choices=list("BLUEs"=TRUE,"BLUPs"=FALSE),selected=TRUE),
                          numericInput(ns("maxitSta"),"Number of iterations",value=35),
                          selectInput(ns("verboseSta"),"Print logs",choices=list("Yes"=TRUE,"No"=FALSE),selected=FALSE)
      ),
      shinydashboard::box(width = 12, status = "primary", background="light-blue",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Trait distributions (optional)...",
                          column(width = 12,DT::DTOutput(ns("traitDistSta")), style = "height:400px; overflow-y: scroll;overflow-x: scroll;")
      ),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      actionButton(ns("runSta"), "Run", icon = icon("play-circle")),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      uiOutput(ns("qaQcStaInfo")),
      textOutput(ns("outSta"))
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
      tabPanel("Report",
               br(),
               uiOutput(ns('reportSta')),
      ),
      tabPanel("Documentation",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,   style = "height:800px; overflow-y: scroll;overflow-x: scroll;",
                                          h1(strong("Details")),
                                          p("This option aims to fit a genetic evaluation trial by trial, where each trial is one level of the fieldinst
                              column (defined when the user matches the expected columns to columns present in the initial phenotypic input file).
                              Genotype is fitted as both, fixed and random. The user defines which should be returned in the predictions table.
                              By default genotype (geno column) predictions are returned since a two-stage approach is assumed, but user can choose.
                                The way the options are used is the following:"),
                                          p(strong("Fixed effects.-"),"Columns to be fitted as fixed effects in each trial."),
                                          p(strong("Traits to analyze.-")," Traits to be analyzed. If no design factors can be fitted simple means are taken."),
                                          p(strong("Number of iterations.-")," Maximum number of restricted maximum likelihood iterations to be run for each trial-trait combination."),
                                          p(strong("Note.-")," A design-agnostic spatial design is carried. That means, all the spatial-related factors will be fitted if pertinent.
                                For example, if a trial has rowcoord information it will be fitted, if not it will be ignored. A two-dimensional spline kernel is only
                                fitted when the trial size exceeds 5 rows and 5 columns. In addition the following rules are followed: 1) Rows or columns are fitted if
                                you have equal or more than 3 levels, 2) Reps are fitted if you have equal or more than 2 levels, 3) Block (Sub-block) are fitted if you
                                have equal or more than 4 levels. "),
                                          h2(strong("References:")),
                                          p("Velazco, J. G., Rodriguez-Alvarez, M. X., Boer, M. P., Jordan, D. R., Eilers, P. H., Malosetti, M., & Van Eeuwijk, F. A. (2017).
                                Modelling spatial trends in sorghum breeding field trials using a two-dimensional P-spline mixed model. Theoretical and Applied
                                Genetics, 130, 1375-1392."),
                                          p("Rodriguez-Alvarez, M. X., Boer, M. P., van Eeuwijk, F. A., & Eilers, P. H. (2018). Correcting for spatial heterogeneity in plant
                                breeding experiments with P-splines. Spatial Statistics, 23, 52-71."),
                                          h3(strong("Software used:")),
                                          p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
                                Vienna, Austria. URL https://www.R-project.org/."),
                                          p("Boer M, van Rossum B (2022). _LMMsolver: Linear Mixed Model Solver_. R package version 1.0.4.9000.")


                                   )
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

    # QA versions to use
    observeEvent(c(data()), {
      req(data())
      dtMta <- data()
      dtMta <- dtMta$status
      dtMta <- dtMta[which(dtMta$module == "qaRaw"),]
      traitsMta <- unique(dtMta$analysisId)
      updateSelectInput(session, "version2Sta", choices = traitsMta)
    })
    # genetic evaluation unit
    observe({
      req(data())
      genetic.evaluation <- c("designation", "mother","father")
      updateSelectInput(session, "genoUnitSta",choices = genetic.evaluation)
    })
    # traits
    observeEvent(c(data(),input$version2Sta,input$genoUnitSta), {
      req(data())
      req(input$version2Sta)
      req(input$genoUnitSta)
      dtSta <- data()
      dtSta <- dtSta$modifications$pheno
      dtSta <- dtSta[which(dtSta$analysisId %in% input$version2Sta),] # only traits that have been QA
      traitsSta <- unique(dtSta$trait)
      updateSelectInput(session, "trait2Sta", choices = traitsSta)
    })
    # fixed effect covariates
    observeEvent(c(data(),input$version2Sta,input$genoUnitSta, input$trait2Sta), {
      req(data())
      req(input$version2Sta)
      req(input$genoUnitSta)
      req(input$trait2Sta)
      dtSta <- data()
      dtSta <- dtSta$modifications$pheno # only traits that have been QA
      dtSta <- dtSta[which(dtSta$analysisId %in% input$version2Sta),]
      traitsSta <- unique(dtSta$trait)
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
      if(sum(data()$status$module %in% "qaRaw") != 0) {
        output$phenoSta <-  DT::renderDT({
          req(data())
          dtSta <- data()
          dtSta <- dtSta$data$pheno
          traitTypes <- unlist(lapply(dtSta,class))
          numeric.output <- names(traitTypes)[which(traitTypes %in% "numeric")]
          DT::formatRound(DT::datatable(dtSta, extensions = 'Buttons',
                                        options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                       lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All')))
          ), numeric.output)
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
      shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
      dtSta <- data()
      myFamily = apply(xx$df,2,function(y){rownames(xx$df)[which(y > 0)[1]]})
      dontHaveDist <- which(is.na(myFamily))
      if(length(dontHaveDist) > 0){myFamily[dontHaveDist] <- "gaussian(link = 'identity')"}

      # run the modeling, but before test if qa/qc done
      if(sum(dtSta$status$module %in% "qaRaw") == 0) {
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
        result <- try(cgiarPipeline::staLMM(phenoDTfile = dtSta, analysisId=input$version2Sta,
                                            trait=input$trait2Sta, traitFamily = myFamily,
                                            fixedTerm = input$fixedTermSta2,
                                            returnFixedGeno=input$genoAsFixedSta, genoUnit = input$genoUnitSta,
                                            verbose = input$verboseSta, maxit = input$maxitSta),
                      silent=TRUE
        )
        if(!inherits(result,"try-error")) {
          data(result) # update data with results
          save(result, file = "./R/outputs/resultSta.RData")
          cat(paste("Single-trial analysis step with id:",result$status$analysisId[length(result$status$analysisId)],"saved."))
        }else{
          print(result)
        }
      }
      shinybusy::remove_modal_spinner()

      if(sum(dtSta$status$module %in% "qaRaw") != 0) {

        output$predictionsSta <-  DT::renderDT({
          if(!inherits(result,"try-error") ){
            # if ( hideAll$clearAll){
            #   return()
            # }else{
            predictions <- result$predictions
            predictions <- predictions[predictions$module=="sta",]
            predictions$analysisId <- as.numeric(predictions$analysisId)
            predictions <- predictions[!is.na(predictions$analysisId),]
            current.predictions <- predictions[predictions$analysisId==max(predictions$analysisId),]
            current.predictions <- subset(current.predictions, select = -c(module,analysisId))
            numeric.output <- c("predictedValue", "stdError", "reliability")
            DT::formatRound(DT::datatable(current.predictions, extensions = 'Buttons',
                                          options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                         lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All')))
            ), numeric.output)
            # }
          }
        })

        output$metricsSta <-  DT::renderDT({
          if(!inherits(result,"try-error") ){
            # if ( hideAll$clearAll){
            #   return()
            # }else{
            metrics <- result$metrics
            metrics <- metrics[metrics$module=="sta",]
            metrics$analysisId <- as.numeric(metrics$analysisId)
            metrics <- metrics[!is.na(metrics$analysisId),]
            current.metrics <- metrics[metrics$analysisId==max(metrics$analysisId),]
            current.metrics <- subset(current.metrics, select = -c(module,analysisId))
            numeric.output <- c("value", "stdError")
            DT::formatRound(DT::datatable(current.metrics, extensions = 'Buttons',
                                          options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                         lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All')))
            ), numeric.output)
            # }
          }
        })

        output$modelingSta <-  DT::renderDT({
          if(!inherits(result,"try-error") ){
            # if ( hideAll$clearAll){
            #   return()
            # }else{
            modeling <- result$modeling
            modeling <- modeling[modeling$module=="sta",]
            modeling$analysisId <- as.numeric(modeling$analysisId)
            modeling <- modeling[!is.na(modeling$analysisId),]
            current.modeling <- modeling[modeling$analysisId==max(modeling$analysisId),]
            current.modeling <- subset(current.modeling, select = -c(module,analysisId))
            DT::datatable(current.modeling, extensions = 'Buttons',
                                          options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                         lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All')))
            )
            # }
          }
        })
        ## report
        output$reportSta <- renderUI({
          HTML(markdown::markdownToHTML(knitr::knit("./R/reportSta.Rmd", quiet = TRUE), fragment.only=TRUE))
        })

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



  }) ## end moduleserver
}

## To be copied in the UI
# mod_staApp_ui("staApp_1")

## To be copied in the server
# mod_staApp_server("staApp_1")

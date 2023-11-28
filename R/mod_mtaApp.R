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
      div(tags$p( h4(strong("Multi Trial Analysis")))),#, style = "color: #817e7e"
      hr(style = "border-top: 1px solid #4c4c4c;"),
      selectInput(ns("version2Mta"), "STA version to analyze", choices = NULL, multiple = FALSE),
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
      textOutput(ns("outMta"))
    ), # end sidebarpanel
    mainPanel(tabsetPanel(
      type = "tabs",

      tabPanel("Input Data",
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
               br(),
               uiOutput(ns('reportMta'))
               ),
      tabPanel("Documentation",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,   style = "height:800px; overflow-y: scroll;overflow-x: scroll;",
                                          h1(strong("Details")),
                                          p("This option aims to fit a genetic evaluation across trials using the results from the single trial
                              analysis and additionally a relationship matrix between individuals.
                                The way the arguments are used is the following:"),
                                          p(strong("Relationship matrix (optional).-"),"Columns to be fitted as fixed effects."),
                                          p(strong("Traits to analyze.-")," Traits to be analyzed. If no design factors can be fitted simple means are taken."),
                                          p(strong("Fixed effects.-")," Columns to be fitted as fixed effects."),
                                          p(strong("Random effects.-")," Columns to be fitted as random effects."),
                                          p(strong("Residuals by.-")," Column to decide at which level the residuals should be fitted by."),
                                          p(strong("Interactions to fit with genotype.-")," Column to fit as interactions with the genotype effect."),
                                          p(strong("Deregress.-")," A TRUE/FALSE value to decide is the STA predictions should be deregressed. Only to be used if you fitted BLUPs in the STA step."),
                                          p(strong("H2(lower bound).-")," Value of H2 to be used to remove trials with low heritability."),
                                          p(strong("H2(upper bound).-"),"  Value of H2 to be used to remove trials with too high heritability."),
                                          p(strong("Number of iterations.-")," Maximum number of restricted maximum likelihood iterations to be run for each trait."),
                                          p(strong("Scale desire file.-")," A TRUE or FALSE value to decide if the DESIRE (software) input file to be created should assume scaled traits or in their original units (scaled is recommended)."),
                                          p(strong("Use rrBLUP method.-")," A TRUE or FALSE value to decide if the MET should use the rrBLUP approach using either a certain number of principal components or the full matrix using the nPC argument."),
                                          p(strong("nPC.-")," Number of principal components for the big MET. If the value is equal to 0 the classical rrBLUP model is used. Otherwise a principal component model is run according to Odegard et al. (2019)."),
                                          p(strong("Some details.-")," If genotypes are fitted as fixed effects the reliability cannot be properly computed and the surrogate of genetic variance is the variance of BLUEs. If STA predictions were BLUPs the deregress option can be used."),
                                          h2(strong("References:")),
                                          p("Finlay, K. W., & Wilkinson, G. N. (1963). The analysis of adaptation in a plant-breeding programme. Australian journal of agricultural research, 14(6), 742-754."),
                                          p("Henderson Jr, C. R. (1982). Analysis of covariance in the mixed model: higher-level, nonhomogeneous, and random regressions. Biometrics, 623-640."),
                                          p("Odegard, J., Indahl, U., Stranden, I., & Meuwissen, T. H. (2018). Large-scale genomic prediction using singular value decomposition of the genotype matrix. Genetics Selection Evolution, 50(1), 1-12."),
                                          h3(strong("Software used:")),
                                          p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
                                Vienna, Austria. URL https://www.R-project.org/."),
                                          p(" Boer M, van Rossum B (2022). _LMMsolver: Linear Mixed Model Solver_. R package version 1.0.4.9000."),
                                          p("Covarrubias-Pazaran G. 2016. Genome assisted prediction of quantitative traits using the R package sommer. PLoS ONE 11(6):1-15.")
                                          # img(src = "www/met.png", height = 400, width = 700) # add an image
                                   )
               )
      )
    )) # end mainpanel


  )
}

#' mtaApp Server Functions
#'
#' @noRd
mod_mtaApp_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })
    ############################################################################
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
    ## traits for plot
    observeEvent(c(data(), input$version2Mta), {
      req(data())
      req(input$version2Mta)
      dtMta <- data()
      dtMta <- dtMta$predictions
      dtMta <- dtMta[which(dtMta$analysisId == input$version2Mta),]
      traitsMta <- unique(dtMta$trait)
      updateSelectInput(session, "traitPredictionsCorrelation", choices = traitsMta)
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
    ## render the input data to be analyzed
    output$phenoMta <-  DT::renderDT({
      req(data())
      req(input$version2Mta)
      dtMta <- data()
      dtMta <- dtMta$predictions
      dtMta <- dtMta[which(dtMta$analysisId == input$version2Mta),setdiff(colnames(dtMta),c("module","analysisId"))]
      numeric.output <- c("predictedValue", "stdError", "reliability")
      DT::formatRound(DT::datatable(dtMta, extensions = 'Buttons',
                                    options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                   lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All')))
      ), numeric.output)
    })
    ## render result of "run" button click
    outMta <- eventReactive(input$runMta, {
      req(data())
      req(input$trait2Mta)
      shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
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
        resultMta <- try(cgiarPipeline::metLMM(
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
          verbose=FALSE
        ),
        silent=TRUE
        )
        if(!inherits(resultMta,"try-error")) {
          data(resultMta) # update data with results
          cat(paste("Multi-trial analysis step with id:",resultMta$status$analysisId[length(resultMta$status$analysisId)],"saved."))
        }else{
          print(resultMta)
        }
        shinybusy::remove_modal_spinner()
      }

      if(!inherits(resultMta,"try-error")) { # if all goes well in the run
        ## predictions table
        output$predictionsMta <-  DT::renderDT({
          # if ( hideAll$clearAll){
          #   return()
          # }else{
            predictions <- data()$predictions
            predictions <- predictions[predictions$module=="mta",]
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
        })
        # metrics table
        output$metricsMta <-  DT::renderDT({
          if(!inherits(resultMta,"try-error") ){
            # if ( hideAll$clearAll){
            #   return()
            # }else{
              metrics <- data()$metrics
              mtas <- data()$status[which(data()$status$module == "mta"),"analysisId"]; mtaId <- mtas[length(mtas)]
              metrics <- metrics[which(metrics$analysisId == mtaId),]
              metrics <- subset(metrics, select = -c(module,analysisId))
              numeric.output <- c("value", "stdError")
              DT::formatRound(DT::datatable(metrics, extensions = 'Buttons',
                                            options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                           lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All')))
              ), numeric.output)
            # }
          }
        })
        # modeling table
        output$modelingMta <-  DT::renderDT({
          if(!inherits(resultMta,"try-error") ){
            # if ( hideAll$clearAll){
            #   return()
            # }else{
              modeling <- data()$modeling
              mtas <- data()$status[which(data()$status$module == "mta"),"analysisId"]; mtaId <- mtas[length(mtas)]
              modeling <- modeling[which(modeling$analysisId == mtaId),]
              modeling <- subset(modeling, select = -c(module,analysisId))
              DT::datatable(modeling, extensions = 'Buttons',
                                            options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                           lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All')))
              )
            # }
          }
        })
        # ## Report tab
        # # plot correlation between environments
        # output$plotPredictionsCorrelation <-  plotly::renderPlotly({
        #   mydata = data()$predictions
        #   stas <- data()$status[which(data()$status$module == "sta"),"analysisId"];staId <- stas[length(stas)]
        #   mydata <- mydata[which(mydata$analysisId == staId),]
        #   corPlotPredictions(mydata, input$traitPredictionsCorrelation, unitOfCorrelation="designation", correlatedAcross="environment",
        #                      valueForCorrelation="predictedValue",input$checkboxCluster,input$checkboxText, input$checkboxAxis)
        #
        # })
        # # plot correlation between traits
        # output$plotPredictionsCorrelationTraits <-  plotly::renderPlotly({
        #   mydata = data()$predictions
        #   mtas <- data()$status[which(data()$status$module == "mta"),"analysisId"];mtaId <- mtas[length(mtas)]
        #   mydata <- mydata[which(mydata$analysisId == mtaId),]
        #   corPlotPredictions(mydata, traitPredictionsCorrelation=NULL, unitOfCorrelation="designation", correlatedAcross="trait",
        #                      valueForCorrelation="predictedValue",input$checkboxCluster,input$checkboxText, input$checkboxAxis)
        #
        # })
        # # table of predictions in wide format
        # output$tablePredictionsTraitsWide <-  DT::renderDT({
        #   # if ( hideAll$clearAll){
        #   #   return()
        #   # }else{
        #     mydata = data()$predictions
        #     mtas <- data()$status[which(data()$status$module == "mta"),"analysisId"];mtaId <- mtas[length(mtas)]
        #     mydata <- mydata[which(mydata$analysisId == mtaId),]
        #     wide <- stats::reshape(mydata[,c(c("designation"),"trait",c("predictedValue"))], direction = "wide", idvar = c("designation"),
        #                            timevar = "trait", v.names = c("predictedValue"), sep= "_")
        #     colnames(wide) <- gsub("predictedValue_","",colnames(wide))
        #     numeric.output <- colnames(wide)[-c(1)]
        #     DT::formatRound(DT::datatable(wide, extensions = 'Buttons',
        #                                   options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        #                                                  lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All')))
        #     ), numeric.output)
        #   # }
        # })

      } else {
        output$predictionsMta <- DT::renderDT({DT::datatable(NULL)})
        output$metricsMta <- DT::renderDT({DT::datatable(NULL)})
        output$modelingMta <- DT::renderDT({DT::datatable(NULL)})
        hideAll$clearAll <- TRUE
      } ### enf of if(!inherits(resultMta,"try-error"))

      save(resultMta, file = "./R/outputs/resultMta.RData")

      output$reportMta <- renderUI({
        HTML(markdown::markdownToHTML(knitr::knit("./R/reportMta.Rmd", quiet = TRUE), fragment.only=TRUE))
      })

      hideAll$clearAll <- FALSE

    }) ## end eventReactive

    output$outMta <- renderPrint({
      outMta()
    })

  })
}

## To be copied in the UI
# mod_mtaApp_ui("mtaApp_1")

## To be copied in the server
# mod_mtaApp_server("mtaApp_1")

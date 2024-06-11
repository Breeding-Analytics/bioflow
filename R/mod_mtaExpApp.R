#' mtaExpApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mtaExpApp_ui <- function(id){
  ns <- NS(id)
  tagList(


    tags$br(),

    mainPanel(width = 12,
              tabsetPanel( id=ns("tabsMain"),
                           type = "tabs",
                           tabPanel(div(icon("book"), "Information-MTA") ,
                                    br(),
                                    column(width = 6,
                                           h1(strong(span("Multi Trial Analysis", tags$a(href="https://www.youtube.com/watch?v=rR1DhTt25n4&list=PLZ0lafzH_UmclOPifjCntlMzysEB2_2wX&index=7", icon("youtube") , target="_blank"), style="color:darkcyan"))),
                                           h2(strong("Status:")),
                                           uiOutput(ns("warningMessage")),
                                           tags$br(),
                                           img(src = "www/mta.png", height = 300, width = 450), # add an image
                                    ),
                                    column(width = 6, shiny::plotOutput(ns("plotDataDependencies")), ),
                                    column(width = 12,
                                           h2(strong("Details")),
                                           p("The core algorithm of the genetic evaluation using the two-step approach is the multi-trial analysis.
                                          This option aims to model breeding values across environments using the results from the single trial (weighted by the standard errors)
                              analysis and optionally a relationship matrix between individuals.
                                The way the arguments are used is the following:"),

                                           p(strong("Traits to analyze.-")," Traits to be analyzed. If no design factors can be fitted simple means are taken."),
                                           p(strong("Fixed effects.-")," Columns to be fitted as fixed effects."),
                                           p(strong("Random effects.-")," Columns to be fitted as random effects."),
                                           # p(strong("Residuals by.-")," Column to decide at which level the residuals should be fitted by."),
                                           p(strong("Interactions to fit with genotype.-")," Column to fit as interactions with the genotype effect. This only be fitted if the number of environments is 4 or more."),
                                           p(strong("Genetic evaluation model.-"),"One of the following models to use: BLUP, PBLUP, GBLUP, rrBLUP, ssBLUP."),
                                           p(strong("Additional settings:")),
                                           p(strong("Deregress.-")," A TRUE/FALSE value to decide is the STA predictions should be deregressed. Only to be used if you fitted BLUPs in the STA step."),
                                           p(strong("H2(lower bound).-")," Value of H2 to be used to remove trials with low heritability."),
                                           p(strong("H2(upper bound).-"),"  Value of H2 to be used to remove trials with too high heritability."),
                                           p(strong("Number of iterations.-")," Maximum number of restricted maximum likelihood iterations to be run for each trait."),
                                           p(strong("Maximum number of markers if rrBLUP.-")," Maximum number of markers that should be used if an rrBLUP genetic evaluation method is selected. Under high density marker SNP panels the user may want to reduce the complexity of the marker-based model, the default is 1000. The markers will be randomly sampled without replacement."),
                                           p(strong("Use weights.-")," a TRUE/FALSE statement indicating if the analysis should be weighted using the standard errors from the single trial analysis. The default is TRUE and should not be modified unless you know what you are doing."),
                                           p(strong("nPC.-")," Number of principal components for the big MET. If the value is equal to 0 the classical rrBLUP model is used. Otherwise a principal component model is run according to Odegard et al. (2019)."),
                                           h2(strong("References:")),
                                           p("Finlay, K. W., & Wilkinson, G. N. (1963). The analysis of adaptation in a plant-breeding programme. Australian journal of agricultural research, 14(6), 742-754."),
                                           p("Henderson Jr, C. R. (1982). Analysis of covariance in the mixed model: higher-level, nonhomogeneous, and random regressions. Biometrics, 623-640."),
                                           p("Odegard, J., Indahl, U., Stranden, I., & Meuwissen, T. H. (2018). Large-scale genomic prediction using singular value decomposition of the genotype matrix. Genetics Selection Evolution, 50(1), 1-12."),
                                           h2(strong("Software used:")),
                                           p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
                                Vienna, Austria. URL https://www.R-project.org/."),
                                           p("Boer M, van Rossum B (2022). _LMMsolver: Linear Mixed Model Solver_. R package version 1.0.4.9000."),
                                           p("Covarrubias-Pazaran G. 2016. Genome assisted prediction of quantitative traits using the R package sommer. PLoS ONE 11(6):1-15."),
                                    ),
                           ),
                           tabPanel(div(icon("arrow-right-to-bracket"), "Input"),
                                    tabsetPanel(
                                      tabPanel("Pick STA-stamp", icon = icon("dice-one"),
                                               br(),
                                               column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                      column(width=8, selectInput(ns("version2Mta"), "STA version to analyze (required)", choices = NULL, multiple = TRUE)),
                                                      column(width=4, tags$br(),
                                                             shinyWidgets::prettySwitch( inputId = ns('launch'), label = "Load example", status = "success"),
                                                      ),
                                               ),
                                               column(width=12,
                                                      hr(style = "border-top: 3px solid #4c4c4c;"),
                                                      h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameter values to be specified in the grey boxes above.", style="color:green"))),
                                                      hr(style = "border-top: 3px solid #4c4c4c;"),
                                               ),
                                               column( width=12, shiny::plotOutput(ns("plotTimeStamps")) ),
                                               DT::DTOutput(ns("statusMta")), # modeling table
                                      ),
                                      tabPanel("Pick the model", icon = icon("dice-three"),
                                               br(),
                                               column(width=12, style = "background-color:grey; color: #FFFFFF",

                                                      column(width=12,
                                                             column(width=4, selectInput(ns("trait2Mta"), "Trait to analyze (required)", choices = NULL, multiple = FALSE) ),
                                                             column(width=2, numericInput(ns("nTerms"), label = "nTerms", value = 2, step = 1, min = 1, max=100) ),
                                                             column(width=3, selectInput(ns("modelMet"), "Designation model", choices = NULL, multiple = FALSE) ),
                                                             column(width=3, tags$span(id = ns('ismarkermodel'), selectInput(ns("versionMarker2Mta"), "Marker QA version to use", choices = NULL, multiple = FALSE), ),  ),
                                                      ),
                                                      column(width=12,
                                                             column(width=4, uiOutput(ns("leftSides")) ),
                                                             column(width=2, uiOutput(ns("center")) ),
                                                             column(width=4, uiOutput(ns("rightSides")) ),
                                                             column(width=2, uiOutput(ns("nPC")) ),
                                                      ),
                                               ),
                                               column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                      shinydashboard::box(width = 12, status = "success", background="green",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Additional model settings...",
                                                                          selectInput(ns("deregressMet"), label = "Deregress Predictions?",  choices = list(TRUE,FALSE), selected = FALSE, multiple=FALSE),
                                                                          textInput(ns("heritLBMet"), label = "Lower H2&R2 bound per trait (separate by commas) or single value across", value="0.2"),
                                                                          textInput(ns("heritUBMet"), label = "Upper H2&R2 bound per trait (separate by commas) or single value across", value="0.95"),
                                                                          textInput(ns("meanLBMet"), label = "Lower environment-mean bound per trait (separate by commas) or single value across", value="0"),
                                                                          textInput(ns("meanUBMet"), label = "Upper environment-mean bound per trait (separate by commas) or single value across", value="1000000"),
                                                                          numericInput(ns("maxitMet"), label = "Number of iterations", value = 70),
                                                                          numericInput(ns("nMarkersRRBLUP"), label = "Maximum number of markers to use in rrBLUP", value = 1000),
                                                                          selectInput(ns("useWeights"), label = "Use weights?", choices = list(TRUE,FALSE), selected = TRUE, multiple=FALSE),
                                                                          numericInput(ns("nPC"), label = "Number of PCs if method is rrBLUP", value = 0)
                                                      ),
                                               ),
                                               column(width=12,
                                                      hr(style = "border-top: 3px solid #4c4c4c;"),
                                                      h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameter values to be specified in the grey boxes above.", style="color:green"))),
                                                      hr(style = "border-top: 3px solid #4c4c4c;"),
                                               ),
                                               tags$span(id = ns('holder3'),
                                                         selectInput(ns("evaluationUnitsTrait"), "Trait to visualize", choices = NULL, multiple = FALSE),
                                                         shiny::plotOutput(ns("evaluationUnits")) ,
                                               ),
                                      ),
                                      tabPanel("Run analysis", icon = icon("play"),
                                               column(width=12,style = "background-color:grey; color: #FFFFFF",
                                                      br(),
                                                      actionButton(ns("runMta"), "Run MTA (click)", icon = icon("play-circle")),
                                                      uiOutput(ns("qaQcMtaInfo")),
                                                      br(),
                                               ),
                                               textOutput(ns("outMta")),
                                      ),
                                    )
                           ),
                           tabPanel(div(icon("arrow-right-from-bracket"), "Output" ) , value = "outputTabs",
                                    tabsetPanel(
                                      tabPanel("Predictions", icon = icon("table"),
                                               br(),
                                               DT::DTOutput(ns("predictionsMta")),
                                      ),
                                      tabPanel("Metrics", icon = icon("table"),
                                               br(),
                                               DT::DTOutput(ns("metricsMta")),
                                      ),
                                      tabPanel("Modeling", icon = icon("table"),
                                               br(),
                                               DT::DTOutput(ns("modelingMta")),
                                      ),
                                      tabPanel("Dashboard", icon = icon("file-image"),
                                               br(),
                                               downloadButton(ns("downloadReportMta"), "Download dashboard"),
                                               br(),
                                               uiOutput(ns('reportMta'))
                                      )
                                    ) # end of tabset
                           )# end of output panel
              )) # end mainpanel


  )
}

#' mtaExpApp Server Functions
#'
#' @noRd
mod_mtaExpApp_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #################
    ## version
    observeEvent(c(data()), {
      req(data())
      dtMta <- data()
      dtMta <- dtMta$status
      dtMta <- dtMta[which(dtMta$module == "sta"),]
      traitsMta <- unique(dtMta$analysisId)
      if(length(traitsMta) > 0){names(traitsMta) <- as.POSIXct(traitsMta, origin="1970-01-01", tz="GMT")}
      updateSelectInput(session, "version2Mta", choices = traitsMta)
    })
    #################
    ## traits
    observeEvent(c(data(), input$version2Mta), {
      req(data())
      req(input$version2Mta)
      dtMta <- data()
      dtMta <- dtMta$predictions
      dtMta <- dtMta[which(dtMta$analysisId %in% input$version2Mta),]
      traitsMta <- unique(dtMta$trait)
      updateSelectInput(session, "trait2Mta", choices = traitsMta)
    })
    ## left formula
    output$leftSides <- renderUI({
      req(data())
      req(input$version2Mta)
      req(input$trait2Mta)
      req(input$nTerms)
      dtMta <- data()
      gg <- goodLevels(dtMta, input$version2Mta)
      choices <- c( "1", gg[[input$trait2Mta]])
      lapply(1:input$nTerms, function(i) {
        selectInput(
          session$ns(paste0('leftSides',i)),
          label = ifelse(i==1, "Intercepts",""),
          choices = choices, multiple = TRUE, selected = "1"
        )
      })
    })
    # covariance structure
    output$center <- renderUI({
      req(data())
      req(input$version2Mta)
      req(input$trait2Mta)
      req(input$nTerms)
      lapply(1:input$nTerms, function(i) {
        selectInput(
          session$ns(paste0('center',i)),
          label = ifelse(i==1, "Structure",""),
          choices = list(' (none)'='','| (UNS)'='|','|| (DIAG)'='||'), multiple = FALSE,
          selected = '|'
        )
      })
    })
    # right-side equation
    output$rightSides <- renderUI({
      req(data())
      req(input$version2Mta)
      req(input$trait2Mta)
      req(input$nTerms)
      dtMta <- data()
      gg <- goodLevels(dtMta, input$version2Mta)
      choices <- c("designation",gg[[input$trait2Mta]])
      lapply(1:input$nTerms, function(i) {
        selectInput(
          inputId=session$ns(paste0('rightSides',i)),
          label = ifelse(i==1, "Slopes",""),
          choices = choices, multiple = TRUE,
          selected = choices[i] #ifelse(i==1,"environment",ifelse(i==2, "designation", NULL))
        )
      })
    })
    # n pricipal components
    output$nPC <- renderUI({
      req(data())
      req(input$version2Mta)
      req(input$trait2Mta)
      req(input$nTerms)
      lapply(1:input$nTerms, function(i) {
        numericInput(
          session$ns(paste0('nPC',i)),
          label = ifelse(i==1, "nPC",""),
          value = 0, min = 0, max = Inf, step = 1
        )
      })
    })
    # inputFormula
    inputFormula = reactive({
      req(data())
      req(input$version2Mta)
      req(input$trait2Mta)
      req(input$nTerms)
      if (length(input$trait2Mta) != 0) {
        values <- vector(mode = "list", length = input$nTerms)
        for (i in 1:input$nTerms) {
          # left
          tempval <- reactive({paste0('input$','leftSides',i)})
          s1 <- eval( parse(text = tempval() ) )
          # center
          tempval <- reactive({paste0('input$','center',i)})
          s2 <- eval( parse(text = tempval() ) )
          # right
          tempval <- reactive({paste0('input$','rightSides',i)})
          s3 <- eval( parse(text = tempval() ) )
          # nPC
          tempval <- reactive({paste0('input$','nPC',i)})
          s4 <- eval( parse(text = tempval() ) )
          # join
          prov <- list(s1, s2, s3, s4)
          names(prov) <- c("left","center","right","nPC")
          values[[i]] <- prov
        }
        names(values) <- paste("vcov",1:input$nTerms)
        return(values)
      }
    })
    ## show or not id marker to select
    observeEvent(
      input$modelMet,
      if (input$modelMet %in% c('gblup','rrblup','ssgblup') ) {
        golem::invoke_js('showid', ns('ismarkermodel'))
      } else {
        golem::invoke_js('hideid', ns('ismarkermodel'))
      }
    )

    #################
    ## model types
    observeEvent(c(data(), input$version2Mta, input$trait2Mta), {
      req(data())
      req(input$version2Mta)
      req(input$trait2Mta)
      # if("designation" %in% input$rightSides ){
      #   traitsMta <- list(BLUE="blue")
      # }else{
        traitsMta <- list(BLUP="blup",pBLUP="pblup",gBLUP="gblup",ssGBLUP="ssgblup",rrBLUP="rrblup")
      # }
      updateSelectInput(session, "modelMet", choices = traitsMta, selected = traitsMta[1])
    })

    ## render result of "run" button click
    outMta <- eventReactive(input$runMta, {

      req(data())
      req(input$version2Mta)
      req(input$trait2Mta)
      req(input$nTerms)

      shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
      dtMta <- data()
      inputFormulation <- inputFormula()
      saveRDS(inputFormulation, file = "inputFormulation.rds")
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
      }else{ # sta is available

        output$qaQcMtaInfo <- renderUI({return(NULL)})
        if(input$modelMet %in% c("gblup","rrblup","ssblup") ){ # warning
          if(input$versionMarker2Mta == '' | is.null(input$versionMarker2Mta) ){ # user didn't provide a modifications id
            if(!is.null(dtMta$data$geno)){ # if user actually has marker data
              shinybusy::remove_modal_spinner()
              cat("Please run the 'Markers QA/QC' module prior to run a gBLUP or rrBLUP model.")
              # stop("Please run the 'Markers QA/QC' module prior to run a gBLUP or rrBLUP model.", call. = FALSE)
            }else{ # if user does NOT have marker data and wanted a marker-based model
              shinybusy::remove_modal_spinner()
              cat("Please pick a different model, rrBLUP, gBLUP and ssBLUP require marker information. Alternatively, go back to the 'Retrieve Data' section and upload your marker data.")
              # stop("Please pick a different model, rrBLUP, gBLUP and ssBLUP require marker information. Alternatively, go back to the 'Retrieve Data' section and upload your marker data.")
            }
          }else{ markerVersionToUse <- input$versionMarker2Mta} # there is a versionMarker2Mta id
        }else{ markerVersionToUse <- NULL } # for non marker based model we don't need to provide this

        result <- try(

          cgiarPipeline::mtaLmmFlex(
            phenoDTfile= dtMta, # analysis to be picked from predictions database
            analysisId=input$version2Mta,
            analysisIdForGenoModifications=markerVersionToUse,
            inputFormulation=inputFormulation,
            envsToInclude=NULL, # x$df
            trait= input$trait2Mta, traitFamily=NULL, # myFamily
            useWeights=input$useWeights,
            heritLB= as.numeric(unlist(strsplit(input$heritLBMet,","))),
            heritUB= as.numeric(unlist(strsplit(input$heritUBMet,","))),
            meanLB = as.numeric(unlist(strsplit(input$meanLBMet,","))),
            meanUB = as.numeric(unlist(strsplit(input$meanUBMet,","))),
            modelType=input$modelMet, # either "blup", "pblup", "gblup", "rrblup"
            nMarkersRRBLUP=input$nMarkersRRBLUP,
            deregress=input$deregressMet,
            maxIters=input$maxitMet, batchSizeToPredict=500, tolParInv=1e-4,
            verbose=TRUE
          ),
          silent=TRUE
        )
        if(!inherits(result,"try-error")) {
          data(result) # update data with results
          cat(paste("Multi-trial analysis step with id:",as.POSIXct(result$status$analysisId[length(result$status$analysisId)], origin="1970-01-01", tz="GMT"),"saved. Please proceed to construct a selection index using this time stamp."))
          updateTabsetPanel(session, "tabsMain", selected = "outputTabs")
        }else{
          cat(paste("Analysis failed with the following error message: \n\n",result[[1]]))
        }
      }

      shinybusy::remove_modal_spinner()

    }) ## end eventReactive

    output$outMta <- renderPrint({
      outMta()
    })


  })
}

## To be copied in the UI
# mod_mtaExpApp_ui("mtaExpApp_1")

## To be copied in the server
# mod_mtaExpApp_server("mtaExpApp_1")

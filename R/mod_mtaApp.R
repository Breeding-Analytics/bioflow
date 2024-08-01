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

    tags$br(),

    mainPanel(width = 12,
              tabsetPanel( id=ns("tabsMain"),
                           type = "tabs",
                           tabPanel(div(icon("book"), "Information-MTA-FW") ,
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
                                                      h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameters to be specified in the grey boxes above.", style="color:green"))),
                                                      hr(style = "border-top: 3px solid #4c4c4c;"),
                                               ),
                                               column( width=4, DT::DTOutput(ns("tableTraitTimeStamps")), br(),br(),  ),
                                               column( width=8, shiny::plotOutput(ns("plotTimeStamps")), br(),br(), ),
                                               column( width=12, DT::DTOutput(ns("statusMta")), br(),br(),  ),# modeling table
                                               column( width=12, DT::DTOutput(ns("phenoMta")), br(),br(),  ),# predictions data table
                                      ),
                                      tabPanel("Select traits", icon = icon("dice-two"),
                                               br(),
                                               column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                      column(width=4, selectInput(ns("trait2Mta"), "Trait(s) to analyze (required)", choices = NULL, multiple = TRUE) ),
                                                      column(width = 8, style = "background-color:grey; color: #FFFFFF",
                                                             br(),
                                                             shinydashboard::box(width = 12, status = "success", solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Alternative response distributions to fit...",
                                                                                 p(span("Trait distributions (double click in the cells if you would like to model the traits with a different trait distribution).", style="color:black")),
                                                                                 DT::DTOutput(ns("traitDistMet")),
                                                             ),
                                                      ),
                                               ),
                                               column(width=12,
                                                      hr(style = "border-top: 3px solid #4c4c4c;"),
                                                      h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameters to be specified in the grey boxes above.", style="color:green"))),
                                                      hr(style = "border-top: 3px solid #4c4c4c;"),
                                               ),
                                               tags$span(id = ns('holder1'),
                                                         column(width=5, selectInput(ns("traitMetrics"), "Trait to visualize", choices = NULL, multiple = TRUE) ),
                                                         column(width=5, selectInput(ns("parameterMetrics"), "Parameter to visualize", choices = NULL, multiple = FALSE) ),
                                                         column(width=2, checkboxInput(ns("checkbox1"), label = "Include x-axis labels", value = TRUE) ),
                                                         column(width=12, plotly::plotlyOutput(ns("barplotPredictionsMetrics")) ),
                                               ),
                                               tags$span(id = ns('holder2'),
                                                         column(width=9, selectInput(ns("trait3Mta"), "Trait to visualize", choices = NULL, multiple = FALSE) ),
                                                         column(width=3, numericInput(ns("transparency"),"Plot transparency",value=0.5, min=0, max=1, step=0.1) ),
                                                         # column(width=5, selectInput(ns("groupMtaInputPlot"), "Group by", choices = c("environment","designation","entryType"), multiple = FALSE, selected = "environment") ),
                                                         # column(width=2, checkboxInput(ns("checkbox2"), label = "Include x-axis labels", value = TRUE) ),
                                                         column(width=12, plotly::plotlyOutput(ns("plotPredictionsCleanOut"))  ), #   shiny::plotOutput(ns("plotPredictionsCleanOut"))
                                               ),
                                      ),

                                      tabPanel("Pick the model", icon = icon("dice-three"),
                                               br(),
                                               column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                      column(width=3, selectInput(ns("fixedTermMta2"), "Fixed effect(s)", choices = NULL, multiple = TRUE) ),
                                                      column(width=3, selectInput(ns("randomTermMta2"), "Random effect(s)", choices = NULL, multiple = TRUE) ),
                                                      column(width=3, selectInput(ns("modelMet"), "Genetic evaluation model", choices = NULL, multiple = FALSE) ),
                                                      column(width=3, tags$span(id = ns('ismarkermodel'), selectInput(ns("versionMarker2Mta"), "Marker QA version to use", choices = NULL, multiple = FALSE), ),  ),
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
                                                      h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameters to be specified in the grey boxes above.", style="color:green"))),
                                                      hr(style = "border-top: 3px solid #4c4c4c;"),
                                               ),
                                               tags$span(id = ns('holder3'),
                                                         selectInput(ns("evaluationUnitsTrait"), "Trait to visualize", choices = NULL, multiple = FALSE),
                                                         shiny::plotOutput(ns("evaluationUnits")) ,
                                               ),
                                      ),
                                      tabPanel("Specify GxE", icon = icon("dice-four"),
                                               br(),
                                               column(width=12,style = "background-color:grey; color: #FFFFFF",
                                                      column(width=4,
                                                             br(),
                                                             selectInput(ns("interactionTermMta2"), "GxE term(s) (optional)", choices = NULL, multiple = TRUE),
                                                      ),
                                                      column(width = 8,
                                                             numericInput(ns("minimumNumberEnvsFW"), label = "Minimum number of levels in GxE factor to fit a random regression", value = 6),
                                                      ),
                                                      column(width=12,
                                                             shinydashboard::box(width = 12, status = "success", solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Fields to exclude (OPTIONAL)...",
                                                                                 p(span("Fields to exclude in the analysis (double click in the cell and set to zero if you would like to ignore an environment for a given trait).", style="color:black")),
                                                                                 DT::dataTableOutput(ns("fieldsMet")),
                                                             ),
                                                      ),
                                               ),
                                               column(width=12,
                                                      hr(style = "border-top: 3px solid #4c4c4c;"),
                                                      h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameters to be specified in the grey boxes above.", style="color:green"))),
                                                      hr(style = "border-top: 3px solid #4c4c4c;"),
                                               ),
                                               tags$span(id = ns('holder4'),
                                                         column(width=12, p(span("Connectivity between environments.", style="color:black")) ),
                                                         column(width = 3,
                                                                selectInput(ns("traitConnect"), "Trait to visualize", choices = NULL, multiple = FALSE),
                                                                selectInput(ns("entryTypeMta"), "Entry type to visualize", choices = NULL, multiple = TRUE),
                                                                checkboxGroupInput(ns("checkboxText"), label = "", choices = list("Add connectivity labels?" = TRUE), selected = FALSE),
                                                                checkboxGroupInput(ns("checkboxAxis"), label = "", choices = list("Add axis labels?" = TRUE), selected = FALSE),
                                                                numericInput(ns("heatmapFontSize"), label = "Font size", value = 6),
                                                         ),
                                                         column(width = 9, shiny::plotOutput(ns("plotPredictionsConnectivity")) ),
                                               ),
                                               tags$span(id = ns('holder5'),
                                                         column(width=12, p(span("Genotypic correlation between environments based on sta.", style="color:black")) ),
                                                         column(width=3,
                                                                selectInput(ns("traitCor"), "Trait to visualize", choices = NULL, multiple = FALSE),
                                                                checkboxGroupInput(ns("checkboxTextCor"), label = "", choices = list("Add correlation labels?" = TRUE), selected = FALSE),
                                                                checkboxGroupInput(ns("checkboxAxisCor"), label = "", choices = list("Add axis labels?" = TRUE), selected = FALSE),
                                                                numericInput(ns("heatmapFontSizeCor"), label = "Font size", value = 6),
                                                         ),
                                                         column(width=9, shiny::plotOutput(ns("plotPredictionsCor")) ),
                                               ),
                                               tags$span(id = ns('holder6'),
                                                         column(width=12, p(span("Sparsity between environments.", style="color:black")) ),
                                                         column(width=6, sliderInput(ns("slider1"), label = "Number of genotypes", min = 1, max = 2000, value = c(1, 100)) ),
                                                         column(width=6, sliderInput(ns("slider2"), label = "Number of environments", min = 1, max = 500, value = c(1, 25)) ),
                                                         column(width=12, shiny::plotOutput(ns("plotPredictionsSparsity")) ),
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

#' mtaApp Server Functions
#'
#' @noRd
mod_mtaApp_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$plotDataDependencies <- shiny::renderPlot({ dependencyPlot() })
    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })
    ############################################################################
    # show shinyWidgets until the user can use the module
    observeEvent(c(data(), input$version2Mta), {
      req(data())
      mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
      if(mappedColumns == 3 & length(input$version2Mta)>0 ){
        golem::invoke_js('showid', ns('holder1'))
        golem::invoke_js('showid', ns('holder2'))
        golem::invoke_js('showid', ns('holder3'))
        golem::invoke_js('showid', ns('holder4'))
        golem::invoke_js('showid', ns('holder5'))
        golem::invoke_js('showid', ns('holder6'))
      }else{
        golem::invoke_js('hideid', ns('holder1'))
        golem::invoke_js('hideid', ns('holder2'))
        golem::invoke_js('hideid', ns('holder3'))
        golem::invoke_js('hideid', ns('holder4'))
        golem::invoke_js('hideid', ns('holder5'))
        golem::invoke_js('hideid', ns('holder6'))
      }
    })
    ## data example loading
    observeEvent(
      input$launch,
      if(length(input$launch) > 0){
        if (input$launch) {
          shinyWidgets::ask_confirmation(
            inputId = ns("myconfirmation"),
            text = "Are you sure you want to load the example data? This will delete any data currently in the environment.",
            title = "Data replacement warning"
          )
        }
      }
    )
    observeEvent(input$myconfirmation, {
      if (isTRUE(input$myconfirmation)) {
        shinybusy::show_modal_spinner('fading-circle', text = 'Loading example...')
        ## replace tables
        data(cgiarBase::create_getData_object())
        tmp <- data()
        utils::data(DT_example, package = "cgiarPipeline")
        if(!is.null(result$data)){tmp$data <- result$data}
        if(!is.null(result$metadata)){tmp$metadata <- result$metadata}
        if(!is.null(result$modifications)){tmp$modifications <- result$modifications}
        if(!is.null(result$predictions)){tmp$predictions <- result$predictions}
        if(!is.null(result$metrics)){tmp$metrics <- result$metrics}
        if(!is.null(result$modeling)){tmp$modeling <- result$modeling}
        if(!is.null(result$status)){tmp$status <- result$status}
        data(tmp) # update data with results
        shinybusy::remove_modal_spinner()
      }else{
        shinyWidgets::updatePrettySwitch(session, "launch", value = FALSE)
      }
    }, ignoreNULL = TRUE)
    ############################################################################
    observeEvent(
      input$modelMet,
      if (input$modelMet %in% c('gblup','rrblup','ssgblup') ) {
        golem::invoke_js('showid', ns('ismarkermodel'))
      } else {
        golem::invoke_js('hideid', ns('ismarkermodel'))
      }
    )
    # warning message
    output$warningMessage <- renderUI(
      if(is.null(data())){
        HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your phenotypic data using the 'Data Retrieval' tab.")) )
      }else{ # data is there
        mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
        if(mappedColumns == 3){
          if("sta" %in% data()$status$module){
            HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to perform the multi-trial analysis specifying your input parameters under the Input tabs.")) )
          }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please perform single-trial analysis before performing a the multi-trial analysis.")) ) }
        }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please make sure that you have computed the 'environment' column, and that column 'designation' and \n at least one trait have been mapped using the 'Data Retrieval' tab.")) )}
      }
    )
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
    # ## version qa marker
    observeEvent(c(data()), {
      req(data())
      dtMta <- data()
      dtMta <- dtMta$status
      dtMta <- dtMta[which(dtMta$module == "qaGeno"),]
      traitsMta <- unique(dtMta$analysisId)
      if(length(traitsMta) > 0){names(traitsMta) <- as.POSIXct(traitsMta, origin="1970-01-01", tz="GMT")}
      updateSelectInput(session, "versionMarker2Mta", choices = traitsMta)
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
      updateSelectInput(session, "traitCor", choices = traitsMta)
      updateSelectInput(session, "traitConnect", choices = traitsMta)
      updateSelectInput(session, "evaluationUnitsTrait", choices = traitsMta)
    })
    #################
    ## fixed effects
    observeEvent(c(data(),input$version2Mta, input$trait2Mta), {
      req(data())
      req(input$version2Mta)
      req(input$trait2Mta)
      dtMta <- data()
      metaPheno <- dtMta$metadata$pheno[which(dtMta$metadata$pheno$parameter %in% c("environment","year","season","country","location","trial","study","management")),]
      metaPheno <- metaPheno[which(!duplicated(metaPheno$value)),] # avoid error when user maps columns twice
      otherMetaCols <- unique(dtMta$data$pheno[,metaPheno$value,drop=FALSE])
      colnames(otherMetaCols) <- cgiarBase::replaceValues(Source = colnames(otherMetaCols), Search = metaPheno$value, Replace = metaPheno$parameter )
      otherMetaCols <- otherMetaCols[which(!duplicated(otherMetaCols[,"environment"])),,drop=FALSE] # we do this in case the users didn't define the environment properly
      dtMta <- dtMta$predictions
      dtMta <- dtMta[which(dtMta$analysisId %in% input$version2Mta),]
      dtMta <- merge(dtMta, otherMetaCols, by="environment", all.x = TRUE)
      traitsMta <- apply(dtMta[,c(metaPheno$parameter,"designation")],2,function(x){length(unique(x))})
      traitsMta <- names(traitsMta)[which(traitsMta > 1)] # remove factors that do not have more than one level
      start <- setdiff(traitsMta,c("designation","entryType","pipeline"))
      updateSelectInput(session, "fixedTermMta2", choices = traitsMta, selected = ifelse('environment'%in%start, 'environment',  start) )
    })
    #################
    ## random effects
    observeEvent(c(data(), input$version2Mta, input$trait2Mta, input$fixedTermMta2 ), {
      req(data())
      req(input$version2Mta)
      req(input$trait2Mta)
      dtMta <- data()
      metaPheno <- dtMta$metadata$pheno[which(dtMta$metadata$pheno$parameter %in% c("environment","year","season","country","location","trial","study","management")),]
      metaPheno <- metaPheno[which(!duplicated(metaPheno$value)),] # avoid error when user maps columns twice
      otherMetaCols <- unique(dtMta$data$pheno[,metaPheno$value,drop=FALSE])
      colnames(otherMetaCols) <- cgiarBase::replaceValues(Source = colnames(otherMetaCols), Search = metaPheno$value, Replace = metaPheno$parameter )
      otherMetaCols <- otherMetaCols[which(!duplicated(otherMetaCols[,"environment"])),,drop=FALSE] # we do this in case the users didn't define the environment properly
      dtMta <- dtMta$predictions
      dtMta <- dtMta[which(dtMta$analysisId %in% input$version2Mta),]
      dtMta <- merge(dtMta, otherMetaCols, by="environment", all.x = TRUE)
      traitsMta <- apply(dtMta[,c(metaPheno$parameter,"designation")],2,function(x){length(unique(x))})
      traitsMta <- names(traitsMta)[which(traitsMta > 1)]
      traitsMta <- setdiff(traitsMta, input$fixedTermMta2)
      updateSelectInput(session, "randomTermMta2", choices = traitsMta, selected = "designation")
    })
    #################
    ## model types
    observeEvent(c(data(), input$version2Mta, input$trait2Mta, input$fixedTermMta2), {
      req(data())
      req(input$version2Mta)
      req(input$trait2Mta)
      if('designation'%in%input$fixedTermMta2){
        traitsMta <- list(BLUE="blue")
      }else{
        traitsMta <- list(BLUP="blup")#,pBLUP="pblup",gBLUP="gblup",ssGBLUP="ssgblup",rrBLUP="rrblup")
        if(!is.null(object$data$geno)){
          traitsMta <- c(traitsMta, list(gBLUP="gblup"),  list(rrBLUP="rrblup") )
        }
        if(!is.null(object$data$pedigree)){
          metaPed <- object$metadata$pedigree
          dataPed <- object$data$pedigree
          pedCols <- metaPed[which(metaPed$parameter %in% c("designation","mother","father")), "value"]
          pedCols <- setdiff(pedCols,"")
          fatherCol <- which(metaPed$parameter == "father")
          if(length(fatherCol) > 0){dataPed <- dataPed[which(!is.na(dataPed[,fatherCol])),]}
          motherCol <- which(metaPed$parameter == "mother")
          if(length(motherCol) > 0){dataPed <- dataPed[which(!is.na(dataPed[,motherCol])),]}
          if(nrow(dataPed) > 0){
            traitsMta <- c(traitsMta, list(pBLUP="pblup") )
          }
        }
        if( all(c("pBLUP","gBLUP") %in% names(traitsMta)) ){ traitsMta <- c(traitsMta, list( ssGBLUP="ssgblup" ) ) }
      }
      updateSelectInput(session, "modelMet", choices = traitsMta, selected = traitsMta[1])
    })
    #################
    ## gXe interactions
    observeEvent(c(data(), input$trait2Mta), {
      req(data())
      req(input$trait2Mta)
      dtMta <- data() # dtMta <- result
      weather <- cgiarPipeline::summaryWeather(dtMta)
      weather$traitParameter <- paste(weather$trait, weather$parameter, sep="_")
      # dtMta <- dtMta$metadata$weather
      # invalidWeatherVars <- c("environment")# c("RH2M","T2M","PRECTOTCORR", "latitude", "longitude", "plantingDate", "harvestingDate" )
      # traitsMta <- unique(c("envIndex",setdiff(unique(dtMta$value), invalidWeatherVars )))
      traitsMta <- unique(weather$traitParameter)
      updateSelectInput(session, "interactionTermMta2", choices = traitsMta)
    })
    #################
    # reactive table for environments to be included
    dtFieldMet = reactive({
      req(data())
      req(input$version2Mta)
      dtMta <- data()
      dtProv = dtMta$predictions
      if(!is.null(dtProv)){
        dtProv <- dtProv[which(dtProv$analysisId %in% input$version2Mta),]
        dtProvTable=  as.data.frame( do.call( rbind, list (with(dtProv, table(environment,trait)) ) ) )
        bad <- which(dtProvTable <= 1, arr.ind = TRUE)
        if(nrow(bad) > 0){dtProvTable[bad] = 0}
        dtProvTable[which(dtProvTable > 1, arr.ind = TRUE)] = 1
      }else{dtProvTable <- data.frame()}
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
    ## render timestamps flow
    output$plotTimeStamps <- shiny::renderPlot({
      req(data()) # req(input$version2Sta)
      xx <- data()$status;  yy <- data()$modeling
      v <- which(yy$parameter == "analysisId")
      if(length(v) > 0){
        yy <- yy[v,c("analysisId","value")]
        zz <- merge(xx,yy, by="analysisId", all.x = TRUE)
      }else{ zz <- xx; zz$value <- NA}
      if(!is.null(xx)){
        colnames(zz) <- cgiarBase::replaceValues(colnames(zz), Search = c("analysisId","value"), Replace = c("outputId","inputId") )
        nLevelsCheck1 <- length(na.omit(unique(zz$outputId)))
        nLevelsCheck2 <- length(na.omit(unique(zz$inputId)))
        if(nLevelsCheck1 > 1 & nLevelsCheck2 > 1){
          X <- with(zz, sommer::overlay(outputId, inputId))
        }else{
          if(nLevelsCheck1 <= 1){
            X1 <- matrix(ifelse(is.na(zz$inputId),0,1),nrow=length(zz$inputId),1); colnames(X1) <- as.character(na.omit(unique(c(zz$outputId))))
          }else{X1 <- model.matrix(~as.factor(outputId)-1, data=zz); colnames(X1) <- levels(as.factor(zz$outputId))}
          if(nLevelsCheck2 <= 1){
            X2 <- matrix(ifelse(is.na(zz$inputId),0,1),nrow=length(zz$inputId),1); colnames(X2) <- as.character(na.omit(unique(c(zz$inputId))))
          }else{X2 <- model.matrix(~as.factor(inputId)-1, data=zz); colnames(X2) <- levels(as.factor(zz$inputId))}
          mynames <- unique(na.omit(c(zz$outputId,zz$inputId)))
          X <- matrix(0, nrow=nrow(zz), ncol=length(mynames)); colnames(X) <- as.character(mynames)
          if(!is.null(X1)){X[,colnames(X1)] <- X1}
          if(!is.null(X2)){X[,colnames(X2)] <- X2}
        };  rownames(X) <- as.character(zz$outputId)
        rownames(X) <-as.character(as.POSIXct(as.numeric(rownames(X)), origin="1970-01-01", tz="GMT"))
        colnames(X) <-as.character(as.POSIXct(as.numeric(colnames(X)), origin="1970-01-01", tz="GMT"))
        # make the network plot
        n <- network::network(X, directed = FALSE)
        network::set.vertex.attribute(n,"family",zz$module)
        network::set.vertex.attribute(n,"importance",1)
        e <- network::network.edgecount(n)
        network::set.edge.attribute(n, "type", sample(letters[26], e, replace = TRUE))
        network::set.edge.attribute(n, "day", sample(1, e, replace = TRUE))
        library(ggnetwork)
        ggplot2::ggplot(n, ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
          ggnetwork::geom_edges(ggplot2::aes(color = family), arrow = ggplot2::arrow(length = ggnetwork::unit(6, "pt"), type = "closed") ) +
          ggnetwork::geom_nodes(ggplot2::aes(color = family), alpha = 0.5, size=5 ) + ggplot2::ggtitle("Network plot of current analyses available") +
          ggnetwork::geom_nodelabel_repel(ggplot2::aes(color = family, label = vertex.names ),
                                          fontface = "bold", box.padding = ggnetwork::unit(1, "lines")) +
          ggnetwork::theme_blank()
      }
    })
    ## render the table of traits and analysisID being selected
    output$tableTraitTimeStamps <-  DT::renderDT({
      req(data())
      # req(input$version2Mta)
      ### change column names for mapping
      if(length(input$version2Mta) > 0){
        status <- data()$status
        modeling <- data()$modeling
        statusPlusModel <- merge(status, unique(modeling[,c("analysisId","trait")]), by="analysisId", all.x = TRUE)
        '%!in%' <- function(x,y)!('%in%'(x,y))
        statusPlusModel <- statusPlusModel[which(statusPlusModel$trait %!in% c("inputObject",NA)),]
        statusPlusModel <- statusPlusModel[which(statusPlusModel$analysisId %in% input$version2Mta),]
        statusPlusModel$analysisId <- as.POSIXct(statusPlusModel$analysisId, origin="1970-01-01", tz="GMT")
      }else{
        statusPlusModel <- data.frame(analysisId=NA, module=NA, trait=NA)
      }
      DT::datatable(statusPlusModel, extensions = 'Buttons', # I changed Blfrtip to lfrtip and silenced the buttons
                    options = list(dom = 'lfrtip',scrollX = TRUE, #buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   lengthMenu = list(c(8,20,50,-1), c(8,20,50,'All'))),
                    caption = htmltools::tags$caption(
                      style = 'color:cadetblue', #caption-side: bottom; text-align: center;
                      htmltools::em('Traits available in the STA-IDs selected.')
                    )
      )
    })
    ## render the input data to be analyzed
    output$statusMta <-  DT::renderDT({
      req(data())
      req(input$version2Mta)
      ### change column names for mapping
      paramsPheno <- data()$modeling
      paramsPheno <- paramsPheno[which(paramsPheno$analysisId %in% input$version2Mta),, drop=FALSE]
      paramsPheno$analysisId <- as.POSIXct(paramsPheno$analysisId, origin="1970-01-01", tz="GMT")
      DT::datatable(paramsPheno, extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All'))),
                    caption = htmltools::tags$caption(
                      style = 'color:cadetblue', #caption-side: bottom; text-align: center;
                      htmltools::em('Past modeling parameters from STA stamp(s) selected.')
                    )
      )
    })

    output$evaluationUnits <-  shiny::renderPlot({ #DT::renderDT({
      req(data())
      req(input$version2Mta)
      req(input$evaluationUnitsTrait)
      object <- data()
      if(!is.null(object$predictions)){
        phenoNames <- na.omit(unique(object$predictions[which(object$predictions$analysisId %in% input$version2Mta  &  object$predictions$trait == input$evaluationUnitsTrait),"designation"]))
      }else{ phenoNames <- character() }

      if(!is.null(object$data$geno)){
        genoNames <- rownames(object$data$geno)
      }else{ genoNames <- character() }

      if(!is.null(object$data$pedigree)){
        metaPed <- object$metadata$pedigree
        dataPed <- object$data$pedigree
        pedCols <- metaPed[which(metaPed$parameter %in% c("designation","mother","father")), "value"]
        pedCols <- setdiff(pedCols,"")
        fatherCol <- which(metaPed$parameter == "father")
        if(length(fatherCol) > 0){dataPed <- dataPed[which(!is.na(dataPed[,fatherCol])),]}
        motherCol <- which(metaPed$parameter == "mother")
        if(length(motherCol) > 0){dataPed <- dataPed[which(!is.na(dataPed[,motherCol])),]}
        pedNames <- na.omit(unique(unlist(as.vector(dataPed[,pedCols, drop=FALSE]))))
      }else{ pedNames <- character() }

      if(!is.null(object$data$qtl)){
        metaQtl <- object$metadata$qtl
        qtlCols <- metaQtl[which(metaQtl$parameter %in% c("designation")), "value"]
        qtlCols <- setdiff(qtlCols,"")
        qtlNames <- na.omit(unique(object$data$qtl[,qtlCols]))
      }else{ qtlNames <- character() }

      splitAggregate <- list(phenoNames, genoNames, pedNames, qtlNames)
      names(splitAggregate) <- c("With-Phenotype","With-Genotype","With-Pedigree","With-QTL")

      nagm <- matrix(0,length(splitAggregate),length(splitAggregate)); rownames(nagm) <- colnames(nagm) <- names(splitAggregate) # prefilled matrix
      for(i in 1:length(splitAggregate)){ # fill the matrix of intersection of individuals between pair of environments
        for(j in 1:i){
          nagm[i,j] <- length(intersect(splitAggregate[[i]],splitAggregate[[j]]))
        }
      }
      nagm[upper.tri(nagm)] <- t(nagm)[upper.tri(nagm)] # fill the upper triangular
      mydata4 <- cgiarBase::matToTab(nagm) # matrix to a dataframe for plot
      maxVal <- max(nagm, na.rm = TRUE) # get the maximum value found in the matrix of connectivity
      midval <- (max(nagm, na.rm = TRUE) - min(nagm, na.rm = TRUE) )/2
      p <- ggplot2::ggplot(data = mydata4, ggplot2::aes(Var2, Var1, fill = Freq))+
        ggplot2::geom_tile(color = "white")+ ggplot2::ggtitle("Available data for this trait.") +
        ggplot2::scale_fill_gradient2(low = "firebrick", high = "#038542", mid = "gold",
                                      midpoint = midval, limit = c(0,maxVal), space = "Lab",
                                      name="Connectivity (data types)") +
        ggplot2::theme_minimal()+
        ggplot2::geom_text(ggplot2::aes(label = Freq), color = "white", size = 3 ) +
        ggplot2::ylab("") + ggplot2::xlab("") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,  hjust = 1, face = "bold"))+
        ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 0, vjust = 1,  hjust = 1, face = "bold")) +
        ggplot2::coord_fixed()
      p
    })

    output$phenoMta <-  DT::renderDT({
      req(data())
      req(input$version2Mta)
      dtMta <- data()
      dtMta <- dtMta$predictions
      dtMta <- dtMta[which(dtMta$analysisId %in% input$version2Mta),setdiff(colnames(dtMta),c("module","analysisId"))]
      numeric.output <- c("predictedValue", "stdError", "reliability")
      DT::formatRound(DT::datatable(dtMta, extensions = 'Buttons',
                                    options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                   lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All'))),
                                    caption = htmltools::tags$caption(
                                      style = 'color:cadetblue', #caption-side: bottom; text-align: center;
                                      htmltools::em('STA predictions table to be used as input.')
                                    )
      ), numeric.output)
    })
    ## render connectivity plot
    observeEvent(c(data(),input$version2Mta), { # update entry types included in the plot
      req(data())
      req(input$version2Mta)
      dtMta <- data()
      dtMta <- dtMta$predictions
      dtMta <- dtMta[which(dtMta$analysisId %in% input$version2Mta),] # only traits that have been QA
      entryTypeMtaInput <- unique(dtMta$entryType)
      updateSelectInput(session, "entryTypeMta", choices = c(entryTypeMtaInput,"Generic"), selected = "Generic")
    })
    output$plotPredictionsSparsity <- shiny::renderPlot({
      req(data())
      req(input$version2Mta)
      req(input$entryTypeMta)
      dtMta <- data()
      mydata <- dtMta$predictions
      mydata <- mydata[which(mydata$analysisId %in% input$version2Mta),] # only PREDICTIONS FROM THE STA
      if(input$entryTypeMta != "Generic"){
        mydata <- mydata[which(mydata[,"entryType"] %in% input$entryTypeMta),]
      }
      M <- table(mydata$designation, mydata$environment)
      M2 <- matrix(M, nrow = nrow(M), ncol = ncol(M))
      M2 <- M2[,(input$slider2[1]):min(c(input$slider2[2], ncol(M2) )), drop=FALSE] # environments
      M2 <- M2[(input$slider1[1]):min(c(input$slider1[2]), nrow(M2) ), ,drop=FALSE] # genotypes
      Matrix::image(as(t(M2), Class = "dgCMatrix"), xlab="Genotypes", ylab="Environments", colorkey=TRUE)
    })
    # render correlation plot
    output$plotPredictionsCor <-  shiny::renderPlot({
      req(data())
      req(input$version2Mta)
      req(input$traitCor)
      req(input$heatmapFontSizeCor)
      dtMta <- data()
      mydata <- dtMta$predictions
      mydata <- mydata[which(mydata$analysisId %in% input$version2Mta),] # only PREDICTIONS FROM THE STA
      predictions.gcorrE <- subset(mydata, select = c(trait,designation,environment,predictedValue))
      predictions.gcorrE2 <- predictions.gcorrE[predictions.gcorrE$trait == input$traitCor, ]
      wide <- stats::reshape(predictions.gcorrE2,
                             direction = "wide", idvar = "designation",
                             timevar = "environment", v.names = "predictedValue", sep= "")
      colnames(wide) <- gsub("predictedValue","",colnames(wide))
      wide2 <- as.data.frame(wide[,-c(1:2)]); colnames(wide2) <- colnames(wide)[-c(1:2)]
      corr <- round(stats::cor(wide2, use="pairwise.complete.obs"),2)
      mydata4 <- cgiarBase::matToTab(corr)
      p <- ggplot2::ggplot(data = mydata4, ggplot2::aes(Var2, Var1, fill = Freq))+
        ggplot2::geom_tile(color = "white")+
        ggplot2::scale_fill_gradient2(low = "#E46726", high = "#038542", mid = "white",
                                      midpoint = 0, limit = c(-1,1), space = "Lab",
                                      name="Pearson\nCorrelation") +
        ggplot2::theme_minimal()+
        ggplot2::ylab("") + ggplot2::xlab("") +
        ggplot2::coord_fixed()
      if(!is.null(input$checkboxTextCor)){ # if user wants to fill cell values
        p <- p + ggplot2::geom_text(ggplot2::aes(label = round(Freq,2) ), color = "black", size = max(c(input$heatmapFontSizeCor-3, 1)))
      }
      if(!is.null(input$checkboxAxisCor)){ # if user wants to add axis labels
        p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, size = input$heatmapFontSizeCor, hjust = 1, face="bold"))+
          ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 0, vjust = 1, size = input$heatmapFontSizeCor, hjust = 1, face = "bold"))
      }else{
        p <- p + ggplot2::theme( axis.text.x=ggplot2::element_blank(), axis.text.y=ggplot2::element_blank() )
      }
      p

    })
    # render connectivity plot
    output$plotPredictionsConnectivity <-  shiny::renderPlot({ # plotly::renderPlotly({
      req(data())
      req(input$version2Mta)
      req(input$traitConnect)
      req(input$entryTypeMta)
      req(input$heatmapFontSize)
      dtMta <- data()
      mydata <- dtMta$predictions # extract predictions
      mydata <- mydata[which(mydata$analysisId %in% input$version2Mta),] # only PREDICTIONS FROM THE STA
      mydata <- mydata[which(mydata$trait %in% input$traitConnect),] # only PREDICTIONS FROM Trait selected
      if(input$entryTypeMta != "Generic"){ # use an specific type of entries
        mydata <- mydata[which(mydata[,"entryType"] %in% input$entryTypeMta),]
      }
      splitAggregate <- with(mydata,  split(mydata[,"designation"],mydata[,"environment"]) ) # split by environment
      splitAggregate <- lapply(splitAggregate,unique); nag <- length(splitAggregate) # get unique individual names
      nagm <- matrix(0,nag,nag); rownames(nagm) <- colnames(nagm) <- names(splitAggregate) # prefilled matrix
      for(i in 1:length(splitAggregate)){ # fill the matrix of intersection of individuals between pair of environments
        for(j in 1:i){
          nagm[i,j] <- length(intersect(splitAggregate[[i]],splitAggregate[[j]]))
        }
      }
      nagm[upper.tri(nagm)] <- t(nagm)[upper.tri(nagm)] # fill the upper triangular
      mydata4 <- cgiarBase::matToTab(nagm) # matrix to a dataframe for plot
      maxVal <- max(nagm, na.rm = TRUE) # get the maximum value found in the matrix of connectivity
      p <- ggplot2::ggplot(data = mydata4, ggplot2::aes(Var2, Var1, fill = Freq))+
        ggplot2::geom_tile(color = "white")+
        ggplot2::scale_fill_gradient2(low = "firebrick", high = "#038542", mid = "gold",
                                      midpoint = 60, limit = c(0,maxVal), space = "Lab",
                                      name="Connectivity") +
        ggplot2::theme_minimal()+
        ggplot2::ylab("") + ggplot2::xlab("") +
        ggplot2::coord_fixed()
      if(!is.null(input$checkboxText)){ # if user wants to add text to the cells
        p <- p + ggplot2::geom_text(ggplot2::aes(label = Freq), color = "white", size = max(c(input$heatmapFontSize-3,1)))
      }
      if(!is.null(input$checkboxAxis)){ # if user wants to add labels to the axis
        p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, size = input$heatmapFontSize, hjust = 1, face = "bold"))+
          ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 0, vjust = 1, size = input$heatmapFontSize, hjust = 1, face = "bold"))
      }else{
        p <- p + ggplot2::theme( axis.text.x=ggplot2::element_blank(), axis.text.y=ggplot2::element_blank() )
      }
      p
    })
    ## render metrics barplot
    observeEvent(c(data(),input$version2Mta), { # update trait
      req(data())
      req(input$version2Mta)
      dtMta <- data()
      dtMta <- dtMta$metrics
      dtMta <- dtMta[which(dtMta$analysisId %in% input$version2Mta),] # only traits that have been QA
      traitMtaInput <- unique(dtMta$trait)
      updateSelectInput(session, "traitMetrics", choices = traitMtaInput, selected = traitMtaInput)
    })
    observeEvent(c(data(),input$version2Mta), { # update parameter
      req(data())
      req(input$version2Mta)
      dtMta <- data()
      dtMta <- dtMta$metrics
      dtMta <- dtMta[which(dtMta$analysisId %in% input$version2Mta),] # only traits that have been QA
      metricsMtaInput <- unique(dtMta$parameter)
      updateSelectInput(session, "parameterMetrics", choices = metricsMtaInput)
    })
    output$barplotPredictionsMetrics <- plotly::renderPlotly({
      req(data())
      req(input$version2Mta)
      dtMta <- data()
      mydata <- dtMta$metrics
      mydata <- mydata[which(mydata$analysisId %in% input$version2Mta),]
      mydata = mydata[which(mydata$parameter %in% input$parameterMetrics),]
      mydata = mydata[which(mydata$trait %in% input$traitMetrics),]
      p <- ggplot2::ggplot(data=mydata, ggplot2::aes(x=environment, y=value, fill=trait)) +
        ggplot2::geom_bar(stat="identity", position=position_dodge())+
        ggplot2::theme_minimal()+  ggplot2::ggtitle("Metrics associated to the STA stamp selected")
      if(input$checkbox1){
        p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1))
      }else{
        p <- p + ggplot2::theme(axis.text.x = ggplot2::element_blank())
      }
      plotly::ggplotly(p)
    })
    ## render trait distribution plot
    observeEvent(c(data(),input$version2Mta), { # update trait
      req(data())
      req(input$version2Mta)
      dtMta <- data()
      dtMta <- dtMta$predictions
      dtMta <- dtMta[which(dtMta$analysisId %in% input$version2Mta),] # only traits that have been QA
      traitMtaInput <- unique(dtMta$trait)
      updateSelectInput(session, "trait3Mta", choices = traitMtaInput)
    })
    output$plotPredictionsCleanOut <- plotly::renderPlotly({ # shiny::renderPlot({ #
      req(data())
      req(input$version2Mta)
      req(input$trait3Mta)
      # req(input$groupMtaInputPlot)
      mydata <- data()$predictions
      mydata <- mydata[which(mydata$analysisId %in% input$version2Mta),] # only traits that have been QA
      mydata <- mydata[which(mydata[,"trait"] %in% input$trait3Mta),]
      mydata[, "environment"] <- as.factor(mydata[, "environment"]); mydata[, "designation"] <- as.factor(mydata[, "designation"])
      p <- ggplot2::ggplot(mydata, ggplot2::aes(x=predictedValue, fill=environment)) +
        ggplot2::geom_histogram(aes(y=..density..), position="identity", alpha=input$transparency ) +
        # ggplot2::geom_boxplot(fill='#A4A4A4', color="black", notch = TRUE)+
        ggplot2::theme_classic() + ggplot2::ggtitle("Dispersal of predictions associated to the STA stamp selected") +
        # ggplot2::geom_jitter(alpha = 0.4, colour="cadetblue") + # ggplot2::aes(colour = color),
        ggplot2::xlab("Trait Predicted Value") + ggplot2::ylab("Density") + ggplot2::scale_color_brewer(palette="Accent")
      # if(input$checkbox2){
      #   p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1))
      # }else{
      #   p <- p + ggplot2::theme(axis.text.x = ggplot2::element_blank())
      # }
      # p
      plotly::ggplotly(p)
    })

    ##############################################################################################
    ##############################################################################################
    ##############################################################################################
    ## mta function
    my_mta <- ExtendedTask$new(function(input, data, x, xx) { # x is envs, xx is families
      promises::future_promise({
        # family distributions input
        myFamily = apply(xx$df,2,function(y){rownames(xx$df)[which(y > 0)[1]]})
        dontHaveDist <- which(is.na(myFamily))
        if(length(dontHaveDist) > 0){myFamily[dontHaveDist] <- "quasi(link = 'identity', variance = 'constant')"}
        # some long process
        getOption("spam.cholsymmetrycheck")
        options(spam.cholsymmetrycheck=FALSE)
        getOption("spam.cholsymmetrycheck")

        if(input$modelMet %in% c("gblup","rrblup","ssblup") ){ # warning
          if(input$versionMarker2Mta == '' | is.null(input$versionMarker2Mta) ){ # user didn't provide a modifications id
            if(!is.null(data$data$geno)){ # if user actually has marker data
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

        result <- try(cgiarPipeline::metLMM(
          phenoDTfile= data, # analysis to be picked from predictions database
          analysisId=input$version2Mta,
          analysisIdForGenoModifications = markerVersionToUse, # marker modifications
          fixedTerm= input$fixedTermMta2,  randomTerm=input$randomTermMta2,  residualBy=NULL,
          interactionsWithGeno=input$interactionTermMta2, envsToInclude=x$df,
          trait= input$trait2Mta, traitFamily=myFamily, useWeights=input$useWeights,
          heritLB= as.numeric(unlist(strsplit(input$heritLBMet,","))),
          heritUB= as.numeric(unlist(strsplit(input$heritUBMet,","))),
          meanLB = as.numeric(unlist(strsplit(input$meanLBMet,","))),
          meanUB = as.numeric(unlist(strsplit(input$meanUBMet,","))),
          modelType=input$modelMet, # either "grm", "nrm", or both
          nMarkersRRBLUP=input$nMarkersRRBLUP,
          deregress=input$deregressMet,  nPC=input$nPC,
          maxIters=input$maxitMet, batchSizeToPredict=500, tolParInv=1e-4,
          minimumNumberEnvsFW =input$minimumNumberEnvsFW,verbose=FALSE
        ),
        silent=TRUE
        )
        return(result)
      })
    })
    ## render result of "run" button click
    observeEvent(input$runMta, {
      req(data())
      req(input$version2Mta)
      req(input$trait2Mta)
      shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')

      ui_inputs <- shiny::reactiveValuesToList(input)
      x_inputs <- shiny::reactiveValuesToList(x)
      xx_inputs <- shiny::reactiveValuesToList(xx)
      data_obj  <- data()
      my_mta$invoke(ui_inputs, data_obj, x_inputs, xx_inputs)
    })
    ## render result of "run" button click
    output$outMta <- renderPrint({

      # run the modeling, but before test if sta was done
      if(sum(data()$status$module %in% "sta") == 0) {
        output$qaQcMtaInfo <- renderUI({
          if (hideAll$clearAll)
            return()
          else
            req(data())
          HTML(as.character(div(style="color: brown;",
                                "Please perform Single-Trial-Analysis before conducting a Multi-Trial Analysis when using a two-stage analysis."))
          )
        })
      }else{ # sta is available
        output$qaQcMtaInfo <- renderUI({return(NULL)})
        result <- my_mta$result()
        shinybusy::remove_modal_spinner()
        if(!inherits(result,"try-error")) {
          data(result) # update data with results
          cat(paste("Multi-trial analysis step with id:",as.POSIXct(result$status$analysisId[length(result$status$analysisId)], origin="1970-01-01", tz="GMT"),"saved. Please proceed to construct a selection index using this time stamp."))
          updateTabsetPanel(session, "tabsMain", selected = "outputTabs")
          ## predictions table
          output$predictionsMta <-  DT::renderDT({
            predictions <- result$predictions
            predictions <- predictions[predictions$module=="mta",]
            predictions$analysisId <- as.numeric(predictions$analysisId)
            predictions <- predictions[!is.na(predictions$analysisId),]
            current.predictions <- predictions[predictions$analysisId==max(predictions$analysisId),]
            current.predictions <- subset(current.predictions, select = -c(module,analysisId))
            numeric.output <- c("predictedValue", "stdError", "reliability")
            DT::formatRound(DT::datatable(current.predictions, extensions = 'Buttons',
                                          options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                         lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
            ), numeric.output)
          })
          # metrics table
          output$metricsMta <-  DT::renderDT({
            if(!inherits(result,"try-error") ){
              metrics <- result$metrics
              mtas <- result$status[which(result$status$module == "mta"),"analysisId"]; mtaId <- mtas[length(mtas)]
              metrics <- metrics[which(metrics$analysisId == mtaId),]
              metrics <- subset(metrics, select = -c(module,analysisId))
              numeric.output <- c("value", "stdError")
              DT::formatRound(DT::datatable(metrics, extensions = 'Buttons',
                                            options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                           lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
              ), numeric.output)
            }
          })
          # modeling table
          output$modelingMta <-  DT::renderDT({
            if(!inherits(result,"try-error") ){
              modeling <- result$modeling
              mtas <- result$status[which(result$status$module == "mta"),"analysisId"]; mtaId <- mtas[length(mtas)]
              modeling <- modeling[which(modeling$analysisId == mtaId),]
              modeling <- subset(modeling, select = -c(module,analysisId))
              # modeling$analysisId <- as.POSIXct(modeling$analysisId, origin="1970-01-01", tz="GMT")
              DT::datatable(modeling, extensions = 'Buttons',
                            options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                           lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
              )
            }
          })
          # ## Report tab
          output$reportMta <- renderUI({
            HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportMta.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
            # HTML(markdown::markdownToHTML(knitr::knit("./R/reportMta.Rmd", quiet = TRUE), fragment.only=TRUE))
          })

          output$downloadReportMta <- downloadHandler(
            filename = function() {
              paste('my-report', sep = '.', switch(
                "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
              ))
            },
            content = function(file) {
              src <- normalizePath(system.file("rmd","reportMta.Rmd",package="bioflow"))
              src2 <- normalizePath('data/resultMta.RData')
              # temporarily switch to the temp dir, in case you do not have write
              # permission to the current working directory
              owd <- setwd(tempdir())
              on.exit(setwd(owd))
              file.copy(src, 'report.Rmd', overwrite = TRUE)
              file.copy(src2, 'resultMta.RData', overwrite = TRUE)
              out <- rmarkdown::render('report.Rmd', params = list(toDownload=TRUE),switch(
                "HTML",
                HTML = rmdformats::robobook(toc_depth = 4)
                # HTML = rmarkdown::html_document()
              ))
              file.rename(out, file)
            }
          )
        }else{
          cat(paste("Analysis failed with the following error message: \n\n",result[[1]]))
          output$predictionsMta <- DT::renderDT({DT::datatable(NULL)})
          output$metricsMta <- DT::renderDT({DT::datatable(NULL)})
          output$modelingMta <- DT::renderDT({DT::datatable(NULL)})
          hideAll$clearAll <- TRUE
        }

      }

      hideAll$clearAll <- FALSE

    })

  })
}
# mod_mtaApp_server <- function(id, data){
#   moduleServer( id, function(input, output, session){
#     ns <- session$ns
#
#     output$plotDataDependencies <- shiny::renderPlot({ dependencyPlot() })
#     ############################################################################ clear the console
#     hideAll <- reactiveValues(clearAll = TRUE)
#     observeEvent(data(), {
#       hideAll$clearAll <- TRUE
#     })
#     ############################################################################
#     # show shinyWidgets until the user can use the module
#     observeEvent(c(data(), input$version2Mta), {
#       req(data())
#       mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
#       if(mappedColumns == 3 & length(input$version2Mta)>0 ){
#         golem::invoke_js('showid', ns('holder1'))
#         golem::invoke_js('showid', ns('holder2'))
#         golem::invoke_js('showid', ns('holder3'))
#         golem::invoke_js('showid', ns('holder4'))
#         golem::invoke_js('showid', ns('holder5'))
#         golem::invoke_js('showid', ns('holder6'))
#       }else{
#         golem::invoke_js('hideid', ns('holder1'))
#         golem::invoke_js('hideid', ns('holder2'))
#         golem::invoke_js('hideid', ns('holder3'))
#         golem::invoke_js('hideid', ns('holder4'))
#         golem::invoke_js('hideid', ns('holder5'))
#         golem::invoke_js('hideid', ns('holder6'))
#       }
#     })
#     ## data example loading
#     observeEvent(
#       input$launch,
#       if(length(input$launch) > 0){
#         if (input$launch) {
#           shinyWidgets::ask_confirmation(
#             inputId = ns("myconfirmation"),
#             text = "Are you sure you want to load the example data? This will delete any data currently in the environment.",
#             title = "Data replacement warning"
#           )
#         }
#       }
#     )
#     observeEvent(input$myconfirmation, {
#       if (isTRUE(input$myconfirmation)) {
#         shinybusy::show_modal_spinner('fading-circle', text = 'Loading example...')
#         ## replace tables
#         data(cgiarBase::create_getData_object())
#         tmp <- data()
#         utils::data(DT_example, package = "cgiarPipeline")
#         if(!is.null(result$data)){tmp$data <- result$data}
#         if(!is.null(result$metadata)){tmp$metadata <- result$metadata}
#         if(!is.null(result$modifications)){tmp$modifications <- result$modifications}
#         if(!is.null(result$predictions)){tmp$predictions <- result$predictions}
#         if(!is.null(result$metrics)){tmp$metrics <- result$metrics}
#         if(!is.null(result$modeling)){tmp$modeling <- result$modeling}
#         if(!is.null(result$status)){tmp$status <- result$status}
#         data(tmp) # update data with results
#         shinybusy::remove_modal_spinner()
#       }else{
#         shinyWidgets::updatePrettySwitch(session, "launch", value = FALSE)
#       }
#     }, ignoreNULL = TRUE)
#     ############################################################################
#     observeEvent(
#       input$modelMet,
#       if (input$modelMet %in% c('gblup','rrblup','ssgblup') ) {
#         golem::invoke_js('showid', ns('ismarkermodel'))
#       } else {
#         golem::invoke_js('hideid', ns('ismarkermodel'))
#       }
#     )
#     # warning message
#     output$warningMessage <- renderUI(
#       if(is.null(data())){
#         HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your phenotypic data using the 'Data Retrieval' tab.")) )
#       }else{ # data is there
#         mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
#         if(mappedColumns == 3){
#           if("sta" %in% data()$status$module){
#             HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to perform the multi-trial analysis specifying your input parameters under the Input tabs.")) )
#           }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please perform single-trial analysis before performing a the multi-trial analysis.")) ) }
#         }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please make sure that you have computed the 'environment' column, and that column 'designation' and \n at least one trait have been mapped using the 'Data Retrieval' tab.")) )}
#       }
#     )
#     #################
#     ## version
#     observeEvent(c(data()), {
#       req(data())
#       dtMta <- data()
#       dtMta <- dtMta$status
#       dtMta <- dtMta[which(dtMta$module == "sta"),]
#       traitsMta <- unique(dtMta$analysisId)
#       if(length(traitsMta) > 0){names(traitsMta) <- as.POSIXct(traitsMta, origin="1970-01-01", tz="GMT")}
#       updateSelectInput(session, "version2Mta", choices = traitsMta)
#     })
#     # ## version qa marker
#     observeEvent(c(data()), {
#       req(data())
#       dtMta <- data()
#       dtMta <- dtMta$status
#       dtMta <- dtMta[which(dtMta$module == "qaGeno"),]
#       traitsMta <- unique(dtMta$analysisId)
#       if(length(traitsMta) > 0){names(traitsMta) <- as.POSIXct(traitsMta, origin="1970-01-01", tz="GMT")}
#       updateSelectInput(session, "versionMarker2Mta", choices = traitsMta)
#     })
#     #################
#     ## traits
#     observeEvent(c(data(), input$version2Mta), {
#       req(data())
#       req(input$version2Mta)
#       dtMta <- data()
#       dtMta <- dtMta$predictions
#       dtMta <- dtMta[which(dtMta$analysisId %in% input$version2Mta),]
#       traitsMta <- unique(dtMta$trait)
#       updateSelectInput(session, "trait2Mta", choices = traitsMta)
#       updateSelectInput(session, "traitCor", choices = traitsMta)
#       updateSelectInput(session, "traitConnect", choices = traitsMta)
#       updateSelectInput(session, "evaluationUnitsTrait", choices = traitsMta)
#     })
#     #################
#     ## fixed effects
#     observeEvent(c(data(),input$version2Mta, input$trait2Mta), {
#       req(data())
#       req(input$version2Mta)
#       req(input$trait2Mta)
#       dtMta <- data()
#       metaPheno <- dtMta$metadata$pheno[which(dtMta$metadata$pheno$parameter %in% c("environment","year","season","country","location","trial","study","management")),]
#       metaPheno <- metaPheno[which(!duplicated(metaPheno$value)),] # avoid error when user maps columns twice
#       otherMetaCols <- unique(dtMta$data$pheno[,metaPheno$value,drop=FALSE])
#       colnames(otherMetaCols) <- cgiarBase::replaceValues(Source = colnames(otherMetaCols), Search = metaPheno$value, Replace = metaPheno$parameter )
#       otherMetaCols <- otherMetaCols[which(!duplicated(otherMetaCols[,"environment"])),,drop=FALSE] # we do this in case the users didn't define the environment properly
#       dtMta <- dtMta$predictions
#       dtMta <- dtMta[which(dtMta$analysisId %in% input$version2Mta),]
#       dtMta <- merge(dtMta, otherMetaCols, by="environment", all.x = TRUE)
#       traitsMta <- apply(dtMta[,c(metaPheno$parameter,"designation")],2,function(x){length(unique(x))})
#       traitsMta <- names(traitsMta)[which(traitsMta > 1)] # remove factors that do not have more than one level
#       start <- setdiff(traitsMta,c("designation","entryType","pipeline"))
#       updateSelectInput(session, "fixedTermMta2", choices = traitsMta, selected = ifelse('environment'%in%start, 'environment',  start) )
#     })
#     #################
#     ## random effects
#     observeEvent(c(data(), input$version2Mta, input$trait2Mta, input$fixedTermMta2 ), {
#       req(data())
#       req(input$version2Mta)
#       req(input$trait2Mta)
#       dtMta <- data()
#       metaPheno <- dtMta$metadata$pheno[which(dtMta$metadata$pheno$parameter %in% c("environment","year","season","country","location","trial","study","management")),]
#       metaPheno <- metaPheno[which(!duplicated(metaPheno$value)),] # avoid error when user maps columns twice
#       otherMetaCols <- unique(dtMta$data$pheno[,metaPheno$value,drop=FALSE])
#       colnames(otherMetaCols) <- cgiarBase::replaceValues(Source = colnames(otherMetaCols), Search = metaPheno$value, Replace = metaPheno$parameter )
#       otherMetaCols <- otherMetaCols[which(!duplicated(otherMetaCols[,"environment"])),,drop=FALSE] # we do this in case the users didn't define the environment properly
#       dtMta <- dtMta$predictions
#       dtMta <- dtMta[which(dtMta$analysisId %in% input$version2Mta),]
#       dtMta <- merge(dtMta, otherMetaCols, by="environment", all.x = TRUE)
#       traitsMta <- apply(dtMta[,c(metaPheno$parameter,"designation")],2,function(x){length(unique(x))})
#       traitsMta <- names(traitsMta)[which(traitsMta > 1)]
#       traitsMta <- setdiff(traitsMta, input$fixedTermMta2)
#       updateSelectInput(session, "randomTermMta2", choices = traitsMta, selected = "designation")
#     })
#     #################
#     ## model types
#     observeEvent(c(data(), input$version2Mta, input$trait2Mta, input$fixedTermMta2), {
#       req(data())
#       req(input$version2Mta)
#       req(input$trait2Mta)
#       # req(input$fixedTermMta2)
#       if('designation'%in%input$fixedTermMta2){
#         traitsMta <- list(BLUE="blue")
#       }else{
#         traitsMta <- list(BLUP="blup",pBLUP="pblup",gBLUP="gblup",ssGBLUP="ssgblup",rrBLUP="rrblup")
#       }
#       updateSelectInput(session, "modelMet", choices = traitsMta, selected = traitsMta[1])
#     })
#     #################
#     ## gXe interactions
#     observeEvent(c(data(), input$trait2Mta), {
#       req(data())
#       req(input$trait2Mta)
#       dtMta <- data() # dtMta <- result
#       dtMta <- dtMta$metadata$weather
#       validWeatherVars <- c("RH2M","T2M","PRECTOTCORR", "latitude", "longitude", "plantingDate", "harvestingDate" )
#       traitsMta <- unique(c("envIndex",intersect(unique(dtMta$trait), validWeatherVars )))
#       updateSelectInput(session, "interactionTermMta2", choices = traitsMta)
#     })
#     #################
#     # reactive table for environments to be included
#     dtFieldMet = reactive({
#       req(data())
#       req(input$version2Mta)
#       dtMta <- data()
#       dtProv = dtMta$predictions
#       if(!is.null(dtProv)){
#         dtProv <- dtProv[which(dtProv$analysisId %in% input$version2Mta),]
#         dtProvTable=  as.data.frame( do.call( rbind, list (with(dtProv, table(environment,trait)) ) ) )
#         bad <- which(dtProvTable <= 1, arr.ind = TRUE)
#         if(nrow(bad) > 0){dtProvTable[bad] = 0}
#         dtProvTable[which(dtProvTable > 1, arr.ind = TRUE)] = 1
#       }else{dtProvTable <- data.frame()}
#       return(dtProvTable)
#     })
#     x = reactiveValues(df = NULL)
#     observe({
#       df <- dtFieldMet()
#       x$df <- df
#     })
#     output$fieldsMet = DT::renderDT(x$df, selection = 'none', editable = TRUE)
#     proxy = DT::dataTableProxy('fieldsMet')
#     observeEvent(input$fieldsMet_cell_edit, {
#       info = input$fieldsMet_cell_edit
#       utils::str(info)
#       i = info$row
#       j = info$col
#       v = info$value
#       x$df[i, j] <- isolate(DT::coerceValue(v, x$df[i, j]))
#     })
#     #################
#     # reactive table for trait family distributions
#     dtDistTrait = reactive({
#       traitNames = input$trait2Mta
#       mm = matrix(0,nrow = 8, ncol = length(traitNames));
#       rownames(mm) <- c(
#         "quasi(link = 'identity', variance = 'constant')", "gaussian(link = 'identity')", "binomial(link = 'logit')",  "Gamma(link = 'inverse')",
#         "inverse.gaussian(link = '1/mu^2')", "poisson(link = 'log')", "quasibinomial(link = 'logit')", "quasipoisson(link = 'log')"
#       );
#       colnames(mm) <- traitNames
#       dtProvTable = as.data.frame(mm);  colnames(dtProvTable) <- traitNames
#       return(dtProvTable)
#     })
#     xx = reactiveValues(df = NULL)
#     observe({
#       df <- dtDistTrait()
#       xx$df <- df
#     })
#     output$traitDistMet = DT::renderDT(xx$df,
#                                        selection = 'none',
#                                        editable = TRUE,
#                                        options = list(paging=FALSE,
#                                                       searching=FALSE,
#                                                       initComplete = I("function(settings, json) {alert('Done.');}")
#                                        )
#     )
#     proxy = DT::dataTableProxy('traitDistMet')
#     observeEvent(input$traitDistMet_cell_edit, {
#       info = input$traitDistMet_cell_edit
#       utils::str(info)
#       i = info$row
#       j = info$col
#       v = info$value
#       xx$df[i, j] <- isolate(DT::coerceValue(v, xx$df[i, j]))
#     })
#
#     ##############################################################################################
#     ##############################################################################################
#     ##############################################################################################
#     ## render timestamps flow
#     output$plotTimeStamps <- shiny::renderPlot({
#       req(data()) # req(input$version2Sta)
#       xx <- data()$status;  yy <- data()$modeling
#       v <- which(yy$parameter == "analysisId")
#       if(length(v) > 0){
#         yy <- yy[v,c("analysisId","value")]
#         zz <- merge(xx,yy, by="analysisId", all.x = TRUE)
#       }else{ zz <- xx; zz$value <- NA}
#       if(!is.null(xx)){
#         colnames(zz) <- cgiarBase::replaceValues(colnames(zz), Search = c("analysisId","value"), Replace = c("outputId","inputId") )
#         nLevelsCheck1 <- length(na.omit(unique(zz$outputId)))
#         nLevelsCheck2 <- length(na.omit(unique(zz$inputId)))
#         if(nLevelsCheck1 > 1 & nLevelsCheck2 > 1){
#           X <- with(zz, sommer::overlay(outputId, inputId))
#         }else{
#           if(nLevelsCheck1 <= 1){
#             X1 <- matrix(ifelse(is.na(zz$inputId),0,1),nrow=length(zz$inputId),1); colnames(X1) <- as.character(na.omit(unique(c(zz$outputId))))
#           }else{X1 <- model.matrix(~as.factor(outputId)-1, data=zz); colnames(X1) <- levels(as.factor(zz$outputId))}
#           if(nLevelsCheck2 <= 1){
#             X2 <- matrix(ifelse(is.na(zz$inputId),0,1),nrow=length(zz$inputId),1); colnames(X2) <- as.character(na.omit(unique(c(zz$inputId))))
#           }else{X2 <- model.matrix(~as.factor(inputId)-1, data=zz); colnames(X2) <- levels(as.factor(zz$inputId))}
#           mynames <- unique(na.omit(c(zz$outputId,zz$inputId)))
#           X <- matrix(0, nrow=nrow(zz), ncol=length(mynames)); colnames(X) <- as.character(mynames)
#           if(!is.null(X1)){X[,colnames(X1)] <- X1}
#           if(!is.null(X2)){X[,colnames(X2)] <- X2}
#         };  rownames(X) <- as.character(zz$outputId)
#         rownames(X) <-as.character(as.POSIXct(as.numeric(rownames(X)), origin="1970-01-01", tz="GMT"))
#         colnames(X) <-as.character(as.POSIXct(as.numeric(colnames(X)), origin="1970-01-01", tz="GMT"))
#         # make the network plot
#         n <- network::network(X, directed = FALSE)
#         network::set.vertex.attribute(n,"family",zz$module)
#         network::set.vertex.attribute(n,"importance",1)
#         e <- network::network.edgecount(n)
#         network::set.edge.attribute(n, "type", sample(letters[26], e, replace = TRUE))
#         network::set.edge.attribute(n, "day", sample(1, e, replace = TRUE))
#         library(ggnetwork)
#         ggplot2::ggplot(n, ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
#           ggnetwork::geom_edges(ggplot2::aes(color = family), arrow = ggplot2::arrow(length = ggnetwork::unit(6, "pt"), type = "closed") ) +
#           ggnetwork::geom_nodes(ggplot2::aes(color = family), alpha = 0.5, size=5 ) + ggplot2::ggtitle("Network plot of current analyses available") +
#           ggnetwork::geom_nodelabel_repel(ggplot2::aes(color = family, label = vertex.names ),
#                                           fontface = "bold", box.padding = ggnetwork::unit(1, "lines")) +
#           ggnetwork::theme_blank()
#       }
#     })
#     ## render the input data to be analyzed
#     output$statusMta <-  DT::renderDT({
#       req(data())
#       req(input$version2Mta)
#       ### change column names for mapping
#       paramsPheno <- data()$modeling
#       paramsPheno <- paramsPheno[which(paramsPheno$analysisId %in% input$version2Mta),, drop=FALSE]
#       paramsPheno$analysisId <- as.POSIXct(paramsPheno$analysisId, origin="1970-01-01", tz="GMT")
#       DT::datatable(paramsPheno, extensions = 'Buttons',
#                     options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
#                                    lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All'))),
#                     caption = htmltools::tags$caption(
#                       style = 'color:cadetblue', #caption-side: bottom; text-align: center;
#                       htmltools::em('Past modeling parameters from STA stamp(s) selected.')
#                     )
#       )
#     })
#
#     output$evaluationUnits <-  shiny::renderPlot({ #DT::renderDT({
#       req(data())
#       req(input$version2Mta)
#       req(input$evaluationUnitsTrait)
#       object <- data()
#       if(!is.null(object$predictions)){
#         phenoNames <- na.omit(unique(object$predictions[which(object$predictions$analysisId %in% input$version2Mta  &  object$predictions$trait == input$evaluationUnitsTrait),"designation"]))
#       }else{ phenoNames <- character() }
#
#       if(!is.null(object$data$geno)){
#         genoNames <- rownames(object$data$geno)
#       }else{ genoNames <- character() }
#
#       if(!is.null(object$data$pedigree)){
#         metaPed <- object$metadata$pedigree
#         pedCols <- metaPed[which(metaPed$parameter %in% c("designation","mother","father")), "value"]
#         pedCols <- setdiff(pedCols,"")
#         pedNames <- na.omit(unique(unlist(as.vector(object$data$pedigree[,pedCols, drop=FALSE]))))
#       }else{ pedNames <- character() }
#
#       if(!is.null(object$data$qtl)){
#         metaQtl <- object$metadata$qtl
#         qtlCols <- metaQtl[which(metaQtl$parameter %in% c("designation")), "value"]
#         qtlCols <- setdiff(qtlCols,"")
#         qtlNames <- na.omit(unique(object$data$qtl[,qtlCols]))
#       }else{ qtlNames <- character() }
#
#       splitAggregate <- list(phenoNames, genoNames, pedNames, qtlNames)
#       names(splitAggregate) <- c("With-Phenotype","With-Genotype","With-Pedigree","With-QTL")
#
#       nagm <- matrix(0,length(splitAggregate),length(splitAggregate)); rownames(nagm) <- colnames(nagm) <- names(splitAggregate) # prefilled matrix
#       for(i in 1:length(splitAggregate)){ # fill the matrix of intersection of individuals between pair of environments
#         for(j in 1:i){
#           nagm[i,j] <- length(intersect(splitAggregate[[i]],splitAggregate[[j]]))
#         }
#       }
#       nagm[upper.tri(nagm)] <- t(nagm)[upper.tri(nagm)] # fill the upper triangular
#       mydata4 <- cgiarBase::matToTab(nagm) # matrix to a dataframe for plot
#       maxVal <- max(nagm, na.rm = TRUE) # get the maximum value found in the matrix of connectivity
#       midval <- (max(nagm, na.rm = TRUE) - min(nagm, na.rm = TRUE) )/2
#       p <- ggplot2::ggplot(data = mydata4, ggplot2::aes(Var2, Var1, fill = Freq))+
#         ggplot2::geom_tile(color = "white")+ ggplot2::ggtitle("Available data for this trait.") +
#         ggplot2::scale_fill_gradient2(low = "firebrick", high = "#038542", mid = "gold",
#                                       midpoint = midval, limit = c(0,maxVal), space = "Lab",
#                                       name="Connectivity (data types)") +
#         ggplot2::theme_minimal()+
#         ggplot2::geom_text(ggplot2::aes(label = Freq), color = "white", size = 3 ) +
#         ggplot2::ylab("") + ggplot2::xlab("") +
#         ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,  hjust = 1, face = "bold"))+
#         ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 0, vjust = 1,  hjust = 1, face = "bold")) +
#         ggplot2::coord_fixed()
#       p
#     })
#
#     output$phenoMta <-  DT::renderDT({
#       req(data())
#       req(input$version2Mta)
#       dtMta <- data()
#       dtMta <- dtMta$predictions
#       dtMta <- dtMta[which(dtMta$analysisId %in% input$version2Mta),setdiff(colnames(dtMta),c("module","analysisId"))]
#       numeric.output <- c("predictedValue", "stdError", "reliability")
#       DT::formatRound(DT::datatable(dtMta, extensions = 'Buttons',
#                                     options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
#                                                    lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All'))),
#                                     caption = htmltools::tags$caption(
#                                       style = 'color:cadetblue', #caption-side: bottom; text-align: center;
#                                       htmltools::em('STA predictions table to be used as input.')
#                                     )
#       ), numeric.output)
#     })
#     ## render connectivity plot
#     observeEvent(c(data(),input$version2Mta), { # update entry types included in the plot
#       req(data())
#       req(input$version2Mta)
#       dtMta <- data()
#       dtMta <- dtMta$predictions
#       dtMta <- dtMta[which(dtMta$analysisId %in% input$version2Mta),] # only traits that have been QA
#       entryTypeMtaInput <- unique(dtMta$entryType)
#       updateSelectInput(session, "entryTypeMta", choices = c(entryTypeMtaInput,"Generic"), selected = "Generic")
#     })
#     output$plotPredictionsSparsity <- shiny::renderPlot({
#       req(data())
#       req(input$version2Mta)
#       req(input$entryTypeMta)
#       dtMta <- data()
#       mydata <- dtMta$predictions
#       mydata <- mydata[which(mydata$analysisId %in% input$version2Mta),] # only PREDICTIONS FROM THE STA
#       if(input$entryTypeMta != "Generic"){
#         mydata <- mydata[which(mydata[,"entryType"] %in% input$entryTypeMta),]
#       }
#       M <- table(mydata$designation, mydata$environment)
#       M2 <- matrix(M, nrow = nrow(M), ncol = ncol(M))
#       M2 <- M2[,(input$slider2[1]):min(c(input$slider2[2], ncol(M2) )), drop=FALSE] # environments
#       M2 <- M2[(input$slider1[1]):min(c(input$slider1[2]), nrow(M2) ), ,drop=FALSE] # genotypes
#       Matrix::image(as(t(M2), Class = "dgCMatrix"), xlab="Genotypes", ylab="Environments", colorkey=TRUE)
#     })
#     # render correlation plot
#     output$plotPredictionsCor <-  shiny::renderPlot({
#       req(data())
#       req(input$version2Mta)
#       req(input$traitCor)
#       req(input$heatmapFontSizeCor)
#       dtMta <- data()
#       mydata <- dtMta$predictions
#       mydata <- mydata[which(mydata$analysisId %in% input$version2Mta),] # only PREDICTIONS FROM THE STA
#       predictions.gcorrE <- subset(mydata, select = c(trait,designation,environment,predictedValue))
#       predictions.gcorrE2 <- predictions.gcorrE[predictions.gcorrE$trait == input$traitCor, ]
#       wide <- stats::reshape(predictions.gcorrE2,
#                              direction = "wide", idvar = "designation",
#                              timevar = "environment", v.names = "predictedValue", sep= "")
#       colnames(wide) <- gsub("predictedValue","",colnames(wide))
#       wide2 <- as.data.frame(wide[,-c(1:2)]); colnames(wide2) <- colnames(wide)[-c(1:2)]
#       corr <- round(stats::cor(wide2, use="pairwise.complete.obs"),2)
#       mydata4 <- cgiarBase::matToTab(corr)
#       p <- ggplot2::ggplot(data = mydata4, ggplot2::aes(Var2, Var1, fill = Freq))+
#         ggplot2::geom_tile(color = "white")+
#         ggplot2::scale_fill_gradient2(low = "#E46726", high = "#038542", mid = "white",
#                                       midpoint = 0, limit = c(-1,1), space = "Lab",
#                                       name="Pearson\nCorrelation") +
#         ggplot2::theme_minimal()+
#         ggplot2::ylab("") + ggplot2::xlab("") +
#         ggplot2::coord_fixed()
#       if(!is.null(input$checkboxTextCor)){ # if user wants to fill cell values
#         p <- p + ggplot2::geom_text(ggplot2::aes(label = round(Freq,2) ), color = "black", size = max(c(input$heatmapFontSizeCor-3, 1)))
#       }
#       if(!is.null(input$checkboxAxisCor)){ # if user wants to add axis labels
#         p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, size = input$heatmapFontSizeCor, hjust = 1, face="bold"))+
#           ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 0, vjust = 1, size = input$heatmapFontSizeCor, hjust = 1, face = "bold"))
#       }else{
#         p <- p + ggplot2::theme( axis.text.x=ggplot2::element_blank(), axis.text.y=ggplot2::element_blank() )
#       }
#       p
#
#     })
#     # render connectivity plot
#     output$plotPredictionsConnectivity <-  shiny::renderPlot({ # plotly::renderPlotly({
#       req(data())
#       req(input$version2Mta)
#       req(input$traitConnect)
#       req(input$entryTypeMta)
#       req(input$heatmapFontSize)
#       dtMta <- data()
#       mydata <- dtMta$predictions # extract predictions
#       mydata <- mydata[which(mydata$analysisId %in% input$version2Mta),] # only PREDICTIONS FROM THE STA
#       mydata <- mydata[which(mydata$trait %in% input$traitConnect),] # only PREDICTIONS FROM Trait selected
#       if(input$entryTypeMta != "Generic"){ # use an specific type of entries
#         mydata <- mydata[which(mydata[,"entryType"] %in% input$entryTypeMta),]
#       }
#       splitAggregate <- with(mydata,  split(mydata[,"designation"],mydata[,"environment"]) ) # split by environment
#       splitAggregate <- lapply(splitAggregate,unique); nag <- length(splitAggregate) # get unique individual names
#       nagm <- matrix(0,nag,nag); rownames(nagm) <- colnames(nagm) <- names(splitAggregate) # prefilled matrix
#       for(i in 1:length(splitAggregate)){ # fill the matrix of intersection of individuals between pair of environments
#         for(j in 1:i){
#           nagm[i,j] <- length(intersect(splitAggregate[[i]],splitAggregate[[j]]))
#         }
#       }
#       nagm[upper.tri(nagm)] <- t(nagm)[upper.tri(nagm)] # fill the upper triangular
#       mydata4 <- cgiarBase::matToTab(nagm) # matrix to a dataframe for plot
#       maxVal <- max(nagm, na.rm = TRUE) # get the maximum value found in the matrix of connectivity
#       p <- ggplot2::ggplot(data = mydata4, ggplot2::aes(Var2, Var1, fill = Freq))+
#         ggplot2::geom_tile(color = "white")+
#         ggplot2::scale_fill_gradient2(low = "firebrick", high = "#038542", mid = "gold",
#                                       midpoint = 60, limit = c(0,maxVal), space = "Lab",
#                                       name="Connectivity") +
#         ggplot2::theme_minimal()+
#         ggplot2::ylab("") + ggplot2::xlab("") +
#         ggplot2::coord_fixed()
#       if(!is.null(input$checkboxText)){ # if user wants to add text to the cells
#         p <- p + ggplot2::geom_text(ggplot2::aes(label = Freq), color = "white", size = max(c(input$heatmapFontSize-3,1)))
#       }
#       if(!is.null(input$checkboxAxis)){ # if user wants to add labels to the axis
#         p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, size = input$heatmapFontSize, hjust = 1, face = "bold"))+
#           ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 0, vjust = 1, size = input$heatmapFontSize, hjust = 1, face = "bold"))
#       }else{
#         p <- p + ggplot2::theme( axis.text.x=ggplot2::element_blank(), axis.text.y=ggplot2::element_blank() )
#       }
#       p
#     })
#     ## render metrics barplot
#     observeEvent(c(data(),input$version2Mta), { # update trait
#       req(data())
#       req(input$version2Mta)
#       dtMta <- data()
#       dtMta <- dtMta$metrics
#       dtMta <- dtMta[which(dtMta$analysisId %in% input$version2Mta),] # only traits that have been QA
#       traitMtaInput <- unique(dtMta$trait)
#       updateSelectInput(session, "traitMetrics", choices = traitMtaInput, selected = traitMtaInput)
#     })
#     observeEvent(c(data(),input$version2Mta), { # update parameter
#       req(data())
#       req(input$version2Mta)
#       dtMta <- data()
#       dtMta <- dtMta$metrics
#       dtMta <- dtMta[which(dtMta$analysisId %in% input$version2Mta),] # only traits that have been QA
#       metricsMtaInput <- unique(dtMta$parameter)
#       updateSelectInput(session, "parameterMetrics", choices = metricsMtaInput)
#     })
#     output$barplotPredictionsMetrics <- plotly::renderPlotly({
#       req(data())
#       req(input$version2Mta)
#       dtMta <- data()
#       mydata <- dtMta$metrics
#       mydata <- mydata[which(mydata$analysisId %in% input$version2Mta),]
#       mydata = mydata[which(mydata$parameter %in% input$parameterMetrics),]
#       mydata = mydata[which(mydata$trait %in% input$traitMetrics),]
#       p <- ggplot2::ggplot(data=mydata, ggplot2::aes(x=environment, y=value, fill=trait)) +
#         ggplot2::geom_bar(stat="identity", position=position_dodge())+
#         ggplot2::theme_minimal()+  ggplot2::ggtitle("Metrics associated to the STA stamp selected")
#       if(input$checkbox1){
#         p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1))
#       }else{
#         p <- p + ggplot2::theme(axis.text.x = ggplot2::element_blank())
#       }
#       plotly::ggplotly(p)
#     })
#     ## render trait distribution plot
#     observeEvent(c(data(),input$version2Mta), { # update trait
#       req(data())
#       req(input$version2Mta)
#       dtMta <- data()
#       dtMta <- dtMta$predictions
#       dtMta <- dtMta[which(dtMta$analysisId %in% input$version2Mta),] # only traits that have been QA
#       traitMtaInput <- unique(dtMta$trait)
#       updateSelectInput(session, "trait3Mta", choices = traitMtaInput)
#     })
#     output$plotPredictionsCleanOut <- shiny::renderPlot({ # plotly::renderPlotly({
#       req(data())
#       req(input$version2Mta)
#       req(input$trait3Mta)
#       req(input$groupMtaInputPlot)
#       mydata <- data()$predictions
#       mydata <- mydata[which(mydata$analysisId %in% input$version2Mta),] # only traits that have been QA
#       mydata <- mydata[which(mydata[,"trait"] %in% input$trait3Mta),]
#       mydata[, "environment"] <- as.factor(mydata[, "environment"]); mydata[, "designation"] <- as.factor(mydata[, "designation"])
#       p <- ggplot2::ggplot(mydata, ggplot2::aes(x=as.factor(environment), y=predictedValue)) +
#         ggplot2::geom_boxplot(fill='#A4A4A4', color="black", notch = TRUE)+
#         ggplot2::theme_classic()+ ggplot2::ggtitle("Dispersal of predictions associated to the STA stamp selected") +
#         ggplot2::geom_jitter(alpha = 0.4, colour="cadetblue") + # ggplot2::aes(colour = color),
#         ggplot2::xlab("Environment") + ggplot2::ylab("Predicted value")
#       if(input$checkbox2){
#         p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1))
#       }else{
#         p <- p + ggplot2::theme(axis.text.x = ggplot2::element_blank())
#       }
#       p
#     })
#     ## render result of "run" button click
#     outMta <- eventReactive(input$runMta, {
#       req(data())
#       req(input$version2Mta)
#       req(input$trait2Mta)
#       shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
#       dtMta <- data()
#       # family distributions input
#       myFamily = apply(xx$df,2,function(y){rownames(xx$df)[which(y > 0)[1]]})
#       dontHaveDist <- which(is.na(myFamily))
#       if(length(dontHaveDist) > 0){myFamily[dontHaveDist] <- "quasi(link = 'identity', variance = 'constant')"}
#
#       # run the modeling, but before test if sta was done
#       if(sum(dtMta$status$module %in% "sta") == 0) {
#         output$qaQcMtaInfo <- renderUI({
#           if (hideAll$clearAll)
#             return()
#           else
#             req(dtMta)
#           HTML(as.character(div(style="color: brown;",
#                                 "Please perform Single-Trial-Analysis before conducting a Multi-Trial Analysis when using a two-stage analysis."))
#           )
#         })
#       }else{ # sta is available
#         output$qaQcMtaInfo <- renderUI({return(NULL)})
#         if(input$modelMet %in% c("gblup","rrblup","ssblup") ){ # warning
#           if(input$versionMarker2Mta == '' | is.null(input$versionMarker2Mta) ){ # user didn't provide a modifications id
#             if(!is.null(dtMta$data$geno)){ # if user actually has marker data
#               shinybusy::remove_modal_spinner()
#               cat("Please run the 'Markers QA/QC' module prior to run a gBLUP or rrBLUP model.")
#               # stop("Please run the 'Markers QA/QC' module prior to run a gBLUP or rrBLUP model.", call. = FALSE)
#             }else{ # if user does NOT have marker data and wanted a marker-based model
#               shinybusy::remove_modal_spinner()
#               cat("Please pick a different model, rrBLUP, gBLUP and ssBLUP require marker information. Alternatively, go back to the 'Retrieve Data' section and upload your marker data.")
#               # stop("Please pick a different model, rrBLUP, gBLUP and ssBLUP require marker information. Alternatively, go back to the 'Retrieve Data' section and upload your marker data.")
#             }
#           }else{ markerVersionToUse <- input$versionMarker2Mta} # there is a versionMarker2Mta id
#         }else{ markerVersionToUse <- NULL } # for non marker based model we don't need to provide this
#         getOption("spam.cholsymmetrycheck")
#         options(spam.cholsymmetrycheck=FALSE)
#         getOption("spam.cholsymmetrycheck")
#         result <- try(cgiarPipeline::metLMM(
#           phenoDTfile= dtMta, # analysis to be picked from predictions database
#           analysisId=input$version2Mta,
#           analysisIdForGenoModifications = markerVersionToUse, # marker modifications
#           fixedTerm= input$fixedTermMta2,  randomTerm=input$randomTermMta2,  residualBy=NULL,
#           interactionsWithGeno=input$interactionTermMta2, envsToInclude=x$df,
#           trait= input$trait2Mta, traitFamily=myFamily, useWeights=input$useWeights,
#           heritLB= as.numeric(unlist(strsplit(input$heritLBMet,","))),
#           heritUB= as.numeric(unlist(strsplit(input$heritUBMet,","))),
#           meanLB = as.numeric(unlist(strsplit(input$meanLBMet,","))),
#           meanUB = as.numeric(unlist(strsplit(input$meanUBMet,","))),
#           modelType=input$modelMet, # either "grm", "nrm", or both
#           nMarkersRRBLUP=input$nMarkersRRBLUP,
#           deregress=input$deregressMet,  nPC=input$nPC,
#           maxIters=input$maxitMet, batchSizeToPredict=500, tolParInv=1e-4,
#           minimumNumberEnvsFW =input$minimumNumberEnvsFW,verbose=FALSE
#         ),
#         silent=TRUE
#         )
#         if(!inherits(result,"try-error")) {
#           data(result) # update data with results
#           cat(paste("Multi-trial analysis step with id:",as.POSIXct(result$status$analysisId[length(result$status$analysisId)], origin="1970-01-01", tz="GMT"),"saved. Please proceed to construct a selection index using this time stamp."))
#           updateTabsetPanel(session, "tabsMain", selected = "outputTabs")
#         }else{
#           cat(paste("Analysis failed with the following error message: \n\n",result[[1]]))
#         }
#
#       }
#       shinybusy::remove_modal_spinner()
#
#       if(!inherits(result,"try-error")) { # if all goes well in the run
#         ## predictions table
#         output$predictionsMta <-  DT::renderDT({
#           # if ( hideAll$clearAll){
#           #   return()
#           # }else{
#           predictions <- result$predictions
#           predictions <- predictions[predictions$module=="mta",]
#           predictions$analysisId <- as.numeric(predictions$analysisId)
#           predictions <- predictions[!is.na(predictions$analysisId),]
#           current.predictions <- predictions[predictions$analysisId==max(predictions$analysisId),]
#           current.predictions <- subset(current.predictions, select = -c(module,analysisId))
#           numeric.output <- c("predictedValue", "stdError", "reliability")
#           DT::formatRound(DT::datatable(current.predictions, extensions = 'Buttons',
#                                         options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
#                                                        lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
#           ), numeric.output)
#           # }
#         })
#         # metrics table
#         output$metricsMta <-  DT::renderDT({
#           if(!inherits(result,"try-error") ){
#             # if ( hideAll$clearAll){
#             #   return()
#             # }else{
#             metrics <- result$metrics
#             mtas <- result$status[which(result$status$module == "mta"),"analysisId"]; mtaId <- mtas[length(mtas)]
#             metrics <- metrics[which(metrics$analysisId == mtaId),]
#             metrics <- subset(metrics, select = -c(module,analysisId))
#             numeric.output <- c("value", "stdError")
#             DT::formatRound(DT::datatable(metrics, extensions = 'Buttons',
#                                           options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
#                                                          lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
#             ), numeric.output)
#             # }
#           }
#         })
#         # modeling table
#         output$modelingMta <-  DT::renderDT({
#           if(!inherits(result,"try-error") ){
#             # if ( hideAll$clearAll){
#             #   return()
#             # }else{
#             modeling <- result$modeling
#             mtas <- result$status[which(result$status$module == "mta"),"analysisId"]; mtaId <- mtas[length(mtas)]
#             modeling <- modeling[which(modeling$analysisId == mtaId),]
#             modeling <- subset(modeling, select = -c(module,analysisId))
#             # modeling$analysisId <- as.POSIXct(modeling$analysisId, origin="1970-01-01", tz="GMT")
#             DT::datatable(modeling, extensions = 'Buttons',
#                           options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
#                                          lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
#             )
#             # }
#           }
#         })
#         # ## Report tab
#         output$reportMta <- renderUI({
#           HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportMta.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
#           # HTML(markdown::markdownToHTML(knitr::knit("./R/reportMta.Rmd", quiet = TRUE), fragment.only=TRUE))
#         })
#
#         output$downloadReportMta <- downloadHandler(
#           filename = function() {
#             paste('my-report', sep = '.', switch(
#               "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
#             ))
#           },
#           content = function(file) {
#             src <- normalizePath(system.file("rmd","reportMta.Rmd",package="bioflow"))
#             src2 <- normalizePath('data/resultMta.RData')
#             # temporarily switch to the temp dir, in case you do not have write
#             # permission to the current working directory
#             owd <- setwd(tempdir())
#             on.exit(setwd(owd))
#             file.copy(src, 'report.Rmd', overwrite = TRUE)
#             file.copy(src2, 'resultMta.RData', overwrite = TRUE)
#             out <- rmarkdown::render('report.Rmd', params = list(toDownload=TRUE),switch(
#               "HTML",
#               HTML = rmdformats::robobook(toc_depth = 4)
#               # HTML = rmarkdown::html_document()
#             ))
#             file.rename(out, file)
#           }
#         )
#
#       } else {
#         output$predictionsMta <- DT::renderDT({DT::datatable(NULL)})
#         output$metricsMta <- DT::renderDT({DT::datatable(NULL)})
#         output$modelingMta <- DT::renderDT({DT::datatable(NULL)})
#         hideAll$clearAll <- TRUE
#       } ### enf of if(!inherits(result,"try-error"))
#
#       hideAll$clearAll <- FALSE
#
#     }) ## end eventReactive
#
#     output$outMta <- renderPrint({
#       outMta()
#     })
#
#   })
# }

## To be copied in the UI
# mod_mtaApp_ui("mtaApp_1")

## To be copied in the server
# mod_mtaApp_server("mtaApp_1")

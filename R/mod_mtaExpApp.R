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
                           tabPanel(div(icon("book"), "Information-MTA-Flex") ,
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
                                           p(strong("Left side formula.-")," Columns to be fitted as fixed effects."),
                                           p(strong("Center formula.-")," Columns to be fitted as random effects."),
                                           p(strong("Right side formula.-")," Column to fit as interactions with the genotype effect. This only be fitted if the number of environments is 4 or more."),
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
                                           p("Henderson Jr, C. R. (1982). Analysis of covariance in the mixed model: higher-level, nonhomogeneous, and random regressions. Biometrics, 623-640."),
                                           p("Odegard, J., Indahl, U., Stranden, I., & Meuwissen, T. H. (2018). Large-scale genomic prediction using singular value decomposition of the genotype matrix. Genetics Selection Evolution, 50(1), 1-12."),
                                           h2(strong("Software used:")),
                                           p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
                                Vienna, Austria. URL https://www.R-project.org/."),
                                           p("COVARRUBIAS PAZARAN, G. E. (2024). lme4breeding: enabling genetic evaluation in the era of genomic data. bioRxiv, 2024-05."),
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
                                      tabPanel("Pick the model", icon = icon("dice-two"),
                                               br(),
                                               column(width=12, style = "background-color:grey; color: #FFFFFF",

                                                      column(width=12,
                                                             column(width=4, selectInput(ns("trait2Mta"), "Trait to analyze (required)", choices = NULL, multiple = TRUE) ),
                                                             column(width=2, numericInput(ns("nTerms"), label = "nTerms", value = 3, step = 1, min = 1, max=100) ),
                                                             column(width=3, selectInput(ns("modelMet"), "Designation model", choices = NULL, multiple = FALSE) ),
                                                             column(width=3, tags$span(id = ns('ismarkermodel'), selectInput(ns("versionMarker2Mta"), "Marker QA version to use", choices = NULL, multiple = FALSE), ),  ),
                                                      ),
                                                      column(width=12,
                                                             column(width=2, shinyWidgets::prettySwitch( inputId = ns('dg_model'), label = "Diagonal Model", status = "success") ),
                                                             column(width=2, shinyWidgets::prettySwitch( inputId = ns('cs_model'), label = "Compound Symmetry", status = "success") ),
                                                             column(width=2, shinyWidgets::prettySwitch( inputId = ns('csdg_model'), label = "Main + Diagonal", status = "success") ),
                                                             column(width=2, shinyWidgets::prettySwitch( inputId = ns('fw_model'), label = "Finlay-Wilkinson Reg", status = "success") ),
                                                             column(width=2, shinyWidgets::prettySwitch( inputId = ns('uns_model'), label = "Unstructured", status = "success") ),
                                                             column(width=2, shinyWidgets::prettySwitch( inputId = ns('fa_model'), label = "Reduced Rank (FA)", status = "success") ),
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
                                                                          numericInput(ns("nMarkersRRBLUP"), label = "Maximum number of markers to use in rrBLUP or GBLUP", value = 1000),
                                                                          selectInput(ns("useWeights"), label = "Use weights?", choices = list(TRUE,FALSE), selected = TRUE, multiple=FALSE),

                                                      ),
                                               ),
                                               column(width=12,
                                                      hr(style = "border-top: 3px solid #4c4c4c;"),
                                                      h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameter values to be specified in the grey boxes above.", style="color:green"))),
                                                      hr(style = "border-top: 3px solid #4c4c4c;"),
                                               ),
                                               tags$span(id = ns('holder3'),
                                                         column(width=12, p(span("Connectivity between data types.", style="color:black")) ),
                                                         selectInput(ns("evaluationUnitsTrait"), "Trait to visualize", choices = NULL, multiple = FALSE),
                                                         shiny::plotOutput(ns("evaluationUnits")) ,
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

#' mtaExpApp Server Functions
#'
#' @noRd
mod_mtaExpApp_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    output$plotDataDependencies <- shiny::renderPlot({ dependencyPlot() })
    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })
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
    #################
    # show shinyWidgets until the user can use the module
    observeEvent(c(data(), input$version2Mta), {
      req(data())
      mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
      if(mappedColumns == 3 & length(input$version2Mta)>0 ){
        golem::invoke_js('showid', ns('holder3'))
        golem::invoke_js('showid', ns('holder4'))
        golem::invoke_js('showid', ns('holder5'))
        golem::invoke_js('showid', ns('holder6'))
      }else{
        golem::invoke_js('hideid', ns('holder3'))
        golem::invoke_js('hideid', ns('holder4'))
        golem::invoke_js('hideid', ns('holder5'))
        golem::invoke_js('hideid', ns('holder6'))
      }
    })
    #################
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
    ## left formula
    output$leftSides <- renderUI({
      req(data())
      req(input$version2Mta)
      req(input$trait2Mta)
      req(input$nTerms)
      dtMta <- data()
      gg <- cgiarBase::goodLevels(object=dtMta, analysisId=input$version2Mta)
      choices <- c( "1", gg[[ input$trait2Mta[1] ]])
      if(length(input$cs_model) > 0){
        if (input$cs_model) { # CS model
          lapply(1:input$nTerms, function(i) {
            selectInput(
              session$ns(paste0('leftSides',i)),
              label = ifelse(i==1, "Intercepts",""),
              choices = choices, multiple = TRUE, selected = "1"
            )
          })
        }else if(input$csdg_model | input$uns_model | input$dg_model ){ # CS+DIAG model
          lapply(1:input$nTerms, function(i) {
            selectInput(
              session$ns(paste0('leftSides',i)),
              label = ifelse(i==1, "Intercepts",""),
              choices = choices, multiple = TRUE, selected = ifelse(i==1,"1", ifelse(i==2, '1', 'environment' ))
            )
          })
        }else if(input$fw_model){ # FW model
          lapply(1:input$nTerms, function(i) {
            selectInput(
              session$ns(paste0('leftSides',i)),
              label = ifelse(i==1, "Intercepts",""),
              choices = choices, multiple = TRUE, selected = ifelse(i==1,"1", ifelse(i==3, '1', paste0("envIndex_",input$trait2Mta[1] ) ))
            )
          })
        }else if(input$fa_model){ # FW model
          lapply(1:input$nTerms, function(i) {
            selectInput(
              session$ns(paste0('leftSides',i)),
              label = ifelse(i==1, "Intercepts",""),
              choices = choices, multiple = TRUE, selected = ifelse(i==1, '1', 'environment'  )
            )
          })
        }else{ # no model specified
          shinyWidgets::updatePrettySwitch(session, "cs_model", value = FALSE)
          shinyWidgets::updatePrettySwitch(session, "dg_model", value = FALSE)
          shinyWidgets::updatePrettySwitch(session, "csdg_model", value = FALSE)
          shinyWidgets::updatePrettySwitch(session, "fw_model", value = FALSE)
          shinyWidgets::updatePrettySwitch(session, "uns_model", value = FALSE)
          shinyWidgets::updatePrettySwitch(session, "fa_model", value = FALSE)
          lapply(1:input$nTerms, function(i) {
            selectInput(
              session$ns(paste0('leftSides',i)),
              label = ifelse(i==1, "Intercepts",""),
              choices = choices, multiple = TRUE, selected = "1"
            )
          })
        }
      }

    })
    # covariance structure
    output$center <- renderUI({
      req(data())
      req(input$version2Mta)
      req(input$trait2Mta)
      req(input$nTerms)
      if(length(input$cs_model) > 0){
        if (input$cs_model | input$fw_model | input$uns_model) { # CS model
          lapply(1:input$nTerms, function(i) {
            selectInput(
              session$ns(paste0('center',i)),
              label = ifelse(i==1, "Structure",""),
              choices = list(' (none)'='','| (UNS)'='|','|| (DIAG)'='||'), multiple = FALSE,
              selected = '|'
            )
          })
        }else if(input$csdg_model | input$dg_model){ # CS+DIAG model
          lapply(1:input$nTerms, function(i) {
            selectInput(
              session$ns(paste0('center',i)),
              label = ifelse(i==1, "Structure",""),
              choices = list(' (none)'='','| (UNS)'='|','|| (DIAG)'='||'), multiple = FALSE,
              selected = ifelse(i==1, '|', ifelse(i==2, '|', '||'))
            )
          })
        }else if(input$fa_model ){ # CS+DIAG model
          lapply(1:input$nTerms, function(i) {
            selectInput(
              session$ns(paste0('center',i)),
              label = ifelse(i==1, "Structure",""),
              choices = list(' (none)'='','| (UNS)'='|','|| (DIAG)'='||'), multiple = FALSE,
              selected = ifelse(i==1, '|', ifelse(i==2, '||', '|'))
            )
          })
        }else{ # no model specified
          shinyWidgets::updatePrettySwitch(session, "cs_model", value = FALSE)
          shinyWidgets::updatePrettySwitch(session, "dg_model", value = FALSE)
          shinyWidgets::updatePrettySwitch(session, "csdg_model", value = FALSE)
          shinyWidgets::updatePrettySwitch(session, "fw_model", value = FALSE)
          shinyWidgets::updatePrettySwitch(session, "uns_model", value = FALSE)
          shinyWidgets::updatePrettySwitch(session, "fa_model", value = FALSE)
          lapply(1:input$nTerms, function(i) {
            selectInput(
              session$ns(paste0('center',i)),
              label = ifelse(i==1, "Structure",""),
              choices = list(' (none)'='','| (UNS)'='|','|| (DIAG)'='||'), multiple = FALSE,
              selected = '|'
            )
          })
          # updateSelectizeInput(session, 'qtl_table_markers', selected = '')
        }
      }

    })
    # right-side equation
    output$rightSides <- renderUI({
      req(data())
      req(input$version2Mta)
      req(input$trait2Mta)
      req(input$nTerms)
      dtMta <- data()
      gg <- cgiarBase::goodLevels(dtMta, input$version2Mta, includeCovars = FALSE)
      choices <- c("designation",gg[[ input$trait2Mta[1] ]])
      if(length(input$cs_model) > 0){
        if (input$cs_model) { # CS model
          lapply(1:input$nTerms, function(i) {
            selectInput(
              inputId=session$ns(paste0('rightSides',i)),
              label = ifelse(i==1, "Slopes",""),
              choices = choices, multiple = TRUE,
              selected = if(i==1){'environment'}else{if(i==2){list('designation')}else{if(i==3){list('environment','designation')}else{choices[i]} } }
            )
          })
        }else if(input$csdg_model | input$fw_model){
          lapply(1:input$nTerms, function(i) {
            selectInput(
              inputId=session$ns(paste0('rightSides',i)),
              label = ifelse(i==1, "Slopes",""),
              choices = choices, multiple = TRUE,
              selected = if(i==1){'environment'}else{if(i==2){list('designation')}else{if(i==3){list('designation')}else{choices[i]} } }
            )
          })
        }else if(input$uns_model | input$dg_model){
          lapply(1:input$nTerms, function(i) {
            selectInput(
              inputId=session$ns(paste0('rightSides',i)),
              label = ifelse(i==1, "Slopes",""),
              choices = choices, multiple = TRUE,
              selected = if(i==1){'environment'}else{if(i==2){ list(setdiff(choices,'designation')[1] )}else{if(i==3){list('designation')}else{choices[i]} } }
            )
          })
        }else if(input$fa_model){
          lapply(1:input$nTerms, function(i) {
            selectInput(
              inputId=session$ns(paste0('rightSides',i)),
              label = ifelse(i==1, "Slopes",""),
              choices = choices, multiple = TRUE,
              selected = if(i==1){'environment'}else{list('designation')}
            )
          })
        }else{ # no model specified
          shinyWidgets::updatePrettySwitch(session, "cs_model", value = FALSE)
          shinyWidgets::updatePrettySwitch(session, "dg_model", value = FALSE)
          shinyWidgets::updatePrettySwitch(session, "csdg_model", value = FALSE)
          shinyWidgets::updatePrettySwitch(session, "fw_model", value = FALSE)
          shinyWidgets::updatePrettySwitch(session, "uns_model", value = FALSE)
          shinyWidgets::updatePrettySwitch(session, "fa_model", value = FALSE)
          lapply(1:input$nTerms, function(i) {
            selectInput(
              inputId=session$ns(paste0('rightSides',i)),
              label = ifelse(i==1, "Slopes",""),
              choices = choices, multiple = TRUE,
              selected = choices[i]
            )
          })
          #
        }
      }

    })
    # n pricipal components
    output$nPC <- renderUI({
      req(data())
      req(input$version2Mta)
      req(input$trait2Mta)
      req(input$nTerms)
      if(length(input$fa_model) > 0){
        if(input$fa_model){
          lapply(1:input$nTerms, function(i) {
            numericInput(
              session$ns(paste0('nPC',i)),
              label = ifelse(i==1, "nPC",""),
              value = if(i==1){0}else{if(i==2){ 0 }else{if(i==3){2}else{0} } },
              min = 0, max = Inf, step = 1
            )
          })
        }else{ # no model specified
          shinyWidgets::updatePrettySwitch(session, "cs_model", value = FALSE)
          shinyWidgets::updatePrettySwitch(session, "csdg_model", value = FALSE)
          shinyWidgets::updatePrettySwitch(session, "fw_model", value = FALSE)
          shinyWidgets::updatePrettySwitch(session, "uns_model", value = FALSE)
          shinyWidgets::updatePrettySwitch(session, "fa_model", value = FALSE)
          lapply(1:input$nTerms, function(i) {
            numericInput(
              session$ns(paste0('nPC',i)),
              label = ifelse(i==1, "nPC",""),
              value = 0, min = 0, max = Inf, step = 1
            )
          })
          #
        }
      }

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
      traitsMta <- list(BLUP="blup",pBLUP="pblup",gBLUP="gblup",ssGBLUP="ssgblup",rrBLUP="rrblup")
      updateSelectInput(session, "modelMet", choices = traitsMta, selected = traitsMta[1])
    })
    #################
    ## render timestamps flow plot
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
    #################
    ## render table of evaluation units
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
        pedCols <- metaPed[which(metaPed$parameter %in% c("designation","mother","father")), "value"]
        pedCols <- setdiff(pedCols,"")
        pedNames <- na.omit(unique(unlist(as.vector(object$data$pedigree[,pedCols, drop=FALSE]))))
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
    #####################
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

    ###################################
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
            verbose=FALSE
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

      if(!inherits(result,"try-error")) { # if all goes well in the run
        ## predictions table
        output$predictionsMta <-  DT::renderDT({
          predictions <- result$predictions
          predictions <- predictions[predictions$module=="mtaFlex",]
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
            mtas <- result$status[which(result$status$module == "mtaFlex"),"analysisId"]; mtaId <- mtas[length(mtas)]
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
            mtas <- result$status[which(result$status$module == "mtaFlex"),"analysisId"]; mtaId <- mtas[length(mtas)]
            modeling <- modeling[which(modeling$analysisId == mtaId),]
            modeling <- subset(modeling, select = -c(module,analysisId))
            DT::datatable(modeling, extensions = 'Buttons',
                          options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                         lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
            )
          }
        })
        ## Report tab
        output$reportMta <- renderUI({
          HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportMtaFlex.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
          # HTML(markdown::markdownToHTML(knitr::knit("./R/reportMta.Rmd", quiet = TRUE), fragment.only=TRUE))
        })

        output$downloadReportMta <- downloadHandler(
          filename = function() {
            paste('my-report', sep = '.', switch(
              "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
            ))
          },
          content = function(file) {
            src <- normalizePath(system.file("rmd","reportMtaFlex.Rmd",package="bioflow"))
            src2 <- normalizePath('data/resultMtaFlex.RData')
            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'report.Rmd', overwrite = TRUE)
            file.copy(src2, 'resultMtaFlex.RData', overwrite = TRUE)
            out <- rmarkdown::render('report.Rmd', params = list(toDownload=TRUE),switch(
              "HTML",
              HTML = rmdformats::robobook(toc_depth = 4)
              # HTML = rmarkdown::html_document()
            ))
            file.rename(out, file)
          }
        )

      } else {
        output$predictionsMta <- DT::renderDT({DT::datatable(NULL)})
        output$metricsMta <- DT::renderDT({DT::datatable(NULL)})
        output$modelingMta <- DT::renderDT({DT::datatable(NULL)})
        hideAll$clearAll <- TRUE
      } ### enf of if(!inherits(result,"try-error"))

      hideAll$clearAll <- FALSE

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

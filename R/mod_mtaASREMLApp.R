#' mtaASREMLApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mtaASREMLApp_ui <- function(id) {
  ns <- NS(id)
  tagList(


    # color guide for backgrounds: https://www.w3schools.com/colors/colors_names.asp
    # color guide for fonts: https://www.w3schools.com/html/html_colors_hex.asp
    tags$br(),

    mainPanel(width = 12,
              tabsetPanel( id=ns("tabsMain"),
                           type = "tabs",
                           tabPanel(div(icon("book"), "Information") ,
                                    br(),
                                    column(width = 6,
                                           h1(strong(span("Multi Trial Analysis Module Using Asreml-R",
                                                          #tags$a(href="https://www.youtube.com/watch?v=rR1DhTt25n4&list=PLZ0lafzH_UmclOPifjCntlMzysEB2_2wX&index=7",icon("youtube") , target="_blank"),
                                                          style="color:darkcyan"))),
                                           h2(strong("Data Status (wait to be displayed):")),
                                           uiOutput(ns("warningMessage")),
                                           tags$br(),
                                           # column(width=4, tags$br(),
                                           shinyWidgets::prettySwitch( inputId = ns('launch'), label = "Load example dataset", status = "success"),
                                           # ),
                                           tags$br(),
                                           img(src = "www/mta.png", height = 375, width = 450), # add an image
                                    ),
                                    column(width = 6,
                                           h2(strong("Details")),
                                           p("The core algorithm of the genetic evaluation using the two-step approach is the multi-trial analysis using Asreml-R,
                                           for use it you must have and ACTIVE LICENSE.

                                          This option aims to model breeding values across environments using the results from the single trial (weighted by the standard errors)
                              analysis and optionally a relationship matrix between levels of the random effects. This module allows the flexibility to build your own customized module by specifying the
                              random effects and any relationship . In addition, the most popular GxE models can be selected with a single click to help the user understand how a
                              specific model could be specified.
                                The way the arguments are used is the following:"),

                                           p(strong("Asreml license code.-")," Provide your code for activate the license."),
                                           p(strong("Traits to analyze.-")," Traits to be analyzed."),
                                           p(strong("Fixed effects.-")," Variables to be fitted as fixed effects."),
                                           p(strong("Random effects.-")," Variables to be fitted as random effects."),
                                           p(strong("Additional settings:")),
                                           p(strong("H2(lower bound).-")," Value of H2 to be used to remove trials with low heritability."),
                                           p(strong("H2(upper bound).-"),"  Value of H2 to be used to remove trials with too high heritability."),
                                           p(strong("mean(lower bound).-")," Value of trait mean to be used to remove trials with low means"),
                                           p(strong("mean(upper bound).-"),"  Value of trait mean to be used to remove trials with too high means"),
                                           p(strong("Number of iterations.-")," Maximum number of restricted maximum likelihood iterations to be run for each trait."),
                                           #p(strong("Use weights.-")," a TRUE/FALSE statement indicating if the analysis should be weighted using the standard errors from the single trial analysis. The default is TRUE and should not be modified unless you know what you are doing."),
                                           h2(strong("References")),
                                           p("Butler, D.G., Cullis, B.R., Gilmour, A.R., Gogel, B.G. and Thompson, R. 2023. ASReml-R Reference Manual Version 4.2. VSN International Ltd., Hemel Hempstead, HP2 4TP, UK."),
                                           h2(strong("Software used")),
                                           p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
                                Vienna, Austria. URL https://www.R-project.org/."),
                                           p("Butler, D.G., Cullis, B.R., Gilmour, A.R., Gogel, B.G. and Thompson, R. (2023). ASReml-R: estimates variance components under a general linear mixed model by residual maximum likelihood (REML)"),
                                    ),
                           ),
                           tabPanel(div(icon("arrow-right-to-bracket"), "Input steps"),
                                    tabsetPanel(
                                      tabPanel(div( icon("dice-one"), "ASReml license", icon("arrow-right") ), # icon = icon("dice-one"),
                                               br(),
                                               column(width=12),
                                               shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = FALSE, collapsed = FALSE, title = "Check status of license",
                                                                   column(width=12,
                                                                          hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                          h5(strong(span("Status (wait to be displayed):", style="color:green"))),
                                                                          hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                          uiOutput(ns("statuslicenseasr")),
                                                                   )
                                               ),
                                      ),
                                      tabPanel(div( icon("dice-one"), "Pick STA-stamp(s)", icon("arrow-right") ), # icon = icon("dice-one"),
                                               br(),
                                               column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                      column(width=8,
                                                             selectInput(ns("version2MtaAsr"),
                                                                         label = tags$span(
                                                                           "STA version(s) to analyze",
                                                                           tags$i(
                                                                             class = "glyphicon glyphicon-info-sign",
                                                                             style = "color:#FFFFFF",
                                                                             title = "Analysis ID(s) from STA runs that should be combined and used to fit a multi-trial analysis."
                                                                           )
                                                                         ),
                                                                         choices = NULL, multiple = TRUE),
                                                             selectInput(ns("version2MtaAsrGeno"),
                                                                         label = tags$span(
                                                                           "Genotype QA/QC version(s) to analyze",
                                                                           tags$i(
                                                                             class = "glyphicon glyphicon-info-sign",
                                                                             style = "color:#FFFFFF",
                                                                             title = "Analysis ID(s) from Genotype QA/QC used to fit a multi-trial analysis."
                                                                           )
                                                                         ),
                                                                         choices = NULL, multiple = FALSE),
                                                      ),
                                               ),
                                               column(width=12),
                                               shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                   column(width=12,
                                                                          hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                          h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameter values to be specified in the grey boxes above.", style="color:green"))),
                                                                          hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                   ),
                                                                   column( width=12, shiny::plotOutput(ns("plotTimeStamps")) ),
                                                                   DT::DTOutput(ns("statusMtaAsr")), # modeling table
                                               ),
                                      ),
                                      tabPanel(div(icon("dice-two"), "Pick trait(s)", icon("arrow-right") ),
                                               br(),
                                               column(width=12,style = "background-color:grey; color: #FFFFFF",
                                                      selectInput(ns("trait2MtaAsr"),
                                                                  label = tags$span(
                                                                    "Trait(s) to analyze",
                                                                    tags$i(
                                                                      class = "glyphicon glyphicon-info-sign",
                                                                      style = "color:#FFFFFF",
                                                                      title = "Only traits present in the STA runs selected will be available."
                                                                    )
                                                                  ),
                                                                  choices = NULL, multiple = TRUE),
                                               ),
                                               shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                   column(width=12,
                                                                          hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                          h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameters to be specified in the grey boxes above.", style="color:green"))),
                                                                          hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                   ),
                                                                   column(width=12,
                                                                          column(width=5, selectInput(ns("traitMetrics"), "Trait to visualize", choices = NULL, multiple = TRUE) ),
                                                                          column(width=5, selectInput(ns("parameterMetrics"), "Parameter to visualize", choices = NULL, multiple = FALSE) ),
                                                                          column(width=2, checkboxInput(ns("checkbox1"), label = "Include x-axis labels", value = TRUE) ),
                                                                          column(width=12, plotly::plotlyOutput(ns("barplotPredictionsMetrics")) ),
                                                                   ),
                                               ),
                                      ),
                                      tabPanel(div(icon("dice-three"), "Form your model", icon("arrow-right") ), # icon = icon("dice-two"),
                                               br(),
                                               column(width=12, style = "background-color:DarkGray; color: #FFFFFF",
                                                      column(width=12,
                                                             column(width=3,
                                                                    numericInput(ns("nTermsFixed"),
                                                                                 label = tags$span(
                                                                                   "nTerms fixed",
                                                                                   tags$i(
                                                                                     class = "glyphicon glyphicon-info-sign",
                                                                                     style = "color:#FFFFFF",
                                                                                     title = "Number of fixed effect to specify in your model. If more than one factor is selected in a white box this is considered to be an interaction."
                                                                                   )
                                                                                 ),
                                                                                 value = NULL), # , step = 1, min = 1, max=10
                                                                    uiOutput(ns("leftSidesFixed"))
                                                             ),
                                                             column(width=3,
                                                                    numericInput(ns("nTermsRandom"),
                                                                                 label = tags$span(
                                                                                   "nTerms random",
                                                                                   tags$i(
                                                                                     class = "glyphicon glyphicon-info-sign",
                                                                                     style = "color:#FFFFFF",
                                                                                     title = "Number of random effect to specify in your model. If more than one factor is selected in a white box this is considered to be an interaction."
                                                                                   )
                                                                                 ),
                                                                                 value = NULL), #
                                                                    uiOutput(ns("leftSidesRandom"))
                                                             ),
                                                             br(),
                                                             br(),
                                                             br(),
                                                             br(),
                                                             column(width=3,
                                                                uiOutput(ns("rightSidesRandom"))
                                                             ),
                                                             column(width=3,
                                                                uiOutput(ns("nFATerm"))
                                                             ),

                                                      ),
                                               ),
                                               column(width=12,
                                               shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Visual aid (click on the '+' symbol on the right to open)",
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
                                                                                    selectInput(ns("entryTypeMtaAsr"), "Entry type to visualize", choices = NULL, multiple = TRUE),
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
                                      ),),#end form your model
                                      tabPanel(div(icon("dice-four"), "Other options", icon("arrow-right") ),
                                               br(),
                                               column(width=12, style = "background-color:DarkGray; color: #FFFFFF",
                                                      br(),
                                                      shinydashboard::box(width = 12,style = "color: #000000", status = "success", solidHeader=FALSE,collapsible = TRUE, collapsed = TRUE, title = "Fields to exclude (optional)...",
                                                                          p(span("Fields to exclude in the analysis (double click in the cell and set to zero if you would like to ignore an environment for a given trait).", style="color:black")),
                                                                          DT::dataTableOutput(ns("fieldsMet")),
                                                      ),
                                               ),
                                               column(width=6, style = "background-color:LightGray; color: #FFFFFF",
                                                      br(),
                                                      shinydashboard::box(width = 12, style = "color: #000000", status = "success", solidHeader=FALSE,collapsible = TRUE, collapsed = TRUE, title = "Additional model settings...",
                                                                          checkboxInput(ns("gscamtaAsr"),
                                                                                        label = tags$span(
                                                                                          "Add to the results GCA/SCA",
                                                                                          tags$i(
                                                                                            class = "glyphicon glyphicon-info-sign",
                                                                                            style = "color:#000000",
                                                                                            title = "If is the case you can calculate directly the GCA for parents, line or tester and/or SCA for hybrids."
                                                                                          )
                                                                                        ),value=FALSE),
                                                                          textInput(ns("heritLBMet"),
                                                                                    label = tags$span(
                                                                                      "Lower H2 and R2 bound",
                                                                                      tags$i(
                                                                                        class = "glyphicon glyphicon-info-sign",
                                                                                        style = "color:#000000",
                                                                                        title = "If a single value of heritability and reliability is specified it will be used across all traits. If a different value is required per trait please separate the values by comma."
                                                                                      )
                                                                                    ),
                                                                                    value="0.1"),
                                                                          textInput(ns("heritUBMet"),
                                                                                    label = tags$span(
                                                                                      "Upper H2 and R2 bound",
                                                                                      tags$i(
                                                                                        class = "glyphicon glyphicon-info-sign",
                                                                                        style = "color:#000000",
                                                                                        title = "If a single value of heritability and reliability is specified it will be used across all traits. If a different value is required per trait please separate the values by comma."
                                                                                      )
                                                                                    ),
                                                                                    value="0.95"),
                                                                          textInput(ns("meanLBMet"),
                                                                                    label = tags$span(
                                                                                      "Lower environment-mean bound",
                                                                                      tags$i(
                                                                                        class = "glyphicon glyphicon-info-sign",
                                                                                        style = "color:#000000",
                                                                                        title = "If a single value for trait means is specified it will be used across all traits. If a different value is required per trait please separate the values by comma."
                                                                                      )
                                                                                    ),
                                                                                    value="0"),
                                                                          textInput(ns("meanUBMet"),
                                                                                    label = tags$span(
                                                                                      "Upper environment-mean bound",
                                                                                      tags$i(
                                                                                        class = "glyphicon glyphicon-info-sign",
                                                                                        style = "color:#000000",
                                                                                        title = "If a single value for trait means is specified it will be used across all traits. If a different value is required per trait please separate the values by comma."
                                                                                      )
                                                                                    ),
                                                                                    value="1000000"),
                                                                          numericInput(ns("maxitMet"),
                                                                                       label = tags$span(
                                                                                         "Maximum # of iterations",
                                                                                         tags$i(
                                                                                           class = "glyphicon glyphicon-info-sign",
                                                                                           style = "color:#000000",
                                                                                           title = "Restricted Maximum Likelihood iterations. If the model doesn't converge you will see that in the metrics output table."
                                                                                         )
                                                                                       ),
                                                                                       value = 35),
                                                      ),
                                               ),
                                               column(width = 6, style = "background-color:LightGray; color: #FFFFFF",
                                                      br(),
                                                      shinydashboard::box(width = 12, status = "success", solidHeader=FALSE,collapsible = TRUE, collapsed = TRUE, title = "Alternative response distributions...",
                                                                          p(span("The Normal distribution is assumed as default for all traits. If you wish to specify a different trait distribution for a given trait double click in the cell corresponding for the trait by distribution combination and make it a '1'.", style="color:black")),
                                                                          DT::DTOutput(ns("traitDistMet")),
                                                      ),
                                               ),
                                      ),#end other options
                                      tabPanel("Run analysis", icon = icon("dice-five"),
                                               br(),
                                               column(width=12,style = "background-color:grey; color: #FFFFFF",
                                                      column(width=3, tags$div(textInput(ns("analysisIdName"), label = tags$span(
                                                        "Analysis Name (optional)", tags$i( class = "glyphicon glyphicon-info-sign", style = "color:#FFFFFF",
                                                                                            title = "An optional name for the analysis besides the timestamp if desired.") ), #width = "100%",
                                                        placeholder = "(optional name)") ) ),
                                                      column(width=3,
                                                             br(),
                                                             actionButton(ns("runMtaAsr"), "Run analysis", icon = icon("play-circle")),
                                                             uiOutput(ns("qaQcMtaAsrInfo")),
                                                             br(),
                                                      ),

                                               ),
                                               textOutput(ns("outMtaAsr")),
                                      ),#end run analysis
                                      )#end tabset input

                           ),#end input panel
                           tabPanel(div(icon("arrow-right-from-bracket"), "Output tabs" ) , value = "outputTabs",
                                    tabsetPanel(
                                      tabPanel("Dashboard", icon = icon("file-image"),
                                               br(),
                                               textOutput(ns("outMtaAsr2")),
                                               br(),
                                               downloadButton(ns("downloadReportMtaAsr"), "Download dashboard"),
                                               br(),
                                               uiOutput(ns('reportMtaAsr'))
                                      ),
                                      tabPanel("Predictions", icon = icon("table"),
                                               br(),
                                               DT::DTOutput(ns("predictionsMtaAsr")),
                                      ),
                                      tabPanel("Metrics", icon = icon("table"),
                                               br(),
                                               DT::DTOutput(ns("metricsMtaAsr")),
                                      ),
                                      tabPanel("Modeling", icon = icon("table"),
                                               br(),
                                               DT::DTOutput(ns("modelingMtaAsr")),
                                      ),

                                    ) # end of tabset
                           )# end of output panel
              )) # end mainpanel

    )
}

#' mtaASREMLApp Server Functions
#'
#' @noRd
mod_mtaASREMLApp_server <- function(id, data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    # output$plotDataDependencies <- shiny::renderPlot({ dependencyPlot() })
    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })


    ################
    ##ASReml license status
    output$statuslicenseasr <- renderUI(
      #check internet connection for activate license
      if(curl::has_internet()){
        # check library(asreml)
        if("asreml"%in%rownames(installed.packages())==TRUE){
          library(asreml)
          csasr=asreml.license.status(quiet = FALSE, task = "checkout", json = "")
          if(csasr$statusMessage=="No error"){
            HTML( as.character(div(style="color: green; font-size: 20px;", "Correct license")) )
          }else{
            HTML( as.character(div(style="color: red; font-size: 20px;", "Please write the activation code of license in the R console")) )
            asreml.license.activate()
          }
        }else{
          HTML( as.character(div(style="color: red; font-size: 20px;", "Please make sure that you have library asreml installed")) )
        }
      }else{
        HTML( as.character(div(style="color: red; font-size: 20px;", "Please make sure that you have internet connection to check asreml library")) )
      }

    )
    #################
    ## version
    observeEvent(data(), {
      req(data())
      dtMtaAsr <- data() # dtMtaAsr <- result
      dtMtaAsr <- dtMtaAsr$status
      dtMtaAsrGeno <- dtMtaAsr[which(dtMtaAsr$module == "qaGeno"),]
      dtMtaAsr <- dtMtaAsr[which(dtMtaAsr$module == "sta"),]
      traitsMtaAsr <- unique(dtMtaAsr$analysisId)
      traitsMtaAsrGeno <- unique(dtMtaAsrGeno$analysisId)

      if(length(traitsMtaAsrGeno)==0){traitsMtaAsrGeno="No data available"}

      if(length(traitsMtaAsr) > 0){
        if("analysisIdName" %in% colnames(dtMtaAsr)){
          names(traitsMtaAsr) <- paste(dtMtaAsr$analysisIdName, as.POSIXct(traitsMtaAsr, origin="1970-01-01", tz="GMT"), sep = "_")
        }else{
          names(traitsMtaAsr) <- as.character(as.POSIXct(traitsMtaAsr, origin="1970-01-01", tz="GMT"))
        }
      }
      updateSelectInput(session, "version2MtaAsr", choices = traitsMtaAsr)
      updateSelectInput(session, "version2MtaAsrGeno", choices = traitsMtaAsrGeno)
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
        tmp <- data()
        data(cgiarBase::create_getData_object())
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
    # show shinyWidgets until the user can use the module (after sta)
    observeEvent(c(data(), input$version2MtaAsr), {
      req(data())
      mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
      if(mappedColumns == 3 & length(input$version2MtaAsr)>0 ){
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
    ## traits available
    observeEvent(c(data(), input$version2MtaAsr), {
      req(data())
      req(input$version2MtaAsr)
      dtMtaAsr <- data()
      dtMtaAsr <- dtMtaAsr$predictions
      dtMtaAsr <- dtMtaAsr[which(dtMtaAsr$analysisId %in% input$version2MtaAsr),]
      traitsMtaAsr <- unique(dtMtaAsr$trait)
      updateSelectInput(session, "trait2MtaAsr", choices = traitsMtaAsr)
      updateSelectInput(session, "traitCor", choices = traitsMtaAsr)
      updateSelectInput(session, "traitConnect", choices = traitsMtaAsr)
      updateSelectInput(session, "evaluationUnitsTrait", choices = traitsMtaAsr)
      updateSelectInput(session, "traitMetrics", choices = traitsMtaAsr, selected = traitsMtaAsr)
    })
    observeEvent(c(data(),input$version2MtaAsr), { # update parameter
      req(data())
      req(input$version2MtaAsr)
      dtMtaAsr <- data()
      dtMtaAsr <- dtMtaAsr$metrics
      dtMtaAsr <- dtMtaAsr[which(dtMtaAsr$analysisId %in% input$version2MtaAsr),] # only traits that have been QA
      metricsMtaAsrInput <- unique(dtMtaAsr$parameter)
      updateSelectInput(session, "parameterMetrics", choices = metricsMtaAsrInput)
    })
    #################
    # plots
    output$barplotPredictionsMetrics <- plotly::renderPlotly({
      req(data())
      req(input$version2MtaAsr)
      dtMtaAsr <- data()
      mydata <- dtMtaAsr$metrics
      mydata <- mydata[which(mydata$analysisId %in% input$version2MtaAsr),]
      mydata = mydata[which(mydata$parameter %in% input$parameterMetrics),]
      mydata = mydata[which(mydata$trait %in% input$traitMetrics),]
      p <- ggplot2::ggplot(data=mydata, ggplot2::aes(x=environment, y=value, fill=trait)) +
        ggplot2::geom_bar(stat="identity", position=ggplot2::position_dodge())+
        ggplot2::theme_minimal()+  ggplot2::ggtitle("Metrics associated to the STA stamp selected")
      if(input$checkbox1){
        p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1))
      }else{
        p <- p + ggplot2::theme(axis.text.x = ggplot2::element_blank())
      }
      plotly::ggplotly(p)
    })


    #################
    ## nTermsFixed
    observeEvent(c(data(), input$version2MtaAsr), {
      req(data())
      req(input$version2MtaAsr)
      dtMta <- data()
      dtMta <- dtMta$predictions
      dtMta <- dtMta[which(dtMta$analysisId %in% input$version2MtaAsr),]
      envs <- unique(dtMta[,"environment"])
      envsDg <- paste0("env",envs)
      updateNumericInput(session, "nTermsFixed", value = 1, step = 1, min = 1, max=10)
    })
    #################
    ## nTermsRandom
    observeEvent(c(data(), input$version2MtaAsr), {
      req(data())
      req(input$version2MtaAsr)
      dtMta <- data()
      dtMta <- dtMta$predictions
      dtMta <- dtMta[which(dtMta$analysisId %in% input$version2MtaAsr),]
      envs <- unique(dtMta[,"environment"])
      envsDg <- paste0("env",envs)
      updateNumericInput(session, "nTermsRandom", value = 2, step = 1, min = 1, max=10)
    })
    ## fixed effects
    output$leftSidesFixed <- renderUI({ # input <- list(version2Mta=result$status$analysisId[3], trait2Mta="YLD_TON",nTermsFixed=1)
      req(data())
      req(input$version2MtaAsr)
      req(input$trait2MtaAsr)
      req(input$nTermsFixed)
      dtMta <- data() # dtMta <- result
      mydata <- dtMta$predictions #
      mydata <- mydata[which(mydata$analysisId %in% input$version2MtaAsr),]
      metaPheno <- dtMta$metadata$pheno[which(dtMta$metadata$pheno$parameter %in% c("pipeline","stage","environment","year","season","timepoint","country","location","trial","study","management")),]
      otherMetaCols <- unique(dtMta$data$pheno[,metaPheno$value,drop=FALSE])
      colnames(otherMetaCols) <- cgiarBase::replaceValues(Source = colnames(otherMetaCols), Search = metaPheno$value, Replace = metaPheno$parameter )
      otherMetaCols <- otherMetaCols[which(!duplicated(otherMetaCols[,"environment"])),,drop=FALSE] # we do this in case the users didn't define the environment properly
      mydata <- merge(mydata, otherMetaCols, by="environment", all.x = TRUE)
      WeatherRow <- as.data.frame(cgiarPipeline::summaryWeather(object=dtMta, wide=TRUE)); WeatherRow$environment <- rownames(WeatherRow)
      mydata <- merge(mydata, WeatherRow, by="environment", all.x = TRUE)

      choices <- setdiff(colnames(mydata), c("predictedValue","stdError","reliability","analysisId","module") )
      envs <- unique(mydata[,"environment"])
      envs <- gsub(" ", "",envs )
      envsDg <- paste0("env",envs)
      lapply(1:input$nTermsFixed, function(i) {
          selectInput(
            session$ns(paste0('leftSidesFixed',i)),
            label = ifelse(i==1, "Fixed Effects",""),
            choices = choices, multiple = TRUE, selected = "environment"
          )
        })
    })
    ## left formula (actual effects) ## input <- list(version2Mta=result$status$analysisId[2])
    output$leftSidesRandom <- renderUI({
      req(data())
      req(input$version2MtaAsr)
      req(input$trait2MtaAsr)
      req(input$nTermsRandom)
      dtMta <- data()
      mydata <- dtMta$predictions #
      mydata <- mydata[which(mydata$analysisId %in% input$version2MtaAsr),]
      # choices
      metaPheno <- dtMta$metadata$pheno[which(dtMta$metadata$pheno$parameter %in% c("pipeline","stage","environment","year","season","timepoint","country","location","trial","study","management")),]
      otherMetaCols <- unique(dtMta$data$pheno[,metaPheno$value,drop=FALSE])
      colnames(otherMetaCols) <- cgiarBase::replaceValues(Source = colnames(otherMetaCols), Search = metaPheno$value, Replace = metaPheno$parameter )
      otherMetaCols <- otherMetaCols[which(!duplicated(otherMetaCols[,"environment"])),,drop=FALSE] # we do this in case the users didn't define the environment properly
      mydata <- merge(mydata, otherMetaCols, by="environment", all.x = TRUE)
      WeatherRow <- as.data.frame(cgiarPipeline::summaryWeather(dtMta, wide=TRUE)); WeatherRow$environment <- rownames(WeatherRow)
      mydata <- merge(mydata, WeatherRow, by="environment", all.x = TRUE)
      choices <- c( setdiff( setdiff(colnames(mydata),"designation"), c("predictedValue","stdError","reliability","analysisId","module") ), "designation")
      fwvars <- colnames(WeatherRow)[grep("envIndex",colnames(WeatherRow))]
      # selected
      envs <- unique(mydata[,"environment"])
      envs <- gsub(" ", "",envs )
      envsDg <- paste0("env",envs)
      envsDg2 <- paste0(rep("environment",10),"")
      desDg <-rep("designation",length(envsDg))
        lapply(1:input$nTermsRandom, function(i) {
          selectInput(
            session$ns(paste0('leftSidesRandom',i)),
            label = ifelse(i==1, "Random Effects",""),
            choices = choices, multiple = TRUE,
            selected = if(i==1){"designation"}else if(i==2){c( desDg[i], envsDg2[i] )}else{"designation"}
          )
        })
    })
    # right-side equation (Cov structure)
    output$rightSidesRandom <- renderUI({
      req(data())
      req(input$version2MtaAsr)
      req(input$trait2MtaAsr)
      req(input$nTermsRandom)
      mydata <- data()$predictions #
      mydata <- mydata[which(mydata$analysisId %in% input$version2MtaAsr),]

      if(input$version2MtaAsrGeno!="0" & all(is.na(data()$data$pedigree[,3]))!=T){#geno and pedrigree data
        choices <- c("Relationship structure_Geno","Relationship structure_Pedigree","Relationship structure_GenoAD","Structure model_fa","Structure model_diag","Structure model_us")
      }
      if(input$version2MtaAsrGeno!="0" & all(is.na(data()$data$pedigree[,3]))==T){#geno and NO pedrigree data
        choices <- c("Relationship structure_Geno","Relationship structure_GenoAD","Structure model_fa","Structure model_diag","Structure model_us")
      }
      if(input$version2MtaAsrGeno=="0" & all(is.na(data()$data$pedigree[,3]))!=T){#NO geno and pedrigree data
        choices <- c("Relationship structure_Pedigree","Structure model_fa","Structure model_diag","Structure model_us")
      }
      if(input$version2MtaAsrGeno=="0" & all(is.na(data()$data$pedigree[,3]))==T){#NO geno and NO pedrigree data
        choices <- c("Structure model_fa","Structure model_diag","Structure model_us")
      }

        lapply(1:input$nTermsRandom, function(i) {
          tempval <- reactive({paste0('input$','leftSidesRandom',i)})
          tempval <- length(eval( parse(text = tempval() ) ))
          #choices <- c("Relationship structure_Geno","Relationship structure_Pedigree","Relationship structure_GenoAD","Structure model_fa","Structure model_diag","Structure model_us")
          noness <- c("none","none.","none..","none...")

          if (i==1){
            choices<-c("none",choices)
            selectInput(
              inputId=session$ns(paste0('rightSidesRandom',i)),
              label = tags$span("Covariance of random effect based on:",tags$i(class = "glyphicon glyphicon-info-sign",style = "color:#FFFFFF",title = "Select one relationship or structure model for each random effect in a white box.")),
              choices = choices, multiple = TRUE,
              selected = choices[1])
          }else{
            choices<-c(noness[1:tempval],choices)
            selectInput(
              inputId=session$ns(paste0('rightSidesRandom',i)),
              label = "",
              choices = choices, multiple = TRUE,
              selected = noness[1:tempval])
          }
        })
    })
    # inputFormula summarizing the fixed effects
    inputFormulaFixed = reactive({
      req(data());  req(input$version2MtaAsr);  req(input$trait2MtaAsr); req(input$nTermsFixed)
      if (length(input$trait2MtaAsr) != 0) {
        values <- vector(mode = "list", length = input$nTermsFixed)
        for (i in 1:input$nTermsFixed) {
          tempval <- reactive({paste0('input$','leftSidesFixed',i)})
          s1 <- eval( parse(text = tempval() ) )
          values[[i]] <- s1
        }
        return(values)
      }
    })
    # inputFormula summarizing the random effects
    inputFormulaRandom = reactive({
      req(data());  req(input$version2MtaAsr);  req(input$trait2MtaAsr); req(input$nTermsRandom)
      if (length(input$trait2MtaAsr) != 0) {
        values <- vector(mode = "list", length = input$nTermsRandom)
        for (i in 1:input$nTermsRandom) {
          tempval <- reactive({paste0('input$','leftSidesRandom',i)})
          s1 <- eval( parse(text = tempval() ) )
          values[[i]] <- s1
        }
        return(values)
      }
    })
    # inputFormula summarizing the covariates
    inputFormulaCovars = reactive({
      req(data());  req(input$version2MtaAsr);  req(input$trait2MtaAsr); req(input$nTermsRandom)
      if (length(input$trait2MtaAsr) != 0) {
        values <- vector(mode = "list", length = input$nTermsRandom)
        for (i in 1:input$nTermsRandom) {
          tempval <- reactive({paste0('input$','rightSidesRandom',i)})
          s1 <- eval( parse(text = tempval() ) )
          values[[i]] <- s1
        }
        return(values)
      }
    })
    ####number of factor analytic
    output$nFATerm <- renderUI({
      req(data())
      req(input$version2MtaAsr)
      req(input$trait2MtaAsr)
      req(input$nTermsRandom)
      #req(input$rightSidesRandom)
      dtMtaAsr <- data() # dtMtaAsr <- result
      dtMtaAsr <- dtMtaAsr$predictions #
      dtMtaAsr <- dtMtaAsr[which(dtMtaAsr$analysisId %in% input$version2MtaAsr),]

      lapply(1:input$nTermsRandom, function(i) {
        tempval <- reactive({paste0('input$','rightSidesRandom',i)})
        s1 <- eval( parse(text = tempval() ) )
        if("Structure model_fa"%in%s1){
          tempval2 <- reactive({paste0('input$','leftSidesRandom',i)})
          s2 <- eval( parse(text = tempval2() ) )
          s2 <- s2[which("Structure model_fa"%in%s1)]
          envs <- unique(dtMtaAsr[,s2])
          n2 <- length(envs)-1
          if(i==1){
            numericInput(
              inputId=session$ns(paste0('nFATerm',i)),
              label = tags$span("FA term",tags$i(class = "glyphicon glyphicon-info-sign",style = "color:#FFFFFF",title = "Number of factor analytic term in the Structure model_fa.")),
              value = 1,step=1,min=1,max=n2)
          }else{
            numericInput(
              inputId=session$ns(paste0('nFATerm',i)),
              label = "",
              value = 1,step=1,min=1,max=n2)
          }
        }else{
          if(i==1){
          numericInput(
            inputId=session$ns(paste0('nFATerm',i)),
            label = tags$span("FA term",tags$i(class = "glyphicon glyphicon-info-sign",style = "color:#FFFFFF",title = "Number of factor analytic term in the Structure model_fa.")),
            value = 0,step=0,min=0,max=0)
          }else{
            numericInput(
              inputId=session$ns(paste0('nFATerm',i)),
              label = "",
              value = 0,step=0,min=0,max=0)
          }
        }

      })

    })
    # inputFormula summarizing the nFATerm
    inputFormulanFATerm = reactive({
      req(data());  req(input$version2MtaAsr);  req(input$trait2MtaAsr); req(input$nTermsRandom)
      if (length(input$trait2MtaAsr) != 0) {
        values <- vector(mode = "list", length = input$nTermsRandom)
        for (i in 1:input$nTermsRandom) {
          tempval <- reactive({paste0('input$','nFATerm',i)})
          s1 <- eval( parse(text = tempval() ) )
          values[[i]] <- s1
        }
        return(values)
      }
    })
    #################
    # reactive table for trait family distributions
    dtDistTrait = reactive({
      traitNames = input$trait2MtaAsr
      mm = matrix(0,nrow = 4, ncol = length(traitNames));
      rownames(mm) <- c(
        "asr_gaussian(link = 'identity')", "asr_binomial(link = 'logit')",  "asr_Gamma(link = 'inverse')",
        "asr_poisson(link = 'log')"
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
                                       server = FALSE,
                                       options = list(
                                         scrollX = TRUE
                                         # autoWidthOpt = TRUE, scrollXOpt = TRUE
                                         # paging=FALSE,
                                         #              searching=FALSE,
                                         #              initComplete = I("function(settings, json) {alert('Done.');}")
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

    #################
    # reactive table for environments to be included
    dtFieldMet = reactive({
      req(data())
      req(input$version2MtaAsr)
      dtMtaAsr <- data()
      dtProv = dtMtaAsr$predictions
      if(!is.null(dtProv)){
        dtProv <- dtProv[which(dtProv$analysisId %in% input$version2MtaAsr),]
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
    output$fieldsMet = DT::renderDT(x$df, selection = 'none', editable = TRUE, server = FALSE)
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
    ## render timestamps flow plot
    output$plotTimeStamps <- shiny::renderPlot({
      req(data()) # req(input$version2Sta)
      xx <- data()$status;  yy <- data()$modeling # xx <- result$status;  yy <- result$modeling
      if("analysisIdName" %in% colnames(xx)){existNames=TRUE}else{existNames=FALSE}
      if(existNames){
        networkNames <- paste(xx$analysisIdName, as.character(as.POSIXct(as.numeric(xx$analysisId), origin="1970-01-01", tz="GMT")),sep = "_" )
        xx$analysisIdName <- as.character(as.POSIXct(as.numeric(xx$analysisId), origin="1970-01-01", tz="GMT"))
      }
      v <- which(yy$parameter == "analysisId")
      if(length(v) > 0){
        yy <- yy[v,c("analysisId","value")]
        zz <- merge(xx,yy, by="analysisId", all.x = TRUE)
      }else{ zz <- xx; zz$value <- NA}
      if(existNames){
        zz$analysisIdName <- cgiarBase::replaceValues(Source = zz$analysisIdName, Search = "", Replace = "?")
        zz$analysisIdName2 <- cgiarBase::replaceValues(Source = zz$value, Search = zz$analysisId, Replace = zz$analysisIdName)
      }
      if(!is.null(xx)){
        if(existNames){
          colnames(zz) <- cgiarBase::replaceValues(colnames(zz), Search = c("analysisIdName","analysisIdName2"), Replace = c("outputId","inputId") )
        }else{
          colnames(zz) <- cgiarBase::replaceValues(colnames(zz), Search = c("analysisId","value"), Replace = c("outputId","inputId") )
        }
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
        };
        rownames(X) <- networkNames
        colnames(X) <- networkNames
        if(existNames){

        }else{
          rownames(X) <-as.character(as.POSIXct(as.numeric(rownames(X)), origin="1970-01-01", tz="GMT"))
          colnames(X) <-as.character(as.POSIXct(as.numeric(colnames(X)), origin="1970-01-01", tz="GMT"))
        }
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
          ggnetwork::geom_nodes(ggplot2::aes(color = family), alpha = 0.5, size=5 ) +
          ggnetwork::geom_nodelabel_repel(ggplot2::aes(color = family, label = vertex.names ),
                                          fontface = "bold", box.padding = ggnetwork::unit(1, "lines")) +
          ggnetwork::theme_blank() + ggplot2::ggtitle("Network plot of current analyses available")
      }
    })
    #################
    ## render table of evaluation units
    output$evaluationUnits <-  shiny::renderPlot({ #DT::renderDT({
      req(data())
      req(input$version2MtaAsr)
      req(input$evaluationUnitsTrait)
      object <- data()
      if(!is.null(object$predictions)){
        phenoNames <- na.omit(unique(object$predictions[which(object$predictions$analysisId %in% input$version2MtaAsr  &  object$predictions$trait == input$evaluationUnitsTrait),"designation"]))
      }else{ phenoNames <- character() }

      if(!is.null(object$data$geno)){
        genoNames <- rownames(as.data.frame(object$data$geno))
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
    observeEvent(c(data(),input$version2MtaAsr), { # update entry types included in the plot
      req(data())
      req(input$version2MtaAsr)
      dtMtaAsr <- data()
      dtMtaAsr <- dtMtaAsr$predictions
      dtMtaAsr <- dtMtaAsr[which(dtMtaAsr$analysisId %in% input$version2MtaAsr),] # only traits that have been QA
      entryTypeMtaAsrInput <- unique(dtMtaAsr$entryType)
      updateSelectInput(session, "entryTypeMtaAsr", choices = c(entryTypeMtaAsrInput,"Generic"), selected = "Generic")
    })
    output$plotPredictionsSparsity <- shiny::renderPlot({
      req(data())
      req(input$version2MtaAsr)
      req(input$entryTypeMtaAsr)
      dtMtaAsr <- data()
      mydata <- dtMtaAsr$predictions
      mydata <- mydata[which(mydata$analysisId %in% input$version2MtaAsr),] # only PREDICTIONS FROM THE STA
      if(input$entryTypeMtaAsr != "Generic"){
        mydata <- mydata[which(mydata[,"entryType"] %in% input$entryTypeMtaAsr),]
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
      req(input$version2MtaAsr)
      req(input$traitCor)
      req(input$heatmapFontSizeCor)
      dtMtaAsr <- data()
      mydata <- dtMtaAsr$predictions
      mydata <- mydata[which(mydata$analysisId %in% input$version2MtaAsr),] # only PREDICTIONS FROM THE STA
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
    # render sparsity plot
    output$plotPredictionsConnectivity <-  shiny::renderPlot({ # plotly::renderPlotly({
      req(data())
      req(input$version2MtaAsr)
      req(input$traitConnect)
      req(input$entryTypeMtaAsr)
      req(input$heatmapFontSize)
      dtMtaAsr <- data()
      mydata <- dtMtaAsr$predictions # extract predictions
      mydata <- mydata[which(mydata$analysisId %in% input$version2MtaAsr),] # only PREDICTIONS FROM THE STA
      mydata <- mydata[which(mydata$trait %in% input$traitConnect),] # only PREDICTIONS FROM Trait selected
      if(input$entryTypeMtaAsr != "Generic"){ # use an specific type of entries
        mydata <- mydata[which(mydata[,"entryType"] %in% input$entryTypeMtaAsr),]
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
    ###################################
    ###################################
    ###################################
    ###################################
    ###################################
    ###################################
    ###################################
    ## render result of "run" button click
    outMtaAsr1 <- eventReactive(input$runMtaAsr, {
    #outMtaAsr <- eventReactive(input$runMtaAsr, {
      req(data())
      req(input$version2MtaAsr)
      req(input$trait2MtaAsr)
      req(input$nTermsFixed)
      req(input$nTermsRandom)

      shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
      dtMtaAsr <- data()
      # inputFormulationFixed <- inputFormulaRandom()
      # saveRDS(inputFormulationFixed, file = "inputFormulationFixed.rds")
      # run the modeling, but before test if sta was done
      if(sum(dtMtaAsr$status$module %in% "sta") == 0) {
        output$qaQcMtaAsrInfo <- renderUI({
          if (hideAll$clearAll)
            return()
          else
            req(dtMtaAsr)
          HTML(as.character(div(style="color: brown;",
                                "Please perform Single-Trial-Analysis before conducting a Multi-Trial Analysis when using a two-stage analysis."))
          )
        })
      }else{ # sta is available

        output$qaQcMtaAsrInfo <- renderUI({return(NULL)})

        # family distributions input
        myFamily = apply(xx$df,2,function(y){rownames(xx$df)[which(y > 0)[1]]})
        dontHaveDist <- which(is.na(myFamily))
        if(length(dontHaveDist) > 0){myFamily[dontHaveDist] <- "asr_gaussian(link = 'identity')"}

        myEnvsTI = apply(x$df,2,function(z){z})


        #analysisId=input$version2MtaAsr
        #fixedTerm= input$TermsFixed
        #randomTerm=input$TermsRandom
        #envsToInclude=myEnvsTI
        #trait= input$trait2MtaAsr
        #traitFamily=myFamily
        #useWeights=TRUE
        #modelo=input$radio
        #modeloG=input$radioModel
        #calculateSE=TRUE
        #heritLB= as.numeric(unlist(strsplit(input$heritLBMet,",")))
        #heritUB= as.numeric(unlist(strsplit(input$heritUBMet,",")))
        #meanLB = as.numeric(unlist(strsplit(input$meanLBMet,",")))
        #meanUB = as.numeric(unlist(strsplit(input$meanUBMet,",")))
        #maxIters=input$maxitMet
		#
        #save(dtMtaAsr,analysisId,fixedTerm, randomTerm, envsToInclude,trait, traitFamily, useWeights,modelo, modeloG,
        #     calculateSE, heritLB,  heritUB, meanLB, meanUB, maxIters,file="METasr.RData")
        #source("C:/Users/RAPACHECO/OneDrive - CIMMYT/Documents/CIMMYT/2025/BioflowTask/metASREML.R")
        result <- try(
          cgiarPipeline::metASREML(
          #metASREML(
            phenoDTfile= dtMtaAsr, analysisId=input$version2MtaAsr,analysisIdgeno=input$version2MtaAsrGeno,gsca=input$gscamtaAsr,
            fixedTerm= inputFormulaFixed(),  randomTerm=inputFormulaRandom(), covMod=inputFormulaCovars(), addG=inputFormulaCovars(),nFA=inputFormulanFATerm(),
            envsToInclude=myEnvsTI, trait= input$trait2MtaAsr, traitFamily=myFamily, useWeights=TRUE,
            calculateSE=TRUE, heritLB= as.numeric(unlist(strsplit(input$heritLBMet,","))),
            heritUB= as.numeric(unlist(strsplit(input$heritUBMet,","))),
            meanLB = as.numeric(unlist(strsplit(input$meanLBMet,","))),
            meanUB = as.numeric(unlist(strsplit(input$meanUBMet,","))),
            maxIters=input$maxitMet,  verbose=TRUE
          ),
          silent=TRUE
        )
        if(!inherits(result,"try-error") ) {
          if("analysisIdName" %in% colnames(result$status) ){result$status$analysisIdName[nrow(result$status)] <- input$analysisIdName}
          data(result) # update data with results
          cat(paste("Multi-trial analysis step with id:",as.POSIXct(result$status$analysisId[length(result$status$analysisId)], origin="1970-01-01", tz="GMT"),"saved. Please proceed to construct a selection index using this time stamp."))
          updateTabsetPanel(session, "tabsMain", selected = "outputTabs")
        }else{
          cat(paste("Analysis failed with the following error message: \n\n",result[[1]]))
        }
      }
      #save(result,file="asrResult.RData")
      shinybusy::remove_modal_spinner()

      if(!inherits(result,"try-error")) { # if all goes well in the run
        ## alert status converge
        observeEvent(result$modeling,{
          modeling <- result$modeling
          MtaAsrs <- result$status[which(result$status$module == "mtaAsr"),"analysisId"]
          MtaAsrId <- MtaAsrs[length(MtaAsrs)]
          modeling <- modeling[which(modeling$analysisId == MtaAsrId),]
          modeling <- subset(modeling, select = -c(module,analysisId))
          vmod<-modeling[which(modeling$parameter=="convergence"),c("trait","value")]
          cmod<-c()
          for(imod in 1:dim(vmod)[1]){
            if(vmod$value[imod]=="TRUE"){
              cmod[imod]<-paste("<font color=\"#008000\"><b>", vmod[imod,2], "</b></font>", " for trait ", vmod[imod,1])
              #paste("div(style='color: green;', 'Convergence status: ",vmod[imod,2]," for trait: ",vmod[imod,1],"')")
            }else{
              cmod[imod]<-paste("<font color=\"#FF0000\"><b>", vmod[imod,2], "</b></font>", " for trait ", vmod[imod,1])
              #paste("div(style='color: red;', 'Convergence status: ",vmod[imod,2]," for trait: ",vmod[imod,1],"')")
            }
          }
          #vmod<-apply(vmod,1,function(x){paste0("Convergence status ",x[2]," for trait ",x[1])})
          cmod<-paste(cmod,collapse="<br>")
          shinyWidgets::show_alert(
            inputId = ns("mystatus"),
            text = HTML(cmod),
            title = "Convergence status",
            type= "info",
            html= TRUE
          )
        })
        ## predictions table
        output$predictionsMtaAsr <-  DT::renderDT({
          predictions <- result$predictions
          predictions <- predictions[predictions$module=="mtaAsr",]
          predictions$analysisId <- as.numeric(predictions$analysisId)
          predictions <- predictions[!is.na(predictions$analysisId),]
          current.predictions <- predictions[predictions$analysisId==max(predictions$analysisId),]
          current.predictions <- subset(current.predictions, select = -c(module,analysisId))
          numeric.output <- c("predictedValue", "stdError", "reliability")
          DT::formatRound(DT::datatable(current.predictions, extensions = 'Buttons',
                                        options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                       lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
          ), numeric.output)
        }, server = FALSE)
        # metrics table
        output$metricsMtaAsr <-  DT::renderDT({
          if(!inherits(result,"try-error") ){
            metrics <- result$metrics
            MtaAsrs <- result$status[which(result$status$module == "mtaAsr"),"analysisId"]
            MtaAsrId <- MtaAsrs[length(MtaAsrs)]
            metrics <- metrics[which(metrics$analysisId == MtaAsrId),]
            metrics <- subset(metrics, select = -c(module,analysisId))
            numeric.output <- c("value", "stdError")
            DT::formatRound(DT::datatable(metrics, extensions = 'Buttons',
                                          options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                         lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
            ), numeric.output)
          }
        }, server = FALSE)
        # modeling table
        output$modelingMtaAsr <-  DT::renderDT({
          if(!inherits(result,"try-error") ){
            modeling <- result$modeling
            MtaAsrs <- result$status[which(result$status$module == "mtaAsr"),"analysisId"]
            MtaAsrId <- MtaAsrs[length(MtaAsrs)]
            modeling <- modeling[which(modeling$analysisId == MtaAsrId),]
            modeling <- subset(modeling, select = -c(module,analysisId))
            DT::datatable(modeling, extensions = 'Buttons',
                          options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                         lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
            )
          }
        }, server = FALSE)
        ## Report tab
        output$reportMtaAsr <- renderUI({
          HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportMtaASREML.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
          # HTML(markdown::markdownToHTML(knitr::knit("./R/reportMta.Rmd", quiet = TRUE), fragment.only=TRUE))
        })

        output$downloadReportMtaAsr <- downloadHandler(
          filename = function() {
            paste(paste0('MtaAsr_dashboard_',gsub("-", "", as.integer(Sys.time()))), sep = '.', switch(
              "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
            ))
          },
          content = function(file) {
            shinybusy::show_modal_spinner(spin = "fading-circle", text = "Generating Report...")

            src <- normalizePath(system.file("rmd","reportMtaASREML.Rmd",package="bioflow"))
            src2 <- normalizePath('data/resultMtaASREML.RData')

            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))

            file.copy(src, 'report.Rmd', overwrite = TRUE)
            file.copy(src2, 'resultMtaASREML.RData', overwrite = TRUE)

            out <- rmarkdown::render('report.Rmd', params = list(toDownload=TRUE),switch(
              "HTML",
              HTML = rmdformats::robobook(toc_depth = 4)
              # HTML = rmarkdown::html_document()
            ))

            # wait for it to land on disk (safety‐net)
            wait.time <- 0
            while (!file.exists(out) && wait.time < 60) {
              Sys.sleep(1); wait.time <- wait.time + 1
            }

            file.rename(out, file)
            shinybusy::remove_modal_spinner()
          }
        )

      } else {
        output$predictionsMtaAsr <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
        output$metricsMtaAsr <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
        output$modelingMtaAsr <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
        hideAll$clearAll <- TRUE
      } ### enf of if(!inherits(result,"try-error"))

      hideAll$clearAll <- FALSE

    }) ## end eventReactive

    output$outMtaAsr <- renderPrint({
      outMtaAsr1()
    })

    })
}


## To be copied in the UI
# mod_mtaASREMLApp_ui("mtaASREMLApp_1")

## To be copied in the server
# mod_mtaASREMLApp_server("mtaASREMLApp_1")

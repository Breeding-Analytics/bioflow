#' tensorMLApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tensorMLApp_ui <- function(id) {
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
                                           h1(strong(span("Predictive Modeling with Machine Learning Using Keras and Tensorflow",
                                                          tags$a(href="https://www.youtube.com/watch?v=rR1DhTt25n4&list=PLZ0lafzH_UmclOPifjCntlMzysEB2_2wX&index=7",
                                                                 icon("youtube") , target="_blank"), style="color:darkcyan"))),
                                           h2(strong("Data Status (wait to be displayed):")),
                                           uiOutput(ns("warningMessage")),
                                           tags$br(),
                                           # column(width=4, tags$br(),
                                           shinyWidgets::prettySwitch( inputId = ns('launch'), label = "Load example dataset", status = "success"),
                                           # ),
                                           tags$br(),
                                           img(src = "www/mta.png", height = 300, width = 450), # add an image
                                    ),
                                    column(width = 6,
                                           h2(strong("Details")),
                                           p("The core algorithm of the genetic evaluation using the two-step approach is the multi-trial analysis.
                                          This option aims to model breeding values across environments using the results from the single trial (weighted by the standard errors)
                              analysis and optionally a relationship matrix between levels of the random effects. This module allows the flexibility to build your own customized module by specifying the
                              random effects and any relationship . In addition, the most popular GxE models can be selected with a single click to help the user understand how a
                              specific model could be specified.
                                The way the arguments are used is the following:"),

                                           p(strong("Traits to analyze.-")," Traits to be analyzed. If no design factors can be fitted simple means are taken."),
                                           p(strong("Fixed effects.-")," Variables to be fitted as fixed effects. If more than one term is selected the term is assumed to be an interaction."),
                                           p(strong("Random effects.-")," Variables to be fitted as random effects. If more than one term is selected the term is assumed to be an interaction."),
                                           p(strong("Additional settings:")),
                                           p(strong("H2(lower bound).-")," Value of H2 to be used to remove trials with low heritability."),
                                           p(strong("H2(upper bound).-"),"  Value of H2 to be used to remove trials with too high heritability."),
                                           p(strong("mean(lower bound).-")," Value of trait mean to be used to remove trials with low means"),
                                           p(strong("mean(upper bound).-"),"  Value of trait mean to be used to remove trials with too high means"),
                                           p(strong("Number of iterations.-")," Maximum number of restricted maximum likelihood iterations to be run for each trait."),
                                           p(strong("Use weights.-")," a TRUE/FALSE statement indicating if the analysis should be weighted using the standard errors from the single trial analysis. The default is TRUE and should not be modified unless you know what you are doing."),
                                           p(strong("nPC.-")," Number of principal components for the big models. If the value is equal to 0 the kernel is used as is (full relationship matrix). Otherwise a principal component model is run according to Odegard et al. (2019)."),
                                           h2(strong("References:")),
                                           p("Henderson Jr, C. R. (1982). Analysis of covariance in the mixed model: higher-level, nonhomogeneous, and random regressions. Biometrics, 623-640."),
                                           p("Odegard, J., Indahl, U., Stranden, I., & Meuwissen, T. H. (2018). Large-scale genomic prediction using singular value decomposition of the genotype matrix. Genetics Selection Evolution, 50(1), 1-12."),
                                           h2(strong("Software used:")),
                                           p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
                                Vienna, Austria. URL https://www.R-project.org/."),
                                           p("COVARRUBIAS PAZARAN, G. E. (2024). lme4breeding: enabling genetic evaluation in the era of genomic data. bioRxiv, 2024-05."),
                                           # column(width = 6, shiny::plotOutput(ns("plotDataDependencies")), ),
                                    ),
                           ),
                           tabPanel(div(icon("arrow-right-to-bracket"), "Input steps"),
                                    tabsetPanel(
                                      tabPanel(div( icon("dice-one"), "Pick STA-stamp", icon("arrow-right") ), # icon = icon("dice-one"),
                                               br(),
                                               column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                      column(width=8, selectInput(ns("version2Mta"), "STA version to analyze (required)", choices = NULL, multiple = TRUE)),

                                               ),
                                               column(width=12),
                                               shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                   column(width=12,
                                                                          hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                          h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameter values to be specified in the grey boxes above.", style="color:green"))),
                                                                          hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                   ),
                                                                   column( width=12, shiny::plotOutput(ns("plotTimeStamps")) ),
                                                                   DT::DTOutput(ns("statusMta")), # modeling table
                                               ),
                                      ),
                                      tabPanel(div(icon("dice-two"), "Pick the model", icon("arrow-right") ), # icon = icon("dice-two"),
                                               br(),
                                               column(width=12, style = "background-color:grey; color: #FFFFFF",

                                                      column(width=12,
                                                             column(width=3, selectInput(ns("trait2Mta"), "Trait to analyze (required)", choices = NULL, multiple = TRUE) ),
                                                             column(width=6,
                                                                    radioButtons(ns("radio"), label = "Popular genetic evaluation models",
                                                                                 choices = list(
                                                                                   "Main" = "mn_model",
                                                                                   "Compound" = "cs_model",
                                                                                   "Finlay-W"="fw_model",
                                                                                   "Diagonal" = "dg_model",
                                                                                   "Main+Diag" = "csdg_model"
                                                                                 ),
                                                                                 selected = "mn_model", inline=TRUE),
                                                             ),
                                                             column(width=3,
                                                                    radioButtons(ns("radioModel"), label = "Covariance for designation",
                                                                                 choices = list(
                                                                                   "None"="none",
                                                                                   "GBLUP" = "geno_model",
                                                                                   "PBLUP" = "pedigree_model"
                                                                                 ),
                                                                                 selected = "none", inline=TRUE),
                                                             ),
                                                      ),
                                                      column(width=12,
                                                             column(width=4,
                                                                    numericInput(ns("nTermsFixed"), label = "nTerms fixed", value = NULL), # , step = 1, min = 1, max=10
                                                                    uiOutput(ns("leftSidesFixed"))
                                                             ),
                                                             column(width=4,
                                                                    # uiOutput(ns("nTermsRandom")),
                                                                    numericInput(ns("nTermsRandom"), label = "nTerms random", value = NULL), #
                                                                    uiOutput(ns("leftSidesRandom"))
                                                             ),
                                                             column(width=4,
                                                                    br(),br(), br(), br(),
                                                                    uiOutput(ns("rightSidesRandom"))
                                                             ),
                                                      ),

                                               ),
                                               column(width=12, style = "background-color:DarkGray; color: #FFFFFF",
                                                      column(width=12, # multiple nesting to center ir (I didn't find any other way)
                                                             column(width=12,
                                                                    uiOutput(ns("nPC"), inline=TRUE)
                                                             ),
                                                      ),
                                               ),
                                               column(width=12, style = "background-color:LightGray; color: #FFFFFF",
                                                      br(),
                                                      shinydashboard::box(width = 12,style = "color: #000000", status = "success", solidHeader=FALSE,collapsible = TRUE, collapsed = TRUE, title = "Fields to exclude (optional)...",
                                                                          p(span("Fields to exclude in the analysis (double click in the cell and set to zero if you would like to ignore an environment for a given trait).", style="color:black")),
                                                                          DT::dataTableOutput(ns("fieldsMet")),
                                                      ),
                                               ),
                                               column(width=6, style = "background-color:LightGray; color: #FFFFFF",
                                                      shinydashboard::box(width = 12, style = "color: #000000", status = "success", solidHeader=FALSE,collapsible = TRUE, collapsed = TRUE, title = "Additional model settings...",
                                                                          textInput(ns("heritLBMet"), label = "Lower H2&R2 bound per trait (separate by commas) or single value across", value="0.1"),
                                                                          textInput(ns("heritUBMet"), label = "Upper H2&R2 bound per trait (separate by commas) or single value across", value="0.95"),
                                                                          textInput(ns("meanLBMet"), label = "Lower environment-mean bound per trait (separate by commas) or single value across", value="0"),
                                                                          textInput(ns("meanUBMet"), label = "Upper environment-mean bound per trait (separate by commas) or single value across", value="1000000"),
                                                                          numericInput(ns("maxitMet"), label = "Number of iterations", value = 70),
                                                                          selectInput(ns("useWeights"), label = "Use weights?", choices = list(TRUE,FALSE), selected = TRUE, multiple=FALSE),
                                                                          selectInput(ns("calcSE"), label = "Calculate SEs?", choices = list(TRUE,FALSE), selected = TRUE, multiple=FALSE),
                                                      ),
                                               ),
                                               column(width = 6, style = "background-color:LightGray; color: #FFFFFF",
                                                      # br(),
                                                      shinydashboard::box(width = 12, status = "success", solidHeader=FALSE,collapsible = TRUE, collapsed = TRUE, title = "Alternative response distributions...",
                                                                          p(span("The Normal distribution is assumed as default for all traits. If you wish to specify a different trait distribution for a given trait double click in the cell corresponding for the trait by distribution combination and make it a '1'.", style="color:black")),
                                                                          DT::DTOutput(ns("traitDistMet")),
                                                      ),
                                               ),
                                               column(width=12),
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
                                      ),
                                      tabPanel("Run analysis", icon = icon("dice-three"),
                                               column(width=12,style = "background-color:grey; color: #FFFFFF",
                                                      br(),
                                                      actionButton(ns("runMta"), "Run MTA (click button)", icon = icon("play-circle")),
                                                      uiOutput(ns("qaQcMtaInfo")),
                                                      br(),
                                               ),
                                               textOutput(ns("outMta")),
                                      ),
                                    )
                           ),
                           tabPanel(div(icon("arrow-right-from-bracket"), "Output tabs" ) , value = "outputTabs",
                                    tabsetPanel(
                                      tabPanel("Dashboard", icon = icon("file-image"),
                                               br(),
                                               textOutput(ns("outMta2")),
                                               br(),
                                               downloadButton(ns("downloadReportMta"), "Download dashboard"),
                                               br(),
                                               uiOutput(ns('reportMta'))
                                      ),
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

                                    ) # end of tabset
                           )# end of output panel
              )) # end mainpanel


  )
}

#' tensorMLApp Server Functions
#'
#' @noRd
mod_tensorMLApp_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_tensorMLApp_ui("tensorMLApp_1")

## To be copied in the server
# mod_tensorMLApp_server("tensorMLApp_1")

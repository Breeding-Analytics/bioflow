#' mtaLMMsolveApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mtaLMMsolveApp_ui <- function(id) {
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
                                           h1(strong(span("Flexible Multi Trial Analysis Module Using LMMsolver",
                                                          tags$a(href="https://www.youtube.com/watch?v=rR1DhTt25n4&list=PLZ0lafzH_UmclOPifjCntlMzysEB2_2wX&index=7",
                                                                 icon("youtube") , target="_blank"), style="color:darkcyan"))),
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
                                           h2(strong("References")),
                                           p("Henderson Jr, C. R. (1982). Analysis of covariance in the mixed model: higher-level, nonhomogeneous, and random regressions. Biometrics, 623-640."),
                                           p("Odegard, J., Indahl, U., Stranden, I., & Meuwissen, T. H. (2018). Large-scale genomic prediction using singular value decomposition of the genotype matrix. Genetics Selection Evolution, 50(1), 1-12."),
                                           p("Finlay, K. W., & Wilkinson, G. N. (1963). The analysis of adaptation in a plant-breeding programme. Australian journal of agricultural research, 14(6), 742-754."),
                                           p("Xiang, T., Christensen, O. F., & Sørensen, A. C. (2016). Genomic evaluation by including dominance effects and inbreeding depression for purebred and crossbred performance with a unified model. Genetics Selection Evolution, 48(1), 84"),
                                           p("Batista, L. G., Mello, V. H., Souza, A. P., & Margarido, G. R. A. (2022). Genomic prediction with allele dosage information in highly polyploid species. Theoretical and Applied Genetics, 135(2), 723–739."),
                                           h2(strong("Software used")),
                                           p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
                                Vienna, Austria. URL https://www.R-project.org/."),
                                           p("COVARRUBIAS PAZARAN, G. E. (2024). lme4breeding: enabling genetic evaluation in the era of genomic data. bioRxiv, 2024-05."),
                                           p("Boer M, van Rossum B (2022). LMMsolver: Linear Mixed Model Solver. R package version 1.0.4.9000."),
                                           # column(width = 6, shiny::plotOutput(ns("plotDataDependencies")), ),
                                    ),
                           ),
                           tabPanel(div(icon("arrow-right-to-bracket"), "Input steps"),
                                    tabsetPanel(
                                      tabPanel(div( icon("dice-one"), "Pick STA-stamp(s)", icon("arrow-right") ), # icon = icon("dice-one"),
                                               br(),
                                               column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                      column(width=8,
                                                             selectInput(ns("version2Mta"),
                                                                         label = tags$span(
                                                                           "STA version(s) to analyze",
                                                                           tags$i(
                                                                             class = "glyphicon glyphicon-info-sign",
                                                                             style = "color:#FFFFFF",
                                                                             title = "Analysis ID(s) from STA runs that should be combined and used to fit a multi-trial analysis."
                                                                           )
                                                                         ),
                                                                         choices = NULL, multiple = TRUE),
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
                                                                   DT::DTOutput(ns("statusMta")), # modeling table
                                               ),
                                      ),
                                      tabPanel(div(icon("dice-two"), "Pick trait(s)", icon("arrow-right") ),
                                               br(),
                                               column(width=12,style = "background-color:grey; color: #FFFFFF",
                                                      selectInput(ns("trait2Mta"),
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
                                               column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                      br(),
                                                      column(
                                                        width  = 12,
                                                        tags$div(
                                                          style = "background:#fff3cd;border:1px solid #ffeeba;color:#856404;padding:12px;border-radius:6px;margin-bottom:12px;",
                                                          tags$b("Warning:"),
                                                          tags$p(style = "margin-bottom:0;","Selecting a shortcut to a popular genetic evaluation model or a surrogate of merit will set the default model."),
                                                          tags$p(style = "margin-top:0;margin-bottom:0;","If you modify the terms, it may no longer match the selected model or merit and will instead follow the terms you specified."),
                                                        )
                                                      ),
                                                      column(width=12,
                                                             column(width=8,
                                                                    radioButtons(ns("radio"),
                                                                                 label = tags$span(
                                                                                   "Shortcut to popular genetic evaluation models",
                                                                                   # tags$i(
                                                                                   #   class = "glyphicon glyphicon-info-sign",
                                                                                   #   style = "color:#FFFFFF",
                                                                                   #   title = "Selecting a popular genetic model sets a default, but if you change the terms, it may no longer match the selected model and will instead follow the terms you specified."
                                                                                   # )
                                                                                 ),
                                                                                 choices = list(
                                                                                   "Main effect" = "mn_model",
                                                                                   "Compound symmetry" = "cs_model",
                                                                                   "Finlay-Wilkinson"="fw_model",
                                                                                   "Diagonal" = "dg_model",
                                                                                   "Main+Diagonal" = "mndg_model",
                                                                                   "Main effects (A+D)" = "ad_model",
                                                                                   "SCA/GCA" = "sca_model",
                                                                                   "GCA" = "gca_model"
                                                                                 ),
                                                                                 selected = "mn_model", inline=TRUE),
                                                                    radioTooltip(id = ns("radio"), choice = "mn_model", title = "The main effect model assumes that there is no genotype by environment interaction or that can be ignored to focus on average effects. This can occur when we are interested in selecting the best individuals across the TPE without further consideration to specific environments.", placement = "right", trigger = "hover"),
                                                                    radioTooltip(id = ns("radio"), choice = "cs_model", title = "The compound symmetry model assumes that there is a main effect driving the performance of the genotypes but also specific deviations in each environment. The assumption is that all environments have the same genetic variance and all pairs of environments have the same genetic covariance.", placement = "right", trigger = "hover"),
                                                                    radioTooltip(id = ns("radio"), choice = "fw_model", title = "The Finlay-Wilkinson model assumes that there is a main effect driving the performance of the genotypes but also specific deviations in each environment. The assumption is that all environments have the same genetic variance and deviations are with respect to an environmental covariate which can be the trait means at different environments or any other covariate.", placement = "right", trigger = "hover"),
                                                                    radioTooltip(id = ns("radio"), choice = "dg_model", title = "The diagonal model assumes that there is a different genetic variance at each environment and that genetic covariance between environments is zero. This relaxes he assumption of the main effect model and ignores a main effect.", placement = "right", trigger = "hover"),
                                                                    radioTooltip(id = ns("radio"), choice = "mndg_model", title = "The diagonal plus main effect model assumes that there is a main effect for genotypes but at the same time each enviroment causes the expression of environment specific genetic variance. The covariance between genotype effects between environments is assumed to be the same.", placement = "right", trigger = "hover"),
                                                                    radioTooltip(id = ns("radio"), choice = "ad_model", title = "This model includes additve and dominance random effects for the designation. Only use in order to run GPCP in the mate optimization module", placement = "right", trigger = "hover"),
                                                                    radioTooltip(id = ns("radio"), choice = "sca_model", title = "This model includes STA random effects for the designation and GCA random effects for mother and father. Requires pedigree information to map each designation to their respective mother and father", placement = "right", trigger = "hover"),
                                                                    radioTooltip(id = ns("radio"), choice = "gca_model", title = "This model includes GCA random effects for mother and father.Requires pedigree information to map each designation to their respective mother and father", placement = "right", trigger = "hover")
                                                             ),
                                                             column(width=4,
                                                                    radioButtons(ns("radioModel"),
                                                                                 label = tags$span(
                                                                                   "Shortcut to different surrogates of merit",
                                                                                   # tags$i(
                                                                                   #   class = "glyphicon glyphicon-info-sign",
                                                                                   #   style = "color:#FFFFFF",
                                                                                   #   title = "Selecting a surrogate of merit sets a default covariance structure, but if you change the covariance structure, it may no longer match the selected surrogate of merit and will instead follow the covariance structure you specified."
                                                                                   # )
                                                                                 ),
                                                                                 choices = list(
                                                                                   "TGV"="none",
                                                                                   "GTGV" = "genoAD_model",
                                                                                   "EBV" = "pedigree_model",
                                                                                   "GEBV" = "geno_model",
                                                                                   "SCA/GCA" = "noneSCA_model",
                                                                                   "GCA" = "noneGCA_model",
                                                                                   "gSCA/gGCA" = "genoSCA_model",
                                                                                   "gGCA" = "genoGCA_model"
                                                                                 ),
                                                                                 selected = "none", inline=TRUE),
                                                                    radioTooltip(id = ns("radioModel"), choice = "none", title = "The TGV model assumes that there is is no known covariance between the levels of designation and therefore the BLUP coming out of that model is considered a total genetic value (TGV). ", placement = "right", trigger = "hover"),
                                                                    radioTooltip(id = ns("radioModel"), choice = "genoAD_model", title = "The GTGV model assumes that genetic markers coded both as additive and dominance effects should be used to calculate the covariance between levels of designation. The resulting BLUPs are considered genomic estimated total genetic values (GTGV).", placement = "right", trigger = "hover"),
                                                                    radioTooltip(id = ns("radioModel"), choice = "pedigree_model", title = "The EBV model assumes that the pedigree should be used to calculate the covariance between levels of designation. The resulting BLUPs are the so-called estimated breeding values (EBV).", placement = "right", trigger = "hover"),
                                                                    radioTooltip(id = ns("radioModel"), choice = "geno_model", title = "The GEBV model assumes that genetic markers should be used to calculate the covariance between levels of designation. The resulting BLUPs are considered genomic estimated breeding values (GEBV).", placement = "right", trigger = "hover"),
                                                                    radioTooltip(id = ns("radioModel"), choice = "noneSCA_model", title = "The STA/GCA model assumes that there is no known covariance between the levels of designation, mother and father", placement = "right", trigger = "hover"),
                                                                    radioTooltip(id = ns("radioModel"), choice = "noneGCA_model", title = "The GCA model assumes that there is no known covariance between the levels of mother and father", placement = "right", trigger = "hover"),
                                                                    radioTooltip(id = ns("radioModel"), choice = "genoSCA_model", title = "The gSCA/gGCA model assumes that genetic markers should be used to calculate the covariance between levels of designation, mother and father", placement = "right", trigger = "hover"),
                                                                    radioTooltip(id = ns("radioModel"), choice = "genoGCA_model", title = "The gGCA model assumes that genetic markers should be used to calculate the covariance between levels of mother and father.", placement = "right", trigger = "hover")
                                                             ),
                                                      ),
                                               ),
                                               column(width=12, style = "background-color:DarkGray; color: #FFFFFF",
                                                      column(width=12,
                                                             column(width=4,
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
                                                             column(width=4,
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
                                                             column(width=4,
                                                                    selectInput(ns("versionMarker2Mta"), label = tags$span("Marker QA version to use",
                                                                                                                           tags$i(class = "glyphicon glyphicon-info-sign",
                                                                                                                                  style = "color:#FFFFFF",
                                                                                                                                  title = "Analysis ID(s) from QA geno modifications that should be combined and used to fit a multi-trial analysis.")),
                                                                                                                           choices = NULL, multiple = FALSE),
                                                                    uiOutput(ns("rightSidesRandom"))
                                                             ),
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
                                      tabPanel(div(icon("dice-four"), "Other options", icon("arrow-right") ),
                                               br(),
                                               column(width=12, style = "background-color:DarkGray; color: #FFFFFF",
                                                      br(),
                                                      shinydashboard::box(
                                                        width = 12, style = "color: #000000", status = "success",
                                                        solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE,
                                                        title = "Fields to exclude (optional)...",
                                                        p(span("Fields to exclude in the analysis (double click in the cell and set to zero if you would like to ignore an environment for a given trait).", style="color:black")),
                                                        div(
                                                          style = "overflow-x: auto; width: 100%;",
                                                          DT::dataTableOutput(ns("fieldsMet"))
                                                        )
                                                      ),
                                               ),
                                               column(width=6, style = "background-color:LightGray; color: #FFFFFF",
                                                      br(),
                                                      shinydashboard::box(width = 12, style = "color: #000000", status = "success", solidHeader=FALSE,collapsible = TRUE, collapsed = TRUE, title = "Additional model settings...",
                                                                          textInput(ns("heritLBMet"),
                                                                                    label = tags$span(
                                                                                      "Lower H2&R2 bound",
                                                                                      tags$i(
                                                                                        class = "glyphicon glyphicon-info-sign",
                                                                                        style = "color:#000000",
                                                                                        title = "If a single value of heritability and reliability is specified it will be used across all traits. If a different value is required per trait please separate the values by comma."
                                                                                      )
                                                                                    ),
                                                                                    value="0.1"),
                                                                          textInput(ns("heritUBMet"),
                                                                                    label = tags$span(
                                                                                      "Upper H2&R2 bound",
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
                                                                          selectInput(ns("useWeights"),
                                                                                      label = tags$span(
                                                                                        "Use weights?",
                                                                                        tags$i(
                                                                                          class = "glyphicon glyphicon-info-sign",
                                                                                          style = "color:#000000",
                                                                                          title = "A TRUE/FALSE value in case you want to avoid using the weights in the two stage analysis (not recommended). The weights are used to account for the differential error variance in each trial."
                                                                                        )
                                                                                      ),
                                                                                      choices = list(TRUE,FALSE), selected = TRUE, multiple=FALSE),
                                                                          selectInput(ns("estHybrids"),
                                                                                      label = tags$span(
                                                                                        "Estimate hybrid ganotypes from parents?",
                                                                                        tags$i(
                                                                                          class = "glyphicon glyphicon-info-sign",
                                                                                          style = "color:#000000",
                                                                                          title = "A TRUE/FALSE value in case you want to avoid estimating hybrids from the genotypes of parents with SCA/GCA model."
                                                                                        )
                                                                                      ),
                                                                                      choices = list(TRUE,FALSE), selected = TRUE, multiple=FALSE),
                                                                          selectInput(ns("calcSE"), label = "Calculate SEs?", choices = list(TRUE,FALSE), selected = TRUE, multiple=FALSE),
                                                      ),
                                               ),
                                               column(width = 6, style = "background-color:LightGray; color: #FFFFFF",
                                                      br(),
                                                      shinydashboard::box(width = 12, status = "success", solidHeader=FALSE,collapsible = TRUE, collapsed = TRUE, title = "Alternative response distributions...",
                                                                          p(span("The Normal distribution is assumed as default for all traits. If you wish to specify a different trait distribution for a given trait double click in the cell corresponding for the trait by distribution combination and make it a '1'.", style="color:black")),
                                                                          DT::DTOutput(ns("traitDistMet")),
                                                      ),
                                               ),
                                               column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                      column(width=12, h5("Number of principal components for possible covariance kernels (0 is none, < 1 restricts to only levels present)"), ),
                                                      column(width=12, # multiple nesting to center ir (I didn't find any other way)
                                                             column(width=12,
                                                                    uiOutput(ns("nPC"), inline=TRUE)
                                                             ),
                                                      ),
                                               ),
                                      ),
                                      tabPanel("Run analysis", icon = icon("dice-five"),
                                               br(),
                                               column(width=12,style = "background-color:grey; color: #FFFFFF",
                                                      column(width=3, tags$div(textInput(ns("analysisIdName"), label = tags$span(
                                                        "Analysis Name (optional)", tags$i( class = "glyphicon glyphicon-info-sign", style = "color:#FFFFFF",
                                                                                            title = "An optional name for the analysis besides the timestamp if desired.") ), #width = "100%",
                                                        placeholder = "(optional name)") ) ),
                                                      column(width=3,
                                                             br(),
                                                             actionButton(ns("runMta"), "Run analysis", icon = icon("play-circle")),
                                                             uiOutput(ns("qaQcMtaInfo")),
                                                             br(),
                                                      ),

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
                                               actionButton(ns("renderReportMta"), "Download dashboard", icon = icon("download")),
                                               downloadButton(ns("downloadReportMta"), "Download dashboard", style = "visibility:hidden;"),
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

#' mtaLMMsolveApp Server Functions
#'
#' @noRd
mod_mtaLMMsolveApp_server <- function(id, data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    # output$plotDataDependencies <- shiny::renderPlot({ dependencyPlot() })
    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })
    #################
    ## version
    observeEvent(c(data()), {
      req(data())
      dtMta <- data() # dtMta <- result
      dtMta <- dtMta$status
      dtMta <- dtMta[which(dtMta$module == "sta"),]
      traitsMta <- unique(dtMta$analysisId)
      if(length(traitsMta) > 0){
        if("analysisIdName" %in% colnames(dtMta)){
          names(traitsMta) <- paste(dtMta$analysisIdName, as.POSIXct(traitsMta, origin="1970-01-01", tz="GMT"), sep = "_")
        }else{
          names(traitsMta) <- as.character(as.POSIXct(traitsMta, origin="1970-01-01", tz="GMT"))
        }
      }
      updateSelectInput(session, "version2Mta", choices = traitsMta)
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
    ## traits available
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
      updateSelectInput(session, "traitMetrics", choices = traitsMta, selected = traitsMta)
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
    # ## version qa marker
    observeEvent(c(data()), {
      req(data())
      dtMta <- data()
      dtMta <- dtMta$status
      dtMta <- dtMta[which(dtMta$module == "qaGeno"),]
      traitsMta <- unique(dtMta$analysisId)
      # if(length(traitsMta) > 0){names(traitsMta) <- as.POSIXct(traitsMta, origin="1970-01-01", tz="GMT")}
      if(length(traitsMta) > 0){
        if("analysisIdName" %in% colnames(dtMta)){
          names(traitsMta) <- paste(dtMta$analysisIdName, as.POSIXct(traitsMta, origin="1970-01-01", tz="GMT"), sep = "_")
        }else{
          names(traitsMta) <- as.character(as.POSIXct(traitsMta, origin="1970-01-01", tz="GMT"))
        }
      }
      updateSelectInput(session, "versionMarker2Mta", choices = traitsMta)
    })
    #################
    # plots
    output$barplotPredictionsMetrics <- plotly::renderPlotly({
      req(data())
      req(input$version2Mta)
      dtMta <- data()
      mydata <- dtMta$metrics
      mydata <- mydata[which(mydata$analysisId %in% input$version2Mta),]
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
    observeEvent(c(data(), input$version2Mta,input$radio), {
      req(data())
      req(input$version2Mta)
      req(input$radio)
      dtMta <- data()
      dtMta <- dtMta$predictions
      dtMta <- dtMta[which(dtMta$analysisId %in% input$version2Mta),]
      envs <- unique(dtMta[,"environment"])
      envsDg <- envs
      if ( input$radio == "mndg_model" | input$radio == "dg_model") {
        n2 <- length(envsDg)
      }else if (input$radio == "ad_model") {
        n2 <- 2  # <- add 'f' alongside 'environment'
      }else{n2 <- 1}
      updateNumericInput(session, "nTermsFixed", value = n2, step = 1, min = 1, max=10)
    })

    observeEvent(input$radio, {
      if (input$radio == "ad_model") {
        updateRadioButtons(session, inputId = "radioModel",
                           # choices = list(
                           #   "GTGV" = "genoAD_model",
                           # ),
                           selected = "genoAD_model", inline = TRUE)
      } else if(input$radio == "sca_model"){
        updateRadioButtons(session, inputId = "radioModel",
                            choices = list(
                              "SCA/GCA" = "noneSCA_model",
                              "gSCA/gGCA" = "genoSCA_model"
                            ),
                           selected = "noneSCA_model", inline = TRUE)
      }else if(input$radio == "gca_model"){
        updateRadioButtons(session, inputId = "radioModel",
                           choices = list(
                             "GCA" = "noneGCA_model",
                             "gGCA" = "genoGCA_model"
                           ),
                           selected = "noneGCA_model", inline = TRUE)
      } else{
        updateRadioButtons(session, inputId = "radioModel",
                            choices = list(
                              "TGV"="none",
                              "GTGV" = "genoAD_model",
                              "EBV" = "pedigree_model",
                              "GEBV" = "geno_model"
                            ),
                           selected = isolate(input$radioModel) %||% "none",
                           inline = TRUE)
      }
    })
    #################
    ## nTermsRandom
    observeEvent(c(data(), input$version2Mta,input$radio), {
      req(data())
      req(input$version2Mta)
      req(input$radio)
      dtMta <- data()
      dtMta <- dtMta$predictions
      dtMta <- dtMta[which(dtMta$analysisId %in% input$version2Mta),]
      envs <- unique(dtMta[,"environment"])
      envsDg <- envs
      if ( input$radio == "cs_model" | input$radio == "fw_model" | input$radio == "ad_model" | input$radio == "gca_model") {
        n <- 2
      }else if( input$radio == "sca_model" ){
        n <- 3
      }else if( input$radio == "mndg_model" ){
        n <- length(envsDg) + 1
      }else if( input$radio == "dg_model" ){
        n <- length(envsDg)
      }else{n <- 1}
      updateNumericInput(session, "nTermsRandom", value = n, step = 1, min = 1, max=10)
    })
    ## fixed effects
    output$leftSidesFixed <- renderUI({ # input <- list(version2Mta=result$status$analysisId[3], trait2Mta="YLD_TON",nTermsFixed=1)
      req(data())
      req(input$version2Mta)
      req(input$trait2Mta)
      req(input$nTermsFixed)
      dtMta <- data() # dtMta <- result
      mydata <- dtMta$predictions #
      mydata <- mydata[which(mydata$analysisId %in% input$version2Mta),]
      metaPheno <- dtMta$metadata$pheno[which(dtMta$metadata$pheno$parameter %in% c("pipeline","stage","environment","year","season","timepoint","country","location","trial","study","management")),]
      otherMetaCols <- unique(dtMta$data$pheno[,metaPheno$value,drop=FALSE])
      colnames(otherMetaCols) <- cgiarBase::replaceValues(Source = colnames(otherMetaCols), Search = metaPheno$value, Replace = metaPheno$parameter )
      otherMetaCols <- otherMetaCols[which(!duplicated(otherMetaCols[,"environment"])),,drop=FALSE] # we do this in case the users didn't define the environment properly
      mydata <- merge(mydata, otherMetaCols, by="environment", all.x = TRUE)
      WeatherRow <- as.data.frame(cgiarPipeline::summaryWeather(object=dtMta, wide=TRUE)); WeatherRow$environment <- rownames(WeatherRow)
      mydata <- merge(mydata, WeatherRow, by="environment", all.x = TRUE)

      choices <- setdiff(colnames(mydata), c("predictedValue","stdError","reliability","analysisId","module") )

      # If model is A+D, add "inbreeding" to choices
      if (input$radio == "ad_model" | input$radio == "sca_model") {
        choices <- unique(c("inbreeding", choices))
      }


      envs <- unique(mydata[,"environment"])
      envsDg <- envs

      if ( input$radio == "mndg_model" | input$radio == "dg_model") {
        lapply(1:input$nTermsFixed, function(i) {
          selectInput(
            session$ns(paste0('leftSidesFixed',i)),
            label = ifelse(i==1, "Fixed Effects",""),
            choices = choices, multiple = TRUE,
            selected = envsDg[i]
          )
        })
      }else{
        lapply(1:input$nTermsFixed, function(i) {
          defaultFixed <- character()
          if (i == 1) {
            defaultFixed <- "environment"
          } else if (i == 2 && (input$radio == "ad_model" |input$radio == "sca_model")) {
            defaultFixed <- "inbreeding"
          }

          selectInput(
            session$ns(paste0('leftSidesFixed', i)),
            label = ifelse(i == 1, "Fixed Effects", ""),
            choices = choices,
            multiple = TRUE,
            selected = defaultFixed
          )
        })
      }

    })
    ## left formula (actual effects) ## input <- list(version2Mta=result$status$analysisId[2])
    output$leftSidesRandom <- renderUI({
      req(data())
      req(input$version2Mta)
      req(input$trait2Mta)
      req(input$nTermsRandom)
      dtMta <- data()
      mydata <- dtMta$predictions #
      mydata <- mydata[which(mydata$analysisId %in% input$version2Mta),]
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
      envsDg <- envs
      envsDg2 <- paste0(rep("environment",10),"")
      desDg <-rep("designation",length(envsDg))
      if ( input$radio == "cs_model") { # CS model
        lapply(1:input$nTermsRandom, function(i) {
          selectInput(
            session$ns(paste0('leftSidesRandom',i)),
            label = ifelse(i==1, "Random Effects",""),
            choices = choices, multiple = TRUE,
            selected = if(i==1){"designation"}else if(i==2){c( desDg[i], envsDg2[i] )}else{"designation"}
          )
        })
      }else if( input$radio == "mndg_model" ){ # main + dg
        lapply(1:input$nTermsRandom, function(i) {
          selectInput(
            session$ns(paste0('leftSidesRandom',i)),
            label = ifelse(i==1, "Random Effects",""),
            choices = choices, multiple = TRUE,
            selected =if( i > length(envsDg) ){"designation" }else{c( envsDg[i], desDg[i] )}
          )
        })
      }else if( input$radio == "dg_model" ){ # DIAG model
        lapply(1:input$nTermsRandom, function(i) {
          selectInput(
            session$ns(paste0('leftSidesRandom',i)),
            label = ifelse(i==1, "Random Effects",""),
            choices = choices, multiple = TRUE,
            selected = c(envsDg[i],"designation")
          )
        })
      }else if(input$radio == "fw_model"){ # FW model
        lapply(1:input$nTermsRandom, function(i) {
          selectInput(
            session$ns(paste0('leftSidesRandom',i)),
            label = ifelse(i==1, "Random Effects",""),
            choices = choices, multiple = TRUE,
            selected = if(i==1){"designation"}else if(i==2){rev(c(fwvars[1], "designation"))}else{"designation"}
          )
        })
      }else if(input$radio == "ad_model"){ # A+D main effect
        lapply(1:input$nTermsRandom, function(i) {
          selectInput(
            session$ns(paste0('leftSidesRandom', i)),
            label = ifelse(i == 1, "Random Effects", ""),
            choices = choices, multiple = TRUE,
            selected = "designation"
          )
        })
      }else if(input$radio == "sca_model"){
        needed   <- c("designation", "mother", "father")
        missing  <- setdiff(needed, choices)

        validate(
          need(
            length(missing) == 0,
            "To use the STA/GCA model you must have mother and father columns from pedigree data",
          )
        )

        lapply(1:input$nTermsRandom, function(i) {
          selectInput(
            session$ns(paste0('leftSidesRandom', i)),
            label = ifelse(i == 1, "Random Effects", ""),
            choices = choices, multiple = TRUE,
            selected = if(i==1){"designation"}else if(i==2){"mother"}else{"father"}
          )
        })
      }else if(input$radio == "gca_model"){
        needed   <- c("mother", "father")
        missing  <- setdiff(needed, choices)

        validate(
          need(
            length(missing) == 0,
            "To use the GCA model you must have mother and father columns from pedigree data",
          )
        )

        lapply(1:input$nTermsRandom, function(i) {
          selectInput(
            session$ns(paste0('leftSidesRandom', i)),
            label = ifelse(i == 1, "Random Effects", ""),
            choices = choices, multiple = TRUE,
            selected = if(i==1){"mother"}else{"father"}
          )
        })
      }else { # main model specified
        lapply(1:input$nTermsRandom, function(i) {
          selectInput(
            session$ns(paste0('leftSidesRandom',i)),
            label = ifelse(i==1, "Random Effects",""),
            choices = choices, multiple = TRUE,
            selected = "designation"
          )
        })
      }

    })
    # right-side equation (explanatory covariates)
    output$rightSidesRandom <- renderUI({
      req(data())
      req(input$version2Mta)
      req(input$trait2Mta)
      req(input$nTermsRandom)
      mydata <- data()$predictions #
      mydata <- mydata[which(mydata$analysisId %in% input$version2Mta),]
      choices <- c(  "none", "none.", "none..", "none...", setdiff(names(data()$data), c("qtl","genodir","pheno") ), unique(mydata$trait) )
      if("geno" %in% choices){choices <- c( cgiarBase::replaceValues(choices,"geno","genoA"),"genoAD","genoD")}
      envs <- unique(mydata[,"environment"])
      envsDg <- envs
      if(input$radioModel == "geno_model"){
        useMod1 <- "none"
        useMod2 <- "genoA"
      }else if(input$radioModel == "pedigree_model"){
        useMod1 <- "none"
        useMod2 <- "pedigree"
      }else if(input$radioModel == "none"){
        useMod1 <- "none"
        useMod2 <- "none."
      }else if(input$radioModel == "genoAD_model"){
        useMod1 <- "none"
        useMod2 <- "genoAD"
      }

      if (input$radio == "cs_model") { # CS model
        lapply(1:input$nTermsRandom, function(i) {
          selectInput(
            inputId=session$ns(paste0('rightSidesRandom',i)),
            label = ifelse(i==1, "Covariance of random effect based on:",""),
            choices = choices, multiple = TRUE,
            selected = if(i==1){useMod2}else if(i==2){rev(c(useMod1,useMod2))}else{useMod2}
          )
        })
      }else if( input$radio == "mndg_model" ){
        lapply(1:input$nTermsRandom, function(i) {
          selectInput(
            inputId=session$ns(paste0('rightSidesRandom',i)),
            label = ifelse(i==1, "Covariance of random effect based on:",""),
            choices = choices, multiple = TRUE,
            selected =if( i > length(envsDg) ){useMod2 }else{c( useMod2, useMod1 )}
          )
        })
      }else if(input$radio == "fw_model"){
        lapply(1:input$nTermsRandom, function(i) {
          selectInput(
            session$ns(paste0('rightSidesRandom',i)),
            label = ifelse(i==1, "Covariance of random effect based on:",""),
            choices = choices, multiple = TRUE,
            selected = if(i==1){useMod2}else if(i==2){c(useMod2, useMod1)}else{useMod1}
          )
        })
      }else if( input$radio == "dg_model" ){
        lapply(1:input$nTermsRandom, function(i) {
          selectInput(
            inputId=session$ns(paste0('rightSidesRandom',i)),
            label = ifelse(i==1, "Covariance of random effect based on:",""),
            choices = choices, multiple = TRUE,
            selected = c(useMod2,useMod1)
          )
        })
      }else if( input$radio == "ad_model" ){
        lapply(1:input$nTermsRandom, function(i) {
          selectInput(
            inputId = session$ns(paste0('rightSidesRandom', i)),
            label = ifelse(i == 1, "Covariance of random effect based on:", ""),
            choices = choices, multiple = TRUE,
            selected = ifelse(i == 1,"genoA","genoD")
          )
        })
      }else if( input$radio == "sca_model" ){
        lapply(1:input$nTermsRandom, function(i) {
          selectInput(
            inputId = session$ns(paste0('rightSidesRandom', i)),
            label = ifelse(i == 1, "Covariance of random effect based on:", ""),
            choices = choices, multiple = TRUE,
            selected = ifelse(i == 1, ifelse(input$radioModel == "noneSCA_model","none","genoD"),
                              ifelse(input$radioModel == "noneSCA_model","none","genoA"))
          )
        })
      }else if( input$radio == "gca_model" ){
        lapply(1:input$nTermsRandom, function(i) {
          selectInput(
            inputId = session$ns(paste0('rightSidesRandom', i)),
            label = ifelse(i == 1, "Covariance of random effect based on:", ""),
            choices = choices, multiple = TRUE,
            selected = ifelse(input$radioModel == "noneGCA_model","none","genoA")
          )
        })
      }else { # main model specified
        lapply(1:input$nTermsRandom, function(i) {
          selectInput(
            inputId=session$ns(paste0('rightSidesRandom',i)),
            label = ifelse(i==1, "Covariance of random effect based on:",""),
            choices = choices, multiple = TRUE,
            selected = useMod2
          )
        })
      }

    })

    # n pricipal components for explanatory covariates
    output$nPC <- renderUI({
      req(data())
      req(input$version2Mta)
      req(input$trait2Mta)

      mydata <- data()$predictions #
      mydata <- mydata[which(mydata$analysisId %in% input$version2Mta),]
      choices <- c( setdiff(names(data()$data), c("qtl","genodir","pheno") ), unique(mydata$trait))
      # if("geno" %in% choices){choices <- c( cgiarBase::replaceValues(choices,"geno","genoA"),"genoAD")}

      lapply(1:length(choices), function(i) {
        div(
          numericInput(
            session$ns(paste0('nPC',i)),
            label = paste0("nPC (",choices[i],")"),
            value = ifelse(choices[i]%in%c("geno","pedigree"),-1,0), # -1 is to subset to only individuals present
            min = -1, max = Inf, step = 1
          ),
          style = "display: inline-block;"
        )
      })

    })
    # inputFormula summarizing the fixed effects
    inputFormulaFixed = reactive({
      req(data());  req(input$version2Mta);  req(input$trait2Mta); req(input$nTermsFixed)
      if (length(input$trait2Mta) != 0) {
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
      req(data());  req(input$version2Mta);  req(input$trait2Mta); req(input$nTermsRandom)
      if (length(input$trait2Mta) != 0) {
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
      req(data());  req(input$version2Mta);  req(input$trait2Mta); req(input$nTermsRandom)
      if (length(input$trait2Mta) != 0) {
        values <- vector(mode = "list", length = input$nTermsRandom)
        for (i in 1:input$nTermsRandom) {
          tempval <- reactive({paste0('input$','rightSidesRandom',i)})
          s1 <- eval( parse(text = tempval() ) )
          values[[i]] <- s1
        }
        return(values)
      }
    })
    # inputFormula summarizing the PCs
    inputFormulaPCs = reactive({
      req(data());  req(input$version2Mta);  req(input$trait2Mta);

      mx <- data()
      mydata <- mx$predictions #
      mydata <- mydata[which(mydata$analysisId %in% input$version2Mta),]
      choices <- c( setdiff(names(mx$data), c("qtl","genodir","pheno") ), unique(mydata$trait))
      # if("geno" %in% choices){choices <- c( cgiarBase::replaceValues(choices,"geno","genoA"),"genoD")}

      if (length(input$trait2Mta) != 0) {
        values <- vector(mode = "list", length = length(choices))
        for (i in 1:length(choices)) {
          tempval <- reactive({paste0('input$','nPC',i)})
          s1 <- eval( parse(text = tempval() ) )
          values[[i]] <- s1
        }

        values <- unlist(values)
        if(!is.null(values)){
          names(values) <- choices
        }
        return(values)
      }
    })
    #################
    # reactive table for trait family distributions
    dtDistTrait = reactive({
      traitNames = input$trait2Mta
      mm = matrix(0,nrow = 10, ncol = length(traitNames));
      rownames(mm) <- c(
        "quasi(link='identity',variance='constant')","quasi(link='logit',variance='constant')",
        "quasi(link='inverse',variance='constant')","quasi(link='1/mu^2',variance='constant')","quasi(link='log',variance='constant')",
        "gaussian(link = 'identity')", "binomial(link = 'logit')",  "Gamma(link = 'inverse')",
        "inverse.gaussian(link = '1/mu^2')", "poisson(link = 'log')"
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
          X <- with(zz, enhancer::overlay(outputId, inputId))
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
      req(input$version2Mta)
      req(input$evaluationUnitsTrait)
      object <- data()
      if(!is.null(object$predictions)){
        phenoNames <- na.omit(unique(object$predictions[which(object$predictions$analysisId %in% input$version2Mta  &  object$predictions$trait == input$evaluationUnitsTrait),"designation"]))
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
    # render sparsity plot
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
    ###################################
    ###################################
    ###################################
    ###################################
    ###################################
    ###################################
    ###################################

    report <- reactiveVal(NULL)

    observeEvent(input$renderReportMta,{
      shinybusy::show_modal_spinner(spin = "fading-circle", text = "Generating Report...")

      result <- data()

      src <- normalizePath(system.file("rmd","reportMtaLMMsolver.Rmd",package="bioflow"))
      src2 <- normalizePath('data/resultMtaLMMsolver.RData')

      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))

      file.copy(src, 'report.Rmd', overwrite = TRUE)
      file.copy(src2, 'resultMtaLMMsolver.RData', overwrite = TRUE)

      outReport <- rmarkdown::render('report.Rmd', params = list(toDownload=TRUE ),
                                     switch("HTML", HTML = rmdformats::robobook(toc_depth = 4)
                                            # HTML = rmarkdown::html_document()
                                     ))

      report(outReport)

      shinybusy::remove_modal_spinner()

      shinyjs::click("downloadReportMta")
    })

    ## render result of "run" button click
    outMta <- eventReactive(input$runMta, {

      req(data())
      req(input$version2Mta)
      req(input$trait2Mta)
      req(input$nTermsFixed)
      req(input$nTermsRandom)

      shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
      dtMta <- data()
      # inputFormulationFixed <- inputFormulaRandom()
      # saveRDS(inputFormulationFixed, file = "inputFormulationFixed.rds")
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

        # qa Geno input
        if(any(c("genoA","genoAD") %in% unique(unlist(inputFormulaCovars())))){ # warning
          if(input$versionMarker2Mta == '' | is.null(input$versionMarker2Mta) ){ # user didn't provide a modifications id
            markerVersionToUse <- NULL
          }else{ markerVersionToUse <- input$versionMarker2Mta} # there is a versionMarker2Mta id
        }else{ markerVersionToUse <- NULL }

        # family distributions input
        myFamily = apply(xx$df,2,function(y){rownames(xx$df)[which(y > 0)[1]]})
        dontHaveDist <- which(is.na(myFamily))
        if(length(dontHaveDist) > 0){myFamily[dontHaveDist] <- "quasi(link = 'identity',variance='constant')"}

        if(nrow(x$df) > 1){myEnvsTI = apply(x$df,2,function(z){z})}else{myEnvsTI <- x$df}

        result1 <- try(
          cgiarPipeline::premetLMMsolver(
            phenoDTfile= dtMta, fixedTerm= inputFormulaFixed(), randomTerm=inputFormulaRandom()
          ),
          silent=TRUE
        )
        if(!inherits(result1,"try-error")) {
          result <- try(
              cgiarPipeline::metLMMsolver(
              phenoDTfile= dtMta, analysisId=input$version2Mta, analysisIdGeno=markerVersionToUse,
              fixedTerm= inputFormulaFixed(),  randomTerm=inputFormulaRandom(), expCovariates=inputFormulaCovars(),
              envsToInclude=myEnvsTI, trait= input$trait2Mta, traitFamily=myFamily,
              useWeights=input$useWeights,estHybrids = as.logical(input$estHybrids),
              calculateSE=as.logical(input$calcSE), heritLB= as.numeric(unlist(strsplit(input$heritLBMet,","))),
              heritUB= as.numeric(unlist(strsplit(input$heritUBMet,","))),
              meanLB = as.numeric(unlist(strsplit(input$meanLBMet,","))),
              meanUB = as.numeric(unlist(strsplit(input$meanUBMet,","))), nPC=inputFormulaPCs(),   # subsetVariable=NULL, subsetVariableLevels=NULL,
              maxIters=input$maxitMet,  verbose=TRUE
            ),
            silent=TRUE
          )
          if(!inherits(result,"try-error")) {
            if("analysisIdName" %in% colnames(result$status)){result$status$analysisIdName[nrow(result$status)] <- input$analysisIdName}
            result2 <- try(
              cgiarPipeline::postmetLMMsolver(phenoDTfile= result, analysisId=result$status$analysisId[length(result$status$analysisId)],
                                              gxeModelNum=result1$gxeModelNum, gxeTerms=result1$gxeTerms),
              silent=TRUE
            )
            if(!inherits(result2,"try-error")) {
              result <- result2
              data(result) # update data with results
              cat(paste("Multi-trial analysis step with id:",as.POSIXct(result$status$analysisId[length(result$status$analysisId)], origin="1970-01-01", tz="GMT"),"saved. Please proceed to construct a selection index using this time stamp."))
              output$outMta2 <- renderPrint({
                cat(paste("Multi-trial analysis step with id:",as.POSIXct(result$status$analysisId[length(result$status$analysisId)], origin="1970-01-01", tz="GMT"),"saved. Please proceed to construct a selection index using this time stamp."))
              })
              updateTabsetPanel(session, "tabsMain", selected = "outputTabs")
            }else{
              cat(paste("Analysis failed with the following error message: \n\n",result2[[1]]))
              output$outMta2 <- renderPrint({
                cat(paste("Analysis failed with the following error message: \n\n",result2[[1]]))
              })
            }
          }else{
            cat(paste("Analysis failed with the following error message: \n\n",result[[1]]))
            output$outMta2 <- renderPrint({
              cat(paste("Analysis failed with the following error message: \n\n",result[[1]]))
            })
          }
        }else{
          cat(paste("Analysis failed with the following error message: \n\n",result1[[1]]))
          output$outMta2 <- renderPrint({
            cat(paste("Analysis failed with the following error message: \n\n",result1[[1]]))
          })
        }
      }

      shinybusy::remove_modal_spinner()

      if(!inherits(result,"try-error")) { # if all goes well in the run
        ## predictions table
        output$predictionsMta <-  DT::renderDT({
          predictions <- result$predictions
          predictions <- predictions[predictions$module=="mtaLmms",]
          predictions$analysisId <- as.numeric(predictions$analysisId)
          predictions <- predictions[!is.na(predictions$analysisId),]
          current.predictions <- predictions[predictions$analysisId==max(predictions$analysisId),]
          current.predictions <- subset(current.predictions, select = -c(module,analysisId))

          # Move "(Intercept)" and "inbreeding" to the top in order
          effect_order <- c("(Intercept)", "inbreeding","environment")
          current.predictions$effectType <- factor(current.predictions$effectType,
                                                   levels = c(effect_order,
                                                              setdiff(unique(current.predictions$effectType), effect_order)))
          # Sort the data by effectType first
          current.predictions <- current.predictions[order(current.predictions$effectType), ]
          numeric.output <- c("predictedValue", "stdError", "reliability")

          DT::formatRound(DT::datatable(current.predictions, extensions = 'Buttons',
                                        options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                       lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
          ), numeric.output)
        }, server = FALSE)
        # metrics table
        output$metricsMta <-  DT::renderDT({
          if(!inherits(result,"try-error") ){
            metrics <- result$metrics
            mtas <- result$status[which(result$status$module == "mtaLmms"),"analysisId"]; mtaId <- mtas[length(mtas)]
            metrics <- metrics[which(metrics$analysisId == mtaId),]
            metrics <- subset(metrics, select = -c(module,analysisId))
            numeric.output <- c("value", "stdError")
            DT::formatRound(DT::datatable(metrics, extensions = 'Buttons',
                                          options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                         lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
            ), numeric.output)
          }
        }, server = FALSE)
        # modeling table
        output$modelingMta <-  DT::renderDT({
          if(!inherits(result,"try-error") ){
            modeling <- result$modeling
            mtas <- result$status[which(result$status$module == "mtaLmms"),"analysisId"]; mtaId <- mtas[length(mtas)]
            modeling <- modeling[which(modeling$analysisId == mtaId),]
            modeling <- subset(modeling, select = -c(module,analysisId))
            DT::datatable(modeling, extensions = 'Buttons',
                          options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                         lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
            )
          }
        }, server = FALSE)
        ## Report tab
        output$reportMta <- renderUI({
          # shiny::withMathJax(HTML(readLines(rmarkdown::render(input = system.file("rmd","reportMtaLMMsolver.Rmd",package="bioflow"),
          #                                                     output_format = rmarkdown::html_fragment(),
          #                                                     params = list(toDownload = FALSE, modelUsed=input$radio),
          #                                                     quiet = TRUE))))
          HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportMtaLMMsolver.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
          # HTML(markdown::markdownToHTML(knitr::knit("./R/reportMta.Rmd", quiet = TRUE), fragment.only=TRUE))
        })

        output$downloadReportMta <- downloadHandler(
          filename = function() {
            paste(paste0('mtaLmms_dashboard_',gsub("-", "", as.integer(Sys.time()))), sep = '.', switch(
              "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
            ))
          },
          content = function(file) {

            out <- report()

            file.rename(out, file)
          }
        )

      } else {
        output$predictionsMta <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
        output$metricsMta <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
        output$modelingMta <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
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
# mod_mtaLMMsolveApp_ui("mtaLMMsolveApp_1")

## To be copied in the server
# mod_mtaLMMsolveApp_server("mtaLMMsolveApp_1")

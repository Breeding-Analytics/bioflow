#' product advancement UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_prodAdvApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    tags$head(
      tags$style(HTML("
        table.decision-table thead th {
          background-color: #2C3E50 !important;
          color: white !important;
          font-weight: 700 !important;
          border-bottom: 2px solid #1B2631 !important;
          white-space: nowrap !important;
        }

        table.decision-table tbody td {
          vertical-align: middle !important;
        }

        div.dataTables_scrollHead table.decision-table,
        div.dataTables_scrollBody table.decision-table {
          margin-top: 0 !important;
          margin-bottom: 0 !important;
        }

        div.dataTables_scrollBody {
          border-bottom: 1px solid #ddd;
        }
      "))
    ),

    shiny::mainPanel(width = 12,
                     tabsetPanel( id=ns("tabsMain"),
                                  type = "tabs",

                                  tabPanel(div(icon("book"), "Information") ,
                                           br(),
                                           column(width = 6,
                                                  h1(strong(span("Product Advancement Selection Module", icon("youtube") , target="_blank")  ,style="color:darkcyan")),
                                                  h2(strong("Data Status (wait to be displayed):")),
                                                  uiOutput(ns("warningMessage")),
                                                  tags$br(),
                                                  # column(width=4, tags$br(),
                                                  shinyWidgets::prettySwitch( inputId = ns('launch'), label = "Load example dataset", status = "success"),
                                                  # ),
                                                  tags$br(),
                                                  img(src = "www/qaRaw.png", height = 200, width = 470), # add an image
                                           ),
                                           column(width = 6,
                                                  tags$body(
                                                    h2(strong("Details")),
                                                    p("The Product Advancement module compiles key information from different sources to support advancement decisions across breeding stages. This option aims to help users review candidate performance in a structured way, combining phenotypic, genetic, and decision-support metrics into a single interface. The way arguments are used is the following:"),
                                                    # column(width = 12, shiny::plotOutput(ns("plotDataDependencies")), ),
                                                  )
                                           ),
                                  ),
                                  tabPanel(div(icon("arrow-right-to-bracket"), "Input steps"),
                                           tabsetPanel(
                                             id = ns("inputStepsTabs"),
                                             tabPanel(
                                               value = "pick_stamps",
                                               div(icon("dice-one"), "Pick analysis stamps", icon("arrow-right")),
                                               br(),

                                               column(
                                                 width = 12,
                                                 style = "background-color:grey; color: #FFFFFF",

                                                 column(
                                                   width = 4,
                                                   selectInput(
                                                     ns("staStamp"),
                                                     label = tags$span(
                                                       "STA version to use",
                                                       tags$i(
                                                         class = "glyphicon glyphicon-info-sign",
                                                         style = "color:#FFFFFF",
                                                         title = "Select the Single Trial Analysis result stamp to use as input for this product advancement workflow."
                                                       )
                                                     ),
                                                     choices = NULL,
                                                     multiple = FALSE
                                                   )
                                                 ),

                                                 column(
                                                   width = 4,
                                                   selectInput(
                                                     ns("mtaStamp"),
                                                     label = tags$span(
                                                       "MTA version to use",
                                                       tags$i(
                                                         class = "glyphicon glyphicon-info-sign",
                                                         style = "color:#FFFFFF",
                                                         title = "Select the Multi Trial Analysis result stamp to use as input for this product advancement workflow."
                                                       )
                                                     ),
                                                     choices = NULL,
                                                     multiple = FALSE
                                                   )
                                                 ),

                                                 column(
                                                   width = 4,
                                                   selectInput(
                                                     ns("indexStamp"),
                                                     label = tags$span(
                                                       "Selection Index version to use (optional)",
                                                       tags$i(
                                                         class = "glyphicon glyphicon-info-sign",
                                                         style = "color:#FFFFFF",
                                                         title = "Select the Selection Index result stamp to use as input for this product advancement workflow."
                                                       )
                                                     ),
                                                     choices = NULL,
                                                     multiple = FALSE
                                                   )
                                                 )
                                               ),

                                               br(),
                                             ),

                                             tabPanel(
                                               value = "global_options",
                                               div(icon("dice-two"), "Global options", icon("arrow-right")),
                                               br(),

                                               column(
                                                 width = 12,
                                                 style = "background-color:grey; color: #FFFFFF",

                                                 column(
                                                   width = 8,
                                                   selectInput(
                                                     ns("traitsToEvaluate"),
                                                     label = tags$span(
                                                       "Traits to evaluate",
                                                       tags$i(
                                                         class = "glyphicon glyphicon-info-sign",
                                                         style = "color:#FFFFFF",
                                                         title = "Select the traits that will be considered in the advancement decision."
                                                       )
                                                     ),
                                                     choices = NULL,
                                                     multiple = TRUE
                                                   )
                                                 ),
                                               ),

                                               br(),

                                               column(
                                                 width = 12,
                                                 style = "background-color:grey; color: #FFFFFF",

                                                 column(
                                                   width = 5,
                                                   selectInput(
                                                     ns("decisionLogic"),
                                                     label = tags$span(
                                                       "Decision logic",
                                                       tags$i(
                                                         class = "glyphicon glyphicon-info-sign",
                                                         style = "color:#FFFFFF",
                                                         title = "Choose how candidate decisions will be combined across traits."
                                                       )
                                                     ),
                                                     choices = c(
                                                       "All traits must pass",
                                                       "At least N traits pass",
                                                       "Weighted index"
                                                     ),
                                                     selected = "All traits must pass",
                                                     multiple = FALSE
                                                   )
                                                 ),

                                                 column(
                                                   width = 3,
                                                   conditionalPanel(
                                                     condition = "input.decisionLogic == 'At least N traits pass'",
                                                     ns = ns,
                                                     numericInput(
                                                       ns("minTraitsPass"),
                                                       label = tags$span(
                                                         "Minimum N traits",
                                                         tags$i(
                                                           class = "glyphicon glyphicon-info-sign",
                                                           style = "color:#FFFFFF",
                                                           title = "Minimum number of traits that must meet their rule for a candidate to be selected."
                                                         )
                                                       ),
                                                       value = 2,
                                                       min = 1,
                                                       step = 1
                                                     )
                                                   )
                                                 )
                                               ),

                                               br(),

                                               conditionalPanel(
                                                 condition = "input.decisionLogic == 'Weighted index'",
                                                 ns = ns,

                                                 column(
                                                   width = 12,
                                                   style = "background-color:grey; color: #FFFFFF",

                                                   column(
                                                     width = 4,
                                                     radioButtons(
                                                       ns("weightSource"),
                                                       label = tags$span(
                                                         "Weight source",
                                                         tags$i(
                                                           class = "glyphicon glyphicon-info-sign",
                                                           style = "color:#FFFFFF",
                                                           title = "Choose whether the weighted index should use desired gains weights or custom user-defined weights."
                                                         )
                                                       ),
                                                       choices = c(
                                                         "Use desired gains weights",
                                                         "Use custom weights"
                                                       ),
                                                       selected = "Use desired gains weights"
                                                     )
                                                   ),

                                                   column(
                                                     width = 3,
                                                     numericInput(
                                                       ns("topPctSelected"),
                                                       label = tags$span(
                                                         "% of top individuals selected",
                                                         tags$i(
                                                           class = "glyphicon glyphicon-info-sign",
                                                           style = "color:#FFFFFF",
                                                           title = "Percentage of top-ranked candidates to retain based on the weighted index."
                                                         )
                                                       ),
                                                       value = 20,
                                                       min = 0,
                                                       max = 100,
                                                       step = 1
                                                     )
                                                   ),

                                                   column(
                                                     width = 5,
                                                     conditionalPanel(
                                                       condition = "input.decisionLogic == 'Weighted index' && input.weightSource == 'Use custom weights'",
                                                       ns = ns,
                                                       uiOutput(ns("customWeightsUI"))
                                                     )
                                                   )
                                                 ),

                                                 br()
                                               ),

                                               column(
                                                 width = 12,
                                                 style = "background-color:grey; color: #FFFFFF",

                                                 column(
                                                   width = 4,
                                                   selectInput(
                                                     ns("checkEntryTypeValue"),
                                                     label = tags$span(
                                                       "Value in entryType that corresponds to checks",
                                                       tags$i(
                                                         class = "glyphicon glyphicon-info-sign",
                                                         style = "color:#FFFFFF",
                                                         title = "Select which value in the entryType column identifies check varieties."
                                                       )
                                                     ),
                                                     choices = NULL,
                                                     multiple = FALSE
                                                   )
                                                 )
                                               ),

                                               br(),
                                             ),

                                             tabPanel(
                                               value = "trait_rules",
                                               div(icon("dice-three"), "Trait-specific rules", icon("arrow-right")),
                                               br(),

                                               column(
                                                 width = 12,
                                                 style = "background-color:grey; color: #FFFFFF; padding-top:15px;",

                                                 column(
                                                   width = 12,
                                                   tags$span(
                                                     "Use this step to specify how each trait contributes to candidate advancement. Trait cards will appear after traits are selected in the Global options tab.",
                                                     tags$i(
                                                       class = "glyphicon glyphicon-info-sign",
                                                       style = "color:#FFFFFF",
                                                       title = "Define one decision rule per selected trait."
                                                     )
                                                   ),
                                                   tags$br(),
                                                   tags$br(),
                                                   checkboxInput(
                                                     ns("applySameRuleToAllTraits"),
                                                     label = "Apply same rules to all traits",
                                                     value = FALSE
                                                   )
                                                 )
                                               ),

                                               br(),

                                               uiOutput(ns("indexOverrideWarning")),

                                               shinydashboard::box(
                                                 width = 12,
                                                 status = "success",
                                                 solidHeader = TRUE,
                                                 collapsible = TRUE,
                                                 collapsed = FALSE,
                                                 title = "Trait rule cards",

                                                 uiOutput(ns("traitRuleCards"))
                                               )
                                             ),

                                             tabPanel(
                                               value = "preselection",
                                               div(icon("dice-four"), "Pre-selection", icon("arrow-right")),
                                               br(),

                                               column(
                                                 width = 12,
                                                 style = "background-color:grey; color: #FFFFFF; padding:15px 0;",

                                                 column(
                                                   width = 12,
                                                   tags$span(
                                                     "To restrict the candidate set before generating the initial recommendation, choose one of the options below."
                                                   ),
                                                   tags$br(),
                                                   tags$br(),

                                                   radioButtons(
                                                     ns("candidateSelectionMode"),
                                                     label = "Candidate set definition",
                                                     choices = c(
                                                       "Use all designations" = "all",
                                                       "Select all designations from a specific selection stage" = "stage",
                                                       "Select candidate designations manually" = "manual"
                                                     ),
                                                     selected = "all"
                                                   )
                                                 )
                                               ),

                                               br(),

                                               column(
                                                 width = 12,
                                                 style = "background-color:grey; color: #FFFFFF; padding:15px 0;",

                                                 column(
                                                   width = 12,

                                                   conditionalPanel(
                                                     condition = "input.candidateSelectionMode == 'stage'",
                                                     ns = ns,

                                                     selectInput(
                                                       ns("selectionStage"),
                                                       label = tags$span(
                                                         "Selection stage",
                                                         tags$i(
                                                           class = "glyphicon glyphicon-info-sign",
                                                           style = "color:#FFFFFF",
                                                           title = "All designations present in the selected stage will be used as candidates."
                                                         )
                                                       ),
                                                       choices = NULL,
                                                       multiple = FALSE
                                                     )
                                                   ),

                                                   conditionalPanel(
                                                     condition = "input.candidateSelectionMode == 'manual'",
                                                     ns = ns,

                                                     selectInput(
                                                       ns("candidateDesignations"),
                                                       label = tags$span(
                                                         "Candidate designations",
                                                         tags$i(
                                                           class = "glyphicon glyphicon-info-sign",
                                                           style = "color:#FFFFFF",
                                                           title = "Select the subset of candidate designations to include in the initial recommendation."
                                                         )
                                                       ),
                                                       choices = NULL,
                                                       multiple = TRUE
                                                     )
                                                   )
                                                 )
                                               )
                                             ),

                                             tabPanel(
                                               value = "run_initial_selection",
                                               div( icon("dice-five"), "Run selection" ),
                                               br(),
                                               column(width=12,style = "background-color:grey; color: #FFFFFF",
                                                      column(width=3,
                                                             tags$div(textInput(ns("initSelectionIdName"),
                                                                                label = tags$span("Analysis Name for initial selection (optional)",
                                                                                                  tags$i( class = "glyphicon glyphicon-info-sign", style = "color:#FFFFFF",
                                                                                                          title = "An optional name for the analysis besides the timestamp if desired.") ),
                                                                                placeholder = "(optional name)")
                                                             )
                                                      ),
                                                      column(width=3,
                                                             br(),
                                                             actionButton(ns("runInitProdAdv"), "Run initial selection", icon = icon("play-circle")),
                                                             br(),
                                                             br(),
                                                      ),
                                               ),

                                               # fluidRow(column(3, verbatimTextOutput(ns("value"))))
                                             ),
                                           ) # end of tabset
                                  ),# end of input panel

                                  tabPanel(
                                    value = "review_output",
                                    div(icon("search-plus",style = "color:#6C5B7B;"), "Review output"),
                                    tabsetPanel(
                                      id = ns("reviewOutputTabs"),
                                      tabPanel(
                                        value = "selection_stamps",
                                        div(icon("tags"), "Selection stamps", icon("arrow-right")),
                                        br(),

                                        column(
                                          width = 12,
                                          style = "background-color:grey; color: #FFFFFF; padding:15px 0;",

                                          column(
                                            width = 12,
                                            tags$span(
                                              "Use this step to load previously saved selection stamps before reviewing plots and final decisions.",
                                              tags$i(
                                                class = "glyphicon glyphicon-info-sign",
                                                style = "color:#FFFFFF",
                                                title = "Choose one initial selection stamp. Optionally, also load one plot selection stamp and one final selection stamp."
                                              )
                                            )
                                          )
                                        ),

                                        br(),

                                        column(
                                          width = 12,
                                          style = "background-color:grey; color: #FFFFFF; padding:15px 0;",

                                          column(
                                            width = 4,
                                            selectInput(
                                              ns("initialSelectionStamp"),
                                              label = tags$span(
                                                "Initial selection stamp",
                                                tags$i(
                                                  class = "glyphicon glyphicon-info-sign",
                                                  style = "color:#FFFFFF",
                                                  title = "Select one previously saved initial selection stamp. This field is mandatory."
                                                )
                                              ),
                                              choices = NULL,
                                              multiple = FALSE
                                            )
                                          ),

                                          column(
                                            width = 4,
                                            selectInput(
                                              ns("plotSelectionStamp"),
                                              label = tags$span(
                                                "Plot selection stamp",
                                                tags$i(
                                                  class = "glyphicon glyphicon-info-sign",
                                                  style = "color:#FFFFFF",
                                                  title = "Optionally select one previously saved plot-adjusted selection stamp."
                                                )
                                              ),
                                              choices = c("No plot selection" = "__none__"),
                                              selected = "",
                                              multiple = FALSE
                                            )
                                          ),

                                          column(
                                            width = 4,
                                            selectInput(
                                              ns("finalSelectionStamp"),
                                              label = tags$span(
                                                "Final selection stamp",
                                                tags$i(
                                                  class = "glyphicon glyphicon-info-sign",
                                                  style = "color:#FFFFFF",
                                                  title = "Optionally select one previously saved final reviewed selection stamp."
                                                )
                                              ),
                                              choices = c("No final selection" = "__none__"),
                                              selected = "",
                                              multiple = FALSE
                                            )
                                          )
                                        ),

                                        br(),

                                        shinydashboard::box(
                                          width = 12,
                                          status = "success",
                                          solidHeader = TRUE,
                                          collapsible = TRUE,
                                          collapsed = TRUE,
                                          title = "Modeling table",

                                          DT::DTOutput(ns("selectionModelingTable"))
                                        )
                                      ),


                                      tabPanel(
                                        div(icon("chart-column"), "Visualisations", icon("arrow-right")),
                                        br(),

                                        column(
                                          width = 12,
                                          style = "background-color:grey; color: #FFFFFF; padding:15px 0;",

                                          column(
                                            width = 12,
                                            tags$span(
                                              "Use this step to select which visualisations to display for reviewing the initial recommendation. Selected vs not-selected candidates will be highlighted in all plots.",
                                              tags$i(
                                                class = "glyphicon glyphicon-info-sign",
                                                style = "color:#FFFFFF",
                                                title = "Choose which interactive plots to generate for candidate review."
                                              )
                                            ),
                                            tags$br(),
                                            tags$br(),

                                            checkboxGroupInput(
                                              ns("reviewPlots"),
                                              label = "Visualisations to display",
                                              choices = c(
                                                "Pair-wise trait scatterplot",
                                                "Beeswarm trait distributions",
                                                "Radar plot",
                                                "Performance heatmap across TPE",
                                                "Per-variety trait performance profile",
                                                "BLUP/BLUE reliability intervals"
                                              ),
                                              selected = NULL
                                            )
                                          )
                                        ),

                                        br(),

                                        conditionalPanel(
                                          condition = "input.reviewPlots && input.reviewPlots.includes('Pair-wise trait scatterplot')",
                                          ns = ns,

                                          column(
                                            width = 12,
                                            style = "background-color:grey; color: #FFFFFF; padding:15px 0;",

                                            column(
                                              width = 4,
                                              uiOutput(ns("scatterXTraitUI"))
                                            ),

                                            column(
                                              width = 4,
                                              uiOutput(ns("scatterYTraitUI"))
                                            )
                                          ),

                                          br()
                                        ),

                                        conditionalPanel(
                                          condition = "input.reviewPlots && input.reviewPlots.includes('Radar plot')",
                                          ns = ns,

                                          column(
                                            width = 12,
                                            style = "background-color:grey; color: #FFFFFF; padding:15px 0;",

                                            column(
                                              width = 6,
                                              uiOutput(ns("radarDesignationUI"))
                                            )
                                          ),

                                          br()
                                        ),

                                        conditionalPanel(
                                          condition = "input.reviewPlots && input.reviewPlots.includes('Performance heatmap across TPE')",
                                          ns = ns,

                                          column(
                                            width = 12,
                                            style = "background-color:grey; color: #FFFFFF; padding:15px 0;",

                                            column(
                                              width = 3,
                                              selectInput(
                                                ns("tpePhase"),
                                                label = tags$span(
                                                  "Growing period phase",
                                                  tags$i(
                                                    class = "glyphicon glyphicon-info-sign",
                                                    style = "color:#FFFFFF",
                                                    title = "Split the selected growing period into an early phase (first 40%) and a late phase (remaining 60%)."
                                                  )
                                                ),
                                                choices = c("Early", "Late"),
                                                selected = "Early"
                                              )
                                            ),

                                            column(
                                              width = 3,
                                              selectInput(
                                                ns("tpeEnvCovariate"),
                                                label = tags$span(
                                                  "Environmental covariate",
                                                  tags$i(
                                                    class = "glyphicon glyphicon-info-sign",
                                                    style = "color:#FFFFFF",
                                                    title = "Choose which summarized environmental covariate to interpolate across the TPE."
                                                  )
                                                ),
                                                choices = c(
                                                  "Mean temperature" = "mean_temperature",
                                                  "Heat stress index" = "heat_stress_index",
                                                  "Rainfall" = "rainfall",
                                                  "Rainfall distribution index" = "rainfall_distribution_index",
                                                  "Humidity" = "humidity"
                                                ),
                                                selected = "rainfall"
                                              )
                                            ),

                                            column(
                                              width = 3,
                                              uiOutput(ns("tpeDesignationUI"))
                                            ),

                                            column(
                                              width = 3,
                                              uiOutput(ns("tpeTraitUI"))
                                            )
                                          ),

                                          br()
                                        ),


                                        conditionalPanel(
                                          condition = "input.reviewPlots && input.reviewPlots.includes('Per-variety trait performance profile')",
                                          ns = ns,
                                          column(
                                            width = 12,
                                            style = "background-color:grey; color: #FFFFFF; padding:15px 0;",

                                            column(
                                              width = 4,
                                              selectInput(
                                                ns("performanceProfileScale"),
                                                label = tags$span(
                                                  "Per-variety profile scale",
                                                  tags$i(
                                                    class = "glyphicon glyphicon-info-sign",
                                                    style = "color:#FFFFFF",
                                                    title = "Choose how performance profiles should be displayed when the per-variety performance plot is selected."
                                                  )
                                                ),
                                                choices = c("% over check", "% over mean"),
                                                selected = "% over check"
                                              )
                                            ),
                                            column(
                                              width = 4,
                                              numericInput(
                                                ns("performanceProfilePage"),
                                                label = tags$span(
                                                  "Page",
                                                  tags$i(
                                                    class = "glyphicon glyphicon-info-sign",
                                                    style = "color:#FFFFFF",
                                                    title = "Each page shows up to 10 designations."
                                                  )
                                                ),
                                                value = 1,
                                                min = 1,
                                                step = 1
                                              )
                                            ),
                                            column(
                                              width = 4,
                                              uiOutput(ns("performanceProfilePageInfo"))
                                            )
                                          )
                                        ),

                                        br(),

                                        conditionalPanel(
                                          condition = "input.reviewPlots && input.reviewPlots.includes('BLUP/BLUE reliability intervals')",
                                          ns = ns,

                                          column(
                                            width = 12,
                                            style = "background-color:grey; color: #FFFFFF; padding:15px 0;",

                                            column(
                                              width = 4,
                                              uiOutput(ns("reliabilityTraitUI"))
                                            )
                                          ),

                                          br()
                                        ),

                                        column(
                                          width = 12,
                                          style = "background-color:grey; color: #FFFFFF; padding:15px 0;",

                                          column(
                                            width = 12,
                                            actionButton(
                                              ns("renderReviewPlots"),
                                              "Render selected plots",
                                              icon = icon("play-circle")
                                            )
                                          )
                                        ),

                                        br(),

                                        shinydashboard::box(
                                          width = 12,
                                          status = "success",
                                          solidHeader = TRUE,
                                          collapsible = TRUE,
                                          collapsed = FALSE,
                                          title = "Interactive visualisations",

                                          uiOutput(ns("reviewPlotsUI"))
                                        ),

                                        br(),

                                        shinydashboard::box(
                                          width = 12,
                                          status = "warning",
                                          solidHeader = TRUE,
                                          collapsible = TRUE,
                                          collapsed = FALSE,
                                          title = "Manual designation selection",

                                          column(
                                            width = 6,
                                            uiOutput(ns("manualDesignationSelectionUI"))
                                          ),

                                          column(
                                            width = 3,
                                            selectInput(
                                              ns("manualDesignationDecision"),
                                              "Assign decision",
                                              choices = c("SELECTED", "NOT SELECTED"),
                                              selected = "SELECTED",
                                              multiple = FALSE
                                            )
                                          ),

                                          column(
                                            width = 3,
                                            br(),
                                            actionButton(
                                              ns("applyManualDesignationDecision"),
                                              "Apply decision",
                                              icon = icon("check")
                                            ),
                                          )
                                        ),

                                        br(),

                                        column(
                                          width = 12,
                                          style = "background-color:grey; color: #FFFFFF; padding:15px 0;",

                                          column(
                                            width = 4,
                                            textInput(
                                              ns("plotSelectionId"),
                                              label = tags$span(
                                                "Plot selection name",
                                                tags$i(
                                                  class = "glyphicon glyphicon-info-sign",
                                                  style = "color:#FFFFFF",
                                                  title = "Assign a name to the current plot-adjusted selection."
                                                )
                                              ),
                                              placeholder = "e.g. LS_review_plot_v1"
                                            )
                                          ),

                                          column(
                                            width = 3,
                                            br(),
                                            actionButton(
                                              ns("savePlotSelection"),
                                              "Save selection",
                                              icon = icon("save")
                                            )
                                          )
                                        )
                                      ),

                                      tabPanel(
                                        div(icon("table"), "Decision table", icon("arrow-right")),
                                        br(),

                                        column(
                                          width = 12,
                                          style = "background-color:grey; color: #FFFFFF; padding:15px 0;",

                                          column(
                                            width = 12,
                                            tags$span(
                                              "Use this step to review candidate-level decisions. The table will display initial input status, plot-adjusted status, and a final decision that can override previous layers.",
                                              tags$i(
                                                class = "glyphicon glyphicon-info-sign",
                                                style = "color:#FFFFFF",
                                                title = "Final candidate decisions can be reviewed and manually adjusted here."
                                              )
                                            )
                                          )
                                        ),

                                        br(),

                                        shinydashboard::box(
                                          width = 12,
                                          status = "success",
                                          solidHeader = TRUE,
                                          collapsible = TRUE,
                                          collapsed = FALSE,
                                          title = "Decision table",

                                          DT::DTOutput(ns("decisionTable"))
                                        ),

                                        br(),

                                        column(
                                          width = 12,
                                          style = "background-color:grey; color: #FFFFFF; padding:15px 0;",

                                          column(
                                            width = 4,
                                            textInput(
                                              ns("finalSelectionId"),
                                              label = tags$span(
                                                "Final selection name",
                                                tags$i(
                                                  class = "glyphicon glyphicon-info-sign",
                                                  style = "color:#FFFFFF",
                                                  title = "Assign a name to the final reviewed decision set."
                                                )
                                              ),
                                              placeholder = "e.g. LS_final_selection_v1"
                                            )
                                          ),

                                          column(
                                            width = 3,
                                            br(),
                                            actionButton(
                                              ns("saveFinalSelection"),
                                              "Save selection",
                                              icon = icon("save")
                                            )
                                          )
                                        )
                                      ),

                                      tabPanel(
                                        div(icon("play-circle"), "Run selection"),
                                        br(),

                                        column(
                                          width = 12,
                                          style = "background-color:grey; color: #FFFFFF; padding:15px 0;",

                                          column(
                                            width = 4,
                                            selectInput(
                                              ns("reportInitialSelectionStamp"),
                                              label = "Initial selection stamp for report",
                                              choices = NULL,
                                              multiple = FALSE
                                            )
                                          ),

                                          column(
                                            width = 4,
                                            selectInput(
                                              ns("reportPlotSelectionStamp"),
                                              label = "Plot selection stamp for report",
                                              choices = c("No plot selection" = "__none__"),
                                              selected = "__none__",
                                              multiple = FALSE
                                            )
                                          ),

                                          column(
                                            width = 4,
                                            selectInput(
                                              ns("reportFinalSelectionStamp"),
                                              label = "Final selection stamp for report",
                                              choices = c("No final selection" = "__none__"),
                                              selected = "__none__",
                                              multiple = FALSE
                                            )
                                          )
                                        ),

                                        br(),

                                        column(
                                          width = 12,
                                          style = "background-color:grey; color: #FFFFFF",
                                          column(
                                            width = 3,
                                            br(),
                                            actionButton(
                                              ns("runFinalProdAdv"),
                                              "Generate final report",
                                              icon = icon("file-lines")
                                            ),
                                            br(),
                                            br()
                                          )
                                        ),

                                        textOutput(ns("outProdAdv_Raw"))
                                      ),
                                    )
                                  ),

                                  tabPanel(div(icon("arrow-right-from-bracket"), "Output tabs" ) , value = "outputTabs",
                                           tabsetPanel(
                                             tabPanel("Dashboard", icon = icon("file-image"),
                                                      br(),
                                                      textOutput(ns("outProdAdv_Raw2")),
                                                      br(),
                                                      actionButton(ns("renderReportProdAdv"), "Download dashboard", icon = icon("download")),
                                                      downloadButton(ns("downloadReportProdAdv"), "Download dashboard", style = "visibility:hidden;"),
                                                      br(),
                                                      uiOutput(ns('reportProdAdv'))
                                             ),
                                           ),
                                  ),
                     )) # end mainpanel

  )
}

#' qaRawApp Server Functions
#'
#' @noRd
mod_prodAdvApp_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #############################################################################
    #Helper functions
    sanitize_trait_id <- function(x) {
      gsub("[^A-Za-z0-9_]", "_", x)
    }

    parse_weather_time <- function(x, tz = "UTC") {
      x <- as.character(x)
      x <- trimws(x)
      x[x == ""] <- NA_character_

      x_clean <- sub(" GMT$", "", x)

      out <- rep(as.POSIXct(NA, tz = tz), length(x_clean))

      formats <- c(
        "%Y-%m-%d %H:%M:%S",     # after removing GMT
        "%Y-%m-%d %H:%M",
        "%Y-%m-%dT%H:%M:%S%z",
        "%Y-%m-%dT%H:%M:%SZ",
        "%Y-%m-%dT%H:%M:%S",
        "%m/%d/%Y %H:%M:%S",
        "%m/%d/%Y %H:%M",
        "%d/%m/%Y %H:%M:%S",
        "%d/%m/%Y %H:%M"
      )

      for (fmt in formats) {
        idx <- which(!is.na(x_clean) & is.na(out))
        if (length(idx) == 0) break

        parsed <- suppressWarnings(
          as.POSIXct(x_clean[idx], format = fmt, tz = tz)
        )

        ok <- !is.na(parsed)
        if (any(ok)) {
          out[idx[ok]] <- parsed[ok]
        }
      }

      out
    }

    parse_weather_date <- function(x) {
      x <- as.character(x)
      x <- trimws(x)
      x[x == ""] <- NA_character_

      parsed <- suppressWarnings(
        as.Date(
          x,
          tryFormats = c(
            "%Y-%m-%d",
            "%m/%d/%Y",
            "%d/%m/%Y",
            "%Y/%m/%d"
          )
        )
      )

      parsed
    }

    make_stamp_choices <- function(df) {
      stamps <- unique(df$analysisId)

      if (length(stamps) == 0) {
        return(character(0))
      }

      stamps_chr <- as.character(stamps)

      if ("analysisIdName" %in% colnames(df)) {
        lbl_df <- unique(df[, c("analysisId", "analysisIdName"), drop = FALSE])
        lbl_df$analysisId <- as.character(lbl_df$analysisId)

        labels <- paste(
          lbl_df$analysisIdName,
          as.POSIXct(as.numeric(lbl_df$analysisId), origin = "1970-01-01", tz = "GMT"),
          sep = "_"
        )

        out <- stamps_chr
        names(out) <- labels[match(stamps_chr, lbl_df$analysisId)]
        return(out)
      } else {
        out <- stamps_chr
        names(out) <- as.character(as.POSIXct(as.numeric(stamps_chr), origin = "1970-01-01", tz = "GMT"))
        return(out)
      }
    }


    build_decision_table_data <- function(dt,
                                          initial_stamp,
                                          plot_stamp = "__none__",
                                          final_stamp = "__none__",
                                          final_overrides = NULL) {

      base_df <- cgiarPipeline::build_prodadv_decision_table_data(
        dt = dt,
        initial_stamp = initial_stamp,
        plot_stamp = plot_stamp,
        final_stamp = final_stamp,
        final_overrides = final_overrides
      )

      selected_traits <- grep("_trait_decision$", colnames(base_df), value = TRUE)
      selected_traits <- sub("_trait_decision$", "", selected_traits)

      display_df <- data.frame(
        designation = base_df$designation,
        stringsAsFactors = FALSE
      )

      for (t in selected_traits) {
        display_df[[t]] <- mapply(
          make_value_badge,
          base_df[[t]],
          base_df[[paste0(t, "_trait_decision")]],
          SIMPLIFY = TRUE,
          USE.NAMES = FALSE
        )
      }

      display_df$initial_decision <- sapply(
        base_df$initial_decision,
        make_decision_badge
      )

      display_df$plot_decision <- sapply(
        base_df$plot_decision,
        make_decision_badge
      )

      display_df$final_decision <- mapply(
        make_final_cell,
        base_df$designation,
        base_df$final_decision_initial,
        base_df$initial_decision,
        SIMPLIFY = TRUE,
        USE.NAMES = FALSE
      )

      list(
        raw = base_df,
        display = display_df
      )
    }



    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })
    ############################################################################
    # show shinyWidgets until the user can use the module
    observeEvent(c(data(), input$staStamp, input$mtaStamp, input$traitsToEvaluate), {
      req(data())
      mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
      if(mappedColumns == 3 & length(input$staStamp)>0 & length(input$mtaStamp)>0 & length(input$traitsToEvaluate)){
        golem::invoke_js('showid', ns('holder1'))
      }else{
        golem::invoke_js('hideid', ns('holder1'))
      }
    })
    ############################################################################
    #Information Tab
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

    # warning message
    output$warningMessage <- renderUI(
      if(is.null(data())){
        HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your phenotypic data using the 'Data Retrieval' tab.")) )
      }else{ # data is there
        mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
        if(mappedColumns == 3){
          if( any( c("mta","mtaAsr","mtaFlex","mtaLmms","mas") %in% data()$status$module ) ){
            HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to the Input tabs.")) )
          }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please perform a Multi-Trial Analysis before Product Advancement")) ) }
        }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please make sure that you have computed the 'environment' column, and that column 'designation' and \n at least one trait have been mapped using the 'Data Retrieval' tab.")) )}
      }
    )

    ############################################################################
    #Input Tabs
    #################


    ########################################
    #Select stamps (idx1)
    ########################################

    observeEvent(c(data()), {
      req(data())
      dt <- data()
      dt <- dt$status

      dtSta <- dt[which(dt$module == "sta"),]
      stampsSta <- unique(dtSta$analysisId)

      dtMta <- dt[which(dt$module %in% c("mta","mtaAsr","mtaFlex","mtaLmms","mas") ),]
      stampsMta <- unique(dtMta$analysisId)

      dtIdxD <- dt[which(dt$module %in% c("indexD")),]
      stampsIdxD <- unique(dtIdxD$analysisId)

      if(length(stampsSta) > 0){
        if("analysisIdName" %in% colnames(dtSta)){
          names(stampsSta) <- paste(dtSta$analysisIdName, as.POSIXct(stampsSta, origin="1970-01-01", tz="GMT"), sep = "_")
        }else{
          names(stampsSta) <- as.character(as.POSIXct(stampsSta, origin="1970-01-01", tz="GMT"))
        }
      }

      if(length(stampsMta) > 0){
        if("analysisIdName" %in% colnames(dtMta)){
          names(stampsMta) <- paste(dtMta$analysisIdName, as.POSIXct(stampsMta, origin="1970-01-01", tz="GMT"), sep = "_")
        }else{
          names(stampsMta) <- as.character(as.POSIXct(stampsMta, origin="1970-01-01", tz="GMT"))
        }
      }

      if(length(stampsIdxD) > 0){
        if("analysisIdName" %in% colnames(dtIdxD)){
          names(stampsIdxD) <- paste(dtIdxD$analysisIdName, as.POSIXct(stampsIdxD, origin="1970-01-01", tz="GMT"), sep = "_")
        }else{
          names(stampsIdxD) <- as.character(as.POSIXct(stampsIdxD, origin="1970-01-01", tz="GMT"))
        }
      }

      updateSelectInput(session, "staStamp", choices = stampsSta)
      updateSelectInput(session, "mtaStamp", choices = stampsMta)
      updateSelectInput(session, "indexStamp", choices = stampsIdxD)
    })

    ########################################
    #Global options (idx2)
    ########################################

    observeEvent(c(data(), input$mtaStamp), {
      req(data())
      req(input$mtaStamp)
      dt <- data()
      dtPred <- dt$predictions
      dtPred <- dtPred[which(dtPred$analysisId %in% input$mtaStamp),]
      traitsProdAdv <- unique(dtPred$trait)
      updateSelectInput(session, "traitsToEvaluate", choices = traitsProdAdv)
    })

    output$customWeightsUI <- renderUI({
      req(input$traitsToEvaluate)
      req(length(input$traitsToEvaluate) > 0)

      trait_list <- input$traitsToEvaluate

      tagList(
        tags$div(
          style = "margin-top: 5px;",
          tags$strong("Custom trait weights")
        ),
        tags$div(
          style = "font-size: 12px; margin-bottom: 10px;",
          "Specify one weight per selected trait."
        ),
        lapply(trait_list, function(trait) {
          safe_trait <- gsub("[^A-Za-z0-9_]", "_", trait)

          numericInput(
            inputId = ns(paste0("weight_", safe_trait)),
            label = trait,
            value = 1,
            step = 0.1
          )
        })
      )
    })

    observe({
      req(data())
      dt <- data()

      entryType_column <- dt$metadata$pheno
      entryType_column <- entryType_column[entryType_column$parameter == "entryType","value"]

      entry_type_values <- dt$data$pheno[,entryType_column]
      entry_type_values <- unique(entry_type_values[!is.na(entry_type_values)])

      updateSelectInput(
        session,
        "checkEntryTypeValue",
        choices = entry_type_values,
        selected = entry_type_values[1]
      )
    })

    observe({
      req(input$traitsToEvaluate)
      req(identical(input$decisionLogic, "At least N traits pass"))

      n_traits <- length(input$traitsToEvaluate)
      current_val <- input$minTraitsPass

      if (is.null(current_val) || !is.finite(current_val)) {
        current_val <- min(1, n_traits)
      }

      updateNumericInput(
        session = session,
        inputId = "minTraitsPass",
        min = 1,
        max = n_traits,
        value = min(current_val, n_traits)
      )
    })

    ########################################
    #Trait-specific rules (idx3)
    ########################################

    output$indexOverrideWarning <- renderUI({
      req(input$candidateSelectionMode)

      if (input$decisionLogic == "Weighted index") {

        div(
          style = "
        background-color: #fff3cd;
        color: #856404;
        border-left: 5px solid #ffeeba;
        padding: 12px;
        margin-bottom: 15px;
        border-radius: 4px;
      ",

          tags$strong("Warning: "),
          "Trait-specific rules override the weighted index. ",
          "If you define a rule (e.g. minimum threshold), candidates failing that rule will be excluded ",
          "even if they have high index values. ",
          tags$br(),
          tags$br(),
          "Use rules only when you want to enforce hard constraints on specific traits."
        )
      }
    })

    output$traitRuleCards <- renderUI({
      req(input$traitsToEvaluate)
      req(length(input$traitsToEvaluate) > 0)

      trait_list <- input$traitsToEvaluate

      tagList(
        lapply(trait_list, function(trait_name) {
          safe_trait <- gsub("[^A-Za-z0-9_]", "_", trait_name)

          shinydashboard::box(
            width = 12,
            title = trait_name,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,

            selectInput(
              ns(paste0("ruleType_", safe_trait)),
              "Rule type",
              choices = c(
                "Threshold",
                "Acceptable range",
                "% over check",
                "% over mean"
              ),
              selected = "Threshold"
            ),

            conditionalPanel(
              condition = sprintf(
                "input['%s'] != 'Acceptable range'",
                paste0("ruleType_", safe_trait)
              ),
              ns = ns,
              selectInput(
                ns(paste0("direction_", safe_trait)),
                "Direction",
                choices = c("Higher is better", "Lower is better"),
                selected = "Higher is better"
              )
            ),

            uiOutput(ns(paste0("ruleInputs_", safe_trait)))
          )
        })
      )
    })

    get_digits_from_range <- function(rng) {
      range_width <- diff(rng)

      if (!is.finite(range_width) || range_width <= 0) {
        return(3)
      }

      e <- floor(log10(range_width))

      if (e < 0) return(3)   # range < 1
      if (e == 0) return(2)  # range on 10^0 scale
      if (e == 1) return(1)  # range on 10^1 scale
      return(0)              # range on 10^2 or larger
    }

    get_step_from_digits <- function(digits) {
      switch(
        as.character(digits),
        "3" = 0.001,
        "2" = 0.01,
        "1" = 0.1,
        "0" = 1,
        0.001
      )
    }

    observe({
      req(input$traitsToEvaluate)
      trait_list <- input$traitsToEvaluate

      get_digits_from_range <- function(rng) {
        range_width <- diff(rng)

        if (!is.finite(range_width) || range_width <= 0) {
          return(3)
        }

        e <- floor(log10(range_width))

        if (e < 0) return(3)
        if (e == 0) return(2)
        if (e == 1) return(1)
        return(0)
      }

      get_step_from_digits <- function(digits) {
        switch(
          as.character(digits),
          "3" = 0.001,
          "2" = 0.01,
          "1" = 0.1,
          "0" = 1,
          0.001
        )
      }

      lapply(trait_list, function(trait_name) {
        safe_trait <- sanitize_trait_id(trait_name)

        output[[paste0("ruleInputs_", safe_trait)]] <- renderUI({
          rule_type <- input[[paste0("ruleType_", safe_trait)]]
          req(rule_type)

          req(data())
          req(input$mtaStamp)

          dt <- data()
          dtPred <- dt$predictions
          dtPred <- dtPred[which(dtPred$analysisId %in% input$mtaStamp), ]
          dtPred_trait <- dtPred[dtPred$trait == trait_name & dtPred$effectType == "designation", ]
          predictedValue <- dtPred_trait$predictedValue[!is.na(dtPred_trait$predictedValue)]

          req(length(predictedValue) > 0)

          rng <- range(predictedValue, na.rm = TRUE)
          req(all(is.finite(rng)))

          digits <- get_digits_from_range(rng)
          slider_step <- get_step_from_digits(digits)

          rng_min <- round(rng[1], digits)
          rng_max <- round(rng[2], digits)

          if (!is.finite(slider_step) || slider_step <= 0) {
            slider_step <- get_step_from_digits(3)
          }

          if (rule_type == "Threshold") {
            tagList(
              sliderInput(
                ns(paste0("minThresholdSlider_", safe_trait)),
                "Threshold value",
                min = rng_min,
                max = rng_max,
                value = rng_min,
                step = slider_step
              ),
              numericInput(
                ns(paste0("minThreshold_", safe_trait)),
                "Threshold value",
                value = rng_min,
                min = rng_min,
                max = rng_max,
                step = slider_step
              )
            )

          } else if (rule_type == "Acceptable range") {
            tagList(
              sliderInput(
                ns(paste0("rangeSlider_", safe_trait)),
                "Acceptable range",
                min = rng_min,
                max = rng_max,
                value = c(rng_min, rng_max),
                step = slider_step
              ),
              fluidRow(
                column(
                  width = 6,
                  numericInput(
                    ns(paste0("rangeMin_", safe_trait)),
                    "Minimum value",
                    value = rng_min,
                    min = rng_min,
                    max = rng_max,
                    step = slider_step
                  )
                ),
                column(
                  width = 6,
                  numericInput(
                    ns(paste0("rangeMax_", safe_trait)),
                    "Maximum value",
                    value = rng_max,
                    min = rng_min,
                    max = rng_max,
                    step = slider_step
                  )
                )
              )
            )

          } else if (rule_type == "% over check") {

            if(!is.null(input$checkEntryTypeValue)){
              check_designations <- dtPred[dtPred$entryType == input$checkEntryTypeValue,"designation"]
              check_designations <- unique(check_designations[!is.na(check_designations)])
            }else{
              check_designations <- "No entryType value selected"
            }


            tagList(
              selectInput(
                ns(paste0("checkVar_", safe_trait)),
                "Reference check",
                choices = check_designations,
                multiple = FALSE
              ),
              sliderInput(
                ns(paste0("pctOverCheckSlider_", safe_trait)),
                "% over check",
                min = 0,
                max = 100,
                value = 0,
                step = 1
              ),
              numericInput(
                ns(paste0("pctOverCheck_", safe_trait)),
                "% over check",
                value = 0,
                min = 0,
                max = 100,
                step = 1
              )
            )

          } else if (rule_type == "% over mean") {
            tagList(
              sliderInput(
                ns(paste0("pctOverMeanSlider_", safe_trait)),
                "% over mean",
                min = 0,
                max = 100,
                value = 0,
                step = 1
              ),
              numericInput(
                ns(paste0("pctOverMean_", safe_trait)),
                "% over mean",
                value = 0,
                min = 0,
                max = 100,
                step = 1
              )
            )
          }
        })

        observeEvent(input[[paste0("minThresholdSlider_", safe_trait)]], ignoreInit = TRUE, {
          val <- input[[paste0("minThresholdSlider_", safe_trait)]]
          updateNumericInput(
            session,
            paste0("minThreshold_", safe_trait),
            value = val
          )
        })

        observeEvent(input[[paste0("minThreshold_", safe_trait)]], ignoreInit = TRUE, {
          val <- input[[paste0("minThreshold_", safe_trait)]]
          updateSliderInput(
            session,
            paste0("minThresholdSlider_", safe_trait),
            value = val
          )
        })

        observeEvent(input[[paste0("rangeSlider_", safe_trait)]], ignoreInit = TRUE, {
          val <- input[[paste0("rangeSlider_", safe_trait)]]
          updateNumericInput(
            session,
            paste0("rangeMin_", safe_trait),
            value = val[1]
          )
          updateNumericInput(
            session,
            paste0("rangeMax_", safe_trait),
            value = val[2]
          )
        })

        observeEvent(input[[paste0("rangeMin_", safe_trait)]], ignoreInit = TRUE, {
          min_val <- input[[paste0("rangeMin_", safe_trait)]]
          max_val <- input[[paste0("rangeMax_", safe_trait)]]

          req(!is.null(min_val), !is.null(max_val))

          updateSliderInput(
            session,
            paste0("rangeSlider_", safe_trait),
            value = c(min(min_val, max_val), max(min_val, max_val))
          )
        })

        observeEvent(input[[paste0("rangeMax_", safe_trait)]], ignoreInit = TRUE, {
          min_val <- input[[paste0("rangeMin_", safe_trait)]]
          max_val <- input[[paste0("rangeMax_", safe_trait)]]

          req(!is.null(min_val), !is.null(max_val))

          updateSliderInput(
            session,
            paste0("rangeSlider_", safe_trait),
            value = c(min(min_val, max_val), max(min_val, max_val))
          )
        })

        observeEvent(input[[paste0("pctOverCheckSlider_", safe_trait)]], ignoreInit = TRUE, {
          val <- input[[paste0("pctOverCheckSlider_", safe_trait)]]
          updateNumericInput(
            session,
            paste0("pctOverCheck_", safe_trait),
            value = val
          )
        })

        observeEvent(input[[paste0("pctOverCheck_", safe_trait)]], ignoreInit = TRUE, {
          val <- input[[paste0("pctOverCheck_", safe_trait)]]
          updateSliderInput(
            session,
            paste0("pctOverCheckSlider_", safe_trait),
            value = val
          )
        })

        observeEvent(input[[paste0("pctOverMeanSlider_", safe_trait)]], ignoreInit = TRUE, {
          val <- input[[paste0("pctOverMeanSlider_", safe_trait)]]
          updateNumericInput(
            session,
            paste0("pctOverMean_", safe_trait),
            value = val
          )
        })

        observeEvent(input[[paste0("pctOverMean_", safe_trait)]], ignoreInit = TRUE, {
          val <- input[[paste0("pctOverMean_", safe_trait)]]
          updateSliderInput(
            session,
            paste0("pctOverMeanSlider_", safe_trait),
            value = val
          )
        })

        observeEvent(input[[paste0("direction_", safe_trait)]], ignoreInit = TRUE, {
          req(input[[paste0("ruleType_", safe_trait)]] == "Threshold")

          direction <- input[[paste0("direction_", safe_trait)]]
          req(direction)

          req(data())
          req(input$mtaStamp)

          dt <- data()
          dtPred <- dt$predictions
          dtPred <- dtPred[which(dtPred$analysisId %in% input$mtaStamp), ]
          dtPred_trait <- dtPred[
            dtPred$trait == trait_name & dtPred$effectType == "designation",
          ]
          predictedValue <- dtPred_trait$predictedValue[!is.na(dtPred_trait$predictedValue)]

          req(length(predictedValue) > 0)

          rng <- range(predictedValue, na.rm = TRUE)
          req(all(is.finite(rng)))

          digits <- get_digits_from_range(rng)
          slider_step <- get_step_from_digits(digits)

          rng_min <- round(rng[1], digits)
          rng_max <- round(rng[2], digits)

          new_value <- if (direction == "Lower is better") rng_max else rng_min

          updateSliderInput(
            session,
            paste0("minThresholdSlider_", safe_trait),
            value = new_value
          )

          updateNumericInput(
            session,
            paste0("minThreshold_", safe_trait),
            value = new_value
          )
        })

      })
    })

    ########################################
    #Pre-selection (idx4)
    ########################################

    observe({
      req(data())

      dt <- data()

      #Get unique stages in data
      dt_pheno <- dt$data$pheno
      mt_dt <- dt$metadata$pheno
      stage_col <- mt_dt$value[mt_dt$parameter == "stage"]
      stages <- unique(dt_pheno[,stage_col])

      #Get unique designations available
      dtPred <- dt$predictions
      dtPred <- dtPred[which(dtPred$analysisId %in% input$mtaStamp), ]
      mta_candidates <- dtPred[dtPred$effectType == "designation" & dtPred$entryType != input$checkEntryTypeValue, "designation"]
      mta_candidates <- unique(mta_candidates)


      updateSelectInput(
        session = session,
        inputId = "selectionStage",
        choices = stages
      )

      updateSelectInput(
        session = session,
        inputId = "candidateDesignations",
        choices = mta_candidates
      )
    })


    selected_candidates <- reactive({
      req(data())

      dt <- data()
      mode <- input$candidateSelectionMode

      #Candidates when selecting all
      dtPred <- dt$predictions
      dtPred <- dtPred[which(dtPred$analysisId %in% input$mtaStamp), ]
      mta_candidates <- dtPred[dtPred$effectType == "designation" & dtPred$entryType != input$checkEntryTypeValue, "designation"]
      mta_candidates <- unique(mta_candidates)

      if (mode == "all") {
        return(mta_candidates)
      }

      if (mode == "stage") {
        req(input$selectionStage)

        dt_pheno <- dt$data$pheno
        mt_dt <- dt$metadata$pheno
        stage_col <- mt_dt$value[mt_dt$parameter == "stage"]
        designation_col <- mt_dt$value[mt_dt$parameter == "designation"]

        stage_candidates <- dt_pheno[dt_pheno[,stage_col]==input$selectionStage,designation_col]
        stage_candidates <- unique(stage_candidates)
        stage_candidates <- stage_candidates[stage_candidates %in% mta_candidates]

        return(stage_candidates)
      }

      if (mode == "manual") {
        req(input$candidateDesignations)
        return(input$candidateDesignations)
      }
    })

    ###########################################
    #Run initial selection (idx5)
    ###########################################

    trait_rules_input <- reactive({
      req(input$traitsToEvaluate)
      req(length(input$traitsToEvaluate) > 0)

      out <- lapply(input$traitsToEvaluate, function(trait_name) {
        safe_trait <- sanitize_trait_id(trait_name)
        rule_type <- input[[paste0("ruleType_", safe_trait)]]

        rule_list <- list(
          trait = trait_name,
          ruleType = rule_type
        )

        if (!is.null(rule_type) && rule_type != "Acceptable range") {
          rule_list$direction <- input[[paste0("direction_", safe_trait)]]
        } else {
          rule_list$direction <- NULL
        }

        if (identical(rule_type, "Threshold")) {
          rule_list$threshold <- input[[paste0("minThreshold_", safe_trait)]]
        }

        if (identical(rule_type, "Acceptable range")) {
          rule_list$minValue <- input[[paste0("rangeMin_", safe_trait)]]
          rule_list$maxValue <- input[[paste0("rangeMax_", safe_trait)]]
        }

        if (identical(rule_type, "% over check")) {
          rule_list$referenceCheck <- input[[paste0("checkVar_", safe_trait)]]
          rule_list$threshold <- input[[paste0("pctOverCheck_", safe_trait)]]
        }

        if (identical(rule_type, "% over mean")) {
          rule_list$threshold <- input[[paste0("pctOverMean_", safe_trait)]]
        }

        rule_list
      })

      names(out) <- input$traitsToEvaluate
      out
    })

    custom_weights_input <- reactive({
      req(input$traitsToEvaluate)

      if (!identical(input$decisionLogic, "Weighted index")) {
        return(NULL)
      }

      if (!identical(input$weightSource, "Use custom weights")) {
        return(NULL)
      }

      out <- sapply(input$traitsToEvaluate, function(trait_name) {
        safe_trait <- sanitize_trait_id(trait_name)
        input[[paste0("weight_", safe_trait)]]
      }, simplify = TRUE, USE.NAMES = TRUE)

      out
    })

    initial_selection_args <- reactive({
      req(input$staStamp)
      req(input$mtaStamp)
      req(input$traitsToEvaluate)
      req(length(input$traitsToEvaluate) > 0)
      req(input$decisionLogic)


      if (identical(input$decisionLogic, "At least N traits pass")) {
        req(input$minTraitsPass)
      }

      if (identical(input$decisionLogic, "Weighted index")) {
        req(input$topPctSelected)
      }

      list(
        analysisIdName = if (nzchar(trimws(input$initSelectionIdName))) trimws(input$initSelectionIdName) else NULL,
        staStamp = input$staStamp,
        mtaStamp = input$mtaStamp,
        indexStamp = if (length(input$indexStamp) > 0 && nzchar(input$indexStamp)) input$indexStamp else NULL,
        traitsToEvaluate = input$traitsToEvaluate,
        decisionLogic = input$decisionLogic,
        minTraitsPass = if (identical(input$decisionLogic, "At least N traits pass")) input$minTraitsPass else NULL,
        weightSource = if (identical(input$decisionLogic, "Weighted index")) input$weightSource else NULL,
        topPctSelected = if (identical(input$decisionLogic, "Weighted index")) input$topPctSelected else NULL,
        customWeights = custom_weights_input(),
        checkEntryTypeValue = input$checkEntryTypeValue,
        applySameRuleToAllTraits = isTRUE(input$applySameRuleToAllTraits),
        traitRules = trait_rules_input(),
        candidateSelectionMode = input$candidateSelectionMode,
        selectionStage = if (identical(input$candidateSelectionMode, "stage")) input$selectionStage else NULL,
        candidateDesignations = if (identical(input$candidateSelectionMode, "manual")) input$candidateDesignations else NULL,
        selectedCandidates = selected_candidates()
      )
    })

    observeEvent(input$runInitProdAdv, {

      args <- initial_selection_args()

      shinybusy::show_modal_spinner(
        spin = "fading-circle",
        text = "Running initial product advancement selection..."
      )

      dt_object <- data()

      result <- tryCatch({
        cgiarPipeline::runInitialProdAdv(analysisId = as.numeric(Sys.time()),
                          analysisIdName = args$analysisIdName,
                          args = args,
                          dt_object = dt_object)
      }, error = function(e) {
        shinybusy::remove_modal_spinner()
        showNotification(
          paste("Initial selection failed:", e$message),
          type = "error",
          duration = NULL
        )
        return(NULL)
      })

      shinybusy::remove_modal_spinner()

      req(result)
      data(result)

      showNotification(
        "Initial selection completed successfully.",
        type = "message"
      )

      # Switch top-level tab to Review output
      updateTabsetPanel(
        session = session,
        inputId = "tabsMain",
        selected = "review_output"
      )

      # Switch inner Review output tab to Selection stamps
      updateTabsetPanel(
        session = session,
        inputId = "reviewOutputTabs",
        selected = "selection_stamps"
      )
    })

    ############################################################################
    #Review Tabs
    #################

    ###########################################
    #Selection stamps (idx6)
    ###########################################

    observeEvent(c(data()), {
      req(data())
      dt <- data()$status

      dtSta <- dt[dt$module == "sta", , drop = FALSE]
      dtMta <- dt[dt$module %in% c("mta","mtaAsr","mtaFlex","mtaLmms","mas"), , drop = FALSE]
      dtIdxD <- dt[dt$module %in% c("indexD"), , drop = FALSE]

      dtInitSel  <- dt[dt$module == "Init_prodAdv",  , drop = FALSE]
      dtPlotSel  <- dt[dt$module == "Plot_prodAdv",  , drop = FALSE]
      dtFinalSel <- dt[dt$module == "Final_prodAdv", , drop = FALSE]

      stampsSta      <- make_stamp_choices(dtSta)
      stampsMta      <- make_stamp_choices(dtMta)
      stampsIdxD     <- make_stamp_choices(dtIdxD)
      stampsInitSel  <- make_stamp_choices(dtInitSel)
      stampsPlotSel  <- make_stamp_choices(dtPlotSel)
      stampsFinalSel <- make_stamp_choices(dtFinalSel)

      current_init  <- isolate(input$initialSelectionStamp)
      current_plot  <- isolate(input$plotSelectionStamp)
      current_final <- isolate(input$finalSelectionStamp)

      if (is.null(current_init) || !current_init %in% unname(stampsInitSel)) {
        current_init <- if (length(stampsInitSel) > 0) unname(stampsInitSel)[1] else NULL
      }

      if (is.null(current_plot) || !nzchar(current_plot) || !current_plot %in% unname(stampsPlotSel)) {
        current_plot <- "__none__"
      }

      if (is.null(current_final) ||
          !nzchar(current_final) ||
          current_final == "__none__" ||
          !current_final %in% unname(stampsFinalSel)) {
        current_final <- "__none__"
      }

      updateSelectInput(session, "staStamp", choices = stampsSta)
      updateSelectInput(session, "mtaStamp", choices = stampsMta)
      updateSelectInput(session, "indexStamp", choices = stampsIdxD)

      updateSelectInput(
        session,
        "initialSelectionStamp",
        choices = stampsInitSel,
        selected = current_init
      )

      updateSelectInput(
        session,
        "plotSelectionStamp",
        choices = c("No plot selection" = "__none__", stampsPlotSel),
        selected = current_plot
      )

      updateSelectInput(
        session,
        "finalSelectionStamp",
        choices = c("No final selection" = "__none__", stampsFinalSel),
        selected = current_final
      )
    })

    output$selectionModelingTable <- DT::renderDT({
      req(data())
      req(input$initialSelectionStamp)

      dt <- data()
      req(!is.null(dt$modeling))
      req("analysisId" %in% colnames(dt$modeling))

      modeling_df <- dt$modeling
      modeling_df <- modeling_df[
        modeling_df$analysisId %in% input$initialSelectionStamp &
          modeling_df$module == "Init_prodAdv",
        ,
        drop = FALSE
      ]

      validate(
        need(nrow(modeling_df) > 0, "No modeling records found for the selected initial selection stamp.")
      )

      DT::datatable(
        modeling_df,
        rownames = FALSE,
        filter = "top",
        options = list(
          scrollX = TRUE,
          pageLength = 10
        )
      )
    })

    ###########################################
    #Visualization (idx7)
    ###########################################


    review_plot_data <- eventReactive(input$renderReviewPlots, {
      req(data())
      req(input$initialSelectionStamp)
      req(input$reviewPlots)
      req(length(input$reviewPlots) > 0)

      out <- cgiarPipeline::build_prodadv_review_plot_data(
        dt = data(),
        initial_stamp = input$initialSelectionStamp,
        plot_stamp = input$plotSelectionStamp,
        final_stamp = input$finalSelectionStamp
      )

      out$selected_plots <- input$reviewPlots
      out$performanceProfileScale <- input$performanceProfileScale

      out
    }, ignoreInit = TRUE)

    output$scatterXTraitUI <- renderUI({
      plot_obj <- review_plot_data()
      req(plot_obj)

      trait_choices <- plot_obj$traits
      req(length(trait_choices) >= 2)

      selectInput(
        ns("scatterXTrait"),
        "X-axis trait",
        choices = trait_choices,
        selected = trait_choices[1]
      )
    })

    output$scatterYTraitUI <- renderUI({
      plot_obj <- review_plot_data()
      req(plot_obj)

      trait_choices <- plot_obj$traits
      req(length(trait_choices) >= 2)

      default_y <- if (length(trait_choices) >= 2) trait_choices[2] else trait_choices[1]

      selectInput(
        ns("scatterYTrait"),
        "Y-axis trait",
        choices = trait_choices,
        selected = default_y
      )
    })

    observeEvent(
      list(review_plot_data(), input$scatterXTrait),
      {
        plot_obj <- review_plot_data()
        req(plot_obj)

        trait_choices <- plot_obj$traits
        req(length(trait_choices) >= 2)

        y_choices <- trait_choices[trait_choices != input$scatterXTrait]

        updateSelectInput(
          session,
          "scatterYTrait",
          choices = y_choices,
          selected = y_choices[1]
        )
      },
      ignoreInit = TRUE
    )

    plot_selection_overrides <- reactiveVal(data.frame(
      designation = character(),
      plot_decision = character(),
      stringsAsFactors = FALSE
    ))

    tpe_weather_warning_cache <- reactiveVal(NULL)

    output$pairwiseScatterPlot <- plotly::renderPlotly({
      plot_obj <- review_plot_data()
      req(plot_obj)

      df <- plot_obj$review_df
      req(nrow(df) > 0)

      req(input$scatterXTrait, input$scatterYTrait)

      x_trait <- input$scatterXTrait
      y_trait <- input$scatterYTrait

      validate(
        need(x_trait %in% colnames(df), paste("Trait not found in data:", x_trait)),
        need(y_trait %in% colnames(df), paste("Trait not found in data:", y_trait)),
        need(x_trait != y_trait, "Please select two different traits.")
      )

      x_rel_col <- paste0("reliability_", x_trait)
      y_rel_col <- paste0("reliability_", y_trait)

      if (!x_rel_col %in% colnames(df)) {
        df[[x_rel_col]] <- NA_real_
      }

      if (!y_rel_col %in% colnames(df)) {
        df[[y_rel_col]] <- NA_real_
      }

      df_plot <- df[
        ,
        c("designation", "plot_status", x_trait, y_trait, x_rel_col, y_rel_col),
        drop = FALSE
      ]

      names(df_plot)[names(df_plot) == x_trait] <- "x_value"
      names(df_plot)[names(df_plot) == y_trait] <- "y_value"
      names(df_plot)[names(df_plot) == x_rel_col] <- "x_reliability"
      names(df_plot)[names(df_plot) == y_rel_col] <- "y_reliability"

      df_plot <- df_plot[stats::complete.cases(df_plot[, c("x_value", "y_value")]), , drop = FALSE]

      overrides <- plot_selection_overrides()

      if (nrow(overrides) > 0) {
        df_plot <- merge(
          df_plot,
          overrides,
          by = "designation",
          all.x = TRUE
        )

        df_plot$plot_status <- ifelse(
          !is.na(df_plot$plot_decision),
          df_plot$plot_decision,
          as.character(df_plot$plot_status)
        )

        df_plot$plot_decision <- NULL
      }

      validate(
        need(nrow(df_plot) > 0, "No complete observations available for the selected trait pair.")
      )

      df_plot$plot_status <- factor(
        df_plot$plot_status,
        levels = c("SELECTED", "NOT SELECTED", "CHECK")
      )

      p <- ggplot2::ggplot(
        df_plot,
        ggplot2::aes(
          x = x_value,
          y = y_value,
          color = plot_status,
          size = plot_status,
          key = designation,
          text = paste0(
            "Designation: ", designation,
            "<br>", x_trait, ": ", signif(x_value, 4),
            "<br>", x_trait, " reliability: ", signif(x_reliability, 4),
            "<br>", y_trait, ": ", signif(y_value, 4),
            "<br>", y_trait, " reliability: ", signif(y_reliability, 4),
            "<br>Status: ", plot_status
          )
        )
      ) +
        ggplot2::geom_point(alpha = 0.85) +
        ggplot2::scale_color_manual(
          values = c(
            "CHECK" = "#CC79A7",
            "SELECTED" = "#0072B2",
            "NOT SELECTED" = "#D55E00"
          )
        ) +
        ggplot2::scale_size_manual(
          values = c(
            "CHECK" = 3.8,
            "SELECTED" = 2.8,
            "NOT SELECTED" = 2.8
          ),
          guide = "none"
        ) +
        ggplot2::labs(
          x = x_trait,
          y = y_trait,
          color = "Decision"
        ) +
        ggplot2::theme_minimal(base_size = 13)

      plotly::ggplotly(p, tooltip = "text", source = ns("pairwiseScatter"))

    })

    observeEvent(plotly::event_data("plotly_click", source = ns("pairwiseScatter")), {
      click <- plotly::event_data("plotly_click", source = ns("pairwiseScatter"))
      req(click)

      clicked_designation <- click$key
      req(clicked_designation)

      base_df <- review_plot_data()$review_df
      base_status <- base_df$plot_status[base_df$designation == clicked_designation][1]

      if (is.na(base_status) || base_status == "CHECK") return()

      overrides <- plot_selection_overrides()

      if (clicked_designation %in% overrides$designation) {
        current_status <- overrides$plot_decision[overrides$designation == clicked_designation][1]
      } else {
        current_status <- base_status
      }

      new_status <- if (current_status == "SELECTED") "NOT SELECTED" else "SELECTED"

      if (new_status == base_status) {
        overrides <- overrides[overrides$designation != clicked_designation, , drop = FALSE]
      } else if (clicked_designation %in% overrides$designation) {
        overrides$plot_decision[overrides$designation == clicked_designation] <- new_status
      } else {
        overrides <- rbind(
          overrides,
          data.frame(
            designation = clicked_designation,
            plot_decision = new_status,
            stringsAsFactors = FALSE
          )
        )
      }

      plot_selection_overrides(overrides)
    })

    output$beeswarmPlot <- plotly::renderPlotly({
      plot_obj <- review_plot_data()
      req(plot_obj)

      df_long <- plot_obj$mta_long
      req(!is.null(df_long))
      req(nrow(df_long) > 0)

      status_df <- unique(plot_obj$review_df[, c("designation", "plot_status"), drop = FALSE])

      df_plot <- merge(
        df_long,
        status_df,
        by = "designation",
        all.x = TRUE
      )

      overrides <- plot_selection_overrides()
      if (nrow(overrides) > 0) {
        df_plot <- merge(df_plot, overrides, by = "designation", all.x = TRUE)
        df_plot$plot_status <- ifelse(
          !is.na(df_plot$plot_decision),
          df_plot$plot_decision,
          as.character(df_plot$plot_status)
        )
        df_plot$plot_decision <- NULL
      }

      df_plot <- df_plot[
        stats::complete.cases(df_plot[, c("trait", "predictedValue", "plot_status")]),
        ,
        drop = FALSE
      ]

      validate(
        need(nrow(df_plot) > 0, "No complete observations available for beeswarm plot.")
      )

      df_plot$plot_status <- factor(
        df_plot$plot_status,
        levels = c("CHECK", "SELECTED", "NOT SELECTED")
      )

      p <- ggplot2::ggplot(
        df_plot,
        ggplot2::aes(
          x = trait,
          y = predictedValue,
          color = plot_status,
          key = designation,
          text = paste0(
            "Designation: ", designation,
            "<br>Trait: ", trait,
            "<br>Predicted value: ", signif(predictedValue, 4),
            "<br>Reliability: ", signif(reliability, 4),
            "<br>Status: ", plot_status
          )
        )
      ) +
        ggbeeswarm::geom_quasirandom(
          width = 0.25,
          alpha = 0.8,
          size = 2.4
        ) +
        ggplot2::facet_wrap(~ trait, scales = "free_y") +
        ggplot2::scale_color_manual(
          values = c(
            "CHECK" = "#CC79A7",
            "SELECTED" = "#0072B2",
            "NOT SELECTED" = "#D55E00"
          )
        ) +
        ggplot2::labs(
          x = NULL,
          y = "Predicted value",
          color = "Decision"
        ) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          strip.text = ggplot2::element_text(face = "bold"),
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank(),
          legend.position = "top"
        )

      plotly::ggplotly(p, tooltip = "text", source = ns("beeswarmPlot"))
    })

    observeEvent(plotly::event_data("plotly_click", source = ns("beeswarmPlot")), {
      click <- plotly::event_data("plotly_click", source = ns("beeswarmPlot"))
      req(click)

      clicked_designation <- click$key
      req(clicked_designation)

      base_df <- review_plot_data()$review_df
      base_status <- base_df$plot_status[base_df$designation == clicked_designation][1]

      if (is.na(base_status) || base_status == "CHECK") return()

      overrides <- plot_selection_overrides()

      if (clicked_designation %in% overrides$designation) {
        current_status <- overrides$plot_decision[overrides$designation == clicked_designation][1]
      } else {
        current_status <- base_status
      }

      new_status <- if (current_status == "SELECTED") "NOT SELECTED" else "SELECTED"

      if (new_status == base_status) {
        overrides <- overrides[overrides$designation != clicked_designation, , drop = FALSE]
      } else if (clicked_designation %in% overrides$designation) {
        overrides$plot_decision[overrides$designation == clicked_designation] <- new_status
      } else {
        overrides <- rbind(
          overrides,
          data.frame(
            designation = clicked_designation,
            plot_decision = new_status,
            stringsAsFactors = FALSE
          )
        )
      }

      plot_selection_overrides(overrides)
    })

    output$radarDesignationUI <- renderUI({
      plot_obj <- review_plot_data()
      req(plot_obj)

      df <- plot_obj$review_df
      req(nrow(df) > 0)

      df <- df[, c("designation", "plot_status"), drop = FALSE]
      df <- df[!is.na(df$designation), , drop = FALSE]

      df$plot_status[is.na(df$plot_status) | df$plot_status == ""] <- "NOT SELECTED"

      df$label <- paste0(
        df$plot_status,
        " - ",
        df$designation
      )

      choices <- df$designation
      names(choices) <- df$label

      selected <- df$designation[df$plot_status == "CHECK"]
      selected <- selected[1]

      selectizeInput(
        ns("radarDesignations"),
        label = "Designations to display in radar plot",
        choices = choices,
        selected = selected,
        multiple = TRUE,
        options = list(
          maxItems = 4,
          placeholder = "Select up to 4 designations"
        )
      )
    })

    build_radar_plot <- function(df_panel, trait_cols, title_text = NULL) {
      req(nrow(df_panel) > 0)

      designation_cols <- grDevices::hcl.colors(
        n = nrow(df_panel),
        palette = "Dark 3"
      )

      p <- plotly::plot_ly()

      for (i in seq_len(nrow(df_panel))) {
        values <- as.numeric(df_panel[i, trait_cols, drop = TRUE])

        status_i <- df_panel$plot_status[i]
        if (is.na(status_i) || !nzchar(status_i)) {
          status_i <- "NOT SELECTED"
        }

        color_i <- designation_cols[i]

        trace_name <- paste0(status_i, " - ", df_panel$designation[i])

        p <- p %>%
          plotly::add_trace(
            type = "scatterpolar",
            mode = "lines+markers",
            r = c(values, values[1]),
            theta = c(trait_cols, trait_cols[1]),
            name = trace_name,
            hovertemplate = paste0(
              "Designation: ", df_panel$designation[i],
              "<br>Decision: ", status_i,
              "<br>Trait: %{theta}",
              "<br>Scaled value: %{r:.3f}<extra></extra>"
            ),
            line = list(
              width = 2,
              color = color_i
            ),
            marker = list(
              size = 5,
              color = color_i
            )
          )
      }

      p %>%
        plotly::layout(
          title = title_text,
          polar = list(
            radialaxis = list(
              visible = TRUE,
              range = c(0, 1)
            )
          ),
          showlegend = TRUE
        )
    }

    output$radarPlot <- plotly::renderPlotly({
      plot_obj <- review_plot_data()
      req(plot_obj)

      req(input$radarDesignations)
      req(length(input$radarDesignations) > 0)

      validate(
        need(length(input$radarDesignations) <= 4, "Please select a maximum of 4 designations.")
      )

      df <- plot_obj$review_df
      trait_cols <- plot_obj$traits

      req(length(trait_cols) > 0)

      df_radar <- df[
        df$designation %in% input$radarDesignations,
        c("designation", "plot_status", trait_cols),
        drop = FALSE
      ]

      validate(
        need(nrow(df_radar) > 0, "No selected designations available for radar plot."),
        need(nrow(df_radar) <= 4, "Radar plot supports a maximum of 4 designations.")
      )

      df_radar <- df_radar[stats::complete.cases(df_radar[, trait_cols, drop = FALSE]), , drop = FALSE]

      validate(
        need(nrow(df_radar) > 0, "Selected designations do not have complete trait values for the radar plot.")
      )

      for (tr in trait_cols) {
        x <- df[[tr]]
        rng <- range(x, na.rm = TRUE)

        if (all(is.finite(rng)) && diff(rng) > 0) {
          df_radar[[tr]] <- (df_radar[[tr]] - rng[1]) / diff(rng)
        } else {
          df_radar[[tr]] <- 0.5
        }
      }

      build_radar_plot(
        df_panel = df_radar,
        trait_cols = trait_cols,
        title_text = "Radar plot"
      )
    })

    hovered_tpe_environment <- reactiveVal(NULL)

    get_weather_phase_label <- function(date_vec, planting_date, harvest_date) {
      total_days <- as.numeric(harvest_date - planting_date) + 1

      if (!is.finite(total_days) || total_days <= 1) {
        return(rep(NA_character_, length(date_vec)))
      }

      split_day <- planting_date + floor(total_days * 0.40) - 1

      ifelse(date_vec <= split_day, "Early", "Late")
    }

    compute_rainfall_distribution_index <- function(rain_vec) {
      rain_vec <- rain_vec[is.finite(rain_vec)]
      if (length(rain_vec) == 0) return(NA_real_)

      rainy_days <- sum(rain_vec >= 1, na.rm = TRUE)
      total_rain <- sum(rain_vec, na.rm = TRUE)

      if (total_rain <= 0) return(0)

      rainy_days / length(rain_vec)
    }

    compute_heat_stress_index <- function(temp_vec, threshold = 32) {
      temp_vec <- temp_vec[is.finite(temp_vec)]
      if (length(temp_vec) == 0) return(NA_real_)

      mean(temp_vec > threshold, na.rm = TRUE) * 100
    }

    tpe_weather_resolution_info <- reactive({
      req(data())
      req(!is.null(data()$data))
      req(!is.null(data()$data$weather))

      weather_df <- data()$data$weather

      validate(
        need(nrow(weather_df) > 0, "Weather data is empty."),
        need(
          all(c("environment", "LON", "LAT", "T2M", "PRECTOTCORR", "RH2M") %in% colnames(weather_df)),
          "Weather data must contain environment, LON, LAT, T2M, PRECTOTCORR, and RH2M."
        )
      )

      time_mode <- NULL

      if ("datetime" %in% colnames(weather_df) && any(!is.na(weather_df$datetime))) {
        weather_df$time_index <- parse_weather_time(weather_df$datetime)
        time_mode <- "datetime"

        validate(
          need(
            any(!is.na(weather_df$time_index)),
            "Weather 'datetime' column exists but could not be parsed into valid timestamps."
          )
        )

      } else if ("date" %in% colnames(weather_df) && any(!is.na(weather_df$date))) {
        weather_df$time_index <- parse_weather_date(weather_df$date)
        time_mode <- "date"

        validate(
          need(
            any(!is.na(weather_df$time_index)),
            "Weather 'date' column exists but could not be parsed into valid dates."
          )
        )

      } else {
        validate(
          need(FALSE, "Weather data must contain either 'datetime' or 'date' to build the TPE environmental plot.")
        )
      }

      weather_df <- weather_df[!is.na(weather_df$time_index), , drop = FALSE]
      weather_df <- weather_df[!is.na(weather_df$environment), , drop = FALSE]

      validate(
        need(nrow(weather_df) > 0, "Weather data has no valid time values.")
      )

      infer_weather_resolution <- function(x) {
        x <- sort(unique(x))
        if (length(x) < 2) return(NA_character_)

        if (inherits(x, "Date")) {
          x_num <- as.numeric(x) * 24 * 3600
        } else if (inherits(x, "POSIXt")) {
          x_num <- as.numeric(x)
        } else {
          return(NA_character_)
        }

        diffs <- diff(x_num)
        med_diff <- stats::median(diffs, na.rm = TRUE)

        if (!is.finite(med_diff)) return(NA_character_)

        if (med_diff <= 3 * 3600) return("hourly")
        if (med_diff <= 36 * 3600) return("daily")
        if (med_diff >= 20 * 24 * 3600) return("monthly")

        "unsupported"
      }

      resolution_by_env <- vapply(
        split(weather_df$time_index, weather_df$environment),
        infer_weather_resolution,
        character(1)
      )

      list(
        weather_df = weather_df,
        time_mode = time_mode,
        resolution_by_env = resolution_by_env,
        supported_envs = names(resolution_by_env)[resolution_by_env %in% c("daily", "hourly")],
        monthly_envs = names(resolution_by_env)[resolution_by_env == "monthly"],
        bad_envs = names(resolution_by_env)[!resolution_by_env %in% c("daily", "hourly", "monthly")]
      )
    })

    observe({
      info <- tpe_weather_resolution_info()

      unsupported_envs <- sort(unique(c(info$monthly_envs, info$bad_envs)))
      current_key <- paste(unsupported_envs, collapse = "||")
      last_key <- tpe_weather_warning_cache()

      if (length(unsupported_envs) == 0) {
        tpe_weather_warning_cache(NULL)
        return()
      }

      if (!identical(current_key, last_key)) {
        shiny::showNotification(
          paste0(
            "Some environments were excluded from the TPE environmental plot because their weather resolution is not supported. ",
            "Supported resolutions: daily and hourly. ",
            if (length(info$monthly_envs) > 0) {
              paste0("Monthly: ", paste(info$monthly_envs, collapse = ", "), ". ")
            } else {
              ""
            },
            if (length(info$bad_envs) > 0) {
              paste0("Other unsupported: ", paste(info$bad_envs, collapse = ", "), ".")
            } else {
              ""
            }
          ),
          type = "warning",
          duration = 8
        )

        tpe_weather_warning_cache(current_key)
      }
    })

    tpe_weather_summary <- reactive({
      info <- tpe_weather_resolution_info()

      validate(
        need(length(info$supported_envs) > 0,
             "TPE environmental visualisation currently supports only daily or hourly weather data. Monthly weather data is not supported.")
      )

      df <- info$weather_df
      df <- df[df$environment %in% info$supported_envs, , drop = FALSE]
      df <- df[order(df$environment, df$time_index), , drop = FALSE]

      compute_heat_stress_index <- function(temp_vec, threshold = 32) {
        temp_vec <- temp_vec[is.finite(temp_vec)]
        if (length(temp_vec) == 0) return(NA_real_)
        mean(temp_vec > threshold, na.rm = TRUE) * 100
      }

      compute_rainfall_distribution_index_daily <- function(rain_vec) {
        rain_vec <- rain_vec[is.finite(rain_vec)]
        if (length(rain_vec) == 0) return(NA_real_)

        total_rain <- sum(rain_vec, na.rm = TRUE)
        if (total_rain <= 0) return(0)

        rainy_days <- sum(rain_vec >= 1, na.rm = TRUE)
        rainy_days / length(rain_vec)
      }

      compute_rainfall_distribution_index_hourly <- function(rain_vec, time_vec) {
        keep <- is.finite(rain_vec) & !is.na(time_vec)
        rain_vec <- rain_vec[keep]
        time_vec <- time_vec[keep]

        if (length(rain_vec) == 0) return(NA_real_)

        day_vec <- as.Date(time_vec)
        daily_rain <- tapply(rain_vec, day_vec, sum, na.rm = TRUE)

        total_rain <- sum(daily_rain, na.rm = TRUE)
        if (length(daily_rain) == 0) return(NA_real_)
        if (total_rain <= 0) return(0)

        rainy_days <- sum(daily_rain >= 1, na.rm = TRUE)
        rainy_days / length(daily_rain)
      }

      env_list <- split(df, df$environment)

      out_list <- lapply(env_list, function(env_df) {
        env_df <- env_df[order(env_df$time_index), , drop = FALSE]

        env_resolution <- info$resolution_by_env[unique(env_df$environment)]

        unique_time <- sort(unique(env_df$time_index))
        n_time <- length(unique_time)

        if (n_time < 2) return(NULL)

        split_idx <- max(1, floor(n_time * 0.40))
        early_time <- unique_time[seq_len(split_idx)]

        env_df$phase <- ifelse(env_df$time_index %in% early_time, "Early", "Late")

        phase_list <- split(env_df, env_df$phase)

        do.call(rbind, lapply(phase_list, function(z) {
          rain_vec <- z$PRECTOTCORR[is.finite(z$PRECTOTCORR)]
          temp_vec <- z$T2M[is.finite(z$T2M)]
          hum_vec  <- z$RH2M[is.finite(z$RH2M)]

          rainfall_distribution_index <- if (identical(env_resolution, "hourly")) {
            compute_rainfall_distribution_index_hourly(z$PRECTOTCORR, z$time_index)
          } else {
            compute_rainfall_distribution_index_daily(z$PRECTOTCORR)
          }

          data.frame(
            environment = z$environment[1],
            phase = z$phase[1],
            LON = z$LON[1],
            LAT = z$LAT[1],
            weather_resolution = env_resolution,
            mean_temperature = if (length(temp_vec) == 0) NA_real_ else mean(temp_vec, na.rm = TRUE),
            heat_stress_index = compute_heat_stress_index(temp_vec, threshold = 32),
            rainfall = if (length(rain_vec) == 0) NA_real_ else sum(rain_vec, na.rm = TRUE),
            rainfall_distribution_index = rainfall_distribution_index,
            humidity = if (length(hum_vec) == 0) NA_real_ else mean(hum_vec, na.rm = TRUE),
            stringsAsFactors = FALSE
          )
        }))
      })

      out <- do.call(rbind, out_list)
      rownames(out) <- NULL

      validate(
        need(!is.null(out) && nrow(out) > 0, "No summarized weather data available for supported environments.")
      )

      out
    })

    selected_env_surface_data <- reactive({
      df <- tpe_weather_summary()
      req(df)

      req(input$tpePhase)
      req(input$tpeEnvCovariate)

      validate(
        need("phase" %in% colnames(df), "Summarized weather data must contain a 'phase' column."),
        need(input$tpeEnvCovariate %in% colnames(df),
             paste("Selected environmental covariate not found:", input$tpeEnvCovariate))
      )

      df_phase <- df[df$phase == input$tpePhase, , drop = FALSE]

      validate(
        need(nrow(df_phase) > 0, paste("No weather summaries available for phase:", input$tpePhase))
      )

      df_phase$env_value <- df_phase[[input$tpeEnvCovariate]]

      df_phase <- df_phase[
        stats::complete.cases(df_phase[, c("environment", "LON", "LAT", "env_value")]),
        ,
        drop = FALSE
      ]

      validate(
        need(nrow(df_phase) > 0, "No complete environmental covariate values available for the selected phase."),
        need(any(is.finite(df_phase$env_value)), "Selected environmental covariate has no finite values.")
      )

      df_phase
    })

    output$tpeDesignationUI <- renderUI({
      plot_obj <- review_plot_data()
      req(plot_obj)

      df <- plot_obj$sta_long
      req(!is.null(df))
      req(nrow(df) > 0)

      validate(
        need("designation" %in% colnames(df), "STA predictions must contain a 'designation' column.")
      )

      designation_choices <- sort(unique(as.character(df$designation)))
      designation_choices <- designation_choices[!is.na(designation_choices) & nzchar(designation_choices)]

      validate(
        need(length(designation_choices) > 0, "No designations available for the TPE performance plot.")
      )

      selectInput(
        ns("tpeDesignation"),
        label = tags$span(
          "Variety",
          tags$i(
            class = "glyphicon glyphicon-info-sign",
            style = "color:#FFFFFF",
            title = "Choose the designation whose environment-specific STA predictions will be shown across the TPE."
          )
        ),
        choices = designation_choices,
        selected = designation_choices[1]
      )
    })

    output$tpeTraitUI <- renderUI({
      plot_obj <- review_plot_data()
      req(plot_obj)

      df <- plot_obj$sta_long
      req(!is.null(df))
      req(nrow(df) > 0)

      validate(
        need("trait" %in% colnames(df), "STA predictions must contain a 'trait' column.")
      )

      trait_choices <- sort(unique(as.character(df$trait)))
      trait_choices <- trait_choices[!is.na(trait_choices) & nzchar(trait_choices)]

      validate(
        need(length(trait_choices) > 0, "No traits available for the TPE performance plot.")
      )

      selectInput(
        ns("tpeTrait"),
        label = tags$span(
          "Trait",
          tags$i(
            class = "glyphicon glyphicon-info-sign",
            style = "color:#FFFFFF",
            title = "Choose the trait whose environment-specific performance will be displayed."
          )
        ),
        choices = trait_choices,
        selected = trait_choices[1]
      )
    })

    selected_tpe_performance <- reactive({
      plot_obj <- review_plot_data()
      req(plot_obj)

      df <- plot_obj$sta_long
      req(!is.null(df))
      req(nrow(df) > 0)

      req(input$tpeDesignation)
      req(input$tpeTrait)

      validate(
        need(
          all(c("designation", "environment", "trait", "predictedValue") %in% colnames(df)),
          "STA predictions must contain designation, environment, trait, and predictedValue."
        )
      )

      df_sel <- df[
        df$designation == input$tpeDesignation &
          df$trait == input$tpeTrait,
        ,
        drop = FALSE
      ]

      validate(
        need(nrow(df_sel) > 0,
             paste("No STA predictions found for designation", input$tpeDesignation,
                   "and trait", input$tpeTrait, ".")),
        need("environment" %in% colnames(df_sel),
             "STA predictions must contain an environment column.")
      )

      # Join coordinates from summarized weather data
      env_coords <- unique(
        tpe_weather_summary()[, c("environment", "LON", "LAT"), drop = FALSE]
      )

      df_sel <- merge(
        df_sel,
        env_coords,
        by = "environment",
        all.x = TRUE
      )

      df_sel <- df_sel[
        stats::complete.cases(df_sel[, c("environment", "designation", "trait", "predictedValue", "LON", "LAT")]),
        ,
        drop = FALSE
      ]

      validate(
        need(nrow(df_sel) > 0,
             "No complete environment-level performance records with coordinates are available for the selected designation and trait.")
      )

      df_sel
    })

    tpe_trait_performance_range <- reactive({
      plot_obj <- review_plot_data()
      req(plot_obj)

      df <- plot_obj$sta_long
      req(!is.null(df))
      req(nrow(df) > 0)

      req(input$tpeTrait)

      validate(
        need(
          all(c("trait", "predictedValue") %in% colnames(df)),
          "STA predictions must contain trait and predictedValue."
        )
      )

      df_trait <- df[
        df$trait == input$tpeTrait &
          is.finite(df$predictedValue),
        ,
        drop = FALSE
      ]

      validate(
        need(nrow(df_trait) > 0,
             paste("No finite STA predicted values found for trait", input$tpeTrait, "."))
      )

      range(df_trait$predictedValue, na.rm = TRUE)
    })

    output$tpePerformanceMap <- plotly::renderPlotly({
      df <- selected_tpe_performance()
      req(df)

      validate(
        need(nrow(df) > 0, "No environment-level performance data available for the selected designation and trait."),
        need(all(c("environment", "designation", "trait", "predictedValue", "LON", "LAT") %in% colnames(df)),
             "Performance data must contain environment, designation, trait, predictedValue, LON, and LAT.")
      )

      map_obj <- tryCatch(
        {
          cgiarPipeline::get_tpe_basemap(
            df_points = df,
            buffer_deg = 3,
            square_expand_factor = 1.05
          )
        },
        error = function(e) {
          validate(need(FALSE, e$message))
        }
      )

      basemap <- map_obj$basemap
      bbox <- map_obj$bbox
      basemap_df <- cgiarPipeline::sf_to_plotly_polygons(basemap)

      rng <- tpe_trait_performance_range()

      validate(
        need(all(is.finite(rng)), "Predicted values are not finite for the selected trait.")
      )

      if (diff(rng) > 0) {
        df$halo_size <- 18 + 22 * (df$predictedValue - rng[1]) / diff(rng)
      } else {
        df$halo_size <- 28
      }

      p <- plotly::plot_ly()

      if (nrow(basemap_df) > 0) {
        split_polys <- split(basemap_df, basemap_df$group)

        for (poly_df in split_polys) {
          p <- p %>%
            plotly::add_trace(
              data = poly_df,
              x = ~LON,
              y = ~LAT,
              type = "scatter",
              mode = "lines",
              line = list(color = "black", width = 1),
              hoverinfo = "skip",
              showlegend = FALSE
            )
        }
      }

      p <- p %>%
        plotly::add_markers(
          data = df,
          x = ~LON,
          y = ~LAT,
          key = ~environment,
          text = ~paste0(
            "Designation: ", designation,
            "<br>Environment: ", environment,
            "<br>Trait: ", trait,
            "<br>Predicted value: ", signif(predictedValue, 4),
            "<br>Reliability: ", signif(reliability, 4)
          ),
          hoverinfo = "text",
          marker = list(
            sizemode = "diameter",
            opacity = 0.22,
            color = "#5B8FF9",
            line = list(width = 0)
          ),
          size = ~halo_size,
          showlegend = FALSE,
          source = ns("tpePerformanceMap")
        )

      p <- p %>%
        plotly::add_markers(
          data = df,
          x = ~LON,
          y = ~LAT,
          key = ~environment,
          text = ~paste0(
            "Designation: ", designation,
            "<br>Environment: ", environment,
            "<br>Trait: ", trait,
            "<br>Predicted value: ", signif(predictedValue, 4),
            "<br>Reliability: ", signif(reliability, 4)
          ),
          hoverinfo = "text",
          marker = list(
            size = 7,
            color = "#1F1F1F",
            line = list(color = "white", width = 1)
          ),
          showlegend = FALSE,
          source = ns("tpePerformanceMap")
        )

      p %>%
        plotly::layout(
          title = paste("Performance across TPE -", input$tpeDesignation, "-", input$tpeTrait),
          xaxis = list(
            visible = FALSE,
            range = c(unname(bbox["xmin"]), unname(bbox["xmax"])),
            fixedrange = TRUE
          ),
          yaxis = list(
            visible = FALSE,
            range = c(unname(bbox["ymin"]), unname(bbox["ymax"])),
            scaleanchor = "x",
            scaleratio = 1,
            fixedrange = TRUE
          ),
          margin = list(l = 0, r = 0, b = 0, t = 50, pad = 0),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )
    })

    observeEvent(
      plotly::event_data("plotly_hover", source = ns("tpePerformanceMap")),
      {
        hover <- plotly::event_data("plotly_hover", source = ns("tpePerformanceMap"))
        req(hover)

        if (!is.null(hover$key) && length(hover$key) > 0) {
          hovered_tpe_environment(as.character(hover$key[[1]]))
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      plotly::event_data("plotly_unhover", source = ns("tpePerformanceMap")),
      {
        hovered_tpe_environment(NULL)
      },
      ignoreInit = TRUE
    )

    output$tpeEnvCovHeatmap <- plotly::renderPlotly({
      df_points <- selected_env_surface_data()
      req(df_points)

      validate(
        need(nrow(df_points) > 0, "No environmental summary data available for the selected phase and covariate."),
        need(all(c("environment", "LON", "LAT", "env_value") %in% colnames(df_points)),
             "Environmental surface data must contain environment, LON, LAT, and env_value.")
      )

      map_obj <- tryCatch(
        {
          cgiarPipeline::get_tpe_basemap(
            df_points = df_points,
            buffer_deg = 3,
            square_expand_factor = 1.05
          )
        },
        error = function(e) {
          validate(need(FALSE, e$message))
        }
      )

      basemap <- map_obj$basemap
      bbox <- map_obj$bbox
      basemap_df <- cgiarPipeline::sf_to_plotly_polygons(basemap)

      grid_df <- tryCatch(
        {
          cgiarPipeline::idw_surface(
            df_points = df_points,
            value_col = "env_value",
            lon_col = "LON",
            lat_col = "LAT",
            n_grid = 140,
            power = 2,
            lon_min = unname(bbox["xmin"]),
            lon_max = unname(bbox["xmax"]),
            lat_min = unname(bbox["ymin"]),
            lat_max = unname(bbox["ymax"])
          )
        },
        error = function(e) {
          validate(need(FALSE, e$message))
        }
      )

      mask_sf <- tryCatch(
        {
          cgiarPipeline::build_tpe_mask(df_points, buffer_km = 220)
        },
        error = function(e) {
          NULL
        }
      )

      #grid_df <- tryCatch(
      #  {
      #    mask_grid_to_polygon(grid_df, mask_sf)
      #  },
      #  error = function(e) {
      #    grid_df
      #  }
      #)

      highlighted_env <- hovered_tpe_environment()

      covariate_label <- switch(
        input$tpeEnvCovariate,
        "mean_temperature" = "Mean temperature",
        "heat_stress_index" = "Heat stress index",
        "rainfall" = "Rainfall",
        "rainfall_distribution_index" = "Rainfall distribution index",
        "humidity" = "Humidity",
        input$tpeEnvCovariate
      )

      p <- plotly::plot_ly()

      if (nrow(grid_df) > 0) {
        p <- p %>%
          plotly::add_trace(
            data = grid_df,
            x = ~LON,
            y = ~LAT,
            z = ~z,
            type = "heatmap",
            colorscale = "Viridis",
            showscale = TRUE,
            hoverinfo = "skip"
          )
      }

      if (nrow(basemap_df) > 0) {
        split_polys <- split(basemap_df, basemap_df$group)

        for (poly_df in split_polys) {
          p <- p %>%
            plotly::add_trace(
              data = poly_df,
              x = ~LON,
              y = ~LAT,
              type = "scatter",
              mode = "lines",
              line = list(color = "black", width = 1),
              hoverinfo = "skip",
              showlegend = FALSE
            )
        }
      }

      p <- p %>%
        plotly::add_markers(
          data = df_points,
          x = ~LON,
          y = ~LAT,
          key = ~environment,
          text = ~paste0(
            "Environment: ", environment,
            "<br>Phase: ", phase,
            "<br>", covariate_label, ": ", signif(env_value, 4),
            "<br>Weather resolution: ", weather_resolution
          ),
          hoverinfo = "text",
          marker = list(
            size = 7,
            color = "white",
            line = list(color = "black", width = 1)
          ),
          showlegend = FALSE
        )

      if (!is.null(highlighted_env)) {
        df_highlight <- df_points[df_points$environment == highlighted_env, , drop = FALSE]

        if (nrow(df_highlight) > 0) {
          p <- p %>%
            plotly::add_markers(
              data = df_highlight,
              x = ~LON,
              y = ~LAT,
              text = ~paste0(
                "Highlighted environment: ", environment,
                "<br>", covariate_label, ": ", signif(env_value, 4)
              ),
              hoverinfo = "text",
              marker = list(
                size = 18,
                color = "rgba(255,255,255,0)",
                line = list(color = "#FF4D4F", width = 3),
                symbol = "circle"
              ),
              showlegend = FALSE
            )
        }
      }

      p %>%
        plotly::layout(
          title = paste("Environmental surface -", input$tpePhase, "-", covariate_label),
          xaxis = list(
            visible = FALSE,
            range = c(unname(bbox["xmin"]), unname(bbox["xmax"])),
            fixedrange = TRUE
          ),
          yaxis = list(
            visible = FALSE,
            range = c(unname(bbox["ymin"]), unname(bbox["ymax"])),
            scaleanchor = "x",
            scaleratio = 1,
            fixedrange = TRUE
          ),
          margin = list(l = 0, r = 0, b = 0, t = 50, pad = 0),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )
    })

    performance_profile_data <- reactive({
      req(review_plot_data())
      req(data())
      req(input$initialSelectionStamp)

      dt <- data()

      modeling_init <- dt$modeling
      modeling_init <- modeling_init[
        modeling_init$analysisId %in% input$initialSelectionStamp &
          modeling_init$module == "Init_prodAdv",
        ,
        drop = FALSE
      ]

      validate(
        need(nrow(modeling_init) > 0, "No modeling records found for the selected initial selection stamp.")
      )

      tryCatch(
        {
          cgiarPipeline::buildProdAdvPerformanceProfile(
            mta_long = review_plot_data()$mta_long,
            review_df = review_plot_data()$review_df,
            modeling_df = modeling_init,
            performance_profile_scale = input$performanceProfileScale,
            plot_selection_overrides = plot_selection_overrides()
          )
        },
        error = function(e) {
          validate(need(FALSE, e$message))
        }
      )
    })

    get_performance_profile_page_data <- function(df_plot, page, page_size = 10) {
      if (!is.data.frame(df_plot)) {
        stop("'df_plot' must be a data.frame.")
      }

      required_cols <- c("designation")
      missing_cols <- setdiff(required_cols, colnames(df_plot))
      if (length(missing_cols) > 0) {
        stop(
          "Missing required columns in 'df_plot': ",
          paste(missing_cols, collapse = ", ")
        )
      }

      designation_order <- unique(as.character(df_plot$designation))
      n_designations <- length(designation_order)

      if (n_designations == 0) {
        stop("No designations available for paging.")
      }

      n_pages <- max(1, ceiling(n_designations / page_size))

      if (is.null(page) || !is.finite(page)) {
        page <- 1
      }

      page <- max(1, min(as.integer(page), n_pages))

      start_idx <- ((page - 1) * page_size) + 1
      end_idx <- min(page * page_size, n_designations)

      keep_designations <- designation_order[start_idx:end_idx]

      df_page <- df_plot[df_plot$designation %in% keep_designations, , drop = FALSE]
      df_page$designation <- factor(df_page$designation, levels = keep_designations)

      list(
        df = df_page,
        page = page,
        n_pages = n_pages,
        n_designations = n_designations,
        start_idx = start_idx,
        end_idx = end_idx
      )
    }

    output$performanceProfilePageInfo <- renderUI({
      prof_obj <- performance_profile_data()
      req(prof_obj)

      page_obj <- get_performance_profile_page_data(
        df_plot = prof_obj$plot_df,
        page = input$performanceProfilePage,
        page_size = 10
      )

      tags$div(
        style = "padding-top: 25px;",
        tags$strong(
          paste0(
            "Showing plots ",
            page_obj$start_idx,
            "-",
            page_obj$end_idx,
            " of ",
            page_obj$n_designations
          )
        ),
        if (page_obj$page < page_obj$n_pages) {
          tags$span(paste0("  |  Next page: ", page_obj$page + 1))
        }
      )
    })

    observe({
      prof_obj <- performance_profile_data()
      req(prof_obj)

      n_pages <- max(1, ceiling(length(unique(prof_obj$plot_df$designation)) / 10))

      current_page <- input$performanceProfilePage
      if (is.null(current_page) || !is.finite(current_page)) {
        current_page <- 1
      }

      updateNumericInput(
        session = session,
        inputId = "performanceProfilePage",
        min = 1,
        max = n_pages,
        value = max(1, min(current_page, n_pages))
      )
    })

    output$performanceProfilePlot <- plotly::renderPlotly({
      prof_obj <- performance_profile_data()
      req(prof_obj)

      df_plot <- prof_obj$plot_df
      x_lab <- prof_obj$x_lab

      validate(
        need(length(unique(df_plot$designation)) > 0, "No designations available for performance profile plot."),
        need(length(unique(df_plot$designation)) <= 50, "Per-variety performance profile supports up to 50 designations.")
      )

      page_obj <- get_performance_profile_page_data(
        df_plot = df_plot,
        page = input$performanceProfilePage,
        page_size = 10
      )

      df_page <- page_obj$df
      df_page$trait <- factor(df_page$trait, levels = rev(unique(df_page$trait)))

      df_page$sign_class <- ifelse(
        df_page$profile_value_clipped > 0, "POS",
        ifelse(df_page$profile_value_clipped < 0, "NEG", "ZERO")
      )

      p <- ggplot2::ggplot(
        df_page,
        ggplot2::aes(
          x = profile_value_clipped,
          y = trait,
          fill = sign_class,
          text = paste0(
            "Designation: ", designation,
            "<br>Trait: ", trait,
            "<br>", x_lab, ": ", round(profile_value, 1), "%",
            ifelse(
              x_lab == "% over check",
              paste0("<br>Reference check: ", reference_check_used),
              ""
            ),
            "<br>Status: ", plot_status
          )
        )
      ) +
        ggplot2::geom_col(width = 0.75, alpha = 1) +
        ggplot2::geom_vline(
          xintercept = 0,
          linetype = "dashed",
          linewidth = 0.5,
          color = "black"
        ) +
        ggplot2::facet_wrap(~ designation, nrow = 2, ncol = 5, scales = "fixed") +
        ggplot2::scale_x_continuous(
          limits = c(-50, 50),
          breaks = c(-50, -25, 0, 25, 50),
          oob = scales::squish
        ) +
        ggplot2::scale_fill_manual(
          values = c(
            "POS" = "#1B9E77",
            "NEG" = "#D95F02",
            "ZERO" = "#BDBDBD"
          ),
          drop = FALSE,
          guide = "none"
        ) +
        ggplot2::labs(
          x = x_lab,
          y = NULL
        ) +
        ggplot2::theme_bw(base_size = 12) +
        ggplot2::theme(
          panel.border = ggplot2::element_rect(
            colour = "black",
            fill = NA,
            linewidth = 0.8
          ),
          panel.background = ggplot2::element_rect(
            fill = "white",
            colour = NA
          ),
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_line(
            colour = "#D9D9D9",
            linewidth = 0.3
          ),
          strip.background = ggplot2::element_rect(
            fill = "#D9D9D9",
            colour = "black",
            linewidth = 0.8
          ),
          strip.text = ggplot2::element_text(
            face = "bold",
            size = 11,
            colour = "black"
          ),
          axis.text.y = ggplot2::element_text(
            colour = "black",
            size = 10
          ),
          axis.text.x = ggplot2::element_text(
            colour = "black",
            size = 9
          ),
          axis.title.x = ggplot2::element_text(
            face = "bold",
            colour = "black"
          ),
          axis.title.y = ggplot2::element_blank(),
          legend.position = "none",
          panel.spacing = grid::unit(0.9, "lines")
        )

      plotly::ggplotly(p, tooltip = "text")
    })

    output$reliabilityTraitUI <- renderUI({
      plot_obj <- review_plot_data()
      req(plot_obj)

      trait_choices <- plot_obj$traits
      req(length(trait_choices) > 0)

      selectInput(
        ns("reliabilityTrait"),
        "Trait for reliability intervals",
        choices = trait_choices,
        selected = trait_choices[1]
      )
    })

    output$reliabilityIntervalPlot <- plotly::renderPlotly({
      plot_obj <- review_plot_data()
      req(plot_obj)

      req(input$reliabilityTrait)

      df <- plot_obj$mta_long
      req(nrow(df) > 0)

      df <- df[
        df$trait == input$reliabilityTrait,
        ,
        drop = FALSE
      ]

      validate(
        need(nrow(df) > 0, "No predictions found for the selected trait."),
        need("stdError" %in% colnames(df), "Standard errors are not available for this trait.")
      )

      if (!"reliability" %in% colnames(df)) {
        df$reliability <- NA_real_
      }

      if (!"plot_status" %in% colnames(df)) {
        status_df <- plot_obj$review_df[, c("designation", "plot_status"), drop = FALSE]
        df <- merge(df, status_df, by = "designation", all.x = TRUE)
      }

      df$lower_2se <- df$predictedValue - df$stdError
      df$upper_2se <- df$predictedValue + df$stdError

      df <- df[order(df$predictedValue, decreasing = TRUE), , drop = FALSE]
      df$designation <- factor(df$designation, levels = df$designation)

      p <- ggplot2::ggplot(
        df,
        ggplot2::aes(
          x = designation,
          y = predictedValue,
          color = plot_status,
          text = paste0(
            "Designation: ", designation,
            "<br>Trait: ", trait,
            "<br>Predicted value: ", signif(predictedValue, 4),
            "<br>Std. error: ", signif(stdError, 4),
            "<br>Approx. interval: [",
            signif(lower_2se, 4), ", ",
            signif(upper_2se, 4), "]",
            "<br>Reliability: ", signif(reliability, 4),
            "<br>Status: ", plot_status
          )
        )
      ) +
        ggplot2::geom_errorbar(
          ggplot2::aes(
            ymin = lower_2se,
            ymax = upper_2se
          ),
          width = 0.2,
          alpha = 0.6
        ) +
        ggplot2::geom_point(size = 2.5) +
        ggplot2::coord_flip() +
        ggplot2::scale_color_manual(
          values = c(
            "SELECTED" = "#0072B2",
            "NOT SELECTED" = "#D55E00",
            "CHECK" = "#CC79A7"
          ),
          drop = FALSE
        ) +
        ggplot2::labs(
          title = paste("BLUP/BLUE reliability intervals -", input$reliabilityTrait),
          x = "Designation",
          y = "Predicted value",
          color = "Decision"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          legend.position = "bottom"
        )

      plotly::ggplotly(p, tooltip = "text")
    })

    output$reviewPlotsUI <- renderUI({
      plot_obj <- review_plot_data()
      req(plot_obj)

      selected_plots <- plot_obj$selected_plots

      ui_list <- list()

      if ("Pair-wise trait scatterplot" %in% selected_plots) {
        ui_list <- c(
          ui_list,
          list(
            shinydashboard::box(
              width = 12,
              title = "Pair-wise trait scatterplot",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE,
              plotly::plotlyOutput(ns("pairwiseScatterPlot"), height = "650px")
            )
          )
        )
      }

      if ("Beeswarm trait distributions" %in% selected_plots) {
        ui_list <- c(
          ui_list,
          list(
            shinydashboard::box(
              width = 12,
              title = "Beeswarm trait distributions",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE,
              plotly::plotlyOutput(ns("beeswarmPlot"), height = "700px")
            )
          )
        )
      }

      if ("Radar plot" %in% selected_plots) {
        ui_list <- c(
          ui_list,
          list(
            shinydashboard::box(
              width = 12,
              title = "Radar plot",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE,
              plotly::plotlyOutput(ns("radarPlot"), height = "700px")
            )
          )
        )
      }

      if ("Performance heatmap across TPE" %in% selected_plots) {
        ui_list <- c(
          ui_list,
          list(
            fluidRow(
              column(
                width = 6,
                shinydashboard::box(
                  width = 12,
                  title = "Environmental covariate across TPE",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  plotly::plotlyOutput(ns("tpeEnvCovHeatmap"), height = "650px")
                )
              ),
              column(
                width = 6,
                shinydashboard::box(
                  width = 12,
                  title = "Variety performance across TPE",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  plotly::plotlyOutput(ns("tpePerformanceMap"), height = "650px")
                )
              )
            )
          )
        )
      }

      if ("Per-variety trait performance profile" %in% selected_plots) {
        ui_list <- c(
          ui_list,
          list(
            shinydashboard::box(
              width = 12,
              title = "Per-variety trait performance profile",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE,
              plotly::plotlyOutput(ns("performanceProfilePlot"), height = "900px")
            )
          )
        )
      }

      if ("BLUP/BLUE reliability intervals" %in% selected_plots) {
        ui_list <- c(
          ui_list,
          list(
            shinydashboard::box(
              width = 12,
              title = "BLUP/BLUE reliability intervals",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE,
              plotly::plotlyOutput(ns("reliabilityIntervalPlot"), height = "750px")
            )
          )
        )
      }

      do.call(tagList, ui_list)
    })

    output$manualDesignationSelectionUI <- renderUI({
      plot_obj <- review_plot_data()
      req(plot_obj)

      df <- plot_obj$review_df
      req(nrow(df) > 0)

      overrides <- plot_selection_overrides()

      df_manual <- df[df$plot_status != "CHECK", c("designation", "plot_status"), drop = FALSE]

      if (nrow(overrides) > 0) {
        df_manual <- merge(df_manual, overrides, by = "designation", all.x = TRUE)
        df_manual$effective_status <- ifelse(
          !is.na(df_manual$plot_decision),
          df_manual$plot_decision,
          as.character(df_manual$plot_status)
        )
      } else {
        df_manual$effective_status <- as.character(df_manual$plot_status)
      }

      df_manual <- unique(df_manual[, c("designation", "effective_status"), drop = FALSE])

      choice_labels <- paste0(df_manual$designation, " [", df_manual$effective_status, "]")
      choice_values <- df_manual$designation
      names(choice_values) <- choice_labels

      selectizeInput(
        ns("manualDesignationSelection"),
        "Select designation(s)",
        choices = choice_values,
        selected = NULL,
        multiple = TRUE,
        options = list(
          placeholder = "Choose one or more designations"
        )
      )
    })

    observeEvent(input$applyManualDesignationDecision, {
      req(input$manualDesignationSelection)
      req(length(input$manualDesignationSelection) > 0)
      req(input$manualDesignationDecision)

      selected_designations <- input$manualDesignationSelection
      new_status <- input$manualDesignationDecision

      base_df <- review_plot_data()$review_df
      req(base_df)

      overrides <- plot_selection_overrides()

      for (des in selected_designations) {
        base_status <- base_df$plot_status[base_df$designation == des][1]

        if (is.na(base_status) || base_status == "CHECK") {
          next
        }

        if (des %in% overrides$designation) {
          overrides$plot_decision[overrides$designation == des] <- new_status
        } else {
          overrides <- rbind(
            overrides,
            data.frame(
              designation = des,
              plot_decision = new_status,
              stringsAsFactors = FALSE
            )
          )
        }

        if (!is.na(base_status) && identical(base_status, new_status)) {
          overrides <- overrides[overrides$designation != des, , drop = FALSE]
        }
      }

      plot_selection_overrides(overrides)

      showNotification(
        paste(length(selected_designations), "designation(s) updated."),
        type = "message"
      )
    })

    observeEvent(input$savePlotSelection, {
      req(data())
      req(input$initialSelectionStamp)

      manual_decisions <- plot_selection_overrides()
      req(nrow(manual_decisions) > 0)

      analysis_id <- as.numeric(Sys.time())

      dt_object <- cgiarPipeline::savePlotProdAdvSelection(
        analysisId = analysis_id,
        analysisIdName = if (nzchar(trimws(input$plotSelectionId))) trimws(input$plotSelectionId) else NULL,
        initialSelectionStamp = input$initialSelectionStamp,
        plotSelectionStamp = if (!is.null(input$plotSelectionStamp) &&
                                 nzchar(input$plotSelectionStamp) &&
                                 input$plotSelectionStamp != "__none__") input$plotSelectionStamp else NULL,
        manual_decisions = manual_decisions,
        dt_object = data()
      )

      data(dt_object)

      new_plot_stamp <- as.character(analysis_id)

      dt_status <- dt_object$status
      dtPlotSel <- dt_status[dt_status$module == "Plot_prodAdv", , drop = FALSE]
      stampsPlotSel <- make_stamp_choices(dtPlotSel)

      session$onFlushed(function() {

        updateSelectInput(
          session,
          "plotSelectionStamp",
          choices = c("No plot selection" = "__none__", stampsPlotSel),
          selected = new_plot_stamp
        )

        updateTabsetPanel(session, "reviewOutputTabs", selected = "Decision table")

      }, once = TRUE)

      showNotification(
        "Plot selection saved successfully.",
        type = "message"
      )
    }, ignoreInit = TRUE)

    ###########################################
    # Decision table (idx8)
    ###########################################

    final_decision_overrides <- reactiveVal(
      data.frame(
        designation = character(),
        final_decision = character(),
        stringsAsFactors = FALSE
      )
    )

    `%||%` <- function(x, y) {
      if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
    }

    make_decision_badge <- function(value) {
      bg <- dplyr::case_when(
        is.na(value) | !nzchar(value) ~ "#F5F5F5",
        value == "SELECTED" ~ "#D6EAF8",
        value == "NOT SELECTED" ~ "#FADBD8",
        value == "CHECK" ~ "#F5DCE9",
        value == "REVISE" ~ "#FFF3CD",
        TRUE ~ "#F5F5F5"
      )

      txt <- ifelse(is.na(value), "", as.character(value))

      sprintf(
        "<div style='width:100%%; background:%s; padding:6px; border-radius:4px; text-align:center; font-weight:600;'>%s</div>",
        bg, txt
      )
    }

    make_value_badge <- function(value, decision) {
      bg <- dplyr::case_when(
        decision == "SELECTED" ~ "#D9F2D9",
        decision == "NOT SELECTED" ~ "#F8D7DA",
        decision == "CHECK" ~ "#D9ECFF",
        TRUE ~ "#FFFFFF"
      )

      txt <- ifelse(is.na(value), "", format(round(as.numeric(value), 3), nsmall = 3))

      sprintf(
        "<div style='width:100%%; background:%s; padding:6px; border-radius:4px; text-align:right;'>%s</div>",
        bg, txt
      )
    }

    make_final_cell <- function(designation, selected_value, initial_decision) {

      if (identical(initial_decision, "CHECK")) {
        return(make_decision_badge("CHECK"))
      }

      selected_value <- selected_value %||% "REVISE"

      choices <- c("SELECTED", "NOT SELECTED", "REVISE")

      options <- vapply(
        choices,
        function(ch) {
          sel <- if (identical(ch, selected_value)) " selected" else ""
          sprintf("<option value='%s'%s>%s</option>", ch, sel, ch)
        },
        character(1)
      )

      sprintf(
        "<select class='form-control final-decision-select' data-designation='%s' style='width:140px;'>%s</select>",
        htmltools::htmlEscape(designation),
        paste(options, collapse = "")
      )
    }

    decision_table_data <- reactive({
      build_decision_table_data(
        dt = data(),
        initial_stamp = input$initialSelectionStamp,
        plot_stamp = input$plotSelectionStamp,
        final_stamp = input$finalSelectionStamp,
        final_overrides = final_decision_overrides()
      )
    })

    output$decisionTable <- DT::renderDT({

      obj <- decision_table_data()
      req(obj)

      DT::datatable(
        obj$display,
        escape = FALSE,
        rownames = FALSE,
        selection = "none",
        class = "compact stripe hover nowrap decision-table",
        options = list(
          scrollX = TRUE,
          scrollY = "500px",
          paging = FALSE,
          searching = FALSE,
          ordering = FALSE,
          autoWidth = FALSE
        ),
        callback = htmlwidgets::JS(sprintf("
      table.on('change', 'select.final-decision-select', function() {
        var designation = $(this).data('designation');
        var value = $(this).val();
        Shiny.setInputValue('%s', {
          designation: designation,
          value: value,
          nonce: Math.random()
        }, {priority: 'event'});
      });
    ", ns("finalDecisionChange")))
      )
    })

    observeEvent(input$finalDecisionChange, {

      current <- final_decision_overrides()

      desig <- input$finalDecisionChange$designation
      val <- input$finalDecisionChange$value

      if (!desig %in% current$designation) {
        current <- rbind(
          current,
          data.frame(
            designation = desig,
            final_decision = val,
            stringsAsFactors = FALSE
          )
        )
      } else {
        current$final_decision[current$designation == desig] <- val
      }

      final_decision_overrides(current)

    }, ignoreInit = TRUE)

    final_decision_table_raw <- reactive({
      obj <- decision_table_data()
      req(obj)

      out <- obj$raw

      trait_cols <- grep("_trait_decision$", colnames(out), invert = TRUE, value = TRUE)
      drop_cols <- c(
        "entryType",
        "is_check_sort",
        "final_decision",
        "final_manual_decision"
      )
      keep_cols <- setdiff(trait_cols, drop_cols)

      out <- out[, keep_cols, drop = FALSE]

      names(out)[names(out) == "final_decision_initial"] <- "final_decision"

      out
    })

    final_report_table <- reactive({
      tbl <- final_decision_table_raw()
      req(tbl)
      req(nrow(tbl) > 0)

      tbl
    })

    observeEvent(input$saveFinalSelection, {
      req(data())
      req(input$initialSelectionStamp)

      final_tbl <- final_decision_table_raw()
      req(nrow(final_tbl) > 0)

      analysis_id <- as.numeric(Sys.time())

      final_modifications <- data.frame(
        module = "Final_prodAdv",
        analysisId = analysis_id,
        designation = final_tbl$designation,
        reason = "final_manual_decision",
        value = final_tbl$final_decision,
        stringsAsFactors = FALSE
      )

      final_status <- data.frame(
        module = "Final_prodAdv",
        analysisId = analysis_id,
        analysisIdName = if (nzchar(trimws(input$finalSelectionId))) trimws(input$finalSelectionId) else NA_character_,
        stringsAsFactors = FALSE
      )

      dt_object <- data()

      if (is.null(dt_object$modifications$selection)) {
        dt_object$modifications$selection <- final_modifications
      } else {
        dt_object$modifications$selection <- rbind(
          dt_object$modifications$selection,
          final_modifications
        )
      }

      if (is.null(dt_object$status)) {
        dt_object$status <- final_status
      } else {
        dt_object$status <- rbind(
          dt_object$status,
          final_status
        )
      }

      data(dt_object)

      final_decision_overrides(
        final_tbl[, c("designation", "final_decision"), drop = FALSE]
      )

      # rebuild final stamp choices and select the newly saved final stamp
      dt_status <- dt_object$status
      dtFinalSel <- dt_status[dt_status$module == "Final_prodAdv", , drop = FALSE]
      stampsFinalSel <- unique(dtFinalSel$analysisId)

      if (length(stampsFinalSel) > 0) {
        if ("analysisIdName" %in% colnames(dtFinalSel)) {
          names(stampsFinalSel) <- paste(
            dtFinalSel$analysisIdName,
            as.POSIXct(stampsFinalSel, origin = "1970-01-01", tz = "GMT"),
            sep = "_"
          )
        } else {
          names(stampsFinalSel) <- as.character(
            as.POSIXct(stampsFinalSel, origin = "1970-01-01", tz = "GMT")
          )
        }
      }

      updateSelectInput(
        session,
        "finalSelectionStamp",
        choices = c(" " = "", stampsFinalSel),
        selected = as.character(analysis_id)
      )

      # intentionally do NOT touch plotSelectionStamp here

      showNotification(
        "Final decision table saved successfully.",
        type = "message"
      )
    }, ignoreInit = TRUE)

    ###########################################
    # Final report (idx7)
    ###########################################

    observeEvent(data(), {
      req(data())

      dt <- data()$status

      dtInitSel  <- dt[dt$module == "Init_prodAdv",  , drop = FALSE]
      dtPlotSel  <- dt[dt$module == "Plot_prodAdv",  , drop = FALSE]
      dtFinalSel <- dt[dt$module == "Final_prodAdv", , drop = FALSE]

      stampsInitSel  <- make_stamp_choices(dtInitSel)
      stampsPlotSel  <- make_stamp_choices(dtPlotSel)
      stampsFinalSel <- make_stamp_choices(dtFinalSel)

      updateSelectInput(
        session,
        "reportInitialSelectionStamp",
        choices = stampsInitSel,
        selected = if (length(stampsInitSel) > 0) unname(stampsInitSel)[1] else NULL
      )

      updateSelectInput(
        session,
        "reportPlotSelectionStamp",
        choices = c("No plot selection" = "__none__", stampsPlotSel),
        selected = "__none__"
      )

      updateSelectInput(
        session,
        "reportFinalSelectionStamp",
        choices = c("No final selection" = "__none__", stampsFinalSel),
        selected = "__none__"
      )
    })

    final_report_table_data <- reactive({
      build_decision_table_data(
        dt = data(),
        initial_stamp = input$reportInitialSelectionStamp,
        plot_stamp = input$reportPlotSelectionStamp,
        final_stamp = input$reportFinalSelectionStamp,
        final_overrides = NULL
      )
    })

    final_report_table_raw <- reactive({
      tbl <- final_report_table_data()$raw

      drop_cols <- c(
        "entryType",
        "is_check_sort",
        "final_decision",
        "final_manual_decision",
        "final_decision_initial"
      )

      tbl <- tbl[, setdiff(colnames(tbl), drop_cols), drop = FALSE]

      if ("final_decision_initial" %in% colnames(final_report_table_data()$raw)) {
        tbl$final_decision <- final_report_table_data()$raw$final_decision_initial
      }

      tbl
    })

    report <- reactiveVal(NULL)

    observeEvent(input$renderReportProdAdv, {

      req(input$reportInitialSelectionStamp)
      req(input$reportPlotSelectionStamp)
      req(input$reportFinalSelectionStamp)

      shinybusy::show_modal_spinner(spin = "fading-circle", text = "Generating Report...")

      result <- data()
      final_table_export <- final_report_table_raw()

      src <- normalizePath(system.file("rmd","reportProdAdv.Rmd", package = "bioflow"))

      tmp_report <- file.path(tempdir(), "reportProdAdv_download.Rmd")
      tmp_rdata  <- file.path(tempdir(), "resultProdAdv.RData")

      save(result, final_table_export, file = tmp_rdata)
      file.copy(src, tmp_report, overwrite = TRUE)

      old <- setwd(tempdir())
      on.exit(setwd(old), add = TRUE)

      outReport <- rmarkdown::render(
        input = basename(tmp_report),
        params = list(toDownload = TRUE),
        output_format = rmdformats::robobook(toc_depth = 4)
      )

      report(outReport)

      shinybusy::remove_modal_spinner()
      shinyjs::click("downloadReportProdAdv")
    })

    observeEvent(input$runFinalProdAdv, {

      req(input$reportInitialSelectionStamp)
      req(input$reportPlotSelectionStamp)
      req(input$reportFinalSelectionStamp)

      shinybusy::show_modal_spinner(spin = "fading-circle", text = "Generating Report...")

      result <- data()
      final_table_export <- final_report_table_raw()

      src <- normalizePath(system.file("rmd","reportProdAdv.Rmd", package = "bioflow"))

      tmp_report <- file.path(tempdir(), "reportProdAdv_tmp.Rmd")
      tmp_rdata  <- file.path(tempdir(), "resultProdAdv.RData")

      save(result, final_table_export, file = tmp_rdata)
      file.copy(src, tmp_report, overwrite = TRUE)

      updateTabsetPanel(session, "tabsMain", selected = "outputTabs")

      output$reportProdAdv <- renderUI({
        old <- setwd(tempdir())
        on.exit(setwd(old), add = TRUE)

        HTML(
          markdown::markdownToHTML(
            knitr::knit(tmp_report, quiet = TRUE),
            fragment.only = TRUE
          )
        )
      })

      shinybusy::remove_modal_spinner()
    })





  })
}

## To be copied in the UI
# mod_qaRawApp_ui("qaPhenoApp_1")

## To be copied in the server
# mod_qaRawApp_server("qaPhenoApp_1")

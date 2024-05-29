#' dataConsistPotatoApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dataConsistPotatoApp_ui <- function(id){
  ns <- NS(id)
  tagList(


    shiny::mainPanel(width = 12,
                     tabsetPanel( #width=9,
                       type = "tabs",

                       tabPanel(div(icon("book"), "Information") ,
                                br(),
                                h2(strong("Status:")),
                                uiOutput(ns("warningMessage")),
                                tags$body(
                                  h2(strong("Details")),
                                  p("Some additional checks for data consistency are benefitial to ensure a high quality analysis. These can include:"),
                                  # img(src = "www/qaRaw.png", height = 300, width = 600), # add an image
                                  p(strong("Check names-")," The onthology for some crops is particularly important. This tab allows to check this to a reference onthology."),
                                  p(strong("Check traits-")," Checking the relationships between traits is particularly useful to identify issues."),
                                  p(strong("Tag inconsistencies-"),"Once inconsistencies are identified, is important to tag them for posterior management in the analytical modules."),
                                  h2(strong("References")),
                                  h2(strong("Software")),
                                  p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
                                Vienna, Austria. URL https://www.R-project.org/."),
                                )
                       ),
                       tabPanel(div(icon("arrow-right-to-bracket"), "Input"),
                                tabsetPanel(
                                  tabPanel("Check names", icon = icon("dice-one"),
                                           br(),
                                           column(width=12,style = "background-color:grey; color: #FFFFFF",
                                                  column(width=8, selectInput(ns("version2Sta"), "QA stamps to apply to traits", choices = NULL, multiple = TRUE) ),
                                                  column(width=4, tags$br(),
                                                         shinyWidgets::prettySwitch( inputId = ns('launch'), label = "Load example", status = "success"),
                                                  ),
                                           ),
                                           DT::DTOutput(ns("phenoSta")),
                                  ),
                                  tabPanel("Check ??", icon = icon("dice-two"),
                                           br(),
                                           column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                  column(width=6, selectInput(ns("trait2Sta"), "Trait(s) to analyze (required)", choices = NULL, multiple = TRUE) ),
                                                  column(width=6,selectInput(ns("fixedTermSta2"), "Covariable(s) (optional)", choices = NULL, multiple = TRUE) ),
                                                  column(width = 12, style = "background-color:grey; color: #FFFFFF",
                                                         shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Alternative response distributions...",
                                                                             p(span("Family of response variable to be fitted.", style="color:black"), span("(double click in the cells if you would like to model a trait with a different distribution other than normal).", style="color:black")), # table of families to assume
                                                                             DT::DTOutput(ns("traitDistSta")),
                                                         ),
                                                  ),
                                           ),
                                           selectInput(ns("trait3Sta"), "Trait to visualize", choices = NULL, multiple = FALSE),
                                           shiny::plotOutput(ns("plotPredictionsCleanOut")), # plotly::plotlyOutput(ns("plotPredictionsCleanOut")),
                                  ),
                                  tabPanel("Tag inconsistencies", icon = icon("play"),
                                           column(width=12,style = "background-color:grey; color: #FFFFFF",
                                                  br(),
                                                  actionButton(ns("runSta"), "Run STA (click)", icon = icon("play-circle")),
                                                  uiOutput(ns("qaQcStaInfo")),
                                                  br(),
                                           ),
                                           textOutput(ns("outSta")),
                                  ),
                                )# of of tabsetPanel
                       ),
                       tabPanel(div(icon("arrow-right-from-bracket"), "Output" ) , value = "outputTabs",
                                tabsetPanel(
                                  tabPanel("Dashboard", icon = icon("file-image"),
                                           br(),
                                           downloadButton(ns("downloadReportSta"), "Download dashboard"),
                                           br(),
                                           uiOutput(ns('reportSta')),
                                  ),
                                  tabPanel("Modifications", icon = icon("table"),
                                           br(),
                                           DT::DTOutput(ns("predictionsSta")),
                                  ),
                                ) # of of tabsetPanel
                       )# end of output panel


                     )) # end mainpanel


  )
}

#' dataConsistPotatoApp Server Functions
#'
#' @noRd
mod_dataConsistPotatoApp_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_dataConsistPotatoApp_ui("dataConsistPotatoApp_1")

## To be copied in the server
# mod_dataConsistPotatoApp_server("dataConsistPotatoApp_1")

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
                                           # img(src = "www/mta.png", height = 300, width = 450), # add an image
                                    ),
                                    column(width = 6,
                                           h2(strong("Details")),
                                           p("The ....
                                The way the arguments are used is the following:"),

                                           p(strong("Epochs.-")," ."),
                                           p(strong("Learning rate.-")," ."),
                                           p(strong("Activation function.-")," ."),
                                           p(strong("Regularization method.-")," ."),
                                           p(strong("Regularization rate.-"),"  ."),
                                           p(strong("Ratio of training to test data.-")," "),
                                           p(strong("Batch size.-"),"  "),
                                           p(strong("Number of layers.-")," ."),
                                           p(strong("Number of neurons.-")," ."),
                                           h2(strong("References:")),
                                           p(""),
                                           p("."),
                                           h2(strong("Software used:")),
                                           p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
                                Vienna, Austria. URL https://www.R-project.org/."),
                                           p("Gulli A, Pal S. Deep learning with Keras. Packt Publishing Ltd; 2017."),
                                           p("Martin Abadi, Ashish Agarwal, ... Yuan Yu, and Xiaoqiang Zheng. TensorFlow: Large-scale machine learning on heterogeneous systems, 2015. Software available from tensorflow.org."),
                                    ),
                           ),
                           tabPanel(div(icon("arrow-right-to-bracket"), "Input steps"),
                                    tabsetPanel(
                                      tabPanel(div( icon("dice-one"), "Pick STA-stamp", icon("arrow-right") ), # icon = icon("dice-one"),
                                               br(),
                                               column(width=12, # style = "background-color:grey; color: #FFFFFF",
                                                      column(width=2, br(), actionButton(ns("runML"), "", icon = icon("play-circle")), ),
                                                      column(width=2, numericInput(ns("nEpochs"), label = "Epochs", value = 1000),  ),
                                                      column(width=2, numericInput(ns("learnRate"), label = "Learning rate", value = 0.03),  ),
                                                      column(width=2, selectInput(ns("activation"), label = "Activation", choices = list("relu","tanh","sigmoid","linear"), selected = "relu", multiple=FALSE), ),
                                                      column(width=2, selectInput(ns("regularization"), label = "Regularization", choices = list("none","L1","L2"), selected = "none", multiple=FALSE), ),
                                                      column(width=2, numericInput(ns("regularRate"), label = "Regularization rate", value = 0),  ),

                                               ),
                                               column(width=2,
                                                      column(width=12, sliderInput(ns("slider1"), label = "Batch size", min = 1, max = 2000, value = c(1, 100)) ),
                                                      column(width=12, sliderInput(ns("slider2"), label = "Ratio training/test", min = 1, max = 2000, value = c(1, 100)) ),
                                                      ),
                                               column(width=10,
                                                      column(width=2,
                                                             # column(width=12,
                                                                    selectInput(ns("labels"), label = "Labels", choices = list("geno","pedigree"), selected = NULL, multiple=TRUE),
                                                             #        ),
                                                             # column(width=12,
                                                                    selectInput(ns("features"), label = "Features", choices = list("geno","pedigree"), selected = NULL, multiple=TRUE),
                                                                    # ),
                                                      ),
                                                      column(width=10,
                                                             column(width=12,
                                                                    column(width=3,
                                                                           numericInput(ns("nlayers"), label = "Hidden layers", value = 1),
                                                                           ),
                                                                    ),
                                                             column(width=2,
                                                                    column(width=12,
                                                                           uiOutput(ns("nNeurons")), # dynamic based on n layers
                                                                           ),
                                                                    ),
                                                      ),
                                               ),

                                      ),
                                      tabPanel(div(icon("dice-two"), "Pick the model", icon("arrow-right") ), # icon = icon("dice-two"),



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

    output$nNeurons <- renderUI({
      req(input$nlayers)

      lapply(1:input$nlayers, function(i) {
        # div(
          numericInput(
            session$ns(paste0('nNeurons',i)),
            label = paste0("neurons in layer",i),
            value = 10, # -1 is to subset to only individuals present
            min = 1, max = Inf, step = 1
          )
        #   style = "display: inline-block;"
        # )
      })
    })

  })
}

## To be copied in the UI
# mod_tensorMLApp_ui("tensorMLApp_1")

## To be copied in the server
# mod_tensorMLApp_server("tensorMLApp_1")

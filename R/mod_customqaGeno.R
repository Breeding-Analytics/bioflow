mod_customqaGenoApp_ui <- function(id) {
  ns <- NS(id)
  tagList(shiny::mainPanel(
    width = 12,
    tabsetPanel(
      id = ns("tabsMain"),
      type = "tabs",
      tabPanel(
        "Information-QA-Geno",
        icon = icon("book"),
        br(),
        tags$body(uiOutput(ns("warningMessage")), )
      ),
      tabPanel("Input",
               icon = icon("book"),
               br(),
               tags$body(
                 tabsetPanel(
                   id = "threshold_panel",
                   type = "tabs",
                   tabPanel(
                     "Set thresholds",
                     icon = icon("magnifying-glass-chart"),
                     tags$body(fluidRow(
                       column(width = 8,
                              titlePanel("Summary"), ),
                       column(
                         width = 4,
                         titlePanel("Filtering Parameters"),
                         h4("By locus"),
                         uiOutput(ns("by_locus_filter_params")),
                         br(),
                         h4("By individual"),
                         uiOutput(ns("by_ind_filter_params")),
                         actionButton(inputId = ns('filter_btn'), "Filter")
                       )
                     ))
                   ),
                   tabPanel("Run Analysis", icon = icon("play"), ),
                 )
               )),
      tabPanel("Output",
               icon = icon("book"),
               br(),
               tags$body(), )
    ),
  ))
}

mod_customqaGenoApp_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })
    ############################################################################

    # warning message
    output$warningMessage <- renderUI(if (is.null(data())) {
      HTML(as.character(
        div(
          style = "color: red; font-size: 20px;",
          "Please retrieve or load your data using the 'Data' tab."
        )
      ))
    } else{
      # data is there
      if (!is.null(data()$data$gl)) {
        HTML(as.character(
          div(
            style = "color: green; font-size: 20px;",
            "Data is complete, please proceed to perform the marker QA specifying your input parameters under the Input tabs."
          )
        ))
      } else{
        HTML(as.character(
          div(
            style = "color: red; font-size: 20px;",
            "Please retrieve or load your genotype data using the 'Data' tab. "
          )
        ))
      }
    })


  })
}

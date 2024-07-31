#' sectionInfoDASHApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sectionInfoDASHApp_ui <- function(id){
  ns <- NS(id)
  tagList(


    tags$div(
      fluidRow(
        shinydashboard::box(width = 12, status = "success", collapsible = FALSE,
                            tags$body(
                              p()
                            ),
                            shinydashboard::box(status="success", width = 12,
                                                title = "Dashboards", solidHeader = TRUE,
                                                p("This section has been developed to help the users rebuild their dashboards from previous analysis (e.g., analytical modules),
                                                  or to combine a set of analysis to create summary dashboards (e.g., ABI dashboards). These graphical representations
                                                  of the data should allow scientists to understand better their experiments and extract value to help the decision making."),
                                                img(src = " www/visuals.png", height = 340, width = 600), # add an image
                                                p("Currently, Bioflow has only two stakeholders that have requested a summary dashboard of breeding pipelines to create show some
                                                  metrics. If more are needed please contact us."),
                                                p(strong("References:")),
                                                p("Image taken from https://stackoverflow.com/tags/plotly/info"),
                            )
        ),
        class = "tab-content"
      )
    )


  )
}

#' sectionInfoDASHApp Server Functions
#'
#' @noRd
mod_sectionInfoDASHApp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_sectionInfoDASHApp_ui("sectionInfoDASHApp_1")

## To be copied in the server
# mod_sectionInfoDASHApp_server("sectionInfoDASHApp_1")

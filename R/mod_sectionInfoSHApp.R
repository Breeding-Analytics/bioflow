#' sectionInfoSHApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sectionInfoSHApp_ui <- function(id){
  ns <- NS(id)
  tagList(


    tags$div(
      fluidRow(
        shinydashboard::box(width = 12, status = "success", collapsible = FALSE,
                            tags$body(
                              p()
                            ),
                            shinydashboard::box(status="success", width = 12,
                                                title = "What do we mean by selection history?", solidHeader = TRUE,
                                                p("The selection history modules allow the user to calculate genetic gain or
                                                  population mean and variance changes across generations for traits of
                                                  interest. In addition, it also allows to calculate the so called 'predicted'
                                                  genetic gain which are parameters such as selection intensity, accuracy,
                                                  genetic variance and cycle time in order to monitor the practices and
                                                  operations of the breeding program along with the management of the genetic
                                                  variability."),
                                                img(src = " www/infosh.png", height = 350, width = 600), # add an image
                                                p("The approach used in bioflow consist in using estimates of genetic value from
                                                  the multi-trait analysis for a particular trait or a selection index of traits."),
                                                p("The 'selection signatures' module allow to identify regions of the genome
                                                  that have experienced artificial or natural selection.")
                            )
        ),
        class = "tab-content"
      )
    )


  )
}

#' sectionInfoSHApp Server Functions
#'
#' @noRd
mod_sectionInfoSHApp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_sectionInfoSHApp_ui("sectionInfoSHApp_1")

## To be copied in the server
# mod_sectionInfoSHApp_server("sectionInfoSHApp_1")

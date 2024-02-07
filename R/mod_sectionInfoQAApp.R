#' sectionInfoQAApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sectionInfoQAApp_ui <- function(id){
  ns <- NS(id)
  tagList(


    tags$div(
      fluidRow(
        shinydashboard::box(width = 12, status = "success", collapsible = FALSE,
                            tags$body(
                              p()
                            ),
                            shinydashboard::box(status="success", width = 12,
                                                title = "What do we mean by quality assurance?", solidHeader = TRUE,
                                                p("The analytical modules available expect good quality data to draw meaningful conclusions.
                                                  Quality controls for phenotypes and genotypes are provided to filter and tag records that
                                                  can lead to difficult interpretations."),
                                                img(src = " www/infoqa.png", height = 350, width = 600), # add an image
                                                p("The 'QA for phenotypes' modules allow the identification of outliers and filtering records based on indication columns."),
                                                p("The 'QA for markers' module allows the identification of markers and individuals with
                                                  high levels of missing data, minor allele frequency, heterozigosity and inbreeding.")
                            )
        ),
        class = "tab-content"
      )
    )



  )
}

#' sectionInfoQAApp Server Functions
#'
#' @noRd
mod_sectionInfoQAApp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_sectionInfoQAApp_ui("sectionInfoQAApp_1")

## To be copied in the server
# mod_sectionInfoQAApp_server("sectionInfoQAApp_1")

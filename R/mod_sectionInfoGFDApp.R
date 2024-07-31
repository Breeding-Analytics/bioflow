#' sectionInfoGFDApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sectionInfoGFDApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    tags$div(
      fluidRow(
        shinydashboard::box(width = 12, status = "success", collapsible = FALSE,
                            tags$body(
                              p()
                            ),
                            shinydashboard::box(status="success", width = 12,
                                                title = "What do we mean by frequency-based selection?", solidHeader = TRUE,
                                                p("Sometimes improvement and research programs have identified markers that allow them to do
                                                multiple things such as 1) link markers to traits of interest to do faster selection,
                                                  2) identify/distinguish quickly an individual not belonging to a genetic group (e.g., hybridity test)
                                                  or 3) recover a genetic background quickly by using flanking markers. All these type of methodologies
                                                  leverage from our current understanding of gene segregation and frecuencies."),
                                                img(src = " www/geneflow.jpg", height = 300, width = 700), # add an image
                                                p("Cavalli-Sforza, L. L. (1966). Population structure and human evolution. Proceedings of the Royal Society of London. Series B. Biological Sciences, 164(995), 362-379."),
                                                p("Image taken from https://en.m.wikipedia.org/wiki/File:Gene_flow.jpg"),
                            )
        ),
        class = "tab-content"
      )
    )

  )
}

#' sectionInfoGFDApp Server Functions
#'
#' @noRd
mod_sectionInfoGFDApp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_sectionInfoGFDApp_ui("sectionInfoGFDApp_1")

## To be copied in the server
# mod_sectionInfoGFDApp_server("sectionInfoGFDApp_1")

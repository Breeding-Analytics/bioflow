#' sectionInfoMHApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sectionInfoMHApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    tags$div(
      fluidRow(
        shinydashboard::box(width = 12, status = "success", collapsible = FALSE,
                            tags$body(
                              p()
                            ),
                            shinydashboard::box(status="success", width = 12,
                                                title = "What do we mean by mutation history?", solidHeader = TRUE,
                                                p("Mutation represents the main source of new variation in DNA-based species in our planet. The accumulation of such mutations tells
                                                  us about the evolutionary history of species of interest. Many parameters can be of interest for biologists, such as the mutation
                                                  rate which represents the rate of evolution of species. Other methods to understand evolution linked to mutations are the so-called
                                                  colascent, which allows to simulate back in the time the evolutionary history."),
                                                img(src = " www/mutationcoal.jpg", height = 200, width = 600), # add an image
                                                p("Currently, Bioflow scope to study mutation is limited to metrics such as mutation rate and hopefully in the future we can add sophisticated
                                                  methods such as the coalescent."),
                                                p(strong("References:")),
                                                p("Wakeley, J. H. (2009). Coalescent theory: an introduction."),
                                                p("Image taken from https://en.wikipedia.org/wiki/Multispecies_coalescent_process"),
                            )
        ),
        class = "tab-content"
      )
    )

  )
}

#' sectionInfoMHApp Server Functions
#'
#' @noRd
mod_sectionInfoMHApp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_sectionInfoMHApp_ui("sectionInfoMHApp_1")

## To be copied in the server
# mod_sectionInfoMHApp_server("sectionInfoMHApp_1")

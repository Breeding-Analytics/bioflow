#' sectionInfoGFDHApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sectionInfoGFDHApp_ui <- function(id){
  ns <- NS(id)
  tagList(


    tags$div(
      fluidRow(
        shinydashboard::box(width = 12, status = "success", collapsible = FALSE,
                            tags$body(
                              p()
                            ),
                            shinydashboard::box(status="success", width = 12,
                                                title = "What do we mean by gene-flow and drift history?", solidHeader = TRUE,
                                                p("If approached from the evolutionary perspective, the gene flow and drift lead to the formation of new
                                                  groups that can can result in the development of new species. Is of special interest for evolutionary
                                                  geneticists to understand the development of these clusters. Some of these methods are normally referred as
                                                  population-structure based methods. These methods use genotypic or phenotypic information from a set of
                                                  individuals to understand their level of differentiation and predict future consequences and exploit
                                                  other charactersitics such as heterosis."),
                                                img(src = " www/popstr.png", height = 300, width = 1000), # add an image
                                                p(strong("References:")),
                                                p("Cavalli-Sforza, L. L. (1966). Population structure and human evolution. Proceedings of the Royal Society of London. Series B. Biological Sciences, 164(995), 362-379."),
                                                p("Image taken from https://en.wikipedia.org/wiki/Population_structure_%28genetics%29"),
                            )
        ),
        class = "tab-content"
      )
    )


  )
}

#' sectionInfoGFDHApp Server Functions
#'
#' @noRd
mod_sectionInfoGFDHApp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_sectionInfoGFDHApp_ui("sectionInfoGFDHApp_1")

## To be copied in the server
# mod_sectionInfoGFDHApp_server("sectionInfoGFDHApp_1")

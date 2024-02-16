#' meetTheTeamApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_meetTheTeamApp_ui <- function(id){
  ns <- NS(id)
  tagList(


    tags$div(
      fluidRow(
        shinydashboard::box(width = 12, status = "success", collapsible = FALSE,
                            tags$body(
                              p()
                            ),
                            shinydashboard::box(status="success", width = 12,
                                                title = "The team behind the scenes:", solidHeader = TRUE,
                                                img(src = " www/team.jpeg", height = 460, width = 600), # add an image
                                                p("Inception meeting celebrated in Nairobi Kenya. From left to right; Star (consultant), Khaled (ICARDA), Young-Wha (BMGF),
                                                  Bert (CIP), Johan (CIAT), Dorcus (GI-BRI), Juan (CIMMYT), Eduardo (IRRI), Keith (CIMMYT), Sergio (CIAT),
                                                  Aubin (AfricaRice), Raul (CIP).")
                            ),
                            shinydashboard::box(status="success", width = 12,
                                                title = "Contributors of analytical modules:", solidHeader = TRUE,
                                                img(src = " www/centers.png", height = 500, width = 600), # add an image
                                                p(" "),
                                                p("All CGIAR centers with Biometrics capacity have contributed to the design of the breeding analytics platform
                                                  and currently work in developing analytical modules. Want to contribute? Contact us.")
                            )
        ),
        class = "tab-content"
      )
    )


  )
}

#' meetTheTeamApp Server Functions
#'
#' @noRd
mod_meetTheTeamApp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_meetTheTeamApp_ui("meetTheTeamApp_1")

## To be copied in the server
# mod_meetTheTeamApp_server("meetTheTeamApp_1")

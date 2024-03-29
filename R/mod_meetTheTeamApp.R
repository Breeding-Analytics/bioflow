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
                            column(width = 12,

                                   shinydashboard::box(status="success", width = 6,
                                                       title = "Contributors of analytical modules:", solidHeader = TRUE,
                                                       img(src = " www/centers.png", height = 500, width = 600), # add an image
                                                       p(" "),
                                                       p("All CGIAR centers with Biometrics capacity have contributed to the design of the breeding analytics platform
                                                  and currently work in developing analytical modules. Want to contribute? Contact us.")
                                   ),
                                   shinydashboard::box(status="success", width = 6,
                                                       title = "Team Activities:", solidHeader = TRUE,
                                                       slickR::slickROutput(ns("teamPhoto")),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       br()
                                   )

                                   ),
                            # shinydashboard::box(status="success", width = 12,
                            #                     title = "The team behind the scenes:", solidHeader = TRUE,
                            #                     img(src = " www/team.jpeg", height = 460, width = 600), # add an image
                            #                     p("Inception meeting celebrated in Nairobi Kenya. From left to right; Star (consultant), Khaled (ICARDA), Young-Wha (BMGF),
                            #                       Bert (CIP), Johan (CIAT), Dorcus (GI-BRI), Juan (CIMMYT), Eduardo (IRRI), Keith (CIMMYT), Sergio (CIAT),
                            #                       Aubin (AfricaRice), Raul (CIP).")
                            # ),

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

    output$teamPhoto <- slickR::renderSlickR({
      imgs <- list.files(paste0(getwd(),"/inst/app/www/activities"), pattern=".jpg", full.names = TRUE)
      slickR::slickR(imgs) + slickR::settings(dots = TRUE, infinite = TRUE, speed = 500,
                                              fade = TRUE, cssEase = "linear", arrows = FALSE)
    })

  })
}

## To be copied in the UI
# mod_meetTheTeamApp_ui("meetTheTeamApp_1")

## To be copied in the server
# mod_meetTheTeamApp_server("meetTheTeamApp_1")

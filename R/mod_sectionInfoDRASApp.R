#' sectionInfoDRASApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sectionInfoDRASApp_ui <- function(id) {
  ns <- NS(id)
  tagList(



    tags$div(
      fluidRow(
        shinydashboard::box(width = 12, status = "success", collapsible = FALSE,
                            tags$body(
                              p()
                            ),
                            shinydashboard::box(status="success", width = 12,
                                                title = "Data Retrieval and Saving", solidHeader = TRUE,
                                                p("The first step in a pipeline consists in being able to retrieve phenotypic, genotypic, environmental information to be cleaned and
                                                  analyzed with the different modules available. This section also allows to save the retrieved and analyzed data as an .RData object
                                                  that can be later uploaded."),
                                                # img(src = " www/transform.png", height = 400, width = 700), # add an image
                                                p(strong("References:")),
                                                p("Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) The New S Language. Wadsworth & Brooks/Cole."),
                            )
        ),
        class = "tab-content"
      )
    )




  )
}

#' sectionInfoDRASApp Server Functions
#'
#' @noRd
mod_sectionInfoDRASApp_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_sectionInfoDRASApp_ui("sectionInfoDRASApp_1")

## To be copied in the server
# mod_sectionInfoDRASApp_server("sectionInfoDRASApp_1")

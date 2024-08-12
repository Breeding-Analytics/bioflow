#' sectionInfoTransformApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sectionInfoTransformApp_ui <- function(id){
  ns <- NS(id)
  tagList(


    tags$div(
      fluidRow(
        shinydashboard::box(width = 12, status = "success", collapsible = FALSE,
                            tags$body(
                              p()
                            ),
                            shinydashboard::box(status="success", width = 12,
                                                title = "Trait transformations", solidHeader = TRUE,
                                                p("Traits of interest for biologist present a variety of scales and more important, different distributions.
                                                  Although bioflow allows flexibility to fit models on traits that present different distributions other than
                                                  normal, having the ability to transform certain traits present another opportunity to simplify the models. In
                                                  adiition, sometimes scientist just need to create a new trait that is a linear transformation of others."),
                                                img(src = " www/transform.png", height = 350, width = 700), # add an image
                                                p("Bioflow currently offers few transformations such as log, square root, identity, among others."),
                                                p(strong("References:")),
                                                p("Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) The New S Language. Wadsworth & Brooks/Cole. (for log, log10 and exp.)"),
                                                p("Image taken from https://dataalltheway.com/posts/001-data-transformation/index.html"),
                            )
        ),
        class = "tab-content"
      )
    )


  )
}

#' sectionInfoTransformApp Server Functions
#'
#' @noRd
mod_sectionInfoTransformApp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_sectionInfoTransformApp_ui("sectionInfoTransformApp_1")

## To be copied in the server
# mod_sectionInfoTransformApp_server("sectionInfoTransformApp_1")

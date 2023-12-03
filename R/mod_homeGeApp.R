#' homeGeApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_homeGeApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    tags$head(
      tags$meta(charset = "utf-8"),
      tags$meta(
        `http-quiv` = "x-ua-compatible",
        content = "ie=edge"
      ),
      tags$meta(
        name = "viewport",
        content = "width=device-width, initial-scale=1"
      ),
      tags$link(
        type = "text/css",
        rel = "stylesheet",
        href = "css/styles.css"
      )
    ),

    tags$div(
      class = "landing-wrapper",
      # child element 1: images
      tags$div(
        class = "landing-block background-content",
        # images: top -> bottom, left -> right
        tags$div(tags$img(src = "www/sta.JPG")),
        tags$div(tags$img(src = "www/homeImageAfrica4.png")),
        tags$div(tags$img(src = "www/homeImageAmericas2.jpg")),
        tags$div(tags$img(src = "www/homeImageEurope3.jpg"))
      ),
      # child element 2: content
      tags$div(
        class = "landing-block foreground-content",
        tags$div(
          class = "foreground-text",
          tags$img(src = "www/cgiar.png"),
          tags$h1("Biometrical Genetics Workflow (bioflow)"),
          tags$p(
            "The OneCGIAR biometrical genetics workflow or pipeline has been built to provide access to automatic state-of-the-art genetic evaluation.",
            "Designed to be database agnostic, it only requires connecting to one of the available databases (EBS, BMS, BreedBase) to retrieve phenotypic,",
            "genotypic or pedigree data, quickly map some features (columns) required, and carry the analytical procedures."
          )
        )
      )
    )

  )
}

#' homeGeApp Server Functions
#'
#' @noRd
mod_homeGeApp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_homeGeApp_ui("homeGeApp_1")

## To be copied in the server
# mod_homeGeApp_server("homeGeApp_1")

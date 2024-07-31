#' hybridityApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_hybridityApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    HTML( as.character(div(style="color: red; font-size: 30px;", "Under construction.")) ),
    img(src = "www/bongo.gif", height = 400, width = 200), # add an image

  )
}

#' hybridityApp Server Functions
#'
#' @noRd
mod_hybridityApp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_hybridityApp_ui("hybridityApp_1")

## To be copied in the server
# mod_hybridityApp_server("hybridityApp_1")

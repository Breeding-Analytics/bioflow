#' gwasqkApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_gwasqkApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    HTML( as.character(div(style="color: red; font-size: 30px;", "Under construction.")) ),
    img(src = "www/bongo.gif", height = 400, width = 200), # add an image

  )
}

#' gwasqkApp Server Functions
#'
#' @noRd
mod_gwasqkApp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_gwasqkApp_ui("gwasqkApp_1")

## To be copied in the server
# mod_gwasqkApp_server("gwasqkApp_1")

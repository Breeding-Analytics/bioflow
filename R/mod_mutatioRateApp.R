#' mutatioRateApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mutatioRateApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    HTML( as.character(div(style="color: red; font-size: 30px;", "Module under construction.")) ),
    img(src = "www/bongo.gif", height = 400, width = 200), # add an image

  )
}

#' mutatioRateApp Server Functions
#'
#' @noRd
mod_mutatioRateApp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_mutatioRateApp_ui("mutatioRateApp_1")

## To be copied in the server
# mod_mutatioRateApp_server("mutatioRateApp_1")

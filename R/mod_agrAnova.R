#' agrAnova UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_agrAnova_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' agrAnova Server Functions
#'
#' @noRd 
mod_agrAnova_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_agrAnova_ui("agrAnova_1")
    
## To be copied in the server
# mod_agrAnova_server("agrAnova_1")

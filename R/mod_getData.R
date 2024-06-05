#' getData UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_getData_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' getData Server Functions
#'
#' @noRd 
mod_getData_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_getData_ui("getData_1")
    
## To be copied in the server
# mod_getData_server("getData_1")

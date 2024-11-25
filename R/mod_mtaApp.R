#' mtaApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_mtaApp_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' mtaApp Server Functions
#'
#' @noRd 
mod_mtaApp_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_mtaApp_ui("mtaApp_1")
    
## To be copied in the server
# mod_mtaApp_server("mtaApp_1")

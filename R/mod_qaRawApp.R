#' qaRawApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_qaRawApp_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' qaRawApp Server Functions
#'
#' @noRd 
mod_qaRawApp_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_qaRawApp_ui("qaRawApp_1")
    
## To be copied in the server
# mod_qaRawApp_server("qaRawApp_1")

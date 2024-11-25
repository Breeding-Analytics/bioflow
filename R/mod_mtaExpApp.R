#' mtaExpApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_mtaExpApp_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' mtaExpApp Server Functions
#'
#' @noRd 
mod_mtaExpApp_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_mtaExpApp_ui("mtaExpApp_1")
    
## To be copied in the server
# mod_mtaExpApp_server("mtaExpApp_1")

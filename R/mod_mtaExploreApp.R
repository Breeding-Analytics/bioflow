#' mtaExploreApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_mtaExploreApp_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' mtaExploreApp Server Functions
#'
#' @noRd 
mod_mtaExploreApp_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_mtaExploreApp_ui("mtaExploreApp_1")
    
## To be copied in the server
# mod_mtaExploreApp_server("mtaExploreApp_1")

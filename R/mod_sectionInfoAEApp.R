#' sectionInfoAEApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sectionInfoAEApp_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' sectionInfoAEApp Server Functions
#'
#' @noRd 
mod_sectionInfoAEApp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_sectionInfoAEApp_ui("sectionInfoAEApp_1")
    
## To be copied in the server
# mod_sectionInfoAEApp_server("sectionInfoAEApp_1")

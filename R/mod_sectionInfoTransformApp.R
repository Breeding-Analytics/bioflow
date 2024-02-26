#' sectionInfoTransformApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sectionInfoTransformApp_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' sectionInfoTransformApp Server Functions
#'
#' @noRd 
mod_sectionInfoTransformApp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_sectionInfoTransformApp_ui("sectionInfoTransformApp_1")
    
## To be copied in the server
# mod_sectionInfoTransformApp_server("sectionInfoTransformApp_1")

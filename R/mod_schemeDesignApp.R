#' schemeDesignApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_schemeDesignApp_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' schemeDesignApp Server Functions
#'
#' @noRd 
mod_schemeDesignApp_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_schemeDesignApp_ui("schemeDesignApp_1")
    
## To be copied in the server
# mod_schemeDesignApp_server("schemeDesignApp_1")

#' traitTransformApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_traitTransformApp_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' traitTransformApp Server Functions
#'
#' @noRd
mod_traitTransformApp_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_traitTransformApp_ui("traitTransformApp_1")

## To be copied in the server
# mod_traitTransformApp_server("traitTransformApp_1")

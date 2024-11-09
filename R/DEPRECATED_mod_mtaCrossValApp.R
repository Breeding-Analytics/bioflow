#' mtaCrossValApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mtaCrossValApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    HTML( as.character(div(style="color: red; font-size: 30px;", "Module under construction.")) ),
    img(src = "www/bongo.gif", height = 400, width = 200), # add an image

  )
}

#' mtaCrossValApp Server Functions
#'
#' @noRd
mod_mtaCrossValApp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_mtaCrossValApp_ui("mtaCrossValApp_1")

## To be copied in the server
# mod_mtaCrossValApp_server("mtaCrossValApp_1")

#' reportSTA UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_reportSTA_ui <- function(id){
  ns <- NS(id)
  tagList(

    sidebarPanel(),
    mainpanel(
      uiOutput(ns('markdown'))
    )






  )
}

#' reportSTA Server Functions
#'
#' @noRd
mod_reportSTA_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$markdown <- renderUI({
      HTML(markdown::markdownToHTML(knit('R/testing_sta.Rmd', quiet = TRUE)))
    })

  })
}

## To be copied in the UI
# mod_reportSTA_ui("reportSTA_1")

## To be copied in the server
# mod_reportSTA_server("reportSTA_1")

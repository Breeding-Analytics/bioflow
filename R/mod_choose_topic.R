#' choose_topic UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_choose_topic_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarPanel(
      tags$style(".well {background-color:black; color: #FFFFFF;}"),
      actionButton("STA", "Single Trial Analysis"),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      actionButton("MTA", "Multi-Trials Analysis")
    ),
    mainPanel(
      tabPanel("Workflow", "Bla Bla Bla"
      ))
  )
}

#' choose_topic Server Functions
#'
#' @noRd
mod_choose_topic_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_choose_topic_ui("choose_topic_1")

## To be copied in the server
# mod_choose_topic_server("choose_topic_1")

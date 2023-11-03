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

    sidebarPanel(

    ),

    mainpanel(
      div(
        tags$p("Please download the report below:")
      ),
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

    output$reportSta <- renderUI({


      # rmarkdown::render(
      #   # input RMD file
      #   input = ("/Users/idieng/Downloads/testRMD/test2RMD/report_STA.Rmd"),
      #   params = list(
      #     data_rds_sta = "/Users/idieng/Downloads/testRMD/test2RMD/result.rds"
      #   ), output_file = file
      # )

      rmarkdown::render(
        # input RMD file
        input = ("R/report_STA2.Rmd"),
        params = list(
          data_rds_sta = "R/result.rds"
        )
      )

    })

  })
}

## To be copied in the UI
# mod_reportSTA_ui("reportSTA_1")

## To be copied in the server
# mod_reportSTA_server("reportSTA_1")

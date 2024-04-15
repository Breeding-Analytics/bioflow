#' contactUsApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_contactUsApp_ui <- function(id){
  ns <- NS(id)
  tagList(


    tags$div(
      fluidRow(
        shinydashboard::box(width = 12, status = "success", collapsible = FALSE,

                            h3(strong("Contact us")),
                            p("Please use the following link", tags$a(href="https://brservices.atlassian.net/servicedesk/customer/portal/3", "(BIOFLOW Support Desk)", target="_blank") ,
                              "to reach our Help Desk and send us your question or request."),

                            h3(strong("Local installation")),

                            p(" If you wish to install bioflow locally in your computer you can run the following three lines in your R or R studio console:"),
                            p("devtools::install_github('Breeding-Analytics/bioflow')"),
                            p("library(bioflow)"),
                            p(" bioflow::run_app()"),
                            p("The first line will install bioflow as an r package in your computer. The second line will call the library/application to the environment. And the third line will start the application."),

                            h3(strong("How to contribute?")),
                            p("We do generate and maintain our code in", tags$a(href="https://github.com/Breeding-Analytics", "Github.", target="_blank") ,
                              "If you want to contribute you can download a sample data object and generate 2 files;", strong("1)")," an R script with a function that uses as
                              input the data object, performs your desired calculations, and returns the same data object. This file should be pushed to the",
                              tags$a(href="https://github.com/Breeding-Analytics/cgiarPipeline", "cgiarPipeline", target="_blank") ," package, ", strong("2)")," an R script
                              for the shiny interface which can use the R function. This file should be pushed to the ",
                              tags$a(href="https://github.com/Breeding-Analytics/bioflow", "bioflow", target="_blank"), "package."),

                            img(src = "www/contribute.png", height = 500, width = 850), # add an image

        )
      ),
      class = "tab-content"
    )


  )
}

#' contactUsApp Server Functions
#'
#' @noRd
mod_contactUsApp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_contactUsApp_ui("contactUsApp_1")

## To be copied in the server
# mod_contactUsApp_server("contactUsApp_1")

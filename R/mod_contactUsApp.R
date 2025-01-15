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
                            p("Please use the following link", tags$a(href="https://github.com/Breeding-Analytics/bioflow/issues", "(BIOFLOW Github Support Desk)", target="_blank") ,
                              "to reach our Help Desk and send us your question or request."),

                            h3(strong("Local installation")),

                            p(" If you wish to install bioflow locally in your computer you have two options; 1) install it as an R library, or 2) download the bioflow portable for Windows systems."),
                            p("The option 1) to install it as an R library requires to run the following three lines in your R or R studio console:"),
                            p("remotes::install_github('Breeding-Analytics/bioflow')"),
                            p("library(bioflow)"),
                            p(" bioflow::run_app()"),
                            p("The first line will install bioflow as an r package in your computer. The second line will call the library/application to the environment. And the third line will start the application."),
                            p("The option 2) to have a portable version of bioflow in an USB or computer is only functional for Windows and requires to just download it from Github"),
                            p("https://github.com/Breeding-Analytics/bioflowPortable"),
                            p("Additional instructions are available in the website above."),
                            h3(strong("How to contribute to bioflow?")),
                            p("We do generate and maintain our code in", tags$a(href="https://github.com/Breeding-Analytics", "Github.", target="_blank") ,
                              "If you want to contribute you can clone the repository and use the sample data object to generate 2 files;", strong("1)")," an R script with a function that uses as
                              input the data object, performs your desired calculations, and returns the same data object. This R function file should be pushed to the",
                              tags$a(href="https://github.com/Breeding-Analytics/cgiarPipeline", "cgiarPipeline", target="_blank") ," package, ", strong("2)")," an R script
                              for the shiny interface that uses behind the scenes the R function. This file should be pushed to the ",
                              tags$a(href="https://github.com/Breeding-Analytics/bioflow", "bioflow", target="_blank"), "package. If you are only comfortable
                              developing the R pipeline function and need support wit the interface contact us to support you."),

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

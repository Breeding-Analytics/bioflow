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
                            p("Please use the following link",
                              tags$a(href="https://github.com/Breeding-Analytics/bioflow/issues", "(BIOFLOW Github Support Desk)", target="_blank") ,
                              "to reach our Help Desk and send us your question or request."),

                            h3(strong("Local installation")),
                            p("If you wish to install bioflow locally on your computer, you have two options:"),
                            p(style = "margin-left: 20px;", "1. install it as an R package, or", tags$br(),
                              "2. download the bioflow portable for Windows systems."),
                            p(tags$strong("Option 1: Install bioflow as an R package")),
                            p("To install bioflow as an R package, run the following three lines in your R or R studio console:"),
                            pre(style = "background-color: #f6f8fa; padding: 12px; border-radius: 8px; font-family: monospace;",
                                paste('remotes::install_github("Breeding-Analytics/bioflow")',
                                      'library(bioflow)',
                                      'bioflow::run_app()',
                                      sep = "\n")),
                            p("Notes:", tags$br(),
                              "The first line will install bioflow as an R package on your computer.", tags$br(),
                              "The second line will load the package into the R environment.", tags$br(),
                              "And the third line will launch the application."),
                            p(tags$strong("Option 2: Download the bioflow portable for Windows systems")),
                            p("To use the portable version of bioflow, download it from GitHub and save it to a USB drive or your local computer:", tags$br(),
                              tags$a(href="https://github.com/Breeding-Analytics/bioflowPortable", "https://github.com/Breeding-Analytics/bioflowPortable", target="_blank")),
                            p("Notes:", tags$br(),
                              "The portable version is available only for Windows systems.", tags$br(),
                              "Additional instructions are provided on the GitHub page."),

                            h3(strong("How to contribute to bioflow?")),
                            p("We develop and maintain our code on", tags$a(href="https://github.com/Breeding-Analytics", "Github.", target="_blank") ,
                              "If you want to contribute, you can clone the repository and use the sample data object as basis in creating the two necessary files:"),
                            p(style = "margin-left: 20px;", tags$strong("1. R script for the function: "),"This script should contain a function that uses the sample data object as input,
                              performs your desired calculations, and returns the modified data object (making sure that the structure of the data object remains the same). This function file should be added to the",
                              tags$a(href="https://github.com/Breeding-Analytics/cgiarPipeline", "cgiarPipeline", target="_blank"),
                              " package. ", tags$br(),
                              tags$strong("2. R script for the shiny interface: "),"This script should use the function from file 1 behind the scenes. This file should be added to the",
                              tags$a(href="https://github.com/Breeding-Analytics/bioflow", "bioflow", target="_blank"),
                              "package."),
                            p("If you are only comfortable developing the function for the cgiarPipeline package and need help with the shiny interface, please contact us so we can support you."),
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

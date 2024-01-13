#' aboutApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_aboutApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    tags$div(
      fluidRow(
        shinydashboard::box(width = 12, status = "success", collapsible = FALSE,
                            tags$body(
                              p()
                            ),
                            shinydashboard::box(status="success", width = 12,
                                                title = "Version control technology", solidHeader = TRUE,
                                                img(src = " www/github.png", height = 320, width = 600), # add an image
                                                p("Our front and back end code is stored in Github to ensure team collaboration and quick fixes and improvement.")
                            ),
                            shinydashboard::box(status="success", width = 12,
                                                title = "Developing environment (IDE)", solidHeader = TRUE,
                                                img(src = " www/rstudio2.png", height = 220, width = 600), # add an image
                                                p(" "),
                                                p("Our team uses R studio to develop packages, functions and pipelines for easy testing. In addition, the interface is developed using the shiny technology under the golem framework.")
                            ),
                            shinydashboard::box(status="success", width = 12,
                                                title = "Data storage technology", solidHeader = TRUE,
                                                img(src = " www/aws.png", height = 230, width = 600), # add an image
                                                p(" "),
                                                p("The data extracted and produced is stored using an AWS-S3 container for flexibility and interoperability with other systems.")
                            ),
                            shinydashboard::box(status="success", width = 12,
                                                title = "Deployment technology", solidHeader = TRUE,
                                                img(src = " www/docker2.png", height = 350, width = 600), # add an image
                                                p("We use the docker technology to ensure the stability of our software and ensure the version control of our analytical pipeline.")
                            )
        ),
        class = "tab-content"
      )
    )


  )
}

#' aboutApp Server Functions
#'
#' @noRd
mod_aboutApp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_aboutApp_ui("aboutApp_1")

## To be copied in the server
# mod_aboutApp_server("aboutApp_1")

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


    tags$head(
      tags$meta(charset = "utf-8"),
      tags$meta(
        `http-quiv` = "x-ua-compatible",
        content = "ie=edge"
      ),
      tags$meta(
        name = "viewport",
        content = "width=device-width, initial-scale=1"
      ),
      tags$link(
        type = "text/css",
        rel = "stylesheet",
        href = "css/styles.css"
      )
    ),

    tags$div(
      class = "landing-wrapper",
      # child element 1: images
      tags$div(
        class = "landing-block background-content",
        # images: top -> bottom, left -> right
        tags$div(
          tags$h2("Version control technology"),
          tags$p("Our front and back end code is stored in Github to ensure team collaboration and quick fixes and improvement."),
          tags$img(src = "www/github.png")
        ),
        tags$div(
          tags$h2("Developing environment (IDE)"),
          tags$p("Our team uses R studio to develop packages, functions and pipelines for easy testing."),
          tags$p("In addition, the interface is developed using the shiny technology under the golem framework."),
          tags$img(src = "www/rstudio2.png")
        ),
        tags$div(
          tags$h2("Data storage technology"),
          tags$p("The data extracted and produced is stored using an AWS-S3 container for flexibility and interoperability with other systems."),
          tags$img(src = "www/aws.png")
        ),
        tags$div(
          tags$h2("Deployment technology"),
          tags$p("We use the docker technology to ensure the stability of our software and ensure the version control of our analytical pipeline."),
          tags$img(src = "www/docker2.png")
        )
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

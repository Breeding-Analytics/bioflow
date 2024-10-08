#' homeApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_homeApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    # below code chunks taken from taken from https://github.com/davidruvolo51/shinyAppTutorials/blob/main/shiny-landing-page/app.R
    # we also pasted the code from https://github.com/davidruvolo51/shinyAppTutorials/tree/main/shiny-landing-page/www/css and pasted it in our custom.css file
    # except for the following portions:
    # html, body {
    #   width: 100%;
    #   padding: 0;
    #   margin: 0;
    #   font-size: 16pt;
    # }
    #
    # /* override bootstrap */
    #   body > .container-fluid {
    #     margin-top: -20px !important;
    #     margin-bottom: 0 !important
    #   }
    #
    # .container-fluid {
    #   padding: 0 !important;
    #   margin: 0 !important;
    # }

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
      ),

      ### START: OAuth2 JS helper functions ####################################

      tags$script('Shiny.addCustomMessageHandler("redirect", function(url) {window.location.href = (url);});'),
      tags$script('Shiny.addCustomMessageHandler("popup", function(url) {window.open(url);});'),
      tags$script('if(window.location.href.indexOf("redirect=") !== -1) {window.location.href = decodeURIComponent(atob(window.location.href.split("redirect=")[1]));};'),

      tags$input(id = "cookies", type = "hidden", value = ""),
      tags$script('$(document).on("shiny:sessioninitialized", function() {
                  document.getElementById("cookies").value = document.cookie;
                  Shiny.setInputValue("cookies", document.cookie)});'),

      if (Sys.getenv("SHINY_PORT") == "") {
        # ensure using localhost domain instead of 127.0.0.1 to match redirect uri registered for the oauth2 client
        tags$script('if(location.hostname == "127.0.0.1") {window.location.href = location.protocol+"//localhost:"+location.port;}')
      }

      ### END: OAuth2 JS helper functions ######################################
    ),

    tags$div(
      class = "landing-wrapper",
      # child element 1: images
      tags$div(
        class = "landing-block background-content",
        # images: top -> bottom, left -> right
        tags$div(tags$img(src = "www/homeImageAsia.JPG")),
        tags$div(tags$img(src = "www/homeImageAfrica4.jpg")),
        tags$div(tags$img(src = "www/homeImageAmericas2.jpg")),
        tags$div(tags$img(src = "www/homeImageEurope3.jpg"))
      ),
      # child element 2: content
      tags$div(
        class = "landing-block foreground-content",
        tags$div(
          class = "foreground-text",
          tags$img(src = "www/cgiar.png", height = 136, width = 480),
          tags$h2(span("Biometrical Genetics Workflow (bioflow)", tags$a(href="https://www.youtube.com/playlist?list=PLZ0lafzH_UmclOPifjCntlMzysEB2_2wX", icon("youtube") , target="_blank"), style="color:darkcyan")),
          # tags$h1("Biometrical Genetics Workflow (bioflow)"),
          tags$p(
            "The OneCGIAR biometrical genetics workflow or pipeline has been built to access methods for understanding or using evolutionary forces",
            "(mutation, gene flow, migration and selection) such as automatic state-of-the-art genetic evaluation (selection force) in decision-making.",
            "Designed to be database agnostic, it can retrieve data from the available phenotypic-pedigree databases (EBS, BMS, BreedBase),",
            "genotypic databases (GIGWA), and environmental databases (NASAPOWER), and carry the analytical procedures."
          )
        )
      )
    )


  )
}

#' homeApp Server Functions
#'
#' @noRd
mod_homeApp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_homeApp_ui("homeApp_1")

## To be copied in the server
# mod_homeApp_server("homeApp_1")

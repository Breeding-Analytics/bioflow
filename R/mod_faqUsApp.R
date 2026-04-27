#' faqUsApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_faqUsApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    tags$div(
      fluidRow(
        shinydashboard::box(width = 12, status = "success", collapsible = FALSE,

                            column(width = 6,

                            h3(strong("Frequently asked questions")),

                            br(),

                            h4(strong("Why breeding analytics is important for my breeding program?")),

                            br(),

                            p("There are three main areas where breeding analytics is relevant for your organization (among others):"),

                            p(strong("Parental selection:"), "The selection for complex traits managed by hundreds or thousands of genes requires more than a trained eye.
                              Biometrical genetics models allow to dissect the genetic signal from the environmental effects to allow the select parents with high breeding value.
                              Together with selection indices and optimal contribution, breeding analytics guarantee greater genetic gains for complex traits compared to mass selection (visual selection)."),

                            p(strong("Product development:"), "Complex target population of environments require the proper understanding of genotype by environment interactions,
                              and stability and sensitivity of materials to the different environmental conditions to be faced by the farmers. Biometrical genetics models
                              can derive the sensititvity and stability of materials in few paramters and visualizations that guarantee the advacement of better and more stable products compared
                              to classical approaches."),

                            p(strong("Trait discovery and introgression:"), "Biometrical genetics offer a variety of linear and non-linear models to identify genes behind traits of human interest.
                              In addition, biometrical procedures make more efficient the introgression of beneficial alleles in terms of resources like time and cost."),

                            br(),

                            h4(strong("What is different between bioflow and other platforms?")),

                            br(),

                            p("The strength of bioflow compared to other platforms is our flexible data structure that allows the transfer of data among different applications/modules. This way we can
                              decouple or make the modules independent, so different biometricians can work in different modules without dependency on another. Certain modules depend on the results
                              of another module (e.g., two stage analysis) but still certain level of independence exist."),
                            ),
                            column(width = 6,
                                   img(src = "www/bioflowTransBg.png", height = 600, width = 600), # add an image
                            ),



        )
      ),
      class = "tab-content"
    )

  )
}

#' faqUsApp Server Functions
#'
#' @noRd
mod_faqUsApp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_faqUsApp_ui("faqUsApp_1")

## To be copied in the server
# mod_faqUsApp_server("faqUsApp_1")

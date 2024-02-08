#' sectionInfoGEApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sectionInfoGEApp_ui <- function(id){
  ns <- NS(id)
  tagList(


    tags$div(
      fluidRow(
        shinydashboard::box(width = 12, status = "success", collapsible = FALSE,
                            tags$body(
                              p()
                            ),
                            shinydashboard::box(status="success", width = 12,
                                                title = "What is genetic evaluation?", solidHeader = TRUE,
                                                p("Genetic evaluation is the process to disect the genetic signal from the phenotypic
                                                  records to estimate surrogates of genetic value [i.e., breeding values (BV), general
                                                  combining ability (GCA), total genetic value(GV)] in oder to use such estimates to apply
                                                  artifical selection and increase the allele frequencies of genes affecting the
                                                  expression of the traits of interest in a particular target
                                                  population of environments (TPE)."),
                                                img(src = " www/infoge.png", height = 350, width = 600), # add an image
                                                p("The approach used in bioflow is the so-called 'two-step'. It starts by cleaning the raw
                                                  phenotypic records from outliers and typos. Then the data is analysed trial by
                                                  trial to remove the spatial noise and extract genotype BLUEs from each trial (step 1). This could be
                                                  followed by an alternative QA step to identify outliers using standardized residuals.
                                                  The genotype BLUEs from the first step (and optionally genetic markers) are used to fit a multi-environment
                                                  trial analysis to produce across-environment surrogates of genetic merit for each
                                                  trait (e.g., BLUPs, GBLUP, etc.) and stability across environments."),
                                                p("It is recommended to use the trait-BVs to produce a selection index (net merit) that
                                                  can be used in one of two alternatives; 1) select parents with high net merit for the
                                                  crossing block, 2) simulate and select crosses  with the highest net merit using the
                                                  optimal cross selection (OCS) procedure based on contribution theory.")
                            )
        ),
        class = "tab-content"
      )
    )



  )
}

#' sectionInfoGEApp Server Functions
#'
#' @noRd
mod_sectionInfoGEApp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_sectionInfoGEApp_ui("sectionInfoGEApp_1")

## To be copied in the server
# mod_sectionInfoGEApp_server("sectionInfoGEApp_1")

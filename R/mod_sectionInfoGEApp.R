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
                                                  combining ability (GCA), total genetic value(GV)] in oder to apply
                                                  artifical selection using such estimates and increase the allele frequencies of genes affecting the
                                                  expression of the traits of interest in a particular target
                                                  population of environments (TPE)."),
                                                img(src = " www/infoge.png", height = 350, width = 600), # add an image
                                                p("The genetic evaluation approach used in bioflow is the so-called 'two-stage'. After cleaning the raw
                                                  phenotypic records from outliers and typos the data is analysed environment by
                                                  environment to remove the spatial noise and extract genotype BLUEs and standard errors (stage 1). This could be
                                                  followed by an alternative QA step to identify outliers using standardized residuals.
                                                  The genotype BLUEs from the first stage (and optionally genetic markers, pedigree information, and environmental data)
                                                  are used to fit a multi-environment analysis to produce across-environment surrogates of genetic merit for each
                                                  trait (e.g., BLUPs, GBLUP, etc.) and stability surrogates across environments."),
                                                p("It is recommended to use the trait-BVs to produce a selection index (net merit) that
                                                  can be used in one of two alternatives; 1) select parents with high net merit for the next
                                                  crossing block, or 2) simulate and select predicted crosses  with the highest net merit using the
                                                  optimal cross selection (OCS) procedure based on contribution theory."),
                                                p(strong("Additional notes:")),
                                                p("For a trait where you only have single replicate data (no replication within environment per genotype) and want to
                                                use pedigree or markers to separate the error from the genetic signal,
                                                you need to perform the single trial analysis first to move your single-replicate records per environment to the second stage
                                                where you can use your pedigree or marker information. Make sure you accept environments with H2=0 coming from the first stage
                                                since unreplicated environments or trial will be assumed to have H2=0.
                                                  "),
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

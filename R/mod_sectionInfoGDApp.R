#' sectionInfoGDApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sectionInfoGDApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    tags$div(
      fluidRow(
        shinydashboard::box(width = 12, status = "success", collapsible = FALSE,
                            tags$body(
                              p()
                            ),
                            shinydashboard::box(status="success", width = 12,
                                                title = "What is gene discovery?", solidHeader = TRUE,
                                                p("Gene discovery is the process of using different sources of information (e.g., phenotypes, genotypes, etc.) coupled
                                                with statistical methodologies to identify causal genes behind the trait of interest. Historically this process has been
                                                  called gene mapping and comprises multiple methodologies. Classical methodologies developed last century such
                                                  as single-marker regression, interval mapping, composite interval mapping have show effective in mapping genes
                                                  in biparental populations."),
                                                img(src = " www/gwas.png", height = 300, width = 700), # add an image
                                                p("More recent techniques in gene mapping include the so-called genome wide association studies (GWAS) that can identify genes in
                                                  more structured populations such as diversity panels and designed populations (e.g., MAGIC) by proper control of
                                                  structure using relarionship matrices and principal components methodologies. Latest methodologies bring the
                                                  benefits of classical composite interval mapping into the GWAS framework. Bioflow currently offers the classical Q+K model
                                                  popularized by Kang et al. (2008, 2010). In addition to quantitative genetics methodologies, many bioinformatic-based methodologies
                                                  have been developed to compare genomes across different species and do gene annotation. These second type of methodologies
                                                  are not within the scope of Bioflow."),
                                                p(strong("References:")),
                                                p("Kang et al. 2008. Efficient control of population structure in model organism association mapping. Genetics 178:1709-1723."),
                                                p("Kang et al. 2010. Variance component model to account for sample structure in genome-wide association studies. Nat. Genet. 42:348-354."),
                                                p("Image taken from https://en.wikipedia.org/wiki/Genome-wide_association_study"),
                            )
        ),
        class = "tab-content"
      )
    )

  )
}

#' sectionInfoGDApp Server Functions
#'
#' @noRd
mod_sectionInfoGDApp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_sectionInfoGDApp_ui("sectionInfoGDApp_1")

## To be copied in the server
# mod_sectionInfoGDApp_server("sectionInfoGDApp_1")

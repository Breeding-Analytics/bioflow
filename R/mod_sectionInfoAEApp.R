#' sectionInfoAEApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sectionInfoAEApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    tags$div(
      fluidRow(
        shinydashboard::box(width = 12, status = "success", collapsible = FALSE,
                            tags$body(
                              p()
                            ),
                            shinydashboard::box(status="success", width = 12,
                                                title = "What is agronomic evaluation?", solidHeader = TRUE,
                                                p("Agricultural research requires to develop not only the germplasm with the highest genetic merit but also
                                                  the proper agronomic management to maximize the performance of varieties on farmers' fields. The purpose of this
                                                  section is to provide analytics to inspect the differences among agronomic treatments (e.g., fertilizer doses,
                                                  health-management treatments, etc.). It is common in agronomic assays to have multi-way treatments evaluated. We
                                                  have enabled comparisons for up to 3-way comparisons for the most popular designs."),
                                                img(src = " www/ageval.jpeg", height = 450, width = 600), # add an image
                                                p("The agronomic evaluation approach used in bioflow assumes that data is balanced in nature and follows
                                                  conventional experimental designs that allow the use of classical analysis of variance (ANOVA), treatment
                                                  comparison through contrasts and multiple-comparison methodologies."),
                            )
        ),
        class = "tab-content"
      )
    )

  )
}

#' sectionInfoAEApp Server Functions
#'
#' @noRd
mod_sectionInfoAEApp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_sectionInfoAEApp_ui("sectionInfoAEApp_1")

## To be copied in the server
# mod_sectionInfoAEApp_server("sectionInfoAEApp_1")

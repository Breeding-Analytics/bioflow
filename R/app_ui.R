#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  div(
    class = "navbar1",
    shinyWidgets::useShinydashboard(),

    navbarPage(

      id="tabso",

      golem_add_external_resources(),

      title=div(tags$b("MyTitle"), style = "color:#FFFFFF"),

      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css")),

      tabPanel("Data Input ", mod_getData_ui("getData_1")),
      # tabPanel("Data Input ", mod_input_file_sta_ui("input_file_sta_1")),

      navbarMenu("Genetic Evaluation",
                 tabPanel("QA/QC (raw data)", mod_qaRawApp_ui("qaRawApp_1") ),
                 tabPanel("Single-Trial Analysis",  mod_staApp_ui("staApp_1")),
                 tabPanel("QA/QC (model-based)",  ) ,
                 tabPanel("Multi-Trial Analysis",  ), # biplot is part of the report in MET
                 tabPanel("Selection Indices",  ) ,
                 tabPanel("Optimal Cross Selection",  )
      ),

      navbarMenu("QTL Mapping",
                 tabPanel("QA/QC",  ),
                 tabPanel("GWAS",  )
      ),

      navbarMenu("Population Structure",
                 tabPanel("Markers (QA/QC)", ),
                 tabPanel("Markers (Clustering)", ),
                 tabPanel("Phenotype (QA/QC)", ),
                 tabPanel("Phenotype (Clustering)", ),
                 tabPanel("Relationship Matrix",  )

      ),

      tabPanel("About", "Bla Bla Bla")

    )

  )

}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "frame"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

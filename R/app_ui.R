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
                 tabPanel("1. QA/QC (raw-data)", mod_qaRawApp_ui("qaRawApp_1") ),
                 tabPanel("2. Single-Trial Analysis",  mod_staApp_ui("staApp_1")),
                 tabPanel("3. QA/QC (model-based)", mod_qaStaApp_ui("qaStaApp_1") ) ,
                 tabPanel("* Marker QA/QC (optional)",  ),
                 tabPanel("4. Multi-Trial Analysis", mod_mtaApp_ui("mtaApp_1") ), # biplot is part of the report in MET
                 tabPanel("5. Selection Indices", mod_indexDesireApp_ui("indexDesireApp_1") ) ,
                 tabPanel("6. Optimal Cross Selection", mod_ocsApp_ui("ocsApp_1") ),
                 tabPanel("7.1. Realized Genetic Gain", ), # user needs to do up to a multi-year genetic evaluation to provide the MET as input
                 tabPanel("7.2. Predicted Genetic Gain", )# user needs to perform a multi-year genetic evaluation to provide the MET as input
      ),

      navbarMenu("QTL Mapping",
                 tabPanel("QA/QC (raw data)", ),
                 tabPanel("Single-Trial Analysis",  ),
                 tabPanel("QA/QC (model-based)",  ) ,
                 tabPanel("Marker QA/QC (optional)",  ),
                 tabPanel("GWAS",  ) # user needs to make all the process up to Marker QA/QC
      ),

      # navbarMenu("Marker Structure ",
      #            tabPanel("Marker QA/QC",  ),
      #            tabPanel("Relationship Matrix (optional)",  ),
      #            tabPanel("Clustering", ) # using PCA, dendograms, etc. and as input either the markers or the relationship matrix
      # ),
      #
      # navbarMenu("Trait Structure",
      #            tabPanel("Phenotype (QA/QC)", ),
      #            tabPanel("Phenotype (Clustering)", ) # using PCA, dendograms, etc. and as input either the phenotypes or MET results
      #
      # ),



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

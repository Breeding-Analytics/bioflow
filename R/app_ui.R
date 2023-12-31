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

      title=div(tags$b(""), style = "color:#FFFFFF"),

      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css")),
      # color the input and output tabs
      tags$style(".fa-arrow-right-to-bracket {color:#FFA500}"),
      tags$style(".input-p {color: #FFA500;}"),
      tags$style(".fa-arrow-right-from-bracket {color:#3CB371}"),
      tags$style(".output-p {color: #3CB371;}"),
      tags$style(".fa-book {color:#6495AD}"),
      tags$style(".info-p {color: #6495AD;}"),

      tabPanel("Home", mod_homeApp_ui("homeApp_1"), icon = icon("house")  ),

      tabPanel("Data Retrieval ", mod_getData_ui("getData_1"), icon = icon("upload") ),

      navbarMenu("Genetic Evaluation", icon = icon("lightbulb"),
                 tabPanel("1. Raw-Phenotypes QA/QC", mod_qaRawApp_ui("qaRawApp_1"), icon = icon("soap") ),
                 tabPanel("2. Single-Trial Analysis",  mod_staApp_ui("staApp_1"), icon = icon("seedling")),
                 tabPanel("**** Model-Based QA/QC", mod_qaStaApp_ui("qaStaApp_1"), icon = icon("chart-bar") ) ,
                 tabPanel("**** Markers QA/QC", mod_qaGenoApp_ui("qaGenoApp_1"), icon = icon("dna")  ),
                 tabPanel("3. Multi-Trial Analysis", mod_mtaApp_ui("mtaApp_1"), icon = icon("circle-nodes") ), # biplot is part of the report in MET
                 tabPanel("4. Selection Indices", mod_indexDesireApp_ui("indexDesireApp_1"), icon = icon("sort") ) ,
                 tabPanel("5. Optimal Cross Selection", mod_ocsApp_ui("ocsApp_1"), icon = icon("shuffle") ),
                 tabPanel("6.1. Realized Genetic Gain", mod_rggApp_ui("rggApp_1"), icon = icon("chart-line") ), # user needs to do up to a multi-year genetic evaluation to provide the MET as input
                 tabPanel("6.2. Predicted Genetic Gain", mod_pggApp_ui("pggApp_1"), icon = icon("barcode"))# user needs to perform a multi-year genetic evaluation to provide the MET as input
      ),

      navbarMenu("QTL Mapping", icon = icon("dna"),
                 tabPanel("P3D-GWAS",  icon = icon("chart-line")) # user needs to make all the process up to Marker QA/QC
      ),

      navbarMenu("About", icon = icon("question"),
               tabPanel("Technology", mod_aboutApp_ui("aboutApp_1"), icon = icon("puzzle-piece") ),
               tabPanel("Contact us", icon = icon("envelope") ),
               tabPanel("Meet the team", icon = icon("yin-yang") ) ,
               )

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

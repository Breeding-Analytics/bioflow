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

      title=div(img(src="www/cgiarmini.png"), "bioflow",style = "color:#FFFFFF"),
      # title=div(tags$b(""), style = "color:#FFFFFF"),

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

      navbarMenu("Selection", icon = icon("lightbulb"),
                 tabPanel(strong("GENETIC EVALUATION"), mod_sectionInfoGEApp_ui("sectionInfoGEApp_1"), icon = icon("filter")),
                 tabPanel("GE1. Raw-Phenotypes QA/QC", mod_qaRawApp_ui("qaRawApp_1"), icon = icon("soap") ),
                 tabPanel("GE2. Single-Trial Analysis",  mod_staApp_ui("staApp_1"), icon = icon("seedling")),
                 tabPanel("**** Model-Based QA/QC", mod_qaStaApp_ui("qaStaApp_1"), icon = icon("chart-bar") ) ,
                 tabPanel("**** Markers QA/QC", mod_qaGenoApp_ui("qaGenoApp_1"), icon = icon("dna")  ),
                 tabPanel("GE3. Multi-Trial Analysis", mod_mtaApp_ui("mtaApp_1"), icon = icon("circle-nodes") ), # biplot is part of the report in MET
                 tabPanel("GE4. Selection Indices", mod_indexDesireApp_ui("indexDesireApp_1"), icon = icon("sort") ) ,
                 tabPanel("GE5. Optimal Cross Selection", mod_ocsApp_ui("ocsApp_1"), icon = icon("shuffle") ),
                 tabPanel(strong("SELECTION HISTORY"),  icon = icon("timeline")),
                 tabPanel("SH1a. Realized Genetic Gain", mod_rggApp_ui("rggApp_1"), icon = icon("chart-line") ), # user needs to do up to a multi-year genetic evaluation to provide the MET as input
                 tabPanel("SH1b. Predicted Genetic Gain", mod_pggApp_ui("pggApp_1"), icon = icon("barcode")),# user needs to perform a multi-year genetic evaluation to provide the MET as input
                 tabPanel("SH2. Selection signatures",  icon = icon("filter")) # may include P3D, traditional single linear regression, Eigen, etc.
      ),

      navbarMenu("Mutation", icon = icon("dna"),
                 tabPanel(strong("DISCOVERY"),  icon = icon("timeline")),
                 tabPanel("D1. Genome wide association",  icon = icon("chart-simple")), # may include P3D, traditional single linear regression, Eigen, etc.
                 tabPanel("D2. Variant calling",  icon = icon("chart-simple")), #
                 tabPanel(strong("MUTATION HISTORY"),  icon = icon("timeline")),
                 tabPanel("MH1. Mutation rate",  icon = icon("disease")) # may include
      ),

      navbarMenu("Gene flow and Drift", icon = icon("wind"),
                 tabPanel(strong("CLUSTERS"),  icon = icon("puzzle-piece")),
                 tabPanel("S1. Population structure",  icon = icon("diagram-project")), # may include PCA based, structure based, clustering
                 tabPanel("S2. Pool formation",  icon = icon("circle-nodes")), # may include k-means, simulated annealing
                 tabPanel("S3. Pop-subset selection",  icon = icon("puzzle-piece")), # may include STPGA
                 tabPanel(strong("GENE FREQUENCIES"),  icon = icon("filter")),
                 tabPanel("GF1. MAS", icon = icon("barcode")),# user needs to perform a multi-year genetic evaluation to provide the MET as input
                 tabPanel("GF2. MABC", icon = icon("shuffle")),# user needs to perform a multi-year genetic evaluation to provide the MET as input
                 tabPanel("GF3. Effective-pop-size",  icon = icon("filter"))

      ),

      navbarMenu("About", icon = icon("question"),
                 tabPanel("Technology", mod_aboutApp_ui("aboutApp_1"), icon = icon("puzzle-piece") ),
                 tabPanel("Meet the team", mod_meetTheTeamApp_ui("meetTheTeamApp_1"), icon = icon("yin-yang") ),
                 tabPanel("Contact us", mod_contactUsApp_ui("contactUsApp_1"), icon = icon("envelope") )
      )



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

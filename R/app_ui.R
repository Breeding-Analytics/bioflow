#' Warning: useShinydashboard has been deprecated and will be removed in a future
#' release of shinyWidgets. If you want to create value box in shiny, see package
#' bslib. If you absolutely need to use this function, copy the source code into
#' your project https://github.com/dreamRs/shinyWidgets/blob/26838f9e9ccdc90a47178b45318d110f5812d6e1/R/useShinydashboard.R
useShinydashboard <- function() {
  if (!requireNamespace(package = "shinydashboard"))
    message("Package 'shinydashboard' is required to run this function")
  deps <- htmltools::findDependencies(shinydashboard::dashboardPage(
    header = shinydashboard::dashboardHeader(),
    sidebar = shinydashboard::dashboardSidebar(),
    body = shinydashboard::dashboardBody()
  ))
  htmltools::attachDependencies(tags$div(class = "main-sidebar", style = "display: none;"), value = deps)
}

#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  div(
    class = "navbar1",
    useShinydashboard(),

    navbarPage(

      id="tabso",

      header=golem_add_external_resources(),

      title=div(img(src="www/cgiarmini.png"), "",style = "color:#FFFFFF"),
      # title=div(tags$b(""), style = "color:#FFFFFF"),

      tabPanel("Home", mod_homeApp_ui("homeApp_1"), icon = icon("house")  ),

      tabPanel("Data Retrieval ", mod_getData_ui("getData_1"), icon = icon("upload") ),

      navbarMenu("QA-QC", icon = icon("medal"),
                 tabPanel(strong("QUALITY CONTROL"),  mod_sectionInfoQAApp_ui("sectionInfoQAApp_1"), icon = icon("medal")),
                 tabPanel("QC1. Outlier Tagging", mod_qaRawApp_ui("qaRawApp_1"), icon = icon("soap") ),
                 tabPanel("**** Trial Filtering",mod_filterPhenoApp_ui("filterPhenoApp_1"), icon = icon("filter") ),
                 tabPanel("QC2. Marker Filtering", mod_qaGenoApp_ui("qaGenoApp_1"), icon = icon("dna")  ),
      ),

      navbarMenu("Selection", icon = icon("lightbulb"),
                 tabPanel(strong("GENETIC EVALUATION"), mod_sectionInfoGEApp_ui("sectionInfoGEApp_1"), icon = icon("filter")),
                 tabPanel("GE1. Single-Trial Analysis",  mod_staApp_ui("staApp_1"), icon = icon("seedling")),
                 tabPanel("**** Model-Based QA/QC", mod_qaStaApp_ui("qaStaApp_1"), icon = icon("chart-bar") ) ,
                 tabPanel("GE2. Multi-Trial Analysis", mod_mtaApp_ui("mtaApp_1"), icon = icon("circle-nodes") ), # biplot is part of the report in MET
                 tabPanel("GE3. Selection Indices", mod_indexDesireApp_ui("indexDesireApp_1"), icon = icon("sort") ) ,
                 tabPanel("GE4. Optimal Cross Selection", mod_ocsApp_ui("ocsApp_1"), icon = icon("shuffle") ),
                 tabPanel(strong("SELECTION HISTORY"), mod_sectionInfoSHApp_ui("sectionInfoSHApp_1"),  icon = icon("timeline")),
                 tabPanel("SH1a. Realized Genetic Gain", mod_rggApp_ui("rggApp_1"), icon = icon("chart-line") ), # user needs to do up to a multi-year genetic evaluation to provide the MET as input
                 tabPanel("SH1b. Predicted Genetic Gain", mod_pggApp_ui("pggApp_1"), icon = icon("barcode")),# user needs to perform a multi-year genetic evaluation to provide the MET as input
                 tabPanel(div("SH2. Selection signatures",style = "color:red"))#  , icon = icon("filter")) # may include P3D, traditional single linear regression, Eigen, etc.
      ),

      navbarMenu("Mutation", icon = icon("dna"),
                 tabPanel(strong("DISCOVERY"),  icon = icon("timeline")),
                 tabPanel(div("D1. Genome wide association",style = "color:red")), #  icon = icon("chart-simple")), # may include P3D, traditional single linear regression, Eigen, etc.
                 tabPanel(div("D2. Variant calling",style = "color:red")), #  icon = icon("chart-simple")), #
                 tabPanel(strong("MUTATION HISTORY"),  icon = icon("timeline")),
                 tabPanel(div("MH1. Mutation rate",style = "color:red")) #  icon = icon("disease")) # may include
      ),

      navbarMenu("Gene flow and Drift", icon = icon("wind"),
                 tabPanel(strong("GENE FREQUENCIES"),  icon = icon("filter")),
                 tabPanel(div("GF1. Marker-assisted selection",style = "color:red")), # icon = icon("barcode")),# user needs to perform a multi-year genetic evaluation to provide the MET as input
                 tabPanel(div("GF2. Marker-assisted backcross",style = "color:red")), # icon = icon("shuffle")),# user needs to perform a multi-year genetic evaluation to provide the MET as input
                 tabPanel(div("GF3. Hybridity test",style = "color:red")),
                 tabPanel(div("GF4. Number of founders",style = "color:red")), #  icon = icon("filter")),
                 tabPanel(strong("DRIFT & FLOW HISTORY"),  icon = icon("puzzle-piece")),
                 tabPanel(div("S1. Population structure",style = "color:red")), #  icon = icon("diagram-project")), # may include PCA based, structure based, clustering
                 tabPanel(div("S2. Pool formation",style = "color:red")), #  icon = icon("circle-nodes")), # may include k-means, simulated annealing
                 tabPanel(div("S3. Pop-subset selection",style = "color:red")) # stpga
      ),

      tabPanel("Save",  mod_saveData_ui("saveData_1"), icon = icon("floppy-disk") ),

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
      app_title = "bioflow"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    # color the input and output tabs

    tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css"),

    tags$style(".fa-arrow-right-to-bracket {color:#FFA500}"),
    tags$style(".input-p {color: #FFA500}"),
    tags$style(".fa-arrow-right-from-bracket {color:#3CB371}"),
    tags$style(".output-p {color: #3CB371;}"),
    tags$style(".fa-book {color:#6495AD}"),
    tags$style(".info-p {color: #6495AD;}"),
  )
}

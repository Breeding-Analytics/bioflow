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

      # navbarMenu("Selection", icon = icon("bullseye"),
      #            tabPanel(div(icon("filter-circle-xmark"), "Pheno-Outlier QA/QC (", icon("seedling"),")" ), mod_qaRawApp_ui("qaRawApp_1") ),
      #            tabPanel(strong("GENETIC EVALUATION"), mod_sectionInfoGEApp_ui("sectionInfoGEApp_1") ),
      #            tabPanel(div(icon("calculator"), icon("dice-one"), "Single-Trial Analysis (", icon("seedling"), ")"),  mod_staApp_ui("staApp_1") ),
      #            tabPanel(div(icon("filter-circle-xmark"), "(optional) Model-Based QA/QC (", icon("seedling"), ")" ), mod_qaStaApp_ui("qaStaApp_1") ) ,
      #            tabPanel(div(icon("filter-circle-xmark"), "(optional) Genetic-Marker QA/QC (", icon("dna"), ")" ), mod_qaGenoApp_ui("qaGenoApp_1")  ),
      #            tabPanel(div(icon("calculator"), icon("dice-two"), "Multi-Trial Analysis (", icon("seedling"), icon("dna"), icon("network-wired"), icon("cloud-bolt") ,")"), mod_mtaApp_ui("mtaApp_1") ), # biplot is part of the report in MET
      #            tabPanel(div(icon("calculator"), icon("dice-three"), "Selection Indices (",icon("seedling"),")" ), mod_indexDesireApp_ui("indexDesireApp_1") ) ,
      #            tabPanel(div(icon("calculator"), icon("dice-four"), "Optimal Cross Selection (", icon("seedling"), ")" ), mod_ocsApp_ui("ocsApp_1") ),
      #            tabPanel(strong("SELECTION HISTORY"), mod_sectionInfoSHApp_ui("sectionInfoSHApp_1") ),  # chart-line , barcode
      #            tabPanel(div(icon("chart-line"), "Realized Genetic Gain (", icon("seedling"), icon("network-wired"),")"), mod_rggApp_ui("rggApp_1") ), # user needs to do up to a multi-year genetic evaluation to provide the MET as input
      #            tabPanel(div(icon("chart-line"), "Predicted Genetic Gain (", icon("seedling"),  icon("network-wired"),")"), mod_pggApp_ui("pggApp_1")),# user needs to perfor m a multi-year genetic evaluation to provide the MET as input
      #            tabPanel(div(icon("chart-line"), "Selection signatures (", icon("dna"),")", style = "color:red") )#  , icon = icon("filter")) # may include P3D, traditional single linear regression, Eigen, etc.
      # ),

      navbarMenu("Selection", icon = icon("bullseye"),
                 tabPanel(strong("GENETIC EVALUATION"), mod_sectionInfoGEApp_ui("sectionInfoGEApp_1") ),
                 tabPanel(div(icon("filter-circle-xmark"), "QA/QC"),
                          tabsetPanel(
                            tabPanel(div("Pheno-Outlier QA/QC (", icon("seedling"),")" ), mod_qaRawApp_ui("qaRawApp_1") ),
                            tabPanel(div("(optional) Genetic-Marker QA/QC (", icon("dna"), ")" ), mod_qaGenoApp_ui("qaGenoApp_1") )
                          )
                 ),

                 tabPanel(div(icon("calculator"), icon("dice-one"), "Single-Trial Analysis (", icon("seedling"), ")"),
                          tabsetPanel(
                            tabPanel(div("Single-Trial Analysis (", icon("seedling"), ")"),  mod_staApp_ui("staApp_1") ),
                            tabPanel(div("(optional) Model-Based QA/QC (", icon("seedling"), ")" ), mod_qaStaApp_ui("qaStaApp_1") )
                          )
                 ),
                 tabPanel(div(icon("calculator"), icon("dice-two"), "Multi-Trial Analysis (", icon("seedling"), icon("dna"), icon("network-wired"), icon("cloud-bolt") ,")"), mod_mtaApp_ui("mtaApp_1") ), # biplot is part of the report in MET
                 tabPanel(div(icon("calculator"), icon("dice-three"), "Selection Indices (",icon("seedling"),")" ), mod_indexDesireApp_ui("indexDesireApp_1") ) ,
                 tabPanel(div(icon("calculator"), icon("dice-four"), "Optimal Cross Selection (", icon("seedling"), ")" ), mod_ocsApp_ui("ocsApp_1") ),
                 tabPanel(strong("SELECTION HISTORY"), mod_sectionInfoSHApp_ui("sectionInfoSHApp_1") ),  # chart-line , barcode
                 tabPanel(div(icon("chart-line"), "Realized Genetic Gain (", icon("seedling"), icon("network-wired"),")"), mod_rggApp_ui("rggApp_1") ), # user needs to do up to a multi-year genetic evaluation to provide the MET as input
                 tabPanel(div(icon("chart-line"), "Predicted Genetic Gain (", icon("seedling"),  icon("network-wired"),")"), mod_pggApp_ui("pggApp_1")),# user needs to perfor m a multi-year genetic evaluation to provide the MET as input
                 tabPanel(div(icon("chart-line"), "Selection signatures (", icon("dna"),")", style = "color:red") )#  , icon = icon("filter")) # may include P3D, traditional single linear regression, Eigen, etc.
      ),

      navbarMenu("Mutation", icon = icon("disease"),
                 tabPanel(strong("DISCOVERY")),
                 tabPanel(div(icon("fire"), "Genome wide association (", icon("dna"),")", style = "color:red")), #  icon = icon("chart-simple")), # may include P3D, traditional single linear regression, Eigen, etc.
                 tabPanel(div(icon("fire"), "Variant calling (", icon("dna"),")",style = "color:red")), #  icon = icon("chart-simple")), #
                 tabPanel(strong("MUTATION HISTORY") ),
                 tabPanel(div(icon("chart-line"), "Mutation rate (", icon("dna"),")", style = "color:red")) #  icon = icon("disease")) # may include
      ),

      navbarMenu("Gene flow and Drift", icon = icon("wind"),
                 tabPanel(strong("GENE FREQUENCIES")), # plus, shuffle, barcode, people-group
                 tabPanel(div(icon("barcode"), "Marker-assisted selection (", icon("plus-minus"),")", style = "color:red")), # icon = icon("barcode")),# user needs to perform a multi-year genetic evaluation to provide the MET as input
                 tabPanel(div(icon("barcode"), "Marker-assisted backcross (", icon("plus-minus"), ")", style = "color:red")), # icon = icon("shuffle")),# user needs to perform a multi-year genetic evaluation to provide the MET as input
                 tabPanel(div(icon("barcode"), "Hybridity test (", icon("dna"), ")", style = "color:red")),
                 tabPanel(div(icon("chart-line"),  "Number of founders (", icon("dna"), ")", style = "color:red")), #  icon = icon("filter")),
                 tabPanel(strong("DRIFT & FLOW HISTORY") ),
                 tabPanel(div(icon("circle-nodes"), "Population structure (", icon("dna"), icon("seedling"), ")", style = "color:red")), #  icon = icon("diagram-project")), # may include PCA based, structure based, clustering
                 tabPanel(div(icon("circle-nodes"), "Pool formation (",  icon("dna"), icon("seedling"), ")", style = "color:red")), #  icon = icon("circle-nodes")), # may include k-means, simulated annealing
                 tabPanel(div(icon("circle-nodes"), "Pop-subset formation (", icon("dna"), ")", style = "color:red")) # stpga
      ),

      navbarMenu("Other QC & Transform", icon = icon("medal"),
                 tabPanel(strong("QUALITY CONTROL"),  mod_sectionInfoQAApp_ui("sectionInfoQAApp_1")),
                 tabPanel(div(icon("filter-circle-xmark"), "(optional) Trial Filtering (", icon("seedling"), ")" ),mod_filterPhenoApp_ui("filterPhenoApp_1") ),
                 tabPanel(div(icon("filter-circle-xmark"), "(optional) Design Filering (", icon("seedling"), ")" ), mod_expDesignEditApp_ui("expDesignEditApp_1")  ),
                 tabPanel(div(icon("filter-circle-xmark"), "(optional) Pedigree Filtering (", icon("network-wired"), ")" ), mod_qaPedApp_ui("qaPedApp_1") ),
                 tabPanel(strong("TRANSFORMATIONS"),  mod_sectionInfoTransformApp_ui("sectionInfoTransformApp_1") ),
                 tabPanel(div(icon("arrows-split-up-and-left"), "(optional) Data Binding (", icon("database"), ")"), mod_bindObjectApp_ui("bindObjectApp_1") ),
                 tabPanel(div(icon("arrows-split-up-and-left"), "(optional) Trait Transformations (", icon("seedling"), ")" ),  mod_traitTransformApp_ui("traitTransformApp_1") ),
                 tabPanel(div(icon("arrows-split-up-and-left"), "(optional) Single-Cross Markers (", icon("dna"), ")" ), mod_singleCrossGenoApp_ui("singleCrossGenoApp_1")  ),
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

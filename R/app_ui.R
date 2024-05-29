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

      navbarMenu("Home", icon = icon("house"),
                 tabPanel(div( "Bioflow", icon("battery") ), mod_homeApp_ui("homeApp_1")  ),
                 tabPanel( tags$a(href="https://cgiar-service-portal-prd.azurewebsites.net/register", "Service Portal (BRI)", target="_blank")  ),
                 tabPanel(div( "About us", icon("question") ),
                          tabsetPanel( #widths = c(1, 11),
                            tabPanel("Technology", mod_aboutApp_ui("aboutApp_1"), icon = icon("puzzle-piece") ),
                            tabPanel("Meet the team", mod_meetTheTeamApp_ui("meetTheTeamApp_1"), icon = icon("yin-yang") ),
                            tabPanel("Contact & Development", mod_contactUsApp_ui("contactUsApp_1"), icon = icon("envelope") ),
                          )
                 ),
      ),

      navbarMenu("Data Management ", icon = icon("upload"),
                 tabPanel(div(icon("folder"), "Retrieve New Data"),
                          tabsetPanel( #widths = c(1, 11),
                            tabPanel(div(icon("seedling"), "Phenotypic"), mod_getDataPheno_ui("getDataPheno_1") ),
                            tabPanel(div(icon("dna"), "Genotypic"), mod_getDataGeno_ui("getDataGeno_1") ),
                            tabPanel(div(icon("code-fork"), "Pedigree"), mod_getDataPed_ui("getDataPed_1") ),
                            tabPanel(div(icon("anchor"), "QTL profile"), mod_getDataQTL_ui("getDataQTL_1") ),
                            tabPanel(div(icon("cloud-sun-rain"), "Weather"), mod_getDataWeather_ui("getDataWeather_1") ),
                          )
                 ),
                 tabPanel(div(icon("folder-open"), "Retrieve Old Analysis"),
                          mod_bindObjectApp_ui("bindObjectApp_1") ,
                 ),
                 tabPanel(div(icon("floppy-disk"), "Save Analysis"),
                          mod_saveData_ui("saveData_1"),
                 ),
      ),

      navbarMenu("Selection", icon = icon("bullseye"),
                 tabPanel(strong("GENETIC EVALUATION"), mod_sectionInfoGEApp_ui("sectionInfoGEApp_1") ),
                 tabPanel(div(icon("filter-circle-xmark"), "Quality Assurance (", icon("seedling"), icon("dna"), icon("network-wired") ,")" ),
                          navlistPanel( widths = c(1, 11),
                                        tabPanel(div("Pheno QA/QC (", icon("seedling"),")" ), mod_qaRawApp_ui("qaRawApp_1") ),
                                        tabPanel(div("(optional) Genetic QA/QC (", icon("dna"), ")" ), mod_qaGenoApp_ui("qaGenoApp_1") ),
                                        #tabPanel(div("(optional) Pedigree QA/QC (", icon("network-wired"), ")" ), mod_qaPedApp_ui("qaPedApp_1") ),
                          )
                 ),

                 tabPanel(div(icon("calculator"), icon("dice-one"), "Single-Trial Analysis (", icon("seedling"), ")"),
                          navlistPanel( widths = c(1, 11),
                                        tabPanel(div("Single-Trial Analysis (", icon("seedling"), ")"),  mod_staApp_ui("staApp_1") ),
                                        tabPanel(div("(optional) Model-Based QA/QC (", icon("seedling"), ")" ), mod_qaStaApp_ui("qaStaApp_1") ),
                                        tabPanel(div("(optional) On Farm Trial Decision (", icon("seedling"), ")" ), mod_oftStaApp_ui("oftStaApp_1") ),
                          )
                 ),
                 tabPanel(div(icon("calculator"), icon("dice-two"), "Multi-Trial Analysis (", icon("seedling"), icon("dna"), icon("network-wired"), icon("cloud-sun-rain") ,")" ), mod_mtaApp_ui("mtaApp_1") ), # biplot is part of the report in MET
                 tabPanel(div(icon("calculator"), icon("dice-three"), "Selection Indices (", icon("seedling"), ")"),
                          navlistPanel( widths = c(1, 11),
                                        tabPanel(div("Desire Index (", icon("seedling"), ")"),  mod_indexDesireApp_ui("indexDesireApp_1") ),
                                        tabPanel(div("Base Index (", icon("seedling"), ")" ), mod_indexBaseApp_ui("indexBaseApp_1") )
                          )
                 ),
                 tabPanel(div(icon("calculator"), icon("dice-four"), "Optimal Cross Selection (", icon("seedling"), icon("dna"), icon("network-wired"), ")" ), mod_ocsApp_ui("ocsApp_1") ),
                 tabPanel(strong("SELECTION HISTORY"), mod_sectionInfoSHApp_ui("sectionInfoSHApp_1") ),  # chart-line , barcode
                 tabPanel(div(icon("chart-line"), "Realized Genetic Gain (", icon("seedling"), icon("network-wired"),")"), mod_rggApp_ui("rggApp_1") ), # user needs to do up to a multi-year genetic evaluation to provide the MET as input
                 tabPanel(div(icon("chart-line"), "Predicted Genetic Gain (", icon("seedling"),  icon("network-wired"),")"), mod_pggApp_ui("pggApp_1")),# user needs to perfor m a multi-year genetic evaluation to provide the MET as input
                 tabPanel(div(icon("chart-line"), "Selection signatures (", icon("dna"),")", style = "color:red") )#  , icon = icon("filter")) # may include P3D, traditional single linear regression, Eigen, etc.
      ),

      navbarMenu("Mutation", icon = icon("disease"),
                 tabPanel(strong("DISCOVERY")),
                 tabPanel(div(icon("fire"), "Genome wide association (", icon("dna"),")", style = "color:red")), #  icon = icon("chart-simple")), # may include P3D, traditional single linear regression, Eigen, etc.
                 tabPanel(strong("MUTATION HISTORY") ),
                 tabPanel(div(icon("chart-line"), "Mutation rate (", icon("dna"),")", style = "color:red")) #  icon = icon("disease")) # may include
      ),

      navbarMenu("Gene flow and Drift", icon = icon("wind"),
                 tabPanel(strong("GENE FREQUENCIES")), # plus, shuffle, barcode, people-group
                 tabPanel(div(icon("barcode"), "Marker-assisted selection (", icon("anchor"),")", style = "color:red")), # icon = icon("barcode")),# user needs to perform a multi-year genetic evaluation to provide the MET as input
                 tabPanel(div(icon("barcode"), "Marker-assisted backcross (", icon("anchor"), ")", style = "color:red")), # icon = icon("shuffle")),# user needs to perform a multi-year genetic evaluation to provide the MET as input
                 tabPanel(div(icon("barcode"), "Hybridity test (", icon("dna"), ")", style = "color:red")),
                 tabPanel(div(icon("chart-line"),  "Number of founders (", icon("dna"), ")", style = "color:red")), #  icon = icon("filter")),
                 tabPanel(strong("DRIFT & FLOW HISTORY") ),
                 tabPanel(div(icon("circle-nodes"), "Population structure (", icon("dna"), icon("seedling"), ")"), mod_PopStrApp_ui("PopStrApp_1") ), #  icon = icon("diagram-project")), # may include PCA based, structure based, clustering
                 tabPanel(div(icon("circle-nodes"), "Pool formation (",  icon("dna"), icon("seedling"), ")", style = "color:red")), #  icon = icon("circle-nodes")), # may include k-means, simulated annealing
                 tabPanel(div(icon("circle-nodes"), "Pop-subset formation (", icon("dna"), ")", style = "color:red")) # stpga
      ),

      navbarMenu("Other functions", icon = icon("medal"),
                 tabPanel(strong("DATA CONSISTENCY")  ),
                 tabPanel(div(icon("circle-check"), "(optional) Consistency (", icon("seedling"),")" ),
                          navlistPanel( widths = c(1, 11),
                                        tabPanel(div("Potato (", icon("seedling"),")" ), mod_dataConsistPotatoApp_ui("dataConsistPotatoApp_1") ),
                                        tabPanel(div("Maize (", icon("seedling"),")" ) ),
                                        tabPanel(div("Wheat (", icon("seedling"),")" ) ),
                                        tabPanel(div("Rice (", icon("seedling"),")" ) ),
                                        tabPanel(div("Cassava (", icon("seedling"),")" ) ),
                                        tabPanel(div("Banana (", icon("seedling"),")" ) ),
                                        tabPanel(div("Beans (", icon("seedling"),")" ) ),
                          )
                 ),
                 tabPanel(strong("FILTERING"),  mod_sectionInfoQAApp_ui("sectionInfoQAApp_1")),
                 tabPanel(div(icon("filter-circle-xmark"), "(optional) Trial Filtering (", icon("seedling"), ")" ),mod_filterPhenoApp_ui("filterPhenoApp_1") ),
                 tabPanel(div(icon("filter-circle-xmark"), "(optional) Design Filtering (", icon("seedling"), ")" ), mod_expDesignEditApp_ui("expDesignEditApp_1")  ),
                 tabPanel(strong("TRANSFORMATIONS"),  mod_sectionInfoTransformApp_ui("sectionInfoTransformApp_1") ),
                 tabPanel(div(icon("arrows-split-up-and-left"), "(optional) Trait Transformations (", icon("seedling"), ")" ),  mod_traitTransformApp_ui("traitTransformApp_1") ),
                 tabPanel(div(icon("arrows-split-up-and-left"), "(optional) Single-Cross Markers (", icon("dna"), ")" ), mod_singleCrossGenoApp_ui("singleCrossGenoApp_1")  ),
                 tabPanel(strong("DASHBOARDS")  ),
                 tabPanel("Accelerate (ABI)", mod_abiDashboard_ui("abiDashboard_1"), icon = icon("puzzle-piece") ),
                 tabPanel("Analytical Modules", mod_reportBuilder_ui("reportBuilder_1") , icon = icon("file") ),
      ),

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

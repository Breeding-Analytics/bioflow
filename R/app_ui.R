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

radioTooltip <- function(id, choice, title, placement = "bottom", trigger = "hover", options = NULL){

  options = shinyBS:::buildTooltipOrPopoverOptionsList(title, placement, trigger, options)
  options = paste0("{'", paste(names(options), options, sep = "': '", collapse = "', '"), "'}")
  bsTag <- shiny::tags$script(shiny::HTML(paste0("
    $(document).ready(function() {
      setTimeout(function() {
        $('input', $('#", id, "')).each(function(){
          if(this.getAttribute('value') == '", choice, "') {
            opts = $.extend(", options, ", {html: true});
            $(this.parentElement).tooltip('destroy');
            $(this.parentElement).tooltip(opts);
          }
        })
      }, 500)
    });
  ")))
  htmltools::attachDependencies(bsTag, shinyBS:::shinyBSDep)
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
    tags$head(
      tags$style(type="text/css", "#inline label{ display: table-cell; text-align: center; vertical-align: middle; }
                #inline .form-group { display: table-row;}")
    ),

    navbarPage(

      id="tabso",

      header=golem_add_external_resources(),

      title=div(img(src="www/cgiarmini.png"), "",style = "color:#FFFFFF"),

      navbarMenu("Home", icon = icon("house"),
                 tabPanel(div( icon("battery"), "Bioflow" ), mod_homeApp_ui("homeApp_1")  ),
                 tabPanel(tags$a(href="https://cgiar-service-portal-prd.azurewebsites.net/register", div( icon("envelopes-bulk"), "Service Portal (BRI)" ), target="_blank")  ),
                 tabPanel(div(icon("question"), "About us"),
                          tabsetPanel( #widths = c(1, 11),
                            tabPanel("Technology", mod_aboutApp_ui("aboutApp_1"), icon = icon("puzzle-piece") ),
                            tabPanel("Meet the team", mod_meetTheTeamApp_ui("meetTheTeamApp_1"), icon = icon("yin-yang") ),
                            tabPanel("Contact & Development", mod_contactUsApp_ui("contactUsApp_1"), icon = icon("envelope") ),
                            tabPanel("FAQ", mod_faqUsApp_ui("faqUsApp_1"), icon = icon("question") ),
                          )
                 ),
                 tabPanel(div(icon("rectangle-list"), "Glossary" ), mod_glossary_ui("glossary_1")  ),
      ),

      navbarMenu("Data Management ", icon = icon("upload"),

                 tabPanel(strong("DATA RETRIEVAL AND SAVING"), mod_sectionInfoDRASApp_ui("sectionInfoDRASApp_1") ),
                 tabPanel(div(icon("folder"), "Retrieve New Data"),
                          tabsetPanel( #widths = c(1, 11),
                            tabPanel(div(icon("seedling"), "Phenotypic"), mod_getDataPheno_ui("getDataPheno_1")  ),
                            tabPanel(div(icon("dna"), "Genotypic"), mod_getDataGeno_ui("getDataGeno_1") ),
                            tabPanel(div(icon("code-fork"), "Pedigree"), mod_getDataPed_ui("getDataPed_1") ),
                            tabPanel(div(icon("anchor"), "QTL profile"), mod_getDataQTL_ui("getDataQTL_1") ),
                            tabPanel(div(icon("cloud-sun-rain"), "Weather"), mod_getDataWeather_ui("getDataWeather_1") ),
                          )
                 ),
                 tabPanel(div(icon("folder-open"), "Retrieve Old Analysis"),
                          mod_bindObjectApp_ui("bindObjectApp_1") ,
                 ),
                 tabPanel(div( icon("floppy-disk"), "Save Data/Results",  ), mod_saveData_ui("saveData_1")  ),

                 tabPanel(strong("DATA QUALITY CHECK"), mod_sectionInfoQAApp_ui("sectionInfoQAApp_1") ),
                 tabPanel(div(icon("soap"), "Phenotype QA/QC (", icon("seedling"),")" ), mod_qaPhenoApp_ui("qaPhenoApp_1") ),
                 tabPanel(div(icon("soap"), "Genotype QA/QC (", icon("dna"), ")" ), mod_qaGenoApp_ui("qaGenoApp_1") ),
                 #tabPanel(div("(optional) Pedigree QA/QC (", icon("network-wired"), ")" ), mod_qaPedApp_ui("qaPedApp_1") ),
                 # tabPanel(div(icon("filter-circle-xmark"), "Data Quality Assurance" ),
                 #          navlistPanel( "Options:", widths = c(1, 11),
                 #          )
                 # ),
                 tabPanel(strong("DATA TRANSFORMATIONS"),  mod_sectionInfoTransformApp_ui("sectionInfoTransformApp_1") ),
                 tabPanel(div(icon("arrows-split-up-and-left"), "Trait Transformations (", icon("seedling"), ")" ),  mod_traitTransformApp_ui("traitTransformApp_1") ),
                 tabPanel(div(icon("arrows-split-up-and-left"), "Single-Cross Markers (", icon("dna"), ")" ), mod_singleCrossGenoApp_ui("singleCrossGenoApp_1")  ),

                 tabPanel(strong("DATA FILTERING"),  mod_sectionInfoQAApp_ui("sectionInfoQAApp_1")),
                 tabPanel(div(icon("filter-circle-xmark"), "Trial Filtering (", icon("seedling"), ")" ),mod_filterPhenoApp_ui("filterPhenoApp_1") ),
                 tabPanel(div(icon("filter-circle-xmark"), "Design Filtering (", icon("seedling"), ")" ), mod_expDesignEditApp_ui("expDesignEditApp_1")  ),
                 tabPanel(div(icon("filter-circle-xmark"), "Consistency Filtering (", icon("seedling"), ")" ), mod_dataConsistApp_ui("dataConsistApp_1") ),


      ),

      navbarMenu("Selection", icon = icon("bullseye"),
                 tabPanel(strong("GENETIC EVALUATION"), mod_sectionInfoGEApp_ui("sectionInfoGEApp_1") ),

                 tabPanel(div(icon("barcode"), "Marker-assisted selection (", icon("anchor"),")"), mod_masApp_ui("masApp_1") ), # icon = icon("barcode")),# user needs to perform a multi-year genetic evaluation to provide the MET as input

                 tabPanel(div(icon("calculator"), icon("dice-one"), "Single-Trial Analysis (", icon("seedling"), ")"), value = "staApp_tab",
                          navlistPanel( "Options:", widths = c(1, 11),
                                        tabPanel(div("Single-Trial Analysis (", icon("seedling"), ")"),  mod_staApp_ui("staApp_1") ),
                                        tabPanel(div("Model-Based QA/QC (", icon("seedling"), ")" ), mod_qaStaApp_ui("qaStaApp_1") ),
                          )
                 ),

                 tabPanel(div(icon("calculator"), icon("dice-two"), "Multi-Trial Analysis (", icon("seedling"), icon("dna"), icon("network-wired"), icon("cloud-sun-rain"), ")"),
                          navlistPanel("Engines:", widths = c(1, 11),
                                       tabPanel(div("LMMsolve" ), mod_mtaLMMsolveApp_ui("mtaLMMsolveApp_1") ), # biplot is part of the report in MET
                                       tabPanel(div("lme4", style = "color:red" )) , # biplot is part of the report in MET
                                       tabPanel(div("sommer", style = "color:red" )) , # biplot is part of the report in MET
                                       tabPanel(div("asreml"), mod_mtaASREMLApp_ui("mtaASREMLApp_1")),
                          )
                 ),

                 tabPanel(div(icon("calculator"), icon("dice-three"), "Selection Indices (", icon("seedling"), ")"),
                          navlistPanel("Options:", widths = c(1, 11),
                                       tabPanel(div("Desire Index (", icon("seedling"), ")"),  mod_indexDesireApp_ui("indexDesireApp_1") ),
                                       tabPanel(div("Base Index (", icon("seedling"), ")" ), mod_indexBaseApp_ui("indexBaseApp_1") )
                          )
                 ),

                 tabPanel(div(icon("calculator"), icon("dice-four"), "Mate optimization (", icon("seedling"), icon("dna"), icon("network-wired"), ")"),
                          navlistPanel("Options:", widths = c(1, 11),
                                       tabPanel(div("OCS" ), mod_ocsApp_ui("ocsApp_1") ),
                                       tabPanel(div("GPCP", style = "color:red") ),
                          )
                 ),

                 tabPanel(strong("SELECTION HISTORY"), mod_sectionInfoSHApp_ui("sectionInfoSHApp_1") ),  # chart-line , barcode
                 tabPanel(div(icon("chart-line"), "Realized Genetic Gain (", icon("seedling"), icon("network-wired"),")"), mod_rggApp_ui("rggApp_1") ), # user needs to do up to a multi-year genetic evaluation to provide the MET as input
                 tabPanel(div(icon("chart-line"), "Predicted Genetic Gain (", icon("seedling"),  icon("network-wired"),")"), mod_pggApp_ui("pggApp_1")),# user needs to perfor m a multi-year genetic evaluation to provide the MET as input
                 tabPanel(div(icon("chart-line"), "Selection signatures (", icon("dna"),")", style = "color:red"), mod_selSignApp_ui("selSignApp_1") ), # icon = icon("filter")) # may include P3D, traditional single linear regression, Eigen, etc.
                 # tabPanel(strong("AGRONOMIC EVALUATION"), mod_sectionInfoAEApp_ui("sectionInfoAEApp_1") ),
                 # tabPanel(div(icon("chart-line"), "Analysis of Variance (", icon("seedling"),")", style = "color:red"), mod_agrAnova_ui("agrAnova_1") )
      ),

      navbarMenu("Mutation", icon = icon("disease"),
                 tabPanel(strong("DISCOVERY"), mod_sectionInfoGDApp_ui("sectionInfoGDApp_1") ),
                 tabPanel(div(icon("fire"), "Genome wide association (", icon("dna"),")"), mod_gwasqkApp_ui("gwasqkApp_1") ), #  icon = icon("chart-simple")), # may include P3D, traditional single linear regression, Eigen, etc.
                 tabPanel(strong("MUTATION HISTORY"), mod_sectionInfoMHApp_ui("sectionInfoMHApp_1") ),
                 tabPanel(div(icon("chart-line"), "Mutation rate (", icon("dna"),")", style = "color:red"), mod_mutatioRateApp_ui("mutatioRateApp_1") ) #  icon = icon("disease")) # may include
      ),

      navbarMenu("Gene flow and Drift", icon = icon("wind"),
                 tabPanel(strong("DRIFT & FLOW HISTORY"), mod_sectionInfoGFDHApp_ui("sectionInfoGFDHApp_1") ),
                 tabPanel(div(icon("circle-nodes"), "Population structure (", icon("dna"), icon("seedling"), ")"), mod_PopStrApp_ui("PopStrApp_1") ), #  icon = icon("diagram-project")), # may include PCA based, structure based, clustering
                 # tabPanel(div(icon("chart-line"),  "Number of founders (", icon("dna"), ")"), mod_neApp_ui("neApp_1") ), #  icon = icon("filter")),
                 tabPanel(div(icon("chart-line"), "Linkage disequilibrium (",  icon("dna"), ")", style = "color:red"),  mod_linkageDisApp_ui("linkageDisApp_1") ), #  icon = icon("circle-nodes")), # may include k-means, simulated annealing
                 tabPanel(div(icon("circle-nodes"), "Pool formation (",  icon("dna"), icon("seedling"), ")", style = "color:red"),  mod_poolFormApp_ui("poolFormApp_1") ), #  icon = icon("circle-nodes")), # may include k-means, simulated annealing
                 tabPanel(div(icon("circle-nodes"), "Pop-subset formation (", icon("dna"), ")", style = "color:red"), mod_subsetSelApp_ui("subsetSelApp_1") ), # stpga
                 tabPanel(div(icon("barcode"), "Marker-assisted introgression (", icon("dna"), ")"), mod_hybridityApp_ui("hybridityApp_1") ),
      ),

      navbarMenu("Other functions", icon = icon("medal"),
                 tabPanel(strong("DASHBOARDS"), mod_sectionInfoDASHApp_ui("sectionInfoDASHApp_1")  ),
                 tabPanel("Analytical Modules", mod_reportBuilder_ui("reportBuilder_1") , icon = icon("file") ),
                 tabPanel("Accelerate (ABI)", mod_abiDashboard_ui("abiDashboard_1"), icon = icon("puzzle-piece") ),
                 # tabPanel(strong("PREDICTIVE MODELING"),   ),
                 # tabPanel("Neural Networks", mod_tensorMLApp_ui("tensorMLApp_1"), icon = icon("puzzle-piece") ),
                 tabPanel("On-Farm Trial Decision", mod_oftStaApp_ui("oftStaApp_1"), icon = icon("file") ),
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
      app_title = "CGIAR Bioflow"
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

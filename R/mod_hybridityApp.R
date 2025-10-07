#' hybridityApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_hybridityApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    # HTML( as.character(div(style="color: red; font-size: 30px;", "Module under construction.")) ),
    # img(src = "www/bongo.gif", height = 400, width = 200), # add an image

    shiny::mainPanel(width = 12,
                     tabsetPanel( id=ns("tabsMain"),
                                  type = "tabs",
                                  tabPanel(div(icon("book"), "Information") ,
                                           br(),
                                           column(width = 6,
                                                  h1(strong(span("Marker-Assisted F1 Verification Module", tags$a(href="https://www.youtube.com/watch?v=6Ooq9I3LEp8&list=PLZ0lafzH_UmclOPifjCntlMzysEB2_2wX&index=5", icon("youtube") , target="_blank"), style="color:darkcyan"))),
                                                  h2(strong("Data Status (wait to be displayed):")),
                                                  uiOutput(ns("warningMessage")),
                                                  tags$br(),
                                                  # column(width=4, tags$br(), tags$br(),
                                                  shinyWidgets::prettySwitch( inputId = ns('launch'), label = "Load example dataset", status = "success"),
                                                  # ),
                                                  tags$br(),
                                                  # img(src = "www/qaGeno.png", height = 100, width = 435), # add an image)
                                           ),


                                           column(width = 6,
                                                  h2(strong("Details")),
                                                  p("The availability of genetic markers allow a more accurate quality assurance of crosses of different kind (e.g., F1, BCn). This module
                                                      allows to use the genetic data to perform this QA/QC:"),
                                                  p(strong("Columns to define the expected genotypes.-"),"  columns that define the expected genotype."),
                                                  p(strong("Markers to be used.-")," selection of genetic markers to be used for the verification process. Markers can be filter according to their informativeness"),
                                                  p(strong("Heteroygosity threshold"),"maximum allowed heterozygosity in parental genotypes"),
                                                  p(strong("Match probability threshold"),"user-defined match probability thresholds for an individual to be classified as a true F1 or not"),
                                                  h2(strong("References")),
                                                  p("Tukey, J. W. (1977). Exploratory Data Analysis. Section 2C."),
                                                  p("Velleman, P. F. and Hoaglin, D. C. (1981). Applications, Basics and Computing of Exploratory Data Analysis. Duxbury Press."),
                                                  # column(width = 12, shiny::plotOutput(ns("plotDataDependencies")), ),
                                           ),

                                  ),
                                  tabPanel(div(icon("arrow-right-to-bracket"), "Input steps"),
                                           tabsetPanel(

                                             # ------------ Pick QA-Stamp tab ------------

                                             tabPanel(div( icon("dice-one"), "Pick QA-stamp(s)", icon("arrow-right") ), # icon = icon("dice-one"),
                                                      br(),
                                                      column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                             column(width=8, selectInput(ns("version2F1qaqc"), "QA-geno stamp(s) to apply", choices = NULL, multiple = TRUE)),

                                                      ),
                                                      column(width=12),
                                                      shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                          column(width=12,
                                                                                 hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                                 h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameters to be specified in the grey boxes above.", style="color:green"))),
                                                                                 hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                          ),
                                                                          column( width=8, shiny::plotOutput(ns("plotTimeStamps")), br(),br(), ),
                                                      ),
                                             ),


                                             # ------------ Select Markers Tab ------------

                                             tabPanel(
                                               div(icon("dice-two"), "Select marker(s)", icon("arrow-right")),
                                               br(),

                                               # --- Bioflow panels ---
                                               tags$head(
                                                 tags$style(HTML(".bioflow-panel {
                                                 background: #6e6e6e;          /* darker gray */
                                                 color: #ffffff;
                                                 border: 1px solid #d6d8dc;
                                                 border-radius: 8px;
                                                 padding: 16px 18px;
                                                 }
                                                 .bioflow-panel-2 {
                                                 background: #f2f3f5;          /* light gray */
                                                 color: #1f1f1f;
                                                 border: 1px solid #5a5a5a;
                                                 border-radius: 6px;
                                                 padding: 12px 14px;
                                                 margin-bottom: 14px;
                                                 }
                                                 .bioflow-panel h4 { margin: 0 0 4px 0; }"
                                                 )
                                                 ),
                                               ),

                                               div(class = "bioflow-panel",

                                                   # Lighter 'About this tab'
                                                   div(
                                                     class = "bioflow-panel-2",
                                                     HTML('
                                                     <strong>Markers informativeness score</strong>
                                                     <ul>
                                                     <li>Score = 2: marker is homozygous in both parents (a single genotype is expected in the progeny).</li>
                                                     <li>Score = 1: marker is heterozygous in one of the parents.</li>
                                                     <li>Score = 0: marker is heterozygous in both parents.</li>
                                                     </ul>')
                                                   ),

                                                 # Row: genetic units to average (default mother + father) + explainer
                                                 fluidRow(
                                                   column(
                                                     width = 6,
                                                     column(width=8, selectizeInput(ns("units2Verif"),
                                                                                    label = tags$span(
                                                                                      "Genetic unit(s) to average for expected genotype",
                                                                                      tags$i(
                                                                                        class = "glyphicon glyphicon-info-sign",
                                                                                        style = "color:#FFFFFF",
                                                                                        title = "Genetic units that will be averaged for estimating the expected F1 dosage at each marker. Defaults are the indicated mother and father columns in the pedigree file"
                                                                                      ))
                                                                                    , choices = NULL, multiple = TRUE)
                                                            )
                                                     )
                                                   ),

                                                 # Row: informativeness filtering options for each cross
                                                 fluidRow(
                                                   column(
                                                     width = 5,  # left side
                                                     div(class = "stacked-checkboxes",
                                                         checkboxInput(
                                                           ns("keepAll"),
                                                           label = tags$span(
                                                             "Select all markers",
                                                             tags$i(
                                                               class = "glyphicon glyphicon-info-sign",
                                                               style = "color:#FFFFFF",
                                                               title = "No filters applied"
                                                             )),
                                                           value = TRUE
                                                         ),
                                                         checkboxInput(
                                                           ns("keepScore2"),
                                                           label = tags$span(
                                                             "Keep only markers with ",strong("score = 2"),
                                                             tags$i(
                                                               class = "glyphicon glyphicon-info-sign",
                                                               style = "color:#FFFFFF",
                                                               title = "This filter is applied in a per-cross basis"
                                                             )),
                                                           value = FALSE
                                                         ),
                                                         checkboxInput(
                                                           ns("keepScoreNon0"),
                                                           label = tags$span(
                                                             "Keep only markers with ",strong("score ≠ 0"),
                                                             tags$i(
                                                               class = "glyphicon glyphicon-info-sign",
                                                               style = "color:#FFFFFF",
                                                               title = "This filter is applied in a per-cross basis"
                                                             )),
                                                           value = FALSE
                                                         )
                                                     )
                                                   ),
                                                   column(width = 7)  # right-side spacer
                                                 ),

                                                 # Row: explicit marker picker
                                                 fluidRow(
                                                   column(
                                                     width = 6,
                                                     selectizeInput(
                                                       ns("markers2Verif"),
                                                       "Marker(s) to use (optional, overrides filters)",
                                                       choices = NULL,
                                                       multiple = TRUE,
                                                       options = list(
                                                         placeholder = "Start typing to search markers…",
                                                         plugins = list("remove_button"),
                                                         maxOptions = 200  # cap matches shown
                                                       )
                                                     )
                                                   )
                                               ),

                                               # Spacer
                                               column(width = 12, br()),

                                               # ------------ Collapsible Visual Aid ------------
                                               shinydashboard::box(
                                                 width = 12, status = "success", solidHeader = TRUE,
                                                 collapsible = TRUE, collapsed = TRUE,
                                                 title = "Visual aid (click the '+' to open)",

                                                 column(
                                                   width = 12,
                                                   hr(style = "border-top: 3px solid #4c4c4c;"),
                                                   h5(
                                                     strong(
                                                       span(
                                                         "These visuals do not affect the analysis; they help you choose filters above.",
                                                         style = "color:green"
                                                       )
                                                     )
                                                   ),
                                                   hr(style = "border-top: 3px solid #4c4c4c;")
                                                 ),

                                                 # Bar plot for informativeness per cross
                                                 column(
                                                   width = 12,
                                                   fluidRow(
                                                     column(
                                                       width = 12,
                                                       h5("Markers' informativeness score count per cross"),
                                                       plotly::plotlyOutput(ns("bar_score"), height = "260px")
                                                       )
                                                     )
                                                 ),
                                               )
                                             ),
                                             ),

                                             # ------------ Parental Quality Control ------------

                                             tabPanel(
                                               div(icon("dice-two"), "Parental homozygosity"),
                                               br(),

                                               # --- Bioflow panels ---
                                               tags$head(
                                                 tags$style(HTML(".bioflow-panel {
                                                 background: #6e6e6e;          /* darker gray */
                                                 color: #ffffff;
                                                 border: 1px solid #d6d8dc;
                                                 border-radius: 8px;
                                                 padding: 16px 18px;
                                                 }
                                                 .bioflow-panel-2 {
                                                 background: #f2f3f5;          /* light gray */
                                                 color: #1f1f1f;
                                                 border: 1px solid #5a5a5a;
                                                 border-radius: 6px;
                                                 padding: 12px 14px;
                                                 margin-bottom: 14px;
                                                 }
                                                 .bioflow-panel h4 { margin: 0 0 4px 0; }"
                                                                )
                                                            ),
                                                 ),

                                               # --- Main gray panel ---
                                               div(class = "bioflow-panel",

                                                   # Lighter 'About this tab'
                                                   div(
                                                     class = "bioflow-panel-2",
                                                     HTML("<strong>About this tab:</strong> This tab is aimed at line breeding programs where parents are expected to be highly homozygous.")
                                                   ),

                                                   # Controls
                                                   fluidRow(
                                                     column(
                                                       width = 4,
                                                       sliderInput(
                                                         ns("parentHetThreshold"),
                                                         label = tags$span(
                                                           "Heterozygosity threshold (%)",
                                                           tags$i(
                                                             class = "glyphicon glyphicon-info-sign",
                                                             style = "color:#FFFFFF",
                                                             title = "Maximum allowed percent of heterozygous loci per parent. Set to 100 to avoid any filters."
                                                           )
                                                         ),
                                                         min = 0, max = 100, value = 100, step = 1
                                                       )
                                                     )
                                                   ),

                                                   # Spacer
                                                   column(width = 12, br()),

                                                   # ------------ Collapsible Visual Aid ------------
                                                   shinydashboard::box(
                                                     width = 12, status = "success", solidHeader = TRUE,
                                                     collapsible = TRUE, collapsed = TRUE,
                                                     title = "Visual aid (click the '+' to open)",

                                                     column(
                                                       width = 12,
                                                       hr(style = "border-top: 3px solid #4c4c4c;"),
                                                       h5(
                                                         strong(
                                                           span(
                                                             "These visuals do not affect the analysis; they help you choose filters above.",
                                                             style = "color:green"
                                                           )
                                                         )
                                                       ),
                                                       hr(style = "border-top: 3px solid #4c4c4c;")
                                                     ),

                                                     # Histogram with heterozygosity distribution
                                                     column(
                                                       width = 12,
                                                       fluidRow(
                                                         width = 6,
                                                         h5("Histogram of parental heterozygosity"),
                                                         plotly::plotlyOutput(ns("hist_par_het"), height = "260px")
                                                         )
                                                       )
                                                     )
                                                   )
                                             ),

                                             # ------------ Match Probability thresholds ------------

                                             tabPanel("Set match thresholds", icon = icon("dice-two"),
                                                      br(),

                                                      # --- Bioflow panels ---
                                                      tags$head(
                                                      tags$style(HTML(".bioflow-panel {
                                                      background: #6e6e6e;          /* darker gray */
                                                      color: #ffffff;
                                                      border: 1px solid #d6d8dc;
                                                      border-radius: 8px;
                                                      padding: 16px 18px;
                                                      }
                                                      .bioflow-panel-2 {
                                                      background: #f2f3f5;          /* light gray */
                                                      color: #1f1f1f;
                                                      border: 1px solid #5a5a5a;
                                                      border-radius: 6px;
                                                      padding: 12px 14px;
                                                      margin-bottom: 14px;
                                                      }.bioflow-panel h4 { margin: 0 0 4px 0; }"
                                                                      )
                                                                 ),
                                                      ),

                                                      div(class = "bioflow-panel",

                                                          # Lighter 'About this tab'
                                                          div(
                                                            class = "bioflow-panel-2",
                                                            HTML("<strong>About this tab:</strong>
                                                                   <li>Set thresholds that classify each progeny as <em>PASS (true F1)</em>, <em>LIKELY F1</em>, <em>UNCERTAIN</em>, or <em>MISMATCH</em> based on its match probability.</li>
                                                                   <li>An individual’s match probability is the mean of the per-marker match probabilities.</li>
                                                                   <li>A marker would only possibly have a match probability of 1 when it has a single expected genotype given the parents (when both parents are homozygous at that marker).</li>
                                                                   <li>Including many less-informative markers (i.e., heterozygous in a parent) lowers match probabilities and increases uncertainty.</li>
                                                                   <li>Using only highly informative markers allow for more strict thresholds </li>
                                                                   </ul>")
                                                          ),

                                                          fluidRow(

                                                            column(width = 3,
                                                                   tags$div(
                                                                     numericInput(ns("upper_thres_mp"),
                                                                                  label = tags$span(
                                                                                    "Upper match probability threshold",
                                                                                    tags$i(class = "glyphicon glyphicon-info-sign",
                                                                                           style = "color:#FFFFFF",
                                                                                           title = "Value of match probability above which the progeny is considered a true F1 (PASS)")
                                                                                  ),value = 1.0,min = 0,max = 1,step = 0.01)
                                                                   )
                                                            ),
                                                            column(width = 3,
                                                                   tags$div(
                                                                     numericInput(ns("mid_thres_mp"),
                                                                                  label = tags$span(
                                                                                    "Mid match probability threshold",
                                                                                    tags$i(class = "glyphicon glyphicon-info-sign",
                                                                                           style = "color:#FFFFFF",
                                                                                           title = "Value of match probability above which the progeny is considered a LIKELY F1")
                                                                                  ),value = 0.9,min = 0,max = 1,step = 0.01)
                                                                   )
                                                            ),
                                                            column(width = 3,
                                                                   tags$div(
                                                                     numericInput(ns("low_thres_mp"),
                                                                                  label = tags$span(
                                                                                    "Lower match probability threshold",
                                                                                    tags$i(class = "glyphicon glyphicon-info-sign",
                                                                                           style = "color:#FFFFFF",
                                                                                           title = "Value of match probability below which the progeny is considered a MISMATCH")
                                                                                  ),value = 0.8,min = 0,max = 1,step = 0.01)
                                                                   )
                                                            )

                                                          )


                                                             )
                                                      ),

                                             # ------------ Run analysis ------------

                                             tabPanel("Run analysis", icon = icon("dice-four"),
                                                      br(),
                                                      column(width=12,style = "background-color:grey; color: #FFFFFF",
                                                             column(width=3, tags$div(textInput(ns("analysisIdName"), label = tags$span(
                                                               "Analysis Name (optional)", tags$i( class = "glyphicon glyphicon-info-sign", style = "color:#FFFFFF",
                                                                                                   title = "An optional name for the analysis besides the timestamp if desired.") ), #width = "100%",
                                                               placeholder = "(optional name)") ) ),
                                                             column(width=3,
                                                                    br(),
                                                                    actionButton(ns("runQaMb"), "Compute verification", icon = icon("play-circle")),
                                                                    uiOutput(ns("qaQcStaInfo")),
                                                                    br(),
                                                             ),

                                                      ),textOutput(ns("outQaMb")),
                                             ),
                                           ) # end of tabset
                                  ),# end of output panel
                                  tabPanel(div(icon("arrow-right-from-bracket"), "Output tabs" ) , value = "outputTabs",
                                           tabsetPanel(
                                             tabPanel("Dashboard", icon = icon("file-image"),
                                                      br(),
                                                      textOutput(ns("outQaMb2")),
                                                      br(),
                                                      actionButton(ns("renderReportVerifGeno"), "Download dashboard", icon = icon("download")),
                                                      downloadButton(ns("downloadReportVerifGeno"), "Download dashboard", style = "visibility:hidden;"),
                                                      br(),
                                                      uiOutput(ns('reportVerifGeno'))
                                             ),
                                             tabPanel("Predictions", icon = icon("table"),
                                                      br(),
                                                      DT::DTOutput(ns("predictionsVerif")),
                                             ),
                                             tabPanel("Metrics", icon = icon("table"),
                                                      br(),
                                                      DT::DTOutput(ns("metricsVerif")),
                                             ),
                                             tabPanel("Modeling", icon = icon("table"),
                                                      br(),
                                                      DT::DTOutput(ns("modelingVerif")),
                                             ),
                                           ),
                                  ),
                     )) # end mainpanel



  )
}

#' hybridityApp Server Functions
#'
#' @noRd
mod_hybridityApp_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # output$plotDataDependencies <- shiny::renderPlot({ dependencyPlot() })
    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })

    ############################################################################
    # show shinyWidgets until the user can use the module
    observeEvent(c(data()), {
      req(data())
      if(!is.null(data()$data$geno)){
        golem::invoke_js('showid', ns('holder'))
      }else{
        golem::invoke_js('hideid', ns('holder'))
      }
    })
    ############################################################################
    # warning message
    output$warningMessage <- renderUI(
      if(is.null(data())){
        HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your data using the 'Data' tab.")) )
      }else{ # data is there
        if(!is.null(data()$data$geno)){

          if(is.null(data()$data$pedigree)){
            HTML( as.character(div(style="color: red; font-size: 20px;", "Pedigree information is required to run this module")))
          }else{
            HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to perform F1 QA/QC specifying your input parameters under the Input tabs.")) )
          }

        }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your genotype data using the 'Data' tab. ")) )}
      }
    )


    ## data example loading
    observeEvent(
      input$launch,
      if(length(input$launch) > 0){
        if (input$launch) {
          shinyWidgets::ask_confirmation(
            inputId = ns("myconfirmation"),
            text = "Are you sure you want to load the example data? This will delete any data currently in the environment.",
            title = "Data replacement warning"
          )
        }
      }
    )

    observeEvent(input$myconfirmation, {
      if (isTRUE(input$myconfirmation)) {
        shinybusy::show_modal_spinner('fading-circle', text = 'Loading example...')
        ## replace tables
        tmp <- data()
        data(cgiarBase::create_getData_object())
        utils::data(F1_example, package = "cgiarPipeline")
        if(!is.null(result$data)){tmp$data <- result$data}
        if(!is.null(result$metadata)){tmp$metadata <- result$metadata}
        if(!is.null(result$modifications)){tmp$modifications <- result$modifications}
        if(!is.null(result$predictions)){tmp$predictions <- result$predictions}
        if(!is.null(result$metrics)){tmp$metrics <- result$metrics}
        if(!is.null(result$modeling)){tmp$modeling <- result$modeling}
        if(!is.null(result$status)){tmp$status <- result$status}
        #tmp$data$pedigree$Hybrid_Parent1 <- sample(result$data$pedigree$Hybrid)
        #tmp$data$pedigree$Hybrid_Parent2 <- sample(result$data$pedigree$Hybrid)
        data(tmp) # update data with results
        shinybusy::remove_modal_spinner()
      }else{
        shinyWidgets::updatePrettySwitch(session, "launch", value = FALSE)
      }
    }, ignoreNULL = TRUE)

    #################

    ###############################
    ###############################
    ###############################
    ## select id tab

    ## render timestamps flow plot
    output$plotTimeStamps <- shiny::renderPlot({
      req(data()) # req(input$version2Sta)
      xx <- data()$status;  yy <- data()$modeling
      v <- which(yy$parameter == "analysisId")
      if(length(v) > 0){
        yy <- yy[v,c("analysisId","value")]
        zz <- merge(xx,yy, by="analysisId", all.x = TRUE)
      }else{ zz <- xx; zz$value <- NA}
      if(!is.null(xx)){
        colnames(zz) <- cgiarBase::replaceValues(colnames(zz), Search = c("analysisId","value"), Replace = c("outputId","inputId") )
        nLevelsCheck1 <- length(na.omit(unique(zz$outputId)))
        nLevelsCheck2 <- length(na.omit(unique(zz$inputId)))
        if(nLevelsCheck1 > 1 & nLevelsCheck2 > 1){
          X <- with(zz, sommer::overlay(outputId, inputId))
        }else{
          if(nLevelsCheck1 <= 1){
            X1 <- matrix(ifelse(is.na(zz$inputId),0,1),nrow=length(zz$inputId),1); colnames(X1) <- as.character(na.omit(unique(c(zz$outputId))))
          }else{X1 <- model.matrix(~as.factor(outputId)-1, data=zz); colnames(X1) <- levels(as.factor(zz$outputId))}
          if(nLevelsCheck2 <= 1){
            X2 <- matrix(ifelse(is.na(zz$inputId),0,1),nrow=length(zz$inputId),1); colnames(X2) <- as.character(na.omit(unique(c(zz$inputId))))
          }else{X2 <- model.matrix(~as.factor(inputId)-1, data=zz); colnames(X2) <- levels(as.factor(zz$inputId))}
          mynames <- unique(na.omit(c(zz$outputId,zz$inputId)))
          X <- matrix(0, nrow=nrow(zz), ncol=length(mynames)); colnames(X) <- as.character(mynames)
          if(!is.null(X1)){X[,colnames(X1)] <- X1}
          if(!is.null(X2)){X[,colnames(X2)] <- X2}
        };  rownames(X) <- as.character(zz$outputId)
        rownames(X) <-as.character(as.POSIXct(as.numeric(rownames(X)), origin="1970-01-01", tz="GMT"))
        colnames(X) <-as.character(as.POSIXct(as.numeric(colnames(X)), origin="1970-01-01", tz="GMT"))
        # make the network plot
        n <- network::network(X, directed = FALSE)
        network::set.vertex.attribute(n,"family",zz$module)
        network::set.vertex.attribute(n,"importance",1)
        e <- network::network.edgecount(n)
        network::set.edge.attribute(n, "type", sample(letters[26], e, replace = TRUE))
        network::set.edge.attribute(n, "day", sample(1, e, replace = TRUE))
        #library(ggnetwork)
        ggplot2::ggplot(n, ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
          ggnetwork::geom_edges(ggplot2::aes(color = family), arrow = ggplot2::arrow(length = ggnetwork::unit(6, "pt"), type = "closed") ) +
          ggnetwork::geom_nodes(ggplot2::aes(color = family), alpha = 0.5, size=5 ) + ggplot2::ggtitle("Network plot of current analyses available") +
          ggnetwork::geom_nodelabel_repel(ggplot2::aes(color = family, label = vertex.names ),
                                          fontface = "bold", box.padding = ggnetwork::unit(1, "lines")) +
          ggnetwork::theme_blank()
      }
    })


    #Select geno qa/qc analysis id

    observeEvent(c(data()), {
      req(data())
      dtMta <- data()
      dtMta <- dtMta$status
      dtMta <- dtMta[which(dtMta$module == "qaGeno"),]
      traitsMta <- unique(dtMta$analysisId)
      if(length(traitsMta) > 0){
        if("analysisIdName" %in% colnames(dtMta)){
          names(traitsMta) <- paste(dtMta$analysisIdName, as.POSIXct(traitsMta, origin="1970-01-01", tz="GMT"), sep = "_")
        }else{
          names(traitsMta) <- as.POSIXct(traitsMta, origin="1970-01-01", tz="GMT")
        }
      }
      updateSelectInput(session, "version2F1qaqc", choices = traitsMta)
    })

    ###############################
    ###############################
    ###############################
    # select markers tab

    #Genetic units box
    observeEvent(c(data(),input$version2F1qaqc), {
      req(data()$data$pedigree)
      dtVerif <- data()
      dtPed <- dtVerif$data$pedigree
      if(!is.null(dtPed)){
        metaPed <- dtVerif$metadata$pedigree
        traitsVerif <- colnames(dtPed)
        first <- metaPed$value[metaPed$parameter=="mother"]
        second <- metaPed$value[metaPed$parameter=="father"]
        updateSelectizeInput(session, "units2Verif", choices = traitsVerif, selected = c(first,second))
      }
    })

    #Filters and custom selection of markers
    observeEvent(input$keepScore2, {
      if (isTRUE(input$keepScore2)) {
        updateCheckboxInput(session, "keepScoreNon0", value = FALSE)
        updateCheckboxInput(session, "keepAll",value = FALSE)
      }
    }, ignoreInit = TRUE)

    observeEvent(input$keepScoreNon0, {
      if (isTRUE(input$keepScoreNon0)) {
        updateCheckboxInput(session, "keepScore2", value = FALSE)
        updateCheckboxInput(session, "keepAll",value = FALSE)
      }
    }, ignoreInit = TRUE)

    observeEvent(input$keepAll, {
      if (isTRUE(input$keepAll)) {
        updateCheckboxInput(session, "keepScore2", value = FALSE)
        updateCheckboxInput(session, "keepScoreNon0",value = FALSE)
        updateSelectizeInput(session, "markers2Verif", selected = character(0))
      }
    }, ignoreInit = TRUE)

    observeEvent(c(data(),input$version2F1qaqc), {
      req(data())
      req(input$version2F1qaqc)
      dtVerif <- data()
      #dtVerif <- dtVerif$data$geno
      if (class(dtVerif$data$geno)[1] == "genlight") {
        qas <- which(names(dtVerif$data$geno_imp) == input$version2F1qaqc)
        #dtVerif <- as.data.frame(dtVerif$data$geno_imp[qas])
        traitsVerif <- adegenet::locNames(dtVerif$data$geno_imp[[qas]])
      }
      if(!is.null(dtVerif)){
        #traitsVerif <- colnames(dtVerif)
        updateSelectizeInput(
          session,
          "markers2Verif",
          choices = traitsVerif,
          selected = character(0),   # don’t preselect anything
          server = TRUE,             # enable server-side searching
          options = list(
            maxOptions = 200,
            placeholder = "Start typing to search markers…"
          )
        )
      }
    })

    observeEvent(input$markers2Verif, {
      if (length(input$markers2Verif %||% character(0)) > 0) {
        updateCheckboxInput(session, "keepAll",   value = FALSE)
        updateCheckboxInput(session, "keepScore2",   value = FALSE)
        updateCheckboxInput(session, "keepScoreNon0", value = FALSE)
      }
    }, ignoreInit = TRUE)

    #Visualization bar plot

    output$bar_score <- plotly::renderPlotly({

      req(data()$data$pedigree)
      req(input$version2F1qaqc)
      req(input$units2Verif)

      #genotypic data
      dtVerif <- data()
      #dtVerif <- dtVerif$data$geno
      if (class(dtVerif$data$geno)[1] == "genlight") {
        qas <- which(names(dtVerif$data$geno_imp) == input$version2F1qaqc)
        Markers <- as.data.frame(dtVerif$data$geno_imp[qas])
      }

      #get ploidy
      ploidy = dtVerif$metadata$geno
      ploidy = ploidy[ploidy$parameter == "ploidity","value"]
      ploidy = as.numeric(ploidy)

      #pedigree
      dtPed <- dtVerif$data$pedigree
      parents = dtPed[,input$units2Verif]
      #keep only F1's with parental information
      parents = stats::na.omit(parents)

      if(length(input$units2Verif) == 1){ #Assume selfing
        M = Markers[parents%in%rownames(Markers),]

        #Flag homozygous genotypes in M
        Mhomo = ((M/ploidy) == 0 |(M/ploidy) == 1)

        #If parents have missing genotypes
        missing_par = parents[!parents %in% rownames(Markers)]
        #Create matrix for parents without genotypes
        if(length(missing_par)!=0){
          Mmiss = matrix(NA, nrow = length(missing_par),ncol = ncol(M))
          rownames(Mmiss) = missing_par
          colnames(Mmiss) = colnames(M)

          Mhomo = rbind(Mhomo,Mmiss)
        }

        Mhomo = Mhomo[parents,]
        score_count = data.frame(cross = parents,
                                 nScore2 = rowSums(Mhomo, na.rm = T),
                                 nScore1 = rep(0,length(parents)),
                                 nScore0 = rowSums(Mhomo == 0),
                                 miss_count = rowSums(is.na(Mhomo)))

      }else{
        Mscore = list()
        idx = 1
        for(par in input$units2Verif){
          M = Markers[parents[,par]%in%rownames(Markers),]
          #Flag homozygous genotypes in M
          Mhomo = ((M/ploidy) == 0 |(M/ploidy) == 1)

          #If parents have missing genotypes
          missing_par = parents[,par][!parents[,par] %in% rownames(Markers)]
          #Create matrix for parents without genotypes
          if(length(missing_par)!=0){
            Mmiss = matrix(NA, nrow = length(missing_par),ncol = ncol(M))
            rownames(Mmiss) = missing_par
            colnames(Mmiss) = colnames(M)

            Mhomo = rbind(Mhomo,Mmiss)
          }

          Mscore[[idx]] = Mhomo[match(parents[,par], rownames(Mhomo)),]
          idx = idx+1
        }
        Mscore = Reduce('+', Mscore)
        score_count = data.frame(cross = do.call(paste, c(parents, sep = "/")),
                                 nScore2 = rowSums(Mscore == 2, na.rm = T),
                                 nScore1 = rowSums(Mscore == 1, na.rm = T),
                                 nScore0 = rowSums(Mscore == 0, na.rm = T),
                                 miss_count = rowSums(is.na(Mscore)))
      }

      score_long = tidyr::pivot_longer(
        score_count,
        cols = c(nScore2, nScore1, nScore0, miss_count),
        names_to = "category",
        values_to = "count"
      )

      score_long$category = factor(
        score_long$category,
        levels = c("nScore2", "nScore1", "nScore0", "miss_count"),
        labels = c("Score = 2", "Score = 1", "Score = 0", "Missing")
      )
      score_long$count[is.na(score_long$count)] = 0

      plotly::plot_ly(
        score_long,
        x = ~cross, y = ~count, color = ~category, type = "bar",
        text = ~paste0("Cross: ", cross,
                       "<br>Category: ", category,
                       "<br>Count: ", count),
        hoverinfo = "text"
      ) |>
        plotly::layout(
          barmode = "stack",
          xaxis = list(title = "Cross", showticklabels = FALSE),
          yaxis = list(title = "Number of Markers"),
          title  = list(text = "Markers’ informativeness score count per cross", x = 0),
          margin = list(t = 60),
          hovermode = "closest"
        )


    })

    #### Parental homozygosity check
    output$hist_par_het = plotly::renderPlotly({
      req(data()$data$pedigree)
      req(input$version2F1qaqc)
      req(input$units2Verif)
      req(input$parentHetThreshold)

      dtVerif <- data()

      # --- Get genotype matrix with individuals as rows, markers as columns ---
      Markers <- NULL
      if (inherits(dtVerif$data$geno, "genlight")) {
        qas <- which(names(dtVerif$data$geno_imp) == input$version2F1qaqc)
        if (length(qas) > 0) {
          gl_obj <- dtVerif$data$geno_imp[[qas]]
          Markers <- as.data.frame(gl_obj)
          rownames(Markers) <- adegenet::indNames(gl_obj)
        }
      }
      validate(need(!is.null(Markers) && nrow(Markers) > 0, "No genotype data available."))

      #get ploidy
      ploidy = dtVerif$metadata$geno
      ploidy = ploidy[ploidy$parameter == "ploidity","value"]
      ploidy = as.numeric(ploidy)

      # --- Collect parent IDs (1 or 2 columns) ---
      dtPed <- dtVerif$data$pedigree
      parents_df <- dtPed[, input$units2Verif, drop = FALSE]
      parents_vec <- unique(na.omit(as.vector(as.matrix(parents_df))))
      validate(need(length(parents_vec) > 0, "No parent IDs present after removing NAs."))

      # --- Match parents to genotype rows ---
      present_parents <- intersect(parents_vec, rownames(Markers))
      if (length(present_parents) == 0) {
        return(
          plotly::plotly_empty(type = "scatter", mode = "markers") %>%
            plotly::layout(
              title = "Heterozygosity of parents",
              annotations = list(
                list(
                  text = "None of the listed parents were found in the genotype data.",
                  x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE
                )
              )
            )
        )
      }

      M <- Markers[present_parents, , drop = FALSE]

      # --- Heterozygosity % per parent: (non-homozygous among non-missing) * 100 ---
      # Homozygous if genotype == 0 or == ploidy. Handle NAs safely.
      Mhomo <- (M == 0 | M == ploidy)
      n_non_missing <- rowSums(!is.na(M))
      n_homo <- rowSums(Mhomo, na.rm = TRUE)
      het_pct <- (pmax(n_non_missing - n_homo, 0) / pmax(n_non_missing, 1)) * 100

      het_prop <- data.frame(
        parent = rownames(M),
        het = het_pct,
        stringsAsFactors = FALSE
      )

      # --- Cutoff and summary ---
      thr <- as.numeric(input$parentHetThreshold)
      excluded_n <- sum(het_prop$het > thr, na.rm = TRUE)
      included_n <- sum(het_prop$het <= thr, na.rm = TRUE)
      total_n <- nrow(het_prop)
      excluded_pct <- round(100 * excluded_n / max(total_n, 1), 1)

      # --- Build histogram with vertical cutoff and shaded excluded region (to the right) ---
      p <- plotly::plot_ly(
        het_prop, x = ~het, type = "histogram",
        nbinsx = 30, name = "Parents"
      ) %>%
        plotly::layout(
          title = list(text = "Parent heterozygosity (%)"),
          xaxis = list(title = "Heterozygosity (%)"),
          yaxis = list(title = "Count"),
          barmode = "overlay",
          shapes = list(
            # Vertical cutoff line
            list(
              type = "line",
              x0 = thr, x1 = thr, xref = "x",
              y0 = 0,   y1 = 1,   yref = "paper",
              line = list(color = "red", width = 2, dash = "dash")
            ),
            # Shaded excluded area to the right of the cutoff
            list(
              type = "rect",
              x0 = thr, x1 = max(het_prop$het, na.rm = TRUE),
              xref = "x",
              y0 = 0, y1 = 1, yref = "paper",
              fillcolor = "rgba(255,0,0,0.08)", line = list(width = 0)
            )
          ),
          annotations = list(
            list(
              x = thr, xref = "x", y = 1.02, yref = "paper",
              text = paste0(
                "Cutoff: ", thr, "% — Excluded: ", excluded_n, "/", total_n,
                " (", excluded_pct, "%)"
              ),
              showarrow = FALSE, align = "left"
            )
          ),
          showlegend = FALSE
        )

      p
    })

    ##########################
    ## run button
    outQaMb <- eventReactive(input$runQaMb, {

      req(data())
      req(input$version2F1qaqc)

      shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
      ##
      ## store the new modifications table

      #Setup marker selection
      dtVerif <- data()

      if (inherits(dtVerif$data$geno, "genlight")) {
        qas <- which(names(dtVerif$data$geno_imp) == input$version2F1qaqc)
        if (length(qas) > 0) {
          gl_obj <- dtVerif$data$geno_imp[[qas]]
          selMarkers<- adegenet::locNames(gl_obj)
        }
      }

      if(input$keepAll){
        filter_by_score = NULL
      }

      if(input$keepScore2){
        filter_by_score = "Score2"
      }

      if(input$keepScoreNon0){
        filter_by_score = "ScoreNon0"
      }

      if(length(input$markers2Verif %||% character(0)) > 0){
        selMarkers = input$markers2Verif
        filter_by_score = NULL
      }

      #get ploidy
      ploidy = dtVerif$metadata$geno
      ploidy = ploidy[ploidy$parameter == "ploidity","value"]
      ploidy = as.numeric(ploidy)


      result <- cgiarPipeline::individualVerification(
        object= data(),
        analysisIdForGenoModifications= input$version2F1qaqc,
        markersToBeUsed=selMarkers,
        colsForExpecGeno=input$units2Verif,
        ploidy=ploidy,
        sc_filter=filter_by_score,
        het=input$parentHetThreshold,
        matchThres=c(input$upper_thres_mp,input$mid_thres_mp,input$low_thres_mp)
      )
      #if(!inherits(result,"try-error")) {
      #  provitional <- individualVerification(
      #    #cgiarPipeline::individualVerification(
      #    object= data(),
      #    analysisIdForGenoModifications= input$version2F1qaqc,
      #    markersToBeUsed=selMarkers,
      #    colsForExpecGeno=input$units2Verif,
      #    ploidy=ploidy,
      #    sc_filter=filter_by_score,
      #    het = input$parentHetThreshold,
      #    matchThres = c(input$upper_thres_mp,input$mid_thres_mp,input$low_thres_mp),
      #    onlyMats=TRUE
      #  )
      #}

      # save(result, file = "./R/outputs/resultVerifGeno.RData")
      shinybusy::remove_modal_spinner()

      if(!inherits(result,"try-error")) { # if all goes well in the run
        cat(paste("Genotype verification analysis saved with id:",as.POSIXct( result$status$analysisId[nrow(result$status)], origin="1970-01-01", tz="GMT") ))
        output$outQaMb2 <- renderPrint({
          cat(paste("Genotype verification analysis saved with id:",as.POSIXct( result$status$analysisId[nrow(result$status)], origin="1970-01-01", tz="GMT") ))
        })
        if("analysisIdName" %in% colnames(result$status)){result$status$analysisIdName[nrow(result$status)] <- input$analysisIdName}
        data(result)
        updateTabsetPanel(session, "tabsMain", selected = "outputTabs")
        # predictions
        output$predictionsVerif <-  DT::renderDT({
          # if(!inherits(result,"try-error") ){
          predictions <- result$predictions
          predictions <- predictions[predictions$module=="gVerif",]
          predictions$analysisId <- as.numeric(predictions$analysisId)
          predictions <- predictions[!is.na(predictions$analysisId),]
          current.predictions <- predictions[predictions$analysisId==max(predictions$analysisId),]
          current.predictions <- subset(current.predictions, select = -c(module,analysisId))
          numeric.output <- c("predictedValue", "stdError", "reliability")
          DT::formatRound(DT::datatable(current.predictions, extensions = 'Buttons',
                                        options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                       lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
          ), numeric.output)
          # }
        }, server = FALSE)
        # metrics
        output$metricsVerif <-  DT::renderDT({
          # if(!inherits(result,"try-error") ){
          metrics <- result$metrics
          metrics <- metrics[metrics$module=="gVerif",]
          current.metrics <- metrics[metrics$analysisId==(metrics$analysisId[length(metrics$analysisId)]),]
          current.metrics <- subset(current.metrics, select = -c(module,analysisId))
          numeric.output <- c("value", "stdError")
          DT::formatRound(DT::datatable(current.metrics, extensions = 'Buttons',
                                        options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                       lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
          ), numeric.output,digits  = c(0, 3))
          # }
        }, server = FALSE)
        # modeling
        output$modelingVerif <-  DT::renderDT({
          # if(!inherits(result,"try-error") ){
          modeling <- result$modeling
          modeling <- modeling[modeling$module=="gVerif",]
          modeling$analysisId <- as.numeric(modeling$analysisId)
          modeling <- modeling[!is.na(modeling$analysisId),]
          current.modeling <- modeling[modeling$analysisId==max(modeling$analysisId),]
          current.modeling <- subset(current.modeling, select = -c(module,analysisId))
          # current.modeling$analysisId <- as.POSIXct(current.modeling$analysisId, origin="1970-01-01", tz="GMT")
          DT::datatable(current.modeling, extensions = 'Buttons',
                        options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                       lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
          )
          # }
        }, server = FALSE)
        ###############################
        # ## Report tab
        output$reportVerifGeno <- renderUI({
          HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportVerifGeno.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
        })

        report <- reactiveVal(NULL)

        observeEvent(input$renderReportVerifGeno,{
          shinybusy::show_modal_spinner(spin = "fading-circle", text = "Generating Report...")

          src <- normalizePath(system.file("rmd","reportVerifGeno.Rmd",package="bioflow"))
          src2 <- normalizePath('data/resultVerifGeno.RData')

          # temporarily switch to the temp dir, in case you do not have write
          # permission to the current working directory
          owd <- setwd(tempdir())
          on.exit(setwd(owd))

          file.copy(src, 'report.Rmd', overwrite = TRUE)
          file.copy(src2, 'resultVerifGeno.RData', overwrite = TRUE)

          outReport <- rmarkdown::render('report.Rmd', params = list(toDownload=TRUE ),
                                         switch("HTML", HTML = rmdformats::robobook(toc_depth = 4)
                                                # HTML = rmarkdown::html_document()
                                         ))

          report(outReport)

          shinybusy::remove_modal_spinner()

          shinyjs::click("downloadReportVerifGeno")
        })

        output$downloadReportVerifGeno <- downloadHandler(
          filename = function() {
            paste(paste0('gVerif_dashboard_',gsub("-", "", as.integer(Sys.time()))), sep = '.', switch(
              "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
            ))
          },
          content = function(file) {

            out <- report()

            file.rename(out, file)
          }
        )

      }else{ hideAll$clearAll <- TRUE}

      hideAll$clearAll <- FALSE

    })
    output$outQaMb <- renderPrint({
      outQaMb()
    })



  })
}

## To be copied in the UI
# mod_hybridityApp_ui("hybridityApp_1")

## To be copied in the server
# mod_hybridityApp_server("hybridityApp_1")

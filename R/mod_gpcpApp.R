#' gpcpApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_gpcpApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    tags$br(),

    mainPanel(width = 12,
              tabsetPanel( id=ns("tabsMain"),
                           type = "tabs",
                           tabPanel( div(icon("book"), "Information") ,
                                     br(),
                                     column(width = 6,
                                            h1(strong(span("Genomic Prediction of Cross Peformance Module", tags$a(href="https://www.youtube.com/watch?v=LTiN7G6zCtM&list=PLZ0lafzH_UmclOPifjCntlMzysEB2_2wX&index=9", icon("youtube") , target="_blank"), style="color:darkcyan"))),
                                            h2(strong("Data Status (wait to be displayed):")),
                                            uiOutput(ns("warningMessage")),
                                            tags$br(),
                                            # column(width=4, tags$br(),
                                            #shinyWidgets::prettySwitch( inputId = ns('launch'), label = "Load example dataset", status = "success"),
                                            # ),
                                            tags$br(),
                                            img(src = "www/ocs2.png", height = 400, width = 250), # add an image
                                     ),
                                     column(width = 6,
                                            h2(strong("Details")),
                                            p("Genomic prediction of cross performance (GPCP) is a method that uses both additive and dominance effects in order to predict the average genetic merit of crosses.
                                            This implementation combines GPCP with optimal contribution selection so that the new crosses are still optimised given a desired trade-off between
                                          short-term gain(performance) and long-term gain (genetic variance).
                                The way the options are used is the following:"),

                                            p(strong("Trait for cross prediction.-")," Trait to be be used for predicting all possible crosses (an index is suggested)."),
                                            p(strong("Entry types to use.-")," Which entry types should be used in the algorithm."),
                                            p(strong("Number of crosses.-")," Number of top crosses to be selected."),
                                            p(strong("Target angle.-")," Target angle defining the trade-off between performance and diversity. Zero degrees is weighting strongly towards performance. Ninety degrees is weighting strongly towards diversity."),
                                            p(strong("Additional settings.-")),
                                            p(strong("Maximum number of top individuals to use.-")," The complexity and computation time of the algorithm scales up with greater number of individuals used for predicted crosses. This arguments applies a filter to only use the top N individuals for the trait of interest."),
                                            p(strong("Stopping criteria.-")," Maximum number of runs (iterations) without change in the genetic algorithm."),
                                            p(strong("Relationship to use.-")," Only GRMs are used for prediction"),
                                            p(strong("Environment to use.-")," If the user wants to use predictions from an specific environment. In NULL all are used."),
                                            p(strong("Notes.-"),"Consider that the predictions table in this particular case is different. In this case, the 'predictedValue' column refers to
                                the expected value of the cross, 'stdError' is the average inbreeding of the cross, and 'rel' has the genetic algorithm value (lower the better)."),
                                            h2(strong("References")),
                                            p("Werner, C. R., Gaynor, R. C., Sargent, D. J., Lillo, A., Gorjanc, G., & Hickey, J. M. (2023). Genomic selection strategies for clonally propagated crops. Theoretical and Applied Genetics, 136(4), 74."),
                                            p("Kinghorn, B. (1999). 19. Mate Selection for the tactical implementation of breeding programs. Proceedings of the Advancement of Animal Breeding and Genetics, 13, 130-133."),
                                            p("https://alphagenes.roslin.ed.ac.uk/wp/wp-content/uploads/2019/05/01_OptimalContributionSelection.pdf?x44213"),
                                            p("Woolliams, J. A., Berg, P., Dagnachew, B. S., & Meuwissen, T. H. E. (2015). Genetic contributions and their optimization. Journal of Animal Breeding and Genetics, 132(2), 89-99."),
                                            h2(strong("Software used")),
                                            p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
                                Vienna, Austria. URL https://www.R-project.org/."),
                                            p("https://github.com/gaynorr/QuantGenResources"),
                                            # column(width = 6, shiny::plotOutput(ns("plotDataDependencies")), ),
                                     ),
                           ),
                           tabPanel(div(icon("arrow-right-to-bracket"), "Input steps"),
                                    tabsetPanel(
                                      tabPanel(div( icon("dice-one"), "Pick Index-stamp", icon("arrow-right") ), #icon = icon("dice-one"),
                                               br(),
                                               column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                      column(width=8, selectInput(ns("version2Gpcp"), "Index version to analyze", choices = NULL, multiple = FALSE)),

                                               ),
                                               column(width=12),
                                               shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                   column(width=12,
                                                                          hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                          h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameters to be specified in the grey boxes above.", style="color:green"))),
                                                                          hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                   ),
                                                                   column( width=12, shiny::plotOutput(ns("plotTimeStamps")) ),
                                                                   DT::DTOutput(ns("statusGpcp")),
                                                                   DT::DTOutput(ns("phenoGpcp")),
                                               ),
                                      ),
                                      tabPanel( div( icon("dice-two"), "Select entries", icon("arrow-right") ) , # icon = icon("dice-two"),
                                                br(),
                                                column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                       column(width=4,
                                                              selectInput(ns("trait2Gpcp"),
                                                                          label = tags$span(
                                                                            "Trait(s) to optimize",
                                                                            tags$i(
                                                                              class = "glyphicon glyphicon-info-sign",
                                                                              style = "color:#FFFFFF",
                                                                              title = "It is important to include traits that also need to be controlled in the population (e.g., height, maturity) even when we don't want to increase them or decrease them."
                                                                            )
                                                                          ),
                                                                          choices = NULL, multiple = TRUE),
                                                       ),
                                                       column(width=4,
                                                              selectInput(ns("effectType2Gpcp"),
                                                                          label = tags$span(
                                                                            "Effect type to use",
                                                                            tags$i(
                                                                              class = "glyphicon glyphicon-info-sign",
                                                                              style = "color:#FFFFFF",
                                                                              title = "Since each fixed and random effect returns estimates the user should specify which effect should be used in the gpcp. It is expected by default that the designation estimates are the ones to be used in gpcp"
                                                                            )
                                                                          ),
                                                                          choices = NULL, multiple = FALSE),
                                                       ),
                                                       column(width=4,
                                                              selectInput(ns("entryType2Gpcp"),
                                                                          label = tags$span(
                                                                            "Entry type to use in optimization",
                                                                            tags$i(
                                                                              class = "glyphicon glyphicon-info-sign",
                                                                              style = "color:#FFFFFF",
                                                                              title = "The labels available in the entryType column can be used to reduce the dataset for the index. By default all entry types are included."
                                                                            )
                                                                          ),
                                                                          choices = NULL, multiple = TRUE)
                                                       ),
                                                ),
                                                column(width=12),
                                                shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                    column(width=12,
                                                                           hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                           h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameters to be specified in the grey boxes above.", style="color:green"))),
                                                                           hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                    ),
                                                                    tags$span(id = ns('holder1'),
                                                                              column(width=4, selectInput(ns("trait3Gpcp"), "Trait to visualize", choices = NULL, multiple = FALSE) ) ,
                                                                              column(width=4, selectInput(ns("groupGpcpInputPlot"), "Group by", choices = c("environment","effectType","entryType"), multiple = FALSE, selected = "entryType") ),
                                                                              column(width=2, checkboxInput(ns("checkbox"), label = "Include x-axis labels", value = TRUE) ),
                                                                              column(width=2, numericInput(ns("fontSize"), label = "x-axis font size", value = 12, step=1),),
                                                                              column(width=12, plotly::plotlyOutput(ns("plotPredictionsCleanOut")) ),
                                                                    ),
                                                ),
                                      ),
                                      tabPanel(div(icon("dice-three"), "Set contribution", icon("arrow-right") ) ,  # icon = icon("dice-three"),
                                               br(),
                                               column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                      column(width=4,
                                                             textInput(ns("nCrossGpcp"),
                                                                       label = tags$span(
                                                                         "Number of crosses",
                                                                         tags$i(
                                                                           class = "glyphicon glyphicon-info-sign",
                                                                           style = "color:#FFFFFF",
                                                                           title = "To specify the final number of crosses to form the new generation."
                                                                         )
                                                                       ),
                                                                       value="70") ),
                                                      column(width=4,
                                                             textInput(ns("targetAngleGpcp"),
                                                                       label = tags$span(
                                                                         "Target angle",
                                                                         tags$i(
                                                                           class = "glyphicon glyphicon-info-sign",
                                                                           style = "color:#FFFFFF",
                                                                           title = "A zero value is equivalent to greater weight to performance and low weight to genetic variance. A 90 degree represents the opposite."
                                                                         )
                                                                       ),
                                                                       value="30") ),
                                                      column(width=4,
                                                             selectInput(ns("relType"),
                                                                         label = tags$span(
                                                                           "Relationship to use",
                                                                           tags$i(
                                                                             class = "glyphicon glyphicon-info-sign",
                                                                             style = "color:#FFFFFF",
                                                                             title = "The optimization to weight the genetic variance requires marker based relationship to calculate the variance covariance between designations."
                                                                           )
                                                                         ),
                                                                         choices = "Marker", multiple = FALSE ) ),

                                                      ## --- NEW: shows only when "Marker" (= "grm") is chosen ----
                                                      column(width = 4,
                                                             uiOutput(ns("version2GpcpGeno_ui"))   # <-- new placeholder
                                                      )
                                               ),
                                               column(width=12),
                                               shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                   column(width=12,
                                                                          hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                          h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameters to be specified in the grey boxes above.", style="color:green"))),
                                                                          hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                   ),
                                                                   column(width=3, img(src = "www/ocs2.png", height = 400, width = 250),), # add an image
                                                                   column(width=9, DT::DTOutput(ns("evaluationUnits")),),
                                               ),
                                      ),
                                      tabPanel("Run analysis", icon = icon("dice-four"),
                                               br(),
                                               column(width=12,style = "background-color:grey; color: #FFFFFF",
                                                      column(width=3, tags$div(textInput(ns("analysisIdName"), label = tags$span(
                                                        "Analysis Name (optional)", tags$i( class = "glyphicon glyphicon-info-sign", style = "color:#FFFFFF",
                                                                                            title = "An optional name for the analysis besides the timestamp if desired.") ), #width = "100%",
                                                        placeholder = "(optional name)") ) ),
                                                      column( width = 3,
                                                              br(),
                                                              actionButton(ns("runGpcp"), "Run analysis", icon = icon("play-circle")),
                                                              uiOutput(ns("qaQcGpcpInfo")),
                                                      ),
                                                      br(),
                                                      column( width = 6,
                                                              shinydashboard::box(width = 12, status = "success", style = "color: #000000", solidHeader=FALSE,collapsible = TRUE, collapsed = TRUE, title = "Additional run settings (optional)...",
                                                                                  numericInput(ns("numberBest"), label = "Maximum number of top individuals to use", value = 100),
                                                                                  numericInput(ns("maxRun"), label = "Stopping criteria (#of iterations without change)", value = 40),
                                                                                  selectInput(ns("env2Gpcp"), "Environment to use", choices = NULL, multiple = FALSE),
                                                                                  selectInput(ns("verboseGpcp"), label = "Print logs?", choices = list(TRUE,FALSE), selected = FALSE, multiple=FALSE)
                                                              ),
                                                      ),
                                                      br(),
                                               ),
                                               textOutput(ns("outGpcp")),
                                      ),
                                    )
                           ),
                           tabPanel(div(icon("arrow-right-from-bracket"), "Output tabs" ) , value = "outputTabs",
                                    tabsetPanel(
                                      tabPanel("Dashboard", icon = icon("file-image"),
                                               br(),
                                               textOutput(ns("outGpcp2")),
                                               br(),
                                               downloadButton(ns("downloadReportGpcp"), "Download dashboard"),
                                               br(),
                                               uiOutput(ns('reportGpcp'))
                                      ),
                                      tabPanel("Predictions", icon = icon("table"),
                                               br(),
                                               DT::DTOutput(ns("predictionsGpcp")),
                                      ),
                                      tabPanel("Metrics", icon = icon("table"),
                                               br(),
                                               DT::DTOutput(ns("metricsGpcp")),
                                      ),
                                      tabPanel("Modeling", icon = icon("table"),
                                               br(),
                                               DT::DTOutput(ns("modelingGpcp")),
                                      ),

                                    )
                           )
              )) # end mainpanel


  )
}


mod_gpcpApp_server <- function(id, data){
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
    observeEvent(c(data(), input$version2Gpcp ), {
      req(data())
      mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
      if(mappedColumns == 3 & length(input$version2Gpcp)>0 ){
        golem::invoke_js('showid', ns('holder1'))
      }else{
        golem::invoke_js('hideid', ns('holder1'))
      }
    })
    ############################################################################
    # warning message
    #output$warningMessage <- renderUI(
    #  if(is.null(data())){
    #    HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your phenotypic data using the 'Data Retrieval' tab.")) )
    #  }else{ # data is there
    #    mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
    #    if(mappedColumns == 3){
    #      if(  any(c("mta","mtaFlex","mtaLmms") %in% data()$status$module) ){
    #        if( ("qaGeno" %in% data()$status$module)){ # user has markers
    #          HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to perform genomic prediction of cross performance (GPCP) specifying your input parameters under the Input tabs.")) )
    #        }else{
    #          HTML( as.character(div(style="color: red; font-size: 20px;", "Please make sure that you have markers (and QA the data) to run this module.")) )
    #        }
    #      }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please perform a Multi-Trial Analysis or a selection index before performing genomic prediction of cross performance (GPCP).")) ) }
    #    }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please make sure that you have computed the 'environment' column, and that column 'designation' and \n at least one trait have been mapped using the 'Data Retrieval' tab.")) )}
    #  }
    #)

    output$warningMessage <- renderUI({

      ## 0. No data --------------------------------------------------------------
      if (is.null(data())) {
        return(
          HTML(as.character(
            div(style = "color:red; font-size:20px;",
                "Please retrieve or load your phenotypic data using the 'Data Retrieval' tab.")
          ))
        )
      }else{
        ## 1. Flags ---------------------------------------------------------------
        mappedColumns <- all(c("environment", "designation", "trait") %in%
                               data()$metadata$pheno$parameter)

        ## 2. Mapping not finished ------------------------------------------------
        if (!mappedColumns) {
          return(
            HTML(as.character(
              div(style = "color:red; font-size:20px;",
                  "Please map 'environment', 'designation', and at least one 'trait' in the Data Retrieval tab.")
            ))
          )
        }else{
          hasMTA       <- any(data()$status$module %in% c("mta", "mtaAsr","mtaFlex", "mtaLmms"))
          hasMarkers   <- "qaGeno" %in% data()$status$module
          #hasGPCPslot  <- ("GPCP" %in% names(data())) && !is.null(data()$GPCP)
		  hasGPCPslot   <- !is.null(which(data()$predictions$effectType=="designationA")) && !is.null(which(data()$predictions$effectType=="designationD")) && !is.null(which(data()$predictions$effectType=="inbreeding"))

          ## 3. Markers / QA missing -----------------------------------------------
          if (!hasMarkers) {
            return(
              HTML(as.character(
                div(style = "color:red; font-size:20px;",
                    "Please upload marker data and run Genotype QA/QC before using this module.")
              ))
            )
          }

          ## 4. MTA (A+D) or GPCP slot missing --------------------------------------
          if (!hasMTA || !hasGPCPslot) {
            return(
              HTML(as.character(
                div(style = "color:red; font-size:20px;",
                    "You need to run the 'Main (A + D)' model in the MTA module before using this module.")
              ))
            )
          }

          ## 5. Everything ready ----------------------------------------------------

          if(hasMTA & hasMarkers & hasGPCPslot){
            HTML(as.character(
              div(style = "color:green; font-size:20px;",
                  "Data complete – you can proceed to Genomic Prediction of Cross Performance (GPCP) via the Input tabs.")
            ))
          }

        }
      }
    })

    ## data example loading
    #observeEvent(
    #  input$launch,
    #  if(length(input$launch) > 0){
    #    if (input$launch) {
    #      shinyWidgets::ask_confirmation(
    #        inputId = ns("myconfirmation"),
    #        text = "Are you sure you want to load the example data? This will delete any data currently in the environment.",
    #        title = "Data replacement warning"
    #      )
    #    }
    #  }
    #)
    #observeEvent(input$myconfirmation, {
    #  if (isTRUE(input$myconfirmation)) {
    #    shinybusy::show_modal_spinner('fading-circle', text = 'Loading example...')
    #    ## replace tables
    #    tmp <- data()
    #    data(cgiarBase::create_getData_object())
    #    utils::data(DT_example, package = "cgiarPipeline")
    #    if(!is.null(result$data)){tmp$data <- result$data}
    #    if(!is.null(result$metadata)){tmp$metadata <- result$metadata}
    #    if(!is.null(result$modifications)){tmp$modifications <- result$modifications}
    #    if(!is.null(result$predictions)){tmp$predictions <- result$predictions}
    #    if(!is.null(result$metrics)){tmp$metrics <- result$metrics}
    #    if(!is.null(result$modeling)){tmp$modeling <- result$modeling}
    #    if(!is.null(result$status)){tmp$status <- result$status}
    #    data(tmp) # update data with results
    #    shinybusy::remove_modal_spinner()
    #  }else{
    #    shinyWidgets::updatePrettySwitch(session, "launch", value = FALSE)
    #  }
    #}, ignoreNULL = TRUE)
    #################
    ## version
    observeEvent(c(data()), {
      req(data())
      dtGpcp <- data()
      dtGpcp <- dtGpcp$status
      dtGpcp <- dtGpcp[which(dtGpcp$module %in% c("indexD")),]
      traitsGpcp <- unique(dtGpcp$analysisId)
      if(length(traitsGpcp) > 0){
        if("analysisIdName" %in% colnames(dtGpcp)){
          names(traitsGpcp) <- paste(dtGpcp$analysisIdName, as.POSIXct(traitsGpcp, origin="1970-01-01", tz="GMT"), sep = "_")
        }else{
          names(traitsGpcp) <- as.POSIXct(traitsGpcp, origin="1970-01-01", tz="GMT")
        }
      }
      updateSelectInput(session, "version2Gpcp", choices = traitsGpcp)
    })
    #################
    ## traits
    observeEvent(c(data(), input$version2Gpcp), {
      req(data())
      req(input$version2Gpcp)
      dtGpcp <- data()
      dtGpcp <- dtGpcp$predictions
      dtGpcp <- dtGpcp[which(dtGpcp$analysisId == input$version2Gpcp),]
      traitsGpcp <- unique(dtGpcp$trait)
      updateSelectInput(session, "trait2Gpcp", choices = traitsGpcp)
    })
    observeEvent(c(data(), input$version2Gpcp), {
      req(data())
      req(input$version2Gpcp)
      dtGpcp <- data()
      dtGpcp <- dtGpcp$predictions
      dtGpcp <- dtGpcp[which(dtGpcp$analysisId == input$version2Gpcp),]
      traitsGpcp <- unique(dtGpcp$trait)
      updateSelectInput(session, "traitFilterPredictions2D2", choices = traitsGpcp)
    })
    ##############
    # treatments
    observeEvent(c(data(), input$version2Gpcp, input$nCrossGpcp, input$targetAngleGpcp, input$traitFilterPredictions2D2), {
      req(data())
      req(input$version2Gpcp)
      req(input$traitFilterPredictions2D2)
      req(input$nCrossGpcp)
      req(input$targetAngleGpcp)
      a <- as.numeric(gsub(" ","",unlist(strsplit(input$nCrossGpcp,","))))
      b <- as.numeric(gsub(" ","",unlist(strsplit(input$targetAngleGpcp,","))))
      forLoop <- expand.grid(a, b)
      traitsGpcp <- apply(forLoop,1, function(x){ paste(input$traitFilterPredictions2D2,"~", paste(x[1],"crosses *",x[2], "degrees"))})
      updateSelectInput(session, "environment", choices = traitsGpcp)
    })

    ##############
    ## effect type
    observeEvent(c(data(), input$version2Gpcp, input$trait2Gpcp), {
      req(data())
      req(input$version2Gpcp)
      req(input$trait2Gpcp)
      dtGpcp <- data()
      dtGpcp <- dtGpcp$predictions
      dtGpcp <- dtGpcp[which(dtGpcp$analysisId == input$version2Gpcp),]
      dtGpcp <- dtGpcp[which(dtGpcp$trait == input$trait2Gpcp),]
      traitsGpcp <- unique(dtGpcp$effectType)
      updateSelectInput(session, "effectType2Gpcp", choices = traitsGpcp, selected = traitsGpcp)
    })

    ##############
    ## entry type
    observeEvent(c(data(), input$version2Gpcp, input$trait2Gpcp, input$effectType2Gpcp), {
      req(data())
      req(input$version2Gpcp)
      req(input$trait2Gpcp)
      req(input$effectType2Gpcp)
      dtGpcp <- data()
      dtGpcp <- dtGpcp$predictions
      dtGpcp <- dtGpcp[which(dtGpcp$analysisId == input$version2Gpcp),]
      dtGpcp <- dtGpcp[which(dtGpcp$trait == input$trait2Gpcp),]
      dtGpcp <- dtGpcp[which(dtGpcp$effectType == input$effectType2Gpcp),]
      traitsGpcp <- unique(dtGpcp$entryType)
      updateSelectInput(session, "entryType2Gpcp", choices = traitsGpcp, selected = traitsGpcp)
    })

    ##############
    ## rel type
    observeEvent(c(data(), input$version2Gpcp, input$trait2Gpcp), {
      req(data())
      req(input$version2Gpcp)
      req(input$trait2Gpcp)
      availableTypes <- list()
      dtGpcp <- data()

      if(!is.null(dtGpcp$metadata$geno)){
        availableTypes[["Marker"]] <- "grm"
      }

      updateSelectInput(session, "relType", choices = availableTypes, selected = availableTypes[1])
    })

    # ──────────────────────────────────────────────────────────────────────────
    # Genotype-QA/QC stamp selector (shown only when relType == "grm")
    # ──────────────────────────────────────────────────────────────────────────

    ## 2.1  Build the choice vector the same way the MTA module does ------------
    qaGenoChoices <- reactive({
      req(data())
      st <- data()$status
      qa  <- st[st$module == "qaGeno", , drop = FALSE]

      ids <- unique(qa$analysisId)

      if (length(ids) == 0)
        return(c("No data available" = "0"))

      if ("analysisIdName" %in% colnames(qa)) {
        names(ids) <- paste(
          qa$analysisIdName[match(ids, qa$analysisId)],
          as.POSIXct(ids, origin = "1970-01-01", tz = "GMT"),
          sep = "_"
        )
      } else {
        names(ids) <- as.character(
          as.POSIXct(ids, origin = "1970-01-01", tz = "GMT")
        )
      }
      ids
    })

    ## 2.2  Render the UI element conditionally ---------------------------------
    output$version2GpcpGeno_ui <- renderUI({
      req(input$relType)              # make sure relType exists

      # in the rel-type observer you mapped "Marker" → value "grm"
      if (input$relType == "grm") {
        selectInput(
          ns("version2GpcpGeno"),
          label = tags$span(
            "Genotype QA/QC version(s) to analyze",
            tags$i(
              class  = "glyphicon glyphicon-info-sign",
              style  = "color:#FFFFFF",
              title  = "Analysis ID(s) from Genotype QA/QC that will be used to build the grm."
            )
          ),
          choices  = qaGenoChoices(),
          multiple = FALSE
        )
      } else {
        NULL              # hide the box for any non-marker relationship type
      }
    })
    ##############
    ## environment
    observeEvent(c(data(), input$version2Gpcp, input$trait2Gpcp), {
      req(data())
      req(input$version2Gpcp)
      req(input$trait2Gpcp)
      dtGpcp <- data()
      dtGpcp <- dtGpcp$predictions
      dtGpcp <- dtGpcp[which(dtGpcp$analysisId == input$version2Gpcp),]
      dtGpcp <- dtGpcp[which(dtGpcp$trait == input$trait2Gpcp),]
      traitsGpcp <- unique(dtGpcp$environment)
      updateSelectInput(session, "env2Gpcp", choices = traitsGpcp)
    })

    ##############################################################################################
    ##############################################################################################
    ##############################################################################################
    ##############################################################################################
    ## render plots
    observeEvent(c(data(),input$version2Gpcp), { # update trait
      req(data())
      req(input$version2Gpcp)
      dtGpcp <- data()
      dtGpcp <- dtGpcp$predictions
      dtGpcp <- dtGpcp[which(dtGpcp$analysisId %in% input$version2Gpcp),] # only traits that have been QA
      traitGpcpInput <- unique(dtGpcp$trait)
      updateSelectInput(session, "trait3Gpcp", choices = traitGpcpInput)
    })
    output$plotPredictionsCleanOut <- plotly::renderPlotly({ # update plot
      req(data())
      req(input$version2Gpcp)
      req(input$trait3Gpcp)
      req(input$fontSize)
      req(input$groupGpcpInputPlot)
      mydata <- data()$predictions
      mydata <- mydata[which(mydata$analysisId %in% input$version2Gpcp),] # only traits that have been QA
      if(input$groupGpcpInputPlot == "entryType"){mydata <- mydata[which(mydata$entryType %in% input$entryType2Gpcp),]}
      mydata <- mydata[which(mydata[,"trait"] %in% input$trait3Gpcp),]
      mydata[, "environment"] <- as.factor(mydata[, "environment"]); mydata[, "designation"] <- as.factor(mydata[, "designation"])
      mydata <- mydata[,which(!duplicated(colnames(mydata)))]
      mydata$myEntryType <- mydata[,input$groupGpcpInputPlot]
      p <- ggplot2::ggplot(mydata, ggplot2::aes(x=as.factor(myEntryType), y=predictedValue)) +
        ggplot2::geom_boxplot(fill='#A4A4A4', color="black", notch = TRUE, outliers = FALSE)+
        ggplot2::theme_classic() + ggplot2::ggtitle("Boxplot of trait dispersion by entry type") +
        ggplot2::geom_jitter(ggplot2::aes(colour = myEntryType), alpha = 0.5) +
        ggplot2::xlab("Entry type") + ggplot2::ylab("Trait value") +
        # ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)) +
        ggplot2::scale_color_manual(values = c(valid = "#66C2A5", tagged = "#FC8D62"))
      if(input$checkbox){
        p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1))
      }else{
        p <- p + ggplot2::theme(axis.text.x = ggplot2::element_blank())
      }
      p
    })
    ## render evaluation units
    output$evaluationUnits <-  DT::renderDT({
      req(data())
      req(input$version2Gpcp)
      object <- data()
      # get the total number of individuals possible to estimate
      metaPed <- object$metadata$pedigree
      pedCols <- metaPed[which(metaPed$parameter %in% c("designation","mother","father")), "value"]
      pedCols <- setdiff(pedCols,"")
      metaCols <- metaPed[which(metaPed$value %in% pedCols), "parameter"]
      n <- apply(object$data$pedigree[,pedCols, drop=FALSE],2,function(x){length(na.omit(unique(x)))})
      # check how many have phenotypes
      dtMta <- object$predictions
      dtMta <- dtMta[which(dtMta$analysisId %in% input$version2Gpcp),] # only traits that have been QA
      nPheno <- apply(unique(object$data$pedigree[,pedCols, drop=FALSE]), 2, function(x){
        length(intersect(na.omit(unique(x)) , unique(dtMta$designation)))
      })
      # check how many have marker
      nGeno <- apply(unique(object$data$pedigree[,pedCols, drop=FALSE]), 2, function(x){
        length(intersect(na.omit(unique(x)) , rownames(as.data.frame(object$data$geno))))
      })
      final <- data.frame(cbind(metaCols,n, nPheno, nGeno))
      colnames(final) <- c("Evaluation unit", "N", "With phenotype", "With markers")
      rownames(final) <- NULL
      DT::datatable(final, extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All'))),
                    caption = htmltools::tags$caption(
                      style = 'color:cadetblue', #caption-side: bottom; text-align: center;
                      htmltools::em('Summary of selection units and marker availability.')
                    )
      )
    }, server = FALSE)
    ## render the data to be analyzed
    output$phenoGpcp <-  DT::renderDT({
      req(data())
      req(input$version2Gpcp)
      dtGpcp <- data()
      dtGpcp <- dtGpcp$predictions
      dtGpcp <- dtGpcp[which(dtGpcp$analysisId == input$version2Gpcp),setdiff(colnames(dtGpcp),c("module","analysisId"))]
      numeric.output <- c("predictedValue", "stdError", "reliability")
      DT::formatRound(DT::datatable(dtGpcp, extensions = 'Buttons',
                                    options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                   lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All'))),
                                    caption = htmltools::tags$caption(
                                      style = 'color:cadetblue', #caption-side: bottom; text-align: center;
                                      htmltools::em('Index predictions table to be used as input.')
                                    )
      ), numeric.output)
    }, server = FALSE)
    ## render timestamps flow
    output$plotTimeStamps <- shiny::renderPlot({
      req(data()) # req(input$version2Sta)
      xx <- data()$status;  yy <- data()$modeling # xx <- result$status;  yy <- result$modeling
      if("analysisIdName" %in% colnames(xx)){existNames=TRUE}else{existNames=FALSE}
      if(existNames){
        xx$analysisIdName <- paste(xx$analysisIdName, as.character(as.POSIXct(as.numeric(xx$analysisId), origin="1970-01-01", tz="GMT")),sep = "_" )
      }
      v <- which(yy$parameter == "analysisId")
      if(length(v) > 0){
        yy <- yy[v,c("analysisId","value")]
        zz <- merge(xx,yy, by="analysisId", all.x = TRUE)
      }else{ zz <- xx; zz$value <- NA}
      if(existNames){
        zz$analysisIdName <- cgiarBase::replaceValues(Source = zz$analysisIdName, Search = "", Replace = "?")
        zz$analysisIdName2 <- cgiarBase::replaceValues(Source = zz$value, Search = zz$analysisId, Replace = zz$analysisIdName)
      }
      if(!is.null(xx)){
        if(existNames){
          colnames(zz) <- cgiarBase::replaceValues(colnames(zz), Search = c("analysisIdName","analysisIdName2"), Replace = c("outputId","inputId") )
        }else{
          colnames(zz) <- cgiarBase::replaceValues(colnames(zz), Search = c("analysisId","value"), Replace = c("outputId","inputId") )
        }
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
        };
        rownames(X) <- as.character(zz$outputId)
        if(existNames){

        }else{
          rownames(X) <-as.character(as.POSIXct(as.numeric(rownames(X)), origin="1970-01-01", tz="GMT"))
          colnames(X) <-as.character(as.POSIXct(as.numeric(colnames(X)), origin="1970-01-01", tz="GMT"))
        }
        # make the network plot
        n <- network::network(X, directed = FALSE)
        network::set.vertex.attribute(n,"family",zz$module)
        network::set.vertex.attribute(n,"importance",1)
        e <- network::network.edgecount(n)
        network::set.edge.attribute(n, "type", sample(letters[26], e, replace = TRUE))
        network::set.edge.attribute(n, "day", sample(1, e, replace = TRUE))
        library(ggnetwork)
        ggplot2::ggplot(n, ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
          ggnetwork::geom_edges(ggplot2::aes(color = family), arrow = ggplot2::arrow(length = ggnetwork::unit(6, "pt"), type = "closed") ) +
          ggnetwork::geom_nodes(ggplot2::aes(color = family), alpha = 0.5, size=5 ) +
          ggnetwork::geom_nodelabel_repel(ggplot2::aes(color = family, label = vertex.names ),
                                          fontface = "bold", box.padding = ggnetwork::unit(1, "lines")) +
          ggnetwork::theme_blank() + ggplot2::ggtitle("Network plot of current analyses available")
      }
    })
    ## render modeling
    output$statusGpcp <-  DT::renderDT({
      req(data())
      req(input$version2Gpcp)
      dtSta <- data() # dtSta<- result
      ### change column names for mapping
      paramsPheno <- data()$modeling
      paramsPheno <- paramsPheno[which(paramsPheno$analysisId %in% input$version2Gpcp),, drop=FALSE]
      paramsPheno$analysisId <- as.POSIXct(paramsPheno$analysisId, origin="1970-01-01", tz="GMT")
      DT::datatable(paramsPheno, extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All'))),
                    caption = htmltools::tags$caption(
                      style = 'color:cadetblue', #caption-side: bottom; text-align: center;
                      htmltools::em('Past modeling parameters from Index stamp selected.')
                    )
      )
    }, server = FALSE)
    ###############################################
    ###############################################
    ###############################################
    ###############################################
    ###############################################
    ## render result of "run" button click
    outGpcp <- eventReactive(input$runGpcp, {
      req(data())
      req(input$version2Gpcp)
      req(input$trait2Gpcp)
      req(input$env2Gpcp)
      req(input$effectType2Gpcp)
      req(input$entryType2Gpcp)
      shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
      dtGpcp <- data()
      # run the modeling, but before test if mta was done
      if(sum(dtGpcp$status$module %in% c("mta","mtaAsr","mtaFlex","mtaLmms","indexD")) == 0) {
        output$qaQcGpcpInfo <- renderUI({
          if (hideAll$clearAll){
            return()
          }else{
            req(dtGpcp)
            HTML(as.character(div(style="color: brown;",
                                  "Please perform Multi-Trial-Analysis or Selection Index before conducting Optimal Cross Selection."))
            )
          }
        })
      }else{
        output$qaQcGpcpInfo <- renderUI({return(NULL)})
        result <- try(
		cgiarPipeline::gpcp(
          phenoDTfile= dtGpcp, # analysis to be picked from predictions database
          analysisId=input$version2Gpcp,
          analysisIdgeno = input$version2GpcpGeno,
          relDTfile= input$relType,
          trait= input$trait2Gpcp, # per trait
          environment=input$env2Gpcp,
          nCross=as.numeric(gsub(" ","",unlist(strsplit(input$nCrossGpcp,",")))),
          targetAngle=as.numeric(gsub(" ","",unlist(strsplit(input$targetAngleGpcp,",")))), # in radians
          verbose=input$verboseGpcp, maxRun = input$maxRun,
          effectType=input$effectType2Gpcp,
          entryType=input$entryType2Gpcp,
          numberBest = input$numberBest
        ),
        silent=TRUE
        )
        if(!inherits(result,"try-error")) {
          if("analysisIdName" %in% colnames(result$status)){result$status$analysisIdName[nrow(result$status)] <- input$analysisIdName}
          data(result) # update data with results

          save(result, file = "data/resultGpcp.RData")

          cat(paste("Optimal cross selection step with id:",as.POSIXct( result$status$analysisId[length(result$status$analysisId)], origin="1970-01-01", tz="GMT"),"saved. Please proceed to print this list and do your crossing block."))
          updateTabsetPanel(session, "tabsMain", selected = "outputTabs")
        }else{
          cat(paste("Analysis failed with the following error message: \n\n",result[[1]]))
        }
      }
      shinybusy::remove_modal_spinner()

      if(!inherits(result,"try-error")) {
        # view predictions
        output$predictionsGpcp <-  DT::renderDT({
          # if ( hideAll$clearAll){
          #   return()
          # }else{
          predictions <- result$predictions
          predictions <- predictions[predictions$module=="gpcp",]
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
        # view metrics
        output$metricsGpcp <-  DT::renderDT({
          # if ( hideAll$clearAll){
          #   return()
          # }else{
          metrics <- result$metrics
          metrics <- metrics[metrics$module=="gpcp",]
          metrics$analysisId <- as.numeric(metrics$analysisId)
          metrics <- metrics[!is.na(metrics$analysisId),]
          current.metrics <- metrics[metrics$analysisId==max(metrics$analysisId),]
          current.metrics <- subset(current.metrics, select = -c(module,analysisId))
          numeric.output <- c("value", "stdError")
          DT::formatRound(DT::datatable(current.metrics, extensions = 'Buttons',
                                        options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                       lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
          ), numeric.output)
          # }
        }, server = FALSE)
        # view modeling
        output$modelingGpcp <-  DT::renderDT({
          # if ( hideAll$clearAll){
          #   return()
          # }else{
          modeling <- result$modeling
          modeling <- modeling[modeling$module=="gpcp",]
          modeling$analysisId <- as.numeric(modeling$analysisId)
          modeling <- modeling[!is.na(modeling$analysisId),]
          current.modeling <- modeling[modeling$analysisId==max(modeling$analysisId),]
          current.modeling <- subset(current.modeling, select = -c(module,analysisId))
          DT::datatable(current.modeling, extensions = 'Buttons',
                        options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                       lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
          )
          # }
        }, server = FALSE)
        ## Report tab
        output$reportGpcp <- renderUI({
          HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportGpcp.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
        })

        output$downloadReportGpcp <- downloadHandler(
          filename = function() {
            paste(paste0('Gpcp_dashboard_',gsub("-", "", Sys.Date())), sep = '.', switch(
              "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
            ))
          },
          content = function(file) {
            src <- normalizePath(system.file("rmd","reportGpcp.Rmd",package="bioflow"))
            src2 <- normalizePath('data/resultGpcp.RData')
            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'report.Rmd', overwrite = TRUE)
            file.copy(src2, 'resultGpcp.RData', overwrite = TRUE)
            shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
            out <- rmarkdown::render('report.Rmd', params = list(toDownload=TRUE),switch(
              "HTML",
              HTML = rmdformats::robobook(toc_depth = 4)
              # HTML = rmarkdown::html_document()
            ))
            shinybusy::remove_modal_spinner()
            file.rename(out, file)
          }
        )

      } else {
        output$predictionsGpcp <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
        output$metricsGpcp <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
        output$modelingGpcp <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
      }


      hideAll$clearAll <- FALSE

    }) ## end eventReactive

    output$outGpcp <- renderPrint({
      outGpcp()
    })

  })
}

## To be copied in the UI
# mod_gpcpApp_ui("gpcpApp_1")

## To be copied in the server
# mod_gpcpApp_server("gpcpApp_1")

#' rggApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_rggApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    tags$br(),

    mainPanel( width = 12,
               tabsetPanel( id=ns("tabsMain"),
                            type = "tabs",
                            tabPanel(div(icon("book"), "Information") ,
                                     br(),
                                     column(width = 6,
                                            h1(strong(span("Realized Genetic Gain Module", tags$a(href="https://www.youtube.com/watch?v=5kkS98iJECs&list=PLZ0lafzH_UmclOPifjCntlMzysEB2_2wX&index=10", icon("youtube") , target="_blank"), style="color:darkcyan"))),
                                            h2(strong("Data Status (wait to be displayed):")),
                                            uiOutput(ns("warningMessage")),
                                            tags$br(),
                                            # column(width=3, tags$br(),
                                            shinyWidgets::prettySwitch( inputId = ns('launch'), label = "Load example dataset", status = "success"),
                                            # ),
                                            tags$br(),
                                            img(src = "www/rgg.png", height = 300, width = 500), # add an image
                                     ),
                                     column(width = 6,
                                            h2(strong("Details")),
                                            p("In order to monitor the efficacy of genetic evaluation across cycles of selection, the realized genetic gain is the preferred process.
                                          This option aims to calculate the realized genetic gain using the methods from Mackay et al. (2011). The
                              method uses across-environment means from multiple years of data that have been adjusted based on a good connectivity
                              to then fit a regression of the form means~year.of.origin. In case the means used are BLUPs these can be
                              deregressed.
                                The way the options are used is the following:"),
                                            p(strong("Method.-")," One of the following; Mackay et al. (2011) or Laidig et al. (2014)."),
                                            p(strong("Trait(s) to use.-")," Trait to be be used for realized genetic gain estimation (an index is suggested)."),
                                            p(strong("Years of origin to use.-")," Selection of the years of origin associated to the tested material to use in the calculation."),
                                            p(strong("Entry types to use.-")," A selection of entry types to use for the realized genetic gain calculation."),
                                            p(strong("Deregress weight.-")," Should any weight be applied to the deregressed value (not recommended but available)."),
                                            p(strong("Partition the data?.-")," When very few years of data are present this option will allow the user to calculate the gain for all 2-year combinations and then average these rates."),
                                            p(strong("Deregress estimates-")," Should we deregress the estimates by dividing over the reliability before performing the realized genetic gain calculation."),
                                            h2(strong("References")),
                                            p("Mackay, I., Horwell, A., Garner, J., White, J., McKee, J., & Philpott, H. (2011). Reanalyses of the historical series of UK variety trials
                                to quantify the contributions of genetic and environmental factors to trends and variability in yield over time. Theoretical and Applied
                                Genetics, 122, 225-238."),
                                            p("Laidig, F., Piepho, H. P., Drobek, T., Meyer, U. (2014). Genetic and non-genetic long-term trends of 12 different crops in German
                                official variety performance trials and on-farm yield trends. Theoretical and Applied Genetics, 127, 2599-2617."),
                                            p("Rutkoski, J. E. (2019). A practical guide to genetic gain. Advances in agronomy, 157, 217-249."),
                                            p("Gorjanc, G., Gaynor, R. C., Hickey, J. M. (2018). Optimal cross selection for long-term genetic gain in two-part programs with rapid recurrent genomic selection. Theoretical and applied genetics, 131, 1953-1966."),
                                            h2(strong("Software used")),
                                            p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
                                Vienna, Austria. URL https://www.R-project.org/."),

                                            # column(width = 12, shiny::plotOutput(ns("plotDataDependencies")), ),
                                     ),
                            ),
                            tabPanel(div(icon("arrow-right-to-bracket"), "Input steps"),
                                     tabsetPanel(
                                       tabPanel(div( icon("dice-one"), "Pick Index-stamp", icon("arrow-right") ), #  icon = icon("dice-one"),
                                                br(),
                                                column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                       column(width=6, selectInput(ns("version2Rgg"), "Data version to analyze", choices = NULL, multiple = FALSE) ),
                                                       column(width=3, radioButtons(ns("methodRgg"),"Method",choices=list("Mackay"="mackay","Piepho"="piepho"), selected="mackay") ),

                                                ),
                                                column(width=12),
                                                shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                    column(width=12,
                                                                           hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                           h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameter values to be specified in the grey boxes above.", style="color:green"))),
                                                                           hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                    ),
                                                                    column(width=12, shiny::plotOutput(ns("plotTimeStamps")) ),
                                                                    DT::DTOutput(ns("phenoRgg")),
                                                ),
                                       ),
                                       tabPanel(div( icon("dice-two"), "Select trait(s)", icon("arrow-right") ) , # icon = icon("dice-two"),
                                                br(),
                                                column(width=12, selectInput(ns("trait2Rgg"), "Trait(s) to use", choices = NULL, multiple = TRUE), style = "background-color:grey; color: #FFFFFF"),
                                                column(width=12),
                                                shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                    column(width=12,
                                                                           hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                           h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameter values to be specified in the grey boxes above.", style="color:green"))),
                                                                           hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                    ),
                                                                    tags$span(id = ns('holder1'),
                                                                              selectInput(ns("trait3Rgg"), "Trait to visualize regression over years.", choices = NULL, multiple = FALSE),
                                                                              plotly::plotlyOutput(ns("plotPredictionsCleanOut")),
                                                                    ),
                                                ),
                                       ),
                                       tabPanel( div( icon("dice-three"), "Select years & units", icon("arrow-right")  ), #  icon = icon("dice-three"),
                                                br(),
                                                column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                       column(width=3, selectInput(ns("yearsToUse"), "Year(s) of origin to use", choices = NULL, multiple = TRUE) ),
                                                       column(width=3, selectInput(ns("environmentToUse"), "Environment to use", choices = NULL, multiple = FALSE) ),
                                                       column(width=3, selectInput(ns("effectTypeToUse"), "Effect type(s) to use", choices = NULL, multiple = TRUE) ),
                                                       column(width=3, selectInput(ns("entryTypeToUse"), "Entry type(s) to use", choices = NULL, multiple = TRUE) ),
                                                ),
                                                column(width=12),
                                                shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                    column(width=12,
                                                                           hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                           h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameter values to be specified in the grey boxes above.", style="color:green"))),
                                                                           hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                    ),
                                                                    tags$span(id = ns('holder2'),
                                                                              selectInput(ns("trait3Rgg2"), "Trait to visualize regression over years.", choices = NULL, multiple = FALSE),
                                                                              plotly::plotlyOutput(ns("plotPredictionsCleanOut2")),
                                                                    ),
                                                ),
                                       ),
                                       tabPanel("Run analysis", icon = icon("dice-four"),
                                                br(),
                                                column(width=12,style = "background-color:grey; color: #FFFFFF",
                                                       column(width=3, tags$div(textInput(ns("analysisIdName"), label = tags$span(
                                                         "Analysis Name (optional)", tags$i( class = "glyphicon glyphicon-info-sign", style = "color:#FFFFFF",
                                                                                             title = "An optional name for the analysis besides the timestamp if desired.") ), #width = "100%",
                                                         placeholder = "(optional name)") ) ),
                                                       column(width=2,
                                                              br(),
                                                              actionButton(ns("runRgg"), "Run analysis", icon = icon("play-circle")),
                                                              uiOutput(ns("qaQcRggInfo")),
                                                       ),
                                                       column(width=7,
                                                              br(),
                                                              column(width=12, #style = "background-color:grey; color: #FFFFFF",
                                                                     shinydashboard::box(width = 12, style = "color: #000000",status = "success",solidHeader=FALSE,collapsible = TRUE, collapsed = TRUE, title = "Additional run settings (optional)...",
                                                                                         tags$span(id = ns('mackayOptions'),
                                                                                                   numericInput(ns("deregressWeight"), label = "Deregression weight", value = 1),
                                                                                                   numericInput(ns("propTopIndsPerYear"), label = "Top proportion individuals to keep per year of origin", value = 1, min = 0.1, max = 1, step = 0.1),
                                                                                                   selectInput(ns("partition"), "Partitioned regression", choices = list(TRUE,FALSE), selected = FALSE, multiple=FALSE),
                                                                                         ),
                                                                                         tags$span(id = ns('piephoOptions'),
                                                                                                   numericInput(ns("sampleN"), label = "Number of entries per environment to sample", value = 50),
                                                                                                   numericInput(ns("bootstrappingN"), label = "Number of bootstrapping per samples", value = 10),
                                                                                         ),
                                                                                         selectInput(ns("verbose"), label = "Print logs?", choices = list(TRUE,FALSE), selected = FALSE, multiple=FALSE),
                                                                                         selectInput(ns("forceRules"), label = "#Years rule applied?", choices = list(TRUE,FALSE), selected = TRUE, multiple=FALSE)
                                                                     ),
                                                              ),
                                                       ),
                                                       br(),
                                                ),
                                                textOutput(ns("outRgg")),
                                       ),
                                     )
                            ),
                            tabPanel(div(icon("arrow-right-from-bracket"), "Output tabs" ) , value = "outputTabs",
                                     tabsetPanel(
                                       tabPanel("Dashboard", icon = icon("file-image"),
                                                br(),
                                                textOutput(ns("outRgg2")),
                                                br(),
                                                downloadButton(ns("downloadReportRgg"), "Download dashboard"),
                                                br(),
                                                uiOutput(ns('reportRgg'))
                                       ),
                                       tabPanel("Metrics", icon = icon("table"),
                                                br(),
                                                DT::DTOutput(ns("metricsRgg")),
                                       ),
                                       tabPanel("Modeling", icon = icon("table"),
                                                br(),
                                                DT::DTOutput(ns("modelingRgg")),
                                       ),

                                     )
                            )# end of output panel
               )) # end mainpanel


  )
}

#' rggApp Server Functions
#'
#' @noRd
mod_rggApp_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # output$plotDataDependencies <- shiny::renderPlot({ dependencyPlot() })
    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })
    #
    observeEvent(
      input$methodRgg,
      if(length(input$methodRgg) > 0){ # added
        if (input$methodRgg == 'piepho') {
          golem::invoke_js('showid', ns('piephoOptions'))
          golem::invoke_js('hideid', ns('mackayOptions'))
        } else if (input$methodRgg == 'mackay') {
          golem::invoke_js('showid', ns('mackayOptions'))
          golem::invoke_js('hideid', ns('piephoOptions'))
        }
      }
    )
    ############################################################################
    # show shinyWidgets until the user can use the module
    observeEvent(c(data(), input$version2Rgg ), {
      req(data())
      mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
      if(mappedColumns == 3 & length(input$version2Rgg)>0  ){
        golem::invoke_js('showid', ns('holder1'))
        golem::invoke_js('showid', ns('holder2'))
      }else{
        golem::invoke_js('hideid', ns('holder1'))
        golem::invoke_js('hideid', ns('holder2'))
      }
    })
    ############################################################################
    output$warningMessage <- renderUI(
      if(is.null(data())){
        HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your phenotypic data using the 'Data Retrieval' tab, compute the 'environment' column, map the 'designation' and at least one trait.")) )
      }else{ # data is there
        mappedColumns <- setdiff(data()$metadata$pedigree[data()$metadata$pedigree$parameter == "yearOfOrigin","value"],"")
        # mappedColumns <- length(which(c("yearOfOrigin") %in% colnames(data()$medata$pedigree)))
        if(length(mappedColumns) == 1){
          if(any(c("mtaLmms","mta","mtaFlex") %in% data()$status$module)){
            myYearOfOrigin <- data()$metadata$pedigree[data()$metadata$pedigree$parameter=="yearOfOrigin","value"]
            if(!is.null(myYearOfOrigin) & !is.na(myYearOfOrigin)){
              HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to perform the realized genetic gain specifying your input parameters under the Input tabs.")) )
            }else{
              HTML( as.character(div(style="color: red; font-size: 20px;", "Please make sure to map the column 'yearOfOrigin' in the 'Pedigree data' extraction section under the 'Data Retrieval' to perform the realized genetic gain analysis.")) )
            }
          }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please perform a Multi-Trial Analysis before performing a realized genetic gain analysis.")) ) }
        }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please make sure that the column: 'yearOfOrigin' has been mapped in the pedigree data using the 'Data Retrieval' tab.")) )}
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
        utils::data(DT_example, package = "cgiarPipeline")
        if(!is.null(result$data)){tmp$data <- result$data}
        if(!is.null(result$metadata)){tmp$metadata <- result$metadata}
        if(!is.null(result$modifications)){tmp$modifications <- result$modifications}
        if(!is.null(result$predictions)){tmp$predictions <- result$predictions}
        if(!is.null(result$metrics)){tmp$metrics <- result$metrics}
        if(!is.null(result$modeling)){tmp$modeling <- result$modeling}
        if(!is.null(result$status)){tmp$status <- result$status}
        data(tmp) # update data with results
        shinybusy::remove_modal_spinner()
      }else{
        shinyWidgets::updatePrettySwitch(session, "launch", value = FALSE)
      }
    }, ignoreNULL = TRUE)
    #################
    ## version
    observeEvent(c(data(), input$methodRgg), {
      req(data())
      req(input$methodRgg)
      dtRgg <- data()
      dtRgg <- dtRgg$status
      if(input$methodRgg == "piepho"){
        dtRgg <- dtRgg[which(dtRgg$module %in% c("sta")),]
      }else if(input$methodRgg == "mackay"){
        dtRgg <- dtRgg[which(dtRgg$module %in% c("mta","mtaLmms","indexD")),]
      }
      traitsRgg <- unique(dtRgg$analysisId)
      if(length(traitsRgg) > 0){
        if("analysisIdName" %in% colnames(dtRgg)){
          names(traitsRgg) <- paste(dtRgg$analysisIdName, as.POSIXct(traitsRgg, origin="1970-01-01", tz="GMT"), sep = "_")
        }else{
          names(traitsRgg) <- as.POSIXct(traitsRgg, origin="1970-01-01", tz="GMT")
        }
      }
      updateSelectInput(session, "version2Rgg", choices = traitsRgg)
    })
    #################
    ## traits
    observeEvent(c(data(), input$version2Rgg), {
      req(data())
      req(input$version2Rgg)
      dtRgg <- data()
      dtRgg <- dtRgg$predictions
      dtRgg <- dtRgg[which(dtRgg$analysisId == input$version2Rgg),]
      #
      if(!is.null(data()$data$pedigree)){
        myYears <- data()$data$pedigree
        paramsPed <- data()$metadata$pedigree
        colnames(myYears) <- cgiarBase::replaceValues(colnames(myYears), Search = paramsPed$value, Replace = paramsPed$parameter )
        if( length(which(colnames(myYears) %in% "yearOfOrigin")) > 0){
          dtRgg <- merge(dtRgg, myYears[,c("designation","yearOfOrigin")], by="designation", all.x=TRUE)
          traitsRgg <- unique(dtRgg$trait)
          updateSelectInput(session, "trait2Rgg", choices = traitsRgg)
        }
      }
    })
    ##############
    ## years
    observeEvent(c(data(), input$version2Rgg, input$trait2Rgg), {
      req(data())
      req(input$version2Rgg)
      req(input$trait2Rgg)
      dtRgg <- data()
      dtRgg <- dtRgg$predictions
      dtRgg <- dtRgg[which(dtRgg$analysisId == input$version2Rgg),]
      if(!is.null(data()$data$pedigree)){
        myYears <- data()$data$pedigree
        paramsPed <- data()$metadata$pedigree
        colnames(myYears) <- cgiarBase::replaceValues(colnames(myYears), Search = paramsPed$value, Replace = paramsPed$parameter )
        if( length(which(colnames(myYears) %in% "yearOfOrigin")) > 0){
          dtRgg <- merge(dtRgg, myYears[,c("designation","yearOfOrigin")], by="designation", all.x=TRUE)
          traitsRgg <- sort(unique(dtRgg$yearOfOrigin), decreasing = FALSE)
          updateSelectInput(session, "yearsToUse", choices = traitsRgg, selected =traitsRgg )
        }
      }
    })
    ## environment type
    observeEvent(c(data(), input$version2Rgg, input$trait2Rgg, input$yearsToUse), {
      req(data())
      req(input$version2Rgg)
      req(input$trait2Rgg)
      req(input$yearsToUse)
      dtRgg <- data()
      dtRgg <- dtRgg$predictions
      dtRgg <- dtRgg[which(dtRgg$analysisId == input$version2Rgg),]
      #
      if(!is.null(data()$data$pedigree)){
        myYears <- data()$data$pedigree
        paramsPed <- data()$metadata$pedigree
        colnames(myYears) <- cgiarBase::replaceValues(colnames(myYears), Search = paramsPed$value, Replace = paramsPed$parameter )
        if( length(which(colnames(myYears) %in% "yearOfOrigin")) > 0){
          dtRgg <- merge(dtRgg, myYears[,c("designation","yearOfOrigin")], by="designation", all.x=TRUE)
          dtRgg <- dtRgg[which(dtRgg$trait == input$trait2Rgg),]
          dtRgg <- dtRgg[which(dtRgg$year == input$yearsToUse),]
          traitsRgg <- unique(dtRgg$environment)
          updateSelectInput(session, "environmentToUse", choices = traitsRgg, selected =traitsRgg[1] )
        }
      }
    })
    ## effect type
    observeEvent(c(data(), input$version2Rgg, input$trait2Rgg, input$yearsToUse, input$environmentToUse), {
      req(data())
      req(input$version2Rgg)
      req(input$trait2Rgg)
      req(input$yearsToUse)
      req(input$environmentToUse)
      dtRgg <- data()
      dtRgg <- dtRgg$predictions
      dtRgg <- dtRgg[which(dtRgg$analysisId == input$version2Rgg),]
      #
      if(!is.null(data()$data$pedigree)){
        myYears <- data()$data$pedigree
        paramsPed <- data()$metadata$pedigree
        colnames(myYears) <- cgiarBase::replaceValues(colnames(myYears), Search = paramsPed$value, Replace = paramsPed$parameter )
        if( length(which(colnames(myYears) %in% "yearOfOrigin")) > 0){
          dtRgg <- merge(dtRgg, myYears[,c("designation","yearOfOrigin")], by="designation", all.x=TRUE)
          dtRgg <- dtRgg[which(dtRgg$trait == input$trait2Rgg),]
          dtRgg <- dtRgg[which(dtRgg$year == input$yearsToUse),]
          dtRgg <- dtRgg[which(dtRgg$environment == input$environmentToUse),]
          traitsRgg <- unique(dtRgg$effectType)
          updateSelectInput(session, "effectTypeToUse", choices = traitsRgg, selected =traitsRgg )
        }
      }
    })
    ## entry type
    observeEvent(c(data(), input$version2Rgg, input$trait2Rgg, input$yearsToUse, input$environmentToUse, input$effectTypeToUse), {
      req(data())
      req(input$version2Rgg)
      req(input$trait2Rgg)
      req(input$yearsToUse)
      req(input$environmentToUse)
      req(input$effectTypeToUse)
      dtRgg <- data()
      dtRgg <- dtRgg$predictions#
      dtRgg <- dtRgg[which(dtRgg$analysisId == input$version2Rgg),]
      if(!is.null(data()$data$pedigree)){
        myYears <- data()$data$pedigree
        paramsPed <- data()$metadata$pedigree
        colnames(myYears) <- cgiarBase::replaceValues(colnames(myYears), Search = paramsPed$value, Replace = paramsPed$parameter )
        if( length(which(colnames(myYears) %in% "yearOfOrigin")) > 0){
          dtRgg <- merge(dtRgg, myYears[,c("designation","yearOfOrigin")], by="designation", all.x=TRUE)
          dtRgg <- dtRgg[which(dtRgg$trait == input$trait2Rgg),]
          dtRgg <- dtRgg[which(dtRgg$year == input$yearsToUse),]
          dtRgg <- dtRgg[which(dtRgg$environment == input$environmentToUse),]
          dtRgg <- dtRgg[which(dtRgg$effectType == input$effectTypeToUse),]
          traitsRgg <- unique(dtRgg$entryType)
          updateSelectInput(session, "entryTypeToUse", choices = traitsRgg, selected =traitsRgg )
        }
      }
    })
    ##############################################################################################
    ##############################################################################################
    ##############################################################################################
    ##  render plots
    observeEvent(c(data(),input$version2Rgg), { # update trait
      req(data())
      req(input$version2Rgg)
      dtRgg <- data()
      dtRgg <- dtRgg$predictions
      dtRgg <- dtRgg[which(dtRgg$analysisId %in% input$version2Rgg),] # only traits that have been QA
      traitRggInput <- unique(dtRgg$trait)
      updateSelectInput(session, "trait3Rgg", choices = traitRggInput)
      updateSelectInput(session, "trait3Rgg2", choices = traitRggInput)
    })
    output$plotPredictionsCleanOut <- plotly::renderPlotly({ # update plot
      req(data())
      req(input$version2Rgg)
      req(input$trait3Rgg)
      ##
      mydata <- data()$predictions
      paramsPheno <- data()$metadata$pheno
      paramsPheno <- paramsPheno[which(paramsPheno$parameter != "trait"),]
      colnames(mydata) <- cgiarBase::replaceValues(colnames(mydata), Search = paramsPheno$value, Replace = paramsPheno$parameter )
      ##
      if(!is.null(data()$data$pedigree)){

        myYears <- data()$data$pedigree
        paramsPed <- data()$metadata$pedigree
        colnames(myYears) <- cgiarBase::replaceValues(colnames(myYears), Search = paramsPed$value, Replace = paramsPed$parameter )

        if( length(which(colnames(myYears) %in% "yearOfOrigin")) > 0){
          mydata <- merge(mydata, myYears[,c("designation","yearOfOrigin")], by="designation", all.x=TRUE)
          mydata <- mydata[which(mydata[,"trait"] %in% input$trait3Rgg),]
          mydata <- mydata[which(mydata[,"environment"] %in% input$environmentToUse),]
          mydata <- mydata[which(mydata$effectType %in% input$effectTypeToUse),]
          mydata <- mydata[which(mydata$entryType %in% input$entryTypeToUse),]
          mydata[, "environment"] <- as.factor(mydata[, "environment"]); mydata[, "designation"] <- as.factor(mydata[, "designation"])

          res <- ggplot2::ggplot(mydata, ggplot2::aes(x=yearOfOrigin, y=predictedValue, color=effectType)) +
            ggplot2::geom_point() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1)) +
            ggplot2::ggtitle("View of current analyses available") + ggplot2::facet_grid(~ entryType)
          plotly::ggplotly(res)
        }

      }

    })
    output$plotPredictionsCleanOut2 <- plotly::renderPlotly({ # update plot
      req(data())
      req(input$trait3Rgg2)
      ##
      mydata <- data()$predictions
      paramsPheno <- data()$metadata$pheno
      paramsPheno <- paramsPheno[which(paramsPheno$parameter != "trait"),]
      colnames(mydata) <- cgiarBase::replaceValues(colnames(mydata), Search = paramsPheno$value, Replace = paramsPheno$parameter )
      ##

      if(!is.null(data()$data$pedigree)){

        myYears <- data()$data$pedigree
        paramsPed <- data()$metadata$pedigree
        colnames(myYears) <- cgiarBase::replaceValues(colnames(myYears), Search = paramsPed$value, Replace = paramsPed$parameter )

        if( length(which(colnames(myYears) %in% "yearOfOrigin")) > 0){
          mydata <- merge(mydata, myYears[,c("designation","yearOfOrigin")], by="designation", all.x=TRUE)
          mydata <- mydata[which(mydata[,"trait"] %in% input$trait3Rgg2),]
          mydata <- mydata[which(mydata[,"environment"] %in% input$environmentToUse),]
          mydata <- mydata[which(mydata$effectType %in% input$effectTypeToUse),]
          mydata <- mydata[which(mydata$entryType %in% input$entryTypeToUse),]
          mydata[, "environment"] <- as.factor(mydata[, "environment"]); mydata[, "designation"] <- as.factor(mydata[, "designation"])

          if(length(input$yearsToUse) > 0){
            mydata <- mydata[which(mydata$yearOfOrigin %in% input$yearsToUse),]
          }
          res <- ggplot2::ggplot(mydata, ggplot2::aes(x=yearOfOrigin, y=predictedValue, color=effectType)) +
            ggplot2::geom_point() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1)) +
            ggplot2::ggtitle("View of current analyses available") + ggplot2::facet_grid(~ entryType)
          plotly::ggplotly(res)
        }

      }

    })
    ## render timestamps flow
    output$plotTimeStamps <- shiny::renderPlot({
      req(data()) # req(input$version2Sta)
      xx <- data()$status;  yy <- data()$modeling # xx <- result$status;  yy <- result$modeling
      if("analysisIdName" %in% colnames(xx)){existNames=TRUE}else{existNames=FALSE}
      if(existNames){
        networkNames <- paste(xx$analysisIdName, as.character(as.POSIXct(as.numeric(xx$analysisId), origin="1970-01-01", tz="GMT")),sep = "_" )
        xx$analysisIdName <- as.character(as.POSIXct(as.numeric(xx$analysisId), origin="1970-01-01", tz="GMT"))
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
        rownames(X) <- networkNames
        colnames(X) <- networkNames
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
    ## render the data to be analyzed
    output$phenoRgg <-  DT::renderDT({
      req(data())
      req(input$version2Rgg)
      dtRgg <- data()
      dtRgg <- dtRgg$predictions
      dtRgg <- dtRgg[which(dtRgg$analysisId == input$version2Rgg),setdiff(colnames(dtRgg),c("module","analysisId"))]
      numeric.output <- c("predictedValue", "stdError", "reliability")
      DT::formatRound(DT::datatable(dtRgg, extensions = 'Buttons',
                                    options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                   lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All'))),
                                    caption = htmltools::tags$caption(
                                      style = 'color:cadetblue', #caption-side: bottom; text-align: center;
                                      htmltools::em('Predictions table.')
                                    )
      ), numeric.output)
    }, server = FALSE)
    ##############################################################################################
    ##############################################################################################
    ##############################################################################################
    ##  actual run
    ## render result of "run" button click

    my_rgg <- ExtendedTask$new(function(input, data) {
      promises::future_promise({

        ### pseudo code to select top n (e.g., 5) lines per yearOfOrigin #########################
        # tmp <- data$predictions[data$predictions$module == "mtaLmms" &
        #                         data$predictions$effectType == "designation", ]
        #
        # tmp <- merge(tmp, data$data$pedigree[, c("Geno", "yearOfOrigin")],
        #              by.x = "designation", by.y = 1)
        #
        # top_geno <- tmp %>%
        #   group_by(yearOfOrigin) %>%
        #   slice_max(order_by = predictedValue, n = 5) %>%
        #   select(yearOfOrigin, designation, predictedValue)
        #
        # data$predictions <- data$predictions[!(data$predictions$module == "mtaLmms" &
        #                                        data$predictions$effectType == "designation" &
        #                                        !(data$predictions$designation %in% top_geno$designation)),]
        # rm(tmp, top_geno)
        ##########################################################################################

        # some long process
        if(input$methodRgg == "piepho"){
          result <- try(cgiarPipeline::rggPiepho(
            phenoDTfile= data,
            analysisId=input$version2Rgg,
            trait=input$trait2Rgg, # per trait
            entryTypeToUse = input$entryTypeToUse,
            effectTypeToUse = input$effectTypeToUse,
            yearsToUse=input$yearsToUse,
            sampleN = input$sampleN,
            bootstrappingN = input$bootstrappingN,
            verbose=input$verbose,
            forceRules = input$forceRules
          ),
          silent=TRUE
          )
        }else if(input$methodRgg == "mackay"){
          result <- try(cgiarPipeline::rggMackay(
            phenoDTfile= data,
            analysisId=input$version2Rgg,
            trait=input$trait2Rgg, # per trait
            entryTypeToUse = input$entryTypeToUse,
            effectTypeToUse = input$effectTypeToUse,
            deregressWeight=input$deregressWeight,
            partition=input$partition,
            yearsToUse=input$yearsToUse,
            verbose=input$verbose,
            forceRules = input$forceRules,
            propTopIndsPerYear = input$propTopIndsPerYear
          ),
          silent=TRUE
          )
        }
        return(result)
      })
    })

    observeEvent(input$runRgg, {
      req(data())
      req(input$methodRgg)
      req(input$version2Rgg)
      req(input$trait2Rgg)
      req(input$yearsToUse)
      shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')

      ui_inputs <- shiny::reactiveValuesToList(input)
      data_obj  <- data()

      my_rgg$invoke(ui_inputs, data_obj)
    })

    output$outRgg <- output$outRgg2 <- renderPrint({
      # run the modeling, but before test if mta was done
      if(sum(data()$status$module %in% c("mta","mtaLmms","indexD")) == 0) {
        output$qaQcRggInfo <- renderUI({
          if (hideAll$clearAll){
            return()
          }else{
            req(data())
            HTML(as.character(div(style="color: brown;",
                                  "Please perform Multi-Trial-Analysis or Selection Index before conducting Optimal Cross Selection."))
            )
          }
        })
      }else{
        output$qaQcRggInfo <- renderUI({return(NULL)})
        result <- my_rgg$result()
        shinybusy::remove_modal_spinner()
        if(!inherits(result,"try-error")) {
          if("analysisIdName" %in% colnames(result$status)){result$status$analysisIdName[nrow(result$status)] <- input$analysisIdName}
          data(result) # update data with results
          # save(result, file = "./R/outputs/resultRgg.RData")
          cat(paste("Realized genetic gain step with id:",as.POSIXct( result$status$analysisId[length(result$status$analysisId)], origin="1970-01-01", tz="GMT"),"saved."))
          updateTabsetPanel(session, "tabsMain", selected = "outputTabs")

          # view metrics
          output$metricsRgg <-  DT::renderDT({
            metrics <- result$metrics
            metrics <- metrics[metrics$module=="rgg",]
            metrics$analysisId <- as.numeric(metrics$analysisId)
            metrics <- metrics[!is.na(metrics$analysisId),]
            current.metrics <- metrics[metrics$analysisId==max(metrics$analysisId),]
            current.metrics <- subset(current.metrics, select = -c(module,analysisId))
            numeric.output <- c("value", "stdError")
            DT::formatRound(DT::datatable(current.metrics, extensions = 'Buttons',
                                          options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                         lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
            ), numeric.output)
          }, server = FALSE)
          # view modeling
          output$modelingRgg <-  DT::renderDT({
            modeling <- result$modeling
            modeling <- modeling[modeling$module=="rgg",]
            modeling$analysisId <- as.numeric(modeling$analysisId)
            modeling <- modeling[!is.na(modeling$analysisId),]
            current.modeling <- modeling[modeling$analysisId==max(modeling$analysisId),]
            current.modeling <- subset(current.modeling, select = -c(module,analysisId))
            DT::datatable(current.modeling, extensions = 'Buttons',
                          options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                         lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
            )
          }, server = FALSE)
          ## Report tab
          output$reportRgg <- renderUI({
            HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportRgg.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
          })

          output$downloadReportRgg <- downloadHandler(
            filename = function() {
              paste(paste0('rgg_dashboard_',gsub("-", "", as.integer(Sys.time()))), sep = '.', switch(
                "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
              ))
            },
            content = function(file) {
              shinybusy::show_modal_spinner(spin = "fading-circle", text = "Generating Report...")

              src <- normalizePath(system.file("rmd","reportRgg.Rmd",package="bioflow"))
              src2 <- normalizePath('data/resultRgg.RData')

              # temporarily switch to the temp dir, in case you do not have write
              # permission to the current working directory
              owd <- setwd(tempdir())
              on.exit(setwd(owd))

              file.copy(src, 'report.Rmd', overwrite = TRUE)
              file.copy(src2, 'resultRgg.RData', overwrite = TRUE)

              out <- rmarkdown::render('report.Rmd', params = list(toDownload=TRUE),switch(
                "HTML",
                HTML = rmdformats::robobook(toc_depth = 4)
                # HTML = rmarkdown::html_document()
              ))

              # wait for it to land on disk (safety‐net)
              wait.time <- 0
              while (!file.exists(out) && wait.time < 60) {
                Sys.sleep(1); wait.time <- wait.time + 1
              }

              file.rename(out, file)
              shinybusy::remove_modal_spinner()
            }
          )
        }else{
          cat(paste("Analysis failed with the following error message: \n\n",result[[1]]))
          output$predictionsRgg <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
          output$metricsRgg <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
          output$modelingRgg <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
        }
        hideAll$clearAll <- FALSE

      }
    })


  })
}

## To be copied in the UI
# mod_rggApp_ui("rggApp_1")

## To be copied in the server
# mod_rggApp_server("rggApp_1")

#' gwasqkApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_gwasqkApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    tags$br(),

    mainPanel(width = 12,
              tabsetPanel( id=ns("tabsMain"),
                           type = "tabs",
                           tabPanel( div(icon("book"), "Information") ,
                                     br(),
                                     # shinydashboard::box(status="success",width = 12, solidHeader = TRUE,
                                     #                     column(width=12,   style = "height:580px; overflow-y: scroll;overflow-x: scroll;",
                                     column(width = 6,
                                            h1(strong(span("Genome wide association", tags$a(href="https://www.youtube.com/watch?v=LTiN7G6zCtM&list=PLZ0lafzH_UmclOPifjCntlMzysEB2_2wX&index=9", icon("youtube") , target="_blank"), style="color:darkcyan"))),
                                            h2(strong("Data Status (wait to be displayed):")),
                                            uiOutput(ns("warningMessage")),
                                            tags$br(),
                                            shinyWidgets::prettySwitch( inputId = ns('launch'), label = "Load example", status = "success"),
                                            tags$br(),
                                            img(src = "www/gwas2.jpg", height = 430, width = 470), # add an image
                                     ),
                                     # column(width = 6, shiny::plotOutput(ns("plotDataDependencies")), ),
                                     column(width = 6,
                                            h2(strong("Details")),
                                            p("The Genome-Wide Association Studies (GWAS) is a popular model that have helped efforts to dissect causal biological mechanisms underlying various agronomically important traits.
                                The way the options are used is the following:"),
                                            #p(strong("Genetic evaluation unit.-")," One or more of the following; designation, mother, father to indicate which column(s) should be considered the unit of genetic evaluation to compute BLUEs or BLUPs in the single trial analysis step."),
                                            p(strong("Traits to analyze.-")," Traits to be analyzed (from STA and MTA modules)."),
                                            p(strong("Markers to analyze.-")," Markers to be analyzed (from filtered markers)."),
                                            p(strong("Environments to analyze.-")," Differents environments to be considered (across for MTA)."),
                                            p(strong("Additional settings.-")),
                                            p(strong("Model to use.-")," Whether rrBLUP or gBLUP should be considered."),
                                            p(strong("Print logs.-")," Whether the logs of the run should be printed in the screen or not."),

                                            h2(strong("References")),
                                            p("Kinghorn, B. (1999). 19. Mate Selection for the tactical implementation of breeding programs. Proceedings of the Advancement of Animal Breeding and Genetics, 13, 130-133."),
                                            p("https://alphagenes.roslin.ed.ac.uk/wp/wp-content/uploads/2019/05/01_OptimalContributionSelection.pdf?x44213"),
                                            p("Woolliams, J. A., Berg, P., Dagnachew, B. S., & Meuwissen, T. H. E. (2015). Genetic contributions and their optimization. Journal of Animal Breeding and Genetics, 132(2), 89-99."),
                                            p("Cano-Gamez, E., & Trynka, G. (2020). From GWAS to function: using functional genomics to identify the mechanisms underlying complex diseases. Frontiers in genetics, 11, 424."),
                                            h2(strong("Software used")),
                                            p("R Core Team (2024). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
                                Vienna, Austria. URL https://www.R-project.org/."),
                                            p("Boer M, van Rossum B (2022). _LMMsolver: Linear Mixed Model Solver_. R package version 1.0.4.9000."),
                                            p("Covarrubias-Pazaran G. 2016. Genome assisted prediction of quantitative traits using the R package sommer. PLoS ONE 11(6):1-15."),

                                     ),
                                     #                     )
                                     # )
                           ),
                           tabPanel(div(icon("arrow-right-to-bracket"), "Input steps"),
                                    tabsetPanel(
                                      tabPanel("Pick STA or MTA-stamp(s)", icon = icon("dice-one"),
                                               br(),
                                               column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                      column(width=3, selectInput(ns("version2Gwas"), "STA/MTA version(s) to analyze", choices = NULL, multiple = TRUE)),
                                                      column(width=3, selectInput(ns("versionMarker2Gwas"), "Marker QA version(s) to use", choices = NULL, multiple = TRUE)),
                                                      column(width=3, selectInput(ns("env2Gwas"), "Environment(s) to use", choices = NULL, multiple = TRUE)),
                                                      # column(width=3, tags$span(id = ns('isSta'), selectInput(ns("env2Gwas"), "Environment to use (required)", choices = NULL, multiple = TRUE), ),  ),
                                                      # column(width=3, tags$br(),
                                                      #        shinyWidgets::prettySwitch( inputId = ns('launch'), label = "Load example", status = "success"),
                                                      # ),
                                               ),
                                               column(width=12),
                                               shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                   column(width=12,
                                                                          hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                          h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameter values to be specified in the grey boxes above.", style="color:green"))),
                                                                          hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                   ),
                                                                   column( width=4, DT::DTOutput(ns("tableTraitTimeStamps")), br(),br(),  ),
                                                                   column( width=8, shiny::plotOutput(ns("plotTimeStamps")), br(),br(), ),
                                                                   DT::DTOutput(ns("statusGwas")),
                                                                   DT::DTOutput(ns("phenoGwas")),
                                                                   DT::DTOutput(ns("genoGwas"))
                                               ),
                                      ),
                                      tabPanel("Select trait(s)", icon = icon("dice-two"),
                                               br(),
                                               column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                      column(width=4, selectInput(ns("trait2Gwas"), "Trait(s) to analyze", choices = NULL, multiple = TRUE) ),
                                               ),
                                               column(width=12),
                                               shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                   column(width=12,
                                                                          hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                          h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameter values to be specified in the grey boxes above.", style="color:green"))),
                                                                          hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                   ),
                                                                   tags$span(id = ns('holder1'),
                                                                             column(width=3, selectInput(ns("trait3Gwas"), "Trait to visualize", choices = NULL, multiple = TRUE) ),
                                                                             # column(width=3, numericInput(ns("transparency"), "Plot transparency", value=0.6, min=0, max=1, step=0.1)),
                                                                             column(width=3, selectInput(ns("env3Gwas"), "Environment to visualize", choices = NULL, multiple = TRUE) ),
                                                                             # column(width=3, selectInput(ns("env3Gwas"), "Environment to visualize", choices = NULL, multiple = TRUE) ),
                                                                             column(width=3, checkboxInput(ns("checkboxMarker"), label = strong(span("Show Genetic marker information")), value = FALSE) ),
                                                                             # column(width=2, checkboxInput(ns("checkbox"), label = "Include x-axis labels", value = TRUE) ),
                                                                             column(width=12, plotly::plotlyOutput(ns("barplotPredictionsMetrics"))),
                                                                             br(),
                                                                             column(width=12, plotly::plotlyOutput(ns("plotPredictionsCleanOutMarker")) ),
                                                                   ),),
                                               # column(width=12, style = "background-color:grey; color: #FFFFFF",
                                               #        column(width=4, selectInput(ns("trait2Gwas"), "Trait(s) to analyze (required)", choices = NULL, multiple = TRUE) ),
                                               # ),
                                               # column(width=12,
                                               #        hr(style = "border-top: 3px solid #4c4c4c;"),
                                               #        h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameter values to be specified in the grey boxes above.", style="color:green"))),
                                               #        hr(style = "border-top: 3px solid #4c4c4c;"),
                                               # ),
                                               # tags$span(id = ns('holder1'),
                                               #           column(width=3, selectInput(ns("trait3Gwas"), "Trait to visualize", choices = NULL, multiple = TRUE) ),
                                               #           # column(width=3, numericInput(ns("transparency"), "Plot transparency", value=0.6, min=0, max=1, step=0.1)),
                                               #           column(width=3, selectInput(ns("env3Gwas"), "Environment to visualize", choices = NULL, multiple = TRUE) ),
                                               #           # column(width=3, selectInput(ns("env3Gwas"), "Environment to visualize", choices = NULL, multiple = TRUE) ),
                                               #           column(width=3, checkboxInput(ns("checkboxMarker"), label = strong(span("Show Genetic marker information")), value = FALSE) ),
                                               #           # column(width=2, checkboxInput(ns("checkbox"), label = "Include x-axis labels", value = TRUE) ),
                                               #           column(width=12, plotly::plotlyOutput(ns("barplotPredictionsMetrics")))
                                               # ),
                                               # br(),
                                               # column(width=12, plotly::plotlyOutput(ns("plotPredictionsCleanOutMarker")) ),
                                      ),
                                      tabPanel("Run analysis", icon = icon("play"),
                                               br(),
                                               column(width=12,style = "background-color:grey; color: #FFFFFF",
                                                      column(width=3, tags$div(textInput(ns("analysisIdName"), label = tags$span(
                                                        "Analysis Name (optional)", tags$i( class = "glyphicon glyphicon-info-sign", style = "color:#FFFFFF",
                                                                                            title = "An optional name for the analysis besides the timestamp if desired.") ), #width = "100%",
                                                        placeholder = "(optional name)") ) ),
                                                      column(width = 2,
                                                              br(),
                                                              actionButton(ns("runGwas"), "Run analysis", icon = icon("play-circle")),
                                                              uiOutput(ns("qaQcGwasInfo")),
                                                      ),
                                                      br(),
                                                      column(width = 7,
                                                              shinydashboard::box(width = 12, status = "success", solidHeader=FALSE, style = "color: #000000",collapsible = TRUE, collapsed = TRUE, title = "Additional model settings...",
                                                                                  selectInput(ns("modelGwas"), label = "Model to use", choices = list("rrBLUP","gBLUP"), selected = "gBLUP", multiple=FALSE),
                                                                                  selectInput(ns("threshold"), label = "Threshold for significant marker", choices = list("Li and Ji (2005)"), selected = "Li and Ji (2005)", multiple=FALSE),
                                                                                  selectInput(ns("verboseGwas"), label = "Print logs?", choices = c(TRUE,FALSE), selected = FALSE, multiple=FALSE)
                                                              ),
                                                      ),
                                                      br(),
                                               ),
                                               textOutput(ns("outGwas")),
                                      ),
                                    )
                           ),
                           tabPanel(div(icon("arrow-right-from-bracket"), "Output tabs" ) , value = "outputTabs",
                                    tabsetPanel(
                                      tabPanel("Dashboard", icon = icon("file-image"),
                                               br(),
                                               # div(tags$p("Please download the report below:") ),
                                               downloadButton(ns("downloadReportGwas"), "Download dashboard"),
                                               br(),
                                               uiOutput(ns('reportGwas'))
                                      ),
                                      tabPanel("Predictions", icon = icon("table"),
                                               br(),
                                               # shinydashboard::box(status="success",width = 12, solidHeader = TRUE,
                                               # column(width=12,
                                               DT::DTOutput(ns("predictionsGwas")),
                                               # style = "height:530px; overflow-y: scroll;overflow-x: scroll;")
                                               # )
                                      ),
                                      tabPanel("Metrics", icon = icon("table"),
                                               br(),
                                               # shinydashboard::box(status="success",width = 12, solidHeader = TRUE,
                                               # column(width=12,br(),
                                               DT::DTOutput(ns("metricsGwas")),
                                               # style = "height:530px; overflow-y: scroll;overflow-x: scroll;")
                                               # )
                                      ),
                                      tabPanel("Modeling", icon = icon("table"),
                                               br(),
                                               # shinydashboard::box(status="success",width = 12, solidHeader = TRUE,
                                               # column(width=12,
                                               DT::DTOutput(ns("modelingGwas")),
                                               # style = "height:530px; overflow-y: scroll;overflow-x: scroll;")
                                               # )
                                      )
                                    )
                           )
              )) # end mainpanel


  )
}



#' gwasqkApp Server Functions
#'
#' @noRd
mod_gwasqkApp_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$plotDataDependencies <- shiny::renderPlot({ dependencyPlot() })
    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })
    ############################################################################
    # show shinyWidgets until the user can use the module
    observeEvent(c(data(), input$version2Gwas ), {
      req(data())
      mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
      if(mappedColumns == 3 & length(input$version2Gwas)>0 ){
        golem::invoke_js('showid', ns('holder1'))
      }else{
        golem::invoke_js('hideid', ns('holder1'))
      }
    })
    ############################################################################
    # warning message
    output$warningMessage <- renderUI(
      if(is.null(data())){
        HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your phenotypic data using the 'Data Retrieval' tab.")) )
      }else{ # data is there
        mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
        if(mappedColumns == 3){
          if("mtaLmms" %in% data()$status$module){
            if("qaGeno" %in% data()$status$module){ # user has markers
              HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to perform the genome wide association study (GWAS) analysis specifying your input parameters under the Input tabs.")) )
            }else{
              HTML( as.character(div(style="color: red; font-size: 20px;", "Please make sure that you have markers (and QA the data) to run this module.")) )
            }
          }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please perform a Multi-Trial Analysis before performing genome wide association study (GWAS).")) ) }
        }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please make sure that you have computed the 'environment' column, and that column 'designation' and \n at least one trait have been mapped using the 'Data Retrieval' tab.")) )}
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
        data(cgiarBase::create_getData_object())
        tmp <- data()
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
    observeEvent(
      input$version2Gwas,
      if(!is.null(data())) {
        dtGwas <- data()
        dtGwas <- dtGwas$status
        dtGwas <- dtGwas[which(dtGwas$module %in% c("mtaLmms")),]
        dtGwas <- dtGwas[length(dtGwas)]
        if (input$version2Gwas %in% dtGwas) {
          golem::invoke_js('hideid', ns('isSta'))
          #golem::invoke_js('hideid', ns('isStaEnv'))
        } else {
          golem::invoke_js('showid', ns('isSta'))
          #golem::invoke_js('showid', ns('isStaEnv'))
        }
      }
    )
    #################
    ## environments
    observeEvent(c(data(), input$version2Gwas), {
      req(data())
      req(input$version2Gwas)
      dtGwas <- data()
      dtGwas <- dtGwas$predictions
      dtGwas <- dtGwas[which(dtGwas$analysisId %in% input$version2Gwas),]
      envGwas <- unique(dtGwas$environment)
      updateSelectInput(session, "env2Gwas", choices = envGwas, selected = NULL)
    })

    #################
    ## version
    observeEvent(c(data()), {
      req(data())
      dtGwas <- data()
      dtGwas <- dtGwas$status
      dtGwas <- dtGwas[which(dtGwas$module %in% c("sta","mtaLmms")),]
      traitsGwas <- unique(dtGwas$analysisId)
      if(length(traitsGwas) > 0){names(traitsGwas) <- as.POSIXct(traitsGwas, origin="1970-01-01", tz="GMT")}
      updateSelectInput(session, "version2Gwas", choices = traitsGwas)
    })
    # ## version qa marker
    observeEvent(c(data()), {
      req(data())
      dtGwas <- data()
      dtGwas <- dtGwas$status
      dtGwas <- dtGwas[which(dtGwas$module == "qaGeno"),]
      traitsGwas <- unique(dtGwas$analysisId)
      if(length(traitsGwas) > 0){names(traitsGwas) <- as.POSIXct(traitsGwas, origin="1970-01-01", tz="GMT")}
      updateSelectInput(session, "versionMarker2Gwas", choices = traitsGwas)
    })
    #################
    ## traits
    observeEvent(c(data(), input$version2Gwas), {
      req(data())
      req(input$version2Gwas)
      dtGwas <- data()
      dtGwas <- dtGwas$predictions
      dtGwas <- dtGwas[which(dtGwas$analysisId == input$version2Gwas),]
      traitsGwas <- unique(dtGwas$trait)
      updateSelectInput(session, "trait2Gwas", choices = traitsGwas, selected = NULL)
    })

    ##############################################################################################
    ## render trait distribution plot
    observeEvent(c(data(),input$version2Gwas), { # update trait
      req(data())
      req(input$version2Gwas)
      dtGwas <- data()
      dtGwas <- dtGwas$predictions
      dtGwas <- dtGwas[which(dtGwas$analysisId %in% input$version2Gwas),] # only traits that have been QA
      traitGwasInput <- unique(dtGwas$trait)
      updateSelectInput(session, "trait3Gwas", choices = traitGwasInput)
    })

    observeEvent(c(data(), input$version2Gwas), {
      req(data())
      req(input$version2Gwas)
      dtGwas <- data()
      dtGwas <- dtGwas$predictions
      dtGwas <- dtGwas[which(dtGwas$analysisId == input$version2Gwas),]
      envGwasInput <- unique(dtGwas$environment)
      updateSelectInput(session, "env3Gwas", choices = envGwasInput, selected = NULL)
    })

    output$barplotPredictionsMetrics <- plotly::renderPlotly({ # update plot
      req(data())
      req(input$version2Gwas)
      # req(input$env3Gwas)
      req(input$trait3Gwas)
      # req(input$transparency)
      # req(input$font)
      mydata <- data()
      mydata <- mydata$predictions

      envGwasSel <- data()
      envGwasSel <- envGwasSel$status
      envGwasSel <- envGwasSel[which(envGwasSel$module %in% c("sta")),]
      envGwasSel <- envGwasSel[length(envGwasSel)]

      idGwas <- data()$status[which(data()$status$module %in% "mtaLmms"),"analysisId"]
      idGwas <- idGwas[length(idGwas)]

      mydata <- mydata[which(mydata[,"trait"] %in% input$trait3Gwas),]

      if(input$version2Gwas %in% envGwasSel){
        req(input$env3Gwas)
        mydata <- mydata[which(mydata[,"environment"] %in% input$env3Gwas),]
      }else{mydata <- mydata[which(mydata[,"analysisId"] %in% idGwas),]}

      mydata <- mydata[,which(!duplicated(colnames(mydata)))]
      mydata$label <- with(mydata,paste0(environment,", ",trait))
      p <- ggplot2::ggplot(mydata, ggplot2::aes(x=predictedValue, fill=trait, color=trait)) +
        ggplot2::geom_histogram(aes(y=..density..), position="identity", alpha=.6) + #  alpha=input$transparency
        ggplot2::geom_density(alpha=.3) + ggplot2::theme_bw()+
        ggplot2::xlab("Predicted values") + ggplot2::ylab("Trait density") + ggplot2::theme(legend.position='none')

      if(input$version2Gwas %in% envGwasSel){
        p <- p + facet_wrap(. ~ label, scales='free') # environment ~ trait ~ label
      }else{
        p <- p + facet_wrap( ~ trait, scales='free')
      }

      plotly::ggplotly(p)

    })

    ## render the expected result for markers
    output$plotPredictionsCleanOutMarker <- plotly::renderPlotly({
      req(data())
      req(input$versionMarker2Gwas)
      dtGwas <- data()$modeling
      dtGwas <- dtGwas[which(dtGwas$analysisId %in% input$versionMarker2Gwas),]
      dtGwas$value <- as.numeric(dtGwas$value)
      propNaUpperThreshForMarker <- dtGwas[dtGwas$parameter=="propNaUpperThreshForMarker", "value"]
      propNaUpperThreshForInds <- dtGwas[dtGwas$parameter=="propNaUpperThreshForInds", "value"]
      maf <- dtGwas[dtGwas$parameter=="maf", "value"]
      propHetUpperThreshForMarker <- dtGwas[dtGwas$parameter=="propHetUpperThreshForMarker", "value"]
      propFisUpperThreshForMarker <- dtGwas[dtGwas$parameter=="propFisUpperThreshForMarker", "value"]
      ploidy <- dtGwas[dtGwas$parameter=="ploidy", "value"]
      mydata <- data()$data$geno
      ##
      if(!is.null(mydata)){
        fig <- plotly::plot_ly(alpha = 0.6)
        ## historgram for marker with missing data
        propNaMarker <- apply(mydata,2,function(x){(length(which(is.na(x)))/length(x))})
        fig <- fig %>% plotly::add_histogram(x = propNaMarker, histnorm = "probability", name="Missing data in markers" )
        ## historgram for individuals with missing data
        propNaIndividual <- apply(mydata,1,function(x){(length(which(is.na(x)))/length(x))})
        fig <- fig %>% plotly::add_histogram(x = propNaIndividual, histnorm = "probability", name="Missing data in individuals" )
        ## historgram for MAF
        MAF <- apply(mydata, 2, function(x) {AF <- mean(x, na.rm = T)/ploidy;MAF <- ifelse(AF > 0.5, 1 - AF, AF) })
        fig <- fig %>% plotly::add_histogram(x = MAF, histnorm = "probability", name="Minor allele frequency" )
        ## histogram for heterozigosity
        q <- MAF; p <- 1 - q
        he <- 2 * p * q;  ho <- colMeans((mydata) == 1, na.rm = TRUE) # Get the obseved heterozygosity.
        fig <- fig %>% plotly::add_histogram(x = ho, histnorm = "probability", name="Observed heterozygosity" )
        ## histogram for inbreeding
        Fis <- ifelse(he == 0, yes = 0, no = 1 - (ho / he))
        fig <- fig %>% plotly::add_histogram(x = Fis, histnorm = "probability", name="Inbreeding" )
        ## add threshold lines and layout
        vline1 <- function(x = 0, color = "blue") {list( type = "line",y0 = 0,y1 = 1,yref = "paper",x0 = x, x1 = x,line = list(color = color, dash="dot"))}
        vline2 <- function(x = 0, color = "red") {list( type = "line",y0 = 0,y1 = 1,yref = "paper",x0 = x, x1 = x,line = list(color = color, dash="dot"))}
        vline3 <- function(x = 0, color = "green") {list( type = "line",y0 = 0,y1 = 1,yref = "paper",x0 = x, x1 = x,line = list(color = color, dash="dot"))}
        fig <- fig %>% plotly::layout(barmode = "overlay", xaxis = list(title = "Value of parameter"), yaxis = list(title = "Proportion of markers" ),
                                      title="Preview of the proportion of markers or individuals tagged for the different QA parameters",
                                      shapes = list(vline1(propNaUpperThreshForMarker),vline2(propNaUpperThreshForInds),vline3(maf) ) )
        # fig
      }else{
        fig = plotly::plot_ly()
        fig = fig %>% plotly::add_annotations(text = "Genetic marker information not available.", x = 1, y = 1)#
        # fig
      }

      if(input$checkboxMarker){fig}else{}

    })

    ## render the table of traits and analysisID being selected
    output$tableTraitTimeStamps <-  DT::renderDT({
      req(data())
      # req(input$version2Mta)
      ### change column names for mapping
      if(length(input$version2Gwas) > 0){
        status <- data()$status
        modeling <- data()$modeling
        statusPlusModel <- merge(status, unique(modeling[,c("analysisId","trait")]), by="analysisId", all.x = TRUE)
        '%!in%' <- function(x,y)!('%in%'(x,y))
        statusPlusModel <- statusPlusModel[which(statusPlusModel$trait %!in% c("inputObject",NA)),]
        statusPlusModel <- statusPlusModel[which(statusPlusModel$analysisId %in% input$version2Gwas),]
        statusPlusModel$analysisId <- as.POSIXct(statusPlusModel$analysisId, origin="1970-01-01", tz="GMT")
      }else{
        statusPlusModel <- data.frame(analysisId=NA, module=NA, trait=NA)
      }
      DT::datatable(statusPlusModel, extensions = 'Buttons', # I changed Blfrtip to lfrtip and silenced the buttons
                    options = list(dom = 'lfrtip',scrollX = TRUE, #buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   lengthMenu = list(c(8,20,50,-1), c(8,20,50,'All'))),
                    caption = htmltools::tags$caption(
                      style = 'color:cadetblue', #caption-side: bottom; text-align: center;
                      htmltools::em('Traits available in the STA or MTA ID selected.')
                    )
      )
    })
    ## render the data to be analyzed
    output$phenoGwas <-  DT::renderDT({
      req(data())
      req(input$version2Gwas)
      dtGwas <- data()
      dtGwas <- dtGwas$predictions
      dtGwas <- dtGwas[which(dtGwas$analysisId %in% input$version2Gwas),setdiff(colnames(dtGwas),c("module","analysisId"))]

      envGwasSel <- data()
      envGwasSel <- envGwasSel$status
      envGwasSel <- envGwasSel[which(envGwasSel$module %in% c("mtaLmms")),]
      envGwasSel <- envGwasSel[length(envGwasSel)]
      if(input$version2Gwas %in% envGwasSel){
        dtGwas <- dtGwas
      }else{dtGwas <- dtGwas[which(dtGwas$environment %in% input$env2Gwas),]}
      numeric.output <- c("predictedValue", "stdError", "reliability")
      DT::formatRound(DT::datatable(dtGwas, extensions = 'Buttons',
                                    options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                   lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All'))),
                                    caption = htmltools::tags$caption(
                                      style = 'color:cadetblue', #caption-side: bottom; text-align: center;
                                      htmltools::em('STA or MTA predictions table to be used as input.')
                                    )
      ), numeric.output)
    })
    ## display the current geno modification table

    output$genoGwas <-  DT::renderDT({
      req(data())
      req(input$versionMarker2Gwas)
      if(!is.null(input$versionMarker2Gwas)){
        mo <- data()$modifications$geno
      }else{
        mo <- data.frame(warningText="Genetic marker data not available")
      }
      DT::datatable(mo, extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All'))),
                    caption = htmltools::tags$caption(
                      style = 'color:cadetblue', #caption-side: bottom; text-align: center;
                      htmltools::em('Preview of potential modifications to add to marker data.')
                    )
      )

    })

    ## render timestamps flow
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
          if(nLevelsCheck1 == 1){
            X1 <- matrix(ifelse(is.na(zz$inputId),0,1),nrow=length(zz$inputId),1); colnames(X1) <- as.character(na.omit(unique(c(zz$outputId))))
          }else{X1 <- model.matrix(~as.factor(outputId)-1, data=zz); colnames(X1) <- levels(as.factor(zz$outputId))}
          if(nLevelsCheck2 == 1){
            X2 <- matrix(ifelse(is.na(zz$inputId),0,1),nrow=length(zz$inputId),1); colnames(X2) <- as.character(na.omit(unique(c(zz$inputId))))
          }else{X2 <- model.matrix(~as.factor(inputId)-1, data=zz); colnames(X2) <- levels(as.factor(zz$inputId))}
          mynames <- unique(na.omit(c(zz$outputId,zz$inputId)))
          X <- matrix(0, nrow=nrow(zz), ncol=length(mynames)); colnames(X) <- as.character(mynames)
          X[,colnames(X1)] <- X1
          X[,colnames(X2)] <- X2
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
        library(ggnetwork)
        ggplot2::ggplot(n, ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
          ggnetwork::geom_edges(ggplot2::aes(color = family), arrow = ggplot2::arrow(length = ggnetwork::unit(6, "pt"), type = "closed") ) +
          ggnetwork::geom_nodes(ggplot2::aes(color = family), alpha = 0.5, size=5 ) + ggplot2::ggtitle("Network plot of current analyses available") +
          ggnetwork::geom_nodelabel_repel(ggplot2::aes(color = family, label = vertex.names ),
                                          fontface = "bold", box.padding = ggnetwork::unit(1, "lines")) +
          ggnetwork::theme_blank()
      }
    })
    ## render modeling
    # output$statusGwas <-  DT::renderDT({
    #   req(data())
    #   req(input$version2Gwas)
    #   dtGwas <- data() # dtSta<- result
    #   ### change column names for mapping
    #   paramsPheno <- data()$modeling
    #   paramsPheno <- paramsPheno[which(paramsPheno$analysisId %in% input$version2Gwas),, drop=FALSE]
    #   paramsPheno$analysisId <- as.POSIXct(paramsPheno$analysisId, origin="1970-01-01", tz="GMT")
    #   DT::datatable(paramsPheno, extensions = 'Buttons',
    #                 options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
    #                                lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All'))),
    #                 caption = htmltools::tags$caption(
    #                   style = 'color:cadetblue', #caption-side: bottom; text-align: center;
    #                   htmltools::em('Past modeling parameters from MTA stamp(s) selected.')
    #                 )
    #   )
    # })

    ## render result of "run" button click
    outGwas <- eventReactive(input$runGwas, {
      req(data())
      req(input$version2Gwas)
      req(input$trait2Gwas)
      req(input$versionMarker2Gwas)
      req(input$env2Gwas)
      req(input$modelGwas)
      req(input$verboseGwas)
      shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
      dtGwas <- data()
      # dt2Gwas <- dtGwas$predictions
      # dt2Gwas <- dt2Gwas[which(dt2Gwas$module %in% "mta"),]
      # run the modeling, but before test if mta was done
      if(sum(dtGwas$status$module %in% c("mtaLmms")) == 0) {
        output$qaQcGwasInfo <- renderUI({
          if (hideAll$clearAll){
            return()
          }else{
            req(dtGwas)
            HTML(as.character(div(style="color: brown;",
                                  "Please perform Single/Multi-Trial-Analysis before conducting Genome wide association."))
            )
          }
        })
      }else{ # sta or mta is available
        output$qaQcGwasInfo <- renderUI({return(NULL)})
        # if(input$modelGwas %in% c("gBLUP","rrBLUP") ){ # warning
        if(input$versionMarker2Gwas == '' | is.null(input$versionMarker2Gwas) ){ # user didn't provide a modifications id
          if(!is.null(dtGwas$data$geno)){ # if user actually has marker data
            shinybusy::remove_modal_spinner()
            cat("Please run the 'Markers QA/QC' module prior to run a GWAS model.")
            # stop("Please run the 'Markers QA/QC' module prior to run a gBLUP or rrBLUP model.", call. = FALSE)
          }else{ # if user does NOT have marker data and wanted a marker-based model
            shinybusy::remove_modal_spinner()
            cat("GWAS requires marker information. Alternatively, go back to the 'Retrieve Data' section and upload your marker data.")
            # stop("Please pick a different model, rrBLUP, gBLUP and ssBLUP require marker information. Alternatively, go back to the 'Retrieve Data' section and upload your marker data.")
          }
        }else{ markerVersionToUse <- input$versionMarker2Gwas} # there is a versionMarker2Mta id
        # }else{ markerVersionToUse <- NULL } # for non marker based model we don't need to provide this

        # envGwasSel <- data()
        # envGwasSel <- envGwasSel$status
        # envGwasSel <- envGwasSel[which(envGwasSel$module %in% c("sta")),]
        # envGwasSel <- envGwasSel[length(envGwasSel)]
        #
        # if(input$version2Gwas %in% envGwasSel) {
        #   field <- input$env2Gwas
        # }else{field <- "across"}

        mod <- data()$status[which(data()$status$analysisId == input$version2Gwas),"module"]
        if("sta" %in% mod){field <- input$env2Gwas}else{field <- "across"}

        result <- try(cgiarPipeline::gwas(
          phenoDTfile = dtGwas, # analysis to be picked from predictions database
          analysisId = input$version2Gwas, # analysis ID
          analysisIdForGenoModifications = markerVersionToUse, # marker modifications
          modelGwas = input$modelGwas, # model type
          trait = input$trait2Gwas, # per trait
          field = field, # per env
          # verbose = input$verboseGwas,
          maxIters = 50,
          tolParInv = 1e-4
        ),
        silent=TRUE
        )

        # }

        if(!inherits(result,"try-error")) {
          data(result) # update data with results
          cat(paste("Genome wide association study step with id:",as.POSIXct( result$status$analysisId[length(result$status$analysisId)], origin="1970-01-01", tz="GMT"),"saved."))
          updateTabsetPanel(session, "tabsMain", selected = "outputTabs")
        }else{
          cat(paste("Analysis failed with the following error message: \n\n",result[[1]]))
        }
      }
      shinybusy::remove_modal_spinner()

      if(!inherits(result,"try-error")) {
        # view predictions
        output$predictionsGwas <-  DT::renderDT({
          # if ( hideAll$clearAll){
          #   return()
          # }else{
          predictions <- result$predictions
          predictions <- predictions[predictions$module=="gwas",]
          idGwas <- predictions$analysisId
          idGwas <- idGwas[length(idGwas)]
          current.predictions <- predictions[predictions$analysisId==idGwas,]
          colnames(current.predictions)[11:13] <- c("-log10(P)", "CHR", "BP")
          current.predictions <- subset(current.predictions, select = -c(module,analysisId))
          numeric.output <- "-log10(P)"
          DT::formatRound(DT::datatable(current.predictions, extensions = 'Buttons',
                                        options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                       lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
          ), numeric.output, digits = 5)
          # }
        })
        # view metrics
        output$metricsGwas <-  DT::renderDT({
          # if ( hideAll$clearAll){
          #   return()
          # }else{
          metrics <- result$metrics
          metrics <- metrics[metrics$module=="gwas",]
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
        })
        #   # view modeling
        output$modelingGwas <-  DT::renderDT({
          # if ( hideAll$clearAll){
          #   return()
          # }else{
          modeling <- result$modeling
          modeling <- modeling[modeling$module=="gwas",]
          modeling$analysisId <- as.numeric(modeling$analysisId)
          modeling <- modeling[!is.na(modeling$analysisId),]
          current.modeling <- modeling[modeling$analysisId==max(modeling$analysisId),]
          current.modeling <- subset(current.modeling, select = -c(module,analysisId))
          DT::datatable(current.modeling, extensions = 'Buttons',
                        options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                       lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
          )
          # }
        })
        ## Report tab
        output$reportGwas <- renderUI({
          HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportGwas.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
        })

        output$downloadReportGwas <- downloadHandler(
          filename = function() {
            paste(paste0('gwas',gsub("-", "", as.integer(Sys.time()))), sep = '.', switch(
              "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
            ))
          },
          content = function(file) {
            shinybusy::show_modal_spinner(spin = "fading-circle", text = "Generating Report...")

            src <- normalizePath(system.file("rmd","reportGwas.Rmd",package="bioflow"))
            src2 <- normalizePath('data/resultGwas.RData')

            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))

            file.copy(src, 'report.Rmd', overwrite = TRUE)
            file.copy(src2, 'resultGwas.RData', overwrite = TRUE)

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

      } else {
        output$predictionsGwas <- DT::renderDT({DT::datatable(NULL)})
        output$metricsGwas <- DT::renderDT({DT::datatable(NULL)})
        output$modelingGwas <- DT::renderDT({DT::datatable(NULL)})
        cat(paste("Analysis failed with the following error message: \n\n",result[[1]]))
      }


      # hideAll$clearAll <- FALSE

    }) ## end eventReactive

    output$outGwas <- renderPrint({
      outGwas()
    })

  })
}

## To be copied in the UI
# mod_gwasqkApp_ui("gwasqkApp_1")

## To be copied in the server
# mod_gwasqkApp_server("gwasqkApp_1")

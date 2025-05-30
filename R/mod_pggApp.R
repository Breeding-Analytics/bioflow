#' pggApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pggApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    tags$br(),

    mainPanel( width =12,
               tabsetPanel( id=ns("tabsMain"),
                            type = "tabs",
                            tabPanel( div(icon("book"), "Information") ,
                                      br(),
                                      column(width = 6,
                                             h1(strong(span("Predicted Genetic Gain Module", tags$a(href="https://www.youtube.com/watch?v=nc4_SddPjjo&list=PLZ0lafzH_UmclOPifjCntlMzysEB2_2wX&index=11", icon("youtube") , target="_blank"), style="color:darkcyan"))),
                                             h2(strong("Data Status (wait to be displayed):")),
                                             uiOutput(ns("warningMessage")),
                                             tags$br(),
                                             # column(width=4, tags$br(),
                                             shinyWidgets::prettySwitch( inputId = ns('launch'), label = "Load example dataset", status = "success"),
                                             # ),
                                             tags$br(),
                                             img(src = "www/pgg.png", height = 250, width = 500), # add an image
                                      ),
                                      column(width = 6,
                                             h2(strong("Details")),
                                             p("In order to monitor the efficacy of genetic evaluation in the current cycle of selection the predicted genetic gain formula is used.
                                          This option aims to calculate the predicted genetic gain from the classical breeders' equation
                              R = i*r*s being R the response to selection, i the selection intensity, r the selection accuracy, s the genetic standard deviation.
                                The way the options are used is the following:"),
                                             p(strong("Trait(s) to use.-")," Trait to be be used for calculating the predicted genetic gain parameters."),
                                             p(strong("Environment(s) to use.-")," Environments-data to be used from the input file."),
                                             p(strong("Proportion selected.-")," Proportion of parents to be selected in order to calculate the selection intensity."),
                                             h2(strong("References")),
                                             p("Lush, J. L. (2013). Animal breeding plans. Read Books Ltd."),
                                             p("Mrode, R. A. (2014). Linear models for the prediction of animal breeding values. Cabi."),
                                             h2(strong("Software used")),
                                             p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
                                Vienna, Austria. URL https://www.R-project.org/."),

                                             # column(width = 12, shiny::plotOutput(ns("plotDataDependencies")), ),
                                      ),

                            ),
                            tabPanel(div(icon("arrow-right-to-bracket"), "Input steps"),
                                     tabsetPanel(
                                       tabPanel(div( icon("dice-one"), "Pick Index-stamp", icon("arrow-right")  ), # icon = icon("dice-one"),
                                                br(),
                                                column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                       column(width=8, selectInput(ns("version2Pgg"), "STA version to analyze", choices = NULL, multiple = FALSE)),

                                                ),
                                                column(width=12),
                                                shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                    column(width=12,
                                                                           hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                           h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameter values to be specified in the grey boxes above.", style="color:green"))),
                                                                           hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                    ),
                                                                    column( width=12, shiny::plotOutput(ns("plotTimeStamps")) ),
                                                                    DT::DTOutput(ns("phenoPgg")),
                                                ),
                                       ),
                                       tabPanel( div( icon("dice-two"), "Select parameters(s)", icon("arrow-right") ) , # icon = icon("dice-two"),
                                                 br(),
                                                 column(width=12,  style = "background-color:grey; color: #FFFFFF",

                                                        column(width=3,
                                                               selectInput(ns("trait2Pgg"), "Trait(s) to use", choices = NULL, multiple = TRUE),
                                                        ),
                                                        column(width=3,
                                                               selectInput(ns("environmentToUse"), "Do analysis by:", choices = NULL, multiple = FALSE),
                                                        ),
                                                        column(width=3,
                                                               numericInput(ns("proportion"), label = "Assumed percentage selected (%)", value = 10, step = 10, max = 100, min = 1),
                                                        ),
                                                 ),
                                                 column(width=12),
                                                 shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                     column(width=12,
                                                                            hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                            h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameter values to be specified in the grey boxes above.", style="color:green"))),
                                                                            hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                     ),
                                                                     tags$span(id = ns('holder1'),
                                                                               column(width=6, selectInput(ns("traitMetrics"), "Trait to visualize", choices = NULL, multiple = TRUE) ) ,
                                                                               column(width=6, selectInput(ns("parameterMetrics"), "Parameter to visualize", choices = NULL, multiple = FALSE) ),
                                                                               column(width=12, plotly::plotlyOutput(ns("barplotPredictionsMetrics")) ),
                                                                     ),
                                                 ),
                                       ),
                                       tabPanel("Run analysis", icon = icon("dice-three"),
                                                br(),
                                                column(width=12,style = "background-color:grey; color: #FFFFFF",
                                                       column(width=3, tags$div(textInput(ns("analysisIdName"), label = tags$span(
                                                         "Analysis Name (optional)", tags$i( class = "glyphicon glyphicon-info-sign", style = "color:#FFFFFF",
                                                                                             title = "An optional name for the analysis besides the timestamp if desired.") ), #width = "100%",
                                                         placeholder = "(optional name)") ) ),
                                                       column(width=3,
                                                              br(),
                                                              actionButton(ns("runPgg"), "Run analysis", icon = icon("play-circle")),
                                                              uiOutput(ns("qaQcPggInfo")),
                                                              br(),
                                                       ),

                                                ),
                                                textOutput(ns("outPgg")),
                                       ),
                                     )
                            ),
                            tabPanel(div(icon("arrow-right-from-bracket"), "Output tabs" ) , value = "outputTabs",
                                     tabsetPanel(
                                       tabPanel("Dashboard", icon = icon("file-image"),
                                                br(),
                                                textOutput(ns("outPgg2")),
                                                br(),
                                                downloadButton(ns("downloadReportPgg"), "Download dashboard"),
                                                br(),
                                                uiOutput(ns('reportPgg'))
                                       ),
                                       tabPanel("Metrics", icon = icon("table"),
                                                br(),
                                                DT::DTOutput(ns("metricsPgg")),
                                       ),
                                       tabPanel("Modeling", icon = icon("table"),
                                                br(),
                                                DT::DTOutput(ns("modelingPgg")),
                                       ),

                                     )
                            )# end of output panel
               )) # end mainpanel

  )
}

#' pggApp Server Functions
#'
#' @noRd
mod_pggApp_server <- function(id, data){
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
    observeEvent(c(data(), input$version2Pgg ), {
      req(data())
      mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
      if(mappedColumns == 3 & length(input$version2Pgg)>0 ){
        golem::invoke_js('showid', ns('holder1'))
      }else{
        golem::invoke_js('hideid', ns('holder1'))
      }
    })
    ############################################################################
    ## warning message
    output$warningMessage <- renderUI(
      if(is.null(data())){
        HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your phenotypic data using the 'Data Retrieval' tab, compute the 'environment' column, map the 'designation' and at least one trait.")) )
      }else{ # data is there
        if( any(c("mtaLmms","mta","mtaFlex") %in% data()$status$module) ){
          HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to perform the predicted genetic gain analysis specifying your input parameters under the Input tabs.")) )
        }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please perform a Multi-Trial Analysis before performing a predicted genetic gain analysis.")) ) }
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
    observeEvent(c(data()), {
      req(data())
      dtPgg <- data()
      dtPgg <- dtPgg$status
      dtPgg <- dtPgg[which(dtPgg$module %in% c("sta")),]
      traitsPgg <- unique(dtPgg$analysisId)
      if(length(traitsPgg) > 0){
        if("analysisIdName" %in% colnames(dtPgg)){
          names(traitsPgg) <- paste(dtPgg$analysisIdName, as.POSIXct(traitsPgg, origin="1970-01-01", tz="GMT"), sep = "_")
        }else{
          names(traitsPgg) <- as.POSIXct(traitsPgg, origin="1970-01-01", tz="GMT")
        }
      }
      updateSelectInput(session, "version2Pgg", choices = traitsPgg)
    })
    #################
    ## traits
    observeEvent(c(data(), input$version2Pgg), {
      req(data())
      req(input$version2Pgg)
      dtPgg <- data()
      dtPgg <- dtPgg$predictions
      dtPgg <- dtPgg[which(dtPgg$analysisId == input$version2Pgg),]
      traitsPgg <- unique(dtPgg$trait)
      updateSelectInput(session, "trait2Pgg", choices = traitsPgg)
    })
    # trait for report tab
    observeEvent(c(data(), input$version2Pgg), {
      req(data())
      req(input$version2Pgg)
      dtPgg <- data()
      dtPgg <- dtPgg$predictions
      dtPgg <- dtPgg[which(dtPgg$analysisId == input$version2Pgg),]
      traitsPgg <- unique(dtPgg$trait)
      updateSelectInput(session, "traitFilterPredictions2D2", choices = traitsPgg)
    })
    # environment for report tab
    observeEvent(c(data(), input$version2Pgg), {
      req(data())
      req(input$version2Pgg)
      dtPgg <- data()
      dtPgg <- dtPgg$predictions
      dtPgg <- dtPgg[which(dtPgg$analysisId == input$version2Pgg),]
      traitsPgg <- unique(dtPgg$environment)
      updateSelectInput(session, "environment", choices = traitsPgg)
    })
    ##############
    ## entry type
    observeEvent(c(data(), input$version2Pgg, input$trait2Pgg), {
      req(data())
      req(input$version2Pgg)
      req(input$trait2Pgg)
      dtPgg <- data()
      metaPheno <- dtPgg$metadata$pheno
      traitsPgg <- setdiff(metaPheno$parameter[metaPheno$parameter != "trait"], c("rep","iBlock","row","col","designation","gid","entryType","stage","pipeline") )
      # dtPgg <- data()$predictions
      # dtPgg <- dtPgg[which(dtPgg$analysisId == input$version2Pgg),]
      # traitsPgg <- unique(dtPgg$environment)
      updateSelectInput(session, "environmentToUse", choices = traitsPgg, selected ="environment" )
    })
    ##############################################################################################
    ##############################################################################################
    ##############################################################################################
    ## render metrics barplot
    observeEvent(c(data(),input$version2Pgg), { # update trait
      req(data())
      req(input$version2Pgg)
      dtPgg <- data()
      dtPgg <- dtPgg$metrics
      dtPgg <- dtPgg[which(dtPgg$analysisId %in% input$version2Pgg),] # only traits that have been QA
      traitPggInput <- unique(dtPgg$trait)
      updateSelectInput(session, "traitMetrics", choices = traitPggInput, selected = traitPggInput)
    })
    observeEvent(c(data(),input$version2Pgg), { # update parameter
      req(data())
      req(input$version2Pgg)
      dtPgg <- data()
      dtPgg <- dtPgg$metrics
      dtPgg <- dtPgg[which(dtPgg$analysisId %in% input$version2Pgg),] # only traits that have been QA
      metricsPggInput <- unique(dtPgg$parameter)
      updateSelectInput(session, "parameterMetrics", choices = metricsPggInput)
    })
    output$barplotPredictionsMetrics <- plotly::renderPlotly({
      req(data())
      req(input$version2Pgg)
      dtPgg <- data()
      mydata <- dtPgg$metrics
      mydata <- mydata[which(mydata$analysisId %in% input$version2Pgg),]
      mydata = mydata[which(mydata$parameter %in% input$parameterMetrics),]
      mydata = mydata[which(mydata$trait %in% input$traitMetrics),]
      res <- ggplot2::ggplot(data=mydata, ggplot2::aes(x=environment, y=value, fill=trait)) +
        ggplot2::geom_bar(stat="identity", position=ggplot2::position_dodge()) +  ggplot2::ggtitle("Metrics associated to this stamp selected")
      plotly::ggplotly(res)
      # res = plotly::plot_ly(data = mydata, x = mydata[,"environment"], y = mydata[,"value"],
      #                       color=mydata[,"trait"]
      #                       # size=mydata[,input$sizeMetrics2D], text=mydata[,"environment"]
      # )   # , type="scatter", mode   = "markers")
      # res = res %>% plotly::add_bars()
      # res
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
    output$phenoPgg <-  DT::renderDT({
      req(data())
      req(input$version2Pgg)
      dtPgg <- data()
      dtPgg <- dtPgg$predictions
      current.predictions <- dtPgg[which(dtPgg$analysisId == input$version2Pgg),setdiff(colnames(dtPgg),c("module","analysisId"))]
      # current.predictions <- subset(dtPgg, select = -c(module,analysisId))
      numeric.output <- c("predictedValue", "stdError", "reliability")
      DT::formatRound(DT::datatable(current.predictions, extensions = 'Buttons',
                                    options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                   lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All'))),
                                    caption = htmltools::tags$caption(
                                      style = 'color:cadetblue', #caption-side: bottom; text-align: center;
                                      htmltools::em('Predictions table to be used as input.')
                                    )
      ), numeric.output)
    }, server = FALSE)
    ## render result of "run" button click
    outPgg <- eventReactive(input$runPgg, {
      req(data())
      req(input$version2Pgg)
      req(input$trait2Pgg)
      req(input$environmentToUse)
      shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
      dtPgg <- data()
      # run the modeling, but before test if mta was done
      if(sum(dtPgg$status$module %in% c("mta","mtaLmms","indexD")) == 0) {
        output$qaQcPggInfo <- renderUI({
          if (hideAll$clearAll){
            return()
          }else{
            req(dtPgg)
            HTML(as.character(div(style="color: brown;",
                                  "Please perform Multi-Trial-Analysis or Selection Index before conducting Optimal Cross Selection."))
            )
          }
        })
      }else{
        output$qaQcPggInfo <- renderUI({return(NULL)})
        result <- try(cgiarPipeline::pgg(
          phenoDTfile= dtPgg,
          analysisId=input$version2Pgg,
          trait=input$trait2Pgg, # per trait
          by=input$environmentToUse,
          percentage=input$proportion,
          verbose=FALSE
        ),
        silent=TRUE
        )
        if(!inherits(result,"try-error")) {
          if("analysisIdName" %in% colnames(result$status)){result$status$analysisIdName[nrow(result$status)] <- input$analysisIdName}
          data(result) # update data with results
          # save(result, file = "./R/outputs/resultPgg.RData")
          cat(paste("Predicted genetic gain step with id:",as.POSIXct( result$status$analysisId[length(result$status$analysisId)], origin="1970-01-01", tz="GMT") ,"saved."))
          updateTabsetPanel(session, "tabsMain", selected = "outputTabs")
        }else{
          cat(paste("Analysis failed with the following error message: \n\n",result[[1]]))
        }
      }
      shinybusy::remove_modal_spinner()

      if(!inherits(result,"try-error")) {
        # view metrics
        output$metricsPgg <-  DT::renderDT({
          # if ( hideAll$clearAll){
          #   return()
          # }else{
          metrics <- result$metrics
          metrics <- metrics[metrics$module=="pgg",]
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
        output$modelingPgg <-  DT::renderDT({
          # if ( hideAll$clearAll){
          #   return()
          # }else{
          modeling <- result$modeling
          modeling <- modeling[modeling$module=="pgg",]
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
        # Report tab
        output$reportPgg <- renderUI({
          HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportPgg.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
        })

        output$downloadReportPgg <- downloadHandler(
          filename = function() {
            paste(paste0('pgg_dashboard_',gsub("-", "", as.integer(Sys.time()))), sep = '.', switch(
              "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
            ))
          },
          content = function(file) {
            shinybusy::show_modal_spinner(spin = "fading-circle", text = "Generating Report...")

            src <- normalizePath(system.file("rmd","reportPgg.Rmd",package="bioflow"))
            src2 <- normalizePath('data/resultPgg.RData')

            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))

            file.copy(src, 'report.Rmd', overwrite = TRUE)
            file.copy(src2, 'resultPgg.RData', overwrite = TRUE)

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
        output$predictionsPgg <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
        output$metricsPgg <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
        output$modelingPgg <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
      }

      hideAll$clearAll <- FALSE

    }) ## end eventReactive

    output$outPgg <- output$outPgg2 <- renderPrint({
      outPgg()
    })


  })
}

## To be copied in the UI
# mod_pggApp_ui("pggApp_1")

## To be copied in the server
# mod_pggApp_server("pggApp_1")

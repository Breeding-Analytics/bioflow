#' ocsApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ocsApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    sidebarPanel(
      # input <- list(version2Ocs="1699508839.68847",trait2Ocs="desireIndex",entryType2Ocs= "TGV_EST004D#TEST_tested",nCrossOcs="20", targetAngleOcs="30",maxRun=40, relType="grm", env2Ocs="across",verboseOcs=TRUE )
      tags$style(".well {background-color:grey; color: #FFFFFF;}"),
      # div(tags$p( h4(strong("Optimal Cross Selection")))),#, style = "color: #817e7e"
      HTML("<img src='www/cgiar3.png' width='42' vspace='10' hspace='10' height='46' align='top'>
                  <font size='5'>Optimal Cross Selection</font>"),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      selectInput(ns("version2Ocs"), "Index or MTA version to analyze", choices = NULL, multiple = FALSE),
      selectInput(ns("trait2Ocs"), "Trait(s) to use", choices = NULL, multiple = FALSE),
      selectInput(ns("entryType2Ocs"), "Entry types to use", choices = NULL, multiple = TRUE),
      textInput(ns("nCrossOcs"), label = "Number of crosses [Enter a numeric vector (comma delimited): e.g: 20,30,40 ]", value="20"),
      textInput(ns("targetAngleOcs"), label = "Target angle [Enter a numeric vector (comma delimited): e.g: 30,60,90 ]", value="30"),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      shinydashboard::box(width = 12, status = "success", background="green",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Settings...",
                          numericInput(ns("numberBest"), label = "Maximum number of top individuals to use", value = 100),
                          numericInput(ns("maxRun"), label = "Stopping criteria (#of iterations without change)", value = 40),
                          selectInput(ns("relType"), "Relationship to use", choices = list(GRM="grm",NRM="nrm", BOTH="both"), multiple = FALSE),
                          selectInput(ns("env2Ocs"), "Environment to use", choices = NULL, multiple = FALSE),
                          selectInput(ns("verboseOcs"), label = "Print logs?", choices = list(TRUE,FALSE), selected = FALSE, multiple=FALSE)
      ),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      actionButton(ns("runOcs"), "Run", icon = icon("play-circle")),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      uiOutput(ns("qaQcOcsInfo")),
      textOutput(ns("outOcs"))
    ), # end sidebarpanel
    mainPanel(tabsetPanel(
      type = "tabs",
      tabPanel(p("Information",class="info-p"),  icon = icon("book"),
               br(),
               shinydashboard::box(status="success",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,   style = "height:800px; overflow-y: scroll;overflow-x: scroll;",
                                          h2(strong("Status:")),
                                          uiOutput(ns("warningMessage")),
                                          h2(strong("Details")),
                                          p("A new generation of individuals with higher genetic merit can be produced selecting the top individuals or
                                          selecting directly the best crosses. This option aims to optimize the new crosses given a desired trade-off between
                                          short-term gain(performance) and long-term gain (genetic variance).
                                The way the options are used is the following:"),
                                          img(src = "www/ocs.png", height = 400, width = 250), # add an image
                                          p(strong("Trait for cross prediction.-")," Trait to be be used for predicting all possible crosses (an index is suggested)."),
                                          p(strong("Environment to analyze.-")," Fieldinst level to be used from the input file."),
                                          p(strong("Number of crosses.-")," Number of crosses to be selected."),
                                          p(strong("Target angle.-")," Target angle defining the trade-off between performance and diversity."),
                                          p(strong("Stopping criteria.-")," Maximum number of runs (iterations) without change in the genetic algorithm."),
                                          p(strong("Notes.-"),"Consider that the predictions table in this particular case is different. In this case, the 'predictedValue' column refers to
                                the expected value of the cross, 'stdError' is the average inbreeding of the cross, and 'rel' has the genetic algorithm value (lower the better)."),
                                          h2(strong("References:")),
                                          p("Kinghorn, B. (1999). 19. Mate Selection for the tactical implementation of breeding programs. Proceedings of the Advancement of Animal Breeding and Genetics, 13, 130-133."),
                                          p("https://alphagenes.roslin.ed.ac.uk/wp/wp-content/uploads/2019/05/01_OptimalContributionSelection.pdf?x44213"),
                                          p("Woolliams, J. A., Berg, P., Dagnachew, B. S., & Meuwissen, T. H. E. (2015). Genetic contributions and their optimization. Journal of Animal Breeding and Genetics, 132(2), 89-99."),
                                          h2(strong("Software used:")),
                                          p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
                                Vienna, Austria. URL https://www.R-project.org/."),
                                          p("https://github.com/gaynorr/QuantGenResources")
                                   )
               )
      ),
      tabPanel(p("Input",class="input-p"), icon = icon("arrow-right-to-bracket"),
               tabsetPanel(
                 tabPanel("Trait distribution", icon = icon("magnifying-glass-chart"),
                          br(),
                          shinydashboard::box(status="success",width = 12, solidHeader = TRUE,
                                              column(width=5, selectInput(ns("trait3Ocs"), "Trait to visualize", choices = NULL, multiple = FALSE) ),
                                              column(width=5, selectInput(ns("groupOcsInputPlot"), "Group by", choices = c("environment","designation","entryType"), multiple = FALSE, selected = "entryType") ),
                                              column(width=2, numericInput(ns("fontSize"), label = "x-axis font size", value = 12, step=1)),
                                              column(width=12, plotly::plotlyOutput(ns("plotPredictionsCleanOut")))
                          )
                 ),
                 tabPanel("Mta-modeling", icon = icon("table"),
                          br(),
                          shinydashboard::box(status="success",width = 12,
                                              solidHeader = TRUE,
                                              column(width=12,DT::DTOutput(ns("statusOcs")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
                          )
                 ),
                 tabPanel("Data", icon = icon("table"),
                          br(),
                          shinydashboard::box(status="success",width = 12,
                                              solidHeader = TRUE,
                                              column(width=12,DT::DTOutput(ns("phenoOcs")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
                          )
                 )
               )
      ),
      tabPanel(p("Output",class="output-p"), icon = icon("arrow-right-from-bracket"),
               tabsetPanel(
                 tabPanel("Predictions", icon = icon("table"),
                          br(),
                          shinydashboard::box(status="success",width = 12,
                                              solidHeader = TRUE,
                                              column(width=12,DT::DTOutput(ns("predictionsOcs")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
                          )
                 ),
                 tabPanel("Metrics", icon = icon("table"),
                          br(),
                          shinydashboard::box(status="success",width = 12,
                                              solidHeader = TRUE,
                                              column(width=12,br(),DT::DTOutput(ns("metricsOcs")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
                          )
                 ),
                 tabPanel("Modeling", icon = icon("table"),
                          br(),
                          shinydashboard::box(status="success",width = 12,
                                              solidHeader = TRUE,
                                              column(width=12,DT::DTOutput(ns("modelingOcs")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
                          )
                 ),
                 tabPanel("Report", icon = icon("file-image"),
                          br(),
                          div(tags$p("Please download the report below:") ),
                          downloadButton(ns("downloadReportOcs"), "Download report"),
                          br(),
                          uiOutput(ns('reportOcs'))
                 )
               )
      )
    )) # end mainpanel


  )
}

#' ocsApp Server Functions
#'
#' @noRd
mod_ocsApp_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })
    ############################################################################
    # data = reactive({
    #   load("~/Documents/bioflow/dataStr0.RData")
    #   data <- res
    #   return(data)
    # })
    # warning message
    output$warningMessage <- renderUI(
      if(is.null(data())){
        HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your phenotypic data using the 'Data Retrieval' tab.")) )
      }else{ # data is there
        mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
        if(mappedColumns == 3){
          if("mta" %in% data()$status$module){
            HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to perform the optimal cross selection (OCS) inspecting the other tabs.")) )
          }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please perform a Multi-Trial Analysis or a selection index before performing optimal cross selection (OCS).")) ) }
        }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please make sure that the columns: 'environment', 'designation' and \n at least one trait have been mapped using the 'Data Retrieval' tab.")) )}
      }
    )
    #################
    ## version
    observeEvent(c(data()), {
      req(data())
      dtOcs <- data()
      dtOcs <- dtOcs$status
      dtOcs <- dtOcs[which(dtOcs$module %in% c("mta","indexD")),]
      traitsOcs <- unique(dtOcs$analysisId)
      if(length(traitsOcs) > 0){names(traitsOcs) <- as.POSIXct(traitsOcs, origin="1970-01-01", tz="GMT")}
      updateSelectInput(session, "version2Ocs", choices = traitsOcs)
    })
    #################
    ## traits
    observeEvent(c(data(), input$version2Ocs), {
      req(data())
      req(input$version2Ocs)
      dtOcs <- data()
      dtOcs <- dtOcs$predictions
      dtOcs <- dtOcs[which(dtOcs$analysisId == input$version2Ocs),]
      traitsOcs <- unique(dtOcs$trait)
      updateSelectInput(session, "trait2Ocs", choices = traitsOcs)
    })
    observeEvent(c(data(), input$version2Ocs), {
      req(data())
      req(input$version2Ocs)
      dtOcs <- data()
      dtOcs <- dtOcs$predictions
      dtOcs <- dtOcs[which(dtOcs$analysisId == input$version2Ocs),]
      traitsOcs <- unique(dtOcs$trait)
      updateSelectInput(session, "traitFilterPredictions2D2", choices = traitsOcs)
    })
    ##############
    # treatments
    observeEvent(c(data(), input$version2Ocs, input$nCrossOcs, input$targetAngleOcs, input$traitFilterPredictions2D2), {
      req(data())
      req(input$version2Ocs)
      req(input$traitFilterPredictions2D2)
      req(input$nCrossOcs)
      req(input$targetAngleOcs)
      a <- as.numeric(gsub(" ","",unlist(strsplit(input$nCrossOcs,","))))
      b <- as.numeric(gsub(" ","",unlist(strsplit(input$targetAngleOcs,","))))
      forLoop <- expand.grid(a, b)
      traitsOcs <- apply(forLoop,1, function(x){ paste(input$traitFilterPredictions2D2,"~", paste(x[1],"crosses *",x[2], "degrees"))})
      updateSelectInput(session, "environment", choices = traitsOcs)
    })
    ##############
    ## entry type
    observeEvent(c(data(), input$version2Ocs, input$trait2Ocs), {
      req(data())
      req(input$version2Ocs)
      req(input$trait2Ocs)
      dtOcs <- data()
      dtOcs <- dtOcs$predictions
      dtOcs <- dtOcs[which(dtOcs$analysisId == input$version2Ocs),]
      dtOcs <- dtOcs[which(dtOcs$trait == input$trait2Ocs),]
      traitsOcs <- unique(dtOcs$entryType)
      updateSelectInput(session, "entryType2Ocs", choices = traitsOcs)
    })
    ##############
    ## environment
    observeEvent(c(data(), input$version2Ocs, input$trait2Ocs), {
      req(data())
      req(input$version2Ocs)
      req(input$trait2Ocs)
      dtOcs <- data()
      dtOcs <- dtOcs$predictions
      dtOcs <- dtOcs[which(dtOcs$analysisId == input$version2Ocs),]
      dtOcs <- dtOcs[which(dtOcs$trait == input$trait2Ocs),]
      traitsOcs <- unique(dtOcs$environment)
      updateSelectInput(session, "env2Ocs", choices = traitsOcs)
    })

    ##############################################################################################
    ##############################################################################################
    ##############################################################################################
    ## render trait distribution plot
    observeEvent(c(data(),input$version2Ocs), { # update trait
      req(data())
      req(input$version2Ocs)
      dtOcs <- data()
      dtOcs <- dtOcs$predictions
      dtOcs <- dtOcs[which(dtOcs$analysisId %in% input$version2Ocs),] # only traits that have been QA
      traitOcsInput <- unique(dtOcs$trait)
      updateSelectInput(session, "trait3Ocs", choices = traitOcsInput)
    })
    output$plotPredictionsCleanOut <- plotly::renderPlotly({ # update plot
      req(data())
      req(input$trait3Ocs)
      req(input$fontSize)
      req(input$groupOcsInputPlot)
      mydata <- data()$predictions
      if(input$groupOcsInputPlot == "entryType"){mydata <- mydata[which(mydata$entryType %in% input$entryType2Ocs),]}
      mydata <- mydata[which(mydata[,"trait"] %in% input$trait3Ocs),]
      mydata[, "environment"] <- as.factor(mydata[, "environment"]); mydata[, "designation"] <- as.factor(mydata[, "designation"])
      res <- plotly::plot_ly(y = mydata[,"predictedValue"], type = "box", boxpoints = "all", jitter = 0.3, color = mydata[,input$groupOcsInputPlot],
                             x = mydata[,input$groupOcsInputPlot], text=mydata[,"designation"], pointpos = -1.8)
      res = res %>% plotly::layout(showlegend = TRUE,  xaxis = list(titlefont = list(size = input$fontSize), tickfont = list(size = input$fontSize)))
      res
    })
    ## render the data to be analyzed
    output$phenoOcs <-  DT::renderDT({
      req(data())
      req(input$version2Ocs)
      dtOcs <- data()
      dtOcs <- dtOcs$predictions
      dtOcs <- dtOcs[which(dtOcs$analysisId == input$version2Ocs),setdiff(colnames(dtOcs),c("module","analysisId"))]
      numeric.output <- c("predictedValue", "stdError", "reliability")
      DT::formatRound(DT::datatable(dtOcs, extensions = 'Buttons',
                                    options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                   lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
      ), numeric.output)
    })
    ## render modeling
    output$statusOcs <-  DT::renderDT({
      req(data())
      req(input$version2Ocs)
      dtSta <- data() # dtSta<- result
      ### change column names for mapping
      paramsPheno <- data()$modeling
      paramsPheno <- paramsPheno[which(paramsPheno$analysisId %in% input$version2Ocs),, drop=FALSE]
      paramsPheno$analysisId <- as.POSIXct(paramsPheno$analysisId, origin="1970-01-01", tz="GMT")
      DT::datatable(paramsPheno, extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
      )
    })
    ## render result of "run" button click
    outOcs <- eventReactive(input$runOcs, {
      req(data())
      req(input$version2Ocs)
      req(input$trait2Ocs)
      req(input$env2Ocs)
      shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
      dtOcs <- data()
      # run the modeling, but before test if mta was done
      if(sum(dtOcs$status$module %in% c("mta","indexD")) == 0) {
        output$qaQcOcsInfo <- renderUI({
          if (hideAll$clearAll){
            return()
          }else{
            req(dtOcs)
            HTML(as.character(div(style="color: brown;",
                                  "Please perform Multi-Trial-Analysis or Selection Index before conducting Optimal Cross Selection."))
            )
          }
        })
      }else{
        output$qaQcOcsInfo <- renderUI({return(NULL)})
        result <- try(cgiarPipeline::ocs(
          phenoDTfile= dtOcs, # analysis to be picked from predictions database
          analysisId=input$version2Ocs,
          relDTfile= input$relType,
          trait= input$trait2Ocs, # per trait
          environment=input$env2Ocs,
          nCross=as.numeric(gsub(" ","",unlist(strsplit(input$nCrossOcs,",")))),
          targetAngle=as.numeric(gsub(" ","",unlist(strsplit(input$targetAngleOcs,",")))), # in radians
          verbose=input$verboseOcs, maxRun = input$maxRun,
          entryType=input$entryType2Ocs,
          numberBest = input$numberBest
        ),
        silent=TRUE
        )
        if(!inherits(result,"try-error")) {
          data(result) # update data with results
          # save(result, file = "./R/outputs/resultOcs.RData")
          cat(paste("Optimal cross selection step with id:",as.POSIXct( result$status$analysisId[length(result$status$analysisId)], origin="1970-01-01", tz="GMT"),"saved."))
        }else{
          cat(paste("Analysis failed with the following error message: \n\n",result[[1]]))
        }
      }
      shinybusy::remove_modal_spinner()

      if(!inherits(result,"try-error")) {
        # view predictions
        output$predictionsOcs <-  DT::renderDT({
          # if ( hideAll$clearAll){
          #   return()
          # }else{
          predictions <- result$predictions
          predictions <- predictions[predictions$module=="ocs",]
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
        })
        # view metrics
        output$metricsOcs <-  DT::renderDT({
          # if ( hideAll$clearAll){
          #   return()
          # }else{
          metrics <- result$metrics
          metrics <- metrics[metrics$module=="ocs",]
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
        # view modeling
        output$modelingOcs <-  DT::renderDT({
          # if ( hideAll$clearAll){
          #   return()
          # }else{
          modeling <- result$modeling
          modeling <- modeling[modeling$module=="ocs",]
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
        output$reportOcs <- renderUI({
          HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportOcs.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
        })

      } else {
        output$predictionsOcs <- DT::renderDT({DT::datatable(NULL)})
        output$metricsOcs <- DT::renderDT({DT::datatable(NULL)})
        output$modelingOcs <- DT::renderDT({DT::datatable(NULL)})
      }


      hideAll$clearAll <- FALSE

    }) ## end eventReactive

    output$downloadReportOcs <- downloadHandler(
      filename = function() {
        paste('my-report', sep = '.', switch(
          "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
        ))
      },
      content = function(file) {
        src <- normalizePath(system.file("rmd","reportOcs.Rmd",package="bioflow"))
        src2 <- normalizePath('data/resultOcs.RData')
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'report.Rmd', overwrite = TRUE)
        file.copy(src2, 'resultOcs.RData', overwrite = TRUE)
        out <- rmarkdown::render('report.Rmd', params = list(toDownload=TRUE),switch(
          "HTML",
          HTML = rmarkdown::html_document()
        ))
        file.rename(out, file)
      }
    )

    output$outOcs <- renderPrint({
      outOcs()
    })

  })
}

## To be copied in the UI
# mod_ocsApp_ui("ocsApp_1")

## To be copied in the server
# mod_ocsApp_server("ocsApp_1")

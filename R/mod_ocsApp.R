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
      div(tags$p( h4(strong("Optimal Cross Selection")))),#, style = "color: #817e7e"
      hr(style = "border-top: 1px solid #4c4c4c;"),
      selectInput(ns("version2Ocs"), "Index or MTA version to analyze", choices = NULL, multiple = FALSE),
      selectInput(ns("trait2Ocs"), "Trait(s) to use", choices = NULL, multiple = FALSE),
      selectInput(ns("entryType2Ocs"), "Entry types to use", choices = NULL, multiple = TRUE),
      textInput(ns("nCrossOcs"), label = "Number of crosses [Enter a numeric vector (comma delimited): e.g: 20,30,40 ]", value="20"),
      textInput(ns("targetAngleOcs"), label = "Target angle [Enter a numeric vector (comma delimited): e.g: 30,60,90 ]", value="30"),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      shinydashboard::box(width = 12, status = "primary", background="light-blue",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Settings...",
                          numericInput(ns("maxRun"), label = "Stopping criteria (#of iterations without change)", value = 40),
                          selectInput(ns("relType"), "Relationship to use", choices = list(GRM="grm",NRM="nrm", BOTH="both"), multiple = FALSE),
                          selectInput(ns("env2Ocs"), "Environment to use", choices = NULL, multiple = FALSE),
                          selectInput(ns("verboseOcs"), label = "Print logs?", choices = list(TRUE,FALSE), selected = FALSE, multiple=FALSE)
      ),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      actionButton(ns("runOcs"), "Run", icon = icon("play-circle")),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      uiOutput(ns("qaQcOcsInfo")),
      shinycssloaders::withSpinner(textOutput(ns("outOcs")),type=8)
    ), # end sidebarpanel
    mainPanel(tabsetPanel(
      type = "tabs",

      tabPanel("Input Data",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,DT::DTOutput(ns("phenoOcs")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
               )
      ),
      tabPanel("Predictions",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,DT::DTOutput(ns("predictionsOcs")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
               )
      ),
      tabPanel("Metrics",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,br(),DT::DTOutput(ns("metricsOcs")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
               )
      ),
      tabPanel("Modeling",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,DT::DTOutput(ns("modelingOcs")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
               )
      ),
      tabPanel("Report",
               br(),
               shinydashboard::box(width = 6, status = "success",# background="light-blue",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Settings...",
                                   selectInput(ns("traitFilterPredictions2D2"), "Trait(s) to visualize", choices = NULL, multiple = FALSE),
                                   selectInput(ns("environment"), "Treatment to view", choices = NULL, multiple = FALSE)
               ),
               shinydashboard::box(width = 6, status = "success", background="light-blue",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Settings...",
                                   selectInput(ns("xAxisPredictions2D"),label = "X axis:", choices = c("designation","mother","father","entryType"),
                                               multiple=FALSE,selected = "designation"),
                                   selectInput(ns("yAxisPredictions2D"),label = "Y axis:",choices = c("predictedValue","stdError","reliability"),
                                               multiple=FALSE,selected = "predictedValue"),
                                   selectInput(ns("colorPredictions2D"),label = "Color by:", choices = c("mother","father","entryType"),
                                               multiple=FALSE, selected = "mother"),
                                   selectInput(ns("sizePredictions2D"),label = "Size by:",choices = c("predictedValue","stdError","reliability"),
                                               multiple=FALSE,selected = "stdError"),
                                   selectInput(ns("textPredictions2D"), label = "Text by:",choices = c("designation","mother","father","entryType"),
                                               multiple=FALSE,selected = "designation")
               ),
               shinydashboard::box(status="success",width = 12,# solidHeader = TRUE, title = "Desired change and expected response",
                                   plotly::plotlyOutput(ns("plotPredictionsScatter"))
               )
      ),
      tabPanel("Documentation",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,   style = "height:800px; overflow-y: scroll;overflow-x: scroll;",
                                          h1(strong("Details")),
                                          p("This option aims to predict breeding values for individuals having marker data by using a
                              training population obtained by the option to calculate marker effects.
                                The way the options are used is the following:"),
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
                                          h3(strong("Software used:")),
                                          p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
                                Vienna, Austria. URL https://www.R-project.org/."),
                                          p("https://github.com/gaynorr/QuantGenResources")
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
    #################
    ## version
    observeEvent(c(data()), {
      req(data())
      dtOcs <- data()
      dtOcs <- dtOcs$status
      dtOcs <- dtOcs[which(dtOcs$module %in% c("mta","indexD")),]
      traitsOcs <- unique(dtOcs$analysisId)
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
                                                   lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All')))
      ), numeric.output)
    })
    ## render result of "run" button click
    outOcs <- eventReactive(input$runOcs, {
      req(data())
      req(input$version2Ocs)
      req(input$trait2Ocs)
      req(input$env2Ocs)
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
          entryType=input$entryType2Ocs
        ),
        silent=TRUE
        )
        if(!inherits(result,"try-error")) {
          data(result) # update data with results
          cat(paste("Optimal cross selection step with id:",result$status$analysisId[length(result$status$analysisId)],"saved."))
        }else{
          print(result)
        }
      }

      if(!inherits(result,"try-error")) {
        # view predictions
        output$predictionsOcs <-  DT::renderDT({
          # if ( hideAll$clearAll){
          #   return()
          # }else{
            predictions <- data()$predictions
            predictions <- predictions[predictions$module=="ocs",]
            predictions$analysisId <- as.numeric(predictions$analysisId)
            predictions <- predictions[!is.na(predictions$analysisId),]
            current.predictions <- predictions[predictions$analysisId==max(predictions$analysisId),]
            current.predictions <- subset(current.predictions, select = -c(module,analysisId))
            numeric.output <- c("predictedValue", "stdError", "reliability")
            DT::formatRound(DT::datatable(current.predictions, extensions = 'Buttons',
                                          options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                         lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All')))
            ), numeric.output)
          # }
        })
        # view metrics
        output$metricsOcs <-  DT::renderDT({
          # if ( hideAll$clearAll){
          #   return()
          # }else{
            metrics <- data()$metrics
            metrics <- metrics[metrics$module=="ocs",]
            metrics$analysisId <- as.numeric(metrics$analysisId)
            metrics <- metrics[!is.na(metrics$analysisId),]
            current.metrics <- metrics[metrics$analysisId==max(metrics$analysisId),]
            current.metrics <- subset(current.metrics, select = -c(module,analysisId))
            numeric.output <- c("value", "stdError")
            DT::formatRound(DT::datatable(current.metrics, extensions = 'Buttons',
                                          options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                         lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All')))
            ), numeric.output)
          # }
        })
        # view modeling
        output$modelingOcs <-  DT::renderDT({
          # if ( hideAll$clearAll){
          #   return()
          # }else{
            modeling <- data()$modeling
            modeling <- modeling[modeling$module=="ocs",]
            modeling$analysisId <- as.numeric(modeling$analysisId)
            modeling <- modeling[!is.na(modeling$analysisId),]
            current.modeling <- modeling[modeling$analysisId==max(modeling$analysisId),]
            current.modeling <- subset(current.modeling, select = -c(module,analysisId))
            DT::datatable(current.modeling, extensions = 'Buttons',
                                          options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                         lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All')))
            )
          # }
        })
        ## Report tab: plot scatter of predictions and color by pedigree
        output$plotPredictionsScatter <-  plotly::renderPlotly({
          # input <- list(traitFilterPredictions2D2="desireIndex", environment="desireIndex ~ 20 crosses * 30 degrees", checkboxBoxplotPredictions2D=TRUE,
          #               xAxisPredictions2D="designation",yAxisPredictions2D="predictedValue",colorPredictions2D="mother", sizePredictions2D="stdError", textPredictions2D="designation")
          mydata = data()$predictions
          mtas <- data()$status[which(data()$status$module == "ocs"),"analysisId"];mtaId <- mtas[length(mtas)]
          mydata <- mydata[which(mydata$analysisId == mtaId),]
          mydata = mydata[which(mydata$trait == input$traitFilterPredictions2D2),]
          mydata = mydata[which(mydata$environment == input$environment),]
          res = plotly::plot_ly(data = mydata, x = mydata[,input$xAxisPredictions2D], y = mydata[,input$yAxisPredictions2D],
                                color=mydata[,input$colorPredictions2D], size=mydata[,input$sizePredictions2D], #boxpoints = "all",
                                text=mydata[,input$textPredictions2D],  type="box")
          # res = res %>% plotly::layout(showlegend = FALSE)
          res
        })

      } else {
        output$predictionsOcs <- DT::renderDT({DT::datatable(NULL)})
        output$metricsOcs <- DT::renderDT({DT::datatable(NULL)})
        output$modelingOcs <- DT::renderDT({DT::datatable(NULL)})
      }

      hideAll$clearAll <- FALSE

    }) ## end eventReactive

    output$outOcs <- renderPrint({
      outOcs()
    })

  })
}

## To be copied in the UI
# mod_ocsApp_ui("ocsApp_1")

## To be copied in the server
# mod_ocsApp_server("ocsApp_1")

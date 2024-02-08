#' indexDesireApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_indexDesireApp_ui <- function(id){
  ns <- NS(id)
  tagList(


    sidebarPanel(

      tags$style(".well {background-color:grey; color: #FFFFFF;}"),
      HTML("<img src='www/cgiar3.png' width='42' vspace='10' hspace='10' height='46' align='top'>
                  <font size='5'>Selection indices</font>"),
      # div(tags$p( h4(strong("Desire selection index")))),#, style = "color: #817e7e"
      hr(style = "border-top: 1px solid #4c4c4c;"),
      selectInput(ns("version2IdxD"), "MTA version to analyze", choices = NULL, multiple = FALSE),

      radioButtons(ns("rbSelectionIndices"),"Selection Index",choices=c("Desire","Base"), selected="Desire"),

      conditionalPanel(condition=paste0("input['", ns("rbSelectionIndices"),"']=='Base'"),
                       selectInput(ns("traitsBaseIndex"), "Trait(s) to analyze", choices = NULL, multiple = TRUE),
                       uiOutput(ns("SliderBaseIndex")),
                       actionButton(ns("runIdxB"), "Run", icon = icon("play-circle")),
                       hr(style = "border-top: 1px solid #4c4c4c;"),
                       uiOutput(ns("qaQcIdxBInfo")),
                       textOutput(ns("outIdxB"))
      ),
      conditionalPanel(condition=paste0("input['", ns("rbSelectionIndices"),"']=='Desire'"),
                       selectInput(ns("trait2IdxD"), "Trait(s) to analyze", choices = NULL, multiple = TRUE),
                       selectInput(ns("scaledIndex"), label = "Scale traits for index?", choices = list(TRUE,FALSE), selected = FALSE, multiple=FALSE),
                       # textInput(ns("desirev"), label = "Desired change in traits [Enter a numeric vector (comma delimited): e.g: 0,100,2. Standard deviations if traits are scaled, otherwise use original-scale values]", value=NULL),
                       uiOutput(ns("SliderDesireIndex")),
                       hr(style = "border-top: 1px solid #4c4c4c;"),
                       shinydashboard::box(width = 12, status = "success", background="green",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Settings...",
                                           numericInput(ns("proportion"), label = "Selected proportion", value = 0.1, min=0.001,max=1, step=0.05),
                                           numericInput(ns("fontSizeRadar"), label = "Font size", value = 12),
                                           selectInput(ns("verboseIndex"), label = "Print logs?", choices = list(TRUE,FALSE), selected = FALSE, multiple=FALSE)
                       ),
                       actionButton(ns("runIdxD"), "Run", icon = icon("play-circle")),
                       hr(style = "border-top: 1px solid #4c4c4c;"),
                       uiOutput(ns("qaQcIdxDInfo")),
                       textOutput(ns("outIdxD"))
      )
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
                                          p("Genetic evaluation has as final purpose to select the individuals with highest genetic merit across
                                          all traits of interest. In order to select for multiple traits at the same time a selection index is preferred.
                                          This option aims to calculate a selection index using across-environment predictions from multiple
                              traits based on user's desired change (used to calculate weights) and return a table of predictions with the index and the traits used
                              for selection.
                                The way the options are used is the following:"),
                                          img(src = "www/indexDesire.png", height = 300, width = 600), # add an image
                                          p(strong("Traits to analyze.-")," Traits to be considered in the index."),
                                          p(strong("Desire vector.-")," Vector of values indicating the desired change in traits."),
                                          p(strong("Scale predictions.-")," A TRUE or FALSE value indicating if the table of traits should be
                                scaled or not. If TRUE is selected, the values of the desire vector are expected to be expressed in
                                standard deviations. If FALSE, the values of the desire vector are expected to be expressed in
                                original units."),
                                          h2(strong("References:")),
                                          p("Pesek, J., & Baker, R. J. (1969). Desired improvement in relation to selection indices. Canadian journal of plant science, 49(6), 803-804."),
                                          p("Ceron-Rojas, J. J., & Crossa, J. (2018). Linear selection indices in modern plant breeding (p. 256). Springer Nature."),
                                          h2(strong("Software used:")),
                                          p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
                                Vienna, Austria. URL https://www.R-project.org/.")
                                   )
               )
      ),
      tabPanel(p("Input",class="input-p"), icon = icon("arrow-right-to-bracket"),
               tabsetPanel(
                 tabPanel("Mta-metrics", icon = icon("table"),
                          br(),
                          shinydashboard::box(status="success",width = 12,solidHeader = TRUE,
                                              column(width=6,selectInput(ns("parameterMetrics"), "Parameter to visualize", choices = NULL, multiple = FALSE) ),
                                              column(width=12, plotly::plotlyOutput(ns("barplotPredictionsMetrics")) )
                          )
                 ),
                 tabPanel("Mta-modeling", icon = icon("table"),
                          br(),
                          shinydashboard::box(status="success",width = 12,
                                              solidHeader = TRUE,
                                              column(width=12,DT::DTOutput(ns("statusIndex")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
                          )
                 ),
                 tabPanel("Mta-traits-wide", icon = icon("table"),
                          br(),
                          shinydashboard::box(status="success",width = 12, solidHeader = TRUE,
                                              column(width=12,DT::DTOutput(ns("tablePredictionsTraitsWide")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
                          )
                 ),
                 tabPanel("Radar plot", icon = icon("table"),
                          br(),
                          shinydashboard::box(status="success",width = 12,solidHeader = TRUE,
                                              plotly::plotlyOutput(ns("plotPredictionsRadar"))
                          )
                 ),
                 tabPanel("Expected response", icon = icon("table"),
                          br(),
                          shinydashboard::box(status="success",width = 12, solidHeader = TRUE,
                                              plotly::plotlyOutput(ns("plotPotentialResponse"))
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
                                              column(
                                                width=12,
                                                conditionalPanel(condition=paste0("input['", ns("rbSelectionIndices"),"']=='Desire'"),
                                                                 DT::DTOutput(ns("predictionsIdxD"))
                                                ),
                                                conditionalPanel(condition=paste0("input['", ns("rbSelectionIndices"),"']=='Base'"),
                                                                 DT::DTOutput(ns("predictionsIdxB"))
                                                ),
                                                style = "height:800px; overflow-y: scroll;overflow-x: scroll;"
                                                )
                          )
                 ),
                 tabPanel("Modeling", icon = icon("table"),
                          br(),
                          shinydashboard::box(status="success",width = 12,
                                              solidHeader = TRUE,
                                              column(
                                                width=12,
                                                conditionalPanel(condition=paste0("input['", ns("rbSelectionIndices"),"']=='Desire'"),
                                                                 DT::DTOutput(ns("modelingIdxD"))
                                                ),
                                                conditionalPanel(condition=paste0("input['", ns("rbSelectionIndices"),"']=='Base'"),
                                                                 DT::DTOutput(ns("modelingIdxB"))
                                                ),
                                                style = "height:800px; overflow-y: scroll;overflow-x: scroll;"
                                                )
                          )
                 ),
                 tabPanel("Report", icon = icon("file-image"),
                          br(),
                          div(tags$p("Please download the report below:") ),
                          downloadButton(ns("downloadReportIndex"), "Download report"),
                          br(),
                          conditionalPanel(condition=paste0("input['", ns("rbSelectionIndices"),"']=='Desire'"),
                                           uiOutput(ns('reportIndexD'))
                          ),
                          conditionalPanel(condition=paste0("input['", ns("rbSelectionIndices"),"']=='Base'"),
                                           DT::DTOutput(ns("BaseIndex"))
                          )

                 )
               )
      )# end of output panel
    )) # end mainpanel


  )
}

#' indexDesireApp Server Functions
#'
#' @noRd
mod_indexDesireApp_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })
    # data = reactive({
    #   load("~/Documents/bioflow/dataStr0.RData")
    #   data <- res
    #   return(data)
    # })
    ############################################################################
    # warning message
    output$warningMessage <- renderUI(
      if(is.null(data())){
        HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your phenotypic data using the 'Data Retrieval' tab.")) )
      }else{ # data is there
        mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
        if(mappedColumns == 3){
          if("mta" %in% data()$status$module){
            HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to perform the selection index inspecting the other tabs.")) )
          }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please perform a Multi-Trial Analysis before performing a selection index")) ) }
        }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please make sure that the columns: 'environment', 'designation' and \n at least one trait have been mapped using the 'Data Retrieval' tab.")) )}
      }
    )
    ######################################################################################
    ######################################################################################
    ########################################### Desire Index
    ######################################################################################
    ######################################################################################

    #################
    ## version
    observeEvent(c(data()), {
      req(data())
      dtIdxD <- data()
      dtIdxD <- dtIdxD$status
      dtIdxD <- dtIdxD[which(dtIdxD$module == "mta"),]
      traitsIdxD <- unique(dtIdxD$analysisId)
      if(length(traitsIdxD) > 0){names(traitsIdxD) <- as.POSIXct(traitsIdxD, origin="1970-01-01", tz="GMT")}
      updateSelectInput(session, "version2IdxD", choices = traitsIdxD)
    })
    #################
    ## traits
    observeEvent(c(data(), input$version2IdxD), {
      req(data())
      req(input$version2IdxD)
      dtIdxD <- data()
      dtIdxD <- dtIdxD$predictions
      dtIdxD <- dtIdxD[which(dtIdxD$analysisId == input$version2IdxD),]
      traitsIdxD <- unique(dtIdxD$trait)
      updateSelectInput(session, "trait2IdxD", choices = traitsIdxD)
    })
    ####################
    ## desired changes for Desire Index
    output$SliderDesireIndex <- renderUI({
      req(data())
      req(input$version2IdxD)
      req(input$trait2IdxD)
      req(input$scaledIndex)
      trait2IdxD <- input$trait2IdxD # trait2IdxD <- c("Yield_Mg_ha_QTL","Ear_Height_cm") # list(trait2IdxD=c("Yield_Mg_ha","Ear_Height_cm"))
      dtIdxD <- data()
      dtIdxD <- dtIdxD$predictions
      dtIdxD <- dtIdxD[which(dtIdxD$analysisId == input$version2IdxD),]
      if(input$scaledIndex){ # if user wants traits scaled
        lapply(1:length(trait2IdxD), function(i) {
          sliderInput(
            session$ns(paste0('SliderDesireIndex',i)),
            paste0('Desired change (SDs)',": ",trait2IdxD[i]),
            min = -5,
            max = 5,
            value = 1,
            step = 0.5
          )
        })
      }else{ # if user wants to use original scale
        lapply(1:length(trait2IdxD), function(i) {
          traitVals <- dtIdxD[which(dtIdxD$trait == trait2IdxD[i]), "predictedValue"]
          sliderInput(
            session$ns(paste0('SliderDesireIndex',i)),
            paste0('Desired change (original scale)',": ",trait2IdxD[i]),
            min = -round(sd(traitVals, na.rm=TRUE)*4,3),
            max = round(sd(traitVals, na.rm=TRUE)*4,3),
            value = round(sd(traitVals, na.rm=TRUE),3),
            step = round(0.5*sd(traitVals, na.rm=TRUE),3)
          )
        })
      }

    })
    #################
    ## render the data to be analyzed (wide format)
    output$statusIndex <-  DT::renderDT({
      req(data())
      req(input$version2IdxD)
      dtSta <- data() # dtSta<- result
      ### change column names for mapping
      paramsPheno <- data()$modeling
      paramsPheno <- paramsPheno[which(paramsPheno$analysisId %in% input$version2IdxD),, drop=FALSE]
      paramsPheno$analysisId <- as.POSIXct(paramsPheno$analysisId, origin="1970-01-01", tz="GMT")
      DT::datatable(paramsPheno, extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
      )
    })
    observeEvent(c(data(),input$version2IdxD), { # update parameter
      req(data())
      req(input$version2IdxD)
      dtMta <- data()
      dtMta <- dtMta$metrics
      dtMta <- dtMta[which(dtMta$analysisId %in% input$version2IdxD),] # only traits that have been QA
      metricsMtaInput <- unique(dtMta$parameter)
      updateSelectInput(session, "parameterMetrics", choices = metricsMtaInput)
    })
    output$barplotPredictionsMetrics <- plotly::renderPlotly({
      req(data())
      req(input$version2IdxD)
      dtMta <- data()
      mydata <- dtMta$metrics
      mydata <- mydata[which(mydata$analysisId %in% input$version2IdxD),]
      mydata = mydata[which(mydata$parameter %in% input$parameterMetrics),]
      res = plotly::plot_ly(data = mydata, x = mydata[,"environment"], y = mydata[,"value"],
                            color=mydata[,"trait"]
      )
      res = res %>% plotly::add_bars()
      res
    })
    output$tablePredictionsTraitsWide <-  DT::renderDT({
      req(data())
      req(input$version2IdxD)
      dtIdxD <- data(); dtIdxD <- dtIdxD$predictions
      dtIdxD <- dtIdxD[which(dtIdxD$analysisId == input$version2IdxD),setdiff(colnames(dtIdxD),c("module","analysisId"))]
      wide <- stats::reshape(dtIdxD[,c(c("designation"),"trait",c("predictedValue"))], direction = "wide", idvar = c("designation"),
                             timevar = "trait", v.names = c("predictedValue"), sep= "_")
      colnames(wide) <- gsub("predictedValue_","",colnames(wide))
      numeric.output <- colnames(wide)[-c(1)]
      DT::formatRound(DT::datatable(wide, extensions = 'Buttons',
                                    options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                   lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
      ), numeric.output)
    })
    # render radar plot for initial values
    output$plotPredictionsRadar <-  plotly::renderPlotly({
      req(data())
      req(input$version2IdxD)
      req(input$trait2IdxD)
      dtIdxD <- data(); dtIdxD <- dtIdxD$predictions
      mydata <- dtIdxD[which(dtIdxD$analysisId == input$version2IdxD),setdiff(colnames(dtIdxD),c("module","analysisId"))]
      if (length(input$trait2IdxD) != 0) {
        values <- NULL
        for (i in 1:length(input$trait2IdxD)) {
          tempval <- reactive({paste0('input$','SliderDesireIndex',i)})
          values[i] <- tempval()
          values[i] <- eval(parse(text = values[i]))
        }
        values <- t(as.numeric(values))
        values <- as.data.frame(values)
        colnames(values) <- input$trait2IdxD
      }
      values <- as.numeric(values)
      ## ensure product profile means come sorted
      if(length(input$trait2IdxD) == length(values) ){
        dd <- data.frame(trait=input$trait2IdxD, value=values )
        dd <- dd[with(dd, order(as.numeric(as.factor(trait)))), ]
        desireRp <- dd[,"value"]
        traitRp <- dd[,"trait"]
      }else{desireRp <- values; traitRp <- input$trait2IdxD}
      radarPlot(mydata, environmentPredictionsRadar2="across",traitFilterPredictionsRadar2=traitRp,proportion=input$proportion,meanGroupPredictionsRadar= paste(desireRp, collapse = ", "),
                             fontSizeRadar=input$fontSizeRadar, r0Radar=NULL, neRadar=NULL, plotSdRadar=FALSE) # send to setting plotSdRadar # send to argument meanGroupPredictionsRadar
    })
    # render plot for potential responses
    output$plotPotentialResponse <-  plotly::renderPlotly({
      req(data())
      req(input$version2IdxD)
      req(input$trait2IdxD)
      dtIdxD <- data();
      if (length(input$trait2IdxD) != 0) {
        values <- NULL
        for (i in 1:length(input$trait2IdxD)) {
          tempval <- reactive({paste0('input$','SliderDesireIndex',i)})
          values[i] <- tempval()
          values[i] <- eval(parse(text = values[i]))
        }
        values <- t(as.numeric(values))
        values <- as.data.frame(values)
        colnames(values) <- input$trait2IdxD
      }
      values <- as.numeric(values)
      plotDensitySelected(object=dtIdxD,environmentPredictionsRadar2="across", traitFilterPredictionsRadar2=input$trait2IdxD, meanGroupPredictionsRadar=paste(values, collapse = ", "), proportion=input$proportion,
                                       analysisId=input$version2IdxD, trait=input$trait2IdxD, desirev=paste(values, collapse = ", "), scaled=input$scaledIndex)
    })
    ## render result of "run" button click
    outIdxD <- eventReactive(input$runIdxD, {
      req(data())
      req(input$version2IdxD)
      req(input$trait2IdxD)
      # req(input$desirev)
      shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
      dtIdxD <- data()
      # define values for slider all traits for base index
      if (length(input$trait2IdxD) != 0) {
        values <- NULL
        for (i in 1:length(input$trait2IdxD)) {
          tempval <- reactive({paste0('input$','SliderDesireIndex',i)})
          values[i] <- tempval()
          values[i] <- eval(parse(text = values[i]))
        }
        values <- t(as.numeric(values))
        values <- as.data.frame(values)
        colnames(values) <- input$trait2IdxD
      }
      values <- as.numeric(values)
      # run the modeling, but before test if mta was done
      if(sum(dtIdxD$status$module %in% "mta") == 0) {
        output$qaQcIdxDInfo <- renderUI({
          if (hideAll$clearAll)
            return()
          else
            req(dtIdxD)
          HTML(as.character(div(style="color: brown;",
                                "Please perform Multi-Trial-Analysis before conducting a Selection index."))
          )
        })
      }else{
        output$qaQcIdxDInfo <- renderUI({return(NULL)})
        result <- try(cgiarPipeline::indexDesire(
          phenoDTfile= dtIdxD, # input data structure
          analysisId=input$version2IdxD, # analysis to be picked from predictions database
          trait= input$trait2IdxD, # traits to include in the index
          desirev = values, # as.numeric(unlist(strsplit(input$desirev,","))), # vector of desired values
          scaled=input$scaledIndex, # whether predicted values should be scaled or not
          verbose=input$verboseIndex # should we print logs or not
        ),
        silent=TRUE
        )
        if(!inherits(result,"try-error")) {
          data(result) # update data with results
          # save(result, file = "./R/outputs/resultIndex.RData")
          cat(paste("Selection index step with id:",as.POSIXct(result$status$analysisId[length(result$status$analysisId)], origin="1970-01-01", tz="GMT"),"saved."))
        }else{
          cat(paste("Analysis failed with the following error message: \n\n",result[[1]]))
        }
      }
      shinybusy::remove_modal_spinner()
      if(!inherits(result,"try-error")) {
        # display table of predictions
        output$predictionsIdxD <-  DT::renderDT({
          # if ( hideAll$clearAll){
          #   return()
          # }else{
            predictions <- result$predictions
            predictions <- predictions[predictions$module=="indexD",]
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
        # display table of modeling
        output$modelingIdxD <-  DT::renderDT({
          # if ( hideAll$clearAll){
          #   return()
          # }else{
            modeling <- result$modeling
            mtas <- result$status[which(result$status$module == "indexD"),"analysisId"]; mtaId <- mtas[length(mtas)]
            modeling <- modeling[which(modeling$analysisId == mtaId),]
            modeling <- subset(modeling, select = -c(module,analysisId))
            DT::datatable(modeling, extensions = 'Buttons',
                                          options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                         lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
            )
          # }
        })
        ## Report tab
        output$reportIndexD <- renderUI({
          HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportIndexD.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
        })

      } else {
        output$predictionsIdxD <- DT::renderDT({DT::datatable(NULL)})
        output$metricsIdxD <- DT::renderDT({DT::datatable(NULL)})
        output$modelingIdxD <- DT::renderDT({DT::datatable(NULL)})
        hideAll$clearAll <- TRUE
      }

      hideAll$clearAll <- FALSE

    }) ## end eventReactive

    output$outIdxD <- renderPrint({
      outIdxD()
    })


    ######################################################################################
    ######################################################################################
    ########################################### Base Index
    ######################################################################################
    ######################################################################################

    ####################
    ## traits for base index
    observeEvent(c(data(), input$version2IdxD), {
      req(data())
      req(input$version2IdxD)
      dtBaseIndex <- data()
      dtBaseIndex <- dtBaseIndex$predictions
      dtBaseIndex <- dtBaseIndex[which(dtBaseIndex$analysisId == input$version2IdxD),]
      traitsBaseIndex <- unique(dtBaseIndex$trait)
      updateSelectInput(session, "traitsBaseIndex", choices = traitsBaseIndex)
    })
    ####################
    ## Weights for traits Base Index
    output$SliderBaseIndex <- renderUI({
      req(data())
      req(input$traitsBaseIndex)
      traitsBaseIndex <- input$traitsBaseIndex
      lapply(1:length(traitsBaseIndex), function(i) {
        sliderInput(
          session$ns(paste0('SliderBaseIndex',i)),
          paste0('Weight',": ",traitsBaseIndex[i]),
          min = -5,
          max = 5,
          value = 1,
          step = 0.5
        )
      })
    })
    ####################
    ## Run button for Base Index
    outIdxB <- eventReactive(input$runIdxB, {
      req(data())
      req(input$version2IdxD)
      req(input$traitsBaseIndex)
      dtBaseIndex <- data()
      shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
      dtBaseIndexPred <- dtBaseIndex$predictions
      dtBaseIndexPred <- dtBaseIndexPred[dtBaseIndexPred$analysisId == input$version2IdxD,]
      traitsBaseIndex <- input$traitsBaseIndex

      # define values for slider all traits for base index
      if (length(traitsBaseIndex) != 0) {
        values <- NULL
        for (i in 1:length(traitsBaseIndex)) {
          tempval <- reactive({paste0('input$','SliderBaseIndex',i)})
          values[i] <- tempval()
          values[i] <- eval(parse(text = values[i]))
        }
        values <- t(as.numeric(values))
        values <- as.data.frame(values)
        colnames(values) <- traitsBaseIndex
      }
      values <- as.numeric(values)

      # run the modeling, but before test if mta was done
      if(sum(dtBaseIndex$status$module %in% "mta") == 0) {
        output$qaQcIdxBInfo <- renderUI({
          if (hideAll$clearAll)
            return()
          else
            req(dtBaseIndex)
          HTML(as.character(div(style="color: brown;",
                                "Please perform Multi-Trial-Analysis before conducting a Selection index."))
          )
        })
      }else{
        output$qaQcIdxBInfo <- renderUI({return(NULL)})
        result <- try(cgiarPipeline::baseIndex(
          dtBaseIndex,
          input$version2IdxD,
          traitsBaseIndex,
          values),
        silent=TRUE
        )
        if(!inherits(result,"try-error")) {
          data(result) # update data with results
          # save(result, file = "./R/outputs/resultIndex.RData")
          cat(paste("Selection index step with id:",result$status$analysisId[length(result$status$analysisId)],"saved."))
        }else{
          cat(paste("Analysis failed with the following error message: \n\n",result[[1]]))
        }
      }
      shinybusy::remove_modal_spinner()
      if(!inherits(result,"try-error")) {
        # display table of predictions
        output$predictionsIdxB <-  DT::renderDT({
          # if ( hideAll$clearAll){
          #   return()
          # }else{
          predictions <- result$predictions
          predictions <- predictions[predictions$module=="indexB",]
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
        # display table of modeling
        output$modelingIdxB <-  DT::renderDT({
          # if ( hideAll$clearAll){
          #   return()
          # }else{
          modeling <- result$modeling
          mtas <- result$status[which(result$status$module == "indexB"),"analysisId"]; mtaId <- mtas[length(mtas)]
          modeling <- modeling[which(modeling$analysisId == mtaId),]
          modeling <- subset(modeling, select = -c(module,analysisId))
          DT::datatable(modeling, extensions = 'Buttons',
                        options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                       lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
          )
          # }
        })
        # Report tab
        analysisIdBaseIndex <- result$status[ result$status$module %in% c("mta","indexB"),"analysisId"]
        predBaseIndex <- result$predictions[result$predictions$analysisId %in% analysisIdBaseIndex,]

        predBaseIndexWide <- reshape(
          data=subset(predBaseIndex, select=c(designation,trait,predictedValue)),
          timevar = "trait",
          idvar = "designation",
          direction="wide"
        )
        colnames(predBaseIndexWide)[-1] <- gsub("predictedValue.","", colnames(predBaseIndexWide)[-1])
        predBaseIndexWide <- predBaseIndexWide[order(predBaseIndexWide$baseIndex,decreasing=TRUE),]
        predBaseIndexWide$Rank <- 1:nrow(predBaseIndexWide)

        output$BaseIndex <-  DT::renderDT({
          # if ( hideAll$clearAll){
          #   return()
          # }else{
          numeric.output <- c(input$traitsBaseIndex,"baseIndex")
          # print(head(predBaseIndexWide))
          DT::formatRound(DT::datatable(predBaseIndexWide,
                                        extensions = 'Buttons',
                                        rownames = FALSE,
                                        class = 'cell-border',
                                        options = list(dom = 'Blfrtip',
                                                       scrollY = "400px",
                                                       scrollX = "400px",
                                                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                       paging=F)
          ), numeric.output)

        })

      }


      # hideAll$clearAll <- FALSE

    })

    output$downloadReportIndex <- downloadHandler(
      filename = function() {
        paste('my-report', sep = '.', switch(
          "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
        ))
      },
      content = function(file) {
        src <- normalizePath(system.file("rmd","reportIndexD.Rmd",package="bioflow"))
        src2 <- normalizePath('data/resultIndex.RData')
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'report.Rmd', overwrite = TRUE)
        file.copy(src2, 'resultIndex.RData', overwrite = TRUE)
        out <- rmarkdown::render('report.Rmd', params = list(toDownload=TRUE),switch(
          "HTML",
          HTML = rmarkdown::html_document()
        ))
        file.rename(out, file)
      }
    )

    output$outIdxB <- renderPrint({
      outIdxB()
    })


  })
}

## To be copied in the UI
# mod_indexDesireApp_ui("indexDesireApp_1")

## To be copied in the server
# mod_indexDesireApp_server("indexDesireApp_1")

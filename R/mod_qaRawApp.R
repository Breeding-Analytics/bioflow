#' qaRawApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_qaRawApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    sidebarPanel(
      tags$style(".well {background-color:grey; color: #FFFFFF;}"),
      div(tags$p( "Outlier detection")),#, style = "color: #817e7e"
      hr(style = "border-top: 1px solid #4c4c4c;"),

      selectInput(ns("traitOutqPheno"), "Trait to QA", choices = NULL, multiple = FALSE),
      numericInput(ns("traitLBOutqPheno"), label = "Trait lower bound", value = 0.01),
      numericInput(ns("traitUBOutqPheno"), label = "Trait upper bound", value = Inf),
      numericInput(ns("outlierCoefOutqPheno"), label = "Outlier coefficient", value = 1.5),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      shinydashboard::box(width = 12, status = "primary", background="light-blue",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Settings...",
                          numericInput(ns("outlierCoefOutqFont"), label = "x-axis font size", value = 12, step=1)
      ),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      # actionButton(ns("runOutqPheno"), "Run", icon = icon("play-circle")),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      # shinycssloaders::withSpinner(textOutput(ns("outQaRaw")),type=8)
    ), # end sidebarpanel
    mainPanel(tabsetPanel(
      type = "tabs",

      tabPanel("Outlier detection",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   plotly::plotlyOutput(ns("plotPredictionsCleanOut"))
                                   # column(width=12,DT::DTOutput(ns("phenoQaRaw")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
               )
      ),
      tabPanel("Modifications",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,DT::DTOutput(ns("modificationsQa")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
               )
      ),
      tabPanel("Data",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,DT::DTOutput(ns("phenoQaRaw")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
               )
      ),
      tabPanel("Documentation",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,   style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
               )
      ),
      tabPanel("References",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,    style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
               )
      )
    )) # end mainpanel

  )
}

#' qaRawApp Server Functions
#'
#' @noRd
mod_qaRawApp_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })
    ############################################################################

    # Create the fields
    observeEvent(data(), {
      req(data())
      dtQaRaw <- data()
      dtQaRaw <- dtQaRaw$metadata$pheno
      traitsQaRaw <- unique(dtQaRaw[dtQaRaw$parameter=="trait","value"])
      updateSelectInput(session, "traitOutqPheno",choices = traitsQaRaw)
    })

    newOutliers <- reactive({ # p('File to be analyzed')
      req(data())
      myObject <- data()
      mydata <- myObject$data$pheno
      mydata$rowindex <- 1:nrow(mydata)
      mydata[, "environment"] <- as.factor(mydata[, "environment"])
      traitClasses <- unlist(lapply(mydata, class))
      analysisId <- as.numeric(Sys.time())
      myoutliers <- myObject$modifications$pheno
      if(!is.null(myoutliers) & !is.null(nrow(myoutliers)) ){ # there's previous outliers for this trait
        outsItrait <- which(myoutliers$trait == input$traitOutqPheno)
        myoutliersReduced <- myoutliers[outsItrait,] # outliers for the trait in turn
      }else{
        myoutliersReduced <- data.frame(matrix(nrow=0, ncol=6))
        colnames(myoutliersReduced) <- c("module" ,"analysisId" ,"trait","reason","row" , "value" )
      }
      ## add new outliers
      outList <- list(); counter=1
      for (i in 1:nlevels(mydata[, "environment"])) {
        sampleDT <- mydata[which(mydata[, "environment"] == levels(mydata[, "environment"])[i]), ] # data for the ith environment
        if(!is.na(input$outlierCoefOutqPheno)){
          outlier <- grDevices::boxplot.stats(x=sampleDT[, input$traitOutqPheno],coef=input$outlierCoefOutqPheno )$out
          toSilence <- sampleDT[which(sampleDT[,input$traitOutqPheno] %in% outlier),"rowindex"]
          typeOut <- rep("outlier",length(toSilence))
        }else{
          toSilence <- numeric()
          typeOut <- character()
        }
        outOfBounds <- which((sampleDT[, input$traitOutqPheno] < input$traitLBOutqPheno) | (sampleDT[, input$traitOutqPheno] > input$traitUBOutqPheno ) )
        if(length(outOfBounds) > 0){toSilence <- c(toSilence, sampleDT[outOfBounds,"rowindex"]); typeOut <- c(typeOut, rep("outlier",length(outOfBounds))) }
        if(length(toSilence) > 0){
          outList[[counter]] <- data.frame(module="qaRaw",analysisId=analysisId,trait=input$traitOutqPheno,reason=typeOut,row=toSilence, value=NA);
          counter=counter+1
        }
        # }# end of if enough data
      }# end for each trial
      if(length(outList) > 0){
        myoutliersReduced2 <- unique(do.call(rbind, outList))
        if( !is.null(myoutliers) & !is.null(nrow(myoutliers)) ){
          myoutliersReduced2 <- unique(rbind(myoutliersReduced,myoutliersReduced2))
        }
      }else{
        if(!is.null(myoutliers)){
          myoutliersReduced2 <- unique(myoutliersReduced)
        }else{
          myoutliersNull <- data.frame(matrix(nrow=0, ncol=6))
          colnames(myoutliersNull) <- c("module" ,"analysisId" ,"trait","reason","row" , "value" )
          myoutliersReduced2 <- myoutliersNull
        }
      }
      ## reactive
      return(myoutliersReduced2)

    })

    ## render the expected result

    output$plotPredictionsCleanOut <- plotly::renderPlotly({
      req(data())
      req(input$outlierCoefOutqFont)
      req(input$traitOutqPheno)
      mydata <- data()$data$pheno
      mydata$rowindex <- 1:nrow(mydata)
      mydata[, "environment"] <- as.factor(mydata[, "environment"])
      mydata[, "designation"] <- as.factor(mydata[, "designation"])
      mo <- newOutliers()
      mydata$color <- 1
      if(nrow(mo) > 0){
        mydata$color[which(mydata$rowindex %in% unique(mo$row))]=2
      }
      mydata$color <- as.factor(mydata$color)
      mydata$environment <- cgiarBase::cleanChar(mydata$environment)# as.factor(apply(data.frame(cgiarBase::cleanChar(mydata$fieldinst)),1,function(x){substr(x,max(c(1,(nchar(x)-18))),nchar(x))}))
      res <- plotly::plot_ly(y = mydata[,input$traitOutqPheno], type = "box", boxpoints = "all", jitter = 0.3,color=mydata[,"color"],
                             x = mydata[,"environment"], text=mydata[,"designation"],
                             pointpos = -1.8)
      res = res %>% plotly::layout(showlegend = FALSE,
                                   xaxis = list(titlefont = list(size = input$outlierCoefOutqFont), tickfont = list(size = input$outlierCoefOutqFont))
      )
      res
    })



    ## display pheno data
    observeEvent(data(),{

      output$phenoQaRaw <-  DT::renderDT({
        # if ( hideAll$clearAll)
        #   return()
        # else
          req(data())
          dtQaRaw <- data()
          dtQaRaw <- dtQaRaw$data$pheno
          DT::datatable(dtQaRaw,
                        options = list(autoWidth = TRUE),
                        filter = "top"
          )

      })

    })

    # ## render result of "run" button click
    # outQaRaw <- eventReactive(input$runQaRaw, {
    #   req(data())
    #   req(input$trait2QaRaw)
    #   req(input$genoUnitQaRaw)
    #   dtQaRaw <- data()
    #
    #   myFamily = apply(xx$df,2,function(y){rownames(xx$df)[which(y > 0)[1]]})
    #   dontHaveDist <- which(is.na(myFamily))
    #   if(length(dontHaveDist) > 0){myFamily[dontHaveDist] <- "gaussian(link = 'identity')"}
    #
    #   # run the modeling, but before test if qa/qc done
    #   if(sum(dtQaRaw$status$module %in% "qa") == 0) {
    #     output$qaQcQaRawInfo <- renderUI({
    #       if (hideAll$clearAll)
    #         return()
    #       else
    #         req(dtQaRaw)
    #       HTML(as.character(div(style="color: brown;",
    #                             "Please perform QA/QC before conducting a Single-Trial Analysis."))
    #       )
    #     })
    #   } else {
    #     output$qaQcQaRawInfo <- renderUI({return(NULL)})
    #     result <- try(cgiarPipe::staLMM(phenoDTfile = dtQaRaw,
    #                                     trait=input$trait2QaRaw, traitFamily = myFamily,
    #                                     fixedTerm = input$fixedTermQaRaw2,
    #                                     returnFixedGeno=input$genoAsFixedQaRaw, genoUnit = input$genoUnitQaRaw,
    #                                     verbose = input$verboseQaRaw, maxit = input$maxitQaRaw),
    #                   silent=TRUE
    #     )
    #   }
    #
    #   if(sum(dtQaRaw$status$module %in% "qa") != 0) {
    #
    #     output$predictionsQaRaw <-  DT::renderDT({
    #       if(!inherits(result,"try-error") ){
    #         if ( hideAll$clearAll)
    #           return()
    #         else
    #           predictions <- result$predictions
    #         predictions <- predictions[predictions$module=="sta",]
    #
    #         predictions$analysisId <- as.numeric(predictions$analysisId)
    #         predictions <- predictions[!is.na(predictions$analysisId),]
    #
    #
    #         current.predictions <- predictions[predictions$analysisId==max(predictions$analysisId),]
    #         current.predictions <- subset(current.predictions, select = -c(module,analysisId))
    #         numeric.output <- c("predictedValue", "stdError", "reliability")
    #         DT::formatRound(DT::datatable(current.predictions,
    #                                       options = list(autoWidth = TRUE),
    #                                       filter = "top"
    #         ), numeric.output)
    #       }
    #     })
    #
    #     output$metricsQaRaw <-  DT::renderDT({
    #       if(!inherits(result,"try-error") ){
    #         if ( hideAll$clearAll)
    #           return()
    #         else
    #           metrics <- result$metrics
    #         metrics <- metrics[metrics$module=="sta",]
    #         metrics$analysisId <- as.numeric(metrics$analysisId)
    #         metrics <- metrics[!is.na(metrics$analysisId),]
    #         current.metrics <- metrics[metrics$analysisId==max(metrics$analysisId),]
    #         current.metrics <- subset(current.metrics, select = -c(module,analysisId))
    #         numeric.output <- c("value", "stdError")
    #         DT::formatRound(DT::datatable(current.metrics,
    #                                       options = list(autoWidth = TRUE),
    #                                       filter = "top"
    #         ), numeric.output)
    #       }
    #     })
    #
    #     output$modelingQaRaw <-  DT::renderDT({
    #       if(!inherits(result,"try-error") ){
    #         if ( hideAll$clearAll)
    #           return()
    #         else
    #           modeling <- result$modeling
    #         modeling <- modeling[modeling$module=="sta",]
    #
    #         modeling$analysisId <- as.numeric(modeling$analysisId)
    #         modeling <- modeling[!is.na(modeling$analysisId),]
    #
    #         current.modeling <- modeling[modeling$analysisId==max(modeling$analysisId),]
    #         current.modeling <- subset(current.modeling, select = -c(module,analysisId))
    #         DT::datatable(current.modeling,
    #                       options = list(autoWidth = TRUE),
    #                       filter = "top"
    #         )
    #       }
    #     })
    #   } else {
    #     output$predictionsQaRaw <- DT::renderDT({DT::datatable(NULL)})
    #     output$metricsQaRaw <- DT::renderDT({DT::datatable(NULL)})
    #     output$modelingQaRaw <- DT::renderDT({DT::datatable(NULL)})
    #   }
    #
    #   hideAll$clearAll <- FALSE
    #
    # }) ## end eventReactive
    #
    # output$outQaRaw <- renderPrint({
    #   outQaRaw()
    # })

    # output$markdown <- renderUI({
    #   HTML(markdown::markdownToHTML(knitr::knit('R/testing_sta.Rmd', quiet = TRUE)))
    # })



  })
}

## To be copied in the UI
# mod_qaRawApp_ui("qaRawApp_1")

## To be copied in the server
# mod_qaRawApp_server("qaRawApp_1")

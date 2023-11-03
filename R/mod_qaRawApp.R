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

    shiny::sidebarPanel(#width = 3,
      width = 3,
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
      actionButton(ns("runQaRaw"), "Save outliers", icon = icon("play-circle")),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      shinycssloaders::withSpinner(textOutput(ns("outQaRaw")),type=8),
      # uiOutput(ns('navigate')),
    ), # end sidebarpanel
    shiny::mainPanel(width = 9,
      tabsetPanel( #width=9,
      type = "tabs",

      tabPanel("Outlier detection",
               br(),
               shinydashboard::box(status="primary",width = 12,
                                   solidHeader = TRUE,
                                   plotly::plotlyOutput(ns("plotPredictionsCleanOut")),
                                   column(width=12,DT::DTOutput(ns("modificationsQa")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
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
      shinyjs::hide(ns("traitOutqPheno"))
    })

    newOutliers <- reactive({ # p('File to be analyzed')
      req(data())
      myObject <- data()
      mydata <- myObject$data$pheno
      mydata$rowindex <- 1:nrow(mydata)
      mydata[, "environment"] <- as.factor(mydata[, "environment"])
      traitClasses <- unlist(lapply(mydata, class))
      analysisId <- NA#as.numeric(Sys.time())
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
          typeOut <- rep("outlierIQR",length(toSilence))
        }else{
          toSilence <- numeric()
          typeOut <- character()
        }
        outOfBounds <- which((sampleDT[, input$traitOutqPheno] < input$traitLBOutqPheno) | (sampleDT[, input$traitOutqPheno] > input$traitUBOutqPheno ) )
        if(length(outOfBounds) > 0){toSilence <- c(toSilence, sampleDT[outOfBounds,"rowindex"]); typeOut <- c(typeOut, rep("outlierIQR",length(outOfBounds))) }
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

    ## display the current outliers
    observeEvent(data(),{

      output$modificationsQa <-  DT::renderDT({

        req(data())
        req(input$outlierCoefOutqFont)
        req(input$traitOutqPheno)
        ## get the outlier table
        outlier <- newOutliers()
        removeCols <- c("module","analysisId","value")
        outlier <- outlier[, setdiff(colnames(outlier),removeCols)]
        colnames(outlier) <- cgiarBase::replaceValues(Source = colnames(outlier), Search = "row", Replace = "record")
        ## add phenotypic data
        dtQaRaw <- data()
        dtQaRaw <- dtQaRaw$data$pheno
        dtQaRaw$outlierRow <- 1:nrow(dtQaRaw)
        removeCols <- c("stage","pipeline","country","year","season","location","trial","gid")
        dtQaRaw <- dtQaRaw[,unique(c("outlierRow",setdiff(colnames(dtQaRaw), removeCols)))]
        ## merge
        myTable <- base::merge(outlier,dtQaRaw, by.x="record", by.y="outlierRow", all.x=TRUE)

        DT::datatable(myTable,
                      options = list(autoWidth = TRUE),
                      filter = "top"
        )

      })

    })

    ## save when user clicks

    outQaRaw <- eventReactive(input$runQaRaw, {

      req(data())
      req(input$outlierCoefOutqFont)
      req(input$traitOutqPheno)
      ## get the outlier table
      outlier <- newOutliers()
      ## get data structure
      dtQaRaw <- data()
      ## update analsisId in the outliers table
      analysisId <- as.numeric(Sys.time())
      outlier[which(is.na(outlier$analysisId)),"analysisId"] <- analysisId
      ## bind new parameters
      dtQaRaw$modifications$pheno <- rbind(dtQaRaw$modifications$pheno, outlier[, colnames(dtQaRaw$modifications)])
      newStatus <- data.frame(module="qaRaw", analysisId=analysisId )
      dtQaRaw$status <- rbind(dtQaRaw$status, newStatus)
      # fileId <- dtQaRaw$metadata$general[which(dtQaRaw$metadata$general$parameter == "fileId"),"value"]
      # save(dtQaRaw, file=paste0(fileId,".RData"))
      print(paste0("Outliers saved for trait: ", input$traitOutqPheno))


    })
    output$outQaRaw <- renderPrint({
      outQaRaw()
    })

    # back_bn  <- actionButton(ns('prev_trait'), 'Back')
    # next_bn  <- actionButton(ns('next_trait'), 'Next')
    #
    # output$navigate <- renderUI({
    #   dtQaRaw <- data()
    #   dtQaRaw <- dtQaRaw$metadata$pheno
    #   traitsQaRaw <- unique(dtQaRaw[dtQaRaw$parameter=="trait","value"])
    #
    #   tags$div(align = 'center',
    #            span(if (which(traitsQaRaw == input$traitOutqPheno) != 1) back_bn),
    #            span(if (which(traitsQaRaw == input$traitOutqPheno) != length(traitsQaRaw)) next_bn),
    #   )
    # })
    #
    # observeEvent(input$prev_trait,
    #              {
    #                dtQaRaw <- data()
    #                dtQaRaw <- dtQaRaw$metadata$pheno
    #                traitsQaRaw <- unique(dtQaRaw[dtQaRaw$parameter=="trait","value"])
    #
    #                n <- which(traitsQaRaw == input$traitOutqPheno)
    #                updateSelectInput(session, "traitOutqPheno", selected = traitsQaRaw[n - 1])
    #              }
    # )
    #
    # observeEvent(input$next_trait,
    #              {
    #                dtQaRaw <- data()
    #                dtQaRaw <- dtQaRaw$metadata$pheno
    #                traitsQaRaw <- unique(dtQaRaw[dtQaRaw$parameter=="trait","value"])
    #
    #                n <- which(traitsQaRaw == input$traitOutqPheno)
    #                updateSelectInput(session, "traitOutqPheno", selected = traitsQaRaw[n + 1])
    #              }
    # )

  })
}

## To be copied in the UI
# mod_qaRawApp_ui("qaRawApp_1")

## To be copied in the server
# mod_qaRawApp_server("qaRawApp_1")

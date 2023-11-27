#' qaStaApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_qaStaApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    # input <- list(traitOutqPheno="YLD_TON-residual",traitLBOutqPheno=-4,traitUBOutqPheno=4,outlierCoefOutqPheno=2.5, outlierCoefOutqFont=12 )
    shiny::sidebarPanel(#width = 3,
      width = 3,
      tags$style(".well {background-color:grey; color: #FFFFFF;}"),
      div(tags$p( h4(strong("Model-based outlier detection")))),#, style = "color: #817e7e"
      hr(style = "border-top: 1px solid #4c4c4c;"),

      selectInput(ns("traitOutqPheno"), "Trait to QA", choices = NULL, multiple = FALSE),
      numericInput(ns("traitLBOutqPheno"), label = "Trait lower bound", value = -4),
      numericInput(ns("traitUBOutqPheno"), label = "Trait upper bound", value = 4),
      numericInput(ns("outlierCoefOutqPheno"), label = "Outlier coefficient", value = 5),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      shinydashboard::box(width = 12, status = "primary", background="light-blue",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Settings...",
                          numericInput(ns("outlierCoefOutqFont"), label = "x-axis font size", value = 12, step=1)
      ),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      actionButton(ns("runQaMb"), "Save outliers", icon = icon("play-circle")),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      shinycssloaders::withSpinner(textOutput(ns("outQaMb")),type=8),
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
                                                    column(width=12,   style = "height:800px; overflow-y: scroll;overflow-x: scroll;",
                                                           tags$body(
                                                             h1(strong("Details")),
                                                             p("This option aims to allow users to select model-based outliers based on plot whiskers and absolute values applied on conditional residuals.
                                The way arguments are used is the following:"),
                                                             p(strong("Outlier coefficient.-")," this determines how far the plot whiskers extend out from the box. If coef is positive, the whiskers extend to the most extreme data point which is no more than coef times the length of the box away from the box. A value of zero causes the whiskers to extend to the data extremes (and no outliers be returned)."),
                                                             p(strong("Trait lower bound.-"),"Lower bound threshold determining as an outlier any value smaller than this."),
                                                             p(strong("Trait upper bound.-"),"Upper bound threshold determining as an outlier any value greater than this."),
                                                             h2(strong("References")),
                                                             p("Tukey, J. W. (1977). Exploratory Data Analysis. Section 2C."),
                                                             p("McGill, R., Tukey, J. W. and Larsen, W. A. (1978). Variations of box plots. The American Statistician, 32, 12–16. doi:10.2307/2683468."),
                                                             p("Velleman, P. F. and Hoaglin, D. C. (1981). Applications, Basics and Computing of Exploratory Data Analysis. Duxbury Press.")
                                                           )
                                                    ),

                                )
                       )
                     )) # end mainpanel


  )
}

#' qaStaApp Server Functions
#'
#' @noRd
mod_qaStaApp_server <- function(id, data){
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
      dtQaMb <- data()
      dtQaMb <- dtQaMb$metadata$pheno
      traitsQaMb <- paste0(unique(dtQaMb[dtQaMb$parameter=="trait","value"]),"-residual")
      traitsQaMb <- intersect(colnames(data()$data$pheno),traitsQaMb)
      updateSelectInput(session, "traitOutqPheno",choices = traitsQaMb)
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
      originalTraitName <- gsub("-residual","",input$traitOutqPheno) # original names
      if(!is.null(myoutliers) & !is.null(nrow(myoutliers)) ){ # there's previous outliers for this trait
        outsItrait <- which(myoutliers$trait == originalTraitName)
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
          outlier <- grDevices::boxplot.stats(x=scale(sampleDT[, input$traitOutqPheno]),coef=input$outlierCoefOutqPheno )$out
          toSilence <- sampleDT[which(scale(sampleDT[,input$traitOutqPheno]) %in% outlier),"rowindex"]
          typeOut <- rep("outlierResidual",length(toSilence))
        }else{
          toSilence <- numeric()
          typeOut <- character()
        }
        outOfBounds <- which(( scale(sampleDT[, input$traitOutqPheno]) < input$traitLBOutqPheno) | ( scale(sampleDT[, input$traitOutqPheno]) > input$traitUBOutqPheno ) )
        if(length(outOfBounds) > 0){toSilence <- c(toSilence, sampleDT[outOfBounds,"rowindex"]); typeOut <- c(typeOut, rep("outlierResidual",length(outOfBounds))) }
        if(length(toSilence) > 0){
          outList[[counter]] <- data.frame(module="qaMb",analysisId=analysisId,trait=originalTraitName,reason=typeOut,row=toSilence, value=NA);
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
      mydata$environment <- cgiarBase::cleanCharField(mydata$environment)#
      fields <- as.character(unique(mydata$environment))
      for(uField in fields){ # scale the residuals by field
        vf <- which(mydata$environment == uField)
        mydata[vf,input$traitOutqPheno2] <- scale(mydata[vf,input$traitOutqPheno2])
      }
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
        dtQaMb <- data()
        dtQaMb <- dtQaMb$data$pheno
        dtQaMb$outlierRow <- 1:nrow(dtQaMb)
        removeCols <- c("stage","pipeline","country","year","season","location","trial","gid")
        dtQaMb <- dtQaMb[,unique(c("outlierRow",setdiff(colnames(dtQaMb), removeCols)))]
        ## merge
        myTable <- base::merge(outlier,dtQaMb, by.x="record", by.y="outlierRow", all.x=TRUE)
        DT::datatable(myTable, extensions = 'Buttons',
                                      options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                     lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All')))
        )
      })

    })

    ## save when user clicks

    outQaMb <- eventReactive(input$runQaMb, {

      req(data())
      req(input$outlierCoefOutqFont)
      req(input$traitOutqPheno)
      ## get the outlier table
      outlier <- newOutliers()
      ## get data structure
      temp <- data()
      ## update analsisId in the outliers table
      analysisId <- as.numeric(Sys.time())
      outlier[which(is.na(outlier$analysisId)),"analysisId"] <- analysisId
      # ## bind new parameters
      temp$modifications$pheno <- rbind(temp$modifications$pheno, outlier[, colnames(temp$modifications)])
      newStatus <- data.frame(module="qaMb", analysisId=analysisId )
      temp$status <- rbind(temp$status, newStatus)
      data(temp)
      print(data()$status)


    })
    output$outQaMb <- renderPrint({
      outQaMb()
    })


  })
}

## To be copied in the UI
# mod_qaStaApp_ui("qaStaApp_1")

## To be copied in the server
# mod_qaStaApp_server("qaStaApp_1")

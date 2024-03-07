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
    # shiny::sidebarPanel( style = "height:690px; overflow-y: scroll;overflow-x: scroll;",
    #   width = 3,
    #   tags$style(".well {background-color:grey; color: #FFFFFF;}"),
    #   # div(tags$p( h4(strong("Model-based outlier detection")))),#, style = "color: #817e7e"
    #   HTML("<img src='www/cgiar3.png' width='42' vspace='10' hspace='10' height='46' align='top'>
    #               <font size='5'>Model-based outlier detection</font>"),
    #   hr(style = "border-top: 1px solid #4c4c4c;"),
    #
    #
    #   hr(style = "border-top: 1px solid #4c4c4c;"),
    #
    #   hr(style = "border-top: 1px solid #4c4c4c;"),
    #
    # ), # end sidebarpanel
    shiny::mainPanel(width = 12,
                     tabsetPanel( #width=9,
                       type = "tabs",
                       tabPanel(div(icon("book"), "Information-QA-MB") ,
                                br(),
                                shinydashboard::box(status="success",width = 12,
                                                    solidHeader = TRUE,
                                                    column(width=12,   style = "height:580px; overflow-y: scroll;overflow-x: scroll;",
                                                           tags$body(
                                                             h1(strong(span("Model-based outlier detection", style="color:green"))),
                                                             h2(strong("Status:")),
                                                             uiOutput(ns("warningMessage")),
                                                             h2(strong("Details")),
                                                             p("The two-step approach of genetic evaluation allows to identify noisy records after the single trial analysis.
                                                             This option aims to allow users to select model-based outliers based on plot whiskers and absolute values applied on conditional residuals.
                                The way arguments are used is the following:"),
                                                             img(src = "www/qaSta.png", height = 300, width = 600), # add an image
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
                       ),
                       tabPanel(div(icon("arrow-right-to-bracket"), "Input"),
                                tabsetPanel(
                                  tabPanel("Set traits & thresholds", icon = icon("magnifying-glass-chart"),
                                           br(),
                                           column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                  column(width=6, selectInput(ns("traitOutqPheno"), "Trait to QA", choices = NULL, multiple = FALSE) ),
                                                  column(width=2, numericInput(ns("traitLBOutqPheno"), label = "Trait lower bound (STD)", value = -4) ),
                                                  column(width=2, numericInput(ns("traitUBOutqPheno"), label = "Trait upper bound (STD)", value = 4) ),
                                                  column(width=2, numericInput(ns("outlierCoefOutqPheno"), label = "Outlier coefficient", value = 5) ),
                                                  ),
                                           h4(strong(span("Visualizations below aim to help you pick the right parameter values. Please inspect them.", style="color:green"))),
                                           hr(style = "border-top: 3px solid #4c4c4c;"),
                                           shinydashboard::box(status="success",width = 12,
                                                               solidHeader = TRUE,
                                                               column(width=12, style = "height:450px; overflow-y: scroll;overflow-x: scroll;",
                                                                      p(span("Preview of outliers that would be tagged using current input parameters above for trait selected.", style="color:black")),
                                                                      shiny::plotOutput(ns("plotPredictionsCleanOut")), # plotly::plotlyOutput(ns("plotPredictionsCleanOut")),
                                                                      shinydashboard::box(width = 12, status = "success", background="green",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Plot settings...",
                                                                                          numericInput(ns("outlierCoefOutqFont"), label = "x-axis font size", value = 12, step=1)
                                                                      ),
                                                                      DT::DTOutput(ns("modificationsQa")),
                                                                      )
                                           )
                                  ),
                                  tabPanel("Run analysis", icon = icon("play"),
                                           br(),
                                           actionButton(ns("runQaMb"), "Tag outliers", icon = icon("play-circle")),
                                           textOutput(ns("outQaMb")),
                                  ),
                                ) # end of tabset
                       ),# end of input panel
                       tabPanel(div(icon("arrow-right-from-bracket"), "Output" ) , value = "outputTabs",
                                tabsetPanel(
                                  tabPanel("Report", icon = icon("file-image"),
                                           br(),
                                           div(tags$p("Please download the report below:") ),
                                           downloadButton(ns("downloadReportQaPheno"), "Download report"),
                                           br(),
                                           uiOutput(ns('reportQaPheno'))
                                  ),
                                ),
                       ), # end of output panel
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
    # warning message
    output$warningMessage <- renderUI(
      if(is.null(data())){
        HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your phenotypic data using the 'Data' tab.")) )
      }else{ # data is there
        mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
        if(mappedColumns == 3){
          if("sta" %in% data()$status$module){
            HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to identify outliers specifying your input parameters under the 'Input' tabs.")) )
          }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please perform the single trial analysis before performing the QA model-based outlier detection.")) ) }
        }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please make sure that you have computed the 'environment' column, and that column 'designation' and \n at least one trait have been mapped using the 'Data Retrieval' tab.")) )}
      }
    )
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
    # function to compute new outliers
    newOutliers <- reactive({
      req(data())
      myObject <- data()
      mydata <- myObject$data$pheno
      ### change column names for mapping
      paramsPheno <- data()$metadata$pheno
      paramsPheno <- paramsPheno[which(paramsPheno$parameter != "trait"),]
      colnames(mydata) <- cgiarBase::replaceValues(colnames(mydata), Search = paramsPheno$value, Replace = paramsPheno$parameter )
      ###
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

    output$plotPredictionsCleanOut <- shiny::renderPlot({ # plotly::renderPlotly({
      req(data())
      req(input$outlierCoefOutqFont)
      req(input$traitOutqPheno)
      if("sta" %in% data()$status$module){
        mydata <- data()$data$pheno
        ### change column names for mapping
        paramsPheno <- data()$metadata$pheno
        paramsPheno <- paramsPheno[which(paramsPheno$parameter != "trait"),]
        colnames(mydata) <- cgiarBase::replaceValues(colnames(mydata), Search = paramsPheno$value, Replace = paramsPheno$parameter )
        ###
        mydata$rowindex <- 1:nrow(mydata)
        mydata[, "environment"] <- as.factor(mydata[, "environment"])
        mydata[, "designation"] <- as.factor(mydata[, "designation"])
        mo <- newOutliers()
        mydata$color <- "valid"
        if(nrow(mo) > 0){mydata$color[which(mydata$rowindex %in% unique(mo$row))]="tagged"}
        # mydata$color <- 1
        # if(nrow(mo) > 0){
        #   mydata$color[which(mydata$rowindex %in% unique(mo$row))]=2
        # }
        # mydata$color <- as.factor(mydata$color)
        mydata$environment <- cgiarBase::cleanCharField(mydata$environment)#
        fields <- as.character(unique(mydata$environment))
        for(uField in fields){ # scale the residuals by field
          vf <- which(mydata$environment == uField)
          mydata[vf,input$traitOutqPheno2] <- scale(mydata[vf,input$traitOutqPheno2])
        }
        # res <- plotly::plot_ly(y = mydata[,input$traitOutqPheno], type = "box", boxpoints = "all", jitter = 0.3,color=mydata[,"color"],
        #                        x = mydata[,"environment"], text=mydata[,"designation"],
        #                        pointpos = -1.8)
        # res = res %>% plotly::layout(showlegend = FALSE,
        #                              xaxis = list(titlefont = list(size = input$outlierCoefOutqFont), tickfont = list(size = input$outlierCoefOutqFont))
        # )
        # res
        mydata$predictedValue <- mydata[,input$traitOutqPheno]
        ggplot2::ggplot(mydata, ggplot2::aes(x=as.factor(environment), y=predictedValue)) +
          ggplot2::geom_boxplot(fill='#A4A4A4', color="black", notch = TRUE)+
          ggplot2::theme_classic()+
          ggplot2::geom_jitter(ggplot2::aes(colour = color), alpha = 0.4) +
          ggplot2::xlab("Environment") + ggplot2::ylab("Standardized residual") +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45))

      }
    })

    ## display the current outliers
    observeEvent(data(),{

      output$modificationsQa <-  DT::renderDT({

        req(data())
        req(input$outlierCoefOutqFont)
        req(input$traitOutqPheno)
        if("sta" %in% data()$status$module){
          ## get the outlier table
          outlier <- newOutliers()
          removeCols <- c("module","analysisId","value")
          outlier <- outlier[, setdiff(colnames(outlier),removeCols)]
          colnames(outlier) <- cgiarBase::replaceValues(Source = colnames(outlier), Search = "row", Replace = "record")
          ## add phenotypic data
          dtQaMb <- data()
          dtQaMb <- dtQaMb$data$pheno
          ### change column names for mapping
          paramsPheno <- data()$metadata$pheno
          paramsPheno <- paramsPheno[which(paramsPheno$parameter != "trait"),]
          colnames(dtQaMb) <- cgiarBase::replaceValues(colnames(dtQaMb), Search = paramsPheno$value, Replace = paramsPheno$parameter )
          ###
          dtQaMb$outlierRow <- 1:nrow(dtQaMb)
          removeCols <- c("stage","pipeline","country","year","season","location","trial","gid")
          dtQaMb <- dtQaMb[,unique(c("outlierRow",setdiff(colnames(dtQaMb), removeCols)))]
          ## merge
          myTable <- base::merge(outlier,dtQaMb, by.x="record", by.y="outlierRow", all.x=TRUE)
          DT::datatable(myTable, extensions = 'Buttons',
                        options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                       lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
          )
        }
      })

    })

    ## save when user clicks

    outQaMb <- eventReactive(input$runQaMb, {

      req(data())
      req(input$outlierCoefOutqFont)
      req(input$traitOutqPheno)
      if("sta" %in% data()$status$module){
        shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
        ## get the outlier table
        outlier <- newOutliers()
        if(nrow(outlier) > 0){
          ## get data structure
          result <- data()
          ## update analsisId in the outliers table
          analysisId <- as.numeric(Sys.time())
          outlier[which(is.na(outlier$analysisId)),"analysisId"] <- analysisId # [which(is.na(outlier$analysisId)),"analysisId"]
          # ## bind new parameters
          result$modifications$pheno <- rbind(result$modifications$pheno, outlier[, colnames(result$modifications$pheno)])
          newStatus <- data.frame(module="qaMb", analysisId=analysisId )
          result$status <- rbind(result$status, newStatus)
          data(result)
          cat(paste("Modifications to phenotype information saved with id:",analysisId))
        }else{
          cat("No modifications to add")
        }
        shinybusy::remove_modal_spinner()

        if(!inherits(result,"try-error")) { # if all goes well in the run
          # ## Report tab
          output$reportQaPheno <- renderUI({
            HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportQaPheno.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
          })

          output$downloadReportQaPheno <- downloadHandler(
            filename = function() {
              paste('my-report', sep = '.', switch(
                "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
              ))
            },
            content = function(file) {
              src <- normalizePath(system.file("rmd","reportQaPheno.Rmd",package="bioflow"))
              src2 <- normalizePath('data/resultQaPheno.RData')
              # temporarily switch to the temp dir, in case you do not have write
              # permission to the current working directory
              owd <- setwd(tempdir())
              on.exit(setwd(owd))
              file.copy(src, 'report.Rmd', overwrite = TRUE)
              file.copy(src2, 'resultQaPheno.RData', overwrite = TRUE)
              out <- rmarkdown::render('report.Rmd', params = list(toDownload=TRUE),switch(
                "HTML",
                HTML = rmarkdown::html_document()
              ))
              file.rename(out, file)
            }
          )

        }else{ hideAll$clearAll <- TRUE}

        hideAll$clearAll <- FALSE

      }
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

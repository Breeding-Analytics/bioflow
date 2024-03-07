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

    shiny::mainPanel(width = 12,
                     tabsetPanel( #width=9,
                       type = "tabs",

                       tabPanel(div(icon("book"), "Information-QA") ,
                                br(),
                                shinydashboard::box(status="success",width = 12,
                                                    solidHeader = TRUE,
                                                    column(width=12,   style = "height:580px; overflow-y: scroll;overflow-x: scroll;",
                                                           h1(strong(span("Outlier detection", style="color:green"))),
                                                           h2(strong("Status:")),
                                                           uiOutput(ns("warningMessage")),
                                                           tags$body(
                                                             h2(strong("Details")),
                                                             p("The first step in genetic evaluation is to ensure that input phenotypic records are of good quality.
                                                             This option aims to allow users to select outliers based on plot whiskers and absolute values.
                                The way arguments are used is the following:"),
                                                             img(src = "www/qaRaw.png", height = 300, width = 600), # add an image
                                                             p(strong("Trait(s) to QA.-")," Trait(s) to apply jointly the parameter values in the grey box. If you wish to apply diffrent values to different traits please go one trait at the time."),
                                                             p(strong("Outlier coefficient.-")," this determines how far the plot whiskers extend out from the box. If coef is positive, the whiskers extend to the most extreme data point which is no more than coef times the length of the box away from the box. A value of zero causes the whiskers to extend to the data extremes (and no outliers be returned)."),
                                                             p(strong("Trait lower bound.-"),"Lower bound threshold determining as an outlier any value smaller than this."),
                                                             p(strong("Trait upper bound.-"),"Upper bound threshold determining as an outlier any value greater than this."),
                                                             h2(strong("References")),
                                                             p("Tukey, J. W. (1977). Exploratory Data Analysis. Section 2C."),
                                                             p("McGill, R., Tukey, J. W. and Larsen, W. A. (1978). Variations of box plots. The American Statistician, 32, 12–16. doi:10.2307/2683468."),
                                                             p("Velleman, P. F. and Hoaglin, D. C. (1981). Applications, Basics and Computing of Exploratory Data Analysis. Duxbury Press.")
                                                           )
                                                    )
                                )
                       ),
                       tabPanel(div(icon("arrow-right-to-bracket"), "Input"),
                                tabsetPanel(
                                  tabPanel("Set traits & thresholds", icon = icon("magnifying-glass-chart"),
                                           br(),
                                           column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                  column(width=6, selectInput(ns("traitOutqPhenoMultiple"), "Trait(s) to QA", choices = NULL, multiple = TRUE) ),
                                                  column(width=2, numericInput(ns("traitLBOutqPheno"), label = "Lower bound", value = 0.01) ),
                                                  column(width=2, numericInput(ns("traitUBOutqPheno"), label = "Upper bound", value = 100000) ),
                                                  column(width=2,numericInput(ns("outlierCoefOutqPheno"), label = "IQR coefficient", value = 2.5) ),
                                                  ),
                                           h4(strong(span("Visualizations below aim to help you pick the right parameter values. Please inspect them.", style="color:green"))),
                                           hr(style = "border-top: 3px solid #4c4c4c;"),
                                           shinydashboard::box(status="success",width = 12, solidHeader = TRUE,
                                                               column(width=12, style = "height:410px; overflow-y: scroll;overflow-x: scroll;",
                                                                      p(span("Preview of outliers that would be tagged using current input parameters above for the trait selected.", style="color:black")),
                                                                      selectInput(ns("traitOutqPheno"), "", choices = NULL, multiple = FALSE),
                                                                      plotly::plotlyOutput(ns("plotPredictionsCleanOut")),
                                                                      shinydashboard::box(width = 12, status = "success", background="green",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Plot settings...",
                                                                                          numericInput(ns("outlierCoefOutqFont"), label = "x-axis font size", value = 12, step=1)
                                                                      ),
                                                                      p(span("Table preview of outliers that would be tagged using current input parameters above for the trait selected.", style="color:black")),
                                                                      DT::DTOutput(ns("modificationsQa")),
                                                               ),
                                           )
                                  ),
                                  tabPanel("Run analysis", icon = icon("play"),
                                           br(),
                                           actionButton(ns("runQaRaw"), "Tag outliers", icon = icon("play-circle")),
                                           textOutput(ns("outQaRaw")),
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
                       ),
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
    # warning message
    output$warningMessage <- renderUI(
      if(is.null(data())){
        HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your phenotypic data using the 'Data Retrieval' tab.")) )
      }else{ # data is there
        mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
        if(mappedColumns == 3){ HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to identify outliers specifying your input parameters under the 'Input' tabs.")) )
        }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please make sure that you have computed the 'environment' column, and that column 'designation' and \n at least one trait have been mapped using the 'Data Retrieval' tab.")) )
        }
      }
    )
    # Create the fields
    observeEvent(data(), {
      req(data())
      dtQaRaw <- data()
      dtQaRaw <- dtQaRaw$metadata$pheno
      traitsQaRaw <- unique(dtQaRaw[dtQaRaw$parameter=="trait","value"])
      updateSelectInput(session, "traitOutqPheno",choices = traitsQaRaw)
      updateSelectInput(session, "traitOutqPhenoMultiple",choices = traitsQaRaw, selected = NULL)
      shinyjs::hide(ns("traitOutqPheno"))
    })
    ## render the expected result
    output$plotPredictionsCleanOut <- plotly::renderPlotly({
      req(data())
      req(input$outlierCoefOutqFont)
      req(input$outlierCoefOutqPheno)
      req(input$traitLBOutqPheno)
      req(input$traitUBOutqPheno)
      req(input$traitOutqPheno)
      mydata <- data()$data$pheno
      ### change column names for mapping
      paramsPheno <- data()$metadata$pheno
      paramsPheno <- paramsPheno[which(paramsPheno$parameter != "trait"),]
      colnames(mydata) <- cgiarBase::replaceValues(colnames(mydata), Search = paramsPheno$value, Replace = paramsPheno$parameter )
      ###
      mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
      if(mappedColumns == 3){ # all required columns are present
        mydata$rowindex <- 1:nrow(mydata)
        mydata[, "environment"] <- as.factor(mydata[, "environment"])
        mydata[, "designation"] <- as.factor(mydata[, "designation"])
        # mo <- newOutliers()
        mo <- cgiarPipeline::newOutliersFun(myObject=data(), trait=input$traitOutqPheno, outlierCoefOutqPheno=input$outlierCoefOutqPheno, traitLBOutqPheno=input$traitLBOutqPheno, traitUBOutqPheno=input$traitUBOutqPheno) # newOutliers()
        mydata$color <- 1
        if(nrow(mo) > 0){mydata$color[which(mydata$rowindex %in% unique(mo$row))]=2}
        mydata$color <- as.factor(mydata$color)
        res <- plotly::plot_ly(y = mydata[,input$traitOutqPheno], type = "box", boxpoints = "all", jitter = 0.3,color=mydata[,"color"],
                               x = mydata[,"environment"], text=mydata[,"designation"],
                               pointpos = -1.8)
        res = res %>% plotly::layout(showlegend = FALSE,  xaxis = list(titlefont = list(size = input$outlierCoefOutqFont), tickfont = list(size = input$outlierCoefOutqFont)))
        res
      }else{}
    })

    ## display the current outliers
    observeEvent(data(),{

      output$modificationsQa <-  DT::renderDT({

        req(data())
        mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
        if(mappedColumns == 3){ # all required columns are present
          req(input$outlierCoefOutqFont)
          req(input$outlierCoefOutqPheno)
          req(input$traitLBOutqPheno)
          req(input$traitUBOutqPheno)
          req(input$traitOutqPheno)
          ## get the outlier table
          outlier <- cgiarPipeline::newOutliersFun(myObject=data(), trait=input$traitOutqPheno, outlierCoefOutqPheno=input$outlierCoefOutqPheno, traitLBOutqPheno=input$traitLBOutqPheno, traitUBOutqPheno=input$traitUBOutqPheno) # newOutliers()
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
          DT::datatable(myTable, extensions = 'Buttons',
                        options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                       lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
          )
        }

      })

    })

    ## save when user clicks

    outQaRaw <- eventReactive(input$runQaRaw, {

      req(data())
      req(input$traitOutqPhenoMultiple)
      mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
      if(mappedColumns == 3){ # all required columns are present
        req(input$outlierCoefOutqFont)
        req(input$traitOutqPhenoMultiple)
        req(input$outlierCoefOutqPheno)
        req(input$traitLBOutqPheno)
        req(input$traitUBOutqPheno)
        shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
        ## get the outlier table
        outlier <- list()
        for(iTrait in input$traitOutqPhenoMultiple){
          outlier[[iTrait]] <- cgiarPipeline::newOutliersFun(myObject=data(), trait=iTrait, outlierCoefOutqPheno=input$outlierCoefOutqPheno, traitLBOutqPheno=input$traitLBOutqPheno, traitUBOutqPheno=input$traitUBOutqPheno)#newOutliers()
        }
        outlier <- do.call(rbind,outlier)
        if(nrow(outlier) > 0){ # if there's new outliers
          ## get data structure
          result <- data()
          ## update analsisId in the outliers table
          analysisId <- as.numeric(Sys.time())
          outlier[which(is.na(outlier$analysisId)),"analysisId"] <- analysisId
          # ## bind new parameters
          if(is.null(result$modifications$pheno )){
            result$modifications$pheno <- outlier
          }else{
            result$modifications$pheno <- rbind(result$modifications$pheno, outlier[, colnames(result$modifications$pheno)])
          }
          # add status table
          newStatus <- data.frame(module="qaRaw", analysisId=analysisId )
          result$status <- rbind(result$status, newStatus)
          # add modeling table
          provMet <- list()
          for(iTrait in input$traitOutqPhenoMultiple){
            provMet[[iTrait]] <- data.frame(module="qaRaw",analysisId=analysisId, trait=iTrait, environment=NA,
                                            parameter=c("traitLBOutqPheno","traitUBOutqPheno","outlierCoefOutqPheno"),
                                            value= c(input$traitLBOutqPheno, input$traitUBOutqPheno, input$outlierCoefOutqPheno) )
          }
          provMet <- do.call(rbind,provMet)
          if(is.null(result$modeling)){
            result$modeling <- provMet
          }else{
            result$modeling <- rbind(result$modeling, provMet[,colnames(result$modeling)])
          }
          data(result)
          cat(paste("Modifications table saved with id:",as.POSIXct( analysisId, origin="1970-01-01", tz="GMT"),"for trait",paste(input$traitOutqPhenoMultiple, collapse = ", "),". Please use this tag to exclude these outliers in the single trial analysis. "))
        }else{

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

      }else{
        cat("Please meet the data conditions before you identify and save outliers.")
      }      # save(result, file="toTest.RData")

    })

    output$outQaRaw <- renderPrint({
      outQaRaw()
    })




  })
}

## To be copied in the UI
# mod_qaRawApp_ui("qaRawApp_1")

## To be copied in the server
# mod_qaRawApp_server("qaRawApp_1")

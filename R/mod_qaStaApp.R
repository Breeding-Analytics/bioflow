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
                                                    column(width=12,   style = "height:660px; overflow-y: scroll;overflow-x: scroll;",
                                                           tags$body(
                                                             h1(strong(span("Model-based outlier detection", style="color:green"))),
                                                             h2(strong("Status:")),
                                                             uiOutput(ns("warningMessage")),
                                                             h2(strong("Details")),
                                                             p("The two-step approach of genetic evaluation allows to identify noisy records after the single trial analysis.
                                                             This option aims to allow users to select model-based outliers based on plot whiskers and absolute values applied on conditional residuals.
                                The way arguments are used is the following:"),
                                                             img(src = "www/qaSta.png", height = 300, width = 700), # add an image
                                                             p(strong("Trait(s) residuals to QA.-")," Trait(s) residuals to apply jointly the parameter values in the grey box."),
                                                             p(strong("Outlier coefficient.-")," this determines how far the plot whiskers extend out from the box. If coef is positive, the whiskers extend to the most extreme data point which is no more than coef times the length of the box away from the box. A value of zero causes the whiskers to extend to the data extremes (and no outliers be returned)."),
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
                                                  column(width=6, selectInput(ns("traitOutqPhenoMultiple"), "Trait to QA", choices = NULL, multiple = TRUE) ),
                                                  column(width=2, numericInput(ns("outlierCoefOutqPheno"), label = "Outlier coefficient", value = 5) ),
                                           ),
                                           h4(strong(span("Visualizations below aim to help you pick the right parameter values. Please inspect them.", style="color:green"))),
                                           hr(style = "border-top: 3px solid #4c4c4c;"),
                                           shinydashboard::box(status="success",width = 12, solidHeader = TRUE,
                                                               column(width=12, style = "height:470px; overflow-y: scroll;overflow-x: scroll;",
                                                                      p(span("Preview of outliers that would be tagged using current input parameters above for trait selected.", style="color:black")),
                                                                      selectInput(ns("traitOutqPheno2"), "", choices = NULL, multiple = FALSE),
                                                                      shiny::plotOutput(ns("plotPredictionsCleanOut")), # plotly::plotlyOutput(ns("plotPredictionsCleanOut")),
                                                                      shinydashboard::box(width = 12, status = "success", background="green",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Plot settings...",
                                                                                          numericInput(ns("outlierCoefOutqFont"), label = "x-axis font size", value = 12, step=1)
                                                                      ),
                                                                      DT::DTOutput(ns("modificationsQa"))
                                                               ),
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
      updateSelectInput(session, "traitOutqPheno2",choices = traitsQaMb)
      updateSelectInput(session, "traitOutqPhenoMultiple", choices = traitsQaMb, selected = NULL)
      shinyjs::hide(ns("traitOutqPheno2"))
    })

    ## render the expected result

    output$plotPredictionsCleanOut <- shiny::renderPlot({ # plotly::renderPlotly({
      if("sta" %in% data()$status$module){
        req(data())
        req(input$outlierCoefOutqFont)
        req(input$traitOutqPheno2)
        mydata <- data()$data$pheno
        ### change column names for mapping
        paramsPheno <- data()$metadata$pheno
        paramsPheno <- paramsPheno[which(paramsPheno$parameter != "trait"),]
        colnames(mydata) <- cgiarBase::replaceValues(colnames(mydata), Search = paramsPheno$value, Replace = paramsPheno$parameter )
        ###
        mydata$rowindex <- 1:nrow(mydata)
        mydata[, "environment"] <- as.factor(mydata[, "environment"])
        mydata[, "designation"] <- as.factor(mydata[, "designation"])
        mo <- cgiarPipeline::newOutliersFun(myObject=data(), trait=input$traitOutqPheno2, outlierCoefOutqPheno=input$outlierCoefOutqPheno)
        mydata$color <- "valid"
        if(nrow(mo) > 0){mydata$color[which(mydata$rowindex %in% unique(mo$row))]="tagged"}
      }
      mydata$predictedValue <- mydata[,input$traitOutqPheno2]
      ggplot2::ggplot(mydata, ggplot2::aes(x=as.factor(environment), y=predictedValue)) +
        ggplot2::geom_boxplot(fill='#A4A4A4', color="black", notch = TRUE, outliers = FALSE)+
        ggplot2::theme_classic()+
        ggplot2::geom_jitter(ggplot2::aes(colour = color), alpha = 0.4) +
        ggplot2::xlab("Environment") + ggplot2::ylab("Residual value") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45)) +
        ggplot2::scale_color_manual(values = c(valid = "#66C2A5", tagged = "#FC8D62")) # specifying colors names avoids having valid points in orange in absence of potential outliers. With only colour = color, valid points are in orange in that case.

    })

    ## display the current outliers
    observeEvent(data(),{
      output$modificationsQa <-  DT::renderDT({
        if("sta" %in% data()$status$module){
          req(data())
          req(input$outlierCoefOutqFont)
          req(input$traitOutqPhenoMultiple)
          ## get the outlier table
          outlier <- list()
          for(iTrait in input$traitOutqPhenoMultiple){
            outlier[[iTrait]] <- cgiarPipeline::newOutliersFun(myObject=data(), trait=iTrait, outlierCoefOutqPheno=input$outlierCoefOutqPheno)
          }
          outlier <- do.call(rbind,outlier)
          outlier$reason <- "outlierResidual"
          #outlier <- newOutliers()
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
          DT::datatable(myTable, #extensions = 'Buttons', filter = "top",
                        caption = htmltools::tags$caption(
                          style = 'color:orange', #caption-side: bottom; text-align: center;
                          htmltools::em('Model-based outliers for the selected traits.')
                        ),
          )
        }
      })
    })

    ## save when user clicks

    outQaMb <- eventReactive(input$runQaMb, {
      if("sta" %in% data()$status$module){
        req(data())
        req(input$outlierCoefOutqFont)
        req(input$traitOutqPhenoMultiple)
        shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
        ## get the outlier table
        outlier <- list()
        newParamsPheno <- list()
        for(iTrait in input$traitOutqPhenoMultiple){
          outlier[[iTrait]] <- cgiarPipeline::newOutliersFun(myObject=data(), trait=iTrait, outlierCoefOutqPheno=input$outlierCoefOutqPheno)
          newParamsPheno[[iTrait]] <- data.frame(module="qaMb",analysisId=NA, trait=iTrait, environment=NA, parameter= "outlierCoefOutqPheno", value= input$outlierCoefOutqPheno)
        }
        outlier <- do.call(rbind,outlier)
        outlier$trait <- gsub("-residual", "", outlier$trait)
        outlier$module <- "qaMb"
        outlier$reason <- "outlierResidual"
        ## get data structure
        result <- data()
        for (iTrait in unique(outlier$trait)) {
          outlier$value[outlier$trait == iTrait] <- result$data$pheno[outlier$row[outlier$trait==iTrait],iTrait]
        }
        newParamsPheno <- do.call(rbind,newParamsPheno)
        newParamsPheno$trait <- gsub("-residual", "", newParamsPheno$trait)
        if(nrow(outlier) > 0){
          ## get data structure
          result <- data()
          ## update analsisId in the outliers table
          analysisId <- as.numeric(Sys.time())
          outlier[which(is.na(outlier$analysisId)),"analysisId"] <- analysisId
          ## bind new parameters
          if(sum(is.na(outlier$row)) != nrow(outlier)) {
            result$modifications$pheno <- rbind(result$modifications$pheno, outlier[, colnames(result$modifications$pheno)])
            newParamsPheno$analysisId <- as.POSIXct(analysisId, origin="1970-01-01", tz="GMT")
            result$modeling <- rbind(result$modeling, newParamsPheno)
            newStatus <- data.frame(module="qaMb", analysisId=analysisId)
            result$status <- rbind(result$status, newStatus)
            data(result)
            cat(paste("Modifications to phenotype information saved with id:",as.POSIXct( analysisId, origin="1970-01-01", tz="GMT")))
          }else{cat("No modifications to add")}
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

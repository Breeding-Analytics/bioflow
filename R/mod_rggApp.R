#' rggApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_rggApp_ui <- function(id){
  ns <- NS(id)
  tagList(


    sidebarPanel(
      # input <- list(version2Rgg="1699508839.68847",trait2Rgg="desireIndex",entryType2Rgg= "TGV_EST004D#TEST_tested",nCrossRgg="20", targetAngleRgg="30",maxRun=40, relType="grm", env2Rgg="across",verboseRgg=TRUE )
      tags$style(".well {background-color:grey; color: #FFFFFF;}"),
      HTML("<img src='www/cgiar3.png' width='42' vspace='10' hspace='10' height='46' align='top'>
                  <font size='5'>Realized Genetic Gain</font>"),
      # div(tags$p( h4(strong("Realized Genetic Gain")))),#, style = "color: #817e7e"
      hr(style = "border-top: 1px solid #4c4c4c;"),
      selectInput(ns("version2Rgg"), "Index or MTA version to analyze", choices = NULL, multiple = FALSE),
      selectInput(ns("trait2Rgg"), "Trait(s) to use", choices = NULL, multiple = TRUE),
      selectInput(ns("yearsToUse"), "Years of origin to use", choices = NULL, multiple = TRUE),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      shinydashboard::box(width = 12, status = "success", background="green",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Settings...",
                          numericInput(ns("deregressWeight"), label = "Deregression weight", value = 1),
                          selectInput(ns("deregress"), "Should deregress estimates", choices = list(TRUE,FALSE), selected = FALSE, multiple=FALSE),
                          selectInput(ns("partition"), "Partitioned regression", choices = list(TRUE,FALSE), selected = FALSE, multiple=FALSE),
                          selectInput(ns("verbose"), label = "Print logs?", choices = list(TRUE,FALSE), selected = FALSE, multiple=FALSE)
      ),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      actionButton(ns("runRgg"), "Run", icon = icon("play-circle")),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      uiOutput(ns("qaQcRggInfo")),
      textOutput(ns("outRgg"))
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
                                          p("In order to monitor the efficacy of genetic evaluation across cycles of selection, the realized genetic gain is the preferred process.
                                          This option aims to calculate the realized genetic gain using the methods from Mackay et al. (2011). The
                              method uses across-environment means from multiple years of data that have been adjusted based on a good connectivity
                              to then fit a regression of the form means~year.of.origin. In case the means used are BLUPs these can be
                              deregressed.
                                The way the options are used is the following:"),
                                          p(strong("Trait for cross prediction.-")," Trait to be be used for predicting all possible crosses (an index is suggested)."),
                                          p(strong("Fixed effects.-")," Fixed effects to add to the regression means~year.of.origin."),
                                          p(strong("Deregress data?.-")," A TRUE/FALSE value indicating if predicted values should be deregressed (to be set to TRUE when BLUPs are used)."),
                                          p(strong("Partition the data?.-")," When very few years of data are present this option will allow the user to calculate the gain for all 2 year combinations and then average these rates."),
                                          p(strong("Deregress weight.-")," Should any weight be applied to the deregressed value (not recommended to be used but available)."),
                                          h2(strong("References:")),
                                          p("Mackay, I., Horwell, A., Garner, J., White, J., McKee, J., & Philpott, H. (2011). Reanalyses of the historical series of UK variety trials
                                to quantify the contributions of genetic and environmental factors to trends and variability in yield over time. Theoretical and Applied
                                Genetics, 122, 225-238."),
                                          p("Laidig, F., Piepho, H. P., Drobek, T., & Meyer, U. (2014). Genetic and non-genetic long-term trends of 12 different crops in German
                                official variety performance trials and on-farm yield trends. Theoretical and Applied Genetics, 127, 2599-2617."),
                                          h2(strong("Software used:")),
                                          p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
                                Vienna, Austria. URL https://www.R-project.org/.")
                                   )
               )
      ),
      tabPanel(p("Input", class="input-p"), icon = icon("arrow-right-to-bracket"),
               br(),
               shinydashboard::box(status="success",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,DT::DTOutput(ns("phenoRgg")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
               )
      ),
      tabPanel(p("Output",class="output-p"), icon = icon("arrow-right-from-bracket"),
               tabsetPanel(
                 tabPanel("Metrics", icon = icon("table"),
                          br(),
                          shinydashboard::box(status="success",width = 12,
                                              solidHeader = TRUE,
                                              column(width=12,br(),DT::DTOutput(ns("metricsRgg")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;"),
                                              downloadButton(ns('downloadMetrics'), 'Download metrics')
                          )
                 ),
                 tabPanel("Modeling", icon = icon("table"),
                          br(),
                          shinydashboard::box(status="success",width = 12,
                                              solidHeader = TRUE,
                                              column(width=12,DT::DTOutput(ns("modelingRgg")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;"),
                                              downloadButton(ns('downloadModeling'), 'Download modeling')
                          )
                 ),
                 tabPanel("Report", icon = icon("file-image"),
                          br(),
                          div(tags$p("Please download the report below:") ),
                          downloadButton(ns("downloadReportRgg"), "Download report"),
                          br(),
                          # selectInput(ns("traitFilterPredictions2D2"), "Trait(s) to use", choices = NULL, multiple = TRUE),
                          # selectInput(ns("environment"), "environment to use", choices = NULL, multiple = TRUE),
                          # plotly::plotlyOutput(ns("plotPredictionsScatter")),
                          uiOutput(ns('reportRgg'))
                 )
               )
      )# end of output panel
    )) # end mainpanel


  )
}

#' rggApp Server Functions
#'
#' @noRd
mod_rggApp_server <- function(id, data){
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
    output$warningMessage <- renderUI(
      if(is.null(data())){
        HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your phenotypic data using the 'Data' tab.")) )
      }else{ # data is there
        mappedColumns <- length(which(c("yearOfOrigin") %in% colnames(data()$data$pedigree)))
        if(mappedColumns == 1){
          if("mta" %in% data()$status$module){
            HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to perform the realized genetic gain inspecting the other tabs.")) )
          }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please perform MTA before performing a realized genetic gain analysis.")) ) }
        }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please make sure that the column: 'yearOfOrigin' has been mapped in the pedigree data using the 'Data input' tab.")) )}
      }
    )
    #################
    ## version
    observeEvent(c(data()), {
      req(data())
      dtRgg <- data()
      dtRgg <- dtRgg$status
      dtRgg <- dtRgg[which(dtRgg$module %in% c("mta","indexD")),]
      traitsRgg <- unique(dtRgg$analysisId)
      updateSelectInput(session, "version2Rgg", choices = traitsRgg)
    })
    #################
    ## traits
    observeEvent(c(data(), input$version2Rgg), {
      req(data())
      req(input$version2Rgg)
      dtRgg <- data()
      dtRgg <- dtRgg$predictions
      dtRgg <- dtRgg[which(dtRgg$analysisId == input$version2Rgg),]
      traitsRgg <- unique(dtRgg$trait)
      updateSelectInput(session, "trait2Rgg", choices = traitsRgg)
    })
    # # trait for report tab
    # observeEvent(c(data(), input$version2Rgg), {
    #   req(data())
    #   req(input$version2Rgg)
    #   dtRgg <- data()
    #   dtRgg <- dtRgg$predictions
    #   dtRgg <- dtRgg[which(dtRgg$analysisId == input$version2Rgg),]
    #   traitsRgg <- unique(dtRgg$trait)
    #   updateSelectInput(session, "traitFilterPredictions2D2", choices = traitsRgg)
    # })
    # # environment for report tab
    # observeEvent(c(data(), input$version2Rgg), {
    #   req(data())
    #   req(input$version2Rgg)
    #   dtRgg <- data()
    #   dtRgg <- dtRgg$predictions
    #   dtRgg <- dtRgg[which(dtRgg$analysisId == input$version2Rgg),]
    #   traitsRgg <- unique(dtRgg$environment)
    #   updateSelectInput(session, "environment", choices = traitsRgg)
    # })
    ##############
    ## entry type
    observeEvent(c(data(), input$version2Rgg, input$trait2Rgg), {
      req(data())
      req(input$version2Rgg)
      req(input$trait2Rgg)
      dtRgg <- data()
      traitsRgg <- unique(dtRgg$data$pedigree$yearOfOrigin)
      updateSelectInput(session, "yearsToUse", choices = traitsRgg, selected =traitsRgg )
    })

    ##############################################################################################
    ##############################################################################################
    ##############################################################################################
    ## render the data to be analyzed
    output$phenoRgg <-  DT::renderDT({
      req(data())
      req(input$version2Rgg)
      dtRgg <- data()
      dtRgg <- dtRgg$predictions
      dtRgg <- dtRgg[which(dtRgg$analysisId == input$version2Rgg),setdiff(colnames(dtRgg),c("module","analysisId"))]
      numeric.output <- c("predictedValue", "stdError", "reliability")
      DT::formatRound(DT::datatable(dtRgg, extensions = 'Buttons',
                                    options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                   lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
      ), numeric.output)
    })
    ## render result of "run" button click
    outRgg <- eventReactive(input$runRgg, {
      req(data())
      req(input$version2Rgg)
      req(input$trait2Rgg)
      req(input$yearsToUse)
      shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
      dtRgg <- data()

      # run the modeling, but before test if mta was done
      if(sum(dtRgg$status$module %in% c("mta","indexD")) == 0) {
        output$qaQcRggInfo <- renderUI({
          if (hideAll$clearAll){
            return()
          }else{
            req(dtRgg)
            HTML(as.character(div(style="color: brown;",
                                  "Please perform Multi-Trial-Analysis or Selection Index before conducting Optimal Cross Selection."))
            )
          }
        })
      }else{
        output$qaQcRggInfo <- renderUI({return(NULL)})
        result <- try(cgiarPipeline::rgg(
          phenoDTfile= dtRgg,
          analysisId=input$version2Rgg,
          trait=input$trait2Rgg, # per trait
          deregressWeight=input$deregressWeight,
          deregress=input$deregress,
          partition=input$partition,
          yearsToUse=input$yearsToUse,
          verbose=input$verbose
        ),
        silent=TRUE
        )
        if(!inherits(result,"try-error")) {
          data(result) # update data with results
          save(result, file = "./R/outputs/resultRgg.RData")
          cat(paste("Realized genetic gain step with id:",result$status$analysisId[length(result$status$analysisId)],"saved."))
        }else{
          cat(paste("Analysis failed with the following error message: \n\n",result[[1]]))
        }
      }
      shinybusy::remove_modal_spinner()

      if(!inherits(result,"try-error")) {

        # view metrics
        output$metricsRgg <-  DT::renderDT({
          # if ( hideAll$clearAll){
          #   return()
          # }else{
          metrics <- result$metrics
          metrics <- metrics[metrics$module=="rgg",]
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
        output$modelingRgg <-  DT::renderDT({
          # if ( hideAll$clearAll){
          #   return()
          # }else{
          modeling <- result$modeling
          modeling <- modeling[modeling$module=="rgg",]
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

        # output$plotPredictionsScatter <-  plotly::renderPlotly({
        #   mydata = result$predictions
        #   mydata <- mydata[which(mydata$analysisId == input$version2Rgg),]
        #   mydata = mydata[which(mydata$trait == input$traitFilterPredictions2D2),]
        #   mydata = mydata[which(mydata$environment == input$environment),]
        #   if(!is.null(result$data$pedigree$yearOfOrigin)){
        #     mydata <- merge(mydata, result$data$pedigree[,c("designation","yearOfOrigin")], by="designation", all.x=TRUE )
        #     mydata <- mydata[which(!is.na(mydata$yearOfOrigin)),]
        #     res = plotly::plot_ly(data = mydata, x = mydata[,"yearOfOrigin"], y = mydata[,"predictedValue"])
        #     # calculate current metrics
        #     metrics <- result$metrics
        #     metrics <- metrics[metrics$module=="rgg",]
        #     metrics$analysisId <- as.numeric(metrics$analysisId)
        #     metrics <- metrics[!is.na(metrics$analysisId),]
        #     metrics <- metrics[metrics$analysisId==max(metrics$analysisId),]
        #     if(input$traitFilterPredictions2D2 %in% metrics$trait ){
        #       rownames(metrics) <- metrics$parameter
        #       mt <- paste0("y = ",round(metrics["ggInter","value"],2), " + ",round(metrics["ggSlope","value"],2),"x", " (gain:",round(metrics["gg%","value"],2),"%)")
        #       res = res %>% plotly::add_annotations(text = mt, x = mean(mydata[,"yearOfOrigin"],na.rm=TRUE), y = max(mydata[,"predictedValue"],na.rm=TRUE))
        #     }
        #     res
        #   }else{
        #     res = plotly::plot_ly()
        #     res = res %>% plotly::add_annotations(text = "yearOfOrigin not available", x = 1, y = 1)#
        #     res
        #   }
        # })

      } else {
        output$predictionsRgg <- DT::renderDT({DT::datatable(NULL)})
        output$metricsRgg <- DT::renderDT({DT::datatable(NULL)})
        output$modelingRgg <- DT::renderDT({DT::datatable(NULL)})
      }

      hideAll$clearAll <- FALSE

    }) ## end eventReactive

    ## Report tab
    output$reportRgg <- renderUI({
      HTML(markdown::markdownToHTML(knitr::knit("./R/reportRgg.Rmd", quiet = TRUE), fragment.only=TRUE))
    })

    output$downloadReportRgg <- downloadHandler(
      filename = function() {
        paste0("reportRgg-",gsub("-|:| ", "", Sys.time()),".html")
      },
      content = function(file) {
        shinybusy::show_modal_spinner(spin = "fading-circle",
                                      color = "#F39C12",
                                      text = "Generating Report...")

        rmarkdown::render(
          # input RMD file
          input = ("R/reportRgg1.Rmd"),

          # input RMD parameters ----
          params = list(),
          output_file = file)
        shinybusy::remove_modal_spinner()
      }, contentType = "html"
    )

    output$outRgg <- renderPrint({
      outRgg()
    })


  })
}

## To be copied in the UI
# mod_rggApp_ui("rggApp_1")

## To be copied in the server
# mod_rggApp_server("rggApp_1")

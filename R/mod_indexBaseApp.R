#' indexBaseApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_indexBaseApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    tags$br(),

    mainPanel( width = 12,
               tabsetPanel( id=ns("tabsMain"),
                            type = "tabs",
                            tabPanel(div(icon("book"), "Information") ,
                                     br(),
                                     column(width = 6,
                                            h1(strong(span("Base Selection Index Module", tags$a(href="https://www.youtube.com/watch?v=YhveKqd0q4s&list=PLZ0lafzH_UmclOPifjCntlMzysEB2_2wX&index=8", icon("youtube") , target="_blank"), style="color:darkcyan"))),
                                            h2(strong("Data Status (wait to be displayed):")),
                                            uiOutput(ns("warningMessage")),
                                            tags$br(),
                                            # column(width=4, tags$br(),
                                            shinyWidgets::prettySwitch( inputId = ns('launch'), label = "Load example dataset", status = "success"),
                                            # ),
                                            tags$br(),
                                            # img(src = "www/indexDesire.png", height = 300, width = 600), # add an image
                                     ),

                                     column(width = 6,
                                            h2(strong("Details")),
                                            p("Genetic evaluation has as final purpose to select the individuals with highest genetic merit across
                                            all traits of interest. In order to select for multiple traits at the same time a selection index is
                                            preferred."),

                                            # br(),

                                            p("This option (base index) aims to calculate a selection index using user's predefined weights and return
                                            a table of predictions with the index and the traits used for selection."),

                                            # br(),

                                            p("If relative economic values of each trait are available and acceptable, a base index can be used to
                                            improve multiple traits simultaneously. In this scenario, an index is calculated for each individual
                                            by using the phenotypic values (BLUPs) observed for each trait and assigning the economic values associated with each
                                            trait as the index coefficients. Assigning economic weights to traits is however a complex task, as it requires knowledge
                                            of various market variables such as prices and profit objectives."),

                                            p("If there are uncertainty on how to determine the economic weights, then an index based on the desired
                                            genetic gains for each trait is recommended"),

                                            p("The way the options are used is the following:"),

                                            p(strong("Traits to include in the index-")," Traits to be considered in the index."),
                                            p(strong("Base values.-")," Vector of values indicating the desired change in traits."),
                                            h2(strong("References")),
                                            p("Brim, C. A., Johnson, H. W., & Cockerham, C. C. (1959). Multiple selection criteria in soybeans 1. Agronomy Journal, 51(1), 42-46."),
                                            p("Ceron-Rojas, J. J., & Crossa, J. (2018). Linear selection indices in modern plant breeding (p. 256). Springer Nature."),
                                            h2(strong("Software used")),
                                            p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
                                Vienna, Austria. URL https://www.R-project.org/."),
                                            column(width = 12, shiny::plotOutput(ns("plotDataDependencies")), ),
                                     ),
                            ),
                            tabPanel(div(icon("arrow-right-to-bracket"), "Input steps"),
                                     tabsetPanel(
                                       tabPanel(div( icon("dice-one"), "Pick MTA-stamp(s)", icon("arrow-right") ), # icon = icon("dice-one"),
                                                br(),
                                                column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                       column(width=8, selectInput(ns("version2IdxB"), "MTA version(s) to analyze", choices = NULL, multiple = TRUE)),

                                                ),
                                                column(width=12),
                                                shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                    column(width=12,
                                                                           hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                           h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameter values to be specified in the grey boxes above.", style="color:green"))),
                                                                           hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                    ),
                                                                    column( width=12, shiny::plotOutput(ns("plotTimeStamps")) ),
                                                                    DT::DTOutput(ns("statusIndex")),
                                                                    DT::DTOutput(ns("tablePredictionsTraitsWide")),
                                                ),
                                       ),
                                       tabPanel( div( icon("dice-two"), "Pick trait(s)",  icon("arrow-right") ), # icon = icon("dice-two"),
                                                 br(),

                                                 column(width=3, style = "background-color:grey; color: #FFFFFF",
                                                        selectInput(ns("traitsBaseIndex"), "Trait(s) to analyze", choices = NULL, multiple = TRUE),
                                                        uiOutput(ns("SliderBaseIndex"))
                                                 ),

                                       ),
                                       tabPanel("Run analysis", icon = icon("dice-three"),
                                                br(),
                                                column(width=12,style = "background-color:grey; color: #FFFFFF",
                                                       column(width=3, tags$div(textInput(ns("analysisIdName"), label = tags$span(
                                                         "Analysis Name (optional)", tags$i( class = "glyphicon glyphicon-info-sign", style = "color:#FFFFFF",
                                                                                             title = "An optional name for the analysis besides the timestamp if desired.") ), #width = "100%",
                                                         placeholder = "(optional name)") ) ),
                                                       column(width = 3,
                                                              br(),
                                                              actionButton(ns("runIdxB"), "Calculate index", icon = icon("play-circle")),
                                                              uiOutput(ns("qaQcIdxBInfo")),
                                                              br(),
                                                              # hr(style = "border-top: 3px solid #4c4c4c;"),
                                                       ),
                                                       column(width=6,
                                                              br(),
                                                              shinydashboard::box(width = 12, status = "success", solidHeader=FALSE, style = "color: #000000",collapsible = TRUE, collapsed = TRUE, title = "Additional run settings (optional)...",
                                                                                  selectInput(ns("verboseIndex"), label = "Print logs?", choices = list(TRUE,FALSE), selected = FALSE, multiple=FALSE)
                                                              ),
                                                       ),
                                                ),textOutput(ns("outIdxB")),
                                       ),
                                     )
                            ),
                            tabPanel(div(icon("arrow-right-from-bracket"), "Output tabs" ) , value = "outputTabs",
                                     tabsetPanel(
                                       tabPanel("Dashboard", icon = icon("file-image"),
                                                br(),
                                                textOutput(ns("outIdxB2")),
                                                br(),
                                                downloadButton(ns("downloadReportIndex"), "Download dashboard"),
                                                br(),
                                                uiOutput(ns("BaseIndex"))
                                       ),
                                       tabPanel("Predictions", icon = icon("table"),
                                                br(),
                                                DT::DTOutput(ns("predictionsIdxB"))
                                       ),
                                       tabPanel("Modeling", icon = icon("table"),
                                                br(),
                                                DT::DTOutput(ns("modelingIdxB"))
                                       ),

                                     )
                            )# end of output panel
               )) # end mainpanel

  )
}

#' indexBaseApp Server Functions
#'
#' @noRd
mod_indexBaseApp_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$plotDataDependencies <- shiny::renderPlot({ dependencyPlot() })
    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })
    ############################################################################
    # show shinyWidgets until the user can use the module
    observeEvent(c(data(), input$version2IdxB, input$traitsBaseIndex ), {
      req(data())
      mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
      if(mappedColumns == 3 & length(input$version2IdxB)>0 & length(input$traitsBaseIndex)>0 ){
        golem::invoke_js('showid', ns('holder1'))
      }else{
        golem::invoke_js('hideid', ns('holder1'))
      }
    })
    ############################################################################
    # warning message
    output$warningMessage <- renderUI(
      if(is.null(data())){
        HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your phenotypic data using the 'Data Retrieval' tab.")) )
      }else{ # data is there
        mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
        if(mappedColumns == 3){
          if(any(c("mta","mtaFlex","mtaLmms","mtaAsr") %in% data()$status$module)){
            HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to perform the selection index specifying your input parameters under the Input tabs.")) )
          }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please perform a Multi-Trial Analysis before performing a selection index")) ) }
        }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please make sure that you have computed the 'environment' column, and that column 'designation' and \n at least one trait have been mapped using the 'Data Retrieval' tab.")) )}
      }
    )
    ## data example loading
    observeEvent(
      input$launch,
      if(length(input$launch) > 0){
        if (input$launch) {
          shinyWidgets::ask_confirmation(
            inputId = ns("myconfirmation"),
            text = "Are you sure you want to load the example data? This will delete any data currently in the environment.",
            title = "Data replacement warning"
          )
        }
      }
    )
    observeEvent(input$myconfirmation, {
      if (isTRUE(input$myconfirmation)) {
        shinybusy::show_modal_spinner('fading-circle', text = 'Loading example...')
        ## replace tables
        tmp <- data()
        data(cgiarBase::create_getData_object())
        utils::data(DT_example, package = "cgiarPipeline")
        if(!is.null(result$data)){tmp$data <- result$data}
        if(!is.null(result$metadata)){tmp$metadata <- result$metadata}
        if(!is.null(result$modifications)){tmp$modifications <- result$modifications}
        if(!is.null(result$predictions)){tmp$predictions <- result$predictions}
        if(!is.null(result$metrics)){tmp$metrics <- result$metrics}
        if(!is.null(result$modeling)){tmp$modeling <- result$modeling}
        if(!is.null(result$status)){tmp$status <- result$status}
        data(tmp) # update data with results
        shinybusy::remove_modal_spinner()
      }else{
        shinyWidgets::updatePrettySwitch(session, "launch", value = FALSE)
      }
    }, ignoreNULL = TRUE)

    ######################################################################################
    ######################################################################################
    ########################################### Base Index
    ######################################################################################
    ######################################################################################

    #################
    ## version
    observeEvent(c(data()), {
      req(data())
      dtIdxB <- data()
      dtIdxB <- dtIdxB$status
      dtIdxB <- dtIdxB[which(dtIdxB$module %in% c("mta","mtaFlex","mtaLmms","mtaAsr")),]
      traitsIdxB <- unique(dtIdxB$analysisId)
      if(length(traitsIdxB) > 0){
        if("analysisIdName" %in% colnames(dtIdxB)){
          names(traitsIdxB) <- paste(dtIdxB$analysisIdName, as.POSIXct(traitsIdxB, origin="1970-01-01", tz="GMT"), sep = "_")
        }else{
          names(traitsIdxB) <- as.POSIXct(traitsIdxB, origin="1970-01-01", tz="GMT")
        }
      }
      updateSelectInput(session, "version2IdxB", choices = traitsIdxB)
    })
    ####################
    ## traits for base index
    observeEvent(c(data(), input$version2IdxB), {
      req(data())
      req(input$version2IdxB)
      dtBaseIndex <- data()
      dtBaseIndex <- dtBaseIndex$predictions
      dtBaseIndex <- dtBaseIndex[which(dtBaseIndex$analysisId %in% input$version2IdxB),]
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
      req(input$version2IdxB)
      req(input$traitsBaseIndex)
      dtBaseIndex <- data()
      shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
      dtBaseIndexPred <- dtBaseIndex$predictions
      dtBaseIndexPred <- dtBaseIndexPred[dtBaseIndexPred$analysisId %in% input$version2IdxB,]
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
      if(sum(dtBaseIndex$status$module %in% c("mta","mtaFlex","mtaLmms","mtaAsr")) == 0) {
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
          input$version2IdxB,
          input$analysisIdName,
          traitsBaseIndex,
          values),
          silent=TRUE
        )
        if(!inherits(result,"try-error")) {
          if("analysisIdName" %in% colnames(result$status)){result$status$analysisIdName[nrow(result$status)] <- input$analysisIdName}
          data(result) # update data with results
          # save(result, file = "./R/outputs/resultIndex.RData")
          cat(paste("Selection index step with id:",result$status$analysisId[length(result$status$analysisId)],"saved. Please proceed to use this time stamp in the optimal cross selection."))
          output$outIdxB2 <- renderPrint({
            cat(paste("Selection index step with id:",result$status$analysisId[length(result$status$analysisId)],"saved. Please proceed to use this time stamp in the optimal cross selection."))
          })
          updateTabsetPanel(session, "tabsMain", selected = "outputTabs")
        }else{
          cat(paste("Analysis failed with the following error message: \n\n",result[[1]]))
          output$outIdxB2 <- renderPrint({
            cat(paste("Analysis failed with the following error message: \n\n",result[[1]]))
          })
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
        }, server = FALSE)
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
        }, server = FALSE)
        # Report tab
        output$BaseIndex <- renderUI({
          HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportIndexB.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
        })

        output$downloadReportIndex <- downloadHandler(
          filename = function() {
            paste(paste0('indexB_dashboard_',gsub("-", "", as.integer(Sys.time()))), sep = '.', switch(
              "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
            ))
          },
          content = function(file) {
            shinybusy::show_modal_spinner(spin = "fading-circle", text = "Generating Report...")

            src <- normalizePath(system.file("rmd","reportIndexB.Rmd",package="bioflow"))
            src2 <- normalizePath('data/resultIndexB.RData')

            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))

            file.copy(src, 'report.Rmd', overwrite = TRUE)
            file.copy(src2, 'resultIndexB.RData', overwrite = TRUE)

            out <- rmarkdown::render('report.Rmd', params = list(toDownload=TRUE),switch(
              "HTML",
              HTML = rmdformats::robobook(toc_depth = 4)
              # HTML = rmarkdown::html_document()
            ))

            # wait for it to land on disk (safety‐net)
            wait.time <- 0
            while (!file.exists(out) && wait.time < 60) {
              Sys.sleep(1); wait.time <- wait.time + 1
            }

            file.rename(out, file)
            shinybusy::remove_modal_spinner()
          }
        )

        # analysisIdBaseIndex <- result$status[ result$status$module %in% c("mta","mtaFlex","mtaLmms","mtaAsr","indexB"),"analysisId"]
        # predBaseIndex <- result$predictions[result$predictions$analysisId %in% analysisIdBaseIndex,]
        #
        # predBaseIndexWide <- reshape(
        #   data=subset(predBaseIndex, select=c(designation,trait,predictedValue)),
        #   timevar = "trait",
        #   idvar = "designation",
        #   direction="wide"
        # )
        # colnames(predBaseIndexWide)[-1] <- gsub("predictedValue.","", colnames(predBaseIndexWide)[-1])
        # predBaseIndexWide <- predBaseIndexWide[order(predBaseIndexWide$baseIndex,decreasing=TRUE),]
        # predBaseIndexWide$Rank <- 1:nrow(predBaseIndexWide)
        #
        # output$BaseIndex <-  DT::renderDT({
        #   # if ( hideAll$clearAll){
        #   #   return()
        #   # }else{
        #   numeric.output <- c(input$traitsBaseIndex,"baseIndex")
        #   # print(head(predBaseIndexWide))
        #   DT::formatRound(DT::datatable(predBaseIndexWide,
        #                                 extensions = 'Buttons',
        #                                 rownames = FALSE,
        #                                 class = 'cell-border',
        #                                 options = list(dom = 'Blfrtip',
        #                                                scrollY = "400px",
        #                                                scrollX = "400px",
        #                                                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        #                                                paging=F)
        #   ), numeric.output)
        #
        # }, server = FALSE)

      } else {
        output$predictionsIdxB <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
        output$metricsIdxB <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
        output$modelingIdxB <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
        hideAll$clearAll <- TRUE
      }

      hideAll$clearAll <- FALSE

    })

    output$outIdxB <- renderPrint({
      outIdxB()
    })



  })
}

## To be copied in the UI
# mod_indexBaseApp_ui("indexBaseApp_1")

## To be copied in the server
# mod_indexBaseApp_server("indexBaseApp_1")

#' qaGenoApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_qaGenoApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::mainPanel(width = 12,
                     tabsetPanel( id=ns("tabsMain"),
                                  type = "tabs",
                                  tabPanel(div(icon("book"), "Information-QA-Geno") ,
                                           br(),
                                           # shinydashboard::box(status="success",width = 12, solidHeader = TRUE,
                                           # column(width=12,   style = "height:580px; overflow-y: scroll;overflow-x: scroll;",
                                           tags$body(
                                             column(width = 6,
                                                    h1(strong(span("QA for genetic markers", tags$a(href="https://www.youtube.com/watch?v=6Ooq9I3LEp8&list=PLZ0lafzH_UmclOPifjCntlMzysEB2_2wX&index=5", icon("youtube") , target="_blank"), style="color:darkcyan"))),
                                                    h2(strong("Status:")),
                                                    uiOutput(ns("warningMessage")),
                                                    tags$br(),
                                                    img(src = "www/qaGeno.png", height = 300, width = 650), # add an image
                                             ),
                                             column(width = 6, shiny::plotOutput(ns("plotDataDependencies")), ),
                                             column(width = 12,
                                                    h2(strong("Details")),
                                                    p("When genetic evaluation is carried using genomic data, we need to ensure the quality of genetic markers.
                                                             This option aims to allow users to identify bad markers or individuals given certain QA parameters.
                                The way arguments are used is the following:"),

                                                    p(strong("Threshold for missing data in markers.-")," this sets a threshold for how much missing data in a marker is allowed. Any marker with more than this value will be marked as a column to be removed in posterior analyses. Value between 0 and 1."),
                                                    p(strong("Threshold for missing data in individuals.-"),"  this sets a threshold for how much missing data in an individual is allowed. Any individual with more than this value it will be marked as a row to be removed in posterior analyses. Value between 0 and 1."),
                                                    p(strong("Minor allele frequency.-")," this sets a lower threshold for what is the minimum allele frequency allowed in the dataset. If lower than this value it will be marked as a column to be removed in posterior analyses. Value between 0 and 1."),
                                                    p(strong("Threshold for heterozygosity in markers.-")," this sets an upper threshold for what is the maximum level of heterozygosity allowed in the markers. If greater than this value it will be marked as a column to be removed in posterior analyses. Value between 0 and 1. For example, a line dataset should not have markers with high heterozigosity."),
                                                    p(strong("Threshold for inbreeding in markers.-")," this sets an upper threshold for what is the maximum level of inbreeding allowed in the markers. If lower than this value it will be marked as a column to be removed in posterior analyses. Value between 0 and 1."),
                                                    p(strong("Additional settings:")),
                                                    p(strong("Imputation method.-")," method to impute missing cells. Median is the only method currently available."),
                                                    p(strong("Ploidy.-")," number of chromosome copies. This value is important to compute some of the paramters. Default is 2 or diploid."),
                                                    h2(strong("References")),
                                                    p("Tukey, J. W. (1977). Exploratory Data Analysis. Section 2C."),
                                                    p("Velleman, P. F. and Hoaglin, D. C. (1981). Applications, Basics and Computing of Exploratory Data Analysis. Duxbury Press.")
                                             ),
                                           )
                                           # ),

                                           # )
                                  ),
                                  tabPanel(div(icon("arrow-right-to-bracket"), "Input"),
                                           tabsetPanel(
                                             tabPanel("Set thresholds", icon = icon("magnifying-glass-chart"),
                                                      br(),
                                                      column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                             column(width=4, numericInput(ns("propNaUpperThreshForMarker"), label = "Upper threshold for missing data in markers (> will be removed)", value = .4, step = .05, max = 1, min = 0) ),
                                                             column(width=4, numericInput(ns("propNaUpperThreshForInds"), label = "Upper threshold for missing data in individuals(> will be removed)", value = .4, step = .05, max = 1, min = 0) ),
                                                             column(width=4, numericInput(ns("maf"), label = "Upper threshold for minor allele frequency (< will be removed)", value = 0, step = .05, max = 1, min = 0) ),
                                                             column(width=4, numericInput(ns("propHetUpperThreshForMarker"), label = "Upper threshold for heterozygosity in markers (> will be removed)", value = 1, step = .05, max = 1, min = 0) ),
                                                             column(width=4, numericInput(ns("propFisUpperThreshForMarker"), label = "Upper threshold for inbreeding in markers (> will be removed)", value = 1, step = .05, max = 1, min = 0) ),
                                                             column(width=4, tags$br(), tags$br(),
                                                                    shinyWidgets::prettySwitch( inputId = ns('launch'), label = "Load example", status = "success"),
                                                             ),
                                                      ),
                                                      column(width=12,
                                                             hr(style = "border-top: 3px solid #4c4c4c;"),
                                                             h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameter values to be specified in the grey boxes above.", style="color:green"))),
                                                             hr(style = "border-top: 3px solid #4c4c4c;"),
                                                      ),

                                                      # shinydashboard::box(status="success",width = 12,solidHeader = TRUE,
                                                      #                     column(width=12, style = "height:440px; overflow-y: scroll;overflow-x: scroll;",
                                                      # p(span("Preview of the proportion of markers or individuals tagged for the different QA parameters.", style="color:black")),
                                                      column(width=12,
                                                             tags$span(id = ns('holder'),
                                                                       plotly::plotlyOutput(ns("plotPredictionsCleanOutMarker")) ,
                                                                       # p(span("Number of individuals and markers available in the dataset.", style="color:black")),
                                                                       DT::DTOutput(ns("summariesGeno")),
                                                                       # p(span("Preview of potential modifications to add.", style="color:black")),
                                                                       DT::DTOutput(ns("modificationsQaMarker")),
                                                             ),
                                                      ),
                                                      #                     ),
                                                      # )
                                             ),
                                             tabPanel("Run analysis", icon = icon("play"),
                                                      br(),
                                                      actionButton(ns("runQaMb"), "Identify & store modifications", icon = icon("play-circle")),
                                                      textOutput(ns("outQaMb")),
                                                      hr(style = "border-top: 3px solid #4c4c4c;"),
                                                      shinydashboard::box(width = 12, status = "success", background="green",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Additional run settings...",
                                                                          selectInput(ns("imputationMethod"), "Imputation method", choices = c("median"), multiple = FALSE),
                                                                          numericInput(ns("ploidy"), label = "Ploidy", value = 2, step=2, max = 10, min=2)
                                                      ),
                                             ),
                                           ) # end of tabset
                                  ),# end of output panel
                                  tabPanel(div(icon("arrow-right-from-bracket"), "Output" ) , value = "outputTabs",
                                           tabsetPanel(
                                             tabPanel("Dashboard", icon = icon("file-image"),
                                                      br(),
                                                      downloadButton(ns("downloadReportQaGeno"), "Download dashboard"),
                                                      br(),
                                                      uiOutput(ns('reportQaGeno'))
                                             ),
                                           ),
                                  ),
                     )) # end mainpanel


  )
}

#' qaGenoApp Server Functions
#'
#' @noRd
mod_qaGenoApp_server <- function(id, data){
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
    observeEvent(c(data()), {
      req(data())
      if(!is.null(data()$data$geno)){
        golem::invoke_js('showid', ns('holder'))
      }else{
        golem::invoke_js('hideid', ns('holder'))
      }
    })
    ############################################################################
    # warning message
    output$warningMessage <- renderUI(
      if(is.null(data())){
        HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your data using the 'Data' tab.")) )
      }else{ # data is there
        if(!is.null(data()$data$geno)){
          HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to perform the marker QA specifying your input parameters under the Input tabs.")) )
        }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your genotype data using the 'Data' tab. ")) )}
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
        data(cgiarBase::create_getData_object())
        tmp <- data()
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
    ##
    newModifications <- reactive({ # p('File to be analyzed')
      req(data())
      req(input$propNaUpperThreshForMarker)
      req(input$propNaUpperThreshForInds)
      req(input$maf)
      req(input$propHetUpperThreshForMarker)
      req(input$propFisUpperThreshForMarker)
      req(input$imputationMethod)
      myObject <- data()
      res <- cgiarBase::computeGenoModifications(
        M = myObject$data$geno, # markers are assumed to come centered
        propNaUpperThreshForMarker=input$propNaUpperThreshForMarker,
        propNaUpperThreshForInds=input$propNaUpperThreshForInds,
        maf=input$maf, ploidy=input$ploidy,
        imputationMethod=input$imputationMethod
      )
      ## reactive
      return(res)
    })

    ## render the expected result for markers
    output$plotPredictionsCleanOutMarker <- plotly::renderPlotly({
      req(data())
      req(input$propNaUpperThreshForMarker)
      req(input$propNaUpperThreshForInds)
      req(input$maf)
      req(input$propHetUpperThreshForMarker)
      req(input$propFisUpperThreshForMarker)
      req(input$imputationMethod)
      mydata <- data()$data$geno
      ##
      if(!is.null(mydata)){
        fig <- plotly::plot_ly(alpha = 0.6)
        ## historgram for marker with missing data
        propNaMarker <- apply(mydata,2,function(x){(length(which(is.na(x)))/length(x))})
        fig <- fig %>% plotly::add_histogram(x = propNaMarker, histnorm = "probability", name="Missing data in markers" )
        ## historgram for individuals with missing data
        propNaIndividual <- apply(mydata,1,function(x){(length(which(is.na(x)))/length(x))})
        fig <- fig %>% plotly::add_histogram(x = propNaIndividual, histnorm = "probability", name="Missing data in individuals" )
        ## historgram for MAF
        MAF <- apply(mydata, 2, function(x) {AF <- mean(x, na.rm = T)/input$ploidy;MAF <- ifelse(AF > 0.5, 1 - AF, AF) })
        fig <- fig %>% plotly::add_histogram(x = MAF, histnorm = "probability", name="Minor allele frequency" )
        ## histogram for heterozigosity
        q <- MAF; p <- 1 - q
        he <- 2 * p * q;  ho <- colMeans((mydata) == 1, na.rm = TRUE) # Get the obseved heterozygosity.
        fig <- fig %>% plotly::add_histogram(x = ho, histnorm = "probability", name="Observed heterozygosity" )
        ## histogram for inbreeding
        Fis <- ifelse(he == 0, yes = 0, no = 1 - (ho / he))
        fig <- fig %>% plotly::add_histogram(x = Fis, histnorm = "probability", name="Inbreeding" )
        ## add threshold lines and layout
        vline1 <- function(x = 0, color = "blue") {list( type = "line",y0 = 0,y1 = 1,yref = "paper",x0 = x, x1 = x,line = list(color = color, dash="dot"))}
        vline2 <- function(x = 0, color = "red") {list( type = "line",y0 = 0,y1 = 1,yref = "paper",x0 = x, x1 = x,line = list(color = color, dash="dot"))}
        vline3 <- function(x = 0, color = "green") {list( type = "line",y0 = 0,y1 = 1,yref = "paper",x0 = x, x1 = x,line = list(color = color, dash="dot"))}
        fig <- fig %>% plotly::layout(barmode = "overlay", xaxis = list(title = "Value of parameter"), yaxis = list(title = "Proportion of markers" ),
                                      title="Preview of the proportion of markers or individuals tagged for the different QA parameters",
                                      shapes = list(vline1(input$propNaUpperThreshForMarker),vline2(input$propNaUpperThreshForInds),vline3(input$maf) ) )
        fig
      }else{
        fig = plotly::plot_ly()
        fig = fig %>% plotly::add_annotations(text = "Genetic marker information not available.", x = 1, y = 1)#
        fig
      }

    })

    ## summaries geno
    output$summariesGeno <-  DT::renderDT({
      req(data())

      if(!is.null(data()$data$geno)){
        mo <- data.frame(nInds=nrow(data()$data$geno), nMarkers=ncol(data()$data$geno)); rownames(mo) <- "summary"
      }else{
        mo <- data.frame(warningText="Genetic marker data not available")
      }
      DT::datatable(mo, extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All'))),
                    caption = htmltools::tags$caption(
                      style = 'color:cadetblue', #caption-side: bottom; text-align: center;
                      htmltools::em('Number of individuals and markers available in the dataset.')
                    )
      )

    })
    ## display the current outliers
    observeEvent(data(),{

      output$modificationsQaMarker <-  DT::renderDT({
        req(data())
        req(input$propNaUpperThreshForMarker)
        req(input$propNaUpperThreshForInds)
        req(input$maf)
        req(input$propHetUpperThreshForMarker)
        req(input$propFisUpperThreshForMarker)
        req(input$imputationMethod)
        ##
        if(!is.null(data()$data$geno)){
          mo <- newModifications()
        }else{
          mo <- data.frame(warningText="Genetic marker data not available")
        }
        DT::datatable(mo, extensions = 'Buttons',
                      options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                     lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All'))),
                      caption = htmltools::tags$caption(
                        style = 'color:cadetblue', #caption-side: bottom; text-align: center;
                        htmltools::em('Preview of potential modifications to add.')
                      )
        )

      })

    })
    ##########################
    ## save when user clicks
    outQaMb <- eventReactive(input$runQaMb, {

      req(data())
      req(input$propNaUpperThreshForMarker)
      req(input$propNaUpperThreshForInds)
      req(input$maf)
      req(input$propHetUpperThreshForMarker)
      req(input$propFisUpperThreshForMarker)
      req(input$imputationMethod)
      shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
      ## get the outlier table
      mods <- newModifications()
      if(nrow(mods) > 0){
        ## store the new modifications table
        result <- data()
        # save(result, file = "./R/outputs/resultQaGeno.RData")
        result$modifications$geno <- rbind(result$modifications$geno, mods )
        ## write the new status table
        newStatus <- data.frame(module="qaGeno", analysisId= mods$analysisId[nrow(mods)])
        result$status <- rbind(result$status, newStatus)
        data(result)
        cat(paste("Modifications to genotype information saved with id:",as.POSIXct( mods$analysisId[nrow(mods)], origin="1970-01-01", tz="GMT") ))
        updateTabsetPanel(session, "tabsMain", selected = "outputTabs")
      }else{
        cat("No modifications to add.")
      }
      shinybusy::remove_modal_spinner()

      if(nrow(mods) > 0) { # if all goes well in the run
        # ## Report tab
        output$reportQaGeno <- renderUI({
          HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportQaGeno.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
        })

        output$downloadReportQaGeno <- downloadHandler(
          filename = function() {
            paste('my-report', sep = '.', switch(
              "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
            ))
          },
          content = function(file) {
            src <- normalizePath(system.file("rmd","reportQaGeno.Rmd",package="bioflow"))
            src2 <- normalizePath('data/resultQaGeno.RData')
            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'report.Rmd', overwrite = TRUE)
            file.copy(src2, 'resultQaGeno.RData', overwrite = TRUE)
            out <- rmarkdown::render('report.Rmd', params = list(toDownload=TRUE),switch(
              "HTML",
              HTML = rmdformats::robobook(toc_depth = 4)
              # HTML = rmarkdown::html_document()
            ))
            file.rename(out, file)
          }
        )

      }else{ hideAll$clearAll <- TRUE}

      hideAll$clearAll <- FALSE

    })
    output$outQaMb <- renderPrint({
      outQaMb()
    })
    #

  })
}

## To be copied in the UI
# mod_qaGenoApp_ui("qaGenoApp_1")

## To be copied in the server
# mod_qaGenoApp_server("qaGenoApp_1")

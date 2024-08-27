#' neApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_neApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::mainPanel(width = 12,
                     tabsetPanel( id=ns("tabsMain"),
                                  type = "tabs",
                                  tabPanel(div(icon("book"), "Information") ,
                                           br(),
                                           tags$body(
                                             column(width = 6,
                                                    h1(strong(span("Number of Founders based on genetic markers", tags$a(href="https://www.youtube.com/watch?v=6Ooq9I3LEp8&list=PLZ0lafzH_UmclOPifjCntlMzysEB2_2wX&index=5", icon("youtube") , target="_blank"), style="color:darkcyan"))),
                                                    h2(strong("Status:")),
                                                    uiOutput(ns("warningMessage")),
                                                    tags$br(),
                                                    # column(width=4, tags$br(),
                                                           shinyWidgets::prettySwitch( inputId = ns('launch'), label = "Load example dataset", status = "success"),
                                                    # ),
                                                    tags$br(),
                                                    # img(src = "www/qaGeno.png", height = 300, width = 650), # add an image
                                             ),
                                             column(width = 6, shiny::plotOutput(ns("plotDataDependencies")), ),
                                             column(width = 12,
                                                    h2(strong("Details")),
                                                    p("One of the key performance indicators of interest for breeding programs is to understand what is the amount of genetic
                                                    variance present in the breeding population. One related metric is the so called effective population size. In this module,
                                                    we use the genetic marker information to infer the number of effective individuals, that is the percentage of alleles that can
                                                    be sampled with n individuals. This can be thought as a surrogate of efective number of individuals behind the population.
                                The way arguments are used is the following:"),

                                                    p(strong("maxNe.-")," The maximum number of potential founders to investigate in the sampling process of percentage of alleles recovered."),
                                                    p(strong("maxMarker.-"),"  The maximum number of markers to sample for the exercise."),
                                                    p(strong("nSamples.-")," number of independent runs for each value of maxNe for the Ne calculation."),
                                                    h2(strong("References")),
                                                    p("Tukey, J. W. (1977). Exploratory Data Analysis. Section 2C."),
                                                    p("Velleman, P. F. and Hoaglin, D. C. (1981). Applications, Basics and Computing of Exploratory Data Analysis. Duxbury Press.")
                                             ),
                                           )
                                  ),
                                  tabPanel(div(icon("arrow-right-to-bracket"), "Input"),
                                           tabsetPanel(
                                             tabPanel("Pick QA-Geno-stamp", icon = icon("dice-one"),
                                                      br(),
                                                      column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                             column(width=8, selectInput(ns("versionMarker2Mta"), "QA-Geno version to use (required)", choices = NULL, multiple = TRUE)),

                                                      ),
                                                      column(width=12,
                                                             hr(style = "border-top: 3px solid #4c4c4c;"),
                                                             h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameter values to be specified in the grey boxes above.", style="color:green"))),
                                                             hr(style = "border-top: 3px solid #4c4c4c;"),
                                                      ),
                                                      column( width=12, shiny::plotOutput(ns("plotTimeStamps")) ),
                                                      # DT::DTOutput(ns("statusMta")), # modeling table
                                             ),
                                             tabPanel("Set thresholds", icon = icon("dice-two"),
                                                      br(),
                                                      column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                             column(width=3, numericInput(ns("maxNe"), label = "Maximum Number of Founders to Explore", value = 100, step = 5, max = 1000, min = 2) ),
                                                             column(width=3, numericInput(ns("maxMarker"), label = "Number of markers to use", value = 1000, step = 50, max = 20000, min = 10) ),
                                                             column(width=3, numericInput(ns("nSamples"), label = "Number of bootstraps", value = 10, step = 5, max = 5, min = 500) ),
                                                      ),
                                                      column(width=12,
                                                             hr(style = "border-top: 3px solid #4c4c4c;"),
                                                             h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameter values to be specified in the grey boxes above.", style="color:green"))),
                                                             hr(style = "border-top: 3px solid #4c4c4c;"),
                                                      ),
                                                      column(width=12,
                                                             tags$span(id = ns('holder'),
                                                                       DT::DTOutput(ns("summariesGeno")),
                                                             ),
                                                      ),
                                             ),
                                             tabPanel("Run analysis", icon = icon("dice-three"),
                                                      column(width=12,style = "background-color:grey; color: #FFFFFF",
                                                             tags$br(),
                                                             # column(width=3,
                                                                    actionButton(ns("runQaMb"), "Run (click)", icon = icon("play-circle")),
                                                             # ),
                                                             tags$br(),
                                                             textOutput(ns("outQaMb")),
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

#' neApp Server Functions
#'
#' @noRd
mod_neApp_server <- function(id, data){
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
          HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to specify your input parameters under the Input tabs.")) )
        }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your genotype data using the 'Data' tab. ")) )}
      }
    )
    ## render timestamps flow plot
    output$plotTimeStamps <- shiny::renderPlot({
      req(data()) # req(input$version2Sta)
      xx <- data()$status;  yy <- data()$modeling
      v <- which(yy$parameter == "analysisId")
      if(length(v) > 0){
        yy <- yy[v,c("analysisId","value")]
        zz <- merge(xx,yy, by="analysisId", all.x = TRUE)
      }else{ zz <- xx; zz$value <- NA}
      if(!is.null(xx)){
        colnames(zz) <- cgiarBase::replaceValues(colnames(zz), Search = c("analysisId","value"), Replace = c("outputId","inputId") )
        nLevelsCheck1 <- length(na.omit(unique(zz$outputId)))
        nLevelsCheck2 <- length(na.omit(unique(zz$inputId)))
        if(nLevelsCheck1 > 1 & nLevelsCheck2 > 1){
          X <- with(zz, sommer::overlay(outputId, inputId))
        }else{
          if(nLevelsCheck1 <= 1){
            X1 <- matrix(ifelse(is.na(zz$inputId),0,1),nrow=length(zz$inputId),1); colnames(X1) <- as.character(na.omit(unique(c(zz$outputId))))
          }else{X1 <- model.matrix(~as.factor(outputId)-1, data=zz); colnames(X1) <- levels(as.factor(zz$outputId))}
          if(nLevelsCheck2 <= 1){
            X2 <- matrix(ifelse(is.na(zz$inputId),0,1),nrow=length(zz$inputId),1); colnames(X2) <- as.character(na.omit(unique(c(zz$inputId))))
          }else{X2 <- model.matrix(~as.factor(inputId)-1, data=zz); colnames(X2) <- levels(as.factor(zz$inputId))}
          mynames <- unique(na.omit(c(zz$outputId,zz$inputId)))
          X <- matrix(0, nrow=nrow(zz), ncol=length(mynames)); colnames(X) <- as.character(mynames)
          if(!is.null(X1)){X[,colnames(X1)] <- X1}
          if(!is.null(X2)){X[,colnames(X2)] <- X2}
        };  rownames(X) <- as.character(zz$outputId)
        rownames(X) <-as.character(as.POSIXct(as.numeric(rownames(X)), origin="1970-01-01", tz="GMT"))
        colnames(X) <-as.character(as.POSIXct(as.numeric(colnames(X)), origin="1970-01-01", tz="GMT"))
        # make the network plot
        n <- network::network(X, directed = FALSE)
        network::set.vertex.attribute(n,"family",zz$module)
        network::set.vertex.attribute(n,"importance",1)
        e <- network::network.edgecount(n)
        network::set.edge.attribute(n, "type", sample(letters[26], e, replace = TRUE))
        network::set.edge.attribute(n, "day", sample(1, e, replace = TRUE))
        library(ggnetwork)
        ggplot2::ggplot(n, ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
          ggnetwork::geom_edges(ggplot2::aes(color = family), arrow = ggplot2::arrow(length = ggnetwork::unit(6, "pt"), type = "closed") ) +
          ggnetwork::geom_nodes(ggplot2::aes(color = family), alpha = 0.5, size=5 ) + ggplot2::ggtitle("Network plot of current analyses available") +
          ggnetwork::geom_nodelabel_repel(ggplot2::aes(color = family, label = vertex.names ),
                                          fontface = "bold", box.padding = ggnetwork::unit(1, "lines")) +
          ggnetwork::theme_blank()
      }
    })
    # ## version qa marker
    observeEvent(c(data()), {
      req(data())
      dtMta <- data()
      dtMta <- dtMta$status
      dtMta <- dtMta[which(dtMta$module == "qaGeno"),]
      traitsMta <- unique(dtMta$analysisId)
      if(length(traitsMta) > 0){names(traitsMta) <- as.POSIXct(traitsMta, origin="1970-01-01", tz="GMT")}
      updateSelectInput(session, "versionMarker2Mta", choices = traitsMta)
    })
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

    ##########################
    ## save when user clicks
    outQaMb <- eventReactive(input$runQaMb, {

      req(data())
      req(input$versionMarker2Mta)
      req(input$maxNe)
      req(input$maxMarker)
      req(input$nSamples)
      shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
      ##

      ## store the new modifications table
      # dt <- data()
      # save(result, file = "./R/outputs/resultQaGeno.RData")
      result <- try( cgiarPipeline::numberFounders(
        object= data(),
        analysisIdForGenoModifications=input$versionMarker2Mta,
        neExplore = seq(10,input$maxNe,round(input$maxNe/10)),
        maxMarker=input$maxMarker,
        nSamples=input$nSamples,
        verbose=FALSE
      ), silent = TRUE)
      ## write the new status table
      data(result)
      cat(paste("Multi-trial analysis step with id:",as.POSIXct(result$status$analysisId[length(result$status$analysisId)], origin="1970-01-01", tz="GMT"),"saved. Please proceed to inspect results."))
      updateTabsetPanel(session, "tabsMain", selected = "outputTabs")

      shinybusy::remove_modal_spinner()

      if(!inherits(result,"try-error")) { # if all goes well in the run
        # ## Report tab
        output$reportQaGeno <- renderUI({
          HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportNeGeno.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
        })

        output$downloadReportQaGeno <- downloadHandler(
          filename = function() {
            paste('my-report', sep = '.', switch(
              "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
            ))
          },
          content = function(file) {
            src <- normalizePath(system.file("rmd","reportNeGeno.Rmd",package="bioflow"))
            src2 <- normalizePath('data/resultNeGeno.RData')
            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'report.Rmd', overwrite = TRUE)
            file.copy(src2, 'resultNeGeno.RData', overwrite = TRUE)
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



  })
}

## To be copied in the UI
# mod_neApp_ui("neApp_1")

## To be copied in the server
# mod_neApp_server("neApp_1")

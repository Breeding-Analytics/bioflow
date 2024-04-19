#' reportBuilder UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_reportBuilder_ui <- function(id){
  ns <- NS(id)
  tagList(

    mainPanel(width = 12,
              tabsetPanel( id=ns("tabsMain"),
                           type = "tabs",
                           tabPanel(div(icon("arrow-right-to-bracket"), "Input"),
                                    tabsetPanel(
                                      tabPanel("Pick module and timestamp", icon = icon("magnifying-glass-chart"),
                                               br(),
                                               column(width=12,
                                                      column(width=6,  selectInput(ns("module"), "Module report", choices = NULL, multiple = FALSE) ),
                                                      column(width=6,  selectInput(ns("timestamp"), "Time stamp", choices = NULL, multiple = FALSE) ),
                                                      style = "background-color:grey; color: #FFFFFF"),
                                               hr(style = "border-top: 3px solid #4c4c4c;"),
                                               h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameter values to be specified in the grey boxes above.", style="color:green"))),
                                               hr(style = "border-top: 3px solid #4c4c4c;"),
                                               shinydashboard::box(status="success",width = 12, style = "height:460px; overflow-y: scroll;overflow-x: scroll;",
                                                                   solidHeader = TRUE,
                                                                   column(width=12,
                                                                          p(span("Current analyses available.", style="color:black")),
                                                                          shiny::plotOutput(ns("plotTimeStamps")),
                                                                   )
                                               ),
                                      ),
                                      tabPanel("Build dashboard", icon = icon("play"),
                                               br(),
                                               actionButton(ns("runReport"), "Build dashboard", icon = icon("play-circle")),
                                               textOutput(ns("outReport")),
                                      ),
                                    )
                           ),
                           tabPanel(div(icon("arrow-right-from-bracket"), "Output" ) , value = "outputTabs",
                                    tabsetPanel(
                                      tabPanel("Dashboard", icon = icon("file-image"),
                                               br(),
                                               downloadButton(ns("downloadReportReport"), "Download dashboard"),
                                               br(),
                                               uiOutput(ns('reportReport'))
                                      )
                                    )
                           )
              )) # end mainpanel



  )
}

#' reportBuilder Server Functions
#'
#' @noRd
mod_reportBuilder_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })

    #################
    ## model types
    observeEvent(c(data()), {
      req(data()) # list(QA="qaRaw" , QAmarkers="qaGeno" , STA="sta" ,   MTA="mta",    Index="indexD", OCS="ocs",    RGG="rgg" ,   PGG="pgg" )
      if(!is.null(data()$status)){
        traitsBuilder <- unique(data()$status$module)
        names(traitsBuilder) <- cgiarBase::replaceValues(Source = traitsBuilder, Search = c("qaRaw","qaGeno","sta","mta","indexD","ocs","rgg","pgg" ) , Replace = c("QA phenotypes (qaRaw)", "QA genotypes (qaGeno)", "Single Trial Analysis (sta)", "Multi Trial Analysis (mta)", "Selection Index (indexD)", "Optimal Cross Selection (ocs)", "Realized Genetic Gain (rgg)", "Predicted Genetic Gain (pgg)") )
        updateSelectInput(session, "module", choices = traitsBuilder )
      }
    })

    observeEvent(c(data(), input$module), {
      req(data())
      req(input$module)
      if(!is.null(data()$status)){
        status <- data()$status
        status <- status[status$module == input$module, ]
        traitsBuilder <- status$analysisId
        if(length(traitsBuilder) > 0){names(traitsBuilder) <- as.POSIXct(traitsBuilder, origin="1970-01-01", tz="GMT")}
        updateSelectInput(session, "timestamp", choices =traitsBuilder  )
      }
    })

    ## render timestamps flow
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
          ggnetwork::geom_nodes(ggplot2::aes(color = family), alpha = 0.5, size=5 ) +
          ggnetwork::geom_nodelabel_repel(ggplot2::aes(color = family, label = vertex.names ),
                                          fontface = "bold", box.padding = ggnetwork::unit(1, "lines")) +
          ggnetwork::theme_blank()
      }
    })

    #################################
    ### ANALYSIS

    ## render result of "run" button click
    outReport <- eventReactive(input$runReport, {
      req(data())
      req(input$module) # minimum requirements for the dashboard is the data and sta
      req(input$timestamp)
      ## start
      shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
      result <- data()
      moveTotheEnd <- which(result$status$analysisId == input$timestamp)
      keepAtTop <- setdiff(1:nrow(result$status), moveTotheEnd)
      result$status <- result$status[c(keepAtTop,moveTotheEnd),]
      markdownType <- cgiarBase::replaceValues(Source = input$module, Search = c("qaRaw","qaGeno","sta","mta","indexD","ocs","rgg","pgg" ) , Replace = c("reportQaPheno.Rmd","reportQaGeno.Rmd","reportSta.Rmd","reportMta.Rmd","reportIndex.Rmd","reportOcs.Rmd","reportRgg.Rmd","reportPgg.Rmd") )
      resultType <- cgiarBase::replaceValues(Source = input$module, Search = c("qaRaw","qaGeno","sta","mta","indexD","ocs","rgg","pgg" ) , Replace = c("resultQaPheno.RData","resultQaGeno.RData","resultSta.RData","resultMta.RData","resultIndex.RData","resultOcs.RData","resultRgg.RData","resultPgg.RData") )
      ## end
      shinybusy::remove_modal_spinner()
      if(!inherits(result,"try-error")) {
        data(result) # update data with results
        cat("Report ready. Please go to the report tab.")
        updateTabsetPanel(session, "tabsMain", selected = "outputTabs")
      }else{
        cat(paste("Analysis failed with the following error message: \n\n",result[[1]]))
      }
      ##

      if(!inherits(result,"try-error")) {

        ## Report tab
        output$reportReport <- renderUI({
          HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd",markdownType,package="bioflow"), quiet = TRUE), fragment.only=TRUE))
        })

        output$downloadReportReport <- downloadHandler(
          filename = function() {
            paste('my-report', sep = '.', switch(
              "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
            ))
          },
          content = function(file) {
            src <- normalizePath(system.file("rmd",markdownType,package="bioflow"))
            src2 <- normalizePath(paste0('data/',resultType))
            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'report.Rmd', overwrite = TRUE)
            file.copy(src2, resultType, overwrite = TRUE)
            out <- rmarkdown::render('report.Rmd',
                                     params = list(toDownload=TRUE ),
                                     switch(
                                       "HTML",
                                       HTML = rmarkdown::html_document()
                                     ))
            file.rename(out, file)
          }
        )

      } else {

      }

      hideAll$clearAll <- FALSE

    }) ## end eventReactive


    output$outReport <- renderPrint({
      outReport()
    })


  })
}

## To be copied in the UI
# mod_reportBuilder_ui("reportBuilder_1")

## To be copied in the server
# mod_reportBuilder_server("reportBuilder_1")

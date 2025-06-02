#' abiDashboard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_abiDashboard_ui <- function(id){
  ns <- NS(id)
  tagList(

    tags$br(),

    mainPanel(width = 12,
              tabsetPanel( id=ns("tabsMain"),
                           type = "tabs",
                           tabPanel(div(icon("book"), "Information") ,
                                    br(),
                                    tags$body(
                                      column(width = 6,
                                             h1(strong(span("Accelerated Breeding Dashboard Module", tags$a(href="https://www.youtube.com/watch?v=6Ooq9I3LEp8&list=PLZ0lafzH_UmclOPifjCntlMzysEB2_2wX&index=5", icon("youtube") , target="_blank"), style="color:darkcyan"))),
                                             h2(strong("Data Status (wait to be displayed):")),
                                             uiOutput(ns("warningMessage")),
                                             br(),
                                             shinyWidgets::prettySwitch( inputId = ns('launch'), label = "Load example data", status = "success"),
                                      ),
                                      column(width = 6,
                                             h2(strong("Details")),
                                             p("This module has the sole purpose of building a dashboard report for the Accelerated Breeding Initiative (ABI).
                                             The idea is that the user only has to specify the OCS and RGG analysis time stamps requested and a search for all needed metrics
                                             by ABI will be executed to build the desired dashboard. The arguments are used in the following way:"),

                                             p(strong("Module report-")," This argument is used to subset the time stamps to specific type of analyisis. For example, if 'sta' is
                                               selected only time stamps associated to sta analysis will be displayed in the next argument. ."),
                                             p(strong("Time stamp-"),"  This is a dropdown menu that contains the times stamps associated to the analysis type selected."),
                                             h2(strong("References")),
                                             p("Tukey, J. W. (1977). Exploratory Data Analysis. Section 2C."),
                                             p("Velleman, P. F. and Hoaglin, D. C. (1981). Applications, Basics and Computing of Exploratory Data Analysis. Duxbury Press."),
                                      )
                                    ),
                           ),
                           tabPanel(div(icon("arrow-right-to-bracket"), "Input steps"),
                                    tabsetPanel(
                                      tabPanel(div( icon("dice-one"), "Pick time stamps", icon("arrow-right") ), # icon = icon("magnifying-glass-chart"),
                                               br(),
                                               column(width=12,
                                                      column(width=5,  selectInput(ns("versionSelection"), "OCS analysis (to trace selection history)", choices = NULL, multiple = FALSE) ),
                                                      column(width=5,  selectInput(ns("versionHistory"), "RGG analysis (to link gain)", choices = NULL, multiple = FALSE) ),
                                                      style = "background-color:grey; color: #FFFFFF"),
                                               column(width=12),
                                               shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                   column(width=12,
                                                                          hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                          h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameters to be specified in the grey boxes above.", style="color:green"))),
                                                                          hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                   ),
                                                                   shiny::plotOutput(ns("plotTimeStamps")),
                                               ),
                                      ),
                                      tabPanel(div(icon("dice-two"),"Build dashboard"),
                                               br(),
                                               column(width=12,style = "background-color:grey; color: #FFFFFF",
                                                      br(),
                                                      actionButton(ns("runAbi"), "Build dashboard", icon = icon("play-circle")),
                                                      uiOutput(ns("qaQcAbiInfo")),
                                                      br(),
                                               ),
                                               textOutput(ns("outAbi")),
                                      ),

                                    )
                           ),
                           tabPanel(div(icon("arrow-right-from-bracket"), "Output tabs" ) , value = "outputTabs",
                                    tabsetPanel(
                                      tabPanel("Dashboard", icon = icon("file-image"),
                                               br(),
                                               downloadButton(ns("downloadReportAbi"), "Download dashboard"),
                                               br(),
                                               uiOutput(ns('reportAbi'))
                                      )
                                    )
                           )
              )) # end mainpanel

  )
}

#' abiDashboard Server Functions
#'
#' @noRd
mod_abiDashboard_server <- function(id, data){
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
        if(mappedColumns == 3){
          if(all(c("ocs","rgg") %in% data()$status$module)){
            HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to building of Accelerated Breeding dashboard.")) )
          }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please perform an optimal cross selection (OCS) and realized genetic gain (RGG) before building Accelerated Breeding dashboard.")) ) }
        }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please make sure that you have computed the 'environment' column, and that column 'designation' and \n at least one trait have been mapped using the 'Data Retrieval' tab.")) )}
      }
    )
    #################
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

    ## versions to use
    observeEvent(c(data()), {
      req(data())
      dtAbi <- data()
      dtAbi <- dtAbi$status
      if(!is.null(dtAbi)){
        dtAbi <- dtAbi[which(dtAbi$module %in% c("ocs")),]
        traitsAbi <- unique(dtAbi$analysisId)
        if(length(traitsAbi) > 0){names(traitsAbi) <- as.POSIXct(traitsAbi, origin="1970-01-01", tz="GMT")}
        updateSelectInput(session, "versionSelection", choices = traitsAbi)
      }
    })
    observeEvent(c(data()), {
      req(data())
      dtAbi <- data()
      dtAbi <- dtAbi$status
      if(!is.null(dtAbi)){
        dtAbi <- dtAbi[which(dtAbi$module %in% c("rgg")),]
        traitsAbi <- unique(dtAbi$analysisId)
        if(length(traitsAbi) > 0){names(traitsAbi) <- as.POSIXct(traitsAbi, origin="1970-01-01", tz="GMT")}
        updateSelectInput(session, "versionHistory", choices = traitsAbi)
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
          ggnetwork::theme_blank() + ggplot2::ggtitle("Network plot of current analyses available")
      }
    })

    #################################
    ### ANALYSIS

    ## render result of "run" button click
    outAbi <- eventReactive(input$runAbi, {
      req(data())
      # req(input$versionMetrics) # minimum requirements for the dashboard is the data and sta
      shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
      result <- data()
      idAbi <- as.numeric(Sys.time())
      abiModeling <- data.frame(module="abiDash", analysisId=idAbi, trait="inputObject", environment=NA,
                                parameter= c( "ocs", "rgg") , # "sta", "mta","indexD",
                                value=c(input$versionSelection, input$versionHistory ) # input$versionMetrics, input$versionTraits, input$versionIndex,
      )
      abiStatus <- data.frame(module="abiDash", analysisId=idAbi, analysisIdName ="")
      result$modeling <- rbind(result$modeling, abiModeling)
      result$status <- rbind(result$status, abiStatus)

      shinybusy::remove_modal_spinner()
      if(!inherits(result,"try-error")) {
        data(result) # update data with results
        cat("Data ready for dashboard. Please go to the report tab.")
        updateTabsetPanel(session, "tabsMain", selected = "outputTabs")
      }else{
        cat(paste("Analysis failed with the following error message: \n\n",result[[1]]))
      }
      ##

      if(!inherits(result,"try-error")) {

        ## Report tab
        output$reportAbi <- renderUI({
          HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportAbi.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
        })

        output$downloadReportAbi <- downloadHandler(
          filename = function() {
            paste(paste0('abiDash_dashboard_',gsub("-", "", as.integer(Sys.time()))), sep = '.', switch(
              "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
            ))
          },
          content = function(file) {
            shinybusy::show_modal_spinner(spin = "fading-circle", text = "Generating Report...")

            src <- normalizePath(system.file("rmd","reportAbi.Rmd",package="bioflow"))
            src2 <- normalizePath('data/resultAbi.RData')
            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'report.Rmd', overwrite = TRUE)
            file.copy(src2, 'resultAbi.RData', overwrite = TRUE)
            out <- rmarkdown::render('report.Rmd',
                                     params = list(toDownload=TRUE ),
                                     switch(
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

      } else {

      }

      hideAll$clearAll <- FALSE

    }) ## end eventReactive

    output$outAbi <- renderPrint({
      outAbi()
    })

  })
}

## To be copied in the UI
# mod_abiDashboard_ui("abiDashboard_1")

## To be copied in the server
# mod_abiDashboard_server("abiDashboard_1")

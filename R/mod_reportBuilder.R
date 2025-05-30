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
                           tabPanel(div(icon("book"), "Information") ,
                                    br(),
                                    tags$body(
                                      column(width = 6,
                                             h1(strong(span("Report Reconstruction Module", tags$a(href="https://www.youtube.com/watch?v=6Ooq9I3LEp8&list=PLZ0lafzH_UmclOPifjCntlMzysEB2_2wX&index=5", icon("youtube") , target="_blank"), style="color:darkcyan"))),
                                             h2(strong("Data Status (wait to be displayed):")),
                                             uiOutput(ns("warningMessage")),
                                             br(),
                                             shinyWidgets::prettySwitch( inputId = ns('launch'), label = "Load example data", status = "success"),
                                             tags$br(),
                                             img(src = "www/visuals.png", height = 430, width = 470), # add an image
                                      ),
                                      column(width = 6,
                                             h2(strong("Details")),
                                             p("This module has the sole purpose of rebuilding the dashboard reports when the user loads an analysis object and doesn't require to
                                               re-run an analysis. The idea is that the user only has to specify the analysis type and time stamp and this should suffice.
                                               The arguments are used in the following way:"),

                                             p(strong("Module report-")," This argument is used to subset the time stamps to specific type of analyisis. For example, if 'sta' is
                                               selected only time stamps associated to sta analysis will be displayed in the next argument. ."),
                                             p(strong("Time stamp-"),"  This is a dropdown menu that contains the times stamps associated to the analysis type selected."),
                                             h2(strong("References")),
                                             p("Tukey, J. W. (1977). Exploratory Data Analysis. Section 2C."),
                                             p("Velleman, P. F. and Hoaglin, D. C. (1981). Applications, Basics and Computing of Exploratory Data Analysis. Duxbury Press."),

                                      ),
                                    )
                           ),
                           tabPanel(div(icon("arrow-right-to-bracket"), "Input steps"),
                                    tabsetPanel(
                                      tabPanel(div(icon("dice-one"), "Pick module and timestamp", icon("arrow-right") ), # icon = icon("magnifying-glass-chart"),
                                               br(),
                                               column(width=12,
                                                      column(width=5,  selectInput(ns("module"), "Module report", choices = NULL, multiple = FALSE) ),
                                                      column(width=5,  selectInput(ns("timestamp"), "Time stamp", choices = NULL, multiple = FALSE) ),

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
                                                      actionButton(ns("runReport"), "Build dashboard", icon = icon("play-circle")),
                                                      textOutput(ns("outReport")),
                                                      br(),
                                               ),

                                      ),
                                    )
                           ),
                           tabPanel(div(icon("arrow-right-from-bracket"), "Output tabs" ) , value = "outputTabs",
                                    tabsetPanel(
                                      tabPanel("Dashboard", icon = icon("file-image"),
                                               br(),
                                               downloadButton(ns("downloadReportReport"), "Download dashboard"),
                                               br(),
                                               uiOutput(ns('reportReport'))
                                      ),
                                      tabPanel("Predictions", icon = icon("table"),
                                               br(),
                                               DT::DTOutput(ns("predictionsX")),
                                      ),
                                      tabPanel("Metrics", icon = icon("table"),
                                               br(),
                                               DT::DTOutput(ns("metricsX")),
                                      ),
                                      tabPanel("Modeling", icon = icon("table"),
                                               br(),
                                               DT::DTOutput(ns("modelingX")),
                                      ),
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
    # warning message
    output$warningMessage <- renderUI(
      if(is.null(data())){
        HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your phenotypic data using the 'Data Retrieval' tab.")) )
      }else{ # data is there
        mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
        if(mappedColumns == 3){
          if(length(data()$status$module) > 0){
            HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to rebuilding available dashboard.")) )
          }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please make sure that any analysis has been done before rebuilding a dashboard.")) ) }
        }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please make sure that you have computed the 'environment' column, and that column 'designation' and \n at least one trait have been mapped using the 'Data Retrieval' tab.")) )}
      }
    )

    ## model types
    observeEvent(c(data()), {
      req(data()) # list(QA="qaRaw" , QAmarkers="qaGeno" , STA="sta" ,   MTA="mta",    Index="indexD", OCS="ocs",    RGG="rgg" ,   PGG="pgg" , OFT="oft")
      if(!is.null(data()$status)){
        traitsBuilder <- unique(data()$status$module)
        names(traitsBuilder) <- cgiarBase::replaceValues(Source = traitsBuilder, Search = c("qaRaw","qaGeno","sta","mta","mtaFlex","mtaLmms","indexD","ocs","rgg","pgg","oft","neMarker","gVerif","mas","abiDash" ) , Replace = c("QA phenotypes (qaRaw)", "QA genotypes (qaGeno)", "Single Trial Analysis (sta)", "Multi Trial Analysis (mta)", "Multi Trial Analysis (mtaFlex)","Multi Trial Analysis (mtaLmms)", "Selection Index (indexD)", "Optimal Cross Selection (ocs)", "Realized Genetic Gain (rgg)", "Predicted Genetic Gain (pgg)", "On Farm Trial (oft)", "Number of Founders (ne)", "Genotype verification (gVerif)","Marker assisted selection (mas)", "ABI dashboard (abiDash)") )
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

    ## render timestamps flow
    output$plotTimeStamps <- shiny::renderPlot({
      req(data()) # req(input$version2Sta)
      xx <- data()$status;  yy <- data()$modeling # xx <- result$status;  yy <- result$modeling
      if("analysisIdName" %in% colnames(xx)){existNames=TRUE}else{existNames=FALSE}
      if(existNames){
        networkNames <- paste(xx$analysisIdName, as.character(as.POSIXct(as.numeric(xx$analysisId), origin="1970-01-01", tz="GMT")),sep = "_" )
        xx$analysisIdName <- as.character(as.POSIXct(as.numeric(xx$analysisId), origin="1970-01-01", tz="GMT"))
      }
      v <- which(yy$parameter == "analysisId")
      if(length(v) > 0){
        yy <- yy[v,c("analysisId","value")]
        zz <- merge(xx,yy, by="analysisId", all.x = TRUE)
      }else{ zz <- xx; zz$value <- NA}
      if(existNames){
        zz$analysisIdName <- cgiarBase::replaceValues(Source = zz$analysisIdName, Search = "", Replace = "?")
        zz$analysisIdName2 <- cgiarBase::replaceValues(Source = zz$value, Search = zz$analysisId, Replace = zz$analysisIdName)
      }
      if(!is.null(xx)){
        if(existNames){
          colnames(zz) <- cgiarBase::replaceValues(colnames(zz), Search = c("analysisIdName","analysisIdName2"), Replace = c("outputId","inputId") )
        }else{
          colnames(zz) <- cgiarBase::replaceValues(colnames(zz), Search = c("analysisId","value"), Replace = c("outputId","inputId") )
        }
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
        };
        rownames(X) <- networkNames
        colnames(X) <- networkNames
        if(existNames){

        }else{
          rownames(X) <-as.character(as.POSIXct(as.numeric(rownames(X)), origin="1970-01-01", tz="GMT"))
          colnames(X) <-as.character(as.POSIXct(as.numeric(colnames(X)), origin="1970-01-01", tz="GMT"))
        }
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
      markdownType <- cgiarBase::replaceValues(Source = input$module, Search = c("qaRaw","qaGeno","sta","mta","mtaFlex","mtaLmms","indexD","ocs","rgg","pgg","oft","neMarker","gVerif","mas","abiDash" ) , Replace = c("reportQaPheno.Rmd","reportQaGeno.Rmd","reportSta.Rmd","reportMta.Rmd","reportMtaFlex.Rmd","reportMtaLMMsolver.Rmd","reportIndex.Rmd","reportOcs.Rmd","reportRgg.Rmd","reportPgg.Rmd", "reportOft.Rmd", "reportNeGeno.Rmd", "reportVerifGeno.Rmd","reportMas.Rmd","reportAbi.Rmd") )
      resultType <- cgiarBase::replaceValues(Source = input$module, Search = c("qaRaw","qaGeno","sta","mta","mtaFlex","mtaLmms","indexD","ocs","rgg","pgg","oft","neMarker","gVerif","mas","abiDash" ) , Replace = c("resultQaPheno.RData","resultQaGeno.RData","resultSta.RData","resultMta.RData", "resultMtaFlex.RData","resultMtaLMMsolver.RData","resultIndex.RData","resultOcs.RData","resultRgg.RData","resultPgg.RData","resultOft.RData", "resultNeGeno.RData", "reportVerifGeno.Rmd", "reportMas.Rmd","reportAbi.Rmd") )
      ## end
      if(!inherits(result,"try-error")) {
        data(result) # update data with results
        cat("Report ready. Please go to the report tab.")
        updateTabsetPanel(session, "tabsMain", selected = "outputTabs")
      }else{
        cat(paste("Analysis failed with the following error message: \n\n",result[[1]]))
      }
      ##
      shinybusy::remove_modal_spinner()

      if(!inherits(result,"try-error")) {

        ## predictions table
        output$predictionsX <-  DT::renderDT({
          predictions <- result$predictions
          current.predictions <- predictions[predictions$analysisId == result$status$analysisId[nrow(result$status)] ,]
          if(nrow(current.predictions) > 0){
            current.predictions <- subset(current.predictions, select = -c(module,analysisId))
            numeric.output <- c("predictedValue", "stdError", "reliability")
            DT::formatRound(DT::datatable(current.predictions, extensions = 'Buttons',
                                          options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                         lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
            ), numeric.output)
          }else{
            DT::datatable(data.frame(), extensions = 'Buttons',
                          options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                         lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
            )
          }
        }, server = FALSE)
        # metrics table
        output$metricsX <-  DT::renderDT({
          if(!inherits(result,"try-error") ){
            metrics <- result$metrics
            metrics <- metrics[which(metrics$analysisId == result$status$analysisId[nrow(result$status)] ),]
            if(nrow(metrics) > 0){
              metrics <- subset(metrics, select = -c(module,analysisId))
              numeric.output <- c("value", "stdError")
              DT::formatRound(DT::datatable(metrics, extensions = 'Buttons',
                                            options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                           lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
              ), numeric.output)
            }else{
              DT::datatable(data.frame(), extensions = 'Buttons',
                            options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                           lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
              )
            }
          }
        }, server = FALSE)
        # modeling table
        output$modelingX <-  DT::renderDT({
          if(!inherits(result,"try-error") ){
            modeling <- result$modeling
            modeling <- modeling[which(modeling$analysisId == result$status$analysisId[nrow(result$status)] ),]
            if(nrow(modeling) > 0){
              modeling <- subset(modeling, select = -c(module,analysisId))
            }else{modeling <- data.frame()}
            DT::datatable(modeling, extensions = 'Buttons',
                          options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                         lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
            )
          }
        }, server = FALSE)

        if(result$status$module[nrow(result$status)] != "oft"){
          ## Report tab
          output$reportReport <- shiny::renderUI({
            HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd",markdownType,package="bioflow"), quiet = TRUE), fragment.only=TRUE))
          })

          output$downloadReportReport <- shiny::downloadHandler(
            filename = function() {
              paste(paste0(result$status$module[nrow(result$status)],"_dashboard_",gsub("-", "", as.integer(Sys.time()))), sep = '.', switch(
                "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
              ))
            },
            content = function(file) {
              shinybusy::show_modal_spinner(spin = "fading-circle", text = "Downloading Dashboard...")

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
        } else{
          subOft <- result$modeling[which(result$modeling$module == "oft" & result$modeling$analysisId == input$timestamp),]
          paramsTraits <- eval(parse(text=ifelse(length(subOft[which(subOft$parameter == "traits"), "value"])>0,subOft[which(subOft$parameter == "traits"), "value"],NULL)))
          paramsFieldinst <- eval(parse(text=ifelse(length(subOft[which(subOft$parameter == "fieldinst"), "value"])>0,subOft[which(subOft$parameter == "fieldinst"), "value"],NULL)))
          paramsMdisease <- ifelse(!is.null(subOft[which(subOft$parameter == "mdisease"), "value"]),subOft[which(subOft$parameter == "mdisease"), "value"],NULL)
          paramsTdisease <- ifelse(!is.null(subOft[which(subOft$parameter == "tdisease"), "value"]),subOft[which(subOft$parameter == "tdisease"), "value"],NULL)
          paramsSdisease <- ifelse(!is.null(subOft[which(subOft$parameter == "sdisease"), "value"]),subOft[which(subOft$parameter == "sdisease"), "value"],NULL)
          paramsVersion <- ifelse(length(subOft[which(subOft$parameter == "analysisId"), "value"])>0,subOft[which(subOft$parameter == "analysisId"), "value"],NULL)

          ## Report tab
          out2 <- rmarkdown::render(input = system.file("rmd",markdownType,package="bioflow"),
                                    output_format = rmarkdown::html_fragment(),
                                    params = list(traits = paramsTraits,
                                                  fieldinst = paramsFieldinst,
                                                  mdisease = paramsMdisease,
                                                  tdisease = paramsTdisease,
                                                  sdisease = paramsSdisease,
                                                  version = paramsVersion),
                                    quiet = TRUE)

          output$reportReport <- shiny::renderUI({
            shiny::withMathJax(HTML(readLines(out2)))
          })

          output$downloadReportReport <- shiny::downloadHandler(
            filename = function() {
              paste(paste0(result$status$module[nrow(result$status)],"_dashboard_",gsub("-", "", as.integer(Sys.time()))), sep = '.', switch(
                "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
              ))
            },
            content = function(file) {
              shinybusy::show_modal_spinner(spin = "fading-circle", text = "Downloading Dashboard...")

              src <- normalizePath(system.file("rmd",markdownType,package="bioflow"))
              src2 <- normalizePath(paste0('data/',resultType))

              # temporarily switch to the temp dir, in case you do not have write
              # permission to the current working directory
              owd <- setwd(tempdir())
              on.exit(setwd(owd))

              file.copy(src, 'report2.Rmd', overwrite = TRUE)
              file.copy(src2, resultType, overwrite = TRUE)

              out3 <- rmarkdown::render('report2.Rmd',
                                        params = list(toDownload = TRUE,
                                                      traits = paramsTraits,
                                                      fieldinst = paramsFieldinst,
                                                      mdisease = paramsMdisease,
                                                      tdisease = paramsTdisease,
                                                      sdisease = paramsSdisease,
                                                      version = paramsVersion),
                                        switch("HTML",HTML = rmdformats::robobook(toc_depth = 4)))

              # wait for it to land on disk (safety‐net)
              wait.time <- 0
              while (!file.exists(out3) && wait.time < 60) {
                Sys.sleep(1); wait.time <- wait.time + 1
              }

              file.rename(out3, file)
              shinybusy::remove_modal_spinner()
            }
          )
        }
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

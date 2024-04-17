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

    mainPanel( width = 12,
               tabsetPanel( id=ns("tabsMain"),
                            type = "tabs",
                            tabPanel(div(icon("book"), "Information-RGG") ,
                                     br(),
                                     shinydashboard::box(status="success",width = 12,
                                                         solidHeader = TRUE,
                                                         column(width=12,   style = "height:580px; overflow-y: scroll;overflow-x: scroll;",
                                                                column(width = 6,
                                                                       h1(strong(span("Realized Genetic Gain", tags$a(href="https://www.youtube.com/channel/UCikAyaDKdC5LAtcbVePWgIg", icon("youtube") , target="_blank"), style="color:green"))),
                                                                       h2(strong("Status:")),
                                                                       uiOutput(ns("warningMessage")),
                                                                ),
                                                                column(width = 6, shiny::plotOutput(ns("plotDataDependencies")), ),
                                                                column(width = 12,
                                                                       h2(strong("Details")),
                                                                       p("In order to monitor the efficacy of genetic evaluation across cycles of selection, the realized genetic gain is the preferred process.
                                          This option aims to calculate the realized genetic gain using the methods from Mackay et al. (2011). The
                              method uses across-environment means from multiple years of data that have been adjusted based on a good connectivity
                              to then fit a regression of the form means~year.of.origin. In case the means used are BLUPs these can be
                              deregressed.
                                The way the options are used is the following:"),
                                                                       p(strong("Method.-")," One of the following; Mackay et al. (2011) or Laidig et al. (2014)."),
                                                                       p(strong("Trait(s) to use.-")," Trait to be be used for realized genetic gain estimation (an index is suggested)."),
                                                                       p(strong("Years of origin to use.-")," Selection of the years of origin associated to the tested material to use in the calculation."),
                                                                       p(strong("Entry types to use.-")," A selection of entry types to use for the realized genetic gain calculation."),
                                                                       p(strong("Deregress weight.-")," Should any weight be applied to the deregressed value (not recommended but available)."),
                                                                       p(strong("Partition the data?.-")," When very few years of data are present this option will allow the user to calculate the gain for all 2-year combinations and then average these rates."),
                                                                       p(strong("Deregress estimates-")," Should we deregress the estimates by dividing over the reliability before performing the realized genetic gain calculation."),
                                                                       h2(strong("References:")),
                                                                       p("Mackay, I., Horwell, A., Garner, J., White, J., McKee, J., & Philpott, H. (2011). Reanalyses of the historical series of UK variety trials
                                to quantify the contributions of genetic and environmental factors to trends and variability in yield over time. Theoretical and Applied
                                Genetics, 122, 225-238."),
                                                                       p("Laidig, F., Piepho, H. P., Drobek, T., & Meyer, U. (2014). Genetic and non-genetic long-term trends of 12 different crops in German
                                official variety performance trials and on-farm yield trends. Theoretical and Applied Genetics, 127, 2599-2617."),
                                                                       h2(strong("Software used:")),
                                                                       p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
                                Vienna, Austria. URL https://www.R-project.org/."),
                                                                ),
                                                         )
                                     )
                            ),
                            tabPanel(div(icon("arrow-right-to-bracket"), "Input"),
                                     tabsetPanel(
                                       tabPanel("Pick Index-stamp", icon = icon("table"),
                                                br(),
                                                column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                       column(width=6, selectInput(ns("version2Rgg"), "Data version to analyze", choices = NULL, multiple = FALSE) ),
                                                       column(width=6, radioButtons(ns("methodRgg"),"Method",choices=list("Mackay"="mackay","Piepho"="piepho"), selected="mackay") ),
                                                ),
                                                column(width=12,
                                                       hr(style = "border-top: 3px solid #4c4c4c;"),
                                                       h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameter values to be specified in the grey boxes above.", style="color:green"))),
                                                       hr(style = "border-top: 3px solid #4c4c4c;"),
                                                ),
                                                shinydashboard::box(status="success",width = 12, style = "height:460px; overflow-y: scroll;overflow-x: scroll;",
                                                                    solidHeader = TRUE,
                                                                    column(width=12,
                                                                           p(span("Network plot of current analyses available.", style="color:black")),
                                                                           shiny::plotOutput(ns("plotTimeStamps")),
                                                                           p(span("Predictions table.", style="color:black")),
                                                                           DT::DTOutput(ns("phenoRgg")),
                                                                    )
                                                )
                                       ),
                                       tabPanel("Select trait(s)", icon = icon("magnifying-glass-chart"),
                                                br(),
                                                column(width=12, selectInput(ns("trait2Rgg"), "Trait(s) to use", choices = NULL, multiple = TRUE), style = "background-color:grey; color: #FFFFFF"),
                                                column(width=12,
                                                       hr(style = "border-top: 3px solid #4c4c4c;"),
                                                       h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameter values to be specified in the grey boxes above.", style="color:green"))),
                                                       hr(style = "border-top: 3px solid #4c4c4c;"),
                                                ),
                                                shinydashboard::box(status="success",width = 12, solidHeader = TRUE,
                                                                    column(width=12, style = "height:450px; overflow-y: scroll;overflow-x: scroll;",
                                                                           p(span("View of current analyses available.", style="color:black")),
                                                                           selectInput(ns("trait3Rgg"), "Trait to visualize regression over years.", choices = NULL, multiple = FALSE),
                                                                           plotly::plotlyOutput(ns("plotPredictionsCleanOut")),
                                                                    ),
                                                )
                                       ),
                                       tabPanel("Select years & units", icon = icon("table"),
                                                br(),
                                                column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                       column(width=6, selectInput(ns("yearsToUse"), "Years of origin to use", choices = NULL, multiple = TRUE) ),
                                                       column(width=6, selectInput(ns("entryTypeToUse"), "Entry types to use", choices = NULL, multiple = TRUE) ),
                                                ),
                                                column(width=12,
                                                       hr(style = "border-top: 3px solid #4c4c4c;"),
                                                       h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameter values to be specified in the grey boxes above.", style="color:green"))),
                                                       hr(style = "border-top: 3px solid #4c4c4c;"),
                                                       column(width=12, style = "height:450px; overflow-y: scroll;overflow-x: scroll;",
                                                              p(span("View of current analyses available.", style="color:black")),
                                                              selectInput(ns("trait3Rgg2"), "Trait to visualize regression over years.", choices = NULL, multiple = FALSE),
                                                              plotly::plotlyOutput(ns("plotPredictionsCleanOut2")),
                                                       ),
                                                ),
                                       ),
                                       tabPanel("Run analysis", icon = icon("play"),
                                                br(),
                                                actionButton(ns("runRgg"), "Run RGG", icon = icon("play-circle")),
                                                uiOutput(ns("qaQcRggInfo")),
                                                textOutput(ns("outRgg")),
                                                hr(style = "border-top: 3px solid #4c4c4c;"),
                                                column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                       shinydashboard::box(width = 12, status = "success", background="green",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Additional run settings...",
                                                                           tags$span(id = ns('mackayOptions'),
                                                                                     numericInput(ns("deregressWeight"), label = "Deregression weight", value = 1),
                                                                                     selectInput(ns("partition"), "Partitioned regression", choices = list(TRUE,FALSE), selected = FALSE, multiple=FALSE),
                                                                           ),
                                                                           tags$span(id = ns('piephoOptions'),
                                                                                     numericInput(ns("sampleN"), label = "Number of entries per environment to sample", value = 50),
                                                                                     numericInput(ns("bootstrappingN"), label = "Number of bootstrapping per samples", value = 10),
                                                                           ),
                                                                           selectInput(ns("deregress"), "Should deregress estimates", choices = list(TRUE,FALSE), selected = FALSE, multiple=FALSE),
                                                                           selectInput(ns("verbose"), label = "Print logs?", choices = list(TRUE,FALSE), selected = FALSE, multiple=FALSE)
                                                       ),
                                                ),
                                       ),
                                     )
                            ),
                            tabPanel(div(icon("arrow-right-from-bracket"), "Output" ) , value = "outputTabs",
                                     tabsetPanel(
                                       tabPanel("Metrics", icon = icon("table"),
                                                br(),
                                                shinydashboard::box(status="success",width = 12,
                                                                    solidHeader = TRUE,
                                                                    column(width=12,br(),DT::DTOutput(ns("metricsRgg")),style = "height:530px; overflow-y: scroll;overflow-x: scroll;"),
                                                )
                                       ),
                                       tabPanel("Modeling", icon = icon("table"),
                                                br(),
                                                shinydashboard::box(status="success",width = 12,
                                                                    solidHeader = TRUE,
                                                                    column(width=12,DT::DTOutput(ns("modelingRgg")),style = "height:530px; overflow-y: scroll;overflow-x: scroll;"),
                                                )
                                       ),
                                       tabPanel("Dashboard", icon = icon("file-image"),
                                                br(),
                                                downloadButton(ns("downloadReportRgg"), "Download dashboard"),
                                                br(),
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

    output$plotDataDependencies <- shiny::renderPlot({ dependencyPlot() })
    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })
    #
    observeEvent(
      input$methodRgg,
      if(length(input$methodRgg) > 0){ # added
        if (input$methodRgg == 'piepho') {
          golem::invoke_js('showid', ns('piephoOptions'))
          golem::invoke_js('hideid', ns('mackayOptions'))
        } else if (input$methodRgg == 'mackay') {
          golem::invoke_js('showid', ns('mackayOptions'))
          golem::invoke_js('hideid', ns('piephoOptions'))
        }
      }
    )
    ############################################################################
    output$warningMessage <- renderUI(
      if(is.null(data())){
        HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your phenotypic data using the 'Data Retrieval' tab, compute the 'environment' column, map the 'designation' and at least one trait.")) )
      }else{ # data is there
        mappedColumns <- setdiff(data()$metadata$pedigree[data()$metadata$pedigree$parameter == "yearOfOrigin","value"],"")
        # mappedColumns <- length(which(c("yearOfOrigin") %in% colnames(data()$medata$pedigree)))
        if(length(mappedColumns) == 1){
          if("mta" %in% data()$status$module){
            myYearOfOrigin <- data()$metadata$pedigree[data()$metadata$pedigree$parameter=="yearOfOrigin","value"]
            if(!is.null(myYearOfOrigin) & !is.na(myYearOfOrigin)){
              HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to perform the realized genetic gain specifying your input parameters under the Input tabs.")) )
            }else{
              HTML( as.character(div(style="color: red; font-size: 20px;", "Please make sure to map the column 'yearOfOrigin' in the 'Pedigree data' extraction section under the 'Data Retrieval' to perform the realized genetic gain analysis.")) )
            }
          }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please perform a Multi-Trial Analysis before performing a realized genetic gain analysis.")) ) }
        }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please make sure that the column: 'yearOfOrigin' has been mapped in the pedigree data using the 'Data Retrieval' tab.")) )}
      }
    )
    #################
    ## version
    observeEvent(c(data(), input$methodRgg), {
      req(data())
      req(input$methodRgg)
      dtRgg <- data()
      dtRgg <- dtRgg$status
      if(input$methodRgg == "piepho"){
        dtRgg <- dtRgg[which(dtRgg$module %in% c("sta")),]
      }else if(input$methodRgg == "mackay"){
        dtRgg <- dtRgg[which(dtRgg$module %in% c("mta","indexD")),]
      }
      traitsRgg <- unique(dtRgg$analysisId)
      if(length(traitsRgg) > 0){names(traitsRgg) <- as.POSIXct(traitsRgg, origin="1970-01-01", tz="GMT")}
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
    ##############
    ## years
    observeEvent(c(data(), input$version2Rgg, input$trait2Rgg), {
      req(data())
      req(input$version2Rgg)
      req(input$trait2Rgg)
      dtRgg <- data()
      dtRgg <- dtRgg$predictions
      dtRgg <- dtRgg[which(dtRgg$analysisId == input$version2Rgg),]
      myYears <- data()$data$pedigree
      paramsPed <- data()$metadata$pedigree
      colnames(myYears) <- cgiarBase::replaceValues(colnames(myYears), Search = paramsPed$value, Replace = paramsPed$parameter )
      dtRgg <- merge(dtRgg, myYears[,c("designation","yearOfOrigin")], by="designation", all.x=TRUE)
      traitsRgg <- sort(unique(dtRgg$yearOfOrigin), decreasing = FALSE)
      updateSelectInput(session, "yearsToUse", choices = traitsRgg, selected =traitsRgg )
    })
    ## entry type
    observeEvent(c(data(), input$version2Rgg, input$trait2Rgg), {
      req(data())
      req(input$version2Rgg)
      req(input$trait2Rgg)
      dtRgg <- data()
      dtRgg <- dtRgg$predictions
      dtRgg <- dtRgg[which(dtRgg$analysisId == input$version2Rgg),]
      traitsRgg <- unique(dtRgg$entryType)
      updateSelectInput(session, "entryTypeToUse", choices = traitsRgg, selected =traitsRgg )
    })
    ##############################################################################################
    ##############################################################################################
    ##############################################################################################
    ## render trait distribution plot
    observeEvent(c(data(),input$version2Rgg), { # update trait
      req(data())
      req(input$version2Rgg)
      dtRgg <- data()
      dtRgg <- dtRgg$predictions
      dtRgg <- dtRgg[which(dtRgg$analysisId %in% input$version2Rgg),] # only traits that have been QA
      traitRggInput <- unique(dtRgg$trait)
      updateSelectInput(session, "trait3Rgg", choices = traitRggInput)
      updateSelectInput(session, "trait3Rgg2", choices = traitRggInput)
    })
    output$plotPredictionsCleanOut <- plotly::renderPlotly({ # update plot
      req(data())
      req(input$trait3Rgg)
      ##
      mydata <- data()$predictions
      paramsPheno <- data()$metadata$pheno
      paramsPheno <- paramsPheno[which(paramsPheno$parameter != "trait"),]
      colnames(mydata) <- cgiarBase::replaceValues(colnames(mydata), Search = paramsPheno$value, Replace = paramsPheno$parameter )
      ##
      myYears <- data()$data$pedigree
      paramsPed <- data()$metadata$pedigree
      colnames(myYears) <- cgiarBase::replaceValues(colnames(myYears), Search = paramsPed$value, Replace = paramsPed$parameter )

      mydata <- merge(mydata, myYears[,c("designation","yearOfOrigin")], by="designation", all.x=TRUE)
      mydata <- mydata[which(mydata[,"trait"] %in% input$trait3Rgg),]
      mydata <- mydata[which(mydata$entryType == input$entryTypeToUse),]
      mydata[, "environment"] <- as.factor(mydata[, "environment"]); mydata[, "designation"] <- as.factor(mydata[, "designation"])
      res <- plotly::plot_ly(y = mydata[,"predictedValue"], type = "scatter", boxpoints = "all", color = mydata[,"entryType"],
                             x = mydata[,"yearOfOrigin"], text=mydata[,"designation"], pointpos = -1.8)
      # res = res %>% plotly::layout(showlegend = TRUE,  xaxis = list(titlefont = list(size = input$fontSize), tickfont = list(size = input$fontSize)))
      res
    })
    output$plotPredictionsCleanOut2 <- plotly::renderPlotly({ # update plot
      req(data())
      req(input$trait3Rgg2)
      ##
      mydata <- data()$predictions
      paramsPheno <- data()$metadata$pheno
      paramsPheno <- paramsPheno[which(paramsPheno$parameter != "trait"),]
      colnames(mydata) <- cgiarBase::replaceValues(colnames(mydata), Search = paramsPheno$value, Replace = paramsPheno$parameter )
      ##
      myYears <- data()$data$pedigree
      paramsPed <- data()$metadata$pedigree
      colnames(myYears) <- cgiarBase::replaceValues(colnames(myYears), Search = paramsPed$value, Replace = paramsPed$parameter )

      mydata <- merge(mydata, myYears[,c("designation","yearOfOrigin")], by="designation", all.x=TRUE)
      mydata <- mydata[which(mydata[,"trait"] %in% input$trait3Rgg2),]
      mydata <- mydata[which(mydata$entryType == input$entryTypeToUse),]
      mydata[, "environment"] <- as.factor(mydata[, "environment"]); mydata[, "designation"] <- as.factor(mydata[, "designation"])
      res <- plotly::plot_ly(y = mydata[,"predictedValue"], type = "scatter", boxpoints = "all", color = mydata[,"entryType"],
                             x = mydata[,"yearOfOrigin"], text=mydata[,"designation"], pointpos = -1.8)
      # res = res %>% plotly::layout(showlegend = TRUE,  xaxis = list(titlefont = list(size = input$fontSize), tickfont = list(size = input$fontSize)))
      res
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
          if(nLevelsCheck1 == 1){
            X1 <- matrix(ifelse(is.na(zz$inputId),0,1),nrow=length(zz$inputId),1); colnames(X1) <- as.character(na.omit(unique(c(zz$outputId))))
          }else{X1 <- model.matrix(~as.factor(outputId)-1, data=zz); colnames(X1) <- levels(as.factor(zz$outputId))}
          if(nLevelsCheck2 == 1){
            X2 <- matrix(ifelse(is.na(zz$inputId),0,1),nrow=length(zz$inputId),1); colnames(X2) <- as.character(na.omit(unique(c(zz$inputId))))
          }else{X2 <- model.matrix(~as.factor(inputId)-1, data=zz); colnames(X2) <- levels(as.factor(zz$inputId))}
          mynames <- unique(na.omit(c(zz$outputId,zz$inputId)))
          X <- matrix(0, nrow=nrow(zz), ncol=length(mynames)); colnames(X) <- as.character(mynames)
          X[,colnames(X1)] <- X1
          X[,colnames(X2)] <- X2
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
        ggplot2::ggplot(n, ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
          ggnetwork::geom_edges(ggplot2::aes(color = family), arrow = ggplot2::arrow(length = ggnetwork::unit(6, "pt"), type = "closed") ) +
          ggnetwork::geom_nodes(ggplot2::aes(color = family), alpha = 0.5, size=5 ) +
          ggnetwork::geom_nodelabel_repel(ggplot2::aes(color = family, label = vertex.names ),
                                          fontface = "bold", box.padding = ggnetwork::unit(1, "lines")) +
          ggnetwork::theme_blank()
      }
    })
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
      req(input$methodRgg)
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
        if(input$methodRgg == "piepho"){
          result <- try(cgiarPipeline::rggPiepho(
            phenoDTfile= dtRgg,
            analysisId=input$version2Rgg,
            trait=input$trait2Rgg, # per trait
            deregress=input$deregress,
            yearsToUse=input$yearsToUse,
            sampleN = input$sampleN,
            bootstrappingN = input$bootstrappingN,
            verbose=input$verbose
          ),
          silent=TRUE
          )
        }else if(input$methodRgg == "mackay"){
          result <- try(cgiarPipeline::rggMackay(
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
        }
        if(!inherits(result,"try-error")) {
          data(result) # update data with results
          # save(result, file = "./R/outputs/resultRgg.RData")
          cat(paste("Realized genetic gain step with id:",as.POSIXct( result$status$analysisId[length(result$status$analysisId)], origin="1970-01-01", tz="GMT"),"saved."))
          updateTabsetPanel(session, "tabsMain", selected = "outputTabs")
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

        ## Report tab
        # output$reportRgg <- renderUI({
        output$reportRgg <- renderUI({
          HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportRgg.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
        })

        output$downloadReportRgg <- downloadHandler(
          filename = function() {
            paste('my-report', sep = '.', switch(
              "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
            ))
          },
          content = function(file) {
            src <- normalizePath(system.file("rmd","reportRgg.Rmd",package="bioflow"))
            src2 <- normalizePath('data/resultRgg.RData')
            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'report.Rmd', overwrite = TRUE)
            file.copy(src2, 'resultRgg.RData', overwrite = TRUE)
            out <- rmarkdown::render('report.Rmd', params = list(toDownload=TRUE),switch(
              "HTML",
              HTML = rmarkdown::html_document()
            ))
            file.rename(out, file)
          }
        )

      } else {
        output$predictionsRgg <- DT::renderDT({DT::datatable(NULL)})
        output$metricsRgg <- DT::renderDT({DT::datatable(NULL)})
        output$modelingRgg <- DT::renderDT({DT::datatable(NULL)})
      }

      hideAll$clearAll <- FALSE

    }) ## end eventReactive

    output$outRgg <- renderPrint({
      outRgg()
    })


  })
}

## To be copied in the UI
# mod_rggApp_ui("rggApp_1")

## To be copied in the server
# mod_rggApp_server("rggApp_1")

#' oftStaApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_oftStaApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    mainPanel(width = 12,
              tabsetPanel( id=ns("tabsMain"),
                           type = "tabs",
                           tabPanel(div(icon("book"), "Information-OFT") ,
                                    br(),
                                    shinydashboard::box(status="success",width = 12,
                                                        solidHeader = TRUE,
                                                        column(width=12,   style = "height:580px; overflow-y: scroll;overflow-x: scroll;",
                                                               h1(strong(span("On Farm Trial Analysis", tags$a(href="https://www.youtube.com/channel/UCikAyaDKdC5LAtcbVePWgIg", icon("youtube") , target="_blank"),  style="color:darkcyan"))),
                                                               h2(strong("Status:")),
                                                               uiOutput(ns("warningMessage")),
                                                               h2(strong("Details")),
                                                               p("Given the high yield gap
between controlled conditions on experimental stations and farmers’ fields, the best hybrids need to be
evaluated under farmers’ conditions, together with appropriate commercial benchmark hybrids and internal
checks. The On-Farm Trials (OFT) are implemented in collaboration with partners from national agricultural research
systems (NARS), seed companies, and non-governmental organizations (NGOs). The overall goal of the On-Farm Trials is to assess the performance of a set of new varieties, which are selected
from a rigorous stage-gate advancement process, under farmers’ conditions to identify promising hybrids
that perform well under farmers’ conditions before these are announced to the partners for further uptake.
                                The way the options are used is the following:"),
                                                               p(strong("Trait(s) to include.-")," Traits to be included in the report. It only includes analyzed traits from sta."),
                                                               p(strong("Year of origin.-")," The name of the column containing the year when the genotype originated."),
                                                               p(strong("Entry type.-")," The name of the column containing the labels of the genotype category (check, tester, entry, etc.)."),
                                                               p(strong("Environment(s) to include.-")," Environments to be included in the report. It only includes analyzed environments from sta."),
                                                        )
                                    )
                           ),
                           tabPanel(div(icon("arrow-right-to-bracket"), "Input"),
                                    tabsetPanel(
                                      tabPanel("Pick STA-stamp", icon = icon("table"),
                                               br(),
                                               column(width=12, selectInput(ns("version2Oft"), "STA version to use", choices = NULL, multiple = FALSE),  style = "background-color:grey; color: #FFFFFF"),
                                               column(width=12,
                                                      hr(style = "border-top: 3px solid #4c4c4c;"),
                                                      h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameter values to be specified in the grey boxes above.", style="color:green"))),
                                                      hr(style = "border-top: 3px solid #4c4c4c;"),
                                               ),
                                               shinydashboard::box(status="success",width = 12, solidHeader = TRUE,
                                                                   column(width=12, style = "height:450px; overflow-y: scroll;overflow-x: scroll;",
                                                                          p(span("Network plot of current analyses available.", style="color:black")),
                                                                          shiny::plotOutput(ns("plotTimeStamps")),
                                                                          p(span("Past modeling parameters from STA stamp selected.", style="color:black")),
                                                                          DT::DTOutput(ns("statusOft")), # modeling table
                                                                          p(span("STA predictions table to be used as input.", style="color:black")),
                                                                          DT::DTOutput(ns("phenoOft")), # predictions data table
                                                                   )
                                               )
                                      ),
                                      tabPanel("Select traits", icon = icon("table"),
                                               br(),
                                               column(width=12, selectInput(ns("trait2Oft"), "Trait(s) to include", choices = NULL, multiple = TRUE), style = "background-color:grey; color: #FFFFFF"),
                                               column(width=12,
                                                      hr(style = "border-top: 3px solid #4c4c4c;"),
                                                      h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameter values to be specified in the grey boxes above.", style="color:green"))),
                                                      hr(style = "border-top: 3px solid #4c4c4c;"),
                                               ),
                                               shinydashboard::box(status="success",width = 12,solidHeader = TRUE,
                                                                   column(width=12, style = "height:450px; overflow-y: scroll;overflow-x: scroll;",
                                                                          p(span("Metrics associated to the STA stamp selected.", style="color:black")),
                                                                          column(width=6, selectInput(ns("traitMetrics"), "Trait to visualize", choices = NULL, multiple = TRUE) ),
                                                                          column(width=6, selectInput(ns("parameterMetrics"), "Parameter to visualize", choices = NULL, multiple = FALSE) ),
                                                                          column(width=12, plotly::plotlyOutput(ns("barplotPredictionsMetrics")) ),
                                                                          p(span("Dispersal of predictions associated to the STA stamp selected.", style="color:black")),
                                                                          column(width=6, selectInput(ns("trait3Oft"), "Trait to visualize", choices = NULL, multiple = FALSE) ),
                                                                          column(width=6, selectInput(ns("groupOftInputPlot"), "Group by", choices = c("environment","designation","entryType"), multiple = FALSE, selected = "environment") ),
                                                                          column(width=12, shiny::plotOutput(ns("plotPredictionsCleanOut"))  ), # plotly::plotlyOutput(ns("plotPredictionsCleanOut"))
                                                                   ),
                                               )
                                      ),

                                      tabPanel("Select year of origin & entry type", icon = icon("table"),
                                               br(),
                                               column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                      column(width=6, selectInput(ns("yearsToUse"), "Year of origin", choices = NULL, multiple = TRUE) ),
                                                      column(width=6, selectInput(ns("entryTypeToUse"), "Entry type", choices = NULL, multiple = TRUE) ),
                                               ),
                                               column(width=12,
                                                      hr(style = "border-top: 3px solid #4c4c4c;"),
                                                      h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameter values to be specified in the grey boxes above.", style="color:green"))),
                                                      hr(style = "border-top: 3px solid #4c4c4c;"),
                                               ),
                                               shinydashboard::box(status="success",width = 12,solidHeader = TRUE,
                                                                   column(width=12, style = "height:450px; overflow-y: scroll;overflow-x: scroll;",
                                                                          p(span("Preview of Pedigree data associated to the STA stamp selected.", style="color:black")),
                                                                          column(width=12, DT::DTOutput(ns('preview_ped'))  ),
                                                                          p(span("Preview of Phenotype data associated to the STA stamp selected.", style="color:black")),
                                                                          column(width=12, DT::DTOutput(ns('preview_pheno')) )
                                                                   ),
                                               )
                                      ),
                                      tabPanel("Select Environments", icon = icon("table"),
                                               br(),
                                               column(width=12, selectInput(ns("env2Oft"), "Environment(s) to include", choices = NULL, multiple = TRUE), style = "background-color:grey; color: #FFFFFF"),
                                               column(width=12,
                                                      hr(style = "border-top: 3px solid #4c4c4c;"),
                                                      h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameter values to be specified in the grey boxes above.", style="color:green"))),
                                                      hr(style = "border-top: 3px solid #4c4c4c;"),
                                               ),
                                               shinydashboard::box(status="success",width = 12,solidHeader = TRUE,
                                                                   column(width=12, style = "height:410px; overflow-y: scroll;overflow-x: scroll;",
                                                                          column(width=12, p(span("Connectivity between environments.", style="color:black")) ),
                                                                          column(width=3, selectInput(ns("traitConnect"), "Trait to visualize", choices = NULL, multiple = FALSE) ),
                                                                          column(width=3, selectInput(ns("entryTypeOft"), "Entry type to visualize", choices = NULL, multiple = TRUE) ),
                                                                          column(width=2, checkboxGroupInput(ns("checkboxText"), label = "", choices = list("Add connectivity labels?" = TRUE), selected = FALSE) ),
                                                                          column(width=2, checkboxGroupInput(ns("checkboxAxis"), label = "", choices = list("Add axis labels?" = TRUE), selected = FALSE) ),
                                                                          column(width=2, numericInput(ns("heatmapFontSize"), label = "Font size", value = 6) ),
                                                                          column(width=12, shiny::plotOutput(ns("plotPredictionsConnectivity")) )
                                                                   ),
                                               )
                                      ),
                                      tabPanel("Generate report", icon = icon("play"),
                                               br(),
                                               actionButton(ns("runOft"), "Generate OFT Report", icon = icon("play-circle")),
                                               uiOutput(ns("outOft"))
                                      ),
                                    )
                           ),
                           tabPanel(div(icon("arrow-right-from-bracket"), "Output" ) , value = "outputTabs",
                                    tabsetPanel(
                                      tabPanel("Report", icon = icon("file-image"),
                                               br(),
                                               div(tags$p("Please download the report below:") ),
                                               downloadButton(ns("downloadReportOft"), "Download report"),
                                               br(),
                                               uiOutput(ns('reportOft'))
                                      )
                                    ) # end of tabset
                           )# end of output panel
              )) # end mainpanel

  )
}

#' oftStaApp Server Functions
#'
#' @noRd
mod_oftStaApp_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$plotDataDependencies <- shiny::renderPlot({ dependencyPlot() })
    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })
    ############################################################################

    # warning message OFT
    output$warningMessage <- renderUI(
      if(is.null(data())){
        HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your phenotypic data using the 'Data Retrieval' tab.")) )
      }else{ # data is there
        mappedColumns <- length(which(c("environment","designation","trait","entryType") %in% data()$metadata$pheno$parameter))
        if(mappedColumns == 4){
          if("sta" %in% data()$status$module){
            mappedColName <- data()$metadata$pedigree[data()$metadata$pedigree$parameter=="yearOfOrigin","value"]
            mappedColumns <- length(setdiff(unique(eval(parse(text=paste0("data()$data$pedigree$",mappedColName)))),NA))
            if(mappedColumns > 0){
              HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to generation of OFT Report.")) )
            } else{
              HTML( as.character(div(style="color: red; font-size: 20px;", "To generate OFT Report, please make sure that the column: 'yearOfOrigin' has been mapped in the pedigree data using the 'Data Retrieval' tab.")) )
            }
          }else{ HTML( as.character(div(style="color: red; font-size: 20px;", "Please perform the single trial analysis before generating OFT report.")) ) }
        }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please make sure that you have computed the 'environment' column, and that column 'designation', 'entryType' and \n at least one trait have been mapped using the 'Data Retrieval' tab.")) )}
      }
    )

    ## version
    observeEvent(c(data()), {
      req(data())
      dtOft <- data()
      dtOft <- dtOft$status
      dtOft <- dtOft[which(dtOft$module == "sta"),]
      traitsOft <- unique(dtOft$analysisId)
      if(length(traitsOft) > 0){names(traitsOft) <- as.POSIXct(traitsOft, origin="1970-01-01", tz="GMT")}
      updateSelectInput(session, "version2Oft", choices = traitsOft)
    })
    ## traits
    observeEvent(c(data(),input$version2Oft), {
      req(data())
      req(input$version2Oft)
      dtOft <- data()
      dtOft <- dtOft$predictions
      dtOft <- dtOft[which(dtOft$analysisId == input$version2Oft),]
      traitsOft <- unique(dtOft$trait)
      updateSelectInput(session, "trait2Oft", choices = traitsOft)
      updateSelectInput(session, "traitConnect", choices = traitsOft)
    })
    ## years
    observeEvent(c(data(),input$version2Oft,input$trait2Oft), {
      req(data())
      req(input$version2Oft)
      req(input$trait2Oft)
      dtOft <- data()
      traitsOft <- dtOft$metadata$pedigree[dtOft$metadata$pedigree$parameter=="yearOfOrigin","value"]
      if(!is.null(traitsOft)){
        if(length(setdiff(unique(eval(parse(text=paste0("dtOft$data$pedigree$",traitsOft)))),NA))>0){
          updateSelectInput(session, "yearsToUse", choices = traitsOft, selected = traitsOft )
        }
      }
    })
    ## entry type
    observeEvent(c(data(),input$version2Oft,input$trait2Oft), {
      req(data())
      req(input$version2Oft)
      req(input$trait2Oft)
      dtOft <- data()
      traitsOft <- dtOft$metadata$pheno[dtOft$metadata$pheno$parameter=="entryType","value"]
      updateSelectInput(session, "entryTypeToUse", choices = traitsOft, selected = traitsOft )
    })
    ## environment
    observeEvent(c(data(),input$version2Oft,input$trait2Oft,input$yearsToUse,input$entryTypeToUse), {
      req(data())
      req(input$version2Oft)
      req(input$trait2Oft)
      dtOft <- data()
      dtOft <- dtOft$predictions
      dtOft <- dtOft[which(dtOft$analysisId == input$version2Oft),]
      traitsOft <- unique(dtOft$environment)
      updateSelectInput(session, "env2Oft", choices = traitsOft)
    })


    ## -------- Select STA Version to use --------- ##
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
        ggplot2::ggplot(n, ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
          ggnetwork::geom_edges(ggplot2::aes(color = family), arrow = ggplot2::arrow(length = ggnetwork::unit(6, "pt"), type = "closed") ) +
          ggnetwork::geom_nodes(ggplot2::aes(color = family), alpha = 0.5, size=5 ) +
          ggnetwork::geom_nodelabel_repel(ggplot2::aes(color = family, label = vertex.names ),
                                          fontface = "bold", box.padding = ggnetwork::unit(1, "lines")) +
          ggnetwork::theme_blank()
      }
    })
    ## render the input data to be analyzed
    output$statusOft <-  DT::renderDT({
      req(data())
      req(input$version2Oft)
      ### change column names for mapping
      paramsPheno <- data()$modeling
      paramsPheno <- paramsPheno[which(paramsPheno$analysisId %in% input$version2Oft),, drop=FALSE]
      paramsPheno$analysisId <- as.POSIXct(paramsPheno$analysisId, origin="1970-01-01", tz="GMT")
      DT::datatable(paramsPheno, extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
      )
    })
    output$phenoOft <-  DT::renderDT({
      req(data())
      req(input$version2Oft)
      dtOft <- data()
      dtOft <- dtOft$predictions
      dtOft <- dtOft[which(dtOft$analysisId == input$version2Oft),setdiff(colnames(dtOft),c("module","analysisId"))]
      numeric.output <- c("predictedValue", "stdError", "reliability")
      DT::formatRound(DT::datatable(dtOft, extensions = 'Buttons',
                                    options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                   lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
      ), numeric.output)
    })


    ## ----------- Select Trait(s) to use ------------ ##
    observeEvent(c(data(),input$version2Oft), { # update trait
      req(data())
      req(input$version2Oft)
      dtOft <- data()
      dtOft <- dtOft$metrics
      dtOft <- dtOft[which(dtOft$analysisId %in% input$version2Oft),] # only traits that have been QA
      traitOftInput <- unique(dtOft$trait)
      updateSelectInput(session, "traitMetrics", choices = traitOftInput, selected = traitOftInput)
    })
    observeEvent(c(data(),input$version2Oft), { # update parameter
      req(data())
      req(input$version2Oft)
      dtOft <- data()
      dtOft <- dtOft$metrics
      dtOft <- dtOft[which(dtOft$analysisId %in% input$version2Oft),] # only traits that have been QA
      metricsOftInput <- unique(dtOft$parameter)
      updateSelectInput(session, "parameterMetrics", choices = metricsOftInput)
    })
    output$barplotPredictionsMetrics <- plotly::renderPlotly({
      req(data())
      req(input$version2Oft)
      dtOft <- data()
      mydata <- dtOft$metrics
      mydata <- mydata[which(mydata$analysisId %in% input$version2Oft),]
      mydata = mydata[which(mydata$parameter %in% input$parameterMetrics),]
      mydata = mydata[which(mydata$trait %in% input$traitMetrics),]
      res = plotly::plot_ly(data = mydata, x = mydata[,"environment"], y = mydata[,"value"],
                            color=mydata[,"trait"]
                            # size=mydata[,input$sizeMetrics2D], text=mydata[,"environment"]
      )   # , type="scatter", mode   = "markers")
      res = res %>% plotly::add_bars()
      res
    })
    ## render trait distribution plot
    observeEvent(c(data(),input$version2Oft), { # update trait
      req(data())
      req(input$version2Oft)
      dtOft <- data()
      dtOft <- dtOft$predictions
      dtOft <- dtOft[which(dtOft$analysisId %in% input$version2Oft),] # only traits that have been QA
      traitOftInput <- unique(dtOft$trait)
      updateSelectInput(session, "trait3Oft", choices = traitOftInput)
    })
    output$plotPredictionsCleanOut <- shiny::renderPlot({ # plotly::renderPlotly({
      req(data())
      req(input$version2Oft)
      req(input$trait3Oft)
      req(input$groupOftInputPlot)
      mydata <- data()$predictions
      mydata <- mydata[which(mydata$analysisId %in% input$version2Oft),] # only traits that have been QA
      mydata <- mydata[which(mydata[,"trait"] %in% input$trait3Oft),]
      mydata[, "environment"] <- as.factor(mydata[, "environment"]); mydata[, "designation"] <- as.factor(mydata[, "designation"])
      # res <- plotly::plot_ly(y = mydata[,"predictedValue"], type = "box", boxpoints = "all", jitter = 0.3, #color = mydata[,input$groupMtaInputPlot],
      #                        x = mydata[,input$groupMtaInputPlot], text=mydata[,"designation"], pointpos = -1.8)
      # res = res %>% plotly::layout(showlegend = FALSE); res
      ggplot2::ggplot(mydata, ggplot2::aes(x=as.factor(environment), y=predictedValue)) +
        ggplot2::geom_boxplot(fill='#A4A4A4', color="black", notch = TRUE)+
        ggplot2::theme_classic()+
        ggplot2::geom_jitter(alpha = 0.4, colour="cadetblue") + # ggplot2::aes(colour = color),
        ggplot2::xlab("Environment") + ggplot2::ylab("Predicted value") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45))
    })

    ## -------- Select yearOfOrigin and entryType --------- ##
    output$preview_pheno <- DT::renderDT({
      req(data())
      dtOft <- data()
      DT::datatable(dtOft$data$pheno,
                    extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',
                                   scrollX = TRUE,
                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All')))
      )
    })
    output$preview_ped <- DT::renderDT({
      req(data())
      dtOft <- data()
      DT::datatable(dtOft$data$pedigree,
                    extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',
                                   scrollX = TRUE,
                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All')))
      )
    })

    ## -------- Select environment(s) to include --------- ##
    ## render connectivity plot
    observeEvent(c(data(),input$version2Oft), { # update entry types included in the plot
      req(data())
      req(input$version2Oft)
      dtOft <- data()
      dtOft <- dtOft$predictions
      dtOft <- dtOft[which(dtOft$analysisId %in% input$version2Oft),] # only traits that have been QA
      entryTypeOftInput <- unique(dtOft$entryType)
      updateSelectInput(session, "entryTypeOft", choices = c(entryTypeOftInput,"Generic"), selected = "Generic")
    })
    output$plotPredictionsConnectivity <-  shiny::renderPlot({ # plotly::renderPlotly({
      req(data())
      req(input$version2Oft)
      req(input$traitConnect)
      req(input$entryTypeOft)
      req(input$heatmapFontSize)
      dtOft <- data()
      mydata <- dtOft$predictions # extract predictions
      mydata <- mydata[which(mydata$analysisId %in% input$version2Oft),] # only PREDICTIONS FROM THE STA
      mydata <- mydata[which(mydata$trait %in% input$traitConnect),] # only PREDICTIONS FROM Trait selected
      if(input$entryTypeOft != "Generic"){ # use an specific type of entries
        mydata <- mydata[which(mydata[,"entryType"] %in% input$entryTypeOft),]
      }
      splitAggregate <- with(mydata,  split(mydata[,"designation"],mydata[,"environment"]) ) # split by environment
      splitAggregate <- lapply(splitAggregate,unique); nag <- length(splitAggregate) # get unique individual names
      nagm <- matrix(0,nag,nag); rownames(nagm) <- colnames(nagm) <- names(splitAggregate) # prefilled matrix
      for(i in 1:length(splitAggregate)){ # fill the matrix of intersection of individuals between pair of environments
        for(j in 1:i){
          nagm[i,j] <- length(intersect(splitAggregate[[i]],splitAggregate[[j]]))
        }
      }
      nagm[upper.tri(nagm)] <- t(nagm)[upper.tri(nagm)] # fill the upper triangular
      mydata4 <- cgiarBase::matToTab(nagm) # matrix to a dataframe for plot
      maxVal <- max(nagm, na.rm = TRUE) # get the maximum value found in the matrix of connectivity
      p <- ggplot2::ggplot(data = mydata4, ggplot2::aes(Var2, Var1, fill = Freq))+
        ggplot2::geom_tile(color = "white")+
        ggplot2::scale_fill_gradient2(low = "firebrick", high = "#038542", mid = "gold",
                                      midpoint = 60, limit = c(0,maxVal), space = "Lab",
                                      name="Connectivity") +
        ggplot2::theme_minimal()+
        ggplot2::ylab("") + ggplot2::xlab("") +
        ggplot2::coord_fixed()
      if(!is.null(input$checkboxText)){ # if user wants to add text to the cells
        p <- p + ggplot2::geom_text(ggplot2::aes(label = Freq), color = "white", size = max(c(input$heatmapFontSize-3,1)))
      }
      if(!is.null(input$checkboxAxis)){ # if user wants to add labels to the axis
        p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, size = input$heatmapFontSize, hjust = 1, face = "bold"))+
          ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 0, vjust = 1, size = input$heatmapFontSize, hjust = 1, face = "bold"))
      }else{
        p <- p + ggplot2::theme( axis.text.x=ggplot2::element_blank(), axis.text.y=ggplot2::element_blank() )
      }
      p
    })

    ## render result of "run" button click
    outOft <- eventReactive(input$runOft, {
      req(data())
      req(input$version2Oft)
      req(input$trait2Oft)
      req(input$yearsToUse)
      req(input$entryTypeToUse)
      req(input$env2Oft)
      result <- data()

      if(sum(result$status$module %in% "sta") != 0) {
        ## report OFT
        shinybusy::show_modal_spinner(spin = "fading-circle",
                                      text = "Generating Report...")
        out <- rmarkdown::render(input = system.file("rmd","reportOft.Rmd",package="bioflow"),
                                 output_format = rmarkdown::html_fragment(),
                                 params = list(traits = input$trait2Oft, fieldinst=input$env2Oft),
                                 quiet = TRUE)
        output$reportOft <- renderUI({
          shiny::withMathJax(HTML(readLines(out)))
          # HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportOft.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
        })
        cat(paste("Report Generated Successfully."))
        shinybusy::remove_modal_spinner()
        updateTabsetPanel(session, "tabsMain", selected = "outputTabs")

        ## report OFT
        output$downloadReportOft <- downloadHandler(
          filename = function() {
            paste('my-report-OFT', sep = '.', switch(
              "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
            ))
          },
          content = function(file) {
            shinybusy::show_modal_spinner(spin = "fading-circle",
                                          text = "Downloading Report...")
            src <- normalizePath(system.file("rmd","reportOft.Rmd",package="bioflow"))
            # src2 <- normalizePath('data/resultSta.RData')

            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'report2.Rmd', overwrite = TRUE)
            # file.copy(src2, 'resultSta.RData', overwrite = TRUE)
            out2 <- rmarkdown::render('report2.Rmd',
                                     params = list(traits = input$trait2Oft, fieldinst=input$env2Oft, toDownload=TRUE),
                                     switch("HTML",HTML = rmarkdown::html_document()))
            file.rename(out2, file)
            shinybusy::remove_modal_spinner()
          }
        )

      } else {
        output$reportOft <- renderUI({NULL})
      }
      hideAll$clearAll <- FALSE
    }) ## end eventReactive

    output$outOft <- renderPrint({
      outOft()
    })

  })
}

## To be copied in the UI
# mod_oftStaApp_ui("oftStaApp_1")

## To be copied in the server
# mod_oftStaApp_server("oftStaApp_1")

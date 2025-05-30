#' hybridityApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_hybridityApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    # HTML( as.character(div(style="color: red; font-size: 30px;", "Module under construction.")) ),
    # img(src = "www/bongo.gif", height = 400, width = 200), # add an image

    shiny::mainPanel(width = 12,
                     tabsetPanel( id=ns("tabsMain"),
                                  type = "tabs",
                                  tabPanel(div(icon("book"), "Information") ,
                                           br(),
                                           # shinydashboard::box(status="success",width = 12, solidHeader = TRUE,
                                           # column(width=12,   style = "height:580px; overflow-y: scroll;overflow-x: scroll;",
                                           tags$body(
                                             column(width = 6,
                                                    h1(strong(span("Marker-Assisted Hybridity Verification Module", tags$a(href="https://www.youtube.com/watch?v=6Ooq9I3LEp8&list=PLZ0lafzH_UmclOPifjCntlMzysEB2_2wX&index=5", icon("youtube") , target="_blank"), style="color:darkcyan"))),
                                                    h2(strong("Data Status (wait to be displayed):")),
                                                    uiOutput(ns("warningMessage")),
                                                    tags$br(),
                                                    # column(width=4, tags$br(), tags$br(),
                                                    shinyWidgets::prettySwitch( inputId = ns('launch'), label = "Load example dataset", status = "success"),
                                                    # ),
                                                    tags$br(),
                                                    # img(src = "www/qaGeno.png", height = 100, width = 435), # add an image
                                             ),

                                             column(width = 6,
                                                    h2(strong("Details")),
                                                    p("The availability of genetic markers allow a more accurate quality assurance of crosses of different kind (e.g., F1, BCn). This module
                                                      allows to use the genetic data to perform this QA/QC:"),
                                                    p(strong("Markers to be used.-")," selection of genetic markers to be used for the verification process."),
                                                    p(strong("Columns to define the expected genotypes.-"),"  columns that define the expected genotype."),
                                                    p(strong("Ploidy.-")," number of chromosome copies. This value is important to compute some of the paramters. Default is 2 or diploid."),
                                                    h2(strong("References")),
                                                    p("Tukey, J. W. (1977). Exploratory Data Analysis. Section 2C."),
                                                    p("Velleman, P. F. and Hoaglin, D. C. (1981). Applications, Basics and Computing of Exploratory Data Analysis. Duxbury Press."),
                                                    # column(width = 12, shiny::plotOutput(ns("plotDataDependencies")), ),
                                             ),
                                           )
                                  ),
                                  tabPanel(div(icon("arrow-right-to-bracket"), "Input steps"),
                                           tabsetPanel(
                                             tabPanel(div( icon("dice-one"), "Pick QA-stamp(s)", icon("arrow-right") ), # icon = icon("dice-one"),
                                                      br(),
                                                      column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                             column(width=8, selectInput(ns("version2Mta"), "QA-geno stamp(s) to apply", choices = NULL, multiple = TRUE)),

                                                      ),
                                                      column(width=12),
                                                      shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                          column(width=12,
                                                                                 hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                                 h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameters to be specified in the grey boxes above.", style="color:green"))),
                                                                                 hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                          ),
                                                                          column( width=4, DT::DTOutput(ns("tableTraitTimeVerifmps")), br(),br(),  ),
                                                                          column( width=8, shiny::plotOutput(ns("plotTimeStamps")), br(),br(), ),
                                                      ),
                                             ),
                                             tabPanel(div(icon("dice-two"), "Select marker(s)", icon("arrow-right") ), # icon = icon("dice-two"),
                                                      br(),
                                                      column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                             column(width=6, selectizeInput(ns("markers2Verif"), "Marker(s) to use", choices = NULL, multiple = TRUE) ),
                                                             column(width=6, checkboxInput(ns("checkbox"), label = "Select all markers?", value = FALSE), ),
                                                      ),
                                                      column(width=12),
                                                      shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                          column(width=12,
                                                                                 hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                                 h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameter values to be specified in the grey boxes above.", style="color:green"))),
                                                                                 hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                          ),
                                                                          column(width=12,
                                                                                 column(width=12, sliderInput(ns("slider1"), label = "Markers to display", min = 1, max = 2000, value = c(1, 10)) ),
                                                                                 # column(width=4, numericInput(ns("nCol"), label = "Max number of columns to display", value = 10, step = 5, max = 1000, min = 1) ),
                                                                          ),
                                                                          column(width=12,
                                                                                 tags$span(id = ns('holder'),
                                                                                           DT::DTOutput(ns("genoDT")),
                                                                                 ),
                                                                          ),
                                                      ),
                                             ),
                                             tabPanel( div( icon("dice-three"), "Selection unit(s)", icon("arrow-right") ), # icon = icon("dice-three"),
                                                      br(),
                                                      column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                             column(width=8, selectizeInput(ns("units2Verif"), "Genetic unit(s) to average for expected genotype", choices = NULL, multiple = TRUE) ),
                                                             column(width=4, numericInput(ns("ploidy"), label = "Ploidy of the organism", value = 2, step = 2, max = 10, min = 0) ),
                                                      ),
                                                      column(width=12),
                                                      shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                          column(width=12,
                                                                                 hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                                 h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameter values to be specified in the grey boxes above.", style="color:green"))),
                                                                                 hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                          ),
                                                                          # column(width=12,
                                                                          #        column(width=4, numericInput(ns("nCol2"), label = "Max number of columns to display", value = 10, step = 5, max = 1000, min = 1) ),
                                                                          # ),
                                                                          column(width=12,
                                                                                 tags$span(id = ns('holder'),
                                                                                           DT::DTOutput(ns("pedigreeDT")),
                                                                                 ),
                                                                          ),
                                                      ),
                                             ),
                                             tabPanel("Run analysis", icon = icon("dice-four"),
                                                      br(),
                                                      column(width=12,style = "background-color:grey; color: #FFFFFF",
                                                             column(width=3, tags$div(textInput(ns("analysisIdName"), label = tags$span(
                                                               "Analysis Name (optional)", tags$i( class = "glyphicon glyphicon-info-sign", style = "color:#FFFFFF",
                                                                                                   title = "An optional name for the analysis besides the timestamp if desired.") ), #width = "100%",
                                                               placeholder = "(optional name)") ) ),
                                                             column(width=3,
                                                                    br(),
                                                                    actionButton(ns("runQaMb"), "Compute verification", icon = icon("play-circle")),
                                                                    uiOutput(ns("qaQcStaInfo")),
                                                                    br(),
                                                             ),

                                                      ),
                                                      textOutput(ns("outQaMb")),
                                             ),
                                           ) # end of tabset
                                  ),# end of output panel
                                  tabPanel(div(icon("arrow-right-from-bracket"), "Output tabs" ) , value = "outputTabs",
                                           tabsetPanel(
                                             tabPanel("Dashboard", icon = icon("file-image"),
                                                      br(),
                                                      downloadButton(ns("downloadReportVerifGeno"), "Download dashboard"),
                                                      br(),
                                                      uiOutput(ns('reportVerifGeno'))
                                             ),
                                             tabPanel("Verification", icon = icon("table"),
                                                      br(),
                                                      column(width = 6, column(width=8, selectizeInput(ns("genoView"), "Progeny", choices = NULL, multiple = FALSE) ), ),
                                                      column(width=6, sliderInput(ns("slider2"), label = "Markers to display", min = 1, max = 2000, value = c(1, 10)) ),
                                                      column(width=12, DT::DTOutput(ns("verificationMap")), ),
                                                      column(width=12),
                                                      column(width=12),
                                                      column(width=4, selectizeInput(ns("matrixType"), "Matrix to plot",
                                                                                     choices = list("ProbabilityMatch"="matchMat","Progeny"="Mprogeny",
                                                                                                    "Female"="Mfemale","Male"="Mmale", "Expected"="Mexpected"
                                                                                     ),
                                                                                     multiple = FALSE) ),
                                                      column(width=4, sliderInput(ns("slider1b"), label = "Number of genotypes", min = 1, max = 2000, value = c(1, 100)) ),
                                                      column(width=4, sliderInput(ns("slider2b"), label = "Number of markers", min = 1, max = 500, value = c(1, 25)) ),
                                                      column(width=12, shiny::plotOutput(ns("matrixOfVerifications")) ),

                                             ),
                                             tabPanel("Predictions", icon = icon("table"),
                                                      br(),
                                                      DT::DTOutput(ns("predictionsVerif")),
                                             ),
                                             tabPanel("Metrics", icon = icon("table"),
                                                      br(),
                                                      DT::DTOutput(ns("metricsVerif")),
                                             ),
                                             tabPanel("Modeling", icon = icon("table"),
                                                      br(),
                                                      DT::DTOutput(ns("modelingVerif")),
                                             ),
                                           ),
                                  ),
                     )) # end mainpanel



  )
}

#' hybridityApp Server Functions
#'
#' @noRd
mod_hybridityApp_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    # output$plotDataDependencies <- shiny::renderPlot({ dependencyPlot() })
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
        tmp$data$pedigree$Hybrid_Parent1 <- sample(result$data$pedigree$Hybrid)
        tmp$data$pedigree$Hybrid_Parent2 <- sample(result$data$pedigree$Hybrid)
        data(tmp) # update data with results
        shinybusy::remove_modal_spinner()
      }else{
        shinyWidgets::updatePrettySwitch(session, "launch", value = FALSE)
      }
    }, ignoreNULL = TRUE)

    #################

    ###############################
    ###############################
    ###############################
    ## select id tab
    observeEvent(c(data()), {
      req(data())
      dtMta <- data()
      dtMta <- dtMta$status
      dtMta <- dtMta[which(dtMta$module == "qaGeno"),]
      traitsMta <- unique(dtMta$analysisId)
      if(length(traitsMta) > 0){
        if("analysisIdName" %in% colnames(dtMta)){
          names(traitsMta) <- paste(dtMta$analysisIdName, as.POSIXct(traitsMta, origin="1970-01-01", tz="GMT"), sep = "_")
        }else{
          names(traitsMta) <- as.POSIXct(traitsMta, origin="1970-01-01", tz="GMT")
        }
      }
      updateSelectInput(session, "version2Mta", choices = traitsMta)
    })
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
    output$tableTraitTimeVerifmps <-  DT::renderDT({
      req(data())
      # req(input$version2Mta)
      ### change column names for mapping
      if(length(input$version2Mta) > 0){
        status <- data()$status
        modeling <- data()$modeling
        statusPlusModel <- merge(status, unique(modeling[,c("analysisId","trait")]), by="analysisId", all.x = TRUE)
        '%!in%' <- function(x,y)!('%in%'(x,y))
        statusPlusModel <- statusPlusModel[which(statusPlusModel$trait %!in% c("inputObject",NA)),]
        statusPlusModel <- statusPlusModel[which(statusPlusModel$analysisId %in% input$version2Mta),]
        statusPlusModel$analysisId <- as.POSIXct(statusPlusModel$analysisId, origin="1970-01-01", tz="GMT")
      }else{
        statusPlusModel <- data.frame(analysisId=NA, module=NA, trait=NA)
      }
      DT::datatable(statusPlusModel, extensions = 'Buttons', # I changed Blfrtip to lfrtip and silenced the buttons
                    options = list(dom = 'lfrtip',scrollX = TRUE, #buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   lengthMenu = list(c(8,20,50,-1), c(8,20,50,'All'))),
                    caption = htmltools::tags$caption(
                      style = 'color:cadetblue', #caption-side: bottom; text-align: center;
                      htmltools::em('Traits available in the STA-IDs selected.')
                    )
      )
    }, server = FALSE)

    ###############################
    ###############################
    ###############################
    # select markers tab
    observeEvent(c(data(),input$version2Mta,input$checkbox), {
      req(data())
      req(input$version2Mta)
      dtVerif <- data()
      dtVerif <- dtVerif$data$geno
      if(!is.null(dtVerif)){
        traitsVerif <- colnames(dtVerif)
        if(input$checkbox == FALSE){
          updateSelectizeInput(session, "markers2Verif", choices = traitsVerif, selected = NULL)
        }else{
          updateSelectizeInput(session, "markers2Verif", choices = traitsVerif, selected = traitsVerif)
        }
      }
    })
    output$genoDT <-  DT::renderDT({
      req(data())
      req(input$slider1)
      dtVerif <- data()
      dtVerif <- dtVerif$data$geno
      if(!is.null(dtVerif)){
        DT::datatable(dtVerif[,min(c(input$slider1[1], ncol(dtVerif) )):min(c(input$slider1[2], ncol(dtVerif) ))], extensions = 'Buttons',
                      options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                     lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All'))),
                      caption = htmltools::tags$caption(
                        style = 'color:cadetblue', #caption-side: bottom; text-align: center;
                        htmltools::em('Raw genotypic data to be used as input.')
                      )
        )
      }
    }, server = FALSE)

    ###############################
    ###############################
    ###############################
    # select genetic unit tab
    observeEvent(c(data(),input$version2Mta), {
      req(data())
      dtVerif <- data()
      dtPed <- dtVerif$data$pedigree
      if(!is.null(dtPed)){
        metaPed <- dtVerif$metadata$pedigree
        traitsVerif <- colnames(dtPed)
        first <- metaPed$value[metaPed$parameter=="designation"]
        updateSelectizeInput(session, "units2Verif", choices = traitsVerif, selected = first)
      }
    })
    output$pedigreeDT <-  DT::renderDT({
      req(data())
      # req(input$nCol)
      dtVerif <- data()
      dtVerif <- dtVerif$data$pedigree
      if(!is.null(dtVerif)){
        DT::datatable(dtVerif, extensions = 'Buttons', # [,1:min(c(input$nCol2,ncol(dtVerif)))]
                      options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                     lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All'))),
                      caption = htmltools::tags$caption(
                        style = 'color:cadetblue', #caption-side: bottom; text-align: center;
                        htmltools::em('Raw pedigree data to be used as input.')
                      )
        )
      }
    }, server = FALSE)




    ##########################
    ## run button
    outQaMb <- eventReactive(input$runQaMb, {

      req(data())
      req(input$version2Mta)
      req(input$units2Verif)
      req(input$markers2Verif)

      shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
      ##
      ## store the new modifications table
      result <- cgiarPipeline::individualVerification(
        object= data(),
        analysisIdForGenoModifications= input$version2Mta,
        markersToBeUsed=input$markers2Verif,
        colsForExpecGeno=input$units2Verif, ploidy=input$ploidy
      )
      if(!inherits(result,"try-error")) {
        provitional <- cgiarPipeline::individualVerification(
          object= data(),
          analysisIdForGenoModifications= input$version2Mta,
          markersToBeUsed=input$markers2Verif,
          colsForExpecGeno=input$units2Verif, ploidy=input$ploidy,
          onlyMats=TRUE
        )
      }

      # save(result, file = "./R/outputs/resultVerifGeno.RData")
      shinybusy::remove_modal_spinner()

      if(!inherits(result,"try-error")) { # if all goes well in the run
        cat(paste("Genotype verification analysis saved with id:",as.POSIXct( result$status$analysisId[nrow(result$status)], origin="1970-01-01", tz="GMT") ))
        if("analysisIdName" %in% colnames(result$status)){result$status$analysisIdName[nrow(result$status)] <- input$analysisIdName}
        data(result)
        updateTabsetPanel(session, "tabsMain", selected = "outputTabs")
        # predictions
        output$predictionsVerif <-  DT::renderDT({
          # if(!inherits(result,"try-error") ){
          predictions <- result$predictions
          predictions <- predictions[predictions$module=="gVerif",]
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
        # metrics
        output$metricsVerif <-  DT::renderDT({
          # if(!inherits(result,"try-error") ){
          metrics <- result$metrics
          metrics <- metrics[metrics$module=="gVerif",]
          current.metrics <- metrics[metrics$analysisId==(metrics$analysisId[length(metrics$analysisId)]),]
          current.metrics <- subset(current.metrics, select = -c(module,analysisId))
          numeric.output <- c("value", "stdError")
          DT::formatRound(DT::datatable(current.metrics, extensions = 'Buttons',
                                        options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                       lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
          ), numeric.output)
          # }
        }, server = FALSE)
        # modeling
        output$modelingVerif <-  DT::renderDT({
          # if(!inherits(result,"try-error") ){
          modeling <- result$modeling
          modeling <- modeling[modeling$module=="gVerif",]
          modeling$analysisId <- as.numeric(modeling$analysisId)
          modeling <- modeling[!is.na(modeling$analysisId),]
          current.modeling <- modeling[modeling$analysisId==max(modeling$analysisId),]
          current.modeling <- subset(current.modeling, select = -c(module,analysisId))
          # current.modeling$analysisId <- as.POSIXct(current.modeling$analysisId, origin="1970-01-01", tz="GMT")
          DT::datatable(current.modeling, extensions = 'Buttons',
                        options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                       lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
          )
          # }
        }, server = FALSE)
        # verification graph
        output$verificationMap <-  DT::renderDT({
          req(data())
          req(input$slider2)
          req(input$genoView)
          dtVerif <- provitional
          pick <- which(rownames(dtVerif$Mprogeny) == input$genoView)
          if(length(pick) > 0){
            dtVerifx <- rbind(dtVerif$Mfemale[pick,], dtVerif$Mmale[pick,], dtVerif$Mexpected[pick,], dtVerif$Mprogeny[pick,], dtVerif$matchMat[pick,])
            dtVerifx <- as.matrix(dtVerifx)
            rownames(dtVerifx) <- c("Female","Male","Expected",input$genoView,"ProbMatch")

            refAlleles <- t(result$metadata$geno[colnames(dtVerifx),c("altAllele","refAllele")])
            rownames(refAlleles) <- c("Alt","Ref")
            dtVerifx[1:4,] <- sommer::atcg1234BackTransform(marks =  dtVerifx[1:4,], refs =  refAlleles)

            DT::datatable(dtVerifx[,min(c(input$slider2[1], ncol(dtVerifx) )):min(c(input$slider2[2], ncol(dtVerifx) ))], extensions = 'Buttons',
                          options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                         lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All'))),
                          caption = htmltools::tags$caption(
                            style = 'color:cadetblue', #caption-side: bottom; text-align: center;
                            htmltools::em('Raw genotypic data to be used as input.')
                          )
            )
          }
        }, server = FALSE)
        output$matrixOfVerifications <- shiny::renderPlot({
          req(data())
          req(input$slider2b)
          req(input$slider1b)
          req(input$matrixType)
          M2 <- as.matrix(provitional[[input$matrixType]])
          M2 <- M2[,(input$slider2b[1]):min(c(input$slider2b[2], ncol(M2) )), drop=FALSE] # environments
          M2 <- M2[(input$slider1b[1]):min(c(input$slider1b[2]), nrow(M2) ), ,drop=FALSE] # genotypes
          Matrix::image(as(M2, Class = "dgCMatrix"), ylab="Genotypes", xlab="Markers", colorkey=TRUE)
        })
        ###############################
        # update genoView in output tab
        observeEvent(c(data(),input$version2Mta), {
          req(data())
          dtVerif <- data()
          dtPed <- dtVerif$data$pedigree
          if(!is.null(dtPed)){
            metaPed <- dtVerif$metadata$pedigree
            colnames(dtPed) <- cgiarBase::replaceValues(colnames(dtPed), Search = metaPed$value, Replace = metaPed$parameter)
            mychoices <- na.omit(unique(dtPed$designation))
            updateSelectizeInput(session, "genoView", choices = mychoices, selected = mychoices[1])
          }
        })
        # ## Report tab
        output$reportVerifGeno <- renderUI({
          HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportVerifGeno.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
        })

        output$downloadReportVerifGeno <- downloadHandler(
          filename = function() {
            paste(paste0('gVerif_dashboard_',gsub("-", "", as.integer(Sys.time()))), sep = '.', switch(
              "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
            ))
          },
          content = function(file) {
            shinybusy::show_modal_spinner(spin = "fading-circle", text = "Generating Report...")

            src <- normalizePath(system.file("rmd","reportVerifGeno.Rmd",package="bioflow"))
            src2 <- normalizePath('data/resultVerifGeno.RData')

            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))

            file.copy(src, 'report.Rmd', overwrite = TRUE)
            file.copy(src2, 'resultVerifGeno.RData', overwrite = TRUE)

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

      }else{ hideAll$clearAll <- TRUE}

      hideAll$clearAll <- FALSE

    })
    output$outQaMb <- renderPrint({
      outQaMb()
    })



  })
}

## To be copied in the UI
# mod_hybridityApp_ui("hybridityApp_1")

## To be copied in the server
# mod_hybridityApp_server("hybridityApp_1")

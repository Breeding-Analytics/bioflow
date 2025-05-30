#' masApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_masApp_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::mainPanel(width = 12,
                     tabsetPanel( id=ns("tabsMain"),
                                  type = "tabs",
                                  tabPanel(div(icon("book"), "Information") ,
                                           br(),
                                           tags$body(
                                             column(width = 6,
                                                    h1(strong(span("Marker-Assisted Selection Module Using Selection Index Theory", tags$a(href="https://www.youtube.com/watch?v=6Ooq9I3LEp8&list=PLZ0lafzH_UmclOPifjCntlMzysEB2_2wX&index=5", icon("youtube") , target="_blank"), style="color:darkcyan"))),
                                                    h2(strong("Data Status (wait to be displayed):")),
                                                    uiOutput(ns("warningMessage")),
                                                    tags$br(),
                                                    shinyWidgets::prettySwitch( inputId = ns('launch'), label = "Load example dataset", status = "success"),
                                                    tags$br(),
                                                    img(src = "www/bottleneck.jpg", height = 380, width = 400), # add an image
                                             ),

                                             column(width = 6,
                                                    h2(strong("Details")),
                                                    p("The availability of genetic markers linked closely to QTLs allow to select directly for the fixation of the QTL without
                                                      further phenotyping. When such markers exist you can use this module to select individuals that help to increase the frequency of the
                                                      positive QTL-allele. Currently, the method consists in weighting each marker by 1-freq, where freq is the frequency of the positive
                                                      QTL-allele. In addition, the user is allowed to multliply that first weight by a factor.The following
                                                      parameters are enabled:"),
                                                    p(strong("Markers to be used.-")," selection of genetic markers to be used for the verification process."),
                                                    p(strong("Desired dosages.-"),"  this is the way to set the direction on what allele should be selected (positive allele for the trait of interest). The allele shown in the slider is the reference allele picked in the transformation for the dosage matrix."),
                                                    p(strong("Relative weights for each marker.-"),"  relative values to apply to each marker denoting the multiplier applied to the 1-freq approach."),
                                                    h2(strong("References")),
                                                    p("Liu, B. H. (2017). Statistical genomics: linkage, mapping, and QTL analysis. CRC press."),
                                                    p("Velleman, P. F. and Hoaglin, D. C. (1981). Applications, Basics and Computing of Exploratory Data Analysis. Duxbury Press."),
                                                    # column(width = 12, shiny::plotOutput(ns("plotDataDependencies")), ),
                                             ),
                                           )
                                  ),
                                  tabPanel(div(icon("arrow-right-to-bracket"), "Input steps"),
                                           tabsetPanel(
                                             tabPanel( div( icon("dice-one"), "Pick QA-stamp(s)", icon("arrow-right") ) , # icon = icon("dice-one"),
                                                       br(),
                                                       column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                              column(width=8,
                                                                     selectInput(ns("version2Mta"),
                                                                                 label = tags$span(
                                                                                   "QA-geno stamp(s) to apply",
                                                                                   tags$i(
                                                                                     class = "glyphicon glyphicon-info-sign",
                                                                                     style = "color:#FFFFFF",
                                                                                     title = "Analysis ID(s) from genotype QA runs that should be applied to the marker information prior to MAS calculation."
                                                                                   )
                                                                                 ),
                                                                                 choices = NULL, multiple = TRUE)
                                                              ),

                                                       ),
                                                       column(width=12),
                                                       shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                           column(width=12,
                                                                                  hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                                  h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameters to be specified in the grey boxes above.", style="color:green"))),
                                                                                  hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                           ),
                                                                           column( width=4, DT::DTOutput(ns("tableTraitTimeMASmps")), br(),br(),  ),
                                                                           column( width=8, shiny::plotOutput(ns("plotTimeStamps")), br(),br(), ),
                                                       ),
                                             ),
                                             tabPanel( div(icon("dice-two"), "Select marker(s)", icon("arrow-right") ), # icon = icon("dice-two"),
                                                       br(),
                                                       column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                              column(width=6,
                                                                     selectizeInput(ns("markers2MAS"),
                                                                                    label = tags$span(
                                                                                      "Marker(s) to use",
                                                                                      tags$i(
                                                                                        class = "glyphicon glyphicon-info-sign",
                                                                                        style = "color:#FFFFFF",
                                                                                        title = "Name of QTL-markers that will be used to assign merit to individuals. The method will apply weight to each marker equal to 1 - freq, where freq is the frequency of the desired QTL in the population of individuals genotyped."
                                                                                      )
                                                                                      # ,tags$span('(*max of 40)',style="color:#FFFFFF")
                                                                                    ),
                                                                                    choices = NULL, multiple = TRUE)), #options = list(maxItems = 40)) ),
                                                              column(width=6, checkboxInput(ns("checkbox"), label = "Select all markers at once?", value = FALSE), ),
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
                                             tabPanel(div(icon("dice-three"), "Set direction", icon("arrow-right") ), # icon = icon("dice-three"),
                                                      br(),
                                                      column(width=4, style = "background-color:grey; color: #FFFFFF",
                                                             column(width=6,
                                                                    uiOutput(ns("SlideDesiredAllele")),
                                                             ),
                                                             column(width=6,
                                                                    uiOutput(ns("SlideDesiredWeight")),
                                                             ),
                                                             column(width=12, numericInput(ns("ploidy"), label = "Ploidy of the organism", value = 2, step = 2, max = 10, min = 0) ),
                                                      ),
                                                      # column(width=12),
                                                      shinydashboard::box(width = 8, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                          column(width=12,
                                                                                 hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                                 h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameter values to be specified in the grey boxes above.", style="color:green"))),
                                                                                 hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                          ),
                                                                          column(width=12,
                                                                                 column(width=12, plotly::plotlyOutput(ns("barplotFrequency")) ),
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
                                                                    actionButton(ns("runMAS"), "Run analysis", icon = icon("play-circle")),
                                                                    uiOutput(ns("qaQcStaInfo")),
                                                                    br(),
                                                             ),
                                                      ),
                                                      textOutput(ns("outMAS")),

                                             ),
                                           ) # end of tabset
                                  ),# end of output panel
                                  tabPanel(div(icon("arrow-right-from-bracket"), "Output tabs" ) , value = "outputTabs",
                                           tabsetPanel(
                                             tabPanel("Dashboard", icon = icon("file-image"),
                                                      br(),
                                                      downloadButton(ns("downloadReportMASGeno"), "Download dashboard"),
                                                      br(),
                                                      uiOutput(ns('reportMASGeno'))
                                             ),
                                             tabPanel("Predictions", icon = icon("table"),
                                                      br(),
                                                      DT::DTOutput(ns("predictionsMAS")),
                                             ),
                                             tabPanel("Metrics", icon = icon("table"),
                                                      br(),
                                                      DT::DTOutput(ns("metricsMAS")),
                                             ),
                                             tabPanel("Modeling", icon = icon("table"),
                                                      br(),
                                                      DT::DTOutput(ns("modelingMAS")),
                                             ),
                                           ),
                                  ),
                     )) # end mainpanel

  )
}

#' masApp Server Functions
#'
#' @noRd
mod_masApp_server <- function(id, data){
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
          if("qaGeno" %in% data()$status$module){
            HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to perform marker assisted selection under the Input tabs.")) )
          }else{
            HTML( as.character(div(style="color: red; font-size: 20px;", "Please perform QA/QC in your genotype data. ")) )
          }
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
    output$tableTraitTimeMASmps <-  DT::renderDT({
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
    observeEvent(c(data(),input$version2Mta), {
      req(data())
      req(input$version2Mta)
      dtMAS <- data()
      dtMAS <- dtMAS$data$geno
      if(!is.null(dtMAS)){
        traitsMAS <- colnames(dtMAS)
        updateCheckboxInput(session, "checkbox", value = FALSE)
        updateSelectizeInput(session, "markers2MAS", choices = traitsMAS, selected = NULL)
      }
    })

    observeEvent(input$checkbox,{
      req(data())
      req(input$version2Mta)
      dtMAS <- data()
      dtMAS <- dtMAS$data$geno
      if(!is.null(dtMAS)){
        traitsMAS <- colnames(dtMAS)
        if(input$checkbox == FALSE){
          updateSelectizeInput(session, "markers2MAS", choices = traitsMAS, selected = NULL)
        }else{
          if (length(traitsMAS) > 40){
            shinyalert::shinyalert(
              "Too many markers in the data, selecting all might crash the application.
              If you want to proceed, click OK. Or you can specify the number of markers you want to select below.",
              type = "input", inputType = "number", showCancelButton = TRUE,
              html = TRUE,
              callbackR = function(x) {
                if(x != FALSE) {
                  if(x == ""){
                    updateSelectizeInput(session, "markers2MAS", choices = traitsMAS, selected = traitsMAS)
                  } else{
                    updateSelectizeInput(session, "markers2MAS", choices = traitsMAS, selected = traitsMAS[1:x])
                  }
                } else{
                  updateCheckboxInput(session, "checkbox", value = FALSE)
                }
              }
            )
            # shinyWidgets::show_alert(title = 'Too many markers in the data. Only the first 40 markers will be selected.', type = 'warning')
            # updateSelectizeInput(session, "markers2MAS", choices = traitsMAS, selected = traitsMAS[1:numMarkers])
          } else{
            updateSelectizeInput(session, "markers2MAS", choices = traitsMAS, selected = traitsMAS)
          }
        }
      }
    })
    output$genoDT <-  DT::renderDT({
      req(data())
      req(input$slider1)
      dtMAS <- data()
      dtMAS <- dtMAS$data$geno
      if(!is.null(dtMAS)){
        DT::datatable(dtMAS[,min(c(input$slider1[1], ncol(dtMAS) )):min(c(input$slider1[2], ncol(dtMAS) ))], extensions = 'Buttons',
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
    # select weights tab
    output$SlideDesiredAllele <- renderUI({
      req(data())
      req(input$version2Mta)
      req(input$markers2MAS)
      req(input$ploidy)
      # req(input$scaledIndex)
      dtMta <- data()
      markers2MAS <- input$markers2MAS # markers2MAS <- c("Yield_Mg_ha_QTL","Ear_Height_cm") # list(markers2MAS=c("Yield_Mg_ha","Ear_Height_cm"))
      Markers <- dtMta$data$geno[,input$markers2MAS]
      X <- dtMta$metadata$geno[input$markers2MAS,]

      lapply(1:length(markers2MAS), function(i) {
        sliderInput(
          inputId=session$ns(paste0('SlideDesiredAllele',i)),
          label=paste0('Desired dosage',": ",markers2MAS[i]),
          min = 0,
          max = input$ploidy,
          value = 0,
          step = input$ploidy,
          post= paste0("-",X$refAllele[i],"-copies")
        )
      })

    })
    desireAlleleValues = reactive({
      req(data())
      req(input$version2Mta)
      req(input$markers2MAS)
      req(input$ploidy)

      if (length(input$markers2MAS) != 0) {
        values <- NULL
        for (i in 1:length(input$markers2MAS)) {
          tempval <- reactive({paste0('input$','SlideDesiredAllele',i)})
          values[i] <- tempval()
          values[i] <- eval(parse(text = values[i]))
        }
        values <- t(as.numeric(values))
        values <- as.data.frame(values)
        colnames(values) <- input$markers2MAS
        values <- as.numeric(values)
        return(values)
      }
    })
    output$SlideDesiredWeight <- renderUI({
      req(data())
      req(input$version2Mta)
      req(input$markers2MAS)
      req(input$ploidy)
      dtMta <- data()
      markers2MAS <- input$markers2MAS # markers2MAS <- c("Yield_Mg_ha_QTL","Ear_Height_cm") # list(markers2MAS=c("Yield_Mg_ha","Ear_Height_cm"))
      Markers <- dtMta$data$geno[,input$markers2MAS]
      X <- dtMta$metadata$geno[input$markers2MAS,]

      lapply(1:length(markers2MAS), function(i) {
        sliderInput(
          inputId=session$ns(paste0('SlideDesiredWeight',i)),
          label=paste0('Relative weight',": ",markers2MAS[i]),
          min = 0,
          max = 5,
          value = 1,
          step = 1
        )
      })

    })
    desireWeightValues = reactive({
      req(data())
      req(input$version2Mta)
      req(input$markers2MAS)
      req(input$ploidy)
      if (length(input$markers2MAS) != 0) {
        values <- NULL
        for (i in 1:length(input$markers2MAS)) {
          tempval <- reactive({paste0('input$','SlideDesiredWeight',i)})
          values[i] <- tempval()
          values[i] <- eval(parse(text = values[i]))
        }
        values <- t(as.numeric(values))
        values <- as.data.frame(values)
        colnames(values) <- input$markers2MAS
        values <- as.numeric(values)
        return(values)
      }
    })
    output$barplotFrequency <- plotly::renderPlotly({
      req(data())
      req(input$version2Mta)
      req(input$markers2MAS)
      req(input$ploidy)
      dtMta <- data()
      # input <- list(markers2MAS=colnames(dtMta$data$geno)[1:10], ploidy=2)
      Markers <- dtMta$data$geno[,input$markers2MAS]
      X <- dtMta$metadata$geno[input$markers2MAS,]
      q <- vector(mode="numeric",length = length(input$markers2MAS))
      for(k in 1:ncol(Markers)){
        ttb <- table(0:input$ploidy)-1 # table of zeros for dosages
        tto <- table(Markers[,k])
        ttb[names(tto)] <- ttb[names(tto)] + tto
        n <- sum(ttb)
        q[k] <- ( ttb[length(ttb)] + (ttb[2:(length(ttb)-1)] / factorial(2:(length(ttb)-1)) ) ) / n
      }
      p <- 1 - q
      df3 <- data.frame(allele= c(X$refAllele, X$altAllele),
                        parameter=c(rep("reference",length(X$refAllele)), rep("alternate",length(X$altAllele)) ),
                        value=c( p, q),
                        marker=c(input$markers2MAS,input$markers2MAS)
      )
      pp <- ggplot2::ggplot(data=df3, ggplot2::aes(x=marker, y=value, fill=allele)) +
        ggplot2::geom_bar(stat="identity") +
        ggplot2::scale_fill_brewer(palette="Accent") +
        ggplot2::theme(axis.text.x = ggplot2::element_blank()) +
        ggplot2::xlab("") + ggplot2::ylab("Allele frequencies")
      plotly::ggplotly(pp)
    })


    ##########################
    ## run button
    outMAS <- eventReactive(input$runMAS, { # input <- list(version2Mta=result$status$analysisId[1], markers2MAS=colnames(result$data$geno)[1:10])

      req(data())
      req(input$version2Mta)
      req(input$markers2MAS)
      req(input$ploidy)

      shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
      ##
      ## store the new modifications table
      alleles <- desireAlleleValues() # alleles <- result$metadata$geno[input$markers2MAS,"refAllele"]
      dtMta <- data()
      X <- dtMta$metadata$geno[input$markers2MAS,]
      positiveAlleles <- ifelse(alleles == 0, X$altAllele,X$refAllele)

      weights <- desireWeightValues()

      ui_inputs <- shiny::reactiveValuesToList(input)
      save(ui_inputs, file="bugmas.RData")
      result <- try( cgiarPipeline::markerAssistedSelection(
        object = data() ,
        analysisIdForGenoModifications= input$version2Mta,
        markersToBeUsed=input$markers2MAS,
        positiveAlleles=positiveAlleles,
        desire=weights, ploidy=input$ploidy
      ), silent = TRUE)

      # save(result, file = "./R/outputs/resultMASGeno.RData")
      shinybusy::remove_modal_spinner()

      if(!inherits(result,"try-error")) { # if all goes well in the run
        cat(paste("Marker Assisted Selection analysis saved with id:",as.POSIXct( result$status$analysisId[nrow(result$status)], origin="1970-01-01", tz="GMT") ))
        if("analysisIdName" %in% colnames(result$status)){result$status$analysisIdName[nrow(result$status)] <- input$analysisIdName}
        data(result)
        updateTabsetPanel(session, "tabsMain", selected = "outputTabs")
        # predictions
        output$predictionsMAS <-  DT::renderDT({
          # if(!inherits(result,"try-error") ){
          predictions <- result$predictions
          predictions <- predictions[predictions$module=="mas",]
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
        output$metricsMAS <-  DT::renderDT({
          # if(!inherits(result,"try-error") ){
          metrics <- result$metrics
          metrics <- metrics[metrics$module=="mas",]
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
        output$modelingMAS <-  DT::renderDT({
          # if(!inherits(result,"try-error") ){
          modeling <- result$modeling
          modeling <- modeling[modeling$module=="mas",]
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
        ###############################
        # update genoView in output tab
        observeEvent(c(data(),input$version2Mta), {
          req(data())
          dtMAS <- data()
          dtPed <- dtMAS$data$pedigree
          if(!is.null(dtPed)){
            metaPed <- dtMAS$metadata$pedigree
            colnames(dtPed) <- cgiarBase::replaceValues(colnames(dtPed), Search = metaPed$value, Replace = metaPed$parameter)
            mychoices <- na.omit(unique(dtPed$designation))
            updateSelectizeInput(session, "genoView", choices = mychoices, selected = mychoices[1])
          }
        })

        # ## Report tab
        output$reportMASGeno <- renderUI({
          HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportMas.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
        })

        output$downloadReportMASGeno <- downloadHandler(
          filename = function() {
            paste(paste0('mas_dashboard_',gsub("-", "", as.integer(Sys.time()))), sep = '.', switch(
              "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
            ))
          },
          content = function(file) {
            shinybusy::show_modal_spinner(spin = "fading-circle", text = "Generating Report...")

            src <- normalizePath(system.file("rmd","reportMas.Rmd",package="bioflow"))
            src2 <- normalizePath('data/resultMas.RData')

            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))

            file.copy(src, 'report.Rmd', overwrite = TRUE)
            file.copy(src2, 'resultMas.RData', overwrite = TRUE)

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

      }else{ cat(paste("Analysis failed with the following error message: \n\n",result[[1]])); hideAll$clearAll <- TRUE}

      hideAll$clearAll <- FALSE

    })
    output$outMAS <- renderPrint({
      outMAS()
    })


  })
}

## To be copied in the UI
# mod_masApp_ui("masApp_1")

## To be copied in the server
# mod_masApp_server("masApp_1")

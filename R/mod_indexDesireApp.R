#' indexDesireApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_indexDesireApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    tags$br(),

    mainPanel( width = 12,
               tabsetPanel( id=ns("tabsMain"),
                            type = "tabs",
                            tabPanel(div(icon("book"), "Information") ,
                                     br(),
                                     column(width = 6,
                                            h1(strong(span("Desire Selection Index Module", tags$a(href="https://www.youtube.com/watch?v=YhveKqd0q4s&list=PLZ0lafzH_UmclOPifjCntlMzysEB2_2wX&index=8", icon("youtube") , target="_blank"), style="color:darkcyan"))),
                                            h2(strong("Data Status (wait to be displayed):")),
                                            uiOutput(ns("warningMessage")),
                                            tags$br(),
                                            # column(width=4, tags$br(),
                                            shinyWidgets::prettySwitch( inputId = ns('launch'), label = "Load example dataset", status = "success"),
                                            # ),
                                            tags$br(),
                                            img(src = "www/indexDesire.png", height = 300, width = 600), # add an image
                                     ),

                                     column(width = 6,
                                            h2(strong("Details")),
                                            p("Genetic evaluation has as final purpose to select the individuals with highest genetic merit across
                                          all traits of interest. In order to select for multiple traits at the same time a selection index is preferred.
                                          This option aims to calculate a selection index using across-environment predictions from multiple
                              traits based on user's desired change (used to calculate weights) and return a table of predictions with the index and the traits used
                              for selection.
                                The way the options are used is the following:"),
                                            p(strong("Traits to include in the index-")," Traits to be considered in the index."),
                                            p(strong("Desire or base values.-")," Vector of values indicating the desired change in traits."),
                                            p(strong("Scale traits.-")," A TRUE or FALSE value indicating if the table of traits should be
                                scaled or not. If TRUE is selected, the values of the desire vector are expected to be expressed in
                                standard deviations. If FALSE, the values of the desire vector are expected to be expressed in
                                original-scale units."),
                                            h2(strong("References")),
                                            p("Pesek, J., & Baker, R. J. (1969). Desired improvement in relation to selection indices. Canadian journal of plant science, 49(6), 803-804."),
                                            p("Ceron-Rojas, J. J., & Crossa, J. (2018). Linear selection indices in modern plant breeding (p. 256). Springer Nature."),
                                            h2(strong("Software used")),
                                            p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
                                Vienna, Austria. URL https://www.R-project.org/."),
                                            # column(width = 12, shiny::plotOutput(ns("plotDataDependencies")), ),
                                     ),
                            ),
                            tabPanel(div(icon("arrow-right-to-bracket"), "Input steps"),
                                     tabsetPanel(
                                       tabPanel(div( icon("dice-one"), "Pick MTA-stamp(s)", icon("arrow-right") ), # icon = icon("dice-one"),
                                                br(),
                                                column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                       column(width=8, selectInput(ns("version2IdxD"),
                                                                                   label = tags$span(
                                                                                     "MTA or MAS version(s) to find traits",
                                                                                     tags$i(
                                                                                       class = "glyphicon glyphicon-info-sign",
                                                                                       style = "color:#FFFFFF",
                                                                                       title = "Analysis ID(s) from MTA runs or MAS runs that contain the trait predictions that should be used to fit a desired selection index."
                                                                                     )
                                                                                   ),
                                                                                   choices = NULL, multiple = TRUE)),

                                                ),
                                                column(width=12),
                                                shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                    column(width=12,
                                                                           hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                           h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameters to be specified in the grey boxes above.", style="color:green"))),
                                                                           hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                    ),
                                                                    column( width=4, DT::DTOutput(ns("tableTraitTimeStamps")), br(),br(),  ),
                                                                    column( width=8, shiny::plotOutput(ns("plotTimeStamps")), br(), br(), ),
                                                                    column( width=12, DT::DTOutput(ns("statusIndex")),br(), br(), ),
                                                                    column( width=12,DT::DTOutput(ns("tablePredictionsTraitsWide")),br(), br(), ),
                                                ),
                                       ),
                                       tabPanel( div( icon("dice-two"), "Pick parameters", icon("arrow-right") ) , # icon = icon("dice-two"),
                                                br(),
                                                column(width=3, style = "background-color:grey; color: #FFFFFF",
                                                       selectInput(ns("trait2IdxD"),
                                                                   label = tags$span(
                                                                     "Trait(s) to analyze",
                                                                     tags$i(
                                                                       class = "glyphicon glyphicon-info-sign",
                                                                       style = "color:#FFFFFF",
                                                                       title = "It is important to include traits that also need to be controlled in the population (e.g., height, maturity) even when we don't want to increase them or decrease them."
                                                                     )
                                                                   ),
                                                                   choices = NULL, multiple = TRUE),
                                                       selectInput(ns("env2IdxD"),
                                                                   label = tags$span(
                                                                     "Environment to use",
                                                                     tags$i(
                                                                       class = "glyphicon glyphicon-info-sign",
                                                                       style = "color:#FFFFFF",
                                                                       title = "Currently onle the across environment estimate is allowed which is labeled as (Intercept)."
                                                                     )
                                                                   ),
                                                                   choices = NULL, multiple = FALSE),
                                                       selectInput(ns("effectType2IdxD"),
                                                                   label = tags$span(
                                                                     "Effect type to use",
                                                                     tags$i(
                                                                       class = "glyphicon glyphicon-info-sign",
                                                                       style = "color:#FFFFFF",
                                                                       title = "Since each fixed and random effect returns estimates the user should specify which effect should be used in the index. It is expected by default that the designation estimates are the ones to be used in the index."
                                                                     )
                                                                   ),
                                                                   choices = NULL, multiple = FALSE),
                                                       selectInput(ns("entryType2IdxD"),
                                                                   label = tags$span(
                                                                     "Entry type(s) to use",
                                                                     tags$i(
                                                                       class = "glyphicon glyphicon-info-sign",
                                                                       style = "color:#FFFFFF",
                                                                       title = "The labels available in the entryType column can be used to reduce the dataset for the index. By default all entry types are included."
                                                                     )
                                                                   ),
                                                                   choices = NULL, multiple = TRUE),
                                                       selectInput(ns("scaledIndex"),
                                                                   label = tags$span(
                                                                     "Scale traits for index?",
                                                                     tags$i(
                                                                       class = "glyphicon glyphicon-info-sign",
                                                                       style = "color:#FFFFFF",
                                                                       title = "If TRUE, the traits are scaled to a standardized normal distribution and the values represent the number of standard deviations desired to change. If FALSE, the trait is conserved in the original units and the user should specify the desired change in native units. The slider still always represents the equivalent to +/- 5 standard deviations."
                                                                     )
                                                                   ),
                                                                   choices = list(TRUE,FALSE), selected = FALSE, multiple=FALSE),
                                                       uiOutput(ns("SliderDesireIndex"))
                                                ),
                                                column(width=9, #style = "height:550px; overflow-y: scroll;overflow-x: scroll;",
                                                       column(width=12),
                                                       shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                           column(width=12,
                                                                                  hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                                  h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameters to be specified in the grey box in the left", style="color:green"))),
                                                                                  hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                           ),
                                                                           tags$span(id = ns('holder1'),
                                                                                     column(width = 12,
                                                                                            numericInput(ns("fontSizeRadar"), label = "Font size", value = 12),
                                                                                            plotly::plotlyOutput(ns("plotPredictionsRadar")),
                                                                                     ),
                                                                                     column(width = 12,
                                                                                            numericInput(ns("proportion"), label = "Selected proportion for graphs", value = 0.1, min=0.001,max=1, step=0.05),
                                                                                            shiny::plotOutput(ns("plotPotentialResponse")),
                                                                                     ),
                                                                                     column(width = 12,
                                                                                            p(span("Metrics associated to the MTA stamp selected.", style="color:black")),
                                                                                            selectInput(ns("parameterMetrics"), "Parameter to visualize", choices = NULL, multiple = FALSE),
                                                                                            plotly::plotlyOutput(ns("barplotPredictionsMetrics")),
                                                                                     ),
                                                                           ),
                                                       ),
                                                ),

                                       ),
                                       tabPanel("Run analysis", icon = icon("dice-three"),
                                                br(),
                                                column(width=12,style = "background-color:grey; color: #FFFFFF",
                                                       column(width=3, tags$div(textInput(ns("analysisIdName"), label = tags$span(
                                                         "Analysis Name (optional)", tags$i( class = "glyphicon glyphicon-info-sign", style = "color:#FFFFFF",
                                                                                             title = "An optional name for the analysis besides the timestamp if desired.") ), #width = "100%",
                                                         placeholder = "(optional name)") ) ),
                                                       column(width=3,
                                                              br(),
                                                              actionButton(ns("runIdxD"), "Calculate index", icon = icon("play-circle")),
                                                              uiOutput(ns("qaQcIdxDInfo")),
                                                       ),
                                                       column(width=6,
                                                              br(),
                                                                shinydashboard::box(width = 12, style = "color: #000000", status = "success", solidHeader=FALSE,collapsible = TRUE, collapsed = TRUE, title = "Additional run settings (optional)...",
                                                                                  selectInput(ns("verboseIndex"), label = "Print logs?", choices = list(TRUE,FALSE), selected = FALSE, multiple=FALSE)
                                                              ),
                                                       ),
                                                       br()
                                                ),
                                                textOutput(ns("outIdxD")),
                                       ),
                                     )
                            ),
                            tabPanel(div(icon("arrow-right-from-bracket"), "Output tabs" ) , value = "outputTabs",
                                     tabsetPanel(
                                       tabPanel("Dashboard", icon = icon("file-image"),
                                                br(),
                                                textOutput(ns("outIdxD2")),
                                                br(),
                                                downloadButton(ns("downloadReportIndex"), "Download dashboard"),
                                                br(),
                                                uiOutput(ns('reportIndex'))

                                       ),
                                       tabPanel("Predictions", icon = icon("table"),
                                                br(),
                                                DT::DTOutput(ns("predictionsIdxD"))
                                       ),
                                       tabPanel("Modeling", icon = icon("table"),
                                                br(),
                                                DT::DTOutput(ns("modelingIdxD"))
                                       ),

                                     )
                            )# end of output panel
               )) # end mainpanel


  )
}

#' indexDesireApp Server Functions
#'
#' @noRd
# mod_indexDesireApp_server <- function(id, data){ # parallel
#   moduleServer( id, function(input, output, session){
#     ns <- session$ns
#
#     output$plotDataDependencies <- shiny::renderPlot({ dependencyPlot() })
#     ############################################################################ clear the console
#     hideAll <- reactiveValues(clearAll = TRUE)
#     observeEvent(data(), {
#       hideAll$clearAll <- TRUE
#     })
#     ############################################################################
#     # show shinyWidgets until the user can use the module
#     observeEvent(c(data(), input$version2IdxD, input$trait2IdxD ), {
#       req(data())
#       mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
#       if(mappedColumns == 3 & length(input$version2IdxD)>0 & length(input$trait2IdxD)>0 ){
#         golem::invoke_js('showid', ns('holder1'))
#       }else{
#         golem::invoke_js('hideid', ns('holder1'))
#       }
#     })
#     ############################################################################
#     # warning message
#     output$warningMessage <- renderUI(
#       if(is.null(data())){
#         HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your phenotypic data using the 'Data Retrieval' tab.")) )
#       }else{ # data is there
#         mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
#         if(mappedColumns == 3){
#           if( any( c("mta","mtaFlex","mtaLmms") %in% data()$status$module ) ){
#             HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to perform the selection index specifying your input parameters under the Input tabs.")) )
#           }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please perform a Multi-Trial Analysis before performing a selection index")) ) }
#         }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please make sure that you have computed the 'environment' column, and that column 'designation' and \n at least one trait have been mapped using the 'Data Retrieval' tab.")) )}
#       }
#     )
#     ## data example loading
#     observeEvent(
#       input$launch,
#       if(length(input$launch) > 0){
#         if (input$launch) {
#           shinyWidgets::ask_confirmation(
#             inputId = ns("myconfirmation"),
#             text = "Are you sure you want to load the example data? This will delete any data currently in the environment.",
#             title = "Data replacement warning"
#           )
#         }
#       }
#     )
#     observeEvent(input$myconfirmation, {
#       if (isTRUE(input$myconfirmation)) {
#         shinybusy::show_modal_spinner('fading-circle', text = 'Loading example...')
#         ## replace tables
#         data(cgiarBase::create_getData_object())
#         tmp <- data()
#         utils::data(DT_example, package = "cgiarPipeline")
#         if(!is.null(result$data)){tmp$data <- result$data}
#         if(!is.null(result$metadata)){tmp$metadata <- result$metadata}
#         if(!is.null(result$modifications)){tmp$modifications <- result$modifications}
#         if(!is.null(result$predictions)){tmp$predictions <- result$predictions}
#         if(!is.null(result$metrics)){tmp$metrics <- result$metrics}
#         if(!is.null(result$modeling)){tmp$modeling <- result$modeling}
#         if(!is.null(result$status)){tmp$status <- result$status}
#         data(tmp) # update data with results
#         shinybusy::remove_modal_spinner()
#       }else{
#         shinyWidgets::updatePrettySwitch(session, "launch", value = FALSE)
#       }
#     }, ignoreNULL = TRUE)
#     ######################################################################################
#     ######################################################################################
#     ########################################### input parameters
#
#     #################
#     ## version
#     observeEvent(c(data()), {
#       req(data())
#       dtIdxD <- data()
#       dtIdxD <- dtIdxD$status
#       dtIdxD <- dtIdxD[which(dtIdxD$module %in% c("mta","mtaFlex","mtaLmms") ),]
#       traitsIdxD <- unique(dtIdxD$analysisId)
#       if(length(traitsIdxD) > 0){names(traitsIdxD) <- as.POSIXct(traitsIdxD, origin="1970-01-01", tz="GMT")}
#       updateSelectInput(session, "version2IdxD", choices = traitsIdxD)
#     })
#     #################
#     ## traits
#     observeEvent(c(data(), input$version2IdxD), {
#       req(data())
#       req(input$version2IdxD)
#       dtIdxD <- data()
#       dtIdxD <- dtIdxD$predictions
#       dtIdxD <- dtIdxD[which(dtIdxD$analysisId %in% input$version2IdxD),]
#       traitsIdxD <- unique(dtIdxD$trait)
#       updateSelectInput(session, "trait2IdxD", choices = traitsIdxD)
#     })
#     #################
#     ## environments
#     observeEvent(c(data(), input$version2IdxD, input$trait2IdxD), {
#       req(data())
#       req(input$version2IdxD)
#       req(input$trait2IdxD)
#       dtIdxD <- data()
#       dtIdxD <- dtIdxD$predictions
#       dtIdxD <- dtIdxD[which(dtIdxD$analysisId %in% input$version2IdxD),]
#       dtIdxD <- dtIdxD[which(dtIdxD$trait %in% input$trait2IdxD),]
#       traitsIdxD <- unique(dtIdxD$environment)
#       updateSelectInput(session, "env2IdxD", choices = traitsIdxD)
#     })
#     #################
#     ## entry types
#     observeEvent(c(data(), input$version2IdxD, input$env2IdxD), {
#       req(data())
#       req(input$version2IdxD)
#       req(input$env2IdxD)
#       dtIdxD <- data()
#       dtIdxD <- dtIdxD$predictions
#       dtIdxD <- dtIdxD[which(dtIdxD$analysisId %in% input$version2IdxD),]
#       # if(input$env2IdxD != "(Intercept)"){}
#       dtIdxD <- dtIdxD[which(dtIdxD$environment %in% input$env2IdxD),]
#       traitsIdxD <- unique(dtIdxD$entryType)
#       updateSelectInput(session, "effectType2IdxD", choices = traitsIdxD, selected = traitsIdxD)
#     })
#     ####################
#     ## desired changes for Desire Index
#     output$SliderDesireIndex <- renderUI({
#       req(data())
#       req(input$version2IdxD)
#       req(input$trait2IdxD)
#       req(input$env2IdxD)
#       # req(input$effectType2IdxD)
#       req(input$scaledIndex)
#       trait2IdxD <- input$trait2IdxD # trait2IdxD <- c("Yield_Mg_ha_QTL","Ear_Height_cm") # list(trait2IdxD=c("Yield_Mg_ha","Ear_Height_cm"))
#       dtIdxD <- data()
#       dtIdxD <- dtIdxD$predictions
#       dtIdxD <- dtIdxD[which(dtIdxD$analysisId %in% input$version2IdxD),]
#       dtIdxD <- dtIdxD[which(dtIdxD$environment %in% input$env2IdxD),]
#       if(!is.null(input$effectType2IdxD)){dtIdxD <- dtIdxD[which(dtIdxD$entryType %in% input$effectType2IdxD),]}
#
#       if(input$scaledIndex){ # if user wants traits scaled
#         lapply(1:length(trait2IdxD), function(i) {
#           sliderInput(
#             session$ns(paste0('SliderDesireIndex',i)),
#             paste0('Desired change (SDs)',": ",trait2IdxD[i]),
#             min = -5,
#             max = 5,
#             value = 0,
#             step = 0.5
#           )
#         })
#       }else{ # if user wants to use original scale
#         lapply(1:length(trait2IdxD), function(i) {
#           traitVals <- dtIdxD[which(dtIdxD$trait == trait2IdxD[i]), "predictedValue"]
#           sliderInput(
#             session$ns(paste0('SliderDesireIndex',i)),
#             paste0('Desired change (original scale)',": ",trait2IdxD[i]),
#             min = -round(sd(traitVals, na.rm=TRUE)*4,3),
#             max = round(sd(traitVals, na.rm=TRUE)*4,3),
#             value = 0, #round(sd(traitVals, na.rm=TRUE),3),
#             step = round(0.5*sd(traitVals, na.rm=TRUE),3)
#           )
#         })
#       }
#
#     })
#     ######################################################################################
#     ######################################################################################
#     ########################################### plots
#     #################
#     ## render the table of traits and analysisID being selected
#     output$tableTraitTimeStamps <-  DT::renderDT({
#       req(data())
#       ### change column names for mapping
#       if(length(input$version2IdxD) > 0){
#         status <- data()$status
#         modeling <- data()$modeling
#         statusPlusModel <- merge(status, unique(modeling[,c("analysisId","trait")]), by="analysisId", all.x = TRUE)
#         '%!in%' <- function(x,y)!('%in%'(x,y))
#         statusPlusModel <- statusPlusModel[which(statusPlusModel$trait %!in% c("inputObject",NA)),]
#         statusPlusModel <- statusPlusModel[which(statusPlusModel$analysisId %in% input$version2IdxD),]
#         statusPlusModel$analysisId <- as.POSIXct(statusPlusModel$analysisId, origin="1970-01-01", tz="GMT")
#       }else{
#         statusPlusModel <- data.frame(analysisId=NA, module=NA, trait=NA)
#       }
#       DT::datatable(statusPlusModel, extensions = 'Buttons', # I changed Blfrtip to lfrtip and silenced the buttons
#                     options = list(dom = 'lfrtip',scrollX = TRUE, #buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
#                                    lengthMenu = list(c(8,20,50,-1), c(8,20,50,'All'))),
#                     caption = htmltools::tags$caption(
#                       style = 'color:cadetblue', #caption-side: bottom; text-align: center;
#                       htmltools::em('Traits available in the STA-IDs selected.')
#                     )
#       )
#     }, server = FALSE)
#     ## render timestamps flow
#     output$plotTimeStamps <- shiny::renderPlot({
#       req(data()) # req(input$version2Sta)
#       xx <- data()$status;  yy <- data()$modeling
#       v <- which(yy$parameter == "analysisId")
#       if(length(v) > 0){
#         yy <- yy[v,c("analysisId","value")]
#         zz <- merge(xx,yy, by="analysisId", all.x = TRUE)
#       }else{ zz <- xx; zz$value <- NA}
#       if(!is.null(xx)){
#         colnames(zz) <- cgiarBase::replaceValues(colnames(zz), Search = c("analysisId","value"), Replace = c("outputId","inputId") )
#         nLevelsCheck1 <- length(na.omit(unique(zz$outputId)))
#         nLevelsCheck2 <- length(na.omit(unique(zz$inputId)))
#         if(nLevelsCheck1 > 1 & nLevelsCheck2 > 1){
#           X <- with(zz, sommer::overlay(outputId, inputId))
#         }else{
#           if(nLevelsCheck1 == 1){
#             X1 <- matrix(ifelse(is.na(zz$inputId),0,1),nrow=length(zz$inputId),1); colnames(X1) <- as.character(na.omit(unique(c(zz$outputId))))
#           }else{X1 <- model.matrix(~as.factor(outputId)-1, data=zz); colnames(X1) <- levels(as.factor(zz$outputId))}
#           if(nLevelsCheck2 == 1){
#             X2 <- matrix(ifelse(is.na(zz$inputId),0,1),nrow=length(zz$inputId),1); colnames(X2) <- as.character(na.omit(unique(c(zz$inputId))))
#           }else{X2 <- model.matrix(~as.factor(inputId)-1, data=zz); colnames(X2) <- levels(as.factor(zz$inputId))}
#           mynames <- unique(na.omit(c(zz$outputId,zz$inputId)))
#           X <- matrix(0, nrow=nrow(zz), ncol=length(mynames)); colnames(X) <- as.character(mynames)
#           X[,colnames(X1)] <- X1
#           X[,colnames(X2)] <- X2
#         };  rownames(X) <- as.character(zz$outputId)
#         rownames(X) <-as.character(as.POSIXct(as.numeric(rownames(X)), origin="1970-01-01", tz="GMT"))
#         colnames(X) <-as.character(as.POSIXct(as.numeric(colnames(X)), origin="1970-01-01", tz="GMT"))
#         # make the network plot
#         n <- network::network(X, directed = FALSE)
#         network::set.vertex.attribute(n,"family",zz$module)
#         network::set.vertex.attribute(n,"importance",1)
#         e <- network::network.edgecount(n)
#         network::set.edge.attribute(n, "type", sample(letters[26], e, replace = TRUE))
#         network::set.edge.attribute(n, "day", sample(1, e, replace = TRUE))
#         library(ggnetwork)
#         ggplot2::ggplot(n, ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
#           ggnetwork::geom_edges(ggplot2::aes(color = family), arrow = ggplot2::arrow(length = ggnetwork::unit(6, "pt"), type = "closed") ) +
#           ggnetwork::geom_nodes(ggplot2::aes(color = family), alpha = 0.5, size=5 ) + ggplot2::ggtitle("Network plot of current analyses available") +
#           ggnetwork::geom_nodelabel_repel(ggplot2::aes(color = family, label = vertex.names ),
#                                           fontface = "bold", box.padding = ggnetwork::unit(1, "lines")) +
#           ggnetwork::theme_blank()
#       }
#     })
#     ## render the data to be analyzed (wide format)
#     output$statusIndex <-  DT::renderDT({
#       req(data())
#       req(input$version2IdxD)
#       dtSta <- data() # dtSta<- result
#       ### change column names for mapping
#       paramsPheno <- data()$modeling
#       paramsPheno <- paramsPheno[which(paramsPheno$analysisId %in% input$version2IdxD),, drop=FALSE]
#       paramsPheno$analysisId <- as.POSIXct(paramsPheno$analysisId, origin="1970-01-01", tz="GMT")
#       DT::datatable(paramsPheno, extensions = 'Buttons',
#                     options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
#                                    lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All'))),
#                     caption = htmltools::tags$caption(
#                       style = 'color:cadetblue', #caption-side: bottom; text-align: center;
#                       htmltools::em('Past modeling parameters from MTA stamp(s) selected.')
#                     )
#       )
#     }, server = FALSE)
#     observeEvent(c(data(),input$version2IdxD), { # update parameter
#       req(data())
#       req(input$version2IdxD)
#       dtMta <- data()
#       dtMta <- dtMta$metrics
#       dtMta <- dtMta[which(dtMta$analysisId %in% input$version2IdxD),] # only traits that have been QA
#       metricsMtaInput <- unique(dtMta$parameter)
#       updateSelectInput(session, "parameterMetrics", choices = metricsMtaInput)
#     })
#     output$barplotPredictionsMetrics <- plotly::renderPlotly({
#       req(data())
#       req(input$version2IdxD)
#       dtMta <- data()
#       mydata <- dtMta$metrics
#       mydata <- mydata[which(mydata$analysisId %in% input$version2IdxD),]
#       mydata = mydata[which(mydata$parameter %in% input$parameterMetrics),]
#       res = plotly::plot_ly(data = mydata, x = mydata[,"environment"], y = mydata[,"value"],
#                             color=mydata[,"trait"]
#       )
#       res = res %>% plotly::add_bars()
#       res
#     })
#     output$tablePredictionsTraitsWide <-  DT::renderDT({
#       req(data())
#       req(input$version2IdxD)
#       dtIdxD <- data(); dtIdxD <- dtIdxD$predictions
#       dtIdxD <- dtIdxD[which(dtIdxD$analysisId %in% input$version2IdxD),setdiff(colnames(dtIdxD),c("module","analysisId"))]
#       wide <- stats::reshape(dtIdxD[,c(c("designation"),"trait",c("predictedValue"))], direction = "wide", idvar = c("designation"),
#                              timevar = "trait", v.names = c("predictedValue"), sep= "_")
#       colnames(wide) <- gsub("predictedValue_","",colnames(wide))
#       numeric.output <- colnames(wide)[-c(1)]
#       DT::formatRound(DT::datatable(wide, extensions = 'Buttons',
#                                     options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
#                                                    lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All'))),
#                                     caption = htmltools::tags$caption(
#                                       style = 'color:cadetblue', #caption-side: bottom; text-align: center;
#                                       htmltools::em('MTA predictions to be used as input.')
#                                     )
#       ), numeric.output)
#     }, server = FALSE)
#
#     desireValues = reactive({
#       req(data())
#       req(input$version2IdxD)
#       req(input$trait2IdxD)
#       dtIdxD <- data(); dtIdxD <- dtIdxD$predictions
#       mydata <- dtIdxD[which(dtIdxD$analysisId %in% input$version2IdxD),setdiff(colnames(dtIdxD),c("module","analysisId"))]
#       if (length(input$trait2IdxD) != 0) {
#         values <- NULL
#         for (i in 1:length(input$trait2IdxD)) {
#           tempval <- reactive({paste0('input$','SliderDesireIndex',i)})
#           values[i] <- tempval()
#           values[i] <- eval(parse(text = values[i]))
#         }
#         values <- t(as.numeric(values))
#         values <- as.data.frame(values)
#         colnames(values) <- input$trait2IdxD
#         values <- as.numeric(values)
#         return(values)
#       }
#     })
#     # render radar plot for initial values
#     output$plotPredictionsRadar <-  plotly::renderPlotly({
#       req(data())
#       req(input$version2IdxD)
#       req(input$trait2IdxD)
#       req(input$env2IdxD)
#       dtIdxD <- data();
#       dtIdxD <- dtIdxD$predictions
#       mydata <- dtIdxD[which(dtIdxD$analysisId %in% input$version2IdxD),setdiff(colnames(dtIdxD),c("module","analysisId"))]
#       mydata <- mydata[which(mydata$environment %in% input$env2IdxD),]
#       if(!is.null(input$effectType2IdxD)){
#         dtIdxD <- dtIdxD[which(dtIdxD$entryType %in% input$effectType2IdxD),]
#       }
#       values <- desireValues()
#       if(!is.null(values)){
#         ## ensure product profile means come sorted
#         if(length(input$trait2IdxD) == length(values) ){
#           dd <- data.frame(trait=input$trait2IdxD, value=values )
#           dd <- dd[with(dd, order(as.numeric(as.factor(trait)))), ]
#           desireRp <- dd[,"value"]
#           traitRp <- dd[,"trait"]
#         }else{desireRp <- values; traitRp <- input$trait2IdxD}
#         radarPlot(mydata=mydata, environmentPredictionsRadar2=input$env2IdxD,traitFilterPredictionsRadar2=traitRp,proportion=input$proportion,meanGroupPredictionsRadar= paste(desireRp, collapse = ", "),
#                   fontSizeRadar=input$fontSizeRadar, r0Radar=NULL, neRadar=NULL, plotSdRadar=FALSE, title="Radar plot to inspect population values versus target values.") # send to setting plotSdRadar # send to argument meanGroupPredictionsRadar
#       }
#     })
#     # render plot for potential responses
#     output$plotPotentialResponse <-  shiny::renderPlot({
#       req(data())
#       req(input$version2IdxD)
#       req(input$trait2IdxD)
#       req(input$env2IdxD)
#       dtIdxD <- data();
#       values <- desireValues()
#       if(!is.null(values)){
#         plotDensitySelected(object=dtIdxD,environmentPredictionsRadar2=input$env2IdxD, traitFilterPredictionsRadar2=input$trait2IdxD, meanGroupPredictionsRadar=paste(values, collapse = ", "), proportion=input$proportion,
#                             analysisId=input$version2IdxD, trait=input$trait2IdxD, desirev=paste(values, collapse = ", "), scaled=input$scaledIndex, title="Expected response to selection using current desire changes")
#       }
#
#     })
#
#     ######################################################################################
#     ######################################################################################
#     ########################################### actual run
#
#     my_desire <- ExtendedTask$new(function(input, data, values) {
#       promises::future_promise({
#         # some long process
#         # define values for slider all traits for base index
#         result <- try(cgiarPipeline::indexDesire(
#           phenoDTfile= data, # input data structure
#           analysisId=input$version2IdxD, # analysis to be picked from predictions database
#           trait= input$trait2IdxD, # traits to include in the index
#           environmentToUse =input$env2IdxD ,
#           entryTypeToUse = input$effectType2IdxD,
#           desirev = values, # as.numeric(unlist(strsplit(input$desirev,","))), # vector of desired values
#           scaled=input$scaledIndex, # whether predicted values should be scaled or not
#           verbose=input$verboseIndex # should we print logs or not
#         ),
#         silent=TRUE
#         )
#
#         return(result)
#       })
#     })
#     ## "run" button click
#     observeEvent(input$runIdxD, {
#       req(data())
#       req(input$version2IdxD)
#       req(input$trait2IdxD)
#       req(input$env2IdxD)
#       shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
#       ui_inputs <- shiny::reactiveValuesToList(input)
#       data_obj  <- data()
#       values <- desireValues()
#       my_desire$invoke(ui_inputs, data_obj, values)
#     })
#
#     ## render
#     output$outIdxD <- output$outIdxD2 <- renderPrint({
#
#       if(sum(data()$status$module %in% c("mta","mtaFlex","mtaLmms") ) == 0) {
#         output$qaQcIdxDInfo <- renderUI({
#           if (hideAll$clearAll){
#             return()
#           }else{
#             req(data())
#             HTML(as.character(div(style="color: brown;",
#                                   "Please perform Multi-Trial-Analysis before conducting a Selection index."))
#             )
#           }
#         })
#       }else{
#         output$qaQcIdxDInfo <- renderUI({return(NULL)})
#         result <- my_desire$result()
#         shinybusy::remove_modal_spinner()
#         if(!inherits(result,"try-error")) {
#           data(result) # update data with results
#           cat(paste("Selection index step with id:",as.POSIXct(result$status$analysisId[length(result$status$analysisId)], origin="1970-01-01", tz="GMT"),"saved. Please proceed to select the best crosses using the OCS module using this time stamp."))
#           updateTabsetPanel(session, "tabsMain", selected = "outputTabs")
#
#           # display table of predictions
#           output$predictionsIdxD <-  DT::renderDT({
#             predictions <- result$predictions
#             predictions <- predictions[predictions$module=="indexD",]
#             predictions$analysisId <- as.numeric(predictions$analysisId)
#             predictions <- predictions[!is.na(predictions$analysisId),]
#             current.predictions <- predictions[predictions$analysisId==max(predictions$analysisId),]
#             current.predictions <- subset(current.predictions, select = -c(module,analysisId))
#             numeric.output <- c("predictedValue", "stdError", "reliability")
#             DT::formatRound(DT::datatable(current.predictions, extensions = 'Buttons',
#                                           options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
#                                                          lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
#             ), numeric.output)
#           }, server = FALSE)
#           # display table of modeling
#           output$modelingIdxD <-  DT::renderDT({
#             modeling <- result$modeling
#             mtas <- result$status[which(result$status$module == "indexD"),"analysisId"]; mtaId <- mtas[length(mtas)]
#             modeling <- modeling[which(modeling$analysisId == mtaId),]
#             modeling <- subset(modeling, select = -c(module,analysisId))
#             DT::datatable(modeling, extensions = 'Buttons',
#                           options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
#                                          lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
#             )
#           }, server = FALSE)
#           ## Report tab
#           output$reportIndex <- renderUI({
#             HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportIndex.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
#           })
#
#           output$downloadReportIndex <- downloadHandler(
#             filename = function() {
#               paste('my-report', sep = '.', switch(
#                 "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
#               ))
#             },
#             content = function(file) {
#               src <- normalizePath(system.file("rmd","reportIndex.Rmd",package="bioflow"))
#               src2 <- normalizePath('data/resultIndex.RData')
#               # temporarily switch to the temp dir, in case you do not have write
#               # permission to the current working directory
#               owd <- setwd(tempdir())
#               on.exit(setwd(owd))
#               file.copy(src, 'report.Rmd', overwrite = TRUE)
#               file.copy(src2, 'resultIndex.RData', overwrite = TRUE)
#               out <- rmarkdown::render('report.Rmd', params = list(toDownload=TRUE),switch(
#                 "HTML",
#                 HTML = rmdformats::robobook(toc_depth = 4)
#                 # HTML = rmarkdown::html_document()
#               ))
#               file.rename(out, file)
#             }
#           )
#
#         }else{
#           output$predictionsIdxD <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
#           output$metricsIdxD <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
#           output$modelingIdxD <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
#           hideAll$clearAll <- TRUE
#           cat(paste("Analysis failed with the following error message: \n\n",result[[1]]))
#         }
#       }
#
#     })
#
#
#   })
# }

mod_indexDesireApp_server <- function(id, data){
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
    observeEvent(c(data(), input$version2IdxD, input$trait2IdxD ), {
      req(data())
      mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
      if(mappedColumns == 3 & length(input$version2IdxD)>0 & length(input$trait2IdxD)>0 ){
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
          if( any( c("mta","mtaFlex","mtaLmms","mas") %in% data()$status$module ) ){
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
    ########################################### input parameters

    #################
    ## version
    observeEvent(c(data()), {
      req(data())
      dtIdxD <- data()
      dtIdxD <- dtIdxD$status
      dtIdxD <- dtIdxD[which(dtIdxD$module %in% c("mta","mtaFlex","mtaLmms","mas") ),]
      traitsIdxD <- unique(dtIdxD$analysisId)
      if(length(traitsIdxD) > 0){
        if("analysisIdName" %in% colnames(dtIdxD)){
          names(traitsIdxD) <- paste(dtIdxD$analysisIdName, as.POSIXct(traitsIdxD, origin="1970-01-01", tz="GMT"), sep = "_")
        }else{
          names(traitsIdxD) <- as.POSIXct(traitsIdxD, origin="1970-01-01", tz="GMT")
        }
      }
      updateSelectInput(session, "version2IdxD", choices = traitsIdxD)
    })
    #################
    ## traits
    observeEvent(c(data(), input$version2IdxD), {
      req(data())
      req(input$version2IdxD)
      dtIdxD <- data()
      dtIdxD <- dtIdxD$predictions
      dtIdxD <- dtIdxD[which(dtIdxD$analysisId %in% input$version2IdxD),]
      traitsIdxD <- unique(dtIdxD$trait)
      updateSelectInput(session, "trait2IdxD", choices = traitsIdxD)
    })
    #################
    ## environments # input <- list(version2IdxD=result$status$analysisId[9], trait2IdxD=c("Ear_Height_cm","Plant_Height_cm","Yield_Mg_ha"))
    observeEvent(c(data(), input$version2IdxD, input$trait2IdxD), {
      # input <- list(version2IdxD=result$status$analysisId[9], trait2IdxD= c("Yield_Mg_ha_QTL","Ear_Height_cm"), env2IdxD="(Intercept)",effectType2IdxD="designation" )
      req(data())
      req(input$version2IdxD)
      req(input$trait2IdxD)
      dtIdxD <- data()
      dtIdxD <- dtIdxD$predictions
      dtIdxD <- dtIdxD[which(dtIdxD$analysisId %in% input$version2IdxD),]
      dtIdxD <- dtIdxD[which(dtIdxD$trait %in% input$trait2IdxD),]
      # only get environments where all the traits are present
      props <- apply(table(dtIdxD$trait, dtIdxD$environment),2,function(x){length(which(x>0))/length(x)})
      envsAva <- names(props)[which(props == 1)]
      updateSelectInput(session, "env2IdxD", choices = envsAva, selected = ifelse("(Intercept)"%in%envsAva,"(Intercept)", envsAva[1]) )
    })
    #################
    ## effect types
    observeEvent(c(data(), input$version2IdxD, input$trait2IdxD, input$env2IdxD), {
      # input <- list(version2IdxD=result$status$analysisId[4], trait2IdxD= c("Pollen_DAP_days", "Plant_Height_cm", "Yield_Mg_ha"), env2IdxD="(Intercept)",effectType2IdxD="designation" )
      req(data())
      req(input$version2IdxD)
      req(input$trait2IdxD)
      req(input$env2IdxD)
      dtIdxD <- data() # dtIdxD <- result
      dtIdxD <- dtIdxD$predictions
      dtIdxD <- dtIdxD[which(dtIdxD$analysisId %in% input$version2IdxD),]
      dtIdxD <- dtIdxD[which(dtIdxD$trait %in% input$trait2IdxD),]
      dtIdxD <- dtIdxD[which(dtIdxD$environment %in% input$env2IdxD),]
      traitsIdxD <- unique(dtIdxD$effectType)
      updateSelectInput(session, "effectType2IdxD", choices = traitsIdxD, selected = ifelse("designation"%in%traitsIdxD,"designation", traitsIdxD[1]) )
    })
    #################
    ## entry types
    observeEvent(c(data(), input$version2IdxD, input$trait2IdxD, input$env2IdxD, input$effectType2IdxD), {
      # input <- list(version2IdxD=result$status$analysisId[9], trait2IdxD= c("Yield_Mg_ha_QTL","Ear_Height_cm"), env2IdxD="(Intercept)",effectType2IdxD="designation" )
      req(data())
      req(input$version2IdxD)
      req(input$trait2IdxD)
      req(input$env2IdxD)
      req(input$effectType2IdxD)
      dtIdxD <- data()
      dtIdxD <- dtIdxD$predictions
      dtIdxD <- dtIdxD[which(dtIdxD$analysisId %in% input$version2IdxD),]
      dtIdxD <- dtIdxD[which(dtIdxD$trait %in% input$trait2IdxD),]
      dtIdxD <- dtIdxD[which(dtIdxD$environment %in% input$env2IdxD),]
      dtIdxD <- dtIdxD[which(dtIdxD$effectType %in% input$effectType2IdxD),]
      traitsIdxD <- unique(dtIdxD$entryType)
      updateSelectInput(session, "entryType2IdxD", choices = traitsIdxD, selected = traitsIdxD)
    })
    ####################
    ## desired changes for Desire Index
    output$SliderDesireIndex <- renderUI({ # input <- list(version2IdxD=result$status$analysisId[9], trait2IdxD= c("Yield_Mg_ha_QTL","Ear_Height_cm"), env2IdxD="(Intercept)",effectType2IdxD="designation" )
      req(data())
      req(input$version2IdxD)
      req(input$trait2IdxD)
      req(input$env2IdxD)
      req(input$effectType2IdxD)
      req(input$scaledIndex)
      trait2IdxD <- input$trait2IdxD #
      dtIdxD <- data()
      dtIdxD <- dtIdxD$predictions
      dtIdxD <- dtIdxD[which(dtIdxD$analysisId %in% input$version2IdxD),]
      dtIdxD <- dtIdxD[which(dtIdxD$environment %in% input$env2IdxD),]
      if(!is.null(input$effectType2IdxD)){dtIdxD <- dtIdxD[which(dtIdxD$effectType %in% input$effectType2IdxD),]}

      if(input$scaledIndex){ # if user wants traits scaled
        lapply(1:length(trait2IdxD), function(i) {
          sliderInput(
            session$ns(paste0('SliderDesireIndex',i)),
            paste0('Desired change (SDs)',": ",trait2IdxD[i]),
            min = -5,
            max = 5,
            value = 0,
            step = 0.5
          )
        })
      }else{ # if user wants to use original scale
        lapply(1:length(trait2IdxD), function(i) {
          traitVals <- dtIdxD[which(dtIdxD$trait == trait2IdxD[i]), "predictedValue"]
          sliderInput(
            session$ns(paste0('SliderDesireIndex',i)),
            paste0('Desired change (original scale)',": ",trait2IdxD[i]),
            min = -round(sd(traitVals, na.rm=TRUE)*4,3),
            max = round(sd(traitVals, na.rm=TRUE)*4,3),
            value = 0, #round(sd(traitVals, na.rm=TRUE),3),
            step = round(0.5*sd(traitVals, na.rm=TRUE),3)
          )
        })
      }

    })
    ######################################################################################
    ######################################################################################
    ########################################### plots
    #################
    ## render the table of traits and analysisID being selected
    output$tableTraitTimeStamps <-  DT::renderDT({
      req(data())
      ### change column names for mapping
      if(length(input$version2IdxD) > 0){
        status <- data()$status
        modeling <- data()$modeling
        statusPlusModel <- merge(status, unique(modeling[,c("analysisId","trait")]), by="analysisId", all.x = TRUE)
        '%!in%' <- function(x,y)!('%in%'(x,y))
        statusPlusModel <- statusPlusModel[which(statusPlusModel$trait %!in% c("inputObject",NA)),]
        statusPlusModel <- statusPlusModel[which(statusPlusModel$analysisId %in% input$version2IdxD),]
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
    ## render the data to be analyzed (wide format)
    output$statusIndex <-  DT::renderDT({
      req(data())
      req(input$version2IdxD)
      dtSta <- data() # dtSta<- result
      ### change column names for mapping
      paramsPheno <- data()$modeling
      paramsPheno <- paramsPheno[which(paramsPheno$analysisId %in% input$version2IdxD),, drop=FALSE]
      paramsPheno$analysisId <- as.POSIXct(paramsPheno$analysisId, origin="1970-01-01", tz="GMT")
      DT::datatable(paramsPheno, extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All'))),
                    caption = htmltools::tags$caption(
                      style = 'color:cadetblue', #caption-side: bottom; text-align: center;
                      htmltools::em('Past modeling parameters from MTA stamp(s) selected.')
                    )
      )
    }, server = FALSE)
    observeEvent(c(data(),input$version2IdxD), { # update parameter
      req(data())
      req(input$version2IdxD)
      dtMta <- data()
      dtMta <- dtMta$metrics
      dtMta <- dtMta[which(dtMta$analysisId %in% input$version2IdxD),] # only traits that have been QA
      metricsMtaInput <- unique(dtMta$parameter)
      updateSelectInput(session, "parameterMetrics", choices = metricsMtaInput)
    })
    output$barplotPredictionsMetrics <- plotly::renderPlotly({
      req(data())
      req(input$version2IdxD)
      dtMta <- data()
      mydata <- dtMta$metrics
      mydata <- mydata[which(mydata$analysisId %in% input$version2IdxD),]
      mydata = mydata[which(mydata$parameter %in% input$parameterMetrics),]
      res = plotly::plot_ly(data = mydata, x = mydata[,"environment"], y = mydata[,"value"],
                            color=mydata[,"trait"]
      )
      res = res %>% plotly::add_bars()
      res
    })
    output$tablePredictionsTraitsWide <-  DT::renderDT({
      req(data())
      req(input$version2IdxD)
      dtIdxD <- data(); dtIdxD <- dtIdxD$predictions
      dtIdxD <- dtIdxD[which(dtIdxD$analysisId %in% input$version2IdxD),setdiff(colnames(dtIdxD),c("module","analysisId"))]
      wide <- stats::reshape(dtIdxD[,c(c("designation"),"trait",c("predictedValue"))], direction = "wide", idvar = c("designation"),
                             timevar = "trait", v.names = c("predictedValue"), sep= "_")
      colnames(wide) <- gsub("predictedValue_","",colnames(wide))
      numeric.output <- colnames(wide)[-c(1)]
      DT::formatRound(DT::datatable(wide, extensions = 'Buttons',
                                    options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                   lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All'))),
                                    caption = htmltools::tags$caption(
                                      style = 'color:cadetblue', #caption-side: bottom; text-align: center;
                                      htmltools::em('MTA predictions to be used as input.')
                                    )
      ), numeric.output)
    }, server = FALSE)

    desireValues = reactive({
      req(data())
      req(input$version2IdxD)
      req(input$trait2IdxD)
      dtIdxD <- data(); dtIdxD <- dtIdxD$predictions
      mydata <- dtIdxD[which(dtIdxD$analysisId %in% input$version2IdxD),setdiff(colnames(dtIdxD),c("module","analysisId"))]
      if (length(input$trait2IdxD) != 0) {
        values <- NULL
        for (i in 1:length(input$trait2IdxD)) {
          tempval <- reactive({paste0('input$','SliderDesireIndex',i)})
          values[i] <- tempval()
          values[i] <- eval(parse(text = values[i]))
        }
        values <- t(as.numeric(values))
        values <- as.data.frame(values)
        colnames(values) <- input$trait2IdxD
        values <- as.numeric(values)
        return(values)
      }
    })
    # render radar plot for initial values
    output$plotPredictionsRadar <-  plotly::renderPlotly({
      req(data())
      req(input$version2IdxD)
      req(input$trait2IdxD)
      req(input$env2IdxD)
      req(input$effectType2IdxD)
      req(input$entryType2IdxD)
      dtIdxD <- data();
      dtIdxD <- dtIdxD$predictions
      mydata <- dtIdxD[which(dtIdxD$analysisId %in% input$version2IdxD),setdiff(colnames(dtIdxD),c("module","analysisId"))]
      mydata <- mydata[which(mydata$environment %in% input$env2IdxD),]
      if(!is.null(input$effectType2IdxD)){
        dtIdxD <- dtIdxD[which(dtIdxD$effectType %in% input$effectType2IdxD),]
      }
      if(!is.null(input$entryType2IdxD)){
        dtIdxD <- dtIdxD[which(dtIdxD$entryType %in% input$entryType2IdxD),]
      }
      values <- desireValues()
      if(!is.null(values)){
        ## ensure product profile means come sorted
        if(length(input$trait2IdxD) == length(values) ){
          dd <- data.frame(trait=input$trait2IdxD, value=values )
          dd <- dd[with(dd, order(as.numeric(as.factor(trait)))), ]
          desireRp <- dd[,"value"]
          traitRp <- dd[,"trait"]
        }else{desireRp <- values; traitRp <- input$trait2IdxD}
        radarPlot(mydata=mydata, environmentPredictionsRadar2=input$env2IdxD,traitFilterPredictionsRadar2=traitRp,
                  proportion=input$proportion,meanGroupPredictionsRadar= paste(desireRp, collapse = ", "),
                  fontSizeRadar=input$fontSizeRadar, r0Radar=NULL, neRadar=NULL, plotSdRadar=FALSE,
                  title="Radar plot to inspect population values versus target values.") # send to setting plotSdRadar # send to argument meanGroupPredictionsRadar
      }
    })
    # render plot for potential responses
    output$plotPotentialResponse <-  shiny::renderPlot({
      # input <- list(version2IdxD=result$status$analysisId[6], trait2IdxD= c("FLW50","HT_AVG","YLDTONHA"), env2IdxD="(Intercept)",effectType2IdxD="designation", entryType2IdxD="EST007" )
      req(data())
      req(input$version2IdxD)
      req(input$trait2IdxD)
      req(input$env2IdxD)
      req(input$effectType2IdxD)
      req(input$entryType2IdxD)
      dtIdxD <- data();
      values <- desireValues()
      if(!is.null(values)){
        plotDensitySelected(object=dtIdxD,analysisId=input$version2IdxD,
                            environmentPredictionsRadar2=input$env2IdxD, traitFilterPredictionsRadar2=input$trait2IdxD,
                            effectTypePredictionRadar2=input$effectType2IdxD, entryTypePredictionRadar2=input$entryType2IdxD,
                            meanGroupPredictionsRadar=paste(values, collapse = ", "), proportion=input$proportion,
                             trait=input$trait2IdxD, desirev=paste(values, collapse = ", "), scaled=input$scaledIndex, title="Expected response to selection using current desire changes")
      }

    })

    ######################################
    ## render result of "run" button click
    outIdxD <- eventReactive(input$runIdxD, {
      req(data())
      req(input$version2IdxD)
      req(input$trait2IdxD)
      req(input$env2IdxD)
      shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
      dtIdxD <- data()
      # define values for slider all traits for base index
      values <- desireValues()
      # run the modeling, but before test if mta was done
      if(sum(dtIdxD$status$module %in% c("mta","mtaFlex","mtaLmms","mas") ) == 0) {
        output$qaQcIdxDInfo <- renderUI({
          if (hideAll$clearAll)
            return()
          else
            req(dtIdxD)
          HTML(as.character(div(style="color: brown;",
                                "Please perform Multi-Trial-Analysis before conducting a Selection index."))
          )
        })
      }else{
        output$qaQcIdxDInfo <- renderUI({return(NULL)})
        result <- try(cgiarPipeline::indexDesire(
          phenoDTfile= dtIdxD, # input data structure
          analysisId=input$version2IdxD, # analysis to be picked from predictions database
          trait= input$trait2IdxD, # traits to include in the index
          environmentToUse =input$env2IdxD ,
          effectTypeToUse = input$effectType2IdxD,
          entryTypeToUse = input$entryType2IdxD,
          desirev = values, # as.numeric(unlist(strsplit(input$desirev,","))), # vector of desired values
          scaled=input$scaledIndex, # whether predicted values should be scaled or not
          verbose=input$verboseIndex # should we print logs or not
        ),
        silent=TRUE
        )
        if(!inherits(result,"try-error")) {
          if("analysisIdName" %in% colnames(result$status)){result$status$analysisIdName[nrow(result$status)] <- input$analysisIdName}
          data(result) # update data with results
          cat(paste("Selection index step with id:",as.POSIXct(result$status$analysisId[length(result$status$analysisId)], origin="1970-01-01", tz="GMT"),"saved. Please proceed to select the best crosses using the OCS module using this time stamp."))
          updateTabsetPanel(session, "tabsMain", selected = "outputTabs")
        }else{
          cat(paste("Analysis failed with the following error message: \n\n",result[[1]]))
        }
      }
      shinybusy::remove_modal_spinner()
      if(!inherits(result,"try-error")) {
        # display table of predictions
        output$predictionsIdxD <-  DT::renderDT({
          # if ( hideAll$clearAll){
          #   return()
          # }else{
          predictions <- result$predictions
          predictions <- predictions[predictions$module=="indexD",]
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
        output$modelingIdxD <-  DT::renderDT({
          # if ( hideAll$clearAll){
          #   return()
          # }else{
          modeling <- result$modeling
          mtas <- result$status[which(result$status$module == "indexD"),"analysisId"]; mtaId <- mtas[length(mtas)]
          modeling <- modeling[which(modeling$analysisId == mtaId),]
          modeling <- subset(modeling, select = -c(module,analysisId))
          DT::datatable(modeling, extensions = 'Buttons',
                        options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                       lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
          )
          # }
        }, server = FALSE)
        ## Report tab
        output$reportIndex <- renderUI({
          HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportIndex.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
        })

        output$downloadReportIndex <- downloadHandler(
          filename = function() {
            paste(paste0('indexD_dashboard_',gsub("-", "", as.integer(Sys.time()))), sep = '.', switch(
              "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
            ))
          },
          content = function(file) {
            shinybusy::show_modal_spinner(spin = "fading-circle", text = "Generating Report...")

            src <- normalizePath(system.file("rmd","reportIndex.Rmd",package="bioflow"))
            src2 <- normalizePath('data/resultIndex.RData')

            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))

            file.copy(src, 'report.Rmd', overwrite = TRUE)
            file.copy(src2, 'resultIndex.RData', overwrite = TRUE)

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

      } else {
        output$predictionsIdxD <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
        output$metricsIdxD <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
        output$modelingIdxD <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
        hideAll$clearAll <- TRUE
      }

      hideAll$clearAll <- FALSE

    }) ## end eventReactive

    output$outIdxD <- renderPrint({
      outIdxD()
    })


  })
}

## To be copied in the UI
# mod_indexDesireApp_ui("indexDesireApp_1")

## To be copied in the server
# mod_indexDesireApp_server("indexDesireApp_1")

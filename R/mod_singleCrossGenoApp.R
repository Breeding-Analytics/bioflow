#' singleCrossGenoApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_singleCrossGenoApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::mainPanel(width = 12,
                     tabsetPanel(id=ns("tabsMain"), #width=9,
                                 type = "tabs",

                                 tabPanel( div(icon("book"), "Information") ,
                                           tags$body(
                                             br(),
                                             column(width = 6,
                                                    h1(strong(span("Single Cross Marker Matrix Building Module", style="color:darkcyan"))),
                                                    h2(strong("Data Status (wait to be displayed):")),
                                                    uiOutput(ns("warningMessage")),
                                                    tags$br(),
                                                    # column(width=4, tags$br(),
                                                    shinyWidgets::prettySwitch( inputId = ns('launch'), label = "Load example dataset", status = "success"),
                                                    # ),
                                             ),

                                             column(width = 6,
                                                    h2(strong("Details")),
                                                    p("Hybrid breeding based on single-cross of lines allows the genotyping of only the parental lines and the consequent
                                                               formation of single-cross marker matrices. The idea is that the user provides the marker information from the parental
                                                               lines and has uploaded a pedigree of the phenotypic dataset. The marker profiles of the possible cross combinations
                                                               will be computed based on the availability of the marker information. Some of the parameters required are:"),
                                                    # img(src = "www/qaRaw.png", height = 300, width = 600), # add an image
                                                    p(strong("Batch size to compute-")," the number of hybrids to build in each batch. Building all hybrids at once can be computationally
                                                               intensive and unnecesary. The default value is 1000 hybrids per batch."),
                                                    p(strong("Compute all possible hybrids?-")," an indication to know if only the marker-profiles for the hybrids present in the pedigree dataset should be computed or
                                                               marker-profiles for all cross combinations should be created. This should be used carefully when the number of males and females in the pedigree file is big."),
                                                    h4(strong("Example")),
                                                    img(src = "www/scm.png",width = 500), # add an image
                                                    h2(strong("References")),
                                                    p("Nishio M and Satoh M. 2014. Including Dominance Effects in the Genomic BLUP Method for Genomic Evaluation. Plos One 9(1), doi:10.1371/journal.pone.0085792"),
                                                    p("Su G, Christensen OF, Ostersen T, Henryon M, Lund MS. 2012. Estimating Additive and Non-Additive Genetic Variances and Predicting Genetic Merits Using Genome-Wide Dense Single Nucleotide Polymorphism Markers. PLoS ONE 7(9): e45293. doi:10.1371/journal.pone.0045293"),
                                                    h2(strong("Software used")),
                                                    p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
                                Vienna, Austria. URL https://www.R-project.org/."),
                                                    p("Covarrubias-Pazaran G. 2016. Genome assisted prediction of quantitative traits using the R package sommer. PLoS ONE 11(6):1-15."),
                                             ),
                                           )
                                 ),
                                 tabPanel(div(icon("arrow-right-to-bracket"), "Input steps"),
                                          tabsetPanel(
                                            tabPanel(div( icon("dice-one"), "Pick QA-stamp", icon("arrow-right") ) , # icon = icon("dice-one"),
                                                     br(),
                                                     column(width=12,style = "background-color:grey; color: #FFFFFF",
                                                            column(width=8, selectInput(ns("version2Scm"), "QA stamp to apply to markers", choices = NULL, multiple = FALSE) ),

                                                     ),
                                                     column(width=12),
                                                     shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                         column(width=12,
                                                                                hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                                h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameters to be specified in the grey boxes above.", style="color:green"))),
                                                                                hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                         ),
                                                                         column( width=12, shiny::plotOutput(ns("plotTimeStamps")) ),
                                                                         DT::DTOutput(ns("statusScm")), # modeling table
                                                                         DT::DTOutput(ns("genoScm")), # data input
                                                     ),
                                            ),
                                            tabPanel(div( icon("dice-two"), "Pick batch size", icon("arrow-right") ), # icon = icon("dice-two"),
                                                     br(),
                                                     column(width=12, numericInput(ns("hybridBatch"), label = "Batch size to compute", value = 1000, min=1, max=10000, step=1000), style = "background-color:grey; color: #FFFFFF"),
                                                     column(width=12),
                                                     shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                         column(width=12,
                                                                                hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                                h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameters to be specified in the grey boxes above.", style="color:green"))),
                                                                                hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                         ),
                                                                         DT::DTOutput(ns("summariesScr")),
                                                     ),
                                            ),
                                            tabPanel( div(icon("dice-three"), "Pick crosses", icon("arrow-right") ), # icon = icon("dice-three"),
                                                      br(),
                                                      column(width=12, selectInput(ns("checkboxAllHybrids"), label = "Compute all possible hybrids?", choices = list(TRUE,FALSE), selected = FALSE, multiple=FALSE), style = "background-color:grey; color: #FFFFFF"),
                                                      column(width=12),
                                                      shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                          column(width=12,
                                                                                 hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                                 h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameters to be specified in the grey boxes above.", style="color:green"))),
                                                                                 hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                          ),
                                                                          column(width = 6, sliderInput(ns("slider1"), label = "Number of mothers", min = 1, max = 500, value = c(1, 15))  ),
                                                                          column(width = 6, sliderInput(ns("slider2"), label = "Number of fathers", min = 1, max = 500, value = c(1, 15))  ),
                                                                          column(width=6, shiny::plotOutput(ns("plotPossibleCrosses")),style = "height:460px; overflow-y: scroll;overflow-x: scroll;" ),
                                                                          column(width=6, shiny::plotOutput(ns("plotPossibleProfiles")),style = "height:460px; overflow-y: scroll;overflow-x: scroll;" )
                                                      ),
                                            ),
                                            tabPanel("Run analysis", icon = icon("dice-four"),
                                                     br(),
                                                     column(width=12,style = "background-color:grey; color: #FFFFFF",
                                                            column(width=3, tags$div(textInput(ns("analysisIdName"), label = tags$span(
                                                              "Analysis Name (optional)", tags$i( class = "glyphicon glyphicon-info-sign", style = "color:#FFFFFF",
                                                                                                  title = "An optional name for the analysis besides the timestamp if desired.") ), #width = "100%",
                                                              placeholder = "(optional name)") ) ),
                                                            column(width=3, br(),
                                                                   actionButton(ns("runScr"), "Build matrix", icon = icon("play-circle")),
                                                                   textOutput(ns("outScr")),
                                                                   br(),
                                                            ),
                                                     )

                                            ),
                                          ), # end of tabset
                                 ),# end of output panel
                                 tabPanel(div(icon("arrow-right-from-bracket"), "Output tabs" ) , value = "outputTabs",
                                          tabsetPanel(
                                            tabPanel("Data", icon = icon("table"),
                                                     br(),
                                                     downloadButton(ns("downloadDataScm"), "Download Genotypic data"),
                                                     br(),
                                                     DT::DTOutput(ns("dataScm")),
                                            ),
                                          ) # of of tabsetPanel
                                 )# end of output panel
                     )) # end mainpanel


  )
}

#' singleCrossGenoApp Server Functions
#'
#' @noRd
mod_singleCrossGenoApp_server <- function(id, data){
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
        HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your phenotypic data using the 'Data Retrieval' tab. We will need it to know which single crosses from all the possible ones should be computed.")) )
      }else{ # data is there
        ## pheno check
        if( length(which(c("designation") %in% data()$metadata$pheno$parameter)) == 0 ){
          HTML( as.character(div(style="color: red; font-size: 20px;", "Please map your 'designation' column using the 'Data Retrieval' tab in the 'Phenotype' section. We need this information to know which hybrids have been tested.")) )
        }else{
          ## ped check
          ped <- data()$data$pedigree
          metaPed <- data()$metadata$pedigree
          if(is.null(ped)){ # no pedigree available
            HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your pedigree data using the 'Data Retrieval' tab under 'Pedigree' section. We need this information to know which hybrid combinations are available and which ones need to be built.")) )
          }else{ # pedigree is there
            if( length(intersect(metaPed$value , colnames(ped))) != 3){
              HTML( as.character(div(style="color: red; font-size: 20px;", "Please map your 'designation', 'mother', and 'father' columns when retrieving the pedigree data. We need this information to know which hybrid combinations are available and which ones need to be built.")) )
            }else{
              colnames(ped) <- cgiarBase::replaceValues(colnames(ped), Search = metaPed$value, Replace = metaPed$parameter )
              parents <- na.omit(c(unique(ped[,"mother"]), unique(ped[,"father"]) ))
              if(length(parents) == 0){
                HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your pedigree data using the 'Data Retrieval' tab. No parents detected. We need this information to know which hybrid combinations are available and which ones need to be built.")) )
              }else{
                ## marker check
                geno <- data()$data$geno
                if(is.null(geno)){ # no markers available
                  HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your marker data using the 'Data Retrieval' tab.")) )
                }else{ # markers are there
                  if("qaGeno" %in% data()$status$module){
                    HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to perform your analysis.")) )
                  }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please run the 'Markers QA/QC' module before performing your analysis.")) ) }
                } # end of if pedigree available
              } #
            }
          } # end of if pedigree available
        }
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
        utils::data(SCM_example, package = "cgiarPipeline")
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
    # QA versions to use
    observeEvent(c(data()), {
      req(data())
      dtScm <- data()
      dtScm <- dtScm$status
      dtScm <- dtScm[which(dtScm$module %in% "qaGeno"),]
      traitsScm <- unique(dtScm$analysisId)
      if(length(traitsScm) > 0){names(traitsScm) <- as.POSIXct(traitsScm, origin="1970-01-01", tz="GMT")}
      updateSelectInput(session, "version2Scm", choices = traitsScm)
    })
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
    ## render the data to be analyzed
    observeEvent(data(),{
      if(sum(data()$status$module %in% "qaGeno") != 0) {
        ## render status
        output$statusScm <-  DT::renderDT({
          req(data())
          req(input$version2Scm)
          dtScm <- data()
          ### change column names for mapping
          paramsGeno <- data()$modifications$geno
          paramsGeno <- paramsGeno[which(paramsGeno$analysisId %in% input$version2Scm),, drop=FALSE]
          paramsGeno$analysisId <- as.POSIXct(paramsGeno$analysisId, origin="1970-01-01", tz="GMT")
          DT::datatable(paramsGeno, extensions = 'Buttons',
                        options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                       lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All'))),
                        caption = htmltools::tags$caption(
                          style = 'color:cadetblue', #caption-side: bottom; text-align: center;
                          htmltools::em('Modifications from QA stamp selected.')
                        )
          )
        }, server = FALSE)
        ## render raw data
        output$genoScm <-  DT::renderDT({
          req(data())
          dtScm <- data()
          req(input$version2Scm)
          dtScm <- dtScm$data$geno
          ### change column names for mapping
          paramsGeno <- data()$metadata$geno
          colnames(dtScm) <- cgiarBase::replaceValues(colnames(dtScm), Search = paramsGeno$value, Replace = paramsGeno$parameter )
          ###
          traitTypes <- unlist(lapply(dtScm,class))
          numeric.output <- names(traitTypes)[which(traitTypes %in% "numeric")]
          DT::formatRound(DT::datatable(dtScm[,1:min(c(50,ncol(dtScm)))],
                                        extensions = 'Buttons',
                                        options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                       lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All'))),
                                        caption = htmltools::tags$caption(
                                          style = 'color:cadetblue', #caption-side: bottom; text-align: center;
                                          htmltools::em('Raw genotypic data to be used as input.')
                                        )
          ), numeric.output)
        }, server = FALSE)
      } else {
        output$genoScm <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
      }
    })
    ## render the expected result
    output$plotPossibleCrosses <- shiny::renderPlot({
      req(data())
      if(!is.null(data()$data$pheno) & !is.null(data()$data$geno) & !is.null(data()$data$pedigree) ){
        ped <- data()$data$pedigree
        metaPed <- data()$metadata$pedigree
        colnames(ped) <- cgiarBase::replaceValues(colnames(ped), Search = metaPed$value, Replace = metaPed$parameter )
        cross <- unique(ped[,c("designation","mother","father")])
        cross$motherN <- as.numeric(as.factor(cross$mother))
        cross$fatherN <- as.numeric(as.factor(cross$father))
        # converts a data.frame into a matrix
        A <- matrix(NA, nrow=max(cross$motherN, na.rm=TRUE), ncol = max(cross$fatherN, na.rm=TRUE))
        A[as.matrix(cross[,c("motherN","fatherN")])] = 1
        rownames(A) <-  levels(as.factor(cross$mother))
        colnames(A) <- levels(as.factor(cross$father))
        A <- A[,(input$slider2[1]):min(c(input$slider2[2], ncol(A) )), drop=FALSE] # environments
        A <- A[(input$slider1[1]):min(c(input$slider1[2]), nrow(A) ), ,drop=FALSE] # genotypes
        Matrix::image(as(A, Class = "dgCMatrix"), xlab="Fathers", ylab="Mothers", colorkey=FALSE, main="Available hybrids")
      }
    })
    output$plotPossibleProfiles <- shiny::renderPlot({
      req(data())
      if(!is.null(data()$data$pheno) & !is.null(data()$data$geno) & !is.null(data()$data$pedigree) ){
        ped <- data()$data$pedigree
        metaPed <- data()$metadata$pedigree
        colnames(ped) <- cgiarBase::replaceValues(colnames(ped), Search = metaPed$value, Replace = metaPed$parameter )
        cross <- unique(ped[,c("designation","mother","father")])
        # subset to crosses that can be built
        possible <- which( (cross$mother %in% rownames(data()$data$geno)) & (cross$father %in% rownames(data()$data$geno))  )
        if(length(possible) > 0){
          cross <- cross[possible, ]
        }
        ##
        cross$motherN <- as.numeric(as.factor(cross$mother))
        cross$fatherN <- as.numeric(as.factor(cross$father))
        # converts a data.frame into a matrix
        A <- matrix(0, nrow=max(cross$motherN, na.rm=TRUE), ncol = max(cross$fatherN, na.rm=TRUE))
        if(length(possible) > 0){
          A[as.matrix(cross[,c("motherN","fatherN")])] = 2
        }
        rownames(A) <-  levels(as.factor(cross$mother))
        colnames(A) <- levels(as.factor(cross$father))
        A <- A[,(input$slider2[1]):min(c(input$slider2[2], ncol(A) )), drop=FALSE] # environments
        A <- A[(input$slider1[1]):min(c(input$slider1[2]), nrow(A) ), ,drop=FALSE] # genotypes
        Matrix::image(as(A, Class = "dgCMatrix"), xlab="Fathers", ylab="Mothers", colorkey=FALSE, main="Possible to build")
      }
    })

    ## display the current outliers
    observeEvent(data(),{

      output$summariesScr <-  DT::renderDT({

        req(data())
        if(!is.null(data()$data$pheno) & !is.null(data()$data$geno) & !is.null(data()$data$pedigree) ){
          ped <- data()$data$pedigree
          metaPed <- data()$metadata$pedigree
          colnames(ped) <- cgiarBase::replaceValues(colnames(ped), Search = metaPed$value, Replace = metaPed$parameter )
          phe <- data()$data$pheno
          metaPhe <- data()$metadata$pheno
          metaPhe <- metaPhe[metaPhe$parameter != "trait",]
          colnames(phe) <- cgiarBase::replaceValues(colnames(phe), Search = metaPhe$value, Replace = metaPhe$parameter )
          gen <- data()$data$geno

          pheped <- merge(phe,ped, by="designation", all.x=TRUE)
          mothers <- unique(pheped[,"mother"])
          fathers <- unique(pheped[,"father"])
          designation <- unique(pheped[,"designation"])
          mothersG <- intersect(mothers, rownames(gen))
          fathersG <- intersect(fathers, rownames(gen))
          designationG <- intersect(designation, rownames(gen))
          final <- data.frame(Metric=c("Mother","Father","Designation"),
                              Phenotyped=c(length(mothers), length(fathers), length(designation)),
                              Genotyped=c(length(mothersG), length(fathersG), length(designationG))
          )
          DT::datatable(final, extensions = 'Buttons',
                        options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                       lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
          )
        }

      }, server = FALSE)

    })

    ## save when user clicks

    outScr <- eventReactive(input$runScr, {

      if(is.null(data())){
        cst("Please retrieve or load your phenotypic data using the 'Data Retrieval' tab.")
      }else{ # data is there
        ## pheno check
        if( length(which(c("designation") %in% data()$metadata$pheno$parameter)) == 0 ){
          cat("Please map your 'designation' column using the 'Data Retrieval' tab in the 'Phenotype' section.")
        }else{
          ## ped check
          ped <- data()$data$pedigree
          metaPed <- data()$metadata$pedigree
          if(is.null(ped)){ # no pedigree available
            cat("Please retrieve or load your pedigree data using the 'Data Retrieval' tab under 'Pedigree' section.")
          }else{ # pedigree is there
            if( length(intersect(metaPed$value , colnames(ped))) != 3){
              cat("Please map your 'designation', 'mother', and 'father' columns when retrieving the pedigree data.")
            }else{
              colnames(ped) <- cgiarBase::replaceValues(colnames(ped), Search = metaPed$value, Replace = metaPed$parameter )
              parents <- na.omit(c(unique(ped[,"mother"]), unique(ped[,"father"]) ))
              if(length(parents) == 0){
                cat("Please retrieve or load your pedigree data using the 'Data Retrieval' tab. No parents detected.")
              }else{
                ## marker check
                geno <- data()$data$geno
                if(is.null(geno)){ # no markers available
                  cat("Please retrieve or load your marker data using the 'Data Retrieval' tab.")
                }else{ # markers are there
                  shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
                  myObject <- data()
                  result <- try(cgiarPipeline::singleCrossMat( # single cross matrix function
                    object= myObject,
                    analysisIdForGenoModifications= input$version2Scm,
                    hybridBatch=input$hybridBatch,
                    allHybrids=input$checkboxAllHybrids,
                    verbose=FALSE
                  ),
                  silent=TRUE
                  )
                  if(!inherits(result,"try-error")) {
                    if("analysisIdName" %in% colnames(result$status)){result$status$analysisIdName[nrow(result$status)] <- input$analysisIdName}
                    data(result) # update data with results
                    cat(paste("Single cross marker matrix building with id:",as.POSIXct(result$status$analysisId[length(result$status$analysisId)], origin="1970-01-01", tz="GMT"),"saved."))
                    updateTabsetPanel(session, "tabsMain", selected = "outputTabs")
                  }else{
                    cat(paste("Analysis failed with the following error message: \n\n",result[[1]]))
                  }
                  shinybusy::remove_modal_spinner()
                } # end of if pedigree available
              } #
            }
          } # end of if pedigree available
        }
      }

    })
    output$outScr <- renderPrint({
      outScr()
    })

    ## output scm
    output$dataScm <- DT::renderDT({
      req(data())
      dtScm <- data()$data$geno

      dtPed <- data()$data$pedigree
      metaPed <- data()$metadata$pedigree
      colnames(dtPed) <- cgiarBase::replaceValues(colnames(dtPed), Search = metaPed$value, Replace = metaPed$parameter)

      dtScm <- dtScm[setdiff(rownames(dtScm),unique(c(dtPed$mother,dtPed$father))),]

      DT::datatable(dtScm[order(rownames(dtScm)),1:min(c(50,ncol(dtScm)))],
                    extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All'))),
                    caption = htmltools::tags$caption(
                      style = 'color:cadetblue', #caption-side: bottom; text-align: center;
                      htmltools::em('Market Profile Computed for Hybrids (showing only first 50 Markers). Click Download button to get complete data.')
                    ))
    }, server = FALSE)

    output$downloadDataScm <- downloadHandler(
      filename = function() {
        paste('scm_geno_data', sep = '.', "csv")
      },
      content = function(file) {
        shinybusy::show_modal_spinner(spin = "fading-circle",
                                      text = "Downloading data...")
        req(data())
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        out <- data()$data$geno
        utils::write.csv(out, file, row.names = TRUE)
        shinybusy::remove_modal_spinner()
      }, contentType = "text/csv"
    )

  })
}

## To be copied in the UI
# mod_singleCrossGenoApp_ui("singleCrossGenoApp_1")

## To be copied in the server
# mod_singleCrossGenoApp_server("singleCrossGenoApp_1")

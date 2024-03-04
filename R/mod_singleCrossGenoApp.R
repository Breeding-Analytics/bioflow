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
                     tabsetPanel( #width=9,
                       type = "tabs",

                       tabPanel( div(icon("book"), "Information-SCM") ,
                                br(),
                                shinydashboard::box(status="success",width = 12,
                                                    solidHeader = TRUE,
                                                    column(width=12,   style = "height:580px; overflow-y: scroll;overflow-x: scroll;",
                                                           h1(strong(span("Single Cross Marker Matrix Build", style="color:green"))),
                                                           h2(strong("Status:")),
                                                           uiOutput(ns("warningMessage")),
                                                           tags$body(
                                                             h2(strong("Details")),
                                                             p("Hybrid breeding based on single-cross of lines allows the genotyping of only the parental lines and the consequent
                                                               formation of single-cross marker matrices. The idea is that the user provides the marker information from the parental
                                                               lines and has uploaded a pedigree of the phenotypic dataset. The marker profiles of the possible cross combinations
                                                               will be computed based on the availability of the marker information. Some of the parameters required are:"),
                                                             # img(src = "www/qaRaw.png", height = 300, width = 600), # add an image
                                                             p(strong("Batch size to compute-")," the number of hybrids to build in each batch. Building all hybrids at once can be computationally
                                                               intensive and unnecesary. The default value is 1000 hybrids per batch."),
                                                             p(strong("Compute all possible hybrids?-")," an indication to know if only the marker-profiles for the hybrids present in the phenotypic dataset should be computed or
                                                               marker-profiles for all cross combinations should be created. This should be used carefully when the number of males and females in the pedigree file is big."),
                                                             h2(strong("References")),
                                                             p("Nishio M and Satoh M. 2014. Including Dominance Effects in the Genomic BLUP Method for Genomic Evaluation. Plos One 9(1), doi:10.1371/journal.pone.0085792"),
                                                             p("Su G, Christensen OF, Ostersen T, Henryon M, Lund MS. 2012. Estimating Additive and Non-Additive Genetic Variances and Predicting Genetic Merits Using Genome-Wide Dense Single Nucleotide Polymorphism Markers. PLoS ONE 7(9): e45293. doi:10.1371/journal.pone.0045293"),
                                                             h2(strong("Software")),
                                                             p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
                                Vienna, Austria. URL https://www.R-project.org/."),
                                                             p("Covarrubias-Pazaran G. 2016. Genome assisted prediction of quantitative traits using the R package sommer. PLoS ONE 11(6):1-15."),
                                                           )
                                                    )
                                )
                       ),
                       tabPanel(div(icon("arrow-right-to-bracket"), "Input"),
                                tabsetPanel(
                                  tabPanel("Pick batch size", icon = icon("magnifying-glass-chart"),
                                           br(),
                                           column(width=12, numericInput(ns("hybridBatch"), label = "Batch size to compute", value = 1000, min=1, max=10000, step=1000), style = "background-color:grey; color: #FFFFFF"),
                                           h4(strong(span("Visualizations below aim to help you pick the right parameter values. Please inspect them.", style="color:green"))),
                                           hr(style = "border-top: 3px solid #4c4c4c;"),
                                           shinydashboard::box(status="success",width = 12, solidHeader = TRUE, #background = "green",
                                                               column(width=12, style = "height:530px; overflow-y: scroll;overflow-x: scroll;",
                                                                      DT::DTOutput(ns("summariesScr")),
                                                                      )
                                           )
                                  ),
                                  tabPanel("Pick crosses", icon = icon("magnifying-glass-chart"),
                                           br(),
                                           column(width=12, selectInput(ns("checkboxAllHybrids"), label = "Compute all possible hybrids?", choices = list(TRUE,FALSE), selected = FALSE, multiple=FALSE), style = "background-color:grey; color: #FFFFFF"),
                                           h4(strong(span("Visualizations below aim to help you pick the right parameter values. Please inspect them.", style="color:green"))),
                                           hr(style = "border-top: 3px solid #4c4c4c;"),
                                           shinydashboard::box(status="success",width = 12,  solidHeader = TRUE, #background = "green",
                                                               column(width = 6, sliderInput(ns("slider1"), label = "Number of mothers", min = 1, max = 500, value = c(1, 15))  ),
                                                               column(width = 6, sliderInput(ns("slider2"), label = "Number of fathers", min = 1, max = 500, value = c(1, 15))  ),
                                                               column(width=6, shiny::plotOutput(ns("plotPossibleCrosses")),style = "height:460px; overflow-y: scroll;overflow-x: scroll;" ),
                                                               column(width=6, shiny::plotOutput(ns("plotPossibleProfiles")),style = "height:460px; overflow-y: scroll;overflow-x: scroll;" )
                                           )
                                  ),
                                  tabPanel("Run analysis", icon = icon("play"),
                                           br(),
                                           actionButton(ns("runScr"), "Build matrix", icon = icon("play-circle")),
                                           textOutput(ns("outScr"))
                                  ),
                                ), # end of tabset
                       ),# end of output panel
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
          HTML( as.character(div(style="color: red; font-size: 20px;", "Please map your 'designation' column using the 'Data Retrieval' tab in the 'Phenotype' section.")) )
        }else{
          ## ped check
          ped <- data()$data$pedigree
          metaPed <- data()$metadata$pedigree
          if(is.null(ped)){ # no pedigree available
            HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your pedigree data using the 'Data Retrieval' tab under 'Pedigree' section.")) )
          }else{ # pedigree is there
            if( length(intersect(metaPed$value , colnames(ped))) != 3){
              HTML( as.character(div(style="color: red; font-size: 20px;", "Please map your 'designation', 'mother', and 'father' columns when retrieving the pedigree data.")) )
            }else{
              colnames(ped) <- cgiarBase::replaceValues(colnames(ped), Search = metaPed$value, Replace = metaPed$parameter )
              parents <- na.omit(c(unique(ped[,"mother"]), unique(ped[,"father"]) ))
              if(length(parents) == 0){
                HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your pedigree data using the 'Data Retrieval' tab. No parents detected.")) )
              }else{
                ## marker check
                geno <- data()$data$geno
                if(is.null(geno)){ # no markers available
                  HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your marker data using the 'Data Retrieval' tab.")) )
                }else{ # markers are there
                  HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to inspect the options.")) )
                } # end of if pedigree available
              } #
            }
          } # end of if pedigree available
        }
      }
    )
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

      })

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
                    hybridBatch=input$hybridBatch,
                    allHybrids=input$checkboxAllHybrids,
                    verbose=FALSE
                  ),
                  silent=TRUE
                  )
                  if(!inherits(result,"try-error")) {
                    data(result) # update data with results
                    cat(paste("Single cross marker matrix building with id:",as.POSIXct(result$status$analysisId[length(result$status$analysisId)], origin="1970-01-01", tz="GMT"),"saved."))
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



  })
}

## To be copied in the UI
# mod_singleCrossGenoApp_ui("singleCrossGenoApp_1")

## To be copied in the server
# mod_singleCrossGenoApp_server("singleCrossGenoApp_1")

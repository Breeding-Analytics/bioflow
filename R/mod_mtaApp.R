#' mtaApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mtaApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    sidebarPanel(

      tags$style(".well {background-color:grey; color: #FFFFFF;}"),
      HTML("<img src='www/cgiar3.png' width='42' vspace='10' hspace='10' height='46' align='top'>
                  <font size='5'>Multi Trial Analysis</font>"),
      # div(tags$p( h4(strong("Multi Trial Analysis")))),#, style = "color: #817e7e"
      hr(style = "border-top: 1px solid #4c4c4c;"),
      selectInput(ns("version2Mta"), "STA version to analyze", choices = NULL, multiple = FALSE),
      selectInput(ns("trait2Mta"), "Trait(s) to analyze", choices = NULL, multiple = TRUE),
      selectInput(ns("fixedTermMta2"), "Fixed effect(s)", choices = NULL, multiple = TRUE),
      selectInput(ns("randomTermMta2"), "Random effect(s)", choices = NULL, multiple = TRUE),
      selectInput(ns("interactionTermMta2"), "GxE term(s)", choices = NULL, multiple = TRUE),
      selectInput(ns("modelMet"), label = "Evaluation method", choices = list(BLUP="blup",pBLUP="pblup",gBLUP="gblup",ssGBLUP="ssgblup",rrBLUP="rrblup"), selected = "blup", multiple=FALSE),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      shinydashboard::box(width = 12, status = "success", background="green",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Fields to include...",
                          column(width = 12,DT::dataTableOutput(ns("fieldsMet")), style = "height:400px; overflow-y: scroll;overflow-x: scroll;")
      ),
      shinydashboard::box(width = 12, status = "success", background="green",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Trait distributions...",
                          column(width = 12,DT::DTOutput(ns("traitDistMet")), style = "height:400px; overflow-y: scroll;overflow-x: scroll;")
      ),
      shinydashboard::box(width = 12, status = "success", background="green",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Settings...",
                          selectInput(ns("deregressMet"), label = "Deregress Predictions?",  choices = list(TRUE,FALSE), selected = FALSE, multiple=FALSE),
                          textInput(ns("heritLBMet"), label = "Lower H2&R2 bound per trait (separate by commas) or single value across", value="0.2"),
                          textInput(ns("heritUBMet"), label = "Upper H2&R2 bound per trait (separate by commas) or single value across", value="0.95"),
                          numericInput(ns("maxitMet"), label = "Number of iterations", value = 70),
                          selectInput(ns("scaledDesireMet"), label = "Scale desire file", choices = list(TRUE,FALSE), selected = TRUE, multiple=FALSE),
                          selectInput(ns("useWeights"), label = "Use weights?", choices = list(TRUE,FALSE), selected = TRUE, multiple=FALSE),
                          numericInput(ns("nPC"), label = "Number of PCs if method is rrBLUP", value = 0)
      ),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      actionButton(ns("runMta"), "Run", icon = icon("play-circle")),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      uiOutput(ns("qaQcMtaInfo")),
      textOutput(ns("outMta")),
      hr(style = "border-top: 1px solid #4c4c4c;")
    ), # end sidebarpanel
    mainPanel(tabsetPanel(
      type = "tabs",
      tabPanel(p("Information",class="info-p"),  icon = icon("book"),
               br(),
               shinydashboard::box(status="success",width = 12,
                                   solidHeader = TRUE,
                                   column(width=12,   style = "height:800px; overflow-y: scroll;overflow-x: scroll;",
                                          h2(strong("Status:")),
                                          uiOutput(ns("warningMessage")),
                                          h2(strong("Details")),
                                          p("The core algorithm of the genetic evaluation using the two-step approach is the multi-trial analysis.
                                          This option aims to model breeding values across environments using the results from the single trial (weighted by the standard errors)
                              analysis and optionally a relationship matrix between individuals.
                                The way the arguments are used is the following:"),
                                          img(src = "www/mta.png", height = 300, width = 600), # add an image
                                          p(strong("Model type (optional).-"),"One of the following models to use: BLUP, PBLUP, GBLUP, rrBLUP, ssBLUP."),
                                          p(strong("Traits to analyze.-")," Traits to be analyzed. If no design factors can be fitted simple means are taken."),
                                          p(strong("Fixed effects.-")," Columns to be fitted as fixed effects."),
                                          p(strong("Random effects.-")," Columns to be fitted as random effects."),
                                          p(strong("Residuals by.-")," Column to decide at which level the residuals should be fitted by."),
                                          p(strong("Interactions to fit with genotype.-")," Column to fit as interactions with the genotype effect."),
                                          p(strong("Deregress.-")," A TRUE/FALSE value to decide is the STA predictions should be deregressed. Only to be used if you fitted BLUPs in the STA step."),
                                          p(strong("H2(lower bound).-")," Value of H2 to be used to remove trials with low heritability."),
                                          p(strong("H2(upper bound).-"),"  Value of H2 to be used to remove trials with too high heritability."),
                                          p(strong("Number of iterations.-")," Maximum number of restricted maximum likelihood iterations to be run for each trait."),
                                          p(strong("Scale desire file.-")," A TRUE or FALSE value to decide if the DESIRE (software) input file to be created should assume scaled traits or in their original units (scaled is recommended)."),
                                          p(strong("Use rrBLUP method.-")," A TRUE or FALSE value to decide if the MET should use the rrBLUP approach using either a certain number of principal components or the full matrix using the nPC argument."),
                                          p(strong("nPC.-")," Number of principal components for the big MET. If the value is equal to 0 the classical rrBLUP model is used. Otherwise a principal component model is run according to Odegard et al. (2019)."),
                                          p(strong("Some details.-")," If genotypes are fitted as fixed effects the reliability cannot be properly computed and the surrogate of genetic variance is the variance of BLUEs. If STA predictions were BLUPs the deregress option can be used."),
                                          h2(strong("References:")),
                                          p("Finlay, K. W., & Wilkinson, G. N. (1963). The analysis of adaptation in a plant-breeding programme. Australian journal of agricultural research, 14(6), 742-754."),
                                          p("Henderson Jr, C. R. (1982). Analysis of covariance in the mixed model: higher-level, nonhomogeneous, and random regressions. Biometrics, 623-640."),
                                          p("Odegard, J., Indahl, U., Stranden, I., & Meuwissen, T. H. (2018). Large-scale genomic prediction using singular value decomposition of the genotype matrix. Genetics Selection Evolution, 50(1), 1-12."),
                                          h2(strong("Software used:")),
                                          p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
                                Vienna, Austria. URL https://www.R-project.org/."),
                                          p(" Boer M, van Rossum B (2022). _LMMsolver: Linear Mixed Model Solver_. R package version 1.0.4.9000."),
                                          p("Covarrubias-Pazaran G. 2016. Genome assisted prediction of quantitative traits using the R package sommer. PLoS ONE 11(6):1-15.")
                                          # img(src = "www/met.png", height = 400, width = 700) # add an image
                                   )
               )
      ),
      tabPanel(p("Input",class="input-p"), icon = icon("arrow-right-to-bracket"),
               tabsetPanel(
                 tabPanel("Sta-metrics", icon = icon("table"),
                          br(),
                          shinydashboard::box(status="success",width = 12,solidHeader = TRUE,
                                              column(width=6,selectInput(ns("traitMetrics"), "Trait to visualize", choices = NULL, multiple = TRUE) ),
                                              column(width=6,selectInput(ns("parameterMetrics"), "Parameter to visualize", choices = NULL, multiple = FALSE) ),
                                              column(width=12, plotly::plotlyOutput(ns("barplotPredictionsMetrics")) )
                          )
                 ),
                 tabPanel("Sta-modeling", icon = icon("table"),
                          br(),
                          shinydashboard::box(status="success",width = 12,
                                              solidHeader = TRUE,
                                              column(width=12,DT::DTOutput(ns("statusMta")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
                          )
                 ),
                 tabPanel("Connectivity", icon = icon("table"),
                          br(),
                          shinydashboard::box(status="success",width = 12,solidHeader = TRUE,
                                              column(width=12,selectInput(ns("entryTypeMta"), "Entry type to visualize", choices = NULL, multiple = TRUE) ),
                                              column(width = 6, checkboxGroupInput(ns("checkboxText"), label = "", choices = list("Add connectivity labels?" = TRUE), selected = FALSE) ),
                                              column(width = 6, checkboxGroupInput(ns("checkboxAxis"), label = "", choices = list("Add axis labels?" = TRUE), selected = FALSE) ),
                                              column(width=12, plotly::plotlyOutput(ns("plotPredictionsConnectivity")) )
                          )
                 ),
                 tabPanel("Sparsity", icon = icon("table"),
                          br(),
                          shinydashboard::box(status="success",width = 12,solidHeader = TRUE,
                                              column(width = 6, sliderInput(ns("slider1"), label = "Number of genotypes", min = 1, max = 2000, value = c(1, 2000))  ),
                                              column(width = 6, sliderInput(ns("slider2"), label = "Number of environments", min = 1, max = 500, value = c(1, 500))  ),
                                              column(width=12, shiny::plotOutput(ns("plotPredictionsSparsity")) )
                          )
                 ),
                 tabPanel("Trait dispersal", icon = icon("magnifying-glass-chart"),
                          br(),
                          shinydashboard::box(status="success",width = 12, solidHeader = TRUE,
                                              column(width=6, selectInput(ns("trait3Mta"), "Trait to visualize", choices = NULL, multiple = FALSE) ),
                                              column(width=6, selectInput(ns("groupMtaInputPlot"), "Group by", choices = c("environment","designation","entryType"), multiple = FALSE, selected = "environment") ),
                                              column(width=12, plotly::plotlyOutput(ns("plotPredictionsCleanOut")))
                          )
                 ),
                 tabPanel("Data", icon = icon("table"),
                          br(),
                          shinydashboard::box(status="success",width = 12, solidHeader = TRUE,
                                              column(width=12,
                                                     DT::DTOutput(ns("phenoMta")),
                                                     style = "height:800px; overflow-y: scroll;overflow-x: scroll;"
                                              )
                          )
                 )
               )
      ),
      tabPanel(p("Output",class="output-p"), icon = icon("arrow-right-from-bracket"),
               tabsetPanel(
                 tabPanel("Predictions", icon = icon("table"),
                          br(),
                          shinydashboard::box(status="success",width = 12,
                                              solidHeader = TRUE,
                                              column(width=12,DT::DTOutput(ns("predictionsMta")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
                          )
                 ),
                 tabPanel("Metrics", icon = icon("table"),
                          br(),
                          shinydashboard::box(status="success",width = 12,
                                              solidHeader = TRUE,
                                              column(width=12,br(),DT::DTOutput(ns("metricsMta")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
                          )
                 ),
                 tabPanel("Modeling", icon = icon("table"),
                          br(),
                          shinydashboard::box(status="success",width = 12,
                                              solidHeader = TRUE,
                                              column(width=12,DT::DTOutput(ns("modelingMta")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
                          )
                 ),
                 tabPanel("Report", icon = icon("file-image"),
                          br(),
                          div(tags$p("Please download the report below:") ),
                          downloadButton(ns("downloadReportMta"), "Download report"),
                          br(),
                          uiOutput(ns('reportMta'))
                 )
               ) # end of tabset
      )# end of output panel
    )) # end mainpanel


  )
}

#' mtaApp Server Functions
#'
#' @noRd
mod_mtaApp_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })
    ############################################################################
    # data = reactive({ # provisional dataset for testing
    #   load(file.path(getwd(),"R/outputs/resultMta.RData"))
    #   data <- resultMta
    #   return(data)
    # })
    # warning message
    output$warningMessage <- renderUI(
      if(is.null(data())){
        HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your phenotypic data using the 'Data Retrieval' tab.")) )
      }else{ # data is there
        mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
        if(mappedColumns == 3){
          if("sta" %in% data()$status$module){
            HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to perform the MTA inspecting the other tabs.")) )
          }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please perform STA before performing an MTA.")) ) }
        }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please make sure that the columns: 'environment', 'designation' and \n at least one trait have been mapped using the 'Data Retrieval' tab.")) )}
      }
    )
    #################
    ## version
    observeEvent(c(data()), {
      req(data())
      dtMta <- data()
      dtMta <- dtMta$status
      dtMta <- dtMta[which(dtMta$module == "sta"),]
      traitsMta <- unique(dtMta$analysisId)
      updateSelectInput(session, "version2Mta", choices = traitsMta)
    })
    # ## version qa marker
    # observeEvent(c(data()), {
    #   req(data())
    #   dtMta <- data()
    #   dtMta <- dtMta$status
    #   dtMta <- dtMta[which(dtMta$module == "qaGeno"),]
    #   traitsMta <- unique(dtMta$analysisId)
    #   updateSelectInput(session, "versionMarker2Mta", choices = traitsMta, selected = traitsMta[length(traitsMta)])
    # })
    #################
    ## traits
    observeEvent(c(data(), input$version2Mta), {
      req(data())
      req(input$version2Mta)
      dtMta <- data()
      dtMta <- dtMta$predictions
      dtMta <- dtMta[which(dtMta$analysisId == input$version2Mta),]
      traitsMta <- unique(dtMta$trait)
      updateSelectInput(session, "trait2Mta", choices = traitsMta)
    })
    ## traits for plot
    observeEvent(c(data(), input$version2Mta), {
      req(data())
      req(input$version2Mta)
      dtMta <- data()
      dtMta <- dtMta$predictions
      dtMta <- dtMta[which(dtMta$analysisId == input$version2Mta),]
      traitsMta <- unique(dtMta$trait)
      updateSelectInput(session, "traitPredictionsCorrelation", choices = traitsMta)
    })
    #################
    ## fixed effects
    observeEvent(c(data(),input$version2Mta, input$trait2Mta), {
      req(data())
      req(input$version2Mta)
      req(input$trait2Mta)
      dtMta <- data()
      dtMta <- dtMta$predictions
      dtMta <- dtMta[which(dtMta$analysisId == input$version2Mta),]
      traitsMta <- apply(dtMta[,c("environment","designation","entryType","pipeline")],2,function(x){length(unique(x))})
      traitsMta <- names(traitsMta)[which(traitsMta > 1)] # remove factors that do not have more than one level
      start <- setdiff(traitsMta,c("designation","entryType","pipeline"))
      updateSelectInput(session, "fixedTermMta2", choices = traitsMta, selected = start)
    })
    #################
    ## random effects
    observeEvent(c(data(), input$version2Mta, input$trait2Mta, input$fixedTermMta2 ), {
      req(data())
      req(input$version2Mta)
      req(input$trait2Mta)
      # req(input$fixedTermMta2)
      dtMta <- data()
      dtMta <- dtMta$predictions
      dtMta <- dtMta[which(dtMta$analysisId == input$version2Mta),]
      traitsMta <- apply(dtMta[,c("environment","designation","entryType","pipeline")],2,function(x){length(unique(x))})
      traitsMta <- names(traitsMta)[which(traitsMta > 1)]
      traitsMta <- setdiff(traitsMta, input$fixedTermMta2)
      updateSelectInput(session, "randomTermMta2", choices = traitsMta, selected = "designation")
    })
    #################
    ## gXe interactions
    observeEvent(c(data(), input$trait2Mta), {
      req(data())
      req(input$trait2Mta)
      dtMta <- data()
      dtMta <- dtMta$metadata$weather
      traitsMta <- unique(c("envIndex",unique(dtMta$parameter)))
      updateSelectInput(session, "interactionTermMta2", choices = traitsMta)
    })
    #################
    # reactive table for environments to be included
    dtFieldMet = reactive({
      req(data())
      req(input$version2Mta)
      dtMta <- data()
      dtProv = dtMta$predictions
      dtProv <- dtProv[which(dtProv$analysisId == input$version2Mta),]
      dtProvTable=  as.data.frame( do.call( rbind, list (with(dtProv, table(environment,trait)) ) ) )
      bad <- which(dtProvTable <= 1, arr.ind = TRUE)
      if(nrow(bad) > 0){dtProvTable[bad] = 0}
      dtProvTable[which(dtProvTable > 1, arr.ind = TRUE)] = 1
      return(dtProvTable)
    })
    x = reactiveValues(df = NULL)
    observe({
      df <- dtFieldMet()
      x$df <- df
    })
    output$fieldsMet = DT::renderDT(x$df, selection = 'none', editable = TRUE)
    proxy = DT::dataTableProxy('fieldsMet')
    observeEvent(input$fieldsMet_cell_edit, {
      info = input$fieldsMet_cell_edit
      utils::str(info)
      i = info$row
      j = info$col
      v = info$value
      x$df[i, j] <- isolate(DT::coerceValue(v, x$df[i, j]))
    })
    #################
    # reactive table for trait family distributions
    dtDistTrait = reactive({
      traitNames = input$trait2Mta
      mm = matrix(0,nrow = 8, ncol = length(traitNames));
      rownames(mm) <- c(
        "quasi(link = 'identity', variance = 'constant')", "gaussian(link = 'identity')", "binomial(link = 'logit')",  "Gamma(link = 'inverse')",
        "inverse.gaussian(link = '1/mu^2')", "poisson(link = 'log')", "quasibinomial(link = 'logit')", "quasipoisson(link = 'log')"
      );
      colnames(mm) <- traitNames
      dtProvTable = as.data.frame(mm);  colnames(dtProvTable) <- traitNames
      return(dtProvTable)
    })
    xx = reactiveValues(df = NULL)
    observe({
      df <- dtDistTrait()
      xx$df <- df
    })
    output$traitDistMet = DT::renderDT(xx$df,
                                       selection = 'none',
                                       editable = TRUE,
                                       options = list(paging=FALSE,
                                                      searching=FALSE,
                                                      initComplete = I("function(settings, json) {alert('Done.');}")
                                       )
    )
    proxy = DT::dataTableProxy('traitDistMet')
    observeEvent(input$traitDistMet_cell_edit, {
      info = input$traitDistMet_cell_edit
      utils::str(info)
      i = info$row
      j = info$col
      v = info$value
      xx$df[i, j] <- isolate(DT::coerceValue(v, xx$df[i, j]))
    })

    ##############################################################################################
    ##############################################################################################
    ##############################################################################################
    ## render the input data to be analyzed
    output$statusMta <-  DT::renderDT({
      req(data())
      req(input$version2Mta)
      dtSta <- data() # dtSta<- result
      ### change column names for mapping
      paramsPheno <- data()$modeling
      paramsPheno <- paramsPheno[which(paramsPheno$analysisId %in% input$version2Mta),, drop=FALSE]
      DT::datatable(paramsPheno, extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
      )
    })

    output$phenoMta <-  DT::renderDT({
      req(data())
      req(input$version2Mta)
      dtMta <- data()
      dtMta <- dtMta$predictions
      dtMta <- dtMta[which(dtMta$analysisId == input$version2Mta),setdiff(colnames(dtMta),c("module","analysisId"))]
      numeric.output <- c("predictedValue", "stdError", "reliability")
      DT::formatRound(DT::datatable(dtMta, extensions = 'Buttons',
                                    options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                   lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
      ), numeric.output)
    })
    ## render connectivity plot
    observeEvent(c(data(),input$version2Mta), { # update entry types included in the plot
      req(data())
      req(input$version2Mta)
      dtMta <- data()
      dtMta <- dtMta$predictions
      dtMta <- dtMta[which(dtMta$analysisId %in% input$version2Mta),] # only traits that have been QA
      entryTypeMtaInput <- unique(dtMta$entryType)
      updateSelectInput(session, "entryTypeMta", choices = c(entryTypeMtaInput,"Generic"), selected = "Generic")
    })

    output$plotPredictionsSparsity <- shiny::renderPlot({
      req(data())
      req(input$version2Mta)
      req(input$entryTypeMta)
      dtMta <- data()
      mydata <- dtMta$predictions
      mydata <- mydata[which(mydata$analysisId %in% input$version2Mta),] # only PREDICTIONS FROM THE STA
      if(input$entryTypeMta != "Generic"){
        mydata <- mydata[which(mydata[,"entryType"] %in% input$entryTypeMta),]
      }
      M <- table(mydata$designation, mydata$environment)
      M2 <- matrix(M, nrow = nrow(M), ncol = ncol(M))
      M2 <- M2[,(input$slider2[1]):min(c(input$slider2[2], ncol(M2) )), drop=FALSE] # environments
      M2 <- M2[(input$slider1[1]):min(c(input$slider1[2]), nrow(M2) ), ,drop=FALSE] # genotypes
      Matrix::image(as(M2, Class = "dgCMatrix"), xlab="Environments", ylab="Genotypes", colorkey=TRUE)
    })

    output$plotPredictionsConnectivity <-  plotly::renderPlotly({
      req(data())
      req(input$version2Mta)
      req(input$entryTypeMta)
      dtMta <- data()
      mydata <- dtMta$predictions
      mydata <- mydata[which(mydata$analysisId %in% input$version2Mta),] # only PREDICTIONS FROM THE STA
      if(input$entryTypeMta != "Generic"){
        mydata <- mydata[which(mydata[,"entryType"] %in% input$entryTypeMta),]
      }
      splitAggregate <- with(mydata,  split(mydata[,"designation"],mydata[,"environment"]) )
      splitAggregate <- lapply(splitAggregate,unique); nag <- length(splitAggregate)
      nagm <- matrix(0,nag,nag); rownames(nagm) <- colnames(nagm) <- names(splitAggregate)
      for(i in 1:length(splitAggregate)){
        for(j in 1:i){
          nagm[i,j] <- length(intersect(splitAggregate[[i]],splitAggregate[[j]]))
        }
      }
      nagm[upper.tri(nagm)] <- t(nagm)[upper.tri(nagm)] # fill the upper triangular
      mydata4 <- as.data.frame(as.table(nagm)); mydata4$mytext <- paste(mydata4$Var1, mydata4$Var2,sep=".")
      mydata4$X1 <- as.numeric(as.factor(mydata4$Var1))
      mydata4$X2 <- as.numeric(as.factor(mydata4$Var2))
      ## make the plot
      fig <-  plotly::plot_ly(mydata4, x = mydata4[,"X1"], y = mydata4[,"X2"],
                              z = mydata4[,"Freq"], color = mydata4[,"Freq"],
                              text=mydata4[,"mytext"], colors = c('#BF382A', '#0C4B8E'))
      fig <- fig %>%  plotly::add_heatmap()
      ## add text inside the corplot?
      if(!is.null(input$checkboxText)){
        fig <- fig %>%  plotly::add_annotations(text =mydata4[,"Freq"],x = mydata4[,"X1"],y = mydata4[,"X2"],
                                                xref = 'x', yref = 'y',showarrow = FALSE, font=list(color='white')) #
      }
      ## add axis labels to the plot?
      if(!is.null(input$checkboxAxis)){
        fig <- fig %>%
          plotly::layout(yaxis = list(dtick = 1, ticktext = unique(mydata4[,"Var1"]), tickmode="array", tickvals = unique(mydata4[,"X1"]) ),
                         xaxis = list(dtick = 1, ticktext = unique(mydata4[,"Var2"]), tickmode="array", tickvals = unique(mydata4[,"X2"]) )

          )
      }
      fig
    })
    ## render metrics barplot
    observeEvent(c(data(),input$version2Mta), { # update trait
      req(data())
      req(input$version2Mta)
      dtMta <- data()
      dtMta <- dtMta$metrics
      dtMta <- dtMta[which(dtMta$analysisId %in% input$version2Mta),] # only traits that have been QA
      traitMtaInput <- unique(dtMta$trait)
      updateSelectInput(session, "traitMetrics", choices = traitMtaInput, selected = traitMtaInput)
    })
    observeEvent(c(data(),input$version2Mta), { # update parameter
      req(data())
      req(input$version2Mta)
      dtMta <- data()
      dtMta <- dtMta$metrics
      dtMta <- dtMta[which(dtMta$analysisId %in% input$version2Mta),] # only traits that have been QA
      metricsMtaInput <- unique(dtMta$parameter)
      updateSelectInput(session, "parameterMetrics", choices = metricsMtaInput)
    })
    output$barplotPredictionsMetrics <- plotly::renderPlotly({
      req(data())
      req(input$version2Mta)
      dtMta <- data()
      mydata <- dtMta$metrics
      mydata <- mydata[which(mydata$analysisId %in% input$version2Mta),]
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
    observeEvent(c(data(),input$version2Mta), { # update trait
      req(data())
      req(input$version2Mta)
      dtMta <- data()
      dtMta <- dtMta$predictions
      dtMta <- dtMta[which(dtMta$analysisId %in% input$version2Mta),] # only traits that have been QA
      traitMtaInput <- unique(dtMta$trait)
      updateSelectInput(session, "trait3Mta", choices = traitMtaInput)
    })
    output$plotPredictionsCleanOut <- plotly::renderPlotly({ # update plot
      req(data())
      req(input$version2Mta)
      req(input$trait3Mta)
      req(input$groupMtaInputPlot)
      mydata <- data()$predictions
      mydata <- mydata[which(mydata$analysisId %in% input$version2Mta),] # only traits that have been QA
      mydata <- mydata[which(mydata[,"trait"] %in% input$trait3Mta),]
      mydata[, "environment"] <- as.factor(mydata[, "environment"]); mydata[, "designation"] <- as.factor(mydata[, "designation"])
      res <- plotly::plot_ly(y = mydata[,"predictedValue"], type = "box", boxpoints = "all", jitter = 0.3, #color = mydata[,input$groupMtaInputPlot],
                             x = mydata[,input$groupMtaInputPlot], text=mydata[,"designation"], pointpos = -1.8)
      res = res %>% plotly::layout(showlegend = FALSE); res
    })
    ## render result of "run" button click
    outMta <- eventReactive(input$runMta, {
      req(data())
      req(input$version2Mta)
      req(input$trait2Mta)
      shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
      dtMta <- data()
      # family distributions input
      myFamily = apply(xx$df,2,function(y){rownames(xx$df)[which(y > 0)[1]]})
      dontHaveDist <- which(is.na(myFamily))
      if(length(dontHaveDist) > 0){myFamily[dontHaveDist] <- "quasi(link = 'identity', variance = 'constant')"}

      # run the modeling, but before test if sta was done
      if(sum(dtMta$status$module %in% "sta") == 0) {
        output$qaQcMtaInfo <- renderUI({
          if (hideAll$clearAll)
            return()
          else
            req(dtMta)
          HTML(as.character(div(style="color: brown;",
                                "Please perform Single-Trial-Analysis before conducting a Multi-Trial Analysis when using a two-stage analysis."))
          )
        })
      }else{
        output$qaQcMtaInfo <- renderUI({return(NULL)})
        resultMta <- try(cgiarPipeline::metLMM(
          phenoDTfile= dtMta, # analysis to be picked from predictions database
          analysisId=input$version2Mta,
          # analysisIdForGenoModifications = input$versionMarker2Mta,
          analysisIdForGenoModifications = NULL,
          fixedTerm= input$fixedTermMta2,  randomTerm=input$randomTermMta2,  residualBy=NULL,
          interactionsWithGeno=input$interactionTermMta2, envsToInclude=x$df,
          trait= input$trait2Mta, traitFamily=myFamily, useWeights=input$useWeights,
          heritLB= as.numeric(unlist(strsplit(input$heritLBMet,","))),
          heritUB= as.numeric(unlist(strsplit(input$heritUBMet,","))),
          modelType=input$modelMet, # either "grm", "nrm", or both
          deregress=input$deregressMet,  nPC=input$nPC,
          maxIters=input$maxitMet, batchSizeToPredict=500, tolParInv=1e-4,
          verbose=FALSE
        ),
        silent=TRUE
        )
        if(!inherits(resultMta,"try-error")) {
          data(resultMta) # update data with results
          # save(resultMta, file = "./R/outputs/resultMta.RData")
          cat(paste("Multi-trial analysis step with id:",resultMta$status$analysisId[length(resultMta$status$analysisId)],"saved."))
        }else{
          cat(paste("Analysis failed with the following error message: \n\n",resultMta[[1]]))
        }
      }
      shinybusy::remove_modal_spinner()

      if(!inherits(resultMta,"try-error")) { # if all goes well in the run
        ## predictions table
        output$predictionsMta <-  DT::renderDT({
          # if ( hideAll$clearAll){
          #   return()
          # }else{
          predictions <- resultMta$predictions
          predictions <- predictions[predictions$module=="mta",]
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
        })
        # metrics table
        output$metricsMta <-  DT::renderDT({
          if(!inherits(resultMta,"try-error") ){
            # if ( hideAll$clearAll){
            #   return()
            # }else{
            metrics <- resultMta$metrics
            mtas <- resultMta$status[which(resultMta$status$module == "mta"),"analysisId"]; mtaId <- mtas[length(mtas)]
            metrics <- metrics[which(metrics$analysisId == mtaId),]
            metrics <- subset(metrics, select = -c(module,analysisId))
            numeric.output <- c("value", "stdError")
            DT::formatRound(DT::datatable(metrics, extensions = 'Buttons',
                                          options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                         lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
            ), numeric.output)
            # }
          }
        })
        # modeling table
        output$modelingMta <-  DT::renderDT({
          if(!inherits(resultMta,"try-error") ){
            # if ( hideAll$clearAll){
            #   return()
            # }else{
            modeling <- resultMta$modeling
            mtas <- resultMta$status[which(resultMta$status$module == "mta"),"analysisId"]; mtaId <- mtas[length(mtas)]
            modeling <- modeling[which(modeling$analysisId == mtaId),]
            modeling <- subset(modeling, select = -c(module,analysisId))
            DT::datatable(modeling, extensions = 'Buttons',
                          options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                         lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
            )
            # }
          }
        })
        # ## Report tab
        output$reportMta <- renderUI({
          HTML(markdown::markdownToHTML(knitr::knit("./R/reportMta.Rmd", quiet = TRUE), fragment.only=TRUE))
        })

        output$downloadReportMta <- downloadHandler(
          filename = function() {
            paste('my-report', sep = '.', switch(
              "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
            ))
          },
          content = function(file) {
            src <- normalizePath('R/reportMta.Rmd')
            src2 <- normalizePath('R/outputs/resultMta.RData')
            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'report.Rmd', overwrite = TRUE)
            file.copy(src2, 'resultMta.RData', overwrite = TRUE)
            out <- rmarkdown::render('report.Rmd', params = list(toDownload=TRUE),switch(
              "HTML",
              HTML = rmarkdown::html_document()
            ))
            file.rename(out, file)
          }
        )

      } else {
        output$predictionsMta <- DT::renderDT({DT::datatable(NULL)})
        output$metricsMta <- DT::renderDT({DT::datatable(NULL)})
        output$modelingMta <- DT::renderDT({DT::datatable(NULL)})
        hideAll$clearAll <- TRUE
      } ### enf of if(!inherits(resultMta,"try-error"))

      hideAll$clearAll <- FALSE

    }) ## end eventReactive

    output$outMta <- renderPrint({
      outMta()
    })

  })
}

## To be copied in the UI
# mod_mtaApp_ui("mtaApp_1")

## To be copied in the server
# mod_mtaApp_server("mtaApp_1")

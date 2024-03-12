#' staApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_staApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    mainPanel( width = 12,
               tabsetPanel(id=ns("tabsMain"),
                           type = "tabs",

                           tabPanel(div(icon("book"), "Information-STA") ,
                                    br(),
                                    shinydashboard::box(status="success",width = 12,
                                                        solidHeader = TRUE,
                                                        column(width=12,   style = "height:580px; overflow-y: scroll;overflow-x: scroll;",
                                                               h1(strong(span("Single Trial Analysis", style="color:green"))),
                                                               h2(strong("Status:")),
                                                               uiOutput(ns("warningMessage")),
                                                               h2(strong("Details")),
                                                               p("The genetic evaluation approach we use known as 'two-step' first analyze trait by trait and trial by trial
                                          to remove the spatial noise from experiments using experimental factors like blocking and spatial coordinates.
                                          Each trial is one level of the environment
                              column (defined when the user matches the expected columns to columns present in the initial phenotypic input file).
                              Genotype is fitted as both, fixed and random. The user defines which should be returned in the predictions table.
                              By default genotype (designation column) predictions and their standard errors are returned.
                                The way the options are used is the following:"),
                                                               img(src = "www/sta.png", height = 200, width = 300), # add an image
                                                               p(strong("Genetic evaluation unit.-")," One or more of the following; designation, mother, father to indicate which column(s) should be considered the unit of genetic evaluation to compute BLUEs or BLUPs in the single trial analysis step."),
                                                               p(strong("Traits to analyze.-")," Traits to be analyzed. If no design factors can be fitted simple means are taken."),
                                                               p(strong("Covariates.-"),"Columns to be fitted as as additional fixed effect covariates in each trial."),
                                                               p(strong("Additional settings.-")),
                                                               p(strong("Type of estimate.-")," Whether BLUEs or BLUPs should be stored for the second stage."),
                                                               p(strong("Number of iterations.-")," Maximum number of restricted maximum likelihood iterations to be run for each trial-trait combination."),
                                                               p(strong("Print logs.-")," Whether the logs of the run should be printed in the screen or not."),
                                                               p(strong("Note.-")," A design-agnostic spatial design is carried. That means, all the spatial-related factors will be fitted if pertinent.
                                For example, if a trial has rowcoord information it will be fitted, if not it will be ignored. A two-dimensional spline kernel is only
                                fitted when the trial size exceeds 5 rows and 5 columns. In addition the following rules are followed: 1) Rows or columns are fitted if
                                you have equal or more than 3 levels, 2) Reps are fitted if you have equal or more than 2 levels, 3) Block (Sub-block) are fitted if you
                                have equal or more than 4 levels. "),
                                                               h2(strong("References:")),
                                                               p("Velazco, J. G., Rodriguez-Alvarez, M. X., Boer, M. P., Jordan, D. R., Eilers, P. H., Malosetti, M., & Van Eeuwijk, F. A. (2017).
                                Modelling spatial trends in sorghum breeding field trials using a two-dimensional P-spline mixed model. Theoretical and Applied
                                Genetics, 130, 1375-1392."),
                                                               p("Rodriguez-Alvarez, M. X., Boer, M. P., van Eeuwijk, F. A., & Eilers, P. H. (2018). Correcting for spatial heterogeneity in plant
                                breeding experiments with P-splines. Spatial Statistics, 23, 52-71."),
                                                               h2(strong("Software used:")),
                                                               p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
                                Vienna, Austria. URL https://www.R-project.org/."),
                                                               p("Boer M, van Rossum B (2022). _LMMsolver: Linear Mixed Model Solver_. R package version 1.0.4.9000.")


                                                        )
                                    )
                           ),
                           tabPanel(div(icon("arrow-right-to-bracket"), "Input"),
                                    tabsetPanel(
                                      tabPanel("Pick QA-stamp(s)", icon = icon("table"),
                                               br(),
                                               column(width=12, selectInput(ns("version2Sta"), "Data QA version(s) to consider (tagged records will be ignored)", choices = NULL, multiple = TRUE), style = "background-color:grey; color: #FFFFFF"),
                                               h4(strong(span("Visualizations below aim to help you pick the right parameter values. Please inspect them.", style="color:green"))),
                                               hr(style = "border-top: 3px solid #4c4c4c;"),
                                               shinydashboard::box(status="success",width = 12, solidHeader = TRUE,
                                                                   column(width=12,style = "height:430px; overflow-y: scroll;overflow-x: scroll;",
                                                                          p(span("Current analyses available.", style="color:black")),
                                                                          shiny::plotOutput(ns("plotTimeStamps")),
                                                                          p(span("Past modeling parameters from QA stamp selected.", style="color:black")),
                                                                          DT::DTOutput(ns("statusSta")), # modeling table
                                                                          p(span("Raw phenotypic data to be used as input.", style="color:black")),
                                                                          DT::DTOutput(ns("phenoSta")), # data input
                                                                   )
                                               )
                                      ),
                                      tabPanel("Pick trait(s)", icon = icon("magnifying-glass-chart"),
                                               br(),
                                               column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                      column(width=6, selectInput(ns("trait2Sta"), "Trait(s) to analyze", choices = NULL, multiple = TRUE) ),
                                                      column(width=6,selectInput(ns("fixedTermSta2"), "Covariable(s)", choices = NULL, multiple = TRUE) ),
                                                      column(width = 12, style = "background-color:grey; color: #FFFFFF",
                                                             shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Alternative response distributions...",
                                                                                 p(span("Family of response variable to be fitted.", style="color:black"), span("(double click in the cells if you would like to model a trait with a different distribution other than normal).", style="color:black")), # table of families to assume
                                                                                 DT::DTOutput(ns("traitDistSta")),
                                                             ),
                                                      ),
                                               ),
                                               h4(strong(span("Visualizations below aim to help you pick the right parameter values. Please inspect them.", style="color:green"))),
                                               hr(style = "border-top: 3px solid #4c4c4c;"),
                                               shinydashboard::box(status="success",width = 12, solidHeader = TRUE,
                                                                   column(width=12, style = "height:420px; overflow-y: scroll;overflow-x: scroll;",
                                                                          p(span("Boxplot of trait dispersion by environment", style="color:black")),
                                                                          selectInput(ns("trait3Sta"), "Trait to visualize", choices = NULL, multiple = FALSE),
                                                                          shiny::plotOutput(ns("plotPredictionsCleanOut")), # plotly::plotlyOutput(ns("plotPredictionsCleanOut")),
                                                                          shinydashboard::box(width = 12, status = "success", background="green",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Additional run settings...",
                                                                                              selectInput(ns("genoAsFixedSta"),"Estimate type",choices=list("BLUEs"=TRUE,"BLUPs"=FALSE),selected=TRUE),
                                                                                              numericInput(ns("maxitSta"),"Number of iterations",value=35),
                                                                                              selectInput(ns("verboseSta"),"Print logs",choices=list("Yes"=TRUE,"No"=FALSE),selected=FALSE)
                                                                          ),
                                                                   ),
                                               )
                                      ),
                                      tabPanel("Pick effect(s)", icon = icon("table"),
                                               br(),
                                               column(width=12, style = "background-color:grey; color: #FFFFFF" ,
                                                      column(width=6, selectInput(ns("genoUnitSta"), "Genetic evaluation unit(s)", choices = NULL, multiple = TRUE) ),
                                                      column(width=6,tags$span(id = ns('geno_unit_holder'), #style="color:orange",
                                                                               p("**If you have hybrid-crop data and plan to use 'mother' and 'father' information for GCA models please make sure you uploaded your Pedigree data (you can use the same Phenotype file if those columns are there)."),
                                                      )),
                                               ),
                                               h4(strong(span("Visualizations below aim to help you pick the right parameter values. Please inspect them.", style="color:green"))),
                                               hr(style = "border-top: 3px solid #4c4c4c;"),
                                               shinydashboard::box(status="success",width = 12, solidHeader = TRUE,
                                                                   column(width=12, style = "height:410px; overflow-y: scroll;overflow-x: scroll;",
                                                                          p(span("Summary of number of individuals, mothers and fathers available in the dataset.", style="color:black")),
                                                                          selectInput(ns("feature"), "Summarize evaluation units by:", choices = NULL, multiple = FALSE),
                                                                          DT::DTOutput(ns("summariesSta")), # genetic evaluation units
                                                                          p(span("Experimental design factos present per environment", style="color:black")),
                                                                          DT::dataTableOutput(ns("dtFieldTraC")), # design units
                                                                   )
                                               )
                                      ),
                                      tabPanel("Run analysis", icon = icon("play"),
                                               br(),
                                               actionButton(ns("runSta"), "Run STA", icon = icon("play-circle")),
                                               uiOutput(ns("qaQcStaInfo")),
                                               textOutput(ns("outSta")),
                                      ),
                                    )# of of tabsetPanel
                           ),
                           tabPanel(div(icon("arrow-right-from-bracket"), "Output" ) , value = "outputTabs",
                                    tabsetPanel(
                                      tabPanel("Predictions", icon = icon("table"),
                                               br(),
                                               shinydashboard::box(status="success",width = 12,
                                                                   solidHeader = TRUE,
                                                                   column(width=12,DT::DTOutput(ns("predictionsSta")),style = "height:530px; overflow-y: scroll;overflow-x: scroll;")
                                               )
                                      ),
                                      tabPanel("Metrics", icon = icon("table"),
                                               br(),
                                               shinydashboard::box(status="success",width = 12,
                                                                   solidHeader = TRUE,
                                                                   column(width=12,br(),DT::DTOutput(ns("metricsSta")),style = "height:530px; overflow-y: scroll;overflow-x: scroll;")
                                               )
                                      ),
                                      tabPanel("Modeling", icon = icon("table"),
                                               br(),
                                               shinydashboard::box(status="success",width = 12,
                                                                   solidHeader = TRUE,
                                                                   column(width=12,br(),DT::DTOutput(ns("modelingSta")),style = "height:530px; overflow-y: scroll;overflow-x: scroll;")
                                               )
                                      ),
                                      tabPanel("Report STA", icon = icon("file-image"),
                                               br(),
                                               div(tags$p("Please download the report below:") ),
                                               downloadButton(ns("downloadReportSta"), "Download report"),
                                               br(),
                                               uiOutput(ns('reportSta')),
                                      ),
                                      # tabPanel("Report OFT", icon = icon("file-image"),
                                      #          br(),
                                      #          selectInput(ns("fieldinst"), label = "Environments to Include in the Report", choices = NULL, multiple = TRUE),
                                      #          br(),
                                      #          div(tags$p("Please download the report below:") ),
                                      #          downloadButton(ns("downloadReportOft"), "Download report")
                                      # )
                                    ) # of of tabsetPanel
                           )# end of output panel


               )) # end mainpanel

  )
}

#' staApp Server Functions
#'
#' @noRd
mod_staApp_server <- function(id,data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })
    # data = reactive({
    #   load("~/Documents/bioflow/dataStr0.RData")
    #   data <- res
    #   return(data)
    # })
    ############################################################################
    observeEvent(
      c(data(),input$version2Sta,input$genoUnitSta),
      if(length(input$genoUnitSta) > 0){ # added
        if ('mother' %in% input$genoUnitSta | 'father' %in% input$genoUnitSta | is.null(input$genoUnitSta) ){
          golem::invoke_js('showid', ns('geno_unit_holder'))
        }else { #
          golem::invoke_js('hideid', ns('geno_unit_holder'))
        }
      }else{golem::invoke_js('hideid', ns('geno_unit_holder'))}
    )
    # warning message
    output$warningMessage <- renderUI(
      if(is.null(data())){
        HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your phenotypic data using the 'Data Retrieval' tab.")) )
      }else{ # data is there
        mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
        if(mappedColumns == 3){
          if("qaRaw" %in% data()$status$module){
            HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to perform the single-trial analysis specifying your input parameters under the 'Input' tabs.")) )
          }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please identify trait-outliers in the phenotypic dataset before performing a single-trial analysis. Go to the 'QC & Transform' tab to do so. ")) ) }
        }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please make sure that you have computed the 'environment' column, and that column 'designation' and \n at least one trait have been mapped using the 'Data Retrieval' tab.")) )}
      }
    )
    # QA versions to use
    observeEvent(c(data()), {
      req(data())
      dtMta <- data()
      dtMta <- dtMta$status
      dtMta <- dtMta[which(dtMta$module %in% c("qaRaw","qaFilter")),]
      traitsMta <- unique(dtMta$analysisId)
      if(length(traitsMta) > 0){names(traitsMta) <- as.POSIXct(traitsMta, origin="1970-01-01", tz="GMT")}
      updateSelectInput(session, "version2Sta", choices = traitsMta)
    })
    # genetic evaluation unit
    observe({
      req(data())
      req(input$version2Sta)
      genetic.evaluation <- c("designation", "mother","father")
      updateSelectInput(session, "genoUnitSta",choices = genetic.evaluation, selected = "designation")
    })
    # traits
    observeEvent(c(data(),input$version2Sta,input$genoUnitSta), {
      req(data())
      req(input$version2Sta)
      req(input$genoUnitSta)
      dtSta <- data()
      dtSta <- dtSta$modifications$pheno
      dtSta <- dtSta[which(dtSta$analysisId %in% input$version2Sta),] # only traits that have been QA
      traitsSta <- unique(dtSta$trait)
      updateSelectInput(session, "trait2Sta", choices = traitsSta, selected = traitsSta)
    })
    # fixed effect covariates
    observeEvent(c(data(),input$version2Sta,input$genoUnitSta, input$trait2Sta), {
      req(data())
      req(input$version2Sta)
      req(input$genoUnitSta)
      req(input$trait2Sta)
      dtSta <- data()
      dtSta <- dtSta$modifications$pheno # only traits that have been QA
      dtSta <- dtSta[which(dtSta$analysisId %in% input$version2Sta),]
      traitsSta <- unique(dtSta$trait)
      '%!in%' <- function(x,y)!('%in%'(x,y))
      updateSelectInput(session, "fixedTermSta2", choices = traitsSta[traitsSta%!in%input$trait2Sta])
    })
    # reactive table for trait family distributions
    dtDistTrait = reactive({
      req(data())
      req(input$trait2Sta)
      dtSta <- data()
      dtSta <- dtSta$data$pheno
      traitNames = input$trait2Sta
      mm = matrix(0,nrow = 8, ncol = length(traitNames));
      rownames(mm) <- c(
        "gaussian(link = 'identity')",
        "quasi(link = 'identity', variance = 'constant')",
        "binomial(link = 'logit')",
        "Gamma(link = 'inverse')",
        "inverse.gaussian(link = '1/mu^2')",
        "poisson(link = 'log')",
        "quasibinomial(link = 'logit')",
        "quasipoisson(link = 'log')"
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
    output$traitDistSta = DT::renderDT(xx$df,
                                       selection = 'none',
                                       editable = TRUE,
                                       options = list(paging=FALSE,
                                                      searching=FALSE,
                                                      initComplete = I("function(settings, json) {alert('Done.');}")
                                       )
    )

    ##

    ##

    proxy = DT::dataTableProxy('traitDistSta')

    observeEvent(input$traitDistSta_cell_edit, {
      info = input$traitDistSta_cell_edit
      utils::str(info)
      i = info$row
      j = info$col
      v = info$value
      xx$df[i, j] <- isolate(DT::coerceValue(v, xx$df[i, j]))
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
    })
    ## render the data to be analyzed
    observeEvent(data(),{
      if(sum(data()$status$module %in% "qaRaw") != 0) {
        ## render status
        output$statusSta <-  DT::renderDT({
          req(data())
          req(input$version2Sta)
          dtSta <- data() # dtSta<- result
          ### change column names for mapping
          paramsPheno <- data()$modeling
          paramsPheno <- paramsPheno[which(paramsPheno$analysisId %in% input$version2Sta),, drop=FALSE]
          paramsPheno$analysisId <- as.POSIXct(paramsPheno$analysisId, origin="1970-01-01", tz="GMT")
          DT::datatable(paramsPheno, extensions = 'Buttons',
                        options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                       lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
          )
        })
        ## render summaries
        output$summariesSta <-  DT::renderDT({
          req(data())
          req(input$version2Sta)
          req(input$feature)
          dtSta <- data() # dtSta<- result
          ### change column names for mapping
          paramsPheno <- data()$metadata$pheno
          paramsPheno <- paramsPheno[which(!duplicated(paramsPheno$value)),]
          colnames(dtSta$data$pheno) <- cgiarBase::replaceValues(colnames(dtSta$data$pheno), Search = paramsPheno$value, Replace = paramsPheno$parameter )
          paramsPed <- data()$metadata$pedigree
          colnames(dtSta$data$pedigree) <- cgiarBase::replaceValues(colnames(dtSta$data$pedigree), Search = paramsPed$value, Replace = paramsPed$parameter )
          ###
          dtSta <- merge(dtSta$data$pheno, dtSta$data$pedigree, by="designation") # merge mother and father info in the pheno data frame
          dtStaList <- split(dtSta, dtSta[,input$feature]) # split info by environment
          dtStaListRes <- list()
          for(i in 1:length(dtStaList)){
            dtStaListRes[[i]] <- as.data.frame(as.table(apply(dtStaList[[i]][,c("designation","mother","father")],2, function(x){length(na.omit(unique(x)))})))
            dtStaListRes[[i]][,input$feature] <- names(dtStaList)[i]
          }
          dtSta <- do.call(rbind, dtStaListRes)
          colnames(dtSta)[1:2] <- c("geneticUnit", "numberOfUnits")
          dtSta <- dtSta[with(dtSta, order(geneticUnit)), ]; rownames(dtSta) <- NULL
          DT::datatable(dtSta, extensions = 'Buttons',
                        options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                       lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
          )
        })
        ## render designs
        output$dtFieldTraC <-  DT::renderDT({
          req(data())
          req(input$version2Sta)
          object <- data()
          dtProv = object$data$pheno
          paramsPheno <- object$metadata$pheno
          if(!is.null(paramsPheno)){
            paramsPheno <- paramsPheno[which(paramsPheno$parameter != "trait"),]
            colnames(dtProv) <- cgiarBase::replaceValues(colnames(dtProv), Search = paramsPheno$value, Replace = paramsPheno$parameter )
            dtProvNames <- c("row","col","rep","iBlock")
            envCol <- paramsPheno[which(paramsPheno$parameter == "environment"),"value"]
            if(length(envCol) > 0){
              fieldNames <- as.character(unique(dtProv[,"environment"]))
              spD <- split(dtProv,dtProv[,"environment"])
              presentFactors <- paramsPheno$parameter[which(paramsPheno$parameter %in% dtProvNames)]
              presentFactorsPerField <- lapply(spD, function(x){
                apply(x[,presentFactors, drop=FALSE], 2, function(y){length(unique(y))})
              })
              presentFactorsPerField <- do.call(rbind, presentFactorsPerField)
              dtProvTable = as.data.frame(presentFactorsPerField);  rownames(dtProvTable) <- fieldNames
              DT::datatable(dtProvTable, extensions = 'Buttons',
                            options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                           lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
              )
            }
          }
        })
        ## render plot of trait distribution
        observeEvent(c(data(),input$version2Sta), { # update trait
          req(data())
          req(input$version2Sta)
          paramsPheno <- data()$metadata$pheno
          paramsPheno <- paramsPheno[which(!duplicated(paramsPheno$value)),]
          paramsPheno <- paramsPheno[which(paramsPheno$parameter != "trait"),]
          traitsSta <- setdiff(paramsPheno$parameter, c("trait","designation"))
          updateSelectInput(session, "feature", choices = traitsSta)
        })
        observeEvent(c(data(),input$version2Sta), { # update trait
          req(data())
          req(input$version2Sta)
          dtSta <- data()
          dtSta <- dtSta$modifications$pheno
          dtSta <- dtSta[which(dtSta$analysisId %in% input$version2Sta),] # only traits that have been QA
          traitsSta <- unique(dtSta$trait)
          updateSelectInput(session, "trait3Sta", choices = traitsSta)
        })
        output$plotPredictionsCleanOut <- shiny::renderPlot({ # plotly::renderPlotly({
          req(data())
          req(input$version2Sta)
          req(input$trait3Sta)
          mydata <- data()$data$pheno
          ### change column names for mapping
          paramsPheno <- data()$metadata$pheno
          paramsPheno <- paramsPheno[which(paramsPheno$parameter != "trait"),]
          colnames(mydata) <- cgiarBase::replaceValues(colnames(mydata), Search = paramsPheno$value, Replace = paramsPheno$parameter )
          ###
          mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
          if(mappedColumns == 3){ # all required columns are present
            mydata$rowindex <- 1:nrow(mydata)
            mydata[, "environment"] <- as.factor(mydata[, "environment"]);mydata[, "designation"] <- as.factor(mydata[, "designation"])
            mo <- data()$modifications$pheno
            mo <- mo[which(mo[,"trait"] %in% input$trait3Sta),]
            # mydata$color <- 1
            # if(nrow(mo) > 0){mydata$color[which(mydata$rowindex %in% unique(mo$row))]=2}
            # mydata$color <- as.factor(mydata$color)
            # res <- plotly::plot_ly(y = mydata[,input$trait3Sta], type = "box", boxpoints = "all", jitter = 0.3,color=mydata[,"color"],
            #                        x = mydata[,"environment"], text=mydata[,"designation"], pointpos = -1.8)
            # res = res %>% plotly::layout(showlegend = FALSE); res
            mydata$color <- "valid"
            if(nrow(mo) > 0){mydata$color[which(mydata$rowindex %in% unique(mo$row))]="tagged"}
            mydata$predictedValue <- mydata[,input$trait3Sta]
            ggplot2::ggplot(mydata, ggplot2::aes(x=as.factor(environment), y=predictedValue)) +
              ggplot2::geom_boxplot(fill='#A4A4A4', color="black", notch = TRUE)+
              ggplot2::theme_classic()+
              ggplot2::geom_jitter(ggplot2::aes(colour = color), alpha = 0.4) +
              ggplot2::xlab("Environment") + ggplot2::ylab("Trait value") +
              ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45))
          }else{}
        })
        ## render raw data
        output$phenoSta <-  DT::renderDT({
          req(data())
          dtSta <- data()
          req(input$version2Sta)
          dtSta <- dtSta$data$pheno
          ### change column names for mapping
          paramsPheno <- data()$metadata$pheno
          paramsPheno <- paramsPheno[which(paramsPheno$parameter != "trait"),]
          colnames(dtSta) <- cgiarBase::replaceValues(colnames(dtSta), Search = paramsPheno$value, Replace = paramsPheno$parameter )
          ###
          traitTypes <- unlist(lapply(dtSta,class))
          numeric.output <- names(traitTypes)[which(traitTypes %in% "numeric")]
          DT::formatRound(DT::datatable(dtSta, extensions = 'Buttons',
                                        options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                       lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
          ), numeric.output)
        })
      } else {
        output$summariesSta <- DT::renderDT({DT::datatable(NULL)})
        output$phenoSta <- DT::renderDT({DT::datatable(NULL)})
        output$plotPredictionsCleanOut <- plotly::renderPlotly({NULL})
      }
    })
    ## render result of "run" button click
    outSta <- eventReactive(input$runSta, {
      req(data())
      req(input$version2Sta)
      req(input$trait2Sta)
      req(input$genoUnitSta)
      req(input$genoAsFixedSta)
      req(input$verboseSta)
      req(input$maxitSta)
      shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
      dtSta <- data()
      myFamily = apply(xx$df,2,function(y){rownames(xx$df)[which(y > 0)[1]]})
      dontHaveDist <- which(is.na(myFamily))
      if(length(dontHaveDist) > 0){myFamily[dontHaveDist] <- "gaussian(link = 'identity')"}

      # run the modeling, but before test if qa/qc done
      if(sum(dtSta$status$module %in% "qaRaw") == 0) {
        output$qaQcStaInfo <- renderUI({
          if (hideAll$clearAll)
            return()
          else
            req(dtSta)
          HTML(as.character(div(style="color: brown;",
                                "Please perform QA/QC before conducting a Single-Trial Analysis."))
          )
        })
      } else {
        output$qaQcStaInfo <- renderUI({return(NULL)})
        # save(dtSta, file = "./R/outputs/resultSta.RData")
        result <- try(cgiarPipeline::staLMM(phenoDTfile = dtSta, analysisId=input$version2Sta,
                                            trait=input$trait2Sta, traitFamily = myFamily,
                                            fixedTerm = input$fixedTermSta2,
                                            returnFixedGeno=input$genoAsFixedSta, genoUnit = input$genoUnitSta,
                                            verbose = input$verboseSta, maxit = input$maxitSta),
                      silent=TRUE
        )
        if(!inherits(result,"try-error")) {
          data(result) # update data with results
          # save(result, file = "./R/outputs/resultSta.RData")
          cat(paste("Single-trial analysis step with id:",as.POSIXct(result$status$analysisId[length(result$status$analysisId)], origin="1970-01-01", tz="GMT"),"saved. Please proceed to perform your multi-trial analysis using this time stamp."))
          updateTabsetPanel(session, "tabsMain", selected = "outputTabs")
        }else{
          cat(paste("Analysis failed with the following error message: \n\n",result[[1]]))
        }
      }
      shinybusy::remove_modal_spinner()

      if(sum(dtSta$status$module %in% "qaRaw") != 0) {

        output$predictionsSta <-  DT::renderDT({
          if(!inherits(result,"try-error") ){
            # if ( hideAll$clearAll){
            #   return()
            # }else{
            predictions <- result$predictions
            predictions <- predictions[predictions$module=="sta",]
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
          }
        })

        output$metricsSta <-  DT::renderDT({
          if(!inherits(result,"try-error") ){
            # if ( hideAll$clearAll){
            #   return()
            # }else{
            metrics <- result$metrics
            metrics <- metrics[metrics$module=="sta",]
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
          }
        })

        output$modelingSta <-  DT::renderDT({
          if(!inherits(result,"try-error") ){
            # if ( hideAll$clearAll){
            #   return()
            # }else{
            modeling <- result$modeling
            modeling <- modeling[modeling$module=="sta",]
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
          }
        })
        ## report STA
        output$reportSta <- renderUI({
          HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportSta.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
        })

        output$downloadReportSta <- downloadHandler(
          filename = function() {
            paste('my-report-STA', sep = '.', switch(
              "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
            ))
          },
          content = function(file) {
            shinybusy::show_modal_spinner(spin = "fading-circle",
                                          text = "Generating Report...")
            src <- normalizePath(system.file("rmd","reportSta.Rmd",package="bioflow"))
            # src2 <- normalizePath('data/resultSta.RData')

            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'report.Rmd', overwrite = TRUE)
            # file.copy(src2, 'resultSta.RData', overwrite = TRUE)
            out <- rmarkdown::render('report.Rmd', params = list(toDownload=TRUE),switch(
              "HTML",
              HTML = rmarkdown::html_document()
            ))
            file.rename(out, file)
            shinybusy::remove_modal_spinner()
          }
        )

        ## report OFT
        updateSelectInput(session, inputId = "fieldinst", choices = result$metrics$environment, selected = result$metrics$environment[1])

        output$downloadReportOft <- downloadHandler(
          filename = function() {
            paste('my-report-OFT', sep = '.', switch(
              "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
            ))
          },
          content = function(file) {
            shinybusy::show_modal_spinner(spin = "fading-circle",
                                          text = "Generating Report...")

            src <- normalizePath(system.file("rmd","reportOft.Rmd",package="bioflow"))
            # src2 <- normalizePath('data/resultSta.RData')

            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'report2.Rmd', overwrite = TRUE)
            # file.copy(src2, 'resultSta.RData', overwrite = TRUE)
            out <- rmarkdown::render('report2.Rmd', params = list(fieldinst=input$fieldinst, toDownload=TRUE),switch(
              "HTML",
              HTML = rmarkdown::html_document()
            ))
            file.rename(out, file)
            shinybusy::remove_modal_spinner()
          }
        )

      } else {
        output$predictionsSta <- DT::renderDT({DT::datatable(NULL)})
        output$metricsSta <- DT::renderDT({DT::datatable(NULL)})
        output$modelingSta <- DT::renderDT({DT::datatable(NULL)})
      }

      hideAll$clearAll <- FALSE

    }) ## end eventReactive

    output$outSta <- renderPrint({
      outSta()
    })



  }) ## end moduleserver
}

## To be copied in the UI
# mod_staApp_ui("staApp_1")

## To be copied in the server
# mod_staApp_server("staApp_1")

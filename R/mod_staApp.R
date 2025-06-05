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

    tags$br(),

    mainPanel( width = 12,
               tabsetPanel(id=ns("tabsMain"),
                           type = "tabs",

                           tabPanel(div(icon("book"), "Information") ,
                                    br(),
                                    column(width = 6,
                                           h1(strong(span("Single Trial Analysis Module", tags$a(href="https://www.youtube.com/watch?v=fqUTdvbzNSE&list=PLZ0lafzH_UmclOPifjCntlMzysEB2_2wX&index=6", icon("youtube") , target="_blank"), style="color:darkcyan"))),
                                           h2(strong("Data Status (wait to be displayed):")),
                                           uiOutput(ns("warningMessage")),
                                           tags$br(),
                                           # column(width=4, tags$br(),
                                           shinyWidgets::prettySwitch( inputId = ns('launch'), label = "Load example dataset", status = "success"),
                                           # ),
                                           tags$br(),
                                           img(src = "www/geospatial.jpg", height = 250, width = 500), # add an image
                                    ),

                                    column(width = 6,
                                           h2(strong("Details")),
                                           p("The genetic evaluation approach we use known as 'two-step' first analyze trait by trait and trial by trial
                                          to remove the spatial noise from experiments using experimental factors like blocking and spatial coordinates.
                                          Each trial is one level of the environment
                              column (defined when the user matches the expected columns to columns present in the initial phenotypic input file).
                              Genotype is fitted as both, fixed and random. The user defines which should be returned in the predictions table.
                              By default genotype (designation column) predictions and their standard errors are returned.
                                The way the options are used is the following:"),
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
                                           h2(strong("References")),
                                           p("Velazco, J. G., Rodriguez-Alvarez, M. X., Boer, M. P., Jordan, D. R., Eilers, P. H., Malosetti, M., & Van Eeuwijk, F. A. (2017).
                                Modelling spatial trends in sorghum breeding field trials using a two-dimensional P-spline mixed model. Theoretical and Applied
                                Genetics, 130, 1375-1392."),
                                           p("Rodriguez-Alvarez, M. X., Boer, M. P., van Eeuwijk, F. A., & Eilers, P. H. (2018). Correcting for spatial heterogeneity in plant
                                breeding experiments with P-splines. Spatial Statistics, 23, 52-71."),
                                           h2(strong("Software used")),
                                           p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
                                Vienna, Austria. URL https://www.R-project.org/."),
                                           p("Boer M, van Rossum B (2022). _LMMsolver: Linear Mixed Model Solver_. R package version 1.0.4.9000."),
                                           # column(width = 12, shiny::plotOutput(ns("plotDataDependencies")), ),
                                    ),
                           ),
                           tabPanel(id=ns("tabsInput0"),  type = "tabs", title=div(icon("arrow-right-to-bracket"), "Input steps"),
                                    tabsetPanel( id=ns("tabsInput1"), type = "tabs",
                                                 tabPanel(value="tabsInputStamp",title=div(icon("dice-one"), "Pick QA-stamp(s)", icon("arrow-right") ) , #icon = icon("dice-one"),
                                                          br(),
                                                          column(width=12,style = "background-color:grey; color: #FFFFFF",
                                                                 column(width=8,
                                                                        selectInput(ns("version2Sta"),
                                                                                    label = tags$span(
                                                                                      "Pheno-modification stamp(s) to apply to the data",
                                                                                      tags$i(
                                                                                        class = "glyphicon glyphicon-info-sign",
                                                                                        style = "color:#FFFFFF",
                                                                                        title = "Analysis ID(s) from QA modifications that should be applied in the STA."
                                                                                      )
                                                                                    ),
                                                                                    choices = NULL, multiple = TRUE),
                                                                 ),

                                                          ),
                                                          column(width=12),
                                                          shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                              column(width=12,
                                                                                     hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                                     h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameters to be specified in the grey boxes above.", style="color:green"))),
                                                                                     hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                              ),
                                                                              column( width=12, shiny::plotOutput(ns("plotTimeStamps")) ),
                                                                              DT::DTOutput(ns("statusSta")), # modeling table
                                                                              DT::DTOutput(ns("phenoSta")),
                                                          ),
                                                 ),
                                                 tabPanel(value="tabsInputTraits",title=div( icon("dice-two"), "Pick trait(s)", icon("arrow-right") ) , #icon = icon("dice-two"),
                                                          br(),
                                                          column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                                 column(width=4,
                                                                        selectInput(ns("trait2Sta"),
                                                                                    label = tags$span(
                                                                                      "Trait(s) to analyze",
                                                                                      tags$i(
                                                                                        class = "glyphicon glyphicon-info-sign",
                                                                                        style = "color:#FFFFFF",
                                                                                        title = "Only cleaned traits will be available."
                                                                                      )
                                                                                    ),
                                                                                    choices = NULL, multiple = TRUE),
                                                                 ),
                                                                 column(width=4,
                                                                        selectInput(ns("fixedTermSta2"),
                                                                                    label = tags$span(
                                                                                      "Covariable(s) (optional)",
                                                                                      tags$i(
                                                                                        class = "glyphicon glyphicon-info-sign",
                                                                                        style = "color:#FFFFFF",
                                                                                        title = "Trait(s) that should be used as fixed effect covariate. Only to be used if a field correction besides the spatial model is needed."
                                                                                      )
                                                                                    ),
                                                                                    choices = NULL, multiple = TRUE),
                                                                 ),
                                                                 # column(width = 12, style = "background-color:grey; color: #FFFFFF",
                                                                 tags$br(),
                                                                 shinydashboard::box(width = 4, style = "color: #000000",status = "success",solidHeader=FALSE,collapsible = TRUE, collapsed = TRUE, title = "Alternative response distributions...",
                                                                                     p(span("The Normal distribution is assumed as default for all traits. If you wish to specify a different trait distribution for a given trait double click in the cell corresponding for the trait by distribution combination and make it a '1'.", style="color:black")),
                                                                                     DT::DTOutput(ns("traitDistSta")),
                                                                 ),
                                                                 # ),
                                                          ),
                                                          column(width=12),
                                                          shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                              column(width=12,
                                                                                     hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                                     h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameters to be specified in the grey boxes above.", style="color:green"))),
                                                                                     hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                              ),
                                                                              tags$span(id = ns('holder1'),
                                                                                        column(width=9, selectInput(ns("trait3Sta"), "Trait to visualize", choices = NULL, multiple = FALSE) ),
                                                                                        # column(width=3, checkboxInput(ns("checkbox"), label = "Include x-axis labels", value = TRUE) ),
                                                                                        column(width=3, numericInput(ns("transparency"),"Plot transparency",value=0.5, min=0, max=1, step=0.1) ),
                                                                                        column(width=12, plotly::plotlyOutput(ns("plotPredictionsCleanOut"))  ), # ,  shiny::plotOutput(ns("plotPredictionsCleanOut"))
                                                                                        column(width=12, plotly::plotlyOutput(ns("plotFieldGrid"))  ),
                                                                              ),
                                                          ),
                                                 ),
                                                 tabPanel(value="tabsInputEffects", title=div(icon("dice-three"), "Pick effect(s)", icon("arrow-right") ) , #icon = icon("dice-three"),
                                                          br(),
                                                          column(width=12, style = "background-color:grey; color: #FFFFFF" ,
                                                                 column(width=4,
                                                                        selectInput(ns("genoUnitSta"),
                                                                                    label = tags$span(
                                                                                      "Genetic evaluation unit(s)",
                                                                                      tags$i(
                                                                                        class = "glyphicon glyphicon-info-sign",
                                                                                        style = "color:#FFFFFF",
                                                                                        title = "Depending on the crop this selection can be males and females for progeny-testing models (e.g., GCA), or designation for per-se performance models. "
                                                                                      )
                                                                                    ),
                                                                                    choices = NULL, multiple = TRUE)
                                                                 ),
                                                                 column( width=8,
                                                                         br(),
                                                                         shinydashboard::box(width = 12, style = "color: #000000",status = "success",solidHeader=FALSE,collapsible = TRUE, collapsed = TRUE, title = "Additional run settings (optional)...",
                                                                                             selectInput(ns("genoAsFixedSta"),
                                                                                                         label = tags$span(
                                                                                                           "Estimate type",
                                                                                                           tags$i(
                                                                                                             class = "glyphicon glyphicon-info-sign",
                                                                                                             style = "color:#FFFFFF",
                                                                                                             title = "Because the expectation is two use the two-stage approach, BLUEs are the default (designation is fitted as fixed). Only use BLUPs (designation is fitted as random) if you are planning to use the results directly to take a selection decision. "
                                                                                                           )
                                                                                                         ),
                                                                                                         choices=list("BLUEs"=TRUE,"BLUPs"=FALSE),selected=TRUE),
                                                                                             numericInput(ns("maxitSta"),
                                                                                                          label = tags$span(
                                                                                                            "Number of iterations",
                                                                                                            tags$i(
                                                                                                              class = "glyphicon glyphicon-info-sign",
                                                                                                              style = "color:#FFFFFF",
                                                                                                              title = "Restricted Maximum Likelihood requires certain number of iterations to converge to optimal values in variance components. The default reflects rules of thumb but it may finish earlier."
                                                                                                            )
                                                                                                          ),
                                                                                                          value=35),
                                                                                             selectInput(ns("verboseSta"),"Print log file?",choices=list("Yes"=TRUE,"No"=FALSE),selected=FALSE)
                                                                         ),
                                                                 ),

                                                          ),
                                                          column(width=12),
                                                          shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                              column(width=12,
                                                                                     hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                                     h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameters to be specified in the grey boxes above.", style="color:green"))),
                                                                                     hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                              ),
                                                                              tags$span(id = ns('holder2'),
                                                                                        selectInput(ns("feature"), "Summarize evaluation units by:", choices = NULL, multiple = FALSE),
                                                                                        DT::DTOutput(ns("summariesSta")), # genetic evaluation units
                                                                                        DT::dataTableOutput(ns("dtFieldTraC")), # design units
                                                                              ),
                                                          ),
                                                 ),
                                                 tabPanel(value="tabsInputRun", title=div(icon("dice-four"), "Run analysis" ), # icon = icon("dice-four"),
                                                          br(),
                                                          column(width=12,style = "background-color:grey; color: #FFFFFF",
                                                                 column(width=3, tags$div(textInput(ns("analysisIdName"), label = tags$span(
                                                                   "Analysis Name (optional)", tags$i( class = "glyphicon glyphicon-info-sign", style = "color:#FFFFFF",
                                                                               title = "An optional name for the analysis besides the timestamp if desired.") ), #width = "100%",
                                                                   placeholder = "(optional name)") ) ),
                                                                 column(width=3,
                                                                        br(),
                                                                        actionButton(ns("runSta"), "Run analysis", icon = icon("play-circle")),
                                                                        uiOutput(ns("qaQcStaInfo")),
                                                                        br(),
                                                                 ),

                                                          ),
                                                          textOutput(ns("outSta")),
                                                 ),
                                    )# of of tabsetPanel
                           ),
                           tabPanel(div(icon("arrow-right-from-bracket"), "Output tabs" ) , value = "outputTabs",
                                    tabsetPanel(
                                      tabPanel("Dashboard", icon = icon("file-image"),
                                               br(),
                                               textOutput(ns("outSta2")),
                                               br(),
                                               downloadButton(ns("downloadReportSta"), "Download dashboard"),
                                               uiOutput(ns('ebsReportButton')),
                                               br(),
                                               uiOutput(ns('reportSta')),
                                      ),
                                      tabPanel("Predictions", icon = icon("table"),
                                               br(),
                                               DT::DTOutput(ns("predictionsSta")),
                                      ),
                                      tabPanel("Metrics", icon = icon("table"),
                                               br(),
                                               DT::DTOutput(ns("metricsSta")),
                                      ),
                                      tabPanel("Modeling", icon = icon("table"),
                                               br(),
                                               DT::DTOutput(ns("modelingSta")),
                                      ),
                                    ) # of of tabsetPanel
                           )# end of output panel
               )) # end mainpanel
  )
}

#' staApp Server Functions
#'
#' @noRd
# mod_staApp_server <- function(id,data){
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
#     observeEvent(c(data(), input$version2Sta), {
#       req(data())
#       mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
#       if(mappedColumns == 3 & length(input$version2Sta)>0 ){
#         golem::invoke_js('showid', ns('holder1'))
#         golem::invoke_js('showid', ns('holder2'))
#       }else{
#         golem::invoke_js('hideid', ns('holder1'))
#         golem::invoke_js('hideid', ns('holder2'))
#       }
#     })
#     ############################################################################
#     observeEvent(
#       c(data(),input$version2Sta,input$genoUnitSta),
#       if(length(input$genoUnitSta) > 0){ # added
#         if ('mother' %in% input$genoUnitSta | 'father' %in% input$genoUnitSta | is.null(input$genoUnitSta) ){
#           golem::invoke_js('showid', ns('geno_unit_holder'))
#         }else { #
#           golem::invoke_js('hideid', ns('geno_unit_holder'))
#         }
#       }else{golem::invoke_js('hideid', ns('geno_unit_holder'))}
#     )
#     # warning message
#     output$warningMessage <- renderUI(
#       if(is.null(data())){
#         HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your phenotypic data using the 'Data Retrieval' tab.")) )
#       }else{ # data is there
#         mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
#         if(mappedColumns == 3){
#           if( any(c("qaRaw","qaFilter","qaDesign","qaConsist") %in% data()$status$module ) ){
#             HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to perform the single-trial analysis specifying your input parameters under the 'Input' tabs.")) )
#           }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please identify trait-outliers in the phenotypic dataset before performing a single-trial analysis. Go to the 'QC & Transform' tab to do so. ")) ) }
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
#
#     # QA versions to use
#     observeEvent(c(data()), {
#       req(data())
#       dtSta <- data()
#       dtSta <- dtSta$status
#       dtSta <- dtSta[which(dtSta$module %in% c("qaRaw", "qaFilter", "qaMb", "qaDesign","qaConsist")),]
#       traitsSta <- unique(dtSta$analysisId)
#       if(length(traitsSta) > 0){names(traitsSta) <- as.POSIXct(traitsSta, origin="1970-01-01", tz="GMT")}
#       updateSelectInput(session, "version2Sta", choices = traitsSta)
#     })
#     # genetic evaluation unit
#     observe({
#       req(data())
#       req(input$version2Sta)
#       genetic.evaluation <- c("designation", "mother","father")
#       updateSelectInput(session, "genoUnitSta",choices = genetic.evaluation, selected = "designation")
#     })
#     # traits
#     observeEvent(c(data(),input$version2Sta,input$genoUnitSta), {
#       req(data())
#       req(input$version2Sta)
#       req(input$genoUnitSta)
#       dtSta <- data()
#       dtSta <- dtSta$modifications$pheno
#       dtSta <- dtSta[which(dtSta$analysisId %in% input$version2Sta),] # only traits that have been QA
#       traitsSta <- unique(dtSta$trait)
#       updateSelectInput(session, "trait2Sta", choices = traitsSta, selected = traitsSta)
#     })
#     # fixed effect covariates
#     observeEvent(c(data(),input$version2Sta,input$genoUnitSta, input$trait2Sta), {
#       req(data())
#       req(input$version2Sta)
#       req(input$genoUnitSta)
#       req(input$trait2Sta)
#       dtSta <- data()
#       dtSta <- dtSta$modifications$pheno # only traits that have been QA
#       dtSta <- dtSta[which(dtSta$analysisId %in% input$version2Sta),]
#       traitsSta <- unique(dtSta$trait)
#       '%!in%' <- function(x,y)!('%in%'(x,y))
#       updateSelectInput(session, "fixedTermSta2", choices = traitsSta[traitsSta%!in%input$trait2Sta])
#     })
#     # reactive table for trait family distributions
#     dtDistTrait = reactive({
#       req(data())
#       req(input$trait2Sta)
#       dtSta <- data()
#       dtSta <- dtSta$data$pheno
#       traitNames = input$trait2Sta
#       mm = matrix(0,nrow = 8, ncol = length(traitNames));
#       rownames(mm) <- c(
#         "gaussian(link = 'identity')",
#         "quasi(link = 'identity', variance = 'constant')",
#         "binomial(link = 'logit')",
#         "Gamma(link = 'inverse')",
#         "inverse.gaussian(link = '1/mu^2')",
#         "poisson(link = 'log')",
#         "quasibinomial(link = 'logit')",
#         "quasipoisson(link = 'log')"
#       );
#       colnames(mm) <- traitNames
#       dtProvTable = as.data.frame(mm);  colnames(dtProvTable) <- traitNames
#       return(dtProvTable)
#     })
#     xx = reactiveValues(df = NULL)
#     observe({
#       df <- dtDistTrait()
#       xx$df <- df
#     })
#     output$traitDistSta = DT::renderDT(xx$df,
#                                        selection = 'none',
#                                        editable = TRUE,
#                                        options = list(paging=FALSE,
#                                                       searching=FALSE,
#                                                       initComplete = I("function(settings, json) {alert('Done.');}")
#                                        ), server = FALSE
#     )
#
#     ##
#     proxy = DT::dataTableProxy('traitDistSta')
#
#     observeEvent(input$traitDistSta_cell_edit, {
#       info = input$traitDistSta_cell_edit
#       utils::str(info)
#       i = info$row
#       j = info$col
#       v = info$value
#       xx$df[i, j] <- isolate(DT::coerceValue(v, xx$df[i, j]))
#     })
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
#           if(nLevelsCheck1 <= 1){
#             X1 <- matrix(ifelse(is.na(zz$inputId),0,1),nrow=length(zz$inputId),1); colnames(X1) <- as.character(na.omit(unique(c(zz$outputId))))
#           }else{X1 <- model.matrix(~as.factor(outputId)-1, data=zz); colnames(X1) <- levels(as.factor(zz$outputId))}
#           if(nLevelsCheck2 <= 1){
#             X2 <- matrix(ifelse(is.na(zz$inputId),0,1),nrow=length(zz$inputId),1); colnames(X2) <- as.character(na.omit(unique(c(zz$inputId))))
#           }else{X2 <- model.matrix(~as.factor(inputId)-1, data=zz); colnames(X2) <- levels(as.factor(zz$inputId))}
#           mynames <- unique(na.omit(c(zz$outputId,zz$inputId)))
#           X <- matrix(0, nrow=nrow(zz), ncol=length(mynames)); colnames(X) <- as.character(mynames)
#           if(!is.null(X1)){X[,colnames(X1)] <- X1}
#           if(!is.null(X2)){X[,colnames(X2)] <- X2}
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
#           ggnetwork::geom_nodes(ggplot2::aes(color = family), alpha = 0.5, size=5 ) +
#           ggnetwork::geom_nodelabel_repel(ggplot2::aes(color = family, label = vertex.names ),
#                                           fontface = "bold", box.padding = ggnetwork::unit(1, "lines")) +
#           ggnetwork::theme_blank() + ggplot2::ggtitle("Network plot of current analyses available")
#       }
#     })
#     ## render the data to be analyzed
#     observeEvent(data(),{
#       if(sum(data()$status$module %in% c("qaRaw", "qaMb", "qaFilter","qaDesign","qaConsist" )) != 0) {
#         ## render status
#         output$statusSta <-  DT::renderDT({
#           req(data())
#           req(input$version2Sta)
#           dtSta <- data() # dtSta<- result
#           ### change column names for mapping
#           paramsPheno <- data()$modeling
#           paramsPheno <- paramsPheno[which(paramsPheno$analysisId %in% input$version2Sta),, drop=FALSE]
#           paramsPheno$analysisId <- as.POSIXct(paramsPheno$analysisId, origin="1970-01-01", tz="GMT")
#           DT::datatable(paramsPheno, extensions = 'Buttons',
#                         options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
#                                        lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All'))),
#                         caption = htmltools::tags$caption(
#                           style = 'color:cadetblue', #caption-side: bottom; text-align: center;
#                           htmltools::em('Past modeling parameters from QA stamp selected.')
#                         )
#           )
#         }, server = FALSE)
#         ## render summaries
#         output$summariesSta <-  DT::renderDT({
#           req(data())
#           req(input$version2Sta)
#           req(input$feature)
#           dtSta <- data() # dtSta<- result
#           ### change column names for mapping
#           paramsPheno <- data()$metadata$pheno
#           paramsPheno <- paramsPheno[paramsPheno$parameter != "trait",]
#           paramsPheno <- paramsPheno[which(!duplicated(paramsPheno$value)),]
#           colnames(dtSta$data$pheno) <- cgiarBase::replaceValues(colnames(dtSta$data$pheno), Search = paramsPheno$value, Replace = paramsPheno$parameter )
#           paramsPed <- data()$metadata$pedigree
#           colnames(dtSta$data$pedigree) <- cgiarBase::replaceValues(colnames(dtSta$data$pedigree), Search = paramsPed$value, Replace = paramsPed$parameter )
#           ### avoid columns mother and father in the pehnotype file
#           '%!in%' <- function(x,y)!('%in%'(x,y))
#           dtSta$data$pheno <- dtSta$data$pheno[,which(colnames(dtSta$data$pheno) %!in% c("mother","father") )]
#           ## merge data
#           dtSta <- merge(dtSta$data$pheno, dtSta$data$pedigree, by="designation") # merge mother and father info in the pheno data frame
#           dtStaList <- split(dtSta, dtSta[,input$feature]) # split info by environment
#           dtStaListRes <- list()
#           for(i in 1:length(dtStaList)){
#             dtStaListRes[[i]] <- as.data.frame(as.table(apply(dtStaList[[i]][,intersect( c("designation","mother","father"), colnames(dtSta) ), drop= FALSE],2, function(x){length(na.omit(unique(x)))})))
#             dtStaListRes[[i]][,input$feature] <- names(dtStaList)[i]
#             if("mother" %!in% dtStaListRes[[i]]$Var1){
#               prov <- dtStaListRes[[i]]
#               prov$Var1 <- "mother"; prov$Freq <- 0
#             }else{prov <- NULL}
#             if("father" %!in% dtStaListRes[[i]]$Var1){
#               prov2 <- dtStaListRes[[i]]
#               prov2$Var1 <- "father"; prov2$Freq <- 0
#             }else{prov2 <- NULL}
#             if(!is.null(prov) | !is.null(prov2)){dtStaListRes[[i]] <- rbind(dtStaListRes[[i]], prov,prov2)}
#           }
#           dtSta <- do.call(rbind, dtStaListRes)
#
#           colnames(dtSta)[1:2] <- c("geneticUnit", "numberOfUnits")
#           dtSta <- dtSta[with(dtSta, order(geneticUnit)), ]; rownames(dtSta) <- NULL
#           DT::datatable(dtSta, extensions = 'Buttons',
#                         options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
#                                        lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All'))),
#                         caption = htmltools::tags$caption(
#                           style = 'color:cadetblue', #caption-side: bottom; text-align: center;
#                           htmltools::em('Summary of number of individuals, mothers and fathers available in the dataset.')
#                         )
#           )
#         }, server = FALSE)
#         ## render designs
#         output$dtFieldTraC <-  DT::renderDT({
#           req(data())
#           req(input$version2Sta)
#           object <- data()
#           dtProv = object$data$pheno
#           paramsPheno <- object$metadata$pheno
#           if(!is.null(paramsPheno)){
#             paramsPheno <- paramsPheno[which(paramsPheno$parameter != "trait"),]
#             colnames(dtProv) <- cgiarBase::replaceValues(colnames(dtProv), Search = paramsPheno$value, Replace = paramsPheno$parameter )
#             dtProvNames <- c("row","col","rep","iBlock")
#             envCol <- paramsPheno[which(paramsPheno$parameter == "environment"),"value"]
#             if(length(envCol) > 0){
#               fieldNames <- as.character(unique(dtProv[,"environment"]))
#               spD <- split(dtProv,dtProv[,"environment"])
#               presentFactors <- paramsPheno$parameter[which(paramsPheno$parameter %in% dtProvNames)]
#               presentFactorsPerField <- lapply(spD, function(x){
#                 apply(x[,presentFactors, drop=FALSE], 2, function(y){length(unique(y))})
#               })
#               presentFactorsPerField <- do.call(rbind, presentFactorsPerField)
#               dtProvTable = as.data.frame(presentFactorsPerField);  rownames(dtProvTable) <- fieldNames
#               DT::datatable(dtProvTable, extensions = 'Buttons',
#                             options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
#                                            lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All'))),
#                             caption = htmltools::tags$caption(
#                               style = 'color:cadetblue', #caption-side: bottom; text-align: center;
#                               htmltools::em('Experimental design factos present per environment.')
#                             )
#               )
#             }
#           }
#         }, server = FALSE)
#         ## render plot of trait distribution
#         observeEvent(c(data(),input$version2Sta), { # update trait
#           req(data())
#           req(input$version2Sta)
#           paramsPheno <- data()$metadata$pheno
#           paramsPheno <- paramsPheno[which(!duplicated(paramsPheno$value)),]
#           paramsPheno <- paramsPheno[which(paramsPheno$parameter != "trait"),]
#           traitsSta <- setdiff(paramsPheno$parameter, c("trait","designation"))
#           updateSelectInput(session, "feature", choices = traitsSta)
#         })
#         observeEvent(c(data(),input$version2Sta), { # update trait
#           req(data())
#           req(input$version2Sta)
#           dtSta <- data()
#           dtSta <- dtSta$modifications$pheno
#           dtSta <- dtSta[which(dtSta$analysisId %in% input$version2Sta),] # only traits that have been QA
#           traitsSta <- unique(dtSta$trait)
#           updateSelectInput(session, "trait3Sta", choices = traitsSta)
#         })
#         output$plotPredictionsCleanOut <- plotly::renderPlotly({ # shiny::renderPlot({ #
#           req(data())
#           req(input$version2Sta)
#           req(input$trait3Sta)
#           mydata <- data()$data$pheno
#           ### change column names for mapping
#           paramsPheno <- data()$metadata$pheno
#           paramsPheno <- paramsPheno[which(paramsPheno$parameter != "trait"),]
#           colnames(mydata) <- cgiarBase::replaceValues(colnames(mydata), Search = paramsPheno$value, Replace = paramsPheno$parameter )
#           ###
#           mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
#           if(mappedColumns == 3){ # all required columns are present
#             mydata$rowindex <- 1:nrow(mydata)
#             mydata[, "environment"] <- as.factor(mydata[, "environment"]);mydata[, "designation"] <- as.factor(mydata[, "designation"])
#             mo <- data()$modifications$pheno
#             mo <- mo[which(mo[,"trait"] %in% input$trait3Sta),]
#             mydata$color <- "valid"
#             if(nrow(mo) > 0){mydata$color[which(mydata$rowindex %in% unique(mo$row))]="tagged"}
#             mydata$predictedValue <- mydata[,input$trait3Sta]
#             mydata <- mydata[,which(!duplicated(colnames(mydata)))]
#             p <- ggplot2::ggplot(mydata, ggplot2::aes(x=predictedValue, fill=environment)) +
#               ggplot2::geom_histogram(aes(y=after_stat(density)), position="identity", alpha=input$transparency ) +
#               # ggplot2::geom_boxplot(fill='#A4A4A4', color="black", notch = TRUE, outliers = FALSE)+
#               ggplot2::theme_classic() + ggplot2::ggtitle("Histogram of trait dispersion by environment") +
#               # ggplot2::geom_jitter(ggplot2::aes(colour = color), alpha = 0.4) +
#               ggplot2::ylab("Density") + ggplot2::xlab("Trait value") + ggplot2::scale_color_brewer(palette="Accent")
#               # ggplot2::scale_color_manual(values = c(valid = "#66C2A5", tagged = "#FC8D62")) # specifying colors names avoids having valid points in orange in absence of potential outliers. With only colour = color, valid points are in orange in that case.
#             # if(input$checkbox){
#             #   p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1))
#             # }else{
#             #   p <- p + ggplot2::theme(axis.text.x = ggplot2::element_blank())
#             # }
#             # p
#             plotly::ggplotly(p)
#           }else{}
#         })
#         ## render raw data
#         output$phenoSta <-  DT::renderDT({
#           req(data())
#           dtSta <- data()
#           req(input$version2Sta)
#           dtSta <- dtSta$data$pheno
#           ### change column names for mapping
#           paramsPheno <- data()$metadata$pheno
#           paramsPheno <- paramsPheno[which(paramsPheno$parameter != "trait"),]
#           colnames(dtSta) <- cgiarBase::replaceValues(colnames(dtSta), Search = paramsPheno$value, Replace = paramsPheno$parameter )
#           ###
#           traitTypes <- unlist(lapply(dtSta,class))
#           numeric.output <- names(traitTypes)[which(traitTypes %in% "numeric")]
#           DT::formatRound(DT::datatable(dtSta, extensions = 'Buttons',
#                                         options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
#                                                        lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All'))),
#                                         caption = htmltools::tags$caption(
#                                           style = 'color:cadetblue', #caption-side: bottom; text-align: center;
#                                           htmltools::em('Raw phenotypic data to be used as input.')
#                                         )
#           ), numeric.output)
#         }, server = FALSE)
#       } else {
#         output$summariesSta <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
#         output$phenoSta <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
#         output$plotPredictionsCleanOut <- plotly::renderPlotly({NULL})
#       }
#     })
#     ## define function
#     my_sta <- ExtendedTask$new(function(input, data, xx) {
#       promises::future_promise({
#
#         myFamily = apply(xx$df,2,function(y){rownames(xx$df)[which(y > 0)[1]]})
#         dontHaveDist <- which(is.na(myFamily))
#         if(length(dontHaveDist) > 0){myFamily[dontHaveDist] <- "gaussian(link = 'identity')"}
#
#         result <- try(cgiarPipeline::staLMM(phenoDTfile = data, analysisId=input$version2Sta,
#                                             trait=input$trait2Sta, traitFamily = myFamily,
#                                             fixedTerm = input$fixedTermSta2,
#                                             returnFixedGeno=input$genoAsFixedSta, genoUnit = input$genoUnitSta,
#                                             verbose = input$verboseSta, maxit = input$maxitSta),
#                       silent=TRUE
#         )
#
#         return(output = result)
#       })
#     })
#     ## render result of "run" button click
#     observeEvent(input$runSta, {
#       # outSta <- eventReactive(input$runSta, {
#       req(data())
#       req(input$version2Sta)
#       req(input$trait2Sta)
#       req(input$genoUnitSta)
#       req(input$genoAsFixedSta)
#       req(input$verboseSta)
#       req(input$maxitSta)
#
#       shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
#       ui_inputs <- shiny::reactiveValuesToList(input)
#       xx_inputs <- shiny::reactiveValuesToList(xx)
#       data_obj  <- data()
#       my_sta$invoke(ui_inputs, data_obj, xx_inputs)
#
#     }) ## end eventReactive
#
#     output$outSta <- output$outSta2 <- renderPrint({
#
#       # run the modeling, but before test if qa/qc done
#       if(sum(data()$status$module %in% c("qaRaw","qaConsist","qaFilter","qaDesign")) == 0) {
#         output$qaQcStaInfo <- renderUI({
#           if (hideAll$clearAll){
#             return()
#           }else{
#             req(data())
#             HTML(as.character(div(style="color: brown;",
#                                   "Please perform QA/QC before conducting a Single-Trial Analysis."))
#             )
#           }
#
#         })
#       } else {
#         output$qaQcStaInfo <- renderUI({return(NULL)})
#         # save(dtSta, file = "./R/outputs/resultSta.RData")
#         result <- my_sta$result()
#         shinybusy::remove_modal_spinner()
#         if(!inherits(result,"try-error")) {
#           data(result) # update data with results
#           cat(paste("Single-trial analysis step with id:",as.POSIXct(result$status$analysisId[length(result$status$analysisId)], origin="1970-01-01", tz="GMT"),"saved. Please proceed to perform your multi-trial analysis using this time stamp."))
#           updateTabsetPanel(session, "tabsMain", selected = "outputTabs")
#
#           # predictions table
#           output$predictionsSta <-  DT::renderDT({
#             if(!inherits(result,"try-error") ){
#               # if ( hideAll$clearAll){
#               #   return()
#               # }else{
#               predictions <- result$predictions
#               predictions <- predictions[predictions$module=="sta",]
#               predictions$analysisId <- as.numeric(predictions$analysisId)
#               predictions <- predictions[!is.na(predictions$analysisId),]
#               current.predictions <- predictions[predictions$analysisId==max(predictions$analysisId),]
#               current.predictions <- subset(current.predictions, select = -c(module,analysisId))
#               numeric.output <- c("predictedValue", "stdError", "reliability")
#               DT::formatRound(DT::datatable(current.predictions, extensions = 'Buttons',
#                                             options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
#                                                            lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
#               ), numeric.output)
#               # }
#             }
#           }, server = FALSE)
#
#           # metrics table
#           output$metricsSta <-  DT::renderDT({
#             if(!inherits(result,"try-error") ){
#               # if ( hideAll$clearAll){
#               #   return()
#               # }else{
#               metrics <- result$metrics
#               metrics <- metrics[metrics$module=="sta",]
#               metrics$analysisId <- as.numeric(metrics$analysisId)
#               metrics <- metrics[!is.na(metrics$analysisId),]
#               current.metrics <- metrics[metrics$analysisId==max(metrics$analysisId),]
#               current.metrics <- subset(current.metrics, select = -c(module,analysisId))
#               numeric.output <- c("value", "stdError")
#               DT::formatRound(DT::datatable(current.metrics, extensions = 'Buttons',
#                                             options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
#                                                            lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
#               ), numeric.output)
#               # }
#             }
#           }, server = FALSE)
#
#           # modeling table
#           output$modelingSta <-  DT::renderDT({
#             if(!inherits(result,"try-error") ){
#               # if ( hideAll$clearAll){
#               #   return()
#               # }else{
#               modeling <- result$modeling
#               modeling <- modeling[modeling$module=="sta",]
#               modeling$analysisId <- as.numeric(modeling$analysisId)
#               modeling <- modeling[!is.na(modeling$analysisId),]
#               current.modeling <- modeling[modeling$analysisId==max(modeling$analysisId),]
#               current.modeling <- subset(current.modeling, select = -c(module,analysisId))
#               # current.modeling$analysisId <- as.POSIXct(current.modeling$analysisId, origin="1970-01-01", tz="GMT")
#               DT::datatable(current.modeling, extensions = 'Buttons',
#                             options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
#                                            lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
#               )
#               # }
#             }
#           }, server = FALSE)
#
#           ## dashboard STA
#           output$reportSta <- renderUI({
#             HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportSta.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
#           })
#
#           output$downloadReportSta <- downloadHandler(
#             filename = function() {
#               paste('my-report', sep = '.', switch(
#                 "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
#               ))
#             },
#             content = function(file) {
#               shinybusy::show_modal_spinner(spin = "fading-circle",
#                                             text = "Generating Report...")
#               src <- normalizePath(system.file("rmd","reportSta.Rmd",package="bioflow"))
#               src2 <- normalizePath('data/resultSta.RData')
#               # temporarily switch to the temp dir, in case you do not have write
#               # permission to the current working directory
#               owd <- setwd(tempdir())
#               on.exit(setwd(owd))
#               file.copy(src, 'report.Rmd', overwrite = TRUE)
#               file.copy(src2, 'resultSta.RData', overwrite = TRUE)
#               out <- rmarkdown::render('report.Rmd', params = list(toDownload=TRUE),switch(
#                 "HTML",
#                 # HTML = rmarkdown::html_document()
#                 HTML = rmdformats::robobook(toc_depth = 4)
#               ))
#               file.rename(out, file)
#               shinybusy::remove_modal_spinner()
#             }
#           )
#
#         }else{
#           cat(paste("Analysis failed with the following error message: \n\n",result[[1]]))
#
#           # else {
#           output$predictionsSta <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
#           output$metricsSta <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
#           output$modelingSta <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
#
#           # }
#         }
#         hideAll$clearAll <- FALSE
#       }
#
#
#     })
#   }) ## end moduleserver
# }
mod_staApp_server <- function(id,data){
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
    observeEvent(c(data(), input$version2Sta), {
      req(data())
      mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
      if(mappedColumns == 3 & length(input$version2Sta)>0 ){
        golem::invoke_js('showid', ns('holder1'))
        golem::invoke_js('showid', ns('holder2'))
      }else{
        golem::invoke_js('hideid', ns('holder1'))
        golem::invoke_js('hideid', ns('holder2'))
      }
    })
    ############################################################################
    # observeEvent(
    #   c(data(),input$version2Sta,input$genoUnitSta),
    #   if(length(input$genoUnitSta) > 0){ # added
    #     if ('mother' %in% input$genoUnitSta | 'father' %in% input$genoUnitSta | is.null(input$genoUnitSta) ){
    #       golem::invoke_js('showid', ns('geno_unit_holder'))
    #     }else { #
    #       golem::invoke_js('hideid', ns('geno_unit_holder'))
    #     }
    #   }else{golem::invoke_js('hideid', ns('geno_unit_holder'))}
    # )
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

    # QA versions to use
    observeEvent(c(data()), {
      req(data())
      dtSta <- data()
      dtSta <- dtSta$status
      dtSta <- dtSta[which(dtSta$module %in% c("qaRaw", "qaFilter", "qaMb", "qaDesign")),]
      traitsSta <- unique(dtSta$analysisId)
      if(length(traitsSta) > 0){
        if("analysisIdName" %in% colnames(dtSta)){
          names(traitsSta) <- paste(dtSta$analysisIdName, as.POSIXct(traitsSta, origin="1970-01-01", tz="GMT"), sep = "_")
        }else{
          names(traitsSta) <- as.POSIXct(traitsSta, origin="1970-01-01", tz="GMT")
        }
      }
      updateSelectInput(session, "version2Sta", choices = traitsSta)
    })
    # genetic evaluation unit
    # observe({
    #   req(data())
    #   req(input$version2Sta)
    #   genetic.evaluation <- c("designation", "mother","father")
    #   updateSelectInput(session, "genoUnitSta",choices = genetic.evaluation, selected = "designation")
    # })
    # traits
    observeEvent(c(data(),input$version2Sta), {
      req(data())
      req(input$version2Sta)
      # req(input$genoUnitSta)
      dtSta <- data()
      dtSta <- dtSta$modifications$pheno
      dtSta <- dtSta[which(dtSta$module %in% "qaRaw"),]
      if(nrow(dtSta) > 0){
        dtSta <- dtSta[which(dtSta$analysisId %in% input$version2Sta),] # only traits that have been QA
        traitsSta <- unique(dtSta$trait)
        updateSelectInput(session, "trait2Sta", choices = traitsSta, selected = traitsSta)
      }
    })
    ## pick unit
    observeEvent(c(data(),input$version2Sta,input$trait2Sta), {
      req(data())
      req(input$version2Sta)
      req(input$trait2Sta)
      dtSta <- data()
      ped <- dtSta$data$pedigree
      colnames(ped) <- cgiarBase::replaceValues(colnames(ped),dtSta$metadata$pedigree$value, dtSta$metadata$pedigree$parameter)
      checkUnits <- apply(ped[,intersect(colnames(ped),c("designation", "mother","father")), drop=FALSE], 2, function(f){length(unique(f))})
      genetic.evaluation <- names(which(checkUnits > 1))
      # genetic.evaluation <- c("designation", "mother","father")
      updateSelectInput(session, "genoUnitSta",choices = genetic.evaluation, selected = "designation")
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
                                       server = FALSE,
                                       options = list( #paging=FALSE,
                                         # searching=FALSE,
                                         scrollX = TRUE
                                         # initComplete = I("function(settings, json) {alert('Done.');}")
                                       )
    )

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
      if(sum(data()$status$module %in% c("qaRaw", "qaMb")) != 0) {
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
                                       lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All'))),
                        caption = htmltools::tags$caption(
                          style = 'color:cadetblue', #caption-side: bottom; text-align: center;
                          htmltools::em('Past modeling parameters from QA stamp selected.')
                        )
          )
        }, server = FALSE)
        ## render summaries
        output$summariesSta <-  DT::renderDT({
          req(data())
          req(input$version2Sta)
          req(input$feature)
          dtSta <- data() # dtSta<- result
          ### change column names for mapping
          paramsPheno <- data()$metadata$pheno
          paramsPheno <- paramsPheno[paramsPheno$parameter != "trait",]
          paramsPheno <- paramsPheno[which(!duplicated(paramsPheno$value)),]
          colnames(dtSta$data$pheno) <- cgiarBase::replaceValues(colnames(dtSta$data$pheno), Search = paramsPheno$value, Replace = paramsPheno$parameter )
          ### avoid columns mother and father in the pehnotype file
          '%!in%' <- function(x,y)!('%in%'(x,y))

          if(!is.null(dtSta$data$pedigree)){
            paramsPed <- data()$metadata$pedigree
            colnames(dtSta$data$pedigree) <- cgiarBase::replaceValues(colnames(dtSta$data$pedigree), Search = paramsPed$value, Replace = paramsPed$parameter )
            myped <- unique(dtSta$data$pedigree[,c("designation","mother","father")])
            mydata <- merge(dtSta$data$pheno, myped, by="designation", all.x = TRUE)
          }else{
            mydata <- dtSta$data$pheno
          }

          if(!is.null(dtSta$data$pedigree)){
            m1 <- aggregate(as.formula(paste("designation~" ,input$feature)), FUN=function(x){length(unique(x))}, data=mydata)
            m2 <- aggregate(as.formula(paste("mother~" ,input$feature)), FUN=function(x){length(unique(x))}, data=mydata)
            m3 <- aggregate(as.formula(paste("father~" ,input$feature)), FUN=function(x){length(unique(x))}, data=mydata)
            resTable <- cbind(m1,m2[,2,drop=FALSE],m3[,2,drop=FALSE])
          }else{
            resTable <- aggregate(as.formula(paste("designation~" ,input$feature)), FUN=function(x){length(unique(x))}, data=mydata)
          }


          DT::datatable(resTable, extensions = 'Buttons',
                        options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                       lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All'))),
                        caption = htmltools::tags$caption(
                          style = 'color:cadetblue', #caption-side: bottom; text-align: center;
                          htmltools::em('Summary of number of individuals, mothers and fathers available in the dataset.')
                        )
          )

        }, server = FALSE)
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
                                           lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All'))),
                            caption = htmltools::tags$caption(
                              style = 'color:cadetblue', #caption-side: bottom; text-align: center;
                              htmltools::em('Experimental design factos present per environment.')
                            )
              )
            }
          }
        }, server = FALSE)
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
        output$plotPredictionsCleanOut <- plotly::renderPlotly({ # shiny::renderPlot({ #
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
            mydata$color <- "valid"
            if(nrow(mo) > 0){mydata$color[which(mydata$rowindex %in% unique(mo$row))]="tagged"}
            mydata$predictedValue <- mydata[,input$trait3Sta]
            mydata <- mydata[,which(!duplicated(colnames(mydata)))]
            p <- ggplot2::ggplot(mydata, ggplot2::aes(x=predictedValue, fill=environment)) +
              ggplot2::geom_histogram(ggplot2::aes(y=ggplot2::after_stat(density)), position="identity", alpha=input$transparency ) +
              # ggplot2::geom_boxplot(fill='#A4A4A4', color="black", notch = TRUE, outliers = FALSE)+
              ggplot2::theme_classic() + ggplot2::ggtitle("Histogram of trait dispersion by environment") +
              # ggplot2::geom_jitter(ggplot2::aes(colour = color), alpha = 0.4) +
              ggplot2::ylab("Density") + ggplot2::xlab("Trait value") + ggplot2::scale_color_brewer(palette="Accent")
            # ggplot2::scale_color_manual(values = c(valid = "#66C2A5", tagged = "#FC8D62")) # specifying colors names avoids having valid points in orange in absence of potential outliers. With only colour = color, valid points are in orange in that case.
            # if(input$checkbox){
            #   p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1))
            # }else{
            #   p <- p + ggplot2::theme(axis.text.x = ggplot2::element_blank())
            # }
            # p
            plotly::ggplotly(p)
          }else{}
        })
        # render fieldGrid heatmap
        output$plotFieldGrid <- plotly::renderPlotly({ # shiny::renderPlot({ #
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

            mappedCoordColumns <- length(which(c("row","col") %in% paramsPheno$parameter))
            if(mappedCoordColumns == 2){

              mydata$value <- mydata[, input$trait3Sta]
              maxVal <- max(mydata$value, na.rm = TRUE) # get the maximum value found in the matrix of connectivity
              minVal <- min(mydata$value, na.rm = TRUE) # get the maximum value found in the matrix of connectivity
              meanVal <- mean(mydata$value, na.rm = TRUE) # get the maximum value found in the matrix of connectivity

              if(!is.na(meanVal)){ # there is data
                pf <- ggplot2::ggplot(data = mydata, ggplot2::aes(row, col, fill = value))+
                  ggplot2::geom_tile(color = "white")+
                  ggplot2::scale_fill_gradient2(low = "#E46726", high = "#038542", mid = "gold",
                                                midpoint = meanVal, limit = c(minVal,maxVal), space = "Lab",
                                                name=input$trait3Sta) +
                  ggplot2::theme_minimal()+
                  ggplot2::ylab("") + ggplot2::xlab("") +
                  ggplot2::ggtitle("Field view")  +
                  ggplot2::facet_wrap(~environment, scales = "free") +
                  ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank() )
                plotly::ggplotly(pf)
              }
            }

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
                                                       lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All'))),
                                        caption = htmltools::tags$caption(
                                          style = 'color:cadetblue', #caption-side: bottom; text-align: center;
                                          htmltools::em('Raw phenotypic data to be used as input.')
                                        )
          ), numeric.output)
        }, server = FALSE)
      } else {
        output$summariesSta <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
        output$phenoSta <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
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
                                            trait=input$trait2Sta,
                                            traitFamily = myFamily,
                                            fixedTerm = input$fixedTermSta2,
                                            returnFixedGeno=input$genoAsFixedSta, genoUnit = input$genoUnitSta,
                                            verbose = input$verboseSta, maxit = input$maxitSta),
                      silent=TRUE
        )
        if(!inherits(result,"try-error")) {
          if("analysisIdName" %in% colnames(result$status)){result$status$analysisIdName[nrow(result$status)] <- input$analysisIdName}
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
        }, server = FALSE)

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
        }, server = FALSE)

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
        }, server = FALSE)
        ## report STA
        output$reportSta <- renderUI({
          HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportSta.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
        })

        output$downloadReportSta <- downloadHandler(
          filename = function() {
            paste(paste0('sta_dashboard_',gsub("-", "", as.integer(Sys.time()))), sep = '.', switch(
              "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
            ))
          },
          content = function(file) {
            shinybusy::show_modal_spinner(spin = "fading-circle", text = "Generating Report...")

            src <- normalizePath(system.file("rmd","reportSta.Rmd",package="bioflow"))
            src2 <- normalizePath('data/resultSta.RData')

            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))

            file.copy(src, 'report.Rmd', overwrite = TRUE)
            file.copy(src2, 'resultSta.RData', overwrite = TRUE)

            out <- rmarkdown::render('report.Rmd', params = list(toDownload=TRUE),switch(
              "HTML",
              # HTML = rmarkdown::html_document()
              HTML = rmdformats::robobook(toc_depth = 4)
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
        output$predictionsSta <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
        output$metricsSta <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
        output$modelingSta <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
      }

      hideAll$clearAll <- FALSE

    }) ## end eventReactive

    output$outSta <- output$outSta2 <- renderPrint({
      outSta()
    })

    output$ebsReportButton <- renderUI({
      query <- parseQueryString(session$clientData$url_search)

      # check if task id exists, and verify (sanitize) this md5 parameter ;-)
      if (is.character(query$task) &&
          is.character(query$domain) &&
          grepl("^[a-f0-9]{32}$", query$task) &&
          grepl("^[a-z_0-9\\.\\-]+$", query$domain)) {

        s3 <- paws::s3()

        bucket_name <- "ebs-bioflow"
        s3_object_path <- paste0(query$domain, "/", query$task, ".RData")

        tryCatch({
          s3_object_head <- s3$head_object(Bucket = bucket_name, Key = s3_object_path)
          actionButton(ns("ebsStaReport"), "Submit Results to EBS")
        }, error = function(e) {
          # no such file on the S3 bucket clipboard
          NULL
        })
      } else {
        # parameters does not match the expected format
        NULL
      }
    })

    observeEvent(input$ebsStaReport, {
      req(data())

      query <- parseQueryString(session$clientData$url_search)

      # check if task id exists, and verify (sanitize) this md5 parameter ;-)
      if (is.character(query$task) &&
          is.character(query$domain) &&
          grepl("^[a-f0-9]{32}$", query$task) &&
          grepl("^[a-z_0-9\\.\\-]+$", query$domain)) {

        shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')

        result <- data()
        s3 <- paws::s3()

        # set S3 bucket parameters
        bucket_name <- "ebs-bioflow"
        s3_object_path <- paste0(query$domain, "/", query$task, ".RData")

        tryCatch({
          # update S3 metadata
          s3_object_head <- s3$head_object(Bucket = bucket_name, Key = s3_object_path)
          s3_object_metadata <- s3_object_head$Metadata
          s3_object_metadata[["Upload-Origin"]] <- "bioflow"

          temp_file <- paste0(tempdir(), "/", query$task, ".RData")
          save(result, file = temp_file)

          # upload data object to S3 bucket
          s3$put_object(
            Body = temp_file,
            Bucket = bucket_name,
            Key = s3_object_path,
            Metadata = s3_object_metadata
          )

          shinybusy::remove_modal_spinner()
          shinyalert::shinyalert(title = "Success!", text = "Analysis results successfully submitted to EBS.", type = "success")
        }, error = function(e) {
          shinybusy::remove_modal_spinner()
          shinyalert::shinyalert(title = "Failed!", text = e$message, type = "error")
        })
      }
    })

  }) ## end moduleserver
}

## To be copied in the UI
# mod_staApp_ui("staApp_1")

## To be copied in the server
# mod_staApp_server("staApp_1")

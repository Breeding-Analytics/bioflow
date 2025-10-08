#' qaPhenoApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_qaDataConsistApp_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(HTML("
          .selectize-input.disabled {
            background-color: white !important; /* Change background color */
            color: black !important; /* Change text color */
            opacity: 1 !important; /* Prevent default opacity reduction */
          }

          #ruleTab li:not(.active) a {
            color: white !important;
          }

          #ruleTab li:not(.active):hover a {
            color: #000 !important;
          }
        "))
    ),
    shiny::mainPanel(width = 12,
                     tabsetPanel( id=ns("tabsMain"),
                                  type = "tabs",

                                  tabPanel(div(icon("book"), "Information") ,
                                           br(),
                                           column(width = 6,
                                                  h1(strong(span("Data Consistency Check Module", tags$a(href="https://www.youtube.com/watch?v=X8lYQ8_LmSg&list=PLZ0lafzH_UmclOPifjCntlMzysEB2_2wX&index=4", icon("youtube") , target="_blank")  ,style="color:darkcyan"))),
                                                  h2(strong("Data Status (wait to be displayed):")),
                                                  uiOutput(ns("warningMessage")),
                                                  tags$br(),
                                                  # column(width=4, tags$br(),
                                                  shinyWidgets::prettySwitch( inputId = ns('launch'), label = "Load example dataset", status = "success"),
                                                  # ),
                                                  tags$br(),
                                                  img(src = "www/qaConsistency.png", height = 200, width = 470), # add an image
                                           ),
                                           column(width = 6,
                                                  tags$body(
                                                    h2(strong("Details")),
                                                    p("The first step in genetic evaluation is to ensure that input phenotypic records are of good quality.
                                                             This option aims to allow users to tag impossible values based on minimum, maximum and acceptable values.
                                The way arguments are used is the following:"),
                                                    p(strong("Trait(s) to QA - Numerical.-")," trait(s) to apply the possible range values."),
                                                    p(strong("Trait type.-")," if trait is numerical, define whether it is an integer, proportion or continuous."),
                                                    p(strong("Minimum.-")," determines the lowest possible value for trait."),
                                                    p(strong("Maximum.-")," determines the highest possible value for trait."),
                                                    p(strong("Trait(s) to QA - Categorical.-")," trait(s) to apply the possible list of values."),
                                                    p(strong("Categories.-")," determines the list of possible values for trait."),
                                                    p(strong("Comparative.-")," rule comparing two traits."),
                                                    p(strong("Condition.-")," rule which sets a condition for trait value(s)."),
                                                    h2(strong("References")),
                                                    # p("Tukey, J. W. (1977). Exploratory Data Analysis. Section 2C."),
                                                    # column(width = 12, shiny::plotOutput(ns("plotDataDependencies")), ),
                                                  )
                                           ),
                                  ),
                                  tabPanel(div(icon("arrow-right-to-bracket"), "Input steps"),
                                           tabsetPanel(
                                             tabPanel(div(icon("dice-one"), "Set ranges (optional)", icon("arrow-right") ), #icon = icon("dice-one"),
                                                      br(),
                                                      column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                             column(width=7, style = "background-color:grey; color: #FFFFFF",
                                                                    column(width=12, selectInput(ns("traitOutqPhenoMultipleNum"),
                                                                                                 label = tags$span(
                                                                                                   "Trait(s) to QA - Numerical",
                                                                                                   tags$i(
                                                                                                     class = "glyphicon glyphicon-info-sign",
                                                                                                     style = "color:#FFFFFF",
                                                                                                     title = "Only traits matched during the data upload will show up. Select traits that are numerical."
                                                                                                   )
                                                                                                 ),
                                                                                                 choices = NULL, multiple = TRUE) ),
                                                                    column(width=12,
                                                                           fluidRow(
                                                                             column(width=3, uiOutput(ns('traitOutqPhenoDefNum'))),
                                                                             column(width=3, uiOutput(ns('traitOutqPhenoType'))),
                                                                             column(width=3, uiOutput(ns('traitOutqPhenoMin'))),
                                                                             column(width=3, uiOutput(ns('traitOutqPhenoMax'))),
                                                                           ),
                                                                    ),
                                                             ),
                                                             column(width=5, style = "background-color:grey; color: #FFFFFF",
                                                                    column(width=12, selectInput(ns("traitOutqPhenoMultipleCat"),
                                                                                                 label = tags$span(
                                                                                                   "Trait(s) to QA - Categorical",
                                                                                                   tags$i(
                                                                                                     class = "glyphicon glyphicon-info-sign",
                                                                                                     style = "color:#FFFFFF",
                                                                                                     title = "Only traits matched during the phenotypic data upload will show up. Select traits that are categorical."
                                                                                                   )
                                                                                                 ),
                                                                                                 choices = NULL, multiple = TRUE) ),
                                                                    column(width=12,
                                                                           fluidRow(
                                                                             column(width=4, uiOutput(ns('traitOutqPhenoDefCat'))),
                                                                             column(width=8, uiOutput(ns('traitOutqPhenoCat'))),
                                                                           ),
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
                                                                          column(width=12,
                                                                                 br(),
                                                                                 tags$span(id = ns('holder'),
                                                                                           column(width=4, selectInput(ns("traitOutqPheno"), "Trait to visualize", choices = NULL, multiple = FALSE) ),
                                                                                 ),
                                                                                 column(width=12, shiny::plotOutput(ns("plotPredictionsCleanOut")) ), # plotly::plotlyOutput(ns("plotPredictionsCleanOut")),
                                                                          ),

                                                      ),
                                             ),
                                             tabPanel(div(icon("dice-two"), "Set rules (optional)", icon("arrow-right") ), #icon = icon("dice-one"),
                                                      br(),
                                                      column(width = 12, style = "background-color:grey; color: #FFFFFF",
                                                             tags$div(tags$h5(strong("To add a rule, click the 'Add' button. To remove a rule,
                                                           select the rule from the table then click the 'Delete' button. When you upload a rule, it will delete
                                                           previously added rules."), style = "color: #FFFFFF;")),
                                                             tags$div(tags$h5(strong("Note: The rules to be set are those that specify which data should be
                                                                                            excluded from the analysis."), style = "color: #FFFFFF;")),
                                                             br(),
                                                             column(width = 6, style = "color: #000000",
                                                                    tabsetPanel(id = "ruleTab",
                                                                                tabPanel("Upload",
                                                                                         br(),
                                                                                         fileInput(inputId = ns('rule_file'), multiple = FALSE,
                                                                                                   label   = NULL, width   = '400px',
                                                                                                   accept  = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
                                                                                         ),
                                                                                         br()
                                                                                ),
                                                                                tabPanel("Comparative",
                                                                                         inputPanel(selectInput(ns('filt_param1a'), 'Trait1:', choices = NULL),
                                                                                                    selectInput(ns('filt_op1'), 'Filter:', choices = c(">", ">=", "<", "<=")),
                                                                                                    selectInput(ns('filt_param1b'), 'Trait2:', choices = NULL)
                                                                                         ),
                                                                                         actionButton(ns('add_filt1'), 'Add'),
                                                                                         br()
                                                                                ),
                                                                                tabPanel("Condition",
                                                                                         inputPanel(selectInput(ns('filt_param2'), 'Trait1:', choices = NULL),
                                                                                                    selectInput(ns('filt_op2'), 'Filter:', choices = c("==", "!=", ">", ">=", "<", "<=")),
                                                                                                    numericInput(ns('filt_tresh2'), 'Value:', value = NULL)
                                                                                         ),
                                                                                         actionButton(ns('add_filt2'), 'Add'),
                                                                                         br()
                                                                                ),
                                                                                br()
                                                                    ),
                                                             ),
                                                             column(width = 6,
                                                                    tags$div(tags$h5(strong("Comparative Rules"), style = "color: #FFFFFF;")),
                                                                    column(width = 12, style = "background-color:#FFFFFF; color: #000000",
                                                                           uiOutput(ns('filt_seq1_params')),
                                                                           br()
                                                                    ),
                                                                    column(width = 12, shiny::tags$p(style="font-size: 1px;")),
                                                                    actionButton(ns('del_filt1'), 'Delete'),
                                                                    column(width = 12),
                                                                    tags$div(tags$h5(strong("Condition Rules"), style = "color: #FFFFFF;")),
                                                                    column(width = 12, style = "background-color:#FFFFFF; color: #000000",
                                                                           uiOutput(ns('filt_seq2_params')),
                                                                           br()
                                                                    ),
                                                                    column(width = 12, shiny::tags$p(style="font-size: 1px;")),
                                                                    actionButton(ns('del_filt2'), 'Delete'),
                                                             ),
                                                             br(),
                                                      ),
                                                      column(width=12),
                                                      shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                          column(width=12,
                                                                                 hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                                 h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameters to be specified in the grey boxes above.", style="color:green"))),
                                                                                 hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                          ),
                                                                          column(width=12,
                                                                                 br(),
                                                                                 tags$span(id = ns('holder'),
                                                                                           column(width=4, selectInput(ns("traitOutqPheno2"), "Trait to visualize", choices = NULL, multiple = FALSE) ),
                                                                                 ),
                                                                                 column(width=12, shiny::plotOutput(ns("plotPredictionsCleanOut2")) ),
                                                                          ),

                                                      ),
                                             ),
                                             tabPanel(div(icon("dice-three"), "View", icon("arrow-right") ), #icon = icon("dice-one"),
                                                      br(),
                                                      column(width = 12, style = "background-color:grey; color: #FFFFFF",
                                                             tags$div(tags$h5(strong("To load impossible values based on set ranges and rules, click the 'Load' button.
                                                             To exclude an impossible value in the analysis, select the row(s) then click the 'Exclude' button."), style = "color: #FFFFFF;")),
                                                             column(width = 12,
                                                                    downloadButton(ns("download_filt"), "Download Ranges/Rules")),
                                                             column(width = 12,
                                                                    tags$div(tags$h5(strong("Impossible Values"), style = "color: #FFFFFF;")),
                                                                    column(width = 12, style = "background-color:#FFFFFF; color: #000000",
                                                                           uiOutput(ns('filt_all')),
                                                                           br()
                                                                    ),
                                                                    column(width = 12, shiny::tags$p(style="font-size: 1px;")),
                                                                    actionButton(ns('load_filt3'), 'Load'),
                                                                    br()
                                                             ),
                                                             column(width = 12, shiny::tags$p()),
                                                      ),
                                             ),
                                             tabPanel(div( icon("dice-four"), "Run analysis" ), # icon = icon("dice-two"),
                                                      br(),
                                                      column(width=12,style = "background-color:grey; color: #FFFFFF",
                                                             column(width=3, tags$div(textInput(ns("analysisIdName"), label = tags$span(
                                                               "Analysis Name (optional)", tags$i( class = "glyphicon glyphicon-info-sign", style = "color:#FFFFFF",
                                                                                                   title = "An optional name for the analysis besides the timestamp if desired.") ), #width = "100%",
                                                               placeholder = "(optional name)") ) ),
                                                             column(width=3,
                                                                    br(),
                                                                    actionButton(ns("runQaDataCons"), "Tag impossible values", icon = icon("play-circle")),
                                                                    br(),
                                                                    br(),
                                                             ),
                                                      ),
                                                      textOutput(ns("outQaDataCons")),
                                                      # fluidRow(column(3, verbatimTextOutput(ns("value"))))
                                             ),
                                           ) # end of tabset
                                  ),# end of input panel

                                  tabPanel(div(icon("arrow-right-from-bracket"), "Output tabs" ) , value = "outputTabs",
                                           tabsetPanel(
                                             tabPanel("Dashboard", icon = icon("file-image"),
                                                      br(),
                                                      textOutput(ns("outQaDataCons2")),
                                                      br(),
                                                      actionButton(ns("renderReportQaDataCons"), "Download dashboard", icon = icon("download")),
                                                      downloadButton(ns("downloadReportQaDataCons"), "Download dashboard", style = "visibility:hidden;"),
                                                      br(),
                                                      uiOutput(ns('reportQaDataCons'))
                                             ),
                                           ),
                                  ),
                     )) # end mainpanel

  )
}

#' qaRawApp Server Functions
#'
#' @noRd
mod_qaDataConsistApp_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    '%!in%' <- function(x,y)!('%in%'(x,y))

    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })
    ############################################################################
    # show shinyWidgets until the user can use the module
    observeEvent(c(data()), {
      req(data())
      mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
      if(mappedColumns == 3){
        golem::invoke_js('showid', ns('holder'))
      }else{
        golem::invoke_js('hideid', ns('holder'))
      }
    })
    # warning message
    output$warningMessage <- renderUI(
      if(is.null(data())){
        HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your phenotypic data using the 'Data Retrieval' tab.")) )
      }else{ # data is there
        mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
        if(mappedColumns == 3){ HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to identify impossible values by specifying your input parameters under the 'Input' tabs.")) )
        }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please make sure that you have computed the 'environment' column, and that column 'designation' and \n at least one trait have been mapped using the 'Data Retrieval' tab.")) )
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
        utils::data(DT_example, package = "cgiarPipeline")
        if(!is.null(result$data)){tmp$data <- result$data}
        if(!is.null(result$metadata)){tmp$metadata <- result$metadata}
        if(!is.null(result$modifications)){tmp$modifications <- result$modifications}
        if(!is.null(result$predictions)){tmp$predictions <- result$predictions}
        if(!is.null(result$metrics)){tmp$metrics <- result$metrics}
        if(!is.null(result$modeling)){tmp$modeling <- result$modeling}
        if(!is.null(result$status)){tmp$status <- result$status}
        data(tmp) # update data with result
        shinybusy::remove_modal_spinner()
      }else{
        shinyWidgets::updatePrettySwitch(session, "launch", value = FALSE)
      }
    }, ignoreNULL = TRUE)

    # Create the available traits
    observeEvent(data(), {
      req(data())
      objectQaRaw <- data()
      metaQaRaw <- objectQaRaw$metadata$pheno
      traitsQaRaw <- unique(metaQaRaw[metaQaRaw$parameter=="trait","value"])
      # traits with variance different than zero
      if(length(traitsQaRaw) > 0){
        varsTraits <- apply(objectQaRaw$data$pheno[,traitsQaRaw,drop=FALSE],2,var, na.rm=TRUE)
        traitsQaRawVar <- names(which(varsTraits > 0))
        # traits with at least some data in any trial
        naTraits <- apply(objectQaRaw$data$pheno[,traitsQaRaw,drop=FALSE],2,sommer::propMissing)
        traitsQaRawNa <- names(which(naTraits < 1))
        # intersection of both trait types
        traitsQaRawVarNa <- intersect(traitsQaRawVar,traitsQaRawNa)
        updateSelectInput(session, "traitOutqPhenoMultipleNum",choices = traitsQaRawVarNa, selected = NULL)
        updateSelectInput(session, "traitOutqPhenoMultipleCat",choices = traitsQaRawVarNa, selected = NULL)
        updateSelectInput(session, "traitOutqPheno",choices = traitsQaRawVarNa)
        updateSelectInput(session, "traitOutqPheno2",choices = traitsQaRawVarNa)

        updateSelectInput(inputId = 'filt_param1a',choices = traitsQaRawVarNa)
        updateSelectInput(inputId = 'filt_param2',choices = traitsQaRawVarNa)
      }
    })

    output$traitOutqPhenoDefNum <- renderUI({ # input <- list(version2Mta=result$status$analysisId[3], trait2Mta="YLD_TON",nTermsFixed=1)
      req(data())
      req(input$traitOutqPhenoMultipleNum)

      traits <- input$traitOutqPhenoMultipleNum

      lapply(1:length(input$traitOutqPhenoMultipleNum), function(i) {
        shinyjs::disabled(selectInput(
          session$ns(paste0('traitNum',i)),
          label = ifelse(i==1, "Trait(s)",""),
          choices = traits, multiple = FALSE,
          selected = traits[i]
        ))
      })

    })

    output$traitOutqPhenoType <- renderUI({ # input <- list(version2Mta=result$status$analysisId[3], trait2Mta="YLD_TON",nTermsFixed=1)
      req(data())
      req(input$traitOutqPhenoMultipleNum)

      lapply(1:length(input$traitOutqPhenoMultipleNum), function(i) {
        selectInput(
          session$ns(paste0('traitType',i)),
          label = ifelse(i==1, "Type",""),
          choices = c("Continuous", "Integer", "Proportion"), multiple = FALSE,
          selected = "Continuous"
        )
      })

    })

    output$traitOutqPhenoMin <- renderUI({ # input <- list(version2Mta=result$status$analysisId[3], trait2Mta="YLD_TON",nTermsFixed=1)
      req(data())
      req(input$traitOutqPhenoMultipleNum)

      traits <- input$traitOutqPhenoMultipleNum

      lapply(1:length(input$traitOutqPhenoMultipleNum), function(i) {
        if(!is.null(eval(parse(text = paste0("input$traitType",i))))){
          if(eval(parse(text = paste0("input$traitType",i," == 'Proportion'")))){
            shinyjs::disabled(numericInput(
              session$ns(paste0('traitMin',i)),
              label = ifelse(i==1, "Lower Threshold",""),
              value = 0,
              min = 0
            ))
          } else{
            numericInput(
              session$ns(paste0('traitMin',i)),
              label = ifelse(i==1, "Lower Threshold",""),
              value = ifelse(eval(parse(text = paste0("input$traitType",i," == 'Continuous'"))),
                             round(min(data()$data$pheno[,traits[i]], na.rm=TRUE),2),
                             min(data()$data$pheno[,traits[i]][which(data()$data$pheno[,traits[i]] %% 1 == 0)], na.rm=TRUE)),
              min = ifelse(eval(parse(text = paste0("input$traitType",i," == 'Continuous'"))),
                           round(min(data()$data$pheno[,traits[i]], na.rm=TRUE),2),
                           min(data()$data$pheno[,traits[i]][which(data()$data$pheno[,traits[i]] %% 1 == 0)], na.rm=TRUE))
            )
          }
        }
      })

    })

    output$traitOutqPhenoMax <- renderUI({ # input <- list(version2Mta=result$status$analysisId[3], trait2Mta="YLD_TON",nTermsFixed=1)
      req(data())
      req(input$traitOutqPhenoMultipleNum)

      traits <- input$traitOutqPhenoMultipleNum

      lapply(1:length(input$traitOutqPhenoMultipleNum), function(i) {
        if(!is.null(eval(parse(text = paste0("input$traitType",i))))){
          if(eval(parse(text = paste0("input$traitType",i," == 'Proportion'")))){
            shinyjs::disabled(numericInput(
              session$ns(paste0('traitMax',i)),
              label = ifelse(i==1, "Upper Threshold",""),
              value = 1,
              max = 1
            ))
          } else{
            numericInput(
              session$ns(paste0('traitMax',i)),
              label = ifelse(i==1, "Upper Threshold",""),
              value = ifelse(eval(parse(text = paste0("input$traitType",i," == 'Continuous'"))),
                             round(max(data()$data$pheno[,traits[i]], na.rm=TRUE),2),
                             max(data()$data$pheno[,traits[i]][which(data()$data$pheno[,traits[i]] %% 1 == 0)], na.rm=TRUE)),
              min = ifelse(eval(parse(text = paste0("input$traitType",i," == 'Continuous'"))),
                           round(max(data()$data$pheno[,traits[i]], na.rm=TRUE),2),
                           max(data()$data$pheno[,traits[i]][which(data()$data$pheno[,traits[i]] %% 1 == 0)], na.rm=TRUE))
            )
          }
        }
      })
    })

    output$traitOutqPhenoDefCat <- renderUI({ # input <- list(version2Mta=result$status$analysisId[3], trait2Mta="YLD_TON",nTermsFixed=1)
      req(data())
      req(input$traitOutqPhenoMultipleCat)

      traits <- input$traitOutqPhenoMultipleCat

      lapply(1:length(input$traitOutqPhenoMultipleCat), function(i) {
        shinyjs::disabled(selectInput(
          session$ns(paste0('traitCat',i)),
          label = ifelse(i==1, "Trait(s)",""),
          choices = traits, multiple = FALSE,
          selected = traits[i]
        ))
      })

    })

    output$traitOutqPhenoCat <- renderUI({ # input <- list(version2Mta=result$status$analysisId[3], trait2Mta="YLD_TON",nTermsFixed=1)
      req(data())
      req(input$traitOutqPhenoMultipleCat)

      traits <- input$traitOutqPhenoMultipleCat
      traitValue <- lapply(unique(data()$data$pheno[,traits, drop = FALSE]), function(x) as.character(unique(x)))

      lapply(1:length(input$traitOutqPhenoMultipleCat), function(i) {
        selectInput(
          session$ns(paste0('traitValCat',i)),
          label = ifelse(i==1, tagList(tags$span(
            "Values",
            tags$i(
              class = "glyphicon glyphicon-info-sign",
              style = "color:#FFFFFF",
              title = "If more than 10 values, only the first 10 values will be automatically selected."
            ))),
            ""),
          choices = sort(setdiff(traitValue[[i]],c(NA,NULL))), multiple = TRUE,
          selected = sort(setdiff(traitValue[[i]],c(NA,NULL))[1:10]),
          width = "100%"
        )
      })

    })

    observe({
      req(data())
      objectQaRaw <- data()
      metaQaRaw <- objectQaRaw$metadata$pheno
      traitsQaRaw <- unique(metaQaRaw[metaQaRaw$parameter=="trait","value"])
      # traits with variance different than zero
      if(length(traitsQaRaw) > 0){
        varsTraits <- apply(objectQaRaw$data$pheno[,traitsQaRaw,drop=FALSE],2,var, na.rm=TRUE)
        traitsQaRawVar <- names(which(varsTraits > 0))
        # traits with at least some data in any trial
        naTraits <- apply(objectQaRaw$data$pheno[,traitsQaRaw,drop=FALSE],2,sommer::propMissing)
        traitsQaRawNa <- names(which(naTraits < 1))
        # intersection of both trait types
        traitsQaRawVarNa <- intersect(traitsQaRawVar,traitsQaRawNa)

        if(!is.null(input$traitOutqPhenoMultipleNum)){
          selected_input1 <- input$traitOutqPhenoMultipleNum
          all_choices_input2 <- traitsQaRawVarNa
          available_choices_input2 <- setdiff(all_choices_input2, selected_input1)
          updateSelectInput(session, "traitOutqPhenoMultipleCat", choices = available_choices_input2, selected = input$traitOutqPhenoMultipleCat)
        }
        updateSelectInput(session, "traitOutqPheno",choices = c(input$traitOutqPhenoMultipleNum, input$traitOutqPhenoMultipleCat))

        selected_input1b <- input$filt_param1a
        all_choices_input2b <- traitsQaRawVarNa
        available_choices_input2b <- setdiff(all_choices_input2b, selected_input1b)
        updateSelectInput(session, "filt_param1b", choices = available_choices_input2b, selected = input$filt_param1b)
      }

    })

    clean_data <- reactiveValues(filt_seq1 = data.frame(Trait1 = NULL,
                                                        Filter = NULL,
                                                        Trait2 = NULL),
                                 filt_seq2 = data.frame(Trait1 = NULL,
                                                        Filter = NULL,
                                                        Threshold = NULL),
                                 filt_all = data.frame(trait = NULL,
                                                       reason = NULL,
                                                       row = NULL,
                                                       value = NULL))

    observeEvent(input$rule_file,{
      if(length(input$rule_file) > 0){
        if (is.null(input$rule_file)) {return(NULL)}else{

          shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
          data <- as.data.frame(data.table::fread(input$rule_file$datapath, sep = ",",
                                                  quote = "\"", dec = ".", header = TRUE))

          if(!is.null(data)){
            if(tools::file_ext(input$rule_file$datapath) != "csv"){
              shinyWidgets::show_alert(title = 'Error!',
                                       text = 'File format should be csv',
                                       type = 'error')
              data <- NULL
            }else if(all(c("Type", "Trait1", "Filter", "Trait2", "Threshold") %!in% colnames(data))){
              shinyWidgets::show_alert(title = 'Error!',
                                       text = 'Missing required column(s)',
                                       type = 'error')
              data <- NULL
            } else if(all(c(data$Trait1, data$Trait2) %!in% unique(data()$metadata$pheno[data()$metadata$pheno$parameter=="trait","value"]))){
              shinyWidgets::show_alert(title = 'Error!',
                                       text = 'Trait(s) specified not available',
                                       type = 'error')
              data <- NULL
            } else{
              if(nrow(data[which(data$Type == "Comparative"),]) > 0){
                dataRule <- data[which(data$Type == "Comparative"),c("Trait1","Filter","Trait2")]
                if(any(is.na(dataRule))){
                  shinyWidgets::show_alert(title = 'Error!',
                                           text = 'There are some missing data',
                                           type = 'error')
                  data <- NULL
                } else if(all(data[which(data$Type == "Comparative"), "Filter"] %!in% c(">", ">=", "<", "<="))){
                  shinyWidgets::show_alert(title = 'Error!',
                                           text = 'Filter(s) specified not available',
                                           type = 'error')
                  data <- NULL
                } else{
                  clean_data$filt_seq1 <- dataRule
                }
              }
              if(nrow(data[which(data$Type == "Condition"),]) > 0){
                dataRule <- data[which(data$Type == "Condition"),c("Trait1","Filter","Threshold")]
                if(any(is.na(dataRule))){
                  shinyWidgets::show_alert(title = 'Error!',
                                           text = 'There are some missing data',
                                           type = 'error')
                  data <- NULL
                } else if(all(data[which(data$Type == "Condition"), "Filter"] %!in% c("==", "!=", ">", ">=", "<", "<="))){
                  shinyWidgets::show_alert(title = 'Error!',
                                           text = 'Filter(s) specified not available',
                                           type = 'error')
                  data <- NULL
                } else{
                  clean_data$filt_seq2 <- dataRule
                }
              }
            }
          }
          shinybusy::remove_modal_spinner()
        }
      }
    })


    add_filt_seq1_row <- observeEvent(input$add_filt1, {
      req(input$filt_param1a)
      req(input$filt_op1)
      req(input$filt_param1b)

      added_row <- data.frame(
        Trait1 = isolate(input$filt_param1a),
        Filter = isolate(input$filt_op1),
        Trait2 = isolate(input$filt_param1b)
      )

      if(nrow(clean_data$filt_seq1) > 0){
        if(do.call(paste0, added_row) %in% do.call(paste0, clean_data$filt_seq1)){
          shinyWidgets::show_alert(title = 'Error!',
                                   text = 'The rule already exists.',
                                   type = 'error')
        } else{
          clean_data$filt_seq1 <- rbind(clean_data$filt_seq1, added_row)
        }
      } else{
        clean_data$filt_seq1 <- rbind(clean_data$filt_seq1, added_row)
      }
    })

    observeEvent(input$add_filt1,{
      if(input$filt_param1b == ""){
        shinyWidgets::show_alert(title = 'Error!',
                                 text = 'The rule is incomplete. Please select Trait2.',
                                 type = 'error')
      }
    })

    observeEvent(input$del_filt1, {
      if (!is.null(input$filt_seq1_tab_rows_selected)) {
        clean_data$filt_seq1 <- clean_data$filt_seq1[-as.numeric(input$filt_seq1_tab_rows_selected), ]
      } else{
        shinyWidgets::show_alert(title = 'Error!',
                                 text = 'No row selected. Please select row to delete.',
                                 type = 'error')
      }
    })

    observeEvent(input$del_filt2, {
      if (!is.null(input$filt_seq2_tab_rows_selected)) {
        clean_data$filt_seq2 <- clean_data$filt_seq2[-as.numeric(input$filt_seq2_tab_rows_selected), ]
      } else{
        shinyWidgets::show_alert(title = 'Error!',
                                 text = 'No row selected. Please select row to delete.',
                                 type = 'error')
      }
    })

    add_filt_seq2_row <- observeEvent(input$add_filt2, {
      req(input$filt_param2)
      req(input$filt_op2)
      req(input$filt_tresh2)

      added_row <- data.frame(
        Trait1 = isolate(input$filt_param2),
        Filter = isolate(input$filt_op2),
        Threshold = isolate(input$filt_tresh2)
      )

      if(nrow(clean_data$filt_seq2) > 0){
        if(do.call(paste0, added_row) %in% do.call(paste0, clean_data$filt_seq2)){
          shinyWidgets::show_alert(title = 'Error!',
                                   text = 'The rule already exists.',
                                   type = 'error')
        } else{
          clean_data$filt_seq2 <- rbind(clean_data$filt_seq2, added_row)
        }
      } else{
        clean_data$filt_seq2 <- rbind(clean_data$filt_seq2, added_row)
      }
    })

    observeEvent(input$add_filt2,{
      if(is.na(input$filt_tresh2)){
        shinyWidgets::show_alert(title = 'Error!',
                                 text = 'The rule is incomplete. Please enter threshold value.',
                                 type = 'error')
      }
    })

    output$filt_seq1_params <- renderUI({
      if (is.data.frame(clean_data$filt_seq1)) {
        if (nrow(clean_data$filt_seq1) > 0) {
          DT::DTOutput(ns('filt_seq1_tab'))
        }
      }
    })

    output$filt_seq2_params <- renderUI({
      if (is.data.frame(clean_data$filt_seq2)) {
        if (nrow(clean_data$filt_seq2) > 0) {
          DT::DTOutput(ns('filt_seq2_tab'))
        }
      }
    })

    output$filt_seq1_tab <- DT::renderDT({
      DT = clean_data$filt_seq1
    }, selection = "single", options = list(paging = FALSE, searching = FALSE, info = FALSE))

    output$filt_seq2_tab <- DT::renderDT({
      DT = clean_data$filt_seq2
    }, selection = "single", options = list(paging = FALSE, searching = FALSE, info = FALSE,
                                            columnDefs = list(
                                              list(className = 'dt-left', targets = 3)
                                            )))

    ## render the expected result
    output$plotPredictionsCleanOut <- shiny::renderPlot({ # plotly::renderPlotly({
      req(data())
      req(input$traitOutqPheno)
      mydata <- data()$data$pheno
      ### change column names for mapping
      paramsPheno <- data()$metadata$pheno
      paramsPheno <- paramsPheno[which(paramsPheno$parameter != "trait"),]
      colnames(mydata) <- cgiarBase::replaceValues(colnames(mydata), Search = paramsPheno$value, Replace = paramsPheno$parameter )
      ###
      mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
      if(mappedColumns == 3){ # all required columns are present
        mydata$rowindex <- 1:nrow(mydata)
        mydata[, "environment"] <- as.factor(mydata[, "environment"])
        mydata[, "designation"] <- as.factor(mydata[, "designation"])
        mo <- mydata
        mydata$color <- "valid"
        if(input$traitOutqPheno %in% input$traitOutqPhenoMultipleNum){
          moNum <- grep(input$traitOutqPheno,input$traitOutqPhenoMultipleNum)
          if(eval(parse(text = paste0("input$traitType",moNum," == 'Integer'")))){
            mo <- mo[which(mo[,input$traitOutqPheno] < eval(parse(text = paste0("input$traitMin",moNum))) |
                             mo[,input$traitOutqPheno] > eval(parse(text = paste0("input$traitMax",moNum))) |
                             mo[,input$traitOutqPheno]%%1 != 0),]
          }else{
            mo <- mo[which(mo[,input$traitOutqPheno] < eval(parse(text = paste0("input$traitMin",moNum))) |
                             mo[,input$traitOutqPheno] > eval(parse(text = paste0("input$traitMax",moNum)))),]
          }
          if(nrow(mo) >= 1){mydata$color[which(mydata$rowindex %in% unique(mo$rowindex))]="tagged_range"}
          mydata$predictedValue <- mydata[,input$traitOutqPheno]
          mydata <- mydata[,which(!duplicated(colnames(mydata)))]
          mydata <- mydata[which(mydata$predictedValue %!in% c(NA,NULL)),]
          p <- ggplot2::ggplot(mydata, ggplot2::aes(x=as.factor(environment), y=predictedValue)) +
            ggplot2::geom_violin(fill='#A4A4A4', color="black")+
            ggplot2::theme_classic()+ ggplot2::ggtitle("Preview of impossible values that would be tagged using current input parameters above for the trait selected") +
            ggplot2::geom_jitter(ggplot2::aes(color = color), alpha = 0.6) +
            ggplot2::xlab("Environment") + ggplot2::ylab("Trait value") +
            ggplot2::scale_color_manual(values = c(valid = "#66C2A5", tagged_range = "#FC8D62")) +
            ggplot2::geom_hline(yintercept = eval(parse(text = paste0("input$traitMin",moNum))),
                                linetype = "dashed", color = "#FC8D62", size = 0.8) +  # specifying colors names avoids having valid points in orange in absence of potential outliers. With only colour = color, valid points are in orange in that case.
            ggplot2::geom_hline(yintercept = eval(parse(text = paste0("input$traitMax",moNum))),
                                linetype = "dashed", color = "#FC8D62", size = 0.8)

          p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1))

          p
        } else if(input$traitOutqPheno %in% input$traitOutqPhenoMultipleCat){
          moNum <- grep(input$traitOutqPheno,input$traitOutqPhenoMultipleCat)
          mo <- mo[which(mo[,input$traitOutqPheno] %!in% c(eval(parse(text = paste0("input$traitValCat",moNum))),NA,NULL)),]

          if(nrow(mo) >= 1){mydata$color[which(mydata$rowindex %in% unique(mo$rowindex))]="tagged_range"}
          mydata$predictedValue <- mydata[,input$traitOutqPheno]
          mydata <- mydata[,which(!duplicated(colnames(mydata)))]
          mydata <- mydata[which(mydata$predictedValue %!in% c(NA,NULL)),]
          p <- ggplot2::ggplot(mydata) +
            ggplot2::geom_bar(ggplot2::aes(x=as.factor(predictedValue), fill=as.factor(color)))+
            ggplot2::facet_wrap(~environment, nrow = 1, scales = "fixed") +
            ggplot2::theme(strip.background = ggplot2::element_blank()) +
            ggplot2::ggtitle("Preview of impossible values that would be tagged using current input parameters above for the trait selected") +
            ggplot2::xlab("Trait value") + ggplot2::ylab("Count") +
            ggplot2::scale_fill_manual("color", values = c(valid = "#66C2A5", tagged_range = "#FC8D62"))

          p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1))

          p
        }

      }
    })

    output$plotPredictionsCleanOut2 <- shiny::renderPlot({
      req(data())
      req(input$traitOutqPheno2)
      mydata <- data()$data$pheno
      ### change column names for mapping
      paramsPheno <- data()$metadata$pheno
      paramsPheno <- paramsPheno[which(paramsPheno$parameter != "trait"),]
      colnames(mydata) <- cgiarBase::replaceValues(colnames(mydata), Search = paramsPheno$value, Replace = paramsPheno$parameter )
      ###
      mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
      if(mappedColumns == 3){ # all required columns are present
        mydata$rowindex <- 1:nrow(mydata)
        mydata[, "environment"] <- as.factor(mydata[, "environment"])
        mydata[, "designation"] <- as.factor(mydata[, "designation"])
        mo <- mydata
        mydata$color <- "valid"

        if(input$traitOutqPheno2 %in% input$traitOutqPhenoMultipleNum){
          moNum <- grep(input$traitOutqPheno2,input$traitOutqPhenoMultipleNum)
          if(eval(parse(text = paste0("input$traitType",moNum," == 'Integer'")))){
            mo <- mo[which(mo[,input$traitOutqPheno2] < eval(parse(text = paste0("input$traitMin",moNum))) |
                             mo[,input$traitOutqPheno2] > eval(parse(text = paste0("input$traitMax",moNum))) |
                             mo[,input$traitOutqPheno2]%%1 != 0),]
          }else{
            mo <- mo[which(mo[,input$traitOutqPheno2] < eval(parse(text = paste0("input$traitMin",moNum))) |
                             mo[,input$traitOutqPheno2] > eval(parse(text = paste0("input$traitMax",moNum)))),]
          }
          if(nrow(mo) >= 1){mydata$color[which(mydata$rowindex %in% unique(mo$rowindex))]="tagged_range"}
        } else if(input$traitOutqPheno2 %in% input$traitOutqPhenoMultipleCat){
          moNum <- grep(input$traitOutqPheno2,input$traitOutqPhenoMultipleCat)
          mo <- mo[which(mo[,input$traitOutqPheno2] %!in% c(eval(parse(text = paste0("input$traitValCat",moNum))),NA,NULL)),]
          if(nrow(mo) >= 1){mydata$color[which(mydata$rowindex %in% unique(mo$rowindex))]="tagged_range"}
        }

        if(nrow(clean_data$filt_seq1)>0 | nrow(clean_data$filt_seq2)>0){
          comb_rule <- data.frame("Type" = NULL,
                                  "Trait1" = NULL,
                                  "Filter" = NULL,
                                  "Trait2" = NULL,
                                  "Threshold" = NULL)
          if(nrow(clean_data$filt_seq1) > 0){
            rule1 <- clean_data$filt_seq1
            rule1$Type <- "Comparative"
            rule1$Threshold <- NA
            comb_rule <- rbind(comb_rule, rule1)
          }
          if(nrow(clean_data$filt_seq2) > 0){
            rule2 <- clean_data$filt_seq2
            rule2$Type <- "Condition"
            rule2$Trait2 <- NA
            comb_rule <- rbind(comb_rule, rule2)
          }
          mo <- mydata[which(mydata$color == "valid"),]
          moFormula <- NULL
          NE_moFormula <- NULL
          if(input$traitOutqPheno2 %in% comb_rule$Trait1){
            for (j in which(comb_rule$Trait1 == input$traitOutqPheno2)){
              if(comb_rule$Type[j] == "Comparative"){
                if(j == which(comb_rule$Trait1 == input$traitOutqPheno2)[1]){
                  moFormula <- paste0("mo[,input$traitOutqPheno2]", comb_rule[j,"Filter"], "mo[,'", comb_rule[j,"Trait2"], "']")
                } else{
                  moFormula <- paste(moFormula, paste0("mo[,input$traitOutqPheno2]", comb_rule[j,"Filter"], "mo[,'", comb_rule[j,"Trait2"], "']"), sep = "|")
                }
              } else if(comb_rule$Type[j] == "Condition" & comb_rule$Filter[j] == "!="){
                if(j == which(comb_rule$Trait1 == input$traitOutqPheno2 & comb_rule$Filter == "!=")[1]){
                  NE_moFormula <- paste0("mo[,input$traitOutqPheno2]", comb_rule[j,"Filter"], comb_rule[j,"Threshold"])
                } else{
                  NE_moFormula <- paste(NE_moFormula, paste0("mo[,input$traitOutqPheno2]", comb_rule[j,"Filter"], comb_rule[j,"Threshold"]), sep = "&")
                }
              } else{
                if(j == which(comb_rule$Trait1 == input$traitOutqPheno2)[1]){
                  moFormula <- paste0("mo[,input$traitOutqPheno2]", comb_rule[j,"Filter"], comb_rule[j,"Threshold"])
                } else{
                  moFormula <- paste(moFormula, paste0("mo[,input$traitOutqPheno2]", comb_rule[j,"Filter"], comb_rule[j,"Threshold"]), sep = "|")
                }
              }
            }
            if(!is.null(NE_moFormula) | !is.null(moFormula)){
              if(is.null(NE_moFormula)){
                mo <- eval(parse(text = paste0("mo[which(",moFormula,"),]")))
              } else if (is.null(moFormula)){
                mo <- eval(parse(text = paste0("mo[which(",NE_moFormula,"),]")))
              } else{
                mo <- eval(parse(text = paste0("mo[which((",NE_moFormula,")|(",moFormula,")),]")))
              }
            }
            if(nrow(mo) >= 1){mydata$color[which(mydata$rowindex %in% unique(mo$rowindex))]="tagged_rule"}
          }
        }

        mydata$predictedValue <- mydata[,input$traitOutqPheno2]
        mydata <- mydata[,which(!duplicated(colnames(mydata)))]
        mydata <- mydata[which(mydata$predictedValue %!in% c(NA,NULL)),]

        p <- ggplot2::ggplot(mydata, ggplot2::aes(x=as.factor(environment), y=predictedValue)) +
          ggplot2::geom_violin(fill='#A4A4A4', color="black")+
          ggplot2::theme_classic()+ ggplot2::ggtitle("Preview of impossible values that would be tagged using current input parameters above for the trait selected") +
          ggplot2::geom_jitter(ggplot2::aes(color = color), alpha = 0.6) +
          ggplot2::xlab("Environment") + ggplot2::ylab("Trait value") +
          ggplot2::scale_color_manual(values = c(valid = "#66C2A5", tagged_range = "#FC8D62", tagged_rule = "#CD5C5C"))

        p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1))

        p

      }
    })

    output$download_filt <- downloadHandler(
      filename = function() {
        paste(paste0('qaConsistency_rule_',gsub("-", "", as.integer(Sys.time()))), sep = '.', 'csv')
      },
      content = function(file) {
        shinybusy::show_modal_spinner(spin = "fading-circle", text = "Downloading Range and Rule...")

        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))

        filt_rule <- setNames(data.frame(matrix(ncol=5, nrow=0)), c("Type","Trait1","Filter","Trait2","Threshold"))
        if(length(input$traitOutqPhenoMultipleNum)>0){
          filt1a_temp <- filt_rule
          for (i in 1:length(input$traitOutqPhenoMultipleNum)){
            filt1a_temp <- data.frame("Type" = "Condition",
                                      "Trait1" = eval(parse(text = paste0("input$traitNum",i))),
                                      "Filter" = c("<", ">"),
                                      "Trait2" = NA,
                                      "Threshold" = c(eval(parse(text = paste0("input$traitMin",i))), eval(parse(text = paste0("input$traitMax",i))))
            )
            filt_rule <- rbind(filt_rule, filt1a_temp)
          }
        }
        if(length(input$traitOutqPhenoMultipleCat)>0){
          filt2a_temp <- filt_rule
          for (i in 1:length(input$traitOutqPhenoMultipleCat)){
            filt2a_temp <- data.frame("Type" = "Condition",
                                      "Trait1" = eval(parse(text = paste0("input$traitCat",i))),
                                      "Filter" = "!=",
                                      "Trait2" = NA,
                                      "Threshold" = eval(parse(text = paste0("input$traitValCat",i)))
            )
            filt_rule <- rbind(filt_rule, filt2a_temp)
          }
        }
        if(nrow(clean_data$filt_seq1) > 0){
          filt1_temp <- clean_data$filt_seq1
          filt1_temp$Type <- "Comparative"
          filt1_temp$Threshold <- NA
          filt_rule <- rbind(filt_rule, filt1_temp)
        }
        if(nrow(clean_data$filt_seq2) > 0){
          filt2_temp <- clean_data$filt_seq2
          filt2_temp$Type <- "Condition"
          filt2_temp$Trait2 <- NA
          filt_rule <- rbind(filt_rule, filt2_temp)
        }
        utils::write.csv(unique(filt_rule), file, row.names = FALSE)
        shinybusy::remove_modal_spinner()
      }, contentType = "text/csv"
    )

    observeEvent(input$load_filt3, {
      req(data())
      mydata <- data()$data$pheno
      mydata$rowindex <- 1:nrow(mydata)
      clean_data$filt_all <- data.frame(trait = NULL,
                                        reason = NULL,
                                        row = NULL,
                                        value = NULL)
      traitQC <- NULL
      traitFilter <- c("input$traitOutqPhenoMultipleNum", "input$traitOutqPhenoMultipleCat", "clean_data$filt_seq1", "clean_data$filt_seq2")
      for(j in 1:4){
        if(j < 3){ if(!is.null(eval(parse(text=traitFilter[j])))){ traitQC <- c(traitQC, eval(parse(text=traitFilter[j])))}
        } else{ if(nrow(eval(parse(text=traitFilter[j]))) > 0){ traitQC <- c(traitQC, eval(parse(text=traitFilter[j]))[,1])}}
      }
      traitQC <- unique(traitQC)
      if(!is.null(traitQC)){
        for (i in 1:length(traitQC)){
          mo <- mydata
          outData <- data.frame(trait = traitQC[i], reason = "valid", row = mydata$rowindex, value = mydata[,traitQC[i]])
          if(traitQC[i] %in% input$traitOutqPhenoMultipleNum){
            moNum <- grep(traitQC[i],input$traitOutqPhenoMultipleNum)
            if(eval(parse(text = paste0("input$traitType",moNum," == 'Integer'")))){
              mo <- mo[which(mo[,traitQC[i]] < eval(parse(text = paste0("input$traitMin",moNum))) |
                               mo[,traitQC[i]] > eval(parse(text = paste0("input$traitMax",moNum))) |
                               mo[,traitQC[i]]%%1 != 0),]
            }else{
              mo <- mo[which(mo[,traitQC[i]] < eval(parse(text = paste0("input$traitMin",moNum))) |
                               mo[,traitQC[i]] > eval(parse(text = paste0("input$traitMax",moNum)))),]
            }
            if(nrow(mo) >= 1){outData$reason[which(outData$row %in% unique(mo$rowindex))]="impossibleValueBasedOnRange"}
          } else if(traitQC[i] %in% input$traitOutqPhenoMultipleCat){
            moNum <- grep(traitQC[i],input$traitOutqPhenoMultipleCat)
            mo <- mo[which(mo[,traitQC[i]] %!in% c(eval(parse(text = paste0("input$traitValCat",moNum))),NA,NULL)),]
            if(nrow(mo) >= 1){outData$reason[which(outData$row %in% unique(mo$rowindex))]="impossibleValueBasedOnRange"}
          }

          if(nrow(clean_data$filt_seq1)>0 | nrow(clean_data$filt_seq2)>0){
            comb_rule <- data.frame("Type" = NULL,
                                    "Trait1" = NULL,
                                    "Filter" = NULL,
                                    "Trait2" = NULL,
                                    "Threshold" = NULL)
            if(nrow(clean_data$filt_seq1) > 0){
              rule1 <- clean_data$filt_seq1
              rule1$Type <- "Comparative"
              rule1$Threshold <- NA
              comb_rule <- rbind(comb_rule, rule1)
            }
            if(nrow(clean_data$filt_seq2) > 0){
              rule2 <- clean_data$filt_seq2
              rule2$Type <- "Condition"
              rule2$Trait2 <- NA
              comb_rule <- rbind(comb_rule, rule2)
            }
            mo <- mydata[which(mydata$rowindex %in% outData[which(outData$reason == "valid"), "row"]),]
            moFormula <- NULL
            NE_moFormula <- NULL
            if(traitQC[i] %in% comb_rule$Trait1){
              for (j in which(comb_rule$Trait1 == traitQC[i])){
                if(comb_rule$Type[j] == "Comparative"){
                  if(j == which(comb_rule$Trait1 == traitQC[i])[1]){
                    moFormula <- paste0("mo[,traitQC[i]]", comb_rule[j,"Filter"], "mo[,'", comb_rule[j,"Trait2"], "']")
                  } else{
                    moFormula <- paste(moFormula, paste0("mo[,traitQC[i]]", comb_rule[j,"Filter"], "mo[,'", comb_rule[j,"Trait2"], "']"), sep = "|")
                  }
                } else if(comb_rule$Type[j] == "Condition" & comb_rule$Filter[j] == "!="){
                  if(j == which(comb_rule$Trait1 == traitQC[i] & comb_rule$Filter == "!=")[1]){
                    NE_moFormula <- paste0("mo[,traitQC[i]]", comb_rule[j,"Filter"], comb_rule[j,"Threshold"])
                  } else{
                    NE_moFormula <- paste(NE_moFormula, paste0("mo[,traitQC[i]]", comb_rule[j,"Filter"], comb_rule[j,"Threshold"]), sep = "&")
                  }
                } else{
                  if(j == which(comb_rule$Trait1 == traitQC[i])[1]){
                    moFormula <- paste0("mo[,traitQC[i]]", comb_rule[j,"Filter"], comb_rule[j,"Threshold"])
                  } else{
                    moFormula <- paste(moFormula, paste0("mo[,traitQC[i]]", comb_rule[j,"Filter"], comb_rule[j,"Threshold"]), sep = "|")
                  }
                }
              }
              if(!is.null(NE_moFormula) | !is.null(moFormula)){
                if(is.null(NE_moFormula)){
                  mo <- eval(parse(text = paste0("mo[which(",moFormula,"),]")))
                } else if (is.null(moFormula)){
                  mo <- eval(parse(text = paste0("mo[which(",NE_moFormula,"),]")))
                } else{
                  mo <- eval(parse(text = paste0("mo[which((",NE_moFormula,")|(",moFormula,")),]")))
                }
              }
              if(nrow(mo) >= 1){outData$reason[which(outData$row %in% unique(mo$rowindex))]="impossibleValueBasedOnRule"}
            }
          }
          clean_data$filt_all <- rbind(clean_data$filt_all, outData[which(outData$reason != "valid"),])
        }
      }
      if(nrow(clean_data$filt_all)<1){
        shinyWidgets::show_alert(title = 'Error!',
                                 text = 'All values are valid',
                                 type = 'error')
      }
    })

    output$filt_all <- renderUI({
      if (is.data.frame(clean_data$filt_all)) {
        if(nrow(clean_data$filt_all) > 0){
          DT::renderDT({
            DT = clean_data$filt_all
          }, selection = "none", options = list(paging = FALSE, scrollY = "500px", searching = FALSE, info = FALSE))
        }
      }
    })

    report <- reactiveVal(NULL)

    observeEvent(input$renderReportQaDataCons,{
      shinybusy::show_modal_spinner(spin = "fading-circle", text = "Generating Report...")

      result <- data()

      src <- normalizePath(system.file("rmd","reportQaPheno.Rmd",package="bioflow"))
      src2 <- normalizePath('data/resultQaConsistency.RData')

      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))

      file.copy(src, 'report.Rmd', overwrite = TRUE)
      file.copy(src2, 'resultQaConsistency.RData', overwrite = TRUE)

      outReport <- rmarkdown::render('report.Rmd', params = list(toDownload=TRUE ),
                                     switch("HTML", HTML = rmdformats::robobook(toc_depth = 4)
                                            # HTML = rmarkdown::html_document()
                                     ))

      report(outReport)

      shinybusy::remove_modal_spinner()

      shinyjs::click("downloadReportQaDataCons")
    })

    outQaDataCons <- eventReactive(input$runQaDataCons, {
      req(data())

      mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
      if(mappedColumns == 3){ # all required columns are present

        shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')

        ## get the outlier table
        analysisId <- as.numeric(Sys.time())
        outlier <- clean_data$filt_all
        if(nrow(outlier)>0){
          outlier$analysisId <- analysisId
          outlier$module <- "qaConsistency"
          ## get data structure
          result <- data()
          ## bind new parameters
          if(is.null(result$modifications$pheno)){
            result$modifications$pheno <- outlier
          }else{
            result$modifications$pheno <- rbind(result$modifications$pheno, outlier[,colnames(result$modifications$pheno)])
          }
          # add status table
          newStatus <- data.frame(module="qaConsistency", analysisId=analysisId, analysisIdName=input$analysisIdName )
          if(!is.null(result$status)){
            result$status <- rbind(result$status, newStatus[,colnames(result$status)])
          }else{result$status <- newStatus}
          # add modeling table

          traitQC <- NULL
          traitFilter <- c("input$traitOutqPhenoMultipleNum", "input$traitOutqPhenoMultipleCat", "clean_data$filt_seq1", "clean_data$filt_seq2")
          for(j in 1:4){
            if(j < 3){ if(!is.null(eval(parse(text=traitFilter[j])))){ traitQC <- c(traitQC, eval(parse(text=traitFilter[j])))}
            } else{ if(nrow(eval(parse(text=traitFilter[j]))) > 0){ traitQC <- c(traitQC, eval(parse(text=traitFilter[j]))[,1])}}
          }
          traitQC <- unique(traitQC)
          provMet1 <- list()
          provMet2 <- list()
          provMet3 <- list()
          if(!is.null(traitQC)){
            for (i in 1:length(traitQC)){
              if(traitQC[i] %in% input$traitOutqPhenoMultipleNum){
                moNum <- grep(traitQC[i],input$traitOutqPhenoMultipleNum)

                provMet1[[traitQC[i]]] <- data.frame(module="qaConsistency",analysisId=analysisId, trait=traitQC[i], environment=NA,
                                                     parameter= c("impossibleValueRangeMin", "impossibleValueRangeMax"),
                                                     value= c(eval(parse(text = paste0("input$traitMin",moNum))),
                                                              eval(parse(text = paste0("input$traitMax",moNum)))))
              } else if(traitQC[i] %in% input$traitOutqPhenoMultipleCat){
                moNum <- grep(traitQC[i],input$traitOutqPhenoMultipleCat)

                provMet1[[traitQC[i]]] <- data.frame(module="qaConsistency",analysisId=analysisId, trait=traitQC[i], environment=NA,
                                                     parameter= "impossibleValueRangeCat",
                                                     value= eval(parse(text = paste0("input$traitValCat",moNum))))
              }

              if(nrow(clean_data$filt_seq1)>0){
                if(traitQC[i] %in% clean_data$filt_seq1$Trait1){
                  for (j in which(clean_data$filt_seq1$Trait1 == traitQC[i])){
                    mo <- paste(clean_data$filt_seq1[j,1], clean_data$filt_seq1[j,2], clean_data$filt_seq1[j,3])
                    provMet2[[traitQC[i]]] <- data.frame(module="qaConsistency",analysisId=analysisId, trait=traitQC[i], environment=NA,
                                                         parameter= "impossibleValueRule",
                                                         value= mo)
                  }
                }
              }
              if(nrow(clean_data$filt_seq2)>0){
                if(traitQC[i] %in% clean_data$filt_seq2$Trait){
                  for (j in which(clean_data$filt_seq2$Trait == traitQC[i])){
                    mo <- paste(clean_data$filt_seq2[j,1], clean_data$filt_seq2[j,2], clean_data$filt_seq2[j,3])
                    provMet3[[traitQC[i]]] <- data.frame(module="qaConsistency",analysisId=analysisId, trait=traitQC[i], environment=NA,
                                                         parameter= "impossibleValueRule",
                                                         value= mo)
                  }
                }
              }
            }
            provMet1 <- do.call(rbind,provMet1)
            provMet2 <- do.call(rbind,provMet2)
            provMet3 <- do.call(rbind,provMet3)

            provMet <- rbind(provMet1, provMet2, provMet3)
          }
          if(is.null(result$modeling)){
            result$modeling <- provMet
          }else{
            result$modeling <- rbind(result$modeling, provMet[,colnames(result$modeling)])
          }
          data(result)
          cat(paste("QA step with id:",as.POSIXct( analysisId, origin="1970-01-01", tz="GMT"),"for trait",paste(traitQC, collapse = ", "),"saved."))
          updateTabsetPanel(session, "tabsMain", selected = "outputTabs")
          shinybusy::remove_modal_spinner()

          if(!inherits(result,"try-error")) { # if all goes well in the run
            # ## Report tab
            output$reportQaDataCons <- renderUI({
              HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportQaPheno.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
            })

            output$downloadReportQaDataCons <- downloadHandler(
              filename = function() {
                paste(paste0('qaConsistency_dashboard_',gsub("-", "", as.integer(Sys.time()))), sep = '.', switch(
                  "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
                ))
              },
              content = function(file) {

                out <- report()

                file.rename(out, file)
              }
            )

            output$outQaDataCons2 <- renderPrint({
              cat(paste("QA step with id:",as.POSIXct( analysisId, origin="1970-01-01", tz="GMT"),"for trait",paste(traitQC, collapse = ", "),"saved."))
            })

          }else{ hideAll$clearAll <- TRUE}

          hideAll$clearAll <- FALSE
        }else {
          shinybusy::remove_modal_spinner()
          cat("There are no modifications to be saved.")
          output$outQaDataCons2 <- renderPrint({
            cat("There are no modifications to be saved.")
          })
        }
      }else{
        shinybusy::remove_modal_spinner()
        cat("Please meet the data conditions before you identify and save impossible values.")
        output$outQaDataCons2 <- renderPrint({
          cat("Please meet the data conditions before you identify and save impossible values.")
        })
      }
    })

    output$outQaDataCons <- renderPrint({
      outQaDataCons()
    })

  })
}

## To be copied in the UI
# mod_qaDataConsistApp_ui("qaDataConsistApp_1")

## To be copied in the server
# mod_qaDataConsistApp_server("qaDataConsistApp_1")

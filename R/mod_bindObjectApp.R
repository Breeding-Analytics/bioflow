#' bindObjectApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bindObjectApp_ui <- function(id){
  ns <- NS(id)
  tagList(


    sidebarPanel(
      # input <- list(version2Bind="1699508839.68847",trait2Bind="desireIndex",entryType2Bind= "TGV_EST004D#TEST_tested",nCrossBind="20", targetAngleBind="30",maxRun=40, relType="grm", env2Bind="across",verboseBind=TRUE )
      tags$style(".well {background-color:grey; color: #FFFFFF;}"),
      HTML("<img src='www/cgiar3.png' width='42' vspace='10' hspace='10' height='46' align='top'>
                  <font size='5'>Realized Genetic Gain</font>"),
      # div(tags$p( h4(strong("Realized Genetic Gain")))),#, style = "color: #817e7e"
      hr(style = "border-top: 1px solid #4c4c4c;"),
      selectInput(
        inputId = ns('previous_object_input'),
        label   = 'Object Source*: ',
        choices = list('Upload from PC' = 'pcfile', 'Upload from cloud' = 'cloudfile'),
        width   = '200px'
      ),
      tags$span(id = ns('previous_object_file_holder'),
                fileInput(
                  inputId = ns('previous_object_file'),
                  label   = NULL,
                  width   = '400px',
                  accept  = c('.rds','.RData')
                ),
                # textOutput(ns("outBind2")),
      ),
      tags$div(id = ns('previous_object_retrieve'),
               actionButton(ns("refreshPreviousAnalysis"), "Click to retrieve previous analysis"),
               uiOutput(ns('previous_input2')),


      ),
      actionButton(ns("runBind"), "Bind objects", icon = icon("play-circle")),
      textOutput(ns("outBind")),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      # actionButton(ns("runBind"), "Bind objects", icon = icon("play-circle")),
      # hr(style = "border-top: 1px solid #4c4c4c;"),
      # uiOutput(ns("qaQcBindInfo")),
      # textOutput(ns("outBind"))
    ), # end sidebarpanel
    mainPanel(tabsetPanel( id=ns("tabsMain"),
                           type = "tabs",
                           tabPanel(p("Information",class="info-p"),  icon = icon("book"),
                                    br(),
                                    shinydashboard::box(status="success",width = 12,
                                                        solidHeader = TRUE,
                                                        column(width=12,   style = "height:800px; overflow-y: scroll;overflow-x: scroll;",
                                                               h2(strong("Status:")),
                                                               uiOutput(ns("warningMessage")),
                                                               h2(strong("Details")),
                                                               p("In order to monitor the efficacy of genetic evaluation across cycles of selection, the realized genetic gain is the preferred process.
                                          This option aims to calculate the realized genetic gain using the methods from Mackay et al. (2011). The
                              method uses across-environment means from multiple years of data that have been adjusted based on a good connectivity
                              to then fit a regression of the form means~year.of.origin. In case the means used are BLUPs these can be
                              deregressed.
                                The way the options are used is the following:"),
                                                               p(strong("Trait for cross prediction.-")," Trait to be be used for predicting all possible crosses (an index is suggested)."),
                                                               p(strong("Fixed effects.-")," Fixed effects to add to the regression means~year.of.origin."),
                                                               p(strong("Deregress data?.-")," A TRUE/FALSE value indicating if predicted values should be deregressed (to be set to TRUE when BLUPs are used)."),
                                                               p(strong("Partition the data?.-")," When very few years of data are present this option will allow the user to calculate the gain for all 2 year combinations and then average these rates."),
                                                               p(strong("Deregress weight.-")," Should any weight be applied to the deregressed value (not recommended to be used but available)."),
                                                               h2(strong("References:")),
                                                               p("Mackay, I., Horwell, A., Garner, J., White, J., McKee, J., & Philpott, H. (2011). Reanalyses of the historical series of UK variety trials
                                to quantify the contributions of genetic and environmental factors to trends and variability in yield over time. Theoretical and Applied
                                Genetics, 122, 225-238."),
                                                               p("Laidig, F., Piepho, H. P., Drobek, T., & Meyer, U. (2014). Genetic and non-genetic long-term trends of 12 different crops in German
                                official variety performance trials and on-farm yield trends. Theoretical and Applied Genetics, 127, 2599-2617."),
                                                               h2(strong("Software used:")),
                                                               p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
                                Vienna, Austria. URL https://www.R-project.org/.")
                                                        )
                                    )
                           ),
    )) # end mainpanel



  )
}

#' bindObjectApp Server Functions
#'
#' @noRd
mod_bindObjectApp_server <- function(id, data=NULL, res_auth=NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })
    ############################################################################
    output$warningMessage <- renderUI(
      if(is.null(data())){
        HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your phenotypic data using the 'Data Retrieval' tab and map the 'designation', 'environment' columns and at least one trait.")) )
      }else{ # data is there
        mappedColumns <- setdiff(data()$metadata$pedigree[data()$metadata$pedigree$parameter == "yearOfOrigin","value"],"")
        # mappedColumns <- length(which(c("yearOfOrigin") %in% colnames(data()$medata$pedigree)))
        if(length(mappedColumns) == 1){
          if("mta" %in% data()$status$module){
            HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to perform the realized genetic gain inspecting the other tabs.")) )
          }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please perform a Multi-Trial Analysis before performing a realized genetic gain analysis.")) ) }
        }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please make sure that the column: 'yearOfOrigin' has been mapped in the pedigree data using the 'Data Retrieval' tab.")) )}
      }
    )
    ####################
    ## load the objects
    observeEvent(
      input$previous_object_input,
      if(length(input$previous_object_input) > 0){ # added
        if (input$previous_object_input == 'pcfile') {
          golem::invoke_js('showid', ns('previous_object_file_holder'))
          golem::invoke_js('hideid', ns('previous_object_retrieve'))
        } else if (input$previous_object_input == 'cloudfile') {
          golem::invoke_js('hideid', ns('previous_object_file_holder'))
          golem::invoke_js('showid', ns('previous_object_retrieve'))
        }
      }
    )
    output$previous_input2 <- renderUI({}) # this 2 lines avoid issues when displaying an uiOutput
    outputOptions(output, "previous_input2", suspendWhenHidden = FALSE)
    observeEvent( # this is the part where we either load the previous analysis from cloud or PC
      c(input$previous_object_input),
      {
        previousFilesAvailable <- eventReactive(input$refreshPreviousAnalysis, { #
          selectInput(inputId=ns('previous_input'), label=NULL, choices=dir(file.path(res_auth$repository)), multiple = TRUE)
        })
        output$previous_input2 <- renderPrint({  previousFilesAvailable()    })
        outBind <- eventReactive(input$runBind, {
          shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
          if(input$previous_object_input == 'cloudfile'){ # upload from cloud
            req(input$previous_input)
            load( file.path( getwd(),res_auth$repository,input$previous_input[1] ) ) # old dataset
            if(length(input$previous_input) > 1){
              for(iFile in 2:length(input$previous_input)){
                result1 <- result
                load( file.path( getwd(),res_auth$repository,input$previous_input[iFile] ) ) # old dataset
                result2 <- result
                result1 <- try(cgiarBase::bindObjects(object1 = result1,
                                                      object2 = result2
                ), silent = TRUE
                )
              }
              result <- result1
            }else{ }
          }else if(input$previous_object_input == 'pcfile'){ # upload rds
            req(input$previous_object_file)
            load(input$previous_object_file$datapath)
          }
          ## replace tables
          tmp <- data()
          tmp$data <- result$data
          tmp$metadata <- result$metadata
          tmp$modifications <- result$modifications
          tmp$predictions <- result$predictions
          tmp$metrics <- result$metrics
          tmp$modeling <- result$modeling
          tmp$status <- result$status
          data(tmp) # update data with results
          shinybusy::remove_modal_spinner()
          if(input$previous_object_input == 'cloudfile'){
            cat(paste("Dataset:", paste(input$previous_input, collapse = " and "),"binded successfully."))
          }else{
            cat(paste("Dataset","binded successfully."))
          }
        }) ## end eventReactive
        output$outBind <- renderPrint({
          outBind()
        })
      }
    )


  })
}

## To be copied in the UI
# mod_bindObjectApp_ui("bindObjectApp_1")

## To be copied in the server
# mod_bindObjectApp_server("bindObjectApp_1")

#' getOldAnalysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_getOldAnalysis_ui <- function(id){
  ns <- NS(id)
  tagList(

    tabPanel(
      title = 'Upload old analysis',
      value = ns('tab6'),
      tags$br(),

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
                # textOutput(ns("outLoad2")),
      ),
      tags$div(id = ns('previous_object_retrieve'),
               actionButton(ns("refreshPreviousAnalysis"), "Click to retrieve previous analysis"),
               uiOutput(ns('previous_input2')),

      ),
      actionButton(ns("runLoadPrevious"), "Load analysis", icon = icon("play-circle")),
      textOutput(ns("outLoad")),

      shiny::plotOutput(ns("plotDataDependencies6")),

    ),

  )
}

#' getOldAnalysis Server Functions
#'
#' @noRd
mod_getOldAnalysis_server <- function(id, data, res_auth){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ### Previous-analyses tab controls ##################################################

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
          selectInput(inputId=ns('previous_input'), label=NULL, choices=dir(file.path(res_auth$repository)), multiple = FALSE)
        })
        output$previous_input2 <- renderPrint({  previousFilesAvailable()    })
        outLoad <- eventReactive(input$runLoadPrevious, {
          if(input$previous_object_input == 'cloudfile'){ # upload from cloud
            req(input$previous_input)
            load( file.path( getwd(),res_auth$repository,input$previous_input ) ) # old dataset
          }else if(input$previous_object_input == 'pcfile'){ # upload rds
            req(input$previous_object_file)
            load(input$previous_object_file$datapath)
          }
          shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
          ## replace tables
          tmp <- data() # current or empty dataset
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
            shinyalert::shinyalert(title = "Success!", text = paste("Dataset:", input$previous_input,"loaded successfully."), type = "success")
            # cat(paste("Dataset:", input$previous_input,"loaded successfully."))
          }else{
            shinyalert::shinyalert(title = "Success!", text = paste("Dataset",input$previous_object_file$name,"loaded successfully."), type = "success")
            # cat(paste("Dataset",input$previous_object_file$name,"loaded successfully."))
          }
        }) ## end eventReactive
        output$outLoad <- renderPrint({
          outLoad()
        })
      }
    )

  })
}

## To be copied in the UI
# mod_getOldAnalysis_ui("getOldAnalysis_1")

## To be copied in the server
# mod_getOldAnalysis_server("getOldAnalysis_1")

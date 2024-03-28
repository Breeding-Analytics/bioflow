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

    br(),
    selectInput(
      inputId = ns('previous_object_input'),
      label   = 'Object Source*: ',
      choices = list('Upload from PC' = 'pcfile', 'Upload from cloud' = 'cloudfile'),
      width   = '200px'
    ),
    tags$span(id = ns('previous_object_file_holder'), # from PC
              fileInput(
                inputId = ns('previous_object_file'),
                multiple = TRUE,
                label   = NULL,
                width   = '400px',
                accept  = c('.rds','.RData')
              ),
    ),
    tags$div(id = ns('previous_object_retrieve'), # from cloud
             actionButton(ns("refreshPreviousAnalysis"), "Click to retrieve data objects"),
             uiOutput(ns('previous_input2')),
    ),

    br(),
    actionButton(ns("runBind"), "Load object(s)", icon = icon("play-circle")),
    textOutput(ns("outBind")),

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
    # output$warningMessage <- renderUI(
    #   HTML( as.character(div(style="color: green; font-size: 20px;", "Please proceed to load the data objects that you want to bind.")) )
    # )
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
              result1 <- result

              for(iFile in 2:length(input$previous_input)){
                load( file.path( getwd(),res_auth$repository,input$previous_input[iFile] ) ) # old dataset
                result2 <- result
                result1 <- try(cgiarBase::bindObjects(object1 = result1,
                                                      object2 = result2
                ), silent = TRUE
                )
              }

              result <- result1
            }else{ iFile=1 }
          }else if(input$previous_object_input == 'pcfile'){ # upload rds
            req(input$previous_object_file)
            load(input$previous_object_file$datapath[1]) # old dataset
            if(length(input$previous_object_file$datapath) > 1){
              result1 <- result
              for(iFile in 2:length(input$previous_object_file$datapath)){
                load(input$previous_object_file$datapath[iFile])  # old dataset
                result2 <- result
                result1 <- try(cgiarBase::bindObjects(object1 = result1,
                                                      object2 = result2
                ), silent = TRUE
                )
              }

              result <- result1
            }else{ iFile=1 }
          }
          ## replace tables
          tmp <- data()
          if(!is.null(result$data)){tmp$data <- result$data}
          if(!is.null(result$metadata)){tmp$metadata <- result$metadata}
          if(!is.null(result$modifications)){tmp$modifications <- result$modifications}
          if(!is.null(result$predictions)){tmp$predictions <- result$predictions}
          if(!is.null(result$metrics)){tmp$metrics <- result$metrics}
          if(!is.null(result$modeling)){tmp$modeling <- result$modeling}
          if(!is.null(result$status)){tmp$status <- result$status}
          save(tmp, file = "test.RData")
          data(tmp) # update data with results
          shinybusy::remove_modal_spinner()

          if(input$previous_object_input == 'cloudfile'){
            if(iFile == 1){
              shinyalert::shinyalert(title = "Success!", text = paste("Dataset:", input$previous_input,"loaded successfully."), type = "success")
            }else{
              shinyalert::shinyalert(title = "Success!", text = paste("Datasets:", paste(input$previous_input, collapse = " and "),"binded successfully. Please proceed to perform your analysis.") , type = "success")
            }
            # cat(paste("Datasets:", paste(input$previous_input, collapse = " and "),"binded successfully. Please proceed to perform your analysis."))
          }else{
            if(iFile == 1){
              shinyalert::shinyalert(title = "Success!", text = paste("Dataset",input$previous_object_file$name,"loaded successfully."), type = "success")
            }else{
              shinyalert::shinyalert(title = "Success!", text = paste("Datasets:", paste(input$previous_object_file$name, collapse = " and "),"binded successfully. Please proceed to perform your analysis.") , type = "success")
            }
            # cat(paste("Datasets","binded successfully. Please proceed to perform your analysis."))
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

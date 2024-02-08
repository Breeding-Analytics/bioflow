#' saveData UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_saveData_ui <- function(id){
  ns <- NS(id)
  tagList(


    shinydashboard::tabBox(
      width = 12,
      id    = ns('tabset'),

      tabPanel(
        title = 'Save analysis',
        value = ns('tab5'),
        tags$br(),

        selectInput(
          inputId = ns('object_output'),
          label   = 'Storing place*: ',
          choices = list('Computer' = 'pcfile', 'Cloud' = 'cloudfile'),
          width   = '200px'
        ),
        textInput(ns("fileNameUpload"), label = "Name assigned to the analysis", value = "Enter name..."),

        tags$span(id = ns('pcfile_holder'),
                  downloadButton(ns("downloadRds"), "Save", icon = icon("floppy-disk")),
                  textOutput(ns("outSave2")),
        ),
        ## provisionally silenced until Khaled enables the S3 saving using the users table
        tags$span(id = ns('cloudfile_holder'),
                  actionButton(ns("runSave"), "Save", icon = icon("floppy-disk")),
                  textOutput(ns("outSave")),
        ),
        hr(style = "border-top: 1px solid #4c4c4c;"),
        DT::DTOutput(ns("statusTable"))
      )

    )


  )
}

#' saveData Server Functions
#'
#' @noRd
mod_saveData_server <- function(id, data, res_auth=NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    observeEvent(
      input$object_output,
      if(length(input$object_output) > 0){ # added
        if (input$object_output == 'pcfile') {
          golem::invoke_js('showid', ns('pcfile_holder'))
          golem::invoke_js('hideid', ns('cloudfile_holder'))
        } else if (input$object_output == 'cloudfile') {
          golem::invoke_js('hideid', ns('pcfile_holder'))
          golem::invoke_js('showid', ns('cloudfile_holder'))
        }
      }
    )

    output$outSave <- renderUI({}) # this 2 lines avoid issues when displaying an output nested
    outputOptions(output, "outSave", suspendWhenHidden = FALSE)
    observeEvent( # this is the part where we either load the previous analysis from cloud or PC
      c(input$object_output),
      {
        if(input$object_output == 'cloudfile'){ # upload from cloud
          outSave <- eventReactive(input$runSave, {
            req(data())
            req(input$fileNameUpload)
            shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
            result <- data() # current or empty dataset
            save(result, file=file.path(getwd(),res_auth$repository, paste0(input$fileNameUpload,".RData") ) ) # old dataset
            shinybusy::remove_modal_spinner()
            cat(paste("Analysis named: '",input$fileNameUpload,"' saved successfully."))
          }) ## end eventReactive
          output$outSave <- renderPrint({
            outSave()
          })
        }else if(input$object_output == 'pcfile'){ # upload rds

          output$downloadRds <- downloadHandler(
            filename <- function(){
              return(file.path(paste0(input$fileNameUpload,".RData")))
            },

            content = function(file) {
              result= data()
              save(result, file = file)
            }
          )

        }else{

        }
      }
    )

    output$statusTable <-  DT::renderDT({
      req(data())
      dtMta <- data()
      status <- dtMta$status; status$analysisId <- as.POSIXct(status$analysisId, origin="1970-01-01", tz="GMT")
      DT::datatable(status, extensions = 'Buttons',
                                    options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                   lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
      )
    })

  })
}

## To be copied in the UI
# mod_saveData_ui("saveData_1")

## To be copied in the server
# mod_saveData_server("saveData_1")

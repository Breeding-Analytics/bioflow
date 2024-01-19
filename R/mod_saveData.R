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
        textInput(ns("fileNameUpload"), label = "Name assigned to the analysis", value = "Enter name..."),
        actionButton(ns("runSave"), "Save analysis", icon = icon("floppy-disk")),
        textOutput(ns("outSave")),
        hr(style = "border-top: 1px solid #4c4c4c;"),
        DT::DTOutput(ns("statusTable"))
      )

    )


  )
}

#' saveData Server Functions
#'
#' @noRd
mod_saveData_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    outSave <- eventReactive(input$runSave, {
      req(data())
      req(input$fileNameUpload)
      shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
      ## save
      tmp <- data() # current or empty dataset
      save(tmp, file=file.path(getwd(),"R/outputs", paste0(input$fileNameUpload,".RData") ) ) # old dataset
      shinybusy::remove_modal_spinner()
      cat(paste("Analysis named: '",input$fileNameUpload,"' saved successfully."))
    }) ## end eventReactive
    output$outSave <- renderPrint({
      outSave()
    })

    output$statusTable <-  DT::renderDT({
      req(data())
      dtMta <- data()
      DT::datatable(dtMta$status, extensions = 'Buttons',
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

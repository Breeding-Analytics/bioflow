#' input_file_sta UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_input_file_sta_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      tags$style(".well {background-color:black; color: #FFFFFF;}"),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      fileInput(ns("file"),"Choose file (.rds)",accept=c(".rds")),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      actionButton(ns("btnPreview"), "View Data", icon = icon("table"))
    ),
    mainPanel(tabsetPanel(
      type = "tabs",
      tabPanel("Data", DT::DTOutput(ns("table")))
    ))
  )
}

#' input_file_sta Server Functions
#'
#' @noRd
mod_input_file_sta_server <- function(id){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    data <- reactiveValues(uploadedData = NULL)

    observeEvent(input$btnPreview, {

      req(input$file)
      showNotification("Uploading file...", type = "message")
      tryCatch({
        data$uploadedData <- readRDS(input$file$datapath)
      }, error = function(e) {
        showNotification(paste0("Error reading file: ", e$message), type = "error")
        NULL
      })

      output$table <- DT::renderDT({
        req(data$uploadedData)
        mydata <- data$uploadedData
        mydata <- mydata$data$pheno

        DT::datatable(mydata,
                      options = list(autoWidth = TRUE),
                      filter = "top"
        )
      })

    })

    return(data)

  })
}

## To be copied in the UI
# mod_input_file_sta_ui("input_file_sta_1")

## To be copied in the server
# mod_input_file_sta_server("input_file_sta_1")

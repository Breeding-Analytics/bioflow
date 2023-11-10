#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  required_mapping <- c("stage", "pipeline", "country", "year", "season",
                        "location", "trial", "environment", "rep", "iBlock",
                        "row", "col", "designation", "gid", "entryType", "trait")

  data_getData <- mod_getData_server("getData_1", map = required_mapping)
  #data.sta <- mod_input_file_sta_server("input_file_sta_1")
  data_qaRaw <- mod_qaRawApp_server("qaRawApp_1", data= data_getData)
  data_Sta <- mod_staApp_server("staApp_1", data_getData)
  data_qaSta <- mod_qaStaApp_server("qaStaApp_1")
  data_Mta <- mod_mtaApp_server("mtaApp_1")
  data_Index <- mod_indexDesireApp_server("indexDesireApp_1")
  data_Ocs <- mod_ocsApp_server("ocsApp_1")

  # observeEvent(input$settingsSta, {
  #   hideTab(inputId = "tabso", target = "Multiple Trials")
  #   showTab(inputId = "tabso", target = "Single Trial")
  # })


}

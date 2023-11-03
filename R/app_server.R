#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  #data.sta <- mod_getData_server("getData_1", map = c("trait", "geno"))
  data.sta <- mod_input_file_sta_server("input_file_sta_1")
  mod_staApp_server("staApp_1", data.sta)

  # observeEvent(input$settingsSta, {
  #   hideTab(inputId = "tabso", target = "Multiple Trials")
  #   showTab(inputId = "tabso", target = "Single Trial")
  # })


}

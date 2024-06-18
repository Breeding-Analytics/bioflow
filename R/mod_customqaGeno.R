mod_customqaGenoApp_ui <- function(id){
  ns <- NS(id)
  tagList()
}

mod_customqaGenoApp_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
  })
}

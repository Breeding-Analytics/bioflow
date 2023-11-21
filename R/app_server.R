#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  data <- reactiveVal()
  observe({data <- create_getData_object()})
  # Your application server logic
  required_mapping <- c("stage", "pipeline", "country", "year", "season",
                        "location", "trial", "environment", "rep", "iBlock",
                        "row", "col", "designation", "gid", "entryType", "trait")

  mod_getData_server("getData_1", map = required_mapping, data = data)
  mod_qaRawApp_server("qaRawApp_1", data = data)
  mod_staApp_server("staApp_1", data = data)
  mod_qaStaApp_server("qaStaApp_1",data = data)
  mod_qaGenoApp_server("qaGenoApp_1")
  mod_mtaApp_server("mtaApp_1", data = data)
  mod_indexDesireApp_server("indexDesireApp_1",data = data)
  mod_ocsApp_server("ocsApp_1", data = data)
  mod_rggApp_server("rggApp_1", data = data)
  mod_pggApp_server("pggApp_1", data = data)
  # observeEvent(input$settingsSta, {
  #   hideTab(inputId = "tabso", target = "Multiple Trials")
  #   showTab(inputId = "tabso", target = "Single Trial")
  # })


}

create_getData_object <- function() {
  obj <- list()

  obj$data <- list()
  obj$metadata <- list()
  obj$modifications <- list()

  obj$data$pheno    <- data.frame()
  obj$data$geno     <- matrix()
  obj$data$weather  <- data.frame()
  obj$data$pedigree <- data.frame(designation = character(),
                                  mother = character(),
                                  father = character())

  obj$metadata$pheno <- data.frame(parameter = character(),
                                   value = character())

  obj$metadata$geno <- data.frame(marker = character(),
                                  chr = character(),
                                  pos = double(),
                                  refAllele = character(),
                                  altAllele = character())

  obj$metadata$pedigree <- data.frame()

  obj$metadata$weather <- data.frame(environment = character(),
                                     parameter = character(),
                                     value = double())

  obj$modifications$pheno <- data.frame(module = character(),
                                        analysisId = character(),
                                        trait = character(),
                                        reason = character(),
                                        row = integer(),
                                        value = double())

  obj$modifications$geno <- data.frame(module = character(),
                                       analysisId = character(),
                                       reason = character(),
                                       row = integer(),
                                       col = integer(),
                                       value = double())

  obj$modifications$pedigree <- data.frame(module = character(),
                                           analysisId = character(),
                                           reason = character(),
                                           row = integer())

  obj$modifications$weather <- data.frame()

  obj$predictions <- data.frame(module = character(),
                                analysisId = character(),
                                pipeline = character(),
                                trait = character(),
                                gid = character(),
                                designation = character(),
                                mother = character(),
                                father = character(),
                                entryType = character(),
                                environment = character(),
                                predictedValue = double(),
                                stdError = double(),
                                reliability = double())

  obj$metrics <- data.frame(module = character(),
                            analysisId = character(),
                            trait = character(),
                            environment = character(),
                            parameter = character(),
                            method = character(),
                            value = double(),
                            stdError = double())

  obj$modeling <- data.frame(module = character(),
                             analysisId = character(),
                             trait = character(),
                             environment = character(),
                             parameter = character(),
                             value = character())

  obj$status <- data.frame(module = character(),
                           analysisId = character())

  return(obj)
}

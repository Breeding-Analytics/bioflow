geno_example  <- 'www/example/geno.hmp.txt'

# Currently polyploid supported formats
polyploid_support <- c("vcf", 'darttag', 'hapmap')
dartseq_formats <- c("dartseqpa", "dartseqsnp")


#' getDataGenoCustom UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_getDataGenoCustom_ui <- function(id) {
  ns <- NS(id)
  tagList(tags$br(),
          navlistPanel(
            widths = c(2, 10),
            tabPanel(
              div("1. Load data"),
              column(width = 8,
                     wellPanel(
                       h4("File Input:"),
                       selectInput(
                         inputId = ns('custom_geno_input'),
                         label   = 'Genotypic SNPs Source*:',
                         choices = list(
                           'HapMap Upload' = 'hapmap',
                           'VCF Upload' = 'vcf',
                           'DartSeq PA' = 'dartseqpa',
                           'DartSeq SNP' = 'dartseqsnp',
                           'DartTag SNP' = 'darttag'
                         ),
                         width   = '200px'
                       ),
                       uiOutput(ns("file_upload_box")),
                       actionButton("load_geno_btn", "Load")
                     ),),
              column(
                width = 4,
                shinydashboard::box(
                  width = 12,
                  title = span(icon('youtube'), ' Tutorial'),
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  h4(strong(
                    span(
                      "",
                      tags$a(href = "https://www.youtube.com/watch?v=gFYGIb9fBLQ&list=PLZ0lafzH_UmclOPifjCntlMzysEB2_2wX&index=2", icon("youtube") , target =
                               "_blank"),
                      style = "color:darkcyan"
                    )
                  )),
                ),

                tags$div(
                  id = ns('geno_table_options'),
                  shinydashboard::box(
                    width = 12,
                    title = span(icon('screwdriver-wrench'), ' Options'),
                    collapsible = TRUE,
                    collapsed = TRUE,
                    status = 'success',
                    solidHeader = TRUE,
                    uiOutput(ns("ploidity_params")),
                    uiOutput(ns("dartseq_params"))
                  ),
                ),

              )

            ),
            tabPanel(div("2. Match columns")),
            tabPanel(div("3. Check status")),

          ),
  )
}

#' getDataGenoCustom Server Functions
#'
#' @noRd
mod_getDataGenoCustom_server <-
  function(id, data = NULL, res_auth = NULL) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      output$file_upload_box <- renderUI({
        req(input$custom_geno_input)
        if (input$custom_geno_input != "darttag") {
          # Only one file uploader
          tags$span(
            id = ns('adegeno_file_holder'),
            fileInput(
              inputId = ns('adegeno_file'),
              label   = "Upload the genotypic data file:",
              width   = '400px',
              accept  = c(
                'application/gzip',
                '.gz',
                '.txt',
                '.hmp',
                '.csv',
                '.vcf'
              )
            ),
          )
        } else {
          # Darttag filebox with counts and dosage uploaders
          tags$span(
            id = ns('adegeno_file_holder'),
            fileInput(
              inputId = ns('darttag_counts_file'),
              label   = "Upload the counts file:",
              width   = '400px',
              accept  = c('.csv')
            ),
            fileInput(
              inputId = ns('darttag_dosage_file'),
              label   = "Upload the dosage file:",
              width   = '400px',
              accept  = c('.csv')
            )
          )
        }
      })


      output$ploidity_params <- renderUI({
        req(input$custom_geno_input)

        if (input$custom_geno_input %in% polyploid_support) {
          print('Polyploid supported format')
          ploidity_list <- list("Diploid" = 2, "Tetraploid" = 4)

        } else {
          # ADD should limit the obtions of poly lvl to only diploid
          print('Polyploid unsupported format')
          ploidity_list <- list("Diploid" = 2)
        }

        selectInput(
          inputId = ns("ploidlvl_input"),
          label = "Ploidity level:",
          choices = ploidity_list,
        )

      })

      output$dartseq_params <- renderUI({
        req(input$custom_geno_input)
        req(input$adegeno_file)

        if (input$custom_geno_input %in% dartseq_formats) {
          print('dartseq format')
          dart_cols <- dart_getCols(input$adegeno_file$datapath)
          tags$span(
            selectInput(
              inputId = ns("dartseq_markerid"),
              label = "Marker id:",
              choices = dart_cols
            ),
            selectInput(
              inputId = ns("dartseq_chrom"),
              label = "Chromosome",
              choices = dart_cols
            ),
            selectInput(
              inputId = ns("dartseq_position"),
              label = "Variant position",
              choices = dart_cols
            )
          )
        }
      })



    })
  }



# Util functions pending to move ------------------------------------------

# get the column names of dart csv files
dart_getCols <- function(path) {
  # remove all rows staring with * character
  top_rows <- read.csv(path,
                       comment.char = "*",
                       nrows = 20)
  return(colnames(top_rows))
}

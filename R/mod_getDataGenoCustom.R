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
              column(
                width = 8,
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
                  actionButton(ns("load_geno_btn"), "Load"),
                ),
              ),
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
            tabPanel(div("2. Genotype data Summary"),
                     column(width = 8,
                            hr(),
                            # Outputs
                            tableOutput(ns('summary_by_chrom')),
                            verbatimTextOutput(ns('geno_summary')),
                            )),

          ),)
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

      get_geno_data <- reactive(
                                 {
                                   print("Geno load btn clicked")
                                   genotype_file <-
                                     input$adegeno_file$datapath
                                   print(as.numeric(input$ploidlvl_input))
                                   print(typeof(as.numeric(input$ploidlvl_input)))

                                   switch(
                                     input$custom_geno_input,
                                     hapmap = {
                                       shinybusy::show_modal_spinner('fading-circle', text = 'Loading...')
                                       tryCatch({
                                         geno_data = cgiarGenomics::read_hapmap(path = genotype_file,
                                                                                ploidity = as.numeric(input$ploidlvl_input))
                                       }, error = function(e) {
                                         print(e)
                                         shinyWidgets::show_alert(title = 'Error !!',
                                                                  text = 'Not a valid file format :-(',
                                                                  type = 'error')
                                       })
                                       shinybusy::remove_modal_spinner()
                                     },
                                     vcf = {
                                       shinybusy::show_modal_spinner('fading-circle', text = 'Loading...')
                                       tryCatch({
                                         geno_data = cgiarGenomics::read_vcf(path = genotype_file,
                                                                             ploidity = as.numeric(input$ploidlvl_input))
                                       }, error = function(e) {
                                         print(e)
                                         shinyWidgets::show_alert(title = 'Error !!',
                                                                  text = 'Not a valid file format :-(',
                                                                  type = 'error')
                                       })
                                       shinybusy::remove_modal_spinner()
                                     },
                                     dartseqpa = {
                                       shinybusy::show_modal_spinner('fading-circle', text = 'Loading...')
                                       tryCatch({
                                         geno_data = cgiarGenomics::read_DArTSeq_PA(
                                           dart_path = genotype_file,
                                           marker_id = input$dartseq_markerid,
                                           chr_name = input$dartseq_chrom,
                                           pos_name = input$dartseq_position
                                         )
                                       }, error = function(e) {
                                         print(e)
                                         shinyWidgets::show_alert(title = 'Error !!',
                                                                  text = 'Not a valid file format :-(',
                                                                  type = 'error')
                                       })
                                       shinybusy::remove_modal_spinner()
                                     },
                                     dartseqsnp = {
                                       shinybusy::show_modal_spinner('fading-circle', text = 'Loading...')
                                       tryCatch({
                                         geno_data = cgiarGenomics::read_DArTSeq_SNP(
                                           dart_path = genotype_file,
                                           snp_id = input$dartseq_markerid,
                                           chr_name = input$dartseq_chrom,
                                           pos_name = input$dartseq_position
                                         )
                                       }, error = function(e) {
                                         print(e)
                                         shinyWidgets::show_alert(title = 'Error !!',
                                                                  text = 'Not a valid file format :-(',
                                                                  type = 'error')
                                       })
                                       shinybusy::remove_modal_spinner()
                                     },
                                     darttag = {
                                       shinybusy::show_modal_spinner('fading-circle', text = 'Loading...')
                                       tryCatch({
                                         geno_data = cgiarGenomics::read_DArT_Tag(
                                           counts.file = input$darttag_counts_file$datapath,
                                           dosage.file = input$darttag_dosage_file$datapath,
                                           ploidity = as.numeric(input$ploidlvl_input)
                                         )
                                       }, error = function(e) {
                                         print(e)
                                         shinyWidgets::show_alert(title = 'Error !!',
                                                                  text = 'Not a valid file format :-(',
                                                                  type = 'error')
                                       })
                                       shinybusy::remove_modal_spinner()
                                     },
                                     {
                                       print("This should not execute D;")
                                     }
                                   )
                                   return(geno_data)
                                 })
      # just for make button reactive
      observeEvent(input$load_geno_btn,
                   {
                     get_geno_data()
                     add_data()
                   })

      # Summary of genotypic data
      output$summary_by_chrom <- renderTable({
        geno_data <- get_geno_data()
        print("Processsing...")
        geno_metadata <- data.frame(CHROM = geno_data@chromosome,
                                    POS = geno_data@position)

        data.frame(
          chrom = unique(geno_metadata$CHROM),
          min_pos = aggregate(POS ~ CHROM, data = geno_metadata, FUN = min)[, 2],
          max_pos = aggregate(POS ~ CHROM, data = geno_metadata, FUN = max)[, 2],
          snps_count = aggregate(POS ~ CHROM, data = geno_metadata, FUN = length)[, 2]
        )
      })

      output$geno_summary <- renderText({
        temp <- data()
        ind_names <- adegenet::indNames(get_geno_data())

        if (!is.null(ind_names) & any(temp$metadata$pheno$parameter == 'designation')) {
          designationColumn <- temp$metadata$pheno[which(temp$metadata$pheno$parameter == "designation"),"value"]
          paste(
            "Data Integrity Checks:\n",

            sum(ind_names %in% unique(temp$data$pheno[, designationColumn ])),
            "Accessions exist in both phenotypic and genotypic files (will be used to train the model)\n",

            sum(!ind_names %in% unique(temp$data$pheno[, designationColumn ])),
            "Accessions have genotypic data but no phenotypic (will be predicted, add to pheno data file with NA value)\n",

            sum(!unique(temp$data$pheno[, designationColumn ]) %in% ind_names),
            'Accessions have phenotypic data but no genotypic (will not contribute to the training model)'
          )
        }
      })

      # Store the genotype data into the data structure
      add_data <- reactive(
        {
          temp <- data()

          if(!is.null(temp$metadata$geno)){temp$metadata$geno <- temp$metadata$geno[0,]} # make sure if an user uploads a new dataset the metadata starts empty
          if(!is.null(temp$modifications$geno)){temp$modifications$geno <- temp$modifications$geno[0,]} # make sure if an user uploads a new dataset the modifications starts empty
          if(!is.null(temp$status)){
            toRemove <- which(temp$status$module == "qaGeno")
            if(length(toRemove) > 0){temp$status <- temp$status[-toRemove,, drop=FALSE]}
          } # make sure if an user uploads a new dataset the qaGeno starts empty
          geno_data <- get_geno_data()
          # Recoded 0,1,2 into -1,0,1
          temp$data$geno <- as.matrix(geno_data) - 1

          geno_metadata <- data.frame(
             'rs#' = adegenet::locNames(geno_data),
             chrom = geno_data@chromosome,
             pos = geno_data@position,
             alleles = adegenet::alleles(geno_data))
          temp$metadata$geno <- geno_metadata

          temp$data$genoformat <- input$custom_geno_input

          # Record filepath
          if (input$custom_geno_input != "darttag") {
            temp$data$genodir <- input$adegeno_file$datapath
          } else {
            temp$data$genodir <- c(input$darttag_counts_file$datapath,
                                   input$darttag_dosage_file$datapath)
          }
          # Record ploidity level
          temp$data$ploidity <- as.numeric(input$ploidlvl_input)
          # Record column names for marker_id, chrom and pos of dartseq formats
          if (input$custom_geno_input %in% dartseq_formats) {

            temp$data$geno_markerid_col <- input$dartseq_markerid
            temp$data$geno_chr_name <- input$dartseq_chrom
            temp$data$geno_pos_name <- input$dartseq_position
          }
          data(temp)
        }
      )

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

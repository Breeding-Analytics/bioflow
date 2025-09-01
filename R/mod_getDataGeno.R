geno_example  <- 'www/example/geno.hmp.txt'

# Currently polyploid supported formats
polyploid_support <- c("vcf", 'dartag', 'hapmap')
dartseq_formats <- c("dartseqsnp")
#' getDataGenoCustom UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_getDataGeno_ui <- function(id) {
  ns <- NS(id)
  tagList(tags$br(),
          navlistPanel("Steps:", id = ns("geno_load_navpanel"), widths = c(2, 10),
            tabPanel("1. Load data",
              column(width = 8,
                     selectInput(inputId = ns('custom_geno_input'),
                                 label   = 'Biallelic SNP format*:',
                                 choices = list('HapMap' = 'hapmap',
                                                'VCF' = 'vcf',
                                                'DartSeq SNP' = 'dartseqsnp',
                                                'DarTag SNP' = 'dartag'
                                                ),
                                 width   = '200px'
                                 ),

                     selectInput(inputId = ns('geno_input'),
                                 label   = 'Data Source*: ',
                                 choices = list('Upload File' = 'file',
                                                'Copy URL' = 'url'),
                                 width   = '200px'
                                 ),

                     tags$span(id = ns('adegeno_file_holder'),
                               fileInput(inputId = ns('adegeno_file'),
                                         label   = NULL,
                                         width   = '400px',
                                         accept  = c('application/gzip', '.gz',
                                                     '.txt', '.hmp', '.csv', '.vcf')
                               )
                     ),
                     tags$span(id = ns('adegeno_url_holder'),
                               textInput(inputId = ns("adegeno_url"),
                                         label = NULL,
                                         width   = '400px',
                                         placeholder = "https://website.com/genotype_file.txt"
                               )
                     ),
                     tags$span(id = ns('adegeno_file_holder2'),
                               fileInput(inputId = ns('darttag_counts_file'),
                                         label   = "Counts:",
                                         width   = '400px',
                                         accept  = c('.csv')
                               ),
                               fileInput(inputId = ns('darttag_dosage_file'),
                                         label   = "Dosage:",
                                         width   = '400px',
                                         accept  = c('.csv')
                               )
                     ),
                     tags$span(id = ns('adegeno_url_holder2'),
                               textInput(inputId = ns("darttag_counts_url"),
                                         label = "Counts:",
                                         width   = '400px',
                                         placeholder = "https://website.com/dartag_counts.csv"
                               ),
                               textInput(inputId = ns("darttag_dosage_url"),
                                         label = "Dosage:",
                                         width   = '400px',
                                         placeholder = "https://website.com/dartag_dosage.csv"
                               ),
                     ),
                     if (!is.null(geno_example)) {
                       shinyWidgets::prettySwitch( inputId = ns('geno_example'), label = "Load example", status = "success")
                     },

                     actionButton(ns("load_geno_btn"), "Load"),
              ),
              column(width = 4,
                shinydashboard::box(width = 12, title = span(icon('youtube'), ' Tutorial'), status = "success", collapsible = TRUE, collapsed = TRUE, solidHeader=FALSE, style = "color: #000000",
                  h4(strong(span("", tags$a(href = "https://www.youtube.com/watch?v=gFYGIb9fBLQ&list=PLZ0lafzH_UmclOPifjCntlMzysEB2_2wX&index=2", icon("youtube") , target ="_blank"), style = "color:darkcyan"))),
                ),
                tags$div(id = ns('geno_table_options'),
                  shinydashboard::box(width = 12,title = span(icon('screwdriver-wrench'), ' Options'), status = "success", collapsible = TRUE, collapsed = FALSE, solidHeader=FALSE, style = "color: #000000",
                    uiOutput(ns("ploidity_params")),
                    uiOutput(ns("dartseq_params"))
                  ),
                ),
              )
            ),
            tabPanel("2. Genotype data Summary",
                     column(width = 8,
                            hr(),
                            # Outputs
                            tableOutput(ns('summary_by_chrom')),
                            DT::DTOutput(ns('ind_summary')),
                            verbatimTextOutput(ns('geno_summary')),
                            actionButton(ns("ind_management_btn"), "Next")
                     )),
            tabPanel("3. Individual Management",
                     uiOutput(ns("indManagementUI"))),
            tabPanel("4. Check status",
                     uiOutput(ns("warningMessage")),
            )))}
mod_getDataGeno_server <-
  function(id, data = NULL, res_auth = NULL) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      library(adegenet)

      output$warningMessage <- renderUI(
        if(is.null(data())){
          HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your genotype data using the 'Data Retrieval' tab. Make sure you use the right data format (e.g., single header, etc.).")) )
        }else{ # data is there
          if(!is.null(data()$data$geno)){
            HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, you can proceed to use the other modules.")) )
          }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your genotype data using the 'Data Retrieval' tab. Make sure you use the right data format (e.g., single header, etc.). ")) )}
        }
      )

      observeEvent(c(input$custom_geno_input, input$geno_input),{
        if(input$custom_geno_input != "dartag"){
          if(length(input$geno_input) >0){
            if (input$geno_input == 'file') {
              golem::invoke_js('showid', ns('adegeno_file_holder'))
              golem::invoke_js('hideid', ns('adegeno_url_holder'))
              golem::invoke_js('hideid', ns('adegeno_file_holder2'))
              golem::invoke_js('hideid', ns('adegeno_url_holder2'))
              updateCheckboxInput(session, 'geno_example', value = FALSE)
            } else if (input$geno_input == 'url') {
              golem::invoke_js('hideid', ns('adegeno_file_holder'))
              golem::invoke_js('showid', ns('adegeno_url_holder'))
              golem::invoke_js('hideid', ns('adegeno_file_holder2'))
              golem::invoke_js('hideid', ns('adegeno_url_holder2'))
            }
          }
        } else{
          if(length(input$geno_input) >0){
            if (input$geno_input == 'file') {
              golem::invoke_js('hideid', ns('adegeno_file_holder'))
              golem::invoke_js('hideid', ns('adegeno_url_holder'))
              golem::invoke_js('showid', ns('adegeno_file_holder2'))
              golem::invoke_js('hideid', ns('adegeno_url_holder2'))
              updateCheckboxInput(session, 'geno_example', value = FALSE)
            } else if (input$geno_input == 'url') {
              golem::invoke_js('hideid', ns('adegeno_file_holder'))
              golem::invoke_js('hideid', ns('adegeno_url_holder'))
              golem::invoke_js('hideid', ns('adegeno_file_holder2'))
              golem::invoke_js('showid', ns('adegeno_url_holder2'))
              updateCheckboxInput(session, 'geno_example', value = FALSE)
            }
          }
        }
      }
      )

      output$ploidity_params <- renderUI({
        req(input$custom_geno_input)

        if (input$custom_geno_input %in% polyploid_support) {
          print('Polyploid supported format')
          ploidity_list <- list("Diploid" = 2, "Tetraploid" = 4, "Hexaploid" = 6)
        } else {
          # ADD should limit the obtions of poly lvl to only diploid
          print('Polyploid unsupported format')
          ploidity_list <- list("Diploid" = 2)
        }

        selectInput(inputId = ns("ploidlvl_input"),
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
            selectInput(inputId = ns("dartseq_markerid"),
                        label = "Marker id:",
                        choices = dart_cols
                        ),
            selectInput(inputId = ns("dartseq_chrom"),
                        label = "Chromosome",
                        choices = dart_cols
                        ),
            selectInput(inputId = ns("dartseq_position"),
                        label = "Variant position",
                        choices = dart_cols
                        )
          )
        }
      })

      ## data example loading
      observeEvent(input$geno_example,{
        if(length(input$geno_example) > 0){
          if (input$geno_example) {
            shinyWidgets::ask_confirmation(
              inputId = ns("myconfirmation"),
              text = "Are you sure you want to load the example genotypes? This will delete any genotypic data currently in the environment.",
              title = "Data replacement warning"
            )
          }
        }
      })

      observeEvent(input$myconfirmation, {
        if (isTRUE(input$myconfirmation)) {
          if(length(input$geno_example) > 0){
            if (input$geno_example) {
              updateSelectInput(session, 'custom_geno_input', selected = 'hapmap')
              updateSelectInput(session, 'geno_input', selected = 'url')

              geno_example_url <- 'https://raw.githubusercontent.com/Breeding-Analytics/bioflow/main/inst/app/www/example/geno.hmp.txt'

              updateTextInput(session, 'adegeno_url', value = geno_example_url)
            } else {
              updateSelectInput(session, 'custom_geno_input', selected = 'hapmap')
              updateSelectInput(session, 'geno_input', selected = 'file')
              updateTextInput(session, 'adegeno_url', value = '')
            }
          }
        }else{
          shinyWidgets::updatePrettySwitch(session, "geno_example", value = FALSE)
        }
      }, ignoreNULL = TRUE)

      # Reactive function where input functions are called
      get_geno_data <- reactive({
        print("Geno load btn clicked")

        if(input$custom_geno_input != "dartag"){
          if (input$geno_input == 'file') {
            genotype_file <- input$adegeno_file$datapath
          } else if (input$geno_input == 'url') {
            temp_genofile <- tempfile(
              tmpdir = tempdir(),
              fileext = sub(".*\\.([a-zA-Z0-9]+)$", ".\\1", input$adegeno_url))
            # Download file
            utils::download.file(input$adegeno_url, temp_genofile)
            genotype_file <- temp_genofile
            }
          } else {
            if (input$geno_input == 'file') {
              dosage_file <- input$darttag_dosage_file$datapath
              counts_file <- input$darttag_counts_file$datapath
            } else if (input$geno_input == 'url') {
              # Dosage file download
              temp_dosagefile <- tempfile(
                tmpdir = tempdir(),
                fileext = sub(".*\\.([a-zA-Z0-9]+)$", ".\\1", input$darttag_dosage_url))
              utils::download.file(input$darttag_dosage_url, temp_dosagefile)
              dosage_file <- temp_dosagefile
              # Counts file download
              temp_countsfile <- tempfile(
                tmpdir = tempdir(),
                fileext = sub(".*\\.([a-zA-Z0-9]+)$", ".\\1", input$darttag_counts_url))
              utils::download.file(input$darttag_counts_url, temp_countsfile)
              counts_file <- temp_countsfile
            }
          }

          print(as.numeric(input$ploidlvl_input))
          print(typeof(as.numeric(input$ploidlvl_input)))
          switch(input$custom_geno_input,
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
                 dartseqsnp = {
                   shinybusy::show_modal_spinner('fading-circle', text = 'Loading...')
                   tryCatch({
                     geno_data = cgiarGenomics::read_DArTSeq_SNP(art_path = genotype_file,
                                                                 snp_id = input$dartseq_markerid,
                                                                 chr_name = input$dartseq_chrom,
                                                                 pos_name = input$dartseq_position)
                     }, error = function(e) {
                       print(e)
                       shinyWidgets::show_alert(title = 'Error !!',
                                                text = 'Not a valid file format :-(',
                                                type = 'error')
                       })
                   shinybusy::remove_modal_spinner()
                   },
                 dartag = {
                   shinybusy::show_modal_spinner('fading-circle', text = 'Loading...')
                   tryCatch({
                     geno_data = cgiarGenomics::read_DArTag_count_dosage(dosage_path = dosage_file,
                                                                         counts_path = counts_file,
                                                                         ploidity = as.numeric(input$ploidlvl_input))
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
      observeEvent(input$load_geno_btn,{
        geno_data <- get_geno_data()
        #add_data() Commented this during duplicate handelling dev REMOVE
        updateNavlistPanel(session = session,
                           inputId = "geno_load_navpanel",
                           selected = "2. Genotype data Summary")

        # Summary of genotypic data
        output$summary_by_chrom <- renderTable({
          # geno_data <- get_geno_data()
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
            ind_names <- adegenet::indNames(geno_data)
            if (!is.null(ind_names)) {
              glue::glue("Accessions in genotypic data:\n{length(ind_names)}")
            }
          })

        output$ind_summary <- DT::renderDT({
            DT::datatable(
              data.frame(sample_id = adegenet::indNames(geno_data)),
              options = list(
                dom = "lfrtip",          # 'f' = global search box
                pageLength = 10,
                lengthMenu = c(5, 10, 25, 50)
              ),
              rownames = FALSE
            )
          })
        })

      # Store the genotype data into the data structure and update the metadata
      add_data <- reactive({
          temp <- data()
          if(!is.null(temp$metadata$geno)){temp$metadata$geno <- temp$metadata$geno[0,]} # make sure if an user uploads a new dataset the metadata starts empty
          if(!is.null(temp$modifications$geno)){temp$modifications$geno <- temp$modifications$geno[0,]} # make sure if an user uploads a new dataset the modifications starts empty
          if(!is.null(temp$status)){
            toRemove <- which(temp$status$module == "qaGeno")
            if(length(toRemove) > 0){temp$status <- temp$status[-toRemove,, drop=FALSE]}
          } # make sure if an user uploads a new dataset the qaGeno starts empty

          # Add the gl object in results$data$geno slot
          temp$data$geno <- get_geno_data()

          # Add metadata
          parameter_names <- c(
            'input_format',
            'ploidity')
          parameter_values <- c(input$custom_geno_input,
                                as.numeric(input$ploidlvl_input))

          tmp_metadata <- data.frame(parameter = parameter_names,
                                     value = parameter_values)

          # If the input file is a dartseqsnp also store in metadata the
          # column names of marker_id, chromosome and position in the csv
          if(input$custom_geno_input == 'dartseqsnp'){
            parameter_names <- c('dartseq_marker_id',
                                 'dartseq_chr',
                                 'dartseq_pos')

            parameter_values <- c(input$dartseq_markerid,
                                  input$dartseq_chrom,
                                  pos_name = input$dartseq_position)

            dartseq_metadata <- data.frame(parameter = parameter_names,
                                           value = parameter_values)

            tmp_metadata <- rbind(tmp_metadata, dartseq_metadata)

          }

          temp$metadata$geno <- tmp_metadata

          data(temp)
        }
      )


# Ind Management server ---------------------------------------------------

      observeEvent(input$ind_management_btn, {
        updateNavlistPanel(session = session,
                           inputId = "geno_load_navpanel",
                           selected = "3. Individual Management")
      })

      output$indManagementUI <- renderUI({
        req(get_geno_data())
        gl <- get_geno_data()
        tags$div(
          shinydashboard::box(width = 12, title = "Duplicate Detection",

                              ),
          shinydashboard::box(width = 12, title = "Duplicate Definition"),
          shinydashboard::box(width = 12, title = "Merge Individuals"),
          shinydashboard::box(width = 12, title = "Remove Individuals")
        )
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

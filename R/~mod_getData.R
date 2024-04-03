#' pheno_example <- 'www/example/pheno.csv'
#' geno_example  <- 'www/example/geno.hmp.txt'
#' ped_example   <- 'www/example/pedigree.csv'
#' qtl_example <- 'www/example/qtl.csv'
#'
#' #' getData UI Function (test forking)
#' #'
#' #' @description A shiny Module.
#' #'
#' #' @param id,input,output,session Internal parameters for {shiny}.
#' #'
#' #' @noRd
#' #'
#' #' @importFrom shiny NS tagList
#' mod_getData_ui <- function(id){
#'   ns <- NS(id)
#'   tagList(
#'     shinydashboard::tabBox(
#'       width = 12,
#'       id    = ns('tabset'),
#'
#'       tabPanel(
#'         title = 'Phenotypic',
#'         value = ns('tab1'),
#'
#'         tags$br(),
#'
#'         selectInput(
#'           inputId = ns('pheno_input'),
#'           label   = 'Data Source*: ',
#'           choices = list('Upload File' = 'file', 'Copy URL' = 'url', 'BrAPI' = 'brapi'),
#'           width   = '200px'
#'         ),
#'
#'         tags$span(id = ns('pheno_file_holder'),
#'                   fileInput(
#'                     inputId = ns('pheno_file'),
#'                     label   = NULL,
#'                     width   = '400px',
#'                     accept  = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
#'                   )
#'         ),
#'
#'         textInput(
#'           inputId = ns('pheno_url'),
#'           label   = NULL,
#'           value   = '',
#'           width   = '400px',
#'           placeholder = 'https://example.com/path/file.csv'
#'         ),
#'
#'         tags$div(id = ns('pheno_csv_options'),
#'                  shinydashboard::box(title = span(icon('screwdriver-wrench'), ' Options'), collapsible = TRUE, collapsed = TRUE, status = 'success', solidHeader = TRUE,
#'                                      shinyWidgets::prettyRadioButtons(ns('pheno_sep'), 'Separator Character', selected = ',', inline = TRUE,
#'                                                                       choices = c('Comma' = ',', 'Semicolon' = ';', 'Tab' = "\t")),
#'
#'                                      shinyWidgets::prettyRadioButtons(ns('pheno_quote'), 'Quoting Character', selected = '"', inline = TRUE,
#'                                                                       choices = c('None' = '', 'Double Quote' = '"', 'Single Quote' = "'")),
#'
#'                                      shinyWidgets::prettyRadioButtons(ns('pheno_dec'), 'Decimal Points', selected = '.', inline = TRUE,
#'                                                                       choices = c('Dot' = '.', 'Comma' = ',')),
#'                  ),
#'         ),
#'
#'         fluidRow(tags$div(id = ns('dictionary_box'),
#'                           shinydashboard::box(width = 6, title = span(icon('bookmark'), ' Dictionary of terms'), status = "success", solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE,
#'                                               p(strong("pipeline.-"),"The name of the column containing the labels describing the breeding effort to satisfy a market segment (e.g., Direct seeded late maturity irrigated)."),
#'                                               p(strong("stage.-"),"The name of the column containing the labels describing the stages of phenotypic evaluation (e.g., Stage 1, PYT, etc.)."),
#'                                               p(strong("year.-"),"The name of the column containing the labels listing the year when a trial was carried out (e.g., 2024)."),
#'                                               p(strong("season-"),"The name of the column containing the labels listing the season when a trial was carried out (e.g., dry-season, wet-season, etc.)."),
#'                                               p(strong("country.-"),"The name of the column containing the labels listing the countries where a trial was carried out (e.g., Nigeria, Mexico, etc.)."),
#'                                               p(strong("location-"),"The name of the column containing the labels listing the locations within a country when a trial was carried out (e.g., Obregon, Toluca, etc.)."),
#'                                               p(strong("trial.-"),"The name of the column containing the labels listing the trial of experiment randomized."),
#'                                               p(strong("study.-"),"The name of the column containing the labels listing the unique occurrences of a trial nested in a year, country, location. If not available, compute it using the button available."),
#'                                               p(strong("rep.-"),"The name of the column containing the labels of the replicates or big blocks within an study (year-season-country-location-trial concatenation)."),
#'                                               p(strong("iBlock.-"),"The name of the column containing the labels of the incomplete blocks within an study."),
#'                                               p(strong("row.-"),"The name of the column containing the labels of the row coordinates for each record within an study."),
#'                                               p(strong("column.-"),"The name of the column containing the labels of the column coordinates for each record within an study."),
#'                                               p(strong("designation.-"),"The name of the column containing the labels of the individuals tested in the environments (e.g., Borlaug123, IRRI-154, Campeon, etc. )."),
#'                                               p(strong("gid.-"),"The name of the column containing the labels with the unique numerical identifier used within the database management system."),
#'                                               p(strong("entryType.-"),"The name of the column containing the labels of the genotype category (check, tester, entry, etc.)."),
#'                                               p(strong("trait.-"),"The name of the column(s) containing the numerical traits to be analyzed."),
#'                           ),
#'         )),
#'
#'         tags$div(id = ns('pheno_brapi_options'),
#'                  tags$span(id = ns('config_server_holder'),
#'                            shinydashboard::box(width = 4, title = span(icon('screwdriver-wrench'), ' Config Server'), status = 'success', solidHeader = TRUE,
#'                                                selectInput(
#'                                                  inputId = ns('pheno_db_type'),
#'                                                  label   = 'Database Type: ',
#'                                                  choices = list('EBS' = 'ebs', 'BMS' = 'bms', 'BreedBase' = 'breedbase')
#'                                                ),
#'                                                textInput(
#'                                                  inputId = ns('pheno_db_url'),
#'                                                  label = 'Server URL:',
#'                                                  placeholder = 'https://cb-qa.ebsproject.org'
#'                                                ),
#'                                                actionButton(
#'                                                  inputId = ns('pheno_db_save'),
#'                                                  label = 'Save',
#'                                                  width = '100%'
#'                                                ),
#'                            )
#'                  ),
#'
#'                  tags$span(id = ns('auth_server_holder'),
#'                            shinydashboard::box(width = 4, title = span(icon('key'), ' Authentication'), status = 'success', solidHeader = TRUE,
#'                                                tags$span(id = ns('pheno_db_user_holder'),
#'                                                          textInput(
#'                                                            inputId = ns('pheno_db_user'),
#'                                                            label = 'Username:'
#'                                                          )
#'                                                ),
#'                                                tags$span(id = ns('pheno_db_password_holder'),
#'                                                          passwordInput(
#'                                                            inputId = ns('pheno_db_password'),
#'                                                            label = 'Password:'
#'                                                          )
#'                                                ),
#'                                                tags$span(id = ns('no_auth_holder'),
#'                                                          checkboxInput(
#'                                                            inputId = ns('no_auth'),
#'                                                            label = 'No authentication required',
#'                                                            value = TRUE
#'                                                          )
#'                                                ),
#'
#'                                                actionButton(
#'                                                  inputId = ns('pheno_db_login'),
#'                                                  label = 'Login',
#'                                                  width = '100%'
#'                                                )                      ),
#'                  ),
#'
#'                  tags$span(id = ns('data_server_holder'),
#'                            shinydashboard::box(width = 4, title = span(icon('magnifying-glass-chart'), ' Data Source'), status = 'success', solidHeader = TRUE,
#'                                                tags$span(id = ns('pheno_db_crop_holder'),
#'                                                          selectizeInput(
#'                                                            inputId = ns('pheno_db_crop'),
#'                                                            label   = 'Crop: ',
#'                                                            choices = list()
#'                                                          )
#'                                                ),
#'                                                selectizeInput(
#'                                                  inputId = ns('pheno_db_program'),
#'                                                  label   = 'Breeding Program: ',
#'                                                  choices = list()
#'                                                ),
#'                                                uiOutput(ns('pheno_db_folder')),
#'                                                uiOutput(ns('pheno_db_trial')),
#'                                                actionButton(
#'                                                  inputId = ns('pheno_db_load'),
#'                                                  label = 'Load',
#'                                                  width = '100%'
#'                                                ),
#'                            )
#'                  ),
#'         ),
#'
#'         shinydashboard::box(width = 12,
#'                             if (!is.null(pheno_example)) {
#'                               checkboxInput(
#'                                 inputId = ns('pheno_example'),
#'                                 label = span('Load example ',
#'                                              a('phenotypic data', target = '_blank',
#'                                                href = pheno_example)),
#'                                 value = FALSE
#'                               )
#'                             },
#'
#'                             p(span("Note: If you have hybrid-crop data and need to map 'mother' and 'father' information for GCA models please provide that information in the Pedigree tab (you can use the same Phenotype file if those columns are there).", style="color:orange")),
#'
#'                             hr(),
#'                             uiOutput(ns('brapi_trait_map')),
#'                             DT::DTOutput(ns('preview_pheno')),
#'                             uiOutput(ns('pheno_map')),
#'                             actionButton(ns("concatenateEnv"), div(p(strong('Compute Environments', span('(*required)',style="color:red"))), style="display: inline-block; line-height:30px;"), icon = icon("play-circle"), style = "height: 45px"),
#'                             p(span("Note: 'Compute environment' will concatenate any info provided in optional columns: 'year', 'season', 'location', 'trial' and 'study' ", style="color:orange")),
#'                             textOutput(ns("outConcatenateEnv")),
#'         ),
#'
#'
#'       ),
#'       tabPanel(
#'         title = 'Genotypic',
#'         value = ns('tab2'),
#'         fluidRow(
#'           style = 'padding: 30px;',
#'
#'           # Source: Upload (web interface to temp local directory) or URL (optional username/password to access)
#'           # Accept *.gz format (7-Zip how-to reference), average genomic file size after compression is 5%
#'           selectInput(
#'             inputId = ns('geno_input'),
#'             label   = 'Genotypic SNPs Source*:',
#'             choices = list('HapMap Upload' = 'file', 'HapMap URL' = 'url',
#'                            'Table Upload' = 'matfile', 'Table URL' = 'matfileurl',
#'                            'VCF Upload' = 'vcf.file', 'VCF URL' = 'vcf.url'),
#'             width   = '200px'
#'           ),
#'           tags$span(id = ns('geno_file_holder'),
#'                     fileInput(
#'                       inputId = ns('geno_file'),
#'                       label   = NULL,
#'                       width   = '400px',
#'                       accept  = c('application/gzip', '.gz', '.txt', '.hmp', '.csv', '.vcf')
#'                     )
#'           ),
#'           textInput(
#'             inputId = ns('geno_url'),
#'             label   = NULL,
#'             value   = '',
#'             width   = '400px',
#'             placeholder = 'https://example.com/path/file.gz'
#'           ),
#'           hr(style = "border-top: 1px solid #4c4c4c;"),
#'           tags$span(id = ns('geno_table_mapping'),
#'                     column(4,
#'                            selectizeInput(
#'                              inputId = ns('geno_table_designation'),
#'                              label   = 'Designation column: ',
#'                              choices = list()
#'                            ),    ),
#'                     column(4,
#'                            selectizeInput(
#'                              inputId = ns('geno_table_firstsnp'),
#'                              label   = 'First SNP column: ',
#'                              choices = list()
#'                            ),   ),
#'                     column(4,
#'                            selectizeInput(
#'                              inputId = ns('geno_table_lastsnp'),
#'                              label   = 'Last SNP column: ',
#'                              choices = list()
#'                            ),
#'                     ),
#'           ),
#'           tags$div(id = ns('geno_table_options'),
#'                    shinydashboard::box(title = span(icon('screwdriver-wrench'), ' Options'), collapsible = TRUE, collapsed = TRUE, status = 'success', solidHeader = TRUE,
#'                                        shinyWidgets::prettyRadioButtons(ns('geno_sep'), 'Separator Character', selected = ',', inline = TRUE,
#'                                                                         choices = c('Comma' = ',', 'Semicolon' = ';', 'Tab' = "\t")),
#'
#'                                        shinyWidgets::prettyRadioButtons(ns('geno_quote'), 'Quoting Character', selected = '"', inline = TRUE,
#'                                                                         choices = c('None' = '', 'Double Quote' = '"', 'Single Quote' = "'")),
#'
#'                                        shinyWidgets::prettyRadioButtons(ns('geno_dec'), 'Decimal Points', selected = '.', inline = TRUE,
#'                                                                         choices = c('Dot' = '.', 'Comma' = ',')),
#'                    ),
#'           ),
#'           if (!is.null(geno_example)) {
#'             checkboxInput(
#'               inputId = ns('geno_example'),
#'               label = span('Load example ',
#'                            a('genotypic data', target = '_blank',
#'                              href = geno_example)),
#'               value = FALSE
#'             )
#'           },
#'
#'           fluidRow(
#'             style = 'padding-right: 0px; padding-left: 0px;',
#'             shinydashboard::box(
#'               title = 'Notes:',
#'               width = 12,
#'               solidHeader = TRUE,
#'               status = 'success',
#'               tags$ul(
#'                 tags$li('Accept HapMap, VCF, CSV formats (tab-delimited text file with a header row).
#'                          The HapMap and VCF files list SNPs in rows and Accessions (individual samples)
#'                          in columns, and viceversa in the case of the CSV. The first 11 columns of the HapMap
#'                          describe attributes of the SNP, but only the first 4 columns data are required for processing:
#'                          rs# (SNP id), alleles (e.g., C/G), chrom (chromosome), and pos (position).'),
#'
#'                 tags$li(
#'                   tags$span(
#'                     'Accept numeric coding ([0,1, and 2] or [-1,0, and 1] for reference/major,
#'                      heterozygous, and alternative/minor alleles respectively), or the ',
#'                     tags$a('IUPAC single-letter', target = '_blank', href = 'https://en.wikipedia.org/wiki/Nucleic_acid_notation#IUPAC_notation'),
#'                     'code (ref. ',
#'                     tags$a('https://doi.org/10.1093/nar/13.9.3021', target = '_blank', href = 'https://www.ncbi.nlm.nih.gov/pmc/articles/PMC341218/'),
#'                     '),', 'and double-letter code.'
#'                   )
#'                 ),
#'
#'                 tags$li('Position should be in bp (base pairs) not cM (CentiMorgan).'),
#'
#'                 tags$li(
#'                   tags$span(
#'                     'We recommend compressing your HapMap genotypic data using the gzip
#'                      format (*.gz extension) to significantly reduce file size. On average,
#'                      the compressed file size is only 5% of the original size. You can use
#'                      free software such as',
#'                     tags$a('7-Zip', href = 'https://www.7-zip.org', target = '_blank'),
#'                     'to perform the compression.'
#'                   )
#'                 ),
#'               )
#'             ),
#'           ),
#'
#'           # Verify file format: Hapmap file format (with reference link)
#'           # highlight that pos unit should be bp not cM
#'           # Report summary statistics (#acc, #snps, etc.)
#'           # to let the user verify before proceeding to the next step
#'           tableOutput(ns('chrom_summary')),
#'
#'           # Accessions exist in both phenotypic and genotypic files (will be used to train the model)
#'           # Accessions have genotypic data but no phenotypic (will predict, add to pheno data file with NA value)
#'           # Accessions have phenotypic data but no genotypic (filter them out from the pheno data file)
#'           verbatimTextOutput(ns('geno_summary')),
#'
#'           hr(),
#'           DT::DTOutput(ns('preview_geno')),
#'         ),
#'
#'
#'       ),
#'       tabPanel(
#'         title = 'Pedigree',
#'         value = ns('tab3'),
#'         # tags$p('Comming soon!', style = 'color: red; font-size: 20px; margin-top: 25px;'),
#'
#'         tags$br(),
#'
#'         selectInput(
#'           inputId = ns('ped_input'),
#'           label   = 'Data Source*: ',
#'           choices = list('Upload File' = 'file', 'Copy URL' = 'url'),
#'           width   = '200px'
#'         ),
#'
#'         tags$span(id = ns('ped_file_holder'),
#'                   fileInput(
#'                     inputId = ns('ped_file'),
#'                     label   = NULL,
#'                     width   = '400px',
#'                     accept  = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
#'                   )
#'         ),
#'
#'         textInput(
#'           inputId = ns('ped_url'),
#'           label   = NULL,
#'           value   = '',
#'           width   = '400px',
#'           placeholder = 'https://example.com/path/file.csv'
#'         ),
#'
#'         tags$span(id = ns('ped_csv_options'),
#'                   shinydashboard::box(title = 'Options', collapsible = TRUE, collapsed = TRUE, status = 'success', solidHeader = TRUE,
#'                                       shinyWidgets::prettyRadioButtons(ns('ped_sep'), 'Separator Character', selected = ',', inline = TRUE,
#'                                                                        choices = c('Comma' = ',', 'Semicolon' = ';', 'Tab' = "\t")),
#'
#'                                       shinyWidgets::prettyRadioButtons(ns('ped_quote'), 'Quoting Character', selected = '"', inline = TRUE,
#'                                                                        choices = c('None' = '', 'Double Quote' = '"', 'Single Quote' = "'")),
#'
#'                                       shinyWidgets::prettyRadioButtons(ns('ped_dec'), 'Decimal Points', selected = '.', inline = TRUE,
#'                                                                        choices = c('Dot' = '.', 'Comma' = ',')),
#'                   ),
#'         ),
#'
#'         if (!is.null(ped_example)) {
#'           checkboxInput(
#'             inputId = ns('ped_example'),
#'             label = span('Load example ',
#'                          a('pedigree data', target = '_blank',
#'                            href = ped_example)),
#'             value = FALSE
#'           )
#'         },
#'
#'         uiOutput(ns('ped_map')),
#'         p(span("**Unknown mothers or fathers should be set as missing data in your file.", style="color:orange")),
#'
#'
#'         verbatimTextOutput(ns('ped_summary')),
#'         hr(),
#'         DT::DTOutput(ns('preview_ped')),
#'
#'
#'       ),
#'       tabPanel(
#'         title = 'Weather',
#'         value = ns('tab4'),
#'         tabsetPanel(
#'           tabPanel("Specify coordinates", icon = icon("magnifying-glass-chart"),
#'                    br(),
#'                    uiOutput(ns("warningMessage")),
#'                    column(width = 12, style = "background-color:grey; color: #FFFFFF",
#'                           column(width = 2, p(strong("Environment")) ),
#'                           column(width = 2, p(strong("Latitude")) ),
#'                           column(width = 2, p(strong("Longitude")) ),
#'                           column(width = 2, p(strong("Planting Date")) ),
#'                           column(width = 2, p(strong("Harvesting Date")) ),
#'                           column(width = 2, p(strong("Extraction interval")) ),
#'                    ),
#'                    column(width = 12, style = "background-color:grey; color: #FFFFFF",
#'                           column(width = 2, uiOutput(ns("environment")) ),
#'                           column(width = 2, uiOutput(ns("latitude")) ),
#'                           column(width = 2, uiOutput(ns("longitude")) ),
#'                           column(width = 2, uiOutput(ns("plantingDate")) ),
#'                           column(width = 2, uiOutput(ns("harvestingDate")) ),
#'                           column(width = 2, selectInput(ns("temporal"),label=NULL, choices = list("hourly","daily","monthly"), selected = "hourly"  ) ),
#'                    ),
#'                    h4(strong(span("Visualizations below aim to help you pick the right parameter values. Please inspect them.", style="color:green"))),
#'                    hr(style = "border-top: 3px solid #4c4c4c;"),
#'                    shinydashboard::box(status="success",width = 12, solidHeader = TRUE,
#'                                        column(width=12, style = "height:410px; overflow-y: scroll;overflow-x: scroll;",
#'                                               p(span("Preview of coordinates selected for extraction.", style="color:black")),
#'                                               plotly::plotlyOutput(ns("plotMeteo")),
#'                                        ),
#'                    )
#'           ),
#'           tabPanel("Run extraction", icon = icon("play"),
#'                    br(),
#'                    actionButton(ns("rungetWeather"), "Extract", icon = icon("play-circle")),
#'                    textOutput(ns("outgetWeather")),
#'           ),
#'         ), # end of tabset
#'         # tags$p('Comming soon!', style = 'color: red; font-size: 20px; margin-top: 25px;'),
#'
#'
#'       ),
#'       tabPanel(
#'         title = 'QTL profile',
#'         value = ns('tab5'),
#'         fluidRow(
#'           style = 'padding: 30px;',
#'           # Source: Upload (web interface to temp local directory) or URL (optional username/password to access)
#'           # Accept *.gz format (7-Zip how-to reference), average genomic file size after compression is 5%
#'           selectInput(
#'             inputId = ns('qtl_input'),
#'             label   = 'QTL Source*:',
#'             choices = list('Table Upload' = 'qtlfile', 'Table URL' = 'qtlfileurl' ),
#'             width   = '200px'
#'           ),
#'           tags$span(id = ns('qtl_file_holder'),
#'                     fileInput(
#'                       inputId = ns('qtl_file'),
#'                       label   = NULL,
#'                       width   = '400px',
#'                       accept  = c('.txt', '.csv')
#'                     )
#'           ),
#'           textInput(
#'             inputId = ns('qtl_url'),
#'             label   = NULL,
#'             value   = '',
#'             width   = '400px',
#'             placeholder = 'https://example.com/path/file.csv'
#'           ),
#'           if (!is.null(qtl_example)) {
#'             checkboxInput(
#'               inputId = ns('qtl_example'),
#'               label = span('Load example ',
#'                            a('QTL data', target = '_blank',
#'                              href = qtl_example)),
#'               value = FALSE
#'             )
#'           },
#'           hr(style = "border-top: 1px solid #4c4c4c;"),
#'           tags$span(id = ns('qtl_table_mapping'),
#'                     column(4,
#'                            selectizeInput(
#'                              inputId = ns('qtl_table_designation'),
#'                              label   = 'Designation column: ',
#'                              choices = list()
#'                            ),    ),
#'                     column(8,
#'                            selectizeInput(
#'                              inputId = ns('qtl_table_firstsnp'),
#'                              label   = 'QTL columns: ', multiple=TRUE,
#'                              choices = list()
#'                            ),   ),
#'           ),
#'           tags$div(id = ns('qtl_table_options'),
#'                    shinydashboard::box(title = span(icon('screwdriver-wrench'), ' Options'), collapsible = TRUE, collapsed = TRUE, status = 'success', solidHeader = TRUE,
#'                                        shinyWidgets::prettyRadioButtons(ns('qtl_sep'), 'Separator Character', selected = ',', inline = TRUE,
#'                                                                         choices = c('Comma' = ',', 'Semicolon' = ';', 'Tab' = "\t")),
#'
#'                                        shinyWidgets::prettyRadioButtons(ns('qtl_quote'), 'Quoting Character', selected = '"', inline = TRUE,
#'                                                                         choices = c('None' = '', 'Double Quote' = '"', 'Single Quote' = "'")),
#'
#'                                        shinyWidgets::prettyRadioButtons(ns('qtl_dec'), 'Decimal Points', selected = '.', inline = TRUE,
#'                                                                         choices = c('Dot' = '.', 'Comma' = ',')),
#'                    ),
#'           ),
#'           hr(style = "border-top: 1px solid #4c4c4c;"),
#'           DT::DTOutput(ns('preview_qtl')),
#'         ),
#'
#'
#'       ),
#'     ),
#'
#'     uiOutput(ns('navigate')),
#'   )
#' }
#'
#' #' getData Server Functions
#' #'
#' #' @noRd
#' mod_getData_server <- function(id, map = NULL, data = NULL, res_auth=NULL){
#'   moduleServer(id , function(input, output, session){
#'     ns <- session$ns
#'
#'     ### Phenotypic tab controls ################################################
#'
#'     observeEvent(
#'       input$pheno_input,
#'       if(length(input$pheno_input) > 0){ # added
#'         if (input$pheno_input == 'file') {
#'           golem::invoke_js('showid', ns('pheno_file_holder'))
#'           golem::invoke_js('hideid', ns('pheno_url'))
#'           golem::invoke_js('showid', ns('pheno_csv_options'))
#'           golem::invoke_js('hideid', ns('pheno_brapi_options'))
#'           golem::invoke_js('showid', ns('pheno_map'))
#'           updateCheckboxInput(session, 'pheno_example', value = FALSE)
#'         } else if (input$pheno_input == 'url') {
#'           golem::invoke_js('hideid', ns('pheno_file_holder'))
#'           golem::invoke_js('showid', ns('pheno_url'))
#'           golem::invoke_js('showid', ns('pheno_csv_options'))
#'           golem::invoke_js('hideid', ns('pheno_brapi_options'))
#'           golem::invoke_js('showid', ns('pheno_map'))
#'         } else { # brapi
#'           golem::invoke_js('hideid', ns('pheno_file_holder'))
#'           golem::invoke_js('hideid', ns('pheno_url'))
#'           golem::invoke_js('hideid', ns('pheno_csv_options'))
#'           golem::invoke_js('showid', ns('pheno_brapi_options'))
#'           golem::invoke_js('showid', ns('config_server_holder'))
#'           golem::invoke_js('hideid', ns('auth_server_holder'))
#'           golem::invoke_js('hideid', ns('data_server_holder'))
#'           golem::invoke_js('hideid', ns('pheno_map'))
#'           updateCheckboxInput(session, 'pheno_example', value = FALSE)
#'         }
#'       }
#'     )
#'
#'     observeEvent(
#'       input$pheno_db_save,
#'       {
#'         if (input$pheno_db_url == '') return(NULL)
#'
#'         # TODO: check if it has a BrAPI endpoints
#'         # http://msdn.microsoft.com/en-us/library/ff650303.aspx
#'         if (!grepl("^(ht|f)tp(s?)\\:\\/\\/[0-9a-zA-Z]([-.\\w]*[0-9a-zA-Z])*(:(0-9)*)*(\\/?)([a-zA-Z0-9\\-\\.\\?\\,\\'\\/\\\\\\+&%\\$#_=]*)?$", input$pheno_db_url)) {
#'           shinyWidgets::show_alert(title = 'Invalid URL!', type = 'error')
#'           return(NULL)
#'         }
#'
#'         golem::invoke_js('showid', ns('auth_server_holder'))
#'         golem::invoke_js('hideid', ns('data_server_holder'))
#'
#'         if (input$pheno_db_type == 'ebs') {
#'           golem::invoke_js('hideid', ns('pheno_db_user_holder'))
#'           golem::invoke_js('hideid', ns('pheno_db_password_holder'))
#'           golem::invoke_js('hideid', ns('pheno_db_crop_holder'))
#'           golem::invoke_js('hideid', ns('no_auth_holder'))
#'
#'           QBMS::set_qbms_config(url = input$pheno_db_url, engine = 'ebs', brapi_ver = 'v2')
#'         } else if (input$pheno_db_type == 'bms') {
#'           golem::invoke_js('showid', ns('pheno_db_user_holder'))
#'           golem::invoke_js('showid', ns('pheno_db_password_holder'))
#'           golem::invoke_js('showid', ns('pheno_db_crop_holder'))
#'           golem::invoke_js('hideid', ns('no_auth_holder'))
#'
#'           QBMS::set_qbms_config(url = input$pheno_db_url, engine = 'bms', brapi_ver = 'v1')
#'         } else if (input$pheno_db_type == 'breedbase') {
#'           golem::invoke_js('hideid', ns('pheno_db_user_holder'))
#'           golem::invoke_js('hideid', ns('pheno_db_password_holder'))
#'           golem::invoke_js('hideid', ns('pheno_db_crop_holder'))
#'           golem::invoke_js('showid', ns('no_auth_holder'))
#'
#'           updateCheckboxInput(session, 'no_auth', value = TRUE)
#'
#'           QBMS::set_qbms_config(url = input$pheno_db_url, engine = 'breedbase', brapi_ver = 'v1')
#'         }
#'
#'         output$preview_pheno <- DT::renderDT(NULL)
#'       }
#'     )
#'
#'     observeEvent(
#'       input$pheno_db_login,
#'       {
#'         tryCatch(
#'           expr = {
#'             if (input$pheno_db_type == 'ebs') {
#'               ebs_instance <- sub('https?://([^\\.]+)\\.([^/]+).*', '\\1', input$pheno_db_url)
#'               ebs_domain   <- sub('https?://([^\\.]+)\\.([^/]+).*', '\\2', input$pheno_db_url)
#'
#'               # EBS QA instance (https://cb-qa.ebsproject.org)
#'               if (ebs_domain == 'ebsproject.org' & ebs_instance == 'cb-qa') {
#'                 ebs_brapi <- sub('(.+)//cb-qa\\.(.+)', '\\1//cbbrapi-qa.\\2', input$pheno_db_url)
#'                 QBMS::set_qbms_config(url = ebs_brapi, engine = 'ebs', brapi_ver = 'v2')
#'                 QBMS::login_oauth2(authorize_url = 'https://auth-dev.ebsproject.org/oauth2/authorize',
#'                                    access_url    = 'https://auth-dev.ebsproject.org/oauth2/token',
#'                                    client_id     = '5crahiqorgj0lppt3n9dkulkst',
#'                                    client_secret = '1sf4tipbp4arj3d5cncjmrvk9c2cu30gor5618hnh8rgkp6v5fs')
#'
#'                 # IRRI production instance (https://prod-cb.ebs.irri.org/)
#'               } else if (ebs_domain == 'ebs.irri.org') {
#'                 ebs_brapi <- sub('(.+)//prod-cb\\.(.+)', '\\1//prod-cbbrapi.\\2', input$pheno_db_url)
#'                 QBMS::set_qbms_config(url = ebs_brapi, engine = 'ebs', brapi_ver = 'v2')
#'                 QBMS::login_oauth2(authorize_url = 'https://auth.ebsproject.org/oauth2/authorize',
#'                                    access_url    = 'https://auth.ebsproject.org/oauth2/token',
#'                                    client_id     = '7s4mb2tu4884679rmbucsuopk1',
#'                                    client_secret = 'nf4m8qobpj8eplpg0a9bbo63g69vh0r3p8rbovtfb0udd28rnk9')
#'
#'                 # CIMMYT instances (https://cb-maize.ebs.cimmyt.org, https://cb-staging.ebs.cimmyt.org/)
#'                 # or evaluation instances (https://cb-mee.ebsproject.org, https://cb-ree.ebsproject.org, and https://cb-wee.ebsproject.org)
#'               } else if (ebs_domain == 'ebs.cimmyt.org' || ebs_instance %in% c('cb-mee', 'cb-ree', 'cb-wee')) {
#'                 ebs_brapi <- sub('(.+)//cb-(.+)', '\\1//cbbrapi-\\2', input$pheno_db_url)
#'                 QBMS::set_qbms_config(url = ebs_brapi, engine = 'ebs', brapi_ver = 'v2')
#'                 QBMS::login_oauth2(authorize_url = 'https://auth.ebsproject.org/oauth2/authorize',
#'                                    access_url    = 'https://auth.ebsproject.org/oauth2/token',
#'                                    client_id     = '346lau0avcptntd1ksbmgdi5c',
#'                                    client_secret = 'q5vnvakfj800ibh5tvqut73vj8klv1tpt6ugtmuneh6d2jb28i3')
#'               } else {
#'                 shinyWidgets::show_alert(title = 'Oops!', type = 'warning', "We can't recognize this EBS instance :-(")
#'                 return()
#'               }
#'             } else if (input$pheno_db_type == 'bms') {
#'               QBMS::login_bms(input$pheno_db_user, input$pheno_db_password)
#'             } else if (input$pheno_db_type == 'breedbase') {
#'               if (input$no_auth) {
#'                 QBMS::set_qbms_config(url = input$pheno_db_url, engine = 'breedbase', brapi_ver = 'v1', no_auth = TRUE)
#'               } else {
#'                 QBMS::login_breedbase(input$pheno_db_user, input$pheno_db_password)
#'               }
#'             }
#'
#'             golem::invoke_js('showid', ns('data_server_holder'))
#'
#'             if (input$pheno_db_type == 'bms') {
#'               shinybusy::show_modal_spinner('fading-circle', text = 'Loading Crops...')
#'
#'               pheno_db_crops <- QBMS::list_crops()
#'
#'               updateSelectizeInput(session,
#'                                    inputId = 'pheno_db_crop',
#'                                    label   = 'Crop: ',
#'                                    choices = c('', pheno_db_crops))
#'               shinybusy::remove_modal_spinner()
#'             } else {
#'               shinybusy::show_modal_spinner('fading-circle', text = 'Loading Programs...')
#'
#'               pheno_db_programs <- QBMS::list_programs()
#'
#'               updateSelectizeInput(session,
#'                                    inputId = 'pheno_db_program',
#'                                    label   = 'Breeding Program: ',
#'                                    choices = c('', pheno_db_programs))
#'
#'               shinybusy::remove_modal_spinner()
#'             }
#'
#'             output$preview_pheno <- DT::renderDT(NULL)
#'
#'           },
#'           error = function(e) {
#'             shinyWidgets::show_alert(title = 'Invalid Credentials!', type = 'error')
#'           }
#'         )
#'       }
#'     )
#'
#'     observeEvent(
#'       input$pheno_db_crop,
#'       if (input$pheno_db_crop != '') {
#'         shinybusy::show_modal_spinner('fading-circle', text = 'Loading Crops...')
#'
#'         QBMS::set_crop(input$pheno_db_crop)
#'
#'         pheno_db_programs <- QBMS::list_programs()
#'
#'         updateSelectizeInput(session,
#'                              inputId = 'pheno_db_program',
#'                              label   = 'Breeding Program: ',
#'                              choices = c('', pheno_db_programs))
#'
#'         shinybusy::remove_modal_spinner()
#'       }
#'     )
#'
#'     observeEvent(
#'       input$pheno_db_program,
#'       if (input$pheno_db_program != '') {
#'         if (input$pheno_db_type == 'breedbase') {
#'           shinybusy::show_modal_spinner('fading-circle', text = 'Loading Folders...')
#'         } else {
#'           shinybusy::show_modal_spinner('fading-circle', text = 'Loading Trials...')
#'         }
#'
#'         QBMS::set_program(input$pheno_db_program)
#'
#'         pheno_db_trials <- QBMS::list_trials()
#'
#'         if (input$pheno_db_type == 'breedbase') {
#'           output$pheno_db_folder <- renderUI({
#'             selectizeInput(
#'               inputId = ns('pheno_db_folder'),
#'               label   = 'Select Folder: ',
#'               choices = c('', pheno_db_trials)
#'             )
#'           })
#'         } else {
#'           output$pheno_db_trial <- renderUI({
#'             selectizeInput(
#'               inputId = ns('pheno_db_trial'),
#'               label   = 'Trial/Study: ',
#'               choices = pheno_db_trials,
#'               multiple = TRUE
#'             )
#'           })
#'         }
#'
#'         shinybusy::remove_modal_spinner()
#'       }
#'     )
#'
#'     observeEvent(
#'       input$pheno_db_folder,
#'       if (input$pheno_db_folder != '') {
#'         shinybusy::show_modal_spinner('fading-circle', text = 'Loading Trials...')
#'
#'         QBMS::set_trial(input$pheno_db_folder)
#'
#'         tryCatch(
#'           expr = {
#'             pheno_db_studies <- QBMS::list_studies()$studyName
#'
#'             output$pheno_db_trial <- renderUI({
#'               selectizeInput(
#'                 inputId = ns('pheno_db_trial'),
#'                 label   = 'Trial/Study: ',
#'                 choices = pheno_db_studies,
#'                 multiple = TRUE
#'               )
#'             })
#'           },
#'           error = function(e) {
#'             if (input$pheno_db_type == 'breedbase') {
#'               shinyWidgets::show_alert(title = 'Empty Folder!',
#'                                        text = 'No trials found in the selected folder! Please choose the last/final folder that contains your trials in any nested folder structure.',
#'                                        type = 'warning')
#'             } else {
#'               shinyWidgets::show_alert(title = e, type = 'error')
#'             }
#'
#'             output$pheno_db_trial <- NULL
#'
#'             return(NULL)
#'           }
#'         )
#'
#'         shinybusy::remove_modal_spinner()
#'       }
#'     )
#'
#'     observeEvent(
#'       input$no_auth,
#'       if(length(input$no_auth) > 0){
#'         if (input$no_auth) {
#'           golem::invoke_js('hideid', ns('pheno_db_user_holder'))
#'           golem::invoke_js('hideid', ns('pheno_db_password_holder'))
#'         } else {
#'           golem::invoke_js('showid', ns('pheno_db_user_holder'))
#'           golem::invoke_js('showid', ns('pheno_db_password_holder'))
#'         }
#'       }
#'     )
#'
#'     observeEvent(
#'       input$brapi_traits,
#'       {
#'         temp <- data()
#'
#'         temp$metadata$pheno <- temp$metadata$pheno[temp$metadata$pheno$parameter != 'trait',]
#'
#'         for (i in input[['brapi_traits']]) {
#'           temp$metadata$pheno <- rbind(temp$metadata$pheno, data.frame(parameter = 'trait', value = i))
#'           temp$data$pheno[[i]] <- as.numeric(temp$data$pheno[[i]])
#'         }
#'
#'         data(temp)
#'       }
#'     )
#'
#'     pheno_data <- reactive({
#'       tryCatch(
#'         expr = {
#'           if(length(input$pheno_input) > 0){
#'             if (input$pheno_input == 'file') {
#'               if (is.null(input$pheno_file)) {return(NULL)}else{
#'                 data <- as.data.frame(data.table::fread(input$pheno_file$datapath, sep = input$pheno_sep,
#'                                                         quote = input$pheno_quote, dec = input$pheno_dec, header = TRUE))
#'               }
#'             } else if (input$pheno_input == 'url') {
#'               if (input$pheno_url == ''){return(NULL)} else{
#'                 data <- as.data.frame(data.table::fread(input$pheno_url, sep = input$pheno_sep,
#'                                                         quote = input$pheno_quote, dec = input$pheno_dec, header = TRUE))
#'               }
#'             } else if (input$pheno_input == 'brapi') {
#'               if (input$pheno_db_load != 1){
#'                 return(NULL)
#'               } else {
#'                 shinybusy::show_modal_spinner('fading-circle', text = 'Loading Data...')
#'
#'                 if (input$pheno_db_type == 'breedbase') {
#'                   QBMS::set_study(input$pheno_db_trial)
#'                   data <- QBMS::get_study_data()
#'
#'                   # get breedbase trait ontology
#'                   ontology <- QBMS::get_trial_obs_ontology()
#'                   fields   <- colnames(data)
#'
#'                   # replace long trait names with short ones from the ontology
#'                   for (i in 1:length(fields)) {
#'                     j <- which(ontology$name %in% fields[i])
#'                     if (length(j) > 0) {
#'                       if(!is.na(ontology$synonyms[[j]][1])) {
#'                         fields[i] <- ontology$synonyms[[j]][1]
#'                       }
#'                     }
#'                   }
#'
#'                   colnames(data) <- fields
#'
#'                 } else {
#'                   QBMS::set_trial(input$pheno_db_trial)
#'                   data <- QBMS::get_trial_data()
#'                 }
#'
#'                 data$trialName <- input$pheno_db_trial
#'
#'                 shinybusy::remove_modal_spinner()
#'
#'                 shinyWidgets::show_alert(title = 'Done!', type = 'success')
#'               }
#'             }
#'
#'             return(data)
#'           }
#'         },
#'         error = function(e) {
#'           shinybusy::remove_modal_spinner()
#'           if (input$pheno_db_type == 'ebs') {
#'             shinyWidgets::show_alert(title = 'Access Denied!', type = 'error', text = 'You do not have permission to access this trial.')
#'           } else {
#'             shinyWidgets::show_alert(title = 'Error!', type = 'error', text = e)
#'           }
#'
#'           return(NULL)
#'         }
#'       )
#'     })
#'
#'     observeEvent(
#'       pheno_data(),
#'       {
#'         #if (is.null(data())) return(NULL)
#'
#'         temp <- data()
#'         temp$data$pheno <- pheno_data()
#'
#'         output$preview_pheno <- DT::renderDT({
#'           DT::datatable(temp$data$pheno,
#'                         extensions = 'Buttons',
#'                         options = list(dom = 'Blfrtip',
#'                                        scrollX = TRUE,
#'                                        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
#'                                        lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All')))
#'           )
#'         })
#'
#'         if (input$pheno_input == 'brapi') {
#'           output$brapi_trait_map <- renderUI({
#'             selectInput(
#'               inputId  = ns('brapi_traits'),
#'               label    = 'Trait(s):',
#'               multiple = TRUE,
#'               choices  = as.list(c('', colnames(temp$data$pheno))),
#'             )
#'           })
#'
#'           ### BrAPI auto mapping ###############################################
#'           mapping <- list(
#'             ebs = list(
#'               year = 'year',
#'               study = 'studyName',
#'               rep = 'rep',
#'               iBlock = 'block',
#'               row = 'positionCoordinateY',
#'               col = 'positionCoordinateX',
#'               designation = 'germplasmName',
#'               gid = 'germplasmDbId',
#'               location = 'locationName',
#'               trial = 'trialName',
#'               entryType = 'entryType'
#'             ),
#'             bms = list(
#'               year = 'year',
#'               study = 'studyName',
#'               rep = 'rep',
#'               iBlock = 'block',
#'               row = 'positionCoordinateY',
#'               col = 'positionCoordinateX',
#'               designation = 'germplasmName',
#'               gid = 'germplasmDbId',
#'               location = 'studyName',
#'               trial = 'trialName',
#'               entryType = 'entryType'
#'             ),
#'             breedbase = list(
#'               year = 'studyYear',
#'               study = 'studyName',
#'               rep = 'replicate',
#'               iBlock = 'blockNumber',
#'               row = 'rowNumber',
#'               col = 'colNumber',
#'               designation = 'germplasmName',
#'               gid = 'germplasmDbId',
#'               location = 'locationName',
#'               trial = 'trialName',
#'               entryType = 'entryType'
#'             )
#'           )
#'
#'           for (field in c('year', 'study', 'rep', 'iBlock', 'row', 'col', 'designation', 'gid', 'location', 'trial', 'entryType')) {
#'             if (mapping[[input$pheno_db_type]][[field]] %in% colnames(temp$data$pheno)) {
#'               if (field %in% temp$metadata$pheno$parameter) {
#'                 temp$metadata$pheno[temp$metadata$pheno$parameter == field, 'value'] <- mapping[[input$pheno_db_type]][[field]]
#'               } else {
#'                 temp$metadata$pheno <- rbind(temp$metadata$pheno, data.frame(parameter = field, value = mapping[[input$pheno_db_type]][[field]]))
#'               }
#'             }
#'           }
#'
#'           # shinybusy::show_modal_spinner('fading-circle', text = 'Loading Ontology...')
#'           #
#'           # traits <- QBMS::get_trial_obs_ontology()
#'           #
#'           # temp$metadata$pheno <- temp$metadata$pheno[temp$metadata$pheno$parameter != 'trait',]
#'           #
#'           # for (i in traits$name) {
#'           #   temp$metadata$pheno <- rbind(temp$metadata$pheno, data.frame(parameter = 'trait', value = i))
#'           # }
#'           #
#'           # shinybusy::remove_modal_spinner()
#'
#'
#'           # dummy pedigree table
#'           temp$data$pedigree <- data.frame(germplasmName = unique(temp$data$pheno$germplasmName), mother = NA, father = NA, yearOfOrigin = NA)
#'           temp$metadata$pedigree <- data.frame(parameter=c("designation","mother","father","yearOfOrigin"), value=c("germplasmName","mother","father","yearOfOrigin") )
#'           # stage       <- NA
#'           # pipeline    <- NA
#'           # country   **<- get_study_info()$locationDbId, then list_locations()
#'           # season    * <- get_study_info()$seasons
#'
#'           ####################################################################
#'         }
#'         data(temp)
#'       }
#'     )
#'
#'     output$pheno_map <- renderUI({
#'       if (is.null(pheno_data())) return(NULL)
#'
#'       header <- colnames(pheno_data())
#'       pheno_map <- lapply(map, function(x) {
#'         column(3,
#'                selectInput(
#'                  inputId  = ns(paste0('select', x)),
#'                  label    = HTML(ifelse(x %in% c('designation','trait','location'), as.character(p(x, span('(*required)',style="color:red"))),  ifelse(x %in% c('rep','iBlock','row','col'), as.character(p(x, span('(*recommended)',style="color:grey"))), as.character(p(x, span('(*optional)',style="color:grey")))  )   ) ) ,
#'                  multiple = ifelse(x == 'trait', TRUE, FALSE),
#'                  choices  = as.list(c('', header)),
#'                  selected = ifelse(length(grep(x,header, ignore.case = TRUE)) > 0, header[grep(x,header, ignore.case = TRUE)[1]], '')
#'                ),
#'
#'                # shinyBS::bsTooltip(ns(paste0('select', x)), 'Mapping this!', placement = 'left', trigger = 'hover'),
#'
#'                renderPrint({
#'                  # req(input[[paste0('select', x)]])
#'                  temp <- data()
#'                  if (x == 'trait') {
#'                    temp$metadata$pheno <- temp$metadata$pheno[temp$metadata$pheno$parameter != 'trait',]
#'                    for (i in input[[paste0('select', x)]]) {
#'                      temp$metadata$pheno <- rbind(temp$metadata$pheno, data.frame(parameter = 'trait', value = i))
#'                      if(!is.numeric(temp$data$pheno[,i])){temp$data$pheno[,i] <- as.numeric(gsub(",","",temp$data$pheno[,i]))}
#'                    }
#'                  } else { # is any other column other than trait
#'                    if (x %in% temp$metadata$pheno$parameter & input[[paste0('select', x)]] != '') {
#'                      temp$metadata$pheno[temp$metadata$pheno$parameter == x, 'value'] <- input[[paste0('select', x)]]
#'                    } else {
#'                      temp$metadata$pheno <- rbind(temp$metadata$pheno, data.frame(parameter = x, value = input[[paste0('select', x)]]))
#'                    }
#'                    if(x %in% c("designation","study") & input[[paste0('select', x)]] != ''){
#'                      temp$data$pheno[,input[[paste0('select', x)]]] <- stringi::stri_trans_general(temp$data$pheno[,input[[paste0('select', x)]]], "Latin-ASCII")
#'                    }
#'                    if(input[[paste0('select', x)]] == ''){
#'                      temp$metadata$pheno <- temp$metadata$pheno[-which(temp$metadata$pheno$parameter == x), ]
#'                    }
#'                  }
#'                  if (x == 'designation') {
#'                    if(input[[paste0('select', x)]] != ''){
#'                      temp$data$pedigree <- data.frame(designation = unique(pheno_data()[[input[[paste0('select', x)]]]]), mother = NA, father = NA, yearOfOrigin = NA)
#'                      temp$metadata$pedigree <- data.frame(parameter=c("designation","mother","father","yearOfOrigin"), value=c("designation","mother","father","yearOfOrigin") )
#'                    }
#'                  }
#'                  data(temp)
#'                }),
#'         )
#'       })
#'       fluidRow(do.call(tagList, pheno_map))
#'     })
#'
#'     observeEvent(
#'       input$pheno_example,
#'       if(length(input$pheno_example) > 0){
#'         if (input$pheno_example) {
#'           updateSelectInput(session, 'pheno_input', selected = 'url')
#'
#'           pheno_example_url <-  paste0(session$clientData$url_protocol, '//',
#'                                        session$clientData$url_hostname, ':',
#'                                        session$clientData$url_port,
#'                                        session$clientData$url_pathname,
#'                                        pheno_example)
#'
#'           updateTextInput(session, 'pheno_url', value = pheno_example_url)
#'
#'           golem::invoke_js('hideid', ns('pheno_file_holder'))
#'           golem::invoke_js('showid', ns('pheno_url'))
#'           golem::invoke_js('hideid', ns('pheno_db'))
#'         } else {
#'           updateSelectInput(session, 'pheno_input', selected = 'file')
#'           updateTextInput(session, 'pheno_url', value = '')
#'
#'           golem::invoke_js('showid', ns('pheno_file_holder'))
#'           golem::invoke_js('hideid', ns('pheno_url'))
#'           golem::invoke_js('hideid', ns('pheno_db'))
#'         }
#'       }
#'
#'     )
#'     outConcatenateEnv <- eventReactive(input$concatenateEnv, { # button to concatenate other columns in study
#'       req(data())
#'       myObject <- data()
#'       myObject$metadata$pheno <- myObject$metadata$pheno %>% dplyr::arrange(factor(parameter, levels = c("year","season","location","trial","study")))
#'       environmentColumn <- which(myObject$metadata$pheno$parameter == "environment")
#'       if(length(environmentColumn) > 0){ # user has mapped an study column
#'         otherEnvironmentColumn <- which(myObject$metadata$pheno$parameter %in% c("year","season","location","trial","study"))
#'         if(length(otherEnvironmentColumn) > 1){ # if user has mapped more than one column
#'           myObject$data$pheno[,myObject$metadata$pheno[environmentColumn, "value"]] <- apply(myObject$data$pheno[, myObject$metadata$pheno[otherEnvironmentColumn, "value"], drop=FALSE],1, function(x){paste(x, collapse = "_")} )
#'           data(myObject)
#'           shinyalert::shinyalert(title = "Success!", text = paste("Columns",paste(myObject$metadata$pheno[otherEnvironmentColumn, "value"], collapse = ", "), "concatenated and pasted in the",myObject$metadata$pheno[environmentColumn, "value"], "column"), type = "success")
#'           # cat(paste("Columns",paste(myObject$metadata$pheno[otherEnvironmentColumn, "value"], collapse = ", "), "concatenated and pasted in the",myObject$metadata$pheno[environmentColumn, "value"], "column"))
#'         }else if(length(otherEnvironmentColumn) == 1){
#'           myObject$data$pheno[,myObject$metadata$pheno[environmentColumn, "value"]] <- myObject$data$pheno[, myObject$metadata$pheno[otherEnvironmentColumn, "value"]]
#'           data(myObject)
#'           shinyalert::shinyalert(title = "Success!", text = paste0("No additional columns to concatenate. '", myObject$metadata$pheno[environmentColumn, "value"], "' column is equal to ", myObject$metadata$pheno[otherEnvironmentColumn, "value"]), type = "success")
#'           # cat("No additional columns to concatenate to your 'study' column")
#'         }else {
#'           myObject$metadata$pheno <- myObject$metadata$pheno[-which(myObject$metadata$pheno$parameter == "environment"), ]
#'           myObject$data$pheno <- myObject$data$pheno [,!(names(myObject$data$pheno) %in% c("environment"))]
#'           data(myObject)
#'           shinyalert::shinyalert(title = "Error!", text = paste("Please map at least one of the columns 'year', 'season', 'location', 'trial' or 'study' to be able to compute the environments and perform a genetic evaluation "), type = "error")
#'         }
#'       }else{ # user has not mapped an study column, we will add it
#'         otherEnvironmentColumn <- which(myObject$metadata$pheno$parameter %in% c("year","season","location","trial","study"))
#'         if(length(otherEnvironmentColumn) > 1){ # if user has mapped more than one column
#'           myObject$data$pheno[,"environment"] <- apply(myObject$data$pheno[, myObject$metadata$pheno[otherEnvironmentColumn, "value"], drop=FALSE],1, function(x){paste(x, collapse = "_")} )
#'           myObject$metadata$pheno <- rbind(myObject$metadata$pheno, data.frame(parameter = 'environment', value = 'environment' ))
#'           data(myObject)
#'           shinyalert::shinyalert(title = "Success!", text = paste("Columns",paste(myObject$metadata$pheno[otherEnvironmentColumn, "value"], collapse = ", "), "concatenated and pasted in a column named 'environment' "), type = "success")
#'           # cat(paste("Columns",paste(myObject$metadata$pheno[otherEnvironmentColumn, "value"], collapse = ", "), "concatenated and pasted in a column named 'environment' "))
#'         }else if(length(otherEnvironmentColumn) == 1){
#'           myObject$data$pheno[,"environment"] <- myObject$data$pheno[, myObject$metadata$pheno[otherEnvironmentColumn, "value"]]
#'           myObject$metadata$pheno <- rbind(myObject$metadata$pheno, data.frame(parameter = 'environment', value = 'environment' ))
#'           data(myObject)
#'           shinyalert::shinyalert(title = "Success!", text = paste("No additional columns to concatenate. 'environment' column is equal to", myObject$metadata$pheno[otherEnvironmentColumn, " value"]), type = "success")
#'         }else{
#'           shinyalert::shinyalert(title = "Error!", text = paste("Please map at least one of the columns 'year', 'season', 'location', 'trial' or 'study' to be able to do compute the environments and perform a genetic evaluation "), type = "error")
#'           # cat(paste("Please map at least one of the columns 'year', 'season', 'location', 'trial' or 'study' to be able to do compute the environments and perform a genetic evaluation "))
#'         }
#'       }
#'     })
#'     output$outConcatenateEnv <- renderPrint({
#'       outConcatenateEnv()
#'     })
#'
#'     # change color of updated study column
#'     observeEvent(input$concatenateEnv,{
#'       output$preview_pheno <- DT::renderDT({
#'         myObject <- data()
#'         phenoColnames <- colnames(myObject$data$pheno)
#'         if("environment" %in% phenoColnames){
#'           newPhenoColnames <- c("environment", setdiff(phenoColnames, "environment"))
#'           myObject$data$pheno <- myObject$data$pheno[, newPhenoColnames]
#'           environmentColumnName <- myObject$metadata$pheno$value[which(myObject$metadata$pheno$parameter == "environment")]
#'           DT::datatable(myObject$data$pheno,
#'                         extensions = 'Buttons',
#'                         options = list(dom = 'Blfrtip',
#'                                        scrollX = TRUE,
#'                                        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
#'                                        lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All')))) %>%
#'             DT::formatStyle(environmentColumnName, backgroundColor = "#009E60")
#'         } else{
#'           DT::datatable(myObject$data$pheno,
#'                         extensions = 'Buttons',
#'                         options = list(dom = 'Blfrtip',
#'                                        scrollX = TRUE,
#'                                        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
#'                                        lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All'))))
#'         }
#'       })
#'     })
#'     ### Genotypic tab controls #################################################
#'
#'     observeEvent(
#'       input$geno_input,
#'       if(length(input$geno_input) > 0){ # added
#'         if (input$geno_input %in% c('file', 'vcf.file')) {
#'           golem::invoke_js('showid', ns('geno_file_holder'))
#'           golem::invoke_js('hideid', ns('geno_url'))
#'           golem::invoke_js('hideid', ns('geno_table_mapping'))
#'           golem::invoke_js('hideid', ns('geno_table_options'))
#'           updateCheckboxInput(session, 'geno_example', value = FALSE)
#'         } else if (input$geno_input %in% c('url', 'vcf.url')) {
#'           golem::invoke_js('hideid', ns('geno_file_holder'))
#'           golem::invoke_js('hideid', ns('geno_table_mapping'))
#'           golem::invoke_js('hideid', ns('geno_table_options'))
#'           golem::invoke_js('showid', ns('geno_url'))
#'         } else if (input$geno_input == 'matfile' ){
#'           golem::invoke_js('showid', ns('geno_file_holder'))
#'           golem::invoke_js('showid', ns('geno_table_mapping'))
#'           golem::invoke_js('showid', ns('geno_table_options'))
#'           golem::invoke_js('hideid', ns('geno_url'))
#'           updateCheckboxInput(session, 'geno_example', value = FALSE)
#'         } else if (input$geno_input == 'matfileurl' ){
#'           golem::invoke_js('hideid', ns('geno_file_holder'))
#'           golem::invoke_js('showid', ns('geno_table_mapping'))
#'           golem::invoke_js('showid', ns('geno_table_options'))
#'           golem::invoke_js('showid', ns('geno_url'))
#'         }
#'       }
#'     )
#'
#'     geno_data_table = reactive({ # function to purely just read a csv when we need to match the genotype file
#'       if(length(input$geno_input) > 0){ # added
#'         if (input$geno_input == 'matfile' ) {
#'           if (is.null(input$geno_file)) {return(NULL)}else{
#'             snps_file <- input$geno_file$datapath
#'           }
#'         } else if(input$geno_input == 'matfileurl'){
#'           if (is.null(input$geno_file)) {return(NULL)}else{
#'             snps_file <- input$geno_url
#'           }
#'         }else {
#'           return(NULL);
#'         }
#'         shinybusy::show_modal_spinner('fading-circle', text = 'Loading...')
#'         df <- as.data.frame(data.table::fread(snps_file, sep = input$geno_sep, quote = input$geno_quote, dec = input$geno_dec, header = TRUE))
#'         shinybusy::remove_modal_spinner()
#'         return(df)
#'       }else{
#'         return(NULL)
#'       }
#'     })
#'
#'     observeEvent(c(geno_data_table()), { # update values for columns in designation and first snp and last snp
#'       req(geno_data_table())
#'       provGeno <- geno_data_table()
#'       output$preview_geno <- DT::renderDT({
#'         req(geno_data_table())
#'         DT::datatable(geno_data_table()[,1:min(c(50,ncol(geno_data_table())))],
#'                       extensions = 'Buttons',
#'                       options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All')))
#'         )
#'       })
#'       updateSelectizeInput(session, "geno_table_firstsnp", choices = colnames(provGeno)[1:min(c(ncol(provGeno),100))], selected = character(0))
#'       updateSelectizeInput(session, "geno_table_lastsnp", choices = colnames(provGeno)[max(c(1,ncol(provGeno)-100)):ncol(provGeno)], selected = character(0))
#'       updateSelectizeInput(session, "geno_table_designation", choices = colnames(provGeno)[1:min(c(ncol(provGeno),100))], selected = character(0))
#'     })
#'
#'     observeEvent( # reactive for the csv geno read, active once the user has selected the proper columns
#'       c(geno_data_table(), input$geno_table_firstsnp, input$geno_table_lastsnp, input$geno_table_designation),
#'       {
#'         req(geno_data_table())
#'         req(input$geno_table_firstsnp)
#'         req(input$geno_table_lastsnp)
#'         req(input$geno_table_designation)
#'         if(!is.null(input$geno_table_firstsnp) & !is.null(input$geno_table_lastsnp) & !is.null(input$geno_table_designation) ){
#'           temp <- data()
#'           tempG <- geno_data_table()
#'           tempG <- tempG[which(!duplicated(tempG[,which(colnames(tempG)==input$geno_table_designation)[1]])),]
#'           rownamestempG <- tempG[,which(colnames(tempG)==input$geno_table_designation)[1] ]
#'           missingData=c("NN","FAIL","FAILED","Uncallable","Unused","NA","")
#'           shinybusy::show_modal_spinner('fading-circle', text = 'Converting...')
#'           for(iMiss in missingData){tempG[which(tempG==iMiss, arr.ind = TRUE)] <- NA}
#'           ## check if the data is in single letter format
#'           markersToSample <- sample(which(colnames(tempG)==input$geno_table_firstsnp):which(colnames(tempG)==input$geno_table_lastsnp),  min(c(ncol(tempG),20)) )
#'           nCharList <- list()
#'           for(iMark in 1:length(markersToSample)){nCharList[[iMark]] <- na.omit(unique(nchar(tempG[,markersToSample[iMark]])))}
#'           singleLetter <- which(unique(unlist(nCharList)) == 1)
#'           if(length(singleLetter) > 0){
#'             tempG <- cgiarBase::transMarkerSingle( markerDTfile= tempG, badCall=NULL,genoColumn=input$geno_table_designation,firstColum= input$geno_table_firstsnp,lastColumn=input$geno_table_lastsnp,verbose=FALSE)
#'           }
#'           tempG <- tempG[,which(colnames(tempG)==input$geno_table_firstsnp):which(colnames(tempG)==input$geno_table_lastsnp)]
#'           ##
#'           tempG <- sommer::atcg1234(tempG, maf = -1, imp = FALSE)
#'           rownames(tempG$M) <- rownamestempG
#'           shinybusy::remove_modal_spinner()
#'           temp$data$geno <- tempG$M
#'           refAlleles <- tempG$ref.alleles
#'           map <- data.frame(a=colnames(tempG$M), chrom=1, pos=1:ncol(tempG$M))
#'           map$refAllele <- tempG$ref.alleles[2,]
#'           map$altAllele <- tempG$ref.alleles[1,]
#'           colnames(map) <- c('marker', 'chr', 'pos', 'refAllele', 'altAllele')
#'           temp$metadata$geno <- map
#'           data(temp)
#'         }else{return(NULL)}
#'       }
#'     )
#'
#'     geno_data <- reactive({
#'       if(length(input$geno_input) > 0){ # added
#'         if (input$geno_input %in% c('file', 'vcf.file')) {
#'           if (is.null(input$geno_file)) {return(NULL)}else{
#'             snps_file <- input$geno_file$datapath
#'           }
#'         } else if (input$geno_input %in% c('url', 'vcf.url')) {
#'           if (input$geno_url == '') {return(NULL)}else{
#'             snps_file <- input$geno_url
#'           }
#'         } else {
#'           return(NULL)
#'         }
#'
#'         # library(vcfR); vcf_data <- vcfR::read.vcfR(snps_file); hmp_data <- vcfR::vcfR2hapmap(vcf.data)
#'
#'         shinybusy::show_modal_spinner('fading-circle', text = 'Loading...')
#'         if (input$geno_input %in% c('file', 'url')) {
#'           df <- as.data.frame(data.table::fread(snps_file, sep = '\t', header = TRUE))
#'
#'         } else if (input$geno_input %in% c('vcf.file', 'vcf.url')) {
#'           vcf.data  <- vcfR::read.vcfR(snps_file)
#'
#'           df <- vcfR::vcfR2hapmap(vcf.data)
#'           df <- df[-1,]
#'
#'           rm(vcf.data)
#'         }
#'
#'         shinybusy::remove_modal_spinner()
#'
#'         hapmap_snp_attr <- c('rs#', 'alleles', 'chrom', 'pos', 'strand', 'assembly#',
#'                              'center', 'protLSID', 'assayLSID', 'panelLSID', 'QCcode',
#'                              'rs', 'assembly','panel' # column versions of vcfR
#'         )
#'         if(length(intersect(hapmap_snp_attr, colnames(df)[1:11])) != 11){
#'           # if (!all(colnames(df)[1:11] == hapmap_snp_attr)) {
#'           shinyWidgets::show_alert(title = 'Error !!', text = 'Not a valid HapMap file format :-(', type = 'error')
#'           return(NULL)
#'         }
#'         colnames(df)[1:11] <- hapmap_snp_attr[1:11]
#'         first_row   <- df[1, -c(1:11)]
#'         valid_IUPAC <- c('A', 'C', 'G', 'T', 'U', 'W', 'S', 'M', 'K', 'R', 'Y', 'B', 'D', 'H', 'V', 'N')
#'         double_code <- c("AA","TT","CC","GG","AT","TA","AC","CA","AG","GA","TC","CT","TG","GT","CG","GC","NN")
#'         # IUPAC single-letter code
#'         if (all(first_row %in% valid_IUPAC)) {
#'
#'           shinybusy::show_modal_spinner('fading-circle', text = 'Converting...')
#'           df <- hapMapChar2Numeric(df)
#'           shinybusy::remove_modal_spinner()
#'
#'           # -1, 0, 1 numeric coding
#'         } else if (min(as.numeric(first_row), na.rm = TRUE) == -1 &
#'                    max(as.numeric(first_row), na.rm = TRUE) == 1) {
#'
#'           df <- cbind(df[, 1:11],
#'                       data.frame(apply(df[, -c(1:11)], 2, function(x) 1 + as.numeric(as.character(x)))))
#'
#'           # 0, 1, 2 numeric coding
#'         } else if (min(as.numeric(first_row), na.rm = TRUE) == 0 &
#'                    max(as.numeric(first_row), na.rm = TRUE) == 2) {
#'
#'           df <- cbind(df[, 1:11],
#'                       data.frame(apply(df[, -c(1:11)], 2, function(x) as.numeric(as.character(x)))))
#'
#'           # something else!
#'         } else if(all(first_row %in% double_code)){
#'           shinybusy::show_modal_spinner('fading-circle', text = 'Converting...')
#'           df <- hapMapChar2NumericDouble(df)
#'           shinybusy::remove_modal_spinner()
#'         }else {
#'           shinyWidgets::show_alert(title = 'Error !!', text = 'Not a valid HapMap file format :-(', type = 'error')
#'           return(NULL)
#'         }
#'         return(list(df,snps_file))
#'
#'       }else{
#'         return(NULL)
#'       }
#'
#'     })
#'
#'     output$chrom_summary <- renderTable({
#'       if (!is.null(geno_data())) {
#'         gd<-geno_data()[[1]]
#'         data.frame(
#'           chrom = unique(gd[,'chrom']),
#'           min_pos = aggregate(pos ~ chrom, data = gd, FUN = min)[,2],
#'           max_pos = aggregate(pos ~ chrom, data = gd, FUN = max)[,2],
#'           snps_count = aggregate(pos ~ chrom, data = gd, FUN = length)[,2]
#'         )
#'       }
#'     })
#'
#'     output$geno_summary <- renderText({
#'       temp <- data()
#'       gd <- geno_data()[[1]]
#'       if (!is.null(gd) & any(temp$metadata$pheno$parameter == 'designation')) {
#'         designationColumn <- temp$metadata$pheno[which(temp$metadata$pheno$parameter == "designation"),"value"]
#'         paste(
#'           "Data Integrity Checks:\n",
#'
#'           sum(colnames(gd[, -c(1:11)]) %in% unique(temp$data$pheno[, designationColumn ])),
#'           "Accessions exist in both phenotypic and genotypic files (will be used to train the model)\n",
#'
#'           sum(!colnames(gd[, -c(1:11)]) %in% unique(temp$data$pheno[, designationColumn ])),
#'           "Accessions have genotypic data but no phenotypic (will be predicted, add to pheno data file with NA value)\n",
#'
#'           sum(!unique(temp$data$pheno[, designationColumn ]) %in% colnames(gd[, -c(1:11)])),
#'           'Accessions have phenotypic data but no genotypic (will not contribute to the training model)'
#'         )
#'       }
#'     })
#'
#'     observeEvent(
#'       geno_data(),
#'       {
#'         temp <- data()
#'         gd<-geno_data()[[1]]
#'         temp$data$geno <- t(as.matrix(gd[, -c(1:11)])) - 1
#'         colnames(temp$data$geno) <- gd$`rs#`
#'
#'         map <- gd[, c('rs#', 'chrom', 'pos', 'alleles', 'alleles')]
#'         colnames(map) <- c('marker', 'chr', 'pos', 'refAllele', 'altAllele')
#'         map$refAllele <- substr(map$refAllele, 1, 1)
#'         map$altAllele <- substr(map$altAllele, 3, 3)
#'
#'         temp$metadata$geno <- map
#'         temp$data$genodir<-geno_data()[[2]]
#'         data(temp)
#'       }
#'     )
#'
#'     observeEvent(
#'       input$geno_example,
#'       if(length(input$geno_example) > 0){ # added
#'         if (input$geno_example) {
#'           updateSelectInput(session, 'geno_input', selected = 'url')
#'
#'           geno_example_url <-  paste0(session$clientData$url_protocol, '//',
#'                                       session$clientData$url_hostname, ':',
#'                                       session$clientData$url_port,
#'                                       session$clientData$url_pathname,
#'                                       geno_example)
#'
#'           updateTextInput(session, 'geno_url', value = geno_example_url)
#'
#'           golem::invoke_js('hideid', ns('geno_file_holder'))
#'           golem::invoke_js('showid', ns('geno_url'))
#'         } else {
#'           updateSelectInput(session, 'geno_input', selected = 'file')
#'           updateTextInput(session, 'geno_url', value = '')
#'
#'           golem::invoke_js('showid', ns('geno_file_holder'))
#'           golem::invoke_js('hideid', ns('geno_url'))
#'         }
#'       }
#'     )
#'
#'     ### Pedigree tab controls ##################################################
#'
#'     observeEvent(
#'       input$ped_input,
#'       if(length(input$ped_input) > 0){ # added
#'         if (input$ped_input == 'file') {
#'           golem::invoke_js('showid', ns('ped_file_holder'))
#'           golem::invoke_js('hideid', ns('ped_url'))
#'           golem::invoke_js('showid', ns('ped_csv_options'))
#'           updateCheckboxInput(session, 'ped_example', value = FALSE)
#'         } else if (input$ped_input == 'url') {
#'           golem::invoke_js('hideid', ns('ped_file_holder'))
#'           golem::invoke_js('showid', ns('ped_url'))
#'           golem::invoke_js('showid', ns('ped_csv_options'))
#'         }
#'       }
#'     )
#'
#'     ped_data <- reactive({
#'       if(length(input$ped_input) > 0){ # added
#'         if (input$ped_input == 'file') {
#'           if (is.null(input$ped_file)) return(NULL)
#'           data <- as.data.frame(data.table::fread(input$ped_file$datapath, sep = input$ped_sep,
#'                                                   quote = input$ped_quote, dec = input$ped_dec, header = TRUE))
#'         } else if (input$ped_input == 'url') {
#'           if (input$ped_url == '') return(NULL)
#'           data <- as.data.frame(data.table::fread(input$ped_url, sep = input$ped_sep,
#'                                                   quote = input$ped_quote, dec = input$ped_dec, header = TRUE))
#'         } else {
#'           return(NULL)
#'         }
#'
#'         return(data)
#'       }
#'     })
#'
#'     observeEvent(
#'       ped_data(),
#'       {
#'         temp <- data()
#'
#'         # should check file format here!
#'         temp$data$pedigree <- ped_data()
#'
#'         output$preview_ped <- DT::renderDT({
#'           req(ped_data())
#'
#'           DT::datatable(ped_data(),
#'                         extensions = 'Buttons',
#'                         options = list(dom = 'Blfrtip',
#'                                        scrollX = TRUE,
#'                                        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
#'                                        lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All')))
#'           )
#'
#'         })
#'
#'         data(temp)
#'       }
#'     )
#'
#'     output$ped_map <- renderUI({
#'       header <- colnames(ped_data())
#'       fluidRow(
#'         column(3, selectInput(
#'           inputId  = ns('ped_designation'),
#'           label    = 'designation',
#'           choices  = as.list(c('', header)),
#'         )),
#'         column(3, selectInput(
#'           inputId  = ns('ped_mother'),
#'           label    = 'mother',
#'           choices  = as.list(c('', header)),
#'         )),
#'         column(3, selectInput(
#'           inputId  = ns('ped_father'),
#'           label    = 'father',
#'           choices  = as.list(c('', header)),
#'         )),
#'         column(3, selectInput(
#'           inputId  = ns('ped_year'),
#'           label    = 'Year of Origin',
#'           choices  = as.list(c('', header)),
#'         )),
#'         ## testing
#'         renderPrint({
#'           req(input$ped_designation)
#'           # req(input$ped_father)
#'           # req(input$ped_mother)
#'           # req(input$ped_year)
#'           temp <- data()
#'           if ("designation" %in% temp$metadata$pedigree$parameter) {
#'             temp$metadata$pedigree[temp$metadata$pedigree$parameter == "designation", 'value'] <- input$ped_designation
#'           } else {
#'             temp$metadata$pedigree <- rbind(temp$metadata$pedigree, data.frame(parameter = "designation", value = input$ped_designation ))
#'           }
#'           if ("mother" %in% temp$metadata$pedigree$parameter) {
#'             temp$metadata$pedigree[temp$metadata$pedigree$parameter == "mother", 'value'] <- input$ped_mother
#'           } else {
#'             temp$metadata$pedigree <- rbind(temp$metadata$pedigree, data.frame(parameter = "mother", value = input$ped_mother ))
#'           }
#'           if ("father" %in% temp$metadata$pedigree$parameter) {
#'             temp$metadata$pedigree[temp$metadata$pedigree$parameter == "father", 'value'] <- input$ped_father
#'           } else {
#'             temp$metadata$pedigree <- rbind(temp$metadata$pedigree, data.frame(parameter = "father", value = input$ped_father ))
#'           }
#'           if ("yearOfOrigin" %in% temp$metadata$pedigree$parameter) {
#'             temp$metadata$pedigree[temp$metadata$pedigree$parameter == "yearOfOrigin", 'value'] <- input$ped_year
#'           } else {
#'             temp$metadata$pedigree <- rbind(temp$metadata$pedigree, data.frame(parameter = "yearOfOrigin", value = input$ped_year ))
#'           }
#'           data(temp)
#'         }),
#'       )
#'     })
#'
#'     output$ped_summary <- renderText({
#'       req(input$ped_designation)
#'       tmp <- data()
#'       ped <- ped_data()
#'       if (!is.null(ped) & any(tmp$metadata$pheno$parameter == 'designation') & any(tmp$metadata$pedigree$parameter == 'designation') ) {
#'         designationColumnInPheno <- tmp$metadata$pheno[which(tmp$metadata$pheno$parameter == "designation"),"value"]
#'         designationColumnInPed <- tmp$metadata$pedigree[which(tmp$metadata$pedigree$parameter == "designation"),"value"]
#'         paste(
#'           "Data Integrity Checks:\n",
#'           ifelse( (length(designationColumnInPed)+length(designationColumnInPheno) ) == 2, sum(ped[, designationColumnInPed ] %in% unique(tmp$data$pheno[, designationColumnInPheno ]) ) , 0),
#'           "Accessions exist in both phenotypic and pedigree files\n",
#'           ifelse( length(designationColumnInPheno) > 0 , sum(!unique(tmp$data$pheno[, designationColumnInPheno ]) %in% ped[, designationColumnInPed ]), 0),
#'           "Accessions have phenotypic data but no pedigree data\n",
#'           ifelse(any(tmp$metadata$pedigree$parameter == 'yearOfOrigin') & (length(designationColumnInPed)+length(designationColumnInPheno) ) == 2,  sum(ped[, designationColumnInPed ] %in% unique(tmp$data$pheno[, designationColumnInPheno ])) ,0),
#'           "Accessions in phenotypic data have no year of origin info\n"
#'         )
#'       }
#'     })
#'
#'     observeEvent(
#'       input$ped_example,
#'       if (input$ped_example) {
#'         updateSelectInput(session, 'ped_input', selected = 'url')
#'
#'         ped_example_url <-  paste0(session$clientData$url_protocol, '//',
#'                                    session$clientData$url_hostname, ':',
#'                                    session$clientData$url_port,
#'                                    session$clientData$url_pathname,
#'                                    ped_example)
#'
#'         updateTextInput(session, 'ped_url', value = ped_example_url)
#'
#'         golem::invoke_js('hideid', ns('ped_file_holder'))
#'         golem::invoke_js('showid', ns('ped_url'))
#'       } else {
#'         updateSelectInput(session, 'ped_input', selected = 'file')
#'         updateTextInput(session, 'ped_url', value = '')
#'
#'         golem::invoke_js('showid', ns('ped_file_holder'))
#'         golem::invoke_js('hideid', ns('ped_url'))
#'       }
#'     )
#'
#'     ### Weather tab controls ##################################################
#'
#'     ### QTL profile tab controls #################################################
#'
#'     observeEvent(
#'       input$qtl_input,
#'       if(length(input$qtl_input) > 0){ # added
#'         if (input$qtl_input == 'qtlfile' ){
#'           golem::invoke_js('showid', ns('qtl_file_holder'))
#'           golem::invoke_js('showid', ns('qtl_table_mapping'))
#'           golem::invoke_js('showid', ns('qtl_table_options'))
#'           golem::invoke_js('hideid', ns('qtl_url'))
#'           updateCheckboxInput(session, 'qtl_example', value = FALSE)
#'         } else if (input$qtl_input == 'qtlfileurl' ){
#'           golem::invoke_js('hideid', ns('qtl_file_holder'))
#'           golem::invoke_js('showid', ns('qtl_table_mapping'))
#'           golem::invoke_js('showid', ns('qtl_table_options'))
#'           golem::invoke_js('showid', ns('qtl_url'))
#'         }
#'       }
#'     )
#'
#'     observeEvent(
#'       input$qtl_example,
#'       if(length(input$qtl_example) > 0){ # if user clicked on qtl example
#'         if (input$qtl_example) {
#'           updateSelectInput(session, 'qtl_input', selected = 'qtlfileurl')
#'
#'           qtl_example_url <-  paste0(session$clientData$url_protocol, '//',
#'                                      session$clientData$url_hostname, ':',
#'                                      session$clientData$url_port,
#'                                      session$clientData$url_pathname,
#'                                      qtl_example)
#'
#'           updateTextInput(session, 'qtl_url', value = qtl_example_url)
#'
#'           golem::invoke_js('hideid', ns('qtl_file_holder'))
#'           golem::invoke_js('showid', ns('qtl_url'))
#'         } else {
#'           updateSelectInput(session, 'qtl_input', selected = 'qtlfile')
#'           updateTextInput(session, 'qtl_url', value = '')
#'
#'           golem::invoke_js('showid', ns('qtl_file_holder'))
#'           golem::invoke_js('hideid', ns('qtl_url'))
#'         }
#'       }
#'
#'     )
#'
#'     qtl_data_table = reactive({ # function to purely just read a csv when we need to match the qtltype file
#'       if(length(input$qtl_input) > 0){ # added
#'         if (input$qtl_input == 'qtlfile' ) {
#'           if (is.null(input$qtl_file)) {return(NULL)}else{
#'             qtls_file <- input$qtl_file$datapath
#'           }
#'         } else if(input$qtl_input == 'qtlfileurl'){
#'           if (input$qtl_url == '') {return(NULL)}else{
#'             qtls_file <- input$qtl_url
#'           }
#'         }else {
#'           return(NULL);
#'         }
#'         shinybusy::show_modal_spinner('fading-circle', text = 'Loading...')
#'         df <- as.data.frame(data.table::fread(qtls_file, sep = input$qtl_sep, quote = input$qtl_quote, dec = input$qtl_dec, header = TRUE))
#'         shinybusy::remove_modal_spinner()
#'         return(df)
#'       }else{
#'         return(NULL)
#'       }
#'     })
#'     observeEvent(c(qtl_data_table()), { # update values for columns in designation and first snp and last snp
#'       req(qtl_data_table())
#'       provqtl <- qtl_data_table()
#'       output$preview_qtl <- DT::renderDT({
#'         req(qtl_data_table())
#'         DT::datatable(qtl_data_table()[,1:min(c(50,ncol(qtl_data_table())))],
#'                       extensions = 'Buttons',
#'                       options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All')))
#'         )
#'       })
#'       updateSelectizeInput(session, "qtl_table_firstsnp", choices = colnames(provqtl),selected = character(0))
#'       updateSelectizeInput(session, "qtl_table_designation", choices = colnames(provqtl)[1:min(c(ncol(provqtl),100))], selected = character(0))
#'     })
#'     observeEvent( # reactive for the csv qtl read, active once the user has selected the proper columns
#'       c(qtl_data_table(), input$qtl_table_firstsnp, input$qtl_table_designation),
#'       {
#'         req(qtl_data_table())
#'         req(input$qtl_table_firstsnp)
#'         req(input$qtl_table_designation)
#'         if(!is.null(input$qtl_table_firstsnp) & !is.null(input$qtl_table_designation) ){
#'           temp <- data()
#'           temp$metadata$qtl <- data.frame(parameter="designation",value=c(input$qtl_table_designation))
#'           temp$data$qtl <- qtl_data_table()[,unique(c(input$qtl_table_designation,input$qtl_table_firstsnp))]
#'           data(temp)
#'         }else{return(NULL)}
#'       }
#'     )
#'
#'
#'     ### Weather data server ####################################################
#'     # warning message
#'     output$warningMessage <- renderUI(
#'       if(is.null(data())){
#'         HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your phenotypic data using the 'Data Retrieval' tab.")) )
#'       }else{ # data is there
#'         mappedColumns <- length(which(c("environment") %in% data()$metadata$pheno$parameter))
#'         if(mappedColumns == 1){ HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to identify the location of your environments.")) )
#'         }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please make sure that you have computed the 'environment' column in 'Data Retrieval' tab for Phenotypes.")) )
#'         }
#'       }
#'     )
#'
#'     output$environment <- renderUI({
#'       req(data())
#'       dtProv = data()$data$pheno
#'       paramsPheno <- data()$metadata$pheno
#'       colnames(dtProv) <- cgiarBase::replaceValues(colnames(dtProv), Search = paramsPheno$value, Replace = paramsPheno$parameter )
#'       envCol <- paramsPheno[which(paramsPheno$parameter == "environment"),"value"]
#'       if(length(envCol) > 0){
#'         fieldNames <- as.character(unique(dtProv[,"environment"]))
#'         lapply(1:length(fieldNames), function(i) {
#'           tags$div(id = "inline", textInput(
#'             session$ns(paste0('environment',i)),
#'             NULL,
#'             value=fieldNames[i], #width = '800px',
#'           ))
#'         })
#'       }
#'     })
#'     output$latitude <- renderUI({
#'       req(data())
#'       dtProv = data()$data$pheno
#'       paramsPheno <- data()$metadata$pheno
#'       colnames(dtProv) <- cgiarBase::replaceValues(colnames(dtProv), Search = paramsPheno$value, Replace = paramsPheno$parameter )
#'       envCol <- paramsPheno[which(paramsPheno$parameter == "environment"),"value"]
#'       if(length(envCol) > 0){
#'         fieldNames <- as.character(unique(dtProv[,"environment"]))
#'         lapply(1:length(fieldNames), function(i) {
#'           tags$div(id = "inline",  numericInput(
#'             session$ns(paste0('latitude',i)),
#'             NULL,
#'             value = 0
#'           ))
#'         })
#'       }
#'     })
#'     output$longitude <- renderUI({
#'       req(data())
#'       dtProv = data()$data$pheno
#'       paramsPheno <- data()$metadata$pheno
#'       colnames(dtProv) <- cgiarBase::replaceValues(colnames(dtProv), Search = paramsPheno$value, Replace = paramsPheno$parameter )
#'       envCol <- paramsPheno[which(paramsPheno$parameter == "environment"),"value"]
#'       if(length(envCol) > 0){
#'         fieldNames <- as.character(unique(dtProv[,"environment"]))
#'         lapply(1:length(fieldNames), function(i) {
#'           tags$div(id = "inline", numericInput(
#'             session$ns(paste0('longitude',i)),
#'             NULL,
#'             value = 0
#'           ))
#'         })
#'       }
#'     })
#'     output$plantingDate <- renderUI({
#'       req(data())
#'       dtProv = data()$data$pheno
#'       paramsPheno <- data()$metadata$pheno
#'       colnames(dtProv) <- cgiarBase::replaceValues(colnames(dtProv), Search = paramsPheno$value, Replace = paramsPheno$parameter )
#'       envCol <- paramsPheno[which(paramsPheno$parameter == "environment"),"value"]
#'       if(length(envCol) > 0){
#'         fieldNames <- as.character(unique(dtProv[,"environment"]))
#'         lapply(1:length(fieldNames), function(i) {
#'           tags$div(id = "inline", dateInput(
#'             session$ns(paste0('plantingDate',i)),
#'             NULL,
#'             value = Sys.Date()-31
#'           ))
#'         })
#'       }
#'     })
#'     output$harvestingDate <- renderUI({
#'       req(data())
#'       dtProv = data()$data$pheno
#'       paramsPheno <- data()$metadata$pheno
#'       colnames(dtProv) <- cgiarBase::replaceValues(colnames(dtProv), Search = paramsPheno$value, Replace = paramsPheno$parameter )
#'       envCol <- paramsPheno[which(paramsPheno$parameter == "environment"),"value"]
#'       if(length(envCol) > 0){
#'         fieldNames <- as.character(unique(dtProv[,"environment"]))
#'         lapply(1:length(fieldNames), function(i) {
#'           tags$div(id = "inline", dateInput(
#'             session$ns(paste0('harvestingDate',i)),
#'             NULL,
#'             value = Sys.Date()-30
#'           ))
#'         })
#'       }
#'     })
#'
#'     ##
#'     dataWeather = reactive({
#'       req(data())
#'       dtProv = data()$data$pheno
#'       paramsPheno <- data()$metadata$pheno
#'       colnames(dtProv) <- cgiarBase::replaceValues(colnames(dtProv), Search = paramsPheno$value, Replace = paramsPheno$parameter )
#'       envCol <- paramsPheno[which(paramsPheno$parameter == "environment"),"value"]
#'       if (length(envCol) > 0) {
#'         fieldNames <- as.character(unique(dtProv[,"environment"]))
#'         values <- values2 <- values3 <- values4 <- values5 <- vector()
#'         for (i in 1:length(fieldNames)) {
#'           tempval <- reactive({paste0('input$','latitude',i)})
#'           values[i] <- tempval()
#'           values[i] <- eval(parse(text = values[i]))
#'           tempval <- reactive({paste0('input$','longitude',i)})
#'           values2[i] <- tempval()
#'           values2[i] <- eval(parse(text = values2[i]))
#'           tempval <- reactive({paste0('input$','plantingDate',i)})
#'           values3[i] <- tempval()
#'           values3[i] <- eval(parse(text = values3[i])) # eval(lubridate::parse_date_time2(as.character(values3[i]), orders='Ymd'))
#'           tempval <- reactive({paste0('input$','harvestingDate',i)})
#'           values4[i] <- tempval()
#'           values4[i] <- eval(parse(text = values4[i]))
#'           tempval <- reactive({paste0('input$','environment',i)})
#'           values5[i] <- tempval()
#'           values5[i] <- eval(parse(text = values5[i]))
#'         }
#'         values <- as.numeric( as.data.frame(t(as.numeric(values))) );
#'         values2 <- as.numeric( as.data.frame(t(as.numeric(values2))) );
#'         values3 <- as.Date(as.numeric( as.data.frame(t(as.numeric(values3))) ), origin="1970-01-01", tz="GMT")
#'         values4 <- as.Date(as.numeric( as.data.frame(t(as.numeric(values4))) ), origin="1970-01-01", tz="GMT")
#'         values5 <- as.character(as.data.frame(t(as.character(values5))) )
#'         xx <- data.frame(latitude=values, longitude=values2, plantingDate=values3, harvestingDate=values4, environment=values5)
#'         return(xx)
#'       }
#'     })
#'     ##produce the map plot
#'     output$plotMeteo <-  plotly::renderPlotly({
#'       req(data())
#'       xx <- dataWeather()
#'       if(!is.null(xx)){
#'         fig <- xx
#'         fig <- fig %>%
#'           plotly::plot_ly(
#'             lat = ~latitude,
#'             lon = ~longitude,
#'             type = "scattermapbox",
#'             hovertext = ~environment, #us_cities[,"City"],
#'             marker = list(color = "fuchsia"))
#'         fig <- fig %>%
#'           plotly::layout(
#'             mapbox = list(
#'               style = 'open-street-map',
#'               zoom =1,
#'               center = list(lon = 0, lat = 0)
#'             )
#'           )
#'         # }
#'         fig
#'       }
#'     })
#'     ## save when user clicks
#'
#'     outgetWeather <- eventReactive(input$rungetWeather, {
#'
#'       req(data())
#'       if(is.null(data())){
#'         cat( "Please retrieve or load your phenotypic data using the 'Data Retrieval' tab.")
#'       }else{ # data is there
#'         ## pheno check
#'         shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
#'         xx <- dataWeather()
#'         # save(xx, file="xx.RData")
#'         howManyProvidede <- apply(xx,1,function(x){length(which(is.na(x)))/length(x)})
#'         if( length(which(howManyProvidede != 0)) > 0  ){ # the user has not provided any full information for any field
#'           cat( "Please fill the 4 fields required for at least one environment to extract weather data.")
#'         }else{
#'           result <- data()
#'           weather <- cgiarPipeline::nasaPowerExtraction(LAT=xx$latitude,LONG=xx$longitude,
#'                                                         date_planted=xx$plantingDate,
#'                                                         date_harvest=xx$harvestingDate,
#'                                                         environments=xx$environment,
#'                                                         temporal=input$temporal
#'           )
#'           result$data$weather <- weather$WTH
#'           result$metadata$weather <- unique(rbind(result$metadata$weather, weather$descriptive))
#'           data(result)
#'           cat(paste("Weather data saved succesfully."))
#'         }
#'         shinybusy::remove_modal_spinner()
#'       }
#'
#'     })
#'     output$outgetWeather <- renderPrint({
#'       outgetWeather()
#'     })
#'     ### Control Nex/Back buttons ###############################################
#'
#'     back_bn  <- actionButton(ns('prev_tab'), 'Back')
#'     next_bn  <- actionButton(ns('next_tab'), 'Next')
#'     tab_list <- c(ns('tab1'), ns('tab2'), ns('tab3'), ns('tab4'), ns('tab5'))
#'
#'     output$navigate <- renderUI({
#'       tags$div(align = 'center',
#'                column(1, if (which(tab_list == input$tabset) != 1) back_bn),
#'                column(1, offset = 10, if (which(tab_list == input$tabset) != length(tab_list)) next_bn),
#'       )
#'     })
#'
#'     observeEvent(input$prev_tab,
#'                  {
#'                    n <- which(tab_list == input$tabset)
#'                    updateTabsetPanel(session, 'tabset', selected = tab_list[n - 1])
#'                  }
#'     )
#'
#'     observeEvent(input$next_tab,
#'                  {
#'                    n <- which(tab_list == input$tabset)
#'                    updateTabsetPanel(session, 'tabset', selected = tab_list[n + 1])
#'                  }
#'     )
#'   })
#' }
#'
#' ## To be copied in the UI
#' # mod_getData_ui("getData_1")
#'
#' ## To be copied in the server
#' # mod_getData_server("getData_1")
#'
#' hapMapChar2Numeric <- function(hapMap) {
#'   # http://adv-r.had.co.nz/C-interface.html
#'   convertChar2Numeric <- inline::cfunction(signature(SNPsMatrix="character",
#'                                                      refAllele="character",
#'                                                      rowsNum="integer",
#'                                                      colsNum="integer"),
#'                                            "int i,j;
#'
#'   // matrix dimentions
#'   int r = asInteger(rowsNum);
#'   int c = asInteger(colsNum);
#'
#'   // length of the matrix
#'   int length = r*c;
#'
#'   // create matrix of integers with the same size as SNPsMatrix
#'   SEXP SNPsNum;
#'   PROTECT(SNPsNum = allocMatrix(INTSXP, r, c));
#'
#'   // convert SNPs codes from the standard IUPAC code (single char) to
#'   // numeric 0, 1, or 2 (use 1 for heterozygous)
#'   for(i = 0; i < r; i++){
#'     char* x;
#'     char alleleA;
#'
#'     // we need to get the reference allele in each SNP (row)
#'     x = (char*)CHAR(STRING_ELT(refAllele, i));
#'     alleleA = x[0];
#'
#'     // convert SNPsMatrix to numeric 0,1,2
#'     // now with alleleA we can convert the genotypes to numeric 0, 1, 2
#'     for(j = 0; j < c; j++){
#'       x = (char*)CHAR(STRING_ELT(SNPsMatrix, i*c+j));
#'
#'       // if current SNP is the same of reference allele (alleleA)
#'       if(x[0] == alleleA){
#'         // then assign 0 in the SNPsNum matrix
#'         // take care of the order of the indexes in matrix is by columns
#'         INTEGER(SNPsNum)[j*r + i] = 0;
#'       }else if(x[0] == 'A' || x[0] == 'T' || x[0] == 'C' || x[0] == 'G'){
#'         // if it is homozygous allele [A,T,C,G]
#'         // but not alleleA (i.e., minor allele)
#'         INTEGER(SNPsNum)[j*r + i] = 2;
#'       }else if(x[0] == 'N'){
#'         // if it is missing allele [N]
#'         INTEGER(SNPsNum)[j*r + i] = -9;
#'       }else{
#'         // if it is not (i.e., heterozygous)
#'         INTEGER(SNPsNum)[j*r + i] = 1;
#'       }
#'     }
#'   }
#'   UNPROTECT(1);
#'   return(SNPsNum);")
#'
#'   hapMap <- as.data.frame(hapMap)
#'
#'   # extract SNP infomation , which is the first 11 columns
#'   SNPInfo <- hapMap[,1:11]
#'
#'   # remove the first 11 columns
#'   hapMap <- hapMap[,-c(1:11)]
#'
#'   # convert the hapMap to numeric
#'   hapMapNumeric <- convertChar2Numeric(unlist(as.matrix(t(hapMap))),
#'                                        unlist(as.matrix(substr(SNPInfo$alleles,1,1))),
#'                                        as.integer(nrow(hapMap)),
#'                                        as.integer(ncol(hapMap)))
#'
#'   # convert to data frame
#'   hapMapNumeric <- as.data.frame(hapMapNumeric)
#'
#'   # convert -9 values to NA
#'   hapMapNumeric[hapMapNumeric == -9] <- NA
#'
#'   # get back the column names (accessions)
#'   colnames(hapMapNumeric) <- colnames(hapMap)
#'
#'   return(cbind(SNPInfo, hapMapNumeric))
#' }
#'
#' hapMapChar2NumericDouble <- function(hapMap) {
#'
#'   hapMap <- as.data.frame(hapMap)
#'   dim(hapMap)
#'   # extract SNP infomation , which is the first 11 columns
#'   SNPInfo <- hapMap[,1:11]
#'
#'   # remove the first 11 columns
#'   hapMap <- hapMap[,-c(1:11)]
#'   missingData=c("NN","FAIL","FAILED","Uncallable","Unused","NA","",-9)
#'   for(iMiss in missingData){hapMap[which(hapMap==iMiss, arr.ind = TRUE)] <- NA}
#'   # convert the hapMap to numeric
#'   Mprov <- t(hapMap); colnames(Mprov) <- SNPInfo[,1]
#'   hapMapNumeric <- sommer::atcg1234(Mprov, maf = -1, imp = FALSE)
#'
#'   multiAllelic <- setdiff(SNPInfo$`rs#`,colnames(hapMapNumeric$M))
#'   if(length(multiAllelic) > 0){
#'     addMulti <- matrix(NA,nrow=nrow(hapMapNumeric$M),ncol=length(multiAllelic))
#'     addMultiRef <- matrix(NA,nrow=2,ncol=length(multiAllelic))
#'     colnames(addMulti) <- colnames(addMultiRef) <- multiAllelic
#'     hapMapNumeric$M <- cbind(hapMapNumeric$M, addMulti)
#'     hapMapNumeric$ref.alleles <- cbind(hapMapNumeric$ref.alleles, addMultiRef)
#'   }
#'   # convert to data frame
#'   refAlleles <- hapMapNumeric$ref.alleles
#'
#'   hapMapNumeric <- as.data.frame(t(hapMapNumeric$M+1))
#'
#'   # add reference and alternate allele
#'   SNPInfo$alleles <- apply(refAlleles,2,function(x){paste(na.omit(x),collapse = "/")})
#'   # convert -9 values to NA
#'   # hapMapNumeric[hapMapNumeric == -9] <- NA
#'
#'   # get back the column names (accessions)
#'   colnames(hapMapNumeric) <- colnames(hapMap)
#'
#'   result <- cbind(SNPInfo, hapMapNumeric)
#'   return(result)
#' }

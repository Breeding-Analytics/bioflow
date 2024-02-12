pheno_example <- 'www/example/pheno.csv'
geno_example  <- 'www/example/geno.hmp.txt'
ped_example   <- 'www/example/pedigree.csv'
qtl_example <- 'www/example/qtl.csv'

#' getData UI Function (test forking)
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_getData_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinydashboard::tabBox(
      width = 12,
      id    = ns('tabset'),

      tabPanel(
        title = 'Phenotypic',
        value = ns('tab1'),

        tags$br(),

        selectInput(
          inputId = ns('pheno_input'),
          label   = 'Data Source*: ',
          choices = list('Upload File' = 'file', 'Copy URL' = 'url', 'BrAPI' = 'brapi'),
          width   = '200px'
        ),

        tags$span(id = ns('pheno_file_holder'),
                  fileInput(
                    inputId = ns('pheno_file'),
                    label   = NULL,
                    width   = '400px',
                    accept  = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
                  )
        ),

        textInput(
          inputId = ns('pheno_url'),
          label   = NULL,
          value   = '',
          width   = '400px',
          placeholder = 'https://example.com/path/file.csv'
        ),

        tags$div(id = ns('pheno_csv_options'),
                 shinydashboard::box(title = span(icon('screwdriver-wrench'), ' Options'), collapsible = TRUE, collapsed = TRUE, status = 'success', solidHeader = TRUE,
                                     shinyWidgets::prettyRadioButtons(ns('pheno_sep'), 'Separator Character', selected = ',', inline = TRUE,
                                                                      choices = c('Comma' = ',', 'Semicolon' = ';', 'Tab' = "\t")),

                                     shinyWidgets::prettyRadioButtons(ns('pheno_quote'), 'Quoting Character', selected = '"', inline = TRUE,
                                                                      choices = c('None' = '', 'Double Quote' = '"', 'Single Quote' = "'")),

                                     shinyWidgets::prettyRadioButtons(ns('pheno_dec'), 'Decimal Points', selected = '.', inline = TRUE,
                                                                      choices = c('Dot' = '.', 'Comma' = ',')),
                 ),
        ),

        fluidRow(tags$div(id = ns('dictionary_box'),
                          shinydashboard::box(width = 6, title = span(icon('bookmark'), ' Dictionary of terms'), status = "success", solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE,
                                              p(strong("pipeline.-"),"The name of the column containing the labels describing the breeding effort to satisfy a market segment (e.g., Direct seeded late maturity irrigated)."),
                                              p(strong("stage.-"),"The name of the column containing the labels describing the stages of phenotypic evaluation (e.g., Stage 1, PYT, etc.)."),
                                              p(strong("year.-"),"The name of the column containing the labels listing the year when a trial was carried out (e.g., 2024)."),
                                              p(strong("season-"),"The name of the column containing the labels listing the season when a trial was carried out (e.g., dry-season, wet-season, etc.)."),
                                              p(strong("country.-"),"The name of the column containing the labels listing the countries where a trial was carried out (e.g., Nigeria, Mexico, etc.)."),
                                              p(strong("location-"),"The name of the column containing the labels listing the locations within a country when a trial was carried out (e.g., Obregon, Toluca, etc.)."),
                                              p(strong("trial.-"),"The name of the column containing the labels listing the trial of experiment randomized."),
                                              p(strong("environment.-"),"The name of the column containing the labels listing the unique occurrences of a trial nested in a year, country, location. If not available, compute it using the button available."),
                                              p(strong("rep.-"),"The name of the column containing the labels of the replicates or big blocks within an environment (year-season-country-location-trial concatenation)."),
                                              p(strong("iBlock.-"),"The name of the column containing the labels of the incomplete blocks within an environment."),
                                              p(strong("row.-"),"The name of the column containing the labels of the row coordinates for each record within an environment."),
                                              p(strong("column.-"),"The name of the column containing the labels of the column coordinates for each record within an environment."),
                                              p(strong("designation.-"),"The name of the column containing the labels of the individuals tested in the environments (e.g., Borlaug123, IRRI-154, Campeon, etc. )."),
                                              p(strong("gid.-"),"The name of the column containing the labels with the unique numerical identifier used within the database management system."),
                                              p(strong("entryType.-"),"The name of the column containing the labels of the genotype category (check, tester, entry, etc.)."),
                                              p(strong("trait.-"),"The name of the column(s) containing the numerical traits to be analyzed."),
                          ),
        )),

        tags$div(id = ns('pheno_brapi_options'),
                 tags$span(id = ns('config_server_holder'),
                           shinydashboard::box(width = 4, title = span(icon('screwdriver-wrench'), ' Config Server'), status = 'success', solidHeader = TRUE,
                                               selectInput(
                                                 inputId = ns('pheno_db_type'),
                                                 label   = 'Database Type: ',
                                                 choices = list('EBS' = 'ebs', 'BMS' = 'bms', 'BreedBase' = 'breedbase')
                                               ),
                                               textInput(
                                                 inputId = ns('pheno_db_url'),
                                                 label = 'Server URL:',
                                                 placeholder = 'https://cb-qa.ebsproject.org'
                                               ),
                                               actionButton(
                                                 inputId = ns('pheno_db_save'),
                                                 label = 'Save',
                                                 width = '100%'
                                               ),
                           )
                 ),

                 tags$span(id = ns('auth_server_holder'),
                           shinydashboard::box(width = 4, title = span(icon('key'), ' Authentication'), status = 'success', solidHeader = TRUE,
                                               tags$span(id = ns('pheno_db_user_holder'),
                                                         textInput(
                                                           inputId = ns('pheno_db_user'),
                                                           label = 'Username:'
                                                         )
                                               ),
                                               tags$span(id = ns('pheno_db_password_holder'),
                                                         passwordInput(
                                                           inputId = ns('pheno_db_password'),
                                                           label = 'Password:'
                                                         )
                                               ),
                                               tags$span(id = ns('no_auth_holder'),
                                                         checkboxInput(
                                                           inputId = ns('no_auth'),
                                                           label = 'No authentication required',
                                                           value = TRUE
                                                         )
                                               ),

                                               actionButton(
                                                 inputId = ns('pheno_db_login'),
                                                 label = 'Login',
                                                 width = '100%'
                                               )                      ),
                 ),

                 tags$span(id = ns('data_server_holder'),
                           shinydashboard::box(width = 4, title = span(icon('magnifying-glass-chart'), ' Data Source'), status = 'success', solidHeader = TRUE,
                                               tags$span(id = ns('pheno_db_crop_holder'),
                                                         selectInput(
                                                           inputId = ns('pheno_db_crop'),
                                                           label   = 'Crop: ',
                                                           choices = list()
                                                         )
                                               ),
                                               selectizeInput(
                                                 inputId = ns('pheno_db_program'),
                                                 label   = 'Breeding Program: ',
                                                 choices = list()
                                               ),
                                               selectizeInput(
                                                 inputId = ns('pheno_db_trial'),
                                                 label   = 'Trial/Study: ',
                                                 choices = list(),
                                                 multiple = TRUE
                                               ),
                                               actionButton(
                                                 inputId = ns('pheno_db_load'),
                                                 label = 'Load',
                                                 width = '100%'
                                               ),
                           )
                 ),
        ),

        if (!is.null(pheno_example)) {
          checkboxInput(
            inputId = ns('pheno_example'),
            label = span('Load example ',
                         a('phenotypic data', target = '_blank',
                           href = pheno_example)),
            value = FALSE
          )
        },

        p(span("**If you have hybrid-crop data and need to map 'mother' and 'father' information for GCA models please provide that information in the Pedigree tab (you can use the same Phenotype file if those columns are there).", style="color:orange")),


        hr(),
        uiOutput(ns('brapi_trait_map')),
        DT::DTOutput(ns('preview_pheno')),
        uiOutput(ns('pheno_map')),
        actionButton(ns("concatenateEnv"), "Update environments", icon = icon("play-circle")),
        textOutput(ns("outConcatenateEnv")),
      ),
      tabPanel(
        title = 'Genotypic',
        value = ns('tab2'),
        fluidRow(
          style = 'padding: 30px;',

          # Source: Upload (web interface to temp local directory) or URL (optional username/password to access)
          # Accept *.gz format (7-Zip how-to reference), average genomic file size after compression is 5%
          selectInput(
            inputId = ns('geno_input'),
            label   = 'Genotypic SNPs Source*:',
            choices = list('HapMap Upload' = 'file', 'HapMap URL' = 'url', 'Table Upload' = 'matfile', 'Table URL' = 'matfileurl' ),
            width   = '200px'
          ),
          tags$span(id = ns('geno_file_holder'),
                    fileInput(
                      inputId = ns('geno_file'),
                      label   = NULL,
                      width   = '400px',
                      accept  = c('application/gzip', '.gz', '.txt', '.hmp', '.csv')
                    )
          ),
          textInput(
            inputId = ns('geno_url'),
            label   = NULL,
            value   = '',
            width   = '400px',
            placeholder = 'https://example.com/path/file.gz'
          ),
          hr(style = "border-top: 1px solid #4c4c4c;"),
          tags$span(id = ns('geno_table_mapping'),
                    column(4,
                           selectizeInput(
                             inputId = ns('geno_table_designation'),
                             label   = 'Designation column: ',
                             choices = list()
                           ),    ),
                    column(4,
                           selectizeInput(
                             inputId = ns('geno_table_firstsnp'),
                             label   = 'First SNP column: ',
                             choices = list()
                           ),   ),
                    column(4,
                           selectizeInput(
                             inputId = ns('geno_table_lastsnp'),
                             label   = 'Last SNP column: ',
                             choices = list()
                           ),
                    ),
          ),
          tags$div(id = ns('geno_table_options'),
                   shinydashboard::box(title = span(icon('screwdriver-wrench'), ' Options'), collapsible = TRUE, collapsed = TRUE, status = 'success', solidHeader = TRUE,
                                       shinyWidgets::prettyRadioButtons(ns('geno_sep'), 'Separator Character', selected = ',', inline = TRUE,
                                                                        choices = c('Comma' = ',', 'Semicolon' = ';', 'Tab' = "\t")),

                                       shinyWidgets::prettyRadioButtons(ns('geno_quote'), 'Quoting Character', selected = '"', inline = TRUE,
                                                                        choices = c('None' = '', 'Double Quote' = '"', 'Single Quote' = "'")),

                                       shinyWidgets::prettyRadioButtons(ns('geno_dec'), 'Decimal Points', selected = '.', inline = TRUE,
                                                                        choices = c('Dot' = '.', 'Comma' = ',')),
                   ),
          ),
          if (!is.null(geno_example)) {
            checkboxInput(
              inputId = ns('geno_example'),
              label = span('Load example ',
                           a('genotypic data', target = '_blank',
                             href = geno_example)),
              value = FALSE
            )
          },

          fluidRow(
            style = 'padding-right: 0px; padding-left: 0px;',
            shinydashboard::box(
              title = 'Notes:',
              width = 12,
              solidHeader = TRUE,
              status = 'success',
              tags$ul(
                tags$li('Accept HapMap format (tab-delimited text file with a header row).
                         The file list SNPs in rows and Accessions (individual samples)
                         in columns. The first 11 columns describe attributes of the SNP,
                         but only the first 4 columns data are required for processing:
                         rs# (SNP id), alleles (e.g., C/G), chrom (chromosome), and pos (position).'),

                tags$li(
                  tags$span(
                    'Accept numeric coding ([0,1, and 2] or [-1,0, and 1] for reference/major,
                     heterozygous, and alternative/minor alleles respectively), or the ',
                    tags$a('IUPAC single-letter', target = '_blank', href = 'https://en.wikipedia.org/wiki/Nucleic_acid_notation#IUPAC_notation'),
                    'code (ref. ',
                    tags$a('https://doi.org/10.1093/nar/13.9.3021', target = '_blank', href = 'https://www.ncbi.nlm.nih.gov/pmc/articles/PMC341218/'),
                    ').'
                  )
                ),

                tags$li('Position should be in bp (base pairs) not cM (CentiMorgan).'),

                tags$li(
                  tags$span(
                    'We recommend compressing your HapMap genotypic data using the gzip
                     format (*.gz extension) to significantly reduce file size. On average,
                     the compressed file size is only 5% of the original size. You can use
                     free software such as',
                    tags$a('7-Zip', href = 'https://www.7-zip.org', target = '_blank'),
                    'to perform the compression.'
                  )
                ),
              )
            ),
          ),

          # Verify file format: Hapmap file format (with reference link)
          # highlight that pos unit should be bp not cM
          # Report summary statistics (#acc, #snps, etc.)
          # to let the user verify before proceeding to the next step
          tableOutput(ns('chrom_summary')),

          # Accessions exist in both phenotypic and genotypic files (will be used to train the model)
          # Accessions have genotypic data but no phenotypic (will predict, add to pheno data file with NA value)
          # Accessions have phenotypic data but no genotypic (filter them out from the pheno data file)
          verbatimTextOutput(ns('geno_summary')),

          hr(),
          DT::DTOutput(ns('preview_geno')),
        )
      ),
      tabPanel(
        title = 'Pedigree',
        value = ns('tab3'),
        # tags$p('Comming soon!', style = 'color: red; font-size: 20px; margin-top: 25px;'),

        tags$br(),

        selectInput(
          inputId = ns('ped_input'),
          label   = 'Data Source*: ',
          choices = list('Upload File' = 'file', 'Copy URL' = 'url'),
          width   = '200px'
        ),

        tags$span(id = ns('ped_file_holder'),
                  fileInput(
                    inputId = ns('ped_file'),
                    label   = NULL,
                    width   = '400px',
                    accept  = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
                  )
        ),

        textInput(
          inputId = ns('ped_url'),
          label   = NULL,
          value   = '',
          width   = '400px',
          placeholder = 'https://example.com/path/file.csv'
        ),

        tags$span(id = ns('ped_csv_options'),
                  shinydashboard::box(title = 'Options', collapsible = TRUE, collapsed = TRUE, status = 'success', solidHeader = TRUE,
                                      shinyWidgets::prettyRadioButtons(ns('ped_sep'), 'Separator Character', selected = ',', inline = TRUE,
                                                                       choices = c('Comma' = ',', 'Semicolon' = ';', 'Tab' = "\t")),

                                      shinyWidgets::prettyRadioButtons(ns('ped_quote'), 'Quoting Character', selected = '"', inline = TRUE,
                                                                       choices = c('None' = '', 'Double Quote' = '"', 'Single Quote' = "'")),

                                      shinyWidgets::prettyRadioButtons(ns('ped_dec'), 'Decimal Points', selected = '.', inline = TRUE,
                                                                       choices = c('Dot' = '.', 'Comma' = ',')),
                  ),
        ),

        if (!is.null(ped_example)) {
          checkboxInput(
            inputId = ns('ped_example'),
            label = span('Load example ',
                         a('pedigree data', target = '_blank',
                           href = ped_example)),
            value = FALSE
          )
        },

        uiOutput(ns('ped_map')),

        verbatimTextOutput(ns('ped_summary')),
        hr(),
        DT::DTOutput(ns('preview_ped')),
      ),
      tabPanel(
        title = 'Weather',
        value = ns('tab4'),
        tags$p('Comming soon!', style = 'color: red; font-size: 20px; margin-top: 25px;'),
      ),
      tabPanel(
        title = 'QTL profile',
        value = ns('tab2'),
        fluidRow(
          style = 'padding: 30px;',
          # Source: Upload (web interface to temp local directory) or URL (optional username/password to access)
          # Accept *.gz format (7-Zip how-to reference), average genomic file size after compression is 5%
          selectInput(
            inputId = ns('qtl_input'),
            label   = 'QTL Source*:',
            choices = list('Table Upload' = 'qtlfile', 'Table URL' = 'qtlfileurl' ),
            width   = '200px'
          ),
          tags$span(id = ns('qtl_file_holder'),
                    fileInput(
                      inputId = ns('qtl_file'),
                      label   = NULL,
                      width   = '400px',
                      accept  = c('.txt', '.csv')
                    )
          ),
          textInput(
            inputId = ns('qtl_url'),
            label   = NULL,
            value   = '',
            width   = '400px',
            placeholder = 'https://example.com/path/file.csv'
          ),
          if (!is.null(qtl_example)) {
            checkboxInput(
              inputId = ns('qtl_example'),
              label = span('Load example ',
                           a('QTL data', target = '_blank',
                             href = qtl_example)),
              value = FALSE
            )
          },
          hr(style = "border-top: 1px solid #4c4c4c;"),
          tags$span(id = ns('qtl_table_mapping'),
                    column(4,
                           selectizeInput(
                             inputId = ns('qtl_table_designation'),
                             label   = 'Designation column: ',
                             choices = list()
                           ),    ),
                    column(8,
                           selectizeInput(
                             inputId = ns('qtl_table_firstsnp'),
                             label   = 'QTL columns: ', multiple=TRUE,
                             choices = list()
                           ),   ),
          ),
          tags$div(id = ns('qtl_table_options'),
                   shinydashboard::box(title = span(icon('screwdriver-wrench'), ' Options'), collapsible = TRUE, collapsed = TRUE, status = 'success', solidHeader = TRUE,
                                       shinyWidgets::prettyRadioButtons(ns('qtl_sep'), 'Separator Character', selected = ',', inline = TRUE,
                                                                        choices = c('Comma' = ',', 'Semicolon' = ';', 'Tab' = "\t")),

                                       shinyWidgets::prettyRadioButtons(ns('qtl_quote'), 'Quoting Character', selected = '"', inline = TRUE,
                                                                        choices = c('None' = '', 'Double Quote' = '"', 'Single Quote' = "'")),

                                       shinyWidgets::prettyRadioButtons(ns('qtl_dec'), 'Decimal Points', selected = '.', inline = TRUE,
                                                                        choices = c('Dot' = '.', 'Comma' = ',')),
                   ),
          ),
          hr(style = "border-top: 1px solid #4c4c4c;"),
          DT::DTOutput(ns('preview_qtl')),
        )
      ),
      tabPanel(
        title = 'Stored objects',
        value = ns('tab5'),
        tags$br(),

        selectInput(
          inputId = ns('previous_object_input'),
          label   = 'Object Source*: ',
          choices = list('Upload from PC' = 'pcfile', 'Upload from cloud' = 'cloudfile'),
          width   = '200px'
        ),

        tags$span(id = ns('previous_object_file_holder'),
                  fileInput(
                    inputId = ns('previous_object_file'),
                    label   = NULL,
                    width   = '400px',
                    accept  = c('.rds','.RData')
                  ),
                  textOutput(ns("outLoad2")),
        ),
        tags$div(id = ns('previous_object_retrieve'),
                 actionButton(ns("refreshPreviousAnalysis"), "Click to retrieve previous analysis"),
                 uiOutput(ns('previous_input2')),
                 actionButton(ns("runLoadPrevious"), "Load analysis", icon = icon("play-circle")),
                 textOutput(ns("outLoad")),
        ),
      ),
    ),

    uiOutput(ns('navigate')),
  )
}

#' getData Server Functions
#'
#' @noRd
mod_getData_server <- function(id, map = NULL, data = NULL, res_auth=NULL){
  moduleServer(id , function(input, output, session){
    ns <- session$ns

    ### Phenotypic tab controls ################################################

    observeEvent(
      input$pheno_input,
      if(length(input$pheno_input) > 0){ # added
        if (input$pheno_input == 'file') {
          golem::invoke_js('showid', ns('pheno_file_holder'))
          golem::invoke_js('hideid', ns('pheno_url'))
          golem::invoke_js('showid', ns('pheno_csv_options'))
          golem::invoke_js('hideid', ns('pheno_brapi_options'))
          golem::invoke_js('showid', ns('pheno_map'))
          updateCheckboxInput(session, 'pheno_example', value = FALSE)
        } else if (input$pheno_input == 'url') {
          golem::invoke_js('hideid', ns('pheno_file_holder'))
          golem::invoke_js('showid', ns('pheno_url'))
          golem::invoke_js('showid', ns('pheno_csv_options'))
          golem::invoke_js('hideid', ns('pheno_brapi_options'))
          golem::invoke_js('showid', ns('pheno_map'))
        } else { # brapi
          golem::invoke_js('hideid', ns('pheno_file_holder'))
          golem::invoke_js('hideid', ns('pheno_url'))
          golem::invoke_js('hideid', ns('pheno_csv_options'))
          golem::invoke_js('showid', ns('pheno_brapi_options'))
          golem::invoke_js('showid', ns('config_server_holder'))
          golem::invoke_js('hideid', ns('auth_server_holder'))
          golem::invoke_js('hideid', ns('data_server_holder'))
          golem::invoke_js('hideid', ns('pheno_map'))
          updateCheckboxInput(session, 'pheno_example', value = FALSE)
        }
      }
    )

    observeEvent(
      input$pheno_db_save,
      {
        if (input$pheno_db_url == '') return(NULL)

        # TODO: check if it has a BrAPI endpoints
        # http://msdn.microsoft.com/en-us/library/ff650303.aspx
        if (!grepl("^(ht|f)tp(s?)\\:\\/\\/[0-9a-zA-Z]([-.\\w]*[0-9a-zA-Z])*(:(0-9)*)*(\\/?)([a-zA-Z0-9\\-\\.\\?\\,\\'\\/\\\\\\+&%\\$#_=]*)?$", input$pheno_db_url)) {
          shinyWidgets::show_alert(title = 'Invalid URL!', type = 'error')
          return(NULL)
        }

        golem::invoke_js('showid', ns('auth_server_holder'))
        golem::invoke_js('hideid', ns('data_server_holder'))

        if (input$pheno_db_type == 'ebs') {
          golem::invoke_js('hideid', ns('pheno_db_user_holder'))
          golem::invoke_js('hideid', ns('pheno_db_password_holder'))
          golem::invoke_js('hideid', ns('pheno_db_crop_holder'))
          golem::invoke_js('hideid', ns('no_auth_holder'))

          QBMS::set_qbms_config(url = input$pheno_db_url, engine = 'ebs', brapi_ver = 'v2')
        } else if (input$pheno_db_type == 'bms') {
          golem::invoke_js('showid', ns('pheno_db_user_holder'))
          golem::invoke_js('showid', ns('pheno_db_password_holder'))
          golem::invoke_js('showid', ns('pheno_db_crop_holder'))
          golem::invoke_js('hideid', ns('no_auth_holder'))

          QBMS::set_qbms_config(url = input$pheno_db_url, engine = 'bms', brapi_ver = 'v1')
        } else if (input$pheno_db_type == 'breedbase') {
          golem::invoke_js('hideid', ns('pheno_db_user_holder'))
          golem::invoke_js('hideid', ns('pheno_db_password_holder'))
          golem::invoke_js('hideid', ns('pheno_db_crop_holder'))
          golem::invoke_js('showid', ns('no_auth_holder'))

          updateCheckboxInput(session, 'no_auth', value = TRUE)

          QBMS::set_qbms_config(url = input$pheno_db_url, engine = 'breedbase', brapi_ver = 'v1')
        }
      }
    )

    observeEvent(
      input$pheno_db_login,
      {
        tryCatch(
          expr = {
            if (input$pheno_db_type == 'ebs') {
              ebs_instance <- sub('https?://([^\\.]+)\\.([^/]+).*', '\\1', input$pheno_db_url)
              ebs_domain   <- sub('https?://([^\\.]+)\\.([^/]+).*', '\\2', input$pheno_db_url)

              # EBS QA instance (https://cb-qa.ebsproject.org)
              if (ebs_domain == 'ebsproject.org' & ebs_instance == 'cb-qa') {
                ebs_brapi <- sub('(.+)//cb-qa\\.(.+)', '\\1//cbbrapi-qa.\\2', input$pheno_db_url)
                QBMS::set_qbms_config(url = ebs_brapi, engine = 'ebs', brapi_ver = 'v2')
                QBMS::login_oauth2(authorize_url = 'https://auth-dev.ebsproject.org/oauth2/authorize',
                                   access_url    = 'https://auth-dev.ebsproject.org/oauth2/token',
                                   client_id     = '5crahiqorgj0lppt3n9dkulkst',
                                   client_secret = '1sf4tipbp4arj3d5cncjmrvk9c2cu30gor5618hnh8rgkp6v5fs')

              # IRRI production instance (https://prod-cb.ebs.irri.org/)
              } else if (ebs_domain == 'ebs.irri.org') {
                ebs_brapi <- sub('(.+)//prod-cb\\.(.+)', '\\1//prod-cbbrapi.\\2', input$pheno_db_url)
                QBMS::set_qbms_config(url = ebs_brapi, engine = 'ebs', brapi_ver = 'v2')
                QBMS::login_oauth2(authorize_url = 'https://auth.ebsproject.org/oauth2/authorize',
                                   access_url    = 'https://auth.ebsproject.org/oauth2/token',
                                   client_id     = '7s4mb2tu4884679rmbucsuopk1',
                                   client_secret = 'nf4m8qobpj8eplpg0a9bbo63g69vh0r3p8rbovtfb0udd28rnk9')

              # CIMMYT instances (https://cb-maize.ebs.cimmyt.org, https://cb-staging.ebs.cimmyt.org/)
              # or evaluation instances (https://cb-mee.ebsproject.org, https://cb-ree.ebsproject.org, and https://cb-wee.ebsproject.org)
              } else if (ebs_domain == 'ebs.cimmyt.org' || ebs_instance %in% c('cb-mee', 'cb-ree', 'cb-wee')) {
                ebs_brapi <- sub('(.+)//cb-(.+)', '\\1//cbbrapi-\\2', input$pheno_db_url)
                QBMS::set_qbms_config(url = ebs_brapi, engine = 'ebs', brapi_ver = 'v2')
                QBMS::login_oauth2(authorize_url = 'https://auth.ebsproject.org/oauth2/authorize',
                                   access_url    = 'https://auth.ebsproject.org/oauth2/token',
                                   client_id     = '346lau0avcptntd1ksbmgdi5c',
                                   client_secret = 'q5vnvakfj800ibh5tvqut73vj8klv1tpt6ugtmuneh6d2jb28i3')
              } else {
                shinyWidgets::show_alert(title = 'Oops!', type = 'warning', "We can't recognize this EBS instance :-(")
                return()
              }
            } else if (input$pheno_db_type == 'bms') {
              QBMS::login_bms(input$pheno_db_user, input$pheno_db_password)
            } else if (input$pheno_db_type == 'breedbase') {
              if (input$no_auth) {
                QBMS::set_qbms_config(url = input$pheno_db_url, engine = 'breedbase', brapi_ver = 'v1', no_auth = TRUE)
              } else {
                QBMS::login_breedbase(input$pheno_db_user, input$pheno_db_password)
              }
            }

            golem::invoke_js('showid', ns('data_server_holder'))

            shinybusy::show_modal_spinner('fading-circle', text = 'Loading Programs...')

            if (input$pheno_db_type == 'bms') {
              pheno_db_crops <- QBMS::list_crops()

              updateSelectInput(session,
                                inputId = 'pheno_db_crop',
                                label   = 'Crop: ',
                                choices = pheno_db_crops)
            } else {
              pheno_db_programs <- QBMS::list_programs()

              updateSelectInput(session,
                                inputId = 'pheno_db_program',
                                label   = 'Breeding Program: ',
                                choices = pheno_db_programs)
            }

            shinybusy::remove_modal_spinner()
          },
          error = function(e) {
            shinyWidgets::show_alert(title = 'Invalid Credentials!', type = 'error')
          }
        )
      }
    )

    observeEvent(
      input$pheno_db_crop,
      if (input$pheno_db_crop != '') {
        shinybusy::show_modal_spinner('fading-circle', text = 'Loading Crops...')

        QBMS::set_crop(input$pheno_db_crop)

        pheno_db_programs <- QBMS::list_programs()

        updateSelectInput(session,
                          inputId = 'pheno_db_program',
                          label   = 'Breeding Program: ',
                          choices = pheno_db_programs)

        shinybusy::remove_modal_spinner()
      }
    )

    observeEvent(
      input$pheno_db_program,
      if (input$pheno_db_program != '') {
        shinybusy::show_modal_spinner('fading-circle', text = 'Loading Trials...')

        QBMS::set_program(input$pheno_db_program)

        pheno_db_trials <- QBMS::list_trials()

        updateSelectInput(session,
                          inputId = 'pheno_db_trial',
                          label   = 'Trial/Study: ',
                          choices = pheno_db_trials)

        shinybusy::remove_modal_spinner()
      }
    )

    observeEvent(
      input$no_auth,
      if(length(input$no_auth) > 0){
        if (input$no_auth) {
          golem::invoke_js('hideid', ns('pheno_db_user_holder'))
          golem::invoke_js('hideid', ns('pheno_db_password_holder'))
        } else {
          golem::invoke_js('showid', ns('pheno_db_user_holder'))
          golem::invoke_js('showid', ns('pheno_db_password_holder'))
        }
      }
    )

    observeEvent(
      input$brapi_traits,
      {
        temp <- data()

        temp$metadata$pheno <- temp$metadata$pheno[temp$metadata$pheno$parameter != 'trait',]

        for (i in input[['brapi_traits']]) {
          temp$metadata$pheno <- rbind(temp$metadata$pheno, data.frame(parameter = 'trait', value = i))
          temp$data$pheno[[i]] <- as.numeric(temp$data$pheno[[i]])
        }

        data(temp)
      }
    )

    pheno_data <- reactive({
      if(length(input$pheno_input) > 0){
        if (input$pheno_input == 'file') {
          if (is.null(input$pheno_file)) {return(NULL)}else{
            data <- as.data.frame(data.table::fread(input$pheno_file$datapath, sep = input$pheno_sep,
                                                    quote = input$pheno_quote, dec = input$pheno_dec, header = TRUE))
          }
        } else if (input$pheno_input == 'url') {
          if (input$pheno_url == ''){return(NULL)} else{
            data <- as.data.frame(data.table::fread(input$pheno_url, sep = input$pheno_sep,
                                                    quote = input$pheno_quote, dec = input$pheno_dec, header = TRUE))
          }
        } else if (input$pheno_input == 'brapi') {
          if (input$pheno_db_load != 1){return(NULL)} else {
            shinybusy::show_modal_spinner('fading-circle', text = 'Loading Data...')

            QBMS::set_trial(input$pheno_db_trial)
            data <- QBMS::get_trial_data()

            data$trialName <- input$pheno_db_trial

            shinybusy::remove_modal_spinner()

            shinyWidgets::show_alert(title = 'Done!', type = 'success')
          }
        }

        return(data)
      }
    })

    observeEvent(
      pheno_data(),
      {
        temp <- data()
        temp$data$pheno <- pheno_data()

        output$preview_pheno <- DT::renderDT({
          req(pheno_data())

          DT::datatable(pheno_data(),
                        extensions = 'Buttons',
                        options = list(dom = 'Blfrtip',
                                       scrollX = TRUE,
                                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                       lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All')))
          )

        })

        if (input$pheno_input == 'brapi') {
          output$brapi_trait_map <- renderUI({
            selectInput(
              inputId  = ns('brapi_traits'),
              label    = 'Trait(s):',
              multiple = TRUE,
              choices  = as.list(c('', colnames(pheno_data()))),
            )
          })

          ### BrAPI auto mapping #############################################
          if ('year' %in% temp$metadata$pheno$parameter) {
            temp$metadata$pheno[temp$metadata$pheno$parameter == 'year', 'value'] <- 'year'
          } else {
            temp$metadata$pheno <- rbind(temp$metadata$pheno, data.frame(parameter = 'year', value = 'year'))
          }

          if ('environment' %in% temp$metadata$pheno$parameter) {
            temp$metadata$pheno[temp$metadata$pheno$parameter == 'environment', 'value'] <- 'studyName'
          } else {
            temp$metadata$pheno <- rbind(temp$metadata$pheno, data.frame(parameter = 'environment', value = 'studyName'))
          }

          if ('rep' %in% temp$metadata$pheno$parameter) {
            temp$metadata$pheno[temp$metadata$pheno$parameter == 'rep', 'value'] <- 'rep'
          } else {
            temp$metadata$pheno <- rbind(temp$metadata$pheno, data.frame(parameter = 'rep', value = 'rep'))
          }

          if ('iBlock' %in% temp$metadata$pheno$parameter) {
            temp$metadata$pheno[temp$metadata$pheno$parameter == 'iBlock', 'value'] <- 'block'
          } else {
            temp$metadata$pheno <- rbind(temp$metadata$pheno, data.frame(parameter = 'iBlock', value = 'block'))
          }

          if ('row' %in% temp$metadata$pheno$parameter) {
            temp$metadata$pheno[temp$metadata$pheno$parameter == 'row', 'value'] <- 'positionCoordinateY'
          } else {
            temp$metadata$pheno <- rbind(temp$metadata$pheno, data.frame(parameter = 'row', value = 'positionCoordinateY'))
          }

          if ('col' %in% temp$metadata$pheno$parameter) {
            temp$metadata$pheno[temp$metadata$pheno$parameter == 'col', 'value'] <- 'positionCoordinateX'
          } else {
            temp$metadata$pheno <- rbind(temp$metadata$pheno, data.frame(parameter = 'col', value = 'positionCoordinateX'))
          }

          if ('designation' %in% temp$metadata$pheno$parameter) {
            temp$metadata$pheno[temp$metadata$pheno$parameter == 'designation', 'value'] <- 'germplasmName'
          } else {
            temp$metadata$pheno <- rbind(temp$metadata$pheno, data.frame(parameter = 'designation', value = 'germplasmName'))
          }

          if ('gid' %in% temp$metadata$pheno$parameter) {
            temp$metadata$pheno[temp$metadata$pheno$parameter == 'gid', 'value'] <- 'germplasmDbId'
          } else {
            temp$metadata$pheno <- rbind(temp$metadata$pheno, data.frame(parameter = 'gid', value = 'germplasmDbId'))
          }

          if ('location' %in% temp$metadata$pheno$parameter) {
            temp$metadata$pheno[temp$metadata$pheno$parameter == 'location', 'value'] <- 'locationName'
          } else {
            temp$metadata$pheno <- rbind(temp$metadata$pheno, data.frame(parameter = 'location', value = 'locationName'))
          }

          if ('trial' %in% temp$metadata$pheno$parameter) {
            temp$metadata$pheno[temp$metadata$pheno$parameter == 'trial', 'value'] <- 'trialName'
          } else {
            temp$metadata$pheno <- rbind(temp$metadata$pheno, data.frame(parameter = 'trial', value = 'trialName'))
          }

          if ('entryType' %in% temp$metadata$pheno$parameter) {
            temp$metadata$pheno[temp$metadata$pheno$parameter == 'entryType', 'value'] <- 'entryType'
          } else {
            temp$metadata$pheno <- rbind(temp$metadata$pheno, data.frame(parameter = 'entryType', value = 'entryType'))
          }

          # shinybusy::show_modal_spinner('fading-circle', text = 'Loading Ontology...')
          #
          # traits <- QBMS::get_trial_obs_ontology()
          #
          # temp$metadata$pheno <- temp$metadata$pheno[temp$metadata$pheno$parameter != 'trait',]
          #
          # for (i in traits$name) {
          #   temp$metadata$pheno <- rbind(temp$metadata$pheno, data.frame(parameter = 'trait', value = i))
          # }
          #
          # shinybusy::remove_modal_spinner()


          # dummy pedigree table
          temp$data$pedigree <- data.frame(germplasmName = unique(pheno_data()$germplasmName), mother = NA, father = NA, yearOfOrigin = NA)
          temp$metadata$pedigree <- data.frame(parameter=c("designation","mother","father","yearOfOrigin"), value=c("germplasmName","mother","father","yearOfOrigin") )
          # stage       <- NA
          # pipeline    <- NA
          # country   **<- get_study_info()$locationDbId, then list_locations()
          # season    * <- get_study_info()$seasons

          ####################################################################
        }
        data(temp)
      }
    )

    output$pheno_map <- renderUI({
      header <- colnames(pheno_data())
      pheno_map <- lapply(map, function(x) {
        column(3,
               selectInput(
                 inputId  = ns(paste0('select', x)),
                 label    = x,
                 multiple = ifelse(x == 'trait', TRUE, FALSE),
                 choices  = as.list(c('', header)),
               ),

               # shinyBS::bsTooltip(ns(paste0('select', x)), 'Mapping this!', placement = 'left', trigger = 'hover'),

               renderPrint({
                 req(input[[paste0('select', x)]])
                 temp <- data()
                 if (x == 'trait') {
                   temp$metadata$pheno <- temp$metadata$pheno[temp$metadata$pheno$parameter != 'trait',]
                   for (i in input[[paste0('select', x)]]) {
                     temp$metadata$pheno <- rbind(temp$metadata$pheno, data.frame(parameter = 'trait', value = i))
                     temp$data$pheno[,i] <- as.numeric(temp$data$pheno[,i])
                   }
                 } else { # is any other column other than trait
                   if (x %in% temp$metadata$pheno$parameter) {
                     temp$metadata$pheno[temp$metadata$pheno$parameter == x, 'value'] <- input[[paste0('select', x)]]
                   } else {
                     temp$metadata$pheno <- rbind(temp$metadata$pheno, data.frame(parameter = x, value = input[[paste0('select', x)]]))
                   }
                 }

                 if (x == 'designation') {
                   temp$data$pedigree <- data.frame(designation = unique(pheno_data()[[input[[paste0('select', x)]]]]), mother = NA, father = NA, yearOfOrigin = NA)
                   temp$metadata$pedigree <- data.frame(parameter=c("designation","mother","father","yearOfOrigin"), value=c("designation","mother","father","yearOfOrigin") )
                 }
                 data(temp)
               }),
        )
      })
      fluidRow(do.call(tagList, pheno_map))
    })

    observeEvent(
      input$pheno_example,
      if(length(input$pheno_example) > 0){
        if (input$pheno_example) {
          updateSelectInput(session, 'pheno_input', selected = 'url')

          pheno_example_url <-  paste0(session$clientData$url_protocol, '//',
                                       session$clientData$url_hostname, ':',
                                       session$clientData$url_port,
                                       session$clientData$url_pathname,
                                       pheno_example)

          updateTextInput(session, 'pheno_url', value = pheno_example_url)

          golem::invoke_js('hideid', ns('pheno_file_holder'))
          golem::invoke_js('showid', ns('pheno_url'))
          golem::invoke_js('hideid', ns('pheno_db'))
        } else {
          updateSelectInput(session, 'pheno_input', selected = 'file')
          updateTextInput(session, 'pheno_url', value = '')

          golem::invoke_js('showid', ns('pheno_file_holder'))
          golem::invoke_js('hideid', ns('pheno_url'))
          golem::invoke_js('hideid', ns('pheno_db'))
        }
      }

    )
    outConcatenateEnv <- eventReactive(input$concatenateEnv, { # button to concatenate other columns in environment
      req(data())
      myObject <- data()
      environmentColumn <- which(myObject$metadata$pheno$parameter == "environment")
      if(length(environmentColumn) > 0){ # user has mapped an environment column
        otherEnvironmentColumn <- which(myObject$metadata$pheno$parameter %in% c("year","season","location","trial","environment"))
        if(length(otherEnvironmentColumn) > 1){ # if user has mapped more than one column
          myObject$data$pheno[,myObject$metadata$pheno[environmentColumn, "value"]] <- apply(myObject$data$pheno[, myObject$metadata$pheno[otherEnvironmentColumn, "value"], drop=FALSE],1, function(x){paste(x, collapse = "_")} )
          data(myObject)
          cat(paste("Columns",paste(myObject$metadata$pheno[otherEnvironmentColumn, "value"], collapse = ", "), "concatenated and pasted in the",myObject$metadata$pheno[environmentColumn, "value"], "column"))
        }else{cat("No additional columns to concatenate to your 'environment' column")}
      }else{ # user has not mapped an environment column, we will add it
        otherEnvironmentColumn <- which(myObject$metadata$pheno$parameter %in% c("year","season","location","trial","environment"))
        if(length(otherEnvironmentColumn) > 0){ # if user has mapped more than one column
          myObject$data$pheno[,"environment"] <- apply(myObject$data$pheno[, myObject$metadata$pheno[otherEnvironmentColumn, "value"], drop=FALSE],1, function(x){paste(x, collapse = "_")} )
          myObject$metadata$pheno <- rbind(myObject$metadata$pheno, data.frame(parameter = 'environment', value = 'environment' ))
          data(myObject)
          cat(paste("Columns",paste(myObject$metadata$pheno[otherEnvironmentColumn, "value"], collapse = ", "), "concatenated and pasted in a column named 'environment' "))
        }else{cat(paste("Please map at least one of the columns 'year', 'season', 'location', 'trial' or 'environment' to be able to do a genetic evaluation "))}
      }
    })
    output$outConcatenateEnv <- renderPrint({
      outConcatenateEnv()
    })
    ### Genotypic tab controls #################################################

    observeEvent(
      input$geno_input,
      if(length(input$geno_input) > 0){ # added
        if (input$geno_input == 'file' ) {
          golem::invoke_js('showid', ns('geno_file_holder'))
          golem::invoke_js('hideid', ns('geno_url'))
          golem::invoke_js('hideid', ns('geno_table_mapping'))
          golem::invoke_js('hideid', ns('geno_table_options'))
          updateCheckboxInput(session, 'geno_example', value = FALSE)
        } else if (input$geno_input == 'url') {
          golem::invoke_js('hideid', ns('geno_file_holder'))
          golem::invoke_js('hideid', ns('geno_table_mapping'))
          golem::invoke_js('hideid', ns('geno_table_options'))
          golem::invoke_js('showid', ns('geno_url'))
        } else if (input$geno_input == 'matfile' ){
          golem::invoke_js('showid', ns('geno_file_holder'))
          golem::invoke_js('showid', ns('geno_table_mapping'))
          golem::invoke_js('showid', ns('geno_table_options'))
          golem::invoke_js('hideid', ns('geno_url'))
          updateCheckboxInput(session, 'geno_example', value = FALSE)
        } else if (input$geno_input == 'matfileurl' ){
          golem::invoke_js('hideid', ns('geno_file_holder'))
          golem::invoke_js('showid', ns('geno_table_mapping'))
          golem::invoke_js('showid', ns('geno_table_options'))
          golem::invoke_js('showid', ns('geno_url'))
        }
      }
    )

    geno_data_table = reactive({ # function to purely just read a csv when we need to match the genotype file
      if(length(input$geno_input) > 0){ # added
        if (input$geno_input == 'matfile' ) {
          if (is.null(input$geno_file)) {return(NULL)}else{
            snps_file <- input$geno_file$datapath
          }
        } else if(input$geno_input == 'matfileurl'){
          if (is.null(input$geno_file)) {return(NULL)}else{
            snps_file <- input$geno_url
          }
        }else {
          return(NULL);
        }
        shinybusy::show_modal_spinner('fading-circle', text = 'Loading...')
        df <- as.data.frame(data.table::fread(snps_file, sep = input$geno_sep, quote = input$geno_quote, dec = input$geno_dec, header = TRUE))
        shinybusy::remove_modal_spinner()
        return(df)
      }else{
        return(NULL)
      }
    })
    observeEvent(c(geno_data_table()), { # update values for columns in designation and first snp and last snp
      req(geno_data_table())
      provGeno <- geno_data_table()
      output$preview_geno <- DT::renderDT({
        req(geno_data_table())
        DT::datatable(geno_data_table()[,1:min(c(50,ncol(geno_data_table())))],
                      extensions = 'Buttons',
                      options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All')))
        )
      })
      updateSelectizeInput(session, "geno_table_firstsnp", choices = colnames(provGeno)[1:min(c(ncol(provGeno),100))], selected = character(0))
      updateSelectizeInput(session, "geno_table_lastsnp", choices = colnames(provGeno)[max(c(1,ncol(provGeno)-100)):ncol(provGeno)], selected = character(0))
      updateSelectizeInput(session, "geno_table_designation", choices = colnames(provGeno)[1:min(c(ncol(provGeno),100))], selected = character(0))
    })
    observeEvent( # reactive for the csv geno read, active once the user has selected the proper columns
      c(geno_data_table(), input$geno_table_firstsnp, input$geno_table_lastsnp, input$geno_table_designation),
      {
        req(geno_data_table())
        req(input$geno_table_firstsnp)
        req(input$geno_table_lastsnp)
        req(input$geno_table_designation)
        if(!is.null(input$geno_table_firstsnp) & !is.null(input$geno_table_lastsnp) & !is.null(input$geno_table_designation) ){
          temp <- data()
          tempG <- geno_data_table()
          rownames(tempG) <- tempG[,which(colnames(tempG)==input$geno_table_designation)]
          tempG <- tempG[,which(colnames(tempG)==input$geno_table_firstsnp):which(colnames(tempG)==input$geno_table_lastsnp)]
          missingData=c("NN","FAIL","FAILED","Uncallable","Unused","NA","")
          for(iMiss in missingData){tempG[which(tempG==iMiss, arr.ind = TRUE)] <- NA}
          shinybusy::show_modal_spinner('fading-circle', text = 'Converting...')
          tempG <- sommer::atcg1234(tempG, maf = -1, imp = FALSE)
          shinybusy::remove_modal_spinner()
          temp$data$geno <- tempG$M
          refAlleles <- tempG$ref.alleles
          map <- data.frame(a=colnames(tempG$M), chrom=1, pos=1:ncol(tempG$M))
          map$refAllele <- tempG$ref.alleles[2,]
          map$altAllele <- tempG$ref.alleles[1,]
          colnames(map) <- c('marker', 'chr', 'pos', 'refAllele', 'altAllele')
          temp$metadata$geno <- map
          data(temp)
        }else{return(NULL)}
      }
    )

    geno_data <- reactive({
      if(length(input$geno_input) > 0){ # added
        if (input$geno_input == 'file' ) {
          if (is.null(input$geno_file)) {return(NULL)}else{
            snps_file <- input$geno_file$datapath
          }
        } else {
          if (input$geno_url == '') {return(NULL)}else{
            snps_file <- input$geno_url
          }
        }

        # library(vcfR); vcf_data <- vcfR::read.vcfR(snps_file); hmp_data <- vcfR::vcfR2hapmap(vcf.data)

        shinybusy::show_modal_spinner('fading-circle', text = 'Loading...')
        df <- as.data.frame(data.table::fread(snps_file, sep = '\t', header = TRUE))
        shinybusy::remove_modal_spinner()

        hapmap_snp_attr <- c('rs#', 'alleles', 'chrom', 'pos', 'strand', 'assembly#',
                             'center', 'protLSID', 'assayLSID', 'panelLSID', 'QCcode',
                             'rs', 'assembly','panel' # column versions of vcfR
        )
        if(length(intersect(hapmap_snp_attr, colnames(df)[1:11])) != 11){
          # if (!all(colnames(df)[1:11] == hapmap_snp_attr)) {
          shinyWidgets::show_alert(title = 'Error !!', text = 'Not a valid HapMap file format :-(', type = 'error')
          return(NULL)
        }
        colnames(df)[1:11] <- hapmap_snp_attr[1:11]
        first_row   <- df[1, -c(1:11)]
        valid_IUPAC <- c('A', 'C', 'G', 'T', 'U', 'W', 'S', 'M', 'K', 'R', 'Y', 'B', 'D', 'H', 'V', 'N')
        double_code <- c("AA","TT","CC","GG","AT","TA","AC","CA","AG","GA","TC","CT","TG","GT","CG","GC","NN")
        # IUPAC single-letter code
        if (all(first_row %in% valid_IUPAC)) {

          shinybusy::show_modal_spinner('fading-circle', text = 'Converting...')
          df <- hapMapChar2Numeric(df)
          shinybusy::remove_modal_spinner()

          # -1, 0, 1 numeric coding
        } else if (min(as.numeric(first_row), na.rm = TRUE) == -1 &
                   max(as.numeric(first_row), na.rm = TRUE) == 1) {

          df <- cbind(df[, 1:11],
                      data.frame(apply(df[, -c(1:11)], 2, function(x) 1 + as.numeric(as.character(x)))))

          # 0, 1, 2 numeric coding
        } else if (min(as.numeric(first_row), na.rm = TRUE) == 0 &
                   max(as.numeric(first_row), na.rm = TRUE) == 2) {

          df <- cbind(df[, 1:11],
                      data.frame(apply(df[, -c(1:11)], 2, function(x) as.numeric(as.character(x)))))

          # something else!
        } else if(all(first_row %in% double_code)){
          shinybusy::show_modal_spinner('fading-circle', text = 'Converting...')
          df <- hapMapChar2NumericDouble(df)
          shinybusy::remove_modal_spinner()
        }else {
          shinyWidgets::show_alert(title = 'Error !!', text = 'Not a valid HapMap file format :-(', type = 'error')
          return(NULL)
        }
        return(df)

      }else{
        return(NULL)
      }

    })

    output$chrom_summary <- renderTable({
      if (!is.null(geno_data())) {
        data.frame(
          chrom = unique(geno_data()[,'chrom']),
          min_pos = aggregate(pos ~ chrom, data = geno_data(), FUN = min)[,2],
          max_pos = aggregate(pos ~ chrom, data = geno_data(), FUN = max)[,2],
          snps_count = aggregate(pos ~ chrom, data = geno_data(), FUN = length)[,2]
        )
      }
    })

    output$geno_summary <- renderText({
      temp <- data()

      if (!is.null(geno_data()) & any(temp$metadata$pheno$parameter == 'designation')) {
        designationColumn <- temp$metadata$pheno[which(temp$metadata$pheno$parameter == "designation"),"value"]
        paste(
          "Data Integrity Checks:\n",

          sum(colnames(geno_data()[, -c(1:11)]) %in% unique(temp$data$pheno[, designationColumn ])),
          "Accessions exist in both phenotypic and genotypic files (will be used to train the model)\n",

          sum(!colnames(geno_data()[, -c(1:11)]) %in% unique(temp$data$pheno[, designationColumn ])),
          "Accessions have genotypic data but no phenotypic (will be predicted, add to pheno data file with NA value)\n",

          sum(!unique(temp$data$pheno[, designationColumn ]) %in% colnames(geno_data()[, -c(1:11)])),
          'Accessions have phenotypic data but no genotypic (will not contribute to the training model)'
        )
      }
    })

    observeEvent(
      geno_data(),
      {
        temp <- data()

        temp$data$geno <- t(as.matrix(geno_data()[, -c(1:11)])) - 1
        colnames(temp$data$geno) <- geno_data()$`rs#`

        map <- geno_data()[, c('rs#', 'chrom', 'pos', 'alleles', 'alleles')]
        colnames(map) <- c('marker', 'chr', 'pos', 'refAllele', 'altAllele')
        map$refAllele <- substr(map$refAllele, 1, 1)
        map$altAllele <- substr(map$altAllele, 3, 3)

        temp$metadata$geno <- map

        data(temp)
      }
    )

    observeEvent(
      input$geno_example,
      if(length(input$geno_example) > 0){ # added
        if (input$geno_example) {
          updateSelectInput(session, 'geno_input', selected = 'url')

          geno_example_url <-  paste0(session$clientData$url_protocol, '//',
                                      session$clientData$url_hostname, ':',
                                      session$clientData$url_port,
                                      session$clientData$url_pathname,
                                      geno_example)

          updateTextInput(session, 'geno_url', value = geno_example_url)

          golem::invoke_js('hideid', ns('geno_file_holder'))
          golem::invoke_js('showid', ns('geno_url'))
        } else {
          updateSelectInput(session, 'geno_input', selected = 'file')
          updateTextInput(session, 'geno_url', value = '')

          golem::invoke_js('showid', ns('geno_file_holder'))
          golem::invoke_js('hideid', ns('geno_url'))
        }
      }
    )

    ### Pedigree tab controls ##################################################

    observeEvent(
      input$ped_input,
      if(length(input$ped_input) > 0){ # added
        if (input$ped_input == 'file') {
          golem::invoke_js('showid', ns('ped_file_holder'))
          golem::invoke_js('hideid', ns('ped_url'))
          golem::invoke_js('showid', ns('ped_csv_options'))
          updateCheckboxInput(session, 'ped_example', value = FALSE)
        } else if (input$ped_input == 'url') {
          golem::invoke_js('hideid', ns('ped_file_holder'))
          golem::invoke_js('showid', ns('ped_url'))
          golem::invoke_js('showid', ns('ped_csv_options'))
        }
      }
    )

    ped_data <- reactive({
      if(length(input$ped_input) > 0){ # added
        if (input$ped_input == 'file') {
          if (is.null(input$ped_file)) return(NULL)
          data <- as.data.frame(data.table::fread(input$ped_file$datapath, sep = input$ped_sep,
                                                  quote = input$ped_quote, dec = input$ped_dec, header = TRUE))
        } else if (input$ped_input == 'url') {
          if (input$ped_url == '') return(NULL)
          data <- as.data.frame(data.table::fread(input$ped_url, sep = input$ped_sep,
                                                  quote = input$ped_quote, dec = input$ped_dec, header = TRUE))
        } else {
          return(NULL)
        }

        return(data)
      }
    })

    observeEvent(
      ped_data(),
      {
        temp <- data()

        # should check file format here!
        temp$data$pedigree <- ped_data()

        output$preview_ped <- DT::renderDT({
          req(ped_data())

          DT::datatable(ped_data(),
                        extensions = 'Buttons',
                        options = list(dom = 'Blfrtip',
                                       scrollX = TRUE,
                                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                       lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All')))
          )

        })

        data(temp)
      }
    )

    output$ped_map <- renderUI({
      header <- colnames(ped_data())
      fluidRow(
        column(3, selectInput(
          inputId  = ns('ped_designation'),
          label    = 'designation',
          choices  = as.list(c('', header)),
        )),
        column(3, selectInput(
          inputId  = ns('ped_mother'),
          label    = 'mother',
          choices  = as.list(c('', header)),
        )),
        column(3, selectInput(
          inputId  = ns('ped_father'),
          label    = 'father',
          choices  = as.list(c('', header)),
        )),
        column(3, selectInput(
          inputId  = ns('ped_year'),
          label    = 'Year of Origin',
          choices  = as.list(c('', header)),
        )),
        ## testing
        renderPrint({
          req(input$ped_designation)
          # req(input$ped_father)
          # req(input$ped_mother)
          # req(input$ped_year)
          temp <- data()
          if ("designation" %in% temp$metadata$pedigree$parameter) {
            temp$metadata$pedigree[temp$metadata$pedigree$parameter == "designation", 'value'] <- input$ped_designation
          } else {
            temp$metadata$pedigree <- rbind(temp$metadata$pedigree, data.frame(parameter = "designation", value = input$ped_designation ))
          }
          if ("mother" %in% temp$metadata$pedigree$parameter) {
            temp$metadata$pedigree[temp$metadata$pedigree$parameter == "mother", 'value'] <- input$ped_mother
          } else {
            temp$metadata$pedigree <- rbind(temp$metadata$pedigree, data.frame(parameter = "mother", value = input$ped_mother ))
          }
          if ("father" %in% temp$metadata$pedigree$parameter) {
            temp$metadata$pedigree[temp$metadata$pedigree$parameter == "father", 'value'] <- input$ped_father
          } else {
            temp$metadata$pedigree <- rbind(temp$metadata$pedigree, data.frame(parameter = "father", value = input$ped_father ))
          }
          if ("yearOfOrigin" %in% temp$metadata$pedigree$parameter) {
            temp$metadata$pedigree[temp$metadata$pedigree$parameter == "yearOfOrigin", 'value'] <- input$ped_year
          } else {
            temp$metadata$pedigree <- rbind(temp$metadata$pedigree, data.frame(parameter = "yearOfOrigin", value = input$ped_year ))
          }
          data(temp)
        }),
      )
    })

    output$ped_summary <- renderText({
      req(input$ped_designation)
      tmp <- data()
      ped <- ped_data()
      if (!is.null(ped) & any(tmp$metadata$pheno$parameter == 'designation') & any(tmp$metadata$pedigree$parameter == 'designation') ) {
        designationColumnInPheno <- tmp$metadata$pheno[which(tmp$metadata$pheno$parameter == "designation"),"value"]
        designationColumnInPed <- tmp$metadata$pedigree[which(tmp$metadata$pedigree$parameter == "designation"),"value"]
        paste(
          "Data Integrity Checks:\n",
          sum(ped[, designationColumnInPed ] %in% unique(tmp$data$pheno[, designationColumnInPheno ])),
          "Accessions exist in both phenotypic and pedigree files\n",
          sum(!unique(tmp$data$pheno[, designationColumnInPheno ]) %in% ped[, designationColumnInPed ]),
          "Accessions have phenotypic data but no pedigree data\n",
          ifelse(any(tmp$metadata$pedigree$parameter == 'yearOfOrigin'),  sum(ped[, designationColumnInPed ] %in% unique(tmp$data$pheno[, designationColumnInPheno ])) ,0),
          "Accessions in phenotypic data have no year of origin info\n"
        )
      }
    })

    observeEvent(
      input$ped_example,
      if (input$ped_example) {
        updateSelectInput(session, 'ped_input', selected = 'url')

        ped_example_url <-  paste0(session$clientData$url_protocol, '//',
                                   session$clientData$url_hostname, ':',
                                   session$clientData$url_port,
                                   session$clientData$url_pathname,
                                   ped_example)

        updateTextInput(session, 'ped_url', value = ped_example_url)

        golem::invoke_js('hideid', ns('ped_file_holder'))
        golem::invoke_js('showid', ns('ped_url'))
      } else {
        updateSelectInput(session, 'ped_input', selected = 'file')
        updateTextInput(session, 'ped_url', value = '')

        golem::invoke_js('showid', ns('ped_file_holder'))
        golem::invoke_js('hideid', ns('ped_url'))
      }
    )

    ### Weather tab controls ##################################################

    ### QTL profile tab controls #################################################

    observeEvent(
      input$qtl_input,
      if(length(input$qtl_input) > 0){ # added
        if (input$qtl_input == 'qtlfile' ){
          golem::invoke_js('showid', ns('qtl_file_holder'))
          golem::invoke_js('showid', ns('qtl_table_mapping'))
          golem::invoke_js('showid', ns('qtl_table_options'))
          golem::invoke_js('hideid', ns('qtl_url'))
          updateCheckboxInput(session, 'qtl_example', value = FALSE)
        } else if (input$qtl_input == 'qtlfileurl' ){
          golem::invoke_js('hideid', ns('qtl_file_holder'))
          golem::invoke_js('showid', ns('qtl_table_mapping'))
          golem::invoke_js('showid', ns('qtl_table_options'))
          golem::invoke_js('showid', ns('qtl_url'))
        }
      }
    )

    observeEvent(
      input$qtl_example,
      if(length(input$qtl_example) > 0){ # if user clicked on qtl example
        if (input$qtl_example) {
          updateSelectInput(session, 'qtl_input', selected = 'qtlfileurl')

          qtl_example_url <-  paste0(session$clientData$url_protocol, '//',
                                     session$clientData$url_hostname, ':',
                                     session$clientData$url_port,
                                     session$clientData$url_pathname,
                                     qtl_example)

          updateTextInput(session, 'qtl_url', value = qtl_example_url)

          golem::invoke_js('hideid', ns('qtl_file_holder'))
          golem::invoke_js('showid', ns('qtl_url'))
        } else {
          updateSelectInput(session, 'qtl_input', selected = 'qtlfile')
          updateTextInput(session, 'qtl_url', value = '')

          golem::invoke_js('showid', ns('qtl_file_holder'))
          golem::invoke_js('hideid', ns('qtl_url'))
        }
      }

    )

    qtl_data_table = reactive({ # function to purely just read a csv when we need to match the qtltype file
      if(length(input$qtl_input) > 0){ # added
        if (input$qtl_input == 'qtlfile' ) {
          if (is.null(input$qtl_file)) {return(NULL)}else{
            qtls_file <- input$qtl_file$datapath
          }
        } else if(input$qtl_input == 'qtlfileurl'){
          if (input$qtl_url == '') {return(NULL)}else{
            qtls_file <- input$qtl_url
          }
        }else {
          return(NULL);
        }
        shinybusy::show_modal_spinner('fading-circle', text = 'Loading...')
        df <- as.data.frame(data.table::fread(qtls_file, sep = input$qtl_sep, quote = input$qtl_quote, dec = input$qtl_dec, header = TRUE))
        shinybusy::remove_modal_spinner()
        return(df)
      }else{
        return(NULL)
      }
    })
    observeEvent(c(qtl_data_table()), { # update values for columns in designation and first snp and last snp
      req(qtl_data_table())
      provqtl <- qtl_data_table()
      output$preview_qtl <- DT::renderDT({
        req(qtl_data_table())
        DT::datatable(qtl_data_table()[,1:min(c(50,ncol(qtl_data_table())))],
                      extensions = 'Buttons',
                      options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All')))
        )
      })
      updateSelectizeInput(session, "qtl_table_firstsnp", choices = colnames(provqtl),selected = character(0))
      updateSelectizeInput(session, "qtl_table_designation", choices = colnames(provqtl)[1:min(c(ncol(provqtl),100))], selected = character(0))
    })
    observeEvent( # reactive for the csv qtl read, active once the user has selected the proper columns
      c(qtl_data_table(), input$qtl_table_firstsnp, input$qtl_table_designation),
      {
        req(qtl_data_table())
        req(input$qtl_table_firstsnp)
        req(input$qtl_table_designation)
        if(!is.null(input$qtl_table_firstsnp) & !is.null(input$qtl_table_designation) ){
          temp <- data()
          temp$metadata$qtl <- data.frame(parameter="designation",value=c(input$qtl_table_designation))
          temp$data$qtl <- qtl_data_table()[,unique(c(input$qtl_table_designation,input$qtl_table_firstsnp))]
          data(temp)
        }else{return(NULL)}
      }
    )


    ### Previous-analyses tab controls ##################################################

    observeEvent(
      input$previous_object_input,
      if(length(input$previous_object_input) > 0){ # added
        if (input$previous_object_input == 'pcfile') {
          golem::invoke_js('showid', ns('previous_object_file_holder'))
          golem::invoke_js('hideid', ns('previous_object_retrieve'))
        } else if (input$previous_object_input == 'cloudfile') {
          golem::invoke_js('hideid', ns('previous_object_file_holder'))
          golem::invoke_js('showid', ns('previous_object_retrieve'))
        }
      }
    )


    output$previous_input2 <- renderUI({}) # this 2 lines avoid issues when displaying an uiOutput
    outputOptions(output, "previous_input2", suspendWhenHidden = FALSE)
    observeEvent( # this is the part where we either load the previous analysis from cloud or PC
      c(input$previous_object_input),
      {
        if(input$previous_object_input == 'cloudfile'){ # upload from cloud

          previousFilesAvailable <- eventReactive(input$refreshPreviousAnalysis, { #
            selectInput(inputId=ns('previous_input'), label=NULL, choices=dir(file.path("R/outputs")), multiple = FALSE)
          })
          output$previous_input2 <- renderPrint({  previousFilesAvailable()    })
          outLoad <- eventReactive(input$runLoadPrevious, {
            # req(data())
            req(input$previous_input)
            shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
            ## replace tables
            tmp <- data() # current or empty dataset
            load(file.path(getwd(),res_auth$repository,input$previous_input)) # old dataset
            tmp$data <- result$data
            tmp$metadata <- result$metadata
            tmp$modifications <- result$modifications
            tmp$predictions <- result$predictions
            tmp$metrics <- result$metrics
            tmp$modeling <- result$modeling
            tmp$status <- result$status
            data(tmp) # update data with results
            shinybusy::remove_modal_spinner()
            cat(paste("Dataset:",input$previous_input,"loaded successfully."))
          }) ## end eventReactive
          output$outLoad <- renderPrint({
            outLoad()
          })
        }else if(input$previous_object_input == 'pcfile'){ # upload rds
          outLoad2 <- eventReactive(input$previous_object_file, {
            req(input$previous_object_file)
            load(input$previous_object_file$datapath)
            tmp <- data()
            tmp$data <- result$data
            tmp$metadata <- result$metadata
            tmp$modifications <- result$modifications
            tmp$predictions <- result$predictions
            tmp$metrics <- result$metrics
            tmp$modeling <- result$modeling
            tmp$status <- result$status
            data(tmp) # update data with results
            cat(paste("Dataset","loaded successfully."))
          }) ## end eventReactive
          output$outLoad2 <- renderPrint({
            outLoad2()
          })
        }else{

        }
      }
    )

    ### Control Nex/Back buttons ###############################################

    back_bn  <- actionButton(ns('prev_tab'), 'Back')
    next_bn  <- actionButton(ns('next_tab'), 'Next')
    tab_list <- c(ns('tab1'), ns('tab2'), ns('tab3'), ns('tab4'), ns('tab5'))

    output$navigate <- renderUI({
      tags$div(align = 'center',
               column(1, if (which(tab_list == input$tabset) != 1) back_bn),
               column(1, offset = 10, if (which(tab_list == input$tabset) != length(tab_list)) next_bn),
      )
    })

    observeEvent(input$prev_tab,
                 {
                   n <- which(tab_list == input$tabset)
                   updateTabsetPanel(session, 'tabset', selected = tab_list[n - 1])
                 }
    )

    observeEvent(input$next_tab,
                 {
                   n <- which(tab_list == input$tabset)
                   updateTabsetPanel(session, 'tabset', selected = tab_list[n + 1])
                 }
    )
  })
}

## To be copied in the UI
# mod_getData_ui("getData_1")

## To be copied in the server
# mod_getData_server("getData_1")

hapMapChar2Numeric <- function(hapMap) {
  # http://adv-r.had.co.nz/C-interface.html
  convertChar2Numeric <- inline::cfunction(signature(SNPsMatrix="character",
                                                     refAllele="character",
                                                     rowsNum="integer",
                                                     colsNum="integer"),
                                           "int i,j;

  // matrix dimentions
  int r = asInteger(rowsNum);
  int c = asInteger(colsNum);

  // length of the matrix
  int length = r*c;

  // create matrix of integers with the same size as SNPsMatrix
  SEXP SNPsNum;
  PROTECT(SNPsNum = allocMatrix(INTSXP, r, c));

  // convert SNPs codes from the standard IUPAC code (single char) to
  // numeric 0, 1, or 2 (use 1 for heterozygous)
  for(i = 0; i < r; i++){
    char* x;
    char alleleA;

    // we need to get the reference allele in each SNP (row)
    x = (char*)CHAR(STRING_ELT(refAllele, i));
    alleleA = x[0];

    // convert SNPsMatrix to numeric 0,1,2
    // now with alleleA we can convert the genotypes to numeric 0, 1, 2
    for(j = 0; j < c; j++){
      x = (char*)CHAR(STRING_ELT(SNPsMatrix, i*c+j));

      // if current SNP is the same of reference allele (alleleA)
      if(x[0] == alleleA){
        // then assign 0 in the SNPsNum matrix
        // take care of the order of the indexes in matrix is by columns
        INTEGER(SNPsNum)[j*r + i] = 0;
      }else if(x[0] == 'A' || x[0] == 'T' || x[0] == 'C' || x[0] == 'G'){
        // if it is homozygous allele [A,T,C,G]
        // but not alleleA (i.e., minor allele)
        INTEGER(SNPsNum)[j*r + i] = 2;
      }else if(x[0] == 'N'){
        // if it is missing allele [N]
        INTEGER(SNPsNum)[j*r + i] = -9;
      }else{
        // if it is not (i.e., heterozygous)
        INTEGER(SNPsNum)[j*r + i] = 1;
      }
    }
  }
  UNPROTECT(1);
  return(SNPsNum);")

  hapMap <- as.data.frame(hapMap)

  # extract SNP infomation , which is the first 11 columns
  SNPInfo <- hapMap[,1:11]

  # remove the first 11 columns
  hapMap <- hapMap[,-c(1:11)]

  # convert the hapMap to numeric
  hapMapNumeric <- convertChar2Numeric(unlist(as.matrix(t(hapMap))),
                                       unlist(as.matrix(substr(SNPInfo$alleles,1,1))),
                                       as.integer(nrow(hapMap)),
                                       as.integer(ncol(hapMap)))

  # convert to data frame
  hapMapNumeric <- as.data.frame(hapMapNumeric)

  # convert -9 values to NA
  hapMapNumeric[hapMapNumeric == -9] <- NA

  # get back the column names (accessions)
  colnames(hapMapNumeric) <- colnames(hapMap)

  return(cbind(SNPInfo, hapMapNumeric))
}

hapMapChar2NumericDouble <- function(hapMap) {

  hapMap <- as.data.frame(hapMap)
  dim(hapMap)
  # extract SNP infomation , which is the first 11 columns
  SNPInfo <- hapMap[,1:11]

  # remove the first 11 columns
  hapMap <- hapMap[,-c(1:11)]
  missingData=c("NN","FAIL","FAILED","Uncallable","Unused","NA","",-9)
  for(iMiss in missingData){hapMap[which(hapMap==iMiss, arr.ind = TRUE)] <- NA}
  # convert the hapMap to numeric
  hapMapNumeric <- sommer::atcg1234(t(hapMap), maf = -1, imp = FALSE)

  # convert to data frame
  refAlleles <- hapMapNumeric$ref.alleles

  hapMapNumeric <- as.data.frame(t(hapMapNumeric$M+1))

  # add reference and alternate allele
  SNPInfo$alleles <- apply(refAlleles,2,function(x){paste(na.omit(x),collapse = "/")})
  # convert -9 values to NA
  # hapMapNumeric[hapMapNumeric == -9] <- NA

  # get back the column names (accessions)
  colnames(hapMapNumeric) <- colnames(hapMap)

  result <- cbind(SNPInfo, hapMapNumeric)
  return(result)
}

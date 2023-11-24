pheno_example <- 'www/example/pheno.csv'
geno_example  <- 'www/example/geno.hmp.gz'

#' getData UI Function
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
          choices = list('Upload File' = 'file', 'Copy URL' = 'url', 'BrAPI' = 'db'),
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

        tags$div(
          id = ns('pheno_db'),
          tags$p('Comming soon!', style = 'color:red; font-size:20px;')
        ),

        tags$span(id = ns('pheno_csv_options'),
          shinydashboard::box(title = 'Options', collapsible = TRUE, collapsed = TRUE, status = 'primary', solidHeader = TRUE,
            shinyWidgets::prettyRadioButtons(ns('pheno_sep'), 'Separator Character', selected = ',', inline = TRUE,
                                             choices = c('Comma' = ',', 'Semicolon' = ';', 'Tab' = "\t")),

            shinyWidgets::prettyRadioButtons(ns('pheno_quote'), 'Quoting Character', selected = '"', inline = TRUE,
                                             choices = c('None' = '', 'Double Quote' = '"', 'Single Quote' = "'")),

            shinyWidgets::prettyRadioButtons(ns('pheno_dec'), 'Decimal Points', selected = '.', inline = TRUE,
                                             choices = c('Dot' = '.', 'Comma' = ',')),
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

        hr(),
        DT::DTOutput(ns('preview_pheno')),
        uiOutput(ns('pheno_map')),
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
            choices = list('Upload File' = 'file', 'Copy URL' = 'url'),
            width   = '200px'
          ),
          tags$span(id = ns('geno_file_holder'),
                    fileInput(
                      inputId = ns('geno_file'),
                      label   = NULL,
                      width   = '400px',
                      accept  = c('application/gzip', '.gz', '.txt', '.hmp')
                    )
          ),
          textInput(
            inputId = ns('geno_url'),
            label   = NULL,
            value   = '',
            width   = '400px',
            placeholder = 'https://example.com/path/file.gz'
          ),

          if (!is.null(pheno_example)) {
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
          #Accessions have genotypic data but no phenotypic (will predict, add to pheno data file with NA value)
          # Accessions have phenotypic data but no genotypic (filter them out from the pheno data file)
          verbatimTextOutput(ns('geno_summary')),
        )
      ),
      tabPanel(
        title = 'Pedigree',
        value = ns('tab3'),
        tags$p('Comming soon!', style = 'color: red; font-size: 20px; margin-top: 25px;'),
      ),
      tabPanel(
        title = 'Weather',
        value = ns('tab4'),
        tags$p('Comming soon!', style = 'color: red; font-size: 20px; margin-top: 25px;'),
      ),
    ),

    uiOutput(ns('navigate')),
  )
}

#' getData Server Functions
#'
#' @noRd
mod_getData_server <- function(id, map = NULL, data = NULL){
  moduleServer(id , function(input, output, session){
    ns <- session$ns

    ### Phenotypic tab controls ################################################

    observeEvent(
      input$pheno_input,
      if (input$pheno_input == 'file') {
        golem::invoke_js('showid', ns('pheno_file_holder'))
        golem::invoke_js('hideid', ns('pheno_url'))
        golem::invoke_js('hideid', ns('pheno_db'))
        golem::invoke_js('showid', ns('pheno_csv_options'))
        updateCheckboxInput(session, 'pheno_example', value = FALSE)
      } else if (input$pheno_input == 'url') {
        golem::invoke_js('hideid', ns('pheno_file_holder'))
        golem::invoke_js('showid', ns('pheno_url'))
        golem::invoke_js('hideid', ns('pheno_db'))
        golem::invoke_js('showid', ns('pheno_csv_options'))
      } else {
        golem::invoke_js('hideid', ns('pheno_file_holder'))
        golem::invoke_js('hideid', ns('pheno_url'))
        golem::invoke_js('showid', ns('pheno_db'))
        golem::invoke_js('hideid', ns('pheno_csv_options'))
        updateCheckboxInput(session, 'pheno_example', value = FALSE)
      }
    )

    pheno_data <- reactive({
      if (input$pheno_input == 'file') {
        if (is.null(input$pheno_file)) return(NULL)
        data <- read.csv(input$pheno_file$datapath, sep = input$pheno_sep,
                         quote = input$pheno_quote, dec = input$pheno_dec)
      } else if (input$pheno_input == 'url') {
        if (input$pheno_url == '') return(NULL)
        data <- read.csv(input$pheno_url, sep = input$pheno_sep,
                         quote = input$pheno_quote, dec = input$pheno_dec)
      } else {
        return(NULL)
      }

      return(data)
    })

    observeEvent(
      pheno_data(),
      {
        temp <- data()
        temp$data$pheno <- pheno_data()
        temp$status <- rbind(temp$status, data.frame(module = 'qa', analysisId = as.numeric(Sys.time())))

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
        data(temp)
      }
    )

    output$pheno_map <- renderUI({
      header <- colnames(pheno_data())
      pheno_map <- lapply(map, function(x) {
        fluidPage(
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
              }
            } else {
              if (x %in% temp$metadata$pheno$parameter) {
                temp$metadata$pheno[temp$metadata$pheno$parameter == x, 'value'] <- input[[paste0('select', x)]]
              } else {
                temp$metadata$pheno <- rbind(temp$metadata$pheno, data.frame(parameter = x, value = input[[paste0('select', x)]]))
              }
            }

            if (x == 'designation') {
              temp$data$pedigree <- data.frame(designation = unique(pheno_data()[[input[[paste0('select', x)]]]]), mother = NA, father = NA)
            }
            data(temp)
          }),
        )
      })
      do.call(tagList, pheno_map)
    })

    observeEvent(
      input$pheno_example,
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
    )

    ### Genotypic tab controls #################################################

    observeEvent(
      input$geno_input,
      if (input$geno_input == 'file') {
        golem::invoke_js('showid', ns('geno_file_holder'))
        golem::invoke_js('hideid', ns('geno_url'))
        updateCheckboxInput(session, 'geno_example', value = FALSE)
      } else if (input$geno_input == 'url') {
        golem::invoke_js('hideid', ns('geno_file_holder'))
        golem::invoke_js('showid', ns('geno_url'))
      }
    )

    geno_data <- reactive({
      if (input$geno_input == 'file') {
        if (is.null(input$geno_file)) return(NULL)
        snps_file <- input$geno_file$datapath
      } else {
        if (input$geno_url == '') return(NULL)
        snps_file <- input$geno_url
      }

      # library(vcfR)
      # vcf <- read.vcfR(file.choose())
      #
      # SNPs_info <- vcfR2tidy(vcf, info_only = TRUE)$fix
      # View(SNPs_info)
      #
      # gt <- extract.gt(vcf, as.numeric = TRUE)
      # View(gt[1:100,1:100])

      shinybusy::show_modal_spinner('fading-circle', text = 'Loading...')
      df <- as.data.frame(data.table::fread(snps_file, sep = '\t', header = TRUE))
      shinybusy::remove_modal_spinner()

      hapmap_snp_attr <- c('rs#', 'alleles', 'chrom', 'pos', 'strand', 'assembly#',
                           'center', 'protLSID', 'assayLSID', 'panelLSID', 'QCcode')

      if (!all(colnames(df)[1:11] == hapmap_snp_attr)) {
        shinyWidgets::show_alert(title = 'Error !!', text = 'Not a valid HapMap file format :-(', type = 'error')
        return(NULL)
      }

      first_row   <- df[1, -c(1:11)]
      valid_IUPAC <- c('A', 'C', 'G', 'T', 'U', 'W', 'S', 'M', 'K', 'R', 'Y', 'B', 'D', 'H', 'V', 'N')

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
      } else {
        shinyWidgets::show_alert(title = 'Error !!', text = 'Not a valid HapMap file format :-(', type = 'error')
        return(NULL)
      }

      return(df)
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
        paste(
          " Data Integrity Checks:\n",

          sum(colnames(geno_data()[, -c(1:11)]) %in% unique(temp$data$pheno$designation)),
          "Accessions exist in both phenotypic and genotypic files (will be used to train the model)\n",

          sum(!colnames(geno_data()[, -c(1:11)]) %in% unique(temp$data$pheno$designation)),
          "Accessions have genotypic data but no phenotypic (will be predicted, add to pheno data file with NA value)\n",

          sum(!unique(temp$data$pheno$designation) %in% colnames(geno_data()[, -c(1:11)])),
          'Accessions have phenotypic data but no genotypic (will filtered them out from the pheno dataset)'
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
    )

    ### Control Nex/Back buttons ###############################################

    back_bn  <- actionButton(ns('prev_tab'), 'Back')
    next_bn  <- actionButton(ns('next_tab'), 'Next')
    tab_list <- c(ns('tab1'), ns('tab2'), ns('tab3'), ns('tab4'))

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

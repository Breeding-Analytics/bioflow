geno_example  <- 'www/example/geno.hmp.txt'

#' getDataGeno UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_getDataGeno_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$br(),

    column(width=8,

           # fluidRow(
           #   style = 'padding: 30px;',

             # Source: Upload (web interface to temp local directory) or URL (optional username/password to access)
             # Accept *.gz format (7-Zip how-to reference), average genomic file size after compression is 5%
             selectInput(
               inputId = ns('geno_input'),
               label   = 'Genotypic SNPs Source*:',
               choices = list('HapMap Upload' = 'file', 'HapMap URL' = 'url',
                              'Table Upload' = 'matfile', 'Table URL' = 'matfileurl',
                              'VCF Upload' = 'vcf.file', 'VCF URL' = 'vcf.url'),
               width   = '200px'
             ),
             tags$span(id = ns('geno_file_holder'),
                       fileInput(
                         inputId = ns('geno_file'),
                         label   = NULL,
                         width   = '400px',
                         accept  = c('application/gzip', '.gz', '.txt', '.hmp', '.csv', '.vcf')
                       )
             ),
             textInput(
               inputId = ns('geno_url'),
               label   = NULL,
               value   = '',
               width   = '400px',
               placeholder = 'https://example.com/path/file.gz'
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
           # ),

    ),

    column(width=4,
           shinydashboard::box(width = 12, title = span(icon('youtube'), ' Tutorial'), status = "success", solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE,
                               h4(strong(span("", tags$a(href="https://www.youtube.com/watch?v=gFYGIb9fBLQ&list=PLZ0lafzH_UmclOPifjCntlMzysEB2_2wX&index=2", icon("youtube") , target="_blank"), style="color:darkcyan"))),
           ),

           tags$div(id = ns('geno_table_options'),
                    shinydashboard::box(width = 12, title = span(icon('screwdriver-wrench'), ' Options'), collapsible = TRUE, collapsed = TRUE, status = 'success', solidHeader = TRUE,
                                        shinyWidgets::prettyRadioButtons(ns('geno_sep'), 'Separator Character', selected = ',', inline = TRUE,
                                                                         choices = c('Comma' = ',', 'Semicolon' = ';', 'Tab' = "\t")),

                                        shinyWidgets::prettyRadioButtons(ns('geno_quote'), 'Quoting Character', selected = '"', inline = TRUE,
                                                                         choices = c('None' = '', 'Double Quote' = '"', 'Single Quote' = "'")),

                                        shinyWidgets::prettyRadioButtons(ns('geno_dec'), 'Decimal Points', selected = '.', inline = TRUE,
                                                                         choices = c('Dot' = '.', 'Comma' = ',')),
                    ),
           ),

    ),

    column(width=12,
           shinydashboard::box(width = 12, status = 'success', solidHeader = FALSE,
                               tags$span(id = ns('geno_table_mapping'),
                                         HTML( as.character(div(style="color:cadetblue; font-weight:bold; font-size: 24px;", "Column match/mapping")) ),
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
           ),
    ),

    column(width=12,
           fluidRow(
             style = 'padding-right: 0px; padding-left: 0px;',
             shinydashboard::box(
               title = 'Notes:',
               width = 12,
               solidHeader = TRUE,
               status = 'success',
               tags$ul(
                 tags$li('Accept HapMap, VCF, CSV formats (tab-delimited text file with a header row).
                         The HapMap and VCF files list SNPs in rows and Accessions (individual samples)
                         in columns, and viceversa in the case of the CSV. The first 11 columns of the HapMap
                         describe attributes of the SNP, but only the first 4 columns data are required for processing:
                         rs# (SNP id), alleles (e.g., C/G), chrom (chromosome), and pos (position).'),

                 tags$li(
                   tags$span(
                     'Accept numeric coding ([0,1, and 2] or [-1,0, and 1] for reference/major,
                     heterozygous, and alternative/minor alleles respectively), or the ',
                     tags$a('IUPAC single-letter', target = '_blank', href = 'https://en.wikipedia.org/wiki/Nucleic_acid_notation#IUPAC_notation'),
                     'code (ref. ',
                     tags$a('https://doi.org/10.1093/nar/13.9.3021', target = '_blank', href = 'https://www.ncbi.nlm.nih.gov/pmc/articles/PMC341218/'),
                     '),', 'and double-letter code.'
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
    ),

    column(width=12,
           # Verify file format: Hapmap file format (with reference link)
           # highlight that pos unit should be bp not cM
           # Report summary statistics (#acc, #snps, etc.)
           # to let the user verify before proceeding to the next step
           tableOutput(ns('chrom_summary')),
           # Accessions exist in both phenotypic and genotypic files (will be used to train the model)
           # Accessions have genotypic data but no phenotypic (will predict, add to pheno data file with NA value)
           # Accessions have phenotypic data but no genotypic (filter them out from the pheno data file)
           verbatimTextOutput(ns('geno_summary')),
           DT::DTOutput(ns('preview_geno')),
    ),



  )
}

#' getDataGeno Server Functions
#'
#' @noRd
mod_getDataGeno_server <- function(id, data = NULL, res_auth=NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(
      input$geno_input,
      if(length(input$geno_input) > 0){ # added
        if (input$geno_input %in% c('file', 'vcf.file')) {
          golem::invoke_js('showid', ns('geno_file_holder'))
          golem::invoke_js('hideid', ns('geno_url'))
          golem::invoke_js('hideid', ns('geno_table_mapping'))
          golem::invoke_js('hideid', ns('geno_table_options'))
          updateCheckboxInput(session, 'geno_example', value = FALSE)
        } else if (input$geno_input %in% c('url', 'vcf.url')) {
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
                      options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All'))),
                      caption = htmltools::tags$caption(
                        style = 'color:cadetblue; font-weight:bold; font-size: 24px', #caption-side: bottom; text-align: center;
                        htmltools::em('Data preview.')
                      )
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

          if(!is.null(temp$metadata$geno)){temp$metadata$geno <- temp$metadata$geno[0,]} # make sure if an user uploads a new dataset the metadata starts empty
          if(!is.null(temp$modifications$geno)){temp$modifications$geno <- temp$modifications$geno[0,]} # make sure if an user uploads a new dataset the modifications starts empty
          if(!is.null(temp$status)){
            toRemove <- which(temp$status$module == "qaGeno")
            if(length(toRemove) > 0){temp$status <- temp$status[-toRemove,, drop=FALSE]}
          } # make sure if an user uploads a new dataset the qaGeno starts empty

          tempG <- tempG[which(!duplicated(tempG[,which(colnames(tempG)==input$geno_table_designation)[1]])),]
          rownamestempG <- tempG[,which(colnames(tempG)==input$geno_table_designation)[1] ]
          missingData=c("NN","FAIL","FAILED","Uncallable","Unused","NA","")
          shinybusy::show_modal_spinner('fading-circle', text = 'Converting...')
          for(iMiss in missingData){tempG[which(tempG==iMiss, arr.ind = TRUE)] <- NA}
          ## check if the data is in single letter format
          markersToSample <- sample(which(colnames(tempG)==input$geno_table_firstsnp):which(colnames(tempG)==input$geno_table_lastsnp),  min(c(ncol(tempG),20)) )
          nCharList <- list()
          for(iMark in 1:length(markersToSample)){nCharList[[iMark]] <- na.omit(unique(nchar(tempG[,markersToSample[iMark]])))}
          singleLetter <- which(unique(unlist(nCharList)) == 1)
          if(length(singleLetter) > 0){
            tempG <- cgiarBase::transMarkerSingle( markerDTfile= tempG, badCall=NULL,genoColumn=input$geno_table_designation,firstColum= input$geno_table_firstsnp,lastColumn=input$geno_table_lastsnp,verbose=FALSE)
          }
          tempG <- tempG[,which(colnames(tempG)==input$geno_table_firstsnp):which(colnames(tempG)==input$geno_table_lastsnp)]
          ##
          tempG <- sommer::atcg1234(tempG, maf = -1, imp = FALSE)
          rownames(tempG$M) <- rownamestempG
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
        if (input$geno_input %in% c('file', 'vcf.file')) {
          if (is.null(input$geno_file)) {return(NULL)}else{
            snps_file <- input$geno_file$datapath
          }
        } else if (input$geno_input %in% c('url', 'vcf.url')) {
          if (input$geno_url == '') {return(NULL)}else{
            snps_file <- input$geno_url
          }
        } else {
          return(NULL)
        }

        # library(vcfR); vcf_data <- vcfR::read.vcfR(snps_file); hmp_data <- vcfR::vcfR2hapmap(vcf.data)

        shinybusy::show_modal_spinner('fading-circle', text = 'Loading...')
        if (input$geno_input %in% c('file', 'url')) {
          df <- as.data.frame(data.table::fread(snps_file, sep = '\t', header = TRUE))

        } else if (input$geno_input %in% c('vcf.file', 'vcf.url')) {
          vcf.data  <- vcfR::read.vcfR(snps_file)

          df <- vcfR::vcfR2hapmap(vcf.data)
          df <- df[-1,]

          rm(vcf.data)
        }

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
        return(list(df,snps_file))

      }else{
        return(NULL)
      }

    })

    output$chrom_summary <- renderTable({
      if (!is.null(geno_data())) {
        gd<-geno_data()[[1]]
        data.frame(
          chrom = unique(gd[,'chrom']),
          min_pos = aggregate(pos ~ chrom, data = gd, FUN = min)[,2],
          max_pos = aggregate(pos ~ chrom, data = gd, FUN = max)[,2],
          snps_count = aggregate(pos ~ chrom, data = gd, FUN = length)[,2]
        )
      }
    })

    output$geno_summary <- renderText({
      temp <- data()
      gd <- geno_data()[[1]]
      if (!is.null(gd) & any(temp$metadata$pheno$parameter == 'designation')) {
        designationColumn <- temp$metadata$pheno[which(temp$metadata$pheno$parameter == "designation"),"value"]
        paste(
          "Data Integrity Checks:\n",

          sum(colnames(gd[, -c(1:11)]) %in% unique(temp$data$pheno[, designationColumn ])),
          "Accessions exist in both phenotypic and genotypic files (will be used to train the model)\n",

          sum(!colnames(gd[, -c(1:11)]) %in% unique(temp$data$pheno[, designationColumn ])),
          "Accessions have genotypic data but no phenotypic (will be predicted, add to pheno data file with NA value)\n",

          sum(!unique(temp$data$pheno[, designationColumn ]) %in% colnames(gd[, -c(1:11)])),
          'Accessions have phenotypic data but no genotypic (will not contribute to the training model)'
        )
      }
    })

    observeEvent(
      geno_data(),
      {
        temp <- data()

        if(!is.null(temp$metadata$geno)){temp$metadata$geno <- temp$metadata$geno[0,]} # make sure if an user uploads a new dataset the metadata starts empty
        if(!is.null(temp$modifications$geno)){temp$modifications$geno <- temp$modifications$geno[0,]} # make sure if an user uploads a new dataset the modifications starts empty
        if(!is.null(temp$status)){
          toRemove <- which(temp$status$module == "qaGeno")
          if(length(toRemove) > 0){temp$status <- temp$status[-toRemove,, drop=FALSE]}
        } # make sure if an user uploads a new dataset the qaGeno starts empty

        gd<-geno_data()[[1]]
        temp$data$geno <- t(as.matrix(gd[, -c(1:11)])) - 1
        colnames(temp$data$geno) <- gd$`rs#`

        map <- gd[, c('rs#', 'chrom', 'pos', 'alleles', 'alleles')]
        colnames(map) <- c('marker', 'chr', 'pos', 'refAllele', 'altAllele')
        map$refAllele <- substr(map$refAllele, 1, 1)
        map$altAllele <- substr(map$altAllele, 3, 3)

        temp$metadata$geno <- map
        temp$data$genodir<-geno_data()[[2]]
        data(temp)
      }
    )

    observeEvent(
      input$geno_example,
      if(length(input$geno_example) > 0){ # added
        if (input$geno_example) {
          updateSelectInput(session, 'geno_input', selected = 'url')

          # geno_example_url <-  paste0(session$clientData$url_protocol, '//',
          #                             session$clientData$url_hostname, ':',
          #                             session$clientData$url_port,
          #                             session$clientData$url_pathname,
          #                             geno_example)

          geno_example_url <- 'https://raw.githubusercontent.com/Breeding-Analytics/bioflow/main/inst/app/www/example/geno.hmp.txt'

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

  })
}

## To be copied in the UI
# mod_getDataGeno_ui("getDataGeno_1")

## To be copied in the server
# mod_getDataGeno_server("getDataGeno_1")

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
  Mprov <- t(hapMap); colnames(Mprov) <- SNPInfo[,1]
  hapMapNumeric <- sommer::atcg1234(Mprov, maf = -1, imp = FALSE)

  multiAllelic <- setdiff(SNPInfo$`rs#`,colnames(hapMapNumeric$M))
  if(length(multiAllelic) > 0){
    addMulti <- matrix(NA,nrow=nrow(hapMapNumeric$M),ncol=length(multiAllelic))
    addMultiRef <- matrix(NA,nrow=2,ncol=length(multiAllelic))
    colnames(addMulti) <- colnames(addMultiRef) <- multiAllelic
    hapMapNumeric$M <- cbind(hapMapNumeric$M, addMulti)
    hapMapNumeric$ref.alleles <- cbind(hapMapNumeric$ref.alleles, addMultiRef)
  }
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

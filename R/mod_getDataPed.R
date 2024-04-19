ped_example   <- 'www/example/pedigree.csv'

#' getDataPed UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_getDataPed_ui <- function(id){
  ns <- NS(id)
  tagList(
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
    p(span("**Unknown mothers or fathers should be set as missing data in your file.", style="color:orange")),


    verbatimTextOutput(ns('ped_summary')),
    hr(),
    DT::DTOutput(ns('preview_ped')),
  )
}

#' getDataPed Server Functions
#'
#' @noRd
mod_getDataPed_server <- function(id, data = NULL, res_auth=NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

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
      if (is.null(ped_data())) return(NULL)

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

        renderPrint({
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
          ifelse( (length(designationColumnInPed)+length(designationColumnInPheno) ) == 2, sum(ped[, designationColumnInPed ] %in% unique(tmp$data$pheno[, designationColumnInPheno ]) ) , 0),
          "Accessions exist in both phenotypic and pedigree files\n",
          ifelse( length(designationColumnInPheno) > 0 , sum(!unique(tmp$data$pheno[, designationColumnInPheno ]) %in% ped[, designationColumnInPed ]), 0),
          "Accessions have phenotypic data but no pedigree data\n",
          ifelse(any(tmp$metadata$pedigree$parameter == 'yearOfOrigin') & (length(designationColumnInPed)+length(designationColumnInPheno) ) == 2,  sum(ped[, designationColumnInPed ] %in% unique(tmp$data$pheno[, designationColumnInPheno ])) ,0),
          "Accessions in phenotypic data have no year of origin info\n"
        )
      }
    })

    observeEvent(
      input$ped_example,
      if (input$ped_example) {
        updateSelectInput(session, 'ped_input', selected = 'url')

        # ped_example_url <-  paste0(session$clientData$url_protocol, '//',
        #                            session$clientData$url_hostname, ':',
        #                            session$clientData$url_port,
        #                            session$clientData$url_pathname,
        #                            ped_example)

        ped_example_url <- 'https://raw.githubusercontent.com/Breeding-Analytics/bioflow/main/inst/app/www/example/pedigree.csv'

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

  })
}

## To be copied in the UI
# mod_getDataPed_ui("getDataPed_1")

## To be copied in the server
# mod_getDataPed_server("getDataPed_1")

qtl_example <- 'www/example/qtl.csv'

#' getDataQTL UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_getDataQTL_ui <- function(id){
  ns <- NS(id)
  tagList(
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
  )
}

#' getDataQTL Server Functions
#'
#' @noRd
mod_getDataQTL_server <- function(id, data = NULL, res_auth=NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

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

          # qtl_example_url <-  paste0(session$clientData$url_protocol, '//',
          #                            session$clientData$url_hostname, ':',
          #                            session$clientData$url_port,
          #                            session$clientData$url_pathname,
          #                            qtl_example)

          qtl_example_url <- 'https://drive.google.com/uc?export=download&id=1oNZsRB8KucLqfGA5qARyaHqf1NyEugF-'

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

  })
}

## To be copied in the UI
# mod_getDataQTL_ui("getDataQTL_1")

## To be copied in the server
# mod_getDataQTL_server("getDataQTL_1")

pheno_example <- 'www/example/pheno.csv'

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
          placeholder = 'https://urgi.versailles.inrae.fr/fairdom/'
        ),

        tags$div(
          id = ns('pheno_db'),
          tags$p('Comming soon!', style = 'color:red; font-size:20px;')
        ),

        tags$span(id = ns('pheno_csv_options'),
          shinydashboard::box(title = 'Options', collapsible = TRUE, collapsed = TRUE, status = 'primary', solidHeader = TRUE,
                              radioButtons(ns('pheno_sep'), 'Separator Character', selected = ',', inline = TRUE,
                                           choices = c('Comma' = ',', 'Semicolon' = ';', 'Tab' = "\t")),

                              radioButtons(ns('pheno_quote'), 'Quoting Character', selected = '"', inline = TRUE,
                                           choices = c('None' = '', 'Double Quote' = '"', 'Single Quote' = "'")),

                              radioButtons(ns('pheno_dec'), 'Decimal Points', selected = '.', inline = TRUE,
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

        uiOutput(ns('pheno_map')),
      ),
      tabPanel(
        title = 'Genotypic',
        value = ns('tab2'),
        tags$p('Comming soon!', style = 'color: red; font-size: 20px; margin-top: 25px;'),
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
mod_getData_server <- function(id, map = NULL, df = NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(
      input$pheno_input,
      if (input$pheno_input == 'file') {
        golem::invoke_js('showid', ns('pheno_file_holder'))
        golem::invoke_js('hideid', ns('pheno_url'))
        golem::invoke_js('hideid', ns('pheno_db'))
        golem::invoke_js('showid', ns('pheno_csv_options'))
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
      }
    )

    pheno_data <- reactive({
      if (input$pheno_input == 'file') {
        if (is.null(input$pheno_file)) return(NULL)
        return(read.csv(input$pheno_file$datapath, sep = input$pheno_sep,
                        quote = input$pheno_quote, dec = input$pheno_dec))
      } else if (input$pheno_input == 'url') {
        if (input$pheno_url == '') return(NULL)
        return(read.csv(input$pheno_url, sep = input$pheno_sep,
                        quote = input$pheno_quote, dec = input$pheno_dec))
      } else {

      }
    })

    output$pheno_map <- renderUI({
      header <- colnames(pheno_data())
      pheno_map <- lapply(map, function(x) {
        fluidPage(
          selectInput(
            inputId = ns(paste0('select', x)),
            label = x,
            choices = as.list(c('', header)),
          ),
          renderPrint({
            req(input[[paste0('select', x)]])
            if (input[[paste0('select', x)]] != '') {
              values <- pheno_data()[, input[[paste0('select', x)]]]
              if (class(values) == 'character') {
                head(as.factor(values))
              } else {
                summary(values)
              }
            }
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

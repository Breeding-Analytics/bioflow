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
mod_getData_server <- function(id, map = NULL, data = NULL){
  moduleServer(id , function(input, output, session){
    ns <- session$ns
    df <- reactiveVal()

    observe({
      if (is.null(data)) {
        df(create_getData_object())
      } else {
        df(data)
      }
    })

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
        temp <- df()
        temp$data$pheno <- pheno_data()
        temp$status <- rbind(temp$status, data.frame(module = 'qa', analysisId = as.numeric(Sys.time())))
        #save(temp, file = 'temp.RData')
        df(temp)

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
          renderPrint({
            req(input[[paste0('select', x)]])
            #if (input[[paste0('select', x)]] != '') {
              # values <- pheno_data()[, input[[paste0('select', x)]]]
              # if (class(values) == 'character') {
              #   head(as.factor(values))
              # } else {
              #   summary(values)
              # }

              temp <- df()
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
              df(temp)
            #}
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

    return(df)
  })
}

create_getData_object <- function() {
  obj <- list()

  obj$data <- list()
  obj$metadata <- list()
  obj$modifications <- list()

  obj$data$pheno    <- data.frame()
  obj$data$geno     <- matrix()
  obj$data$weather  <- data.frame()
  obj$data$pedigree <- data.frame(designation = character(),
                                  mother = character(),
                                  father = character())

  obj$metadata$pheno <- data.frame(parameter = character(),
                                   value = character())

  obj$metadata$geno <- data.frame(marker = character(),
                                  chr = character(),
                                  pos = double(),
                                  refAllele = character(),
                                  altAllele = character())

  obj$metadata$pedigree <- data.frame()

  obj$metadata$weather <- data.frame(environment = character(),
                                     parameter = character(),
                                     value = double())

  obj$modifications$pheno <- data.frame(module = character(),
                                        analysisId = character(),
                                        trait = character(),
                                        reason = character(),
                                        row = integer(),
                                        value = double())

  obj$modifications$geno <- data.frame(module = character(),
                                       analysisId = character(),
                                       reason = character(),
                                       row = integer(),
                                       col = integer(),
                                       value = double())

  obj$modifications$pedigree <- data.frame(module = character(),
                                           analysisId = character(),
                                           reason = character(),
                                           row = integer())

  obj$modifications$weather <- data.frame()

  obj$predictions <- data.frame(module = character(),
                                analysisId = character(),
                                pipeline = character(),
                                trait = character(),
                                gid = character(),
                                designation = character(),
                                mother = character(),
                                father = character(),
                                entryType = character(),
                                environment = character(),
                                predictedValue = double(),
                                stdError = double(),
                                reliability = double())

  obj$metrics <- data.frame(module = character(),
                            analysisId = character(),
                            trait = character(),
                            environment = character(),
                            parameter = character(),
                            method = character(),
                            value = double(),
                            stdError = double())

  obj$modeling <- data.frame(module = character(),
                             analysisId = character(),
                             trait = character(),
                             environment = character(),
                             parameter = character(),
                             value = character())

  obj$status <- data.frame(module = character(),
                           analysisId = character())

  return(obj)
}

## To be copied in the UI
# mod_getData_ui("getData_1")

## To be copied in the server
# mod_getData_server("getData_1")

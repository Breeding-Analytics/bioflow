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
    tags$br(),

    navlistPanel( "Steps:", widths = c(2, 10),
                  tabPanel(div("1. Load data" ),
                           column(width=8,

                                  selectInput(
                                    inputId = ns('qtl_input'),
                                    label   = 'Data Source*: ',
                                    choices = list('Upload File' = 'qtlfile', 'Copy URL' = 'qtlfileurl'),
                                    width   = '200px'
                                  ),

                                  tags$span(id = ns('qtl_file_holder'),
                                            fileInput(
                                              inputId = ns('qtl_file'),
                                              label   = NULL,
                                              width   = '400px',
                                              accept  = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
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
                                    shinyWidgets::prettySwitch( inputId = ns('qtl_example'), label = "Load example", status = "success")
                                  },

                           ),


                           column(width=4,

                                  tags$span(id = ns('qtl_table_options'),
                                            shinydashboard::box(width=12, title = span(icon('screwdriver-wrench'), ' Options'), collapsible = TRUE, collapsed = TRUE, status = 'success', solidHeader = FALSE, style = "color: #000000",
                                                                shinyWidgets::prettyRadioButtons(ns('qtl_sep'), 'Separator Character', selected = ',', inline = TRUE,
                                                                                                 choices = c('Comma' = ',', 'Semicolon' = ';', 'Tab' = "\t")),

                                                                shinyWidgets::prettyRadioButtons(ns('qtl_quote'), 'Quoting Character', selected = '"', inline = TRUE,
                                                                                                 choices = c('None' = '', 'Double Quote' = '"', 'Single Quote' = "'")),

                                                                shinyWidgets::prettyRadioButtons(ns('qtl_dec'), 'Decimal Points', selected = '.', inline = TRUE,
                                                                                                 choices = c('Dot' = '.', 'Comma' = ',')),
                                            ),
                                  ),

                           ),
                           # column(width=12,
                           shinydashboard::box(width = 12,  status = 'success', solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                               title = "Preview of uploaded data (click on the '+' symbol on the right to view)",
                                               DT::DTOutput(ns('preview_qtl')),
                           ),
                           # ),
                  ),
                  tabPanel(div("2. Match columns" ),
                           # column(width=12,
                           shinydashboard::box(width = 12, status = 'success', solidHeader = FALSE,
                                               tags$span(id = ns('qtl_table_mapping_title'),
                                                         HTML( as.character(div(style="color:cadetblue; font-weight:bold; font-size: 24px;", "Column match/mapping")) ),
                                               ),
                                               uiOutput(ns('qtl_map')),
                                               column(width=4),
                                               column(width=8,
                                                      shinyWidgets::prettySwitch( inputId = ns('qtl_all'), label = "Select all columns", status = "success")
                                               ),
                           ),
                           # ),
                           # column(width=12,
                           shinydashboard::box(width = 12,  status = 'success', solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                               title = "Preview of uploaded data (click on the '+' symbol on the right to view)",
                                               DT::DTOutput(ns('preview_qtl2')),

                           ),
                           # ),
                  ),
                  tabPanel(div("3. Check status" ),
                           uiOutput(ns("warningMessage")),
                  ),
    ),


  )
}

#' getDataQTL Server Functions
#'
#' @noRd
mod_getDataQTL_server <- function(id, data = NULL, res_auth=NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # warning message
    output$warningMessage <- renderUI({
      if (is.null(data()$data$qtl)) {
        return(HTML(as.character(div(style="color: red; font-size: 20px;",
                                     "Please retrieve or load your QTL Profile data using the 'Data Retrieval' tab. "))))
      }

      md <- data()$metadata$qtl
      # required fields for completeness
      req_params <- c("designation", "qtl")

      missing_cols <- if (is.null(md)) {
        req_params
      } else {
        setdiff(req_params, md$parameter)
      }

      ok <- !is.null(md) &&
        all(req_params %in% md$parameter) &&
        all(md$value[match(req_params, md$parameter)] != "")

      if (ok) {
        HTML(as.character(div(style="color: green; font-size: 20px;",
                              "Data is complete, you can proceed to use the other modules.")))
      } else {
        HTML(as.character(div(style="color: red; font-size: 20px;", paste("You are still missing columns: ", paste( missing_cols, collapse = ", ") ) )))
      }
    })


    observeEvent(
      input$qtl_input,
      if(length(input$qtl_input) > 0){ # added
        golem::invoke_js('hideid', ns('qtl_table_mapping_title'))
        if (input$qtl_input == 'qtlfile') {
          golem::invoke_js('showid', ns('qtl_file_holder'))
          golem::invoke_js('hideid', ns('qtl_url'))
          golem::invoke_js('showid', ns('qtl_table_options'))
          updateCheckboxInput(session, 'qtl_example', value = FALSE)
        } else if (input$qtl_input == 'qtlfileurl') {
          golem::invoke_js('hideid', ns('qtl_file_holder'))
          golem::invoke_js('showid', ns('qtl_url'))
          golem::invoke_js('showid', ns('qtl_table_options'))
        }
      }
    )

    observeEvent(c(data()), { # update trait
      req(data())
      dtMta <- data()$data$qtl
      if(!is.null(dtMta)){
        golem::invoke_js('showid', ns('qtl_table_mapping_title'))
      }
    })

    qtl_data_table <- reactive({
      if(length(input$qtl_input) > 0){ # added
        if (input$qtl_input == 'qtlfile') {
          if (is.null(input$qtl_file)){
            return(NULL)
          } else{
            data <- as.data.frame(data.table::fread(input$qtl_file$datapath, sep = input$qtl_sep,
                                                    quote = input$qtl_quote, dec = input$qtl_dec, header = TRUE))
          }
        } else if (input$qtl_input == 'qtlfileurl') {
          if (input$qtl_url == ''){
            return(NULL)
          } else{
            data <- as.data.frame(data.table::fread(input$qtl_url, sep = input$qtl_sep,
                                                    quote = input$qtl_quote, dec = input$qtl_dec, header = TRUE))
          }
        } else {
          return(NULL)
        }

        return(data)
      }
    })

    observeEvent(
      qtl_data_table(),
      {
        temp <- data()

        # should check file format here!
        temp$data$qtl <- qtl_data_table()

        if(!is.null(temp$metadata$qtl)){temp$metadata$qtl <- temp$metadata$qtl[0,]} # make sure if an user uploads a new dataset the metadata starts empty
        if(!is.null(temp$modifications$qtl)){temp$modifications$qtl <- temp$modifications$qtl[0,]} # make sure if an user uploads a new dataset the modifications starts empty
        if(!is.null(temp$status)){
          toRemove <- which(temp$status$module == "qaQtl")
          if(length(toRemove) > 0){temp$status <- temp$status[-toRemove,, drop=FALSE]}
        } # make sure if an user uploads a new dataset the qaQtl starts empty

        output$preview_qtl <- output$preview_qtl2  <- DT::renderDT({
          req(qtl_data_table())

          DT::datatable(temp$data$qtl[1:min(c( round(33000/ncol(temp$data$qtl)) , nrow(temp$data$qtl))),],
                        extensions = 'Buttons',
                        options = list(dom = 'Blfrtip',
                                       scrollX = TRUE,
                                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                       lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All'))),
                        caption = htmltools::tags$caption(
                          style = 'color:cadetblue; font-weight:bold; font-size: 18px', #caption-side: bottom; text-align: center;
                          htmltools::em('Data preview.')
                        )
          )

        }, server = FALSE)

        data(temp)
      }
    )

    output$qtl_map <- renderUI({
      if (is.null(qtl_data_table())){
        return(NULL)
      } else{
        header <- colnames(qtl_data_table())
        map <- c("designation", "qtl")
        qtl_map <- lapply(map, function(x) {
          column(width = if (x == "qtl") 8 else 4,
                 selectInput(
                   inputId  = ns(paste0('select', x)),
                   label    = HTML(as.character(p(x, span('(*required)',style="color:red")))),
                   multiple = ifelse(x == 'qtl', TRUE, FALSE),
                   choices  = as.list(c('', header )),
                   selected = ifelse(length(grep(paste0("^", x, "$"),header, ignore.case = TRUE)) > 0, header[grep(paste0("^", x, "$"),header, ignore.case = TRUE)[1]], '')
                 ),

                 renderPrint({
                   temp <- data()
                   if (x == 'qtl') {
                     temp$metadata$qtl <- temp$metadata$qtl[temp$metadata$qtl$parameter != 'qtl',]
                     for (i in input[[paste0('select', x)]]) {
                       temp$metadata$qtl <- rbind(temp$metadata$qtl, data.frame(parameter = 'qtl', value = i))
                     }
                   } else {
                     if (x %in% temp$metadata$qtl$parameter & input[[paste0('select', x)]] != '') {
                       temp$metadata$qtl[temp$metadata$qtl$parameter == x, 'value'] <- input[[paste0('select', x)]]
                       temp$data$qtl[,input[[paste0('select', x)]]] <- stringi::stri_trans_general(temp$data$qtl[,input[[paste0('select', x)]]], "Latin-ASCII")
                     } else { # if is not in the metadata
                       temp$metadata$qtl <- rbind(temp$metadata$qtl, data.frame(parameter = x, value = input[[paste0('select', x)]]))
                     }
                     if(input[[paste0('select', x)]] == ''){
                       temp$metadata$qtl <- temp$metadata$qtl[-which(temp$metadata$qtl$parameter == x), ]
                     }
                   }
                   data(temp)
                 }),
          )
        })
        fluidRow(do.call(tagList, qtl_map))
      }
    })


    ## data example loading
    observeEvent(
      input$qtl_example,
      if(length(input$qtl_example) > 0){
        if (input$qtl_example) {
          shinyWidgets::ask_confirmation(
            inputId = ns("myconfirmation"),
            text = "Are you sure you want to load the example QTL data? This will delete any QTL data currently in the environment.",
            title = "Data replacement warning"
          )
        }
      }
    )
    observeEvent(input$myconfirmation, {
      if (isTRUE(input$myconfirmation)) {
        if (input$qtl_example) {
          updateSelectInput(session, 'qtl_input', selected = 'qtlfileurl')

          qtl_example_url <- 'https://raw.githubusercontent.com/Breeding-Analytics/bioflow/main/inst/app/www/example/qtl.csv'

          updateTextInput(session, 'qtl_url', value = qtl_example_url)

          golem::invoke_js('hideid', ns('qtl_file_holder'))
          golem::invoke_js('showid', ns('qtl_url'))
        } else {
          updateSelectInput(session, 'qtl_input', selected = 'qtlfile')
          updateTextInput(session, 'qtl_url', value = '')

          golem::invoke_js('showid', ns('qtl_file_holder'))
          golem::invoke_js('hideid', ns('qtl_url'))
        }
      }else{
        shinyWidgets::updatePrettySwitch(session, "qtl_example", value = FALSE)
      }
    }, ignoreNULL = TRUE)

    ############
    # check box to load all qtls

    observeEvent(
      input$qtl_all,{
        if (input$qtl_all) {
          header <- colnames(qtl_data_table())
          updateSelectInput(session, 'selectqtl', selected = header)
        }else{
          shinyWidgets::updatePrettySwitch(session, "qtl_all", value = FALSE)
          updateSelectInput(session, 'selectqtl', selected = '')
        }
      })

  })
}

## To be copied in the UI
# mod_getDataQTL_ui("getDataQTL_1")

## To be copied in the server
# mod_getDataQTL_server("getDataQTL_1")

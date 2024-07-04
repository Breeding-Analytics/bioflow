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

    navlistPanel( widths = c(2, 10),
                  tabPanel(div("1. Load data" ),
                           column(width=8,
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
                                    shinyWidgets::prettySwitch( inputId = ns('qtl_example'), label = "Load example", status = "success")
                                  },
                           ),

                           column(width=4,
                                  tags$div(id = ns('qtl_table_options'),
                                           shinydashboard::box(width=12, title = span(icon('screwdriver-wrench'), ' Options'), collapsible = TRUE, collapsed = TRUE, status = 'success', solidHeader = TRUE,
                                                               shinyWidgets::prettyRadioButtons(ns('qtl_sep'), 'Separator Character', selected = ',', inline = TRUE,
                                                                                                choices = c('Comma' = ',', 'Semicolon' = ';', 'Tab' = "\t")),

                                                               shinyWidgets::prettyRadioButtons(ns('qtl_quote'), 'Quoting Character', selected = '"', inline = TRUE,
                                                                                                choices = c('None' = '', 'Double Quote' = '"', 'Single Quote' = "'")),

                                                               shinyWidgets::prettyRadioButtons(ns('qtl_dec'), 'Decimal Points', selected = '.', inline = TRUE,
                                                                                                choices = c('Dot' = '.', 'Comma' = ',')),
                                           ),
                                  ),
                           ),
                           column(width=12,
                                  shinydashboard::box(width = 12,  status = 'success', solidHeader = FALSE,
                                                      DT::DTOutput(ns('preview_qtl')),
                                  ),
                           ),
                  ),
                  tabPanel(div("2. Match columns" ),
                           column(width=12,
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
                           ),
                           column(width=12,
                                  shinydashboard::box(width = 12,  status = 'success', solidHeader = FALSE,
                                                      DT::DTOutput(ns('preview_qtl2')),
                                  ),
                           ),
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
    output$warningMessage <- renderUI(
      if(!is.null(data()$data$qtl)){
        if(!is.null(data()$metadata$qtl)){
          if(length(setdiff(data()$metadata$qtl$value,"")) > 0){
            HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, you can proceed to use the other modules.")) )
          }else{
            HTML( as.character(div(style="color: red; font-size: 20px;", "Please map/match your columns.")) )
          }
        }else{
          HTML( as.character(div(style="color: red; font-size: 20px;", "Please map/match your columns.")) )
        }
      }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your data and match the needed columns. ")) )}

    )

    observeEvent(
      input$qtl_input,
      if(length(input$qtl_input) > 0){ # added
        golem::invoke_js('hideid', ns('qtl_table_mapping_title'))
        if (input$qtl_input == 'qtlfile' ){
          golem::invoke_js('showid', ns('qtl_file_holder'))
          # golem::invoke_js('showid', ns('qtl_table_mapping'))
          golem::invoke_js('showid', ns('qtl_table_options'))
          golem::invoke_js('hideid', ns('qtl_url'))
          updateCheckboxInput(session, 'qtl_example', value = FALSE)
        } else if (input$qtl_input == 'qtlfileurl' ){
          golem::invoke_js('hideid', ns('qtl_file_holder'))
          # golem::invoke_js('showid', ns('qtl_table_mapping'))
          golem::invoke_js('showid', ns('qtl_table_options'))
          golem::invoke_js('showid', ns('qtl_url'))
        }
      }
    )

    observeEvent(c(qtl_data_table()), { # update trait
      req(qtl_data_table())
      dtMta <- qtl_data_table()
      if(!is.null(dtMta)){
        golem::invoke_js('showid', ns('qtl_table_mapping_title'))
      }
    })

    output$qtl_map <- renderUI({
      if (is.null(qtl_data_table())) return(NULL)

      header <- colnames(qtl_data_table())
      fluidRow(
        column(4,
               selectizeInput(
                 inputId = ns('qtl_table_designation'),
                 label   = 'Designation column: ',
                 choices = as.list(c('', header)),
               ),    ),
        column(8,
               selectizeInput(
                 inputId = ns('qtl_table_markers'),
                 label   = 'QTL columns: ', multiple=TRUE,
                 choices = as.list(c('', header)),
               ),   ),

        renderPrint({
          temp <- data()

          if(length(input$qtl_table_designation) > 0){
            if ("designation" %in% temp$metadata$qtl$parameter) {
              temp$metadata$qtl[temp$metadata$qtl$parameter == "designation", 'value'] <- input$qtl_table_designation
            } else {
              temp$metadata$qtl <- rbind(temp$metadata$qtl, data.frame(parameter = "designation", value = input$qtl_table_designation ))
            }
          }

          if(length(input$qtl_table_markers) > 0){
            if ("qtl_table_markers" %in% temp$metadata$qtl$parameter) {
              temp$metadata$qtl[temp$metadata$qtl$parameter == "qtl", 'value'] <- input$qtl_table_markers
            } else {
              temp$metadata$qtl <- unique(rbind(temp$metadata$qtl, data.frame(parameter = "qtl", value = input$qtl_table_markers )))
            }
          }
          data(temp)
        }),
      )
    })

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
    observeEvent(c(qtl_data_table()), { # display preview of qtl data
      req(qtl_data_table())
      provqtl <- qtl_data_table()
      output$preview_qtl <- output$preview_qtl2 <- DT::renderDT({
        req(qtl_data_table())
        DT::datatable(qtl_data_table()[,1:min(c(50,ncol(qtl_data_table())))],
                      extensions = 'Buttons',
                      options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All'))),
                      caption = htmltools::tags$caption(
                        style = 'color:cadetblue; font-weight:bold; font-size: 18px', #caption-side: bottom; text-align: center;
                        htmltools::em('Data preview.')
                      )
        )
      })
    })
    observeEvent( # save data for qtl
      c(qtl_data_table(), input$qtl_table_markers, input$qtl_table_designation),
      {
        req(qtl_data_table())
        req(input$qtl_table_markers)
        req(input$qtl_table_designation)
        if(!is.null(input$qtl_table_markers) & !is.null(input$qtl_table_designation) ){ # if both designation and qtls are selected
          temp <- data()

          if(!is.null(temp$metadata$qtl)){temp$metadata$qtl <- temp$metadata$qtl[0,]} # make sure if an user uploads a new dataset the metadata starts empty
          if(!is.null(temp$modifications$qtl)){temp$modifications$qtl <- temp$modifications$qtl[0,]} # make sure if an user uploads a new dataset the modifications starts empty
          if(!is.null(temp$status)){ # if the user is uploading a new qtl data remove the qa analysis on qtls
            toRemove <- which(temp$status$module == "qaQtl")
            if(length(toRemove) > 0){temp$status <- temp$status[-toRemove,, drop=FALSE]}
          } # make sure if an user uploads a new dataset the qaQtl starts empty

          temp$data$qtl <- qtl_data_table()[,unique(c(input$qtl_table_designation,input$qtl_table_markers))]
          data(temp)
        }else{return(NULL)}
      }
    )

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
        if(length(input$qtl_example) > 0){ # if user clicked on qtl example
          if (input$qtl_example) {
            updateSelectInput(session, 'qtl_input', selected = 'qtlfileurl')

            # qtl_example_url <-  paste0(session$clientData$url_protocol, '//',
            #                            session$clientData$url_hostname, ':',
            #                            session$clientData$url_port,
            #                            session$clientData$url_pathname,
            #                            qtl_example)

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
        }
      }else{
        shinyWidgets::updatePrettySwitch(session, "qtl_example", value = FALSE)
      }
    }, ignoreNULL = TRUE)


    ############
    # check box to load all qtls

    observeEvent(
      input$qtl_all,
      if(length(input$qtl_all) > 0){
        if (input$qtl_all) {
          header <- colnames(qtl_data_table())
          updateSelectizeInput(session, 'qtl_table_markers', selected = header)
        }else{
          shinyWidgets::updatePrettySwitch(session, "qtl_all", value = FALSE)
          updateSelectizeInput(session, 'qtl_table_markers', selected = '')
        }
      }
    )

  })
}

## To be copied in the UI
# mod_getDataQTL_ui("getDataQTL_1")

## To be copied in the server
# mod_getDataQTL_server("getDataQTL_1")

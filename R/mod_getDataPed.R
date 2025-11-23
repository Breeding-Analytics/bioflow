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

    navlistPanel( "Steps:", widths = c(2, 10),
                  tabPanel(div("1. Load data" ),
                           column(width=8,

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

                                  if (!is.null(ped_example)) {
                                    # checkboxInput(
                                    #   inputId = ns('ped_example'),
                                    #   label = span('Load example ',
                                    #                a('pedigree data', target = '_blank',
                                    #                  href = ped_example)),
                                    #   value = FALSE
                                    # )
                                    shinyWidgets::prettySwitch( inputId = ns('ped_example'), label = "Load example", status = "success")
                                  },

                           ),


                           column(width=4,

                                  tags$span(id = ns('ped_csv_options'),
                                            shinydashboard::box(width=12, title = span(icon('screwdriver-wrench'), ' Options'), collapsible = TRUE, collapsed = TRUE, status = 'success', solidHeader = FALSE, style = "color: #000000",
                                                                shinyWidgets::prettyRadioButtons(ns('ped_sep'), 'Separator Character', selected = ',', inline = TRUE,
                                                                                                 choices = c('Comma' = ',', 'Semicolon' = ';', 'Tab' = "\t")),

                                                                shinyWidgets::prettyRadioButtons(ns('ped_quote'), 'Quoting Character', selected = '"', inline = TRUE,
                                                                                                 choices = c('None' = '', 'Double Quote' = '"', 'Single Quote' = "'")),

                                                                shinyWidgets::prettyRadioButtons(ns('ped_dec'), 'Decimal Points', selected = '.', inline = TRUE,
                                                                                                 choices = c('Dot' = '.', 'Comma' = ',')),
                                            ),
                                  ),

                           ),
                           # column(width=12,
                           shinydashboard::box(width = 12,  status = 'success', solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                               title = "Preview of uploaded data (click on the '+' symbol on the right to view)",
                                               column(width=8,
                                                      DT::DTOutput(ns('preview_ped')),
                                               ),
                                               column(width=4,
                                                      verbatimTextOutput(ns('ped_summary')),
                                               )

                           ),
                           # ),
                  ),
                  tabPanel(div("2. Match columns" ),
                           # column(width=12,
                           shinydashboard::box(width = 12, status = 'success', solidHeader = FALSE,
                                               tags$span(id = ns('ped_table_mapping'),
                                                         HTML( as.character(div(style="color:cadetblue; font-weight:bold; font-size: 24px;", "Column match/mapping")) ),
                                                         p(span("**Unknown mothers or fathers should be set as missing data in your input file.", style="color:orange")),
                                               ),
                                               uiOutput(ns('ped_map')),
                           ),
                           # ),
                           # column(width=12,
                           shinydashboard::box(width = 12,  status = 'success', solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                               title = "Preview of uploaded data (click on the '+' symbol on the right to view)",
                                               column(width=8,
                                                      DT::DTOutput(ns('preview_ped2')),
                                               ),
                                               column(width=4,
                                                      verbatimTextOutput(ns('ped_summary2')),
                                               )

                           ),
                           # ),
                  ),
                  tabPanel(div("3. Check status" ),
                           uiOutput(ns("warningMessage")),
                  ),
    ),


  )
}

#' getDataPed Server Functions
#'
#' @noRd
mod_getDataPed_server <- function(id, data = NULL, res_auth=NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # warning message
    output$warningMessage <- renderUI({
      if (is.null(data())) {
        return(HTML(as.character(div(style="color: red; font-size: 20px;",
                                     "Please retrieve or load your data using the 'Data' tab."))))
      }

      if (is.null(data()$data$pedigree)) {
        return(HTML(as.character(div(style="color: red; font-size: 20px;",
                                     "Please retrieve or load your data using the 'Data' tab. "))))
      }

      md <- data()$metadata$pedigree
      # required fields for completeness
      req_params <- c("designation", "mother", "father")

      ok <- !is.null(md) &&
        all(req_params %in% md$parameter) &&
        all(md$value[match(req_params, md$parameter)] != "")

      if (ok) {
        HTML(as.character(div(style="color: green; font-size: 20px;",
                              "Data is complete, you can proceed to use the other modules.")))
      } else {
        HTML(as.character(div(style="color: red; font-size: 20px;",
                              "Please map/match your columns (required: designation, mother, father).")))
      }
    })


    observeEvent(
      input$ped_input,
      if(length(input$ped_input) > 0){ # added
        golem::invoke_js('hideid', ns('ped_table_mapping'))
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

    observeEvent(c(data()), { # update trait
      req(data())
      dtMta <- data()$data$pedigree
      if(!is.null(dtMta)){
        golem::invoke_js('showid', ns('ped_table_mapping'))
      }
    })

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

        if(!is.null(temp$metadata$pedigree)){temp$metadata$pedigree <- temp$metadata$pedigree[0,]} # make sure if an user uploads a new dataset the metadata starts empty
        if(!is.null(temp$modifications$pedigree)){temp$modifications$pedigree <- temp$modifications$pedigree[0,]} # make sure if an user uploads a new dataset the modifications starts empty
        if(!is.null(temp$status)){
          toRemove <- which(temp$status$module == "qaPed")
          if(length(toRemove) > 0){temp$status <- temp$status[-toRemove,, drop=FALSE]}
        } # make sure if an user uploads a new dataset the qaPed starts empty

        output$preview_ped <- output$preview_ped2  <- DT::renderDT({
          req(ped_data())

          DT::datatable(temp$data$pedigree[1:min(c( round(33000/ncol(temp$data$pedigree)) , nrow(temp$data$pedigree))),],
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
        column(3, selectInput(
          inputId  = ns('ped_sample_id'),
          label    = 'sample_id (optional)',
          choices  = as.list(c('', header))
        )),
        column(3, selectInput(
          inputId  = ns('ped_crossType'),
          label    = tags$span(
            "crossType (optional)",
            tags$i(
              class = "glyphicon glyphicon-info-sign",
              style = "color:#000000",
              title = "Mandatory to run F1 qa/qc module. Select column that indicates whether an individual is an F1 or a parent"
            )
          ),
          choices  = as.list(c('', header))
        )),
        column(3, selectInput(
          inputId  = ns('ped_otherF1'),
          label    = tags$span(
            "F1 qa/qc other metadata (optional)",
            tags$i(
              class = "glyphicon glyphicon-info-sign",
              style = "color:#000000",
              title = "Allow inclusion of plant number or other metadata relevant for F1 qa/qc"
            )
          ),
          choices  = as.list(c('', header))
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

          if ("sample_id" %in% temp$metadata$pedigree$parameter) {
            temp$metadata$pedigree[temp$metadata$pedigree$parameter == "sample_id", 'value'] <- input$ped_sample_id
          } else {
            temp$metadata$pedigree <- rbind(temp$metadata$pedigree, data.frame(parameter = "sample_id", value = input$ped_sample_id))
          }

          if ("crossType" %in% temp$metadata$pedigree$parameter) {
            temp$metadata$pedigree[temp$metadata$pedigree$parameter == "crossType", 'value'] <- input$ped_crossType
          } else {
            temp$metadata$pedigree <- rbind(temp$metadata$pedigree, data.frame(parameter = "crossType", value = input$ped_crossType))
          }

          if ("other" %in% temp$metadata$pedigree$parameter) {
            temp$metadata$pedigree[temp$metadata$pedigree$parameter == "other", 'value'] <- input$ped_otherF1
          } else {
            temp$metadata$pedigree <- rbind(temp$metadata$pedigree, data.frame(parameter = "other", value = input$ped_otherF1))
          }

          data(temp)
        }),
      )
    })


    ## data example loading
    observeEvent(
      input$ped_example,
      if(length(input$ped_example) > 0){
        if (input$ped_example) {
          shinyWidgets::ask_confirmation(
            inputId = ns("myconfirmation"),
            text = "Are you sure you want to load the example pedigree? This will delete any pedigree data currently in the environment.",
            title = "Data replacement warning"
          )
        }
      }
    )
    observeEvent(input$myconfirmation, {
      if (isTRUE(input$myconfirmation)) {
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
      }else{
        shinyWidgets::updatePrettySwitch(session, "ped_example", value = FALSE)
      }
    }, ignoreNULL = TRUE)

  })
}

## To be copied in the UI
# mod_getDataPed_ui("getDataPed_1")

## To be copied in the server
# mod_getDataPed_server("getDataPed_1")

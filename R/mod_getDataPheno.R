pheno_example <- 'www/example/pheno.csv'

#' getDataPheno UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_getDataPheno_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(HTML("
          .selectize-input.disabled {
            background-color: white !important; /* Change background color */
            color: black !important; /* Change text color */
            opacity: 1 !important; /* Prevent default opacity reduction */
          }
        "))
    ),
    tags$br(),
    # tags$p("The time is ", textOutput(ns("current_time"))),

    navlistPanel( "Steps:",widths = c(2, 10),
                  tabPanel(div("1. Load data" ),

                           column(width=8,

                                  selectInput(
                                    inputId = ns('pheno_input'),
                                    label   = 'Data Source*: ',
                                    choices = list('Upload File' = 'file', 'Copy URL' = 'url', 'BrAPI' = 'brapi'),
                                    width   = '200px'
                                  ),

                                  tags$span(id = ns('pheno_file_holder'),
                                            fileInput(
                                              inputId = ns('pheno_file'),
                                              multiple = TRUE,
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
                                                                           placeholder = 'https://cb-wee.ebsproject.org'
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
                                                                         tags$span(id = ns('pheno_db_token_holder'),
                                                                                   textInput(
                                                                                     inputId = ns('pheno_token_user'),
                                                                                     label = 'Token:'
                                                                                   )
                                                                         ),
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
                                                                                   selectizeInput(
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
                                                                         uiOutput(ns('pheno_db_folder')),
                                                                         uiOutput(ns('pheno_db_trial')),
                                                                         actionButton(
                                                                           inputId = ns('pheno_db_load'),
                                                                           label = 'Load',
                                                                           width = '100%'
                                                                         ),
                                                     )
                                           ),
                                  ),
                                  if (!is.null(pheno_example)) {

                                    shinyWidgets::prettySwitch( inputId = ns('pheno_example'), label = "Load example", status = "success")

                                  },


                           ),

                           column(width=4,
                                  shinydashboard::box(width = 12, title = span(icon('youtube'), ' Tutorial'), status = "success", collapsible = TRUE, collapsed = TRUE, solidHeader=FALSE, style = "color: #000000",
                                                      h4(strong(span("", tags$a(href="https://www.youtube.com/watch?v=gFYGIb9fBLQ&list=PLZ0lafzH_UmclOPifjCntlMzysEB2_2wX&index=2", icon("youtube") , target="_blank"), style="color:darkcyan"))),
                                  ),
                                  tags$div(id = ns('pheno_csv_options'),
                                           shinydashboard::box(width = 12, title = span(icon('screwdriver-wrench'), ' Options'), collapsible = TRUE, collapsed = TRUE, status = 'success', solidHeader = FALSE, style = "color: #000000",
                                                               shinyWidgets::prettyRadioButtons(ns('pheno_sep'), 'Separator Character', selected = ',', inline = TRUE,
                                                                                                choices = c('Comma' = ',', 'Semicolon' = ';', 'Tab' = "\t")),

                                                               shinyWidgets::prettyRadioButtons(ns('pheno_quote'), 'Quoting Character', selected = '"', inline = TRUE,
                                                                                                choices = c('None' = '', 'Double Quote' = '"', 'Single Quote' = "'")),

                                                               shinyWidgets::prettyRadioButtons(ns('pheno_dec'), 'Decimal Points', selected = '.', inline = TRUE,
                                                                                                choices = c('Dot' = '.', 'Comma' = ',')),
                                           ),
                                  ),
                                  shinydashboard::box(width = 12, title = span(icon('bookmark'), ' Dictionary of terms'), status = "success", collapsible = TRUE, collapsed = TRUE, solidHeader=FALSE, style = "color: #000000",
                                                      p(strong("pipeline.-"),"The name of the column containing the labels describing the breeding effort to satisfy a market segment (e.g., Direct seeded late maturity irrigated)."),
                                                      p(strong("stage.-"),"The name of the column containing the labels describing the stages of phenotypic evaluation (e.g., Stage 1, PYT, etc.)."),
                                                      p(strong("year.-"),"The name of the column containing the labels listing the year when a trial was carried out (e.g., 2024)."),
                                                      p(strong("season-"),"The name of the column containing the labels listing the season when a trial was carried out (e.g., dry-season, wet-season, etc.)."),
                                                      p(strong("timepoint-"),"The name of the column containing the labels listing the timepoints from time series."),
                                                      p(strong("country.-"),"The name of the column containing the labels listing the countries where a trial was carried out (e.g., Nigeria, Mexico, etc.)."),
                                                      p(strong("location-"),"The name of the column containing the labels listing the locations within a country when a trial was carried out (e.g., Obregon, Toluca, etc.)."),
                                                      p(strong("trial.-"),"The name of the column containing the labels listing the trial of experiment randomized."),
                                                      p(strong("study.-"),"The name of the column containing the labels listing the unique occurrences of a trial nested in a year, country, location."),
                                                      p(strong("management-"),"The name of the column containing the labels listing the unique occurrences of a management (e.g., drought, irrigated, etc.) nested in a trial, nested in a year, country, location."),
                                                      p(strong("rep.-"),"The name of the column containing the labels of the replicates or big blocks within an study (year-season-country-location-trial concatenation)."),
                                                      p(strong("iBlock.-"),"The name of the column containing the labels of the incomplete blocks within an study."),
                                                      p(strong("row.-"),"The name of the column containing the labels of the row coordinates for each record within an study."),
                                                      p(strong("column.-"),"The name of the column containing the labels of the column coordinates for each record within an study."),
                                                      p(strong("designation.-"),"The name of the column containing the labels of the individuals tested in the environments (e.g., Borlaug123, IRRI-154, Campeon, etc. )."),
                                                      p(strong("gid.-"),"The name of the column containing the labels with the unique numerical identifier used within the database management system."),
                                                      p(strong("entryType.-"),"The name of the column containing the labels of the genotype category (check, tester, entry, etc.)."),
                                                      p(strong("trait.-"),"The name of the column(s) containing the numerical traits to be analyzed."),
                                  ),
                           ),

                           # column(width=12,
                           shinydashboard::box(width = 12,  status = 'success', solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                               id = ns('preview_data_holder'),
                                               title = "Preview of uploaded data (click on the '+' symbol on the right to view)",
                                               hr(),
                                               DT::DTOutput(ns('preview_pheno')),
                           ),
                           # ),


                  ),
                  tabPanel(div("2. Match columns",),

                           # column(width=12,
                           shinydashboard::box(width = 12, status = 'success', solidHeader = FALSE,
                                               hr(),
                                               # tags$div(id = ns('mapping_title_holder'),
                                               #          HTML( as.character(div(style="color:cadetblue; font-weight:bold; font-size: 18px;", "2. Match/map your columns")) ),
                                               # ),
                                               uiOutput(ns('pheno_map')),
                                               uiOutput(ns('brapi_trait_map')),
                                               uiOutput(ns('pheno_map2'))
                           ),
                           # ),
                           # column(width=12,
                           shinydashboard::box(width = 12,  status = 'success', solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                               title = "Preview of uploaded data (click on the '+' symbol on the right to view)",
                                               hr(),
                                               DT::DTOutput(ns('preview_pheno2')),
                           ),
                           # ),

                  ),
                  tabPanel(div("3. Define environment" ),

                           column(width=12,
                                  tags$div(id = ns('concat_environment_holder'),
                                           selectInput(ns("featuresEnvironment"),
                                                       label = "Columns forming environment:",
                                                       choices = NULL, multiple = TRUE),
                                           actionButton(ns("concatenateEnv"),
                                                        tags$span(
                                                          div(p(strong('Compute Environment Column', span('(*required)',style="color:red"))), style="display: inline-block; line-height:30px;"),
                                                          tags$i(
                                                            class = "glyphicon glyphicon-info-sign",
                                                            style = "color:#000000",
                                                            title = "Remove non-alphanumeric characters from the selected column values, then concatenate the results with underscore as the separator, and add 'env' as a prefix"
                                                          )
                                                        ),
                                                        # icon = icon("play-circle"),
                                                        style = "height: 45px"),
                                           hr(),
                                           textOutput(ns("outConcatenateEnv")),
                                  ),
                           ),
                           # column(width=12,
                           shinydashboard::box(width = 12,  status = 'success', solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                               title = "Preview of uploaded data (click on the '+' symbol on the right to view)",
                                               hr(),
                                               DT::DTOutput(ns('preview_pheno3')),
                           ),
                           # ),

                  ),
                  tabPanel(div("4. Check status" ),
                           uiOutput(ns("warningMessage")),
                  ),
    )


  )
}

#' getDataPheno Server Functions
#'
#' @noRd
mod_getDataPheno_server <- function(id, map = NULL, data = NULL, res_auth=NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # output$current_time <- renderText({
    #   invalidateLater(1000)
    #   format(Sys.time(), "%H:%M:%S %p")
    # })

    # warning message
    output$warningMessage <- renderUI(
      if(is.null(data())){
        HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your phenotypic data using the 'Data Retrieval' tab.")) )
      }else{ # data is there
        mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
        if(mappedColumns == 3){ HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, you can proceed to use other modules.")) )
        }else{HTML( as.character(div(style="color: red; font-size: 20px;", paste("You are still missing columns: ", paste( setdiff(c("environment","designation","trait"), data()$metadata$pheno$parameter), collapse = ", ") ) )) )
        }
      }
    )

    golem::invoke_js('hideid', ns('pheno_brapi_options'))
    golem::invoke_js('hideid', ns('pheno_url'))

    observeEvent(
      input$pheno_input,
      if(length(input$pheno_input) > 0){ # added
        golem::invoke_js('hideid', ns('concat_environment_holder'))
        # golem::invoke_js('hideid', ns('mapping_title_holder'))

        if (input$pheno_input == 'file') {
          golem::invoke_js('showid', ns('pheno_file_holder'))
          golem::invoke_js('hideid', ns('pheno_url'))
          golem::invoke_js('showid', ns('pheno_csv_options'))
          golem::invoke_js('hideid', ns('pheno_brapi_options'))
          golem::invoke_js('showid', ns('pheno_map'))
          golem::invoke_js('hideid', ns('pheno_map2'))
          updateCheckboxInput(session, 'pheno_example', value = FALSE)
        } else if (input$pheno_input == 'url') {
          golem::invoke_js('hideid', ns('pheno_file_holder'))
          golem::invoke_js('showid', ns('pheno_url'))
          golem::invoke_js('showid', ns('pheno_csv_options'))
          golem::invoke_js('hideid', ns('pheno_brapi_options'))
          golem::invoke_js('showid', ns('pheno_map'))
          golem::invoke_js('hideid', ns('pheno_map2'))
        } else { # brapi
          golem::invoke_js('hideid', ns('pheno_file_holder'))
          golem::invoke_js('hideid', ns('pheno_url'))
          golem::invoke_js('hideid', ns('pheno_csv_options'))
          golem::invoke_js('showid', ns('pheno_brapi_options'))
          golem::invoke_js('showid', ns('config_server_holder'))
          golem::invoke_js('hideid', ns('auth_server_holder'))
          golem::invoke_js('hideid', ns('data_server_holder'))
          golem::invoke_js('hideid', ns('pheno_map'))
          golem::invoke_js('showid', ns('pheno_map2'))
          updateCheckboxInput(session, 'pheno_example', value = FALSE)

          if(input$pheno_db_save > 0){
            updateTextInput(session, "pheno_db_url", value ='')

            updateTextInput(session, "pheno_token_user", value ='')
            updateTextInput(session, "pheno_db_user", value ='')
            updateTextInput(session, "pheno_db_password", value ='')
            updateCheckboxInput(session, "no_auth", value =TRUE)

            if(input$pheno_db_load > 0){
              pheno_data_brapi(NULL)
            }
          }
        }
      }
    )

    observeEvent(input$pheno_db_type,{
      updateTextInput(session, "pheno_db_url", value ='')

      updateTextInput(session, "pheno_token_user", value ='')
      updateTextInput(session, "pheno_db_user", value ='')
      updateTextInput(session, "pheno_db_password", value ='')
      updateCheckboxInput(session, "no_auth", value =TRUE)

      golem::invoke_js('hideid', ns('auth_server_holder'))
      golem::invoke_js('hideid', ns('data_server_holder'))
      golem::invoke_js('hideid', ns('pheno_db_token_holder'))
    })

    observeEvent(
      input$pheno_db_save,
      {
        if (input$pheno_db_url == ''){
          return(NULL)
          updateTextInput(session, "pheno_token_user", value ='')
          updateTextInput(session, "pheno_db_user", value ='')
          updateTextInput(session, "pheno_db_password", value ='')
          updateCheckboxInput(session, "no_auth", value =TRUE)
        }

        # TODO: check if it has a BrAPI endpoints
        # http://msdn.microsoft.com/en-us/library/ff650303.aspx
        if (!grepl("^(ht|f)tp(s?)\\:\\/\\/[0-9a-zA-Z]([-.\\w]*[0-9a-zA-Z])*(:(0-9)*)*(\\/?)([a-zA-Z0-9\\-\\.\\?\\,\\'\\/\\\\\\+&%\\$#_=]*)?$", input$pheno_db_url)) {
          shinyWidgets::show_alert(title = 'Invalid URL!', type = 'error')
          return(NULL)
        }

        golem::invoke_js('showid', ns('auth_server_holder'))
        golem::invoke_js('hideid', ns('data_server_holder'))
        golem::invoke_js('hideid', ns('pheno_db_token_holder'))

        if (input$pheno_db_type == 'ebs') {
          golem::invoke_js('hideid', ns('pheno_db_user_holder'))
          golem::invoke_js('hideid', ns('pheno_db_password_holder'))
          golem::invoke_js('hideid', ns('pheno_db_crop_holder'))
          golem::invoke_js('hideid', ns('no_auth_holder'))

          updateTextInput(session, "pheno_token_user", value ='')

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

        output$preview_pheno <- output$preview_pheno2 <- output$preview_pheno3 <- DT::renderDT(NULL, server = FALSE)
      }
    )

    observeEvent(
      input$pheno_db_login,
      {
        error_occurred <- FALSE
        tryCatch(
          expr = {
            if (input$pheno_db_type == 'ebs') {
              ebs_instance <- sub('https?://([^\\.]+)\\.([^/]+).*', '\\1', input$pheno_db_url)
              ebs_domain   <- sub('https?://([^\\.]+)\\.([^/]+).*', '\\2', input$pheno_db_url)

              if (ebs_domain == 'ebs.irri.org') {

                ebs_brapi         <- paste0('https://prod-cbbrapi.', ebs_domain)
                ebs_authorize_url <- 'https://auth-irri.ebsproject.org/oauth2/authorize'
                ebs_access_url    <- 'https://auth-irri.ebsproject.org/oauth2/token'
                ebs_client_id     <- '5qo8e9j6m61bqgbjvudhujpg7'
                ebs_client_secret <- 'eke3bke2ikcl20955so16lesi7r6sml1mr9qd6ai6abqcpg7ibn'

              } else if (ebs_domain == 'icarda.ebsproject.org') {

                ebs_brapi         <- paste0('https://app-cbbrapi.', ebs_domain)
                ebs_authorize_url <- 'https://auth.icarda.ebsproject.org/oauth2/authorize'
                ebs_access_url    <- 'https://auth.icarda.ebsproject.org/oauth2/token'
                ebs_client_id     <- 'b27e613di2vtmrm3okfhajidl'
                ebs_client_secret <- '11dj6g7n5eellkmodat0n4qic7tas2iauhrov8d5ksup4suo15kb'

              } else if (ebs_domain == 'ciat.ebsproject.org') {

                ebs_brapi         <- paste0('https://cbbrapi.', ebs_domain)
                ebs_authorize_url <- 'https://auth.ciat.ebsproject.org/oauth2/authorize'
                ebs_access_url    <- 'https://auth.ciat.ebsproject.org/oauth2/token'
                ebs_client_id     <- '58n0nccnih9qc0iibd9l735ksv'
                ebs_client_secret <- '1gqsfcj2kb1p4jtdfnlu8u5jsgnom9psu7cebgfq1i2lljcv97kd'

              } else {
                ebs_instance <- sub('(.*-)', '', ebs_instance)
                ebs_brapi    <- paste0('https://cbbrapi-', ebs_instance, '.', ebs_domain)

                if (ebs_instance %in% c('qa', 'dev')) {

                  ebs_authorize_url <- 'https://auth-dev.ebsproject.org/oauth2/authorize'
                  ebs_access_url    <- 'https://auth-dev.ebsproject.org/oauth2/token'
                  ebs_client_id     <- '5crahiqorgj0lppt3n9dkulkst'
                  ebs_client_secret <- '1sf4tipbp4arj3d5cncjmrvk9c2cu30gor5618hnh8rgkp6v5fs'

                } else if (ebs_instance %in% c('legumes')) {

                  ebs_authorize_url <- 'https://auth.ebs.iita.org/oauth2/authorize'
                  ebs_access_url    <- 'https://auth.ebs.iita.org/oauth2/token'
                  ebs_client_id     <- '2kvos0ijj8q3a6ek2okf431p2i'
                  ebs_client_secret <- '1p1i9ehhuf5fjg2an0qoaau3vb1c5bsmir9ci96rrpobd5mv4iqd'

                } else if (ebs_instance %in% c('dcp')) {

                  ebs_authorize_url <- 'https://auth.ebsproject.org/oauth2/authorize'
                  ebs_access_url    <- 'https://auth.ebsproject.org/oauth2/token'
                  ebs_client_id     <- 'lo1fpgt1gi2aucf4oj2dbgshu'
                  ebs_client_secret <- 'k95d36vdaqugvi95augdk2k45omc44p1godm48jko17vqjrcdc0'

                } else if (ebs_instance %in% c('staging', 'wheat', 'maize', 'wee', 'mee', 'ree')) {

                  ebs_authorize_url <- 'https://auth.ebsproject.org/oauth2/authorize'
                  ebs_access_url    <- 'https://auth.ebsproject.org/oauth2/token'
                  ebs_client_id     <- '346lau0avcptntd1ksbmgdi5c'
                  ebs_client_secret <- 'q5vnvakfj800ibh5tvqut73vj8klv1tpt6ugtmuneh6d2jb28i3'

                } else if (ebs_instance %in% c('demo')) {

                  ebs_authorize_url <- 'https://auth-sandbox.ebsproject.org/oauth2/authorize'
                  ebs_access_url    <- 'https://auth-sandbox.ebsproject.org/oauth2/token'
                  ebs_client_id     <- '14lqul2cds3o917vnu5aaququ4'
                  ebs_client_secret <- '1mk05qlg0l8lm2vfegj2m6hei0ig9dl80al26r9no771tcfc97mo'

                } else {
                  shinyWidgets::show_alert(title = 'Oops!', type = 'warning', "We can't recognize this EBS instance :-(")
                  return()
                }
              }

              golem::invoke_js('showid', ns('pheno_db_token_holder'))

              # golem::invoke_js("getCookie")
              shiny_app_uri <- paste0(session$clientData$url_protocol, '//',
                                      session$clientData$url_hostname,
                                      if(session$clientData$url_port == 1410) {
                                        paste0(':', session$clientData$url_port)
                                      },
                                      session$clientData$url_pathname)

              ebs_redirect_uri <- paste0(shiny_app_uri, 'www/callback/')

              EBS_client <- httr2::oauth_client(
                id        = ebs_client_id,
                secret    = ebs_client_secret,
                token_url = ebs_access_url,
                name      = "EBS"
              )

              if (input$pheno_token_user == "") {
                ebs_oauth_state <- httr2:::base64_url_rand()

                # set_cookie(session, "ebs_oauth_state", ebs_oauth_state)

                ebs_auth_url <- httr2::oauth_flow_auth_code_url(
                  client       = EBS_client,
                  auth_url     = ebs_authorize_url,
                  redirect_uri = ebs_redirect_uri,
                  state        = ebs_oauth_state
                )

                session$sendCustomMessage("popup", paste0(shiny_app_uri,
                                                          '?redirect=',
                                                          jsonlite::base64_enc(ebs_auth_url)))
                return()
              } else {
                # ebs_token <- sub(".*ebs_token=([^;]*).*", "\\1", input$cookies)
                ebs_token <- input$pheno_token_user
                payload <- jsonlite::base64_dec(ebs_token) |> rawToChar() |> jsonlite::fromJSON()

                token <- httr2:::oauth_client_get_token(client = EBS_client,
                                                        grant_type = "authorization_code",
                                                        code = payload$code,
                                                        state = payload$state,
                                                        redirect_uri = ebs_redirect_uri)

                QBMS::set_qbms_config(url = ebs_brapi, engine = 'ebs', brapi_ver = 'v2')
                QBMS::set_token(token$id_token)
              }

            } else if (input$pheno_db_type == 'bms') {
              QBMS::login_bms(input$pheno_db_user, input$pheno_db_password)
            } else if (input$pheno_db_type == 'breedbase') {
              if (input$no_auth) {
                QBMS::set_qbms_config(url = input$pheno_db_url, engine = 'breedbase', brapi_ver = 'v1', no_auth = TRUE)
              } else {
                QBMS::set_qbms_config(url = input$pheno_db_url, engine = 'breedbase')
                QBMS::login_breedbase(input$pheno_db_user, input$pheno_db_password)
              }
            }

            if (input$pheno_db_type == 'bms') {
              pheno_db_crops <- QBMS::list_crops()
            } else{
              pheno_db_programs <- QBMS::list_programs()$programName
            }

            output$preview_pheno <- output$preview_pheno2 <- output$preview_pheno3 <- DT::renderDT(NULL, server = FALSE)

          },
          error = function(e) {
            error_occurred <<- TRUE
            shinyWidgets::show_alert(title = 'Invalid Credentials!', type = 'error')
          }
        )

        if(error_occurred){
          golem::invoke_js('hideid', ns('data_server_holder'))
        } else{
          golem::invoke_js('showid', ns('data_server_holder'))

          if (input$pheno_db_type == 'bms') {
            shinybusy::show_modal_spinner('fading-circle', text = 'Loading Crops...')

            # pheno_db_crops <- QBMS::list_crops()

            updateSelectizeInput(session,
                                 inputId = 'pheno_db_crop',
                                 label   = 'Crop: ',
                                 choices = c('', pheno_db_crops))
            shinybusy::remove_modal_spinner()
          } else {
            shinybusy::show_modal_spinner('fading-circle', text = 'Loading Programs...')

            # pheno_db_programs <- QBMS::list_programs()$programName

            updateSelectizeInput(session,
                                 inputId = 'pheno_db_program',
                                 label   = 'Breeding Program: ',
                                 choices = c('', pheno_db_programs))

            shinybusy::remove_modal_spinner()
          }
        }
      }
    )

    observeEvent(
      input$pheno_db_crop,
      if (input$pheno_db_crop != '') {
        shinybusy::show_modal_spinner('fading-circle', text = 'Loading Crops...')

        QBMS::set_crop(input$pheno_db_crop)

        pheno_db_programs <- QBMS::list_programs()$programName

        updateSelectizeInput(session,
                             inputId = 'pheno_db_program',
                             label   = 'Breeding Program: ',
                             choices = c('', pheno_db_programs))

        shinybusy::remove_modal_spinner()
      }
    )

    observeEvent(
      input$pheno_db_program,
      if (input$pheno_db_program != '') {
        if (input$pheno_db_type == 'breedbase') {
          shinybusy::show_modal_spinner('fading-circle', text = 'Loading Folders...')
        } else {
          shinybusy::show_modal_spinner('fading-circle', text = 'Loading Trials...')
        }

        QBMS::set_program(input$pheno_db_program)

        tryCatch(
          expr = {
            pheno_db_trials <- NULL
            pheno_db_trials <- QBMS::list_trials()$trialName
          },
          error = function(e) {
            shinybusy::remove_modal_spinner()
            shinyWidgets::show_alert(title = 'No Trials!', type = 'warning')
          }
        )

        if (input$pheno_db_type == 'breedbase') {
          output$pheno_db_folder <- renderUI({
            selectizeInput(
              inputId = ns('pheno_db_folder'),
              label   = 'Select Folder: ',
              choices = c('', pheno_db_trials)
            )
          })
        } else {
          output$pheno_db_trial <- renderUI({
            selectizeInput(
              inputId = ns('pheno_db_trial'),
              label   = 'Trial/Study: ',
              choices = pheno_db_trials,
              multiple = TRUE
            )
          })
        }

        shinybusy::remove_modal_spinner()
      } else{
        output$pheno_db_folder <- NULL
        output$pheno_db_trial <- NULL
      }
    )

    observeEvent(
      input$pheno_db_folder,
      if (input$pheno_db_folder != '') {
        shinybusy::show_modal_spinner('fading-circle', text = 'Loading Trials...')

        QBMS::set_trial(input$pheno_db_folder)

        tryCatch(
          expr = {
            pheno_db_studies <- QBMS::list_studies()$studyName

            output$pheno_db_trial <- renderUI({
              selectizeInput(
                inputId = ns('pheno_db_trial'),
                label   = 'Trial/Study: ',
                choices = pheno_db_studies,
                multiple = TRUE
              )
            })
          },
          error = function(e) {
            if (input$pheno_db_type == 'breedbase') {
              shinyWidgets::show_alert(title = 'Empty Folder!',
                                       text = 'No trials found in the selected folder! Please choose the last/final folder that contains your trials in any nested folder structure.',
                                       type = 'warning')
            } else {
              shinyWidgets::show_alert(title = e, type = 'error')
            }

            output$pheno_db_trial <- NULL

            return(NULL)
          }
        )

        shinybusy::remove_modal_spinner()
      } else{
        output$pheno_db_trial <- NULL
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

    pheno_data_brapi <- reactiveVal(NULL)

    observeEvent(input$pheno_db_load,{
      if (input$pheno_input == 'brapi') {
        req(input$pheno_db_program)
        req(input$pheno_db_trial)
        if(input$pheno_db_type == 'breedbase'){
          req(input$pheno_db_folder)
        } else if(input$pheno_db_type == 'bms'){
          req(input$pheno_db_crop)
        }
        shinybusy::show_modal_spinner('fading-circle', text = 'Loading Data...')
        if (input$pheno_db_type == 'breedbase') {
          for (trial in unlist(strsplit(input$pheno_db_trial, ","))) {
            QBMS::set_study(trial)
            temp <- QBMS::get_study_data()

            # get breedbase trait ontology
            ontology <- QBMS::get_trial_obs_ontology()
            fields   <- colnames(temp)

            # replace long trait names with short ones from the ontology
            for (i in 1:length(fields)) {
              j <- which(ontology$name %in% fields[i])
              if (length(j) > 0) {
                if(!is.na(ontology$synonyms[[j]][1])) {
                  fields[i] <- ontology$synonyms[[j]][1]
                }
              }
            }

            colnames(temp) <- fields
            temp$trialName <- trial

            if (is.data.frame(data)) {
              data <- plyr::rbind.fill(data, temp)
            } else {
              data <- temp
            }
          }
        } else {
          for (trial in unlist(strsplit(input$pheno_db_trial, ","))) {
            QBMS::set_trial(trial)
            temp <- QBMS::get_trial_data()
            temp$trialName <- trial

            if (is.data.frame(data)) {
              data <- plyr::rbind.fill(data, temp)
            } else {
              data <- temp
            }
          }
        }

        shinybusy::remove_modal_spinner()

        if (is.data.frame(data)) {
          pheno_data_brapi(data)
          shinyWidgets::show_alert(title = 'Done!', type = 'success')
        } else {
          pheno_data_brapi(NULL)
          shinyWidgets::show_alert(title = 'No Data Available!', type = 'warning')
        }
      }
    })

    pheno_data <- reactive({
      tryCatch(
        expr = {
          if(length(input$pheno_input) > 0){
            if (input$pheno_input == 'file') {
              if (is.null(input$pheno_file)) {return(NULL)}else{

                shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
                if(length(input$pheno_file$datapath) > 1){
                  dataList <- list()
                  for(iFile in 1:length(input$pheno_file$datapath)){
                    dataList[[iFile]] <- as.data.frame(data.table::fread(input$pheno_file$datapath[iFile], sep = input$pheno_sep,
                                                                         quote = input$pheno_quote, dec = input$pheno_dec, header = TRUE))
                  }
                  dataListColNames <- lapply(dataList,colnames)
                  allColumnNames <- unique(unlist(dataListColNames))
                  for(jFile in 1:length(dataList)){
                    prov <- as.data.frame( matrix(NA, nrow=nrow(dataList[[jFile]]), ncol=length(allColumnNames)) )
                    colnames(prov) <- allColumnNames
                    prov[,colnames(dataList[[jFile]])] <- dataList[[jFile]]
                    dataList[[jFile]] <- prov
                  }
                  data <- do.call(rbind, dataList)
                }else{
                  data <- as.data.frame(data.table::fread(input$pheno_file$datapath, sep = input$pheno_sep,
                                                          quote = input$pheno_quote, dec = input$pheno_dec, header = TRUE))
                }
                shinybusy::remove_modal_spinner()

              }
            } else if (input$pheno_input == 'url') {
              if (input$pheno_url == ''){return(NULL)} else{
                data <- as.data.frame(data.table::fread(input$pheno_url, sep = input$pheno_sep,
                                                        quote = input$pheno_quote, dec = input$pheno_dec, header = TRUE))
              }
            } else if(input$pheno_input == 'brapi'){
              data <- pheno_data_brapi()
            }
            return(data)
          }
        },
        error = function(e) {
          shinybusy::remove_modal_spinner()
          if (input$pheno_db_type == 'ebs') {
            shinyWidgets::show_alert(title = 'Access Denied!', type = 'error', text = e)# 'You do not have permission to access this trial.')
          } else {
            shinyWidgets::show_alert(title = 'Error!', type = 'error', text = e)
          }

          return(NULL)
        }
      )
    })


    observeEvent(c(data()), { # update trait
      req(data())
      dtMta <- data()$data$pheno
      if(!is.null(dtMta)){
        golem::invoke_js('showid', ns('concat_environment_holder'))
        # golem::invoke_js('showid', ns('mapping_title_holder'))
      } else{
        golem::invoke_js('hideid', ns('concat_environment_holder'))
      }
    })

    observeEvent(
      pheno_data(),
      {
        # if (is.null(data())) return(NULL)

        temp <- data()
        temp$data$pheno <- pheno_data()
        # if(!is.data.frame(temp$data$pheno)) { return() }
        if(!is.null(temp$metadata$pheno)){temp$metadata$pheno <- temp$metadata$pheno[0,]} # make sure if an user uploads a new dataset the metadata starts empty
        if(!is.null(temp$modifications$pheno)){temp$modifications$pheno <- temp$modifications$pheno[0,]} # make sure if an user uploads a new dataset the modifications starts empty
        if(!is.null(temp$status)){
          toRemove <- which(temp$status$module == "qaRaw")
          if(length(toRemove) > 0){temp$status <- temp$status[-toRemove,, drop=FALSE]}
        } # make sure if an user uploads a new dataset the qaRaw starts empty
        if(is.data.frame(temp$data$pheno)) {
          output$preview_pheno <- output$preview_pheno2 <- output$preview_pheno3 <- DT::renderDT({
            DT::datatable(temp$data$pheno[1:min(c( round(33000/ncol(temp$data$pheno)) , nrow(temp$data$pheno))),],
                          extensions = 'Buttons',
                          options = list(dom = 'Blfrtip',
                                         scrollX = TRUE,
                                         buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                         lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All'))
                          )#,
                          # caption = htmltools::tags$caption(
                          #   style = 'color:cadetblue; font-weight:bold; font-size: 24px', #caption-side: bottom; text-align: center;
                          #   htmltools::em('Data preview.')
                          # )
            )
          }, server = FALSE)

          if (input$pheno_input == 'brapi') {
            output$brapi_trait_map <- renderUI({
              selectInput(
                inputId  = ns('brapi_traits'),
                label    = HTML(as.character(p('trait', span('(*required)',style="color:red")))),
                multiple = TRUE,
                choices  = as.list(c('', colnames(temp$data$pheno))),
              )

            })

            ### BrAPI auto mapping ###############################################
            mapping <- list(
              ebs = list(
                year = 'year',
                study = 'studyName',
                rep = 'rep',
                iBlock = 'block',
                row = 'positionCoordinateY', # 'designY',#
                col =  'positionCoordinateX', # 'designX',
                designation = 'germplasmName',
                gid = 'germplasmDbId',
                location = 'locationName',
                trial = 'trialName',
                entryType = 'entryType'
              ),
              bms = list(
                year = 'year',
                study = 'studyName',
                rep = 'rep',
                iBlock = 'block',
                row = 'positionCoordinateY',
                col = 'positionCoordinateX',
                designation = 'germplasmName',
                gid = 'germplasmDbId',
                location = 'studyName',
                trial = 'trialName',
                entryType = 'entryType'
              ),
              breedbase = list(
                year = 'studyYear',
                study = 'studyName',
                rep = 'replicate',
                iBlock = 'blockNumber',
                row = 'rowNumber',
                col = 'colNumber',
                designation = 'germplasmName',
                gid = 'germplasmDbId',
                location = 'locationName',
                trial = 'trialName',
                entryType = 'entryType'
              )
            )

            for (field in c('year', 'study', 'rep', 'iBlock', 'row', 'col', 'designation', 'gid', 'location', 'trial', 'entryType')) {
              if (mapping[[input$pheno_db_type]][[field]] %in% colnames(temp$data$pheno)) {
                if (field %in% temp$metadata$pheno$parameter) {
                  temp$metadata$pheno[temp$metadata$pheno$parameter == field, 'value'] <- mapping[[input$pheno_db_type]][[field]]
                } else {
                  temp$metadata$pheno <- rbind(temp$metadata$pheno, data.frame(parameter = field, value = mapping[[input$pheno_db_type]][[field]]))
                }
              }
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
            temp$data$pedigree <- data.frame(germplasmName = unique(temp$data$pheno$germplasmName), mother = NA, father = NA, yearOfOrigin = NA)
            temp$metadata$pedigree <- data.frame(parameter=c("designation","mother","father","yearOfOrigin"), value=c("germplasmName","mother","father","yearOfOrigin") )
            # stage       <- NA
            # pipeline    <- NA
            # country   **<- get_study_info()$locationDbId, then list_locations()
            # season    * <- get_study_info()$seasons

            ####################################################################
          } else{
            output$brapi_trait_map <- renderUI({NULL})
          }
        } else{
          output$preview_pheno <- output$preview_pheno2 <- output$preview_pheno3 <- DT::renderDT({NULL})
          output$brapi_trait_map <- renderUI({NULL})
        }
        data(temp)
      }, ignoreNULL = FALSE
    )

    output$pheno_map <- renderUI({
      if (is.null(pheno_data())){
        return(NULL)
      } else{
        header <- colnames(pheno_data())
        pheno_map <- lapply(map, function(x) {
          column(3,
                 selectInput(
                   inputId  = ns(paste0('select', x)),
                   label    = HTML(ifelse(x %in% c('designation','trait','location'), as.character(p(x, span('(*required)',style="color:red"))),  ifelse(x %in% c('rep','iBlock','row','col'), as.character(p(x, span('(*recommended)',style="color:grey"))), as.character(p(x, span('(*optional)',style="color:grey")))  )   ) ) ,
                   multiple = ifelse(x == 'trait', TRUE, FALSE),
                   choices  = as.list(c('', header )),
                   selected = ifelse(length(grep(x,header, ignore.case = TRUE)) > 0, header[grep(x,header, ignore.case = TRUE)[1]], '')
                 ),

                 # shinyBS::bsTooltip(ns(paste0('select', x)), 'Mapping this!', placement = 'left', trigger = 'hover'),

                 renderPrint({
                   # req(input[[paste0('select', x)]])
                   temp <- data()
                   if (x == 'trait') {
                     temp$metadata$pheno <- temp$metadata$pheno[temp$metadata$pheno$parameter != 'trait',]
                     for (i in input[[paste0('select', x)]]) {
                       temp$metadata$pheno <- rbind(temp$metadata$pheno, data.frame(parameter = 'trait', value = i))
                       if(!is.numeric(temp$data$pheno[,i])){temp$data$pheno[,i] <- as.numeric(gsub(",","",temp$data$pheno[,i]))}
                     }
                   } else { # is any other column other than trait
                     # if x is already in the metadata for pheno
                     if (x %in% temp$metadata$pheno$parameter & input[[paste0('select', x)]] != '') {
                       temp$metadata$pheno[temp$metadata$pheno$parameter == x, 'value'] <- input[[paste0('select', x)]]
                     } else { # if is not in the metadata
                       temp$metadata$pheno <- rbind(temp$metadata$pheno, data.frame(parameter = x, value = input[[paste0('select', x)]]))
                     }
                     if(x %in% c("designation","study") & input[[paste0('select', x)]] != ''){
                       temp$data$pheno[,input[[paste0('select', x)]]] <- stringi::stri_trans_general(temp$data$pheno[,input[[paste0('select', x)]]], "Latin-ASCII")
                     }
                     if(input[[paste0('select', x)]] == ''){
                       temp$metadata$pheno <- temp$metadata$pheno[-which(temp$metadata$pheno$parameter == x), ]
                     }
                   }
                   if (x == 'designation') {
                     if(input[[paste0('select', x)]] != ''){
                       temp$data$pedigree <- data.frame(designation = unique(pheno_data()[[input[[paste0('select', x)]]]]), mother = NA, father = NA, yearOfOrigin = NA)
                       temp$metadata$pedigree <- data.frame(parameter=c("designation","mother","father","yearOfOrigin"), value=c("designation","mother","father","yearOfOrigin") )
                     }
                   }
                   data(temp)
                 }),
          )
        })
        fluidRow(do.call(tagList, pheno_map))
      }
    })

    output$pheno_map2 <- renderUI({
      if (is.null(pheno_data())){
        return(NULL)
      } else{
        temp <- data()
        mapp2 <- c('year', 'study', 'rep', 'iBlock', 'row', 'col', 'designation', 'gid', 'location', 'trial', 'entryType')
        header2 <- colnames(pheno_data())
        pheno_map2 <- lapply(mapp2, function(x) {
          column(3,
                 shinyjs::disabled(selectInput(
                   inputId  = ns(paste0('select2', x)),
                   label    = x,
                   multiple = FALSE,
                   choices  = as.list(c('', header2 )),
                   # selected = temp$metadata$pheno[which(temp$metadata$pheno$parameter == x), 'value']
                   selected = if(x %in% temp$metadata$pheno$parameter){
                     temp$metadata$pheno[which(temp$metadata$pheno$parameter == x), 'value']
                   }else{''}
                 ))
          )
        })
        fluidRow(do.call(tagList,pheno_map2))
      }
    })

    # observeEvent(
    #   input$pheno_example,
    #   if(length(input$pheno_example) > 0){
    #     if (input$pheno_example) {
    #       updateSelectInput(session, 'pheno_input', selected = 'url')
    #
    #       # pheno_example_url <-  paste0(session$clientData$url_protocol, '//',
    #       #                              session$clientData$url_hostname, ':',
    #       #                              session$clientData$url_port,
    #       #                              session$clientData$url_pathname,
    #       #                              pheno_example)
    #
    #       pheno_example_url <- 'https://raw.githubusercontent.com/Breeding-Analytics/bioflow/main/inst/app/www/example/pheno.csv'
    #
    #       updateTextInput(session, 'pheno_url', value = pheno_example_url)
    #
    #       golem::invoke_js('hideid', ns('pheno_file_holder'))
    #       golem::invoke_js('showid', ns('pheno_url'))
    #       golem::invoke_js('hideid', ns('pheno_db'))
    #     } else {
    #       updateSelectInput(session, 'pheno_input', selected = 'file')
    #       updateTextInput(session, 'pheno_url', value = '')
    #
    #       golem::invoke_js('showid', ns('pheno_file_holder'))
    #       golem::invoke_js('hideid', ns('pheno_url'))
    #       golem::invoke_js('hideid', ns('pheno_db'))
    #     }
    #   }
    # )

    ## data example loading
    observeEvent(
      input$pheno_example,
      if(length(input$pheno_example) > 0){
        if (input$pheno_example) {
          shinyWidgets::ask_confirmation(
            inputId = ns("myconfirmation"),
            text = "Are you sure you want to load the example phenotypes? This will delete any phenotypic data currently in the environment.",
            title = "Data replacement warning"
          )
        }
      }
    )
    observeEvent(input$myconfirmation, {
      if (isTRUE(input$myconfirmation)) {
        if(length(input$pheno_example) > 0){
          if (input$pheno_example) {
            updateSelectInput(session, 'pheno_input', selected = 'url')

            # pheno_example_url <-  paste0(session$clientData$url_protocol, '//',
            #                              session$clientData$url_hostname, ':',
            #                              session$clientData$url_port,
            #                              session$clientData$url_pathname,
            #                              pheno_example)

            pheno_example_url <- 'https://raw.githubusercontent.com/Breeding-Analytics/bioflow/main/inst/app/www/example/pheno.csv'

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
      }else{
        shinyWidgets::updatePrettySwitch(session, "pheno_example", value = FALSE)
      }
    }, ignoreNULL = TRUE)

    # update trait
    observeEvent(c(data()), {
      req(data())
      dtMta <- data()$metadata$pheno
      if(!is.null(dtMta)){
        traitMtaInput <- intersect( c('pipeline','stage','year', 'season','timepoint', 'country', 'location', 'trial', 'study','management'), dtMta$parameter )
        updateSelectInput(session, "featuresEnvironment", choices = traitMtaInput, selected = traitMtaInput)
      }
    })

    outConcatenateEnv <- eventReactive(input$concatenateEnv, { # button to concatenate other columns in study
      req(data())
      req(input$featuresEnvironment)
      myObject <- data()
      myObject$metadata$pheno <- myObject$metadata$pheno %>% dplyr::arrange(factor(parameter, levels = input$featuresEnvironment ))
      environmentColumn <- which(myObject$metadata$pheno$parameter == "environment")
      if(length(environmentColumn) > 0){ # user has mapped an study column
        otherEnvironmentColumn <- which(myObject$metadata$pheno$parameter %in% input$featuresEnvironment)
        if(length(otherEnvironmentColumn) > 1){ # if user has mapped more than one column
          myObject$data$pheno[,myObject$metadata$pheno[environmentColumn, "value"]] <- apply(myObject$data$pheno[, myObject$metadata$pheno[otherEnvironmentColumn, "value"], drop=FALSE],1, function(x){paste(gsub("[^[:alnum:]]","",x), collapse = "_")} )
          myObject$data$pheno[,myObject$metadata$pheno[environmentColumn, "value"]] <- paste0("env",myObject$data$pheno[,myObject$metadata$pheno[environmentColumn, "value"]])
          data(myObject)
          shinyalert::shinyalert(title = "Success!", text = paste0("Columns ",paste(myObject$metadata$pheno[otherEnvironmentColumn, "value"], collapse = ", "), " concatenated and pasted in the '",myObject$metadata$pheno[environmentColumn, "value"], "' column"), type = "success")
          # cat(paste("Columns",paste(myObject$metadata$pheno[otherEnvironmentColumn, "value"], collapse = ", "), "concatenated and pasted in the",myObject$metadata$pheno[environmentColumn, "value"], "column"))
        }else if(length(otherEnvironmentColumn) == 1){
          myObject$data$pheno[,myObject$metadata$pheno[environmentColumn, "value"]] <- paste0("env",gsub("[^[:alnum:]]","",myObject$data$pheno[, myObject$metadata$pheno[otherEnvironmentColumn, "value"]]))
          data(myObject)
          shinyalert::shinyalert(title = "Success!", text = paste0("'",myObject$metadata$pheno[environmentColumn, "value"], "' column is equal to ", myObject$metadata$pheno[otherEnvironmentColumn, "value"]), type = "success")
          # cat("No additional columns to concatenate to your 'study' column")
        }else {
          myObject$metadata$pheno <- myObject$metadata$pheno[-which(myObject$metadata$pheno$parameter == "environment"), ]
          myObject$data$pheno <- myObject$data$pheno [,!(names(myObject$data$pheno) %in% c("environment"))]
          data(myObject)
          shinyalert::shinyalert(title = "Error!", text = paste("Please map at least one of the columns: 'pipeline', 'stage', 'year', 'season', 'timepoint', 'location', 'trial', 'study' or 'management' to be able to compute the environments and perform a genetic evaluation "), type = "error")
        }
      }else{ # user has not mapped an study column, we will add it
        otherEnvironmentColumn <- which(myObject$metadata$pheno$parameter %in% input$featuresEnvironment)
        if(length(otherEnvironmentColumn) > 1){ # if user has mapped more than one column
          myObject$data$pheno[,"environment"] <- apply(myObject$data$pheno[, myObject$metadata$pheno[otherEnvironmentColumn, "value"], drop=FALSE],1, function(x){paste(gsub("[^[:alnum:]]","",x), collapse = "_")} )
          myObject$data$pheno[,"environment"] <- paste0("env",myObject$data$pheno[,"environment"])
          myObject$metadata$pheno <- rbind(myObject$metadata$pheno, data.frame(parameter = 'environment', value = 'environment' ))
          data(myObject)
          shinyalert::shinyalert(title = "Success!", text = paste("Columns",paste(myObject$metadata$pheno[otherEnvironmentColumn, "value"], collapse = ", "), "concatenated and pasted in a column named 'environment' "), type = "success")
          # cat(paste("Columns",paste(myObject$metadata$pheno[otherEnvironmentColumn, "value"], collapse = ", "), "concatenated and pasted in a column named 'environment' "))
        }else if(length(otherEnvironmentColumn) == 1){
          myObject$data$pheno[,"environment"] <- paste0("env",gsub("[^[:alnum:]]","",myObject$data$pheno[, myObject$metadata$pheno[otherEnvironmentColumn, "value"]]))
          myObject$metadata$pheno <- rbind(myObject$metadata$pheno, data.frame(parameter = 'environment', value = 'environment' ))
          data(myObject)
          shinyalert::shinyalert(title = "Success!", text = paste("No additional columns to concatenate. 'environment' column is equal to", myObject$metadata$pheno[otherEnvironmentColumn, "value"]), type = "success")
        }else{
          shinyalert::shinyalert(title = "Error!", text = paste("Please map at least one of the columns 'year', 'season', 'timepoint', 'location', 'trial', 'study' or 'management' to be able to do compute the environments and perform a genetic evaluation "), type = "error")
          # cat(paste("Please map at least one of the columns 'year', 'season', 'location', 'trial' or 'study' to be able to do compute the environments and perform a genetic evaluation "))
        }
      }
    })

    output$outConcatenateEnv <- renderPrint({
      outConcatenateEnv()
    })

    # change color of updated study column
    observeEvent(input$concatenateEnv,{
      output$preview_pheno <- output$preview_pheno2 <- output$preview_pheno3 <- DT::renderDT({
        myObject <- data()
        phenoColnames <- colnames(myObject$data$pheno)
        if("environment" %in% phenoColnames){
          newPhenoColnames <- c("environment", setdiff(phenoColnames, "environment"))
          myObject$data$pheno <- myObject$data$pheno[, newPhenoColnames]
          environmentColumnName <- myObject$metadata$pheno$value[which(myObject$metadata$pheno$parameter == "environment")]
          # 33K is the maximum number of cells to display in a render DT with server=FALSE
          DT::datatable(myObject$data$pheno[1:min(c( round(33000/ncol(myObject$data$pheno)) , nrow(myObject$data$pheno))),],
                        extensions = 'Buttons',
                        options = list(dom = 'Blfrtip',
                                       scrollX = TRUE,
                                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                       lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All'))),
                        caption = htmltools::tags$caption(
                          style = 'color:cadetblue; font-weight:bold; font-size: 24px', #caption-side: bottom; text-align: center;
                          htmltools::em('Data preview.')
                        )
          ) %>%
            DT::formatStyle(environmentColumnName, backgroundColor = "#009E60")
        } else{ # 33000 is the maximum number of cells to display
          DT::datatable(myObject$data$pheno[1:min(c( round(33000/ncol(myObject$data$pheno)) , nrow(myObject$data$pheno))),],
                        extensions = 'Buttons',
                        options = list(dom = 'Blfrtip',
                                       scrollX = TRUE,
                                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                       lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All'))),
                        caption = htmltools::tags$caption(
                          style = 'color:cadetblue; font-weight:bold; font-size: 24px', #caption-side: bottom; text-align: center;
                          htmltools::em('Data preview.')
                        )
          )
        }
      }, server = FALSE)
    })

  })
}

## To be copied in the UI
# mod_getDataPheno_ui("getDataPheno_1")

## To be copied in the server
# mod_getDataPheno_server("getDataPheno_1")

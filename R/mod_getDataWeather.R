#' getDataWeather UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_getDataWeather_ui <- function(id){
  ns <- NS(id)
  tagList(

    tags$br(),

    navlistPanel("Steps:", widths = c(2, 10),
                  tabPanel(div("1. Select the source" ),
                           # tags$br(),
                           selectInput(
                             inputId = ns('weather_input'),
                             label   = 'Data Source*:',
                             choices = list('Table Upload' = 'weatherfile', 'API NASAPOWER' = 'weatherapi' ),
                             width   = '200px'
                           ),
                  ),
                  tabPanel(div("2. Set coordinates" ),
                           tags$span(id = ns('apiOptionsInput'),
                                     tabPanel("Specify coordinates", icon = icon("magnifying-glass-chart"),
                                              br(),
                                              tags$span(id = ns('weather_message_holder'),
                                                        uiOutput(ns("warningMessage")),
                                              ),

                                              tags$span(id = ns('weather_file_holder'),
                                                        column(width = 12, style = "background-color:green; color: #FFFFFF",
                                                               column(width = 2, p(strong("Environment")) ),
                                                               column(width = 2, p(strong("Latitude")) ),
                                                               column(width = 2, p(strong("Longitude")) ),
                                                               column(width = 2, p(strong("Planting Date")) ),
                                                               column(width = 2, p(strong("Harvesting Date")) ),
                                                               column(width = 2, p(strong("Extraction interval")) ),
                                                        ),
                                                        column(width = 12, style = "background-color:green; color: #FFFFFF",
                                                               column(width = 2, uiOutput(ns("environment")) ),
                                                               column(width = 2, uiOutput(ns("latitude")) ),
                                                               column(width = 2, uiOutput(ns("longitude")) ),
                                                               column(width = 2, uiOutput(ns("plantingDate")) ),
                                                               column(width = 2, uiOutput(ns("harvestingDate")) ),
                                                               column(width = 2,
                                                                      selectInput(ns("temporal"),label=NULL, choices = list("hourly","daily","monthly"), selected = "daily"  ),
                                                               ),
                                                        ),
                                                        shinydashboard::box(status="success",width = 12, solidHeader = TRUE,
                                                                            column(width=12, style = "height:410px; overflow-y: scroll;overflow-x: scroll;",
                                                                                   p(span("Preview of coordinates selected for extraction.", style="color:black")),
                                                                                   plotly::plotlyOutput(ns("plotMeteo")),
                                                                            ),
                                                        )
                                              ),
                                     ),
                           ), # end of API options
                           tags$span(id = ns('fileOptionsInput'),
                                     column(width=8,
                                            # tags$span(id = ns('weather_file_holder'),
                                            fileInput(
                                              inputId = ns('weather_file'),
                                              label   = NULL,
                                              width   = '400px',
                                              accept  = c('.txt', '.csv')
                                            ),
                                            # ),
                                     ),
                                     column(width=4,
                                            shinydashboard::box(width=12, title = span(icon('screwdriver-wrench'), ' Options'), collapsible = TRUE, collapsed = TRUE, status = 'success', solidHeader = TRUE,
                                                                shinyWidgets::prettyRadioButtons(ns('weather_sep'), 'Separator Character', selected = ',', inline = TRUE,
                                                                                                 choices = c('Comma' = ',', 'Semicolon' = ';', 'Tab' = "\t")),

                                                                shinyWidgets::prettyRadioButtons(ns('weather_quote'), 'Quoting Character', selected = '"', inline = TRUE,
                                                                                                 choices = c('None' = '', 'Double Quote' = '"', 'Single Quote' = "'")),

                                                                shinyWidgets::prettyRadioButtons(ns('weather_dec'), 'Decimal Points', selected = '.', inline = TRUE,
                                                                                                 choices = c('Dot' = '.', 'Comma' = ',')),
                                            ),
                                     ),
                                     column(width=12,
                                            shinydashboard::box(width = 12,  status = 'success', solidHeader = FALSE,
                                                                DT::DTOutput(ns('preview_weather2')),
                                            ),
                                     ),
                           ), # end of file upload options
                  ),
                  tabPanel(div("3. Retrieve/Match data" ),
                           tags$span(id = ns('apiOptionsRetrieve'),
                                     tags$br(),
                                     actionButton(ns("rungetWeather"), "Extract", icon = icon("play-circle")),
                                     textOutput(ns("outgetWeather")),
                           ),
                           tags$span(id = ns('fileOptionsRetrieve'),

                                     column(width=12,
                                            shinydashboard::box(width = 12, status = 'success', solidHeader = FALSE,
                                                                hr(),
                                                                uiOutput(ns('weather_map')),
                                            ),
                                     ),
                                     column(width=12,
                                            shinydashboard::box(width = 12,  status = 'success', solidHeader = FALSE,
                                                                DT::DTOutput(ns('preview_weather')),
                                            ),
                                     ),

                           ),
                  ),
                  tabPanel(div("4. Check status" ),
                           tags$br(),
                           uiOutput(ns("warningMessage2")),
                  ),
    ),

  )
}

#' getDataWeather Server Functions
#'
#' @noRd
mod_getDataWeather_server <- function(id, map=NULL, data = NULL, res_auth=NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(
      input$weather_input,
      if(length(input$weather_input) > 0){ # added
        if (input$weather_input == 'weatherfile') {
          golem::invoke_js('showid', ns('fileOptionsInput'))
          golem::invoke_js('hideid', ns('apiOptionsInput'))

          golem::invoke_js('showid', ns('fileOptionsRetrieve'))
          golem::invoke_js('hideid', ns('apiOptionsRetrieve'))
        } else if (input$weather_input == 'weatherapi') {
          golem::invoke_js('showid', ns('apiOptionsInput'))
          golem::invoke_js('hideid', ns('fileOptionsInput'))

          golem::invoke_js('showid', ns('apiOptionsRetrieve'))
          golem::invoke_js('hideid', ns('fileOptionsRetrieve'))
        }
      }
    )

    output$warningMessage <- renderUI(
      if(is.null(data()$data$pheno)){
        golem::invoke_js('hideid', ns('weather_file_holder'))
        golem::invoke_js('showid', ns('weather_message_holder'))
        HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your phenotypic data first to identify the environments.")) )
      }else{ # data is there
        mappedColumns <- length(which(c("environment") %in% data()$metadata$pheno$parameter))
        if(mappedColumns == 1){
          golem::invoke_js('showid', ns('weather_file_holder'))
          golem::invoke_js('hideid', ns('weather_message_holder'))
        }else{
          golem::invoke_js('hideid', ns('weather_file_holder'))
          golem::invoke_js('showid', ns('weather_message_holder'))
          HTML( as.character(div(style="color: orange; font-size: 20px;", "Please make sure that you have defined the 'environment' column in 'Data Retrieval' tab for Phenotypes.")) )
        }
      }
    )

    output$warningMessage2 <- renderUI(
      if(!is.null(data()$data$weather)){
        HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, you can proceed to use the other modules.")) )
      }else{
        if(is.null(data())){
          HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your phenotypic data first to identify the environments.")) )
        }else{ # data is there
          mappedColumns <- length(which(c("environment") %in% data()$metadata$pheno$parameter))
          if(mappedColumns == 1){
            HTML( as.character(div(style="color: gold; font-size: 20px;", "Please specify the coordinates and retrieve weather data.")) )
          }else{
            HTML( as.character(div(style="color: orange; font-size: 20px;", "Please make sure that you have defined the 'environment' column in 'Data Retrieval' tab for Phenotypes.")) )
          }
        }
      }

    )

    #####################################################
    # inputs for API
    output$environment <- renderUI({
      req(data())
      dtProv = data()$data$pheno
      paramsPheno <- data()$metadata$pheno
      colnames(dtProv) <- cgiarBase::replaceValues(colnames(dtProv), Search = paramsPheno$value, Replace = paramsPheno$parameter )
      envCol <- paramsPheno[which(paramsPheno$parameter == "environment"),"value"]
      if(length(envCol) > 0){
        fieldNames <- as.character(unique(dtProv[,"environment"]))
        lapply(1:length(fieldNames), function(i) {
          tags$div(id = "inline", textInput(
            session$ns(paste0('environment',i)),
            NULL,
            value=fieldNames[i], #width = '800px',
          ))
        })
      }
    })
    output$latitude <- renderUI({
      req(data())
      dtProv = data()$data$pheno
      paramsPheno <- data()$metadata$pheno
      colnames(dtProv) <- cgiarBase::replaceValues(colnames(dtProv), Search = paramsPheno$value, Replace = paramsPheno$parameter )
      envCol <- paramsPheno[which(paramsPheno$parameter == "environment"),"value"]
      if(length(envCol) > 0){
        fieldNames <- as.character(unique(dtProv[,"environment"]))
        lapply(1:length(fieldNames), function(i) {
          tags$div(id = "inline",  numericInput(
            session$ns(paste0('latitude',i)),
            NULL,
            value = 0
          ))
        })
      }
    })
    output$longitude <- renderUI({
      req(data())
      dtProv = data()$data$pheno
      paramsPheno <- data()$metadata$pheno
      colnames(dtProv) <- cgiarBase::replaceValues(colnames(dtProv), Search = paramsPheno$value, Replace = paramsPheno$parameter )
      envCol <- paramsPheno[which(paramsPheno$parameter == "environment"),"value"]
      if(length(envCol) > 0){
        fieldNames <- as.character(unique(dtProv[,"environment"]))
        lapply(1:length(fieldNames), function(i) {
          tags$div(id = "inline", numericInput(
            session$ns(paste0('longitude',i)),
            NULL,
            value = 0
          ))
        })
      }
    })
    output$plantingDate <- renderUI({
      req(data())
      dtProv = data()$data$pheno
      paramsPheno <- data()$metadata$pheno
      colnames(dtProv) <- cgiarBase::replaceValues(colnames(dtProv), Search = paramsPheno$value, Replace = paramsPheno$parameter )
      envCol <- paramsPheno[which(paramsPheno$parameter == "environment"),"value"]
      if(length(envCol) > 0){
        fieldNames <- as.character(unique(dtProv[,"environment"]))
        lapply(1:length(fieldNames), function(i) {
          tags$div(id = "inline", dateInput(
            session$ns(paste0('plantingDate',i)),
            NULL,
            value = Sys.Date()-31
          ))
        })
      }
    })
    output$harvestingDate <- renderUI({
      req(data())
      dtProv = data()$data$pheno
      paramsPheno <- data()$metadata$pheno
      colnames(dtProv) <- cgiarBase::replaceValues(colnames(dtProv), Search = paramsPheno$value, Replace = paramsPheno$parameter )
      envCol <- paramsPheno[which(paramsPheno$parameter == "environment"),"value"]
      if(length(envCol) > 0){
        fieldNames <- as.character(unique(dtProv[,"environment"]))
        lapply(1:length(fieldNames), function(i) {
          tags$div(id = "inline", dateInput(
            session$ns(paste0('harvestingDate',i)),
            NULL,
            value = Sys.Date()-30
          ))
        })
      }
    })
    dataWeather = reactive({ ## function to return input parameters (env, latitude, longitude, etc.)
      req(data())
      dtProv = data()$data$pheno
      paramsPheno <- data()$metadata$pheno
      colnames(dtProv) <- cgiarBase::replaceValues(colnames(dtProv), Search = paramsPheno$value, Replace = paramsPheno$parameter )
      envCol <- which(paramsPheno$parameter == "environment")
      if (length(envCol) > 0) {
        fieldNames <- as.character(unique(dtProv[,"environment"]))
        values <- values2 <- values3 <- values4 <- values5 <- vector()
        for (i in 1:length(fieldNames)) {
          tempval <- reactive({paste0('input$','latitude',i)})
          values[i] <- tempval()
          values[i] <- eval(parse(text = values[i]))
          tempval <- reactive({paste0('input$','longitude',i)})
          values2[i] <- tempval()
          values2[i] <- eval(parse(text = values2[i]))
          tempval <- reactive({paste0('input$','plantingDate',i)})
          values3[i] <- tempval()
          values3[i] <- eval(parse(text = values3[i])) # eval(lubridate::parse_date_time2(as.character(values3[i]), orders='Ymd'))
          tempval <- reactive({paste0('input$','harvestingDate',i)})
          values4[i] <- tempval()
          values4[i] <- eval(parse(text = values4[i]))
          tempval <- reactive({paste0('input$','environment',i)})
          values5[i] <- tempval()
          values5[i] <- eval(parse(text = values5[i]))
        }
        values <- as.numeric( as.data.frame(t(as.numeric(values))) );
        values2 <- as.numeric( as.data.frame(t(as.numeric(values2))) );
        values3 <- as.Date(as.numeric( as.data.frame(t(as.numeric(values3))) ), origin="1970-01-01", tz="GMT")
        values4 <- as.Date(as.numeric( as.data.frame(t(as.numeric(values4))) ), origin="1970-01-01", tz="GMT")
        values5 <- as.character(as.data.frame(t(as.character(values5))) )
        xx <- data.frame(latitude=values, longitude=values2, plantingDate=values3, harvestingDate=values4, environment=values5)
        return(xx)
      }
    })
    ##produce the map plot
    output$plotMeteo <-  plotly::renderPlotly({
      req(data())
      xx <- dataWeather()
      if(!is.null(xx)){
        fig <- xx
        fig <- fig %>%
          plotly::plot_ly(
            lat = ~latitude,
            lon = ~longitude,
            type = "scattermapbox",
            hovertext = ~environment, #us_cities[,"City"],
            marker = list(color = "fuchsia"))
        fig <- fig %>%
          plotly::layout(
            mapbox = list(
              style = 'open-street-map',
              zoom =1,
              center = list(lon = 0, lat = 0)
            )
          )
        # }
        fig
      }
    })
    ## save when user clicks

    outgetWeather <- eventReactive(input$rungetWeather, {

      req(data())
      if(is.null(data())){
        cat( "Please retrieve or load your phenotypic data using the 'Data Retrieval' tab.")
      }else{ # data is there
        ## pheno check
        shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
        xx <- dataWeather()
        # save(xx, file="xx.RData")
        howManyProvidede <- apply(xx,1,function(x){length(which(is.na(x)))/length(x)})
        if( length(which(howManyProvidede != 0)) > 0  ){ # the user has not provided any full information for any field
          cat( "Please fill the 4 fields required (latitude, longitude, planting date and harvesting date) for all environments to extract weather data.")
        }else{

          if ( nrow(unique(xx[,c("latitude","longitude")])) == nrow(xx) ){ # unique coordinates for each environment

            result <- data()
            weather <- cgiarPipeline::nasaPowerExtraction(LAT=xx$latitude,LONG=xx$longitude,
                                                          date_planted=xx$plantingDate,
                                                          date_harvest=xx$harvestingDate,
                                                          environments=xx$environment,
                                                          temporal=input$temporal
            )
            result$data$weather <- weather$data
            result$metadata$weather <- weather$metadata # unique(rbind(result$metadata$weather, weather$descriptive))
            data(result)
            cat(paste("Weather data saved succesfully."))

          }else{
            cat(paste("Different environments cannot have the same coordinates. Please correct."))
          }

        }
        shinybusy::remove_modal_spinner()
      }

    })
    output$outgetWeather <- renderPrint({
      outgetWeather()
    })

    ########################################################
    # input for FILE
    weather_data_table = reactive({ # function to purely just read a csv when we need to match the table file
      if(length(input$weather_input) > 0){ # added
        if (input$weather_input == 'weatherfile' ) {

          if (is.null(input$weather_file)) {return(NULL)}else{
            weather_file <- input$weather_file$datapath
            shinybusy::show_modal_spinner('fading-circle', text = 'Loading...')
            df <- as.data.frame(data.table::fread(weather_file, sep = input$weather_sep, quote = input$weather_quote, dec = input$weather_dec, header = TRUE))
            shinybusy::remove_modal_spinner()
            return(df)
          }

        } else {
          return(NULL);
        }
      }else{
        return(NULL)
      }
    })

    observeEvent(c(weather_data_table()), { # display preview of weather data
      req(weather_data_table())
      provweather <- weather_data_table()
      output$preview_weather <- output$preview_weather2 <- DT::renderDT({
        req(weather_data_table())
        DT::datatable(weather_data_table()[,1:min(c(50,ncol(weather_data_table())))],
                      extensions = 'Buttons',
                      options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All'))),
                      caption = htmltools::tags$caption(
                        style = 'color:cadetblue; font-weight:bold; font-size: 18px', #caption-side: bottom; text-align: center;
                        htmltools::em('Data preview.')
                      )
        )
      }, server = FALSE)
      # read the data and save in the data object
      temp <- data()
      temp$data$weather <- weather_data_table()
      data(temp)
    })

    output$weather_map <- renderUI({
      if (is.null(weather_data_table())) return(NULL)

      header <- colnames(weather_data_table())
      weather_map <- lapply(map, function(x) {
        column(3,
               selectInput(
                 inputId  = ns(paste0('select', x)),
                 label    = HTML(ifelse(x %in% c('environment','latitude','longitude','trait'), as.character(p(x, span('(*required)',style="color:red"))),  ifelse(x %in% c('year','month','day'), as.character(p(x, span('(*recommended)',style="color:grey"))), as.character(p(x, span('(*optional)',style="color:grey")))  )   ) ) ,
                 multiple = ifelse(x == 'trait', TRUE, FALSE),
                 choices  = as.list(c('', header)),
                 selected = ifelse(length(grep(x,header, ignore.case = TRUE)) > 0, header[grep(x,header, ignore.case = TRUE)[1]], '')
               ),

               # shinyBS::bsTooltip(ns(paste0('select', x)), 'Mapping this!', placement = 'left', trigger = 'hover'),

               renderPrint({
                 # req(input[[paste0('select', x)]])
                 temp <- data()
                 if (x == 'trait') {
                   temp$metadata$weather <- temp$metadata$weather[temp$metadata$weather$parameter != 'trait',]
                   for (i in input[[paste0('select', x)]]) {
                     temp$metadata$weather <- rbind(temp$metadata$weather, data.frame(parameter = 'trait', value = i))
                     # if(!is.numeric(temp$data$weather[,i])){temp$data$weather[,i] <- as.numeric(gsub(",","",temp$data$weather[,i]))}
                   }
                 } else { # is any other column other than trait
                   if (x %in% temp$metadata$weather$parameter & input[[paste0('select', x)]] != '') {
                     temp$metadata$weather[temp$metadata$weather$parameter == x, 'value'] <- input[[paste0('select', x)]]
                   } else {
                     temp$metadata$weather <- rbind(temp$metadata$weather, data.frame(parameter = x, value = input[[paste0('select', x)]]))
                   }
                   if(input[[paste0('select', x)]] == ''){
                     temp$metadata$weather <- temp$metadata$weather[-which(temp$metadata$weather$parameter == x), ]
                   }
                 }
                 data(temp)
               }),
        )
      })
      fluidRow(do.call(tagList, weather_map))
    })


  })
}

## To be copied in the UI
# mod_getDataWeather_ui("getDataWeather_1")

## To be copied in the server
# mod_getDataWeather_server("getDataWeather_1")

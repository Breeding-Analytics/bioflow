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
                                     actionButton(ns("rungetWeather"), "Extract", icon = icon("play-circle")),
                                     textOutput(ns("outgetWeather")),
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
    # tabPanel("Run extraction", icon = icon("play"),
    #          br(),
    #          actionButton(ns("rungetWeather"), "Extract", icon = icon("play-circle")),
    #          textOutput(ns("outgetWeather")),
    # ),
  )
}

#' getDataWeather Server Functions
#'
#' @noRd
mod_getDataWeather_server <- function(id, data = NULL, res_auth=NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$warningMessage <- renderUI(
      if(is.null(data())){
        golem::invoke_js('hideid', ns('weather_file_holder'))
        golem::invoke_js('showid', ns('weather_message_holder'))
        HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your phenotypic data using the 'Data Retrieval' tab.")) )
      }else{ # data is there
        mappedColumns <- length(which(c("environment") %in% data()$metadata$pheno$parameter))
        if(mappedColumns == 1){
          golem::invoke_js('showid', ns('weather_file_holder'))
          golem::invoke_js('hideid', ns('weather_message_holder'))
          # HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to identify the location of your environments.")) )
        }else{
          golem::invoke_js('hideid', ns('weather_file_holder'))
          golem::invoke_js('showid', ns('weather_message_holder'))
          HTML( as.character(div(style="color: red; font-size: 20px;", "Please make sure that you have computed the 'environment' column in 'Data Retrieval' tab for Phenotypes.")) )
        }
      }
    )

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

    ##
    dataWeather = reactive({
      req(data())
      dtProv = data()$data$pheno
      paramsPheno <- data()$metadata$pheno
      colnames(dtProv) <- cgiarBase::replaceValues(colnames(dtProv), Search = paramsPheno$value, Replace = paramsPheno$parameter )
      envCol <- paramsPheno[which(paramsPheno$parameter == "environment"),"value"]
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
          cat( "Please fill the 4 fields required for at least one environment to extract weather data.")
        }else{
          result <- data()
          weather <- cgiarPipeline::nasaPowerExtraction(LAT=xx$latitude,LONG=xx$longitude,
                                                        date_planted=xx$plantingDate,
                                                        date_harvest=xx$harvestingDate,
                                                        environments=xx$environment,
                                                        temporal=input$temporal
          )
          result$data$weather <- weather$WTH
          result$metadata$weather <- unique(rbind(result$metadata$weather, weather$descriptive))
          data(result)
          cat(paste("Weather data saved succesfully."))
        }
        shinybusy::remove_modal_spinner()
      }

    })
    output$outgetWeather <- renderPrint({
      outgetWeather()
    })
  })
}

## To be copied in the UI
# mod_getDataWeather_ui("getDataWeather_1")

## To be copied in the server
# mod_getDataWeather_server("getDataWeather_1")

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

    tags$head(
      tags$style(type="text/css", "#inline label{ display: table-cell; text-align: right; vertical-align: middle; }
                #inline .form-group { display: table-row;}")
    ),

    shiny::mainPanel(width = 12,
                     tabsetPanel( #width=9,
                       type = "tabs",

                       tabPanel(div(icon("book"), "Information-Weather-Extract") ,
                                br(),
                                shinydashboard::box(status="success",width = 12,
                                                    solidHeader = TRUE,
                                                    column(width=12,   style = "height:580px; overflow-y: scroll;overflow-x: scroll;",
                                                           h1(strong(span("Weather Data Extraction", style="color:green"))),
                                                           h2(strong("Status:")),
                                                           uiOutput(ns("warningMessage")),
                                                           tags$body(
                                                             h2(strong("Details")),
                                                             p("The use of environmental information to properly use evolutionary forces is critical for adaptation and understanding genotype by environment.
                                                               The following parameters are needed to extract data properly:"),
                                                             # img(src = "www/getWeather.png", height = 300, width = 600), # add an image
                                                             p(strong("Environments.-")," A list of environments to link to specific location coordinates."),
                                                             p(strong("Latitude.-")," The list of latitude coordinates associated to the environments."),
                                                             p(strong("Longitude.-"),"A list of longitude coordinates associated to the environments."),
                                                             p(strong("Date planting.-"),"A list of planting dates associated to the different environments."),
                                                             p(strong("Date harvesting.-"),"A list of harvesting dates associated to the different environments."),
                                                             h2(strong("References")),
                                                             p("Tukey, J. W. (1977). Exploratory Data Analysis. Section 2C."),
                                                           )
                                                    )
                                )
                       ),
                       tabPanel(div(icon("arrow-right-to-bracket"), "Input"),
                                tabsetPanel(
                                  tabPanel("Specify coordinates", icon = icon("magnifying-glass-chart"),
                                           br(),
                                           column(width = 12, style = "background-color:grey; color: #FFFFFF",
                                                  column(width = 2, p(strong("Environment")) ),
                                                  column(width = 2, p(strong("Latitude")) ),
                                                  column(width = 2, p(strong("Longitude")) ),
                                                  column(width = 2, p(strong("Planting Date")) ),
                                                  column(width = 2, p(strong("Harvesting Date")) ),
                                                  column(width = 2, p(strong("Extraction interval")) ),
                                           ),
                                           column(width = 12, style = "background-color:grey; color: #FFFFFF",
                                                  column(width = 2, uiOutput(ns("environment")) ),
                                                  column(width = 2, uiOutput(ns("latitude")) ),
                                                  column(width = 2, uiOutput(ns("longitude")) ),
                                                  column(width = 2, uiOutput(ns("plantingDate")) ),
                                                  column(width = 2, uiOutput(ns("harvestingDate")) ),
                                                  column(width = 2, selectInput(ns("temporal"),label=NULL, choices = list("hourly","daily","monthly"), selected = "hourly"  ) ),
                                           ),
                                           h4(strong(span("Visualizations below aim to help you pick the right parameter values. Please inspect them.", style="color:green"))),
                                           hr(style = "border-top: 3px solid #4c4c4c;"),
                                           shinydashboard::box(status="success",width = 12, solidHeader = TRUE,
                                                               column(width=12, style = "height:410px; overflow-y: scroll;overflow-x: scroll;",
                                                                      p(span("Preview of coordinates selected for extraction.", style="color:black")),
                                                                      plotly::plotlyOutput(ns("plotMeteo")),
                                                               ),
                                           )
                                  ),
                                  tabPanel("Run extraction", icon = icon("play"),
                                           br(),
                                           actionButton(ns("rungetWeather"), "Extract", icon = icon("play-circle")),
                                           textOutput(ns("outgetWeather")),
                                  ),
                                ) # end of tabset
                       ),# end of input panel
                       tabPanel(div(icon("arrow-right-from-bracket"), "Output" ) , value = "outputTabs",
                                tabsetPanel(
                                  tabPanel("Data", icon = icon("table"),
                                           br(),
                                           shinydashboard::box(status="success",width = 12,
                                                               solidHeader = TRUE,
                                                               column(width=12,DT::DTOutput(ns("dataWeatherDisplay")),style = "height:530px; overflow-y: scroll;overflow-x: scroll;")
                                           )
                                  ),
                                  tabPanel("Summaries", icon = icon("table"),
                                           br(),
                                           shinydashboard::box(status="success",width = 12,
                                                               solidHeader = TRUE,
                                                               column(width=12,br(),DT::DTOutput(ns("metadataWeather")),style = "height:530px; overflow-y: scroll;overflow-x: scroll;")
                                           )
                                  ),
                                  tabPanel("Report", icon = icon("file-image"),
                                           br(),
                                           div(tags$p("Please download the report below:") ),
                                           downloadButton(ns("downloadReportWeather"), "Download report"),
                                           br(),
                                           uiOutput(ns('reportWeather'))
                                  ),
                                ),
                       ),
                     )) # end mainpanel



  )
}

#' getDataWeather Server Functions
#'
#' @noRd
mod_getDataWeather_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })
    ############################################################################
    # warning message
    output$warningMessage <- renderUI(
      if(is.null(data())){
        HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your phenotypic data using the 'Data Retrieval' tab.")) )
      }else{ # data is there
        mappedColumns <- length(which(c("environment") %in% data()$metadata$pheno$parameter))
        if(mappedColumns == 1){ HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to identify the location of your environments.")) )
        }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please make sure that you have computed the 'environment' column using the 'Data Retrieval' tab.")) )
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

        if(!inherits(result,"try-error")) { # if all goes well in the run

          ## data table
          output$dataWeatherDisplay <-  DT::renderDT({
            # if ( hideAll$clearAll){
            #   return()
            # }else{
            current.predictions <- result$data$weather
            numeric.output <- c("RH2M","T2M","PRECTOTCORR")
            DT::formatRound(DT::datatable(current.predictions, extensions = 'Buttons',
                                          options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                         lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
            ), numeric.output)
            # }
          })
          # metrics table
          output$metadataWeather <-  DT::renderDT({
            if(!inherits(result,"try-error") ){
              # if ( hideAll$clearAll){
              #   return()
              # }else{
              metrics <- result$metadata$weather
              numeric.output <- c("value")
              DT::formatRound(DT::datatable(metrics, extensions = 'Buttons',
                                            options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                           lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
              ), numeric.output)
              # }
            }
          })

          # ## Report tab
          output$reportWeather <- renderUI({
            HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportWeather.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
          })

          output$downloadReportWeather <- downloadHandler(
            filename = function() {
              paste('my-report', sep = '.', switch(
                "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
              ))
            },
            content = function(file) {
              src <- normalizePath(system.file("rmd","reportWeather.Rmd",package="bioflow"))
              src2 <- normalizePath('data/resultWeather.RData')
              # temporarily switch to the temp dir, in case you do not have write
              # permission to the current working directory
              owd <- setwd(tempdir())
              on.exit(setwd(owd))
              file.copy(src, 'report.Rmd', overwrite = TRUE)
              file.copy(src2, 'resultWeather.RData', overwrite = TRUE)
              out <- rmarkdown::render('report.Rmd', params = list(toDownload=TRUE),switch(
                "HTML",
                HTML = rmarkdown::html_document()
              ))
              file.rename(out, file)
            }
          )

        }else{ hideAll$clearAll <- TRUE}

        hideAll$clearAll <- FALSE
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

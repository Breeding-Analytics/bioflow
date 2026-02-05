#' bindObjectApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bindObjectApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    tags$br(),

    navlistPanel( widths = c(2, 10),
                  tabPanel(div("1. Select object(s)" ),
                           column(width=8,
                                  selectInput(
                                    inputId = ns('previous_object_input'),
                                    label   = 'Object Source*: ',
                                    choices = list('Upload from PC' = 'pcfile', 'Upload from cloud' = 'cloudfile'),
                                    width   = '200px'
                                  ),
                                  tags$span(id = ns('previous_object_file_holder'), # from PC
                                            fileInput(
                                              inputId = ns('previous_object_file'),
                                              multiple = TRUE,
                                              label   = NULL,
                                              width   = '400px',
                                              accept  = c('.rds','.RData')
                                            ),
                                  ),
                                  tags$div(id = ns('previous_object_retrieve'), # from cloud
                                           actionButton(ns("refreshPreviousAnalysis"), "Click to retrieve data objects"),
                                  ),
                                  uiOutput(ns('previous_input2'))
                           ),
                           column(width=4,
                                  shinydashboard::box(width = 12, title = span(icon('youtube'), ' Tutorial'), status = "success", solidHeader=FALSE, style = "color: #000000", collapsible = TRUE, collapsed = TRUE,
                                                      h4(strong(span("", tags$a(href="https://www.youtube.com/channel/UCikAyaDKdC5LAtcbVePWgIg", icon("youtube") , target="_blank"), style="color:darkcyan"))),
                                  ),
                           ),
                  ),
                  tabPanel(div("2. Load object(s)" ),

                           column( width=12,
                                   br(),
                                   actionButton(ns("runBind"), "Load object(s)", icon = icon("play-circle")),
                                   # textOutput(ns("outBind")),
                           ),
                           column( width=6,
                                   br(),
                                   DT::DTOutput(ns('summary_data')) ),
                           column( width=6,
                                   br(),
                                   shiny::plotOutput(ns("plotTimeStamps")) ),

                  ),
                  tabPanel(div("* Update columns (optional)" ),

                           tabsetPanel( #widths = c(1, 11),

                             tabPanel( div(icon("seedling"), "Phenotypic" ),
                                       br(),
                                       uiOutput(ns('pheno_map')),
                             ),
                             tabPanel( div(icon("code-fork"), "Pedigree" ),
                                       br(),
                                       uiOutput(ns('ped_map')),
                             ),
                             tabPanel( div(icon("anchor"), "QTL profile" ),
                                       br(),
                                       uiOutput(ns('qtl_map')),
                             ),
                             tabPanel( div(icon("cloud-sun-rain"), "Weather" ),
                                       br(),
                                       uiOutput(ns('weather_map')),
                             )
                           ),

                  ),
    ),

  )
}

#' bindObjectApp Server Functions
#'
#' @noRd
mod_bindObjectApp_server <- function(id, data=NULL, res_auth=NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })
    ############################################################################
    # output$warningMessage <- renderUI(
    #   HTML( as.character(div(style="color: green; font-size: 20px;", "Please proceed to load the data objects that you want to bind.")) )
    # )
    ####################
    ## render timestamps flow
    output$plotTimeStamps <- shiny::renderPlot({
      req(data()) # req(input$version2Sta)
      xx <- data()$status;  yy <- data()$modeling # xx <- result$status;  yy <- result$modeling
      if("analysisIdName" %in% colnames(xx)){existNames=TRUE}else{existNames=FALSE}
      if(existNames){
        networkNames <- paste(xx$analysisIdName, as.character(as.POSIXct(as.numeric(xx$analysisId), origin="1970-01-01", tz="GMT")),sep = "_" )
        xx$analysisIdName <- as.character(as.POSIXct(as.numeric(xx$analysisId), origin="1970-01-01", tz="GMT"))
      }
      v <- which(yy$parameter == "analysisId")
      if(!is.null(xx)){
        if(nrow(xx) > 0){
          if(length(v) > 0){
            yy <- yy[v,c("analysisId","value")]
            zz <- merge(xx,yy, by="analysisId", all.x = TRUE)
          }else{ zz <- xx; zz$value <- NA}
          if(existNames){
            zz$analysisIdName <- cgiarBase::replaceValues(Source = zz$analysisIdName, Search = "", Replace = "?")
            zz$analysisIdName2 <- cgiarBase::replaceValues(Source = zz$value, Search = zz$analysisId, Replace = zz$analysisIdName)
          }
        }
      }

      if(!is.null(xx)){
        if(nrow(xx) > 0){
          if(existNames){
            colnames(zz) <- cgiarBase::replaceValues(colnames(zz), Search = c("analysisIdName","analysisIdName2"), Replace = c("outputId","inputId") )
          }else{
            colnames(zz) <- cgiarBase::replaceValues(colnames(zz), Search = c("analysisId","value"), Replace = c("outputId","inputId") )
          }
          nLevelsCheck1 <- length(na.omit(unique(zz$outputId)))
          nLevelsCheck2 <- length(na.omit(unique(zz$inputId)))
          if(nLevelsCheck1 > 1 & nLevelsCheck2 > 1){
            X <- with(zz, enhancer::overlay(outputId, inputId))
          }else{
            if(nLevelsCheck1 <= 1){
              X1 <- matrix(ifelse(is.na(zz$inputId),0,1),nrow=length(zz$inputId),1); colnames(X1) <- as.character(na.omit(unique(c(zz$outputId))))
            }else{X1 <- model.matrix(~as.factor(outputId)-1, data=zz); colnames(X1) <- levels(as.factor(zz$outputId))}
            if(nLevelsCheck2 <= 1){
              X2 <- matrix(ifelse(is.na(zz$inputId),0,1),nrow=length(zz$inputId),1); colnames(X2) <- as.character(na.omit(unique(c(zz$inputId))))
            }else{X2 <- model.matrix(~as.factor(inputId)-1, data=zz); colnames(X2) <- levels(as.factor(zz$inputId))}
            mynames <- unique(na.omit(c(zz$outputId,zz$inputId)))
            X <- matrix(0, nrow=nrow(zz), ncol=length(mynames)); colnames(X) <- as.character(mynames)
            if(!is.null(X1)){X[,colnames(X1)] <- X1}
            if(!is.null(X2)){X[,colnames(X2)] <- X2}
          };
          rownames(X) <- networkNames
          colnames(X) <- networkNames
          if(existNames){

          }else{
            rownames(X) <-as.character(as.POSIXct(as.numeric(rownames(X)), origin="1970-01-01", tz="GMT"))
            colnames(X) <-as.character(as.POSIXct(as.numeric(colnames(X)), origin="1970-01-01", tz="GMT"))
          }
          # make the network plot
          n <- network::network(X, directed = FALSE)
          network::set.vertex.attribute(n,"family",zz$module)
          network::set.vertex.attribute(n,"importance",1)
          e <- network::network.edgecount(n)
          network::set.edge.attribute(n, "type", sample(letters[26], e, replace = TRUE))
          network::set.edge.attribute(n, "day", sample(1, e, replace = TRUE))
          library(ggnetwork)
          ggplot2::ggplot(n, ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
            ggnetwork::geom_edges(ggplot2::aes(color = family), arrow = ggplot2::arrow(length = ggnetwork::unit(6, "pt"), type = "closed") ) +
            ggnetwork::geom_nodes(ggplot2::aes(color = family), alpha = 0.5, size=5 ) +
            ggnetwork::geom_nodelabel_repel(ggplot2::aes(color = family, label = vertex.names ),
                                            fontface = "bold", box.padding = ggnetwork::unit(1, "lines")) +
            ggnetwork::theme_blank() + ggplot2::ggtitle("Network plot of current analyses available")
        }
      }
    })
    ## render data summary
    output$summary_data <- DT::renderDT({
      req(data())
      DT::datatable(cgiarBase::summaryData(data()),
                    extensions = 'Buttons',
                    options = list(dom = 'lfrtip', # I changed Blfrtip to lfrtip and silenced the buttons
                                   # scrollX = TRUE,
                                   # buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All'))),
                    caption = htmltools::tags$caption(
                      style = 'color:cadetblue; font-weight:bold; font-size: 18px', #caption-side: bottom; text-align: center;
                      htmltools::em('Data summaries')
                    )
      )

    }, server = FALSE)
    ## load the objects
    observeEvent(
      input$previous_object_input,
      if(length(input$previous_object_input) > 0){ # added
        if (input$previous_object_input == 'pcfile') {
          golem::invoke_js('showid', ns('previous_object_file_holder'))
          golem::invoke_js('hideid', ns('previous_object_retrieve'))
          output$previous_input2 <- renderUI({NULL})
        } else if (input$previous_object_input == 'cloudfile') {
          golem::invoke_js('hideid', ns('previous_object_file_holder'))
          golem::invoke_js('showid', ns('previous_object_retrieve'))
        }
      }
    )
    # output$previous_input2 <- renderUI({}) # this 2 lines avoid issues when displaying an uiOutput
    # outputOptions(output, "previous_input2", suspendWhenHidden = FALSE)

    observeEvent(input$refreshPreviousAnalysis,{
      output$previous_input2 <- renderUI({
        selectInput(inputId=ns('previous_input'), label=NULL, choices=dir(file.path(res_auth$repository)), multiple = TRUE)
      })
    })

    observeEvent(input$runBind,{
      req(input$previous_object_input)
      shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')

      if(input$previous_object_input == 'cloudfile'){ # upload from cloud
        req(input$previous_input)
        load( file.path( getwd(),res_auth$repository,input$previous_input[1] ) ) # old dataset
        if(length(input$previous_input) > 1){
          result1 <- result

          for(iFile in 2:length(input$previous_input)){
            load( file.path( getwd(),res_auth$repository,input$previous_input[iFile] ) ) # old dataset
            result2 <- result
            result1 <- try(cgiarBase::bindObjects(object1 = result1,
                                                  object2 = result2
            ), silent = TRUE
            )
          }

          result <- result1
        }else{ iFile=1 }
      }else if(input$previous_object_input == 'pcfile'){ # upload rds
        req(input$previous_object_file)
        load(input$previous_object_file$datapath[1]) # old dataset
        if(length(input$previous_object_file$datapath) > 1){
          result1 <- result
          for(iFile in 2:length(input$previous_object_file$datapath)){
            load(input$previous_object_file$datapath[iFile])  # old dataset
            result2 <- result
            result1 <- try(cgiarBase::bindObjects(object1 = result1,
                                                  object2 = result2
            ), silent = TRUE
            )
          }

          result <- result1
        }else{ iFile=1 }
      }
      ## replace tables

      tmp <- cgiarBase::create_getData_object() #data()

      if(!is.null(data()$user)){tmp$user <- data()$user}
      if(!is.null(result$data)){tmp$data <- result$data}
      if(!is.null(result$metadata)){
        tmp$metadata <- result$metadata
        if(any(c("source","sourceId") %in% tmp$metadata$pheno$parameter)){
          tmp$metadata$pheno <- tmp$metadata$pheno[-which(tmp$metadata$pheno$parameter %in% c("source", "sourceId")), ]
        }
      }
      if(!is.null(result$modifications)){tmp$modifications <- result$modifications}
      if(!is.null(result$predictions)){tmp$predictions <- result$predictions}
      if(!is.null(result$metrics)){tmp$metrics <- result$metrics}
      if(!is.null(result$modeling)){tmp$modeling <- result$modeling}
      if(!is.null(result$status)){
        if("analysisIdName" %in% colnames(result$status)){
          tmp$status <- result$status
        } else{
          tmp$status <- result$status
          tmp$status$analysisIdName <- ""
        }
      }
      if(!is.null(result$GPCP)){tmp$GPCP <- result$GPCP}
      data(tmp) # update data with results
      shinybusy::remove_modal_spinner()

      if(input$previous_object_input == 'cloudfile'){
        if(iFile == 1){
          shinyalert::shinyalert(title = "Success!", text = paste("Dataset:", input$previous_input,"loaded successfully."), type = "success")
        }else{
          shinyalert::shinyalert(title = "Success!", text = paste("Datasets:", paste(input$previous_input, collapse = " and "),"binded successfully. Please proceed to perform your analysis.") , type = "success")
        }
        # cat(paste("Datasets:", paste(input$previous_input, collapse = " and "),"binded successfully. Please proceed to perform your analysis."))
      }else{
        if(iFile == 1){

          shinyalert::shinyalert(title = "Success!", text = paste("Dataset",input$previous_object_file$name,"loaded successfully."), type = "success")
        }else{
          shinyalert::shinyalert(title = "Success!", text = paste("Datasets:", paste(input$previous_object_file$name, collapse = " and "),"binded successfully. Please proceed to perform your analysis.") , type = "success")
        }
        # cat(paste("Datasets","binded successfully. Please proceed to perform your analysis."))
      }
    })

    ####  Phenotypic Data Mapping ####
    mapp <- c("pipeline", "stage", "year", "season","timepoint",
      "country", "location", "trial","study", "management","rep", "iBlock",
      "row", "col", "designation", "gid", "entryType", "trait")

    output$pheno_map <- renderUI({
      req(data())
      req(!is.null(data()$data$pheno))

      header <- colnames(data()$data$pheno)

      labels <- function(x){
        x <- as.character(x)

        if (x %in% c('designation','trait','location')) {
          suffix <- '(*required)'
        } else if (x %in% c('rep','iBlock','row','col')) {
          suffix <- '(*recommended)'
        } else {
          suffix <- '(*optional)'
        }

        color <- ifelse (suffix == '(*required)','red','grey')

        HTML(paste0(x, " <span style='color:", color, "'>", suffix, "</span>"))
      }

      tagList(
        fluidRow(
          lapply(mapp, function(x) {

            column(
              width = 3,
              if(any(c("sourceId", "source") %in% data()$metadata$pheno$parameter)){
                shinyjs::disabled(selectInput(
                  inputId  = ns(paste0("select", x)),
                  label    = labels(x),
                  multiple = ifelse(x == 'trait', TRUE, FALSE),
                  choices  = c("", header),
                  selected = if (x %in% isolate(data()$metadata$pheno$parameter)) {
                    isolate(data()$metadata$pheno$value[data()$metadata$pheno$parameter == x])
                  } else {""}
                ))
              } else{
                selectInput(
                  inputId  = ns(paste0("select", x)),
                  label    = labels(x),
                  multiple = ifelse(x == 'trait', TRUE, FALSE),
                  choices  = c("", header),
                  selected = if (x %in% isolate(data()$metadata$pheno$parameter)) {
                    isolate(data()$metadata$pheno$value[data()$metadata$pheno$parameter == x])
                  } else {""}
                )
              }
            )
          })
        )
      )
    })

    lapply(mapp, function(x) {
      observeEvent(input[[paste0("select", x)]], {
        req(data())
        temp <- data()
        new_val <- input[[paste0("select", x)]]

        if (x == "trait") {
          temp$metadata$pheno <- temp$metadata$pheno[temp$metadata$pheno$parameter != "trait",]
          if (length(new_val) > 0 && any(new_val != "")) {
            for (trait in new_val) {
              temp$metadata$pheno <- rbind(temp$metadata$pheno,
                                           data.frame(parameter="trait", value=trait))
              if (!is.numeric(temp$data$pheno[[trait]])) {
                temp$data$pheno[[trait]] <- as.numeric(gsub(",", "", temp$data$pheno[[trait]]))
              }
            }
          }
          data(temp)
          return(invisible())
        } else{ # is any other column other than trait
          if (length(new_val) == 0 || all(new_val == "")) {
            temp$metadata$pheno <- temp$metadata$pheno[temp$metadata$pheno$parameter != x,]
            data(temp)
            return(invisible())
          }
          if (x %in% temp$metadata$pheno$parameter) { # if x is already in the metadata for pheno
            temp$metadata$pheno$value[temp$metadata$pheno$parameter == x] <- new_val
          } else { # if is not in the metadata
            temp$metadata$pheno <- rbind(temp$metadata$pheno,
                                         data.frame(parameter=x, value=new_val))
          }
          if (x %in% c("designation", "study")) {
            temp$data$pheno[[new_val]] <- stringi::stri_trans_general(temp$data$pheno[[new_val]], "Latin-ASCII")
          }
          data(temp)
        }
      })
    })

    ####  Pedigree Data Mapping ####
    ped_mapp <- c("designation", "mother", "father", "yearOfOrigin")

    output$ped_map <- renderUI({
      req(data())
      req(!is.null(data()$data$pedigree))

      ped_header <- colnames(data()$data$pedigree)

      tagList(
        fluidRow(
          lapply(ped_mapp, function(x) {
            column(
              width = 3,
              if(any(c("sourceId", "source") %in% data()$metadata$pheno$parameter)){
                shinyjs::disabled(selectInput(
                  inputId  = ns(paste0("ped_select", x)),
                  label    = x,
                  multiple = FALSE,
                  choices  = c("", ped_header),
                  selected = if (x %in% isolate(data()$metadata$pedigree$parameter)) {
                    isolate(data()$metadata$pedigree$value[data()$metadata$pedigree$parameter == x])
                  } else {""}
                ))
              } else{
                selectInput(
                  inputId  = ns(paste0("ped_select", x)),
                  label    = x,
                  multiple = FALSE,
                  choices  = c("", ped_header),
                  selected = if (x %in% isolate(data()$metadata$pedigree$parameter)) {
                    isolate(data()$metadata$pedigree$value[data()$metadata$pedigree$parameter == x])
                  } else {""}
                )
              }
            )
          })
        )
      )
    })

    lapply(ped_mapp, function(x) {
      observeEvent(input[[paste0("ped_select", x)]], {
        req(data())
        ped_temp <- data()
        ped_new_val <- input[[paste0("ped_select", x)]]

        if (length(ped_new_val) == 0 || all(ped_new_val == "")) {
          ped_temp$metadata$pedigree <- ped_temp$metadata$pedigree[ped_temp$metadata$pedigree$parameter != x,]
          data(ped_temp)
          return(invisible())
        }
        if (x %in% ped_temp$metadata$pedigree$parameter) { # if x is already in the metadata for pedigree
          ped_temp$metadata$pedigree$value[ped_temp$metadata$pedigree$parameter == x] <- ped_new_val
        } else { # if is not in the metadata
          ped_temp$metadata$pedigree <- rbind(ped_temp$metadata$pedigree,
                                       data.frame(parameter=x, value=ped_new_val))
        }
        if (x == "designation") {
          ped_temp$data$pedigree[[ped_new_val]] <- stringi::stri_trans_general(ped_temp$data$pedigree[[ped_new_val]], "Latin-ASCII")
        }
        data(ped_temp)
      })
    })

    ####  QTL Profile Data Mapping ####
    qtl_mapp <- c("designation","qtl")

    output$qtl_map <- renderUI({
      req(data())
      req(!is.null(data()$data$qtl))

      qtl_header <- colnames(data()$data$qtl)

      tagList(
        fluidRow(
          lapply(qtl_mapp, function(x) {
            column(
              width = ifelse(x == 'qtl', 9, 3),
              if(any(c("sourceId", "source") %in% data()$metadata$pheno$parameter)){
                shinyjs::disabled(selectInput(
                  inputId  = ns(paste0("qtl_select", x)),
                  label    = x,
                  multiple = ifelse(x == 'qtl', TRUE, FALSE),
                  choices  = c("", qtl_header),
                  selected = if (x %in% isolate(data()$metadata$qtl$parameter)) {
                    isolate(data()$metadata$qtl$value[data()$metadata$qtl$parameter == x])
                  } else {""},
                  width = "100%"
                ))
              } else{
                selectInput(
                  inputId  = ns(paste0("qtl_select", x)),
                  label    = x,
                  multiple = ifelse(x == 'qtl', TRUE, FALSE),
                  choices  = c("", qtl_header),
                  selected = if (x %in% isolate(data()$metadata$qtl$parameter)) {
                    isolate(data()$metadata$qtl$value[data()$metadata$qtl$parameter == x])
                  } else {""},
                  width = "100%"
                )
              }

            )
          })
        )
      )
    })

    lapply(qtl_mapp, function(x) {
      observeEvent(input[[paste0("qtl_select", x)]], {
        req(data())
        qtl_temp <- data()
        qtl_new_val <- input[[paste0("qtl_select", x)]]

        if (x == "qtl") {
          qtl_temp$metadata$qtl <- qtl_temp$metadata$qtl[qtl_temp$metadata$qtl$parameter != "qtl",]
          if (length(qtl_new_val) > 0 && any(qtl_new_val != "")) {
            for (qtl in qtl_new_val) {
              qtl_temp$metadata$qtl <- rbind(qtl_temp$metadata$qtl,
                                           data.frame(parameter="qtl", value=qtl))
            }
          }
          data(qtl_temp)
          return(invisible())
        } else{ # is any other column other than trait
          if (length(qtl_new_val) == 0 || all(qtl_new_val == "")) {
            qtl_temp$metadata$qtl <- qtl_temp$metadata$qtl[qtl_temp$metadata$qtl$parameter != x,]
            data(qtl_temp)
            return(invisible())
          }
          if (x %in% qtl_temp$metadata$qtl$parameter) { # if x is already in the metadata for pheno
            qtl_temp$metadata$qtl$value[qtl_temp$metadata$qtl$parameter == x] <- qtl_new_val
          } else { # if is not in the metadata
            qtl_temp$metadata$qtl <- rbind(qtl_temp$metadata$qtl,
                                         data.frame(parameter=x, value=qtl_new_val))
          }
          if (x == "designation") {
            qtl_temp$data$qtl[[qtl_new_val]] <- stringi::stri_trans_general(qtl_temp$data$qtl[[qtl_new_val]], "Latin-ASCII")
          }
          data(qtl_temp)
        }
      })
    })

    ####  Weather Data Mapping ####
    weather_mapp <- c("environment", "latitude", "longitude", "year", "month", "day", "date", "trait")

    output$weather_map <- renderUI({
      req(data())
      req(!is.null(data()$data$weather))

      weather_header <- colnames(data()$data$weather)

      weather_labels <- function(x){
        x <- as.character(x)

        if (x %in% c('year', 'month', 'day')) {
          suffix <- '(*recommended)'
        } else if (x %in% c('date')) {
          suffix <- '(*optional)'
        } else {
          suffix <- '(*required)'
        }

        color <- ifelse (suffix == '(*required)','red','grey')

        HTML(paste0(x, " <span style='color:", color, "'>", suffix, "</span>"))
      }

      tagList(
        fluidRow(
          lapply(weather_mapp, function(x) {
            column(
              width = 3,
              if(any(c("sourceId", "source") %in% data()$metadata$pheno$parameter)){
                shinyjs::disabled(selectInput(
                  inputId  = ns(paste0("weather_select", x)),
                  label    = weather_labels(x),
                  multiple = ifelse(x == 'trait', TRUE, FALSE),
                  choices  = c("", weather_header),
                  selected = if (x %in% isolate(data()$metadata$weather$parameter)) {
                    isolate(data()$metadata$weather$value[data()$metadata$weather$parameter == x])
                  } else {""}
                ))
              } else{
                selectInput(
                  inputId  = ns(paste0("weather_select", x)),
                  label    = weather_labels(x),
                  multiple = ifelse(x == 'trait', TRUE, FALSE),
                  choices  = c("", weather_header),
                  selected = if (x %in% isolate(data()$metadata$weather$parameter)) {
                    isolate(data()$metadata$weather$value[data()$metadata$weather$parameter == x])
                  } else {""}
                )
              }
            )
          })
        )
      )
    })

    lapply(weather_mapp, function(x) {
      observeEvent(input[[paste0("weather_select", x)]], {
        req(data())
        weather_temp <- data()
        weather_new_val <- input[[paste0("weather_select", x)]]

        if (x == "trait") {
          weather_temp$metadata$weather <- weather_temp$metadata$weather[weather_temp$metadata$weather$parameter != "trait",]
          if (length(weather_new_val) > 0 && any(weather_new_val != "")) {
            for (trait in weather_new_val) {
              weather_temp$metadata$weather <- rbind(weather_temp$metadata$weather,
                                           data.frame(parameter="trait", value=trait))
              if (!is.numeric(weather_temp$data$weather[[trait]])) {
                weather_temp$data$weather[[trait]] <- as.numeric(gsub(",", "", weather_temp$data$weather[[trait]]))
              }
            }
          }
          data(weather_temp)
          return(invisible())
        } else{ # is any other column other than trait
          if (length(weather_new_val) == 0 || all(weather_new_val == "")) {
            weather_temp$metadata$weather <- weather_temp$metadata$weather[weather_temp$metadata$weather$parameter != x,]
            data(weather_temp)
            return(invisible())
          }
          if (x %in% weather_temp$metadata$weather$parameter) { # if x is already in the metadata for pheno
            weather_temp$metadata$weather$value[weather_temp$metadata$weather$parameter == x] <- weather_new_val
          } else { # if is not in the metadata
            weather_temp$metadata$weather <- rbind(weather_temp$metadata$weather,
                                         data.frame(parameter=x, value=weather_new_val))
          }
          data(weather_temp)
        }
      })
    })

  })
}

## To be copied in the UI
# mod_bindObjectApp_ui("bindObjectApp_1")

## To be copied in the server
# mod_bindObjectApp_server("bindObjectApp_1")

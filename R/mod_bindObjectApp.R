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
                                           uiOutput(ns('previous_input2')),
                                  ),
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
                                   textOutput(ns("outBind")),
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

                             tabPanel( div("Phenotype metadata" ),
                                       br(),
                                       uiOutput(ns('pheno_map')),
                             ),
                             # tabPanel( div("Phenotype metadata" ),
                             #           br(),
                             #           uiOutput(ns('pheno_map')),
                             # ),
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
      if(length(v) > 0){
        yy <- yy[v,c("analysisId","value")]
        zz <- merge(xx,yy, by="analysisId", all.x = TRUE)
      }else{ zz <- xx; zz$value <- NA}
      if(existNames){
        zz$analysisIdName <- cgiarBase::replaceValues(Source = zz$analysisIdName, Search = "", Replace = "?")
        zz$analysisIdName2 <- cgiarBase::replaceValues(Source = zz$value, Search = zz$analysisId, Replace = zz$analysisIdName)
      }
      if(!is.null(xx)){
        if(existNames){
          colnames(zz) <- cgiarBase::replaceValues(colnames(zz), Search = c("analysisIdName","analysisIdName2"), Replace = c("outputId","inputId") )
        }else{
          colnames(zz) <- cgiarBase::replaceValues(colnames(zz), Search = c("analysisId","value"), Replace = c("outputId","inputId") )
        }
        nLevelsCheck1 <- length(na.omit(unique(zz$outputId)))
        nLevelsCheck2 <- length(na.omit(unique(zz$inputId)))
        if(nLevelsCheck1 > 1 & nLevelsCheck2 > 1){
          X <- with(zz, sommer::overlay(outputId, inputId))
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
        } else if (input$previous_object_input == 'cloudfile') {
          golem::invoke_js('hideid', ns('previous_object_file_holder'))
          golem::invoke_js('showid', ns('previous_object_retrieve'))
        }
      }
    )
    output$previous_input2 <- renderUI({}) # this 2 lines avoid issues when displaying an uiOutput
    outputOptions(output, "previous_input2", suspendWhenHidden = FALSE)
    observeEvent( # this is the part where we either load the previous analysis from cloud or PC
      c(input$previous_object_input),
      {
        previousFilesAvailable <- eventReactive(input$refreshPreviousAnalysis, { #
          selectInput(inputId=ns('previous_input'), label=NULL, choices=dir(file.path(res_auth$repository)), multiple = TRUE)
        })
        output$previous_input2 <- renderPrint({  previousFilesAvailable()    })

        outBind <- eventReactive(input$runBind, {

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
          tmp <- data()
          if(!is.null(result$data)){tmp$data <- result$data}
          if(!is.null(result$metadata)){tmp$metadata <- result$metadata}
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
        }) ## end eventReactive
        output$outBind <- renderPrint({
          outBind()
        })
      }
    )


    output$pheno_map <- renderUI({

      req(data())

      if (is.null(data()$data$pheno)){
        return(NULL)
      }

      header <- colnames(data()$data$pheno)
      mapp <- c("pipeline", "stage", "year", "season","timepoint",
                "country", "location", "trial","study", "management","rep", "iBlock",
                "row", "col", "designation", "gid", "entryType", "trait")
      pheno_map <- lapply(mapp, function(x) {
        column(3,
               selectInput(
                 inputId  = ns(paste0('select', x)),
                 label    = HTML(ifelse(x %in% c('designation','trait','location'), as.character(p(x, span('(*required)',style="color:red"))),  ifelse(x %in% c('rep','iBlock','row','col'), as.character(p(x, span('(*recommended)',style="color:grey"))), as.character(p(x, span('(*optional)',style="color:grey")))  )   ) ) ,
                 multiple = ifelse(x == 'trait', TRUE, FALSE),
                 choices  = as.list(c('', header )),
                 selected = if( x %in% data()$metadata$pheno$parameter ){data()$metadata$pheno$value[which(data()$metadata$pheno$parameter %in% x)]}else{''}
               ),

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
                 # if (x == 'designation') {
                 #   if(input[[paste0('select', x)]] != ''){
                 #     temp$data$pedigree <- data.frame(designation = unique(pheno_data()[[input[[paste0('select', x)]]]]), mother = NA, father = NA, yearOfOrigin = NA)
                 #     temp$metadata$pedigree <- data.frame(parameter=c("designation","mother","father","yearOfOrigin"), value=c("designation","mother","father","yearOfOrigin") )
                 #   }
                 # }
                 data(temp)
               }),
        )
      })
      fluidRow(do.call(tagList, pheno_map))
    })


  })
}

## To be copied in the UI
# mod_bindObjectApp_ui("bindObjectApp_1")

## To be copied in the server
# mod_bindObjectApp_server("bindObjectApp_1")

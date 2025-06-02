#' expDesignEditApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_expDesignEditApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::mainPanel(width = 12,
                     tabsetPanel( #width=9,
                       type = "tabs",

                       tabPanel(div(icon("book"), "Information") ,
                                br(),
                                column(width = 6,
                                       h1(strong(span("Experimental Design Factor Filtering Module", style="color:darkcyan"))),
                                       h2(strong("Data Status (wait to be displayed):")),
                                       uiOutput(ns("warningMessage")),
                                ),
                                column(width = 6,
                                       tags$body(
                                         h2(strong("Details")),
                                         p("Sometimes is required to set to missing experimental design factors that we are aware that are not correctly saved
                                                               in the databases. Although the tight solution would be to fix this information in the original database, a pragmatic
                                                               approach is to set certain factors from an especific environment to missing so it is ignored in the model fitting or
                                                               any other analytical module using this information."),
                                         # img(src = "www/qaRaw.png", height = 300, width = 600), # add an image
                                         p(strong("Editing table-")," there is not much complexity of how to use this module. There is a table with a column for
                                                               each experimental design factor and a row for each environment. By default this table is filled with the number of levels
                                                               wherever this information is available. If the user wants to silence a particular factor it just needs to double click in
                                                               the cell and set the value to zero."),
                                       )
                                ),
                       ),
                       tabPanel(div(icon("arrow-right-to-bracket"), "Input steps"),
                                tabsetPanel(
                                  tabPanel(div(  icon("dice-one"), "Pick factor(s)", icon("arrow-right") ), # icon = icon("dice-one"),
                                           br(),
                                           column(width = 6, style = "overflow-y: scroll;overflow-x: scroll;", # height:300px;
                                           tags$span(id = ns('holder1'),
                                                     p(span("The experimental design factors (columns) present in a particular environment (rows) are displayed in the table below. Please double click in any cell (environment by factor combination) that you would like to silence by setting the value to zero. Then run the analysis to save those modifications for posterior analyses.", style="color:black")),
                                           ),
                                           DT::dataTableOutput(ns("transTableC")),
                                           ),

                                           column(width=6,
                                                  column(width=12),
                                                  shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                      hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                      h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameter values to be specified in the grey boxes above.", style="color:green"))),
                                                                      hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                      p(span("Heatmap to explore the spatial distribution of factors and traits. Row and column information need to be mapped for this visualization to properly display.", style="color:black")),
                                                                      tags$span(id = ns('holder2'),
                                                                                column(width = 4, selectInput(ns("fieldinstCleaned3Dtraits"), "Environment to visualize", choices = NULL, multiple = FALSE) ),
                                                                                column(width = 4, selectInput(ns("zaxisCleaned3Dtraits"), "Color field by", choices = NULL, multiple = FALSE) ),
                                                                                column(width = 4, selectInput(ns("textCleaned3Dtraits"), "Text cells by", choices = NULL, multiple = FALSE) ),
                                                                                column(width=12,
                                                                                       plotly::plotlyOutput(ns("plotCleaned3Dtraits")) ,
                                                                                       # style = "height:300px; overflow-y: scroll;overflow-x: scroll;"
                                                                                ),
                                                                      ),
                                                  ),
                                           ),
                                  ),
                                  tabPanel("Run analysis", icon = icon("dice-two"),
                                           br(),
                                           column(width=12,style = "background-color:grey; color: #FFFFFF",
                                                  column(width=3, tags$div(textInput(ns("analysisIdName"), label = tags$span(
                                                    "Analysis Name (optional)", tags$i( class = "glyphicon glyphicon-info-sign", style = "color:#FFFFFF",
                                                                                        title = "An optional name for the analysis besides the timestamp if desired.") ), #width = "100%",
                                                    placeholder = "(optional name)") ) ),
                                                  column(width=3,
                                                         br(),
                                                         actionButton(ns("runFieldClean"), "Tag factors", icon = icon("play-circle")),
                                                         textOutput(ns("outExp"))
                                                  ),
                                           ),
                                  ),
                                ) # end of tabset
                       ),# end of input panel
                       tabPanel(div(icon("arrow-right-from-bracket"), "Output tabs" ) , value = "outputTabs",
                                tabsetPanel(
                                  tabPanel("Dashboard", icon = icon("file-image"),
                                           br(),
                                           downloadButton(ns("downloadReportQaPheno"), "Download dashboard"),
                                           br(),
                                           uiOutput(ns('reportQaPheno'))
                                  ),
                                ),
                       ), # end of output panel# end of output panel
                     )
    ) # end mainpanel
  )
}

#' expDesignEditApp Server Functions
#'
#' @noRd
mod_expDesignEditApp_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })
    ############################################################################
    # show shinyWidgets until the user can use the module
    observeEvent(c(data() ), {
      req(data())
      mappedColumns <- length(which(c("environment","designation","trait","rep","iBlock","row","col") %in% data()$metadata$pheno$parameter))
      if(mappedColumns > 3){
        golem::invoke_js('showid', ns('holder1'))
        golem::invoke_js('showid', ns('holder2'))
      }else{
        golem::invoke_js('hideid', ns('holder1'))
        golem::invoke_js('hideid', ns('holder2'))
      }
    })
    ############################################################################
    # warning message
    output$warningMessage <- renderUI(
      if(is.null(data())){
        HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your phenotypic data using the 'Data Retrieval' tab.")) )
      }else{ # data is there
        ## pheno check
        available <- data()$metadata$pheno[data()$metadata$pheno$parameter %in% c("row","col","iBlock","rep"), "value"]
        available <- setdiff(available,"")
        if( length(available) == 0 ){
          HTML( as.character(div(style="color: red; font-size: 20px;", "Please map some of your experimental design columns using the 'Data Retrieval' tab in the 'Phenotype' section to use this module.")) )
        }else{
          HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to inspect the options.")) )
        }
      }
    )
    # update color by
    observeEvent(c(data()), {
      req(data())
      if("environment" %in% colnames(data()$data$pheno)){
        available <- data()$metadata$pheno[data()$metadata$pheno$parameter %in% c("row","col","iBlock","rep","trait"), "value"]
        traitsMta <- setdiff(available,"")
        updateSelectInput(session, "zaxisCleaned3Dtraits", choices = traitsMta)
      }

    })
    # update text by
    observeEvent(c(data()), {
      req(data())
      if("environment" %in% colnames(data()$data$pheno)){
        available <- data()$metadata$pheno[data()$metadata$pheno$parameter %in% c("row","col","iBlock","rep","designation"), "value"]
        traitsMta <- setdiff(available,"")
        updateSelectInput(session, "textCleaned3Dtraits", choices = traitsMta)
      }
    })
    # update environments by
    observeEvent(c(data()), {
      req(data())
      dtSta <- data()$data$pheno
      paramsPheno <- data()$metadata$pheno
      if("environment" %in% colnames(data()$data$pheno)){
        if(is.null(paramsPheno)){
          traitsMta <- NULL
        }else{
          paramsPheno <- paramsPheno[which(paramsPheno$parameter != "trait"),]
          if("environment" %in% paramsPheno$parameter){ # good to go
            if(!is.null(dtSta)){
              colnames(dtSta) <- cgiarBase::replaceValues(colnames(dtSta), Search = paramsPheno$value, Replace = paramsPheno$parameter )
              traitsMta <- na.omit(unique(dtSta[,"environment"]))
            }else{traitsMta <- NULL}
          }else{
            traitsMta <- NULL
          }
        }
        updateSelectInput(session, "fieldinstCleaned3Dtraits", choices = traitsMta)
      }
    })

    ##produce the plot
    output$plotCleaned3Dtraits <- plotly::renderPlotly({
      req(data())
      req(input$fieldinstCleaned3Dtraits)
      req(input$zaxisCleaned3Dtraits)
      req(input$textCleaned3Dtraits)
      mydata <- data()$data$pheno
      paramsPheno <- data()$metadata$pheno
      envFeature <- paramsPheno[which(paramsPheno$parameter == "environment"),"value"]
      # paramsPheno <- paramsPheno[which(paramsPheno$parameter != "trait"),]
      # colnames(mydata) <- cgiarBase::replaceValues(colnames(mydata), Search = paramsPheno$value, Replace = paramsPheno$parameter )
      mydata <- mydata[which(mydata[,envFeature] %in% input$fieldinstCleaned3Dtraits ),]
      classesInData <- lapply(mydata,class)

      if(classesInData[input$zaxisCleaned3Dtraits] %in% c("character","factor")){
        z <- as.numeric(as.factor(mydata[,input$zaxisCleaned3Dtraits]))
      }else{z <- mydata[,input$zaxisCleaned3Dtraits]}

      if(!is.na( stats::var(z, na.rm = TRUE) )){ # if we are good to go
        rowFeature <- paramsPheno[which(paramsPheno$parameter == "row"),"value"]
        rowFeature <- setdiff(rowFeature, "")
        colFeature <- paramsPheno[which(paramsPheno$parameter == "col"),"value"]
        colFeature <- setdiff(colFeature, "")
        if(length(c(rowFeature,colFeature)) == 2){
          fig <- plotly::plot_ly(mydata, x = mydata[,rowFeature], y = mydata[,colFeature],
                                 z = z, text = mydata[,input$textCleaned3Dtraits]
          )
          fig <- fig %>% plotly::add_heatmap()
          fig <- fig %>% plotly::layout(scene = list(
            zaxis = list(title = input$zaxisCleaned3Dtraits)))
          fig
        }else{
          fig <- plotly::plot_ly(x=1,y=1,name="No row and column information available for this field")
        }
      }
      # just a test
    })


    ## our function to keep track of reps, rows, cols that should be set to NA for a given field
    dtFieldTraC = reactive({
      req(data())
      object <- data()
      dtProv = object$data$pheno
      paramsPheno <- object$metadata$pheno
      if("environment" %in% colnames(dtProv)){ # if environment was computed
        if(!is.null(paramsPheno) & !is.null(dtProv)){
          paramsPheno <- paramsPheno[which(paramsPheno$parameter != "trait"),]
          colnames(dtProv) <- cgiarBase::replaceValues(colnames(dtProv), Search = paramsPheno$value, Replace = paramsPheno$parameter )
          dtProvNames <- c("row","col","rep","iBlock")
          envCol <- paramsPheno[which(paramsPheno$parameter == "environment"),"value"]
          if(length(envCol) > 0){
            fieldNames <- as.character(unique(dtProv[,"environment"]))
            spD <- split(dtProv,dtProv[,"environment"])
            presentFactors <- paramsPheno$parameter[which(paramsPheno$parameter %in% dtProvNames)]
            presentFactorsPerField <- lapply(spD, function(x){
              apply(x[,presentFactors, drop=FALSE], 2, function(y){length(unique(y))})
            })
            presentFactorsPerField <- do.call(rbind, presentFactorsPerField)
            dtProvTable = as.data.frame(presentFactorsPerField);  rownames(dtProvTable) <- fieldNames
            return(dtProvTable)
          }
        }
      }
    })
    xx = reactiveValues(df = NULL)
    observe({
      df <- dtFieldTraC()
      xx$df <- df
    })
    output$transTableC = DT::renderDT(xx$df,
                                      selection = 'none',
                                      editable = TRUE,
                                      server = FALSE,
                                      options = list(#paging=FALSE,
                                                     #searching=FALSE,
                                                     scrollX = TRUE
                                                     #initComplete = I("function(settings, json) {alert('Done.');}")
                                      )
    )
    proxy = DT::dataTableProxy('transTableC')
    observeEvent(input$transTableC_cell_edit, {
      info = input$transTableC_cell_edit
      utils::str(info)
      i = info$row
      j = info$col
      v = info$value
      xx$df[i, j] <- isolate(DT::coerceValue(v, xx$df[i, j]))
    })
    ## save when user clicks

    outExp <- eventReactive(input$runFieldClean, {

      if(is.null(data())){
        cat( "Please retrieve or load your phenotypic data using the 'Data Retrieval' tab.")
      }else{ # data is there
        ## pheno check
        shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
        available <- data()$metadata$pheno[data()$metadata$pheno$parameter %in% c("row","col","iBlock","rep"), "value"]
        available <- setdiff(available,"")
        if( length(available) == 0 ){
          cat( "Please map some of your experimental design columns using the 'Data Retrieval' tab in the 'Phenotype' section to use this module.")
        }else{
          object <- data()
          result <- cgiarPipeline::modifExpDesign(object, df=xx$df)
          aid <- result$status$analysisId[length(result$status$analysisId)]
          # result$modifications$pheno[result$modifications$pheno$analysisId == aid,"module"]
          if("analysisIdName" %in% colnames(result$status)){result$status$analysisIdName[nrow(result$status)] <- input$analysisIdName}
          data(result)
          cat(paste("QA step with id:",as.POSIXct( aid, origin="1970-01-01", tz="GMT"),"saved."))
        }
        shinybusy::remove_modal_spinner()

        if(!inherits(result,"try-error")) { # if all goes well in the run
          # ## Report tab
          output$reportQaPheno <- renderUI({
            HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportQaPheno.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
          })

          output$downloadReportQaPheno <- downloadHandler(
            filename = function() {
              paste(paste0('qaDesign_dashboard_',gsub("-", "", as.integer(Sys.time()))), sep = '.', switch(
                "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
              ))
            },
            content = function(file) {
              shinybusy::show_modal_spinner(spin = "fading-circle", text = "Generating Report...")

              src <- normalizePath(system.file("rmd","reportQaPheno.Rmd",package="bioflow"))
              src2 <- normalizePath('data/resultQaPheno.RData')

              # temporarily switch to the temp dir, in case you do not have write
              # permission to the current working directory
              owd <- setwd(tempdir())
              on.exit(setwd(owd))

              file.copy(src, 'report.Rmd', overwrite = TRUE)
              file.copy(src2, 'resultQaPheno.RData', overwrite = TRUE)

              out <- rmarkdown::render('report.Rmd', params = list(toDownload=TRUE),switch(
                "HTML",
                HTML = rmdformats::robobook(toc_depth = 4)
                # HTML = rmarkdown::html_document()
              ))

              # wait for it to land on disk (safety‐net)
              wait.time <- 0
              while (!file.exists(out) && wait.time < 60) {
                Sys.sleep(1); wait.time <- wait.time + 1
              }

              file.rename(out, file)
              shinybusy::remove_modal_spinner()
            }
          )

        }else{ hideAll$clearAll <- TRUE}

        hideAll$clearAll <- FALSE
      }

    })
    output$outExp <- renderPrint({
      outExp()
    })




  })
}

## To be copied in the UI
# mod_expDesignEditApp_ui("expDesignEditApp_1")

## To be copied in the server
# mod_expDesignEditApp_server("expDesignEditApp_1")

#' dataConsistApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dataConsistApp_ui <- function(id){
  ns <- NS(id)
  tagList(


    shiny::mainPanel(width = 12,
                     tabsetPanel( id=ns("tabsMain"),
                       type = "tabs",

                       tabPanel(div(icon("book"), "Information") ,
                                br(),
                                column(width = 6,
                                       h1(strong(span("Data Consistency QA Module", style="color:darkcyan"))),
                                       h2(strong("Data Status (wait to be displayed):")),
                                       uiOutput(ns("warningMessage")),
                                       tags$br(),
                                       # column(width=4, tags$br(),
                                       shinyWidgets::prettySwitch( inputId = ns('launch'), label = "Load example dataset", status = "success"),
                                       # ),
                                       tags$br(),
                                ),
                                column(width = 6,
                                       tags$body(
                                         h2(strong("Details")),
                                         p("Some additional checks for data consistency are benefitial to ensure a high quality analysis. These can include:"),
                                         # img(src = "www/qaRaw.png", height = 300, width = 600), # add an image
                                         p(strong("Check names-")," The onthology for some crops is particularly important. This tab allows to check this to a reference onthology."),
                                         p(strong("Check traits-")," Checking the relationships between traits is particularly useful to identify issues."),
                                         p(strong("Tag inconsistencies-"),"Once inconsistencies are identified, is important to tag them for posterior management in the analytical modules."),
                                         h2(strong("References")),
                                         h2(strong("Software used")),
                                         p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Consisttistical Computing,
                                Vienna, Austria. URL https://www.R-project.org/."),
                                       )
                                ),
                       ),
                       tabPanel(div(icon("arrow-right-to-bracket"), "Input steps"),
                                tabsetPanel(
                                  tabPanel(div( icon("dice-one"), "Select crop", icon("arrow-right") ) , # icon = icon("dice-one"),
                                           br(),
                                           column(width=12,style = "background-color:grey; color: #FFFFFF",
                                                  column(width=4, selectInput(ns("cropConsist"), "Select your crop filter:", choices = list(Banana="banana", Beans="beans",  Cassava="cassava", Maize="maize", PearMillet="pmillet", Plantain="plantain", Potato="potato", Rice="rice", Soybean="soybean", SweetPotato="spotato", Sorghum="sorghum", Wheat="wheat"), multiple = FALSE) ),

                                           ),
                                           br(),
                                           br(),
                                           br(),
                                           br(),
                                           DT::DTOutput(ns("phenoConsist")),
                                  ),
                                  tabPanel(div(icon("dice-two"), "Design and extreme value", icon("arrow-right") ) ,
                                           br(),
                                           column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                  column(width = 4,
                                                         numericInput(ns("fConsist"),
                                                                      label = tags$span("IQR coefficient",
                                                                                        tags$i(class = "glyphicon glyphicon-info-sign",
                                                                                               style = "color:#FFFFFF",
                                                                                               title = "Number of IQRs to detect extreme values")),
                                                                      value = 5, max = 100000, min = 0, step = 1)),
                                                  column(width = 4,
                                                         selectInput(ns("designConsist"),
                                                                     "Design for residuals calculation",
                                                                     choices = list(None = "none", RCBD = "rcbd", MET = "met"),
                                                                     multiple = FALSE)),
                                                  column(width = 4,
                                                         numericInput(ns("maxresConsist"),
                                                                      label = tags$span("Residual threshold",
                                                                                        tags$i(class = "glyphicon glyphicon-info-sign",
                                                                                               style = "color:#FFFFFF",
                                                                                               title = "Number of standardized residuals to detect outliers")),
                                                                      value = 4, max = 100000, min = 0, step = 1)),
                                           ),
                                           br(),
                                           column(width=12),
                                           shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                               column(width=12,
                                                                      hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                      h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameters to be specified in the grey boxes above.", style="color:green"))),
                                                                      hr(style = "border-top: 3px solid #4c4c4c;"),
                                                               ),
                                                               column(width=12, p(span("Matrix of issues (transposed dataset).", style="color:black")) ),
                                                               column(width=6, sliderInput(ns("slider1"), label = "Records", min = 1, max = 2000, value = c(1, 200)) ),
                                                               column(width=6, sliderInput(ns("slider2"), label = "Columns/traits", min = 1, max = 500, value = c(1, 250)) ),
                                                               column(width=12, shiny::plotOutput(ns("plotConsistencySparsity")) ),
                                           ),
                                  ),
                                  tabPanel("Run analysis", icon = icon("dice-three"),
                                           br(),
                                           column(width=12,style = "background-color:grey; color: #FFFFFF",
                                                  column(width=3, tags$div(textInput(ns("analysisIdName"), label = tags$span(
                                                    "Analysis Name (optional)", tags$i( class = "glyphicon glyphicon-info-sign", style = "color:#FFFFFF",
                                                                                        title = "An optional name for the analysis besides the timestamp if desired.") ), #width = "100%",
                                                    placeholder = "(optional name)") ) ),
                                                  column(width=3,style = "background-color:grey; color: #FFFFFF",
                                                         br(),
                                                         actionButton(ns("runConsist"), "Run consistency check", icon = icon("play-circle")),
                                                         uiOutput(ns("qaQcConsistInfo")),
                                                         br(),
                                                  ),
                                           ),
                                           textOutput(ns("outConsist")),
                                  ),
                                )# of of tabsetPanel
                       ),
                       tabPanel(div(icon("arrow-right-from-bracket"), "Output tabs" ) , value = "outputTabs",
                                tabsetPanel(
                                  tabPanel("Dashboard", icon = icon("file-image"),
                                           br(),
                                           downloadButton(ns("downloadReportQaPheno"), "Download dashboard"),
                                           br(),
                                           uiOutput(ns('reportQaPheno')),
                                  ),
                                ) # of of tabsetPanel
                       )# end of output panel


                     )) # end mainpanel


  )
}

#' dataConsistApp Server Functions
#'
#' @noRd
mod_dataConsistApp_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })
    ######################################
    # warning message
    output$warningMessage <- renderUI(
      if(is.null(data())){
        HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your phenotypic data using the 'Data Retrieval' tab.")) )
      }else{ # data is there
        mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
        if(mappedColumns == 3){ HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to identify outliers specifying your input parameters under the 'Input' tabs.")) )
        }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please make sure that you have computed the 'environment' column, and that column 'designation' and \n at least one trait have been mapped using the 'Data Retrieval' tab.")) )
        }
      }
    )

    ## data example loading
    observeEvent(
      input$launch,
      if(length(input$launch) > 0){
        if (input$launch) {
          shinyWidgets::ask_confirmation(
            inputId = ns("myconfirmation"),
            text = "Are you sure you want to load the example data? This will delete any data currently in the environment.",
            title = "Data replacement warning"
          )
        }
      }
    )
    observeEvent(input$myconfirmation, {
      if (isTRUE(input$myconfirmation)) {
        shinybusy::show_modal_spinner('fading-circle', text = 'Loading example...')
        ## replace tables
        tmp <- data()
        data(cgiarBase::create_getData_object())
        utils::data(DT_example, package = "cgiarPipeline")
        if(!is.null(result$data)){tmp$data <- result$data}
        if(!is.null(result$metadata)){tmp$metadata <- result$metadata}
        if(!is.null(result$modifications)){tmp$modifications <- result$modifications}
        if(!is.null(result$predictions)){tmp$predictions <- result$predictions}
        if(!is.null(result$metrics)){tmp$metrics <- result$metrics}
        if(!is.null(result$modeling)){tmp$modeling <- result$modeling}
        if(!is.null(result$status)){tmp$status <- result$status}
        data(tmp) # update data with results
        shinybusy::remove_modal_spinner()
      }else{
        shinyWidgets::updatePrettySwitch(session, "launch", value = FALSE)
      }
    }, ignoreNULL = TRUE)

    ####################################################
    ## INPUT VISUALIZATIONS

    ## Data consistency visualization plot
    output$plotConsistencySparsity <- shiny::renderPlot({
      req(data())
      dtMta <- data()

      if(input$cropConsist == "spotato"){
        main <- "Sweet potato consistency flags"
        out <- st4gi::check.data(dfr = st4gi::convert.co(dtMta$data$pheno, 'co.to.labels', crop = 'sp'),
                                 f = input$fConsist,
                                 out.mod = input$designConsist,
                                 out.max = input$maxresConsist,
                                 print.text = FALSE,
                                 crop = 'sp')$Inconsist.Matrix
      }else if(input$cropConsist == "potato"){
        main <- "Potato consistency flags"
        out <- st4gi::check.data(dfr = st4gi::convert.co(dtMta$data$pheno, 'co.to.labels', crop = 'pt'),
                                 f = input$fConsist,
                                 out.mod = input$designConsist,
                                 out.max = input$maxresConsist,
                                 print.text = FALSE,
                                 crop = 'pt')$Inconsist.Matrix
      }else{
        main <- "Crop not available, no consistency flags"
        out <- matrix(0, nrow=nrow(dtMta$data$pheno), ncol=ncol(dtMta$data$pheno))
      }

      M2 <- as.matrix(out)
      M2 <- M2[,(input$slider2[1]):min(c(input$slider2[2], ncol(M2) )), drop=FALSE] # environments
      M2 <- M2[(input$slider1[1]):min(c(input$slider1[2]), nrow(M2) ), ,drop=FALSE] # genotypes
      Matrix::image(as(M2, Class = "dgCMatrix"), xlab="Columns/traits", ylab="Records", main=main, colorkey=TRUE)
    })

    ## pheno data TABLE
    output$phenoConsist <-  DT::renderDT({
      req(data())
      mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
      if(mappedColumns == 3){ # all required columns are present
        dtSta <- data()
        # req(input$version2Sta)
        dtSta <- dtSta$data$pheno
        ### change column names for mapping
        paramsPheno <- data()$metadata$pheno
        paramsPheno <- paramsPheno[which(paramsPheno$parameter != "trait"),]
        colnames(dtSta) <- cgiarBase::replaceValues(colnames(dtSta), Search = paramsPheno$value, Replace = paramsPheno$parameter )
        ###
        traitTypes <- unlist(lapply(dtSta,class))
        numeric.output <- names(traitTypes)[which(traitTypes %in% "numeric")]
        DT::formatRound(DT::datatable(dtSta, extensions = 'Buttons',
                                      options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                     lengthMenu = list(c(5,20,50,-1), c(5,20,50,'All'))),
                                      caption = htmltools::tags$caption(
                                        style = 'color:cadetblue', #caption-side: bottom; text-align: center;
                                        htmltools::em('Raw phenotypic data to be used as input.')
                                      )
        ), numeric.output)
      }
    }, server = FALSE)
    ## save when user clicks

    outConsist <- eventReactive(input$runConsist, {
      req(data())
      # req(input$traitOutqPhenoMultiple)
      mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
      if(mappedColumns == 3){ # all required columns are present
        shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
        ## get the outlier table
        outlier <- list()
        analysisId <- as.numeric(Sys.time())

        ## get data structure
        result <- data()
        ## apply the function

        if(input$cropConsist %in% c("spotato","potato")){

          if(input$cropConsist == "spotato"){
            out <- st4gi::check.data(dfr=st4gi::convert.co(result$data$pheno, 'co.to.labels', crop = 'sp'),
                                     out.mod=input$designConsist,
                                     f=input$fConsist,
                                     print.text = FALSE,
                                     crop = 'sp')
          }else if(input$cropConsist == "potato"){
            out <- st4gi::check.data(dfr=st4gi::convert.co(result$data$pheno, 'co.to.labels', crop = 'pt'),
                                     out.mod=input$designConsist,
                                     f=input$fConsist,
                                     print.text = FALSE,
                                     crop = 'pt')
          }
          outs <- which(out$Inconsist.Matrix > 0 , arr.ind = TRUE)
          if(nrow(outs) > 0){
            traitsInOuts <- colnames(result$data$pheno)[outs[,2]]
            rows <- outs[,1]
            vals <- as.numeric(result$data$pheno[outs])
            # Problem detected, short comment
            reasons <- as.character(out$Inconsist.Matrix[outs])
            reasons[reasons == '1'] <- 'inconsistency among variables'
            reasons[reasons == '2'] <- 'out of range value'
            reasons[reasons == '3'] <- 'extreme value or outlier'
            outlier <- data.frame(module="qaConsist", analysisId=analysisId, trait=traitsInOuts, reason = reasons, row=rows, value=vals )
          }else{outlier <- data.frame(module = "qaConsist", analysisId = analysisId, trait = "none", reason = "none", row = NA, value = NA)}

          ## bind new parameters
          result$modifications$pheno <- outlier
          # add status table
          newStatus <- data.frame(module = "qaConsist", analysisId = analysisId, analysisIdName = input$analysisIdName)
          if (!is.null(result$status)) {
            result$status <- rbind(result$status, newStatus[, colnames(result$status)])
          } else {
            result$status <- newStatus
          }
          # add modeling table
          provMet <- data.frame(module="qaConsist",analysisId=analysisId, trait="all", environment=NA,
                                parameter= "fConsist", value= input$fConsist)
          result$modeling <- provMet
          data(result)
          # cat(crayon::green(paste("QA step with id:",as.POSIXct( analysisId, origin="1970-01-01", tz="GMT"),"for trait",paste(input$traitOutqPhenoMultiple, collapse = ", "),"saved. Now you can proceed to perform Single Trial Analysis.")))
          cat(paste("QA step with id:",as.POSIXct( analysisId, origin="1970-01-01", tz="GMT"),"saved. Now you can move to another module."))
          updateTabsetPanel(session, "tabsMain", selected = "outputTabs")
          shinybusy::remove_modal_spinner()
        }else{
          shinybusy::remove_modal_spinner()
          stop("Consistency rules for selected crop not available yet", call. = FALSE)
        }

        if(!inherits(result,"try-error")) { # if all goes well in the run
          # ## Report tab
          output$reportQaPheno <- renderUI({
            HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportQaPheno.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
          })

          output$downloadReportQaPheno <- downloadHandler(
            filename = function() {
              paste(paste0('qaConsist_dashboard_',gsub("-", "", as.integer(Sys.time()))), sep = '.', switch(
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
      }else{
        cat("Please meet the data conditions before you identify and save outliers.")
      }
    })

    output$outConsist <- renderPrint({
      outConsist()
    })



  })
}

## To be copied in the UI
# mod_dataConsistApp_ui("dataConsistApp_1")

## To be copied in the server
# mod_dataConsistApp_server("dataConsistApp_1")

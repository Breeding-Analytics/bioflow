#' dataConsistPotatoApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dataConsistPotatoApp_ui <- function(id){
  ns <- NS(id)
  tagList(


    shiny::mainPanel(width = 12,
                     tabsetPanel( id=ns("tabsMain"),
                       type = "tabs",

                       tabPanel(div(icon("book"), "Information") ,
                                br(),
                                h2(strong("Status:")),
                                uiOutput(ns("warningMessage")),
                                tags$body(
                                  h2(strong("Details")),
                                  p("Some additional checks for data consistency are benefitial to ensure a high quality analysis. These can include:"),
                                  # img(src = "www/qaRaw.png", height = 300, width = 600), # add an image
                                  p(strong("Check names-")," The onthology for some crops is particularly important. This tab allows to check this to a reference onthology."),
                                  p(strong("Check traits-")," Checking the relationships between traits is particularly useful to identify issues."),
                                  p(strong("Tag inconsistencies-"),"Once inconsistencies are identified, is important to tag them for posterior management in the analytical modules."),
                                  h2(strong("References")),
                                  h2(strong("Software")),
                                  p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Consisttistical Computing,
                                Vienna, Austria. URL https://www.R-project.org/."),
                                )
                       ),
                       tabPanel(div(icon("arrow-right-to-bracket"), "Input"),
                                tabsetPanel(
                                  tabPanel("Select crop", icon = icon("dice-one"),
                                           br(),
                                           column(width=12,style = "background-color:grey; color: #FFFFFF",
                                                  column(width=4, selectInput(ns("cropConsist"), "Select your crop filter:", choices = list(Banana="banana", Beans="beans",  Cassava="cassava", Maize="maize", PearMillet="pmillet", Plantain="plantain", Potato="potato", Rice="rice", Soybean="soybean", SweetPotato="spotato", Sorghum="sorghum", Wheat="wheat"), multiple = FALSE) ),
                                                  column(width=4, tags$br(),
                                                         shinyWidgets::prettySwitch( inputId = ns('launch'), label = "Load example", status = "success"),
                                                  ),
                                           ),
                                           br(),
                                           br(),
                                           DT::DTOutput(ns("phenoConsist")),
                                  ),
                                  tabPanel("Design and extreme value", icon = icon("dice-two"),
                                           br(),
                                           column(width=12,style = "background-color:grey; color: #FFFFFF",
                                                  column(width=4, selectInput(ns("designConsist"), "Design expected", choices = list(None="none", RCBD="rcbd", MET="met"), multiple = FALSE) ),
                                                  column(width=4, numericInput(ns("fConsist"), "Extreme value allowed", value = 5, max = 100000, min = -100000, step = 1) ),
                                           ),
                                           br(),
                                           column(width=12, p(span("Matrix of issues (transposed dataset).", style="color:black")) ),
                                           column(width=6, sliderInput(ns("slider1"), label = "Records", min = 1, max = 2000, value = c(1, 200)) ),
                                           column(width=6, sliderInput(ns("slider2"), label = "Columns/traits", min = 1, max = 500, value = c(1, 250)) ),
                                           column(width=12, shiny::plotOutput(ns("plotConsistencySparsity")) ),

                                  ),
                                  tabPanel("Tag inconsistencies", icon = icon("dice-three"),
                                           column(width=12,style = "background-color:grey; color: #FFFFFF",
                                                  br(),
                                                  actionButton(ns("runConsist"), "Run consistency check (click)", icon = icon("play-circle")),
                                                  uiOutput(ns("qaQcConsistInfo")),
                                                  br(),
                                           ),
                                           textOutput(ns("outConsist")),
                                  ),
                                )# of of tabsetPanel
                       ),
                       tabPanel(div(icon("arrow-right-from-bracket"), "Output" ) , value = "outputTabs",
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

#' dataConsistPotatoApp Server Functions
#'
#' @noRd
mod_dataConsistPotatoApp_server <- function(id, data){
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
        data(cgiarBase::create_getData_object())
        tmp <- data()
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

    ## SPARISTY PLOT
    output$plotConsistencySparsity <- shiny::renderPlot({
      req(data())
      dtMta <- data()

      if(input$cropConsist == "spotato"){
        main <- "Sweet potato consistency flags"
        out <- st4gi::check.data.sp(dtMta$data$pheno, out.mod=input$designConsist, f=input$fConsist)$Inconsist.Matrix
      }else if(input$cropConsist == "potato"){
        main <- "Potato consistency flags"
        out <- st4gi::check.data.pt(dtMta$data$pheno, out.mod=input$designConsist, f=input$fConsist)$Inconsist.Matrix
      }else{
        main <- "Crop not available, no consistency flags"
        out <- matrix(0, nrow=nrow(dtMta$data$pheno), ncol=ncol(dtMta$data$pheno))
      }

      M2 <- as.matrix(out)
      M2 <- M2[,(input$slider2[1]):min(c(input$slider2[2], ncol(M2) )), drop=FALSE] # environments
      M2 <- M2[(input$slider1[1]):min(c(input$slider1[2]), nrow(M2) ), ,drop=FALSE] # genotypes
      Matrix::image(as(t(M2), Class = "dgCMatrix"), ylab="Columns/traits", xlab="Records", main=main, colorkey=TRUE)
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
    })
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
            out <- st4gi::check.data.sp(result$data$pheno,out.mod=input$designConsist, f=input$fConsist)
          }else if(input$cropConsist == "potato"){
            out <- st4gi::check.data.pt(result$data$pheno, out.mod=input$designConsist, f=input$fConsist)
          }
          outs <- which(out$Inconsist.Matrix > 0 , arr.ind = TRUE)
          if(nrow(outs) > 0){
            traitsInOuts <- colnames(result$data$pheno)[outs[,2]]
            rows <- outs[,1]
            vals <- as.numeric(result$data$pheno[outs])
            outlier <- data.frame(module="qaConsist", analysisId=analysisId, trait=traitsInOuts, row=rows, value=vals )
          }else{outlier <- data.frame(module = "qaConsist", analysisId = analysisId, trait = "none", reason = "none", row = NA, value = NA)}

          ## bind new parameters
          result$modifications$pheno <- outlier
          # add status table
          newStatus <- data.frame(module="qaConsist", analysisId=analysisId )
          result$status <- rbind(result$status, newStatus)
          myId <- result$status
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
              paste('my-report', sep = '.', switch(
                "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
              ))
            },
            content = function(file) {
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
              file.rename(out, file)
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
# mod_dataConsistPotatoApp_ui("dataConsistPotatoApp_1")

## To be copied in the server
# mod_dataConsistPotatoApp_server("dataConsistPotatoApp_1")

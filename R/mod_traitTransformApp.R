#' traitTransformApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_traitTransformApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::mainPanel(width = 12, id = ns("tabs"),
                     tabsetPanel( #width=9,
                       type = "tabs",

                       tabPanel(div(icon("book"), "Information") ,
                                tags$body(
                                  column(width = 6,
                                         br(),
                                         h1(strong(span("Trait Transformation Module", style="color:darkcyan"))),
                                         h2(strong("Data Status (wait to be displayed):")),
                                         uiOutput(ns("warningMessage")),
                                  ),

                                  column(width = 6,
                                         br(),
                                         h2(strong("Details")),
                                         p("Some trait transformations are required when data starts to grow or presents modeling challenges. The current
                                                               implementations include:"),
                                         # img(src = "www/qaRaw.png", height = 300, width = 600), # add an image
                                         p(strong("Conversion-")," The trait is transformed by a function such as cubic root, square root, log., etc.."),
                                         p(strong("Equalizing-")," a set of traits with different name are considered to be the same trait and the user needs to equalize them ."),
                                         p(strong("Balancing-")," a trait with numerical values in different units depending on the environment needs to be converted to same units across environments ."),
                                         h2(strong("References")),
                                         h2(strong("Software used")),
                                         p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
                                Vienna, Austria. URL https://www.R-project.org/."),
                                  ),
                                )
                       ),
                       tabPanel("Conversion", icon = icon("rocket"),
                                tabsetPanel(id=ns("tabConversion"),
                                  tabPanel(div(icon("arrow-right-to-bracket"), "Input"), value = "tabConInput",
                                           br(),
                                           shinydashboard::box(status="success",width = 12, #background = "green", solidHeader = TRUE,
                                                               actionButton(ns("runTra"), "Convert traits", icon = icon("play-circle")),
                                                               textOutput(ns("outTraC")),
                                                               hr(style = "border-top: 1px solid #4c4c4c;"),
                                                               DT::DTOutput(ns("transTableC")),
                                           ),
                                  ),
                                  tabPanel(div(icon("arrow-right-from-bracket"), "Output") ,value = "tabConOutput" ,
                                           br(),
                                           shinydashboard::box(status="success",width = 12, #background = "green", solidHeader = TRUE,
                                                               DT::DTOutput(ns("rawPheno")),
                                           ),
                                  ),
                                ),
                       ), # end of output panel
                       tabPanel("Equalizing", icon = icon("equals"),
                                tabsetPanel(id=ns("tabEqualizing"),
                                  tabPanel(div(icon("arrow-right-to-bracket"), "Input"),value = "tabEqualInput",
                                           br(),
                                           selectInput(ns("traitEqualPheno"), "Trait(s) to equalize", choices = NULL, multiple = TRUE),
                                           textInput(ns("newName"), label="Name for the new trait", value=NULL, placeholder = "New trait name"),
                                           actionButton(ns("runEqual"), "Equalize traits", icon = icon("play-circle")),
                                           textOutput(ns("outEqual")),
                                           # hr(style = "border-top: 1px solid #4c4c4c;"),
                                  ),
                                  tabPanel(div(icon("arrow-right-from-bracket"), "Output") ,value = "tabEqualOutput" ,
                                           br(),
                                           DT::DTOutput(ns("rawPheno2")),
                                  ),
                                ),
                       ), # end of output panel
                       tabPanel("Free functions", icon = icon("scale-balanced"),
                                tabsetPanel(id=ns("tabFree"),
                                  tabPanel(div(icon("arrow-right-to-bracket"), "Input"),value = "tabFreeInput",
                                           br(),
                                           h4("Traits available"),
                                           uiOutput(ns("buttonsCalc")),
                                           br(),
                                           textInput(ns("newNameCalc"), label="Name for the new trait", value=NULL, placeholder = "New trait name"),
                                           textInput(ns("text"), label = tags$span(
                                             "Formula to compute",
                                             tags$i(
                                               class = "glyphicon glyphicon-info-sign",
                                               style = "color:#000000",
                                               title = "List the traits to be used (separated by a comma) inside the function() then write the formula inside the curling braces {}"
                                             )
                                           ), value = "function(x){x...}"),
                                           # br(),
                                           # fluidRow(column(2, verbatimTextOutput(ns("valuex")))),
                                           # br(),
                                           actionButton(ns("runBal"), "Compute new trait", icon = icon("play-circle")),
                                           # br(),
                                           textOutput(ns("outBal")),
                                  ),
                                  tabPanel(div(icon("arrow-right-from-bracket"), "Output") ,value = "tabFreeOutput" ,
                                           br(),
                                           DT::DTOutput(ns("rawPheno3")),
                                  ),
                                ),
                       ), # end of output panel

                     )) # end mainpanel


  )
}

#' traitTransformApp Server Functions
#'
#' @noRd
mod_traitTransformApp_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    paramsNew <- reactiveValues(vars = NULL)

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
        mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
        if(mappedColumns == 3){ HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to compute your transformations.")) )
        }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please make sure that the columns: 'environment', 'designation' and \n at least one trait have been mapped using the 'Data Retrieval' tab.")) )
        }
      }
    )

    ## function to create the table of possibilities
    dtFieldTraC = reactive({
      req(data())
      dtProv = data()$metadata$pheno
      traitsFound <- which(dtProv$parameter == "trait")
      if(length(traitsFound) > 0){
        traitNames = dtProv[traitsFound,"value"]
        mm = matrix(0,nrow = 4, ncol = length(traitNames)); rownames(mm) <- c("I","log","sqrt","cbrt"); colnames(mm) <- traitNames
        dtProvTable = as.data.frame(mm);  colnames(dtProvTable) <- traitNames
      }else{
        mm = matrix(0,nrow = 4, ncol = 0); rownames(mm) <- c("I","log","sqrt","cbrt");
        dtProvTable = as.data.frame(mm);
      }
      return(dtProvTable)
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
                                      # options = list(paging=FALSE,
                                      #                searching=FALSE,
                                      #                initComplete = I("function(settings, json) {alert('Done.');}")
                                      # )
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
    ## render result of "run" button click
    outTraC <- eventReactive(input$runTra, {
      req(data())
      mydata <- data()
      if(!is.null(mydata$data$pheno)){
        myTransformation <- apply(xx$df,2,function(y){rownames(xx$df)[which(y > 0)]})
        if(is.matrix(myTransformation)){
          names(myTransformation) <- rep(colnames(myTransformation), each = nrow(myTransformation))
          myTransformation <- c(myTransformation)
        }
        myTransformation <- myTransformation[!is.na(myTransformation)]
        selTrait <- rep(names(myTransformation), lengths(myTransformation))
        selTrans <- as.vector(unlist(myTransformation))
        result <- try(cgiarPipeline::traitTransformation(object = mydata, # input data
                                                         trait=selTrait, # selected trait
                                                         transformation = selTrans), # transformation for that trait
                      silent=TRUE
        )
        if(!inherits(result,"try-error")) {
          data(result) # update data with results
          observe({paramsNew$vars <- intersect(colnames(result$data$pheno), paste(selTrait, selTrans, sep="_"))})
          updateTabsetPanel(session, "tabConversion", selected = "tabConOutput")
          cat(paste("Additional traits added to the phenotypic dataset."))
        }else{
          cat(paste("Analysis failed with the following error message: \n\n",result[[1]]))
        }
      }
    })
    output$outTraC <- renderPrint({
      outTraC()
    })

    ## render raw data
    output$rawPheno <-  output$rawPheno2 <- output$rawPheno3 <- DT::renderDT({
      req(data())
      dtSta <- data()
      dtSta <- dtSta$data$pheno
      ### change column names for mapping
      paramsPheno <- data()$metadata$pheno
      paramsPheno <- paramsPheno[which(paramsPheno$parameter != "trait"),]
      colnames(dtSta) <- cgiarBase::replaceValues(colnames(dtSta), Search = paramsPheno$value, Replace = paramsPheno$parameter )
      ###
      traitTypes <- unlist(lapply(dtSta,class))
      numeric.output <- names(traitTypes)[which(traitTypes %in% "numeric")]
      table <- DT::datatable(dtSta, extensions = 'Buttons',
                             options = list(dom = 'Blfrtip',scrollX = TRUE,
                                            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                            lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))) %>%
        DT::formatRound(numeric.output) %>%
        DT::formatStyle(isolate({paramsNew$vars}), backgroundColor = "#009E60")
      table
    }, server = FALSE)

    ################################################################################################
    ################################################################################################
    ## EQUALIZE MODULE
    ################################################################################################
    ################################################################################################
    # traits
    observeEvent(c(data(),input$version2Sta,input$genoUnitSta), {
      req(data())
      dtProv = data()$metadata$pheno
      traitNames = dtProv[dtProv$parameter == "trait","value"]
      updateSelectInput(session, "traitEqualPheno", choices = traitNames, selected = NULL)
    })

    outEqual <- eventReactive(input$runEqual, {
      req(input$traitEqualPheno)

      if(length(input$traitEqualPheno) >1 ){

        mydatax <- data()
        ## now we take that list and bind them as independent datasets
        result <- try(cgiarPipeline::equalizeTraits(object = mydatax, traits=input$traitEqualPheno, newName=input$newName ), silent = TRUE
        )
        if(!inherits(result,"try-error")) {
          data(result) # update data with results
          observe({paramsNew$vars <- input$newName})
          updateTabsetPanel(session, "tabEqualizing", selected = "tabEqualOutput")
          cat(paste("Traits", paste(input$traitEqualPheno, collapse = ", "),"equalized. New trait created in the phenotypic dataset."))
          # observe({input$newName <- NULL})
        }else{
          cat(paste("Analysis failed with the following error message: \n\n",result[[1]]))
        }
      }else{
        print("You need to provide more than one trait")
      }
    })
    output$outEqual <- renderPrint({
      outEqual()
    })

    ################################################################################################
    ################################################################################################
    ## BALANCING MODULE
    ################################################################################################
    ################################################################################################

    output$buttonsCalc <- renderUI({
      req(data())
      dtBal <- data()
      traitsBal <- dtBal$metadata$pheno[which(dtBal$metadata$pheno$parameter == "trait"),"value"]
      # traitsBal <- c(traitsBal,"+","-","/","*","^","1","2","3","4","5","6","7","8","9","0")
      lapply(1:length(traitsBal), function(i) {
        actionButton(inputId= session$ns(paste0('buttonsCalc',i)),
                     paste0(traitsBal[i])
        )
      })
    })

    # output$valuex <- renderPrint({ input$text })

    outBal <- eventReactive(input$runBal, {
      req(data())
      req(input$newNameCalc)

      mydatax <- data()

      result <- try(cgiarPipeline::freeFunction(object = mydatax, formula=input$text, newName=input$newNameCalc ), silent = TRUE)
      if(!inherits(result,"try-error")) {
        data(result)
        observe({paramsNew$vars <- input$newNameCalc})
        updateTabsetPanel(session, "tabFree", selected = "tabFreeOutput")
        print(paste("New variable", input$newNameCalc, "added to the dataset."))
        cat(paste("Trait", input$newNameCalc, "computed. New trait created in the phenotypic dataset."))
        # observe({input$newNameCalc <- NULL})
      }else{
        cat(paste("Analysis failed with the following error message: \n\n",result[[1]]))
      }

    })
    output$outBal <- renderPrint({
      outBal()
    })


  })
}

## To be copied in the UI
# mod_traitTransformApp_ui("traitTransformApp_1")

## To be copied in the server
# mod_traitTransformApp_server("traitTransformApp_1")

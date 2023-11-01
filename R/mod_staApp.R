#' staApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_staApp_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarPanel(
      tags$style(".well {background-color:black; color: #FFFFFF;}"),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      selectInput(ns("genoUnitSta"), "Genetic evaluation unit(s)", choices = NULL, multiple = TRUE),
      selectInput(ns("fixedTermSta2"), "Fixed effects", choices = NULL, multiple = TRUE),
      selectInput(ns("trait2Sta"), "Trait(s) to analyze", choices = NULL, multiple = TRUE),

      hr(style = "border-top: 1px solid #4c4c4c;"),

      shinyjs::useShinyjs(),
      actionButton(ns("togglesettingsSta"), "Settings..."),

      selectInput(ns("genoAsFixedSta"), "Return BLUEs", choices = list(TRUE,FALSE), selected = TRUE),
      numericInput(ns("maxitSta"), "Number of iterations", value = 35),
      selectInput(ns("verboseSta"), "Print logs", choices = list(TRUE,FALSE), selected = FALSE),

      # div(id="settingsSta",
      #     selectInput(ns("genoAsFixedSta"), "Return BLUEs", choices = list(TRUE,FALSE), selected = TRUE),
      #     numericInput(ns("maxitSta"), "Number of iterations", value = 35),
      #     selectInput(ns("verboseSta"), "Print logs", choices = list(TRUE,FALSE), selected = FALSE)
      # ),

      # conditionalPanel(
      #   condition = "input.settingsSta > 0",
      #   selectInput(ns("genoAsFixedSta"), "Return BLUEs", choices = list(TRUE,FALSE), selected = TRUE),
      #   numericInput(ns("maxitSta"), "Number of iterations", value = 35),
      #   selectInput(ns("verboseSta"), "Print logs", choices = list(TRUE,FALSE), selected = FALSE),
      # ),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      actionButton(ns("runSta"), "Run", icon = icon("play-circle"))
    ),
    mainPanel(tabsetPanel(
      type = "tabs",

      tabPanel("Data",
               br(),

      ),
      tabPanel("Predictions",
               br(),
               DT::DTOutput(ns("predictionsSta"))

      ),
      tabPanel("Metrics",
               br(),

      ),
      tabPanel("Modeling",
               br(),

      ),




    ))

  )
}

#' staApp Server Functions
#'
#' @noRd
mod_staApp_server <- function(id,data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Create the fields
    observe({req(data())
      dtSta <- data()$data$pheno
      dNames <- names(dtSta)
      updateSelectInput(session, "genoUnitSta", choices = dNames)
    })
    observeEvent(input$genoUnitSta, {
      req(data())
      dtSta <- data()$data$pheno
      dNames <- names(dtSta)
      updateSelectInput(session, "fixedTermSta2",
                        choices = dNames[dNames!=input$genoUnitSta])
    })
    observeEvent(c(input$genoUnitSta,input$fixedTermSta2), {
      req(data())
      dtSta <- data()$data$pheno
      dNames <- names(dtSta)
      updateSelectInput(session, "trait2Sta",
                        choices = dNames[!dNames %in% c(input$fixedTermSta2,input$genoUnitSta)])
    })
    # toggle the button "Settings"
    observeEvent(input$togglesettingsSta, {
      shinyjs::toggle("genoAsFixedSta")
      shinyjs::toggle("maxitSta")
      shinyjs::toggle("verboseSta")
    })

    # reactive table for trait family distributions
    dtDistTrait = reactive({
      dtSta <- data()$data$pheno
      req(dtSta)
      req(input$trait2Sta)
      # dtProv = dtSta()$predictions
      traitNames = input$trait2Sta#unique(dtProv$trait)
      mm = matrix(0,nrow = 8, ncol = length(traitNames));
      rownames(mm) <- c(
        "gaussian(link = 'identity')",
        "quasi(link = 'identity', variance = 'constant')",
        "binomial(link = 'logit')",
        "Gamma(link = 'inverse')",
        "inverse.gaussian(link = '1/mu^2')",
        "poisson(link = 'log')",
        "quasibinomial(link = 'logit')",
        "quasipoisson(link = 'log')"
      );
      colnames(mm) <- traitNames
      dtProvTable = as.data.frame(mm);  colnames(dtProvTable) <- traitNames
      return(dtProvTable)
    })
    xx = reactiveValues(df = NULL)
    observe({
      df <- dtDistTrait()
      xx$df <- df
    })
    output$traitDistSta = DT::renderDT(xx$df,
                                       selection = 'none',
                                       editable = TRUE,
                                       options = list(paging=FALSE,
                                                      searching=FALSE,
                                                      initComplete = I("function(settings, json) {alert('Done.');}")
                                       )
    )

    proxy = DT::dataTableProxy('traitDistSta')

    observeEvent(input$traitDistSta_cell_edit, {
      info = input$traitDistSta_cell_edit
      utils::str(info)
      i = info$row
      j = info$col
      v = info$value
      xx$df[i, j] <- isolate(DT::coerceValue(v, xx$df[i, j]))
    })
    ## render result of "run" button click
    observeEvent(input$runSta, {
    # outSta <- eventReactive(input$runSta, {
      # dtSta <- data()$data$pheno
      # family distributions input
      myFamily = apply(xx$df,2,function(y){rownames(xx$df)[which(y > 0)[1]]})
      dontHaveDist <- which(is.na(myFamily))
      if(length(dontHaveDist) > 0){myFamily[dontHaveDist] <- "gaussian(link = 'identity')"}
      # run the modeling
      # print(dtSta)
      result <- try(cgiarPipe::staLMM(phenoDTfile = data(),
                                      trait=input$trait2Sta, traitFamily = myFamily,
                                      fixedTerm = input$fixedTermSta2,
                                      returnFixedGeno=input$genoAsFixedSta, genoUnit = input$genoUnitSta,
                                      verbose = input$verboseSta, maxit = input$maxitSta),
                    silent=TRUE
      )
      print("xxxx")
      print(ls())
      print(result)
      # if(!inherits(result,"try-error") ){
      #   # saveRDS(result, file = paste0("OUTPUT.rds"))
      #   # googledrive::drive_upload(
      #   #   media= paste0("OUTPUT.rds"),
      #   #   path = file.path(res_auth$userType, paste0(result$id,".rds") ),
      #   #   overwrite = TRUE
      #   # )
      #   # print(paste("Your results have been stored on the cloud with ID:",result$id))
      # } #else{
      #   print(result[[1]])
      # }
      # if(file.exists("staAppData.rds")){unlink("staAppData.rds")}
      # if(file.exists("OUTPUT.rds")){unlink("OUTPUT.rds")}

      output$predictionsSta <-  DT::renderDT({

        if(!inherits(result,"try-error") ){
          DT::datatable(result$predictions,
                        options = list(autoWidth = TRUE),
                        filter = "top" # This will position the column filters at the top
          )
        }

      })
    })




    # output$outSta <- renderPrint({
    #   outSta()
    # })
    # ## render result of selected file
    # output$tableStaAnalysis <- DT::renderDataTable( dtSta$cleaned[1:min(c(100,nrow(dtSta$cleaned))),], filter = list(position = 'top', clear = FALSE),
    #                                                 options = list(
    #                                                   # pageLength = 50,
    #                                                   paging=TRUE,
    #                                                   initComplete = I("function(settings, json) {alert('Done.');}")
    #                                                 )
    # )

## end
  })
}

## To be copied in the UI
# mod_staApp_ui("staApp_1")

## To be copied in the server
# mod_staApp_server("staApp_1")

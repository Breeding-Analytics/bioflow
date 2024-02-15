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


    shiny::sidebarPanel(#width = 3,
      width = 6,
      tags$style(".well {background-color:grey; color: #FFFFFF;}"),
      HTML("<img src='www/cgiar3.png' width='42' vspace='10' hspace='10' height='46' align='top'>
                  <font size='5'>Edit Experimental Design Information</font>"),
      tags$h4("Double click in a environment by design factor combination and set to 0 to ignore this factor in coming runs."),
      shinydashboard::box(width = 12, solidHeader=FALSE,collapsible = FALSE, collapsed = FALSE, title = "",
                          column(width = 12,DT::dataTableOutput(ns("transTableC")), style = "height:220px; overflow-y: scroll;overflow-x: scroll;"),
      ),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      actionButton(ns("runFieldClean"), "Filter dataset", icon = icon("play-circle")),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      textOutput(ns("outExp"))
    ), # end sidebarpanel
    shiny::mainPanel(width = 6,
                     tabsetPanel( #width=9,
                       type = "tabs",

                       tabPanel(p("Information",class="info-p"), icon = icon("book"),
                                br(),
                                shinydashboard::box(status="success",width = 12,
                                                    solidHeader = TRUE,
                                                    column(width=12,   style = "height:800px; overflow-y: scroll;overflow-x: scroll;",
                                                           h2(strong("Status:")),
                                                           uiOutput(ns("warningMessage")),
                                                           tags$body(
                                                             h2(strong("Details")),
                                                             p("Sometimes is required to set to missing experimental design factors that we are aware that are not correctly saved
                                                               in the databases. Although the tight solution would be to fix this information in the original database, a pragmatic
                                                               approach is to set certain factors from an especific environment to missing so it is ignored in the model fitting or
                                                               or any other analytical module using this information."),
                                                             # img(src = "www/qaRaw.png", height = 300, width = 600), # add an image
                                                             p(strong("Editing table-")," there is not much complexity of how to use this module. There is a table with a column for
                                                               each experimental design factor and a row for each environment. By default this table is filled with ones wherever this
                                                               information is available. If the user wants to silence a particular factor it just needs to double click in the cell and
                                                               set the value to zero."),
                                                           )
                                                    )
                                )
                       ),
                       tabPanel(p("Input",class="input-p"), icon = icon("arrow-right-to-bracket"),
                                tabsetPanel(
                                  tabPanel("Heatmap", icon = icon("magnifying-glass-chart"),
                                           br(),
                                           shinydashboard::box(status="success",width = 12, #background = "green", solidHeader = TRUE,
                                                               selectInput(ns("fieldinstCleaned3Dtraits"), "Environment to visualize", choices = NULL, multiple = FALSE),
                                                               selectInput(ns("zaxisCleaned3Dtraits"), "Color field by", choices = NULL, multiple = FALSE),
                                                               selectInput(ns("textCleaned3Dtraits"), "Text cells by", choices = NULL, multiple = FALSE),
                                                               plotly::plotlyOutput(ns("plotCleaned3Dtraits"))

                                           )
                                  ),
                                ) # end of tabset
                       )# end of output panel
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
      available <- data()$metadata$pheno[data()$metadata$pheno$parameter %in% c("row","col","iBlock","rep","trait"), "value"]
      traitsMta <- setdiff(available,"")
      updateSelectInput(session, "zaxisCleaned3Dtraits", choices = traitsMta)
    })
    # update text by
    observeEvent(c(data()), {
      req(data())
      available <- data()$metadata$pheno[data()$metadata$pheno$parameter %in% c("row","col","iBlock","rep","designation"), "value"]
      traitsMta <- setdiff(available,"")
      updateSelectInput(session, "textCleaned3Dtraits", choices = traitsMta)
    })
    # update environments by
    observeEvent(c(data()), {
      req(data())
      dtSta <- data()$data$pheno
      paramsPheno <- data()$metadata$pheno
      if(is.null(paramsPheno)){
        traitsMta <- NULL
      }else{
        paramsPheno <- paramsPheno[which(paramsPheno$parameter != "trait"),]
        if("environment" %in% paramsPheno$parameter){ # good to go
          colnames(dtSta) <- cgiarBase::replaceValues(colnames(dtSta), Search = paramsPheno$value, Replace = paramsPheno$parameter )
          traitsMta <- na.omit(unique(dtSta[,"environment"]))
        }else{
          traitsMta <- NULL
        }
      }
      updateSelectInput(session, "fieldinstCleaned3Dtraits", choices = traitsMta)
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

      if(!is.na( stats::var(z) )){ # if we are good to go
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


    # ## our function to keep track of reps, rows, cols that should be set to NA for a given field
    # dtFieldTraC = reactive({
    #   req(data())
    #   dtProv = data()$data$pheno
    #   paramsPheno <- data()$metadata$pheno
    #   if(!is.null(paramsPheno)){
    #     paramsPheno <- paramsPheno[which(paramsPheno$parameter != "trait"),]
    #     colnames(dtProv) <- cgiarBase::replaceValues(colnames(dtProv), Search = paramsPheno$value, Replace = paramsPheno$parameter )
    #     dtProvNames <- c("row","col","rep","iBlock")
    #     envCol <- paramsPheno[which(paramsPheno$parameter == "environment"),"value"]
    #     if(length(envCol) > 0){
    #       fieldNames <- as.character(unique(dtProv[,"environment"]))
    #       mm = matrix(1,ncol = length(dtProvNames), nrow = length(fieldNames));
    #       colnames(mm) <- dtProvNames; rownames(mm) <- fieldNames
    #       dtProvTable = as.data.frame(mm);  rownames(dtProvTable) <- fieldNames
    #       return(dtProvTable)
    #     }
    #   }
    # })
    # xx = reactiveValues(df = NULL)
    # observe({
    #   df <- dtFieldTraC()
    #   xx$df <- df
    # })
    # output$transTableC = DT::renderDT(xx$df,
    #                                   selection = 'none',
    #                                   editable = TRUE,
    #                                   options = list(paging=FALSE,
    #                                                  searching=FALSE,
    #                                                  initComplete = I("function(settings, json) {alert('Done.');}")
    #                                   )
    # )
    # proxy = DT::dataTableProxy('transTableC')
    # observeEvent(input$transTableC_cell_edit, {
    #   info = input$transTableC_cell_edit
    #   utils::str(info)
    #   i = info$row
    #   j = info$col
    #   v = info$value
    #   xx$df[i, j] <- isolate(DT::coerceValue(v, xx$df[i, j]))
    # })
    # ## save when user clicks
    #
    # outExp <- eventReactive(input$runFieldClean, {
    #
    #   if(is.null(data())){
    #     cst("Please retrieve or load your phenotypic data using the 'Data Retrieval' tab.")
    #   }else{ # data is there
    #     ## pheno check
    #     if( length(which(c("designation") %in% data()$metadata$pheno$parameter)) == 0 ){
    #       cat("Please map your 'designation' column using the 'Data Retrieval' tab in the 'Phenotype' section.")
    #     }else{
    #
    #     }
    #   }
    #
    # })
    # output$outExp <- renderPrint({
    #   outExp()
    # })
# just a test again 2 and 3



  })
}

## To be copied in the UI
# mod_expDesignEditApp_ui("expDesignEditApp_1")

## To be copied in the server
# mod_expDesignEditApp_server("expDesignEditApp_1")

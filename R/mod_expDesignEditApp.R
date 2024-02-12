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
      column(width = 12,DT::dataTableOutput(ns("transTableC")), style = "height:220px; overflow-y: scroll;overflow-x: scroll;"),
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
    #   dtProv = dtScatterCleaned3Dtraits()$cleaned
    #   dtProvNames <- c("rowcoord","colcoord","rep","block")
    #   fieldNames <- as.character(unique(dtProv[,"fieldinstF"]))
    #   mm = matrix(1,nrow = length(dtProvNames), ncol = length(fieldNames));
    #   rownames(mm) <- dtProvNames; colnames(mm) <- fieldNames
    #   dtProvTable = as.data.frame(mm);  colnames(dtProvTable) <- fieldNames
    #   return(dtProvTable)
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

    # render the expected result
    # output$plotPossibleCrosses <- shiny::renderPlot({
    #   req(data())
    #   if(!is.null(data()$data$pheno) & !is.null(data()$data$geno) & !is.null(data()$data$pedigree) ){
    #     ped <- data()$data$pedigree
    #     metaPed <- data()$metadata$pedigree
    #     colnames(ped) <- cgiarBase::replaceValues(colnames(ped), Search = metaPed$value, Replace = metaPed$parameter )
    #     cross <- unique(ped[,c("designation","mother","father")])
    #     cross$motherN <- as.numeric(as.factor(cross$mother))
    #     cross$fatherN <- as.numeric(as.factor(cross$father))
    #     # converts a data.frame into a matrix
    #     A <- matrix(NA, nrow=max(cross$motherN, na.rm=TRUE), ncol = max(cross$fatherN, na.rm=TRUE))
    #     A[as.matrix(cross[,c("motherN","fatherN")])] = 1
    #     rownames(A) <-  levels(as.factor(cross$mother))
    #     colnames(A) <- levels(as.factor(cross$father))
    #     A <- A[,(input$slider2[1]):min(c(input$slider2[2], ncol(A) )), drop=FALSE] # environments
    #     A <- A[(input$slider1[1]):min(c(input$slider1[2]), nrow(A) ), ,drop=FALSE] # genotypes
    #     Matrix::image(as(A, Class = "dgCMatrix"), xlab="Fathers", ylab="Mothers", colorkey=FALSE, main="Available hybrids")
    #   }
    # })

    # output$plotPossibleProfiles <- shiny::renderPlot({
    #   req(data())
    #   if(!is.null(data()$data$pheno) & !is.null(data()$data$geno) & !is.null(data()$data$pedigree) ){
    #     ped <- data()$data$pedigree
    #     metaPed <- data()$metadata$pedigree
    #     colnames(ped) <- cgiarBase::replaceValues(colnames(ped), Search = metaPed$value, Replace = metaPed$parameter )
    #     cross <- unique(ped[,c("designation","mother","father")])
    #     # subset to crosses that can be built
    #     possible <- which( (cross$mother %in% rownames(data()$data$geno)) & (cross$father %in% rownames(data()$data$geno))  )
    #     if(length(possible) > 0){
    #       cross <- cross[possible, ]
    #     }
    #     ##
    #     cross$motherN <- as.numeric(as.factor(cross$mother))
    #     cross$fatherN <- as.numeric(as.factor(cross$father))
    #     # converts a data.frame into a matrix
    #     A <- matrix(0, nrow=max(cross$motherN, na.rm=TRUE), ncol = max(cross$fatherN, na.rm=TRUE))
    #     if(length(possible) > 0){
    #       A[as.matrix(cross[,c("motherN","fatherN")])] = 2
    #     }
    #     rownames(A) <-  levels(as.factor(cross$mother))
    #     colnames(A) <- levels(as.factor(cross$father))
    #     A <- A[,(input$slider2[1]):min(c(input$slider2[2], ncol(A) )), drop=FALSE] # environments
    #     A <- A[(input$slider1[1]):min(c(input$slider1[2]), nrow(A) ), ,drop=FALSE] # genotypes
    #     Matrix::image(as(A, Class = "dgCMatrix"), xlab="Fathers", ylab="Mothers", colorkey=FALSE, main="Possible to build")
    #   }
    # })
    #
    # ## display the current outliers
    # observeEvent(data(),{
    #
    #   output$summariesScr <-  DT::renderDT({
    #
    #     req(data())
    #     if(!is.null(data()$data$pheno) & !is.null(data()$data$geno) & !is.null(data()$data$pedigree) ){
    #       ped <- data()$data$pedigree
    #       metaPed <- data()$metadata$pedigree
    #       colnames(ped) <- cgiarBase::replaceValues(colnames(ped), Search = metaPed$value, Replace = metaPed$parameter )
    #       phe <- data()$data$pheno
    #       metaPhe <- data()$metadata$pheno
    #       colnames(phe) <- cgiarBase::replaceValues(colnames(phe), Search = metaPhe$value, Replace = metaPhe$parameter )
    #       gen <- data()$data$geno
    #
    #       pheped <- merge(phe,ped, by="designation", all.x=TRUE)
    #       mothers <- unique(pheped[,"mother"])
    #       fathers <- unique(pheped[,"father"])
    #       designation <- unique(pheped[,"designation"])
    #       mothersG <- intersect(mothers, rownames(gen))
    #       fathersG <- intersect(fathers, rownames(gen))
    #       designationG <- intersect(designation, rownames(gen))
    #       final <- data.frame(Metric=c("Mother","Father","Designation"),
    #                           Phenotyped=c(length(mothers), length(fathers), length(designation)),
    #                           Genotyped=c(length(mothersG), length(fathersG), length(designationG))
    #       )
    #       DT::datatable(final, extensions = 'Buttons',
    #                     options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
    #                                    lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
    #       )
    #     }
    #
    #   })
    #
    # })

    ## save when user clicks

    outExp <- eventReactive(input$runScr, {

      if(is.null(data())){
        cst("Please retrieve or load your phenotypic data using the 'Data Retrieval' tab.")
      }else{ # data is there
        ## pheno check
        if( length(which(c("designation") %in% data()$metadata$pheno$parameter)) == 0 ){
          cat("Please map your 'designation' column using the 'Data Retrieval' tab in the 'Phenotype' section.")
        }else{

        }
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

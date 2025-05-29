#' qaPhenoApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_qaPhenoApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::mainPanel(width = 12,
                     tabsetPanel( id=ns("tabsMain"),
                                  type = "tabs",

                                  tabPanel(div(icon("book"), "Information") ,
                                           br(),
                                           column(width = 6,
                                                  h1(strong(span("Raw Phenotype Outlier Detection Module", tags$a(href="https://www.youtube.com/watch?v=X8lYQ8_LmSg&list=PLZ0lafzH_UmclOPifjCntlMzysEB2_2wX&index=4", icon("youtube") , target="_blank")  ,style="color:darkcyan"))),
                                                  h2(strong("Data Status (wait to be displayed):")),
                                                  uiOutput(ns("warningMessage")),
                                                  tags$br(),
                                                  # column(width=4, tags$br(),
                                                  shinyWidgets::prettySwitch( inputId = ns('launch'), label = "Load example dataset", status = "success"),
                                                  # ),
                                                  tags$br(),
                                                  img(src = "www/qaRaw.png", height = 200, width = 470), # add an image
                                           ),
                                           column(width = 6,
                                                  tags$body(
                                                    h2(strong("Details")),
                                                    p("The first step in genetic evaluation is to ensure that input phenotypic records are of good quality.
                                                             This option aims to allow users to select outliers based on plot whiskers and absolute values.
                                The way arguments are used is the following:"),
                                                    p(strong("Trait(s) to QA.-")," Trait(s) to apply jointly the parameter values in the grey box."),
                                                    p(strong("Outlier coefficient.-")," this determines how far the plot whiskers extend out from the box. If coef is positive, the whiskers extend to the most extreme data point which is no more than coef times the length of the box away from the box. A value of zero causes the whiskers to extend to the data extremes (and no outliers be returned)."),
                                                    h2(strong("References")),
                                                    p("Tukey, J. W. (1977). Exploratory Data Analysis. Section 2C."),
                                                    p("McGill, R., Tukey, J. W. and Larsen, W. A. (1978). Variations of box plots. The American Statistician, 32, 12–16. doi:10.2307/2683468."),
                                                    p("Velleman, P. F. and Hoaglin, D. C. (1981). Applications, Basics and Computing of Exploratory Data Analysis. Duxbury Press."),
                                                    # column(width = 12, shiny::plotOutput(ns("plotDataDependencies")), ),
                                                  )
                                           ),
                                  ),
                                  tabPanel(div(icon("arrow-right-to-bracket"), "Input steps"),
                                           tabsetPanel(
                                             tabPanel(div(icon("dice-one"), "Set trait(s) & threshold", icon("arrow-right") ), #icon = icon("dice-one"),
                                                      br(),
                                                      column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                             column(width=6, selectInput(ns("traitOutqPhenoMultiple"),
                                                                                         label = tags$span(
                                                                                           "Trait(s) to QA",
                                                                                           tags$i(
                                                                                             class = "glyphicon glyphicon-info-sign",
                                                                                             style = "color:#FFFFFF",
                                                                                             title = "Only traits matched during the phenotypic data upload will show up."
                                                                                           )
                                                                                         ),
                                                                                         choices = NULL, multiple = TRUE) ),
                                                             column(width=2,numericInput(ns("outlierCoefOutqPheno"),
                                                                                         label = tags$span(
                                                                                           "IQR coefficient",
                                                                                           tags$i(
                                                                                             class = "glyphicon glyphicon-info-sign",
                                                                                             style = "color:#FFFFFF",
                                                                                             title = "The interquantile range is defined as the difference between the 75th and 25th percentiles of the data. To calculate the IQR, the data set is divided into quartiles, or four rank-ordered even parts via linear interpolation. These quartiles are denoted by Q1 (also called the lower quartile), Q2 (the median), and Q3 (also called the upper quartile). The lower quartile corresponds with the 25th percentile and the upper quartile corresponds with the 75th percentile, so IQR = Q3 −  Q1."
                                                                                           )
                                                                                         ),
                                                                                         value = 3.2) ),

                                                      ),
                                                      column(width=12),
                                                      shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                          column(width=12,
                                                                                 hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                                 h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameters to be specified in the grey boxes above.", style="color:green"))),
                                                                                 hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                          ),
                                                                          tags$span(id = ns('holder'),
                                                                                    column(width=4, selectInput(ns("traitOutqPheno"), "Trait to visualize", choices = NULL, multiple = FALSE) ),
                                                                                    column(width=3, numericInput(ns("transparency"),"Plot transparency",value=0.6, min=0, max=1, step=0.1) ),
                                                                                    column(width=3, numericInput(ns("outlierCoefOutqFont"), label = "x-axis font size", value = 12, step=1) ),
                                                                                    column(width=2, checkboxInput(ns("checkbox"), label = "Include x-axis labels", value = TRUE) ),
                                                                          ),
                                                                          column(width=12, shiny::plotOutput(ns("plotPredictionsCleanOut")) ), # plotly::plotlyOutput(ns("plotPredictionsCleanOut")),
                                                                          column(width=12,
                                                                                 DT::DTOutput(ns("modificationsQa")),
                                                                          )
                                                      ),
                                             ),
                                             tabPanel(div( icon("dice-two"), "Run analysis" ), # icon = icon("dice-two"),
                                                      br(),
                                                      column(width=12,style = "background-color:grey; color: #FFFFFF",
                                                             column(width=3, tags$div(textInput(ns("analysisIdName"), label = tags$span(
                                                               "Analysis Name (optional)", tags$i( class = "glyphicon glyphicon-info-sign", style = "color:#FFFFFF",
                                                                                                   title = "An optional name for the analysis besides the timestamp if desired.") ), #width = "100%",
                                                               placeholder = "(optional name)") ) ),
                                                             column(width=3,
                                                                    br(),
                                                                    actionButton(ns("runQaRaw"), "Tag outliers", icon = icon("play-circle")),
                                                                    br(),
                                                                    br(),
                                                             ),
                                                      ),
                                                      textOutput(ns("outQaRaw")),
                                                      # fluidRow(column(3, verbatimTextOutput(ns("value"))))
                                             ),
                                           ) # end of tabset
                                  ),# end of input panel

                                  tabPanel("Correlations", icon = icon("magnifying-glass-chart"),
                                           br(),
                                           column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                  column(width=6, selectInput(ns("correlationTraits"), "Trait(s) to QA", choices = NULL, multiple = TRUE) ),
                                                  column(width=6,selectInput(ns("correlationType"),
                                                                             label = "Select Correlation Method:",
                                                                             choices = c("spearman","pearson","kendall") ))),
                                           #column(width=4,numericInput(ns("beta"), label = "Confidence Level",
                                           #                             value=0.95, min=0, max=1,
                                           #                            step=0.01))),
                                           #column(width=4,actionButton("Run_c","Calculate"))),
                                           column(width=12, mainPanel(plotly::plotlyOutput(ns("plotCorrelationHeatmap")))),
                                           #shiny::plotOutput(ns("plotCorrelationHeatmap")


                                           # column(width=12,
                                           #        hr(style = "border-top: 3px solid #4c4c4c;"),
                                           #        h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameter values to be specified in the grey boxes above.", style="color:green"))),
                                           #        hr(style = "border-top: 3px solid #4c4c4c;")),


                                           #column(width=12, shiny::DTOutput(ns("data_table")) ),
                                  ),

                                  tabPanel(div(icon("arrow-right-from-bracket"), "Output tabs" ) , value = "outputTabs",
                                           tabsetPanel(
                                             tabPanel("Dashboard", icon = icon("file-image"),
                                                      br(),
                                                      textOutput(ns("outQaRaw2")),
                                                      br(),
                                                      downloadButton(ns("downloadReportQaPheno"), "Download dashboard"),
                                                      br(),
                                                      uiOutput(ns('reportQaPheno'))
                                             ),
                                           ),
                                  ),
                     )) # end mainpanel

  )
}

#' qaRawApp Server Functions
#'
#' @noRd
mod_qaPhenoApp_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # output$plotDataDependencies <- shiny::renderPlot({ dependencyPlot() })
    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })
    ############################################################################
    # show shinyWidgets until the user can use the module
    observeEvent(c(data()), {
      req(data())
      mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
      if(mappedColumns == 3){
        golem::invoke_js('showid', ns('holder'))
      }else{
        golem::invoke_js('hideid', ns('holder'))
      }
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

    # Create the available traits
    observeEvent(data(), {
      req(data())
      objectQaRaw <- data()
      metaQaRaw <- objectQaRaw$metadata$pheno
      traitsQaRaw <- unique(metaQaRaw[metaQaRaw$parameter=="trait","value"])
      # traits with variance different than zero
      if(length(traitsQaRaw) > 0){
        varsTraits <- apply(objectQaRaw$data$pheno[,traitsQaRaw,drop=FALSE],2,var, na.rm=TRUE)
        traitsQaRawVar <- names(which(varsTraits > 0))
        # traits with at least some data in any trial
        naTraits <- apply(objectQaRaw$data$pheno[,traitsQaRaw,drop=FALSE],2,sommer::propMissing)
        traitsQaRawNa <- names(which(naTraits < 1))
        # intersection of both trait types
        traitsQaRawVarNa <- intersect(traitsQaRawVar,traitsQaRawNa)
        updateSelectInput(session, "traitOutqPheno",choices = traitsQaRawVarNa)
        updateSelectInput(session, "traitOutqPhenoMultiple",choices = traitsQaRawVarNa, selected = NULL)
        updateSelectInput(session, "correlationTraits",choices = traitsQaRaw, selected = NULL) #correlation types
      }
      shinyjs::hide(ns("traitOutqPheno"))
    })

    # Filtered data based on selected traits to correlations

    filtered_data <- reactive({
      selected_traits <- input$correlationTraits
      mydata <- na.omit(data()$data$pheno)
      mydata <- mydata[, selected_traits, drop = FALSE]
      mydata <- mydata[,which(!duplicated(colnames(mydata)))]
      mydata
    })

    #plot of correlations

    output$plotCorrelationHeatmap <- plotly::renderPlotly({
      req(data())
      req(input$correlationTraits)
      req(input$correlationType)
      # Check if there are more than one selected traits
      req(length(input$correlationTraits) > 1)
      mydata <- filtered_data()
      if(length(input$correlationTraits) > 1){
        # Correlation matrix
        matrix <- cor(mydata[,input$correlationTraits], method = input$correlationType)

        # Compute a matrix of correlation p-values
        p.mat <- rstatix::cor_pmat(mydata[,input$correlationTraits], method = input$correlationType, conf.level = 0.95)
        mt <- as.matrix(p.mat[,-1])

        # Barring the no significant coefficient
        g<-ggcorrplot::ggcorrplot(matrix, hc.order = TRUE, type = "lower", p.mat = mt,lab=T)
        plotly::ggplotly(g)
      } else {
        print("Select more than one!")
      }
    }
    )

    ## render the expected result
    output$plotPredictionsCleanOut <- shiny::renderPlot({ # plotly::renderPlotly({
      req(data())
      req(input$outlierCoefOutqFont)
      req(input$outlierCoefOutqPheno)
      req(input$traitOutqPheno)
      mydata <- data()$data$pheno
      ### change column names for mapping
      paramsPheno <- data()$metadata$pheno
      paramsPheno <- paramsPheno[which(paramsPheno$parameter != "trait"),]
      colnames(mydata) <- cgiarBase::replaceValues(colnames(mydata), Search = paramsPheno$value, Replace = paramsPheno$parameter )
      ###
      mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
      if(mappedColumns == 3){ # all required columns are present
        mydata$rowindex <- 1:nrow(mydata)
        mydata[, "environment"] <- as.factor(mydata[, "environment"])
        mydata[, "designation"] <- as.factor(mydata[, "designation"])
        mo <- cgiarPipeline::newOutliersFun(myObject=data(), trait=input$traitOutqPheno, outlierCoefOutqPheno=input$outlierCoefOutqPheno)
        mo <- mo[which(is.na(mo$analysisId)),]
        mydata$color <- "valid"
        if(nrow(mo) >= 1){mydata$color[which(mydata$rowindex %in% unique(mo$row))]="tagged"}
        mydata$predictedValue <- mydata[,input$traitOutqPheno]
        mydata <- mydata[,which(!duplicated(colnames(mydata)))]
        p <- ggplot2::ggplot(mydata, ggplot2::aes(x=as.factor(environment), y=predictedValue)) +
          ggplot2::geom_boxplot(fill='#A4A4A4', color="black", notch = TRUE, outliers = FALSE)+
          ggplot2::theme_classic()+ ggplot2::ggtitle("Preview of outliers that would be tagged using current input parameters above for the trait selected") +
          ggplot2::geom_jitter(ggplot2::aes(color = color), alpha = input$transparency) +
          ggplot2::xlab("Environment") + ggplot2::ylab("Trait value") +
          ggplot2::scale_color_manual(values = c(valid = "#66C2A5", tagged = "#FC8D62")) # specifying colors names avoids having valid points in orange in absence of potential outliers. With only colour = color, valid points are in orange in that case.
        if(input$checkbox){
          p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1))
        }else{
          p <- p + ggplot2::theme(axis.text.x = ggplot2::element_blank())
        }
        p
      }else{}
    })

    ## display the current outliers
    observeEvent(data(),{
      req(data())
      output$modificationsQa <-  DT::renderDT({
        mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
        if(mappedColumns == 3){ # all required columns are present
          req(input$outlierCoefOutqFont)
          req(input$outlierCoefOutqPheno)
          req(input$traitOutqPhenoMultiple)
          ## get the outlier table
          outlier <- list()
          for(iTrait in input$traitOutqPhenoMultiple){
            outlier[[iTrait]] <- cgiarPipeline::newOutliersFun(myObject=data(), trait=iTrait, outlierCoefOutqPheno=input$outlierCoefOutqPheno)
          }
          outlier <- do.call(rbind,outlier)
          removeCols <- c("module","analysisId","value")
          outlier <- outlier[, setdiff(colnames(outlier),removeCols)]
          colnames(outlier) <- cgiarBase::replaceValues(Source = colnames(outlier), Search = "row", Replace = "record")
          ## add phenotypic data
          dtQaRaw <- data()
          dtQaRaw <- dtQaRaw$data$pheno
          dtQaRaw$outlierRow <- 1:nrow(dtQaRaw)
          removeCols <- c("stage","pipeline","country","year","season","location","trial","gid")
          dtQaRaw <- dtQaRaw[,unique(c("outlierRow",setdiff(colnames(dtQaRaw), removeCols)))]
          ## merge
          myTable <- base::merge(outlier,dtQaRaw, by.x="record", by.y="outlierRow", all.x=TRUE)
          myTable <- myTable[!duplicated(paste(myTable$record, myTable$trait)),]
          myTable <- myTable[!is.na(myTable$record),]
          DT::datatable(myTable, extensions = 'Buttons',
                        options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                       lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All'))),
                        caption = htmltools::tags$caption(
                          style = 'color:cadetblue', #caption-side: bottom; text-align: center;
                          htmltools::em('Please check potential outliers and click on relevant data points to keep them in the workflow.')
                        )
          )

          # DT::datatable(myTable, filter = "top", # extensions = 'Buttons',
          #               caption = htmltools::tags$caption(
          #                 style = 'color:cadetblue', #caption-side: bottom; text-align: center;
          #                 htmltools::em('Please check potential outliers and click on relevant data points to keep them in the workflow.')
          #               )
          # )
        }
      }, server = FALSE)
    })

    myTablePrevious <-  reactive({
      mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
      if(mappedColumns == 3){ # all required columns are present
        req(input$traitOutqPhenoMultiple)
        req(input$outlierCoefOutqFont)
        # req(input$outlierCoefOutqPheno)
        # get the outlier table
        outlier <- list()
        for(iTrait in input$traitOutqPhenoMultiple){
          outlier[[iTrait]] <- cgiarPipeline::newOutliersFun(myObject=data(), trait=iTrait, outlierCoefOutqPheno=input$outlierCoefOutqPheno)
        }
        outlier <- do.call(rbind,outlier)
        removeCols <- c("module","analysisId","value")
        outlier <- outlier[, setdiff(colnames(outlier),removeCols)]
        colnames(outlier) <- cgiarBase::replaceValues(Source = colnames(outlier), Search = "row", Replace = "record")
        ## add phenotypic data
        dtQaRaw <- data()
        dtQaRaw <- dtQaRaw$data$pheno
        dtQaRaw$outlierRow <- 1:nrow(dtQaRaw)
        removeCols <- c("stage","pipeline","country","year","season","location","trial","gid")
        dtQaRaw <- dtQaRaw[,unique(c("outlierRow",setdiff(colnames(dtQaRaw), removeCols)))]
        ## merge
        myTable <- base::merge(outlier,dtQaRaw, by.x="record", by.y="outlierRow", all.x=TRUE)
        myTable <- myTable[!duplicated(myTable$record),]
      }
    })

    # filter the selected outliers
    myTableSelected <- reactive({
      mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
      if(mappedColumns == 3){ # all required columns are present
        req(input$outlierCoefOutqFont)
        # req(input$outlierCoefOutqPheno)
        req(input$traitOutqPheno)
        ## get the outlier table
        outlier <- list()
        for(iTrait in input$traitOutqPhenoMultiple){
          outlier[[iTrait]] <- cgiarPipeline::newOutliersFun(myObject=data(), trait=iTrait, outlierCoefOutqPheno=input$outlierCoefOutqPheno)
        }
        outlier <- do.call(rbind,outlier)
        removeCols <- c("module","analysisId","value")
        outlier <- outlier[, setdiff(colnames(outlier),removeCols)]
        colnames(outlier) <- cgiarBase::replaceValues(Source = colnames(outlier), Search = "row", Replace = "record")
        ## add phenotypic data
        dtQaRaw <- data()
        dtQaRaw <- dtQaRaw$data$pheno
        dtQaRaw$outlierRow <- 1:nrow(dtQaRaw)
        removeCols <- c("stage","pipeline","country","year","season","location","trial","gid")
        dtQaRaw <- dtQaRaw[,unique(c("outlierRow",setdiff(colnames(dtQaRaw), removeCols)))]
        ## merge
        myTable <- base::merge(outlier,dtQaRaw, by.x="record", by.y="outlierRow", all.x=TRUE)
        myTable <- myTable[-input$modificationsQa_rows_selected,]
        myTable <- myTable[!duplicated(myTable$record),]
      }
    })

    myTableNoSelected <- reactive({
      mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
      if(mappedColumns == 3){ # all required columns are present
        req(input$outlierCoefOutqFont)
        # req(input$outlierCoefOutqPheno)
        req(input$traitOutqPheno)
        ## get the outlier table
        outlier <- list()
        for(iTrait in input$traitOutqPhenoMultiple){
          outlier[[iTrait]] <- cgiarPipeline::newOutliersFun(myObject=data(), trait=iTrait, outlierCoefOutqPheno=input$outlierCoefOutqPheno)
        }
        outlier <- do.call(rbind,outlier)
        removeCols <- c("module","analysisId","value")
        outlier <- outlier[, setdiff(colnames(outlier),removeCols)]
        colnames(outlier) <- cgiarBase::replaceValues(Source = colnames(outlier), Search = "row", Replace = "record")
        ## add phenotypic data
        dtQaRaw <- data()
        dtQaRaw <- dtQaRaw$data$pheno
        dtQaRaw$outlierRow <- 1:nrow(dtQaRaw)
        removeCols <- c("stage","pipeline","country","year","season","location","trial","gid")
        dtQaRaw <- dtQaRaw[,unique(c("outlierRow",setdiff(colnames(dtQaRaw), removeCols)))]
        ## merge
        myTable <- base::merge(outlier,dtQaRaw, by.x="record", by.y="outlierRow", all.x=TRUE)
        rowSelected <- myTable[input$modificationsQa_rows_selected,]
        rowSelected
      }
    })

    ## save when user clicks

    # output$value <- renderPrint({ input$outlierCoefOutqPheno })

    outQaRaw <- outQaRaw2 <- eventReactive(input$runQaRaw, {
      req(data())
      req(input$traitOutqPhenoMultiple)
      # req(input$outlierCoefOutqPheno)
      mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
      if(mappedColumns == 3){ # all required columns are present

        shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
        ## get the outlier table
        outlier <- list()
        analysisId <- as.numeric(Sys.time())

        for(iTrait in input$traitOutqPhenoMultiple){
          outliers <- cgiarPipeline::newOutliersFun(myObject=data(), trait=iTrait, outlierCoefOutqPheno=input$outlierCoefOutqPheno)
          outliers <- outliers[which(is.na(outliers$analysisId)),,drop=FALSE]
          removeCols <- c("module","analysisId","value")
          outliers <- outliers[, setdiff(colnames(outliers),removeCols)]
          colnames(outliers) <- cgiarBase::replaceValues(Source = colnames(outliers), Search = "row", Replace = "record")
          outliers$analysisId <- analysisId
          if (nrow(myTableNoSelected()) > 0) {
            selections <- subset(myTableSelected(), select = c("record",iTrait))
          } else {
            selections <- subset(myTablePrevious(), select = c("record",iTrait))
          }
          outliers <- base::merge(outliers,selections, by.x="record")
          colnames(outliers) <- cgiarBase::replaceValues(Source = colnames(outliers), Search = "record", Replace = "row")
          colnames(outliers) <- cgiarBase::replaceValues(Source = colnames(outliers), Search = iTrait, Replace = "value")
          outliers$module <- "qaRaw"
          outliers <- outliers[,c("module","analysisId", "trait","reason","row","value")]
          outlier[[iTrait]] <- outliers
        }
        outlier <- do.call(rbind,outlier)
        ## get data structure
        result <- data()
        ## bind new parameters
        if(is.null(result$modifications$pheno)){
          result$modifications$pheno <- outlier
        }else{
          result$modifications$pheno <- rbind(result$modifications$pheno, outlier[,colnames(result$modifications$pheno)])
        }
        # add status table
        newStatus <- data.frame(module="qaRaw", analysisId=analysisId, analysisIdName=input$analysisIdName )
        if(!is.null(result$status)){
          result$status <- rbind(result$status, newStatus[,colnames(result$status)])
        }else{result$status <- newStatus}
        # myId <- result$status
        # add modeling table
        provMet <- list()
        for(iTrait in input$traitOutqPhenoMultiple){
          provMet[[iTrait]] <- data.frame(module="qaRaw",analysisId=analysisId, trait=iTrait, environment=NA,
                                          parameter= "outlierCoefOutqPheno", value= input$outlierCoefOutqPheno)
        }
        provMet <- do.call(rbind,provMet)
        if(is.null(result$modeling)){
          result$modeling <- provMet
        }else{
          result$modeling <- rbind(result$modeling, provMet[,colnames(result$modeling)])
        }
        data(result)
        # cat(crayon::green(paste("QA step with id:",as.POSIXct( analysisId, origin="1970-01-01", tz="GMT"),"for trait",paste(input$traitOutqPhenoMultiple, collapse = ", "),"saved. Now you can proceed to perform Single Trial Analysis.")))
        cat(paste("QA step with id:",as.POSIXct( analysisId, origin="1970-01-01", tz="GMT"),"for trait",paste(input$traitOutqPhenoMultiple, collapse = ", "),"saved. Now you can proceed to perform Single Trial Analysis."))
        updateTabsetPanel(session, "tabsMain", selected = "outputTabs")
        shinybusy::remove_modal_spinner()

        if(!inherits(result,"try-error")) { # if all goes well in the run
          # ## Report tab
          output$reportQaPheno <- renderUI({
            HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportQaPheno.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
          })

          output$downloadReportQaPheno <- downloadHandler(
            filename = function() {
              paste(paste0('qaRaw_dashboard_',gsub("-", "", as.integer(Sys.time()))), sep = '.', switch(
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

    output$outQaRaw <- renderPrint({
      outQaRaw()
    })

  })
}

## To be copied in the UI
# mod_qaRawApp_ui("qaPhenoApp_1")

## To be copied in the server
# mod_qaRawApp_server("qaPhenoApp_1")

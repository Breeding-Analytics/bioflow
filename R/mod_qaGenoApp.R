#' qaGenoApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_qaGenoApp_ui <- function(id){
  ns <- NS(id)
  tagList(


    # input <- list(propNaUpperThreshForMarker=0.3,propNaUpperThreshForInds=0.3,maf=0.5,imputationMethod="median", ploidy=2 )
    shiny::sidebarPanel(#width = 3,
      width = 3,
      tags$style(".well {background-color:grey; color: #FFFFFF;}"),
      div(tags$p( h4(strong("Quality Assurance for Genetic Markers")))),#, style = "color: #817e7e"
      hr(style = "border-top: 1px solid #4c4c4c;"),

      numericInput(ns("propNaUpperThreshForMarker"), label = "Threshold for missing data in markers", value = .3, step = .05, max = 1, min = 0),
      numericInput(ns("propNaUpperThreshForInds"), label = "Threshold for missing data in individuals", value = .3, step = .05, max = 1, min = 0),
      numericInput(ns("maf"), label = "Minor allele frequency", value = .05, step = .05, max = 1, min = 0),
      numericInput(ns("propHetUpperThreshForMarker"), label = "Threshold for heterozygosity in markers", value = .05, step = .05, max = 1, min = 0),
      numericInput(ns("propFisUpperThreshForMarker"), label = "Threshold for inbreeding in markers", value = .05, step = .05, max = 1, min = 0),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      shinydashboard::box(width = 12, status = "primary", background="light-blue",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Settings...",
                          selectInput(ns("imputationMethod"), "Imputation method", choices = c("median"), multiple = FALSE),
                          numericInput(ns("ploidy"), label = "Ploidy", value = 2, step=2, max = 10, min=2)
      ),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      actionButton(ns("runQaMb"), "Save modifications", icon = icon("play-circle")),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      shinycssloaders::withSpinner(textOutput(ns("outQaMb")),type=8),
      # uiOutput(ns('navigate')),
    ), # end sidebarpanel
    shiny::mainPanel(width = 9,
                     tabsetPanel( #width=9,
                       type = "tabs",


                       tabPanel("Overview",
                                br(),
                                shinydashboard::box(status="primary",width = 12,
                                                    solidHeader = TRUE,
                                                    plotly::plotlyOutput(ns("plotPredictionsCleanOutMarker"))
                                )
                       ),
                       tabPanel("Modifications table",
                                br(),
                                shinydashboard::box(status="primary",width = 12,
                                                    solidHeader = TRUE,
                                                    column(width=12,DT::DTOutput(ns("modificationsQaMarker")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;"),
                                                    downloadButton(ns('downnloadData'), 'Download data')
                                )
                       ),
                       tabPanel("Documentation",
                                br(),
                                shinydashboard::box(status="primary",width = 12,
                                                    solidHeader = TRUE,
                                                    column(width=12,   style = "height:800px; overflow-y: scroll;overflow-x: scroll;",
                                                           tags$body(
                                                             h1(strong("Details")),
                                                             p("This option aims to allow users to select model-based outliers based on plot whiskers and absolute values applied on conditional residuals.
                                The way arguments are used is the following:"),
                                                             p(strong("Outlier coefficient.-")," this determines how far the plot whiskers extend out from the box. If coef is positive, the whiskers extend to the most extreme data point which is no more than coef times the length of the box away from the box. A value of zero causes the whiskers to extend to the data extremes (and no outliers be returned)."),
                                                             p(strong("Trait lower bound.-"),"Lower bound threshold determining as an outlier any value smaller than this."),
                                                             p(strong("Trait upper bound.-"),"Upper bound threshold determining as an outlier any value greater than this."),
                                                             h2(strong("References")),
                                                             p("Tukey, J. W. (1977). Exploratory Data Analysis. Section 2C."),
                                                             p("McGill, R., Tukey, J. W. and Larsen, W. A. (1978). Variations of box plots. The American Statistician, 32, 12–16. doi:10.2307/2683468."),
                                                             p("Velleman, P. F. and Hoaglin, D. C. (1981). Applications, Basics and Computing of Exploratory Data Analysis. Duxbury Press.")
                                                           )
                                                    ),

                                )
                       )
                     )) # end mainpanel


  )
}

#' qaGenoApp Server Functions
#'
#' @noRd
mod_qaGenoApp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })
    ############################################################################

    data = reactive({ # provisional dataset for testing
      load("dataStr0.RData")
      data <- res
      return(data)
    })

    ##
    newModifications <- reactive({ # p('File to be analyzed')
      req(data())
      req(input$propNaUpperThreshForMarker)
      req(input$propNaUpperThreshForInds)
      req(input$maf)
      req(input$propHetUpperThreshForMarker)
      req(input$propFisUpperThreshForMarker)
      req(input$imputationMethod)
      myObject <- data()
      res <- cgiarBase::computeGenoModifications(
        M = myObject$data$geno, # markers are assumed to come centered
        propNaUpperThreshForMarker=input$propNaUpperThreshForMarker,
        propNaUpperThreshForInds=input$propNaUpperThreshForInds,
        maf=input$maf, ploidy=input$ploidy,
        imputationMethod=input$imputationMethod
      )
      ## reactive
      return(res)
    })

    ## render the expected result for markers
    output$plotPredictionsCleanOutMarker <- plotly::renderPlotly({
      req(data())
      req(input$propNaUpperThreshForMarker)
      req(input$propNaUpperThreshForInds)
      req(input$maf)
      req(input$propHetUpperThreshForMarker)
      req(input$propFisUpperThreshForMarker)
      req(input$imputationMethod)
      mydata <- data()$data$geno
      ##
      fig <- plotly::plot_ly(alpha = 0.6)
      ## historgram for marker with missing data
      propNaMarker <- apply(mydata,2,function(x){(length(which(is.na(x)))/length(x))})
      fig <- fig %>% plotly::add_histogram(x = propNaMarker, histnorm = "probability", name="Missing data in markers" )
      ## historgram for individuals with missing data
      propNaIndividual <- apply(mydata,1,function(x){(length(which(is.na(x)))/length(x))})
      fig <- fig %>% plotly::add_histogram(x = propNaIndividual, histnorm = "probability", name="Missing data in individuals" )
      ## historgram for MAF
      MAF <- apply(mydata+1, 2, function(x) {AF <- mean(x, na.rm = T)/input$ploidy;MAF <- ifelse(AF > 0.5, 1 - AF, AF) })
      fig <- fig %>% plotly::add_histogram(x = MAF, histnorm = "probability", name="Minor allele frequency" )
      ## histogram for heterozigosity
      q <- MAF; p <- 1 - q
      he <- 2 * p * q;  ho <- colMeans((mydata+1) == 1, na.rm = TRUE) # Get the obseved heterozygosity.
      fig <- fig %>% plotly::add_histogram(x = ho, histnorm = "probability", name="Observed heterozygosity" )
      ## histogram for inbreeding
      Fis <- ifelse(he == 0, yes = 0, no = 1 - (ho / he))
      fig <- fig %>% plotly::add_histogram(x = Fis, histnorm = "probability", name="Inbreeding" )
      ## add threshold lines and layout
      vline1 <- function(x = 0, color = "blue") {list( type = "line",y0 = 0,y1 = 1,yref = "paper",x0 = x, x1 = x,line = list(color = color, dash="dot"))}
      vline2 <- function(x = 0, color = "red") {list( type = "line",y0 = 0,y1 = 1,yref = "paper",x0 = x, x1 = x,line = list(color = color, dash="dot"))}
      vline3 <- function(x = 0, color = "green") {list( type = "line",y0 = 0,y1 = 1,yref = "paper",x0 = x, x1 = x,line = list(color = color, dash="dot"))}
      fig <- fig %>% plotly::layout(barmode = "overlay", xaxis = list(title = "Percentage of data"), yaxis = list(title = "Proportion of the population" ),
                                    shapes = list(vline1(input$propNaUpperThreshForMarker),vline2(input$propNaUpperThreshForInds),vline3(input$maf) ) )
      fig
    })

    ## display the current outliers
    observeEvent(data(),{

      output$modificationsQaMarker <-  DT::renderDT({
        req(data())
        req(input$propNaUpperThreshForMarker)
        req(input$propNaUpperThreshForInds)
        req(input$maf)
        req(input$propHetUpperThreshForMarker)
        req(input$propFisUpperThreshForMarker)
        req(input$imputationMethod)
        mo <- newModifications()
        DT::datatable(mo,options = list(autoWidth = TRUE), filter = "top")

      })

    })

    output$downnloadData <- downloadHandler(
      filename = function() {
        paste("tableOutliersMarkers-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        toDownload <-  newModifications()
        utils::write.csv(toDownload, file)
      })

    ##########################
    ## save when user clicks

    outQaMb <- eventReactive(input$runQaMb, {

      req(data())
      req(input$propNaUpperThreshForMarker)
      req(input$propNaUpperThreshForInds)
      req(input$maf)
      req(input$propHetUpperThreshForMarker)
      req(input$propFisUpperThreshForMarker)
      req(input$imputationMethod)
      ## get the outlier table
      mods <- newModifications()
      ## store the new modifications table
      temp <- data()
      temp$modifications$geno <- rbind(temp$modifications$geno, mods )
      ## write the new status table
      newStatus <- data.frame(module="qaMb", analysisId= mods$analysisId[nrow(mods)])
      temp$status <- rbind(temp$status, newStatus)
    })
    output$outQaMb <- renderPrint({
      outQaMb()
    })
    #


  })
}

## To be copied in the UI
# mod_qaGenoApp_ui("qaGenoApp_1")

## To be copied in the server
# mod_qaGenoApp_server("qaGenoApp_1")

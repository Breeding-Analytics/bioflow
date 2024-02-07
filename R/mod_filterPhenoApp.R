#' filterPhenoApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_filterPhenoApp_ui <- function(id){
  ns <- NS(id)
  tagList(


    shiny::sidebarPanel(#width = 3,
      width = 5,
      tags$style(".well {background-color:grey; color: #FFFFFF;}"),
      HTML("<img src='www/cgiar3.png' width='42' vspace='10' hspace='10' height='46' align='top'>
                  <font size='5'>Data filtering</font>"),
      hr(style = "border-top: 1px solid #4c4c4c;"),

      selectInput(ns("traitFilterPheno"), "Trait to filter", choices = NULL, multiple = FALSE),
      selectInput(ns("years"), "Years to keep", choices = NULL, multiple = TRUE),
      selectInput(ns("seasons"), "Seasons to keep", choices = NULL, multiple = TRUE),
      selectInput(ns("countries"), "Countries to keep", choices = NULL, multiple = TRUE),
      selectInput(ns("locations"), "Locations to keep", choices = NULL, multiple = TRUE),
      selectInput(ns("trials"), "Trials to keep", choices = NULL, multiple = TRUE),
      selectInput(ns("environments"), "Environments to keep", choices = NULL, multiple = TRUE),

      hr(style = "border-top: 1px solid #4c4c4c;"),
      shinydashboard::box(width = 12, status = "success", background="green",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Settings...",

      ),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      actionButton(ns("runFilterRaw"), "Filter dataset", icon = icon("play-circle")),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      textOutput(ns("outFilterRaw")),
    ), # end sidebarpanel
    shiny::mainPanel(width = 7,
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
                                                             p("The first step in genetic evaluation is to ensure that input phenotypic records are of good quality.
                                                             This option aims to allow users to filter (more concretely tag records for exclusion) specific years, seasons, countries, locations, etc. from the posterior analyses.
                                The way arguments are used is the following:"),
                                                             # img(src = "www/qaRaw.png", height = 300, width = 600), # add an image
                                                             p(strong("label.-")," the different columns to subset the phenotypic dataset to exclude certain environments for certain traits."),
                                                             h2(strong("References")),
                                                             p("Tukey, J. W. (1977). Exploratory Data Analysis. Section 2C."),
                                                             p("Velleman, P. F. and Hoaglin, D. C. (1981). Applications, Basics and Computing of Exploratory Data Analysis. Duxbury Press.")
                                                           )
                                                    )
                                )
                       ),
                       tabPanel(p("Output", class="output-p"), icon = icon("arrow-right-from-bracket"),
                                tabsetPanel(
                                  tabPanel("Phenotype filter", icon = icon("magnifying-glass-chart"),
                                           br(),
                                           shinydashboard::box(status="success",width = 12, #background = "green",
                                                               solidHeader = TRUE,
                                                               column(width=12, shiny::plotOutput(ns("plotFilterOut")) ),
                                                               column(width=12,DT::DTOutput(ns("modificationsFilter")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
                                           )
                                  )
                                ) # end of tabset
                       )# end of output panel
                     )) # end mainpanel


  )
}

#' filterPhenoApp Server Functions
#'
#' @noRd
mod_filterPhenoApp_server <- function(id, data){
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
        mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
        if(mappedColumns == 3){ HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to filter your data and visualize it under the 'Output' section.")) )
        }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please make sure that the columns: 'environment', 'designation' and \n at least one trait have been mapped using the 'Data Retrieval' tab.")) )
        }
      }
    )
    # Create the traits
    observeEvent(data(), {
      req(data())
      dtQaRaw <- data(); dtQaRaw <- dtQaRaw$metadata$pheno
      traitsQaRaw <- unique(dtQaRaw[dtQaRaw$parameter=="trait","value"])
      updateSelectInput(session, "traitFilterPheno",choices = traitsQaRaw)
      shinyjs::hide(ns("traitFilterPheno"))
    })
    # create the years
    observeEvent(c(data(), input$traitFilterPheno), {
      req(data()); req(input$traitFilterPheno)
      object <- data()
      mtdtQaRaw <- object$metadata$pheno; dtQaRaw <- object$data$pheno
      traitsQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="year","value"]) # column for year
      if(length(traitsQaRaw)>0){
        newLevels <- na.omit(unique(dtQaRaw[which(!is.na(dtQaRaw[,input$traitFilterPheno])),traitsQaRaw]))
      }else{newLevels <- "all"}
      updateSelectInput(session, "years",choices = newLevels, selected = newLevels)
      shinyjs::hide(ns("years"))
    })
    # create the seasons
    observeEvent(c(data(), input$traitFilterPheno, input$years), {
      req(data()); req(input$years)
      object <- data()
      mtdtQaRaw <- object$metadata$pheno; dtQaRaw <- object$data$pheno
      yearQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="year","value"]) # column for year
      seasonQaQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="season","value"]) # column for year
      if(length(seasonQaQaRaw)>0){
        reduced1 <- dtQaRaw[which(!is.na(dtQaRaw[,input$traitFilterPheno])),]
        if("all" %in%  input$years){reduced2 <- reduced1}else{reduced2 <- reduced1[which(reduced1[,yearQaRaw] %in% input$years),]}
        newLevels <- na.omit(unique(reduced2[,seasonQaQaRaw]))
      }else{newLevels <- "all"}
      updateSelectInput(session, "seasons",choices = newLevels, selected = newLevels)
      shinyjs::hide(ns("seasons"))
    })
    # create the countries
    observeEvent(c(data(), input$traitFilterPheno, input$years, input$seasons), {
      req(data()); req(input$seasons)
      object <- data()
      mtdtQaRaw <- object$metadata$pheno; dtQaRaw <- object$data$pheno
      yearQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="year","value"]) # column for year
      seasonQaQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="season","value"]) # column for year
      countryQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="country","value"]) # column for year
      if(length(countryQaRaw)>0){
        reduced1 <- dtQaRaw[which(!is.na(dtQaRaw[,input$traitFilterPheno])),]
        if("all" %in%  input$years){reduced2 <- reduced1}else{reduced2 <- reduced1[which(reduced1[,yearQaRaw] %in% input$years),]}
        if("all" %in%  input$seasons){reduced3 <- reduced2}else{reduced3 <- reduced2[which(reduced2[,seasonQaQaRaw] %in% input$seasons),] }
        newLevels <- na.omit(unique(reduced3[,countryQaRaw]))
      }else{newLevels <- "all"}
      updateSelectInput(session, "countries",choices = newLevels, selected = newLevels)
      shinyjs::hide(ns("countries"))
    })
    # create the locations
    observeEvent(c(data(), input$traitFilterPheno, input$years, input$seasons, input$countries), {
      req(data());  req(input$countries)
      object <- data()
      mtdtQaRaw <- object$metadata$pheno; dtQaRaw <- object$data$pheno
      yearQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="year","value"]) # column for year
      seasonQaQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="season","value"]) # column for year
      countryQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="country","value"]) # column for year
      locationQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="location","value"]) # column for year
      if(length(locationQaRaw)>0){
        reduced1 <- dtQaRaw[which(!is.na(dtQaRaw[,input$traitFilterPheno])),]
        if("all" %in%  input$years){reduced2 <- reduced1}else{reduced2 <- reduced1[which(reduced1[,yearQaRaw] %in% input$years),]}
        if("all" %in%  input$seasons){reduced3 <- reduced2}else{reduced3 <- reduced2[which(reduced2[,seasonQaQaRaw] %in% input$seasons),] }
        if("all" %in%  input$countries){reduced4 <- reduced3}else{reduced4 <- reduced3[which(reduced3[,countryQaRaw] %in% input$countries),]}
        newLevels <- na.omit(unique(reduced4[,locationQaRaw]))
      }else{newLevels <- "all"}
      updateSelectInput(session, "locations",choices = newLevels, selected = newLevels)
      shinyjs::hide(ns("locations"))
    })
    # create the trials
    observeEvent(c(data(), input$traitFilterPheno, input$years, input$seasons, input$countries, input$locations), {
      req(data());  req(input$locations)
      object <- data()
      mtdtQaRaw <- object$metadata$pheno; dtQaRaw <- object$data$pheno
      yearQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="year","value"]) # column for year
      seasonQaQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="season","value"]) # column for year
      countryQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="country","value"]) # column for year
      locationQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="location","value"]) # column for year
      trialQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="trial","value"]) # column for year
      if(length(trialQaRaw)>0){
        reduced1 <- dtQaRaw[which(!is.na(dtQaRaw[,input$traitFilterPheno])),]
        if("all" %in%  input$years){reduced2 <- reduced1}else{reduced2 <- reduced1[which(reduced1[,yearQaRaw] %in% input$years),]}
        if("all" %in%  input$seasons){reduced3 <- reduced2}else{reduced3 <- reduced2[which(reduced2[,seasonQaQaRaw] %in% input$seasons),] }
        if("all" %in%  input$countries){reduced4 <- reduced3}else{reduced4 <- reduced3[which(reduced3[,countryQaRaw] %in% input$countries),]}
        if("all" %in%  input$locations){reduced5 <- reduced4}else{reduced5 <- reduced4[which(reduced4[,locationQaRaw] %in% input$locations),]}
        newLevels <- na.omit(unique(reduced5[,trialQaRaw]))
      }else{newLevels <- "all"}
      updateSelectInput(session, "trials",choices = newLevels, selected = newLevels)
      shinyjs::hide(ns("trials"))
    })
    # create the environments
    observeEvent(c(data(), input$traitFilterPheno, input$years, input$seasons, input$countries, input$locations, input$trials), {
      req(data());  req(input$trials)
      object <- data()
      mtdtQaRaw <- object$metadata$pheno; dtQaRaw <- object$data$pheno
      yearQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="year","value"]) # column for year
      seasonQaQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="season","value"]) # column for year
      countryQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="country","value"]) # column for year
      locationQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="location","value"]) # column for year
      trialQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="trial","value"]) # column for year
      environmentQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="environment","value"]) # column for year
      if(length(environmentQaRaw)>0){
        reduced1 <- dtQaRaw[which(!is.na(dtQaRaw[,input$traitFilterPheno])),]
        if("all" %in%  input$years){reduced2 <- reduced1}else{reduced2 <- reduced1[which(reduced1[,yearQaRaw] %in% input$years),]}
        if("all" %in%  input$seasons){reduced3 <- reduced2}else{reduced3 <- reduced2[which(reduced2[,seasonQaQaRaw] %in% input$seasons),] }
        if("all" %in%  input$countries){reduced4 <- reduced3}else{reduced4 <- reduced3[which(reduced3[,countryQaRaw] %in% input$countries),]}
        if("all" %in%  input$locations){reduced5 <- reduced4}else{reduced5 <- reduced4[which(reduced4[,locationQaRaw] %in% input$locations),]}
        if("all" %in%  input$trials){reduced6 <- reduced5}else{reduced6 <- reduced5[which(reduced5[,trialQaRaw] %in% input$trials),]}
        newLevels <- na.omit(unique(reduced6[,environmentQaRaw]))
      }else{newLevels <- "all"}
      updateSelectInput(session, "environments",choices = newLevels, selected = newLevels)
      shinyjs::hide(ns("environments"))
    })
    ## function to calculate outliers
    newOutliers <- reactive({ #
      req(data());  req(input$traitFilterPheno); req(input$years);  req(input$seasons); req(input$countries); req(input$locations); req(input$trials); req(input$environments)
      myObject <- data()
      mydata <- data()$data$pheno
      ### change column names for mapping
      paramsPheno <- data()$metadata$pheno
      paramsPheno <- paramsPheno[which(paramsPheno$parameter != "trait"),]
      colnames(mydata) <- cgiarBase::replaceValues(colnames(mydata), Search = paramsPheno$value, Replace = paramsPheno$parameter )
      mydata$rowindex <- 1:nrow(mydata)
      # start
      if("all" %in%  input$years){keep1 <- 1:nrow(mydata)}else{keep1 <- which(mydata[,"year"] %in% input$years) }
      if("all" %in%  input$seasons){keep2 <- 1:nrow(mydata)}else{keep2 <- which(mydata[,"season"] %in% input$seasons) }
      if("all" %in%  input$countries){keep3 <- 1:nrow(mydata)}else{keep3 <- which(mydata[,"country"] %in% input$countries) }
      if("all" %in%  input$locations){keep4 <- 1:nrow(mydata)}else{keep4 <- which(mydata[,"location"] %in% input$locations) }
      if("all" %in%  input$trials){keep5 <- 1:nrow(mydata)}else{keep5 <- which(mydata[,"trial"] %in% input$trials) }
      if("all" %in%  input$environments){keep6 <- 1:nrow(mydata)}else{keep6 <- which(mydata[,"environment"] %in% input$environments) }
      keep <- Reduce(intersect, list(keep1,keep2,keep3,keep4,keep5,keep6) )#which( (mydata[,"year"] %in% input$years) & (mydata[,"season"] %in% input$seasons) & (mydata[,"country"] %in% input$countries) & (mydata[,"location"] %in% input$locations) & (mydata[,"trial"] %in% input$trials) & (mydata[,"environment"] %in% input$environments) )
      toSilence <- setdiff(mydata$rowindex, keep)
      if(length(toSilence) > 0){
        myoutliersReduced <- data.frame(module="qaRaw",analysisId=NA,trait=input$traitFilterPheno,reason="outlierIQR",row=toSilence, value=NA);
      }else{
        myoutliersReduced <- data.frame(matrix(nrow=0, ncol=6))
        colnames(myoutliersReduced) <- c("module" ,"analysisId" ,"trait","reason","row" , "value" )
      }
      return(myoutliersReduced)
    }) # returns the ones to exclude
    outFilterRaw <- eventReactive(input$runFilterRaw, {
      req(data());  req(input$traitFilterPheno); req(input$years);  req(input$seasons); req(input$countries); req(input$locations); req(input$trials); req(input$environments)
      myObject <- data()
      shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
      outliers <- newOutliers()
      myoutliers <- myObject$modifications$pheno
      analysisId <- as.numeric(Sys.time())
      myoutliersReduced <- unique(rbind(myoutliers, outliers))
      if(nrow(outliers) == 0){ # no outliers found
        cat("No data to filter.")
      }else{ # we found outliers
        outliers$analysisId <- analysisId
        cat(paste("Filtering step with id:",analysisId,"for trait",input$traitFilterPheno,"saved."))
      }
      myObject$modifications$pheno <- myoutliersReduced
      data(myObject)
      shinybusy::remove_modal_spinner()
    })
    output$outFilterRaw <- renderPrint({
      outFilterRaw()
    })
    # ## render the expected result
    output$plotFilterOut <- shiny::renderPlot({
      req(data());  req(input$traitFilterPheno); req(input$years);  req(input$seasons); req(input$countries); req(input$locations); req(input$trials); req(input$environments)
      dtMta <- data()$data$pheno
      outs <- newOutliers()
      ## we need to remove the rows from the outliers
      if(nrow(outs) > 0){
        keep <- setdiff(1:nrow(dtMta), outs$row)
        dtMta <- dtMta[keep,]
      }
      ##
      mtdtQaRaw <- data()$metadata$pheno;
      traitsQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter%in%c("trial", "year","season","country","location","environment"),"value"]) # column for year
      envsQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter%in%c("environment"),"value"]) # column for year
      xx <- unique(dtMta[,traitsQaRaw])
      X <- list()
      for(kEffect in traitsQaRaw){
        if(length(table(xx[,kEffect])) > 1){
          X[[kEffect]] <- model.matrix(as.formula(paste("~", kEffect,"-1")),data = xx)
        }
      }
      X <- do.call(cbind, X); rownames(X) <- xx[,envsQaRaw]
      hc <- hclust(dist(X), "ave")
      dend1 <- as.dendrogram(hc) # "print()" method
      plot(dend1)
    })

    ## display the current outliers
    output$modificationsFilter <-  DT::renderDT({
      if(!inherits(result,"try-error") ){
        DT::datatable(newOutliers(), extensions = 'Buttons',
                                      options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                     lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
        )
      }
    })

    ## save when user clicks

  })
}

## To be copied in the UI
# mod_filterPhenoApp_ui("filterPhenoApp_1")

## To be copied in the server
# mod_filterPhenoApp_server("filterPhenoApp_1")

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

    shiny::mainPanel(width = 12,
                     tabsetPanel(id=ns("tabsMain"), #width=9,
                       type = "tabs",

                       tabPanel(div(icon("book"), "Information") ,
                                br(),
                                column(width = 6,
                                       h1(strong(span("Environment-Pheno Filtering Module", style="color:darkcyan"))),
                                       h2(strong("Data Status (wait to be displayed):")),
                                       uiOutput(ns("warningMessage")),
                                ),
                                column(width = 6,
                                       tags$body(
                                         h2(strong("Details")),
                                         p("The first step in genetic evaluation is to ensure that input phenotypic records are of good quality.
                                                             This option allows users to filter (more concretely tag records for exclusion) specific years, seasons, countries, locations, etc. from the posterior analyses. This is just an optional module that most users will not require.
                                The way arguments are used is the following:"),
                                         img(src = "www/dataFilter.png", height = 300, width = 500), # add an image
                                         p(strong("label.-")," the different columns to subset the phenotypic dataset to exclude certain years, seasons, countries, locations, trials, or environments for certain traits."),
                                         h2(strong("References")),
                                         p("Tukey, J. W. (1977). Exploratory Data Analysis. Section 2C."),
                                         p("Velleman, P. F. and Hoaglin, D. C. (1981). Applications, Basics and Computing of Exploratory Data Analysis. Duxbury Press.")
                                       )
                                ),
                       ),
                       tabPanel(div(icon("arrow-right-to-bracket"), "Input steps"),
                                tabsetPanel(
                                  tabPanel(div(  icon("dice-one"), "Set filters", icon("arrow-right") ), # icon = icon("dice-one"),
                                           br(),
                                           column(width=6, style = "background-color:grey; color: #FFFFFF",
                                                  selectInput(ns("traitFilterPheno"), "Trait to filter", choices = NULL, multiple = FALSE),
                                                  selectInput(ns("years"), "Years to keep", choices = NULL, multiple = TRUE),
                                                  selectInput(ns("seasons"), "Seasons to keep", choices = NULL, multiple = TRUE),
                                                  selectInput(ns("countries"), "Countries to keep", choices = NULL, multiple = TRUE),
                                                  selectInput(ns("locations"), "Locations to keep", choices = NULL, multiple = TRUE),
                                                  selectInput(ns("trials"), "Trials to keep", choices = NULL, multiple = TRUE),
                                                  selectInput(ns("environments"), "Environments to keep", choices = NULL, multiple = TRUE),
                                                  shinydashboard::box(width = 12, status = "success", solidHeader=FALSE,collapsible = TRUE, collapsed = TRUE, title = "Settings...",
                                                                      sliderInput(ns("slider1"), label = "Maximum #of genotypes allowed (trials with more will be removed)", min = 0,max = 500000, value = 500000)
                                                  ),
                                           ),
                                           column(width=6, #style = "height:580px; overflow-y: scroll;overflow-x: scroll;",
                                                  column(width=12),
                                                  shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                      hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                      h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameter values to be specified in the grey boxes above.", style="color:green"))),
                                                                      hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                      # p(span("Network plot showing the dependencies between years, seasons, locations, trials, etc.", style="color:black")),
                                                                      shiny::plotOutput(ns("plotFilterOut")),
                                                                      DT::DTOutput(ns("modificationsFilter")),
                                                  ),
                                           )

                                  ),
                                  tabPanel(div( icon("dice-two"), "Pick trait(s)" , icon("arrow-right") ), # icon = icon("dice-two"),
                                           br(),
                                           column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                  column(width=3, selectInput(ns("multiTraitFilter"), label = "Apply same filter to other trait(s)?", choices = list(TRUE, FALSE), selected = FALSE, multiple=FALSE) ),
                                                  column(width=9,
                                                         tags$span(id = ns('multiTraitFilter_holder'),
                                                                   selectInput(ns("traitFilterPhenoMultiple"), "Trait(s) to apply the same filters", choices = NULL, multiple = TRUE, selected = NULL),
                                                         )
                                                  ),
                                           ),
                                  ),
                                  tabPanel("Run analysis", icon = icon("dice-three"),
                                           br(),
                                           column(width=12,style = "background-color:grey; color: #FFFFFF",
                                                  column(width=3, tags$div(textInput(ns("analysisIdName"), label = tags$span(
                                                    "Analysis Name (optional)", tags$i( class = "glyphicon glyphicon-info-sign", style = "color:#FFFFFF",
                                                                                        title = "An optional name for the analysis besides the timestamp if desired.") ), #width = "100%",
                                                    placeholder = "(optional name)") ) ),
                                                  column(width=3,
                                                         br(),
                                                         actionButton(ns("runFilterRaw"), "Filter dataset", icon = icon("play-circle")),
                                                  ),
                                           ),textOutput(ns("outFilterRaw")),
                                  ),
                                ) # end of tabset
                       ),# end of input panel
                       tabPanel(div(icon("arrow-right-from-bracket"), "Output tabs" ) , value = "outputTabs",
                                tabsetPanel(
                                  tabPanel("Dashboard", icon = icon("file-image"),
                                           br(),
                                           textOutput(ns("outFilterRaw2")),
                                           br(),
                                           actionButton(ns("renderReportQaPheno"), "Download dashboard", icon = icon("download")),
                                           downloadButton(ns("downloadReportQaPheno"), "Download dashboard", style = "visibility:hidden;"),
                                           br(),
                                           uiOutput(ns('reportQaPheno'))
                                  ),
                                ),
                       ), # end of output panel
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
    observeEvent( c(data(), input$multiTraitFilter ),
                  if (input$multiTraitFilter) { # if user wants to apply filter to multiple traits
                    golem::invoke_js('showid', ns('multiTraitFilter_holder'))
                  } else { # if user wants to go trait by trait
                    golem::invoke_js('hideid', ns('multiTraitFilter_holder'))
                  }
    )
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
      dtQaRaw <- data();
      dtQaRaw <- dtQaRaw$metadata$pheno
      if("environment" %in% colnames(data()$data$pheno)){
        if(!is.null(data()$data$pheno)){
          traitsQaRaw <- unique(dtQaRaw[dtQaRaw$parameter=="trait","value"])
          updateSelectInput(session, "traitFilterPheno",choices = traitsQaRaw)
          updateSelectInput(session, "traitFilterPhenoMultiple",choices = traitsQaRaw, selected = NULL)
          shinyjs::hide(ns("traitFilterPheno"))
        }
      }
    })

    # create the years
    observeEvent(c(data(),input$traitFilterPheno), {
      req(data()); req(input$traitFilterPheno); req(input$slider1)
      if("environment" %in% colnames(data()$data$pheno)){
        object <- data()
        mtdtQaRaw <- object$metadata$pheno; dtQaRaw <- object$data$pheno
        if(length(which(mtdtQaRaw$parameter %in% c("environment","designation"))) == 2){
          envCol <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="environment","value"])
          desCol <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="designation","value"])
          ll <- split(dtQaRaw, dtQaRaw[,envCol])
          keepEnvs <- names(which( unlist(lapply(ll, function(x){length(unique(x[,desCol]))})) < input$slider1))
          dtQaRaw <- droplevels(dtQaRaw[which(dtQaRaw[,envCol] %in% keepEnvs),])

          traitsQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="year","value"]) # column for year
          if(length(traitsQaRaw) > 0){
            if( traitsQaRaw %in% colnames(dtQaRaw)  & input$traitFilterPheno %in% colnames(dtQaRaw) ){
              newLevels <-  na.omit(unique(dtQaRaw[which(!is.na(dtQaRaw[,input$traitFilterPheno])),traitsQaRaw]))
              if(length(newLevels)==0){newLevels <- "all"}
            }else{newLevels <- "all"}
          }else{newLevels <- "all"}

          updateSelectInput(session, "years",choices = newLevels, selected = newLevels)
          shinyjs::hide(ns("years"))
        }
      }
    })

    # create the seasons
    observeEvent(c(data(), input$traitFilterPheno, input$years ), {
      req(data()); req(input$years); req(input$slider1)
      if("environment" %in% colnames(data()$data$pheno)){
        object <- data()
        mtdtQaRaw <- object$metadata$pheno; dtQaRaw <- object$data$pheno
        if(length(which(mtdtQaRaw$parameter %in% c("environment","designation"))) == 2){
          envCol <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="environment","value"])
          desCol <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="designation","value"])
          keepEnvs <- names(which( unlist(lapply(split(dtQaRaw, dtQaRaw[,envCol]), function(x){length(unique(x[,desCol]))})) < input$slider1))
          dtQaRaw <- dtQaRaw[which(dtQaRaw[,envCol] %in% keepEnvs),]

          yearQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="year","value"]) # column for year
          seasonQaQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="season","value"]) # column for year

          if(length(seasonQaQaRaw) > 0){
            if(length(seasonQaQaRaw)>0 & input$traitFilterPheno %in% colnames(dtQaRaw) ){
              reduced1 <- dtQaRaw[which(!is.na(dtQaRaw[,input$traitFilterPheno])),]
              if("all" %in%  input$years){reduced2 <- reduced1}else{reduced2 <- reduced1[which(reduced1[,yearQaRaw] %in% input$years),]}
              newLevels <- na.omit(unique(reduced2[,seasonQaQaRaw]))
              if(length(newLevels)==0){newLevels <- "all"}
            }else{newLevels <- "all"}
          }else{newLevels <- "all"}

          updateSelectInput(session, "seasons",choices = newLevels, selected = newLevels)
          shinyjs::hide(ns("seasons"))
        }
      }
    })

    # create the countries
    observeEvent(c(data(),input$traitFilterPheno, input$years, input$seasons), {
      req(data()); req(input$years); req(input$seasons); req(input$slider1)
      if("environment" %in% colnames(data()$data$pheno)){
        object <- data()
        mtdtQaRaw <- object$metadata$pheno; dtQaRaw <- object$data$pheno
        if(length(which(mtdtQaRaw$parameter %in% c("environment","designation"))) == 2){
          envCol <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="environment","value"])
          desCol <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="designation","value"])
          keepEnvs <- names(which( unlist(lapply(split(dtQaRaw, dtQaRaw[,envCol]), function(x){length(unique(x[,desCol]))})) < input$slider1))
          dtQaRaw <- dtQaRaw[which(dtQaRaw[,envCol] %in% keepEnvs),]

          yearQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="year","value"]) # column for year
          seasonQaQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="season","value"]) # column for year
          countryQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="country","value"]) # column for year
          if(length(countryQaRaw) > 0){
            if(length(countryQaRaw)>0 & input$traitFilterPheno %in% colnames(dtQaRaw) ){
              reduced1 <- dtQaRaw[which(!is.na(dtQaRaw[,input$traitFilterPheno])),]
              if("all" %in%  input$years){reduced2 <- reduced1}else{reduced2 <- reduced1[which(reduced1[,yearQaRaw] %in% input$years),]}
              if("all" %in%  input$seasons){reduced3 <- reduced2}else{reduced3 <- reduced2[which(reduced2[,seasonQaQaRaw] %in% input$seasons),] }
              newLevels <- na.omit(unique(reduced3[,countryQaRaw]))
              if(length(newLevels)==0){newLevels <- "all"}
            }else{newLevels <- "all"}
          }else{newLevels <- "all"}

          updateSelectInput(session, "countries",choices = newLevels, selected = newLevels)
          shinyjs::hide(ns("countries"))
        }
      }
    })

    # create the locations
    observeEvent(c(data(), input$traitFilterPheno, input$years, input$seasons, input$countries), {
      req(data()); req(input$years); req(input$seasons);  req(input$countries); req(input$slider1)
      if("environment" %in% colnames(data()$data$pheno)){
        object <- data()
        mtdtQaRaw <- object$metadata$pheno; dtQaRaw <- object$data$pheno
        if(length(which(mtdtQaRaw$parameter %in% c("environment","designation"))) == 2){
          envCol <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="environment","value"])
          desCol <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="designation","value"])
          keepEnvs <- names(which( unlist(lapply(split(dtQaRaw, dtQaRaw[,envCol]), function(x){length(unique(x[,desCol]))})) < input$slider1))
          dtQaRaw <- dtQaRaw[which(dtQaRaw[,envCol] %in% keepEnvs),]

          yearQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="year","value"]) # column for year
          seasonQaQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="season","value"]) # column for year
          countryQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="country","value"]) # column for year
          locationQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="location","value"]) # column for year
          if(length(locationQaRaw) > 0){
            if(length(locationQaRaw)>0 & input$traitFilterPheno %in% colnames(dtQaRaw) ){
              reduced1 <- dtQaRaw[which(!is.na(dtQaRaw[,input$traitFilterPheno])),]
              if("all" %in%  input$years){reduced2 <- reduced1}else{reduced2 <- reduced1[which(reduced1[,yearQaRaw] %in% input$years),]}
              if("all" %in%  input$seasons){reduced3 <- reduced2}else{reduced3 <- reduced2[which(reduced2[,seasonQaQaRaw] %in% input$seasons),] }
              if("all" %in%  input$countries){reduced4 <- reduced3}else{reduced4 <- reduced3[which(reduced3[,countryQaRaw] %in% input$countries),]}
              newLevels <- na.omit(unique(reduced4[,locationQaRaw]))
              if(length(newLevels)==0){newLevels <- "all"}
            }else{newLevels <- "all"}
          }else{newLevels <- "all"}

          updateSelectInput(session, "locations",choices = newLevels, selected = newLevels)
          shinyjs::hide(ns("locations"))
        }
      }
    })

    # create the trials
    observeEvent(c(data(), input$traitFilterPheno, input$years, input$seasons, input$countries, input$locations), {
      req(data()); req(input$years); req(input$seasons);  req(input$countries);req(input$locations);  req(input$slider1)
      if("environment" %in% colnames(data()$data$pheno)){
        object <- data()
        mtdtQaRaw <- object$metadata$pheno; dtQaRaw <- object$data$pheno
        if(length(which(mtdtQaRaw$parameter %in% c("environment","designation"))) == 2){
          envCol <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="environment","value"])
          desCol <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="designation","value"])
          keepEnvs <- names(which( unlist(lapply(split(dtQaRaw, dtQaRaw[,envCol]), function(x){length(unique(x[,desCol]))})) < input$slider1))
          dtQaRaw <- dtQaRaw[which(dtQaRaw[,envCol] %in% keepEnvs),]

          yearQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="year","value"]) # column for year
          seasonQaQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="season","value"]) # column for year
          countryQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="country","value"]) # column for year
          locationQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="location","value"]) # column for year
          trialQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="trial","value"]) # column for year
          if(length(trialQaRaw) > 0){
            if(length(trialQaRaw)>0 & input$traitFilterPheno %in% colnames(dtQaRaw) ){
              reduced1 <- dtQaRaw[which(!is.na(dtQaRaw[,input$traitFilterPheno])),]
              if("all" %in%  input$years){reduced2 <- reduced1}else{reduced2 <- reduced1[which(reduced1[,yearQaRaw] %in% input$years),]}
              if("all" %in%  input$seasons){reduced3 <- reduced2}else{reduced3 <- reduced2[which(reduced2[,seasonQaQaRaw] %in% input$seasons),] }
              if("all" %in%  input$countries){reduced4 <- reduced3}else{reduced4 <- reduced3[which(reduced3[,countryQaRaw] %in% input$countries),]}
              if("all" %in%  input$locations){reduced5 <- reduced4}else{reduced5 <- reduced4[which(reduced4[,locationQaRaw] %in% input$locations),]}
              newLevels <- na.omit(unique(reduced5[,trialQaRaw]))
              if(length(newLevels)==0){newLevels <- "all"}
            }else{newLevels <- "all"}
          }else{newLevels <- "all"}

          updateSelectInput(session, "trials",choices = newLevels, selected = newLevels)
          shinyjs::hide(ns("trials"))
        }
      }
    })

    # create the environments
    observeEvent(c(data(), input$traitFilterPheno, input$years, input$seasons, input$countries, input$locations, input$trials ), {
      req(data()); req(input$years); req(input$seasons);  req(input$countries);req(input$locations);   req(input$trials); req(input$slider1)
      if("environment" %in% colnames(data()$data$pheno)){
        object <- data()
        mtdtQaRaw <- object$metadata$pheno; dtQaRaw <- object$data$pheno
        if(length(which(mtdtQaRaw$parameter %in% c("environment","designation"))) == 2 ){
          envCol <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="environment","value"])
          desCol <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="designation","value"])
          keepEnvs <- names(which( unlist(lapply(split(dtQaRaw, dtQaRaw[,envCol]), function(x){length(unique(x[,desCol]))})) < input$slider1))
          dtQaRaw <- dtQaRaw[which(dtQaRaw[,envCol] %in% keepEnvs),]

          yearQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="year","value"]) # column for year
          seasonQaQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="season","value"]) # column for year
          countryQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="country","value"]) # column for year
          locationQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="location","value"]) # column for year
          trialQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="trial","value"]) # column for year
          environmentQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="environment","value"]) # column for year
          if(length(environmentQaRaw) > 0){
            if(length(environmentQaRaw)>0 & input$traitFilterPheno %in% colnames(dtQaRaw) ){
              reduced1 <- dtQaRaw[which(!is.na(dtQaRaw[,input$traitFilterPheno])),]
              if("all" %in%  input$years){reduced2 <- reduced1}else{reduced2 <- reduced1[which(reduced1[,yearQaRaw] %in% input$years),]}
              if("all" %in%  input$seasons){reduced3 <- reduced2}else{reduced3 <- reduced2[which(reduced2[,seasonQaQaRaw] %in% input$seasons),] }
              if("all" %in%  input$countries){reduced4 <- reduced3}else{reduced4 <- reduced3[which(reduced3[,countryQaRaw] %in% input$countries),]}
              if("all" %in%  input$locations){reduced5 <- reduced4}else{reduced5 <- reduced4[which(reduced4[,locationQaRaw] %in% input$locations),]}
              if("all" %in%  input$trials){reduced6 <- reduced5}else{reduced6 <- reduced5[which(reduced5[,trialQaRaw] %in% input$trials),]}
              newLevels <- na.omit(unique(reduced6[,environmentQaRaw]))
              if(length(newLevels)==0){newLevels <- "all"}
            }else{newLevels <- "all"}
          }else{newLevels <- "all"}

          updateSelectInput(session, "environments",choices = newLevels, selected = newLevels)
          shinyjs::hide(ns("environments"))
        }
      }
    })

    ## function to calculate outliers
    newOutliers <- reactive({ #
      req(data());  req(input$traitFilterPheno); req(input$years);  req(input$seasons); req(input$countries); req(input$locations); req(input$trials); req(input$environments); req(input$slider1)
      result <- data()
      mydata <- data()$data$pheno

      if("environment" %in% colnames(data()$data$pheno)){
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
        keepEnvs <- names(which( unlist(lapply(split(mydata, mydata[,"environment"]), function(x){length(unique(x[,"designation"]))})) < input$slider1))
        keep7 <- which(mydata[,"environment"] %in% keepEnvs)
        # records to keep
        keep <- Reduce(intersect, list(keep1,keep2,keep3,keep4,keep5,keep6, keep7) )#which( (mydata[,"year"] %in% input$years) & (mydata[,"season"] %in% input$seasons) & (mydata[,"country"] %in% input$countries) & (mydata[,"location"] %in% input$locations) & (mydata[,"trial"] %in% input$trials) & (mydata[,"environment"] %in% input$environments) )
        toSilence <- setdiff(mydata$rowindex, keep)

        if(length(toSilence) > 0 & input$traitFilterPheno %in% colnames(mydata) ){
          myoutliersReduced <- data.frame(module="qaFilter",analysisId=NA,trait=input$traitFilterPheno,reason="outlierIQR",row=toSilence, value=mydata[toSilence,input$traitFilterPheno]);
        }else{
          myoutliersReduced <- data.frame(matrix(nrow=0, ncol=6))
          colnames(myoutliersReduced) <- c("module" ,"analysisId" ,"trait","reason","row" , "value" )
        }
        return(myoutliersReduced)
      }
    })

    report <- reactiveVal(NULL)

    observeEvent(input$renderReportQaPheno,{
      shinybusy::show_modal_spinner(spin = "fading-circle", text = "Generating Report...")

      result <- data()

      src <- normalizePath(system.file("rmd","reportQaPheno.Rmd",package="bioflow"))
      src2 <- normalizePath('data/resultQaPheno.RData')

      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))

      file.copy(src, 'report.Rmd', overwrite = TRUE)
      file.copy(src2, 'resultQaPheno.RData', overwrite = TRUE)

      outReport <- rmarkdown::render('report.Rmd', params = list(toDownload=TRUE ),
                                     switch("HTML", HTML = rmdformats::robobook(toc_depth = 4)
                                            # HTML = rmarkdown::html_document()
                                     ))

      report(outReport)

      shinybusy::remove_modal_spinner()

      shinyjs::click("downloadReportQaPheno")
    })

    # returns the ones to exclude
    outFilterRaw <- eventReactive(input$runFilterRaw, {
      req(data());  req(input$traitFilterPheno); req(input$years);  req(input$seasons); req(input$countries); req(input$locations); req(input$trials); req(input$environments)
      req(input$multiTraitFilter)
      result <- data()
      mydata <- result$data$pheno
      mydata$rowindex <- 1:nrow(mydata)

      shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
      if(input$multiTraitFilter){ # if user wants multiple traits at once
        outliers <- list()
        if(length(input$traitFilterPhenoMultiple) > 0){
          prov <- newOutliers()
          for(iTrait in input$traitFilterPhenoMultiple){
            if(nrow(prov) > 0){prov$trait <- iTrait}
            prov$value <- mydata[which(mydata$rowindex %in% prov$row),iTrait]
            outliers[[iTrait]] <- prov
          }
          outliers <- do.call(rbind, outliers)
        }else{
          outliers <- data.frame(matrix(nrow=0, ncol=6))
          colnames(outliers) <- c("module" ,"analysisId" ,"trait","reason","row" , "value" )
          cat("No traits selected to filter.")
          output$outFilterRaw2 <- renderPrint({
            cat("No traits selected to filter.")
          })
        }
      }else{ # if user only wants to filter one trait
        outliers <- newOutliers()
      }
      myoutliers <- result$modifications$pheno
      analysisId <- as.numeric(Sys.time())
      if(nrow(outliers) == 0){ # no outliers found
        cat("No data to filter.")
        output$outFilterRaw2 <- renderPrint({
          cat("No data to filter.")
        })
      }else{ # we found outliers
        # outliers$analysisId <- analysisId
        outliers[,"analysisId"] <- analysisId
        outliers[,"module"] <- "qaFilter"
        cat(paste("Filtering step with id:",as.POSIXct(analysisId, origin="1970-01-01", tz="GMT"),"for trait",ifelse(input$multiTraitFilter, paste(input$traitFilterPhenoMultiple, collapse = ", "),input$traitFilterPheno),"saved."))
        output$outFilterRaw2 <- renderPrint({
          cat(paste("Filtering step with id:",as.POSIXct(analysisId, origin="1970-01-01", tz="GMT"),"for trait",ifelse(input$multiTraitFilter, paste(input$traitFilterPhenoMultiple, collapse = ", "),input$traitFilterPheno),"saved."))
        })
      }
      myoutliersReduced <- unique(rbind(myoutliers, outliers))

      # add status table
      newStatus <- data.frame(module = "qaFilter", analysisId = analysisId, analysisIdName = input$analysisIdName)
      if (!is.null(result$status)) {
        result$status <- rbind(result$status, newStatus[, colnames(result$status)])
      } else {
        result$status <- newStatus
      }
      #
      result$modifications$pheno <- myoutliersReduced
      if("analysisIdName" %in% colnames(result$status)){result$status$analysisIdName[nrow(result$status)] <- input$analysisIdName}
      data(result)
      updateTabsetPanel(session, "tabsMain", selected = "outputTabs")
      shinybusy::remove_modal_spinner()
      if(!inherits(result,"try-error")) { # if all goes well in the run
        # ## Report tab
        output$reportQaPheno <- renderUI({
          HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportQaPheno.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
        })

        output$downloadReportQaPheno <- downloadHandler(
          filename = function() {
            paste(paste0('qaFilter_dashboard_',gsub("-", "", as.integer(Sys.time()))), sep = '.', switch(
              "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
            ))
          },
          content = function(file) {

            out <- report()

            file.rename(out, file)
          }
        )

      }else{ hideAll$clearAll <- TRUE}

      hideAll$clearAll <- FALSE
    })
    output$outFilterRaw <- renderPrint({
      outFilterRaw()
    })
    # ## render the expected result
    output$plotFilterOut <- shiny::renderPlot({
      req(data());  req(input$traitFilterPheno); req(input$years);  req(input$seasons); req(input$countries); req(input$locations); req(input$trials); req(input$environments)
      dtQaRaw <- data()$data$pheno
      outs <- newOutliers()
      ## we need to remove the rows from the outliers
      if(nrow(outs) > 0){
        keep <- setdiff(1:nrow(dtQaRaw), outs$row)
        dtQaRaw <- dtQaRaw[keep,]
      }
      ##
      mtdtQaRaw <- data()$metadata$pheno;

      envCol <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="environment","value"])
      desCol <- unique(mtdtQaRaw[mtdtQaRaw$parameter=="designation","value"])
      keepEnvs <- names(which( unlist(lapply(split(dtQaRaw, dtQaRaw[,envCol]), function(x){length(unique(x[,desCol]))})) < input$slider1))
      dtQaRaw <- dtQaRaw[which(dtQaRaw[,envCol] %in% keepEnvs),]

      traitsQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter%in%c("trial", "year","season","country","location","environment"),"value"]) # column for year
      envsQaRaw <- unique(mtdtQaRaw[mtdtQaRaw$parameter%in%c("environment"),"value"]) # column for year
      xx <- unique(dtQaRaw[,traitsQaRaw,drop=FALSE])
      X <- list()
      for(kEffect in traitsQaRaw){
        if(length(table(xx[,kEffect])) > 1){
          X[[kEffect]] <- model.matrix(as.formula(paste("~", kEffect,"-1")),data = xx)
        }
      }
      X <- do.call(cbind, X); rownames(X) <- xx[,envsQaRaw]
      hc <- hclust(dist(X), "ave")
      dend1 <- as.dendrogram(hc) # "print()" method
      plot(dend1, main="Network plot showing the dependencies between years, seasons, locations, trials, etc.")
    })

    ## display the current outliers
    output$modificationsFilter <-  DT::renderDT({
      DT::datatable(newOutliers(), extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All'))),
                    caption = htmltools::tags$caption(
                      style = 'color:cadetblue', #caption-side: bottom; text-align: center;
                      htmltools::em('Records identified to be tagged and ignored.')
                    )
      )
    }, server = FALSE)

    ## save when user clicks

  })
}

## To be copied in the UI
# mod_filterPhenoApp_ui("filterPhenoApp_1")

## To be copied in the server
# mod_filterPhenoApp_server("filterPhenoApp_1")

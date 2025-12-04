#' CoreSubSetApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

#' @importFrom shiny NS tagList

mod_CoreSubsetApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::mainPanel(width = 12,
                     tabsetPanel(id=ns("tabsMain"),
                                 type = "tabs",
                                 tabPanel(div(icon("book"), "Information") ,
                                          br(),
                                          # shinydashboard::box(status="success",width = 12, solidHeader = TRUE,
                                          column(width=6,   #style = "height:800px; overflow-y: scroll;overflow-x: scroll;",
                                                 tags$body(
                                                   h1(strong(span("Pop Subset", style="color:darkcyan"))),
                                                   h2(strong("Data Status (wait to be displayed):")),
                                                   uiOutput(ns("warningMessage")),
                                                   tags$br(),
                                                   shinyWidgets::prettySwitch( inputId = ns('launch'), label = "Load example dataset", status = "success"),
                                                   tags$br(),
                                                   img(src = "www/popstr.png", height = 135, width = 540), # add an image
                                                 )
                                          ),
                                          column(width=6,
                                                 h2(strong("Details")),
                                                 p("Genebanks were established by national or international breeding, or conservation programs with the goal to safeguard genetic diversity for future use.
                                                   Many breeding programs have established genebanks as a resource for new variation in the crops they breed, allowing them to react to changing environments
                                                   and emerging biotic and abiotic stresses. Accessions are often divided between active (or working) and base collections. Examples of active collections
                                                   include seed stores or live plants that can be accessed quickly by plant breeders and researchers through germination or clonal propagation. In contrast,
                                                   accessions in base collections are held in long-term storage, such as cryopreservation, and require some time for regeneration and propagation before being made available."),

                                                p("During the last few decades the collections stored in genebanks have grown enormously, and cost of maintaining viable germplasm within genebanks has increased.
                                                   Genebank curators must make decisions about which accessions to maintain in the active collection versus the base collection, and may even consider not
                                                   maintaining an accession at all. The concept of a core collection was introduced to help with these decisions, and is defined as subset of the complete
                                                   collection which most represents the diversity of the entire collection with minimum redundancy. Genebank curators can use core collections to define
                                                   the active collection over the base collection. Core collections can also be used to aid researchers and plant breeders in the choice of starting material.
                                                   For example, the potential for use of core collections has been shown for association studies. "),

                                                 p(strong("Size of Core Subset.- "),"You must type numbers between [0,1] for indicate the percent of size for the new subset. If larger than one the value is used as the absolute core size after rounding."),
                                                 p(strong("Optimization by.- "),"You must select the option for maximizes or minimizes the diversity parameter for created the subset."),
                                                 h2(strong("References")),
                                                 p("H. De Beukelaer and V. Fack (2023). corehunter: Multi-Purpose Core Subset Selection.")
                                          ),
                                          # )
                                 ),

                                 tabPanel(div(icon("arrow-right-to-bracket"), "Input steps"),
                                          tabsetPanel(
                                            tabPanel(div( icon("dice-one"), "Pick QA-stamp", icon("arrow-right") ) , # icon = icon("dice-one"),
                                                     br(),
                                                     column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                            column(width=8, selectInput(ns("version2CoreSet"), choices = NULL, multiple = FALSE,  label = tags$span(
                                                              "QA-geno stamp to apply (optional)",
                                                              tags$i(
                                                                class = "glyphicon glyphicon-info-sign",
                                                                style = "color:#FFFFFF",
                                                                title = "You can select a QC object"
                                                              )
                                                            )
                                                            )),

                                                     ),
                                                     column(width=12),
                                                     shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                         column(width=12,
                                                                                hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                                h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameters to be specified in the grey boxes above.", style="color:green"))),
                                                                                hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                         ),
                                                                         column( width=8, shiny::plotOutput(ns("plotTimeStamps")), br(),br(), ),

                                                     ),
                                            ),
                                            tabPanel(div(icon("dice-two"), "Additional settings", icon("arrow-right") ), # icon = icon("dice-two"),
                                                     br(),
                                                     column(width=6, style = "background-color:#d6d4d4; color: black",

                                                            textInput(ns('sizeset'),label = tags$span("Size of new subset",
                                                                                                     tags$i(class = "glyphicon glyphicon-info-sign",style = "color:#000000",
                                                                                                            title = "Numbers between [0,1] for indicate the percent of size for the new subset."
                                                                                                     )
                                                            ),value='0.2'),

                                                            radioButtons(ns("optset"), "Optimization by:",choices = c("Maximizes the average distances between each selected genotype and the closest other selected genotype in the core." = "EN",
                                                                                                                      "Minimizes the mean distance between each genotype from the entire collection and the most similar core genotype, including itself in case the genotype has been selected." = "AN",
                                                                                                                      "Maximizes the average distances between each pair of selected genotypes in the core." = "EE",
                                                                                                                      "Maximizes the entropy, as used in information theory, of the selected core." = "SH",
                                                                                                                      "Maximizes the expected proportion of heterozygous loci in offspring produced from random crossing within the selected core." = "HE",
                                                                                                                      "Maximizes the proportion of alleles observed in the full dataset that ara retained in the selected core." = "CV")),
                                                            radioTooltip(id = ns("optset"), choice = "EN", title = "This criterion can be maximized to construct highly diverse cores in which all accessions are maximally different.", placement = "right", trigger = "hover"),
                                                            radioTooltip(id = ns("optset"), choice = "AN", title = "Minimizing this criterion yields cores that maximally represent all individual genotypes.", placement = "right", trigger = "hover"),
                                                            #radioTooltip(id = ns("optset"), choice = "EE", title = "Maximizes the average distances between each pair of selected individuals in the core.", placement = "right", trigger = "hover"),
                                                            #radioTooltip(id = ns("optset"), choice = "SH", title = "Maximizes the entropy, as used in information theory, of the selected core.", placement = "right", trigger = "hover"),
                                                            #radioTooltip(id = ns("optset"), choice = "HE", title = "Maximizes the expected proportion of heterozygous loci in offspring produced from random crossing within the selected core.", placement = "right", trigger = "hover"),
                                                            #radioTooltip(id = ns("optset"), choice = "CV", title = "Maximizes the proportion of alleles observed in the full dataset that ara retained in the selected core.", placement = "right", trigger = "hover"),

                                                     )
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
                                                                   actionButton(ns("runCoreSet"), "Run analysis", icon = icon("play-circle")),
                                                                   #uiOutput(ns("qaQcPopStrInfo")),
                                                                   br(),
                                                            ),

                                                     ),
                                                     textOutput(ns("outCoreSet")),
                                            )
                                          )),

                                 tabPanel(div(icon("arrow-right-from-bracket"), "Output tabs" ) , value = "outputTabs",
                                          tabsetPanel(
                                            tabPanel("Dashboard", icon = icon("file-image"),
                                                     br(),
                                                     textOutput(ns("outCoreSet2")),
                                                     br(),
                                                     downloadButton(ns("downloadReportCoreSet"), "Download dashboard"),
                                                     br(),
                                                     uiOutput(ns('reportCoreSet'))
                                            ),
                                            #termina report
                                            tabPanel("MDS", icon = icon("magnifying-glass-chart"),
                                                     br(),
                                                     shinydashboardPlus::box(title = "2D-Plot",closable = FALSE, width=12, solidHeader = TRUE, collapsible = TRUE,status="success",
                                                                             sidebar = shinydashboardPlus::boxSidebar(id="mdsplot2d",width = 25,
                                                                                                                      selectInput(ns('xcol'), 'X Variable',choices =c("Factor1","Factor2","Factor3"), selected="Factor1"),
                                                                                                                      selectInput(ns('ycol'), 'Y Variable',choices =c("Factor1","Factor2","Factor3"), selected="Factor2"),
                                                                                                                      tags$hr(),
                                                                                                                      #Creacion de color para cambiar el color de los puntos del grafico de acuerdo a una lista de colores generada
                                                                                                                      selectInput(ns('color'),'Choose a color',choices = '',selected="",multiple=T),
                                                                                                                      tags$hr(),
                                                                                                                      textInput(ns('tp'),'Plot Title',value="MDS Plot"),
                                                                                                                      selectInput(ns('pnc'),'Title Color',choices=colors()[-c(1:25,27:30,37:46,48,57:67,69,80,82,83,85:89,92:93,97:98,101:106,108:113,123:127,
                                                                                                                                                                              130:131,138,140:141,152:253,255:256,260:366,377:392,394:447,449,451,463:464,468:469,
                                                                                                                                                                              478:489,492,499:500,504:505,509:511,513:534,536:546,548:550,553:554,557:561,563:564,
                                                                                                                                                                              569:570,579:583,585:586,589:609,611:612,617,620:629,631:632,636:637,642:644,646:651)],
                                                                                                                                  selected = 'blue'),
                                                                                                                      sliderInput(ns('ts'),'Title Size',min=5,max=20,step = 1,value = 12),
                                                                                                                      tags$hr(),
                                                                                                                      selectInput(ns('ac'),'Axes color',choices=colors()[-c(1:25,27:30,37:46,48,57:67,69,80,82,83,85:89,92:93,97:98,101:106,108:113,123:127,
                                                                                                                                                                            130:131,138,140:141,152:253,255:256,260:366,377:392,394:447,449,451,463:464,468:469,
                                                                                                                                                                            478:489,492,499:500,504:505,509:511,513:534,536:546,548:550,553:554,557:561,563:564,
                                                                                                                                                                            569:570,579:583,585:586,589:609,611:612,617,620:629,631:632,636:637,642:644,646:651)],
                                                                                                                                  selected = 'red'),
                                                                                                                      sliderInput(ns('szl'),'Axes Label Size',min=5,max=20,step=1,value=12),
                                                                                                                      tags$hr(),
                                                                                                                      #Creacion de bkgp para cambiar el color de fondo en el grafico
                                                                                                                      selectInput(ns('bkgp'),'Plot Background color',
                                                                                                                                  choices=c("white","antique","azure","beige",
                                                                                                                                            "black","blanchedalmond","burlywood",
                                                                                                                                            "cornsilk","darkgray","dimgray","floralwhite",
                                                                                                                                            "gainsboro","ghostwhite","gray","honeydew",
                                                                                                                                            "ivory","lavender","lavenderblush","lemonchiffon",
                                                                                                                                            "lightcyan","lightgoldenrodyellow",
                                                                                                                                            "lightgray","lightslategray","lightsteelblue","linen","mintcream","mistyrose",
                                                                                                                                            "moccasin","oldlace","palegoldenrod","papayawhip","peachpuff","seashell","snow",
                                                                                                                                            "tan","thistle","whitesmoke"),selected=NULL),
                                                                                                                      tags$hr(),
                                                                                                                      # selectInput(ns('eti'),'Label points',choices=''),
                                                                                                                      sliderInput(ns('size'),'Points size',min=5,max=25,value=7)
                                                                             ),
                                                                             #grafico 2d
                                                                             div(plotly::plotlyOutput(ns("try"),height = "750px",width = "950px"),align="center")
                                                     ),


                                            )

                                          )
                                 )# end of output panel
                     )) # end mainpanel


  )
}


#' PopStrApp Server Functions
#'
#' @noRd
mod_CoreSubsetApp_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })
    ############################################################################
    # warning message
    output$warningMessage <- renderUI({
      if(is.null(data())){
        HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your data using the 'Data Retrieval' tab." )) )
      }else{
        if(!is.null(data()$data$geno)){
          HTML( as.character(div(style="color:green ; font-size: 20px;", "Data is complete, you could use your raw data or go to do a QC for control missing data and imputation, decide and please proceed to perform the other tabs." )) )
        }else{
          HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your genotype data using the 'Data Retrieval' tab. ")) )
        }
      }
    })

    #################
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
    #################
    ## render timestamps flow plot
    output$plotTimeStamps <- shiny::renderPlot({
      req(data()) # req(input$version2Sta)
      xx <- data()$status;  yy <- data()$modeling # xx <- result$status;  yy <- result$modeling
      if("analysisIdName" %in% colnames(xx)){existNames=TRUE}else{existNames=FALSE}
      if(existNames){
        networkNames <- paste(xx$analysisIdName, as.character(as.POSIXct(as.numeric(xx$analysisId), origin="1970-01-01", tz="GMT")),sep = "_" )
        xx$analysisIdName <- as.character(as.POSIXct(as.numeric(xx$analysisId), origin="1970-01-01", tz="GMT"))
      }
      v <- which(yy$parameter == "analysisId")
      if(length(v) > 0){
        yy <- yy[v,c("analysisId","value")]
        zz <- merge(xx,yy, by="analysisId", all.x = TRUE)
      }else{ zz <- xx; zz$value <- NA}
      if(existNames){
        zz$analysisIdName <- cgiarBase::replaceValues(Source = zz$analysisIdName, Search = "", Replace = "?")
        zz$analysisIdName2 <- cgiarBase::replaceValues(Source = zz$value, Search = zz$analysisId, Replace = zz$analysisIdName)
      }
      if(!is.null(xx)){
        if(existNames){
          colnames(zz) <- cgiarBase::replaceValues(colnames(zz), Search = c("analysisIdName","analysisIdName2"), Replace = c("outputId","inputId") )
        }else{
          colnames(zz) <- cgiarBase::replaceValues(colnames(zz), Search = c("analysisId","value"), Replace = c("outputId","inputId") )
        }
        nLevelsCheck1 <- length(na.omit(unique(zz$outputId)))
        nLevelsCheck2 <- length(na.omit(unique(zz$inputId)))
        if(nLevelsCheck1 > 1 & nLevelsCheck2 > 1){
          X <- with(zz, enhancer::overlay(outputId, inputId))
        }else{
          if(nLevelsCheck1 <= 1){
            X1 <- matrix(ifelse(is.na(zz$inputId),0,1),nrow=length(zz$inputId),1); colnames(X1) <- as.character(na.omit(unique(c(zz$outputId))))
          }else{X1 <- model.matrix(~as.factor(outputId)-1, data=zz); colnames(X1) <- levels(as.factor(zz$outputId))}
          if(nLevelsCheck2 <= 1){
            X2 <- matrix(ifelse(is.na(zz$inputId),0,1),nrow=length(zz$inputId),1); colnames(X2) <- as.character(na.omit(unique(c(zz$inputId))))
          }else{X2 <- model.matrix(~as.factor(inputId)-1, data=zz); colnames(X2) <- levels(as.factor(zz$inputId))}
          mynames <- unique(na.omit(c(zz$outputId,zz$inputId)))
          X <- matrix(0, nrow=nrow(zz), ncol=length(mynames)); colnames(X) <- as.character(mynames)
          if(!is.null(X1)){X[,colnames(X1)] <- X1}
          if(!is.null(X2)){X[,colnames(X2)] <- X2}
        };
        rownames(X) <- networkNames
        colnames(X) <- networkNames
        if(existNames){

        }else{
          rownames(X) <-as.character(as.POSIXct(as.numeric(rownames(X)), origin="1970-01-01", tz="GMT"))
          colnames(X) <-as.character(as.POSIXct(as.numeric(colnames(X)), origin="1970-01-01", tz="GMT"))
        }
        # make the network plot
        n <- network::network(X, directed = FALSE)
        network::set.vertex.attribute(n,"family",zz$module)
        network::set.vertex.attribute(n,"importance",1)
        e <- network::network.edgecount(n)
        network::set.edge.attribute(n, "type", sample(letters[26], e, replace = TRUE))
        network::set.edge.attribute(n, "day", sample(1, e, replace = TRUE))
        library(ggnetwork)
        ggplot2::ggplot(n, ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
          ggnetwork::geom_edges(ggplot2::aes(color = family), arrow = ggplot2::arrow(length = ggnetwork::unit(6, "pt"), type = "closed") ) +
          ggnetwork::geom_nodes(ggplot2::aes(color = family), alpha = 0.5, size=5 ) +
          ggnetwork::geom_nodelabel_repel(ggplot2::aes(color = family, label = vertex.names ),
                                          fontface = "bold", box.padding = ggnetwork::unit(1, "lines")) +
          ggnetwork::theme_blank() + ggplot2::ggtitle("Network plot of current analyses available")
      }
    })
    # ## version qa marker
    observeEvent(c(data()), {
      req(data())
      dtMta <- data()
      dtMta <- dtMta$status
      dtMta <- dtMta[which(dtMta$module == "qaGeno"),]
      traitsMta <- unique(dtMta$analysisId)
      if(length(traitsMta) > 0){
        if("analysisIdName" %in% colnames(dtMta)){
          names(traitsMta) <- paste(dtMta$analysisIdName, as.POSIXct(traitsMta, origin="1970-01-01", tz="GMT"), sep = "_")
        }else{
          names(traitsMta) <- as.POSIXct(traitsMta, origin="1970-01-01", tz="GMT")
        }
      }
      updateSelectInput(session, "version2CoreSet", choices = c(0,traitsMta))
    })

    ###################################
    ###################################
    ###################################
    ###################################
    ###################################
    ###################################
    ###################################
    ###################################
    ## render result of "run" button click
    outCoreSet1 <- eventReactive(input$runCoreSet, {
      catv <-"CoreSet"
      req(data())
      result <- data()
      if(input$version2CoreSet==0){
        mydata<-as.data.frame(result$data$geno)
      }else{
        qas<-which( names(result$data$geno_imp)==input$version2CoreSet )
        mydata<-as.data.frame(result$data$geno_imp[qas])
      }


      paropt=input$optset
      if(paropt%in%c("EN","AN","EE")){
        dir_fileGen="none"
        dir_filePhen="none"
        dir_fileDist<- "used"
      }else{
        dir_fileGen="used"
        dir_filePhen="none"
        dir_fileDist="none"
      }
      size1=input$sizeset
      w=rep(0,15)
      w[which(c("ENMR","ENCE","ENGD","EN","ANMR","ANCE","ANGD","AN","EEMR","EECE","EEGD","EE","SH","HE","CV")==paropt)]=1

      #save(mydata,dir_fileGen,dir_filePhen,dir_fileDist,size1,w,file="core.RData")
      shinybusy::show_modal_spinner('fading-circle', text = 'Optimizing...')

      #source("C:\\Users\\RAPACHECO\\Downloads\\functCORE.R")
      unocheck <- try(
        uno<-cgiarPipeline::funchunt(mydata,dir_fileGen,dir_filePhen,dir_fileDist,size1,w),
        #uno<-funchunt(mydata,dir_fileGen,dir_filePhen,dir_fileDist,size1,w),
        silent=TRUE
      )

      # add status table
      analysisId <- as.numeric(Sys.time())
      if(input$analysisIdName!=""){
        newStatus <- data.frame(module="CoreSetM", analysisId=analysisId, analysisIdName=input$analysisIdName)
      }else{
        newStatus <- data.frame(module="CoreSetM", analysisId=analysisId, analysisIdName="myCoreSetOne")
      }
      result$status <- rbind(result$status, newStatus)

      tmpMDS<-uno[[1]]
      newPredMDS=list()
      for ( f in 2:4){
        newPredMDS[[f]] <- data.frame(module="CoreSetM",analysisId=analysisId, pipeline="MDS", trait=names(tmpMDS)[f], gid=NA, designation=tmpMDS$Gen, mother=NA, father=NA, entryType=tmpMDS[,5], environment=NA, predictedValue=tmpMDS[,f], stdError=NA, reliability=NA, effectType=NA)
        newPredMDS[[f]]$predictedValue<-as.numeric(newPredMDS[[f]]$predictedValue)
      }
      newPredMDS=do.call(rbind,newPredMDS)

      newmetricsSum=list()
      for (f in 1:14){
        newmetricsSum[[f]] <- data.frame(module="CoreSetM",analysisId=analysisId, trait=NA, environment=uno[[2]][f,2], parameter=uno[[2]][f,1], method="SummaryDiversityAnalysis", value= uno[[2]][f,3], stdError=NA)
        newmetricsSum[[f]]$value<-as.numeric(newmetricsSum[[f]]$value)
      }
      newmetricsSum=do.call(rbind,newmetricsSum)

      newmetricsPerc=list()
      for (f in 1:3){
        newmetricsPerc[[f]] <- data.frame(module="CoreSetM",analysisId=analysisId, trait=NA, environment=NA, parameter=paste0("Factor",f), method="VarExplained", value= uno[[3]][f], stdError=NA)
        newmetricsPerc[[f]]$value<-as.numeric(newmetricsPerc[[f]]$value)
      }
      newmetricsPerc=do.call(rbind,newmetricsPerc)

      result$predictions<-rbind(result$predictions,newPredMDS)
      result$metrics <- rbind(result$metrics, newmetricsSum, newmetricsPerc)

      if(!inherits(unocheck,"try-error")) {
        if("analysisIdName" %in% colnames(result$status)){result$status$analysisIdName[nrow(result$status)] <- input$analysisIdName}
        data(result) # update data with results
        cat(paste("Core Subset analysis with id:",as.POSIXct(result$status$analysisId[length(result$status$analysisId)], origin="1970-01-01", tz="GMT"),"was saved."))
        output$outCoreSet2 <- renderPrint({
          cat(paste("Core Subset analysis with id:",as.POSIXct(result$status$analysisId[length(result$status$analysisId)], origin="1970-01-01", tz="GMT"),"was saved."))
        })
        updateTabsetPanel(session, "tabsMain", selected = "outputTabs")
      }else{
        cat(paste("Analysis failed with the following error message: \n\n",uno[[1]]))
        output$outCoreSet2 <- renderPrint({
          cat(paste("Analysis failed with the following error message: \n\n",uno[[1]]))
        })
      }
      rm(newmetricsSum,newPredMDS)
      shinybusy::remove_modal_spinner()

      if(!inherits(unocheck,"try-error")) {
        idMta <- result$status[which(result$status$module == "CoreSetM"),"analysisId"]
        idMta <- idMta[length(idMta)]
        uno<-as.data.frame(result$predictions[which(result$predictions$module=="CoreSetM" & result$predictions$analysisId==idMta & result$predictions$pipeline=="MDS" ),])
        uno$predictedValue=as.numeric(uno$predictedValue)
        dos=subset(uno,uno$trait=="Factor1")
        uno<-data.frame(reshape::cast(uno,designation~trait,value= "predictedValue", fun.aggregate = mean))
        uno<-cbind(uno,dos$entryType)
        names(uno)[5]="CoreSet"
        #if(length(names(uno))>5) {catv<-names(uno)[6]}

        txlab <- paste0('Factor 1 (',result$metrics[which(result$metrics$module=="CoreSetM" & result$metrics$analysisId==idMta & result$metrics$parameter=="Factor1" & result$metrics$method=="VarExplained"),7],'%)')
        tylab <- paste0('Factor 2 (',result$metrics[which(result$metrics$module=="CoreSetM" & result$metrics$analysisId==idMta & result$metrics$parameter=="Factor2" & result$metrics$method=="VarExplained"),7],'%)')
        tzlab <- paste0('Factor 3 (',result$metrics[which(result$metrics$module=="CoreSetM" & result$metrics$analysisId==idMta & result$metrics$parameter=="Factor3" & result$metrics$method=="VarExplained"),7],'%)')
        eti <- "Gen"
        #Actualiza la variable catv (grupos) en conjunto con la seleccion de colores
        set.seed(7)
        colores=colors()[-c(1,3:12,13:25,24,37:46,57:67,80,82,83,85:89,101:106,108:113,126:127,138,140:141,152:253,260:366,377:392,
                            394:447,449,478:489,492,513:534,536:546,557:561,579:583,589:609,620:629,418,436,646:651)]
        d=sample(colores,100)
        #var=as.factor(uno[[5]][,catv])
        grupos=nlevels(as.factor(uno$CoreSet))
        updateSelectInput(session,'color','Choose a color',choices=d,selected=d[1:grupos])
        updateSelectInput(session,'colordend','Choose a color', choices=d,selected=d[1:grupos])

        result$metrics=rbind(result$metrics,data.frame(module="CoreSetM",analysisId=idMta, trait=NA, environment=NA, parameter="ColorsDend", method=NA, value= d[1:grupos], stdError=NA))

        #Ver datos en tabla dinamica Summary Diversity
        output$seeDataDiver<-DT::renderDT({
          idMta <- result$status[which(result$status$module == "CoreSetM"),"analysisId"]
          idMta <- idMta[length(idMta)]
          seedatosum<-as.data.frame(result$metrics[which(result$metrics$module=="CoreSetM" & result$metrics$analysisId==idMta & result$metrics$method=="SummaryDiversityAnalysis"),c(4,5,7)])
          colnames(seedatosum)=c("Population","Parameter","Value")
          seedatosum[,2]=round(as.numeric(seedatosum[,2]),3)
          DT::datatable(seedatosum, extensions = 'Buttons',
                        options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                       lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
          )
        })

        #Ver datos en tabla dinamica MDS
        output$seeDataMDS<-DT::renderDT({
          idMta <- result$status[which(result$status$module == "CoreSetM"),"analysisId"]
          idMta <- idMta[length(idMta)]
          seedatosMDS<-as.data.frame(result$predictions[which(result$predictions$module=="CoreSetM" & result$predictions$analysisId==idMta & result$predictions$pipeline=="MDS" & result$predictions$trait!=catv),])
          grupos=as.factor(result$predictions[which(result$predictions$module=="CoreSetM" & result$predictions$analysisId==idMta & result$predictions$trait==catv),11])
          seedatosMDS$predictedValue=as.numeric(seedatosMDS$predictedValue)
          seedatosMDS<-as.data.frame(reshape::cast(seedatosMDS,designation~trait,value = "predictedValue", fun.aggregate = mean))
          seedatosMDS=cbind(seedatosMDS,grupos)
          names(seedatosMDS)[c(1,5)]=c("Gen",catv)
          seedatosMDS[,2:4]=apply(seedatosMDS[,2:4],2,function(x){round(as.numeric(x),4)})
          DT::datatable(seedatosMDS, extensions = 'Buttons',
                        options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                       lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
          )
        })
        #grafico 2d
        output$try=plotly::renderPlotly({
          txlab2=txlab
          if(input$xcol=="Factor2") txlab2=tylab
          if(input$xcol=="Factor3") txlab2=tzlab
          tylab2=txlab
          if(input$ycol=="Factor2") tylab2=tylab
          if(input$ycol=="Factor3") tylab2=tzlab

          if(!is.null(result$data$geno)){
            idMta <- result$status[which(result$status$module == "CoreSetM"),"analysisId"]
            idMta <- idMta[length(idMta)]
            catv <-"CoreSet"
            eti="designation"
            seedatosMDS<-as.data.frame(result$predictions[which(result$predictions$module=="CoreSetM" & result$predictions$analysisId==idMta & result$predictions$pipeline=="MDS" ),])
            seedatosMDS$predictedValue=as.numeric(seedatosMDS$predictedValue)
            dos=subset(seedatosMDS,seedatosMDS$trait=="Factor1")
            seedatosMDS<-data.frame(reshape::cast(seedatosMDS,designation~trait,value= "predictedValue", fun.aggregate = mean))
            seedatosMDS<-cbind(seedatosMDS,dos$entryType)
            names(seedatosMDS)[5]="CoreSet"
            grupos=nlevels(as.factor(seedatosMDS$CoreSet))
            p=plotly::plot_ly(data=seedatosMDS,x=seedatosMDS[,input$xcol],y=seedatosMDS[,input$ycol],color=seedatosMDS[,catv],
                              type="scatter",mode="markers",colors = input$color,xaxis=F, yaxis=F,
                              text=seedatosMDS[,eti],marker=list(size=input$size))
            #color de fondo del grafico
            p= p %>% plotly::layout(plot_bgcolor=input$bkgp)
            #titulo y etiquetas ejes
            p= p %>% plotly::layout(title=input$tp,titlefont=list(size=input$ts,color=input$pnc), xaxis = list(title = txlab2, titlefont=list(size=input$szl,color=input$ac)),
                                    yaxis = list(title = tylab2,titlefont=list(size=input$szl,color=input$ac)))
            p
          }else{
            fig = plotly::plot_ly()
            fig = fig %>% plotly::add_annotations(text = "Information not available.", x = 1, y = 1)#
            fig
          }

        })

        #For report
        output$reportCoreSet <- renderUI({
          HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportCoreSet.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
        })
        ## Report tab
        output$downloadReportCoreSet <- downloadHandler(
          filename = function() {
            paste(paste0('popStrM_dashboard_',gsub("-", "", as.integer(Sys.time()))), sep = '.', switch(
              "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
            ))
          },
          content = function(file) {
            shinybusy::show_modal_spinner(spin = "fading-circle", text = "Generating Report...")

            src <- normalizePath(system.file("rmd","reportCoreSet.Rmd",package="bioflow"))
            src2 <- normalizePath('./resultCoreSet.RData')

            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))

            file.copy(src, 'report.Rmd', overwrite = TRUE)
            file.copy(src2, 'resultCoreSet.RData', overwrite = TRUE)

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
      }else{
          output$seeDataDiver <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
          output$seeDataMDS <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
          hideAll$clearAll <- TRUE
        }### enf of if(!inherits(result,"try-error"))

        hideAll$clearAll <- FALSE
    })

        output$outCoreSet <- renderPrint({
          outCoreSet1()
        })


  })#end modeleServer
}#end server

## To be copied in the UI
# mod_CoreSubsetApp_ui("CoreSubsetApp_1")

## To be copied in the server
# mod_CoreSubsetApp_server("CoreSubsetApp_1")


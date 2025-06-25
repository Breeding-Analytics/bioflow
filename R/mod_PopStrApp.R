geno_groupPopStr<- 'www/example/Groupgeno.csv'
#' PopStrApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

#' @importFrom shiny NS tagList

mod_PopStrApp_ui <- function(id){
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
                                         h1(strong(span("Population structure", style="color:darkcyan"))),
                                         h2(strong("Data Status (wait to be displayed):")),
                                         uiOutput(ns("warningMessage")),
                                         tags$br(),
                                         img(src = "www/popstr.png", height = 135, width = 540), # add an image
                                       )
                                ),
                                column(width=6,
                                       h2(strong("Details")),
                                       p("calculate heterozygosity,diversity among and within groups, shannon index, number of effective allele,
																                                percent of polymorphic loci, Rogers distance, Nei distance, cluster analysis and multidimensional scaling 2D plot and 3D plot; you can included external groups for colored the
																                                dendogram or MDS plots"),

                                       p(strong("Add external group.- "),"When you have passport information, you can include this information like a groups. You must load a *.csv file, this should contain in the first column the same names of designation, in the next column the passport information."),
                                       p(strong("Remove monomorphic markers.-"),"When we conform groups by cluster analysis or by external file, this new groups will be contain monomorphics markers, so you can decide if delete or not from the analysis."),
                                       p(strong("No. Clusters.-"),"For the cluster analysis, you must write the number of groups that you need to divide the population."),
                                       p(strong("Genetic distance to be calculate.-"),"You can decide wich genetic distance will be calculate"),

                                       h2(strong("References")),
                                       p("de Vicente, M.C., Lopez, C. y Fulton, T. (eds.). 2004. Analisis de la Diversidad Genetica Utilizando
                                                                Datos de Marcadores Moleculares: Modulo de Aprendizaje. Instituto Internacional
                                                                de Recursos Fitogeneticos (IPGRI), Roma, Italia.")
                                ),
                                # )
                       ),

                       tabPanel(div(icon("arrow-right-to-bracket"), "Input steps"),
                                tabsetPanel(
                                  tabPanel(div( icon("dice-one"), "Pick QA-stamp", icon("arrow-right") ) , # icon = icon("dice-one"),
                                           br(),
                                           column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                  column(width=8, selectInput(ns("version2PopStr"), choices = NULL, multiple = FALSE,  label = tags$span(
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
                                           column(width=12, style = "background-color:#d6d4d4; color: black",
                                                  HTML("<font size='3'>Add external information for groups in csv format (optional)</font>"),
                                                  tags$span(id = ns('fileenvbio_holder'),
                                                            fileInput(
                                                              inputId = ns('fileenvbio'),
                                                              label   = NULL,
                                                              width   = '400px',
                                                              accept  = c('.csv')
                                                            )
                                                  ),
                                                  textInput(
                                                    inputId = ns('fileenvbio_url'),
                                                    label   = NULL,
                                                    value   = '',
                                                    width   = '400px',
                                                    placeholder = 'https://example.com/path/file.gz'
                                                  ),
                                                  if (!is.null(geno_groupPopStr)) {
                                                    #checkboxInput(
                                                    #  inputId = ns('geno_groupPopStr'),
                                                    #  label = span('Load example ',
                                                    #               a('external group data', target = '_blank',
                                                    #                 href = geno_groupPopStr)),
                                                    #  value = FALSE
                                                    #)
                                                    shinyWidgets::prettySwitch( inputId = ns('geno_groupPopStr'), label = "Load example", status = "success")
                                                  },
                                                  hr(),
                                                  checkboxInput(ns("quitomono"),
                                                                label = tags$span("Remove monomorphic markers from groups (optional)",
                                                                                  tags$i(class = "glyphicon glyphicon-info-sign",style = "color:#000000",
                                                                                         title = "remove monomorphic"
                                                                                  )
                                                                ),value=FALSE),

                                                  textInput(ns('nclust'),label = tags$span("No. Clusters (default)",
                                                                                           tags$i(class = "glyphicon glyphicon-info-sign",style = "color:#000000",
                                                                                                  title = "Number of cluster that you want to calculated"
                                                                                           )
                                                  ),value='3'),

                                                  radioButtons(ns("distk"), "Genetic distance to be calculate",choices = c(Rogers = "Rogers", Nei = "Nei")),
                                                  radioTooltip(id = ns("distk"), choice = "Rogers", title = "Rogers distance", placement = "right", trigger = "hover"),
                                                  radioTooltip(id = ns("distk"), choice = "Nei", title = "Nei distance", placement = "right", trigger = "hover")

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
                                                         actionButton(ns("runPopStr"), "Run analysis", icon = icon("play-circle")),
                                                         #uiOutput(ns("qaQcPopStrInfo")),
                                                         br(),
                                                  ),

                                           ),
                                           textOutput(ns("outPopStr")),
                                  )
                                )),

                       tabPanel(div(icon("arrow-right-from-bracket"), "Output tabs" ) , value = "outputTabs",
                                tabsetPanel(
                                  tabPanel("Dashboard", icon = icon("file-image"),
                                           br(),
                                           textOutput(ns("outPopStr2")),
                                           br(),
                                           downloadButton(ns("downloadReportPopStr"), "Download dashboard"),
                                           br(),
                                           uiOutput(ns('reportPopStr'))
                                  ),
                                  #termina report
                                  tabPanel("Statistics", icon = icon("table"),
                                           br(),
                                           shinydashboard::box(title="By Genotypes",status="success",width = 12, collapsible = TRUE, solidHeader = TRUE,
                                                               column(width=12,DT::DTOutput(ns("seeDataStatGeno")),style = "overflow-y: scroll;overflow-x: scroll;")
                                           ),shinydashboard::box(title="By Markers",status="success",width = 12,solidHeader = TRUE,
                                                                 column(width=12,DT::DTOutput(ns("seeDataStatMark")),style = "overflow-y: scroll;overflow-x: scroll;")
                                           )
                                  ),
                                  #Termina statistics
                                  #inicia heatmap
                                  tabPanel("General diversity", icon = icon("magnifying-glass-chart"),
                                           br(),
                                           shinydashboardPlus::box(title="Heatmap Distance Matrix",closable = FALSE, width=12, solidHeader = TRUE, collapsible = TRUE, status="success",
                                                                   sidebar = shinydashboardPlus::boxSidebar(id="Heatmap",width = 25,
                                                                                                            selectInput(ns('colorheat'), 'Scale color',choices =c("Jet","Reds","Blues","Viridis"), selected="Jet")
                                                                   ),
                                                                   #grafico heatmap
                                                                   div(plotly::plotlyOutput(ns("heat"),height = "750px",width = "950px"),align="center")
                                           ),
                                           shinydashboard::box(title="Statiscs of diversity",status="success",width = 12,solidHeader = TRUE,
                                                               column(width=12,DT::DTOutput(ns("seeDataDiver")),style = "overflow-y: scroll;overflow-x: scroll;")
                                           )
                                  ),
                                  #termina heatmap
                                  tabPanel("Population structure", icon = icon("table"),
                                           br(),
                                           shinydashboardPlus::box(title = "Dendogram",closable = FALSE, width=12, solidHeader = TRUE, collapsible = TRUE, status="success",
                                                                   sidebar = shinydashboardPlus::boxSidebar(id="dend",width = 25,
                                                                                                            sliderInput(ns('sizeline'),'Size cluster line',min=0.1,max=2,value=0.9),
                                                                                                            sliderInput(ns('sizelab'),'Size labels',min=0.5,max=3,value=0.6),
                                                                                                            sliderInput(ns('space'),'Spaces',min=0.1,max=2,value=0.2),
                                                                                                            tags$hr(),
                                                                                                            selectInput(ns('poslen'),'Position legend',choices=c('left','top','bottom','right'),selected='left'),
                                                                                                            tags$hr(),
                                                                                                            selectInput(ns('typeclust'),'Type',choices=c('circular','rectangular'), selected='circular'),
                                                                                                            #selectInput(ns('catvdend'),'Group',choices = "GroupClust",selected="GroupClust"),
                                                                                                            #Creacion de color para cambiar el color de los puntos del grafico de acuerdo a una lista de colores generada
                                                                                                            selectInput(ns('colordend'),'Choose a color',choices = '',selected="",multiple=T)
                                                                   ),
                                                                   div(plotOutput(ns("dend"),height = "750px",width = "1350px"),align="center")

                                           ),
                                           shinydashboard::box(title="Factors and Groups", status="success",width = 12, solidHeader = TRUE,
                                                               column(width=12,DT::DTOutput(ns("seeDataMDS")),style = "overflow-y: scroll;overflow-x: scroll;")
                                           )
                                  ),
                                  #termina structure
                                  tabPanel("MDS", icon = icon("magnifying-glass-chart"),
                                           br(),
                                           shinydashboardPlus::box(title = "2D-Plot",closable = FALSE, width=12, solidHeader = TRUE, collapsible = TRUE,status="success",
                                                                   sidebar = shinydashboardPlus::boxSidebar(id="mdsplot2d",width = 25,
                                                                                                            selectInput(ns('xcol'), 'X Variable',choices =c("Factor1","Factor2","Factor3"), selected="Factor1"),
                                                                                                            selectInput(ns('ycol'), 'Y Variable',choices =c("Factor1","Factor2","Factor3"), selected="Factor2"),
                                                                                                            #selectInput(ns('zcol'), 'Z Variable',choices =c("Factor1","Factor2","Factor3"), selected="Factor3"),
                                                                                                            tags$hr(),
                                                                                                            #selectInput(ns('catv'),'Group',choices = "GroupClust",selected="GroupClust"),
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
                                                                                                            #textInput(ns('tx'),'X Axis Label',value = "Factor 1 ( 40.51 %)"),
                                                                                                            #textInput(ns('ty'),'Y Axis Label',value = "Factor 2 ( 28.09 %)"),
                                                                                                            #textInput(ns('tz'),'Z Axis Label',value = "Factor 3 ( 8.91 %)"),
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

                                           shinydashboard::box(title="AMOVA",status="success",width = 12,solidHeader = TRUE, collapsible = FALSE,
                                                               column(width=12,DT::DTOutput(ns("seeDataGAMOVA")),style = "overflow-y: scroll;overflow-x: scroll;")
                                           )
                                  )

                                )
                       )# end of output panel
                     )) # end mainpanel


  )
}


#' PopStrApp Server Functions
#'
#' @noRd
mod_PopStrApp_server <- function(id, data){
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
          HTML( as.character(div(style="color:green ; font-size: 20px;", "Data is complete, you could use your raw data or go to do a QC for control missing data and imputation, decide and please proceed to perform the Population structure inspecting the other tabs." )) )
        }else{
          HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your genotype data using the 'Data Retrieval' tab. ")) )
        }
      }
    })

    golem::invoke_js('hideid', ns('fileenvbio_url'))


    observeEvent(input$geno_groupPopStr,
                 if(input$geno_groupPopStr!=FALSE) {
                   shinyWidgets::ask_confirmation(
                     inputId = ns("myconfirmation"),
                     text = "Are you sure you want to load the example of external groups? This will delete any external group file currently in the environment.",
                     title = "Data replacement warning"
                   )
                 }

    )

    observeEvent(input$myconfirmation, {
      if (isTRUE(input$myconfirmation)) {
        bio_example_url <- 'https://raw.githubusercontent.com/Breeding-Analytics/bioflow/main/inst/app/www/example/Groupgeno.csv'
        updateTextInput(session, 'fileenvbio_url', value = bio_example_url)
        golem::invoke_js('hideid', ns('fileenvbio_holder'))
        golem::invoke_js('showid', ns('fileenvbio_url'))
      } else {
        golem::invoke_js('showid', ns('fileenvbio_holder'))
        golem::invoke_js('hideid', ns('fileenvbio_url'))
        updateTextInput(session, 'fileenvbio_url', value = '')
        shinyWidgets::updatePrettySwitch(session, "geno_groupPopStr", value = FALSE)
      }
      save.image(file="popstr.RData")
    }, ignoreNULL = TRUE)

    ## render timestamps flow plot
    output$plotTimeStamps <- shiny::renderPlot({
      req(data()) # req(input$version2Sta)
      xx <- data()$status;  yy <- data()$modeling
      v <- which(yy$parameter == "analysisId")
      if(length(v) > 0){
        yy <- yy[v,c("analysisId","value")]
        zz <- merge(xx,yy, by="analysisId", all.x = TRUE)
      }else{ zz <- xx; zz$value <- NA}
      if(!is.null(xx)){
        colnames(zz) <- cgiarBase::replaceValues(colnames(zz), Search = c("analysisId","value"), Replace = c("outputId","inputId") )
        nLevelsCheck1 <- length(na.omit(unique(zz$outputId)))
        nLevelsCheck2 <- length(na.omit(unique(zz$inputId)))
        if(nLevelsCheck1 > 1 & nLevelsCheck2 > 1){
          X <- with(zz, sommer::overlay(outputId, inputId))
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
        };  rownames(X) <- as.character(zz$outputId)
        rownames(X) <-as.character(as.POSIXct(as.numeric(rownames(X)), origin="1970-01-01", tz="GMT"))
        colnames(X) <-as.character(as.POSIXct(as.numeric(colnames(X)), origin="1970-01-01", tz="GMT"))
        # make the network plot
        n <- network::network(X, directed = FALSE)
        network::set.vertex.attribute(n,"family",zz$module)
        network::set.vertex.attribute(n,"importance",1)
        e <- network::network.edgecount(n)
        network::set.edge.attribute(n, "type", sample(letters[26], e, replace = TRUE))
        network::set.edge.attribute(n, "day", sample(1, e, replace = TRUE))
        #library(ggnetwork)
        ggplot2::ggplot(n, ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
          ggnetwork::geom_edges(ggplot2::aes(color = family), arrow = ggplot2::arrow(length = ggnetwork::unit(6, "pt"), type = "closed") ) +
          ggnetwork::geom_nodes(ggplot2::aes(color = family), alpha = 0.5, size=5 ) + ggplot2::ggtitle("Network plot of current analyses available") +
          ggnetwork::geom_nodelabel_repel(ggplot2::aes(color = family, label = vertex.names ),
                                          fontface = "bold", box.padding = ggnetwork::unit(1, "lines")) +
          ggnetwork::theme_blank()
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
      updateSelectInput(session, "version2PopStr", choices = c(0,traitsMta))
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
    outPopStr1 <- eventReactive(input$runPopStr, {
      req(data())
      catv <-"GroupClust"
      result <- data()
      if(input$version2PopStr==0){
        #if(!"PopStrM" %in% data()$status$module){
        mydata<-as.data.frame(result$data$geno)
        #genodir<-result$data$genodir
      }else{
        qas<-which( names(result$data$geno_imp)==input$version2PopStr )
        mydata<-as.data.frame(result$data$geno_imp[qas])
      }
      distk <- as.character(input$distk)
      nclust <- input$nclust
      dfenvbio<-NULL
      if(length(input$fileenvbio$datapath)!=0) dfenvbio <- read.csv(input$fileenvbio$datapath)
      if(input$fileenvbio_url!='') dfenvbio <- read.csv(input$fileenvbio_url)

      shinybusy::show_modal_spinner('fading-circle', text = 'Calculated...')

      unocheck <- try(
        uno<-cgiarBase::Biodv(mydata,distk, nclust, dfenvbio, catv),
        silent=TRUE
        )
      #uno <- Biodv(mydata,distk, nclust, dfenvbio, catv)
      # add status table
      analysisId <- as.numeric(Sys.time())
      if(input$analysisIdName!=""){
        newStatus <- data.frame(module="PopStrM", analysisId=analysisId, analysisIdName=input$analysisIdName)
      }else{
        newStatus <- data.frame(module="PopStrM", analysisId=analysisId, analysisIdName="myPopStrOne")
      }
      result$status <- rbind(result$status, newStatus)

      if(dim(uno[[5]])[2]==6){
        tmpMDS=uno[[5]][,c(1:4,6)]
        names(tmpMDS)=c("Gen","Factor1","Factor2","Factor3",catv)
      }else{tmpMDS=uno[[5]]}
      newPredMDS=list()
      for ( f in 2:5){
        newPredMDS[[f]] <- data.frame(module="PopStrM",analysisId=analysisId, pipeline="MDS", trait=names(tmpMDS)[f], gid=NA, designation=tmpMDS$Gen, mother=NA, father=NA, entryType=NA, environment=NA, predictedValue=tmpMDS[,f], stdError=NA, reliability=NA)
        newPredMDS[[f]]$predictedValue<-as.numeric(newPredMDS[[f]]$predictedValue)
      }
      newPredMDS=do.call(rbind,newPredMDS)

      newPredGeno=list()
      for ( f in 2:7){
        newPredGeno[[f]] <- data.frame(module="PopStrM",analysisId=analysisId, pipeline="CalculusPerGenotype",trait=names(uno[[4]][[5]])[f], gid=NA, designation=uno[[4]][[5]]$Genotype, mother=NA, father=NA, entryType=NA, environment=NA, predictedValue=uno[[4]][[5]][,f], stdError=NA, reliability=NA)
        newPredGeno[[f]]$predictedValue<-as.numeric(newPredGeno[[f]]$predictedValue)
      }
      newPredGeno=do.call(rbind,newPredGeno)

      newPredMark=list()
      for ( f in 2:8){
        newPredMark[[f]] <- data.frame(module="PopStrM",analysisId=analysisId, pipeline="CalculusPerMarker",trait=names(uno[[4]][[4]])[f], gid=NA, designation=rownames(uno[[4]][[4]]), mother=NA, father=NA, entryType=NA, environment=NA, predictedValue=uno[[4]][[4]][,f], stdError=NA, reliability=NA)
        newPredMark[[f]]$predictedValue<-as.numeric(newPredMark[[f]]$predictedValue)
      }
      newPredMark=do.call(rbind,newPredMark)

      newmetricsSum=list()
      for (f in 1:9){
        newmetricsSum[[f]] <- data.frame(module="PopStrM",analysisId=analysisId, trait=NA, environment=NA, parameter=uno[[4]][[6]][f,1], method="SummaryDiversityAnalysis", value= uno[[4]][[6]][f,2], stdError=NA)
        newmetricsSum[[f]]$value<-as.numeric(newmetricsSum[[f]]$value)
      }
      newmetricsSum=do.call(rbind,newmetricsSum)

      newmetricsAMOVA=list()
      h=0
      for (f in 1:3){
        for (g in 2:9){
          h=h+1
          newmetricsAMOVA[[h]] <- data.frame(module="PopStrM",analysisId=analysisId, trait=paste0(f,uno[[7]][f,1]), environment=NA, parameter=names(uno[[7]])[g], method="AMOVA", value= uno[[7]][f,g], stdError=NA)
          newmetricsAMOVA[[f]]$value<-as.numeric(newmetricsAMOVA[[f]]$value)
        }
      }
      newmetricsAMOVA=do.call(rbind,newmetricsAMOVA)

      newmetricsPerc=list()
      for (f in 1:3){
        newmetricsPerc[[f]] <- data.frame(module="PopStrM",analysisId=analysisId, trait=NA, environment=NA, parameter=paste0("Factor",f), method="VarExplained", value= uno[[3]][f], stdError=NA)
        newmetricsPerc[[f]]$value<-as.numeric(newmetricsPerc[[f]]$value)
      }
      newmetricsPerc=do.call(rbind,newmetricsPerc)

      DistMat3=as.matrix(apply(uno[[4]][[7]][,-c(1,2)],2,as.numeric))
      colnames(DistMat3)=uno[[4]][[7]][,2]
      rownames(DistMat3)=uno[[4]][[7]][,2]
      DistMat3=data.frame(rows=rownames(DistMat3)[row(DistMat3)],cols=colnames(DistMat3)[col(DistMat3)],values=c(DistMat3))
      newPredDist=data.frame(module="PopStrM",analysisId=analysisId, pipeline=NA,trait="DistMat", gid=DistMat3[,1], designation=DistMat3[,2], mother=NA, father=NA, entryType=NA, environment=NA, predictedValue=DistMat3[,3], stdError=NA, reliability=NA)
      newPredDist$predictedValue<-as.numeric(newPredDist$predictedValue)

      result$predictions<-rbind(result$predictions,newPredMDS,newPredGeno,newPredMark,newPredDist)
      result$metrics <- rbind(result$metrics, newmetricsSum,newmetricsAMOVA,newmetricsPerc)
#save(result,file="verRes.RData")
      rm(newmetricsAMOVA,newmetricsPerc,newmetricsSum,newPredDist,newPredGeno,newPredMark, newPredMDS, DistMat3, uno,tmpMDS)
      gc()

      if(!inherits(unocheck,"try-error")) {
        if("analysisIdName" %in% colnames(result$status)){result$status$analysisIdName[nrow(result$status)] <- input$analysisIdName}
        data(result) # update data with results
        cat(paste("Population structure analysis with id:",as.POSIXct(result$status$analysisId[length(result$status$analysisId)], origin="1970-01-01", tz="GMT"),"was saved."))
        output$outPopStr2 <- renderPrint({
          cat(paste("Population structure analysis with id:",as.POSIXct(result$status$analysisId[length(result$status$analysisId)], origin="1970-01-01", tz="GMT"),"was saved."))
        })
        updateTabsetPanel(session, "tabsMain", selected = "outputTabs")
      }else{
        cat(paste("Analysis failed with the following error message: \n\n",uno[[1]]))
        output$outPopStr2 <- renderPrint({
          cat(paste("Analysis failed with the following error message: \n\n",uno[[1]]))
        })
      }

      shinybusy::remove_modal_spinner()

      if(!inherits(unocheck,"try-error")) {
        idMta <- result$status[which(result$status$module == "PopStrM"),"analysisId"]
        idMta <- idMta[length(idMta)]
        uno<-as.data.frame(result$predictions[which(result$predictions$module=="PopStrM" & result$predictions$analysisId==idMta & result$predictions$pipeline=="MDS" ),])
        uno$predictedValue=as.numeric(uno$predictedValue)
        uno<-data.frame(reshape::cast(uno,designation~trait,value= "predictedValue", fun.aggregate = mean))
        if(length(names(uno))>5) {catv<-names(uno)[6]}

        txlab <- paste0('Factor 1 (',result$metrics[which(result$metrics$module=="PopStrM" & result$metrics$analysisId==idMta & result$metrics$parameter=="Factor1" & result$metrics$method=="VarExplained"),7],'%)')
        tylab <- paste0('Factor 2 (',result$metrics[which(result$metrics$module=="PopStrM" & result$metrics$analysisId==idMta & result$metrics$parameter=="Factor2" & result$metrics$method=="VarExplained"),7],'%)')
        tzlab <- paste0('Factor 3 (',result$metrics[which(result$metrics$module=="PopStrM" & result$metrics$analysisId==idMta & result$metrics$parameter=="Factor3" & result$metrics$method=="VarExplained"),7],'%)')
        eti <- "Gen"
        #Actualiza la variable catv (grupos) en conjunto con la seleccion de colores
        set.seed(7)
        colores=colors()[-c(1,3:12,13:25,24,37:46,57:67,80,82,83,85:89,101:106,108:113,126:127,138,140:141,152:253,260:366,377:392,
                            394:447,449,478:489,492,513:534,536:546,557:561,579:583,589:609,620:629,418,436,646:651)]
        d=sample(colores,100)
        #var=as.factor(uno[[5]][,catv])
        grupos=nlevels(as.factor(result$predictions[which(result$predictions$module=="PopStrM" & result$predictions$analysisId==idMta & result$predictions$trait==catv),11]))
        updateSelectInput(session,'color','Choose a color',choices=d,selected=d[1:grupos])
        updateSelectInput(session,'colordend','Choose a color', choices=d,selected=d[1:grupos])

        result$metrics=rbind(result$metrics,data.frame(module="PopStrM",analysisId=idMta, trait=NA, environment=NA, parameter="ColorsDend", method=NA, value= d[1:grupos], stdError=NA))

        #Ver datos en tabla dinamica Summary Diversity
        output$seeDataDiver<-DT::renderDT({
          idMta <- result$status[which(result$status$module == "PopStrM"),"analysisId"]
          idMta <- idMta[length(idMta)]
          seedatosum<-as.data.frame(result$metrics[which(result$metrics$module=="PopStrM" & result$metrics$analysisId==idMta & result$metrics$method=="SummaryDiversityAnalysis"),c(5,7)])
          colnames(seedatosum)=c("Parameter","Value")
          seedatosum[,2]=round(as.numeric(seedatosum[,2]),3)
          DT::datatable(seedatosum, extensions = 'Buttons',
                        options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                       lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
          )
        })

        #Ver datos en tabla dinamica AMOVA
        output$seeDataGAMOVA<-DT::renderDT({
          idMta <- result$status[which(result$status$module == "PopStrM"),"analysisId"]
          idMta <- idMta[length(idMta)]
          seedatosAV=as.data.frame(result$metrics[which(result$metrics$module=="PopStrM" & result$metrics$analysisId==idMta & result$metrics$method=="AMOVA"),])
          seedatosAV$value=as.numeric(seedatosAV$value)
          seedatosAV=data.frame(reshape::cast(seedatosAV,trait~parameter,value = "value", fun.aggregate = mean))
          rownames(seedatosAV)=seedatosAV[,1]
          seedatosAV=seedatosAV[,-1]
          DT::datatable(seedatosAV, extensions = 'Buttons',
                        options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                       lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
          )
        })

        #Ver datos en tabla dinamica %NA, He, Ho by marker
        output$seeDataStatMark<-DT::renderDT({
          idMta <- result$status[which(result$status$module == "PopStrM"),"analysisId"]
          idMta <- idMta[length(idMta)]
          seedatosStaM<-as.data.frame(result$predictions[which(result$predictions$module=="PopStrM" & result$predictions$analysisId==idMta & result$predictions$pipeline=="CalculusPerMarker" ),])
          seedatosStaM$predictedValue=as.numeric(seedatosStaM$predictedValue)
          seedatosStaM<-data.frame(reshape::cast(seedatosStaM,designation~trait,value = "predictedValue", fun.aggregate = mean))
          names(seedatosStaM)[1:2]=c("Marker","%NA")
          seedatosStaM[,1]=rownames(seedatosStaM)
          seedatosStaM[,2:7]=apply(seedatosStaM[,2:7],2,function(x){round(as.numeric(x),4)})
          DT::datatable(seedatosStaM, extensions = 'Buttons',
                        options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                       lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
          )
        })
        #Ver datos en tabla dinamica %NA,He,Ho by genotype
        output$seeDataStatGeno<-DT::renderDT({
          idMta <- result$status[which(result$status$module == "PopStrM"),"analysisId"]
          idMta <- idMta[length(idMta)]
          seedatosStaG<-as.data.frame(result$predictions[which(result$predictions$module=="PopStrM" & result$predictions$analysisId==idMta & result$predictions$pipeline=="CalculusPerGenotype" ),])
          seedatosStaG$predictedValue=as.numeric(seedatosStaG$predictedValue)
          seedatosStaG<-data.frame(reshape::cast(seedatosStaG,designation~trait,value = "predictedValue", fun.aggregate = mean))
          names(seedatosStaG)[1:2]=c("Entry","%NA")
          seedatosStaG[,2:6]=apply(seedatosStaG[,2:6],2,function(x){round(as.numeric(x),4)})
          DT::datatable(seedatosStaG, extensions = 'Buttons',
                        options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                       lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
          )
        })
        #Ver datos en tabla dinamica MDS
        output$seeDataMDS<-DT::renderDT({
          idMta <- result$status[which(result$status$module == "PopStrM"),"analysisId"]
          idMta <- idMta[length(idMta)]
          seedatosMDS<-as.data.frame(result$predictions[which(result$predictions$module=="PopStrM" & result$predictions$analysisId==idMta & result$predictions$pipeline=="MDS" & result$predictions$trait!=catv),])
          grupos=as.factor(result$predictions[which(result$predictions$module=="PopStrM" & result$predictions$analysisId==idMta & result$predictions$trait==catv),11])
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
            idMta <- result$status[which(result$status$module == "PopStrM"),"analysisId"]
            idMta <- idMta[length(idMta)]
            seedatosMDS<-as.data.frame(result$predictions[which(result$predictions$module=="PopStrM" & result$predictions$analysisId==idMta & result$predictions$pipeline=="MDS" & result$predictions$trait!=catv),])
            grupos=as.factor(result$predictions[which(result$predictions$module=="PopStrM" & result$predictions$analysisId==idMta & result$predictions$trait==catv),11])
            seedatosMDS$predictedValue=as.numeric(seedatosMDS$predictedValue)
            seedatosMDS<-as.data.frame(reshape::cast(seedatosMDS,designation~trait,value = "predictedValue", fun.aggregate = mean))
            seedatosMDS=cbind(seedatosMDS,grupos)
            names(seedatosMDS)[c(1,5)]=c("Gen",catv)
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

        #Plot heatmap
        output$heat<-plotly::renderPlotly({
          idMta <- result$status[which(result$status$module == "PopStrM"),"analysisId"]
          idMta <- idMta[length(idMta)]
          distMat=result$predictions[which(result$predictions$module=="PopStrM" & result$predictions$analysisId==idMta & result$predictions$trait=="DistMat"),]
          distMat$predictedValue=as.numeric(distMat$predictedValue)
          distMat=data.frame(reshape::cast(distMat,gid~designation,value = "predictedValue", fun.aggregate = mean))
          if(!is.null(distMat)){
            names=as.character(distMat[,1])
            distMat=as.matrix(distMat[,-1])
            distplot=plotly::plot_ly(x=names,y=names,z = distMat, colorscale=input$colorheat,type = "heatmap")
            distplot=distplot %>% plotly::layout(xaxis = list(showticklabels = F), yaxis = list(showticklabels = F))
            distplot
          }else{
            fig = plotly::plot_ly()
            fig = fig %>% plotly::add_annotations(text = "Information not available.", x = 1, y = 1)#
            fig
          }
        })

        #Dendogram plot
        output$dend=renderPlot({
          if(!is.null(result$data$geno)){
            idMta <- result$status[which(result$status$module == "PopStrM"),"analysisId"]
            idMta <- idMta[length(idMta)]
            info<-as.data.frame(result$predictions[which(result$predictions$module=="PopStrM" & result$predictions$analysisId==idMta & result$predictions$pipeline=="MDS" & result$predictions$trait==catv ),c(6,11)])
            names(info)=c("Gen",catv)
            info<- cbind(ID=info$Gen,info)
            names(info)=c("ID","Gen","Group")
            info$Group=as.factor(info$Group)

            distMat=result$predictions[which(result$predictions$module=="PopStrM" & result$predictions$analysisId==idMta & result$predictions$trait=="DistMat"),]
            distMat$predictedValue=as.numeric(distMat$predictedValue)
            distMat=data.frame(reshape::cast(distMat,gid~designation,value = "predictedValue", fun.aggregate = mean))
            distMat=as.matrix(distMat[,-1])
            rownames(distMat)=colnames(distMat)
            clust=cluster::agnes(distMat, method = "ward")
            tree=ape::as.phylo(as.hclust(clust))

            if (input$typeclust=="rectangular"){
              plot(tree, type = "phylogram", cex = input$sizelab, label.offset = input$space, show.tip.label = TRUE, edge.color = "black", edge.width =input$sizeline, edge.lty = 1,tip.color = input$colordend[info$Group])
              legend(input$poslen, legend=levels(info$Group), fill=input$colordend,box.lty=0)
            }else{
              plot(tree, type = "fan", cex = input$sizelab, label.offset = input$space, show.tip.label = TRUE, edge.color = "black", edge.width =input$sizeline, edge.lty = 1,tip.color = input$colordend[info$Group])
              legend(input$poslen, legend=levels(info$Group), fill=input$colordend,box.lty=0)
            }

            result$metrics=rbind(result$metrics,data.frame(module="PopStrM",analysisId=analysisId, trait=NA, environment=NA, parameter="ColorsDend", method=NA, value= input$colordend, stdError=NA))

          }
        })

        #For report
        output$reportPopStr <- renderUI({
          HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportPopStr.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
        })
        ## Report tab
        output$downloadReportPopStr <- downloadHandler(
          filename = function() {
            paste(paste0('popStrM_dashboard_',gsub("-", "", as.integer(Sys.time()))), sep = '.', switch(
              "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
            ))
          },
          content = function(file) {
            shinybusy::show_modal_spinner(spin = "fading-circle", text = "Generating Report...")

            src <- normalizePath(system.file("rmd","reportPopStr.Rmd",package="bioflow"))
            src2 <- normalizePath('./resultPopStr.RData')

            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))

            file.copy(src, 'report.Rmd', overwrite = TRUE)
            file.copy(src2, 'resultPopStr.RData', overwrite = TRUE)

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

        #if(length(input$fileenvbio$datapath)!=0 & catv=="GroupClust"){
        #  HTML( as.character(div(style="color:green ; font-size: 20px;", "Ready but please check the external groups file, did't find genotype names matches" )) )
        #}else{
        #  HTML( as.character(div(style="color:green ; font-size: 20px;", "Ready" )) )
        #}

      }else{
        #HTML( as.character(div(style="color:green ; font-size: 20px;", "Some errors were found" )) )
        output$seeDataDive <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
        output$seeDataGAMOVA <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
        output$seeDataStatMark <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
        output$seeDataStatGeno <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
        output$seeDataMDS <- DT::renderDT({DT::datatable(NULL)}, server = FALSE)
        hideAll$clearAll <- TRUE
      }### enf of if(!inherits(result,"try-error"))

      hideAll$clearAll <- FALSE
    })

    output$outPopStr <- renderPrint({
      outPopStr1()
    })

  })
}

## To be copied in the UI
# mod_PopStrApp_ui("PopStrApp_1")

## To be copied in the server
# mod_PopStrApp_server("PopStrApp_1")

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
                     tabsetPanel(
                       type = "tabs",
                       tabPanel(div(icon("book"), "Information-Population structure") ,
                                br(),
                                # shinydashboard::box(status="success",width = 12, solidHeader = TRUE,
                                                    column(width=6,   #style = "height:800px; overflow-y: scroll;overflow-x: scroll;",
                                                           tags$body(
                                                             h1(strong(span("Population structure", style="color:green"))),
                                                             h2(strong("Data Status (wait to be displayed):")),
                                                             uiOutput(ns("warningMessage")),

                                                           )
                                                    ),
                                                    column(width=6,
                                                           h2(strong("Details")),
                                                           p("calculate heterozygosity,diversity among and within groups, shannon index, number of effective allele,
																                                percent of polymorphic loci, Rogers distance, Nei distance, cluster analysis and multidimensional scaling 2D plot and 3D plot; you can included external groups for colored the
																                                dendogram or MDS plots"),

                                                           p(strong("Add external group.- "),"When you have passport information, you can include this information like a groups. You must load a *.csv file, this should contain in the first column the same names of designation, in the next columns the passport information."),
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

                       tabPanel(div(icon("arrow-right-to-bracket"), "Input"),
                                tabsetPanel(
                                  tabPanel("Run analysis", icon = icon("dice-one"),
                                           br(),
                                           actionButton(ns("runPopStr"), "Run analysis (click)", icon = icon("play-circle")),
                                           uiOutput(ns("outPopStr")),

                                  ),
                                  tabPanel("Additional settings...", icon = icon("table"),
                                           br(),
                                           br(),
                                           column(width=12, style = "background-color:#d6d4d4; color: black",
                                                  HTML("<font size='3'>Add external information for groups in csv format</font>"),
                                                  tags$span(id = ns('fileenvbio_holder'),
                                                            fileInput(
                                                              inputId = ns('fileenvbio'),
                                                              label   = NULL,
                                                              width   = '400px',
                                                              accept  = c('.csv')
                                                            )
                                                  ),
                                                  if (!is.null(geno_groupPopStr)) {
                                                    checkboxInput(
                                                      inputId = ns('geno_groupPopStr'),
                                                      label = span('Load example ',
                                                                   a('external group data', target = '_blank',
                                                                     href = geno_groupPopStr)),
                                                      value = FALSE
                                                    )},
                                                  hr(),
                                                  checkboxInput(ns("quitomono"),"Remove monomorphic markers from groups",value=FALSE),
                                                  textInput(ns('nclust'),'No. Clusters',value='3'),
                                                  radioButtons(ns("distk"), "Genetic distance to be calculate",choices = c(Rogers = "Rogers", Nei = "Nei"))
                                           )
                                  )

                                )
                       ),

                       tabPanel(div(icon("arrow-right-from-bracket"), "Output" ) , value = "outputTabs",
                                tabsetPanel(
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
                                                                                                            selectInput(ns('typeclust'),'Type',choices=c('circular','rectangular'), selected='rectangular'),
                                                                                                            #selectInput(ns('catvdend'),'Group',choices = "GroupClust",selected="GroupClust"),
                                                                                                            #Creacion de color para cambiar el color de los puntos del grafico de acuerdo a una lista de colores generada
                                                                                                            selectInput(ns('colordend'),'Choose a color',choices = '',selected="",multiple=T)
                                                                   ),
                                                                   div(plotOutput(ns("dend"),height = "750px",width = "1250px"),align="center")

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
                                  ),
                                  tabPanel("Dashboard", icon = icon("file-image"),
                                           br(),
                                           #div(tags$p("Please download the report below:") ),
                                           downloadButton(ns("downloadReportPopStr"), "Download dashboard"),
                                           uiOutput(ns('reportPopStr'))
                                  )
                                  #termina report
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
          HTML( as.character(div(style="color:green ; font-size: 20px;", "Data is complete, please proceed to perform the Population structure inspecting the other tabs." )) )
        }else{
          HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your genotype data using the 'Data Retrieval' tab. ")) )
        }
      }
    })

    output$outPopStr <- renderUI({
      outPopStr()
    })


    outPopStr <- eventReactive(input$runPopStr, {
      req(data())
      result <- data()
      distk <- as.character(input$distk)
      nclust <- input$nclust
      dfenvbio <- NULL
      catv <-"GroupClust"
      if(input$geno_groupPopStr!=FALSE) {
        src <- normalizePath("inst/app/www/example/Groupgeno.csv")
        dfenvbio <- read.csv(src)
      }
      if(length(input$fileenvbio$datapath)!=0) dfenvbio <- read.csv(input$fileenvbio$datapath)
      #print(str(dfenvbio))
      shinybusy::show_modal_spinner('fading-circle', text = 'Calculated...')
      #source("Biodv.R")
      #uno <- Biodv(result,distk, nclust, dfenvbio, catv)
      uno <- cgiarBase::Biodv(result,distk, nclust, dfenvbio, catv)
      shinybusy::remove_modal_spinner()

      rm(result)
      if(!inherits(uno,"try-error")) {
        result <- list(PopStr=uno[[4]])
        result$PopStr$AMOVA=uno[[7]]
        result$PopStr$Plots=list(uno[[3]],uno[[5]],uno[[6]])
        save(result,file=normalizePath("./resultPopStr.RData"))
		
		
        if(length(names(uno[[5]]))>5) catv<-names(uno[[5]])[6]
        txlab <- paste0('Factor 1 (',uno[[3]][1],'%)')
        tylab <- paste0('Factor 2 (',uno[[3]][2],'%)')
        tzlab <- paste0('Factor 3 (',uno[[3]][3],'%)')
        eti <- "Gen"
        #Actualiza la variable catv (grupos) en conjunto con la seleccion de colores
          set.seed(7)
          colores=colors()[-c(1,3:12,13:25,24,37:46,57:67,80,82,83,85:89,101:106,108:113,126:127,138,140:141,152:253,260:366,377:392,
                              394:447,449,478:489,492,513:534,536:546,557:561,579:583,589:609,620:629,418,436,646:651)]
          d=sample(colores,100)
          var=as.factor(uno[[5]][,catv])
          grupos=nlevels(var)		 
          updateSelectInput(session,'color','Choose a color',choices=d,selected=d[1:grupos])
          updateSelectInput(session,'colordend','Choose a color', choices=d,selected=d[1:grupos]) 
		
		  parmBi=list(input$xcol, input$ycol,  d[1:grupos],  input$size,  input$bkgp,  input$tp,  input$ts,  input$pnc,  input$szl,  input$ac)
          parmlist=list(input$typeclust,input$sizelab,input$space,input$sizeline,d[1:grupos],input$poslen)
          plist=list(parmBi,parmlist)
          save(plist,file=normalizePath("./parmDendMDS.RData"))
        
        #Ver datos en tabla dinamica Summary Diversity
        output$seeDataDiver<-DT::renderDT({
          allInfo=result[["PopStr"]]
          seedatosum<-as.data.frame(allInfo[["SummaryDiversityAnalysis"]])
          colnames(seedatosum)=c("Parameter","Value")
          seedatosum[,2]=round(as.numeric(seedatosum[,2]),3)
          DT::datatable(seedatosum, extensions = 'Buttons',
                        options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                       lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
          )
        })


        #Ver datos en tabla dinamica AMOVA
        output$seeDataGAMOVA<-DT::renderDataTable({
          seedatosAV=as.data.frame(uno[[7]])
          rownames(seedatosAV)=seedatosAV[,1]
          seedatosAV=seedatosAV[,-1]
          DT::datatable(seedatosAV, extensions = 'Buttons',
                        options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                       lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
          )
        })

        #Ver datos en tabla dinamica %NA, He, Ho by marker
        output$seeDataStatMark<-DT::renderDT({
          seedatosStaM<-as.data.frame(result[["PopStr"]][[4]])
          seedatosStaM[,1]=rownames(seedatosStaM)
          seedatosStaM[,2:7]=apply(seedatosStaM[,2:7],2,function(x){round(as.numeric(x),4)})
          DT::datatable(seedatosStaM, extensions = 'Buttons',
                        options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                       lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
          )
        })
        #Ver datos en tabla dinamica %NA,He,Ho by genotype
        output$seeDataStatGeno<-DT::renderDT({
          seedatosStaG<-as.data.frame(result[["PopStr"]][[5]])
          seedatosStaG[,2:6]=apply(seedatosStaG[,2:6],2,function(x){round(as.numeric(x),4)})
          DT::datatable(seedatosStaG, extensions = 'Buttons',
                        options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                       lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
          )
        })
        #Ver datos en tabla dinamica MDS
        output$seeDataMDS<-DT::renderDT({
          seedatosMDS<-as.data.frame(uno[[5]])
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

          mydata<-data()$data$geno
          if(!is.null(mydata)){
            p=plotly::plot_ly(data=uno[[5]],x=uno[[5]][,input$xcol],y=uno[[5]][,input$ycol],color=uno[[5]][,catv],
                              type="scatter",mode="markers",colors = input$color,xaxis=F, yaxis=F,
                              text=uno[[5]][,eti],marker=list(size=input$size))
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
        output$heat=plotly::renderPlotly({
          distMat=result[["PopStr"]][["Distance_Matrix"]]
          if(!is.null(distMat)){
            names=as.character(distMat[,2])
            distMat=as.matrix(distMat[,-c(1,2)])
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
          mydata<-data()$data$geno
          if(!is.null(mydata)){
            data=as.data.frame(uno[[5]])
            info<- data[,c("Gen",catv)]
            info<- cbind(ID=info$Gen,info)
            names(info)=c("ID","Gen","Group")
            tree=uno[[6]]

            if (input$typeclust=="rectangular"){
              plot(tree, type = "phylogram", cex = input$sizelab, label.offset = input$space, show.tip.label = TRUE, edge.color = "black", edge.width =input$sizeline, edge.lty = 1,tip.color = input$colordend[info$Group])
              legend(input$poslen, legend=levels(info$Group), fill=input$colordend,box.lty=0)
            }else{
              plot(tree, type = "fan", cex = input$sizelab, label.offset = input$space, show.tip.label = TRUE, edge.color = "black", edge.width =input$sizeline, edge.lty = 1,tip.color = input$colordend[info$Group])
              legend(input$poslen, legend=levels(info$Group), fill=input$colordend,box.lty=0)
            }
			
		  parmBi=list(input$xcol, input$ycol,  input$color,  input$size,  input$bkgp,  input$tp,  input$ts,  input$pnc,  input$szl,  input$ac)
          parmlist=list(input$typeclust,input$sizelab,input$space,input$sizeline,input$colordend,input$poslen)
          plist=list(parmBi,parmlist)
          save(plist,file=normalizePath("./parmDendMDS.RData"))
		  
          }
        })
		
		#For report
        output$reportPopStr <- renderUI({          
          HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportPopStr.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
        })
        ## Report tab
        output$downloadReportPopStr <- downloadHandler(
          filename = function() {
            paste('my-report', sep = '.', switch(
              "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
            ))
          },
          content = function(file) {
		  shinybusy::show_modal_spinner(spin = "fading-circle",
                                          text = "Generating Report...")
            src <- normalizePath(system.file("rmd","reportPopStr.Rmd",package="bioflow"))
            src2 <- normalizePath('./resultPopStr.RData')
            src3 <- normalizePath('./parmDendMDS.RData')

            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'report.Rmd', overwrite = TRUE)
            file.copy(src2, 'resultPopStr.RData', overwrite = TRUE)
            file.copy(src3, 'parmDendMDS.RData', overwrite = TRUE)
            out <- rmarkdown::render('report.Rmd', params = list(toDownload=TRUE),switch(
              "HTML",
              # HTML = rmarkdown::html_document()
              HTML = rmdformats::robobook(toc_depth = 4)
            ))
            file.rename(out, file)
            shinybusy::remove_modal_spinner()
          }
        )

        if(length(input$fileenvbio$datapath)!=0 & catv=="GroupClust"){
          HTML( as.character(div(style="color:green ; font-size: 20px;", "Ready but please check the external groups file, did't find genotype names matches" )) )
        }else{
          HTML( as.character(div(style="color:green ; font-size: 20px;", "Ready" )) )
        }

      }else{
        HTML( as.character(div(style="color:green ; font-size: 20px;", "Some errors were found" )) )
      }
    })

  })
}

## To be copied in the UI
# mod_PopStrApp_ui("PopStrApp_1")

## To be copied in the server
# mod_PopStrApp_server("PopStrApp_1")

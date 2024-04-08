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
                                shinydashboard::box(status="success",width = 12,
                                                    solidHeader = TRUE,
                                                    column(width=12,   style = "height:800px; overflow-y: scroll;overflow-x: scroll;",
                                                           tags$body(
                                                             h1(strong(span("Population structure", style="color:green"))),
                                                             h2(strong("Status:")),
                                                             uiOutput(ns("warningMessage")),
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
                                                              )
                                                          ),
                                                    )
                                ),

                                tabPanel(div(icon("arrow-right-to-bracket"), "Input"),
                                    tabsetPanel(
                                         tabPanel("Run analysis", icon = icon("play"),
                                                  br(),
                                                  actionButton(ns("runPopStr"), "Run analysis", icon = icon("play-circle")),
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
                                                    checkboxInput(
                                                      inputId = ns('geno_groupPopStr'),
                                                      label = span('Load example ',
                                                                   a('external group data', target = '_blank',
                                                                     href = geno_groupPopStr)),
                                                      value = FALSE
                                                    ),
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
                                                               column(width=12,DT::DTOutput(ns("seeDataStatGeno")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
                                           ),shinydashboard::box(title="By Markers",status="success",width = 12,solidHeader = TRUE,
                                                                 column(width=12,DT::DTOutput(ns("seeDataStatMark")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
                                           )
                                  ),
                                  #Termina statistics
                                  #inicia heatmap
                                  tabPanel("General diversity", icon = icon("magnifying-glass-chart"),
                                           br(),
                                           shinydashboardPlus::box(title="Heatmap Distance Matrix",closable = FALSE, width=12, solidHeader = TRUE, collapsible = TRUE, status="success",
                                                                   sidebar = shinydashboardPlus::boxSidebar(id="Heatmap",width = 25,
                                                                                                            selectInput(ns('colorheat'), 'Scale color',choices =c("Reds","Jet","Blues","Viridis"), selected="Reds")
                                                                   ),
                                                                   #grafico heatmap
                                                                   div(plotly::plotlyOutput(ns("heat"),height = "750px",width = "950px"),align="center")
                                           ),
                                           shinydashboard::box(title="Statiscs of diversity",status="success",width = 12,solidHeader = TRUE,
                                                               column(width=12,DT::DTOutput(ns("seeDataDiver")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
                                           )
                                  ),
                                  #termina heatmap
                                  tabPanel("Population structure", icon = icon("table"),
                                           br(),
                                           shinydashboardPlus::box(title = "Dendogram",closable = FALSE, width=12, solidHeader = TRUE, collapsible = TRUE, status="success",
                                                                   sidebar = shinydashboardPlus::boxSidebar(id="dend",width = 25,
                                                                                                            sliderInput(ns('sizeline'),'Size cluster line',min=0.1,max=2,value=0.9),
                                                                                                            sliderInput(ns('sizelab'),'Size labels',min=0.5,max=0.6,value=3),
                                                                                                            sliderInput(ns('space'),'Spaces',min=0.1,max=2,value=0.2),
                                                                                                            tags$hr(),
                                                                                                            selectInput(ns('poslen'),'Position legend',choices=c('left','top','bottom','right'),selected='left'),
                                                                                                            tags$hr(),
                                                                                                            selectInput(ns('typeclust'),'Type',choices=c('circular','rectangular'), selected='rectangular'),
                                                                                                            selectInput(ns('catvdend'),'Group',choices = '',selected=NULL),
                                                                                                            #Creacion de color para cambiar el color de los puntos del grafico de acuerdo a una lista de colores generada
                                                                                                            selectInput(ns('colordend'),'Choose a color',choices = '',selected="",multiple=T)
                                                                   ),
                                                                   div(plotOutput(ns("dend"),height = "750px",width = "1250px"),align="center")
                                                                   
                                           ),
                                           shinydashboard::box(title="Fst",status="success",width = 12,solidHeader = TRUE, collapsible = FALSE,
                                                               column(width=12,DT::DTOutput(ns("seeDataGDiver")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
                                           )
                                  ),
                                  #termina structure
                                  tabPanel("MDS", icon = icon("magnifying-glass-chart"),
                                           br(),
                                           shinydashboardPlus::box(title = "2D-Plot",closable = FALSE, width=12, solidHeader = TRUE, collapsible = TRUE,status="success",
                                                                   sidebar = shinydashboardPlus::boxSidebar(id="mdsplot2d",width = 25,
                                                                                                            selectInput(ns('xcol'), 'X Variable',choices =""),
                                                                                                            selectInput(ns('ycol'), 'Y Variable',choices =""),
                                                                                                            selectInput(ns('zcol'), 'Z Variable',choices =""),
                                                                                                            tags$hr(),
                                                                                                            selectInput(ns('catv'),'Group',choices = '',selected=NULL),
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
                                                                                                            textInput(ns('tx'),'X Axis Label',value = "Factor 1 ( 40.51 %)"),
                                                                                                            textInput(ns('ty'),'Y Axis Label',value = "Factor 2 ( 28.09 %)"),
                                                                                                            textInput(ns('tz'),'Z Axis Label',value = "Factor 3 ( 8.91 %)"),
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
                                                                                                            selectInput(ns('eti'),'Label points',choices=''),
                                                                                                            sliderInput(ns('size'),'Points size',min=5,max=25,value=7)
                                                                   ),
                                                                   #grafico 2d
                                                                   div(plotly::plotlyOutput(ns("try"),height = "750px",width = "950px"),align="center")
                                           ),

                                           br(),
                                                    shinydashboard::box(title="Factors and Groups", status="success",width = 12, solidHeader = TRUE,
                                                                        column(width=12,DT::DTOutput(ns("seeDataMDS")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
                                                    )
                                           #)
                                  ),
                                  tabPanel("Report", icon = icon("file-image"),
                                           br(),
                                           div(tags$p("Please download the report below:") ),
                                           downloadButton(ns("downloadReportPopStr"), "Download report"),
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
        result <- data()
        uno=DoforDiv()
        result$PopStr<-uno[[8]]
        result$PopStr$treeStr<-uno[[4]]
        result$PopStr$MarkStr<-uno[[5]]
        result$PopStr$DistStr<-uno[[6]]
        ## write the new status table
        newStatus <- data.frame(module="PopStr", analysisId= as.numeric(Sys.time()))
        result$status <- rbind(result$status, newStatus)
        data(result)
        save(result,file=normalizePath("R/outputs/resultPopStr.RData"))
        output$reportPopStr <- renderUI({
          HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportPopStr.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
        })
        HTML( as.character(div(style="color:green ; font-size: 20px;", "Ready" )) )
    })

    DoforDiv<-reactive({
      req(data())
      outFolder<-"BioAnalysis"
      nall=2
      mydata=data()
      datos<-t(as.data.frame(mydata$data$geno))
	    distk<- as.character(input$distk)
      typedata<- "SNP"
      missval=0
      mayorque=0.95
      menorque=0.05
      ht1=-1
      ht2=0
      ht3=1
      if (typedata=="SNP"){
		    datos=replace(datos,datos==ht1,99)
		    datos=replace(datos,datos==ht2,0.5)
		    datos=replace(datos,datos==ht3,999)
		    datos=replace(datos,datos==99,1)
		    datos=replace(datos,datos==999,0)
		    datos <- data.frame(datos)
      }
      if (typedata=="FREQ"){datos <- data.frame(datos)}

      dirfile=as.character(mydata$data$genodir)
      nn=length(unlist((strsplit(dirfile,"[\\]"))))
      filename=as.character(unlist((strsplit(dirfile,"[\\]")))[nn])
      outFolder <- paste("DiversityAnalysis_",stringr::str_replace(filename,".csv",""),sep="")

      ### Correr funciones ----------------------------------------
      if (typedata=="CUENTA"){
		    dtmp=as.data.frame(cbind(Allele=datos[,1],SNP=as.numeric(rep(c(NA,2)),dim(datos)[1]/2),datos[,-1]))
		    datos <- data.frame(CounToFreqNI(data_set=dtmp))
	    }
	    if (typedata=="DistMat"){
		    mrdMAT=as.matrix(datos[,-1])
		    rownames(mrdMAT)=colnames(datos)[-1]
		    colnames(mrdMAT)=colnames(datos)[-1]
		    ##graphical representation, the MDS (multidimensional scaling) analysis
		    mds=cmdscale(mrdMAT, k=3,eig=T)
		    coord=mds$points
		    perctCP12=c(round(mds$eig[1]/sum(mds$eig)*100,2),round(mds$eig[2]/sum(mds$eig)*100,2),round(mds$eig[3]/sum(mds$eig)*100,2))
		    colnames(coord)=c("dim1","dim2","dim3")
		    rm(mds)
		    ###For do dendograms and MDSgraph
		    clust=cluster::agnes(mrdMAT, method = "ward")
		    coord2=cbind(gen=colnames(datos)[-1],coord)
		    names(coord2)=c("Gen","Factor1","Factor2","Factor3")
		    datos=NULL
		    div=NULL
		    biodata=list(as.data.frame(div),coord2, getwd(), clust, datos, mrdMAT, perctCP12)
	    }else{
	      shinybusy::show_modal_spinner('fading-circle', text = 'Calculated...')
	      biodata=cgiarBase::Biodv(stringr::str_replace(filename,".csv",""),datos,nall,distk,mayorque,menorque,missval,typedata,ht1,ht2,ht3)
	      shinybusy::remove_modal_spinner()
	    }

	    updateTextInput(session,'tx','X Axis Label',value = paste0('Factor 1 (',biodata[[7]][1],'%)'))
	    updateTextInput(session,'ty','Y Axis Label',value = paste0('Factor 2 (',biodata[[7]][2],'%)'))
	    updateTextInput(session,'tz','Z Axis Label',value = paste0('Factor 3 (',biodata[[7]][3],'%)'))
	    updateTextInput(session,'tx3D','X Axis Label',value = paste0('Factor 1 (',biodata[[7]][1],'%)'))
	    updateTextInput(session,'ty3D','Y Axis Label',value = paste0('Factor 2 (',biodata[[7]][2],'%)'))
	    updateTextInput(session,'tz3D','Z Axis Label',value = paste0('Factor 3 (',biodata[[7]][3],'%)'))
	    return(biodata)
  })


 observeEvent(data(),{
  #Ver datos en tabla dinamica Summary Diversity
  output$seeDataDiver<-DT::renderDT({
    allInfo=data()[["PopStr"]]
    seedatosum<-as.data.frame(allInfo[["SummaryDiversityAnalysis"]])
    colnames(seedatosum)=c("Parameter","Value")
    seedatosum[,2]=round(as.numeric(seedatosum[,2]),3)
    DT::datatable(seedatosum, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                 lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
                  )

  })
})

 observeEvent(data(),{
  #Ver datos en tabla dinamica Population structure
  output$seeDataGDiver<-DT::renderDT({
    seedatosPS<-data()[["PopStr"]]
    datos<-as.data.frame(seedatosPS[["MarkStr"]])
    datos1<-as.data.frame(mdata1()[[1]])
	  seedatosFst=cgiarBase::gdiv(datos,datos1,as.character(input$catv),as.character(input$quitomono),as.data.frame(seedatosPS[["DistStr"]]))
	  seedatos1<-as.data.frame(seedatosFst[[1]])
    names(seedatos1)=c("Parameter","Groups","Fst","NumMark_UsedForCalculated")
    seedatos1$Fst=round(as.numeric(seedatos1$Fst),4)
	  DT::datatable(seedatos1, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                 lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
    )
  })
})

 observeEvent(data(),{
   #Ver datos en tabla dinamica %NA, He, Ho by marker
   output$seeDataStatMark<-DT::renderDT({
     seedatos<-data()[["PopStr"]]
     seedatosStaM<-as.data.frame(seedatos[[4]])
     seedatosStaM[,1]=rownames(seedatosStaM)
     seedatosStaM[,2:7]=apply(seedatosStaM[,2:7],2,function(x){round(as.numeric(x),4)})
     DT::datatable(seedatosStaM, extensions = 'Buttons',
                   options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                  lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
     )
   })
 })

 observeEvent(data(),{
   #Ver datos en tabla dinamica %NA,He,Ho by genotype
   output$seeDataStatGeno<-DT::renderDT({
     seedatos<-data()[["PopStr"]]
     seedatosStaG<-as.data.frame(seedatos[[5]])
     seedatosStaG[,2:6]=apply(seedatosStaG[,2:6],2,function(x){round(as.numeric(x),4)})
     DT::datatable(seedatosStaG, extensions = 'Buttons',
                   options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                  lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
     )
   })
 })

 observeEvent(data(),{
   #Ver datos en tabla dinamica MDS
   output$seeDataMDS<-DT::renderDT({
     seedatosMDS<-as.data.frame(mdata1()[[1]])
     seedatosMDS[,2:4]=apply(seedatosMDS[,2:4],2,function(x){round(as.numeric(x),4)})
     DT::datatable(seedatosMDS, extensions = 'Buttons',
                   options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                  lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
     )
   })
 })
  
 #Transformacion de los datos, para el uso posterior en los graficos
 DataGPopStr <- reactiveVal(NULL)
 observeEvent(input[["input$fileenvbio"]], {
   dat <- read.csv(input[["input$fileenvbio"]][["datapath"]])
   DataGPopStr(dat)
 })
 
 observeEvent(input[["geno_groupPopStr"]], {
   src <- normalizePath("inst/app/www/example/Groupgeno.csv")
   dat <- read.csv(src)
   DataGPopStr(dat)
 })

  mdata1=reactive({
    req(data())
    see1=data()[["PopStr"]]
    #Cada que se actualice nclust
    pp=as.data.frame(cutree(as.hclust(see1[["treeStr"]]), k = input$nclust))
    TFArx=ape::as.phylo(as.hclust(see1[["treeStr"]]))
    #pp=as.data.frame(cutree(as.hclust(DoforDiv()[[4]]), k = input$nclust))
    #TFArx=ape::as.phylo(as.hclust(DoforDiv()[[4]]))
    groups=as.data.frame(pp)
    coord2=as.data.frame(see1[["MDSTable"]])
    coord2=cbind(rownames(coord2),coord2)
    #coord2=as.data.frame(DoforDiv()[[2]])
    data1=as.data.frame(cbind(coord2,groups[,1]))
    names(data1)=c("Gen","Factor1","Factor2","Factor3","GroupClust")
    data1$Factor1=as.numeric(as.character(data1$Factor1))
    data1$Factor2=as.numeric(as.character(data1$Factor2))
    data1$Factor3=as.numeric(as.character(data1$Factor3))
    data1$GroupClust=as.factor(as.character(data1$GroupClust))
    #Cuando se agrega un archivo para grupos externos
    if (!is.null(DataGPopStr())){
      dfenvbio <-DataGPopStr()
  	  dfenvbio[,1]<-dfenvbio[,1]
	    indexCOV <- match(data1$Gen,as.character(dfenvbio[,1]))
      if(length(indexCOV)>0)	dfenvbio <- dfenvbio[indexCOV,]
      dfenvbio[,2] <- as.factor(dfenvbio[,2])
      usenames=names(dfenvbio)[-1]
	    #save(dfenvbio,data1,file="ver.RData")
      dfenvbio<-dfenvbio[match(data1$Gen,dfenvbio[,1]),]
      for(i in 1:dim(dfenvbio)[2]){dfenvbio[,i]=as.factor(as.character(dfenvbio[,i]))}
      data1<-cbind(data1,dfenvbio[,-1])
	    names(data1)=c("Gen","Factor1","Factor2","Factor3","GroupClust",usenames)
    }
    #Nombre de las variables en la base de datos
    vars=names(data1)
    #Actualiza el selectinput de acuerdo a la base de datos cargada
    updateSelectInput(session,'xcol', 'X Variable',choices = vars,selected=vars[2])
    updateSelectInput(session,'ycol', 'Y Variable',choices = vars,selected=vars[3])
    updateSelectInput(session,'zcol', 'Z Variable',choices = vars,selected=vars[4])
    updateSelectInput(session,'catv', 'Group',choices = vars,selected=vars[5])
    updateSelectInput(session,'eti', 'Label',choices = vars,selected=vars[1])
    updateSelectInput(session,'xcol3D', 'X Variable',choices = vars,selected=vars[2])
    updateSelectInput(session,'ycol3D', 'Y Variable',choices = vars,selected=vars[3])
    updateSelectInput(session,'zcol3D', 'Z Variable',choices = vars,selected=vars[4])
    updateSelectInput(session,'catv3D', 'Group',choices = vars,selected=vars[5])
    updateSelectInput(session,'eti3D', 'Label',choices = vars,selected=vars[1])
    updateSelectInput(session,'catvdend', 'Group',choices = vars,selected=vars[5])

    result=list(data1,TFArx)
    return(result)
  })
  #Actualiza la variable catv (grupos) en conjunto con la seleccion de colores
  #Cada que se elige un grupo se genera una cantidad de colores correspondiente
  #al numero de elementos de cada grupo
  observeEvent(input$catv,{
    set.seed(7)
    colores=colors()[-c(1,3:12,13:25,24,37:46,57:67,80,82,83,85:89,101:106,108:113,126:127,138,140:141,152:253,260:366,377:392,
                        394:447,449,478:489,492,513:534,536:546,557:561,579:583,589:609,620:629,418,436,646:651)]
    d=sample(colores,100)
    var=as.factor(mdata1()[[1]][,input$catv])
    grupos=nlevels(var)
    updateSelectInput(session,'color','Choose a color',choices=d,selected=d[1:grupos])
  })
  observeEvent(input$catvdend,{
    set.seed(7)
    colores=colors()[-c(1,3:12,13:25,24,37:46,57:67,80,82,83,85:89,101:106,108:113,126:127,138,140:141,152:253,260:366,377:392,
                        394:447,449,478:489,492,513:534,536:546,557:561,579:583,589:609,620:629,418,436,646:651)]
    d=sample(colores,100)
    var=as.factor(mdata1()[[1]][,input$catvdend])
    grupos=nlevels(var)
    updateSelectInput(session,'colordend','Choose a color', choices=d,selected=d[1:grupos])
  })
  #grafico 2d
  output$try=plotly::renderPlotly({
    mydata<-data()$data$geno
    if(!is.null(mydata)){
    p=plotly::plot_ly(data=mdata1()[[1]],x=mdata1()[[1]][,input$xcol],y=mdata1()[[1]][,input$ycol],color=mdata1()[[1]][,input$catv],
              type="scatter",mode="markers",colors = input$color,xaxis=F, yaxis=F,
              text=mdata1()[[1]][,input$eti],marker=list(size=input$size))
      #color de fondo del grafico
      p= p %>% plotly::layout(plot_bgcolor=input$bkgp)
      #titulo y etiquetas ejes
      p= p %>% plotly::layout(title=input$tp,titlefont=list(size=input$ts,color=input$pnc), xaxis = list(title = input$tx, titlefont=list(size=input$szl,color=input$ac)),
             yaxis = list(title = input$ty,titlefont=list(size=input$szl,color=input$ac)))
    p
    }else{
      fig = plotly::plot_ly()
      fig = fig %>% plotly::add_annotations(text = "Information not available.", x = 1, y = 1)#
      fig
    }
  })

#Plot heatmap
  output$heat=plotly::renderPlotly({
    distMat=data()[["PopStr"]][["Distance_Matrix"]]
    if(!is.null(distMat)){
    names=as.character(distMat[,2])
    distMat=as.matrix(distMat[,-c(1,2)])
    distplot=plotly::plot_ly(x=names,y=names,z = distMat, colorscale="viridis",type = "heatmap")
    distplot=distplot %>% plotly::layout(xaxis = list(showticklabels = F), yaxis = list(showticklabels = F))
    distplot
    }else{
      fig = plotly::plot_ly()
      fig = fig %>% plotly::add_annotations(text = "Information not available.", x = 1, y = 1)#
      fig
    }
  })

  output$dend=renderPlot({
    mydata<-data()$data$geno
    if(!is.null(mydata)){
      data=as.data.frame(mdata1()[[1]])
      info<- data[,c("Gen",input$catvdend)]
      info<- cbind(ID=info$Gen,info)
      names(info)=c("ID","Gen","Group")
      tree=mdata1()[[2]]
      
      if (input$typeclust=="rectangular"){
        plot(tree, type = "phylogram", cex = input$sizelab, label.offset = input$space, show.tip.label = TRUE, edge.color = "black", edge.width =input$sizeline, edge.lty = 1,tip.color = input$colordend[info$Group])
        legend(input$poslen, legend=levels(info$Group), fill=input$colordend,box.lty=0)
      }else{
        plot(tree, type = "fan", cex = input$sizelab, label.offset = input$space, show.tip.label = TRUE, edge.color = "black", edge.width =input$sizeline, edge.lty = 1,tip.color = input$colordend[info$Group])
        legend(input$poslen, legend=levels(info$Group), fill=input$colordend,box.lty=0)
      }
      #colnames(info)[1]="label"
      #p=ggtree::ggtree(tree, layout=input$typeclust ,size=input$sizeline)
      #c1=data.frame(as.character(p$data$label))
      #names(c1)="label"
      #if(dim(info)[1]!=length(p$data$label)){
      #  v=merge(info,c1,id="label")
      #  v1=data.frame(cbind(label=rep(NA,abs(length(p$data$label)-dim(info)[1])),Gen=rep(NA,abs(length(p$data$label)-dim(info)[1])),Group=rep(NA,abs(length(p$data$label)-dim(info)[1]))))
      #  v2=rbind(v,v1)
      #}
      #p$data$Gen=v2$Gen
      #p$data$Group=v2$Group
      #p=p + ggtree::scale_color_manual(values=input$colordend)
      #p=p + ggtree::geom_tiplab(mapping=ggplot2::aes(label=Gen,color=Group),size=input$sizelab, offset=input$space, hjust=0.5)
      #p=p + ggtree::theme(legend.position=input$poslen)
      #p
    }
  })
  
  ## Report tab
  output$downloadReportPopStr <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    content = function(file) {
      src <- normalizePath(system.file("rmd","reportPopStr.Rmd",package="bioflow"))
      src2 <- normalizePath('R/outputs/resultPopStr.RData')
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      file.copy(src2, 'resultPopStr.RData', overwrite = TRUE)
      out <- rmarkdown::render('report.Rmd', params = list(toDownload=TRUE),switch(
        "HTML",
        HTML = rmarkdown::html_document()
      ))
      file.rename(out, file)
    }
  )


  })
}

## To be copied in the UI
# mod_PopStrApp_ui("PopStrApp_1")

## To be copied in the server
# mod_PopStrApp_server("PopStrApp_1")

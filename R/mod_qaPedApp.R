#' qaPedApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_qaPedApp_ui <- function(id){
  ns <- NS(id)
  tagList(


    # input <- list(propNaUpperThreshForMarker=0.3,propNaUpperThreshForInds=0.3,maf=0.5,imputationMethod="median", ploidy=2 )
    shiny::sidebarPanel(#width = 3,
      width = 3,
      tags$style(".well {background-color:grey; color: #FFFFFF;}"),
      # div(tags$p( h4(strong("Quality Assurance for Genetic Markers")))),#, style = "color: #817e7e"
      HTML("<img src='www/cgiar3.png' width='42' vspace='10' hspace='10' height='46' align='top'>
                  <font size='5'>Quality Assurance for Pedigree</font>"),
      hr(style = "border-top: 1px solid #4c4c4c;"),

      wellPanel(
      h3("Imputed data"),
      sliderInput(ns("inSlider1"), "Designation:",
                  min = 10000, max = 20000, step=500,value = c(14000, 17000)),
      sliderInput(ns("inSlider2"), "Mother:",
                  min = 10000, max = 20000, step=500,value = 16000),
      sliderInput(ns("inSlider3"), "Father:",
                  min = 10000, max = 20000, step=500,value = 16000)
      ),

      wellPanel(
        h3("Coincidence"),
        sliderInput(ns("inSlider4"), "QC%:",
                    min = 0, max = 3, step=0.1,value = c(1.2, 2)),
        sliderInput(ns("inSlider41"), "QC counts fail:",
                    min = 100, max = 2000, step=100,value = 1000),
        sliderInput(ns("inSlider42"), "QC counts fail G1:",
                    min = 100, max = 2000, step=100,value = 1400),
        sliderInput(ns("inSlider43"), "QC counts fail G2:",
                    min = 100, max = 2000, step=100,value = 1200)
      ),

      wellPanel(
        h3("Heterozygosity"),
        sliderInput(ns("inSlider5"), "Designation:",
                    min = 0, max = 10, step=1,value = 5),
        sliderInput(ns("inSlider6"), "Mother:",
                    min = 0, max = 10, step=1,value = 5),
        sliderInput(ns("inSlider7"), "Father:",
                    min = 0, max = 10, step=1,value = 5)
      ),

      wellPanel(
      h3("G matrix values comparison"),
      sliderInput(ns("inSlider8"), "Mother-Designation:",
                  min = 0, max = 1.6, step=0.1,value = c(0.2,0.7)),
      sliderInput(ns("inSlider81"), "Mother-Designation Hight:",
                  min = 1.5, max = 3, step=0.1,value = 1.6),
      sliderInput(ns("inSlider9"), "Father-Designation:",
                  min = 0, max = 1.5, step=0.1,value = c(0.2,0.7)),
      sliderInput(ns("inSlider10"), "diag(Designation)-(Mother-Designation):",
                  min = 0, max = 2, step=0.1,value = 0.2)
      ),

      hr(style = "border-top: 1px solid #4c4c4c;"),
      actionButton(ns("runQaPed"), "Add QC", icon = icon("play-circle")),
      hr(style = "border-top: 1px solid #4c4c4c;"),
      textOutput(ns("outQaPed"))
    ), # end sidebarpanel
    shiny::mainPanel(width = 9,
                     tabsetPanel( #width=9,
                       type = "tabs",
                       tabPanel(p("Information",class="info-p"),  icon = icon("book"),
                                br(),
                                shinydashboard::box(status="success",width = 12,
                                                    solidHeader = TRUE,
                                                    column(width=12,   style = "height:800px; overflow-y: scroll;overflow-x: scroll;",
                                                           tags$body(
                                                             h2(strong("Status:")),
                                                             uiOutput(ns("warningMessage")),
                                                             h2(strong("Details")),
                                                             p("When pedigree information will be use for do analysis, we need to ensure the quality of relation of genetic markers for each triplet of individuals.
                                                             This option aims to allow users to identify bad individuals given certain QA parameters. The",strong(style="color: red; font-size: 15px;","RECOMENDATION is to keep the limits of arguments based in genetic theory"), "but if the user decide change
                                                            the way arguments are used is the following:"),

                                                             p(h4(strong("Imputed data"))," this sets a threshold for how much missing data in a marker information is allowed for each individual in the triplet of the pedigree information and the classification for each one is:"),
                                                             tags$ul(
                                                             tags$li(strong("Designation imputed")),
                                                             tags$ul(
                                                              tags$li("<14000 is good designation"),
                                                              tags$li("1400-17000 is sample_poor designation"),
                                                              tags$li(">17000 is sample_fail designation")
                                                             ),
                                                             tags$li(strong("Mother imputed")),
                                                             tags$ul(
                                                              tags$li(">16000 poor mother"),
                                                              tags$li("<16000 correct mother")
                                                             ),
                                                             tags$li(strong("Father imputed")),
                                                             tags$ul(
                                                              tags$li(">16000 poor father"),
                                                              tags$li("<16000 correct father")
                                                             )
                                                             ),

                                                             p(h4(strong("Coincidence")),"  this sets a threshold for allele quality control."),
                                                             tags$ul(
                                                              tags$li(strong("Allele QC%")),
                                                              tags$ul(
                                                                tags$li("0-1.2% QC pass"),
                                                                tags$li("1.2-2% QC borderline"),
                                                                tags$li(">2 QC fail")
                                                             ),
                                                             tags$li(strong("QC count fail"),"QC fail when have less than 1000 ocurrences, we can create the following groups accord imputed information limits"),
                                                             tags$ul(
                                                              tags$li(strong("QC count fail G1")),
                                                              tags$ul(
                                                              tags$li("Designation imputed > Qc G1 No assessment-designation sample too poor")
                                                              ),
                                                              tags$li(strong("QC count fail G2")),
                                                              tags$ul(
                                                                tags$li("Designation imputed <= Qc G1 & Designation mother>Qc G2 & Designation father>Qc G2 No assessment mother and father samples too poor"),
                                                                tags$li("Mother imputed>Father imputed No QC test, no mother assessment - mother sample too poor"),
                                                                tags$li("Mother imputed<Father imputed No QC test, no father assessment - father sample too poor")
                                                             ))),
                                                             p(h4(strong("Heterozigocity"))," this sets a threshold for what is the heterozygosity allowed for each individual in the triplet of the pedigree information and the classification for each one is:"),
                                                             tags$ul(
                                                             tags$li(strong("Heterozygosity Designation")),
                                                             tags$ul(
                                                              tags$li(">5% designation excess heterozygosity"),
                                                              tags$li("<5% correct designation")
                                                             ),
                                                             tags$li(strong("Heterozygosity Mother")),
                                                             tags$ul(
                                                              tags$li(">5% mother excess heterozygosity"),
                                                              tags$li("<5% correct mother")
                                                             ),
                                                             tags$li(strong("Heterozygosity Father")),
                                                             tags$ul(
                                                             tags$li(">5% father excess heterozygosity"),
                                                             tags$li("<5% correct father")
                                                              )
                                                             ),
                                                             p(h4(strong("G matrix values comparison"))," this sets a threshold for G matrix values comparisons."),
                                                             tags$ul(
                                                             tags$li(strong("Gmat Mother-Designation.-")),
                                                             tags$ul(
                                                              tags$li("<0.2 mother wrong"),
                                                              tags$li("0.2-0.7 mother borderline"),
                                                              tags$li("0.7-1.6 correct mother")
                                                             ),
                                                             tags$li(strong("Gmat Mother-Designation higth")),
                                                             tags$ul(
                                                              tags$li(">1.6 mother hight")
                                                             ),
                                                             tags$li(strong("Gmat Father-Designation")),
                                                             tags$ul(
                                                              tags$li("<0.2 male wrong"),
                                                              tags$li("0.2-0.7 male boderline"),
                                                              tags$li("0.7-1.5 correct father")
                                                             ),
                                                             tags$li(strong("(Gmat diag Designation)-(Gmat Mother-Designation).-")),
                                                             tags$ul(
                                                              tags$li("<0.2 probable self"),
                                                              tags$li(">0.2 correct")
                                                             )
                                                              ),

                                                             h2(strong("References")),
                                                             p("")
                                                           )
                                                    ),

                                )
                       ),
                       tabPanel(p("Output",class="output-p"), icon = icon("arrow-right-from-bracket"),
                                tabsetPanel(
                                  tabPanel("Classification", icon = icon("table"),
                                  br(),
                                  shinydashboard::box(status="success",width = 12,
                                                    solidHeader = TRUE,
                                                    column(width=12,DT::DTOutput(ns("modificationsQaPed")),style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
                                )
                                ),
                                tabPanel("Plot classification", icon = icon("magnifying-glass-chart"),
                                         br(),
                                         shinydashboard::box(status="success",width = 12,
                                                             solidHeader = TRUE,
                                                             selectInput(ns("traitOutqPed"), "", choices = NULL, multiple = FALSE),
                                                             plotly::plotlyOutput(ns("plotClassPed"))
                                         )
                                )
                                )
                       )# end of output panel
                     )) # end mainpanel


  )
}

#' qaPedApp Server Functions
#'
#' @noRd
mod_qaPedApp_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })
    ############################################################################

    # data = reactive({ # provisional dataset for testing
    #   load("dataStr0.RData")
    #   data <- res
    #   return(data)
    # })
    # warning message
    output$warningMessage <- renderUI({

      if(is.null(data())){
        HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your data using the 'Data Retrieval' tab." )) )
      }else{
        if(!is.null(data()$data$geno) & !is.null(data()$data$pedigree)){
            c1=as.data.frame(data.table::fread(data()$data$genodir, sep = '\t', header = F,nrows=1))[1:11]
            c2=as.data.frame(data.table::fread(data()$data$genodir, sep = '\t', header = F,nrows=1))[-c(1:11)]
            objP<-as.vector(data()$metadata$pedigree$value[1:3])
            hapmap_snp_attr <- c('rs#', 'alleles', 'chrom', 'pos', 'strand', 'assembly#',
                               'center', 'protLSID', 'assayLSID', 'panelLSID', 'QCcode','assembly','rs')
            if(length(intersect(hapmap_snp_attr, c1)) != 11){
                HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your genotype data in hapmap format using the 'Data Retrieval' tab." )) )
            }else{
              if(length(which(unique(unlist(data()$data$pedigree[,objP]))%in%c2==F))!=0){
                HTML( as.character(div(style="color: red; font-size: 20px;", "Please review your data, some pedigree individuals were not found in the genotype data" )) )
              }else{
                HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to perform the pedigree QA inspecting the other tabs.")) )
            }
            }
        }else{
        if(is.null(data()$data$geno) & !is.null(data()$data$pedigree)){
          HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your genotype data in hapmap format using the 'Data Retrieval' tab. ")) )
      }else{
      if(!is.null(data()$data$geno) & is.null(data()$data$pedigree)){
        HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your pedigree data using the 'Data Retrieval' tab. ")) )
      }
      }
        }
      }
   })
    ##
    newModificationsPed <- reactive({ # p('File to be analyzed')
      req(data())
      myObject <- data()
      objP<-as.vector(myObject$metadata$pedigree$value[1:3])
      shinybusy::show_modal_spinner('fading-circle', text = 'Do classification...')
      res <- computePedModifications(
        M = myObject$data$geno,
        Tb1=myObject$data$pedigree[,objP],
        Tb2=myObject$data$genodir,
        Limp1=as.numeric(input$inSlider1)[1],
        Limp2=as.numeric(input$inSlider1)[2],
        QCc1=as.numeric(input$inSlider4)[1],
        QCc2=as.numeric(input$inSlider4)[2],
        GmatFL1=as.numeric(input$inSlider8)[1],
        GmatFL2=as.numeric(input$inSlider81),
        GmatFL3=as.numeric(input$inSlider8)[2],
        GmatML1=as.numeric(input$inSlider9)[1],
        GmatML2=as.numeric(input$inSlider9)[2],
        GmatD_FL1=as.numeric(input$inSlider10),
        HetL1=as.numeric(input$inSlider5),
        HetF1=as.numeric(input$inSlider6),
        HetM1=as.numeric(input$inSlider7),
        Fimp1=as.numeric(input$inSlider2),
        Mimp1=as.numeric(input$inSlider3),
        QcTest=as.numeric(input$inSlider41),
        QcTestG1=as.numeric(input$inSlider42),
        QcTestG2=as.numeric(input$inSlider43)
      )
      shinybusy::remove_modal_spinner()
      updateSelectInput(session,"traitOutqPed",choices = names(res)[c(7,9,11,13,15,17,19,21,23,25,27)])
      #myObject$data$pedigree<-res
      ## reactive
      return(res)
    })

    PlotSelected<-reactive({
      req(data())
      req(input$traitOutqPed)
      QCPed <- newModificationsPed()
      ver2<-as.character(input$traitOutqPed)
      ### change column names for mapping
      ver=data.frame(table(QCPed[,ver2]))
      ver=cbind(Class=rep(ver2),ver)
      ver$Freq=ver$Freq/sum(ver$Freq)
      ver$Var1=as.factor(ver$Var1)
      ver$Class=as.factor(ver$Class)
      figS <- plotly::plot_ly(ver,x=~Class,y=~Freq,type = 'bar', name =~ Var1) #%>% layout(yaxis = list(title = 'Percent'),barmode = 'stack')
      figS
    })

    output$plotClassPed <- plotly::renderPlotly({
      PlotSelected()
    })

    ## display the current outliers
    observeEvent(data(),{

      output$modificationsQaPed <-  DT::renderDT({
        req(data())
        ##
        c1=as.data.frame(data.table::fread(data()$data$genodir, sep = '\t', header = F,nrows=1))[1:11]
        #c1=read.delim(data()$data$genodir,colClasses = "character",comment.char="",check.names=FALSE,header=F,nrows=1)[1:11]
        hapmap_snp_attr <- c('rs#', 'alleles', 'chrom', 'pos', 'strand', 'assembly#',
                             'center', 'protLSID', 'assayLSID', 'panelLSID', 'QCcode','assembly','rs')
        c2=as.data.frame(data.table::fread(data()$data$genodir, sep = '\t', header = F,nrows=1))[-c(1:11)]
        objP<-as.vector(data()$metadata$pedigree$value[1:3])
#& (length(intersect(hapmap_snp_attr, c1))==11) & (length(which(unique(unlist(data()$data$pedigree[,objP]))%in%c2==F))==0)
        if(!is.null(data()$data$pedigree) && !is.null(data()$data$geno) && (length(intersect(hapmap_snp_attr, c1))==11) && (length(which(unique(unlist(data()$data$pedigree[,objP]))%in%c2==F))==0)){
          mo <- newModificationsPed()
        }else{
          mo <- data.frame(warningText="Genetic marker/Pedigree data not available")
        }
        DT::datatable(mo, extensions = 'Buttons',
                      options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                     lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
        )
      })

    })
    ##########################
    ## save when user clicks
    outQaPed <- eventReactive(input$runQaPed, {

      req(data())
      final1 <- newModificationsPed()
      shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
      ## get the outlier table
      objP<-as.vector(data()$metadata$pedigree$value[1:3])
      Tb1=data()$data$pedigree[,objP]
      qamAnalysisId <- as.numeric(Sys.time())
      mods <- as.data.frame(matrix(nrow=length(which(final1[,27]==1)), ncol=6))
      colnames(mods) <- c("module" ,     "analysisId"  ,"reason"  ,     "row" ,"col"  , "value" )
      mods$reason<-final1[which(final1[,27]==1),26]
      mods$row=which(rownames(data()$data$geno)%in%Tb1[which(final1[,27]==1),1]==T)
      mods$col="only row"
      mods$module="qaPed"
      mods$analysisId=qamAnalysisId
      mods$value="1"

      if(nrow(mods) > 0){
        ## store the new modifications table
        result <- data()
        #save(mods, file = "./R/outputs/resultQaGeno.RData")
        result$modifications$geno <- rbind(result$modifications$geno, mods )
        ## write the new status table
        newStatus <- data.frame(module="qaPed", analysisId= mods$analysisId[nrow(mods)])
        result$status <- rbind(result$status, newStatus)
        data(result)
        cat(paste("Modifications to genotype information saved with id:",as.POSIXct( mods$analysisId[nrow(mods)], origin="1970-01-01", tz="GMT") ))
      }else{
        cat("No modifications to add.")
      }
      shinybusy::remove_modal_spinner()
    })
    output$outQaPed <- renderPrint({
      outQaPed()
    })
    #

  })
}

## To be copied in the UI
# mod_qaPedApp_ui("qaPedApp_1")

## To be copied in the server
# mod_qaPedApp_server("qaPedApp_1")

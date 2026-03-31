#' qaPedApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_qaPedApp_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # color guide for backgrounds: https://www.w3schools.com/colors/colors_names.asp
    # color guide for fonts: https://www.w3schools.com/html/html_colors_hex.asp
    tags$br(),
    
    mainPanel(width = 12,
              tabsetPanel( id=ns("tabsMain"),
                           type = "tabs",
                           tabPanel(div(icon("book"), "Information") ,
                                    br(),
                                    column(width = 6,
                                           h1(strong(span("Pedigree QA/QC",
                                                          #tags$a(href="https://www.youtube.com/watch?v=rR1DhTt25n4&list=PLZ0lafzH_UmclOPifjCntlMzysEB2_2wX&index=7",icon("youtube") , target="_blank"),
                                                          style="color:darkcyan"))),
                                           h2(strong("Data Status (wait to be displayed):")),
                                           uiOutput(ns("warningMessage")),
                                           tags$br(),
                                           # column(width=4, tags$br(),
                                           shinyWidgets::prettySwitch( inputId = ns('launch'), label = "Load example dataset", status = "success"),
                                           # ),
                                           tags$br(),
                                           img(src = "www/qaGeno.png",height = 100, width = 435), # add an image
                                    ),
                                    column(width = 6,
                                           h2(strong("Details")),
                                           p("When pedigree information will be use for do analysis, we need to ensure the quality of relation of genetic markers for each triplet of individuals.
                                                             This option aims to allow users to identify bad individuals given certain QA parameters. The",strong(style="color: red; font-size: 15px;","RECOMENDATION is to keep the limits of arguments based in genetic theory")),
                                           p(strong("Threshold")," "),
                                           
                                           h2(strong("References")),
                                           p(""),
                                           h2(strong("Software used")),
                                           p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
                                Vienna, Austria. URL https://www.R-project.org/."),
                                           p(""),
                                    ),
                           ),
                           tabPanel(div(icon("arrow-right-to-bracket"), "Input steps"),
                                    tabsetPanel(
                                      tabPanel(div( icon("dice-one"), "Pick STA-stamp(s)", icon("arrow-right") ), # icon = icon("dice-one"),
                                               br(),
                                               column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                      column(width=8,
                                                             selectInput(ns("version2qaPed"),
                                                                         label = tags$span(
                                                                           "Genotype QA/QC version(s) to analyze",
                                                                           tags$i(
                                                                             class = "glyphicon glyphicon-info-sign",
                                                                             style = "color:#FFFFFF",
                                                                             title = "Analysis ID(s) from Genotype QA/QC used to Pedigree QA/QC."
                                                                           )
                                                                         ),
                                                                         choices = NULL, multiple = FALSE),
                                                      ),
                                               ),
                                               column(width=12),
                                               shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = TRUE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                   column(width=12,
                                                                          hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                          h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you pick the right input-parameter values to be specified in the grey boxes above.", style="color:green"))),
                                                                          hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                   ),
                                                                   column( width=12, shiny::plotOutput(ns("plotTimeStamps")) ),
                                                                   DT::DTOutput(ns("statusqaPed")), # modeling table
                                               ),
                                      ),
                                      tabPanel(div(icon("dice-two"), "Percent of wrong threshold", icon("arrow-right") ),
                                               br(),
                                               fluidRow(
                                                 column(width = 4,
                                                        numericInput(ns('wrongG'),'% wrong (Impossible genotypes >):', value = 5, min = 0, max = 1, step = 0.01),
                                                        wellPanel(
                                                          HTML("<div style='text-align: justify; font-size: 15px; line-height: 1.6; padding: 10px;'>
										<strong>For each triplet of parents with a child, the veracity 
										of the result is checked for each marker, that is, if there is AAA, 
										this combination is a correct possibility, however, KMA is not correct. 
										In this way, for each triplet, a percentage can be calculated that 
										indicates how far the children are from the parents. Therefore, the 
										higher this percentage, the lower the coincidence with the parents.</strong> 
										</div>"))
                                                 ),
                                                 column(width = 8,
                                                        plotly::plotlyOutput(ns("hist_wrongG"))
                                                 )
                                               )
                                      ),
                                      tabPanel(div(icon("dice-three"), "Line-Mother GRM thresholds", icon("arrow-right") ), # icon = icon("dice-two"),
                                               br(),
                                               fluidRow(
                                                 column(width = 4,
                                                        numericInput(ns('GLML'),'Mother wrong (<):', value = 0.2, min = 0, max = 1, step = 0.01),
                                                        numericInput(ns('GLMB'),'Mother borderline [><]:', value = 0.7, min = 0, max = 1, step = 0.01),
                                                        numericInput(ns('GLMC'),'Mother correct [>=]:', value = 1.6, min = 0, max = 1, step = 0.01),
                                                        wellPanel(
                                                          HTML("<div style='text-align: justify; font-size: 15px; line-height: 1.6; padding: 10px;'>
								<strong>Let us remember that the kinship matrix calculated 
								by VanRaden defines values close to 2 if the individuals 
								have a greater genetic similarity, then with the help 
								of the plot and your wisdom about the crop you are analyzing, 
								verify that the threshold values to determine if the child 
								resembles the mother are correct.</strong> 
										</div>"))
                                                 ),
                                                 column(width = 8,
                                                        plotly::plotlyOutput(ns("hist_GLM"))
                                                 )
                                               )
                                      ),#end three
                                      tabPanel(div(icon("dice-four"), "Line-Father GRM thresholds", icon("arrow-right") ),
                                               br(),
                                               fluidRow(
                                                 column(width = 4,
                                                        numericInput(ns('GLFL'),'Father wrong (<):', value = 0.2, min = 0, max = 1, step = 0.01),
                                                        numericInput(ns('GLFB'),'Father borderline [><]:', value = 0.7, min = 0, max = 1, step = 0.01),
                                                        numericInput(ns('GLFC'),'Father correct [>=]:', value = 1.5, min = 0, max = 1, step = 0.01),
                                                        wellPanel(
                                                          HTML("<div style='text-align: justify; font-size: 15px; line-height: 1.6; padding: 10px;'>
								<strong>Let us remember that the kinship matrix calculated 
							   by VanRaden defines values close to 2 if the individuals have 
							   a greater genetic similarity, then with the help of the plot 
							   and your wisdom about the crop you are analyzing, verify that 
							   the threshold values to determine if the child resembles the 
							   father are correct.</strong> 
										</div>"))
                                                 ),
                                                 column(width = 8,
                                                        plotly::plotlyOutput(ns("hist_GLF"))
                                                 )
                                               )
                                      ),#end four
                                      tabPanel(div(icon("dice-five"), "(Diag Line)-(Line-Mother) GRM thresholds", icon("arrow-right") ),
                                               br(),
                                               fluidRow(
                                                 column(width = 4,
                                                        numericInput(ns('GDLMC'),'Correct (>):', value = 0.2, min = 0, max = 1, step = 0.01),
                                                        wellPanel(
                                                          HTML("<div style='text-align: justify; font-size: 15px; line-height: 1.6; padding: 10px;'>
								<strong>Let us remember that the kinship matrix calculated 
							   by VanRaden defines values close to 2 if the individuals 
							   have a greater genetic similarity, in the case of the 
							   diagonal the values must be 2 because it is the comparison 
							   of the individual with itself, then if we take the value 
							   of the child of the diagonal and subtract it from the 
							   value that relates the mother with the child, this should 
							   be very close to zero if they are related, then with 
							   the help of the plot and your wisdom about the crop you 
							   are analyzing, verify that the threshold value are correct.</strong> 
										</div>"))
                                                 ),
                                                 column(width = 8,
                                                        plotly::plotlyOutput(ns("hist_GDLM"))
                                                 )
                                               )
                                      ),
                                      tabPanel("Run analysis", icon = icon("dice-six"),
                                               br(),
                                               column(width=12,style = "background-color:grey; color: #FFFFFF",
                                                      column(width=3, tags$div(textInput(ns("analysisIdName"), label = tags$span(
                                                        "Analysis Name (optional)", tags$i( class = "glyphicon glyphicon-info-sign", style = "color:#FFFFFF",
                                                                                            title = "An optional name for the analysis besides the timestamp if desired.") ), #width = "100%",
                                                        placeholder = "(optional name)") ) ),
                                                      column(width=3,
                                                             br(),
                                                             actionButton(ns("runqaPed"), "Identify & store modifications", icon = icon("play-circle")),
                                                             uiOutput(ns("qaPedInfo")),
                                                             br(),
                                                      ),
                                                      
                                               ),
                                               textOutput(ns("outqaPed")),
                                      ),#end run analysis
                                    )#end tabset input
                                    
                           ),#end input panel
                           tabPanel(div(icon("arrow-right-from-bracket"), "Output tabs" ) , value = "outputTabs",
                                    tabsetPanel(
                                      tabPanel("Dashboard", icon = icon("file-image"),
                                               br(),
                                               textOutput(ns("outqaPed2")),
                                               br(),
                                               actionButton(ns("renderReportqaPed"), "Download dashboard", icon = icon("download")),
                                               downloadButton(ns("downloadReportqaPed"), "Download dashboard", style = "visibility:hidden;"),
                                               br(),
                                               uiOutput(ns('reportqaPed'))
                                      )
                                    ) # end of tabset
                           )# end of output panel
              )) # end mainpanel
    
  )
}

#' qaPedApp Server Functions
#'
#' @noRd
mod_qaPedApp_server <- function(id, data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })
    
    #################
    ## version
    observeEvent(data(), {
      req(data())
      dtMtaAsr <- data() # dtMtaAsr <- result
      dtMtaAsr <- dtMtaAsr$status
      dtMtaAsrGeno <- dtMtaAsr[which(dtMtaAsr$module == "qaGeno"),]
      traitsMtaAsrGeno <- unique(dtMtaAsrGeno$analysisId)

	  if(!is.null(traitsMtaAsrGeno)){
      	#if(length(traitsMtaAsrGeno)==0){traitsMtaAsrGeno="No data available"}
      	if(length(traitsMtaAsrGeno) > 0){
        	if("analysisIdName" %in% colnames(dtMtaAsrGeno)){
          		names(traitsMtaAsrGeno) <- paste(dtMtaAsrGeno$analysisIdName, as.POSIXct(traitsMtaAsrGeno, origin="1970-01-01", tz="GMT"), sep = "_")
        	}else{
          		names(traitsMtaAsrGeno) <- as.character(as.POSIXct(traitsMtaAsrGeno, origin="1970-01-01", tz="GMT"))
        	}
      	}
	  }
      updateSelectInput(session, "version2qaPed", choices = traitsMtaAsrGeno)
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
    
    
    # functions for checking pedigree (father) data
    is_valid_fatherCol <- function(x) {
      !is.null(x) && length(x) > 0 && !is.na(x) && nzchar(x)
    }
    
    #################
    # warning message
    output$warningMessage <- renderUI(
      if(is.null(data())){
        HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your data using the 'Data Retrieval' tab.")) )
      }else{ # pedigree and genotype data is there
        fatherCol <- NULL
        if(!is.null(data()$metadata$pedigree)){
          if("father" %in% data()$metadata$pedigree$parameter){
            fatherCol <- data()$metadata$pedigree[which(data()$metadata$pedigree$parameter == 'father'),'value']
          }
        }
        if(is_valid_fatherCol(fatherCol)){
          if("qaGeno" %in% data()$status$module){
            HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to perform the QA/QC Pedigree specifying your input parameters under the Input tabs.")) )
          }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please perform QA/QC Genotype before performing QA/QC Ped.")) ) }
        }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please make sure that you load you Pedigree data.")) )}
      }
    )
    
    #################
    ## render timestamps flow plot
    output$plotTimeStamps <- shiny::renderPlot({
      req(data()) # req(input$version2Sta)
      xx <- data()$status;  yy <- data()$modeling # xx <- result$status;  yy <- result$modeling
      if("analysisIdName" %in% colnames(xx)){existNames=TRUE}else{existNames=FALSE}
      if(existNames){
        xx$analysisIdName <- paste(xx$analysisIdName, as.character(as.POSIXct(as.numeric(xx$analysisId), origin="1970-01-01", tz="GMT")),sep = "_" )
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
        rownames(X) <- as.character(zz$outputId)
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
    
    
    ped_qa_data <- reactiveValues(GRM_metric = NULL, impossible_metric = NULL)
    
    plot_impossible<-function(peddata,metaped,glgeno){
      print("Comparison of triplets")
      load("InputInfo.RData")
      uno=data.frame(value=names(peddata))
      #tres=merge(uno,dos,by="value")
      names(peddata)=metaped$parameter[which(metaped$value%in%uno$value==T)]
      resDMF<-lapply(split(peddata[,c("designation","mother","father")],1:dim(peddata)[1]),unlist)
      dimres<-unlist(lapply(resDMF,length))
      useful<-which(dimres==3)
      #save(peddata,metaped,glgeno,file="ver.RData")
      if((length(which(dimres==3))*100)/dim(peddata)[1]>=0.8){
        resDMF<-resDMF[which(dimres==3)]
        
        #CompareTriplets file
        iupac_map <- c(
          "A" = "A", "T" = "T", "C" = "C", "G" = "G",
          "AG" = "R", "CT" = "Y", "GC" = "S", "AT" = "W",
          "GA" = "R", "TC" = "Y", "CG" = "S", "TA" = "W",
          "GT" = "K", "AC" = "M", "NN" = "N",
          "TG" = "K", "CA" = "M"
        )
        withProgress(message="Calculating percent of wrong for each designation...",value=0, {
          # Separar alelos una sola vez
          alleles <- do.call(rbind, strsplit(glgeno$loc.all, "/", fixed = TRUE))
          # Lookup hash ultra rápido
          lookup <- setNames(as.matrix(CompareTriplets[,2:3]), CompareTriplets$Combi)
          # Conversión a matriz (mucho más rápido que data.frame)
          geno_matrix <- as.matrix(glgeno)
          calc_fun <- function(idx){
            if(all(idx %in% rownames(geno_matrix))) {
              geno_ind <- t(geno_matrix[idx,,drop=FALSE])
              geno_ind[is.na(geno_ind)] <- "NN"
              combi <- apply(cbind(alleles, geno_ind), 1, function(r){
                a1 <- r[1]
                a2 <- r[2]
                g <- r[3:5]
                g[g=="0"] <- a1
                g[g=="2"] <- a2
                g[g=="1"] <- paste0(a1,a2)
                g[g=="NN"] <- "NN"
                paste(iupac_map[g], collapse="")
              })
              wrtst <- lookup[match(combi, CompareTriplets$Combi),,drop=FALSE]
              sums <- colSums(wrtst, na.rm=TRUE)
              c(sums, round(100*sums[1]/sums[2],3))
          }else{
              c(NA,NA,NA)            
            }
          }
          n <- length(resDMF)
          resDMF <- lapply(seq_along(resDMF), function(i){
            incProgress(1/n, detail="processing")
            calc_fun(resDMF[[i]])
          })
          
        })
        return(impossible_metric=list(useful,dimres,resDMF))
      }else{
        return(impossible_metric=list(useful,dimres))
      }
      
    }
    
    plot_GRM<-function(peddata,metaped,geno,useful){
      print("Calculate GRM")
      uno=data.frame(value=names(peddata))
      names(peddata)=metaped$parameter[which(metaped$value%in%uno$value==T)]
      peddata<-peddata[useful,]
      ploidyFactor <- max(geno)/2
      G <- sommer::A.mat(as.matrix(geno-ploidyFactor))
      resM <- lapply(split(peddata[,c("designation","mother")],1:dim(peddata)[1]),unlist)
      resD <- lapply(split(peddata[,c("designation","designation")],1:dim(peddata)[1]),unlist)
      resF<-lapply(split(peddata[,c("designation","father")],1:dim(peddata)[1]),unlist)
      resM<-lapply(resM, function(x) match(x, rownames(G)))
      resD <- lapply(resD, function(x) match(x, rownames(G)))
      resF<-lapply(resF, function(x) match(x, rownames(G)))
      GLM_metric<-unlist(lapply(resM,function(x){G[x[1],x[2]]}))
      GLF_metric<-unlist(lapply(resF,function(x){G[x[1],x[2]]}))
      GDLM_metric<-(unlist(lapply(resD,function(x){G[x[1],x[1]]}))-unlist(lapply(resM,function(x){G[x[1],x[2]]})))
      GRM_metric=list(GLM_metric,GLF_metric,GDLM_metric)
      return(GRM_metric)
    }
    
    filters_ped<-function(peddata,metaped,GRM_metric,impossible_metric,pwrong,mlong,mborder,mcorrect,flong,fborder,fcorrect,dllcorrect){
      print("Pedigree classification")
      load("InputInfo.RData")
      uno=data.frame(value=names(peddata))
      names(peddata)=metaped$parameter[which(metaped$value%in%uno$value==T)]
      #save(peddata,GRM_metric,impossible_metric,pwrong,mlong,mborder,mcorrect,flong,fborder,fcorrect,dllcorrect,file="list.RData")
      namesline=peddata[impossible_metric[[1]],c("designation","mother","father")]
      QCc<-sapply(impossible_metric[[3]], function(x) x[3])
      GmatFL<-GRM_metric[[1]]
      GmatML<-GRM_metric[[2]]
      GmatD_FL<-GRM_metric[[3]]
      QCc=ifelse(is.na(QCc),NA,ifelse(QCc<pwrong,"Allele_QC_PASS", ifelse(QCc>pwrong,"Allele_QC_FAIL","Allele_QC_BORDERLINE")))
      #GmatFL=ifelse(is.na(QCc),NA,ifelse(GmatFL < flong,"FEMALE_GMAT_WRONG",ifelse(GmatFL > fcorrect,"FEMALE_HIGH_GMAT",ifelse(GmatFL < fborder,"FEMALE_BORDERLINE_GMAT","XXX"))))
	  GmatFL=ifelse(is.na(QCc),NA,ifelse(GmatFL < flong,"FEMALE_GMAT_WRONG",ifelse(GmatFL >= flong & GmatFL <= fborder,"FEMALE_BORDERLINE_GMAT","XXX")))			  
      GmatML=ifelse(is.na(QCc),NA,ifelse(GmatML<mlong,"MALE_GMAT_WRONG", ifelse(GmatML>=mlong & GmatML<=mborder,"MALE_BORDERLINE_GMAT","XXX")))
      GmatD_FL=ifelse(is.na(QCc),NA,ifelse(GmatD_FL<dllcorrect,"PROBABLE_SELF","XXX"))
      condtest=data.frame(Condicional=paste0("XXX_",QCc,"_",GmatFL,"_",GmatML,"_XXX_XXX_XXX_",GmatD_FL,"_XXX_XXX"))
      #save(condtest,FinalClass,file="ver.RData")
      finaltab <- cbind(condtest,FinalClass[match(condtest$Condicional, FinalClass$Condicional), -1])
      finaltab<-finaltab[,c(2,4)]
      finaltab$Final.Classification[is.na(finaltab$Final.Classification)] <- "NO GENO DATA" 
      finaltab$Category[is.na(finaltab$Category)] <- "NO GENO DATA" 
      tmp=do.call(rbind,impossible_metric[[3]])
      tmp=cbind(namesline,tmp,QCc,GRM_metric[[1]],GmatFL,GRM_metric[[2]],GmatML,GRM_metric[[3]],GmatD_FL,finaltab)
      names(tmp)=c("designation","Mother","Father","wrong","testable","QCPer_wrong","QCPer_wrong_class","G_matrix_Line_Female","G_matrix_Line_Female_Class","G_matrix_Line_Male","G_matrix_Line_Male_Class",
                   "Diagonal_minus_Female_G_matrix","Diagonal_Female_G_matrix_Class","Final_Classification","Status")
      return(tmp)
    }
    
    
    output$hist_wrongG<- plotly::renderPlotly({
      req(data())
      req(input$version2qaPed)
      geno <- data()$data$geno
      qas <- which(names(data()$data$geno_imp) == input$version2qaPed)
      geno_imp<-data()$data$geno_imp[[qas]]
      geno<-modifyGeno(geno,geno_imp)
      peddata<-data()$data$pedigree
      metaped<-data()$metadata$pedigree
        shinybusy::show_modal_spinner('fading-circle', text = 'Long process...')
        impossible_metric=plot_impossible(peddata,metaped,geno)
        ped_qa_data$impossible_metric=impossible_metric
        shinybusy::remove_modal_spinner()
        
        if((length(which(impossible_metric[[2]]==3))*100)/dim(peddata)[1]>=0.8){
          plotly::plot_ly(x = sapply(impossible_metric[[3]], function(x) x[3]),
                          type = "histogram",
                          name = "Percentage of wrong")
        }else{
          ggplot2::ggplot() + ggplot2::ggtitle("Not enough matching information was found")
        }
    })
    
    modifyGeno<-function(geno, geno_imp){
        #geno<-result$data$geno
        #geno_imp<-result$data$geno_imp[[1]]
        loc_names_imp<-adegenet::locNames(geno_imp)
        ind_names_imp<-adegenet::indNames(geno_imp)
        loc_names<-which(adegenet::locNames(geno)%in%loc_names_imp==T)
        ind_names<-which(adegenet::indNames(geno)%in%ind_names_imp==T)
        geno<-geno[ind_names,loc_names]
        return(geno)
    }
    
    output$hist_GLM<- plotly::renderPlotly({
      req(data())
      req(ped_qa_data$impossible_metric)
      req(input$version2qaPed)
      qas <- which(names(data()$data$geno_imp) == input$version2qaPed)
      Markers <- as.data.frame(data()$data$geno_imp[[qas]])
      peddata<-data()$data$pedigree
      metaped<-data()$metadata$pedigree
      
        shinybusy::show_modal_spinner('fading-circle', text = 'Calculated GRM...')
        useful=ped_qa_data$impossible_metric[[1]]
        GRM_metric=plot_GRM(peddata,metaped,Markers,useful)
        ped_qa_data$GRM_metric=GRM_metric
        shinybusy::remove_modal_spinner()
        if( (length(useful)*100)/dim(peddata)[1]>=0.8){
          plotly::plot_ly(x = GRM_metric[[1]],
                          type = "histogram",
                          name = "Original Data")
        }else{
          ggplot2::ggplot() + ggplot2::ggtitle("Not enough matching information was found")
        }
    })
    
    output$hist_GLF<- plotly::renderPlotly({
      req(data())
      req(ped_qa_data$impossible_metric)
      peddata<-data()$data$pedigree
      
        shinybusy::show_modal_spinner('fading-circle', text = 'Calculated GRM...')
        useful=ped_qa_data$impossible_metric[[1]]
        GRM_metric=ped_qa_data$GRM_metric
        shinybusy::remove_modal_spinner()
        if((length(useful)*100)/dim(peddata)[1]>=0.8){
          plotly::plot_ly(x = GRM_metric[[2]],
                          type = "histogram",
                          name = "Original Data")
        }else{
          ggplot2::ggplot() + ggplot2::ggtitle("Not enough matching information was found")
        }
    })
    
    output$hist_GDLM<- plotly::renderPlotly({
      req(data())
      req(ped_qa_data$impossible_metric)
      peddata<-data()$data$pedigree
      
        shinybusy::show_modal_spinner('fading-circle', text = 'Calculated GRM...')
        useful=ped_qa_data$impossible_metric[[1]]
        GRM_metric=ped_qa_data$GRM_metric
        shinybusy::remove_modal_spinner()
        if((length(useful)*100)/dim(peddata)[1]>=0.8){
          plotly::plot_ly(x = GRM_metric[[3]],
                          type = "histogram",
                          name = "Original Data")
        }else{
          ggplot2::ggplot() + ggplot2::ggtitle("Not enough matching information was found")
        }
    })
    
    report <- reactiveVal(NULL)
    
    observeEvent(input$renderReportqaPed,{
      shinybusy::show_modal_spinner(spin = "fading-circle", text = "Generating Report...")
      
      result <- data()
      
      src <- normalizePath(system.file("rmd","reportqaPed.Rmd",package="bioflow"))
      src2 <- normalizePath('data/resultqaPed.RData')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      file.copy(src2, 'resultqaPed.RData', overwrite = TRUE)
      
      outReport <- rmarkdown::render('report.Rmd', params = list(toDownload=TRUE ),
                                     switch("HTML", HTML = rmdformats::robobook(toc_depth = 4)
                                            # HTML = rmarkdown::html_document()
                                     ))
      
      report(outReport)
      
      shinybusy::remove_modal_spinner()
      
      shinyjs::click("downloadReportqaPed")
    })
    
    ## render result of "run" button click
    outqaPed1 <- eventReactive(input$runqaPed, {
      req(data())
      req(input$version2qaPed)
      req(ped_qa_data$GRM_metric)
      req(ped_qa_data$impossible_metric)
      
      shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
      
      peddata<-data()$data$pedigree
      metaped<-data()$metadata$pedigree
      GRM_metric<-ped_qa_data$GRM_metric
      impossible_metric<-ped_qa_data$impossible_metric
        
      decisionped<-try(filters_ped(peddata,metaped,GRM_metric,impossible_metric,input$wrongG,input$GLML,input$GLMB,input$GLMC,
                                 input$GLFL,input$GLFB,input$GLFC,input$GDLMC),
                       silent=TRUE
      )
      
      if(!inherits(decisionped,"try-error") ) {
        result<-data()        
        
		Idstatus<-as.numeric(Sys.time())
		newStatus <- data.frame(module="qaPed", analysisId=Idstatus, analysisIdName=input$analysisIdName)
        if(!is.null(result$status)){
          result$status <- rbind(result$status, newStatus)
        }else{result$status <- newStatus}
        
		newModeling <- data.frame(module=rep("qaPed",8), analysisId=rep(Idstatus,8), trait=rep("none",8), environment=rep("general",8) , parameter=c("pwrong","G_matrix_Line_Male_wrong","G_matrix_Line_Male_borderline","G_matrix_Line_Male_correct",
		"G_matrix_Line_Female_wrong","G_matrix_Line_Female_borderline","G_matrix_Line_Female_correct", "Diagonal_minus_Female_G_matrix_correct"), value=c(input$wrongG,input$GLML,input$GLMB,input$GLMC,input$GLFL,input$GLFB,input$GLFC,input$GDLMC))
		if(!is.null(result$modeling)){
          result$modeling <- rbind(result$modeling, newModeling)
        }else{result$modeling <- newModeling}
        
		delrow<-which(decisionped$Status=="FAIL")
		if(length(delrow)!=0){
			newModifications <-data.frame(module=rep("qaPed",length(delrow)), analysisId=rep(Idstatus,length(delrow)), reason=rep("FAIL",length(delrow)),row=delrow)
		}else{
			newModifications<-data.frame(module="qaPed", analysisId=as.numeric(Sys.time()), reason=NA,row=NA)
		}
        if(!is.null(result$modifications$pedigree)){
          result$modifications$pedigree <- rbind(result$modifications$pedigree, newModifications)
        }else{result$modifications$pedigree <- newModifications}
		
		
		df_long1 <- decisionped[,1:7] %>% tidyr::pivot_longer(cols = -c(designation, Mother, Father, QCPer_wrong_class), names_to = "variable", values_to = "value")
    df_long2 <- decisionped[,c(1:3,8:9)] %>% tidyr::pivot_longer(cols = -c(designation, Mother, Father, G_matrix_Line_Female_Class), names_to = "variable", values_to = "value") 
		df_long3 <- decisionped[,c(1:3,10:11)] %>% tidyr::pivot_longer(cols = -c(designation, Mother, Father, G_matrix_Line_Male_Class), names_to = "variable", values_to = "value") 
		df_long4 <- decisionped[,c(1:3,12:13)] %>% tidyr::pivot_longer(cols = -c(designation, Mother, Father, Diagonal_Female_G_matrix_Class), names_to = "variable", values_to = "value") 
		df_long5 <- decisionped[,c(1:3,15,15)] %>% tidyr::pivot_longer(cols = -c(designation, Mother, Father,Status.1), names_to = "variable", values_to = "value") 
		df_long5$value <- as.numeric(as.factor(df_long5$value))
		names(df_long1)=c("designation","mother","father","entryType","trait","predictedValue")
		names(df_long2)=c("designation","mother","father","entryType","trait","predictedValue")
		names(df_long3)=c("designation","mother","father","entryType","trait","predictedValue")
		names(df_long4)=c("designation","mother","father","entryType","trait","predictedValue")
		names(df_long5)=c("designation","mother","father","entryType","trait","predictedValue")
		df_long <- rbind(df_long1,df_long2,df_long3,df_long4,df_long5)
		lldf<-dim(df_long)[1]
        tmpP<-data.frame(module=rep("qaPed",lldf),analysisId=rep(Idstatus,lldf), pipeline=rep("unknown",lldf), trait=df_long[,5], gid=rep("unknown",lldf), designation=df_long[,1], mother=df_long[,2], father=df_long[,3],
		effectType=rep("unknown",lldf), entryType=df_long[,4], environment=rep("general",lldf), predictedValue=df_long[,6], stdError=rep(NA,lldf), reliability=rep(NA,lldf))
		
		
		if(!is.null(result$predictions)){
          result$predictions <- rbind(result$predictions, tmpP)
        } else {
          result$predictions <- tmpP
        }
		
		geno <- result$data$geno
		vars <- apply(as.matrix(geno), 2, var, na.rm = TRUE)        
		newmetrics <- data.frame(module=rep("qaPed",6), analysisId=rep(Idstatus,6), trait=rep("none",6),environment=rep("general",6),parameter=c("nMarkers","nInds","monomorphicMarkersN","polymorphicMarkersN","indivMatchedN","indivUnmatchedN"),
		method=rep("sum",6),value=c(dim(geno)[2],dim(decisionped)[1],round(sum(vars == 0)),dim(geno)[2]-round(sum(vars == 0)),length(which(decisionped[,15]=="PASS")),(dim(decisionped)[1]-length(which(decisionped[,15]=="PASS")))),stdError=rep(NA,6) )
		save(newmetrics,file="ver.RData")
        if(!is.null(result$metrics)){
          result$metrics <- rbind(result$metrics, newmetrics)
        }else{result$metrics <- newmetrics}
		
		
        #if("analysisIdName" %in% colnames(result$status) ){result$status$analysisIdName[nrow(result$status)] <- input$analysisIdName}
        data(result) # update data with results
        save(result,file="res.RData")
        cat(paste("Pedigree QA/QC step with id:",as.POSIXct(result$status$analysisId[length(result$status$analysisId)], origin="1970-01-01", tz="GMT"),"saved. Please proceed to construct a selection index using this time stamp."))
        updateTabsetPanel(session, "tabsMain", selected = "outputTabs")
      }else{
        cat(paste("Analysis failed"))
      }
    
      shinybusy::remove_modal_spinner()
      
      ## Report tab
      output$reportqaPed <- renderUI({
        data(result)
        HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportqaPed.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
      })
      
      output$downloadReportqaPed <- downloadHandler(
        filename = function() {
          paste(paste0('qaPed_dashboard_',gsub("-", "", Sys.Date())), sep = '.', switch(
            "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
          ))
        },
        content = function(file) {
          
          out <- report()
          
          file.rename(out, file)
        }
      )
      
      hideAll$clearAll <- FALSE
      
    }) ## end eventReactive
    
    output$outqaPed <- renderPrint({
      outqaPed1()
    })
      
  })  
  }
  
  
  ## To be copied in the UI
  # mod_qaPedApp_ui("qaPedApp_1")
  
  ## To be copied in the server
  # mod_qaPedApp_server("qaPedApp_1")    

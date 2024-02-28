#' traitTransformApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_traitTransformApp_ui <- function(id){
  ns <- NS(id)
  tagList(


    # shiny::sidebarPanel(  style = "height:690px; overflow-y: scroll;overflow-x: scroll;",
    #                       width = 3,
    #                       tags$style(".well {background-color:grey; color: #FFFFFF;}"),
    #                       HTML("<img src='www/cgiar3.png' width='42' vspace='10' hspace='10' height='46' align='top'>
    #               <font size='5'>Single-Cross Marker Building</font>"),
    #                       hr(style = "border-top: 1px solid #4c4c4c;"),
    #                       numericInput(ns("hybridBatch"), label = "Batch size to compute", value = 1000, min=1, max=10000, step=1000),
    #                       selectInput(ns("checkboxAllHybrids"), label = "Compute all possible hybrids?", choices = list(TRUE,FALSE), selected = FALSE, multiple=FALSE),
    #                       hr(style = "border-top: 1px solid #4c4c4c;"),
    #                       actionButton(ns("runScr"), "Build matrix", icon = icon("play-circle")),
    #                       hr(style = "border-top: 1px solid #4c4c4c;"),
    #                       textOutput(ns("outScr"))
    # ), # end sidebarpanel
    shiny::mainPanel(width = 9,
                     tabsetPanel( #width=9,
                       type = "tabs",

                       tabPanel(p("Information",class="info-p"), icon = icon("book"),
                                br(),
                                shinydashboard::box(status="success",width = 12,
                                                    solidHeader = TRUE,
                                                    column(width=12,   style = "height:580px; overflow-y: scroll;overflow-x: scroll;",
                                                           h2(strong("Status:")),
                                                           uiOutput(ns("warningMessage")),
                                                           tags$body(
                                                             h2(strong("Details")),
                                                             p("Some trait transformations are required when data starts to grow or presents modeling challenges. The current
                                                               implementations include:"),
                                                             # img(src = "www/qaRaw.png", height = 300, width = 600), # add an image
                                                             p(strong("Conversion-")," The trait is transformed by a function such as cubic root, square root, log., etc.."),
                                                             p(strong("Equalizing-")," a set of traits with different name are considered to be the same trait and the user needs to equalize them ."),
                                                             p(strong("Balancing-")," a trait with numerical values in different units depending on the environment needs to be converted to same units across environments ."),
                                                             h2(strong("References")),
                                                             h2(strong("Software")),
                                                             p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
                                Vienna, Austria. URL https://www.R-project.org/."),
                                                           )
                                                    )
                                )
                       ),
                       tabPanel(p("Conversion", class="output-p"), icon = icon("arrow-right-from-bracket"),
                                tabsetPanel(
                                  tabPanel("Input", icon = icon("magnifying-glass-chart"),
                                           br(),
                                           shinydashboard::box(status="success",width = 12, #background = "green", solidHeader = TRUE,
                                                               column(width=12,DT::DTOutput(ns("summariesScr1")),style = "height:530px; overflow-y: scroll;overflow-x: scroll;")
                                           )
                                  ),
                                ) # end of tabset
                       ), # end of output panel
                       tabPanel(p("Equalizing", class="output-p"), icon = icon("arrow-right-from-bracket"),
                                tabsetPanel(
                                  tabPanel("Input", icon = icon("magnifying-glass-chart"),
                                           br(),
                                           shinydashboard::box(status="success",width = 12, #background = "green", solidHeader = TRUE,
                                                               column(width=12,DT::DTOutput(ns("summariesScr2")),style = "height:530px; overflow-y: scroll;overflow-x: scroll;")
                                           )
                                  ),
                                ) # end of tabset
                       ), # end of output panel
                       tabPanel(p("Balancing", class="output-p"), icon = icon("arrow-right-from-bracket"),
                                tabsetPanel(
                                  tabPanel("Input", icon = icon("magnifying-glass-chart"),
                                           br(),
                                           shinydashboard::box(status="success",width = 12, #background = "green", solidHeader = TRUE,
                                                               column(width=12,DT::DTOutput(ns("summariesScr3")),style = "height:530px; overflow-y: scroll;overflow-x: scroll;")
                                           )
                                  ),
                                ) # end of tabset
                       ), # end of output panel

                     )) # end mainpanel


  )
}

#' traitTransformApp Server Functions
#'
#' @noRd
mod_traitTransformApp_server <- function(id, data){
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
        ## pheno check
        if( length(which(c("designation") %in% data()$metadata$pheno$parameter)) == 0 ){
          HTML( as.character(div(style="color: red; font-size: 20px;", "Please map your 'designation' column using the 'Data Retrieval' tab in the 'Phenotype' section.")) )
        }else{
          ## ped check
          ped <- data()$data$pedigree
          metaPed <- data()$metadata$pedigree
          if(is.null(ped)){ # no pedigree available
            HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your pedigree data using the 'Data Retrieval' tab under 'Pedigree' section.")) )
          }else{ # pedigree is there
            if( length(intersect(metaPed$value , colnames(ped))) != 3){
              HTML( as.character(div(style="color: red; font-size: 20px;", "Please map your 'designation', 'mother', and 'father' columns when retrieving the pedigree data.")) )
            }else{
              colnames(ped) <- cgiarBase::replaceValues(colnames(ped), Search = metaPed$value, Replace = metaPed$parameter )
              parents <- na.omit(c(unique(ped[,"mother"]), unique(ped[,"father"]) ))
              if(length(parents) == 0){
                HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your pedigree data using the 'Data Retrieval' tab. No parents detected.")) )
              }else{
                ## marker check
                geno <- data()$data$geno
                if(is.null(geno)){ # no markers available
                  HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your marker data using the 'Data Retrieval' tab.")) )
                }else{ # markers are there
                  HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to inspect the options.")) )
                } # end of if pedigree available
              } #
            }
          } # end of if pedigree available
        }
      }
    )





  })
}

## To be copied in the UI
# mod_traitTransformApp_ui("traitTransformApp_1")

## To be copied in the server
# mod_traitTransformApp_server("traitTransformApp_1")

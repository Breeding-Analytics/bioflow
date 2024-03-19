#' oftStaApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_oftStaApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    mainPanel(width = 12,
              tabsetPanel( id=ns("tabsMain"),
                           type = "tabs",
                           tabPanel(div(icon("book"), "Information-OFT") ,
                                    br(),
                                    shinydashboard::box(status="success",width = 12,
                                                        solidHeader = TRUE,
                                                        column(width=12,   style = "height:580px; overflow-y: scroll;overflow-x: scroll;",
                                                               h1(strong(span("On Farm Trial Analysis", style="color:green"))),
                                                               h2(strong("Status:")),
                                                               uiOutput(ns("warningMessage")),
                                                               h2(strong("Details")),
                                                               p("********. The way the arguments are used is the following:"),
                                                               # img(src = "www/mta.png", height = 300, width = 600), # add an image
                                                               # p(strong("Traits to analyze.-")," Traits to be analyzed. If no design factors can be fitted simple means are taken."),
                                                               # h2(strong("References:")),
                                                               # p("Finlay, K. W., & Wilkinson, G. N. (1963). The analysis of adaptation in a plant-breeding programme. Australian journal of agricultural research, 14(6), 742-754."),
                                                               # h2(strong("Software used:")),
                                                               # p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
                                                               # Vienna, Austria. URL https://www.R-project.org/."),
                                                        )
                                    )
                           ),
                           tabPanel(div(icon("arrow-right-to-bracket"), "Input"),
                                    tabsetPanel(
                                      tabPanel("Select traits", icon = icon("table"),
                                               br(),
                                               column(width=12, selectInput(ns("trait2Oft"), "Trait(s) to include", choices = NULL, multiple = TRUE), style = "background-color:grey; color: #FFFFFF")
                                      ),

                                      tabPanel("Select year of origin & entry type", icon = icon("table"),
                                               br(),
                                               column(width=12, style = "background-color:grey; color: #FFFFFF",
                                                      column(width=6, selectInput(ns("yearsToUse"), "Year of origin", choices = NULL, multiple = TRUE) ),
                                                      column(width=6, selectInput(ns("entryTypeToUse"), "Entry type", choices = NULL, multiple = TRUE) ),
                                               )
                                      ),
                                      tabPanel("Select Environments", icon = icon("table"),
                                               br(),
                                               column(width=12, selectInput(ns("env2Oft"), "Environment(s) to include", choices = NULL, multiple = TRUE), style = "background-color:grey; color: #FFFFFF")
                                      ),
                                      tabPanel("Generate report", icon = icon("play"),
                                               br(),
                                               actionButton(ns("runOft"), "Generate OFT Report", icon = icon("play-circle")),
                                               uiOutput(ns("outOft"))
                                      ),
                                    )
                           ),
                           tabPanel(div(icon("arrow-right-from-bracket"), "Output" ) , value = "outputTabs",
                                    tabsetPanel(
                                      tabPanel("Report", icon = icon("file-image"),
                                               br(),
                                               div(tags$p("Please download the report below:") ),
                                               downloadButton(ns("downloadReportOft"), "Download report"),
                                               br(),
                                               uiOutput(ns('reportOft'))
                                      )
                                    ) # end of tabset
                           )# end of output panel
              )) # end mainpanel

  )
}

#' oftStaApp Server Functions
#'
#' @noRd
mod_oftStaApp_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })
    ############################################################################
    # warning message OFT
    output$warningMessage <- renderUI(
      if(is.null(data())){
        HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your phenotypic data using the 'Data Retrieval' tab.")) )
      }else{ # data is there
        mappedColumns <- length(which(c("environment","designation","trait","entryType") %in% data()$metadata$pheno$parameter))
        if(mappedColumns == 4){
          if("sta" %in% data()$status$module){
            mappedColName <- data()$metadata$pedigree[data()$metadata$pedigree$parameter=="yearOfOrigin","value"]
            mappedColumns <- length(setdiff(unique(eval(parse(text=paste0("data()$data$pedigree$",mappedColName)))),NA))
            if(mappedColumns > 0){
              HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to generation of OFT Report.")) )
            } else{
              HTML( as.character(div(style="color: red; font-size: 20px;", "To generate OFT Report, please make sure that the column: 'yearOfOrigin' has been mapped in the pedigree data using the 'Data Retrieval' tab.")) )
            }
          }else{ HTML( as.character(div(style="color: red; font-size: 20px;", "Please perform the single trial analysis before generating OFT report.")) ) }
        }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please make sure that you have computed the 'environment' column, and that column 'designation', 'entryType' and \n at least one trait have been mapped using the 'Data Retrieval' tab.")) )}
      }
    )

    ## traits
    observeEvent(data(), {
      req(data())
      dtOft <- data()
      dtOft <- dtOft$metadata$pheno
      traitsOft <- paste0(unique(dtOft[dtOft$parameter=="trait","value"]),"-residual")
      traitsOft <- intersect(colnames(data()$data$pheno),traitsOft)
      traitsOft <- gsub("-residual", "", traitsOft)
      updateSelectInput(session, "trait2Oft",choices = traitsOft)
    })
    ## years
    observeEvent(data(), {
      req(data())
      req(input$trait2Oft)
      dtOft <- data()
      traitsOft <- dtOft$metadata$pedigree[dtOft$metadata$pedigree$parameter=="yearOfOrigin","value"]
      if(!is.null(traitsOft)){
        if(length(setdiff(unique(eval(parse(text=paste0("dtOft$data$pedigree$",traitsOft)))),NA))>0){
          updateSelectInput(session, "yearsToUse", choices = traitsOft, selected =traitsOft )
        }
      }
    })
    ## entry type
    observeEvent(data(), {
      req(data())
      req(input$trait2Oft)
      req(input$yearsToUse)
      dtOft <- data()
      traitsOft <- dtOft$metadata$pheno[dtOft$metadata$pheno$parameter=="entryType","value"]
      updateSelectInput(session, "entryTypeToUse", choices = traitsOft, selected =traitsOft )
    })
    ## environment
    observeEvent(data(), {
      req(data())
      req(input$trait2Oft)
      req(input$yearsToUse)
      req(entryTypeToUse)
      dtOft <- data()
      traitsOft <- dtOft$metadata$pheno[dtOft$metadata$pheno$parameter=="environment","value"]
      updateSelectInput(session, "env2Oft", choices = traitsOft, selected =traitsOft )
    })

    output$downloadReportOft <- downloadHandler(
      filename = function() {
        paste('my-report-OFT', sep = '.', switch(
          "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
        ))
      },
      content = function(file) {
        shinybusy::show_modal_spinner(spin = "fading-circle",
                                      text = "Generating Report...")

        src <- normalizePath(system.file("rmd","reportOft.Rmd",package="bioflow"))
        # src2 <- normalizePath('data/resultSta.RData')

        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'report2.Rmd', overwrite = TRUE)
        # file.copy(src2, 'resultSta.RData', overwrite = TRUE)
        out <- rmarkdown::render('report2.Rmd', params = list(fieldinst=input$env2Oft, toDownload=TRUE),switch(
          "HTML",
          HTML = rmarkdown::html_document()
        ))
        file.rename(out, file)
        shinybusy::remove_modal_spinner()
      }
    )

  })
}

## To be copied in the UI
# mod_oftStaApp_ui("oftStaApp_1")

## To be copied in the server
# mod_oftStaApp_server("oftStaApp_1")

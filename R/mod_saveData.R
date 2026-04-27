#' saveData UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_saveData_ui <- function(id){
  ns <- NS(id)
  tagList(

    tags$br(),

    navlistPanel( widths = c(2, 10),
                  tabPanel(div("1. Select storing place" ),
                           column(width = 8,
                                  tags$br(),

                                  selectInput(
                                    inputId = ns('object_output'),
                                    label   = 'Storing place*: ',
                                    choices = list('Computer' = 'pcfile', 'Cloud' = 'cloudfile'),
                                    width   = '200px'
                                  ),

                                  DT::DTOutput(ns("statusTable")),
                                  tags$br(),
                                  DT::DTOutput(ns('summary_data'))
                           ),
                           column(width = 4,
                                  tags$br(),
                                  shinydashboard::box(width = 12, title = span(icon('youtube'), ' Tutorial'), status = "success", solidHeader=FALSE, style = "color: #000000", collapsible = TRUE, collapsed = TRUE,
                                                      h4(strong(span("", tags$a(href="https://www.youtube.com/watch?v=B3JaVw36dvI&list=PLZ0lafzH_UmclOPifjCntlMzysEB2_2wX&index=3", icon("youtube") , target="_blank"), style="color:darkcyan"))),
                                  ),
                           )
                  ),
                  tabPanel(div("2. Assign name" ),
                           tags$br(),
                           textInput(ns("fileNameUpload"), label = "Name assigned to the analysis object", placeholder = "Enter name..."),
                  ),
                  tabPanel(div("3. Save object" ),
                           tags$br(),
                           tags$span(id = ns('pcfile_holder'),
                                     downloadButton(ns("downloadRds"), "Save object", icon = icon("floppy-disk")),
                                     textOutput(ns("outSave2")),
                           ),
                           ## provisionally silenced until Khaled enables the S3 saving using the users table
                           tags$span(id = ns('cloudfile_holder'),
                                     actionButton(ns("runSave"), "Save object", icon = icon("floppy-disk")),
                                     textOutput(ns("outSave")),
                           ),
                  ),
    ),

  )
}

#' saveData Server Functions
#'
#' @noRd
mod_saveData_server <- function(id, data, res_auth=NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    observeEvent(
      input$object_output,
      if(length(input$object_output) > 0){ # added
        if (input$object_output == 'pcfile') {
          golem::invoke_js('showid', ns('pcfile_holder'))
          golem::invoke_js('hideid', ns('cloudfile_holder'))
        } else if (input$object_output == 'cloudfile') {
          golem::invoke_js('hideid', ns('pcfile_holder'))
          golem::invoke_js('showid', ns('cloudfile_holder'))
        }
      }
    )

    output$outSave <- renderUI({}) # this 2 lines avoid issues when displaying an output nested
    outputOptions(output, "outSave", suspendWhenHidden = FALSE)
    observeEvent( # this is the part where we either load the previous analysis from cloud or PC
      c(input$object_output),
      {
        if(input$object_output == 'cloudfile'){ # upload from cloud
          outSave <- eventReactive(input$runSave, {
            req(data())
            req(input$fileNameUpload)

            is_local     <- Sys.getenv("SHINY_PORT") == ""
            cloud_domain <- session$clientData$url_hostname

            if(is_local){
              shinyalert::shinyalert(title = "Cloud Save Unavailable!", text = "Cloud save is not available in the offline version of Bioflow. Please use the production or testing server to access this feature.", type = "warning")
              return(cat("Offline version detected. Cloud save is unavailable."))
            }

            result <- data()

            ### set up paws library ################################################
            # to access AWS using a pre-configured SSO (Single Sign-On) profile (switch to paws R package?)
            Sys.setenv("AWS_PROFILE" = "bioflow")
            Sys.setenv("AWS_DEFAULT_REGION" = "ap-southeast-1")

            bucket_name <- "ebs-bioflow"

            valid_domains <- c("bioflow.ebsproject.org", "bioflow-prd.ebsproject.org", "bioflow-test.ebsproject.org")

            if (cloud_domain %in% valid_domains) {
              if (cloud_domain == "bioflow-test.ebsproject.org") {
                data_domain <- "bioflow-test"
              } else {
                data_domain <- result$user
              }
            } else {
              shinyalert::shinyalert(title = "Cloud Service Restricted!", text = "This feature works only on the official Bioflow cloud servers (production or testing). The current hosting server does not support it.", type = "info")
              return(cat("Invalid hosting server. Cloud save is unavailable."))
            }

            s3_object_path <- paste0(data_domain, "/", input$fileNameUpload, ".RData")

            s3 <- paws::s3()

            shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')

            tryCatch({
              temp_file <- paste0(tempdir(), "/", input$fileNameUpload, ".RData")
              save(result, file = temp_file)

              # upload data object to S3 bucket
              s3$put_object(
                Body = temp_file,
                Bucket = bucket_name,
                Key = s3_object_path
              )

              shinybusy::remove_modal_spinner()
              shinyalert::shinyalert(title = "Success!", text = paste("Analysis named: '",input$fileNameUpload,"' saved successfully on the cloud."), type = "success")
            }, error = function(e) {
              shinybusy::remove_modal_spinner()
              shinyalert::shinyalert(title = "Failed!", text = e$message, type = "error")
            })

            # save(result, file=file.path(getwd(),res_auth$repository, paste0(input$fileNameUpload,".RData")))
            return(cat(paste("Saved successfully on", s3_object_path)))
          })

          output$outSave <- renderPrint({
            outSave()
          })

        }else if(input$object_output == 'pcfile'){ # upload rds

          output$downloadRds <- downloadHandler(
            filename <- function(){
              return(file.path(paste0(input$fileNameUpload,".RData")))
            },

            content = function(file) {
              result= data()
              save(result, file = file)
            }
          )

        }else{

        }
      }
    )

    output$statusTable <-  DT::renderDT({
      req(data())
      dtMta <- data()
      if(!is.null(dtMta$status)){
        status <- dtMta$status; status$analysisId <- as.POSIXct(status$analysisId, origin="1970-01-01", tz="GMT")
        DT::datatable(status, extensions = 'Buttons',
                      options = list(dom = 'Blfrtip',buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                     lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All'))),
                      caption = htmltools::tags$caption(
                        style = 'color:cadetblue', #caption-side: bottom; text-align: center;
                        htmltools::em('Preview of analysis available in the current data object')
                      )
        )
      }
    }, server = FALSE)

    ## render data summary
    output$summary_data <- DT::renderDT({
      req(data())
      DT::datatable(cgiarBase::summaryData(data()),
                    extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All'))),
                    caption = htmltools::tags$caption(
                      style = 'color:cadetblue', #caption-side: bottom; text-align: center;
                      htmltools::em('Preview of summary data available in the current data object')
                    )
      )

    }, server = FALSE)

  })
}

## To be copied in the UI
# mod_saveData_ui("saveData_1")

## To be copied in the server
# mod_saveData_server("saveData_1")

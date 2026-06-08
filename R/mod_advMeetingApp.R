#' Advancement Meeting Dashboard UI Function
#'
#' @description A shiny Module for multi-stakeholder advancement meeting decisions.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_advMeetingApp_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::mainPanel(width = 12,
                     tabsetPanel( id=ns("tabsMain"),
                                  type = "tabs",

                                  tabPanel(div(icon("book"), "Information") ,
                                           br(),
                                           column(width = 6,
                                                  h1(strong(span("Advancement Meeting Dashboard", style="color:darkcyan"))),
                                                  h2(strong("Status:")),
                                                  uiOutput(ns("warningMessage")),
                                                  tags$br(),
                                           ),
                                           column(width = 6,
                                                  tags$body(
                                                    h2(strong("Details")),
                                                    p("The Advancement Meeting Dashboard enables multi-stakeholder consensus on product advancement decisions."),
                                                    p(strong("Workflow:")),
                                                    tags$ol(
                                                      tags$li("Each stakeholder runs the Pre-advancement module individually and saves their selection as an RData file."),
                                                      tags$li("During the meeting, all RData files are loaded here."),
                                                      tags$li("The module compares selection decisions, highlights controversial candidates, and supports joint decision-making."),
                                                      tags$li("A final meeting report is generated with consensus decisions.")
                                                    ),
                                                    p(strong("Note:"), " This module requires that all stakeholders have completed the Pre-advancement workflow on the same underlying MTA analysis."),
                                                  )
                                           ),
                                  ),

                                  tabPanel(div(icon("upload"), "Load Stakeholder Files"),
                                           br(),
                                           column(width = 12,
                                                  p(style = "color:orange; font-size:16px;",
                                                    icon("triangle-exclamation"),
                                                    " This module is under development. Full functionality will be available in a future release."
                                                  ),
                                           ),
                                  ),

                                  tabPanel(div(icon("people-group"), "Consensus Review"),
                                           br(),
                                           column(width = 12,
                                                  p(style = "color:orange; font-size:16px;",
                                                    icon("triangle-exclamation"),
                                                    " This module is under development. Full functionality will be available in a future release."
                                                  ),
                                           ),
                                  ),

                                  tabPanel(div(icon("file-lines"), "Meeting Report"),
                                           br(),
                                           column(width = 12,
                                                  p(style = "color:orange; font-size:16px;",
                                                    icon("triangle-exclamation"),
                                                    " This module is under development. Full functionality will be available in a future release."
                                                  ),
                                           ),
                                  ),

                     ) # end tabsetPanel
    ) # end mainPanel

  )
}

#' Advancement Meeting Dashboard Server Functions
#'
#' @noRd
mod_advMeetingApp_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Warning message
    output$warningMessage <- renderUI({
      HTML(as.character(div(style="color: orange; font-size: 20px;",
                            "This module is under development. Please complete the Pre-advancement workflow first.")))
    })

  })
}

## To be copied in the UI
# mod_advMeetingApp_ui("advMeetingApp_1")

## To be copied in the server
# mod_advMeetingApp_server("advMeetingApp_1")

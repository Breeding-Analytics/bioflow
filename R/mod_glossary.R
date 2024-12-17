#' glossary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_glossary_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidPage(
      downloadButton(ns("downloadGlossary"), "Download Glossary"),
      uiOutput(ns('glossary'))
    )

  )
}

#' glossary Server Functions
#'
#' @noRd
mod_glossary_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$glossary <- renderUI({
      HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","glossary.Rmd",package="bioflow"),
                                                quiet = TRUE), fragment.only=TRUE))
    })

    output$downloadGlossary <- downloadHandler(
      filename = function() {
        paste(paste0('glossary_bioflow_',gsub("-", "", Sys.Date())), sep = '.', switch(
          "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
        ))
      },
      content = function(file) {
        shinybusy::show_modal_spinner(spin = "fading-circle",
                                      text = "Downloading...")
        src <- normalizePath(system.file("rmd","glossary.Rmd",package="bioflow"))
        # src2 <- normalizePath('data/resultSta.RData')
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'report.Rmd', overwrite = TRUE)
        # file.copy(src2, 'resultSta.RData', overwrite = TRUE)
        out <- rmarkdown::render('report.Rmd', params = list(toDownload=TRUE),switch(
          "HTML",
          # HTML = rmarkdown::html_document()
          HTML = rmdformats::robobook(toc_depth = 4)
        ))
        file.rename(out, file)
        shinybusy::remove_modal_spinner()
      }
    )

  })
}

## To be copied in the UI
# mod_glossary_ui("glossary_1")

## To be copied in the server
# mod_glossary_server("glossary_1")

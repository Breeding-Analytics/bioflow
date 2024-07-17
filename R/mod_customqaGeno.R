mod_customqaGenoApp_ui <- function(id) {
  ns <- NS(id)
  tagList(shiny::mainPanel(
    width = 12,
    tabsetPanel(
      id = ns("tabsMain"),
      type = "tabs",
      tabPanel(
        "Information-QA-Geno",
        icon = icon("book"),
        br(),
        tags$body(uiOutput(ns("warningMessage")), )
      ),
      tabPanel("Input",
               icon = icon("book"),
               br(),
               tags$body(
                 tabsetPanel(
                   id = "QA_panel",
                   type = "tabs",
                   tabPanel(
                     "Filtering",
                     icon = icon("magnifying-glass-chart"),
                     tags$body(fluidRow(
                       column(
                         width = 4,
                         titlePanel("Filtering Parameters"),
                         h4("By locus"),
                         uiOutput(ns("by_locus_filter_params")),

                       ),
                       column(width = 4,
                              h4("By individual"),
                              uiOutput(ns("by_ind_filter_params")),
                              actionButton(inputId = ns('filter_btn'), "Filter")
                              ),
                       column(width = 4,
                              titlePanel("Summary"),
                                uiOutput(ns("genoSummaryStats"))
                              ),
                     ))
                   ),
                   tabPanel("Run Analysis", icon = icon("play"), ),
                 )
               )),
      tabPanel("Output",
               icon = icon("book"),
               br(),
               tags$body(), )
    ),
  ))
}

mod_customqaGenoApp_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })
    ############################################################################

    # warning message
    output$warningMessage <- renderUI(if (is.null(data())) {
      HTML(as.character(
        div(
          style = "color: red; font-size: 20px;",
          "Please retrieve or load your data using the 'Data' tab."
        )
      ))
    } else{
      # data is there
      if (!is.null(data()$data$gl)) {
        HTML(as.character(
          div(
            style = "color: green; font-size: 20px;",
            "Data is complete, please proceed to perform the marker QA specifying your input parameters under the Input tabs."
          )
        ))
      } else{
        HTML(as.character(
          div(
            style = "color: red; font-size: 20px;",
            "Please retrieve or load your genotype data using the 'Data' tab. "
          )
        ))
      }
    })


    get_geno_stats <- reactive({
      req(data()$data$gl)
      gl <- data()$data$gl
      # Get loc missing stats
      loc_miss <- cgiarGenomics::get_loc_missing(gl)
      # Get ind missing stats
      ind_miss <- cgiarGenomics::get_ind_missing(gl)
      # Get overall missing stats
      ov_miss <- cgiarGenomics::get_overall_missingness(gl)
      # Heterozygosis metrics
      het <- cgiarGenomics::get_heterozygosity_metrics(gl, ploidy = data()$data$ploidity)
      loc_het <- het$het_loc
      ind_het <-het$het_ind
      # Minor allele frequency
      maf <- cgiarGenomics::get_maf(gl)

      return(list(
        loc_miss = loc_miss,
        ind_miss = ind_miss,
        ov_miss = ov_miss,
        loc_het = loc_het,
        ind_het = ind_het,
        maf = maf
      ))

    })




    output$genoSummaryStats <- renderUI({
      req(get_geno_stats())
      geno_stats <- get_geno_stats()


      loc_miss <- format_percentage(mean(geno_stats$loc_miss))
      ov_miss <- format_percentage(geno_stats$ov_miss)
      ind_miss <- format_percentage(mean(geno_stats$ind_miss))
      loc_het <- format_percentage(mean(geno_stats$loc_het))
      ind_het <- format_percentage(mean(geno_stats$ind_het))
      maf <- format_percentage(mean(geno_stats$maf))


      tags$table(border = 2, width = "100%",
        tags$tr(
          tags$th(colspan = 3,align = "center", "Missingness")
        ),
        tags$tr(
          tags$th(align = "center", "Overall"),
          tags$th(align = "center", "Locus"),
          tags$th(align = "center", "Individual"),
        ),
        tags$tr(
          tags$td(ov_miss),
          tags$td(loc_miss),
          tags$td(ind_miss)
        ),
        tags$tr(
          tags$th(colspan = 2,align = "center", "Heterozygosis"),
          tags$th(colspan = 1,align = "center", "MAF")
        ),
        tags$tr(
          tags$th(align = "center","Locus"),
          tags$th(align = "center","Individual"),
          tags$th(align = "center","Mean"),
        ),
        tags$tr(
          tags$td(loc_het),
          tags$td(ind_het),
          tags$td(maf)
        ),
      )

    })


    output$by_ind_filter_params <- renderUI({

      geno_stats <- get_geno_stats()


      tags$span(
        sliderInput(ns("ind_missing"),
                    " Minimum percentage of genotyped loci by individual:",
                    round(min(geno_stats$ind_miss),2),
                    round(max(geno_stats$ind_miss),2),
                    step = 0.01,
                    value = round(min(geno_stats$ind_miss), 2)),
        sliderInput(ns("ind_he"),
                    " Minimum percentage of heterozygosity by individual:",
                    round(min(geno_stats$ind_het), 2),
                    round(max(geno_stats$ind_het),2),
                    step = 0.01,
                    value = round(min(geno_stats$ind_het), 2))
      )
    })

    output$by_locus_filter_params <- renderUI({
      gl <- data()$data$gl
      geno_stats <- get_geno_stats()

      tags$span(
        sliderInput(ns("loc_missing"),
                    " Minimum number of genotyped individuals by marker:",
                    round(min(geno_stats$loc_miss)*adegenet::nInd(gl)),
                    round(max(geno_stats$loc_miss)*adegenet::nInd(gl)),
                    step = 1,
                    value = round(min(geno_stats$loc_miss)*adegenet::nInd(gl))),
        sliderInput(ns("loc_he"),
                    " Minimum heterozygosity by locus:",
                    round(min(geno_stats$loc_het),2),
                    round(max(geno_stats$loc_het),2),
                    step = 0.01,
                    value = round(min(geno_stats$loc_het), 2)),
        sliderInput(ns("maf"),
                    " Minimum minor allele frequency (MAF):",
                    min = 0,
                    max = round(max(geno_stats$maf),2),
                    step = 0.01,
                    value = 0)
        # ADD multiple select to remove target locus
      )
    })



  })
}

format_percentage <- function(percentage){
  return (paste0(round(percentage*100, 2), "%"))
}

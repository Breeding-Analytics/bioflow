mod_customqaGenoApp_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::mainPanel(
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
                 titlePanel("Filtering Stages"),
                 tags$body(
                   fluidRow(
                     column(width = 8,
                            tabsetPanel(id = ns("filter_panel"),
                                        type = "tabs",
                                        tabPanel("Ind. Miss",
                                                 uiOutput(ns("ind_miss_filter_params")),
                                                 plotly::plotlyOutput(ns("hist_ind_miss")),
                                                 actionButton(inputId = ns('ind_miss_btn'), "Filter"),
                                                 actionButton(inputId = ns('ind_miss_next_btn'), "Next"),
                                                 ),
                                        tabPanel("Loc. Miss",
                                                 uiOutput(ns("loc_miss_filter_params")),
                                                 plotly::plotlyOutput(ns("hist_loc_miss")),
                                                 actionButton(inputId = ns('loc_miss_btn'), "Filter"),
                                                 actionButton(inputId = ns('loc_miss_next_btn'), "Next"),
                                                 ),
                                        tabPanel("MAF",
                                                 uiOutput(ns("maf_filter_params")),
                                                 plotly::plotlyOutput(ns("hist_maf")),
                                                 actionButton(inputId = ns('maf_btn'), "Filter"),
                                                 actionButton(inputId = ns('maf_next_btn'), "Next"),
                                                 ),
                                        tabPanel("Loc. Het",
                                                 uiOutput(ns("loc_het_filter_params")),
                                                 plotly::plotlyOutput(ns("hist_loc_het")),
                                                 actionButton(inputId = ns('loc_het_btn'), "Filter"),
                                                 actionButton(inputId = ns('loc_het_next_btn'), "Next"),
                                                 ),
                                        tabPanel("Ind. Het",
                                                 uiOutput(ns("ind_het_filter_params")),
                                                 plotly::plotlyOutput(ns("hist_ind_het")),
                                                 actionButton(inputId = ns('ind_het_btn'), "Filter"),
                                                 actionButton(inputId = ns('loc_het_end_btn'), "Finish"),
                                                 ),
                              )
                            ),
                     column(width = 4,
                            tags$h2("Genotype data Summary"),
                            tableOutput(ns("genoSummaryStats"))
                            ),
                   )
                 ),
                 ),
        tabPanel("Output",
                 icon = icon("book"),
                 br(),
                 tags$body(
                   tags$h2("Genotype Filtering Summary"),
                   tableOutput(ns("genoSummaryStats2")),
                   tags$h2("Filtering Parameters Selected"),
                   tableOutput(ns("filter_params_status")),
                 )
                 )
      )
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



    get_geno <- reactive({
      req(data()$data$gl)
      gl <- data()$data$gl
      return(gl)
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

    # Slider for missingness by individual
    output$ind_miss_filter_params <- renderUI({
      req(get_geno())
      gl<-get_geno()

      tags$span(
        sliderInput(ns("ind_missing_slider"),
                    "Minimum percentage of genotyped loci by individual:",
                    0,
                    round(max(gl@other$ind.metrics$ind_miss)*1.05, 4),
                    step = 0.0001,
                    value = round(max(gl@other$ind.metrics$ind_miss), 4)),
        textInput(
          inputId = ns("ind_missing_textbox"),
          placeholder = "Input the numeric theshold",
          label = NULL
        ),
        actionButton(
          inputId = ns("ind_missing_update_btn"),
          "Update value"
        )
      )
    })

    observeEvent(input$ind_missing_update_btn, {
        updateSliderInput(
          session = session,
          inputId = "ind_missing_slider",
          value = as.numeric(input$ind_missing_textbox))

    })

    # Histogram for missingness by individual
    output$hist_ind_miss <- plotly::renderPlotly({

      if (input$ind_miss_btn == 0){
        gl <- get_geno()
        baseline_stats <- cgiarGenomics::get_overall_summary(gl)
        summary_table <- overall_summary()
        summary_table['Baseline', 'Nind'] <- baseline_stats[['nind']]
        summary_table['Baseline', 'Nloc'] <- baseline_stats[['nloc']]
        summary_table['Baseline', 'Ov_miss'] <- format_percentage(baseline_stats[['ov_miss']])
        summary_table['Baseline', 'Ov_Het'] <- format_percentage(baseline_stats[['ov_het']])
        summary_table['Baseline', 'MAF'] <- format_percentage(baseline_stats[['ov_maf']])

        overall_summary(summary_table)

      } else{
        gl <- ind_miss_filtered()
      }

      fig <- plotly::plot_ly(alpha = 0.6)
      fig <- fig %>% plotly::add_histogram(x = gl@other$ind.metrics$ind_miss,
                                           name="Missing data by indivudual" )

    })

    ind_miss_filtered <- eventReactive(
      input$ind_miss_btn,
      {gl<-get_geno()
      gl_filt <- cgiarGenomics::filter_missing_rate_by_indv(gl, input$ind_missing_slider)

      filter_stats <- cgiarGenomics::get_overall_summary(gl_filt)
      summary_table <- overall_summary()
      summary_table['Filtered', 'Nind'] <- filter_stats[['nind']]
      summary_table['Filtered', 'Nloc'] <- filter_stats[['nloc']]
      summary_table['Filtered', 'Ov_miss'] <- format_percentage(filter_stats[['ov_miss']])
      summary_table['Filtered', 'Ov_Het'] <- format_percentage(filter_stats[['ov_het']])
      summary_table['Filtered', 'MAF'] <- format_percentage(filter_stats[['ov_maf']])

      overall_summary(summary_table)

      return(gl_filt)
    })

    observeEvent(input$ind_miss_next_btn, {
      summary_table <- overall_summary()
      removed_nind <- as.numeric(summary_table['Baseline', 'Nind']) - as.numeric(summary_table['Filtered', 'Nind'])
      showModal(modalDialog(
        title = "Information",
        paste0("Were removed ", removed_nind, " Individuals"),
        easyClose = TRUE,
        footer = NULL
      ))
      updateTabsetPanel(session, "filter_panel", selected = "Loc. Miss")
    })



    # Slider for missingness by locus
    output$loc_miss_filter_params <- renderUI({
      req(ind_miss_filtered())
      gl<-ind_miss_filtered()

      tags$span(
        sliderInput(ns("loc_missing_slider"),
                    "Minimum missing rate by locus:",
                    0,
                    round(max(gl@other$loc.metrics$loc_miss)*1.05,4),
                    step = 0.0001,
                    value = round(max(gl@other$loc.metrics$loc_miss),4)),
        textInput(
          inputId = ns("loc_missing_textbox"),
          placeholder = "Input the numeric theshold",
          label = NULL
        ),
        actionButton(
          inputId = ns("loc_missing_update_btn"),
          "Update value"
        )
      )
    })

    observeEvent(input$loc_missing_update_btn, {
      updateSliderInput(
        session = session,
        inputId = "loc_missing_slider",
        value = as.numeric(input$loc_missing_textbox))

    })

    # Histogram for missingness by locus
    output$hist_loc_miss <- plotly::renderPlotly({

      if (input$loc_miss_btn == 0){
        gl <- ind_miss_filtered()
      } else{
        gl <- loc_miss_filtered()
      }

      fig <- plotly::plot_ly(alpha = 0.6)
      fig <- fig %>% plotly::add_histogram(x = gl@other$loc.metrics$loc_miss,
                                           name="Missing rate by locus" )

    })

    loc_miss_filtered <- eventReactive(
      input$loc_miss_btn,
      {gl<-ind_miss_filtered()
      gl_filt <- cgiarGenomics::filter_missing_rate_by_marker(gl, input$loc_missing_slider)

      filter_stats <- cgiarGenomics::get_overall_summary(gl_filt)
      summary_table <- overall_summary()
      summary_table['Filtered', 'Nind'] <- filter_stats[['nind']]
      summary_table['Filtered', 'Nloc'] <- filter_stats[['nloc']]
      summary_table['Filtered', 'Ov_miss'] <- format_percentage(filter_stats[['ov_miss']])
      summary_table['Filtered', 'Ov_Het'] <- format_percentage(filter_stats[['ov_het']])
      summary_table['Filtered', 'MAF'] <- format_percentage(filter_stats[['ov_maf']])

      overall_summary(summary_table)

      return(gl_filt)
      })

    observeEvent(input$loc_miss_next_btn, {
      summary_table <- overall_summary()
      removed_nloc <- as.numeric(summary_table['Baseline', 'Nloc']) - as.numeric(summary_table['Filtered', 'Nloc'])
      showModal(modalDialog(
        title = "Information",
        paste0("Were removed ", removed_nloc, " Loci"),
        easyClose = TRUE,
        footer = NULL
      ))
      updateTabsetPanel(session, "filter_panel", selected = "MAF")
    })

    # Slider for maf
    output$maf_filter_params <- renderUI({
      req(loc_miss_filtered())
      gl<-loc_miss_filtered()

      tags$span(
        sliderInput(ns("maf_slider"),
                    "Minimum minor allele frequency (MAF):",
                    0,
                    round(max(gl@other$loc.metrics$maf)*1.05,4),
                    step = 0.0001,
                    value = round(min(gl@other$loc.metrics$maf), 4)),
        textInput(
          inputId = ns("maf_textbox"),
          placeholder = "Input the numeric theshold",
          label = NULL
        ),
        actionButton(
          inputId = ns("maf_update_btn"),
          "Update value"
        )
      )
    })

    observeEvent(input$maf_update_btn, {
      updateSliderInput(
        session = session,
        inputId = "maf_slider",
        value = as.numeric(input$maf_textbox))

    })
    # Histogram for maf
    output$hist_maf <- plotly::renderPlotly({

      if (input$maf_btn == 0){
        gl <- loc_miss_filtered()
      } else{
        gl <- maf_filtered()
      }

      fig <- plotly::plot_ly(alpha = 0.6)
      fig <- fig %>% plotly::add_histogram(x = gl@other$loc.metrics$maf,
                                           name="Minor allele frequency" )

    })

    maf_filtered <- eventReactive(
      input$maf_btn,
      {gl<-loc_miss_filtered()
      gl_filt <- cgiarGenomics::filter_MAF(gl, input$maf_slider)

      filter_stats <- cgiarGenomics::get_overall_summary(gl_filt)
      summary_table <- overall_summary()
      summary_table['Filtered', 'Nind'] <- filter_stats[['nind']]
      summary_table['Filtered', 'Nloc'] <- filter_stats[['nloc']]
      summary_table['Filtered', 'Ov_miss'] <- format_percentage(filter_stats[['ov_miss']])
      summary_table['Filtered', 'Ov_Het'] <- format_percentage(filter_stats[['ov_het']])
      summary_table['Filtered', 'MAF'] <- format_percentage(filter_stats[['ov_maf']])

      overall_summary(summary_table)

      return(gl_filt)


      })

    observeEvent(input$maf_next_btn, {
      summary_table <- overall_summary()
      removed_nloc <- as.numeric(summary_table['Baseline', 'Nloc']) - as.numeric(summary_table['Filtered', 'Nloc'])
      showModal(modalDialog(
        title = "Information",
        paste0("Were removed ", removed_nloc, " Loci"),
        easyClose = TRUE,
        footer = NULL
      ))
      updateTabsetPanel(session, "filter_panel", selected = "Loc. Het")
    })

    # Slider for heterozygosis by locus
    output$loc_het_filter_params <- renderUI({
      req(maf_filtered())
      gl<-maf_filtered()

      tags$span(
        sliderInput(ns("loc_het_slider"),
                    "Maximum Heterozygosity by locus:",
                    0,
                    round(max(gl@other$loc.metrics$loc_het)*1.05,4),
                    step = 0.0001,
                    value = round(max(gl@other$loc.metrics$loc_het), 4)),
        textInput(
          inputId = ns("loc_het_textbox"),
          placeholder = "Input the numeric theshold",
          label = NULL
        ),
        actionButton(
          inputId = ns("loc_het_update_btn"),
          "Update value"
        )

      )
    })


    observeEvent(input$loc_het_update_btn, {
      updateSliderInput(
        session = session,
        inputId = "loc_het_slider",
        value = as.numeric(input$loc_het_textbox))
    })
    # Histogram for heterozygosis by locus
    output$hist_loc_het <- plotly::renderPlotly({
      if (input$loc_het_btn == 0){
        gl <- maf_filtered()
      } else{
        gl <- het_loc_filtered()
      }
      fig <- plotly::plot_ly(alpha = 0.6)
      fig <- fig %>% plotly::add_histogram(x = gl@other$loc.metrics$loc_het,
                                           name="Heterozygosity by locus" )

    })

    het_loc_filtered <- eventReactive(
      input$loc_het_btn,
      {gl<-maf_filtered()
      gl_filt <- cgiarGenomics::filter_heterozygosis_by_loc(gl, input$loc_het_slider)

      filter_stats <- cgiarGenomics::get_overall_summary(gl_filt)
      summary_table <- overall_summary()
      summary_table['Filtered', 'Nind'] <- filter_stats[['nind']]
      summary_table['Filtered', 'Nloc'] <- filter_stats[['nloc']]
      summary_table['Filtered', 'Ov_miss'] <- format_percentage(filter_stats[['ov_miss']])
      summary_table['Filtered', 'Ov_Het'] <- format_percentage(filter_stats[['ov_het']])
      summary_table['Filtered', 'MAF'] <- format_percentage(filter_stats[['ov_maf']])

      overall_summary(summary_table)
      return(gl_filt)
      })

    observeEvent(input$loc_het_next_btn, {
      summary_table <- overall_summary()
      removed_nloc <- as.numeric(summary_table['Baseline', 'Nloc']) - as.numeric(summary_table['Filtered', 'Nloc'])
      showModal(modalDialog(
        title = "Information",
        paste0("Were removed ", removed_nloc, " Loci"),
        easyClose = TRUE,
        footer = NULL
      ))
      updateTabsetPanel(session, "filter_panel", selected = "Ind. Het")
    })

    # Slider for heterozygosis by individual
    output$ind_het_filter_params <- renderUI({
      req(het_loc_filtered())
      gl<-het_loc_filtered()

      tags$span(
        sliderInput(ns("ind_het_slider"),
                    "Maximum Heterozygosity by individual:",
                    0,
                    round(max(gl@other$ind.metrics$ind_het),2),
                    step = 0.01,
                    value = max(gl@other$ind.metrics$ind_het)),
        textInput(
          inputId = ns("ind_het_textbox"),
          placeholder = "Input the numeric theshold",
          label = NULL
        ),
        actionButton(
          inputId = ns("ind_het_update_btn"),
          "Update value"
        )
      )
    })

    observeEvent(input$ind_het_update_btn, {
      updateSliderInput(
        session = session,
        inputId = "ind_het_slider",
        value = as.numeric(input$ind_het_textbox))
    })

    # Histogram for heterozygosis by individual
    output$hist_ind_het <- plotly::renderPlotly({
      if (input$ind_het_btn == 0){
        gl <- het_loc_filtered()
      } else{
        gl <- het_ind_filtered()
      }
      fig <- plotly::plot_ly(alpha = 0.6)
      fig <- fig %>% plotly::add_histogram(x = gl@other$ind.metrics$ind_het,
                                           name="Heterozygosity by individual" )
    })

    het_ind_filtered <- eventReactive(
      input$ind_het_btn,
      {gl<-het_loc_filtered()
      gl_filt <- cgiarGenomics::filter_heterozygosis_by_ind(gl, input$ind_het_slider)

      filter_stats <- cgiarGenomics::get_overall_summary(gl_filt)
      summary_table <- overall_summary()
      summary_table['Filtered', 'Nind'] <- filter_stats[['nind']]
      summary_table['Filtered', 'Nloc'] <- filter_stats[['nloc']]
      summary_table['Filtered', 'Ov_miss'] <- format_percentage(filter_stats[['ov_miss']])
      summary_table['Filtered', 'Ov_Het'] <- format_percentage(filter_stats[['ov_het']])
      summary_table['Filtered', 'MAF'] <- format_percentage(filter_stats[['ov_maf']])

      overall_summary(summary_table)
      return(gl_filt)
      })

    observeEvent(input$loc_het_end_btn, {
      summary_table <- overall_summary()
      removed_nind <- as.numeric(summary_table['Baseline', 'Nind']) - as.numeric(summary_table['Filtered', 'Nind'])
      showModal(modalDialog(
        title = "Information",
        paste0("Were removed ", removed_nind, " Individuals"),
        easyClose = TRUE,
        footer = NULL
      ))


      param_table <- params()
      param_table['Param', 'Ind_miss'] <- input$ind_missing_slider
      param_table['Param', 'Loc_miss'] <- input$loc_missing_slider
      param_table['Param', 'MAF'] <- input$maf_slider
      param_table['Param', 'Loc_het'] <- input$loc_het_slider
      param_table['Param', 'Ind_Het'] <- input$ind_het_slider

      params(param_table)

      shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')

      result <- data()
      print(result)
      qamAnalysisId <- as.numeric(Sys.time())

      print(qamAnalysisId)

      modifications <- data.frame(
        module = rep("qaGeno", dim(param_table)[2]),
        analysisId = rep(qamAnalysisId, dim(param_table)[2]),
        param = colnames(param_table),
        threshold = as.numeric(param_table["Param",])
      )
      print(modifications)
      result$modifications$geno <- modifications

      newStatus <- data.frame(module="qaGeno", analysisId= modifications$analysisId[nrow(modifications)])
      result$status <- rbind(result$status, newStatus)
      print(result$status)
      data(result)

      cat(paste("Modifications to genotype information saved with id:",as.POSIXct( modifications$analysisId[nrow(modifications)], origin="1970-01-01", tz="GMT") ))

      shinybusy::remove_modal_spinner()
      updateTabsetPanel(session, "tabsMain", selected = "Output")
    })


    overall_summary <- reactiveVal(
      data.frame(
        Nind = c("", ""),
        Nloc = c("", ""),
        Ov_miss = c("", ""),
        Ov_Het = c("", ""),
        MAF = c("", ""),
        row.names = c("Baseline", "Filtered")
      )
    )

    params <- reactiveVal(data.frame(
      Ind_miss = c(""),
      Loc_miss = c(""),
      MAF = c(""),
      Loc_het = c(""),
      Ind_Het = c(""),
      row.names = c("Param")
    ))

    output$genoSummaryStats <- output$genoSummaryStats2 <- renderTable({
      overall_summary()
    },
    striped = TRUE,
    bordered = TRUE,
    rownames = TRUE
    )


    output$filter_params_status <- renderTable({
      results <- params()

    },
    rownames = TRUE,
    bordered = TRUE)
  })

}

format_percentage <- function(percentage){
  return (paste0(round(percentage*100, 2), "%"))
}

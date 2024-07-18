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
                 titlePanel("Filtering Parameters"),
                 tags$body(
                   fluidRow(
                     column(width = 8,
                            tabsetPanel(id = ns("filter_panel"),
                                        type = "tabs",
                                        tabPanel("Ind. Miss",
                                                 uiOutput(ns("ind_miss_filter_params")),
                                                 plotly::plotlyOutput(ns("hist_ind_miss")),
                                                 actionButton(inputId = ns('filter_btn'), "Filter")
                                                 ),
                                        tabPanel("Loc. Miss",
                                                 uiOutput(ns("loc_miss_filter_params")),
                                                 plotly::plotlyOutput(ns("hist_loc_miss")),
                                                 actionButton(inputId = ns('filter_btn'), "Filter")
                                                 ),
                                        tabPanel("MAF",
                                                 uiOutput(ns("maf_filter_params")),
                                                 plotly::plotlyOutput(ns("hist_maf")),
                                                 actionButton(inputId = ns('filter_btn'), "Filter")
                                                 ),
                                        tabPanel("Loc. Het",
                                                 uiOutput(ns("loc_het_filter_params")),
                                                 plotly::plotlyOutput(ns("hist_loc_het")),
                                                 actionButton(inputId = ns('filter_btn'), "Filter")
                                                 ),
                                        tabPanel("Ind. Het",
                                                 uiOutput(ns("ind_het_filter_params")),
                                                 plotly::plotlyOutput(ns("hist_ind_het")),
                                                 actionButton(inputId = ns('filter_btn'), "Filter")
                                                 ),
                              )
                            ),
                     column(width = 4,
                            uiOutput(ns("genoSummaryStats"))
                            ),
                   )
                 ),
                 ),
        tabPanel("Output",
                 icon = icon("book"),
                 br(),
                 tags$body()
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



    # get_geno <- reactive({
    #   req(data()$data$gl)
    #   gl <- data()$data$gl
    #   return(gl)
    # })

    get_geno_filtered <- reactive({
      req(data()$data$gl)
      gl <- data()$data$gl

      # if(!is.null(input$ind_missing_slider) & input$ind_missing_slider > 0){
      #   print("enter here?")
      #   gl <- cgiarGenomics::filter_missing_rate_by_indv(gl, input$ind_missing_slider)
      # }
      # if(!is.null(input$loc_missing_slider) & input$loc_missing_slider > 0){
      #   gl <- cgiarGenomics::filter_missing_rate_by_marker(gl, input$loc_missing_slider)
      # }
      # if(!is.null(input$maf_slider) & input$maf_slider > 0){
      #   gl <- cgiarGenomics::filter_MAF(gl, input$maf_slider)
      # }
      # if(!is.null(input$loc_het_slider) & input$loc_het_slider > 0){
      #   gl <- cgiarGenomics::filter_heterozygosis_by_loc(gl, input$loc_het_slider)
      # }
      # if(!is.null(input$ind_het_slider) & input$ind_het_slider > 0){
      #   gl <- cgiarGenomics::filter_heterozygosis_by_ind(gl, input$ind_het_slider)
      # }
      # print("genlight produced")
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
      req(get_geno_filtered())
      gl<-get_geno_filtered()

      tags$span(
        sliderInput(ns("ind_missing_slider"),
                    "Minimum percentage of genotyped loci by individual:",
                    0,
                    round(max(gl@other$ind.metrics$ind_miss),2),
                    step = 0.01,
                    value = 0),
      )
    })
    # Histogram for missingness by individual
    output$hist_ind_miss <- plotly::renderPlotly({
      req(get_geno_filtered())
      gl<-get_geno_filtered()
      fig <- plotly::plot_ly(alpha = 0.6)
      fig <- fig %>% plotly::add_histogram(x = gl@other$ind.metrics$ind_miss,
                                           name="Missing data by indivudual" )

    })

#
#     ind_miss_filtered <- eventReactive(
#       input$ind_miss_btn,
#       {gl<-get_geno()
#       return(cgiarGenomics::filter_missing_rate_by_indv(gl, input$ind_missing_slider))}
#     )

    # Slider for missingness by locus
    output$loc_miss_filter_params <- renderUI({
      req(get_geno_filtered())
      gl<-get_geno_filtered()

      tags$span(
        sliderInput(ns("loc_missing_slider"),
                    "Minimum missing rate by locus:",
                    0,
                    round(max(gl@other$loc.metrics$loc_miss),2),
                    step = 0.01,
                    value = 0),
      )
    })

    # Histogram for missingness by locus
    output$hist_loc_miss <- plotly::renderPlotly({
      req(get_geno_filtered())
      gl<-get_geno_filtered()
      fig <- plotly::plot_ly(alpha = 0.6)
      fig <- fig %>% plotly::add_histogram(x = gl@other$loc.metrics$loc_miss,
                                           name="Missing rate by locus" )

    })

    # loc_miss_filtered <- eventReactive(
    #   input$loc_miss_btn,
    #   {gl<-ind_miss_filtered()
    #   return(cgiarGenomics::filter_missing_rate_by_marker(gl, input$loc_missing_slider))}
    # )

    # Slider for maf
    output$maf_filter_params <- renderUI({
      req(get_geno_filtered())
      gl<-get_geno_filtered()

      tags$span(
        sliderInput(ns("maf_slider"),
                    "Minimum minor allele frequency (MAF):",
                    0,
                    round(max(gl@other$loc.metrics$maf),2),
                    step = 0.01,
                    value = 0),
      )
    })
    # Histogram for maf
    output$hist_maf <- plotly::renderPlotly({
      req(get_geno_filtered())
      gl<-get_geno_filtered()
      fig <- plotly::plot_ly(alpha = 0.6)
      fig <- fig %>% plotly::add_histogram(x = gl@other$loc.metrics$maf,
                                           name="Minor allele frequency" )

    })
#
#     maf_filtered <- eventReactive(
#       input$maf_btn,
#       {gl<-loc_miss_filtered()
#       return(cgiarGenomics::filter_MAF(gl, input$maf_slider))}
#     )

    # Slider for heterozygosis by locus
    output$loc_het_filter_params <- renderUI({
      req(get_geno_filtered())
      gl<-get_geno_filtered()

      tags$span(
        sliderInput(ns("loc_het_slider"),
                    "Maximum Heterozygosity by locus:",
                    0,
                    round(max(gl@other$loc.metrics$loc_het),2),
                    step = 0.01,
                    value = 0),
      )
    })
    # Histogram for heterozygosis by locus
    output$hist_loc_het <- plotly::renderPlotly({
      req(get_geno_filtered())
      gl<-get_geno_filtered()
      fig <- plotly::plot_ly(alpha = 0.6)
      fig <- fig %>% plotly::add_histogram(x = gl@other$loc.metrics$loc_het,
                                           name="Heterozygosity by locus" )

    })
#
#     het_loc_filtered <- eventReactive(
#       input$loc_het_btn,
#       {gl<-maf_filtered()
#       return(cgiarGenomics::filter_heterozygosis_by_loc(gl, input$loc_het_slider))}
#     )

    # Slider for heterozygosis by individual
    output$ind_het_filter_params <- renderUI({
      req(get_geno_filtered())
      gl<-get_geno_filtered()

      tags$span(
        sliderInput(ns("ind_het_slider"),
                    "Maximum Heterozygosity by individual:",
                    0,
                    round(max(gl@other$ind.metrics$ind_het),2),
                    step = 0.01,
                    value = 0),
      )
    })
    # Histogram for heterozygosis by individual
    output$hist_ind_het <- plotly::renderPlotly({
      req(get_geno_filtered())
      gl<-get_geno_filtered()
      fig <- plotly::plot_ly(alpha = 0.6)
      fig <- fig %>% plotly::add_histogram(x = gl@other$ind.metrics$ind_het,
                                           name="Heterozygosity by individual" )

    })

    # het_ind_filtered <- eventReactive(
    #   input$ind_het_btn,
    #   {gl<-het_loc_filtered()
    #   return(cgiarGenomics::filter_heterozygosis_by_ind(gl, input$ind_het_slider))}
    # )

    # filter_data <- eventReactive(
    #   input$filter_btn,
    #   {
    #     get_geno_filtered()
    #   }
    # )




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








  })
}

format_percentage <- function(percentage){
  return (paste0(round(percentage*100, 2), "%"))
}

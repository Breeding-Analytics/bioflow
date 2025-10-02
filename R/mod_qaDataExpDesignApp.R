#' qaPhenoApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_qaDataExpDesignApp_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    shiny::mainPanel(width = 12,
                     tabsetPanel( #width=9,
                       type = "tabs",
                       
                       tabPanel(div(icon("book"), "Information") ,
                                br(),
                                column(width = 6,
                                       h1(strong(span("Phenotype QA/QC Module (Experimental Design)", style="color:darkcyan"))),
                                       h2(strong("Data Status (wait to be displayed):")),
                                       uiOutput(ns("warningMessage")),
                                ),
                                column(width = 6,
                                       tags$body(
                                         h2(strong("Details")),
                                         p("Sometimes is required to set to missing experimental design factors that we are aware that are not correctly saved
                                                               in the databases. Although the tight solution would be to fix this information in the original database, a pragmatic
                                                               approach is to set certain factors from an especific environment to missing so it is ignored in the model fitting or
                                                               any other analytical module using this information."),
                                       )
                                ),
                       ),
                       tabPanel(div(icon("arrow-right-to-bracket"), "Input steps"),
                                tabsetPanel(
                                  tabPanel(div(  icon("dice-one"), "Experimental Grid", icon("arrow-right") ), # icon = icon("dice-one"),
                                           br(),
                                           column(width = 6, 
                                                  hr(style = "border-top: 3px solid #4c4c4c;"),
                                                  h5(strong(span("The table shows the conflicts found due to different genotypes in the same plot ", style="color:green"))),
                                                  hr(style = "border-top: 3px solid #4c4c4c;"),
                                                  DT::DTOutput(ns("transTableC")),
                                           ),
                                           column(width=6, #style = "height:580px; overflow-y: scroll;overflow-x: scroll;",
                                                  #column(width=12),
                                                  selectInput(ns("selEnv"), "Environment", choices = NULL, multiple = FALSE),
                                                  shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = FALSE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                      hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                      h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you fix the errors in the experiment grid", style="color:green"))),
                                                                      hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                      plotly::plotlyOutput(ns("plotGridExp")),
                                                  ),
                                           )
                                           
                                  ),
                                  tabPanel(div(  icon("dice-two"), "Balance by environment", icon("arrow-right") ), # icon = icon("dice-one"),
                                           br(),
                                           column(width=6, #style = "background-color:grey; color: #FFFFFF",
                                                  selectInput(ns("balselEnv"), "Environment:",choices = NULL, multiple = TRUE),
                                                  tabsetPanel(
                                                  tabPanel("Blocks", DT::DTOutput(ns("tabla_bloques"))),
                                                  tabPanel("Replication", DT::DTOutput(ns("tabla_replica")))
                                                  )
                                                  #DT::dataTableOutput(ns("KeepTable"))
                                           ),
                                           column(width=6,
                                                  shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = FALSE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                      hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                      h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you know the connectivity between environments", style="color:green"))),
                                                                      hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                      plotly::plotlyOutput(ns("heatmap_conectividad")),
                                                  )
                                                  )
                                           
                                  ),
                                  tabPanel(div(  icon("dice-three"), "Select environment", icon("arrow-right") ), # icon = icon("dice-one"),
                                           br(),
                                           column(width=12, #style = "background-color:DarkGray; color: #FFFFFF",
                                           br(),
                                           shinydashboard::box(width = 12,style = "color: #000000", status = "success", solidHeader=TRUE,collapsible = TRUE, collapsed = FALSE, title = "Environments to exclude (optional)...",
                                                               hr(style = "border-top: 3px solid #4c4c4c;"),
                                                               p(span("Double click in the cell and set to zero if you would like to ignore an environment in all the next analysis.", style="color:green")),
                                                               hr(style = "border-top: 3px solid #4c4c4c;"),
                                                               DT::DTOutput(ns("DropEnv")),
                                           ),
                                    ),
                                  ),
                                  tabPanel("Run analysis", icon = icon("dice-four"),
                                           br(),
                                           column(width=12,style = "background-color:grey; color: #FFFFFF",
                                                  column(width=3, tags$div(textInput(ns("analysisIdName"), label = tags$span(
                                                    "Analysis Name (optional)", tags$i( class = "glyphicon glyphicon-info-sign", style = "color:#FFFFFF",
                                                                                        title = "An optional name for the analysis besides the timestamp if desired.") ), #width = "100%",
                                                    placeholder = "(optional name)") ) ),
                                                  column(width=3,
                                                         br(),
                                                         actionButton(ns("runFieldClean"), "Tag factors", icon = icon("play-circle")),
                                                  ),
                                           ),textOutput(ns("outExp"))
                                  ),
                                ) # end of tabset
                       ),# end of input panel
                       tabPanel(div(icon("arrow-right-from-bracket"), "Output tabs" ) , value = "outputTabs",
                                tabsetPanel(
                                  tabPanel("Dashboard", icon = icon("file-image"),
                                           br(),
                                           textOutput(ns("outExp2")),
                                           br(),
                                           downloadButton(ns("downloadReportQaPheno"), "Download dashboard"),
                                           br(),
                                           uiOutput(ns('reportQaPheno'))
                                  ),
                                ),
                       ), # end of output panel# end of output panel
                     )
    ) # end mainpanel
  )
}


#' qaRawApp Server Functions
#'
#' @noRd
mod_qaDataExpDesignApp_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })
    ############################################################################
    ############################################################################
    # warning message
    output$warningMessage <- renderUI(
      if(is.null(data())){
        HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your phenotypic data using the 'Data Retrieval' tab.")) )
      }else{ # data is there
        mappedColumns <- length(which(c("environment","row","col","iBlock","rep") %in% data()$metadata$pheno$parameter))
        if( length(mappedColumns) == 0 ){
          HTML( as.character(div(style="color: red; font-size: 20px;", "Please map some of your experimental design columns using the 'Data Retrieval' tab in the 'Phenotype' section to use this module.")) )
        }else{
          HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to inspect the options.")) )
        }
      }
    )
    
    observeEvent(data(), {
      req(data())
      dtQaRaw <- data();
      mydata <- as.data.frame(dtQaRaw$data$pheno)
      dtQaRaw <- as.data.frame(dtQaRaw$metadata$pheno)
      colnames(mydata) <- cgiarBase::replaceValues(colnames(mydata), Search = dtQaRaw$value, Replace = dtQaRaw$parameter )
      if("environment" %in% colnames(mydata)){
          EnvQaRaw <- unique(mydata[,"environment"])
          updateSelectInput(session, "selEnv",choices = EnvQaRaw, selected = EnvQaRaw[1])
      }
    })
    ##produce the Grid plot
    output$plotGridExp <- plotly::renderPlotly({#shiny::renderPlot({
      req(data())
      result<-data()
      mydata <- as.data.frame(result$data$pheno)
      paramsPheno <- as.data.frame(result$metadata$pheno)
      colnames(mydata) <- cgiarBase::replaceValues(colnames(mydata), Search = paramsPheno$value, Replace = paramsPheno$parameter )
      if(all(c("environment","row","col") %in% colnames(mydata))){ # if we are good to go
        visualizar_grid_todos_ambientes <- function(df,
                                                    ambiente_col = "environment",
                                                    row_col = "row",
                                                    col_col = "col",
                                                    genotype_col = "designation",
                                                    as_numeric_grid = TRUE,
                                                    conflict_symbol = " ",
                                                    conflict_symbol_size = 6,
                                                    conflict_symbol_color = "white",
                                                    ambiente = NULL) {
          stopifnot(all(c(ambiente_col, row_col, col_col, genotype_col) %in% names(df)))
          
          df2 <- df |>
            dplyr::rename(.amb = all_of(ambiente_col),
                          .r_orig = all_of(row_col),
                          .c_orig = all_of(col_col),
                          .g = all_of(genotype_col)) |>
            dplyr::mutate(
              .r_chr = as.character(.r_orig),
              .c_chr = as.character(.c_orig),
              .g = as.character(.g)
            )
          
          # Si el usuario indicó ambientes, verificar y filtrar ya aquí
          if (!is.null(ambiente)) {
            # permitir que pasen factores, números o caracteres
            ambientes_disponibles <- unique(df2$.amb)
            # normalizar a carácter para la comparación
            ambiente <- as.character(ambiente)
            faltan <- setdiff(ambiente, as.character(ambientes_disponibles))
            if (length(faltan) > 0) {
              stop("Los siguientes ambientes solicitados no existen en los datos: ",
                   paste(faltan, collapse = ", "))
            }
            df2 <- df2 |> dplyr::filter(.amb %in% ambiente)
          }
          
          # --- procesar cada ambiente y juntar ---
          ambientes <- unique(df2$.amb)
          todo_list <- list()
          
          for (amb in ambientes) {
            sub <- df2 |> dplyr::filter(.amb == amb)
            
            agg <- sub |>
              dplyr::group_by(.r_chr, .c_chr) |>
              dplyr::summarise(
                n_distinct_genotypes = dplyr::n_distinct(na.omit(.g)),
                .groups = "drop"
              )
            
            # detectar si se pueden usar grids numéricos
            is_row_num <- if (nrow(sub) == 0) FALSE else suppressWarnings(!any(is.na(as.numeric(unique(sub$.r_chr)))))
            is_col_num <- if (nrow(sub) == 0) FALSE else suppressWarnings(!any(is.na(as.numeric(unique(sub$.c_chr)))))
            
            if (as_numeric_grid && is_row_num && is_col_num && nrow(sub) > 0) {
              rows_num <- base::seq(min(as.numeric(sub$.r_chr), na.rm = TRUE),
                                    max(as.numeric(sub$.r_chr), na.rm = TRUE))
              cols_num <- base::seq(min(as.numeric(sub$.c_chr), na.rm = TRUE),
                                    max(as.numeric(sub$.c_chr), na.rm = TRUE))
              full_grid <- base::expand.grid(.r_chr = as.character(rows_num),
                                             .c_chr = as.character(cols_num),
                                             stringsAsFactors = FALSE)
              full_grid$.r_num <- as.numeric(full_grid$.r_chr)
              full_grid$.c_num <- as.numeric(full_grid$.c_chr)
              numeric_grid <- TRUE
            } else {
              rows_vec <- unique(sub$.r_chr)
              cols_vec <- unique(sub$.c_chr)
              full_grid <- base::expand.grid(.r_chr = rows_vec,
                                             .c_chr = cols_vec,
                                             stringsAsFactors = FALSE)
              full_grid$.r_num <- NA_real_
              full_grid$.c_num <- NA_real_
              numeric_grid <- FALSE
            }
            
            merged <- full_grid |>
              dplyr::left_join(agg, by = c(".r_chr", ".c_chr")) |>
              dplyr::mutate(
                n_distinct_genotypes = ifelse(is.na(n_distinct_genotypes), 0L, n_distinct_genotypes),
                status = dplyr::case_when(
                  n_distinct_genotypes == 0 ~ "missing",
                  n_distinct_genotypes == 1 ~ "ok",
                  n_distinct_genotypes >= 2 ~ "conflict"
                ),
                symbol = ifelse(status == "conflict", conflict_symbol, ""),
                .amb = amb
              )
            
            if (numeric_grid) {
              merged <- merged |>
                dplyr::mutate(.r_plot = .r_num, .c_plot = .c_num)
            } else {
              merged <- merged |>
                dplyr::mutate(.r_plot = factor(.r_chr, levels = rev(unique(.r_chr))),
                              .c_plot = factor(.c_chr, levels = unique(.c_chr)))
            }
            
            todo_list[[as.character(amb)]] <- merged
          }
          
          merged_all <- dplyr::bind_rows(todo_list)
          
          # colores
          color_map <- c("conflict" = "red", "ok" = "green", "missing" = "yellow")
          
          base_plot <- ggplot2::ggplot(merged_all, ggplot2::aes(x = .c_plot, y = .r_plot)) +
            ggplot2::geom_tile(ggplot2::aes(fill = status), colour = "grey40") +
            ggplot2::geom_text(
              data = merged_all |> dplyr::filter(status == "conflict"),
              ggplot2::aes(label = symbol),
              size = conflict_symbol_size,
              color = conflict_symbol_color,
              fontface = "bold",
              show.legend = FALSE
            ) +
            ggplot2::scale_fill_manual(values = color_map, name = "State",
                                       labels = c(conflict = "Conflict", ok = "Correct", missing = "Missing")) +
            ggplot2::labs(title = " ", x = col_col, y = row_col) +
            ggplot2::theme_minimal() +
            ggplot2::theme(
              axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5, hjust = 0.5),
              axis.title = ggplot2::element_text(face = "bold"),
              panel.grid = ggplot2::element_blank()
            )
          
          # Si el grid es numérico y sólo hay un ambiente mostrado -> forzar breaks de 1 en 1
          single_ambiente <- length(unique(merged_all$.amb)) == 1
          if (numeric_grid && single_ambiente) {
            # calcular rangos seguros
            xmin <- min(merged_all$.c_plot, na.rm = TRUE)
            xmax <- max(merged_all$.c_plot, na.rm = TRUE)
            ymin <- min(merged_all$.r_plot, na.rm = TRUE)
            ymax <- max(merged_all$.r_plot, na.rm = TRUE)
            
            base_plot <- base_plot +
              ggplot2::scale_x_continuous(breaks = base::seq(xmin, xmax, by = 1)) +
              ggplot2::scale_y_continuous(breaks = base::seq(ymin, ymax, by = 1))
          }
          
          # Facetear sólo si se muestran varios ambientes
          if (single_ambiente) {
            p <- base_plot
          } else {
            p <- base_plot + ggplot2::facet_wrap(~.amb, scales = "free")
          }
          
          return(p)
        }
        
        mydata<-mydata[,c("environment","row","col","designation")]
        #print(visualizar_grid_todos_ambientes(mydata,ambiente=input$selEnv))
        fig<-plotly::ggplotly(visualizar_grid_todos_ambientes(mydata,ambiente=input$selEnv))
        fig
      }else{
          #plot(x=1,y=1,name="No row and column information available for this field")
        fig = plotly::plot_ly()
        fig = fig %>% plotly::add_annotations(text = "No row and column information available for this field", x = 1, y = 1)#
        fig
        }
      # just a test
    })
    
    ## our function to keep track of rows, cols unique geno in each env
    dtFieldTraC <- reactive({
      req(data())
      result<-data()
      dtProv <- as.data.frame(result$data$pheno)
      paramsPheno <- as.data.frame(result$metadata$pheno)
      colnames(dtProv) <- cgiarBase::replaceValues(colnames(dtProv), Search = paramsPheno$value, Replace = paramsPheno$parameter )
      if(all(c("environment","row","col") %in% colnames(dtProv))){ # if we are good to go
        dtProv<-dtProv[,c("environment","row","col","designation")]
        conflictos <- dtProv |>
              dplyr::group_by(environment, row, col) |>
              dplyr::summarise(
                n_genotipos = dplyr::n_distinct(designation),
                genotipos   = paste(unique(designation), collapse = ", "),
                .groups = "drop"
              ) |>
              dplyr::filter(n_genotipos > 1)
        if(dim(conflictos)[1]<1){conflictos=as.data.frame("No conflicts found")}
      }else{
        conflictos=as.data.frame("Not found row and column")
      }
      return(conflictos)
    })
    output$transTableC <- DT::renderDT({
      DT::datatable(dtFieldTraC(), extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
                    #caption = htmltools::tags$caption(
                    #  style = 'color:cadetblue', #caption-side: bottom; text-align: center;
                      #htmltools::em('Records identified to be tagged and ignored.')
                    #)
      )
    }, server = FALSE)
    
    
    # ----------------------------
    # Función de resumen (genera .plot_id internamente)
    # ----------------------------
    resumen_balanceado_bloques <- function(df,
                                           ambiente_col = "environment",
                                           replica_col = "rep",
                                           block_col = "iBlock",
                                           genotype_col = "designation") {
      
      stopifnot(ambiente_col %in% names(df), genotype_col %in% names(df))
      
      cols_presentes <- names(df)
      has_replica <- replica_col %in% cols_presentes
      has_block   <- block_col %in% cols_presentes
      
      # Definir columnas de agrupamiento para generar plot id (si replica/block existen, los usamos)
      group_for_plot <- c(ambiente_col)
      if (has_replica) group_for_plot <- c(group_for_plot, replica_col)
      if (has_block)   group_for_plot <- c(group_for_plot, block_col)
      
      # Generar .plot_id secuencial dentro de cada grupo_for_plot
      df2 <- df |>
        dplyr::group_by(dplyr::across(dplyr::all_of(group_for_plot))) |>
        dplyr::mutate(.plot_id = paste0(.data[[ambiente_col]], "_P", dplyr::row_number())) |>
        dplyr::ungroup()
      
      # Número de plots por (ambiente[, replica[, block]])
      group_cols_block <- group_for_plot
      n_plot_block <- df2 |>
        dplyr::group_by(dplyr::across(dplyr::all_of(group_cols_block))) |>
        dplyr::summarise(
          n_plot = dplyr::n_distinct(.plot_id),
          .groups = "drop"
        )
      
      # Frecuencia de genotipos por el mismo nivel (ambiente[, replica[, block]])
      freq_genotipo_block <- df2 |>
        dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols_block, genotype_col)))) |>
        dplyr::summarise(
          freq = dplyr::n(),
          .groups = "drop"
        ) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(group_cols_block))) |>
        dplyr::summarise(
          min_designation = min(freq),
          max_designation = max(freq),
          balanced_block = (min(freq) == max(freq)),
          total_designation = dplyr::n_distinct(.data[[genotype_col]]),
          .groups = "drop"
        )
      
      # Resumen por réplica (si replica existe) o por ambiente si solo block/ninguno
      resumen_replica <- NULL
      if (has_replica || has_block) {
        # definir cols para resumir por "replica-level" (ambient + replica if present, else ambient + block)
        group_cols_replica <- c(ambiente_col)
        if (has_replica) group_cols_replica <- c(group_cols_replica, replica_col)
        # si sólo existe block (no replica) igual mostramos por ambiente+block como "replica-level"
        if (!has_replica && has_block) group_cols_replica <- c(group_cols_replica, block_col)
        
        resumen_replica <- freq_genotipo_block |>
          dplyr::group_by(dplyr::across(dplyr::all_of(group_cols_replica))) |>
          dplyr::summarise(
            n_blocks = if (has_block && has_replica) dplyr::n_distinct(.data[[block_col]]) else if (has_block) 1 else NA_integer_,
            min_designation = min(min_designation),
            max_designation = max(max_designation),
            balanced_rep = all(balanced_block),
            .groups = "drop"
          )
      } else {
        # No hay replica ni block -> resumen por ambiente
        resumen_replica <- freq_genotipo_block |>
          dplyr::group_by(.data[[ambiente_col]]) |>
          dplyr::summarise(
            min_designation = min(min_designation),
            max_designation = max(max_designation),
            balanced_rep = all(balanced_block),
            .groups = "drop"
          )
      }
      
      list(
        balance_por_bloque = freq_genotipo_block,
        n_plot_block = n_plot_block,
        resumen_por_replica = resumen_replica,
        df_with_plot_id = df2  # devuelvo el df con .plot_id por si lo necesitas
      )
    }
    ##fill environment for block and rep
    observeEvent(data(), {
      req(data())
      dtQaRaw1 <- data();
      mydata2 <- as.data.frame(dtQaRaw1$data$pheno)
      dtQaRaw1 <- as.data.frame(dtQaRaw1$metadata$pheno)
      colnames(mydata2) <- cgiarBase::replaceValues(colnames(mydata2), Search = dtQaRaw1$value, Replace = dtQaRaw1$parameter )
      if("environment" %in% colnames(mydata2)){
        EnvQaRaw1 <- unique(mydata2[,"environment"])
        updateSelectInput(session,"balselEnv",choices = sort(EnvQaRaw1), selected = sort(EnvQaRaw1))
      }
    })
    
    # Resumen por bloques y réplicas
    resumen_filtrado <- reactive({
      req(input$balselEnv)
      req(data())
      result<-data()
      mydata <- as.data.frame(result$data$pheno)
      paramsPheno <- as.data.frame(result$metadata$pheno)
      colnames(mydata) <- cgiarBase::replaceValues(colnames(mydata), Search = paramsPheno$value, Replace = paramsPheno$parameter )
      fincols<-which(colnames(mydata)%in%c("environment","designation","iBlock","rep")==T)
      if(length(fincols)>=3){ 
          mydata<-mydata[,which(colnames(mydata)%in%c("environment","designation","iBlock","rep")==T)]
          df <-  mydata|> dplyr::filter(environment %in% input$balselEnv)
          resumen_balanceado_bloques(df)
      }else{data.frame("No found factor of design (rep and block)")}
    })
    
    # Tabla bloques: unimos conteo de plots con balance por bloque
    output$tabla_bloques <- DT::renderDT({
      res <- resumen_filtrado()
      tabla_blocks <- res$balance_por_bloque |>
        dplyr::left_join(res$n_plot_block, by = intersect(names(res$balance_por_bloque), names(res$n_plot_block)))
      
      DT::datatable(tabla_blocks, options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                             lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))) |>
        DT::formatStyle(
          "balanced_block",
          target = "row",
          backgroundColor = DT::styleEqual(c(TRUE, FALSE), c(NA, "salmon"))
        )
    })
    
    # Tabla réplicas
    output$tabla_replica <- DT::renderDT({
      tabla <- resumen_filtrado()$resumen_por_replica
      DT::datatable(tabla, options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                    lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))) |>
        DT::formatStyle(
          "balanced_rep",
          target = "row",
          backgroundColor = DT::styleEqual(c(TRUE, FALSE), c(NA, "lightblue"))
        )
    })
    
    
    output$heatmap_conectividad <- plotly::renderPlotly({
      res <- resumen_filtrado()
      df <- res$df_with_plot_id
      
      ambientes <- sort(unique(df$environment))
      tgen<-length(unique(df$designation))
      n <- length(ambientes)
      if (n == 0) return(NULL)
      
      mat <- matrix(0, nrow = n, ncol = n, dimnames = list(ambientes, ambientes))
      mat_perc <- matrix(0, nrow = n, ncol = n, dimnames = list(ambientes, ambientes))
      matt <- matrix(0, nrow = n, ncol = n, dimnames = list(ambientes, ambientes))
      
      # Calcular número y porcentaje de genotipos compartidos
      for (i in seq_len(n)) {
        g_i <- unique(df$designation[df$environment == ambientes[i]])
        for (j in seq_len(n)) {
          g_j <- unique(df$designation[df$environment == ambientes[j]])
          inter <- length(intersect(g_i, g_j))
          mat[i, j] <- inter
          matt[i, j]<- paste0(round(inter,0),"/",tgen)
          mat_perc[i, j] <- if (length(g_i) > 0) inter / tgen * 100 else 0
        }
      }
      
      # Crear tooltip vectorizado en el mismo orden que z (fila por fila)
      tooltip_vec <- character(0)
      for (i in seq_len(n)) {
        for (j in seq_len(n)) {
          tooltip_vec <- c(tooltip_vec,
                           paste0(
                             "Environment X: ", ambientes[i],
                             "<br>Environment Y: ", ambientes[j],
                             "<br>Shared designation: ", mat[i, j],
                             "<br>Percent respect to total: ", round(mat_perc[i, j], 1), "%"
                           ))
        }
      }
      tooltip_mat <- matrix(tooltip_vec, nrow = n, ncol = n, byrow = TRUE)
      # Escala de colores: rojo → amarillo → verde
      colorscale <- list(
        list(0, "red"),
        list(0.5, "yellow"),
        list(1, "green")
      )
      
      plotly::plot_ly(
        x = ambientes,
        y = ambientes,
        z = mat_perc,
        type = "heatmap",
        colorscale = colorscale,
        reversescale = FALSE,
        text = matt,        # el texto que se mostrará en cada celda
        hovertext = tooltip_mat,          # tooltip detallado al pasar el mouse
        hoverinfo = "text",
        texttemplate = "%{text}",         # usa el valor de `text`
        textfont = list(color = "black")  # color de la fuente dentro de los cuadros
      ) |>
        plotly::layout(
          #title = "Conectivity entre ambientes (% respecto al total de X)",
          xaxis = list(title = "Environment"),
          yaxis = list(title = "Environment")
        )
    })
    
    #################
    # reactive table for environments to be included
    dtFieldMet = reactive({
      req(data())
      result<-data()
      mydata <- as.data.frame(result$data$pheno)
      paramsPheno <- as.data.frame(result$metadata$pheno)
      colnames(mydata) <- cgiarBase::replaceValues(colnames(mydata), Search = paramsPheno$value, Replace = paramsPheno$parameter )
      if("environment" %in% colnames(mydata)){
        dtProvTable <- data.frame(unique(mydata[,"environment"]))
        dtProvTable$Available<-rep(1,dim(dtProvTable)[1])
        colnames(dtProvTable)<-c("Environments","Available")
      }else{dtProvTable <- data.frame()}
      return(dtProvTable)
    })
    x = reactiveValues(df = NULL)
    observe({
      df <- dtFieldMet()
      x$df <- df
    })
    output$DropEnv = DT::renderDT(x$df, selection = 'none', editable = TRUE, server = FALSE)
    proxy = DT::dataTableProxy('DropEnv')
    observeEvent(input$DropEnv_cell_edit, {
      info = input$DropEnv_cell_edit
      utils::str(info)
      i = info$row
      j = info$col
      v = info$value
      x$df[i, j] <- isolate(DT::coerceValue(v, x$df[i, j]))
    })
    
    modifEnv <- function(object, df){
      # object is the data object from bioflow
      # df is a table of environments (rows) by factors (cols)
      analysisId <- as.numeric(Sys.time())
      mods <- as.data.frame(matrix(NA, nrow=0, ncol=6))# data.frame(module="qaRaw",analysisId=analysisId,trait=trait,reason=typeOut,row=toSilence, value=NA);
      colnames(mods) <- c("module","analysisId","trait","reason","row","value")
      envCol <- object$metadata$pheno[which(object$metadata$pheno$parameter %in% "environment"),"value"]
      
      
      mydata <- object$data$pheno
      paramsPheno <- as.data.frame(result$metadata$pheno)
      colnames(mydata) <- cgiarBase::replaceValues(colnames(mydata), Search = paramsPheno$value, Replace = paramsPheno$parameter )
      
      counter <- 1; myList <- list()
      for(j in 1:nrow(df)){ # for each environment
        if(df[j,2] == 0){ # if user wants to delete this information
          rowsToSilence <- which(mydata[,envCol] == df[j,1]) # rows for this environments
          myList[[counter]] <- data.frame(module="qaExpDesign",analysisId=analysisId,trait=df[j,1],reason="dropEnv",row=rowsToSilence, value=NA);
          counter <- counter + 1
        }
      }
      
      myMods <- do.call(rbind, myList)
      if(!is.null(object$modifications$pheno)){
        object$modifications$pheno <- rbind(object$modifications$pheno, myMods)
      }else{
        object$modifications$pheno <- myMods
      }
      status <- data.frame(module="qaExpDesign", analysisId=analysisId, analysisIdName=NA)
      if(!is.null(object$status)){
        object$status <- rbind(object$status, status[,colnames(object$status)])
      }else{
        object$status <- status
      }
      return(object)
    }
    
    outExp <- eventReactive(input$runFieldClean, {
      
      if(is.null(data())){
        cat( "Please retrieve or load your phenotypic data using the 'Data Retrieval' tab.")
      }else{ # data is there
        ## pheno check
        shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')
        available <- data()$metadata$pheno[data()$metadata$pheno$parameter %in% c("row","col","iBlock","rep"), "value"]
        available <- setdiff(available,"")
        if( length(available) == 0 ){
          cat( "Please map some of your experimental design columns using the 'Data Retrieval' tab in the 'Phenotype' section to use this module.")
          output$outExp2 <- renderPrint({
            cat( "Please map some of your experimental design columns using the 'Data Retrieval' tab in the 'Phenotype' section to use this module.")
          })
        }else{
          object <- data()
          result <- modifEnv(object, df=x$df)
          aid <- result$status$analysisId[length(result$status$analysisId)]
          if("analysisIdName" %in% colnames(result$status)){result$status$analysisIdName[nrow(result$status)] <- input$analysisIdName}
          data(result)
          cat(paste("QA Exp Design step with id:",as.POSIXct( aid, origin="1970-01-01", tz="GMT"),"saved."))
          output$outExp2 <- renderPrint({
            cat(paste("QA Exp Design step with id:",as.POSIXct( aid, origin="1970-01-01", tz="GMT"),"saved."))
          })
        }
        shinybusy::remove_modal_spinner()
        
        if(!inherits(result,"try-error")) { # if all goes well in the run
          # ## Report tab
          output$reportQaPheno <- renderUI({
            HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportQaExpDes.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
          })
          
          output$downloadReportQaPheno <- downloadHandler(
            filename = function() {
              paste(paste0('qaDesign_dashboard_',gsub("-", "", as.integer(Sys.time()))), sep = '.', switch(
                "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
              ))
            },
            content = function(file) {
              shinybusy::show_modal_spinner(spin = "fading-circle", text = "Generating Report...")
              
              src <- normalizePath(system.file("rmd","reportQaExpDes.Rmd",package="bioflow"))
              src2 <- normalizePath('data/resultQaExpDes.RData')
              
              # temporarily switch to the temp dir, in case you do not have write
              # permission to the current working directory
              owd <- setwd(tempdir())
              on.exit(setwd(owd))
              
              file.copy(src, 'report.Rmd', overwrite = TRUE)
              file.copy(src2, 'resultQaExpDes.RData', overwrite = TRUE)
              
              out <- rmarkdown::render('report.Rmd', params = list(toDownload=TRUE),switch(
                "HTML",
                HTML = rmdformats::robobook(toc_depth = 4)
                # HTML = rmarkdown::html_document()
              ))
              
              # wait for it to land on disk (safety‐net)
              wait.time <- 0
              while (!file.exists(out) && wait.time < 60) {
                Sys.sleep(1); wait.time <- wait.time + 1
              }
              
              file.rename(out, file)
              shinybusy::remove_modal_spinner()
            }
          )
          
        }else{ hideAll$clearAll <- TRUE}
        
        hideAll$clearAll <- FALSE
      }
      
    })
    output$outExp <- renderPrint({
      outExp()
    })
    
  
  })
}

## To be copied in the UI
# mod_qaDataExpDesignApp_ui("qaDataExpDesignApp_1")

## To be copied in the server
# mod_qaDataExpDesignApp_server("qaDataExpDesignApp_1")

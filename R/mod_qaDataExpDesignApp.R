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
                     tabsetPanel( id = ns("tabsMain"), #width=9,
                       type = "tabs",
                       tabPanel(div(icon("book"), "Information") ,
                                br(),
                                column(width = 6,
                                       h1(strong(span("Phenotype QA/QC Module - Experimental Design", style="color:darkcyan"))),
                                       h2(strong("Data Status (wait to be displayed):")),
                                       uiOutput(ns("warningMessage")),
                                       tags$br(),
                                       shinyWidgets::prettySwitch( inputId = ns('launchExpDes'), label = "Load example dataset", status = "success"),
                                       tags$br(),
                                       img(src = "www/expdes.png", height = 375, width = 450), # add an image
                                ),
                                column(width = 6,
                                       tags$body(
                                         h2(strong("Details")),
                                         p("Sometimes is required to set to missing experimental design factors that we are aware that are not correctly saved
                                                               in the databases. Although the tight solution would be to fix this information in the original database, a pragmatic
                                                               approach is to set certain factors from an especific environment to missing so it is ignored in the model fitting or
                                                               any other analytical module using this information."),
                                         h2(strong("Software used")),
                                         p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
                                            Vienna, Austria. URL https://www.R-project.org/."),
                                      )
                                ),
                       ),
                       tabPanel(div(icon("arrow-right-to-bracket"), "Input steps"),
                                tabsetPanel(
                                  tabPanel(div(  icon("dice-one"), "Experiment grid", icon("arrow-right") ), # icon = icon("dice-one"),
                                           br(),
                                           column(width = 4,
                                                  selectInput(ns("selEnv"), "Environment", choices = NULL, multiple = TRUE),
                                                  tabsetPanel(
                                                    tabPanel(div(  icon("dice-one"), "By plot", icon("arrow-right") ), 
                                                            hr(style = "border-top: 3px solid #4c4c4c;"),
                                                            h5(strong(span("The table shows the conflicts found due to different genotypes in the same plot ", style="color:green"))),
                                                            hr(style = "border-top: 3px solid #4c4c4c;"),
                                                            DT::DTOutput(ns("transTableC"))),
                                                    tabPanel(div(  icon("dice-two"), "By blocks", icon("arrow-right") ), 
                                                             hr(style = "border-top: 3px solid #4c4c4c;"),
                                                             h5(strong(span("The table show infomation for each environment about how many plots for each block in each replication there are; when something is rare the rows appears in red", style="color:green"))),
                                                             hr(style = "border-top: 3px solid #4c4c4c;"),
                                                             DT::DTOutput(ns("tabla_bloques"))),
                                                    tabPanel(div(  icon("dice-three"), "By replication", icon("arrow-right") ), 
                                                             hr(style = "border-top: 3px solid #4c4c4c;"),
                                                             h5(strong(span("The table show infomation for each environment about how many blocks in each replication there are;when something is rare the rows appears in blue", style="color:green"))),
                                                             hr(style = "border-top: 3px solid #4c4c4c;"),
                                                             DT::DTOutput(ns("tabla_replica")))
                                                  )
                                           ),
                                           column(width=8, #style = "height:1080px;overflow-y: scroll;overflow-x: scroll;",
                                                  shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = FALSE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                      tags$div(
                                                                        style = "max-height:900px; overflow-y:auto; overflow-x:auto;",
                                                                        hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                        h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you fix the errors in the experiment grid", style="color:green"))),
                                                                        hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                        plotly::plotlyOutput(ns("plotGridExp"), height="1100px", width="1100px")
                                                                      )
                                                  ),
                                           )
                                           
                                  ),
                                  tabPanel(div(  icon("dice-two"), "Trait grid", icon("arrow-right") ), # icon = icon("dice-one"),
                                           br(),
                                           column(width=12, #style = "height:1080px;overflow-y: scroll;overflow-x: scroll;",
                                                  selectInput(ns("selTrait"), "Trait", choices = NULL, multiple = FALSE),
                                                  shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = FALSE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                      tags$div(
                                                                        style = "max-height:600px; overflow-y:auto; overflow-x:auto;",
                                                                        hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                        h5(strong(span("The visualizations of the input-data located below will not affect your analysis but may help you fix the errors in the behavior of the trait", style="color:green"))),
                                                                        hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                        plotly::plotlyOutput(ns("plotGridTrait"), height="1800px", width="1800px")
                                                                      )
                                                  ),
                                           )
                                           
                                  ),
                                  tabPanel(div(  icon("dice-three"), "Connectivity", icon("arrow-right") ), # icon = icon("dice-one"),
                                           br(),                                           
                                           column(width=12,
                                                  selectInput(ns("balselEnv"), "Environment:",choices = NULL, multiple = TRUE),
                                                  numericInput(ns("ConThr"),label = "Connectivity Threshold",value = 30, min = 0, max= 100,step=1),
                                                  shinydashboard::box(width = 12, status = "success",solidHeader=TRUE,collapsible = TRUE, collapsed = FALSE, title = "Visual aid (click on the '+' symbol on the right to open)",
                                                                      hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                      h5(strong(span("The visualizations of the input-data located below help you to know the connectivity between environments, 
                                                                                     if the connectivity between environments are low than 30%, that environment will be out of the analysis. 
                                                                                     You can increased or decreased this threshold according of the type of data", style="color:green"))),
                                                                      hr(style = "border-top: 3px solid #4c4c4c;"),
                                                                      plotly::plotlyOutput(ns("heatmap_conectividad")),
                                                    )
                                                  )
                                           
                                  ),
                                  #tabPanel(div(  icon("dice-four"), "Select environment", icon("arrow-right") ), # icon = icon("dice-one"),
                                  #         br(),
                                  #         column(width=12, #style = "background-color:DarkGray; color: #FFFFFF",
                                  #         br(),
                                  #         shinydashboard::box(width = 12,style = "color: #000000", status = "success", solidHeader=TRUE,collapsible = TRUE, collapsed = FALSE, title = "Environments to exclude (optional)...",
                                  #                             hr(style = "border-top: 3px solid #4c4c4c;"),
                                  #                             h5(strong(span("Double click in the cell and set to zero if you would like to ignore an environment in all the next analysis.", style="color:green"))),
                                  #                             hr(style = "border-top: 3px solid #4c4c4c;"),
                                  #                             DT::DTOutput(ns("DropEnv")),
                                  #         ),
                                  #  ),
                                  #),
                                  tabPanel("Run analysis", icon = icon("dice-five"),
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
                                           uiOutput( ns("warningsExp")),
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
    # show shinyWidgets until the user can use the module
    observeEvent(data(), {
      d <- data()
      # El objeto puede estar temporalmente incompleto durante la actualización
      if (is.null(d) ||
          is.null(d$metadata) ||
          is.null(d$metadata$pheno) ||
          !is.data.frame(d$metadata$pheno) ||
          !"parameter" %in% names(d$metadata$pheno)) {
        golem::invoke_js("hideid", ns("holder"))
        return()
      }
      params <- as.character(d$metadata$pheno$parameter)
      required <- c("environment", "designation", "trait")
      mapped <- all(required %in% params)
      if (mapped) {
        golem::invoke_js("showid", ns("holder"))
      } else {
        golem::invoke_js("hideid", ns("holder"))
      }
    }, ignoreNULL = FALSE)
    ############################################################################
    # warning message
    
    # warning message
    output$warningMessage <- renderUI(
      if(is.null(data())){
        HTML( as.character(div(style="color: red; font-size: 20px;", "Please retrieve or load your phenotypic data using the 'Data Retrieval' tab.")) )
      }else{ # data is there
        mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
        if(mappedColumns == 3){ HTML( as.character(div(style="color: green; font-size: 20px;", "Data is complete, please proceed to inspect the options in the 'Input' tabs.")) )
        }else{HTML( as.character(div(style="color: red; font-size: 20px;", "Please make sure that you have computed the 'environment' column, and that column 'designation' and \n at least one trait have been mapped using the 'Data Retrieval' tab.")) )
        }
      }
    )
    
    #################
    ## data example loading
    observeEvent(
      input$launchExpDes,
      if(length(input$launchExpDes) > 0){
        if (input$launchExpDes) {
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
        shinyWidgets::updatePrettySwitch(session, "launchExpDes", value = FALSE)
      }
    }, ignoreNULL = TRUE)
    
    observeEvent(data(), {
      req(data())
      dtQaRaw <- data()
      mydata <- as.data.frame(dtQaRaw$data$pheno)
      dtQaRaw <- as.data.frame(dtQaRaw$metadata$pheno)
      colnames(mydata) <- cgiarBase::replaceValues(colnames(mydata), Search = dtQaRaw$value, Replace = dtQaRaw$parameter )
      if("environment" %in% colnames(mydata)){
          EnvQaRaw <- unique(mydata[,"environment"])
          updateSelectInput(session, "selEnv",choices = EnvQaRaw, selected = EnvQaRaw)
      }
    })
    ##produce the Grid plot
    output$plotGridExp <- plotly::renderPlotly({
      req(data())
      result <- data()
      mydata <- as.data.frame(result$data$pheno)
      paramsPheno <- as.data.frame(result$metadata$pheno)
      paramsPheno<-paramsPheno[-which(paramsPheno$parameter=="trait"),]
      colnames(mydata) <- cgiarBase::replaceValues(colnames(mydata),
                                                   Search = paramsPheno$value,
                                                   Replace = paramsPheno$parameter)
      
      # Validación mínima
      if (!all(c("environment", "row", "col", "designation") %in% colnames(mydata))) {
        return(plotly::plot_ly() %>%
                 plotly::add_annotations(text = "No row and column information available for this field",
                                         x = 1, y = 1, showarrow = FALSE))
      }
      
      env_selected <- if (!is.null(input$selEnv)) input$selEnv else unique(mydata$environment)
      mydata2 <- mydata[mydata$environment %in% env_selected, , drop = FALSE]
      
      # Forzar caracteres para evitar factores raros
      mydata2$row <- as.character(mydata2$row)
      mydata2$col <- as.character(mydata2$col)
      mydata2$designation <- as.character(mydata2$designation)
      if ("rep" %in% names(mydata2)) mydata2$rep <- as.character(mydata2$rep)
      if ("iBlock" %in% names(mydata2)) mydata2$iBlock <- as.character(mydata2$iBlock)
      
      ambientes <- unique(mydata2$environment)
      plots <- vector("list", length(ambientes))
      
      for (i in seq_along(ambientes)) {
        amb <- ambientes[i]
        sub <- mydata2[mydata2$environment == amb, , drop = FALSE]
        subs<- sub[,c("designation","row","col")]
        subs$row<-as.numeric(subs$row)
        subs$col<-as.numeric(subs$col)
        subs<- subs |> tidyr::complete(row=seq(min(subs$row),max(subs$row)),col=seq(min(subs$col),max(subs$col)))
        # Agregado
        agg <- aggregate(designation ~ row + col, data = subs,
                         FUN = function(x) length(unique(na.omit(x))))
        names(agg)[names(agg) == "designation"] <- "n_distinct"
        
        # Determinar filas/cols únicas en el orden que corresponda
        # Si son numéricos usamos orden numérico; si no, preservamos orden alfabético/encuentro
        possible_row_nums <- suppressWarnings(as.numeric(unique(subs$row)))
        is_row_num <- !any(is.na(possible_row_nums))
        possible_col_nums <- suppressWarnings(as.numeric(unique(subs$col)))
        is_col_num <- !any(is.na(possible_col_nums))
        
        if (is_row_num) {
          rows_u <- as.character(sort(as.numeric(unique(subs$row))))
        } else {
          rows_u <- unique(subs$row) # conserva la aparición
        }
        if (is_col_num) {
          cols_u <- as.character(sort(as.numeric(unique(subs$col))))
        } else {
          cols_u <- unique(subs$col)
        }
        
        nrowm <- length(rows_u)
        ncolm <- length(cols_u)
        
        # Matrices iniciales
        mat_z <- matrix(0L, nrow = nrowm, ncol = ncolm)
        mat_text <- matrix("", nrow = nrowm, ncol = ncolm)
        
        # Rellenar: usar índices numéricos vía match()
        if (nrow(agg) > 0) {
          r_pos <- match(as.character(agg$row), rows_u)
          c_pos <- match(as.character(agg$col), cols_u)
          for (k in seq_len(nrow(agg))) {
            ri <- r_pos[k]; ci <- c_pos[k]
            if (is.na(ri) || is.na(ci)) next  # seguridad adicional
            val <- agg$n_distinct[k]
            state <- if (val == 0) "missing" else if (val == 1) "ok" else "conflict"
            color_val <- if (state == "missing") 0L else if (state == "ok") 1L else 2L
            mat_z[ri, ci] <- color_val
            # Tooltip: usamos rows_u/cols_u (originales) para mostrar
            mat_text[ri, ci] <- paste0("Env: ", amb, "<br>",
                                       "State: ", state, "<br>",
                                       "Row: ", rows_u[ri], "<br>",
                                       "Col: ", cols_u[ci], "<br>")
          }
        }
        
        # Rellenar celdas vacías con missing tooltip
        for (ri in seq_len(nrowm)) {
          for (ci in seq_len(ncolm)) {
            if (mat_text[ri, ci] == "") {
              mat_text[ri, ci] <- paste0("Env: ", amb, "<br>",
                                         "State: missing<br>",
                                         "Row: ", rows_u[ri], "<br>",
                                         "Col: ", cols_u[ci], "<br>")
            }
          }
        }
        
        # IMPORTANTE: Plotly heatmap asume (1,1) en esquina superior izquierda si invertimos filas manualmente.
        # Queremos que la fila menor quede arriba: invertir la matriz verticalmente
        #mat_z_plot <- mat_z[nrow(mat_z):1, , drop = FALSE]
        #mat_text_plot <- mat_text[nrow(mat_text):1, , drop = FALSE]
        mat_z_plot=mat_z
        mat_text_plot=mat_text
        
        p <- plotly::plot_ly(
          z = ~mat_z_plot,
          text = ~mat_text_plot,
          hovertemplate = "%{text}<extra></extra>",
          type = "heatmap",
          showscale = FALSE
        ) %>%
          plotly::style(colorscale = list(
            list(0.0, "gold"),
            list(0.5, "green"),
            list(1.0, "red")
          ), zmin = 0, zmax = 2, traces = 1)
        
        # --- Shapes para rep / iBlock (si existen) ---
        shapes <- list()
        
        # Para calcular límites convertimos filas/cols a índices (1..n)
        # sub_indexed contiene índices coincidentes con rows_u/cols_u
        sub_indexed <- sub
        sub_indexed$.r_idx <- match(as.character(sub_indexed$row), rows_u)
        sub_indexed$.c_idx <- match(as.character(sub_indexed$col), cols_u)
        
        # rep como rectángulos (contorno negro)
        if ("rep" %in% names(sub_indexed)) {
          rep_limits <- sub_indexed %>%
            dplyr::filter(!is.na(.r_idx) & !is.na(.c_idx)) %>%
            dplyr::group_by(rep) %>%
            dplyr::summarise(
              rmin = min(.r_idx), rmax = max(.r_idx),
              cmin = min(.c_idx), cmax = max(.c_idx),
              .groups = "drop"
            )
          if (nrow(rep_limits) > 0) {
            for (j in seq_len(nrow(rep_limits))) {
              rm <- rep_limits$rmin[j]; rM <- rep_limits$rmax[j]
              cm <- rep_limits$cmin[j]; cM <- rep_limits$cmax[j]
              # coordenadas en el heatmap (la y está invertida)
              x0 <- cm; x1 <- cM
              y0 <- nrowm - rM; y1 <- nrowm - rm
              shapes[[length(shapes) + 1]] <- list(
                type = "rect",
                x0 = x0-1, x1 = x1-1,
                y0 = y0-0.5, y1 = y1+0.5,
                xref = "x", yref = "y",
                line = list(color = "black", width = 2),
                fillcolor = "rgba(0,0,0,0)"
              )
            }
          }
        }
        
        # iBlock como rectángulos contorno azul dashed
        if ("iBlock" %in% names(sub_indexed)) {
          block_limits <- sub_indexed %>%
            dplyr::filter(!is.na(.r_idx) & !is.na(.c_idx)) %>%
            dplyr::group_by(iBlock) %>%
            dplyr::summarise(
              rmin = min(.r_idx), rmax = max(.r_idx),
              cmin = min(.c_idx), cmax = max(.c_idx),
              .groups = "drop"
            )
          if (nrow(block_limits) > 0) {
            for (j in seq_len(nrow(block_limits))) {
              rm <- block_limits$rmin[j]; rM <- block_limits$rmax[j]
              cm <- block_limits$cmin[j]; cM <- block_limits$cmax[j]
              x0 <- cm; x1 <- cM
              y0 <- nrowm - rM; y1 <- nrowm - rm
              shapes[[length(shapes) + 1]] <- list(
                type = "rect",
                x0 = x0-1, x1 = x1-1,
                y0 = y0-0.5, y1 = y1+0.5,
                xref = "x", yref = "y",
                line = list(color = "blue", width = 1),
                fillcolor = "rgba(0,0,0,0)"
              )
            }
          }
        }
        
        if (length(shapes) > 0) p <- p %>% plotly::layout(shapes = shapes)
        
        # Título simple para el subplot
        p <- p %>% plotly::layout(#title = amb,
                                  xaxis = list(title = "col",showticklabels = FALSE, ticks=""),
                                  yaxis = list(title = "row",showticklabels = FALSE, ticks=""),
                                  margin = list(t = 60))
        
        plots[[i]] <- p
      } # fin for ambientes
      
      ncols <- ifelse(length(plots) >= 2, 2, 1)
      out <- plotly::subplot(plots, nrows = ceiling(length(plots)/ncols),
                             shareX = FALSE, shareY = FALSE) %>%
        plotly::layout(margin = list(t = 80))
      
      out
    })
    
    
    ## our function to keep track of rows, cols unique geno in each env
    # Almacenar el resultado
    dtFieldTraC <- reactiveVal(NULL)
    
    # Calcular cuando cambie data()
    observeEvent(data(), {
      d <- data()
      if (is.null(d) ||
          is.null(d$data) ||
          is.null(d$data$pheno) ||
          is.null(d$metadata) ||
          is.null(d$metadata$pheno)) {
        dtFieldTraC(NULL)
        return()
      }
      conflictos <- tryCatch({
        dtProv <- as.data.frame(d$data$pheno)
        paramsPheno <- as.data.frame(d$metadata$pheno)
        
        if (!all(c("parameter", "value") %in% names(paramsPheno))) {
          return(NULL)
        }
        
        # Renombrar columnas de acuerdo con los metadatos
        idx <- match(names(dtProv), paramsPheno$value)
        valid <- !is.na(idx)
        
        new_names <- names(dtProv)
        new_names[valid] <- as.character(
          paramsPheno$parameter[idx[valid]]
        )
        names(dtProv) <- make.unique(new_names)
        
        # Verificar las columnas necesarias
        required_cols <- c(
          "environment", "row", "col", "designation"
        )
        
        if (!all(required_cols %in% names(dtProv))) {
          return(data.frame(
            Message = "Not found row, column, environment or designation"
          ))
        }
        
        dtProv <- dtProv[, required_cols, drop = FALSE]
        
        # Identificar conflictos en las mismas coordenadas
        conflictos <- dtProv |>
          dplyr::group_by(environment, row, col) |>
          dplyr::summarise(
            n_designation = dplyr::n_distinct(designation),
            designation = paste(
              unique(designation),
              collapse = ", "
            ),
            .groups = "drop"
          ) |>
          dplyr::filter(n_designation > 1)
        
        if (nrow(conflictos) == 0) {
          conflictos <- data.frame(
            Message = "No conflicts found"
          )
        }
        
        conflictos
        
      }, error = function(e) {
        message(
          "Error en dtFieldTraC: ",
          conditionMessage(e)
        )
        NULL
      })
      
      dtFieldTraC(conflictos)
      
    }, ignoreNULL = FALSE)
    
    output$transTableC <- DT::renderDT({
      req(input$selEnv) 
      # Obtener la tabla original
      df <- dtFieldTraC()
      if("environment"%in%colnames(df)){
        df <- df[df$environment %in% input$selEnv, ]
      }else{
        df<-as.data.frame("No conflicts found")
      }
      DT::datatable(df, extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))
      )
    }, server = FALSE)
    
    ##----------------------------------------------
    ##For select trait in tab trait grid
    ##----------------------------------------------
    observeEvent(data(), {
      req(data())
      ttQaRaw <- data()
      mydatat <- as.data.frame(ttQaRaw$data$pheno)
      ttQaRaw <- as.data.frame(ttQaRaw$metadata$pheno)
      #colnames(mydata) <- cgiarBase::replaceValues(colnames(mydata), Search = dtQaRaw$value, Replace = dtQaRaw$parameter )
      if(nrow(ttQaRaw)!=0){
        nmtr<-ttQaRaw[which(ttQaRaw[,1]=="trait"),2]
        if(length(which(nmtr %in% colnames(mydatat)))>0){
          traitQaRaw <- unique(nmtr)
          updateSelectInput(session, "selTrait",choices = traitQaRaw, selected = traitQaRaw[1])
        }
      }
    })
    
    output$plotGridTrait <- plotly::renderPlotly({
      req(data())
      result <- data()
      coordNames <- result$metadata$pheno[result$metadata$pheno$parameter %in% c("row","col"), "value"]
      rowcol <- length(which(result$metadata$pheno$parameter %in% c("row","col")))
      if (rowcol != 2) {
        return(plotly::plot_ly() %>% plotly::add_annotations(text = "No row and column information available for this field", x=0.5, y=0.5, showarrow=FALSE))
      }
      
      pheno <- result$data$pheno
      pheno <- pheno[, which(!duplicated(colnames(pheno)))]
      metaPheno <- result$metadata$pheno
      metaPheno <- metaPheno[metaPheno$parameter != "trait", ]
      colnames(pheno) <- cgiarBase::replaceValues(colnames(pheno), Search = metaPheno$value, Replace = metaPheno$parameter)
      pheno$value <- pheno[, input$selTrait]
      
      pheno2 <- pheno %>%
        dplyr::mutate(
          .r_num = suppressWarnings(as.numeric(as.character(row))),
          .c_num = suppressWarnings(as.numeric(as.character(col)))
        ) %>%
        dplyr::mutate(
          .r_num = ifelse(is.na(.r_num), as.numeric(as.factor(row)), .r_num),
          .c_num = ifelse(is.na(.c_num), as.numeric(as.factor(col)), .c_num)
        ) %>%
        dplyr::filter(!is.na(.r_num) & !is.na(.c_num))
      
      # valores globales para zmin/zmax (misma escala en todos)
      global_min <- min(pheno2$value, na.rm = TRUE)
      global_max <- max(pheno2$value, na.rm = TRUE)
      if (!is.finite(global_min)) global_min <- 0
      if (!is.finite(global_max)) global_max <- 1
      
      envs <- unique(pheno2$environment)
      plots_list <- vector("list", length(envs))
      
      for (i in seq_along(envs)) {
        sub <- pheno2[pheno2$environment == envs[i], ]
        if(!all(is.na(sub$value))){
        # tamaño de matriz
        rmin <- min(sub$.r_num); rmax <- max(sub$.r_num)
        cmin <- min(sub$.c_num); cmax <- max(sub$.c_num)
        nrowm <- rmax - rmin + 1
        ncolm <- cmax - cmin + 1
        
        # inicializar matrices
        mat <- matrix(NA_real_, nrow = nrowm, ncol = ncolm)
        txt <- matrix("", nrow = nrowm, ncol = ncolm)
        
        # mapas de índices
        r_idx <- sub$.r_num - rmin + 1
        c_idx <- sub$.c_num - cmin + 1
        
        # agregación por posición (promedio del trait si hay duplicados)
        pos_keys <- paste(r_idx, c_idx, sep = "_")
        agg_vals <- tapply(sub$value, pos_keys, mean, na.rm = TRUE)
        
        # llenar mat y txt usando keys
        if (length(agg_vals) > 0) {
          for (k in names(agg_vals)) {
            parts <- strsplit(k, "_")[[1]]
            ri <- as.integer(parts[1]); ci <- as.integer(parts[2])
            v <- agg_vals[[k]]
            mat[ri, ci] <- v
            # calculamos row/col num reales (pos -> valor)
            row_val <- rmin + ri - 1
            col_val <- cmin + ci - 1
            txt[ri, ci] <- paste0(
              "Env: ", envs[i], "<br>",
              input$selTrait, ": ", ifelse(is.na(v), "NA", formatC(v, digits = 1, format = "f")), "<br>",
              "Row: ", row_val, "<br>",
              "Col: ", col_val
            )
          }
        }
        
        # llenar celdas vacías del texto (missing)
        for (ri in seq_len(nrow(txt))) {
          for (ci in seq_len(ncol(txt))) {
            if (txt[ri, ci] == "") {
              row_val <- rmin + ri - 1
              col_val <- cmin + ci - 1
              txt[ri, ci] <- paste0(
                "Env: ", envs[i], "<br>",
                input$selTrait, ": NA<br>",
                "Row: ", row_val, "<br>",
                "Col: ", col_val
              )
            }
          }
        }
        
        # invertir filas para que row menor quede arriba
        #mat_plot <- mat[nrow(mat):1, , drop = FALSE]
        #txt_plot <- txt[nrow(txt):1, , drop = FALSE]
        mat_plot=mat
        txt_plot=txt
        
        # Solo la primera traza debe mostrar colorbar para tener una sola leyenda
        show_cb <- ifelse(i == 1, TRUE, FALSE)
        
        hm <- plotly::plot_ly(
          z = mat_plot,
          text = txt_plot,
          hovertemplate = "%{text}<extra></extra>",
          type = "heatmap",
          showscale = show_cb,
          zmin = global_min,
          zmax = global_max
        )
        
        # Si es la primera traza ajustamos el colorbar (posición/fuente)
        if (show_cb) {
          hm <- hm %>% plotly::colorbar(title = input$selTrait, len = 0.7, y = 0.8)
        }
        
        # Título simple en cada subplot (centra el título sobre la traza)
        hm <- hm %>% plotly::layout(
          #title = list(text = as.character(envs[i]), x = 0.5, xanchor = "center"),
          xaxis = list(title = "col", showticklabels = FALSE, ticks=""),
          yaxis = list(title = "row", showticklabels = FALSE, ticks=""),
          margin = list(t = 60)
        )
        
        plots_list[[i]] <- hm
      }else{
          hm1<-plotly::plot_ly() %>% plotly::add_annotations(text = paste0("No information available for field ",envs[i]), x=0.5, y=0.5, showarrow=FALSE)
          plots_list[[i]] <- hm1
        }
      }
      
      # combinar subplots
      ncols <- ifelse(length(plots_list) >= 2, 2, 1)
      out <- plotly::subplot(plots_list, nrows = ceiling(length(plots_list)/ncols),
                             shareX = FALSE, shareY = FALSE, titleX = TRUE, titleY = TRUE)
      
      # Si quieres mover ligeramente la colorbar final (opcional)
      out <- out %>% plotly::layout(margin = list(t = 80))
      
      out
    })
    
      
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
    ##fill environment for connectivity
    observeEvent(data(), {
      req(data())
      dtQaRaw1 <- data()
      mydata2 <- as.data.frame(dtQaRaw1$data$pheno)
      dtQaRaw1 <- as.data.frame(dtQaRaw1$metadata$pheno)
      colnames(mydata2) <- cgiarBase::replaceValues(colnames(mydata2), Search = dtQaRaw1$value, Replace = dtQaRaw1$parameter )
      if("environment" %in% colnames(mydata2)){
        EnvQaRaw1 <- unique(mydata2[,"environment"])
        updateSelectInput(session,"balselEnv",choices = sort(EnvQaRaw1), selected = sort(EnvQaRaw1))
      }
    })
    
    # Resumen por bloques y réplicas
    # Almacenar el resumen
    resumen_filtrado <- reactiveVal(data.frame())
    
    # Actualizar cuando cambien los datos o los ambientes seleccionados
    observeEvent(
      list(data(), input$selEnv),
      {
        
        result <- data()
        selEnv <- input$selEnv
        
        # Validar datos y selección
        if (is.null(result) ||
            is.null(result$data) ||
            is.null(result$data$pheno) ||
            is.null(result$metadata) ||
            is.null(result$metadata$pheno) ||
            is.null(selEnv) ||
            length(selEnv) == 0) {
          resumen_filtrado(data.frame())
          return()
        }
        
        res <- tryCatch({
          
          mydata <- as.data.frame(result$data$pheno)
          paramsPheno <- as.data.frame(result$metadata$pheno)
          
          if (!all(c("parameter", "value") %in%
                   names(paramsPheno))) {
            return(data.frame())
          }
          
          # Renombrar columnas según los metadatos
          idx <- match(names(mydata), paramsPheno$value)
          valid <- !is.na(idx)
          
          new_names <- names(mydata)
          new_names[valid] <- as.character(
            paramsPheno$parameter[idx[valid]]
          )
          names(mydata) <- make.unique(new_names)
          
          # Comprobar los factores de diseño
          required_cols <- c(
            "environment", "designation", "iBlock", "rep"
          )
          
          if (sum(required_cols %in% names(mydata)) < 3) {
            return(data.frame(
              Message = "No found factor of design (rep and block)"
            ))
          }
          
          # Seleccionar las columnas disponibles
          available_cols <- intersect(
            required_cols,
            names(mydata)
          )
          
          mydata <- mydata[, available_cols, drop = FALSE]
          
          # Filtrar ambientes
          if (!"environment" %in% names(mydata)) {
            return(data.frame())
          }
          
          df <- dplyr::filter(
            mydata,
            environment %in% selEnv
          )
          
          resumen_balanceado_bloques(df)
          
        }, error = function(e) {
          message(
            "Error en filter summary: ",
            conditionMessage(e)
          )
          NULL
        })
        
        # Guardar el resultado
        if (!is.null(res)) {
          resumen_filtrado(res)
        }
        
      },
      ignoreNULL = FALSE
    )
    
    # Tabla bloques: unimos conteo de plots con balance por bloque
    output$tabla_bloques <- DT::renderDT({
      req(data())
      result<-data()
      mydata <- as.data.frame(result$data$pheno)
      paramsPheno <- as.data.frame(result$metadata$pheno)
      colnames(mydata) <- cgiarBase::replaceValues(colnames(mydata), Search = paramsPheno$value, Replace = paramsPheno$parameter )
      fincols<-which(colnames(mydata)%in%c("environment","designation","iBlock")==T)
      if(length(fincols)==3){ 
        res <- resumen_filtrado()
        tabla_blocks <- res$balance_por_bloque |>
        dplyr::left_join(res$n_plot_block, by = intersect(names(res$balance_por_bloque), names(res$n_plot_block)))
      
        tabla_blocks <- tabla_blocks[ , !(names(tabla_blocks) %in% c("min_designation","max_designation"))]
        names(tabla_blocks)[names(tabla_blocks) == "balanced_block"] <- "block_test"
        tabla_blocks <- tabla_blocks[ , c(setdiff(names(tabla_blocks), "block_test"), "block_test")]
        DT::datatable(tabla_blocks, options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                   lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))) |>
          DT::formatStyle(
            "block_test",
            target = "row",
            backgroundColor = DT::styleEqual(c(TRUE, FALSE), c(NA, "salmon"))
          )
      }else{
        tabla_blocks<-data.frame("No found factor block")
        names(tabla_blocks)=c("No found factor block")
        DT::datatable(tabla_blocks, options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                   lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All'))))
      }
    })
    
    # Tabla réplicas
    output$tabla_replica <- DT::renderDT({
      req(data())
      result<-data()
      mydata <- as.data.frame(result$data$pheno)
      paramsPheno <- as.data.frame(result$metadata$pheno)
      colnames(mydata) <- cgiarBase::replaceValues(colnames(mydata), Search = paramsPheno$value, Replace = paramsPheno$parameter )
      fincols<-which(colnames(mydata)%in%c("environment","designation","rep")==T)
      if(length(fincols)==3){ 
        tabla <- resumen_filtrado()$resumen_por_replica
        tabla <- tabla[ , !(names(tabla) %in% c("min_designation","max_designation"))]
        names(tabla)[names(tabla) == "balanced_rep"] <- "rep_test"
        DT::datatable(tabla, options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                            lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))) |>
          DT::formatStyle(
            "rep_test",
            target = "row",
            backgroundColor = DT::styleEqual(c(TRUE, FALSE), c(NA, "lightblue"))
          )
      }else{
        tabla<-data.frame("No found factor rep")
        names(tabla)=c("No found factor rep")
        DT::datatable(tabla, options = list(dom = 'Blfrtip',scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                            lengthMenu = list(c(10,20,50,-1), c(10,20,50,'All')))) 
      }
    })
    
    
    output$heatmap_conectividad <- plotly::renderPlotly({
      req(input$balselEnv)
      req(data())
      result<-data()
      mydata <- as.data.frame(result$data$pheno)
      paramsPheno <- as.data.frame(result$metadata$pheno)
      colnames(mydata) <- cgiarBase::replaceValues(colnames(mydata), Search = paramsPheno$value, Replace = paramsPheno$parameter )
      fincols<-which(colnames(mydata)%in%c("environment","designation")==T)
      if(length(fincols)==2){ 
        mydata<-mydata[,which(colnames(mydata)%in%c("environment","designation")==T)]
        df <-  mydata|> dplyr::filter(environment %in% input$balselEnv)
      
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
        text = mat,        # el texto que se mostrará en cada celda
        hovertext = tooltip_mat,          # tooltip detallado al pasar el mouse
        hoverinfo = "text",
        texttemplate = "%{text}",         # usa el valor de `text`
        textfont = list(color = "black")  # color de la fuente dentro de los cuadros
      ) |>
        plotly::layout(
          title = paste0("Total of designation unique across environments: ",tgen),
          xaxis = list(title = "Environment"),
          yaxis = list(title = "Environment")
        )
      }else{
        plotly::plot_ly() %>% plotly::add_annotations(text = "No information available for this table", x=0.5, y=0.5, showarrow=FALSE)
      }
    })
    
    #################
    # Reactive table for environments to be included
    
    dtFieldMet <- reactiveVal(data.frame())
    
    observeEvent(data(), {
      result <- data()
      
      # Validar la estructura de entrada
      if (is.null(result) ||
          is.null(result$data) ||
          is.null(result$data$pheno) ||
          is.null(result$metadata) ||
          is.null(result$metadata$pheno)) {
        
        dtFieldMet(data.frame())
        return()
      }
      
      dtProvTable <- tryCatch({
        
        mydata <- as.data.frame(result$data$pheno)
        paramsPheno <- as.data.frame(result$metadata$pheno)
        
        # Validar metadatos
        if (!all(c("parameter", "value") %in% names(paramsPheno))) {
          return(data.frame())
        }
        
        # Renombrar columnas según los metadatos
        idx <- match(names(mydata), as.character(paramsPheno$value))
        valid <- !is.na(idx)
        
        new_names <- names(mydata)
        new_names[valid] <- as.character(paramsPheno$parameter[idx[valid]])
        
        names(mydata) <- make.unique(new_names)
        
        # Verificar la columna environment
        if (!"environment" %in% names(mydata)) {
          return(data.frame())
        }
        
        envs <- unique(as.character(mydata$environment))
        envs <- envs[!is.na(envs)]
        
        # Obtener las columnas asociadas a traits
        nmtr <- as.character(
          paramsPheno$value[paramsPheno$parameter == "trait"]
        )
        
        nmtr <- unique(nmtr[!is.na(nmtr) & nzchar(nmtr)])
        
        # Construir la tabla
        dtProvTable <- data.frame(
          Environments = envs,
          stringsAsFactors = FALSE
        )
        
        for (tr in nmtr) {
          dtProvTable[[tr]] <- rep(1, length(envs))
        }
        
        dtProvTable
        
      }, error = function(e) {
        message("Error en dtFieldMet: ", conditionMessage(e))
        NULL
      })
      
      # Actualizar el valor reactivo
      if (is.null(dtProvTable)) {
        cat("dtFieldMet error; conserve last value\n")
      } else {
        dtFieldMet(dtProvTable)
      }
    }, ignoreNULL = FALSE)
    
    
    # Mantener reactiveValues para los componentes existentes
    x <- reactiveValues(df = NULL)
    
    # Propagar los cambios de dtFieldMet a x$df
    observeEvent(dtFieldMet(), {
      df <- dtFieldMet()
      
      if (!is.null(df) && nrow(df) > 0) {
        x$df <- df
      }
    }, ignoreInit = FALSE)
    
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
    
    output$warningsExp <- renderUI({
      req(input$ConThr)
      
      if(input$ConThr!=30){
      div(
        class = "alert alert-warning",
        strong("The recommendation for connectivity threshold (30%)"),
        br(),
        paste("was changed for the user to:",input$ConThr)
      )
      }
    })
    # Detect which environments should be removed and why.
    # Returns a named list: environment -> character vector of reasons.
    detectEnvIssues <- function(object, dropDf, connThreshold = 30){
      paramsPheno <- as.data.frame(object$metadata$pheno)
      # exclude trait rows from the mapping: every trait maps to the parameter
      # "trait", which would create several duplicated "trait" columns and break
      # the downstream dplyr/tibble operations. Only design columns are renamed.
      mapParams <- paramsPheno[paramsPheno$parameter != "trait", , drop = FALSE]
      mydata <- as.data.frame(object$data$pheno)
      colnames(mydata) <- cgiarBase::replaceValues(colnames(mydata), Search = mapParams$value, Replace = mapParams$parameter)

      reasons <- list()
      addReason <- function(envs, r){
        for(e in envs){ e <- as.character(e); reasons[[e]] <<- unique(c(reasons[[e]], r)) }
      }

      if(!("environment" %in% colnames(mydata))) return(reasons)
      envs <- unique(as.character(mydata$environment))

      # 1) Plot conflicts: same environment/row/col holds more than one designation
      if(all(c("row","col","designation") %in% colnames(mydata))){
        conf <- mydata |>
          dplyr::group_by(environment, row, col) |>
          dplyr::summarise(n = dplyr::n_distinct(designation), .groups = "drop") |>
          dplyr::filter(n > 1)
        if(nrow(conf) > 0) addReason(unique(conf$environment), "Plot conflict")
      }

      # 2) & 3) Block / replication imbalance
      if("designation" %in% colnames(mydata) && (("iBlock" %in% colnames(mydata)) || ("rep" %in% colnames(mydata)))){
        dfbr <- mydata[, intersect(c("environment","designation","iBlock","rep"), colnames(mydata)), drop = FALSE]
        res <- try(resumen_balanceado_bloques(dfbr), silent = TRUE)
        if(!inherits(res, "try-error")){
          if("iBlock" %in% colnames(mydata)){
            blk <- res$balance_por_bloque
            if(!is.null(blk) && all(c("environment","balanced_block") %in% names(blk))){
              badBlk <- unique(blk$environment[blk$balanced_block %in% FALSE])
              if(length(badBlk) > 0) addReason(badBlk, "Block imbalance")
            }
          }
          if("rep" %in% colnames(mydata)){
            rept <- res$resumen_por_replica
            if(!is.null(rept) && all(c("environment","balanced_rep") %in% names(rept))){
              badRep <- unique(rept$environment[rept$balanced_rep %in% FALSE])
              if(length(badRep) > 0) addReason(badRep, "Replication imbalance")
            }
          }
        }
      }

      # 4) Low connectivity: share of an environment's designations also present
      #    in any other environment is below the threshold (default 30%).
      if("designation" %in% colnames(mydata) && length(envs) > 1){
        for(e in envs){
          di    <- unique(mydata$designation[mydata$environment == e])
          drest <- unique(mydata$designation[mydata$environment != e])
          conn  <- if(length(di) > 0) length(intersect(di, drest)) / length(di) * 100 else 0
          if(conn < connThreshold) {
            if (connThreshold==30) {
              addReason(e, paste0("Low connectivity (<", connThreshold, "%)"))
            }else{
              addReason(e, paste0("Low connectivity was decided by user(<", connThreshold, "%)") )  
              }
            }
        }
      }

      # 5) User-selected environments (any factor cell set to 0 in the Drop table)
      if(!is.null(dropDf) && nrow(dropDf) > 0){
        for(j in 1:nrow(dropDf)){
          cells <- suppressWarnings(as.numeric(unlist(dropDf[j, -1])))
          if(any(cells == 0, na.rm = TRUE)) addReason(dropDf[j, 1], "User selected")
        }
      }

      reasons
    }

    # Build the updated data object: whole-environment modifications for every
    # flagged environment, a modeling summary for the report, and a consolidation
    # of the Missingness + Consistency modifications under this qaExpDesign stamp.
    buildExpDesignResult <- function(object, dropDf, connThreshold = 30){
      analysisId <- as.numeric(Sys.time())
      paramsPheno <- as.data.frame(object$metadata$pheno)
      envCol    <- paramsPheno$value[paramsPheno$parameter == "environment"][1]
      traitCols <- paramsPheno$value[paramsPheno$parameter == "trait"]
      mydata    <- object$data$pheno

      reasons <- detectEnvIssues(object, dropDf, connThreshold)

      modList     <- list()
      summaryList <- list()
      if(length(reasons) > 0 && !is.na(envCol) && envCol %in% colnames(mydata)){
        for(env in names(reasons)){
          reasonStr <- paste(reasons[[env]], collapse = "; ")
          rowsEnv   <- which(as.character(mydata[, envCol]) == env)
          summaryList[[env]] <- data.frame(module = "qaExpDesign", analysisId = analysisId, trait = NA,
                                           environment = env, parameter = "dropEnvironment", value = reasonStr,
                                           stringsAsFactors = FALSE)
          for(tr in traitCols){
            if(tr %in% colnames(mydata) && length(rowsEnv) > 0){
              modList[[paste(env, tr)]] <- data.frame(module = "qaExpDesign", analysisId = analysisId, trait = tr,
                                                      reason = reasonStr, row = rowsEnv,
                                                      value = mydata[rowsEnv, tr], stringsAsFactors = FALSE)
            }
          }
        }
      }
      myMods    <- if(length(modList) > 0) do.call(rbind, modList) else NULL
      mySummary <- if(length(summaryList) > 0) do.call(rbind, summaryList) else NULL

      # Consolidate the latest Missingness + Consistency modifications under this stamp
      consolidated <- NULL
      prior <- object$modifications$pheno
      if(!is.null(prior) && nrow(prior) > 0){
        priorQA <- prior[prior$module %in% c("qaMissing", "qaConsistency"), , drop = FALSE]
        if(nrow(priorQA) > 0){
          keep <- do.call(rbind, lapply(split(priorQA, priorQA$module), function(d){
            d[d$analysisId == max(d$analysisId), , drop = FALSE]
          }))
          keep$analysisId <- analysisId  # re-stamp so they travel with the qaExpDesign label
          consolidated <- keep
        }
      }

      allMods <- do.call(rbind, Filter(Negate(is.null), list(object$modifications$pheno, myMods, consolidated)))
      object$modifications$pheno <- allMods

      # modeling summary (used by the dashboard report)
      if(!is.null(mySummary)){
        if(is.null(object$modeling)){ object$modeling <- mySummary }
        else { object$modeling <- rbind(object$modeling, mySummary[, colnames(object$modeling)]) }
      }

      status <- data.frame(module = "qaExpDesign", analysisId = analysisId, analysisIdName = NA)
      if(!is.null(object$status)){ object$status <- rbind(object$status, status[, colnames(object$status)]) }
      else { object$status <- status }

      object
    }
    
    ##############
    ##Tag results
    ################
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
          result <- buildExpDesignResult(object, dropDf=x$df, connThreshold=as.numeric(input$ConThr))
          conftab <- dtFieldTraC()
          cmod=c()
          if(c("\"No conflicts found\"")%in%names(conftab)){
              cmod<-paste("<font color=\"#008000\"><b>","No conflicts found", "</b></font>")
          }else{
              nenvconf<-unique(conftab$environment)
              for (x in 1:length(nenvconf)){
                cmod[x]<-paste("<font color=\"#FF0000\"><b>", nenvconf[x], "</b></font>", " have conflicts.")
              }
            }
          
          #shinyWidgets::show_alert(
          #  inputId = ns("statusConflicts"),
          #  text = HTML(cmod),
          #  title = "Review experimental design factors in environment:",
          #  type= "info",
          #  html= TRUE
          #)
          
          aid <- result$status$analysisId[length(result$status$analysisId)]
          if("analysisIdName" %in% colnames(result$status)){result$status$analysisIdName[nrow(result$status)] <- input$analysisIdName}
          data(result)
          # persist the shared object (carries qaMissing + qaConsistency + qaExpDesign)
          # so the combined dashboard report can be rendered and downloaded.
          # tempdir() is always writable and stable within the R session, which the
          # app working directory may not be (e.g. inside a container).
          try({
            save(result, file = file.path(tempdir(), "resultQaPhenoDashboard.RData"))
          }, silent = TRUE)
          cat(paste("QA Exp Design step with id:",as.POSIXct( aid, origin="1970-01-01", tz="GMT"),"saved."))
          output$outExp2 <- renderPrint({
            cat(paste("QA Exp Design step with id:",as.POSIXct( aid, origin="1970-01-01", tz="GMT"),"saved."))
          })
        }
        updateTabsetPanel(session, "tabsMain", selected = "outputTabs")
        shinybusy::remove_modal_spinner()
        
        if(!inherits(result,"try-error")) { # if all goes well in the run
          # ## Report tab
          output$reportQaPheno <- renderUI({
            HTML(markdown::markdownToHTML(knitr::knit(system.file("rmd","reportQaPhenoDashboard.Rmd",package="bioflow"), quiet = TRUE), fragment.only=TRUE))
          })
          
          output$downloadReportQaPheno <- downloadHandler(
            filename = function() {
              paste(paste0('qaPheno_dashboard_',gsub("-", "", as.integer(Sys.time()))), sep = '.', switch(
                "HTML", PDF = 'pdf', HTML = 'html', Word = 'docx'
              ))
            },
            content = function(file) {
              shinybusy::show_modal_spinner(spin = "fading-circle", text = "Generating Report...")
              
              src <- normalizePath(system.file("rmd","reportQaPhenoDashboard.Rmd",package="bioflow"))
              
              # temporarily switch to the temp dir, in case you do not have write
              # permission to the current working directory. The data object was
              # already saved to tempdir() by the run step, and the report looks it
              # up there, so no extra copy of the .RData is needed.
              owd <- setwd(tempdir())
              on.exit(setwd(owd))
              
              file.copy(src, 'report.Rmd', overwrite = TRUE)
              
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
          
          output$outExp2 <- renderPrint({
            cat(paste("QA step with id:",as.POSIXct( aid, origin="1970-01-01", tz="GMT"),"for trait",paste(input$traitOutqPhenoMultiple, collapse = ", "),"saved. Now you can proceed to perform detection of outlier by residuals."))
          })
          
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

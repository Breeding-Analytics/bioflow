#' qaPhenoApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_qaDataMissApp_ui  <- function(id){
  ns <- NS(id)
  tagList(
    
    shiny::mainPanel(width = 12,
                     tabsetPanel( id=ns("tabsMain"),
                       type = "tabs",
                       tabPanel(div(icon("book"), "Information") ,
                                br(),
                                column(width = 6,
                                       h1(strong(span("Phenotype QA/QC Module - Missingness", style="color:darkcyan"))),
                                       h2(strong("Data Status (wait to be displayed):")),
                                       uiOutput(ns("warningMessage")),
                                       tags$br(),
                                       shinyWidgets::prettySwitch( inputId = ns('launch'), label = "Load example dataset", status = "success"),
                                       tags$br(),
                                       img(src = "www/missd.png", height = 375, width = 450) # add an image
                                ),
                                column(width = 6,
                                       tags$body(
                                         h2(strong("Details")),
                                         p("The Missingness module identifies and summarizes missing information in phenotypic trial data across experimental 
                                           design variables and traits. It evaluates missingness by environment, replication, and block, detects potential 
                                           missingness patterns, and flags critical missing information that may affect downstream analyses. Users can review 
                                           records with missing values and distinguish observations intended for genomic prediction from records that should be 
                                           excluded from statistical analyses. The module generates warnings and summaries to support the preparation of an 
                                           analysis-ready dataset before Single Trial and Multi-Environment Analysis."),
                                         h2(strong("Software used")),
                                         p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
                                            Vienna, Austria. URL https://www.R-project.org/.")
                                       )
                                ),
                       ),
                       tabPanel(div(icon("arrow-right-to-bracket"), "Input steps"),
                                tabsetPanel(
                                  tabPanel(div(  icon("dice-one"), "Trait missingness", icon("arrow-right") ), # icon = icon("dice-one"),
                                           br(),
                                           h4("Percent of missing by Environment × Trait",style = "color: #008000; font-weight: bold;"),
                                           helpText(HTML("
                                            <p>This table acts as a <strong>Data Quality Matrix</strong>. It displays the percentage of missing values (data gaps) 
                                            for each specific characteristic (Trait) for each different testing Environment.</p><ul>
                                            <li><strong>0% (White/Low):</strong> Perfect data collection. Every data point was recorded.</li>
                                            <li><strong>30% (High):</strong> No measurements could be taken for that trait in that environment.</li></ul>
                                            <p><em>Use this to identify which field sites or traits suffered from logistical issues, weather damage, or sensor failures.</em></p>"
                                           )),
                                           DT::DTOutput(ns("env_trait_table")),
                                           br(),
                                           h4("Overall trait missingness",style = "color: #008000; font-weight: bold;"),
                                           helpText(HTML("
                                            <p>This table acts as a <strong>Data Quality Matrix</strong>. It displays the percentage of missing values (data gaps) 
                                            for each specific characteristic (Trait) across different testing Environment.</p><ul>
                                            <li><strong>0% (White/Low):</strong> Perfect data collection. Every data point was recorded.</li>
                                            <li><strong>30% (High):</strong> No measurements could be taken for that trait.</li></ul>
                                            <p><em>Use this to identify which field sites or traits suffered from logistical issues, weather damage, or sensor failures.</em></p>"
                                           )),
                                           DT::DTOutput(ns("trait_table"))
                                  ),
                                  tabPanel(div(  icon("dice-two"), "Missingness patterns", icon("arrow-right") ), # icon = icon("dice-one"),
                                           br(),
                                           h4("Missing data in the experimental design",style = "color: #008000; font-weight: bold;"),
                                           helpText(HTML("
                                            <p>This table acts as a <strong>Data Quality Matrix</strong>. It displays the percentage of missing values (data gaps) 
                                            for each specific characteristic (Trait) for each different testing Environment and the potential design variables that affect the next analysis.</p><ul>
                                            <li><strong>0% (White/Low):</strong> Perfect data collection. Every data point was recorded.</li>
                                            <li><strong>30% (High):</strong> No measurements could be taken for that trait.</li></ul>"
                                           )),
                                           DT::DTOutput(ns("design_trait_table")),
                                           br(),
                                           h4("Affected observations",style = "color: #008000; font-weight: bold;"),
                                           helpText(HTML("
                                            <p>This table display the potential design variables that are complete missing.</p>"
                                           )),
                                           DT::DTOutput(ns("design_rows"))
                                  ), 
                                  tabPanel(div(  icon("dice-three"), "Review & keep", icon("arrow-right") ), # icon = icon("dice-one"),
                                           br(),
                                           uiOutput( ns("warnings")),
                                           h4("Unphenotyped individuals",style = "color: #008000; font-weight: bold;"),
                                           helpText(HTML("
                                            <p>The individual designation that are complete missing in all the traits appears in the next table, 
                                                         if you have marker information or enought data for keep, please check the box.</p>"
                                           )),
                                           checkboxInput(ns("enable_gp"),"Keep selected individuals for genomic prediction",value = FALSE),
                                           br(),
                                           DT::DTOutput(ns("unphenotyped_table")),
                                           br(),
                                           h4("Environments to keep",style = "color: #008000; font-weight: bold;"),
                                           helpText(HTML("
                                            <p>According to the missing rules, next environments will keep to the next analysis.</p>"
                                           )),
                                           checkboxGroupInput(ns("keep_environments"), NULL,choices = NULL),
                                           h4("Traits to keep", style = "color: #008000; font-weight: bold;"),
                                           helpText(HTML("
                                            <p>According to the missing rules, next traits will keep to the next analysis.</p>"
                                           )),
                                           checkboxGroupInput(ns("keep_traits"), NULL, choices = NULL)
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
                                                         actionButton(ns("runMissClean"), "Tag factors", icon = icon("play-circle"))
                                                  ),
                                           ),textOutput(ns("outMiss"))
                                  ),
                                ) # end of tabset
                       ),# end of input panel
                     )
    ) # end mainpanel
  )
}


#' qaRawApp Server Functions
#'
#' @noRd
mod_qaDataMissApp_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    ############################################################################ clear the console
    hideAll <- reactiveValues(clearAll = TRUE)
    observeEvent(data(), {
      hideAll$clearAll <- TRUE
    })
    ############################################################################
    # show shinyWidgets until the user can use the module
    observeEvent(c(data()), {
      req(data())
      mappedColumns <- length(which(c("environment","designation","trait") %in% data()$metadata$pheno$parameter))
      if(mappedColumns == 3){
        golem::invoke_js('showid', ns('holder'))
      }else{
        golem::invoke_js('hideid', ns('holder'))
      }
    })
    
    ###########################################################################
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
      input$launch,
      if(length(input$launch) > 0){
        if (input$launch) {
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
        shinyWidgets::updatePrettySwitch(session, "launch", value = FALSE)
      }
    }, ignoreNULL = TRUE)
    
    # Almacenar el resultado del análisis
    
    results <- reactiveVal(NULL)
    
    observeEvent(data(), {
      
      d <- data()
      
      # 1. Esperar a que existan los datos
      if (is.null(d) ||
          is.null(d$metadata) ||
          is.null(d$metadata$pheno) ||
          is.null(d$data) ||
          is.null(d$data$pheno) ) {
        #message("[INFO] Esperando datos fenotípicos completos.")
        return()
      }
      
      metaQaMiss <- d$metadata$pheno
      QaMiss <- d$data$pheno
      
      # 2. Esperar a que las estructuras sean válidas
      if (!is.data.frame(metaQaMiss) ||
          !is.data.frame(QaMiss) ||
          !all(c("parameter", "value") %in% names(metaQaMiss)) ||
          nrow(metaQaMiss) == 0 ||
          nrow(QaMiss) == 0) {
        #message("[INFO] Metadatos o fenotipos todavía incompletos.")
        return()
      }
      
      # 3. Esperar a que estén los parámetros obligatorios
      required_params <- c("environment", "designation", "trait")
      
      params_present <- unique(as.character(metaQaMiss$parameter))
      
      if (!all(required_params %in% params_present)) {
        #message("[INFO] Esperando parámetros: ",paste(setdiff(required_params, params_present), collapse = ", "))
        return()
      }
      
      # 4. Validar los nombres de columnas
      selected_cols <- as.character(metaQaMiss$value)
      
      if (anyNA(selected_cols) ||
          any(!nzchar(selected_cols)) ||
          anyDuplicated(selected_cols) ||
          !all(selected_cols %in% names(QaMiss))) {
        #message("[INFO] Esperando a que los nombres de columnas coincidan.")
        return()
      }
      
      # 5. Preparar los datos
      trait_cols <- selected_cols[metaQaMiss$parameter == "trait"]
      #trait_cols <- input$traitMissSel
      
      if (length(trait_cols) == 0) {
        #message("[INFO] Todavía no hay traits seleccionados en los metadatos.")
        return()
      }
      
      design_vars <- as.character(
        metaQaMiss$parameter[metaQaMiss$parameter != "trait"]
      )
      
      QaMiss <- QaMiss[, selected_cols, drop = FALSE]
      
      design_idx <- metaQaMiss$parameter != "trait"
      colnames(QaMiss)[design_idx] <- design_vars
      
      # 6. Variables opcionales de diseño
      rep_col <- if ("rep" %in% design_vars) "rep" else NULL
      blk_col <- if ("iBlock" %in% design_vars) "iBlock" else NULL
      row_col <- if ("row" %in% design_vars) "row" else NULL
      col_col <- if ("col" %in% design_vars) "col" else NULL
      
      # 7. Ejecutar el análisis solo cuando los datos estén listos
      res <- tryCatch({
        cgiarPipeline::run_missingness_analysis(
          data = QaMiss,
          genotype_col = "designation",
          environment_col = "environment",
          trait_cols = trait_cols,
          design_vars = design_vars,
          replicate_col = rep_col,
          block_col = blk_col,
          row_col = row_col,
          column_col = col_col
        )
      }, error = function(e) {
        #message("[ERROR] Missingness analysis: ", conditionMessage(e))
        NULL
      })
      
      # 8. Guardar únicamente un resultado válido
      if (!is.null(res)) {
        results(res)
        #message("[INFO] Análisis de missingness actualizado.")
      }
      
    }, ignoreNULL = FALSE)
    
    
    #output$design_table <- DT::renderDT({
    #  res <- results()
      
    #  if (is.null(res) || is.null(res$design)) {
    #    return(NULL)
    #  }
      
    #  DT::datatable(res$design,
    #                options = list(pageLength = 10,scrollX = TRUE)
    #  )
    #})
    
    # ----------------------------------
    # Affected rows
    # ----------------------------------
    output$design_rows <- DT::renderDT({
      res <- results()
      if (is.null(res) || is.null(res$design_rows)) {
        return(NULL)
      }
      DT::datatable(res$design_rows,
                    options = list(pageLength = 10,scrollX = TRUE)
      )
    })
    # ----------------------------------
    # Environment × Trait
    # ----------------------------------
    output$env_trait_table <- DT::renderDT({
      res <- results()
      
      if (is.null(res) || is.null(res$env_trait)) {
        return(NULL)
      }
      
      DT::datatable(res$env_trait,
                    options = list(pageLength = 10,scrollX = TRUE)
      )
    })
    # ----------------------------------
    # Overall traits
    # ----------------------------------
    output$trait_table <- DT::renderDT({
      res <- results()
      
      if (is.null(res) || is.null(res$traits)) {
        return(NULL)
      }
      
      DT::datatable(res$traits,
                    options = list(pageLength = 10,scrollX = TRUE)
      )
    })
    # ----------------------------------
    # Rep / Block
    # ----------------------------------
    output$design_trait_table <- DT::renderDT({
      res <- results()
      
      if (is.null(res) || is.null(res$design_trait)) {
        return(NULL)
      }
      tt<- res$design_trait[order(res$design_trait$trait,res$design_trait$environment,res$design_trait$rep, res$design_trait$iBlock),]
      DT::datatable(tt,
                    options = list(pageLength = 10,scrollX = TRUE)
      )
    })
    # ----------------------------------
    # Unphenotyped
    # ----------------------------------
    output$unphenotyped_table <- DT::renderDT({
      res <- results()
      
      if (is.null(res) || is.null(res$unphenotyped)) {
        return(NULL)
      }
      
      DT::datatable(res$unphenotyped,
                    options = list(pageLength = 10,scrollX = TRUE)
      )
    })
    # ----------------------------------
    # Warning messages
    # ----------------------------------
    output$warnings <- renderUI({
      res <- results()
      
      if (is.null(res)) {
        return(NULL)
      }
      
      warnings <- list()
      
      # Verificar que existan las secciones esperadas
      env_info <- res$environment
      env_trait <- res$env_trait
      unphenotyped <- res$unphenotyped
      
      if (!is.null(env_info) &&
          isTRUE(env_info$missing)) {
        
        n_missing <- env_info$n_missing
        if (is.null(n_missing) || is.na(n_missing)) {
          n_missing <- 0
        }
        
        warnings <- c(warnings, list(
          div(
            class = "alert alert-warning",
            strong("Environment information missing"),
            br(),
            paste(
              n_missing,
              "observations have missing Environment information."
            ),
            br(),
            "G×E models will not be available."
          )
        ))
      }
      
      if (!is.null(env_trait) &&
          is.data.frame(env_trait) &&
          all(c("status") %in% names(env_trait))) {
        
        high <- env_trait[
          !is.na(env_trait$status) &
            env_trait$status == "HIGH",
          ,
          drop = FALSE
        ]
        
        if (nrow(high) > 0) {
          warnings <- c(warnings, list(
            div(
              class = "alert alert-danger",
              strong("High trait missingness detected"),
              br(),
              paste(
                nrow(high),
                "Environment × Trait combinations exceed the critical threshold."
              )
            )
          ))
        }
      }
      
      if (is.data.frame(unphenotyped) &&
          nrow(unphenotyped) > 0) {
        
        warnings <- c(warnings, list(
          div(
            class = "alert alert-info",
            strong("Potential unphenotyped individuals"),
            br(),
            paste(
              nrow(unphenotyped),
              "individuals have no phenotypic observations."
            ),
            br(),
            "Review whether they should be retained for genomic prediction."
          )
        ))
      }
      
      if (length(warnings) == 0) {
        return(div(
          class = "alert alert-success",
          strong("No major missingness issues detected.")
        ))
      }
      
      tagList(warnings)
    })
    
    # ----------------------------------
    # Environment choices
    # ----------------------------------
    observeEvent(data(), {
      d <- data()
      
      if (is.null(d) ||
          is.null(d$metadata) ||
          is.null(d$metadata$pheno) ||
          is.null(d$data) ||
          is.null(d$data$pheno)) {
        
        updateCheckboxGroupInput(
          session,
          "keep_environments",
          choices = character(0),
          selected = character(0)
        )
        return()
      }
      
      meta <- d$metadata$pheno
      pheno <- d$data$pheno
      
      if (!is.data.frame(meta) ||
          !all(c("parameter", "value") %in% names(meta)) ||
          !"environment" %in% meta$parameter) {
        return()
      }
      
      env_values <- as.character(
        meta$value[meta$parameter == "environment"]
      )
      
      env_values <- env_values[
        !is.na(env_values) & nzchar(env_values)
      ]
      
      if (length(env_values) == 0) {
        return()
      }
      
      envCol <- env_values[1]
      
      if (!envCol %in% names(pheno)) {
        return()
      }
      
      envs <- unique(as.character(pheno[[envCol]]))
      envs <- envs[!is.na(envs) & nzchar(envs)]
      
      # Conservar la selección actual.
      # Si el selector todavía no tiene selección, usar todos.
      old_selected <- isolate(input$keep_environments)
      
      if (is.null(old_selected)) {
        selected_envs <- envs
      } else {
        selected_envs <- intersect(old_selected, envs)
      }
      
      updateCheckboxGroupInput(
        session,
        "keep_environments",
        choices = envs,
        selected = selected_envs
      )
      
    }, ignoreNULL = FALSE)
    
    # ----------------------------------
    # Trait choices
    # ----------------------------------
    observe({
      req(data())
      metaQaMiss <- data()$metadata$pheno
      QaMiss <- data()$data$pheno
      trait_cols<-metaQaMiss$value[which(metaQaMiss$parameter%in%"trait"==T)]
      updateCheckboxGroupInput(session,"keep_traits",choices = trait_cols,selected = trait_cols)
    })
    
    # ============================================================
    # QA Missingness: ejecutar, persistir decisiones y devolver
    # resultados al módulo padre
    # ============================================================
    
    final_results <- reactiveVal(NULL)
    
    observeEvent(input$runMissClean,{
      # --------------------------------------------------------
      # 1. Validar datos y resultados originales
      # --------------------------------------------------------
      original_data <- data()
      res <- results()
      
      if (is.null(original_data)) {
        output$outMiss <- renderPrint({
          cat(
            "Please retrieve or load your phenotypic data ",
            "using the 'Data Retrieval' tab."
          )
        })
        return()
      }
      
      if (is.null(res) || is.null(res$data)) {
        output$outMiss <- renderPrint({
          cat("No valid analysis results are available.")
        })
        return()
      }
      
      if (!is.data.frame(res$data) ||
          !"environment" %in% names(res$data)) {
        output$outMiss <- renderPrint({
          cat(
            "The analysis results must contain a valid data frame ",
            "with an environment column."
          )
        })
        return()
      }
      
      if (is.null(original_data$metadata$pheno) ||
          !is.data.frame(original_data$metadata$pheno)) {
        output$outMiss <- renderPrint({
          cat("Phenotypic metadata are missing or invalid.")
        })
        return()
      }
      
      shinybusy::show_modal_spinner("fading-circle",text = "Processing...")
      
      tryCatch({
        
        # ------------------------------------------------------
        # 2. Capturar las decisiones confirmadas por el usuario
        # ------------------------------------------------------
        keep_envs <- as.character(
          input$keep_environments %||% character(0)
        )
        
        keep_traits_input <- as.character(
          input$keep_traits %||% character(0)
        )
        
        enable_gp <- isTRUE(input$enable_gp)
        
        # Crear una copia del objeto compartido.
        # No modificar results(), que conserva el análisis original.
        result <- original_data
        
        # Identificador de esta ejecución
        analysisId <- as.numeric(Sys.time())
        
        # ------------------------------------------------------
        # 3. Obtener los metadatos de rasgos
        # ------------------------------------------------------
        paramsPheno <- result$metadata$pheno
        
        env_values <- as.character(
          paramsPheno$value[
            paramsPheno$parameter == "environment"
          ]
        )
        
        env_values <- env_values[
          !is.na(env_values) & nzchar(env_values)
        ]
        
        envCol <- if (length(env_values) > 0) {
          env_values[1]
        } else {
          NA_character_
        }
        
        traitCols <- as.character(
          paramsPheno$value[
            paramsPheno$parameter == "trait"
          ]
        )
        
        traitCols <- unique(
          traitCols[!is.na(traitCols) & nzchar(traitCols)]
        )
        
        # ------------------------------------------------------
        # 4. Calcular la tabla final filtrada
        #    usando results(), no el objeto persistido data()
        # ------------------------------------------------------
        d <- res$data
        
        trait_cols <- intersect(
          traitCols,
          names(d)
        )
        
        keep_traits <- intersect(
          keep_traits_input,
          trait_cols
        )
        
        # Conservar únicamente los ambientes seleccionados.
        # Si no hay ambientes seleccionados, conservar cero filas.
        if (length(keep_envs) > 0) {
          d <- d[
            as.character(d$environment) %in% keep_envs,
            ,
            drop = FALSE
          ]
        } else {
          d <- d[0, , drop = FALSE]
        }
        
        # Conservar columnas de diseño y rasgos seleccionados.
        non_traits <- setdiff(
          names(d),
          trait_cols
        )
        
        selected_cols <- c(
          non_traits,
          keep_traits
        )
        
        d <- d[
          ,
          intersect(selected_cols, names(d)),
          drop = FALSE
        ]
        
        # Esta es la tabla que recibirá el módulo padre.
        filtered_data <- d
        
        # ------------------------------------------------------
        # 5. Preparar modificaciones de ambientes y rasgos
        # ------------------------------------------------------
        mydata <- result$data$pheno
        
        if (!is.data.frame(mydata)) {
          stop("The original phenotypic data are invalid.")
        }
        
        allEnvs <- if (
          length(envCol) == 1 &&
          !is.na(envCol) &&
          envCol %in% names(mydata)
        ) {
          unique(as.character(mydata[[envCol]]))
        } else {
          character(0)
        }
        
        allEnvs <- allEnvs[!is.na(allEnvs)]
        
        dropEnvs <- setdiff(
          allEnvs,
          keep_envs
        )
        
        # Registrar solamente rasgos que realmente existan
        # en los datos originales.
        traits_originales <- intersect(
          traitCols,
          names(mydata)
        )
        
        dropTraits <- setdiff(
          traits_originales,
          keep_traits
        )
        
        modList <- list()
        
        # ------------------------------------------------------
        # 5a. Ambientes eliminados: registrar todas sus filas
        #     para cada rasgo original
        # ------------------------------------------------------
        if (
          length(dropEnvs) > 0 &&
          length(envCol) == 1 &&
          !is.na(envCol) &&
          envCol %in% names(mydata)
        ) {
          for (e in dropEnvs) {
            
            rowsE <- which(
              as.character(mydata[[envCol]]) == e
            )
            
            if (length(rowsE) == 0) next
            
            for (tr in traits_originales) {
              
              modList[[paste("env", e, tr)]] <- data.frame(
                module = "qaMissing",
                analysisId = analysisId,
                trait = tr,
                reason = "dropEnvironment",
                row = rowsE,
                value = mydata[[tr]][rowsE],
                stringsAsFactors = FALSE
              )
            }
          }
        }
        
        # ------------------------------------------------------
        # 5b. Rasgos eliminados: registrar todas sus filas
        # ------------------------------------------------------
        if (length(dropTraits) > 0) {
          
          rowsAll <- seq_len(nrow(mydata))
          
          for (tr in dropTraits) {
            
            modList[[paste("trait", tr)]] <- data.frame(
              module = "qaMissing",
              analysisId = analysisId,
              trait = tr,
              reason = "dropTrait",
              row = rowsAll,
              value = mydata[[tr]],
              stringsAsFactors = FALSE
            )
          }
        }
        
        myMods <- if (length(modList) > 0) {
          do.call(rbind, modList)
        } else {
          NULL
        }
        # ------------------------------------------------------
        # 7. Persistir modificaciones en el objeto compartido
        # ------------------------------------------------------
        if (!is.null(myMods) && nrow(myMods) > 0) {
          
          result$modifications <- result$modifications %||% list()
          
          result$modifications$pheno <- rbind(
            result$modifications$pheno,
            myMods
          )
        }
        
        # ------------------------------------------------------
        # 8. Guardar las decisiones como parámetros del modelo
        # ------------------------------------------------------
        decisions <- data.frame(
          module = "qaMissing",
          analysisId = analysisId,
          trait = NA_character_,
          environment = NA_character_,
          parameter = c(
            "keepEnvironments",
            "keepTraits",
            "genomicPrediction"
          ),
          value = c(
            paste(keep_envs, collapse = ", "),
            paste(keep_traits_input, collapse = ", "),
            as.character(enable_gp)
          ),
          stringsAsFactors = FALSE
        )
        
        result$modeling <- rbind(
          result$modeling,
          decisions
        )
        
        # ------------------------------------------------------
        # 9. Registrar el estado del módulo
        # ------------------------------------------------------
        status <- data.frame(
          module = "qaMissing",
          analysisId = analysisId,
          analysisIdName = input$analysisIdName,
          stringsAsFactors = FALSE
        )
        
        result$status <- rbind(
          result$status,
          status
        )
        
        # ------------------------------------------------------
        # 10. Actualizar el objeto compartido
        # ------------------------------------------------------
        data(result)
        
        # ------------------------------------------------------
        # 11. Construir el resultado final para el módulo padre
        # ------------------------------------------------------
        final_results(
          list(
            data = filtered_data,
            
            decisions = list(
              environments = keep_envs,
              traits = keep_traits_input,
              genomic_prediction = enable_gp
            ),
            
            pipeline = res$pipeline
          )
        )
        
        # ------------------------------------------------------
        # 12. Mensaje de éxito y navegación
        # ------------------------------------------------------
        output$outMiss <- renderPrint({
          cat(
            "QA Missingness step with id:",
            as.POSIXct(
              analysisId,
              origin = "1970-01-01",
              tz = "GMT"
            ),
            "saved. Moving to the Experimental Design module."
          )
        })
        
        session$sendCustomMessage(
          "go_to_phen_tab",
          "PhenExpApp_tab"
        )
        
      }, error = function(e) {
        
        output$outMiss <- renderPrint({
          cat(
            "Error during QA Missingness processing:",
            conditionMessage(e)
          )
        })
        
      }, finally = {
        shinybusy::remove_modal_spinner()
      })
    },
    ignoreInit = TRUE
    )
    
    # ============================================================
    # Salida reactiva: mantener la interfaz del módulo
    # ============================================================
    return(
      reactive({
        final_results()
      })
    )
    
    
    
  })
}

## To be copied in the UI
# mod_qaDataMissApp_ui("qaDataMissApp_1")

## To be copied in the server
# mod_qaDataMissApp_server("qaDataMissApp_1")



    

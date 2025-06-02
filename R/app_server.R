
### START: OAuth2 utility functions ############################################

production_server <- TRUE

if (production_server) {
  validate_url  <- "https://serviceportal-authentication-prd.azurewebsites.net/TokenValidation/validate"
  authorize_url <- "https://serviceportal-authentication-prd.azurewebsites.net/connect/authorize"
  access_url    <- "https://serviceportal-authentication-prd.azurewebsites.net/connect/token"
  get_user_info <- "https://serviceportal-authentication-prd.azurewebsites.net/connect/userinfo"
  client_secret <- "SrVvPpmRdN&41@"
} else {
  validate_url  <- "https://serviceportal-authentication-tst.azurewebsites.net/TokenValidation/validate"
  authorize_url <- "https://serviceportal-authentication-tst.azurewebsites.net/connect/authorize"
  access_url    <- "https://serviceportal-authentication-tst.azurewebsites.net/connect/token"
  get_user_info <- "https://serviceportal-authentication-tst.azurewebsites.net/connect/userinfo"
  client_secret <- "secret"
}

oauth2_scope <- "openid profile"

modules <- data.frame(module = c("qaRaw","sta","mta","indexD","ocs","rgg","pgg","qaGeno","mas","gVerif","neMarker","popStrM","mtaLmms","qaFilter","qaDesign","qaConsist"),
                      moduleName = c("Quality Assurance Phenotypes","Single Trial Analysis","Multi Trial Analysis","Selection Indices","Optimal Cross Selection","Realized Genetic Gain","Predicted Genetic Gain","Quality Assurance Genotypes","Marker Assisted Selection","Genotype Verification","Number of Founders","Population Structure","Flexible MTA using LMMsolver","Trial Filtering","Design Filtering","Consistency Filtering"))

# NOTE: set cookie function
set_cookie <- function(session, name, value){
  stopifnot(is.character(name) && length(name) == 1)
  stopifnot(is.null(value) || (is.character(value) && length(value) == 1))
  if(is.null(value)) { value <- "" }

  cookie_options <- list(path = "/", same_site = "None", secure = TRUE)
  parts <- rlang::list2(!!name := value, !!!cookie_options)
  parts <- parts[!vapply(parts, is.null, logical(1))]

  names  <- names(parts)
  sep    <- ifelse(vapply(parts, isTRUE, logical(1)), "", "=")
  values <- ifelse(vapply(parts, isTRUE, logical(1)), "", as.character(parts))
  header <- paste(collapse = "; ", paste0(names, sep, values))
  hdr    <- list("Set-Cookie" = header)

  script_url <- session$registerDataObj(
    name = paste("type", "cookie", httr2:::base64_url_rand(), sep = "_"),
    data = httpResponse(headers = hdr),
    filterFunc = function(data, req) {data}
  )

  insertUI("body", where = "afterBegin", ui = tagList(tags$script(src = script_url)), immediate = TRUE, session = session)
}

### END: OAuth2 utility functions ##############################################

#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

future::plan(future::multisession)

options(shiny.maxRequestSize=8000*1024^2) # 8GB or 8,000Mb
users <- readRDS("users.rds")
app_server <- function(input, output, session) {

  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials( users  )
  )
  data <- reactiveVal()
  observe({data <- cgiarBase::create_getData_object()})
  # Your application server logic
  required_mapping <- c("pipeline", "stage", "year", "season", "timepoint",
                        "country", "location", "trial","study", "management","rep", "iBlock",
                        "row", "col", "designation", "gid", "entryType", "trait")

  required_mapping_weather <- c("environment", "latitude","longitude",
                               "year", "month","day","date", "trait")

  ## HOME
  mod_homeApp_server("homeApp_1")

  ## about tabs
  mod_aboutApp_server("aboutApp_1") # about the application
  mod_meetTheTeamApp_server("meetTheTeamApp_1") # about the team
  mod_contactUsApp_server("contactUsApp_1") # link to contact us
  mod_faqUsApp_server("faqUsApp_1")

  ## Glossary
  mod_glossary_server("glossary_1")

  ### START: OAuth2 flow mechanism #############################################
  use_login <- reactive({
    decision <- session$clientData$url_hostname == "bioflow.ebsproject.org"
    # decision <- TRUE
    return(decision)
  })

  observe({ if(use_login()){
    query <- parseQueryString(session$clientData$url_search)

    is_local <- Sys.getenv("SHINY_PORT") == ""

    # NOTE: check if we run it on localhost!
    if (is_local) {
      client_id     <- "shiny_local"
      redirect_uri  <- "http://localhost:1410"
    } else {
      client_id     <- "bioflow_ebs"
      redirect_uri  <- "https://bioflow.ebsproject.org"
    }

    # get the data object to access the user identifier
    temp <- data()

    if (!is.null(query$token) && is.null(temp$user)) {
      req  <- httr2::request(validate_url)
      req  <- httr2::req_body_raw(req, paste0('{"Token": "', query$token, '"}'), type = "application/json")

      resp <- httr2::req_perform(req)

      rslt <- httr2::resp_body_json(resp)

      if (rslt$message == "valid_token") {
        jwt <- strsplit(query$token, ".", fixed = TRUE)[[1]]

        payload <- jsonlite::base64url_dec(jwt[2]) |> rawToChar() |> jsonlite::fromJSON()

        updateQueryString(redirect_uri, mode = "replace", session = session)

        temp$user <- payload$email
        data(temp)

        removeModal()

        shinyWidgets::show_alert(title = paste("Welcome", temp$user), type = "success")
      } else {
        shinyWidgets::show_alert(title = "Invalid Token!", type = "error")
      }

      updateQueryString(redirect_uri, mode = "replace", session = session)
    }

    if (is.null(temp$user)) {
      showModal(modalDialog(
        tags$div(align = "center",
                 tags$img(src = "www/cgiar.png", height = 136, width = 480),
                 tags$h2(span("Biometrical Genetics Workflow (bioflow)", tags$a(href="https://www.youtube.com/playlist?list=PLZ0lafzH_UmclOPifjCntlMzysEB2_2wX", icon("youtube") , target="_blank"), style="color:darkcyan")),
                 tags$p(
                   "The OneCGIAR biometrical genetics workflow or pipeline has been built to access methods for understanding or using evolutionary forces",
                   "(mutation, gene flow, migration and selection) such as automatic state-of-the-art genetic evaluation (selection force) in decision-making.",
                   "Designed to be database agnostic, it can retrieve data from the available phenotypic-pedigree databases (EBS, BMS, BreedBase),",
                   "genotypic databases (GIGWA), and environmental databases (NASAPOWER), and carry the analytical procedures."
                 ),
        ),

        tags$hr(),

        fluidRow(
          column(width = 10, p(strong("Note:"), "You can sign in to the Bioflow Portal using your Service Portal login details.")),
          column(width = 2, actionButton("login", "Sign In")),
        ),
        tags$br(),
        fluidRow(
          column(width = 10, p("Alternatively, you can sign up by submitting a new request to create a Service Portal account before gaining access to the Bioflow Portal. Once you submit an account request, you will have to wait for it to be approved.")),
          column(width = 2, actionButton("register", "Sign Up")),
        ),

        size = "l",
        easyClose = FALSE,
        footer = NULL
      ))
    }

    observeEvent(input$cookies, {
      if (!is.null(query$code) && !is.null(query$state) && is.null(temp$user)) {
        shinybusy::show_modal_spinner()

        scriptoria_client <- httr2::oauth_client(
          id = client_id,
          secret = client_secret,
          token_url = access_url,
          name = "serviceportal"
        )

        # oauth_state   <- sub(".*oauth_state=([^;]*).*", "\\1", session$request$HTTP_COOKIE)
        # pkce_verifier <- sub(".*pkce_verifier=([^;]*).*", "\\1", session$request$HTTP_COOKIE)
        oauth_state   <- sub(".*oauth_state=([^;]*).*", "\\1", input$cookies)
        pkce_verifier <- sub(".*pkce_verifier=([^;]*).*", "\\1", input$cookies)

        # NOTE: checkpoint to interrupt Shiny app execution to inspect env vars!
        # browser()

        code <- httr2:::oauth_flow_auth_code_parse(query, oauth_state)

        token <- httr2:::oauth_client_get_token(client = scriptoria_client,
                                                grant_type = "authorization_code",
                                                code = query$code,
                                                state = query$state,
                                                code_verifier = pkce_verifier,
                                                redirect_uri = redirect_uri)

        jwt <- strsplit(token$access_token, ".", fixed = TRUE)[[1]]

        payload <- jsonlite::base64url_dec(jwt[2]) |> rawToChar() |> jsonlite::fromJSON()

        updateQueryString(redirect_uri, mode = "replace", session = session)

        temp$user <- payload$email
        data(temp)

        removeModal()

        shinyWidgets::show_alert(title = paste("Welcome", temp$user), type = "success")

        shinybusy::remove_modal_spinner()
      }
    })

    observeEvent(input$login, {
      shinybusy::show_modal_spinner()

      scriptoria_client <- httr2::oauth_client(
        id = client_id,
        secret = client_secret,
        token_url = access_url,
        name = "serviceportal"
      )

      oauth_state <- httr2:::base64_url_rand()

      pkce <- httr2::oauth_flow_auth_code_pkce()

      set_cookie(session, "oauth_state", oauth_state)
      set_cookie(session, "pkce_verifier", pkce$verifier)

      auth_url <- httr2::oauth_flow_auth_code_url(
        client = scriptoria_client,
        auth_url = authorize_url,
        redirect_uri = redirect_uri,
        state = oauth_state,
        auth_params = list(
          scope = oauth2_scope,
          code_challenge = pkce$challenge,
          code_challenge_method = pkce$method
        )
      )

      session$sendCustomMessage("redirect", auth_url)
    })

    observeEvent(input$register, {
      session$sendCustomMessage("redirect", "https://cgiar-service-portal-prd.azurewebsites.net/register")
    })
  }})
  ### END: OAuth2 flow mechanism ###############################################


  ### START: Service Portal KPI Dashboard ######################################

  observe({
    session$userData$url_hostname <- session$clientData$url_hostname
    session$userData$temp <- data()
  })

  # this code will be run after the client has disconnected
  session$onSessionEnded(function() {
    dashboard_key <- "Bioflow@wkH71dJ&qaT"
    req_origin    <- "https://bioflow.ebsproject.org"

    if (session$userData$url_hostname == "bioflow.ebsproject.org") {
      dashboard_url <- "https://cgiar-service-portal-prd.azurewebsites.net/api/BioflowUsage/AddBioflowUsage"
      user_email    <- session$userData$temp$user
    } else {
      dashboard_url <- "https://cgiar-service-portal-prd.azurewebsites.net/api/BioflowUsage/AddBioflowUsage"
      user_email    <- "offline"
    }

    status <- session$userData$temp$status

    for (i in 1:nrow(status)) {
      # avoid reporting usage of loaded data from a previous session
      if (status[i,"analysisId"] < as.numeric(Sys.time()) - 3600*24) next

      # prepare the data as an R list
      module_name <- ifelse(length(modules[modules$module == status[i,"module"], "moduleName"]) == 0,
                            "", modules[modules$module == status[i,"module"], "moduleName"])

      data <- list(
        Email      = user_email,
        Module     = status[i,"module"],
        ModuleName = module_name,
        Timestamp  = format(as.POSIXct(status[i,"analysisId"], tz = "GMT"), "%Y-%m-%dT%H:%M:%OS4")
      )

      # convert the data list to JSON
      message_json <- jsonlite::toJSON(data, auto_unbox = TRUE)

      # compute the HMAC SHA-256 signature in hexadecimal format
      signature <- digest::hmac(key = dashboard_key, object = data$Email, algo = "sha256", raw = FALSE)

      # build the request using httr2
      req <- httr2::request(dashboard_url)
      req <- httr2::req_headers(req, "HMAC-SIGNATURE" = signature, "Origin" = req_origin)
      req <- httr2::req_body_raw(req, message_json, type = "application/json")

      # perform the POST request and handle response
      response <- tryCatch({
        httr2::req_perform(req)
      }, error = function(e) {
        stop("HTTP request failed: ", e$message)
      })

      # check for HTTP errors
      if (httr2::resp_status(response) >= 400) {
        stop(sprintf("HTTP error! status: %s", httr2::resp_status(response)))
      }

      # warning(httr2::resp_body_string(response))

      Sys.sleep(1)
    }
  })

  ### END: Service Portal KPI Dashboard ########################################


  ## DATA extraction
  # mod_getData_server("getData_1", map = required_mapping, data = data, res_auth=res_auth)
  mod_getDataPheno_server("getDataPheno_1", map = required_mapping, data = data, res_auth = res_auth)
  mod_getDataGeno_server("getDataGeno_1", data = data, res_auth = res_auth)
  mod_getDataPed_server("getDataPed_1", data = data, res_auth = res_auth)
  mod_getDataWeather_server("getDataWeather_1", map = required_mapping_weather, data = data, res_auth = res_auth)
  mod_getDataQTL_server("getDataQTL_1", data = data, res_auth = res_auth)

  mod_bindObjectApp_server("bindObjectApp_1", data = data, res_auth=res_auth)

  ## QUALITY ASSURANCE tabs
  mod_qaPhenoApp_server("qaPhenoApp_1", data = data)
  mod_qaGenoApp_server("qaGenoApp_1",data = data)
  #mod_qaPedApp_server("qaPedApp_1",data = data)

  ## DATA TRANSFORMATIONS
  mod_traitTransformApp_server("traitTransformApp_1", data = data)
  mod_bindObjectApp_server("bindObjectApp_1",data = data, res_auth=res_auth)
  mod_singleCrossGenoApp_server("singleCrossGenoApp_1",data = data)
  mod_expDesignEditApp_server("expDesignEditApp_1", data = data )
  mod_filterPhenoApp_server("filterPhenoApp_1", data = data)

  ## DATA CONSISTENCY
  mod_dataConsistApp_server("dataConsistApp_1", data = data)

  # SELECTION - genetic evaluation
  mod_staApp_server("staApp_1", data = data) # single trial analysis
  mod_qaStaApp_server("qaStaApp_1",data = data) # model-based QA
  mod_oftStaApp_server("oftStaApp_1",data = data) # OFT report
  mod_mtaLMMsolveApp_server("mtaLMMsolveApp_1",data = data)
  mod_mtaASREMLApp_server("mtaASREMLApp_1", data = data)
  # mod_mtaApp_server("mtaApp_1",data = data) # multi-trial analysis
  # mod_mtaExpApp_server("mtaExpApp_1", data = data) # mta flexible approach
  # mod_mtaCrossValApp_server("mtaCrossValApp_1") # cross validation for mta module
  mod_indexDesireApp_server("indexDesireApp_1", data = data) # selection indices (Desire)
  mod_indexBaseApp_server("indexBaseApp_1", data = data) # selection indices (Base)
  mod_ocsApp_server("ocsApp_1", data = data) # optimal cross selection
  # SELECTION - selection history
  mod_rggApp_server("rggApp_1", data = data) # realized genetic gain
  mod_pggApp_server("pggApp_1", data = data) # predicted genetic gain
  mod_selSignApp_server("selSignApp_1")

  # MUTATION - mutation discovery
  mod_gwasqkApp_server("gwasqkApp_1", data = data) # GWAS Q+K method
  # MUTATION - mutation history
  mod_mutatioRateApp_server("mutatioRateApp_1") # mutation rate

  # GENE FLOW AND DRIFT - frequency-based selection
  mod_masApp_server("masApp_1", data = data) # MAS
  mod_hybridityApp_server("hybridityApp_1", data = data) # hybridity test
  # mod_neApp_server("neApp_1", data = data) # effective size
  # GENE FLOW AND DRIFT - gene flow history
  mod_PopStrApp_server("PopStrApp_1", data = data) # populationn structure
  # mod_poolFormApp_server("poolFormApp_1")
  mod_poolFormApp_server("poolFormApp_1") # pool formation
  mod_subsetSelApp_server("subsetSelApp_1") # subset selection

  ## SAVE results tab
  mod_saveData_server("saveData_1", data = data, res_auth=res_auth)

  ## DASHBOARDS
  mod_abiDashboard_server("abiDashboard_1", data = data)
  mod_reportBuilder_server("reportBuilder_1", data = data)

  ## info section tabs
  mod_sectionInfoDRASApp_server("sectionInfoDRASApp_1")
  mod_sectionInfoQAApp_server("sectionInfoQAApp_1") # info for quality assurance
  mod_sectionInfoTransformApp_server("sectionInfoTransformApp_1") # info for data transformations
  mod_sectionInfoGEApp_server("sectionInfoGEApp_1") # info for genetic evaluation
  mod_sectionInfoSHApp_server("sectionInfoSHApp_1") # info for selection history
  mod_sectionInfoGDApp_server("sectionInfoGDApp_1") # info for gene discovery
  mod_sectionInfoMHApp_server("sectionInfoMHApp_1") # info for mutation history
  mod_sectionInfoGFDApp_server("sectionInfoGFDApp_1") # info for gene frequency modules
  mod_sectionInfoGFDHApp_server("sectionInfoGFDHApp_1") # info for gene flow and drift history
  mod_sectionInfoDASHApp_server("sectionInfoDASHApp_1") # infor for dashboard creation

  ## agronomic evaluation
  # mod_sectionInfoAEApp_server("sectionInfoAEApp_1")
  # mod_agrAnova_server("agrAnova_1") #anova for agronomic experiments

  ## predictive models
  # mod_tensorMLApp_server("tensorMLApp_1")

  ### START: Push Analysis Mechanism ###########################################

  # http://localhost:1410/?task=18b7d89cc61fef60e93973edd9d9df38&module=STA&domain=dev

  observeEvent(session$clientData$url_search, {

    query <- parseQueryString(session$clientData$url_search)

    # check if task id exists, and verify (sanitize) this md5 parameter ;-)
    if (is.character(query$task) &&
        is.character(query$domain) &&
        grepl("^[a-f0-9]{32}$", query$task) &&
        grepl("^[a-z_0-9\\.\\-]+$", query$domain)) {

      shinybusy::show_modal_spinner('fading-circle', text = 'Processing...')

      ### set up paws library ################################################

      # Sys.setenv("AWS_ACCESS_KEY_ID"     = "XXXXXXXXXXXXXXXXXXXX")
      # Sys.setenv("AWS_SECRET_ACCESS_KEY" = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
      # Sys.setenv("AWS_SESSION_TOKEN"     = "xxxxxxxxxxx...xxxxxxxxxxxxxxxxxxxxxxxxx=")

      # # to access AWS using a pre-configured SSO (Single Sign-On) profile (switch to paws R package?)
      Sys.setenv("AWS_PROFILE" = "bioflow")
      Sys.setenv("AWS_DEFAULT_REGION" = "ap-southeast-1")

      bucket_name <- "ebs-bioflow"

      ### get RData object file from EBS S3 clipboard ##########################

      task_id        <- query$task
      s3_object_path <- paste0(query$domain, "/", task_id, ".RData")

      s3 <- paws::s3()

      tryCatch({
        s3_download <- s3$get_object(
          Bucket = bucket_name,
          Key = s3_object_path
        )

        raw_con <- rawConnection(s3_download$Body)
        load(raw_con)
        close(raw_con)

        ## replace tables
        tmp <- data()
        if(!is.null(result$data)){tmp$data <- result$data}
        if(!is.null(result$metadata)){tmp$metadata <- result$metadata}
        if(!is.null(result$modifications)){tmp$modifications <- result$modifications}
        if(!is.null(result$predictions)){tmp$predictions <- result$predictions}
        if(!is.null(result$metrics)){tmp$metrics <- result$metrics}
        if(!is.null(result$modeling)){tmp$modeling <- result$modeling}
        if(!is.null(result$status)){tmp$status <- result$status}
        data(tmp) # update data with results

        shinybusy::remove_modal_spinner()
        shinyalert::shinyalert(title = "Success!", text = "Data successfully loaded from EBS.", type = "success")
      }, error = function(e) {
        shinybusy::remove_modal_spinner()
        shinyalert::shinyalert(title = "Failed!", text = "The task data object could not be found in the S3 bucket clipboard.", type = "error")
      })
    }

    if (!is.null(query$module) && query$module == "STA") {
      # "tabso" is the `id` of the `navbarPage` in the app_ui.R script
      # "staApp_tab" is the `value` of the `tabPanel` in the app_ui.R script

      updateNavbarPage(session, "tabso", selected = "staApp_tab")
    }
  })

  ### END: Push Analysis Mechanism #############################################

}


### START: OAuth2 utility functions ############################################

production_server <- TRUE

if (production_server) {
  authorize_url <- "https://serviceportal-authentication-prd.azurewebsites.net/connect/authorize"
  access_url    <- "https://serviceportal-authentication-prd.azurewebsites.net/connect/token"
  get_user_info <- "https://serviceportal-authentication-prd.azurewebsites.net/connect/userinfo"
  client_secret <- "SrVvPpmRdN&41@"
} else {
  authorize_url <- "https://serviceportal-authentication-tst.azurewebsites.net/connect/authorize"
  access_url    <- "https://serviceportal-authentication-tst.azurewebsites.net/connect/token"
  get_user_info <- "https://serviceportal-authentication-tst.azurewebsites.net/connect/userinfo"
  client_secret <- "secret"
}

oauth2_scope <- "openid profile"

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
  required_mapping <- c("pipeline", "stage", "year", "season",
                        "country", "location", "trial","study", "management","rep", "iBlock",
                        "row", "col", "designation", "gid", "entryType", "trait")

  ## HOME
  mod_homeApp_server("homeApp_1")

  ### START: OAuth2 flow mechanism #############################################
  use_login <- reactive({
    decision <- session$clientData$url_hostname == "shiny-analytics.ebsproject.org"
    # decision <- FALSE
    return(decision)
  })

  observe({ if(use_login()){
    observeEvent(input$login, {
      shinybusy::show_modal_spinner()

      local_server <- Sys.getenv("SHINY_PORT") == ""

      # NOTE: check if we run it on localhost!
      if (local_server) {
        client_id     <- "shiny_local"
        redirect_uri  <- "http://localhost:1410"
      } else {
        client_id     <- "shiny_test"
        redirect_uri  <- "https://shiny-analytics.ebsproject.org/bioflow"
      }

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

    observeEvent(input$cookies, {
      query <- parseQueryString(session$clientData$url_search)
      temp  <- data()

      if (!is.null(query$code) && !is.null(query$state)) {
        shinybusy::show_modal_spinner()

        local_server <- Sys.getenv("SHINY_PORT") == ""

        # NOTE: check if we run it on localhost!
        if (local_server) {
          client_id     <- "shiny_local"
          redirect_uri  <- "http://localhost:1410"
        } else {
          client_id     <- "shiny_test"
          redirect_uri  <- "https://shiny-analytics.ebsproject.org/bioflow"
        }

        scriptoria_client <- httr2::oauth_client(
          id = client_id,
          secret = client_secret,
          token_url = access_url,
          name = "serviceportal"
        )

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

        shinybusy::remove_modal_spinner()
      } else if (is.null(temp$user)) {
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
    })

    observeEvent(input$register, {
      session$sendCustomMessage("redirect", "https://cgiar-service-portal-prd.azurewebsites.net/register")
    })
  }})
  ### END: OAuth2 flow mechanism ###############################################

  ## DATA extraction
  # mod_getData_server("getData_1", map = required_mapping, data = data, res_auth=res_auth)
  mod_getDataPheno_server("getDataPheno_1", map = required_mapping, data = data, res_auth = res_auth)
  mod_getDataGeno_server("getDataGeno_1", data = data, res_auth = res_auth)
  mod_getDataPed_server("getDataPed_1", data = data, res_auth = res_auth)
  mod_getDataWeather_server("getDataWeather_1", data = data, res_auth = res_auth)
  mod_getDataQTL_server("getDataQTL_1", data = data, res_auth = res_auth)

  mod_bindObjectApp_server("bindObjectApp_1",data = data, res_auth=res_auth)

  ## QUALITY ASSURANCE tabs
  mod_qaRawApp_server("qaRawApp_1", data = data)
  mod_qaGenoApp_server("qaGenoApp_1",data = data)
  #mod_qaPedApp_server("qaPedApp_1",data = data)

  ## DATA TRANSFORMATIONS
  mod_traitTransformApp_server("traitTransformApp_1", data = data)
  mod_bindObjectApp_server("bindObjectApp_1",data = data, res_auth=res_auth)
  mod_singleCrossGenoApp_server("singleCrossGenoApp_1",data = data)
  mod_expDesignEditApp_server("expDesignEditApp_1", data = data )
  mod_filterPhenoApp_server("filterPhenoApp_1", data = data)

  ## DATA CONSISTENCY
  # mod_dataConsistPotatoApp_server("dataConsistPotatoApp_1", data = data)

  # SELECTION - genetic evaluation
  mod_staApp_server("staApp_1", data = data) # single trial analysis
  mod_qaStaApp_server("qaStaApp_1",data = data) # model-based QA
  mod_oftStaApp_server("oftStaApp_1",data = data) # OFT report
  mod_mtaApp_server("mtaApp_1",data = data) # multi-trial analysis
  mod_mtaExpApp_server("mtaExpApp_1", data = data)
  mod_indexDesireApp_server("indexDesireApp_1", data = data) # selection indices (Desire)
  mod_indexBaseApp_server("indexBaseApp_1", data = data) # selection indices (Base)
  mod_ocsApp_server("ocsApp_1", data = data) # optimal cross selection
  # SELECTION - selection history
  mod_rggApp_server("rggApp_1", data = data) # realized genetic gain
  mod_pggApp_server("pggApp_1", data = data) # predicted genetic gain

  # GENE FLOW AND DRIFT
  mod_PopStrApp_server("PopStrApp_1", data = data) # populationn structure

  ## SAVE results tab
  mod_saveData_server("saveData_1", data = data, res_auth=res_auth)

  ## DASHBOARDS
  mod_abiDashboard_server("abiDashboard_1", data = data)
  mod_reportBuilder_server("reportBuilder_1", data = data)

  ## about tabs
  mod_aboutApp_server("aboutApp_1") # about the application
  mod_meetTheTeamApp_server("meetTheTeamApp_1") # about the team
  mod_contactUsApp_server("contactUsApp_1") # link to contact us

  ## info section tabs
  mod_sectionInfoQAApp_server("sectionInfoQAApp_1") # info for quality assurance
  mod_sectionInfoTransformApp_server("sectionInfoTransformApp_1") # info for data transformations
  mod_sectionInfoGEApp_server("sectionInfoGEApp_1") # info for genetic evaluation
  mod_sectionInfoSHApp_server("sectionInfoSHApp_1") # info for selection history

}

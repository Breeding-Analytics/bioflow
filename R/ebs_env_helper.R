# EBS Environment Resolution Helper
#
# Resolves per-login EBS environment settings (BrAPI URL, OAuth endpoints and
# client credentials) from a JSON catalog instead of a hardcoded if/else chain,
# so that a single Bioflow deployment can serve many EBS environments.
#
# The catalog is provided to the container out of band (fetched from SSM at
# startup and written to a file). The path is injected via the ECS task
# definition:
#   EBS_ENV_CONFIG_PATH   path to the catalog JSON (default below)
#
# For backward compatibility with existing one-to-one deployments, when no
# catalog file is present the helper falls back to flat EBS_* env vars:
#   EBS_DOMAIN, EBS_BRAPI_URL, EBS_AUTHORIZE_URL, EBS_ACCESS_URL,
#   EBS_CLIENT_ID, EBS_CLIENT_SECRET

library(jsonlite)

# Default on-disk location the startup bootstrap writes the catalog to.
EBS_ENV_CONFIG_DEFAULT_PATH <- "/srv/shiny-server/ebs_environments.json"

#' Resolve the catalog file path.
#'
#' Uses EBS_ENV_CONFIG_PATH when set, otherwise the packaged default.
#'
#' @return A single character path (may not exist on disk).
#' @noRd
get_ebs_catalog_path <- function() {
  path <- Sys.getenv("EBS_ENV_CONFIG_PATH", "")
  if (nchar(path) == 0) {
    path <- EBS_ENV_CONFIG_DEFAULT_PATH
  }
  path
}

#' Read the EBS environment catalog.
#'
#' Reads the catalog JSON from the path in EBS_ENV_CONFIG_PATH (or the default
#' path) and parses it preserving list structure.
#'
#' @return A list with an `environments` element (a list of entries), or NULL
#'   when no catalog file is available.
#' @noRd
get_ebs_catalog <- function() {
  path <- get_ebs_catalog_path()

  if (!file.exists(path)) {
    return(NULL)
  }

  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

#' Resolve EBS environment settings from flat EBS_* env vars.
#'
#' Backward-compatible fallback for one-to-one deployments that still inject
#' credentials directly as environment variables.
#'
#' @param ebs_instance The instance segment parsed from the login URL.
#' @param ebs_domain The domain segment parsed from the login URL.
#' @return A named list of resolved settings, or NULL when the flat vars are
#'   not configured.
#' @noRd
resolve_ebs_env_from_vars <- function(ebs_instance, ebs_domain) {
  client_id <- Sys.getenv("EBS_CLIENT_ID", "")
  if (nchar(client_id) == 0) {
    return(NULL)
  }

  domain <- Sys.getenv("EBS_DOMAIN", "")
  if (nchar(domain) == 0) {
    domain <- ebs_domain
  }

  brapi <- Sys.getenv("EBS_BRAPI_URL", "")
  if (nchar(brapi) == 0) {
    brapi <- paste0("https://cbbrapi-", ebs_instance, ".", domain)
  }

  list(
    brapi         = brapi,
    authorize_url = Sys.getenv("EBS_AUTHORIZE_URL", ""),
    access_url    = Sys.getenv("EBS_ACCESS_URL", ""),
    client_id     = client_id,
    client_secret = Sys.getenv("EBS_CLIENT_SECRET", "")
  )
}

#' Find the catalog entry matching an instance/domain.
#'
#' Matches by exact domain first, then by instance membership.
#'
#' @param environments The list of catalog entries.
#' @param ebs_instance The instance segment parsed from the login URL.
#' @param ebs_domain The domain segment parsed from the login URL.
#' @return The matching entry (a list), or NULL when none matches.
#' @noRd
match_ebs_entry <- function(environments, ebs_instance, ebs_domain) {
  # Exact domain match first.
  for (entry in environments) {
    match_domain <- entry$match$domain
    if (!is.null(match_domain) && identical(match_domain, ebs_domain)) {
      return(entry)
    }
  }

  # Then instance membership.
  for (entry in environments) {
    match_instances <- entry$match$instance
    if (!is.null(match_instances) && ebs_instance %in% unlist(match_instances)) {
      return(entry)
    }
  }

  NULL
}

#' Resolve EBS environment settings for a login URL.
#'
#' Parses the instance and domain from the login URL exactly the way the
#' legacy hardcoded handler did, looks up the matching catalog entry (exact
#' domain, then instance membership) and builds the resolved settings. When no
#' catalog file is present it falls back to flat EBS_* env vars.
#'
#' The BrAPI URL is built as:
#'   - https://<brapi_prefix>.<domain>      when brapi_prefix is non-null
#'   - https://cbbrapi-<instance>.<domain>  when brapi_prefix is null
#'
#' @param url The EBS login URL entered by the user.
#' @return A named list with brapi, authorize_url, access_url, client_id and
#'   client_secret, or NULL when the instance cannot be recognized.
#' @noRd
resolve_ebs_env <- function(url) {
  ebs_instance <- sub("https?://([^\\.]+)\\.([^/]+).*", "\\1", url)
  ebs_domain   <- sub("https?://([^\\.]+)\\.([^/]+).*", "\\2", url)

  catalog <- get_ebs_catalog()

  if (is.null(catalog) || is.null(catalog$environments)) {
    # No catalog available: fall back to the legacy instance normalization and
    # flat EBS_* env vars so existing one-to-one deployments keep working.
    ebs_instance <- sub("(.*-)", "", ebs_instance)
    return(resolve_ebs_env_from_vars(ebs_instance, ebs_domain))
  }

  entry <- match_ebs_entry(catalog$environments, ebs_instance, ebs_domain)

  if (is.null(entry)) {
    # Mirror the legacy fallback branch: normalize the instance (strip any
    # leading "<something>-" prefix) and try instance membership again.
    ebs_instance <- sub("(.*-)", "", ebs_instance)
    entry <- match_ebs_entry(catalog$environments, ebs_instance, ebs_domain)
  }

  if (is.null(entry)) {
    return(NULL)
  }

  brapi_prefix <- entry$brapi_prefix
  if (!is.null(brapi_prefix) && nchar(brapi_prefix) > 0) {
    brapi <- paste0("https://", brapi_prefix, ".", ebs_domain)
  } else {
    brapi <- paste0("https://cbbrapi-", ebs_instance, ".", ebs_domain)
  }

  list(
    brapi         = brapi,
    authorize_url = entry$authorize_url,
    access_url    = entry$access_url,
    client_id     = entry$client_id,
    client_secret = entry$client_secret
  )
}

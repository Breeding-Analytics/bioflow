#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

#' Convert pre-EBS module IDs in a loaded analysis object
#' sta -> soa, mtaLmms -> moaLmms, mtaAsr -> moaAsr
#' @noRd
update_module_ids <- function(object) {
  if (!is.list(object)) return(object)
  old_new <- c(sta = "soa", mtaLmms = "moaLmms", mtaAsr = "moaAsr", MtaAsr = "moaAsr")
  fix <- function(x) {
    if (is.data.frame(x) && "module" %in% colnames(x)) {
      x$module <- as.character(x$module)
      hit <- x$module %in% names(old_new)
      x$module[hit] <- unname(old_new[x$module[hit]])
    }
    x
  }
  for (tb in names(object)) {
    if (is.data.frame(object[[tb]])) {
      object[[tb]] <- fix(object[[tb]])
    } else if (is.list(object[[tb]])) {
      object[[tb]] <- lapply(object[[tb]], fix)
    }
  }
  object
}

#' Check that an object is a usable Bioflow data object
#'
#' Used before saving and after loading RData files so that an empty object
#' (e.g. NULL left behind by a failed analysis) is never written or loaded.
#' @noRd
is_valid_bioflow_object <- function(object) {
  is.list(object) && is.list(object$data) && length(object$data) > 0
}

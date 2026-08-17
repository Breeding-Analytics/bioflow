#' helpers 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

#' Build a network plot of analysis dependencies
#'
#' @description Creates a ggplot-based network visualization showing
#'   the dependency graph between analyses in a bioflow object.
#'
#' @param statusDf The status data.frame from a bioflow object.
#' @param modelingDf The modeling data.frame from a bioflow object.
#'
#' @return A ggplot object representing the network, or NULL if no data.
#'
#' @noRd
build_network_plot <- function(statusDf, modelingDf) {
  xx <- statusDf
  yy <- modelingDf
  if (is.null(xx) || nrow(xx) == 0) return(NULL)

  # Determine if human-readable names are available
  existNames <- "analysisIdName" %in% colnames(xx)
  if (existNames) {
    nameLookup <- setNames(
      paste(xx$analysisIdName, as.character(as.POSIXct(as.numeric(xx$analysisId), origin = "1970-01-01", tz = "GMT")), sep = "_"),
      as.character(xx$analysisId)
    )
    xx$analysisIdName <- as.character(xx$analysisId)
  }

  # Merge status with modeling to get input-output relationships

  v <- which(yy$parameter == "analysisId")
  if (length(v) > 0) {
    yy <- yy[v, c("analysisId", "value")]
    zz <- merge(xx, yy, by = "analysisId", all.x = TRUE)
  } else {
    zz <- xx
    zz$value <- NA
  }

  if (existNames) {
    zz$analysisIdName <- cgiarBase::replaceValues(Source = zz$analysisIdName, Search = "", Replace = "?")
    zz$analysisIdName2 <- cgiarBase::replaceValues(Source = zz$value, Search = zz$analysisId, Replace = zz$analysisIdName)
    colnames(zz) <- cgiarBase::replaceValues(colnames(zz), Search = c("analysisIdName", "analysisIdName2"), Replace = c("outputId", "inputId"))
  } else {
    colnames(zz) <- cgiarBase::replaceValues(colnames(zz), Search = c("analysisId", "value"), Replace = c("outputId", "inputId"))
  }

  # Build a square adjacency matrix (one row/col per unique analysis node)
  mynames <- unique(na.omit(c(zz$outputId, zz$inputId)))
  if (length(mynames) == 0) return(NULL)

  X <- matrix(0, nrow = length(mynames), ncol = length(mynames))
  rownames(X) <- colnames(X) <- as.character(mynames)
  for (iRow in seq_len(nrow(zz))) {
    outId <- as.character(zz[iRow, "outputId"])
    inId  <- as.character(zz[iRow, "inputId"])
    if (!is.na(inId) && inId %in% mynames && outId %in% mynames) {
      X[outId, inId] <- 1
    }
  }

  # Translate node names to display names
  if (existNames) {
    rownames(X) <- ifelse(rownames(X) %in% names(nameLookup), nameLookup[rownames(X)], rownames(X))
    colnames(X) <- ifelse(colnames(X) %in% names(nameLookup), nameLookup[colnames(X)], colnames(X))
  } else {
    rownames(X) <- as.character(as.POSIXct(as.numeric(rownames(X)), origin = "1970-01-01", tz = "GMT"))
    colnames(X) <- as.character(as.POSIXct(as.numeric(colnames(X)), origin = "1970-01-01", tz = "GMT"))
  }

  # Assign module family per node (first match)
  nodeFamily <- zz$module[match(mynames, zz$outputId)]

  # Create network and plot
  n <- network::network(X, directed = FALSE)
  network::set.vertex.attribute(n, "family", nodeFamily)
  network::set.vertex.attribute(n, "importance", 1)
  e <- network::network.edgecount(n)
  network::set.edge.attribute(n, "type", sample(letters[26], e, replace = TRUE))
  network::set.edge.attribute(n, "day", sample(1, e, replace = TRUE))

  ggplot2::ggplot(n, ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
    ggnetwork::geom_edges(ggplot2::aes(color = family), arrow = ggplot2::arrow(length = ggnetwork::unit(6, "pt"), type = "closed")) +
    ggnetwork::geom_nodes(ggplot2::aes(color = family), alpha = 0.5, size = 5) +
    ggnetwork::geom_nodelabel_repel(ggplot2::aes(color = family, label = vertex.names),
                                    fontface = "bold", box.padding = ggnetwork::unit(1, "lines")) +
    ggnetwork::theme_blank() + ggplot2::ggtitle("Network plot of current analyses available")
}

#' Retrieve layer names from MODIS Swath products
#'
#' @description
#' Retrieve names of the single scientific datasets (SDS) associated with the
#' MODIS Atmospheric Profiles product (MOD/MYD_L2).
#'
#' @param x \code{character}, one or multiple .hdf filename(s).
#' @param type \code{character}, type of information to be returned.
#' \code{"raw"} provides some additional information on the single layers (e.g.,
#' number of rows and columns, bands, and data type), whereas \code{"name"}
#' returns parameters only that serve as input for \code{\link{getSwathSDS}}.
#'
#' @return A \code{character} vector holding SDS information.
#'
#' @seealso \code{\link{GDALinfo}}, \code{\link{getSwathSDS}}.
#'
#' @export listSwathSDS
#' @name listSwathSDS
listSwathSDS <- function(x, type = c("raw", "name")) {

  ## read sds information
  info <- suppressWarnings(rgdal::GDALinfo(x, returnScaleOffset = FALSE))
  sds <- attr(info, "subdsmdata")

  ## if type == "name", return sds names only
  if (type[1] == "name") {
    sds <- sds[grep("HDF4_SDS", sds)[1]:length(sds)]
    sds <- sds[seq(2, length(sds), 2)]
    sds <- sapply(strsplit(sds, " "), "[[", 2)
  }

  return(sds)
}

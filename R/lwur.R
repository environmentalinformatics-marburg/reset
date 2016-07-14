#' Longwave upward radiation
#'
#' @description
#' Longwave upward radiation model proposed by Allen \emph{et al.} (2007; see
#' References).
#'
#' @param lst \code{Raster} object, land surface temperature.
#' @param bse \code{Raster} object, broadband surface emissivity.
#' @param ... Further arguments passed on to \code{\link{overlay}}.
#'
#' @seealso \code{\link{surfaceEmissivity}}.
#'
#' @references
#' Allen RG, Tasumi M, Trezza R (2007) Satellite-Based Energy Balance for
#' Mapping Evapotranspiration with Internalized Calibration (METRIC) -- Model.
#' \emph{Journal of Irrigation and Drainage Engineering} 133(4), 395-406,
#' doi:10.1061/(ASCE)0733-9437(2007)133:4(395).
#'
#' @export lwur
#' @name lwur
lwur <- function(lst, bse, ...) {

  ## define constants
  sigma <- 5.67e-8 # Stefan-Boltzmann constant

  ## compute longwave upward radiation
  raster::overlay(bse, lst, fun = function(x, y) {
    sigma * x * y^4
  }, ...)
}

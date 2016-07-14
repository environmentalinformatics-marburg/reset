#' Longwave downward radiation
#'
#' @description
#' Longwave downward radiation model proposed by Allen \emph{et al.} (2007) and
#' Hwang \emph{et al.} (2013; currently not implemented).
#'
#' @param ea \code{Raster*} object, effective atmospheric emissivity, e.g.
#' derived from \code{\link{atmosphericEmissivity}}.
#' @param ta \code{Raster*} object, air (or land surface) temperature.
#' @param ... Further arguments passed on to \code{\link{overlay}}.
#'
#' @references
#' Allen RG, Tasumi M, Trezza R (2007) Satellite-Based Energy Balance for
#' Mapping Evapotranspiration with Internalized Calibration (METRIC) -- Model.
#' \emph{Journal of Irrigation and Drainage Engineering} 133(4), 395-406,
#' doi:10.1061/(ASCE)0733-9437(2007)133:4(395).
#'
#' Hwang K, Choi M, Lee SO, Seo JW (2013) Estimation of instantaneous and daily
#' net radiation from MODIS data under clear sky conditions: a case study in
#' East Asia. \emph{Irrigation Science} 31, 1173-1184,
#' \url{http://link.springer.com/article/10.1007\%2Fs00271-012-0396-3} (accessed
#' 2016-03-30).
#'
#' @seealso \code{\link{atmosphericEmissivity}}.
#'
#' @export lwdr
#' @name lwdr
lwdr <- function(ea, ta, ...) {

  ## define constants
  sigma <- 5.67e-8 # Stefan-Boltzmann constant

  ## compute longwave upward radiation
  raster::overlay(ea, ta, fun = function(x, y) {
    x * sigma * y^4
  }, ...)
}

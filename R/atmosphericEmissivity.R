#' Effective atmospheric emissivity
#'
#' @description
#' Compute the effective atmospheric emissivity proposed by Bastiaanssen (1995)
#' and used by Allen \emph{et al.} (2007) to derive the longwave downward
#' radiation.

#' @param tau Broadband atmospheric transmissivity, e.g. derived from
#' \code{\link{atmosTrans}}.
#' @param cf1,cf2 \code{numeric} multiplying factor (\code{cf1}) and exponent,
#' default to the values defined by Allen \emph{et al.} (2007).
#' @param ... Further arguments passed on to \code{\link{calc}}.
#'
#' @seealso \code{\link{lwdr}}.
#'
#' @references
#' Allen RG, Tasumi M, Trezza R (2007) Satellite-Based Energy Balance for
#' Mapping Evapotranspiration with Internalized Calibration (METRIC) -- Model.
#' \emph{Journal of Irrigation and Drainage Engineering} 133(4), 395-406,
#' doi:10.1061/(ASCE)0733-9437(2007)133:4(395).
#'
#' Bastiaanssen WGM (1995) Regionalization of surface flux densities and
#' moisture indicators in composite terrain: a remote sensing approach under
#' clear skies in Mediterranean climates. PhD dissertation, CIP Data Koninklijke
#' Bibliotheek, Den Haag, The Netherlands,
#' \url{http://library.wur.nl/WebQuery/groenekennis/918192} (accessed
#' 2016-03-30).
#'
#' @export atmosphericEmissivity
#' @name atmosphericEmissivity
atmosphericEmissivity <- function(tau, cf1 = 0.85, cf2 = 0.09, ...) {
  raster::calc(tau, fun = function(x) {
    cf1 * (-log(x))^cf2
  }, ...)
}

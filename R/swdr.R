#' Shortwave downward radiation
#'
#' @description
#' Shortwave downward radiation model proposed by Allen \emph{et al.} (2007).
#'
#' @param theta \code{Raster*} object, solar incidence angle.
#' @param d \code{numeric} inverse squared relative earth-sun distance or
#' \code{Date} object from which to derive it.
#' @param tau Broadband atmospheric transmissivity, e.g. derived from
#' \code{\link{atmosTrans}}.
#'
#' @references
#' Allen RG, Tasumi M, Trezza R (2007) Satellite-Based Energy Balance for
#' Mapping Evapotranspiration with Internalized Calibration (METRIC) -- Model.
#' \emph{Journal of Irrigation and Drainage Engineering} 133(4), 395-406,
#' \url{http://dx.doi.org/10.1061/(ASCE)0733-9437(2007)133:4(395)}
#' (accessed 2016-03-07).
#'
#' @seealso \code{\link{calcEarthSunDist}}, \code{\link{atmosTrans}}.
#'
#' @export swdr
#' @name swdr
swdr <- function(d, tau, theta) {

  ## define constants
  Gsc <- 1367 # solar constant

  ## if a date is supplied, compute relative squared earth-sun distance
  if (class(d) == "Date")
    d <- satellite::calcEarthSunDist(d, "Duffie")

  ## compute shortwave downward radiation
  Gsc * d * tau * cos(theta)
}

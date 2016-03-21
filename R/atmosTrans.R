#' Broadband atmospheric transmissivity
#'
#' Broadband atmospheric transmissivity outlined in Hwang \emph{et al.} (2013).
#'
#' @param Pa \code{Raster*} object, atmospheric pressure (kPa).
#' @param W \code{Raster*} object, water in the atmosphere (mm).
#' @param theta \code{Raster*} object, solar zenith angle over planar surfaces
#' (radians).
#' @param Kt \code{numeric}, turbidity coefficient, defaults to \code{1.0} for
#' clean air (Allen \emph{et al.}, 1998).
#'
#' @references
#' Allen RG, Pereira LS, Raes D, Smith M (1998) \emph{FAO Irrigation and
#' Drainage Paper} 56. Food and Agriculture Organization of the United Nations
#' (FAO), Rome. Available online at
#' \url{https://appgeodb.nancy.inra.fr/biljou/pdf/Allen_FAO1998.pdf} (accessed
#' 2016-03-07).
#'
#' Hwang K, Choi M, Lee SO, Seo JW (2013) Estimation of instantaneous and daily
#' net radiation from MODIS data under clear sky conditions: a case study in
#' East Asia. \emph{Irrigation Science} 31, 1173-1184,
#' \url{http://dx.doi.org/10.1007/s00271-012-0396-3} (accessed 2016-03-07).
#'
#' @export atmosTrans
#' @name atmosTrans
atmosTrans <- function(Pa, Kt = 1, theta, W) {

  ## taken from Hwang et al. (2013, p. 1177)
  0.35 + 0.627 * 10^((-0.00146*Pa)/(Kt*cos(theta)) - 0.075*(W/cos(theta))^0.4)
}

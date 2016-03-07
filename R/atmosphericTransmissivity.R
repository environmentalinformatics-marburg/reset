#' Broadband atmospheric transmissivity
#'
#' Broadband atmospheric transmissivity outlined in Hwang \emph{et al.} (2013).
#'
#' @param Pa Atmospheric pressure (kPa).
#' @param W Water in the atmosphere (mm).
#' @param theta Solar incidence angle.
#' @param Kt Turbidity coefficient, defaults to \code{1.0} for clean air (Allen
#' \emph{et al.}, 1998).
#'
#' @references
#' Allen, R.G., Pereira, L.S., Raes, D. and M. Smith (1998). \emph{FAO
#' Irrigation and Drainage Paper} 56. Food and Agriculture Organization of the
#' United Nations (FAO), Rome. Available online at
#' \url{https://appgeodb.nancy.inra.fr/biljou/pdf/Allen_FAO1998.pdf} (accessed
#' 2016-03-07).
#'
#' Hwang, K., Choi, M., Lee, S.O. and J.-W. Seo (2013). Estimation of
#' instantaneous and daily net radiation from MODIS data under clear sky
#' conditions: a case study in East Asia. \emph{Irrigation Science} 31,
#' 1173-1184, \url{http://dx.doi.org/10.1007/s00271-012-0396-3}
#' (accessed 2016-03-07).
#'
#' @export atmosphericTransmissivity
#' @name atmosphericTransmissivity
atmosphericTransmissivity <- function(Pa, Kt = 1, theta, W) {
  ## taken from Hwang et al. (2013, p. 1177)
  0.35 + 0.627 * 10^((-0.00146*Pa)/(Kt*cos(theta)) - 0.075*(W/cos(theta))^0.4)
}

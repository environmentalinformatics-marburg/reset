#' Interception evaporation
#'
#' @description
#' Calculate the interception evaporation and related parameters.
#'
#' @param rH Relative humidity in \%.
#' @param alpha \code{numeric}. Priestley and Taylor (1972) constant, defaults
#' to \code{1.26}.
#' @param delta Slope of the saturation vapor pressure curve in kPa / deg C,
#' typically derived from \code{\link{vaporPressureSat}}.
#' @param gamma \code{numeric}. Psychrometric constant, defaults to \code{0.066}
#' kPa / deg C as proposed by Fisher et al. (2008).
#' @param rnc Net radiation to the canopy in W^2 / m^2.
#'
#' @return
#' Interception evaporation in W^2 / m^2.
#'
#' @seealso
#' \code{\link{surfaceWetness}}, \code{\link{radNetCanopy}}.
#'
#' @references
#' Fisher JB, Tu KP, Baldocchi DD (2008) Global estimates of the
#' land--atmosphere water flux based on monthly AVHRR and ISLSCP-II data,
#' validated at 16 FLUXNET sites. Remote Sensing of Environment 112: 901--919,
#' doi:10.1016/j.rse.2007.06.025.

#' Priestley CHB, Taylor RJ (1972) On the assessment of surface heat flux and
#' evaporation using large scale parameters. Monthly Weather Review 100: 81--92,
#' doi:10.1175/1520-0493(1972)100<0081:OTAOSH>2.3.CO;2.
#'
#' @export evapoIntercept
#' @name evapoIntercept
evapoIntercept <- function(rH, alpha = 1.26, delta, gamma = 0.066, rnc) {
  fwet <- surfaceWetness(rH)
  fwet * alpha * (delta / (delta + gamma)) * rnc
}

#' @describeIn evapoIntercept Relative surface wetness.
#' @aliases surfaceWetness
#' @export surfaceWetness
surfaceWetness <- function(rH) {
  return(rH^4)
}

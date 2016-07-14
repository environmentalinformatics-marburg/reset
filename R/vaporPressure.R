#' (Saturation) vapor pressure
#'
#' @description
#' Calculate actual (Peng et al. 2006) and saturation vapor pressure (Allen and
#' Robinson 2007) from air temperature and, in the case of the former, relative
#' air humidity.
#'
#' @param ta Air temperature (deg C).
#' @param rh Relative humidity (\%).
#' @param slope \code{logical}. If \code{TRUE}, the slope of the saturation
#' vapor pressure curve is returned rather than the actual saturation vapor
#' pressure.
#'
#' @return
#' The actual (or saturation) vapor pressure (kPa) or, if \code{slope = TRUE},
#' the slope of the saturation vapor pressure curve (kPa / deg C).
#'
#' @references
#' Allen RG, Robinson CW (2007) Evapotranspiration and Consumptive Irrigation
#' Water Requirements for Idaho. University of Idaho Research and Extension
#' Center: Kimberly, Idaho, USA.
#'
#' Dingman LS (2008) Physical Hydrology. Waveland Press Inc.: Long Grove,
#' Illinois, USA.
#'
#' Jensen ME, Burman RD, Allen RG (1990) Evapotranspiration and Irrigation Water
#' Requirements. ASCE Manuals and Reports on Engineering Practices No. 70.
#' American Society of Civil Engineers: New York, New York, USA.
#'
#' Murray FW (1967) On the Computation of Saturation Vapor Pressure. Journal of
#' Applied Meteorology 6: 203--204, doi:10.1175/1520-0450(1967)006<0203:OTCOSV>2.0.CO;2.
#'
#' Peng G, Li J, Chen Y, Norizan AP, Tay L (2006) High-resolution Surface
#' Relative Humidity Computation Using MODIS Image in Peninsular Malaysia.
#' Chinese Geographical Science 16(3): 260--264, doi:10.1007/s11769-006-0260-6.
#'
#' Tetens O (1930) Ueber einige meteorologische Begriffe. Zeitschrift fuer
#' Geophysik 6: 297--309.
#'
#' @export vaporPressure
#' @name vaporPressure
vaporPressure <- function(ta, rh) {
  es <- vaporPressureSat(ta)
  rh * es
}

#' @describeIn vaporPressure Saturation vapor pressure in kPa.
#' @aliases vaporPressureSat
#' @export vaporPressureSat
vaporPressureSat <- function(ta, slope = FALSE) {
  es <- 0.6108 * 10^(17.27 * ta / (ta + 237.3))

  if (slope) {
    4098 * es / (ta + 237.3)^2
  } else {
    return(es)
  }
}

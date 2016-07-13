#' Net radiation to the canopy
#'
#' @description
#' Calculate net radiation to the canopy from overall net radiation and net
#' radiation to the soil which is, in turn, calculated from leaf area index.
#'
#' @param rn Net radiation.
#' @param rns Net radiation to the soil.
#' @param filename Output filename (optional).
#' @param ... Additional arguments passed on to \code{\link{writeRaster}}.
#'
#' @seealso
#' \code{\link{netRadiation}}, \code{\link{radNetSoil}}.
#'
#' @references
#' Fisher JB, Tu KP, Baldocchi DD (2008) Global estimates of the
#' land--atmosphere water flux based on monthly AVHRR and ISLSCP-II data,
#' validated at 16 FLUXNET sites. Remote Sensing of Environment 112: 901--919,
#' doi:10.1016/j.rse.2007.06.025.
#'
#' @export radNetCanopy
#' @name radNetCanopy
radNetCanopy <- function(rn, rns, filename = "", ...) {
  rst_rnc <- rn - rns

  if (nchar(filename) > 0)
    rst_rnc <- writeRaster(rst_rnc, filename, ...)

  return(rst_rnc)
}

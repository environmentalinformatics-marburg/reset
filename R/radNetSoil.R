#' Net radiation to the soil
#'
#' @description
#' Calculate net radiation to the soil from overall net radiation and leaf area
#' index.
#'
#' @param rn Net radiation.
#' @param krn Extinction coefficient, defaults to \code{0.6} as proposed by
#' Impens and Lemeur (1969; see References).
#' @param lai Leaf area index.
#' @param filename Output filename (optional).
#' @param ... Additional arguments passed on to \code{\link{writeRaster}}.
#'
#' @seealso
#' \code{\link{netRadiation}}.
#'
#' @references
#' Fisher JB, Tu KP, Baldocchi DD (2008) Global estimates of the
#' land--atmosphere water flux based on monthly AVHRR and ISLSCP-II data,
#' validated at 16 FLUXNET sites. Remote Sensing of Environment 112: 901--919,
#' doi:10.1016/j.rse.2007.06.025.
#'
#' Impens I, Lemeur R (1969) Extinction of net radiation in different crop
#' canopies. Theoretical and Applied Climatology 17(4): 403--412,
#' doi:10.1007\%2FBF02243377.
#'
#' @export radNetSoil
#' @name radNetSoil
radNetSoil <- function(rn, krn = 0.6, lai, filename = "", ...) {
  rst_rns <- rn * 10^(-krn * lai)

  if (nchar(filename) > 0)
    rst_rns <- writeRaster(rst_rns, filename, ...)

  return(rst_rns)
}

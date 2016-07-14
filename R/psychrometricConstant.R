#' Psychrometric constant
#'
#' @description
#' Calculate the psychrometric constant from the atmospheric pressure.
#'
#' @param Pa \code{numeric} or \code{Raster*} object, atmospheric pressure
#' (kPa).
#'
#' @return Same as input (kPa).
#'
#' @seealso \code{\link{potentialEvaporation}}.
#' @references
#' Allen RG, Pereira LS, Raes D, Smith M (1998) Crop evaporation -
#' Guidelines for computing crop water requirements. \emph{FAO Irrigation and
#' Drainage Paper} 56. Food and Agriculture Organization of the United Nations
#' (FAO), Rome. Available online at
#' \url{https://appgeodb.nancy.inra.fr/biljou/pdf/Allen_FAO1998.pdf} (accessed
#' 2016-03-07).
#'
#' @export psychrometricConstant
#' @name psychrometricConstant
psychrometricConstant <- function(Pa) {

  ## define constants
  lambda <- 2.45   # latent heat of vaporization
  cp <- 1.013e-3   # specific heat at constant pressure
  epsilon <- 0.622 # ratio molecular weight of water vapour to dry air

  ## compute psychrometric constant
  cp * Pa / (epsilon * lambda)
}

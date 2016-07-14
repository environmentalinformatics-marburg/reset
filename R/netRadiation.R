#' Net radiation
#'
#' @description
#' Compute the net radiation as the remainder of the 4-component land surface
#' energy budget (shortwave and longwave upward and downward radiation,
#' respectively).
#'
#' @param rsd \code{Raster*}, shortwave downward radiation.
#' @param rsu \code{Raster*}, shortwave upward radiation.
#' @param rld \code{Raster*}, longwave downward radiation.
#' @param es \code{Raster*}, broadband surface emissivity.
#' @param rlu \code{Raster*}, longwave upward radiation.
#' @param ... Further arguments passed on to \code{\link{overlay}}.
#'
#' @return A \code{Raster*} object holding information about net radiation.
#'
#' @seealso \code{\link{swdr}}, \code{\link{lwdr}},
#' \code{\link{surfaceEmissivity}}, \code{\link{lwur}}.
#'
#' @export netRadiation
#' @name netRadiation
netRadiation <- function(rsd, rsu, rld, es, rlu, ...) {
  raster::overlay(rsd, rsu, rld, es, rlu, fun = function(v, w, x, y, z) {
    v - w + y * x - z
  }, ...)
}

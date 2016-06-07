#' Soil-adjusted vegetation index
#'
#' @description
#' Calculate the soil-adjusted vegetation index (SAVI; Huete, 1988) from red and
#' near-infrared wavelengths.
#'
#' @param red \code{Raster*} object, red band.
#' @param nir \code{Raster*} object, near-infrared band.
#' @param L \code{numeric} canopy background adjustment factor.
#' @param cores Number of cores for parallel computing. If \code{cores > 1L},
#' a separate \code{filename} is required for each layer in \code{red} (and
#' \code{nir}).
#' @param filename Output filename (optional).
#' @param ... Additional arguments passed on to \code{\link{writeRaster}}.
#'
#' @return The calculated SAVI as \code{Raster*} object.
#'
#' @author Florian Detsch
#'
#' @seealso \code{\link{ndvi}}.
#'
#' @references
#' Huete, A.R. (1988). A Soil-Adjusted Vegetation Index (SAVI).
#' \emph{Remote Sensing of Environment} 25, 295-309,
#' doi:10.1016/0034-4257(88)90106-X. Available online at
#' \url{http://www.sciencedirect.com/science/article/pii/003442578890106X}
#' (accessed on 2016-03-02).
#'
#' @export savi
#' @name savi

savi <- function(red,
                 nir,
                 L = 0.5,
                 cores = 1L,
                 filename = "",
                 ...) {

  ## savi function
  ratio <- function(x, y) {
    (1 + L) * (y - x) / (y + x + L)
  }

  ## single-core version
  if (cores == 1L) {
    raster::overlay(red, nir, fun = ratio, filename = filename, ...)


  ## multi-core version
  } else {
    cl <- parallel::makePSOCKcluster(cores)

    parallel::clusterEvalQ(cl, library(raster))
    parallel::clusterExport(cl, c("red", "nir", "L", "ratio", "filename", "..."),
                            envir = environment())

    lst <- parallel::parLapply(cl, 1:(raster::nlayers(red)), function(i) {
      raster::overlay(red[[i]], nir[[i]], fun = ratio,
                      filename = filename[i], ...)
    })

    parallel::stopCluster(cl)
    raster::stack(lst)
  }

}

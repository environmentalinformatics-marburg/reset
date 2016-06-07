#' Normalized difference vegetation index
#'
#' @description
#' Calculate the normalized difference vegetation index (NDVI; Tucker, 1979)
#' from red and near-infrared wavelengths.
#'
#' @param red \code{Raster*} object, red band.
#' @param nir \code{Raster*} object, near-infrared band.
#' @param cores Number of cores for parallel computing. If \code{cores > 1L},
#' a separate \code{filename} is required for each layer in \code{red} (and
#' \code{nir}).
#' @param filename Output filename (optional).
#' @param ... Additional arguments passed on to \code{\link{writeRaster}}.
#'
#' @return The calculated NDVI as \code{Raster*} object.
#'
#' @author Florian Detsch
#'
#' @seealso \code{\link{savi}}.
#'
#' @references
#' Tucker, C.J. (1979). Red and photographic infrared linear combinations for
#' monitoring vegetation. \emph{Remote Sensing of Environment} 8, 127-150,
#' doi:10.1016/0034-4257(79)90013-0. Available online at
#' \url{http://www.sciencedirect.com/science/article/pii/0034425779900130}
#' (accessed on 2016-03-02).
#'
#' @export ndvi
#' @name ndvi
ndvi <- function(red,
                 nir,
                 cores = 1L,
                 filename = "",
                 ...) {

  ## ndvi function
  ratio <- function(x, y) {
    z <- (y - x) / (y + x)
    z[z[] > 1] <- 1
    z[z[] < -1] <- -1
    return(z)
  }

  ## single-core version
  if (cores == 1) {
    raster::overlay(red, nir, fun = ratio, filename = filename, ...)

  ## multi-core version
  } else {
    cl <- parallel::makePSOCKcluster(cores)

    parallel::clusterEvalQ(cl, library(raster))
    parallel::clusterExport(cl, c("red", "nir", "ratio", "filename", "..."),
                            envir = environment())

    lst <- parallel::parLapply(cl, 1:(raster::nlayers(red)), function(i) {
      raster::overlay(red[[i]], nir[[i]], fun = ratio,
                      filename = filename[i], ...)
    })

    parallel::stopCluster(cl)
    raster::stack(lst)
  }
}

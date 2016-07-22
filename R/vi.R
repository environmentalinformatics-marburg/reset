#' Various vegetation indices
#'
#' @description
#' Calculate various vegetation indices (VI) from red and near-infrared
#' wavelengths. Currently available methods include the Normalized Difference
#' Vegetation Index (Tucker 1979), the Soil-adjusted Vegetation Index (Huete
#' 1988), and the Wide Dynamic Range Vegetation Index (Gitelson 2004).
#'
#' @param red \code{Raster*} object, red band.
#' @param nir \code{Raster*} object, near-infrared band.
#' @param cores Number of cores for parallel computing. If \code{cores > 1L},
#' a separate \code{filename} is required for each layer in \code{red} (and
#' \code{nir}).
#' @param filename Output filename (optional).
#' @param ... Additional arguments passed on to \code{\link{writeRaster}}.
#' @param L \code{numeric} canopy background adjustment factor, defaults to
#' \code{0.5}.
#' @param ndvi \code{Raster*} object, e.g., derived from \code{\link{ndvi}}. If
#' specified, arguments \code{red} and \code{nir} are automatically ignored.
#' @param alpha \code{numeric} weighting coefficient, defaults to \code{0.2}.
#'
#' @return The calculated VI as \code{Raster*} object.
#'
#' @author Florian Detsch
#'
#' @references
#' Gitelson, A.A. (2004). Wide dynamic range vegetation index for remote
#' quantification of crop biophysical characteristics. \emph{Journal of Plant
#' Physiology} 161, 165-173, doi:10.1078/0176-1617-01176. Available online at
#' \url{http://www.sciencedirect.com/science/article/pii/S0176161704705726}
#' (accessed on 2016-07-22).
#'
#' Huete, A.R. (1988). A Soil-Adjusted Vegetation Index (SAVI).
#' \emph{Remote Sensing of Environment} 25, 295-309,
#' doi:10.1016/0034-4257(88)90106-X. Available online at
#' \url{http://www.sciencedirect.com/science/article/pii/003442578890106X}
#' (accessed on 2016-03-02).
#'
#' Tucker, C.J. (1979). Red and photographic infrared linear combinations for
#' monitoring vegetation. \emph{Remote Sensing of Environment} 8, 127-150,
#' doi:10.1016/0034-4257(79)90013-0. Available online at
#' \url{http://www.sciencedirect.com/science/article/pii/0034425779900130}
#' (accessed on 2016-03-02).
#'
#' @name vi
NULL

### ndvi -----

#' @describeIn vi Normalized Difference Vegetation Index (Tucker 1979)
#' @export ndvi
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


### savi -----

#' @describeIn vi Soil-adjusted Vegetation Index (Huete 1988)
#' @export savi
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


### wdrvi -----

#' @describeIn vi Wide Dynamic Range Vegetation Index (Gitelson 2004)
#' @export wdrvi
wdrvi <- function(red,
                  nir,
                  ndvi = NULL,
                  alpha = 0.2,
                  cores = 1L,
                  filename = "",
                  ...) {

  ## ndvi is specified
  if (!is.null(ndvi)) {

    # single-core version
    if (cores == 1L) {
      raster::calc(ndvi, fun = function(x) {
        ((alpha + 1) * x + (alpha - 1)) / ((alpha - 1) * x + (alpha + 1))
      }, filename = filename, ...)

    # multi-core version
    } else {
      cl <- parallel::makePSOCKcluster(cores)

      parallel::clusterEvalQ(cl, library(raster))
      parallel::clusterExport(cl, c("ndvi", "alpha", "filename", "..."),
                              envir = environment())

      lst <- parallel::parLapply(cl, 1:(raster::nlayers(ndvi)), function(i) {
        raster::calc(ndvi[[i]], fun = function(x) {
          ((alpha + 1) * x + (alpha - 1)) / ((alpha - 1) * x + (alpha + 1))
        }, filename = filename[i], ...)
      })

      parallel::stopCluster(cl)
      raster::stack(lst)
    }

  ## ndvi is not specified
  } else {

    # single-core version
    if (cores == 1L) {
      raster::overlay(red, nir, fun = function(x, y) {
        (alpha * y - x) / (alpha * y + x)
      }, filename = filename, ...)

    # multi-core version
    } else {
      cl <- parallel::makePSOCKcluster(cores)

      parallel::clusterEvalQ(cl, library(raster))
      parallel::clusterExport(cl, c("red", "nir", "alpha", "filename", "..."),
                              envir = environment())

      lst <- parallel::parLapply(cl, 1:(raster::nlayers(red)), function(i) {
        raster::overlay(red[[i]], nir[[i]], fun = function(x, y) {
          (alpha * y - x) / (alpha * y + x)
        }, filename = filename[i], ...)
      })

      parallel::stopCluster(cl)
      raster::stack(lst)
    }
  }
}

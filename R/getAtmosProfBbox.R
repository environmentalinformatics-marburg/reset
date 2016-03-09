#' Retrieve MOD/MYD07_L2 bounding box
#'
#' @description
#' Retrieve bounding box from the MODIS Atmospheric Profiles product
#' (MOD/MYD07_L2).
#'
#' @param x \code{character}, one or multiple .hdf filename(s).
#' @param cores \code{integer}, number of cores for parallel computing when
#' processing large amounts of data.
#'
#' @return
#' If \code{length(x) == 1}, a single \code{Extent} object, else a
#' \code{list} of \code{Extent} objects.
#'
#' @seealso \code{\link{GDALinfo}}.
#'
#' @export getAtmosProfBbox
#' @name getAtmosProfBbox
getAtmosProfBbox <- function(x, cores = 1L) {

  ## coordinate string patterns
  pattern <- paste0(c("WEST", "EAST", "SOUTH", "NORTH"), "BOUNDINGCOORDINATE")


  ### single core --------------------------------------------------------------

  if (cores == 1) {

    ## loop over files
    lst <- lapply(x, function(i) {
      # gdalinfo
      info <- suppressWarnings(rgdal::GDALinfo(i, returnScaleOffset = FALSE))
      # metadata
      meta <- attr(info, "mdata")
      # search for coordinates patterns in metadata
      crd <- meta[sapply(pattern, function(i) grep(i, meta))]

      # create 'extent'
      crd <- as.numeric(sapply(strsplit(crd, "="), "[[", 2))
      raster::extent(crd)
    })


    ### multi-core ---------------------------------------------------------------

  } else {

    ## parallelization
    if (cores > parallel::detectCores()) cores <- parallel::detectCores() - 1
    cl <- makePSOCKcluster(cores)

    jnk <- parallel::clusterEvalQ(cl, c(library(rgdal), library(raster)))
    parallel::clusterExport(cl, "x", envir = environment())

    ## loop over files
    lst <- parLapply(cl, x, function(i) {
      # gdalinfo
      info <- suppressWarnings(rgdal::GDALinfo(i, returnScaleOffset = FALSE))
      # metadata
      meta <- attr(info, "mdata")
      # search for coordinates patterns in metadata
      crd <- meta[sapply(pattern, function(i) grep(i, meta))]

      # create 'extent'
      crd <- as.numeric(sapply(strsplit(crd, "="), "[[", 2))
      raster::extent(crd)
    })

    ## deregister parallel backend
    parallel::stopCluster(cl)
  }

  ## return extents
  if (length(x) == 1)
    return(lst[[1]])
  else
    return(lst)
}

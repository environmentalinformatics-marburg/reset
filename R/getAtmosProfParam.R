#' Extract atmospheric profiles
#'
#' @description
#' Extract and, to a limited extent, process scientific datasets (SDS) from the
#' MODIS Atmospheric Profiles product (MOD/MYD07_L2).
#'
#' @param x \code{character}, one or multiple .hdf filename(s).
#' @param prm \code{character}, one or multiple SDS to be processed. See
#' \code{\link{getAtmosProfSds}} for valid layer names.
#' @param ext \code{Extent} or any other objects from which an \code{Extent} can
#' be derived (optional), passed on to \code{\link[raster]{crop}}.
#' @param dsn \code{character}, target folder for file output (optional). Note
#' that if not provided, all resulting images are stored 'in memory'.
#' @param cores \code{integer}, number of cores for parallel computing, see
#' \code{\link{getAtmosProfBbox}}.
#' @param verbose \code{logical}, determines whether to print additional
#' information to the console.
#'
#' @return A \code{list} of \code{length(x)}. Sub-entries per list entry
#' correspond to the target SDS layers specified in \code{prm}.
#'
#' @seealso \code{\link{GDALinfo}}.
#'
#' @export getAtmosProfParam
#' @name getAtmosProfParam
getAtmosProfParam <- function(x, prm = "Surface_Pressure", ext = NULL, dsn = "",
                              cores = 1L, verbose = FALSE) {

  ## retrieve bounding boxes
  bb <- getAtmosProfBbox(x, cores = cores)

  ## loop over files
  if (!is.list(bb)) bb <- list(bb)

  lst_out <- lapply(1:length(x), function(h) {

    ## status message
    if (verbose)
      cat(paste0("Commencing with file #", h, ":"), x[h], "\n")


    ### process parameters -----------------------------------------------------

    ## read metadata
    info <- suppressWarnings(rgdal::GDALinfo(x[h], returnScaleOffset = FALSE))
    meta <- attr(info, "mdata")
    sds <- attr(info, "subdsmdata")

    ## parallelization
    if (cores > parallel::detectCores()) cores <- parallel::detectCores() - 1
    cl <- parallel::makePSOCKcluster(cores)

    jnk <- parallel::clusterEvalQ(cl, c(library(rgdal), library(raster)))
    parallel::clusterExport(cl, c("x", "prm", "dsn", "sds", "bb", "h", "ext"),
                            envir = environment())


    ## loop over parameters
    lst_prm <- parallel::parLapply(cl, prm, function(i) {

      # identify relevant sds
      sds_prm <- sds[grep(i, sds)[1]]
      sds_prm <- sapply(strsplit(sds_prm, "="), "[[", 2)

      # retrieve scale and offset of current parameter
      info_prm <- suppressWarnings(rgdal::GDALinfo(sds_prm))
      meta_prm <- attr(info_prm, "mdata")
      scale_offset_prm <- sapply(c("^scale_factor", "^add_offset"), function(j) {
        prm <- meta_prm[grep(j, meta_prm)]
        as.numeric(strsplit(prm, "=")[[1]][2])
      })

      # single-layer bands
      if (info_prm[["bands"]] == 1) {
        # rasterize band
        rst_prm <- suppressWarnings(
          raster::raster(rgdal::readGDAL(sds_prm, as.is = TRUE, silent = TRUE))
        )

        # multi-layer bands
      } else {
        # rasterize band
        rst_prm <- suppressWarnings(
          raster::stack(rgdal::readGDAL(sds_prm, as.is = TRUE, silent = TRUE))
        )
      }

      # apply offset and scale factor
      rst_prm <- raster::calc(rst_prm, fun = function(x) {
        (x - scale_offset_prm[2]) * scale_offset_prm[1]
      })

      # set extent and crs
      raster::extent(rst_prm) <- bb[[h]]
      raster::projection(rst_prm) <- "+init=epsg:4326"

      # crop by reference extent (optional)
      if (!is.null(ext))
        rst_prm <- raster::crop(rst_prm, ext, snap = "out")


      ### file output (optional) -----------------------------------------------

      # create target folder and file
      if (dsn != "") {
        dir_prm <- paste0(dsn, "/", i)
        if (!dir.exists(dir_prm)) dir.create(dir_prm)

        # remove suffix (similar to Orcs::pureBasename)
        fls_prm <- basename(x[h])
        fls_prm <- unlist(strsplit(fls_prm, "\\."))
        fls_prm <- fls_prm[-length(fls_prm)]
        fls_prm <- paste(fls_prm, collapse = ".")

        fls_prm <- paste0(dir_prm, "/", fls_prm, "_", i)

        # write to file and return
        rst_prm <- raster::writeRaster(rst_prm, filename = fls_prm,
                                       format = "GTiff", overwrite = TRUE)
      }

      return(rst_prm)
    })

    ## deregister parallel backend
    parallel::stopCluster(cl)
    return(lst_prm)
  })
}

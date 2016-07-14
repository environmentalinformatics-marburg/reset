#' Extract data layers from MODIS Swath products
#'
#' @description
#' Extract and process scientific datasets (SDS) from MODIS Swath products.
#'
#' @param x \code{character}, one or multiple .hdf filename(s).
#' @param prm \code{character}, one or multiple SDS to be processed. See
#' \code{\link{listSwathSDS}} for valid layer names.
#' @param ext \code{Extent} or any other objects from which an \code{Extent} can
#' be derived, passed on to \code{\link[raster]{crop}}. The required coordinate
#' reference system is EPSG:4326 (see
#' \url{http://spatialreference.org/ref/epsg/wgs-84/}). Note that although the
#' usage of this argument is optional, specifying a reference extent results in
#' considerable speed gains, particularly when a \code{template} is provided.
#' @param template \code{Raster*} template with parameters that the target SDS
#' should be resampled to, see \code{\link{resample}}.
#' @param method \code{character}, the resampling method passed on to
#' \code{\link{resample}} if \code{template} is specified.
#' @param dsn \code{character}, target folder for file output (optional). Note
#' that if not provided, all resulting images are stored 'in memory'.
#' @param cores \code{integer}, number of cores for parallel computing, see
#' \code{\link{getSwathExtent}}.
#' @param verbose \code{logical}, determines whether to print additional
#' information to the console.
#'
#' @return A \code{list} of \code{length(x)}. Sub-entries per list entry
#' correspond to the target SDS layers specified in \code{prm}.
#'
#' @seealso \code{\link{GDALinfo}}.
#'
#' @export getSwathSDS
#' @name getSwathSDS
getSwathSDS <- function(x, prm, ext = NULL, template = NULL,
                        method = "bilinear", dsn = "", cores = 1L,
                        verbose = FALSE) {

  ## retrieve bounding boxes
  bb <- getSwathExtent(x, cores = cores)

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
    parallel::clusterExport(cl, c("x", "prm", "dsn", "sds", "bb", "h", "ext",
                                  "template", "method"), envir = environment())


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
      if (info_prm[["bands"]] == 1 | i == "Quality_Assurance_Near_Infrared") {

        sgr <- suppressWarnings(
          rgdal::readGDAL(sds_prm, as.is = TRUE, silent = TRUE)
        )

        # rasterize band
        rst_prm <- if (i == "Quality_Assurance_Near_Infrared") {
          raster::raster(t(sgr@data))
        } else {
          raster::raster(sgr)
        }

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

      # project and resample
      if (!is.null(template)) {
        if (!raster::compareCRS(rst_prm, template))
          rst_prm <- raster::projectRaster(rst_prm, template, method = method)

        rst_prm <- raster::resample(rst_prm, template, method = method)
      }


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

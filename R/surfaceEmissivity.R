if ( !isGeneric("surfaceEmissivity") ) {
  setGeneric("surfaceEmissivity", function(ndvi, lai, ...)
    standardGeneric("surfaceEmissivity"))
}
#' Compute broadband surface emissivity
#'
#' @description
#' Compute the broadband surface emissivity from the normalized difference
#' vegetation index (NDVI) and leaf area index (LAI) as proposed by Tasumi
#' (2003; see References).
#'
#' @param ndvi \code{numeric} or \code{Raster*}, NDVI.
#' @param lai \code{numeric} or \code{Raster*}, LAI.
#' @param ... In case of \code{Raster*} input, further arguments passed on to
#' \code{\link{overlay}}.
#'
#' @return The broadband surface emissivity in the same format as the input.
#'
#' @references
#' Tasumi (2003) Progress in operational estimation of regional
#' evapotranspiration using satellite imagery. PhD dissertation, University of
#' Idaho, Moscow, Idaho, USA.
#'
#' @seealso \code{\link{lwur}}, \code{\link{overlay}}.
#'
#' @examples
#' surfaceEmissivity(ndvi = 0.5, lai = 2)
#'
#' @export surfaceEmissivity
#' @name surfaceEmissivity
NULL

### 'numeric','numeric' method -----
#' @aliases surfaceEmissivity,numeric,numeric-method
#' @rdname surfaceEmissivity
setMethod("surfaceEmissivity",
          signature(ndvi = "numeric", lai = "numeric"),
          function(ndvi, lai) {

            if (any(is.na(ndvi), is.na(lai))) {
              return(NA)
            } else {
              if (ndvi > 0 & lai <= 3) {
                0.95 + 0.01 * lai
              } else if (ndvi > 0 & lai > 3) {
                0.98
              } else {
                0.985
              }
            }
          }
)

### 'Raster','Raster' method -----
#' @aliases surfaceEmissivity,Raster,Raster-method
#' @rdname surfaceEmissivity
setMethod("surfaceEmissivity",
          signature(ndvi = "Raster", lai = "Raster"),
          function(ndvi, lai, ...) {

            raster::overlay(ndvi, lai, fun = function(x, y) {
              num_x <- x[]; num_y <- y[]
              
              sapply(1:length(num_x), function(i) {
                surfaceEmissivity(num_x[i], num_y[i])
              })
            }, ...)
          }
)

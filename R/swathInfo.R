#' Retrieve Information from MODIS Swath Products
#'
#' @description
#' Set of helper functions to retrieve information from MODIS swath products.
#'
#' @param x \code{character}, one or multiple .hdf filename(s).
#' @param format \code{character}, target format passed on to
#' \code{\link{strftime}}.
#' @param as_date \code{logical}, determines whether or not to return a
#' \code{Date} object.
#' @param ... Further arguments passed on to underlying functions (see Value).
#'
#' @return
#' \itemize{
#' \item \code{getSwathDateTime}: if \code{format} is missing, a \code{POSIXlt}
#' object created from \code{\link{strptime}}; else a \code{character} object
#' from \code{\link{strftime}}.
#' \item \code{getSwathDate}: if \code{as_date = TRUE} (default), a \code{Date}
#' object created from \code{\link{as.Date}}; else a \code{character} object
#' with a leading 'A'.
#' \item \code{getSwathTime}: a \code{character} object in the form of '\%H\%S'.
#' }
#'
#' @examples
#' x <- "MOD05_L2.A2013001.0835.006.2015064204640.hdf"
#'
#' getSwathDate(x)                  # scan date as 'Date'
#' getSwathDate(x, as_date = FALSE) # scan date as 'character'
#'
#' getSwathTime(x) # scan time
#'
#' getSwathDateTime(x)                            # scan date and time as 'POSIXlt'
#' getSwathDateTime(x, format = "%Y-%m-%d %H:%S") # scan date and time as 'character'
#'
#' @name swathInfo
NULL

### return scan datetime -----

#' @describeIn swathInfo Return MODIS Swath scan date and time
#' @export getSwathDateTime
getSwathDateTime <- function(x, format, ...) {
  dtm <- paste(getSwathDate(x, FALSE), getSwathTime(x))
  dtm <- strptime(dtm, "A%Y%j %H%M")

  if (!missing(format)) dtm <- strftime(dtm, format, ...)
  return(dtm)
}

### return scan date -----

#' @describeIn swathInfo Return MODIS Swath scan date
#' @export getSwathDate
getSwathDate <- function(x, as_date = TRUE) {
  dt <- sapply(strsplit(basename(x), "\\."), "[[", 2)
  if (as_date) dt <- as.Date(dt, "A%Y%j")
  return(dt)
}

### return scan time -----

#' @describeIn swathInfo Return MODIS Swath scan time
#' @export getSwathTime
getSwathTime <- function(x) {
  sapply(strsplit(basename(x), "\\."), "[[", 3)
}

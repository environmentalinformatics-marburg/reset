#' Shortwave downward radiation
#'
#' Shortwave downward radiation model proposed by Allen \emph{et al.} (2007; see
#' References)
#'
#' @param g_sc \code{numeric}, solar constant.
#' @param theta Solar incidence angle.
#' @param d_r Inverse squared relative earth-sun distance.
#' @param tau_sw Broadband atmospheric transmissivity.
#'
#' @references
#' Allen, R.G., Tasumi, M. and R. Trezza (2007). Satellite-Based Energy Balance
#' for Mapping Evapotranspiration with Internalized Calibration (METRIC) --
#' Model. \emph{Journal of Irrigation and Drainage Engineering} 133(4), 395-406,
#' \url{http://dx.doi.org/10.1061/(ASCE)0733-9437(2007)133:4(395)}
#' (accessed 2016-03-07).
#'
#' @export rsd
#' @name rsd
rsd <- function(g_sc = 1367, theta, d_r, tau_sw) {

  return(g_sc)
}

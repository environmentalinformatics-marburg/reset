% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/radNetCanopy.R
\name{radNetCanopy}
\alias{radNetCanopy}
\title{Net radiation to the canopy}
\usage{
radNetCanopy(rn, rns, filename = "", ...)
}
\arguments{
\item{rn}{Net radiation.}

\item{rns}{Net radiation to the soil, e.g. derived from
\code{\link{radNetSoil}}.}

\item{filename}{Output filename (optional).}

\item{...}{Additional arguments passed on to \code{\link{writeRaster}}.}
}
\description{
Calculate net radiation to the canopy from overall net radiation and net
radiation to the soil which is, in turn, calculated from leaf area index.
}
\references{
Fisher JB, Tu KP, Baldocchi DD (2008) Global estimates of the
land--atmosphere water flux based on monthly AVHRR and ISLSCP-II data,
validated at 16 FLUXNET sites. Remote Sensing of Environment 112: 901--919,
doi:10.1016/j.rse.2007.06.025.
}
\seealso{
\code{\link{netRadiation}}, \code{\link{radNetSoil}}.
}

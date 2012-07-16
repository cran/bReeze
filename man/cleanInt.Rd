\encoding{UTF-8}
\name{cleanInt}
\alias{cleanInt}
\title{Internal function for clean}
\description{Internal function for \code{\link{clean}}}
\usage{cleanInt(dat, v.avg.min, v.avg.max, dir.clean, turb.clean, icing)}
\arguments{
  \item{dat}{Dataset to be cleaned.}
  \item{v.avg.min}{Lower limit for wind speeds as numeric value.}
  \item{v.avg.max}{Upper limit for wind speeds as numeric value.}
  \item{dir.clean}{If \code{TRUE}, faulty wind direction values (<0 or >360) are excluded.}
  \item{turb.clean}{If \code{TRUE}, turbulence intensity values are excluded for wind speeds lower then 4 m/s.}
  \item{icing}{If \code{TRUE}, wind direction values are excluded, if standard deviation of wind direction is 0, assuming icing.}
}
\value{Returns cleaned dataset.}
\author{Christian Graul}
\seealso{\code{\link{clean}}}
\keyword{methods}
\keyword{internal}
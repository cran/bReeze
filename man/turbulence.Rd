\encoding{UTF-8}
\name{turbulence}
\alias{turbulence}
\title{Calculation of turbulence intensity}
\description{Calculates turbulence intensity and mean wind speed for each given direction sector.}
\usage{
turbulence(mast, turb.set, dir.set, num.sectors=12, 
  bins=c(5, 10, 15, 20))
	}
\arguments{
  \item{mast}{Met mast object created by \code{\link{createMast}}.}
  \item{turb.set}{Set used for turbulence intensity as integer value (optional, if \code{dir.set} is given).}
  \item{dir.set}{Set used for wind direction as integer value (optional, if \code{turb.set} is given).}
  \item{num.sectors}{Number of wind direction sectors as integer value. One of \code{4}, \code{8}, \code{12} or \code{16}. Default is \code{12}.}
  \item{bins}{Wind speed bins as numeric vector or \code{NULL} if no classification is desired.}
}
\details{
Turbulence can be perceived as wind speed fluctuations on a relatively short time scale and it strongly depends on surface roughness, terrain features, as well as thermal effects. High turbulence should be avoided, since it is a main driver of fatigue loads and might decrease energy output. A measure of the overall level of turbulence, is the turbulence intensity \eqn{I}, which is defined as:
\deqn{I = \frac{\sigma}{\bar v}}{I = sigma / v}
where \eqn{\sigma}{sigma} is the standard deviation of wind speed -- usually measured over a 10-minutes period -- and \eqn{\bar v}{v} is the mean wind speed over this period. 
}
\value{Returns a data frame containing:
  \item{wind.speed}{Mean wind speed for each direction sector.}
  \item{total}{Total turbulence intensity for each direction sector.}
  \item{\dots}{Turbulence intensities per direction sector for each given wind speed bin.}
}
\references{
Albers, A. (2010) Turbulence and Shear Normalisation of Wind Turbine Power Curve. Proceedings of the EWEC 2010, Warsaw, Poland

Burton, T., Sharpe, D., Jenkins, N., Bossanyi, E. (2001) \emph{Wind Energy Handbook}. New York: Wiley

Langreder, W. (2010) Wind Resource and Site Assessment. In: Wei Tong (Ed.), Wind Power Generation and Wind Turbine Design, Chapter 2, p. 49--87, Southampton: WIT Press
}
\author{Christian Graul}
\seealso{\code{\link{createMast}}, \code{\link{plotTurbulence}}}
\examples{
# load and prepare data
data(winddata)
set40 <- createSet(height=40, v.avg=winddata[,2], v.std=winddata[,5],
dir.avg=winddata[,14])
set30 <- createSet(height=30, v.avg=winddata[,6], v.std=winddata[,9],
dir.avg=winddata[,16])
set20 <- createSet(height=20, v.avg=winddata[,10], v.std=winddata[,13])
ts <- formatTS(winddata[,1])
neubuerg <- createMast(time.stamp=ts, loc=NULL, desc=NULL, 
	set40, set30, set20	)
neubuerg <- clean(neubuerg)

# calculate turbulence intensity
neubuerg.ti <- turbulence(neubuerg, 1)

# use different datasets for turb.set and dir.set
neubuerg.ti.2 <- turbulence(mast=neubuerg, turb.set=1, dir.set=2)

# change number of direction sectors
neubuerg.ti.3 <- turbulence(mast=neubuerg, turb.set=1, num.sectors=4)

# compare output
neubuerg.ti
neubuerg.ti.2
neubuerg.ti.3


# calculate turbulence intensity for 1 m/s speed bins and without binning
neubuerg.ti.4 <- turbulence(mast=neubuerg, turb.set=1, bins=1:25)
neubuerg.ti.5 <- turbulence(mast=neubuerg, turb.set=1, bins=NULL)

# compare output
# note: bins exceeding maximum wind speed of the data dropped
neubuerg.ti.4
neubuerg.ti.5
}
\keyword{methods}

\encoding{UTF-8}
\name{frequency}
\alias{frequency}
\title{Calculation of frequency and mean wind speed}
\description{Calculates the frequency of occurrence and mean wind speed per wind direction sector.}
\usage{
frequency(mast, v.set, dir.set, num.sectors=12, 
  bins=c(5, 10, 15, 20))
}
\arguments{
  \item{mast}{Met mast object created by \code{\link{createMast}}.}
  \item{v.set}{Set used for wind speed as integer value (optional, if \code{dir.set} is given).}
  \item{dir.set}{Set used for wind direction as integer value (optional, if \code{v.set} is given).}
  \item{num.sectors}{Number of wind direction sectors as integer value. One of \code{4}, \code{8}, \code{12} or \code{16}. Default is \code{12}.}
  \item{bins}{Wind speed bins as numeric vector or \code{NULL} if no classification is desired.}
}
\details{
The directional frequency of occurrence is an important factor for the design of a wind project. The influence is clear for the arrangement of turbines in a wind farm, that should be perpendicular to the most frequent wind direction. But also single turbines are affected, e.g. by fatigue tower loads in most frequent directions or in directions with highest wind speeds.
}
\value{Returns a data frame containing:
  \item{wind.speed}{Mean wind speed per direction sector.}
  \item{total}{Frequency per direction sector.}
  \item{\dots}{Frequencies per direction sector, for each given wind speed bin.}
}
\references{
Brower, M., Marcus, M., Taylor, M., Bernadett, D., Filippelli, M., Beaucage, P., Hale, E., Elsholz, K., Doane, J., Eberhard, M., Tensen, J., Ryan, D. (2010) Wind Resource Assessment Handbook. http://www.awstruepower.com/wp-content/uploads/2012/01/10-30_wind-resource-handbook.pdf
}
\author{Christian Graul}
\seealso{\code{\link{createMast}}, \code{\link{plotFrequency}}}
\examples{
\dontrun{
# load and prepare data
data(winddata)
set40 <- createSet(height=40, v.avg=winddata[,2], dir.avg=winddata[,14])
set30 <- createSet(height=30, v.avg=winddata[,6], dir.avg=winddata[,16])
set20 <- createSet(height=20, v.avg=winddata[,10])
ts <- formatTS(winddata[,1])
neubuerg <- createMast(time.stamp=ts, loc=NULL, desc=NULL, 
	set40, set30, set20)
neubuerg <- clean(neubuerg)

# calculate frequency
neubuerg.freq <- frequency(neubuerg, 1)

# if only one of v.set and dir.set is given, 
# the dataset is assigned to both
neubuerg.freq.2 <- frequency(mast=neubuerg, v.set=1)
neubuerg.freq.3 <- frequency(mast=neubuerg, dir.set=1)

# compare output
neubuerg.freq
neubuerg.freq.2
neubuerg.freq.3


# use different datasets for wind speed and direction
neubuerg.freq.4 <- frequency(neubuerg, 3)	# error: no direction in dataset
neubuerg.freq.4 <- frequency(neubuerg, v.set=3, dir.set=2)

# compare output
neubuerg.freq
neubuerg.freq.4


# change number of direction sectors 
neubuerg.freq.5 <- frequency(neubuerg, v.set=1, num.sectors=4)
neubuerg.freq.6 <- frequency(neubuerg, v.set=1, num.sectors=16)

# compare output
neubuerg.freq.5
neubuerg.freq.6


# calculate frequency for 1 m/s speed bins and without binning
neubuerg.freq.7 <- frequency(neubuerg, v.set=1, bins=1:25)
neubuerg.freq.8 <- frequency(neubuerg, v.set=1, bins=0:25)	# same as above
neubuerg.freq.9 <- frequency(neubuerg, v.set=1, bins=NULL)

# compare output
# note: bins exceeding maximum wind speed of the data dropped
neubuerg.freq.7
neubuerg.freq.8
neubuerg.freq.9
}
}
\keyword{methods}

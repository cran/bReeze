\encoding{UTF-8}
\name{availability}
\alias{availability}
\title{Calculate availability for pairs of wind speed and direction}
\description{Calculates the availability for pairs of wind speed and wind direction of a met mast.}
\usage{availability(mast, v.set, dir.set)}
\arguments{
  \item{mast}{Met mast object created by \code{\link{createMast}}.}
  \item{v.set}{Set to be used for wind speed as integer value. Argument is optional -- if missing, all sets containing wind speed and wind direction data are used.}
  \item{dir.set}{Set to be used for wind direction as integer value. Argument is optional -- if missing, the set used for wind speed data will also be used for wind direction.}
}
\details{
The availability is the number of pairs of valid wind speed and valid wind direction data samples as a percentage of the total possible for the measurement period, i.e.:
\deqn{Availability = \frac{N(v_{valid} \wedge dir_{valid})}{N}}{Availability = N(v_valid & dir_valid) / N}
where \eqn{N} is the total possible number of samples and \eqn{v_{valid} \wedge dir_{valid}}{v_valid & dir_valid} is a pair of valid wind speed and direction data.

Causes of invalid or missing data are manifold. Typical causes are icing and defects of sensors or other measurement equipment.
}
\value{
  Returns a list of sub lists for each pair of \code{v.set}/\code{dir.set} containing:
  \item{total}{Total availability (\%), effective period (days) and total period (days) of a set/pair of wind speed and direction.}
  \item{daily}{Availability per day, i.e. number of samples per day.}
}
\references{
Brower, M., Marcus, M., Taylor, M., Bernadett, D., Filippelli, M., Beaucage, P., Hale, E., Elsholz, K., Doane, J., Eberhard, M., Tensen, J., Ryan, D. (2010) Wind Resource Assessment Handbook. http://www.awstruepower.com/wp-content/uploads/2012/01/10-30_wind-resource-handbook.pdf
}
\author{Christian Graul}
\seealso{\code{\link{createMast}}, \code{\link{clean}}, \code{\link{plotAvailability}}}
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
	set40=set40, set30=set30, set20=set20)
neubuerg <- clean(neubuerg)

# calculate availability
neubuerg.avail <- availability(neubuerg)

# compare the total availability of 'set40' and 'set30'
neubuerg.avail$set40$total
neubuerg.avail$set30$total

# check the daily availability of 'set40'
neubuerg.avail$set40$daily

# note: availability for 'set20' is missing - its availability is NULL,
# since it does not contain wind direction data

# calculate availability for 'set20' using wind direction data from 'set30'
\dontrun{set20.avail <- availability(mast=neubuerg, v.set=3, dir.set=2)}
}
\keyword{methods}

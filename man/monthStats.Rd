\encoding{UTF-8}
\name{monthStats}
\alias{monthStats}
\title{Calculation of monthly means}
\description{Calculates monthly means of wind speed.}
\usage{monthStats(mast, set)}
\arguments{
  \item{mast}{Met mast object created by \code{\link{createMast}}.}
  \item{set}{Set used for calculation as integer value. If missing, the calculation is carried out for all datasets that contain average wind speed \code{v.avg}.}
}
\details{
\code{monthStats} averages valid wind speed data for each month and year in the measurement period. Means strongly depend on the measurement period and the number of samples. One important requirement for a reliable wind assessment is a measurement period covering the full seasonal cycle of variations.  A typical bias is a measurement limited to winter months, which usually results in overestimated wind speeds.
}
\value{Returns a list of data frames (one for each dataset) containing monthly, annual and total means of wind speed.}
\references{
Brower, M., Marcus, M., Taylor, M., Bernadett, D., Filippelli, M., Beaucage, P., Hale, E., Elsholz, K., Doane, J., Eberhard, M., Tensen, J., Ryan, D. (2010) Wind Resource Assessment Handbook. http://www.awstruepower.com/wp-content/uploads/2012/01/10-30_wind-resource-handbook.pdf
}
\author{Christian Graul}
\seealso{\code{\link{createMast}}, \code{\link{plotMonthStats}}}
\examples{
# load and prepare data
data(winddata)
set40 <- createSet(height=40, v.avg=winddata[,2])
set30 <- createSet(height=30, v.avg=winddata[,6])
set20 <- createSet(height=20, v.avg=winddata[,10])
ts <- formatTS(winddata[,1])
neubuerg <- createMast(time.stamp=ts, loc=NULL, desc=NULL, 
	set40, set30, set20)
neubuerg <- clean(neubuerg)

# calculate monthly means
neubuerg.stats <- monthStats(neubuerg)

# calculate monthly means for one dataset
neubuerg.stats.2 <- monthStats(mast=neubuerg, set=1)

# compare output
neubuerg.stats
neubuerg.stats.2
}
\keyword{methods}

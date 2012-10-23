\encoding{UTF-8}
\name{printMast}
\alias{printMast}
\title{Overview of met mast}
\description{Summarizes information about a given met mast object.}
\usage{printMast(mast)}
\arguments{
  \item{mast}{Met mast object created by \code{\link{createMast}}.}
}
\value{Returns a list of information about the given met mast:
  \item{location}{Lat/lon coordinates of the site (if available).}
  \item{description}{Plain text description of the met mast (if available).}
  \item{num.sets}{Number of datasets.}
  \item{heights}{List of measuring heights of the datasets.}
  \item{signals}{List of signals of each dataset.}
  \item{period}{List of Starting time stamp, end time stamp and duration of the measuring period.}
  \item{num.samples}{Number of samples.}
  \item{wind.speed}{List of average wind speed of the datasets (if available).}
  \item{availability}{List of the availability of the datasets (if available).}
  \item{cleaned}{List of the cleaning status of the datasets.}
}
\author{Christian Graul}
\seealso{\code{\link{createMast}}}
\examples{
\dontrun{
# load and prepare data
data(winddata)
set40 <- createSet(height=40, v.avg=winddata[,2], v.std=winddata[,5],
dir.avg=winddata[,14])
set30 <- createSet(height=30, v.avg=winddata[,6], v.std=winddata[,9],
dir.avg=winddata[,16])
set20 <- createSet(height=20, v.avg=winddata[,10], v.std=winddata[,13])
ts <- formatTS(winddata[,1])
neubuerg <- createMast(time.stamp=ts, loc=NULL, desc=NULL, set40)
neubuerg.2 <- clean(neubuerg)
neubuerg.3 <- createMast(time.stamp=ts, loc=c(49.8909,11.4017), 
	desc="Site #247 - Neubuerg", set40=set40, set30=set30, set20=set20)

# view met masts
printMast(neubuerg)
printMast(neubuerg.2)
printMast(neubuerg.3)

# get cleaning info
printMast(neubuerg)$cleaned
printMast(neubuerg.2)$cleaned
}
}
\keyword{methods}

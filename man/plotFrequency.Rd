\encoding{UTF-8}
\name{plotFrequency}
\alias{plotFrequency}
\title{Plot frequency data}
\description{Plots joint probability of wind speeds and directions from a \code{\link{frequency}} object in a rose plot.}
\usage{plotFrequency(freq, ...)}
\arguments{
  \item{freq}{Frequency object created by \code{\link{frequency}}.}
  \item{\dots}{Optional graphical parameters, see below for details.}
}
\section{Optional graphical parameters}{
The following graphical parameters can optionally be added to customize the plot:
  \itemize{
    \item \code{col}: Vector of colours -- one colour for each wind speed bin or a single colour if \code{frequency} only contains the total frequency per direction sector.
    \item \code{cex}: Numeric value, giving the amount by which text on the plot should be scaled relative to the default (which is 1).
  }
}
\author{Christian Graul}
\seealso{\code{\link{frequency}}}
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
neubuerg.freq.2 <- frequency(neubuerg, 1, bins=NULL)

# plot frequency
plotFrequency(neubuerg.freq)
plotFrequency(neubuerg.freq.2)

# customize plot
plotFrequency(neubuerg.freq, col=rainbow(5))
plotFrequency(neubuerg.freq.2, col=gray(0.4), cex=1.5)
}
}
\keyword{methods}

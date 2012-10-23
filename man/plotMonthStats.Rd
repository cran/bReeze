\encoding{UTF-8}
\name{plotMonthStats}
\alias{plotMonthStats}
\title{Plot monthly means}
\description{Plots monthly means of wind speed from a \code{\link{monthStats}} object.}
\usage{plotMonthStats(month.stats, set, ...)}
\arguments{
  \item{month.stats}{Monthly means created by \code{\link{monthStats}}.}
  \item{set}{Set used for plotting as integer value. Argument is optional -- if missing, all sets in \code{month.stats} are plotted.}
  \item{\dots}{Optional graphical parameters, see below for details.}
}
\section{Optional graphical parameters}{
The following graphical parameters can optionally be added to customize the plot:
  \itemize{
  	\item \code{col}: Vector of colours, one for each year in the measurement period.
  	\item \code{ylab}: List of two string vectors. The first vector gives the outer labels (e.g. name of dataset), the second gives the inner labels (e.g. name and dimension of signal).
    \item \code{cex}: Numeric value, giving the amount by which text on the plot should be scaled relative to the default (which is 1).
  }
}
\author{Christian Graul}
\seealso{\code{\link{monthStats}}}
\examples{
\dontrun{
data(winddata)
set40 <- createSet(height=40, v.avg=winddata[,2])
set30 <- createSet(height=30, v.avg=winddata[,6])
set20 <- createSet(height=20, v.avg=winddata[,10])
ts <- formatTS(winddata[,1])
neubuerg <- createMast(time.stamp=ts, loc=NULL, desc=NULL, 
	set40, set30, set20)
neubuerg <- clean(neubuerg)

# calculate stats
neubuerg.stats <- monthStats(neubuerg)

# plot all datasets
plotMonthStats(neubuerg.stats)

# plot one dataset
plotMonthStats(neubuerg.stats, set=1)

# change colour, labels and text size
plotMonthStats(neubuerg.stats, col=c(gray(0.4), gray(0.6)), cex=1.2, 
	ylab=list(rep("Wind speed [m/s]", 3), c("40 m", "30 m", "20 m")))
plotMonthStats(neubuerg.stats, set=1, col=c(2,3), cex=0.8, 
	ylab=list("C1.A1", "Velocity [m/s]"))
}
}
\keyword{methods}

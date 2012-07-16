\encoding{UTF-8}
\name{plotTimeSeries}
\alias{plotTimeSeries}
\title{Plot time series of met mast data}
\description{Plots time series of one or more signals of a given met mast object.}
\usage{
plotTimeSeries(mast, set, signal=c("v.avg", "dir.avg", "turb.int"), 
  start, end, ...)
}
\arguments{
  \item{mast}{Met mast object created by \code{\link{createMast}}.}
  \item{set}{Set used for plotting as integer value. Argument is optional -- if missing, all sets containing the given \code{signal}(s) are used.}
  \item{signal}{Signal(s) to be plotted as vector of strings giving the signal names.}
  \item{start}{Date and time the plot starts at, as string.}
  \item{end}{Date and time the plot ends at, as string.}
  \item{\dots}{Optional graphical parameters, see below for details.}
}
\section{Optional graphical parameters}{
The following graphical parameters can optionally be added to customize the plot:
  \itemize{
  	\item \code{col}: Vector of colours -- one for each set plotted.
    \item \code{lty}: Vector of line types -- one for each set plotted. See \code{par} for available line types.
    \item \code{cex}: Numeric value, giving the amount by which text on the plot should be scaled relative to the default (which is 1).
    \item \code{ylab}: String vector of labels for the y axis.
  }
}
\author{Christian Graul}
\seealso{\code{\link{createMast}}}
\examples{
# load and prepare data
data(winddata)
set40 <- createSet(height=40, v.avg=winddata[,2], v.max=winddata[,3], 
	v.min=winddata[,4], v.std=winddata[,5], dir.avg=winddata[,14])
set30 <- createSet(height=30, v.avg=winddata[,6], v.std=winddata[,9], 
	dir.avg=winddata[,16])
set20 <- createSet(height=20, v.avg=winddata[,10], v.std=winddata[,13])
ts <- formatTS(winddata[,1])
neubuerg <- createMast(time.stamp=ts, loc=NULL, desc=NULL, 
	set40, set30, set20)
neubuerg <- clean(neubuerg)

# plot time series
plotTimeSeries(neubuerg)
\dontrun{plotTimeSeries(neubuerg, set=1)

# change signals 
plotTimeSeries(neubuerg, signal=c("v.avg", "v.min", "v.max", "v.std"))
plotTimeSeries(neubuerg, signal=c("turb.int"))
plotTimeSeries(neubuerg, set=1, signal=c("v.avg", "dir.avg"))

# change time scale
plotTimeSeries(neubuerg, start="2010-01-01 00:10:00")
plotTimeSeries(neubuerg, start="2009-10-11 00:10:00", 
	end="2009-10-11 23:50:00")
plotTimeSeries(neubuerg, set=1, signal="v.max", 
	start="2009-12-27 16:30:00", end="2009-12-27 18:30:00")

# customize plot
plotTimeSeries(neubuerg, col=gray(1:3 / 4), lty=c(1,2,3), 
	start="2009-12-01 00:10:00", end="2009-12-01 23:50:00")
plotTimeSeries(neubuerg, col=c("blue", "red", "green"), lty=c(2,2,2))
plotTimeSeries(neubuerg, signal="dir.avg", cex=1.3, ylab="Direction [°]")}
}
\keyword{methods}

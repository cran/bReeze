\encoding{UTF-8}
\name{plotDay}
\alias{plotDay}
\title{Plot diurnal wind speed}
\description{Plots the diurnal variation of wind speed or wind direction.}
\usage{plotDay(mast, set, signal=c("v", "dir"), ...)}
\arguments{
  \item{mast}{Met mast object created by \code{\link{createMast}}.}
  \item{set}{Set used for plotting as integer value. Argument is optional -- if missing, all sets containing the choosen \code{signal} are used.}
  \item{signal}{Signal to be plotted. One of \code{"v"} (wind speed) or \code{"dir"} (wind direction).}
  \item{\dots}{Optional graphical parameters, see below for details.}
}
\section{Optional graphical parameters}{
The following graphical parameters can optionally be added to customize the plot:
  \itemize{
    \item \code{col}: Vector of colours, one for each set plotted.
    \item \code{lty}: Vector of line types, one for each set plotted. See \code{par} for available line types.
    \item \code{lwd}: Vector of line widths, one for each set plotted. See \code{par} for usage.
    \item \code{cex}: Numeric value, giving the amount by which text on the plot should be scaled relative to the default (which is 1).
  }
}
\details{
\code{plotDay} reveals diurnal variations of wind speed or wind direction. If wind speeds are usually low with high peaks only during a short period of time each day, problems with grid stability could arise. The plot may also show outliers that indicate data inconsistancy.
}
\author{Christian Graul}
\seealso{\code{\link{createMast}}}
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

# plot all datasets
plotDay(neubuerg, signal="v")
plotDay(neubuerg, signal="dir")

# plot one dataset
plotDay(neubuerg, set=1, signal="v")
plotDay(neubuerg, set=2, signal="dir")

# customize plot
plotDay(neubuerg, set=2, signal="v", 
	col=1, lwd=2.5, cex=1.5)
plotDay(neubuerg, signal="dir", 
	col="brown", lwd=3, lty=c(2,3))
plotDay(neubuerg, signal="dir", 
	col=c("darkgray", "lightgray"), lwd=c(3,2), lty=3)
}
}
\keyword{methods}

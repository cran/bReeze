\encoding{UTF-8}
\name{plotTurbulence}
\alias{plotTurbulence}
\title{Plot turbulence intensity}
\description{Plots sectoral turbulence intensity from a \code{\link{turbulence}} object in a rose plot.}
\usage{plotTurbulence(turb, ...)}
\arguments{
  \item{turb}{Turbulence intensity object created by \code{\link{turbulence}}.}
  \item{\dots}{Optional graphical parameters, see below for details.}
}
\section{Optional graphical parameters}{
The following graphical parameters can optionally be added to customize the plot:
  \itemize{
  	\item \code{col}: Vector of colours -- one colour for each wind speed bin or a single colour if \code{turbulence} only contains the total turbulence intensity per direction sector.
    \item \code{cex}: Numeric value, giving the amount by which text on the plot should be scaled relative to the default (which is 1).
  }
}
\author{Christian Graul}
\seealso{\code{\link{turbulence}}}
\examples{
# load and prepare data
data(winddata)
set1 <- createSet(height=40, v.avg=winddata[,2], v.std=winddata[,5], 
	dir.avg=winddata[,14])
ts <- formatTS(winddata[,1])
neubuerg <- createMast(time.stamp=ts, loc=NULL, desc=NULL, set1)
neubuerg <- clean(neubuerg)

# calculate turbulence intensity
neubuerg.ti <- turbulence(neubuerg, 1)

# plot turbulence intensity
plotTurbulence(neubuerg.ti)

# change colour and text size
\dontrun{plotTurbulence(neubuerg.ti, col="darkgray", cex=1.3)
plotTurbulence(neubuerg.ti, col=3, cex=0.9)}
}
\keyword{methods}

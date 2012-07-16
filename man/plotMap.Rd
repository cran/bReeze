\encoding{UTF-8}
\name{plotMap}
\alias{plotMap}
\title{Plot map or satellite image}
\description{Plots a map or satellite image of the met mast location.}
\usage{
plotMap(mast, type=c("sat", "map"), width=0.02, height=0.02, 
  plot.coords=TRUE, ...)
}
\arguments{
  \item{mast}{Met mast object created by \code{\link{createMast}}.}
  \item{type}{Type of the map as string. One of \code{"sat"} (satellite image) or \code{"map"} (OpenStreetMap).}
  \item{width}{Width of map in degrees as numeric value. Default is \code{0.02}.}
  \item{height}{Height of map in degrees as numeric value. Default is \code{0.02}.}
  \item{plot.coords}{If \code{TRUE}, the coordinates are placed next to the location symbol.}
  \item{\dots}{Optional graphical parameters, see below for details.}
}
\section{Optional graphical parameters}{
The following graphical parameters can optionally be added to customize the plot:
  \itemize{
    \item \code{col}: Vector of colours, one for each set plotted.
    \item \code{pch}: Location symbol, either as integer or as single character. See \code{\link{points}} for possible values and their interpretation.
    \item \code{cex}: Numeric value, giving the amount by which text on the plot should be scaled relative to the default (which is 1).
  }
}
\details{
This function is based on the \code{OpenStreetMap} package by Ian Fellows, which uses OpenStreetMap Mapnik tiles for maps and bing aerial for satellite image views.
}
\note{
There might be some issues while package install or loading. Some hints can be found on the developer's blog: \url{http://blog.fellstat.com/?p=130}.
}
\author{Christian Graul}
\seealso{\code{\link{createMast}}}
\examples{
\dontrun{
# load and prepare data
data(winddata)
set1 <- createSet(height=40, v.avg=winddata[,2])
ts <- formatTS(winddata[,1])
neubuerg <- createMast(time.stamp=ts, loc=c(49.8909,11.4017),
	desc=NULL, set1)

# plot satellite image
plotMap(neubuerg)

# plot map
plotMap(neubuerg, type="map")

# change scale and omit coordinates
plotMap(neubuerg, type="map", width=1, height=0.7, plot.coords=FALSE)
plotMap(neubuerg, type="sat", width=0.006, height=0.005, 
plot.coords=FALSE)

# change colour, symbol and text size
plotMap(neubuerg, type="sat", col="white", pch="+", cex=2)
plotMap(neubuerg, type="map", col=4, pch=7, cex=1.5)
}
}
\keyword{methods}

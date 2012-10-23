\encoding{UTF-8}
\name{plotPolar}
\alias{plotPolar}
\title{Plot wind speed vs. direction}
\description{Plots wind speeds against directions in a polar plot.}
\usage{plotPolar(mast, v.set=1, dir.set=1, ...)}
\arguments{
  \item{mast}{Met mast object created by \code{\link{createMast}}.}
  \item{v.set}{Set used for wind speed values as integer value.}
  \item{dir.set}{Set used for wind speed values as integer value.}
  \item{\dots}{Optional graphical parameters, see below for details.}
}
\section{Optional graphical parameters}{
The following graphical parameters can optionally be added to customize the plot:
  \itemize{
  	\item \code{col}: The colour of the symbols plotted.
    \item \code{pch}: Either an integer specifying a symbol or a single character to be used as symbol -- see \code{\link{points}} for possible values and their interpretation.
    \item \code{cex}: Numeric value, giving the amount by which text on the plot should be scaled relative to the default (which is 1).
    \item \code{cex.pts}: Numeric value, giving the amount by which the plot symbols should be scaled relative to the default (which is 1).
  }
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

# plot v vs. dir
plotPolar(neubuerg)
plotPolar(neubuerg, v.set=3, dir.set=2)

# customize plot
plotPolar(neubuerg, col="black", pch=1)
plotPolar(neubuerg, col=2, pch="*")
plotPolar(neubuerg, pch="+", cex=1.2, cex.pts=0.8)
plotPolar(neubuerg, pch="+", cex=0.8, cex.pts=1.2)
}
}
\keyword{methods}

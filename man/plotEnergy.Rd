\encoding{UTF-8}
\name{plotEnergy}
\alias{plotEnergy}
\title{Plot wind energy rose}
\description{Plots a wind energy rose from an \code{\link{energy}} object.}
\usage{plotEnergy(energy, show.total=TRUE, ...)}
\arguments{
  \item{energy}{Energy object created by \code{\link{energy}}.}
  \item{show.total}{If \code{TRUE}, the total amount of wind energy per square meter is shown.}
  \item{\dots}{Optional graphical parameters, see below for details.}
}
\section{Optional graphical parameters}{
The following graphical parameters can optionally be added to customize the plot:
  \itemize{
    \item \code{col}: Vector of colours -- one colour for each wind speed bin or a single colour if \code{energy} only contains the total energy per direction sector.
    \item \code{cex}: Numeric value, giving the amount by which text on the plot should be scaled relative to the default (which is 1).
  }
}
\author{Christian Graul}
\seealso{\code{\link{energy}}}
\examples{
# load and prepare data
data(winddata)
set40 <- createSet(height=40, v.avg=winddata[,2], dir.avg=winddata[,14])
set30 <- createSet(height=30, v.avg=winddata[,6], dir.avg=winddata[,16])
set20 <- createSet(height=20, v.avg=winddata[,10])
ts <- formatTS(winddata[,1])
neubuerg <- createMast(time.stamp=ts, loc=NULL, desc=NULL, 
	set40, set30, set20)
neubuerg <- clean(neubuerg)

# calculate energy
neubuerg.wb <- weibull(neubuerg, 1)
neubuerg.e <- energy(neubuerg.wb)
neubuerg.e.2 <- energy(neubuerg.wb, bins=NULL)	# no binning

# plot energy rose
plotEnergy(neubuerg.e)
\dontrun{plotEnergy(neubuerg.e.2)

# omit total amount
plotEnergy(neubuerg.e, show.total=FALSE)

# change colour and text size
plotEnergy(neubuerg.e, col=gray(4:1 / 5), cex=0.9)
plotEnergy(neubuerg.e.2, col="limegreen", cex=1.3)}

}
\keyword{methods}

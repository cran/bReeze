\encoding{UTF-8}
\name{plotAep}
\alias{plotAep}
\title{Plot annual energy production}
\description{Plots sectoral annual energy production (AEP) from an \code{\link{aep}} object in a rose plot.}
\usage{plotAep(aep, show.total=TRUE, ...)}
\arguments{
  \item{aep}{AEP object created by \code{\link{aep}}.}
  \item{show.total}{If \code{TRUE} the total AEP is added to the plot.}
  \item{\dots}{Optional graphical parameters, see below for details.}
}
\section{Optional graphical parameters}{
The following graphical parameters can optionally be added to customize the plot:
  \itemize{
    \item \code{col}: Vector of colours -- one colour for each wind speed bin or a single colour if \code{aep} only contains the total AEP.
    \item \code{cex}: Numeric value, giving the amount by which text on the plot should be scaled relative to the default (which is 1).
  }
}
\author{Christian Graul}
\seealso{\code{\link{aep}}}
\examples{
\dontrun{
# load and prepare data
data(winddata)
set1 <- createSet(height=40, v.avg=winddata[,2], dir.avg=winddata[,14])
set2 <- createSet(height=30, v.avg=winddata[,6], dir.avg=winddata[,16])
ts <- formatTS(winddata[,1])
neubuerg <- createMast(time.stamp=ts, loc=NULL, desc=NULL, set1, set2)
neubuerg <- clean(neubuerg)

# calculate AEP
neubuerg.wp <- profile(neubuerg, v.set=c(1, 2), dir.set=1)
pw.56 <- readPC(system.file(package="bReeze", "powercurves", 
	"PowerWind_56_900kW.wtg"))
neubuerg.aep <- aep(neubuerg.wp, pw.56, 71)

# plot AEP
plotAep(neubuerg.aep)
plotAep(neubuerg.aep, show.total=FALSE)	# omit total AEP

# change colour and text size
plotAep(neubuerg.aep, col=gray(5:0 / 5), 0.8)
plotAep(neubuerg.aep, col=rainbow(5), cex=1.1)
}
}
\keyword{methods}

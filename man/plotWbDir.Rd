\encoding{UTF-8}
\name{plotWbDir}
\alias{plotWbDir}
\title{Plot Weibull fit per direction sector}
\description{Plots the fitted Weibull distribution per direction sector from a \code{\link{weibull}} object.}
\usage{plotWbDir(wb, show.ak=FALSE, ...)}
\arguments{
  \item{wb}{Weibull object created by \code{\link{weibull}}.}
  \item{show.ak}{If \code{TRUE}, the Weibull parameters A and k are added to the legend.}
  \item{\dots}{Optional graphical parameters, see below for details.}
}
\section{Optional graphical parameters}{
The following graphical parameters can optionally be added to customize the plot:
  \itemize{
  	\item \code{col}: Vector of colours for the sectoral distributions, with the length of sector number + 1, where the last colour is used for the general profile.
    \item \code{lty}: Vector of line types, assigned like \code{col}. See \code{par} for available line types.
    \item \code{lwd}: Vector of line widths, assigned like \code{col}. See \code{par} for usage.
    \item \code{cex}: Numeric value, giving the amount by which text on the plot should be scaled relative to the default (which is 1).
  }
}
\author{Christian Graul}
\seealso{\code{\link{weibull}}}
\examples{
\dontrun{
# load and prepare data
data(winddata)
set1 <- createSet(height=40, v.avg=winddata[,2], dir.avg=winddata[,14])
ts <- formatTS(winddata[,1])
neubuerg <- createMast(time.stamp=ts, loc=NULL, desc=NULL, set1)
neubuerg <- clean(neubuerg)

# calculate Weibull parameters
neubuerg.wb <- weibull(neubuerg, 1)

# plot
plotWbDir(neubuerg.wb)

# show parameters in legend
plotWbDir(neubuerg.wb, show.ak=TRUE)

# customize plot
plotWbDir(neubuerg.wb, col=c(rainbow(12), gray(0.4)), 
	lty=c(rep(3, 12), 1), lwd=c(rep(1, 12), 2), cex=1.2)
plotWbDir(neubuerg.wb, lty=c(2,2,3,3,3,4,4,4,5,5,5,2,1), 
	lwd=rep(1, 12), cex=0.9)
}
}
\keyword{methods}

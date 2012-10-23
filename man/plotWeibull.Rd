\encoding{UTF-8}
\name{plotWeibull}
\alias{plotWeibull}
\title{Plot wind speed distribution and Weibull fit}
\description{Plots the distribution of wind speeds and the fitted weibull distribution from a \code{\link{weibull}} object.}
\usage{plotWeibull(wb, show.ak=FALSE, ...)}
\arguments{
  \item{wb}{Weibull object created by \code{\link{weibull}}.}
  \item{show.ak}{If \code{TRUE}, the Weibull parameters A and k are added to the legend.}
  \item{\dots}{Optional graphical parameters, see below for details.}
}
\section{Optional graphical parameters}{
The following graphical parameters can optionally be added to customize the plot:
  \itemize{
    \item \code{col}: Colour, used to fill the bars.
    \item \code{border}: Colour, used for the border around the bars.
    \item \code{line}: Colour, used for the Weibull fit line.
    \item \code{lty}: Line type of the Weibull fit line -- see \code{par} for available line types.
    \item \code{lwd}: Line width for the Weibull fit line -- see \code{par} for usage.
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
plotWeibull(neubuerg.wb)

# show parameters in legend
plotWeibull(neubuerg.wb, show.ak=TRUE)

# customize bars
plotWeibull(neubuerg.wb, col="lightgray", border="darkgray")
plotWeibull(neubuerg.wb, col=3, border="white", cex=1.2)

# customize line
plotWeibull(neubuerg.wb, line="black", lty="dotdash", lwd=2, cex=0.7)
plotWeibull(neubuerg.wb, line="orange", lty=2, lwd=1.5)
}
}
\keyword{methods}

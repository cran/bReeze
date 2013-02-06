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
    \item \code{cex}: Numeric value, giving the amount by which text on the plot should be scaled relative to the default (which is 1).
    \item \code{cex.axis}: Amount by which axis annotations should be scaled, as numeric value.
    \item \code{cex.lab}: Amount by which axis labels should be scaled, as numeric value.
    \item \code{circles}: Manual definition of circles to be drawn, as numeric vector of the form c(inner circle, outer circle, interval between the circles).
    \item \code{col}: Colour to be used for the sectors.
    \item \code{col.axis}: Colour to be used for axis annotations -- default is \code{"gray45"}.
    \item \code{col.border}: Colour to be used for sector borders -- default is \code{NULL} (no border is drawn).
    \item \code{col.circle}: Colour to be used for circles and axis lines -- default is \code{"gray45"}.
    \item \code{col.lab}: Colour to be used for axis labels -- default is \code{"black"}.
    \item \code{fg}: If \code{TRUE}, sectors are plotted in foreground (on top of axis lines and circles) -- default is \code{FALSE}.
    \item \code{lwd.border}: Line width of the sector borders -- default is \code{0.5}. Only used if \code{col.border} is set.
    \item \code{lwd.circle}: Line width of circles and axis lines, as numeric value -- default is \code{0.7}.
    \item \code{pos.axis}: Position of axis labels in degree, as numeric value -- default is \code{60}.
    \item \code{sec.space}: Space between plotted sectors, as numeric value between \code{0} and \code{1} -- default is \code{0.2}.
  }
}
\author{Christian Graul}
\seealso{\code{\link{turbulence}}}
\examples{
\dontrun{
# load and prepare data
data(winddata)
set1 <- createSet(height=40, v.avg=winddata[,2], v.std=winddata[,5], 
  dir.avg=winddata[,14])
ts <- formatTS(time.stamp=winddata[,1])
neubuerg <- createMast(time.stamp=ts, set1)
neubuerg <- clean(mast=neubuerg)

# calculate turbulence intensity
neubuerg.ti <- turbulence(mast=neubuerg, turb.set=1)

# plot turbulence intensity
plotTurbulence(turb=neubuerg.ti)

# change colour and text size
plotTurbulence(turb=neubuerg.ti, cex.axis=0.7, cex.lab=0.9, 
  circles=c(0.05,0.20,0.05), col="lightgray", col.axis="darkgray", 
  col.border="gray", col.circle="darkgray", col.lab="darkgray", 
  fg=TRUE, lwd.border=1.2, lwd.circle=1.2, pos.axis=135, sec.space=0.6)
}
}
\keyword{methods}

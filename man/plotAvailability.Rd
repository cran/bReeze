\encoding{UTF-8}
\name{plotAvailability}
\alias{plotAvailability}
\title{Plot availability}
\description{Plots daily availability from an \code{\link{availability}} object in a calendar plot.}
\usage{plotAvailability(avail, set, ...)}
\arguments{
  \item{avail}{Availability object created by \code{\link{availability}}.}
  \item{set}{Number of dataset to plot as integer value (optional).}
  \item{\dots}{Optional graphical parameters, see below for details.}
}
\section{Optional graphical parameters}{
The following graphical parameters can optionally be added to customize the plot:
  \itemize{
    \item \code{col}: Vector of three colours, where the first indicates 'full availability', the second 'partial availability' and the third 'not available'.
    \item \code{border}: Colour of the cell borders -- default value is \code{black}.
    \item \code{cex}: Numeric value, giving the amount by which text on the plot should be scaled relative to the default (which is 1).
  }
}
\author{Christian Graul}
\seealso{\code{\link{availability}}}
\examples{
# load and prepare data
data(winddata)
set1 <- createSet(height=40, v.avg=winddata[,2], dir.avg=winddata[,14])
set2 <- createSet(height=30, v.avg=winddata[,6], dir.avg=winddata[,16])
set3 <- createSet(height=20, v.avg=winddata[,10])
ts <- formatTS(winddata[,1])
neubuerg <- createMast(time.stamp=ts, loc=NULL, desc=NULL, set1, set2, set3)
neubuerg <- clean(neubuerg)

# calculate availability
neubuerg.avail <- availability(neubuerg)

# plot availability
plotAvailability(neubuerg.avail)
\dontrun{plotAvailability(avail=neubuerg.avail, set=2)	# one dataset

# change colours, borders and text size
plotAvailability(avail=neubuerg.avail, col=c("green", "yellow", "red"))
plotAvailability(avail=neubuerg.avail, set=1, border="white", cex=1.2)}
}
\keyword{methods}

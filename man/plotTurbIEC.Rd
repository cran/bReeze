\encoding{UTF-8}
\name{plotTurbIEC}
\alias{plotTurbIEC}
\title{Plot turbulence intensity site classification}
\description{Plots the turbulence intensity and site classification after IEC.}
\usage{plotTurbIEC(mast, set, ...)}
\arguments{
  \item{mast}{Met mast object created by \code{createMast}.}
  \item{set}{Set used for plotting as integer value.}
  \item{\dots}{Optional graphical parameters, see below for details.}
}
\section{Optional graphical parameters}{
The following graphical parameters can optionally be added to customize the plot:
  \itemize{
  	\item \code{col}: Colour, used to fill the bars.
    \item \code{border}: Colour, used for the border around the bars.
    \item \code{space}: Numeric value between 0 and 1, giving the space left before each bar. Default space is \code{0.2}.
    \item \code{line}: Vector of three colours -- one for each IEC turbulence class.
    \item \code{lty}: Vector of three line types -- one for each IEC turbulence class. See \code{par} for available line types.
    \item \code{lwd}: Vector of three line widths -- one for each IEC turbulence class. See \code{par} for usage.
    \item \code{cex}: Numeric value, giving the amount by which text on the plot should be scaled relative to the default (which is 1).
  }
}
\details{
The IEC defines wind turbine classes by wind speed and turbulence characteristics. In terms of turbulence intensity three reference values (at 15 m/s) are defined:
\tabular{ll}{
  \emph{ Turbulence class} \tab \emph{Reference value} \cr
  A \tab 0.16 \cr
  B \tab 0.14 \cr
  C \tab 0.12
}
\code{plotTurbIEC} plots these IEC references together with the sites values to allow for a classification.

See \code{\link{turbulence}} for a definition of turbulence intensity.
}
\references{
International Electrotechnical Commission (2005) IEC 61400-1 Wind Turbines -- Part 1: Design Requirements. IEC Standard
}
\author{Christian Graul}
\seealso{\code{\link{createMast}}}
\examples{
# load and prepare data
data(winddata)
set40 <- createSet(height=40, v.avg=winddata[,2], v.std=winddata[,5])
set30 <- createSet(height=30, v.avg=winddata[,6], v.std=winddata[,9])
set20 <- createSet(height=20, v.avg=winddata[,10], v.std=winddata[,13])
ts <- formatTS(winddata[,1])
neubuerg <- createMast(time.stamp=ts, loc=NULL, desc=NULL, 
	set40, set30, set20)
neubuerg <- clean(neubuerg)

# plot
plotTurbIEC(neubuerg, set=1)

# customize bars
\dontrun{plotTurbIEC(neubuerg, set=1, col="gray", border="black", space=0.6, 
	cex=1.2)
plotTurbIEC(neubuerg, set=2, col="brown", border="brown", space=0.1)

# customize lines
plotTurbIEC(neubuerg, set=1, line="darkgreen", lty="dashed", lwd=0.5:2.5)
plotTurbIEC(neubuerg, set=2, line=c(3,4,2), lty=c(3,2,1), lwd=2, cex=0.8)}
}
\keyword{methods}

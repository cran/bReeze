\encoding{UTF-8}
\name{plotProfile}
\alias{plotProfile}
\title{Plot wind profile}
\description{Plots wind profiles from a \code{\link{profile}} object.}
\usage{plotProfile(profile, sector, measured=TRUE, ...)}
\arguments{
  \item{profile}{Wind profile object created by \code{\link{profile}}.}
  \item{sector}{Direction sector as integer (sector number) or string (sector code). If missing or \code{NULL}, all sectors are plotted.}
  \item{measured}{If \code{TRUE}, measured sector mean wind speeds are added to the plot.}
  \item{\dots}{Optional graphical parameters, see below for details.}
}
\section{Optional graphical parameters}{
The following graphical parameters can optionally be added to customize the plot:
  \itemize{
  	\item \code{col}: Colour(s) of the profile lines -- either one colour (which is repeated) or a vector with the length of sector numbers + 1, where the last colour is used for the general profile.
    \item \code{lty}: Line type(s) of the profile lines -- assigned like \code{col}. See \code{par} for available line types.
    \item \code{lwd}: Line width(s) of the profile lines -- assigned like \code{col}. See \code{par} for usage.
    \item \code{cex}: Numeric value, giving the amount by which text on the plot should be scaled relative to the default (which is 1).
    \item \code{xlim}: Numeric vector of the x limits of the plot.
    \item \code{ylim}: Numeric vector of the y limits.
  }
}
\author{Christian Graul}
\seealso{\code{\link{profile}}}
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

# create profile
neubuerg.wp <- profile(neubuerg, v.set=c(1,2), dir.set=1)

# plot all profiles
plotProfile(neubuerg.wp)

# plot only one sector
\dontrun{row.names(neubuerg.wp$profile)	# available sectors
plotProfile(neubuerg.wp, sector=3)	# ENE by sector number
plotProfile(neubuerg.wp, sector="ene")	# ENE by sector code
plotProfile(neubuerg.wp, sector="all")	# general profile

# omit 'measured' points
plotProfile(neubuerg.wp, measured=FALSE)

# customize plot
plotProfile(neubuerg.wp, sector="s", col=1, lty=4, lwd=2, cex=1.3, 
	xlim=c(0,6), ylim=c(0,100))
plotProfile(neubuerg.wp, col=rainbow(13), lty=c(rep(3,12),1), 
	lwd=c(rep(0.8,12), 1.5), ylim=c(0,100))
plotProfile(neubuerg.wp, measured=FALSE, col=c(gray(1:12 / 13), "black"), 
	lty=1, lwd=c(rep(1,12), 3), xlim=c(1,10))}
}
\keyword{methods}

\encoding{UTF-8}
\name{plotPC}
\alias{plotPC}
\title{Plot power curve}
\description{Plots a power curve object.}
\usage{plotPC(pc, cp=TRUE, ct=TRUE, ...)}
\arguments{
  \item{pc}{Power curve object ceated by \code{\link{createPC}} or imported from file by \code{\link{readPC}}.}
  \item{cp}{If \code{TRUE}, the power coefficient (cp) is added to the plot (if available).}
  \item{ct}{If \code{TRUE}, the thrust coefficient (ct) is added to the plot (if available).}
  \item{\dots}{Optional graphical parameters, see below for details.}
}
\section{Optional graphical parameters}{
The following graphical parameters can optionally be added to customize the plot:
  \itemize{
  	\item \code{col}: Vector of colours. The first colour is used for the power curve. If only one coefficient is available, the second colour is used for this coefficient, if both coefficients are available, the second colour is used for the pc and the third for the ct.
    \item \code{lty}: Vector of line types, assigned like \code{col}. See \code{par} for available line types.
    \item \code{lwd}: Vector of line widths, assigned like \code{col}. See \code{par} for usage.
    \item \code{cex}: Numeric value, giving the amount by which text on the plot should be scaled relative to the default (which is 1).
  }
}
\author{Christian Graul}
\seealso{\code{\link{createPC}}, \code{\link{readPC}}}
\examples{
\dontrun{
# create power curve
v <- seq(3, 25, 0.5)
p <- c(5, 15.5, 32, 52, 71, 98, 136, 182, 230, 285, 345, 419, 497, 594, 
	687, 760, 815, 860, 886, rep(900, 26))cp <- c(0.263, NA, 0.352, NA, 0.423, NA, 0.453, NA, 0.470, NA, 0.478, NA, 
	0.480, NA, 0.483, NA, 0.470, NA, 0.429, NA, 0.381, NA, 0.329, NA, 0.281, 
	NA, 0.236, NA, 0.199, NA, 0.168, NA, 0.142, NA, 0.122, NA, 0.105, NA, 
	0.092, NA, 0.080, NA, 0.071, NA, 0.063)ct <- c(0.653, NA, 0.698, NA, 0.705, NA, 0.713, NA, 0.720, NA, 0.723, NA, 
	0.724, NA, 0.727, NA, 0.730, NA, 0.732, NA, 0.385, NA, 0.301, NA, 0.242, 
	NA, 0.199, NA, 0.168, NA, 0.146, NA, 0.128, NA, 0.115, NA, 0.103, NA, 
	0.094, NA, 0.086, NA, 0.079, NA, 0.073)my.pc <- createPC(v=v, p=p, cp=cp, ct=ct, rho=1.195, rated.p="900 kW")

# plot power curve with and without coefficients
plotPC(my.pc)
plotPC(my.pc, cp=FALSE, ct=FALSE)

# customize plot
plotPC(my.pc, cp=FALSE, ct=FALSE, col="red", lwd=2, cex=1.2)
plotPC(my.pc, col=c("red", gray(0.4), gray(0.4)), lty=c(1,5,4), 
	lwd=c(2,1,1))
plotPC(my.pc, ct=FALSE, col=c("red", "darkblue"), lty=c(1,2), 
	lwd=c(1.5,1.5), cex=0.9)
}
}
\keyword{methods}

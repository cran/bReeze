\encoding{UTF-8}
\name{weibull}
\alias{weibull}
\alias{wb}
\alias{plot.weibull}
\title{Calculation of Weibull parameters}
\description{Calculates the shape parameters (k) and scale parameters (A) of the Weibull distribution per direction sector.}
\usage{
weibull(mast, v.set, dir.set, num.sectors=12, 
  subset, digits=3, print=TRUE)
wb(mast, v.set, dir.set, num.sectors=12, 
  subset, digits=3, print=TRUE)

\method{plot}{weibull}(x, type=c("hist", "dir"), show.ak=FALSE, ...)
}
\arguments{
  \item{mast}{Met mast object created by \code{\link{mast}}.}
  \item{v.set}{Set used for wind speed, specified as set number or set name (optional, if dir.set is given).}
  \item{dir.set}{Set used for wind direction, specified as set number or set name (optional, if \code{v.set} is given).}
  \item{num.sectors}{Number of wind direction sectors as integer value greater 1. Default is \code{12}.}
  \item{subset}{Optional start and end time stamp for a data subset, as string vector \code{c(start, end)}. The time stamps format shall follow the rules of ISO 8601 international standard, e.g. "2012-08-08 22:55:00".}
  \item{digits}{Number of decimal places to be used for results as numeric value. Default is \code{3}.}
  \item{print}{If \code{TRUE}, results are printed directly.}
  
  \item{x}{Weibull object, created by \code{weibull}.}
  \item{type}{Plot type - one of \code{"hist"} (histogram plot) or \code{"dir"} (directional plot).}
  \item{show.ak}{If \code{TRUE} (the default), the Weibull parameters A and k are added to the legend.}
  \item{\dots}{Arguments to be passed to methods. For optional graphical parameters see below.}
}
\details{
To evaluate the potential energy production of a site the observed data of a particular measurement period must be generalized to a wind speed distribution. This is commonly done by fitting the Weibull function to the data. The two-parametered Weibull distribution is expressed mathematically as:
\deqn{f(v) = \frac{k}{A} \left({\frac{v}{A}} \right)^{k-1} e^{-\left({\frac{v}{A}} \right)^k}}{f(v) = k/A*(v/A)^(k-1)*exp(-(v/A)^k))}
where \eqn{f(v)} is the frequency of occurence of wind speed \eqn{v}, \eqn{A} is the scale parameter (measure for the wind speed) and \eqn{k} is the shape parameter (description of the shape of the distribution).

The resulting Weibull distribution characterizes the wind regime on the site and can directly be used for the calculation of the potential energy production of a wind turbine (see \code{\link{aep}}).
}
\section{Optional graphical parameters}{
Graphical parameters to customize the histogram plot:
  \itemize{  
    \item \code{border}: Colour, used for the border around the bars -- default is \code{"white"}.
    \item \code{breaks}: A numeric vector giving the breakpoints between histogram cells.
    \item \code{bty}: Type of box to be drawn around the plot region. Allowed values are \code{"o"} (the default), \code{"l"}, \code{"7"}, \code{"c"}, \code{"u"}, or \code{"]"}. The resulting box resembles the corresponding upper case letter. A value of \code{"n"} suppresses the box. 
    \item \code{bty.leg}: Type of box to be drawn around the legend. Allowed values are \code{"n"} (no box, the default) and \code{"o"}.
    \item \code{cex}: Amount by which text on the plot should be scaled relative to the default (which is \code{1}), as numeric. To be used for scaling of all texts at once.
	\item \code{cex.axis}: Amount by which axis annotations should be scaled, as numeric value.
    \item \code{cex.lab}: Amount by which axis labels should be scaled, as numeric value.
    \item \code{cex.leg}: Amount by which legend text should be scaled, as numeric value.
    \item \code{col}: Colour, used to fill the bars.
	\item \code{col.axis}: Colour to be used for axis annotations -- default is \code{"black"}.
	\item \code{col.box}: Colour to be used for the box around the plot region (if \code{bty}) -- default is \code{"black"}.
    \item \code{col.lab}: Colour to be used for axis labels -- default is \code{"black"}.
    \item \code{col.leg}: Colour to be used for legend text -- default is \code{"black"}.
    \item \code{col.ticks}: Colours for the axis line and the tick marks respectively -- default is \code{"black"}.
    \item \code{las}: Style of axis labels. One of \code{0} (always parallel to the axis, default), \code{1} (always horizontal), \code{2} (always perpendicular to the axis), \code{3} (always vertical).
    \item \code{legend}: If \code{TRUE} (the default) a legend is drawn.
    \item \code{leg.text}: A character or \code{\link{expression}} vector to appear in the legend.
    \item \code{line}: Colour, used for the Weibull fit line.
    \item \code{lty}: Line type of the Weibull fit line -- see \code{\link{par}} for available line types.
    \item \code{lwd}: Line width for the Weibull fit line -- see \code{\link{par}} for usage.
    \item \code{mar}: A numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be specified on the four sides of the plot (only for plots with one dataset) -- default is \code{c(4.5, 4.5, 1, 1)}.
    \item \code{mgp}: A numerical vector of the form c(label, annotation, line), which gives the margin line for the axis label, axis annotation and axis line. The default is \code{c(2.2, 0.7, 0)}.
    \item \code{pos.leg}: Position of legend -- one of \code{"bottomright"}, \code{"bottom"}, \code{"bottomleft"}, \code{"left"}, \code{"topleft"}, \code{"top"}, \code{"topright"}, \code{"right"} or \code{"center"}. Use \code{NULL} to hide the legend.
	\item \code{xlab}: Alternative label for the x axis.
	\item \code{ylab}: Alternative label for the y axis.
	\item \code{xlim}: Limits of the x axis, as vector of two values.
	\item \code{ylim}: Limits of the y axis, as vector of two values.
	\item \code{x.intersp}: Horizontal interspacing factor for legend text, as numeric -- default is \code{0.4}.
	\item \code{y.intersp}: Vertical interspacing factor for legend text, as numeric -- default is \code{0.8}.
  }

Graphical parameters to customize the directional plot:
  \itemize{    
    \item \code{bty}: Type of box to be drawn around the plot region. Allowed values are \code{"o"} (the default), \code{"l"}, \code{"7"}, \code{"c"}, \code{"u"}, or \code{"]"}. The resulting box resembles the corresponding upper case letter. A value of \code{"n"} suppresses the box.
    \item \code{bty.leg}: Type of box to be drawn around the legend. Allowed values are \code{"n"} (no box, the default) and \code{"o"}.
    \item \code{cex}: Amount by which text on the plot should be scaled relative to the default (which is \code{1}), as numeric. To be used for scaling of all texts at once.
	\item \code{cex.axis}: Amount by which axis annotations should be scaled, as numeric value.
    \item \code{cex.lab}: Amount by which axis labels should be scaled, as numeric value.
    \item \code{cex.leg}: Amount by which legend text should be scaled, as numeric value.
    \item \code{col}: Vector of colours for the sectoral distributions, with the length of sector number + 1, where the last colour is used for the general profile.
	\item \code{col.axis}: Colour to be used for axis annotations -- default is \code{"black"}.
	\item \code{col.box}: Colour to be used for the box around the plot region (if \code{bty}) -- default is \code{"black"}.
    \item \code{col.lab}: Colour to be used for axis labels -- default is \code{"black"}.
    \item \code{col.leg}: Colour to be used for legend text -- default is \code{"black"}.
    \item \code{col.ticks}: Colours for the axis line and the tick marks respectively -- default is \code{"black"}.
    \item \code{las}: Style of axis labels. One of \code{0} (always parallel to the axis, default), \code{1} (always horizontal), \code{2} (always perpendicular to the axis), \code{3} (always vertical).
    \item \code{lty}: Vector of line types, assigned like \code{col}. See \code{\link{par}} for usage.
    \item \code{lwd}: Vector of line widths, assigned like \code{col}. See \code{\link{par}} for usage.
    \item \code{mar}: A numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be specified on the four sides of the plot -- default is \code{c(4.5, 4.5, 1, 1)}.
    \item \code{mgp}: A numerical vector of the form c(label, annotation, line), which gives the margin line for the axis label, axis annotation and axis line. The default is \code{c(2.5, 0.7, 0)}.
    \item \code{pos.leg}: Position of legend -- one of \code{"bottomright"}, \code{"bottom"}, \code{"bottomleft"}, \code{"left"}, \code{"topleft"}, \code{"top"}, \code{"topright"}, \code{"right"} or \code{"center"}. Use \code{NULL} to hide the legend.
   	\item \code{xlab}: Alternative label for the x axis.
	\item \code{ylab}: Alternative label for the y axis.
	\item \code{xlim}: Numeric vector of the x limits of the plot.
    \item \code{ylim}: Numeric vector of the y limits.
	\item \code{x.intersp}: Horizontal interspacing factor for legend text, as numeric -- default is \code{0.4}.
    \item \code{y.intersp}: Vertical line distance for legend text, as numeric -- default is \code{0.8}.
  }
}
\value{
Returns a data frame containing:
  \item{k}{Shape parameter of the Weibull distribution for each direction sector.}
  \item{A}{scale parameter of the Weibull distribution for each direction sector.}
  \item{wind.speed}{Mean wind speed of the Weibull distribution for each direction sector.}
  \item{frequency}{Frequency of the Weibull distribution for each direction sector.}
}
\references{
Brower, M., Marcus, M., Taylor, M., Bernadett, D., Filippelli, M., Beaucage, P., Hale, E., Elsholz, K., Doane, J., Eberhard, M., Tensen, J., Ryan, D. (2010) Wind Resource Assessment Handbook. http://www.renewablenrgsystems.com/TechSupport/~/media/Files/PDFs/wind_resource_handbook.ashx

Langreder, W. (2010) Wind Resource and Site Assessment. In: Wei Tong (Ed.), Wind Power Generation and Wind Turbine Design, Chapter 2, p. 49--87, Southampton: WIT Press

Weibull, W. (1951) A Statistical Distribution Function of Wide Applicability. \emph{Journal of Applied Mechanics -- Trans. ASME} \bold{18}(3), 293--297
}
\author{Christian Graul}
\seealso{\code{\link{mast}}}
\examples{
\dontrun{
## load and prepare data
data("winddata", package="bReeze")
set40 <- set(height=40, v.avg=winddata[,2], dir.avg=winddata[,14])
set30 <- set(height=30, v.avg=winddata[,6], dir.avg=winddata[,16])
set20 <- set(height=20, v.avg=winddata[,10])
ts <- timestamp(timestamp=winddata[,1])
neubuerg <- mast(timestamp=ts, set40, set30, set20)
neubuerg <- clean(mast=neubuerg)


## calculate Weibull parameters
weibull(mast=neubuerg, v.set=1)  # default

# if only one of v.set and dir.set is given, 
# the dataset is assigned to both
weibull(mast=neubuerg, v.set=1)
weibull(mast=neubuerg, dir.set=1)
weibull(mast=neubuerg, dir.set="set1")

# change number of direction sectors
weibull(mast=neubuerg, v.set=1, num.sectors=16)

# data subsets
weibull(mast=neubuerg, v.set=1, 
  subset=c("2009-12-01 00:00:00", "2009-12-31 23:50:00"))
weibull(mast=neubuerg, v.set=1, 
  subset=c("2010-01-01 00:00:00", NA)) # just 'start' time stamp
weibull(mast=neubuerg, v.set=1, 
  subset=c(NA, "2009-12-31 23:50:00")) # just 'end' time stamp

# change number of digits and hide results
neubuerg.wb <- weibull(mast=neubuerg, v.set=1, digits=2, print=FALSE)


## plot weibull objects - histogram plot
plot(neubuerg.wb)  # default
plot(neubuerg.wb, type="dir")  # same as above
plot(neubuerg.wb, show.ak=TRUE)  # show parameters in legend

# customize plot
plot(neubuerg.wb, bty="l", bty.leg="l", cex.axis=1.2, cex.lab=1.4, cex.leg=0.9, 
  col.axis="darkgray", col.box="darkgray", col.lab="darkgray", col.leg="darkgray", 
  col.ticks="darkgray", las=0, leg.text=c("measured", "calculated"), 
  mar=c(3,3,0.5,0.5), mgp=c(1.8,0.5,0), pos.leg="right", xlab="velocity [m/s]", 
  ylab="frequency [\%]", xlim=c(0,20), ylim=c(0,15), x.intersp=1, y.intersp=1)

# customize bars
plot(neubuerg.wb, border="darkgray", breaks=seq(0,21,0.5), 
  col="lightgray")

# customize line
plot(neubuerg.wb, line="black", lty="dotdash", lwd=2)


## plot weibull objects - directional plot
plot(neubuerg.wb, type="dir")

# show parameters in legend
plot(neubuerg.wb, type="dir", show.ak=TRUE)

# customize plot
plot(neubuerg.wb, type="dir", bty="l", bty.leg="o", cex.axis=0.8, cex.lab=0.9, 
  cex.leg=0.7, col=c(rainbow(12), gray(0.4)), col.axis=gray(0.2), col.box=gray(0.4), 
  col.lab=gray(0.2), col.leg=gray(0.2), col.ticks=gray(0.2), las=0, 
  lty=c(rep(3, 12), 1), lwd=c(rep(1, 12), 2), mar=c(3,3,0.5,0.5), mgp=c(1.5,0.5,0), 
  pos.leg="right", xlab="velocity [m/s]", ylab="frequency [m/s]", xlim=c(0,15), 
  ylim=c(0,25), x.intersp=1, y.intersp=1)
}
}
\keyword{methods}

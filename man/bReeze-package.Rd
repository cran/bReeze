\encoding{UTF-8}
\name{bReeze-package}
\alias{bReeze-package}
\alias{bReeze}
\docType{package}
\title{Functions for wind resource assessment}
\description{
A collection of functions to analyse, visualize and interpret wind data and to calculate the potential energy production of wind turbines.
}
\details{
\tabular{lll}{
  \tab Package: \tab bReeze\cr
  \tab Type: \tab Package\cr
  \tab Version: \tab 0.1-1\cr
  \tab Date: \tab 2012-07-16\cr
  \tab License: \tab GPL (>= 2)\cr
}
Wind power is a global source of energy which attracts a large share of investments in renewables. Project sites with high wind potential do not only minimise the financial risk but increase the efficiency in carbon footprint reduction. Hence a thorough productivity analysis of a wind project is essential, regarding both economic and environmental aspects. The evaluation of the potential of a site requires a wind resource assessment, which is best based on data gained by a local measurement campaign. A methodology of processing the measured data has been established, resulting in a better understanding of the wind conditions of a site and therefore a more reliable estimation of energy production.

\code{bReeze} is a collection of widely used methods to analyse, visualise and interpret wind data. Wind analyses can subsequently be combined with characteristics of wind turbines to estimate the potential energy production on the investigated site.

Usually the data to be analysed are collected by meteorological masts (met masts) and averaged over ten minutes, but other time intervals are also processable. 

\code{bReeze} depends on three packages, written by other developers: \code{OpenStreetMap} (by Ian Fellows) provides maps and satellite images for visualising the site locations. \code{RColorBrewer} (by Erich Neuwirth) provides nice colours for the graphics. \code{XML} (by Duncan Temple Lang) provides xml parsing for power curve import. Although all analysis functions and some of the plotting functions can be used without these three packages, full functionality requires their installation. This can be done by running the following command:

install.packages(c("OpenStreetMap", "RColorBrewer", "XML"))
	
Try the examples below to check if \code{bReeze} has been correctly installed. Any question and feedback is welcome via email to <christian.graul@gmail.com>.
}
\author{
Christian Graul and Carsten Poppinga

Maintainer: Christian Graul <christian.graul@gmail.com>
}
\references{
The following handbook gives a detailed thematic overview and is available online:

Brower, M., Marcus, M., Taylor, M., Bernadett, D., Filippelli, M., Beaucage, P., Hale, E., Elsholz, K., Doane, J., Eberhard, M., Tensen, J., Ryan, D. (2010) Wind Resource Assessment Handbook. http://www.awstruepower.com/wp-content/uploads/2012/01/10-30_wind-resource-handbook.pdf

Further references are given under the specific functions of the package.
}
\keyword{package}
\examples{
# load example data
data(winddata)

# create two datasets
set40 <- createSet(height=40, v.avg=winddata[,2], v.std=winddata[,5],
dir.avg=winddata[,14])
set30 <- createSet(height=30, v.avg=winddata[,6], v.std=winddata[,9],
dir.avg=winddata[,16])

# format time stamp
ts <- formatTS(winddata[,1])

# create met mast object
metmast <- createMast(time.stamp=ts, loc=NULL,
	desc=NULL, set40=set40, set30=set30)

# plot time series of met mast signals
plotTimeSeries(metmast)
}

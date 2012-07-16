\encoding{UTF-8}
\name{clean}
\alias{clean}
\title{Clean faulty values}
\description{Cleans faulty values of a met mast object, set or specified set of a met mast. Faulty values are replaced by \code{NA}.}
\usage{
clean(mast, set, v.avg.min=0.4, v.avg.max=50, 
  dir.clean=TRUE, turb.clean=TRUE, icing=FALSE)
}
\arguments{
  \item{mast}{Met mast object created by \code{\link{createMast}}. To be ignored, if a single dataset shall be cleaned.}
  \item{set}{Set object created by \code{\link{createSet}} (if no \code{mast} is given) or set number of met mast as integer value. To be ignored, if all datasets of \code{mast} shall be cleaned.}
  \item{v.avg.min}{Lower limit for wind speeds as numeric value. Default is \code{0.4} m/s.}
  \item{v.avg.max}{Upper limit for wind speeds as numeric value. Default is \code{50} m/s.}
  \item{dir.clean}{If \code{TRUE}, faulty wind direction values, i. e. \code{dir.avg}<0 or \code{dir.avg}>360, are excluded. Default is \code{TRUE}.}
  \item{turb.clean}{If \code{TRUE}, turbulence intensity values are excluded for wind speeds lower then 4 m/s. Default is \code{TRUE}.}
  \item{icing}{If \code{TRUE}, wind direction values are excluded, where standard deviation of wind direction is 0, assuming icing. Default is \code{FALSE}.}
}
\value{Returns the input met mast or dataset object with cleaned data.}
\author{Christian Graul}
\seealso{\code{\link{createSet}}, \code{\link{createMast}}}
\examples{
# load and prepare data
data(winddata)
set40 <- createSet(height=40, v.avg=winddata[,2], v.std=winddata[,5],
dir.avg=winddata[,14])
set30 <- createSet(height=30, v.avg=winddata[,6], v.std=winddata[,9],
dir.avg=winddata[,16])
set20 <- createSet(height=20, v.avg=winddata[,10], v.std=winddata[,13])
ts <- formatTS(winddata[,1])
neubuerg <- createMast(time.stamp=ts, loc=NULL, desc=NULL, 
	set40=set40, set30=set30, set20=set20)

# clean faulty values of a met mast
neubuerg.clean <- clean(neubuerg)

# compare a subset of the original and cleaned data
neubuerg$sets$set40$data$v.avg[660:670]
neubuerg.clean$sets$set40$data$v.avg[660:670]


# clean faulty values of a dataset
set40.clean <- clean(set=set40)
  
# clean just one dataset of a met mast
neubuerg.clean.2 <- clean(mast=neubuerg, set=1)


# change lower wind speed limit 
neubuerg.clean.3 <- clean(neubuerg, v.avg.min=0.3)

# compare number of samples set to 'NA', due to lowered limit
length(which(is.na(neubuerg.clean$sets$set40$data$v.avg)==TRUE))
length(which(is.na(neubuerg.clean.3$sets$set40$data$v.avg)==TRUE))


# omit cleaning of turbulence intensity
neubuerg.clean.4 <- clean(neubuerg, turb.clean=FALSE)

# compare number of samples set to 'NA', due to turb.clean
neubuerg.clean$sets$set40$data$turb.int[75:100]
neubuerg.clean.4$sets$set40$data$turb.int[75:100]


# check whether icing is assumed for any samples
neubuerg.clean.5 <- clean(neubuerg, set=1, v.avg.min=0, v.avg.max=100, 
	dir.clean=FALSE, turb.clean=FALSE, icing=TRUE)
not.cleaned <- which(is.na(neubuerg$sets$set40$data$dir.avg)==TRUE)
cleaned <- which(is.na(neubuerg.clean.5$sets$set40$data$dir.avg)==TRUE)
length(cleaned)-length(cleaned)	# no icing here

# if icing is detected, the time stamp should be checked to exclude 
# implausible icing assumptions, e.g in summer
neubuerg.clean.5$time.stamp[cleaned]
}
\keyword{methods}

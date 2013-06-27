### short name wrapper functions

avail <- function(mast, v.set, dir.set, digits=1, print=TRUE) {
	availability(mast, v.set, dir.set, digits=1, print=TRUE)
}

cln <- function(mast, set, v.avg.min=0.4, v.avg.max=50, dir.clean=TRUE, turb.clean=4, icing=FALSE, rep=NULL, n.rep=5) {
	clean(mast, set, v.avg.min=0.4, v.avg.max=50, dir.clean=TRUE, turb.clean=4, icing=FALSE, rep=NULL, n.rep=5)
}

mast <- function(time.stamp, ..., loc=NULL, desc=NULL) {
	createMast(time.stamp, ..., loc=NULL, desc=NULL)
}

pc <- function(v, p, cp, ct, rho=1.225, rated.p, desc) {
	createPC(v, p, cp, ct, rho=1.225, rated.p, desc)
}
	
set <- function(height, desc, v.avg, v.max, v.min, v.std, dir.avg, dir.std, tmp, ...) {
	createSet(height, desc, v.avg, v.max, v.min, v.std, dir.avg, dir.std, tmp, ...)
}

en <- function(wb, rho=1.225, bins=c(5,10,15,20), digits=0, print=TRUE) {
	energy(wb, rho=1.225, bins=c(5,10,15,20), digits=0, print=TRUE)
}

forts <- function(time.stamp, pattern) {
	formatTS(time.stamp, pattern)
}

freq <- function(mast, v.set, dir.set, num.sectors=12, bins=c(5,10,15,20), digits=3, print=TRUE) {
	frequency(mast, v.set, dir.set, num.sectors=12, bins=c(5,10,15,20), digits=3, print=TRUE)
}

ms <- function(mast, set, digits=3, print=TRUE) {
	monthStats(mast, set, digits=3, print=TRUE)
}

plaep <- function(aep, show.total=TRUE, ...) {
	plotAep(aep, show.total=TRUE, ...)
}

plavail <- function(avail, set, ...) {
	plotAvailability(avail, set, ...)
}

plday <- function(mast, set, signal, ...) {
	plotDay(mast, set, signal, ...)
}

plen <- function(energy, show.total=TRUE, ...) {
	plotEnergy(energy, show.total=TRUE, ...)
}

plfreq <- function(freq, ...) {
	plotFrequency(freq, ...)
}

plms <- function(stats, set, ...) {
	plotMonthStats(stats, set, ...)
}

plpc <- function(pc, cp=TRUE, ct=TRUE, ...) {
	plotPC(pc, cp=TRUE, ct=TRUE, ...)
}

plpol <- function(mast, v.set=1, dir.set=1, ...) {
	plotPolar(mast, v.set=1, dir.set=1, ...)
}

plpro <- function(profile, sector, measured=TRUE, ...) {
	plotProfile(profile, sector, measured=TRUE, ...)
}

plts <- function(mast, set, signal=c("v.avg", "dir.avg", "turb.int"), start, end, ...) {
	plotTimeSeries(mast, set, signal=c("v.avg", "dir.avg", "turb.int"), start, end, ...)
}

pliec <- function(mast, set, ...) {
	plotTurbIEC(mast, set, ...)
}

plturb <- function(turb, ...) {
	plotTurbulence(turb, ...)
}

plwbd <- function(wb, show.ak=FALSE, ...) {
	plotWbDir(wb, show.ak=FALSE, ...)
}

plwb <- function(wb, show.ak=FALSE, ...) {
	plotWeibull(wb, show.ak=FALSE, ...)
}

probj <- function(object) {
	printObject(object)
}

pro <- function(mast, v.set, dir.set, num.sectors=12, alpha=NULL, digits=3, print=TRUE) {
	profile(mast, v.set, dir.set, num.sectors=12, alpha=NULL, digits=3, print=TRUE)
}

rpc <- function(file) {
	readPC(file)
}

turb <- function(mast, turb.set, dir.set, num.sectors=12, bins=c(5,10,15,20), digits=3, print=TRUE) {
	turbulence(mast, turb.set, dir.set, num.sectors=12, bins=c(5,10,15,20), digits=3, print=TRUE)
}

wb <- function(mast, v.set, dir.set, num.sectors=12, digits=3, print=TRUE) {
	weibull(mast, v.set, dir.set, num.sectors=12, digits=3, print=TRUE)
}
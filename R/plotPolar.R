plotPolar <-
function(mast, v.set=1, dir.set=1, ...) {
### plotting wind speed vs. wind direction in polar plot
	
	if(is.null(attr(mast, "call"))) stop(paste(substitute(mast), "is no mast object"))
	if(attr(mast, "call")$func!="createMast") stop(paste(substitute(mast), "is no mast object"))
	num.sets <- length(mast$sets)
	if(!is.numeric(v.set) | v.set<=0 | v.set>num.sets) stop("Specified 'v.set' could not be found - please specify as number\n")
	if(is.null(mast$sets[[v.set]]$data$v.avg)) stop("Specified 'v.set' does not contain average wind speed data\n")
	if(!is.numeric(dir.set) | dir.set<=0 | dir.set>num.sets) stop("Specified 'dir.set' could not be found - please specify as number\n")
	if(is.null(mast$sets[[dir.set]]$data$dir.avg)) stop("Specified 'dir.set' does not contain average wind direction data\n")
	
	ws <- mast$sets[[v.set]]$data$v.avg[!is.na(mast$sets[[v.set]]$data$v.avg) & !is.na(mast$sets[[dir.set]]$data$dir.avg)]
	wd <- mast$sets[[dir.set]]$data$dir.avg[!is.na(mast$sets[[v.set]]$data$v.avg) & !is.na(mast$sets[[dir.set]]$data$dir.avg)]*-pi/180+pi/2
	v.max <- max(mast$sets[[v.set]]$data$v.avg, na.rm=TRUE)
	circles <- seq(5, 5*(trunc(ceiling(v.max)/5)+1), by=5)
	unit <- attr(mast$sets[[v.set]]$data$v.avg, "unit")
	
	# prepare plot
	old.par <- par(no.readonly=TRUE)
	on.exit(par(old.par))
	
	plot.param <- list(...)
	if(any(names(plot.param)=="col")) col <- plot.param$col
	else col <- "#3182BD"
	if(any(names(plot.param)=="pch")) pch <- plot.param$pch
	else pch <- "."
	if(any(names(plot.param)=="cex")) cex <- plot.param$cex
	else cex <- 1
	if(any(names(plot.param)=="cex.pts")) cex.pts <- plot.param$cex.pts
	else cex.pts <- 1
	
	# plot
	par(mar=c(0,0,0,0), las=1)
	plot.new()
	pin <- par("pin")
	xlim <- ylim <- c(-1, 1)
	if (pin[1] > pin[2]) xlim <- (pin[1]/pin[2]) * xlim
	else ylim <- (pin[2]/pin[1]) * ylim
	plot.window(xlim, ylim, "", asp=1)
	
	xlist <- 0.9 * ws/tail(circles, 1) * cos(wd)
	ylist <- 0.9 * ws/tail(circles, 1) * sin(wd)
	points(xlist, ylist, pch=pch, col=col, cex=cex.pts)
	circle.pts <- seq(0, 2*pi, length.out=360)
	
	for(i in 1:length(circles)) {
		rad <- 0.9 * circles[i]/tail(circles, 1)
		circle.x <- cos(circle.pts)*rad
		circle.y <- sin(circle.pts)*rad
		lines(circle.x, circle.y, lwd=0.7, lty=2, col="gray45")	
		text(cos(pi/8)*rad, sin(pi/8)*rad, circles[i], cex=cex-0.2, col="gray45")
	}
	
	lines(c(-0.92, 0.92), c(0, 0), lwd=0.7, col="gray45")
	lines(c(0, 0), c(0.92, -0.92), lwd=0.7, col="gray45")
	text(0, -0.9, "S", pos=1, cex=cex)
	text(-0.9, 0, "W", pos=2, cex=cex)
	text(0, 0.9, "N", pos=3, cex=cex)
	text(0.9, 0, "E", pos=4, cex=cex)
	text(1, -1, unit, pos=2, cex=cex-0.2, col="gray45")
}

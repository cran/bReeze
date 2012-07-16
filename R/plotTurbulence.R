plotTurbulence <-
function(turb, ...) {
### plotting turbulence intensity from turbulence object
	
	if(is.null(attr(turb, "call")$func)) stop(paste(substitute(turb), "is no turbulence object\n"))
	if(attr(turb, "call")$func!="turbulence") stop(paste(substitute(turb), "is no turbulence object\n"))
	
	# prepare plot
	old.par <- par(no.readonly=TRUE)
	on.exit(par(old.par))
	
	plot.param <- list(...)
	if(any(names(plot.param)=="col")) col <- plot.param$col
	else col <- "#E41A1C"
	if(any(names(plot.param)=="cex")) cex <- plot.param$cex
	else cex <- 1
	
	num.sectors <- dim(turb)[1] - 1
	sectors <- seq(0, 360-360/num.sectors, by=360/num.sectors)
	sectors <- sectors+90
	sector.edges <- sectors*pi/180
	sector.width <- sector.edges[2] - sector.edges[1]
	
	turb.max <- 100*max(turb$total[1:num.sectors], na.rm=TRUE)
	circles <- seq(5, 5*(trunc(ceiling(turb.max)/5)+1), by=5)/100
	
	# plot
	par(mar=c(1,1,1,1), las=1)
	plot.new()
	pin <- par("pin")
	xlim <- ylim <- c(-1, 1)
	if (pin[1] > pin[2]) xlim <- (pin[1]/pin[2]) * xlim
	else ylim <- (pin[2]/pin[1]) * ylim
	plot.window(xlim, ylim, "", asp=1)
	
	plot.data <- c(tail(rev(as.vector(turb$total[1:num.sectors])), n=1), head(rev(as.vector(turb$total[1:num.sectors])), n=-1))    	
	for (i in 1:num.sectors) {
		arc.pts <- seq(sector.edges[i] - sector.width/2*0.85, sector.edges[i] + sector.width/2*0.85, length.out=trunc(360/num.sectors*0.85))
		rad <- 0.95 * plot.data[i]/tail(circles, 1)
		xlist <- c(0, rad * cos(arc.pts), 0)
		ylist <- c(0, rad * sin(arc.pts), 0)
	   	polygon(xlist, ylist, col=col, border=col)
	}
	
   	circle.pts <- seq(0, 2*pi, length.out=360)
	
	for(i in 1:length(circles)) {
		rad <- 0.95 * circles[i]/tail(circles, 1)
		circle.x <- cos(circle.pts)*rad
		circle.y <- sin(circle.pts)*rad
		lines(circle.x, circle.y, lwd=0.7, lty=2, col="gray45")
		text(cos(pi/8)*rad, sin(pi/8)*rad, circles[i], cex=cex-0.2, col="gray45")
	}
	    
	lines(c(-0.97, 0.97), c(0, 0), lwd=0.7, col="gray45")
	lines(c(0, 0), c(0.97, -0.97), lwd=0.7, col="gray45")
	text(0, -0.95, "S", pos=1, cex=cex)
	text(-0.95, 0, "W", pos=2, cex=cex)
	text(0, 0.95, "N", pos=3, cex=cex)
	text(0.95, 0, "E", pos=4, cex=cex)
}

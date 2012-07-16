plotFrequency <-
function(freq, ...) {
### plotting frequency from frequency object

	stopifnot(require(RColorBrewer))
	
	if(is.null(attr(freq, "call")$func)) stop(paste(substitute(freq), "is no frequency object\n"))
	if(attr(freq, "call")$func!="frequency") stop(paste(substitute(freq), "is no frequency object\n"))
	 
	dim.data <- dim(freq)
	num.sectors <- dim.data[1] - 1
	num.classes <- dim.data[2] - 2
	if(num.classes>1) {
		freq.cum <- freq[1:num.sectors,3:dim.data[2]]
		for(i in 2:num.classes) freq.cum[,i] <- freq.cum[,i] + freq.cum[,i-1]
	} else {
		freq.cum <- data.frame(freq[1:num.sectors,2])
		num.classes <- 1
	}
	
	sectors <- seq(0, 360-360/num.sectors, by=360/num.sectors)
	sectors <- sectors+90
	sector.edges <- sectors*pi/180
	sector.width <- sector.edges[2] - sector.edges[1]
	
	freq.max <- max(freq.cum, na.rm=TRUE)
	circles <- seq(5, 5*(trunc(ceiling(freq.max)/5)+1), by=5)
	
	# prepare plot
	old.par <- par(no.readonly=TRUE)
	on.exit(par(old.par))
	
	plot.param <- list(...)
	if(any(names(plot.param)=="col")) col.set <- plot.param$col
	else {
		if(num.classes==1) col.set <- c("#4575B4")
		else if(num.classes==2) col.set <- c("#4575B4", "#91BFDB")
		else if(num.classes>2 & num.classes<=11) col.set <- rev(brewer.pal(num.classes, "RdYlBu"))
	    else col.set <- rev(rainbow(num.classes, start=0.0, end=0.7))
	}
	if(any(names(plot.param)=="cex")) cex <- plot.param$cex
	else cex <- 1
	
	if(num.classes>1) lo <- layout(matrix(1:2, 1, 2), widths=c(5, 1))
	
	# plot
	plot.new()
	par(mar=c(1,1,1,1), las=1)
	pin <- par("pin")
	xlim <- ylim <- c(-1, 1)
	if (pin[1] > pin[2]) xlim <- (pin[1]/pin[2]) * xlim
	else ylim <- (pin[2]/pin[1]) * ylim
	plot.window(xlim, ylim, "", asp=1)
	
	for(c in num.classes:1) {
		plot.data <- c(tail(rev(as.vector(freq.cum[,c])), n=1), head(rev(as.vector(freq.cum[,c])), n=-1))
		
		for (i in 1:num.sectors) {
			arc.pts <- seq(sector.edges[i] - sector.width/2*0.85, sector.edges[i] + sector.width/2*0.85, length.out=trunc(360/num.sectors*0.85))
			rad <- 0.9 * plot.data[i]/tail(circles, 1)
			xlist <- c(0, rad * cos(arc.pts), 0)
			ylist <- c(0, rad * sin(arc.pts), 0)
			polygon(xlist, ylist, col=col.set[c], border=col.set[c])
		}
	}
	
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
	text(0, -0.90, "S", pos=1, cex=cex)
	text(-0.90, 0, "W", pos=2, cex=cex)
	text(0, 0.90, "N", pos=3, cex=cex)
	text(0.90, 0, "E", pos=4, cex=cex)
	text(1, -1, "%", pos=2, cex=cex-0.2, col="gray45")
	
	if(num.classes>1) {
		par(mar=c(0,0,0,0))
		plot(0, type="n", axes=FALSE, xlab="", ylab="")
		legend("left", legend=names(freq.cum), title="Wind speed\n[m/s]", fill=col.set[1:num.classes], xjust=0, bty="n", cex=cex-0.2, x.intersp=0.4, y.intersp=0.8)
	}
}

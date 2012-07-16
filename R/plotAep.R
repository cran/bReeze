plotAep <-
function(aep, show.total=TRUE, ...) {
###	plotting annual energy production rose

	stopifnot(require(RColorBrewer))
	
	if(is.null(attr(aep, "call")$func)) stop(paste(substitute(aep), "is no aep object\n"))
	if(attr(aep, "call")$func!="aep") stop(paste(substitute(aep), "is no aep object\n"))
	
	dim.data <- dim(aep)
	num.sectors <- dim.data[1] - 1
	num.classes <- dim.data[2] - 3
	total <- aep$total[num.sectors+1]
	unit <- attr(aep$total, "unit")
	
	if(num.classes>1) {
		aep.cum <- aep[1:num.sectors,4:dim.data[2]]
		for(i in 2:num.classes) {
			aep.cum[,i] <- aep.cum[,i] + aep.cum[,i-1]
		}
	} else {
		aep.cum <- data.frame(aep[1:num.sectors,3])
		num.classes <- 1
	}
	
	# prepare plot
	old.par <- par(no.readonly=TRUE)
	on.exit(par(old.par))
	
	sectors <- seq(0, 360-360/num.sectors, by=360/num.sectors)
	sectors <- sectors+90
	sector.edges <- sectors*pi/180
	sector.width <- sector.edges[2] - sector.edges[1]
	
	aep.max <- max(aep.cum, na.rm=TRUE)
	mag <-length(strsplit(as.character(aep.max),"")[[1]])-1
	circ.max <- ceiling(aep.max/10^mag)*10^mag
	if(circ.max<=2*10^mag) circles <- tail(seq(0, 2*10^mag, length.out=5), -1)
	if(circ.max>2*10^mag & circ.max<=3*10^mag) circles <- tail(seq(0, 3*10^mag, length.out=4), -1)
	if(circ.max>3*10^mag & circ.max<=4*10^mag) circles <- tail(seq(0, 4*10^mag, length.out=5), -1)
	if(circ.max>4*10^mag & circ.max<=5*10^mag) circles <- tail(seq(0, 5*10^mag, length.out=5), -1)
	if(circ.max>5*10^mag & circ.max<=6*10^mag) circles <- tail(seq(0, 6*10^mag, length.out=5), -1)
	if(circ.max>6*10^mag & circ.max<=8*10^mag) circles <- tail(seq(0, 8*10^mag, length.out=5), -1)
	if(circ.max>8*10^mag) circles <- tail(seq(0, 10^(mag+1), length.out=5), -1)
	
	plot.param <- list(...)
	if(any(names(plot.param)=="col")) col <- plot.param$col
	else {
		if(num.classes==1) col <- c("#3182BD")
		else if(num.classes==2) col <- c("#3182BD", "#9ECAE1")
		else if(num.classes>2 & num.classes<=11) {
	    	col <- rev(brewer.pal(num.classes, "Blues"))
	    } else {
	    	col <- rev(rainbow(num.classes, start=0.0, end=0.7))
	    }
	}
	if(any(names(plot.param)=="cex")) cex <- plot.param$cex
	else cex <- 1
	
	if(num.classes>1) lo <- layout(matrix(1:2, 1, 2), widths=c(5, 1))
	
	# plot
	par(mar=c(1,1,1,1), las=1)
	plot.new()
	pin <- par("pin")
	xlim <- ylim <- c(-1, 1)
	if (pin[1] > pin[2]) {
		xlim <- (pin[1]/pin[2]) * xlim
	} else {
		ylim <- (pin[2]/pin[1]) * ylim
	}
	plot.window(xlim, ylim, "", asp=1)
		
	for(c in num.classes:1) {
    	plot.data <- c(tail(rev(as.vector(aep.cum[,c])), n=1), head(rev(as.vector(aep.cum[,c])), n=-1))
		
		for (i in 1:num.sectors) {
			arc.pts <- seq(sector.edges[i] - sector.width/2*0.85, sector.edges[i] + sector.width/2*0.85, length.out=trunc(360/num.sectors*0.85))
			rad <- 0.9 * plot.data[i]/tail(circles, 1)
			xlist <- c(0, rad * cos(arc.pts), 0)
			ylist <- c(0, rad * sin(arc.pts), 0)
			polygon(xlist, ylist, col=col[c], border=col[c])
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
	text(0, -0.9, "S", pos=1, cex=cex)
	text(-0.9, 0, "W", pos=2, cex=cex)
	text(0, 0.9, "N", pos=3, cex=cex)
	text(0.9, 0, "E", pos=4, cex=cex)
	if(show.total) text(-1, 1, paste("Total:", total), pos=4, cex=cex-0.2)
	text(1, -1, unit, pos=2, cex=cex-0.2, col="gray45")
	
	if(num.classes>1) {
		par(mar=c(0,0,0,0))
		plot(0, type="n", axes=FALSE, xlab="", ylab="")
		legend("left", legend=names(aep.cum), title="Wind speed\n[m/s]", fill=col[1:num.classes], xjust=0, bty="n", cex=cex-0.2, x.intersp=0.4, y.intersp=0.8)
	}
}

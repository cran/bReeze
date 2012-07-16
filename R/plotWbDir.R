plotWbDir <-
function(wb, show.ak=FALSE, ...) {
### plotting fitted weibull distribution per direction sector from weibull object
	
	if(is.null(attr(wb, "call")$func)) stop(paste(substitute(wb), "is no weibull object\n"))
	if(attr(wb, "call")$func!="weibull") stop(paste(substitute(wb), "is no weibull object\n"))
	
	if(is.null(attr(wb, "call")$mast)) stop(paste("Source mast object of", substitute(wb), "could not be found\n"))
	mast <- get(attr(wb, "call")$mast)
	v.set <- attr(wb, "call")$v.set
	num.sectors <- dim(wb)[1]-1
	
	# prepare plot
	old.par <- par(no.readonly=TRUE)
    on.exit(par(old.par))
	
	plot.param <- list(...)
	if(any(names(plot.param)=="col")) col <- plot.param$col
	else {
		if(num.sectors==4) col <- c("#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#E41A1C")
		if(num.sectors==8) col <- c("#377EB8", "#41B6C4", "#4DAF4A", "#9970AB", "#984EA3", "#F781BF", "#FF7F00", "#A6761D", "#E41A1C")
		if(num.sectors==12) col <- c("#08519C", "#3182BD", "#74C476", "#006D2C", "#31A354", "#9E9AC8", "#54278F", "#756BB1", "#FED976", "#FD8D3C", "#FEB24C", "#6BAED6", "#E41A1C")
		if(num.sectors==16) col <- c("#08519C", "#3182BD", "#41B6C4", "#74C476", "#006D2C", "#31A354", "#9970AB", "#9E9AC8", "#54278F", "#756BB1", "#F781BF", "#FED976", "#FD8D3C", "#FEB24C", "#A6761D", "#6BAED6", "#E41A1C")
	}	
	if(any(names(plot.param)=="lty")) lty <- plot.param$lty
	else {
		if(num.sectors==4) lty <- c(5, 5, 5, 5, 1)
		if(num.sectors==8) lty <- c(5, 3, 5, 3, 5, 3, 5, 3, 1)
		if(num.sectors==12) lty <- c(5, 4, 3, 5, 4, 3, 5, 4, 3, 5, 4, 3, 1)
		if(num.sectors==16) lty <- c(5, 4, 2, 3, 5, 4, 2, 3, 5, 4, 2, 3, 5, 4, 2, 3, 1)
	}	
	if(any(names(plot.param)=="lwd")) lwd <- plot.param$lwd
	else lwd <- c(rep(1.2, num.sectors), 2)
	if(any(names(plot.param)=="cex")) cex <- plot.param$cex
	else cex <- 1
	if(any(names(plot.param)=="xlim")) xlim <- plot.param$xlim
	else xlim <- c(0, ceiling(max(mast$sets[[v.set]]$data$v.avg, na.rm=TRUE)))
	if(any(names(plot.param)=="ylim")) ylim <- plot.param$ylim
	else ylim <- NULL
	
	v <- seq(0, xlim[2], 0.1)
	if(is.null(ylim)) {
		limax <- c()
		for(i in 1:(num.sectors+1)) {
			limax <- append(limax, max(dweibull(v, shape=wb$k[i], scale=wb$A[i]), na.rm=TRUE))
		}
		ylim <- c(0, 100*max(limax))
	}
	
	# plot
	par(mar=c(4.5,5,1.5,1), las=1)
	plot(x=v, y=100*dweibull(v, shape=wb$k[num.sectors], scale=wb$A[num.sectors]), xlab="", ylab="Frequency [%]", bty="o", type="l", xlim=xlim, ylim=ylim, col=col[num.sectors], lty=lty[num.sectors], lwd=lwd[num.sectors], cex=cex, cex.axis=cex, cex.lab=cex)
	for(i in 1:(num.sectors-1)) {
		lines(x=v, y=100*dweibull(v, shape=wb$k[i], scale=wb$A[i]), col=col[i], lty=lty[i], lwd=lwd[i])
	}
	lines(x=v, y=100*dweibull(v, shape=wb$k[num.sectors+1], scale=wb$A[num.sectors+1]), col=col[num.sectors+1], lty=lty[num.sectors+1], lwd=lwd[num.sectors+1])
	mtext("Wind speed [m/s]", 1, 2, cex=cex)
	if(show.ak) leg <- paste(row.names(wb), " (A:", round(wb$A, digits=1), ", k:", round(wb$k, digits=1), ")", sep="")
	else leg <- row.names(wb)
	legend("topright", legend=leg, col=col, lty=lty, lwd=lwd, bty="n", cex=cex-0.2, x.intersp=0.4, y.intersp=0.8)	
}

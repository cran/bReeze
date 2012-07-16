plotWeibull <-
function(wb, show.ak=FALSE, ...) {
### plotting fitted weibull distribution from weibull object
	
	if(is.null(attr(wb, "call")$func)) stop(paste(substitute(wb), "is no weibull object\n"))
	if(attr(wb, "call")$func!="weibull") stop(paste(substitute(wb), "is no weibull object\n"))
	
	# prepare plot
	old.par <- par(no.readonly=TRUE)
    on.exit(par(old.par))
	
	plot.param <- list(...)
	if(any(names(plot.param)=="col")) col <- plot.param$col
	else col <- "#3182BD"
	if(any(names(plot.param)=="border")) border <- plot.param$border
	else border <- "white"
	if(any(names(plot.param)=="line")) line <- plot.param$line
	else line <- "#E41A1C"
	if(any(names(plot.param)=="lty")) lty <- plot.param$lty
	else lty <- 1
	if(any(names(plot.param)=="lwd")) lwd <- plot.param$lwd
	else lwd <- 1.2
	if(any(names(plot.param)=="cex")) cex <- plot.param$cex
	else cex <- 1
	
	if(is.null(attr(wb, "call")$mast)) stop(paste("Source mast object of", substitute(wb), "could not be found\n"))
	mast <- get(attr(wb, "call")$mast)
	v.set <- attr(wb, "call")$v.set
	unit <- attr(mast$sets[[v.set]]$data$v.avg, "unit")
	
	breaks <- seq(0, ceiling(max(mast$sets[[v.set]]$data$v.avg, na.rm=TRUE)), 1)
	density <- hist(mast$sets[[v.set]]$data$v.avg, breaks=breaks, plot=FALSE)$density
	
	# plot
	par(mar=c(4.5,5,1.5,1), las=1)
	hist(mast$sets[[v.set]]$data$v.avg, breaks=breaks, axes=FALSE, freq=FALSE, col=col, border=border, main=NULL, xlab=NULL, ylab="Frequency [%]", cex.lab=cex)
	box()
	axis(1, cex.axis=cex)
	axis(2, at=seq(0, floor(max(density)*100)/100, 0.02), labels=seq(0, floor(max(density)*100), 2), cex.axis=cex)
	
	mtext(paste("Wind speed [", unit, "]", sep=""), 1, 2, cex=cex)
	x <- NULL # just to satisfy R CMD check
	curve(dweibull(x, shape=tail(wb$k, 1), scale=tail(wb$A, 1)), col=line, lty=lty, lwd=lwd, add=TRUE)
	
	if(show.ak) leg <- c("measured", paste("Weibull (A:", round(tail(wb$A, 1), digits=1), ", k:", round(tail(wb$k, 1), digits=1), ")", sep=""))
	else leg <- c("measured", "Weibull")
	legend("topright", legend=leg, col=c(col, line), lty=c(NA, lty), lwd=c(NA, lwd), pch=c(15, NA), bty="n", cex=cex-0.2, x.intersp=0.4, y.intersp=0.8)
}

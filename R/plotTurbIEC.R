plotTurbIEC <-
function(mast, set, ...) {
### plotting turbulence intesity and site classification after IEC from mast object
		
	if(is.null(attr(mast, "call"))) stop(paste(substitute(mast), "is no mast object"))
	if(attr(mast, "call")$func!="createMast") stop(paste(substitute(mast), "is no mast object"))
	num.sets <- length(mast$sets)
	if(!is.numeric(set)) stop("'set' must be numeric\n")
	if(set<0 | set>num.sets) stop("'set' not found\n")
	if(is.null(mast$sets[[set]]$data$turb.int)) stop("Specified set does not contain turbulence intensity data\n")
	unit <- attr(mast$sets[[set]]$data$v.avg, "unit")
	
	# prepare plot
	old.par <- par(no.readonly=TRUE)
	on.exit(par(old.par))
	par(mar=c(4.5,5,1.5,1), las=1)
	
	plot.param <- list(...)
	if(any(names(plot.param)=="col")) col <- plot.param$col
	else col <- "#E41A1C"
	if(any(names(plot.param)=="border")) border <- plot.param$border
	else border <- col
	if(any(names(plot.param)=="space")) {
		if(plot.param$space<1 & plot.param$space>0) space <- plot.param$space
		else space <- 0.2
	} else space <- 0.2
	if(any(names(plot.param)=="line")) line <- plot.param$line
	else line <- "black"
	if(any(names(plot.param)=="lty")) lty <- plot.param$lty
	else lty <- c(3, 2, 1)
	if(any(names(plot.param)=="lwd")) lwd <- plot.param$lwd
	else lwd <- 1.2
	if(any(names(plot.param)=="cex")) cex <- plot.param$cex
	else cex <- 1
	if(length(line)==1) line <- rep(line, 3)
	if(length(lty)==1) lty <- rep(lty, 3)
	if(length(lwd)==1) lwd <- rep(lwd, 3)
	
	vmax <- ceiling(max(mast$sets[[set]]$data$v.avg, na.rm=TRUE))
	site.turb <- c()
	for(i in 0:(vmax-1)) {
		site.turb <- append(site.turb, mean(mast$sets[[set]]$data$turb.int[mast$sets[[set]]$data$v.avg>=i & mast$sets[[set]]$data$v.avg<i+1], na.rm=TRUE))
	}
		
	v <- seq(0, vmax, 1)
	sigma1 <- 0.16*(0.75*v+5.6)/v
	sigma2 <- 0.14*(0.75*v+5.6)/v
	sigma3 <- 0.12*(0.75*v+5.6)/v
	
	plot(v, sigma1, type="l",  xlim=c(0, vmax), ylim=c(0, 0.6), xlab="", ylab="Turbulence intensity [-]", lty=lty[3], lwd=lwd[3], col=line[3], cex.axis=cex, cex.lab=cex)
	mtext(paste("Wind speed [", unit, "]", sep=""), 1, 2, cex=cex)
	lines(v, sigma2, lty=lty[2], lwd=lwd[2], col=line[2])
	lines(v, sigma3, lty=lty[1], lwd=lwd[1], col=line[1])
	for(i in 5:vmax) {
		polygon(c(i-space/2, i-space/2, i-1+space/2, i-1+space/2), c(0, site.turb[i], site.turb[i], 0), col=col, border=border)
	}
	legend("topright", legend=c("Class A (0.16)", "Class B (0.14)", "Class C (0.12)", "Site"), col=c(line, col), lty=c(lty, NA), lwd=c(lwd, NA), pch=c(NA, NA, NA, 15), bty="n", cex=cex-0.2, x.intersp=0.4, y.intersp=0.8)
}

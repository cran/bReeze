plotPC <-
function(pc, cp=TRUE, ct=TRUE, ...) {
###	plotting power curve
	
	if(any((names(pc)[1:2]==c("v", "P"))==FALSE) & any((attr(pc, "units")[1:2]==c("m/s", "kW"))==FALSE)) stop(paste(substitute(pc), "is no power curve object - use createPC to create a power curve or readPC to import from file\n"))
	
	old.par <- par(no.readonly=TRUE)
	on.exit(par(old.par))
	
	plot.param <- list(...)
	if(any(names(plot.param)=="col")) col <- plot.param$col
	else col <- c("#3182BD", "#E41A1C", "#E41A1C")
	if(any(names(plot.param)=="cex")) cex <- plot.param$cex
	else cex <- 1
	if(any(names(plot.param)=="lty")) lty <- plot.param$lty
	else lty <- c(1:3)
	if(any(names(plot.param)=="lwd")) lwd <- plot.param$lwd
	else lwd <- c(1,1,1)
	
	# plot
	if((cp & !is.null(pc$cp)) | (ct & !is.null(pc$ct))) par(mar=c(5,5,1,5), las=1)
	else par(mar=c(5,5,1,1), las=1)
	
	plot(pc$v[!is.na(pc$P)], pc$P[!is.na(pc$P)], type="l", xlab="Wind speed [m/s]", ylab="Power [kW]", col=col[1], lty=lty[1], lwd=lwd[1], cex=cex, cex.axis=cex, cex.lab=cex)
	
	if(cp & !is.null(pc$cp)) {
		par(new=TRUE, las=1)
		plot(pc$v[!is.na(pc$cp)], pc$cp[!is.na(pc$cp)], type="l", axes=FALSE, ylab="", xlab="", ylim=c(0,1), , col=col[2], lty=lty[2], lwd=lwd[2]) 
		axis(4, cex=cex, cex.axis=cex, cex.lab=cex)  
	}
	if(cp & !is.null(pc$cp) & ct & !is.null(pc$ct)) {
		par(new=TRUE, las=1)
		plot(pc$v[!is.na(pc$ct)], pc$ct[!is.na(pc$ct)], type="l", axes=FALSE, ylab="", xlab="", ylim=c(0,1), , col=col[3], lty=lty[3], lwd=lwd[3], cex=cex, cex.axis=cex, cex.lab=cex) 
		axis(4, cex=cex, cex.axis=cex, cex.lab=cex)  
	}
	if((!cp | is.null(pc$cp)) & ct & !is.null(pc$ct)) {
		par(new=TRUE, las=1)
		plot(pc$v[!is.na(pc$ct)], pc$ct[!is.na(pc$ct)], type="l", axes=FALSE, ylab="", xlab="", ylim=c(0,1), , col=col[2], lty=lty[2], lwd=lwd[2], cex=cex, cex.axis=cex, cex.lab=cex) 
		axis(4, cex=cex, cex.axis=cex, cex.lab=cex)  
	}
	
	if(cp & !is.null(pc$cp) & ct & !is.null(pc$ct)) {
		mtext("Coefficients [-]",4, line=3, las=0, cex=cex)
		legend("topleft", legend=c("Power", "Cp", "Ct"), bty="n", col=col, lty=lty[1:3], lwd=lwd[1:3], x.intersp=0.4, y.intersp=0.8, cex=cex-0.2)
	} else if(cp & !is.null(pc$cp) & (!ct | is.null(pc$ct))) {
		mtext("Power coefficient [-]",4, line=3, las=0, cex=cex)
		legend("topleft", legend=c("Power", "Cp"), bty="n", col=col[c(1,2)], lty=lty[c(1,2)], lwd=lwd[c(1,2)], x.intersp=0.4, y.intersp=0.8, cex=cex-0.2)
	} else if(ct & !is.null(pc$ct) & (!cp | is.null(pc$cp))) {
		mtext("Thrust coefficient [-]",4, line=3, las=0, cex=cex)
		legend("topleft", legend=c("Power", "Ct"), bty="n", col=col[c(1,2)], lty=lty[c(1,2)], lwd=lwd[c(1,2)], x.intersp=0.4, y.intersp=0.8, cex=cex-0.2)
	}
}

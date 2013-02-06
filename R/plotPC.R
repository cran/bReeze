plotPC <-
function(pc, cp=TRUE, ct=TRUE, ...) {
###	plotting power curve
	
	if(!any((names(pc)[1:2]==c("v", "P"))) && !any((attr(pc, "units")[1:2]==c("m/s", "kW")))) stop(paste(substitute(pc), "is no power curve object - use createPC to create a power curve or readPC to import from file\n"))
	
	unit <- attr(pc, "units")
	old.par <- par(no.readonly=TRUE)
	on.exit(par(old.par))
	
	plot.param <- list(...)
	if(any(names(plot.param)=="col")) col <- plot.param$col
	else col <- c("#3182BD", "#E41A1C", "#E41A1C")
	if(any(names(plot.param)=="col.lab")) col.lab <- plot.param$col.lab
	else col.lab <- "black"
	if(any(names(plot.param)=="col.axis")) col.axis <- plot.param$col.axis
	else col.axis <- "black"
	if(any(names(plot.param)=="col.leg")) col.leg <- plot.param$col.leg
	else col.leg <- "black"
	if(any(names(plot.param)=="col.ticks")) col.ticks <- plot.param$col.ticks
	else col.ticks <- "black"
	if(any(names(plot.param)=="col.box")) col.box <- plot.param$col.box
	else col.box <- "black"
	if(any(names(plot.param)=="cex")) cex <- plot.param$cex
	else cex <- 1
	if(any(names(plot.param)=="cex.lab")) cex.lab <- plot.param$cex.lab
	else cex.lab <- cex
	if(any(names(plot.param)=="cex.axis")) cex.axis <- plot.param$cex.axis
	else cex.axis <- cex
	if(any(names(plot.param)=="cex.leg")) cex.leg <- plot.param$cex.leg
	else cex.leg <- cex-0.2
	if(any(names(plot.param)=="lty")) lty <- plot.param$lty
	else lty <- c(1:3)
	if(any(names(plot.param)=="lwd")) lwd <- plot.param$lwd
	else lwd <- c(1,1,1)
	if(any(names(plot.param)=="x.intersp")) x.intersp <- plot.param$x.intersp
	else x.intersp <- 0.4
	if(any(names(plot.param)=="y.intersp")) y.intersp <- plot.param$y.intersp
	else y.intersp <- 0.8
	if(any(names(plot.param)=="bty.leg")) bty.leg <- plot.param$bty.leg
	else bty.leg <- "n"
	if(any(names(plot.param)=="pos.leg")) pos.leg <- plot.param$pos.leg
	else pos.leg <- "topleft"
	if(any(names(plot.param)=="xlab")) xlab <- plot.param$xlab
	else xlab <- paste("Wind speed [", unit[1], "]", sep="")
	if(any(names(plot.param)=="ylab")) ylab <- plot.param$ylab
	else ylab <- paste("Power [", unit[2], "]", sep="")
	if(any(names(plot.param)=="ylim")) ylim <- plot.param$ylim
	else ylim <- NULL
	if(any(names(plot.param)=="mar")) mar <- plot.param$mar
	else mar <- NULL
	if(any(names(plot.param)=="mgp")) mgp <- plot.param$mgp
	else mgp <- c(3,1,0)
	if(any(names(plot.param)=="las")) las <- plot.param$las
	else las <- 1
	if(any(names(plot.param)=="bty")) bty <- plot.param$bty
	else bty <- "o"
	if(any(names(plot.param)=="legend")) legend <- plot.param$legend
	else legend <- TRUE
	if(any(names(plot.param)=="leg.text")) leg.text <- plot.param$leg.text
	else leg.text <- NULL
	
	# plot
	if((cp && !is.null(pc$cp)) || (ct && !is.null(pc$ct))) {
		if(is.null(mar)) par(mar=c(5,5,1,5), mgp=mgp, las=las, bty="n") else par(mar=mar, mgp=mgp, las=las, bty="n")
	} else {
		if(is.null(mar)) par(mar=c(5,5,1,1), mgp=mgp, las=las, bty="n") else par(mar=mar, mgp=mgp, las=las, bty="n")
	}
	
	plot(pc$v[!is.na(pc$P)], pc$P[!is.na(pc$P)], type="l", xaxt="n", yaxt="n", xlab=xlab, ylab=ylab[1], col=col[1], lty=lty[1], lwd=lwd[1], cex=cex, ylim=ylim, col.axis=col.axis, col.lab=col.lab, bty="n")
	box(bty=bty, col=col.box)
	axis(1, col=col.ticks, col.axis=col.axis, cex.axis=cex.axis)
	axis(2, col=col.ticks, col.axis=col.axis, cex.axis=cex.axis)
	
	if(cp && !is.null(pc$cp)) {
		par(new=TRUE, mgp=mgp, las=las)
		plot(pc$v[!is.na(pc$cp)], pc$cp[!is.na(pc$cp)], type="l", axes=FALSE, ylab="", xlab="", ylim=c(0,1), , col=col[2], lty=lty[2], lwd=lwd[2]) 
		axis(4, col=col.ticks, cex=cex, cex.axis=cex.axis, cex.lab=cex.lab, col.axis=col.axis, col.lab=col.lab)  
	}
	if(cp && !is.null(pc$cp) && ct && !is.null(pc$ct)) {
		par(new=TRUE, mgp=mgp, las=las)
		plot(pc$v[!is.na(pc$ct)], pc$ct[!is.na(pc$ct)], type="l", axes=FALSE, ylab="", xlab="", ylim=c(0,1), , col=col[3], lty=lty[3], lwd=lwd[3], cex=cex, cex.axis=cex, cex.lab=cex) 
		axis(4, col=col.ticks, cex=cex, cex.axis=cex.axis, cex.lab=cex.lab, col.axis=col.axis, col.lab=col.lab)  
	}
	if((!cp || is.null(pc$cp)) && ct && !is.null(pc$ct)) {
		par(new=TRUE, mgp=mgp, las=las)
		plot(pc$v[!is.na(pc$ct)], pc$ct[!is.na(pc$ct)], type="l", axes=FALSE, ylab="", xlab="", ylim=c(0,1), , col=col[2], lty=lty[2], lwd=lwd[2], cex=cex, cex.axis=cex, cex.lab=cex) 
		axis(4, col=col.ticks, cex=cex, cex.axis=cex.axis, cex.lab=cex.lab, col.axis=col.axis, col.lab=col.lab)  
	}
	
	if(cp && !is.null(pc$cp) && ct && !is.null(pc$ct)) {
		if(length(ylab)==1) txt <- "Coefficients [-]" else txt <- ylab[2]
		if(is.null(leg.text)) leg.text <- c("Power", expression(c[p]), expression(c[t]))
		mtext(txt, 4, line=mgp[1], las=0, cex=cex.lab, col=col.lab)
		if(legend) legend(pos.leg, legend=leg.text, bty=bty.leg, col=col, lty=lty[1:3], lwd=lwd[1:3], x.intersp=x.intersp, y.intersp=y.intersp, cex=cex.leg, text.col=col.leg)
	} else if(cp && !is.null(pc$cp) && (!ct || is.null(pc$ct))) {
		if(length(ylab)==1) txt <- "Power coefficient [-]" else txt <- ylab[2]
		if(is.null(leg.text)) leg.text <- c("Power", expression(c[p]))
		mtext(txt, 4, line=mgp[1], las=0, cex=cex.lab, col=col.lab)
		if(legend) legend(pos.leg, legend=leg.text, bty=bty.leg, col=col[c(1,2)], lty=lty[c(1,2)], lwd=lwd[c(1,2)], x.intersp=x.intersp, y.intersp=y.intersp, cex=cex.leg, text.col=col.leg)
	} else if(ct && !is.null(pc$ct) && (!cp || is.null(pc$cp))) {
		if(length(ylab)==1) txt <- "Thrust coefficient [-]" else txt <- ylab[2]
		if(is.null(leg.text)) leg.text <- c("Power", expression(c[t]))
		mtext(txt, 4, line=mgp[1], las=0, cex=cex.lab, col=col.lab)
		if(legend) legend(pos.leg, legend=leg.text, bty=bty.leg, col=col[c(1,2)], lty=lty[c(1,2)], lwd=lwd[c(1,2)], x.intersp=x.intersp, y.intersp=y.intersp, cex=cex.leg, text.col=col.leg)
	}
}

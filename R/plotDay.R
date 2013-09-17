plotDay <-
function(mast, set, signal, ...) {
### plotting diurnal wind speed data
	
	if(is.null(attr(mast, "call"))) stop(paste(substitute(mast), "is no mast object"))
	if(attr(mast, "call")$func!="createMast") stop(paste(substitute(mast), "is no mast object"))
	num.sets <- length(mast$sets)
	if(missing(set)) set <- "all"
	if(missing(signal)) stop("No signal to plot\n")
	if(length(signal)>1) stop("Please choose only one signal\n")
	h.unit <- attr(mast$sets[[1]]$height, "unit")
	unit <- NULL
	for(i in 1:num.sets) {
		if(any(names(mast$sets[[i]]$data)==signal)) unit <- attr(mast$sets[[i]]$data[,signal], "unit")
		break
	}
	
	# prepare plot
	plot.param <- list(...)
	if(any(names(plot.param)=="col")) col <- plot.param$col
	else {
		if(num.sets<=9) {
			col <- col1 <- brewer.pal(3, "Set1")
			if(num.sets>3) col <- col1 <- brewer.pal(num.sets, "Set1")
			col[1] <- col1[2]
			col[2] <- col1[1]
		} else col <- c("blue", "green", "red", "cyan", "magenta", "orange", "brown", "violet", "yellow", "pink", colors())
	}
	if(any(names(plot.param)=="col.lab")) col.lab <- plot.param$col.lab
	else col.lab <- "black"
	if(any(names(plot.param)=="col.axis")) col.axis <- plot.param$col.axis
	else col.axis <- "black"
	if(any(names(plot.param)=="col.ticks")) col.ticks <- plot.param$col.ticks
	else col.ticks <- "black"
	if(any(names(plot.param)=="col.box")) col.box <- plot.param$col.box
	else col.box <- "black"
	if(any(names(plot.param)=="col.leg")) col.leg <- plot.param$col.leg
	else col.leg <- "black"
	if(any(names(plot.param)=="lty")) lty <- plot.param$lty
	else lty <- rep(1, num.sets)
	if(any(names(plot.param)=="lwd")) lwd <- plot.param$lwd
	else lwd <- rep(1, num.sets)
	if(any(names(plot.param)=="cex")) cex <- plot.param$cex
	else cex <- 1
	if(any(names(plot.param)=="cex.lab")) cex.lab <- plot.param$cex.lab
	else cex.lab <- cex
	if(any(names(plot.param)=="cex.axis")) cex.axis <- plot.param$cex.axis
	else cex.axis <- cex
	if(any(names(plot.param)=="cex.leg")) cex.leg <- plot.param$cex.leg
	else cex.leg <- cex-0.2
	if(any(names(plot.param)=="x.intersp")) x.intersp <- plot.param$x.intersp
	else x.intersp <- 0.4
	if(any(names(plot.param)=="y.intersp")) y.intersp <- plot.param$y.intersp
	else y.intersp <- 0.8
	if(any(names(plot.param)=="bty.leg")) bty.leg <- plot.param$bty.leg
	else bty.leg <- "n"
	if(any(names(plot.param)=="pos.leg")) pos.leg <- plot.param$pos.leg
	else pos.leg <- "topright"
	if(any(names(plot.param)=="xlab")) xlab <- plot.param$xlab
	else xlab <- "Hour of day"
	if(any(names(plot.param)=="ylab")) ylab <- plot.param$ylab
	else {
		ylab <- signal
		if(signal=="v.avg" || signal=="v.max" || signal=="v.min") ylab <- paste("Wind speed [", unit, "]", sep="")
		if(signal=="dir.avg") ylab <- paste("Wind direction [", unit, "]", sep="")
		if(signal=="turb.int") ylab <- paste("Turbulence intensity [", unit, "]", sep="")
	}
	if(any(names(plot.param)=="ylim")) ylim <- plot.param$ylim
	else ylim <- NULL
	if(any(names(plot.param)=="mar")) mar <- plot.param$mar
	else mar <- c(4.5,5,1.5,1)
	if(any(names(plot.param)=="mgp")) mgp <- plot.param$mgp
	else mgp <- c(2,0.7,0)
	if(any(names(plot.param)=="las")) las <- plot.param$las
	else las <- 1
	if(any(names(plot.param)=="bty")) bty <- plot.param$bty
	else bty <- "o"
	
	old.par <- par(no.readonly=TRUE)
	on.exit(par(old.par))
	par(mar=mar, mgp=mgp, las=las, bty="n")
	
	# calculate and plot
	if(set!="all") { # one set
		if(!is.numeric(set)) if(set!="all") set <- match(set, names(mast$sets))
		if(is.na(set)) stop("'set' not found\n")
		if(set<0 || set>num.sets) stop("'set' not found\n")
		if(!any(names(mast$sets[[set]]$data)==signal)) stop("Specified set does not contain the choosen signal\n")
		dat <- mast$sets[[set]]$data[,which(names(mast$sets[[set]]$data)==signal)]
		if(length(col)==1) col <- rep(col, set)
		if(length(lty)==1) lty <- rep(lty, set)
		if(length(lwd)==1) lwd <- rep(lwd, set)
		
		diurnal <- NULL
		for(i in 0:23) {
			hour.idx <- mast$time.stamp$hour==i
			hour.values <- dat[hour.idx]
			hour.values <- hour.values[!is.na(hour.values)]
			if(length(hour.values)>0) diurnal <- append(diurnal, mean(hour.values))
		}
		if(is.null(ylim)) ylim <- c(0.8*min(diurnal), 1.2*max(diurnal))
		plot(0:23, diurnal, type="l", xaxt="n", yaxt="n", xlim=c(0, 24), ylim=ylim, xlab=xlab, ylab=ylab, col=col[set], lty=lty[set], lwd=lwd[set], cex.lab=cex.lab, col.lab=col.lab)
		box(bty=bty, col=col.box)
		axis(1, at=c(0,6,12,18,24), col=col.ticks, col.axis=col.axis, cex.axis=cex.axis)
		axis(2, col=col.ticks, col.axis=col.axis, cex.axis=cex.axis)
		#mtext(xlab, 1, 2, cex=cex.lab, col=col.lab)
		if(!is.null(pos.leg)) legend(pos.leg, legend=paste(names(mast$sets)[set], " (", mast$sets[[set]]$height, h.unit, ")", sep=""), col=col[set], lty=lty[set], lwd=lwd[set], bty=bty.leg, cex=cex.leg, x.intersp=x.intersp, y.intersp=y.intersp, text.col=col.leg)
	} else { # all sets
		set.index <- NULL
		for(s in 1:num.sets) if(any(names(mast$sets[[s]]$data)==signal)) set.index <- append(set.index, s)
		if(is.null(set.index)) stop("Signal not found in any set\n")
		if(any(names(plot.param)=="col")) {
			n.set <- length(set.index)
			if(length(col)==1) col <- rep(col, n.set)
			if(n.set!=length(col)) stop(paste(n.set, "colours needed"))
			set.all <- 1:set.index[n.set]
			col.all <- rep(NA, set.index[n.set])
			col.all[set.index] <- col
			col <- col.all
		}
		if(any(names(plot.param)=="lty")) {
			n.set <- length(set.index)
			if(length(lty)==1) lty <- rep(lty, n.set)
			if(n.set!=length(lty)) stop(paste(n.set, "line types needed"))
			set.all <- 1:set.index[n.set]
			lty.all <- rep(NA, set.index[n.set])
			lty.all[set.index] <- lty
			lty <- lty.all
		}
		if(any(names(plot.param)=="lwd")) {
			n.set <- length(set.index)
			if(length(lwd)==1) lwd <- rep(lwd, n.set)
			if(n.set!=length(lwd)) stop(paste(n.set, "line widths needed"))
			set.all <- 1:set.index[n.set]
			lwd.all <- rep(NA, set.index[n.set])
			lwd.all[set.index] <- lwd
			lwd <- lwd.all
		}
		
		diurnal <- NULL
		for(i in 0:23) {
			hour.idx = mast$time.stamp$hour==i
			hour.values <- mast$sets[[set.index[1]]]$data[hour.idx,which(names(mast$sets[[set.index[1]]]$data)==signal)]
			hour.values <- hour.values[!is.na(hour.values)]
			if(length(hour.values)>0) diurnal <- append(diurnal, mean(hour.values))
		}
		if(is.null(ylim)) ylim <- c(0.8*min(diurnal), 1.2*max(diurnal))
		plot(0:23, diurnal, type="l", xaxt="n", yaxt="n", xlim=c(0, 24), ylim=ylim, xlab=xlab, ylab=ylab, col=col[set.index[1]], lty=lty[set.index[1]], lwd=lwd[set.index[1]], cex.lab=cex.lab, col.lab=col.lab)
		box(bty=bty, col=col.box)
		axis(1, at=c(0,6,12,18,24), col=col.ticks, col.axis=col.axis, cex.axis=cex.axis)
		axis(2, col=col.ticks, col.axis=col.axis, cex.axis=cex.axis)
		#mtext(xlab, 1, 2, cex=cex.lab, col=col.lab)

		if(length(set.index)>1) {
			for(s in 2:length(set.index)) {
				diurnal <- NULL
				for(i in 0:23) {
					hour.idx <- mast$time.stamp$hour==i
					hour.values <- mast$sets[[set.index[s]]]$data[hour.idx,which(names(mast$sets[[set.index[s]]]$data)==signal)]
					hour.values <- hour.values[!is.na(hour.values)]
					if(length(hour.values)>0) diurnal <- append(diurnal, mean(hour.values))
				}
				lines(0:23, diurnal, col=col[set.index[s]], lty=lty[set.index[s]], lwd=lwd[set.index[s]])
			}
		}
		
		heights <- c()
		for(s in 1:num.sets) {
			if(any(names(mast$sets[[s]]$data)==signal)	) heights <- append(heights, mast$sets[[s]]$height)
		}
		
		if(!is.null(pos.leg)) legend(pos.leg, legend=paste(names(mast$sets)[set.index], " (", heights, h.unit, ")", sep=""), col=col[set.index], lty=lty[set.index], lwd=lwd[set.index], bty=bty.leg, cex=cex.leg, x.intersp=x.intersp, y.intersp=y.intersp, text.col=col.leg)
	}
}

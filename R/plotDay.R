plotDay <-
function(mast, set, signal=c("v", "dir"), ...) {
### plotting diurnal wind speed data

	stopifnot(require(RColorBrewer))
	
	if(is.null(attr(mast, "call"))) stop(paste(substitute(mast), "is no mast object"))
	if(attr(mast, "call")$func!="createMast") stop(paste(substitute(mast), "is no mast object"))
	num.sets <- length(mast$sets)
	if(missing(set)) set <- "all"
	signal <- match.arg(signal)
	if(length(signal)==2 & signal[1]=="v" & signal[2]=="dir") signal <- "v"
	if(length(signal)>1) stop("Please choose only one signal\n")
	signal <- paste(signal, ".avg", sep="")
	h.unit <- attr(mast$sets[[1]]$height, "unit")
	
	# prepare plot
	plot.param <- list(...)
	old.par <- par(no.readonly=TRUE)
	on.exit(par(old.par))
	par(mar=c(4.5,5,1.5,1), las=1)
	
	if(any(names(plot.param)=="col")) col <- plot.param$col
	else {
		if(num.sets<=9) {
			col <- col1 <- brewer.pal(3, "Set1")
			if(num.sets>3) col <- col1 <- brewer.pal(num.sets, "Set1")
			col[1] <- col1[2]
			col[2] <- col1[1]
		} else col <- c("blue", "green", "red", "cyan", "magenta", "orange", "brown", "violet", "yellow", "pink", colors())
	}
	if(any(names(plot.param)=="lty")) lty <- plot.param$lty
	else lty <- rep(1, num.sets)
	if(any(names(plot.param)=="lwd")) lwd <- plot.param$lwd
	else lwd <- rep(1, num.sets)
	if(any(names(plot.param)=="cex")) cex <- plot.param$cex
	else cex <- 1
	
	for(i in 1:num.sets) {
		if(any(names(mast$sets[[i]]$data)==signal)) unit <- attr(mast$sets[[i]]$data[,signal], "unit")
		break
	}
	if(signal=="v.avg") ylab <- paste("Wind speed [", unit, "]", sep="")
	if(signal=="dir.avg") ylab <- paste("Wind direction [", unit, "]", sep="")
	
	# calculate and plot
	if(set!="all") { # one set
		if(!is.numeric(set)) stop("'set' must be numeric\n")
		if(set<0 | set>num.sets) stop("'set' not found\n")
		if(signal=="v.avg") {
			if(is.null(mast$sets[[set]]$data$v.avg)) stop("Specified set does not contain average wind speed data\n")
			dat <- mast$sets[[set]]$data$v.avg
		}
		if(signal=="dir.avg") {
			if(is.null(mast$sets[[set]]$data$dir.avg)) stop("Specified set does not contain average wind direction data\n")
			dat <- mast$sets[[set]]$data$dir.avg
		}
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
		plot(0:23,diurnal, type="l", ylim=c(0.8*min(diurnal), 1.2*max(diurnal)), xlab="", ylab=ylab, col=col[set], lty=lty[set], lwd=lwd[set], cex.axis=cex, cex.lab=cex)
		mtext("Hour of day", 1, 2, cex=cex)
		legend("topright", legend=paste(names(mast$sets)[set], " (", mast$sets[[set]]$height, h.unit, ")", sep=""), col=col[set], lty=lty[set], lwd=lwd[set], bty="n", cex=cex-0.2, x.intersp=0.4, y.intersp=0.8)
	} else { # all sets
		set.index <- NULL
		for(s in 1:num.sets) {
			if(signal=="v.avg" & !is.null(mast$sets[[s]]$data$v.avg)) set.index <- append(set.index, s)
			if(signal=="dir.avg" & !is.null(mast$sets[[s]]$data$dir.avg)) set.index <- append(set.index, s)
		}
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
			if(signal=="v.avg") hour.values <- mast$sets[[set.index[1]]]$data$v.avg[hour.idx]
			else hour.values <- mast$sets[[set.index[1]]]$data$dir.avg[hour.idx]
			hour.values <- hour.values[!is.na(hour.values)]
			if(length(hour.values)>0) diurnal <- append(diurnal, mean(hour.values))
		}
		plot(0:23,diurnal, type="l", ylim=c(0.8*min(diurnal), 1.2*max(diurnal)), xlab="", ylab=ylab, col=col[set.index[1]], lty=lty[set.index[1]], lwd=lwd[set.index[1]], cex.axis=cex, cex.lab=cex)
		mtext("Hour of day", 1, 2, cex=cex)

		if(length(set.index)>1) {
			for(s in 2:length(set.index)) {
				diurnal <- NULL
				for(i in 0:23) {
					hour.idx <- mast$time.stamp$hour==i
					if(signal=="v.avg") hour.values <- mast$sets[[set.index[s]]]$data$v.avg[hour.idx]
					else hour.values <- mast$sets[[set.index[s]]]$data$dir.avg[hour.idx]
					hour.values <- hour.values[!is.na(hour.values)]
					if(length(hour.values)>0) diurnal <- append(diurnal, mean(hour.values))
				}
				lines(0:23,diurnal, col=col[set.index[s]], lty=lty[set.index[s]], lwd=lwd[set.index[s]])
			}
		}
		
		heights <- c()
		for(s in 1:num.sets) {
			if(signal=="v.avg" & !is.null(mast$sets[[s]]$data$v.avg)) heights <- append(heights, mast$sets[[s]]$height)
			if(signal=="dir.avg" & !is.null(mast$sets[[s]]$data$dir.avg)) heights <- append(heights, mast$sets[[s]]$height)
		}
		
		legend("topright", legend=paste(names(mast$sets)[set.index], " (", heights, h.unit, ")", sep=""), col=col[set.index], lty=lty[set.index], lwd=lwd[set.index], bty="n", cex=cex-0.2, x.intersp=0.4, y.intersp=0.8)
	}
}

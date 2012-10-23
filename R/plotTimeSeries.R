plotTimeSeries <-
function(mast, set, signal=c("v.avg", "dir.avg", "turb.int"), start, end, ...) {
### plotting time series of mast data
	
	stopifnot(require(RColorBrewer))
	
	if(is.null(attr(mast, "call"))) stop(paste(substitute(mast), "is no mast object"))
	if(attr(mast, "call")$func!="createMast") stop(paste(substitute(mast), "is no mast object"))
	num.sets <- length(mast$sets)
	time.stamp <- mast$time.stamp
	num.samples <- length(time.stamp)
	
	if(missing(set)) set <- 1:num.sets
	if(any(!is.numeric(set)==TRUE)) stop("'set' must be numeric")
	if(missing(start)) start <- as.character(time.stamp[1])
	if(missing(end)) end <- as.character(time.stamp[num.samples])
	start <- strptime(start, "%Y-%m-%d %H:%M:%S")
	end <- strptime(end, "%Y-%m-%d %H:%M:%S")
	if(is.na(start)) stop("Specified 'start' not correctly formated\n")
	if(is.na(end)) stop("Specified 'end' not correctly formated\n")

	# match start and end date
	if(start<time.stamp[1] | start>time.stamp[num.samples]) stop("Specified 'start' not in period\n")
	match.date <- difftime(time.stamp, ISOdatetime(1,1,1,0,0,0), tz="GMT", units="days") - difftime(start, ISOdatetime(1,1,1,0,0,0), tz="GMT", units="days")
	start <- which(abs(as.numeric(match.date)) == min(abs(as.numeric(match.date))))
	
	if(end<time.stamp[1] | end>time.stamp[num.samples]) stop("Specified 'end' not in period\n")
	match.date <- difftime(time.stamp, ISOdatetime(1,1,1,0,0,0), tz="GMT", units="days") - difftime(end, ISOdatetime(1,1,1,0,0,0), tz="GMT", units="days")
	end <- which(abs(as.numeric(match.date)) == min(abs(as.numeric(match.date))))
	
	# get units
	n.sig <- length(signal)
	n.set <- length(set)
	h.unit <- attr(mast$sets[[1]]$height, "unit")
	units <- rep("", n.sig)
	for(s in 1:n.sig) {
		for(i in 1:n.set) {
			if(any(names(mast$sets[[i]]$data)==signal[s])) {
				if(!is.null(attr(mast$sets[[i]]$data[,signal[s]], "unit")))	units[s] <- attr(mast$sets[[i]]$data[,signal[s]], "unit"); break
			}
		}
	}
	  
	# prepare plot
	old.par <- par(no.readonly=TRUE)
	on.exit(par(old.par))
	
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
	if(any(names(plot.param)=="cex")) cex <- plot.param$cex
	else cex <- 1
	if(any(names(plot.param)=="lty")) lty <- plot.param$lty
	else lty <- rep(1, num.sets)
	if(any(names(plot.param)=="ylab")) ylab <- plot.param$ylab
	else {
		ylab <- NULL
		for(i in 1:n.sig) {
			if(signal[i]=="v.avg") ylab <- append(ylab, paste("Wind speed [", units[i], "]", sep=""))
			else if(signal[i]=="v.max") ylab <- append(ylab, paste("Max wind speed [", units[i], "]", sep=""))
			else if(signal[i]=="v.min") ylab <- append(ylab, paste("Min wind speed [", units[i], "]", sep=""))
			else if(signal[i]=="dir.avg") ylab <- append(ylab, paste("Wind direction [", units[i], "]", sep=""))
			else if(signal[i]=="turb.int") ylab <- append(ylab, paste("Turbulence intensity [", units[i], "]", sep=""))
			else ylab <- append(ylab, paste(signal[i], " [", units[i], "]", sep=""))
		}
	}
	for(i in 1:length(ylab)) if(substr(ylab[i], nchar(ylab[i])-2, nchar(ylab[i]))==" []") ylab[i] <- substr(ylab[i], 1, nchar(ylab[i])-3)
	
	# layout
	lo <- layout(matrix(c(n.sig+2, 1:(n.sig+1)), n.sig+2, 1), heights=c(1, rep(4, n.sig), 1))
	if(n.sig==1) lo <- layout(matrix(c(n.sig+2, 1:(n.sig+1)), n.sig+2, 1), heights=c(1, 9, 1))
	n.set <- length(set)
	set.idx <- data.frame(matrix(NA, ncol=n.sig, nrow=n.set))
	names(set.idx) <- signal
	for(i in 1:n.sig) {
		for(j in 1:n.set) {
			if(any(names(mast$sets[[set[j]]]$data)==signal[i])) set.idx[j,i] <- j
		}
	}
		
	# plot	
	for(i in 1:n.sig) {
		par(mar=c(1,5,0,1), las=1)
		sets <- set.idx[!is.na(set.idx[,which(names(set.idx)==signal[i])]),which(names(set.idx)==signal[i])]
		if(length(sets)>=1) {
			plot(time.stamp[start:end], mast$sets[[sets[1]]]$data[[which(names(mast$sets[[sets[1]]]$data)==signal[i])]][start:end], type="l", col=col[sets[1]], ylab=ylab[i], xaxt="n", cex.axis=cex, cex.lab=cex, lty=lty[sets[1]])
			if(i<n.sig) axis.POSIXct(1, at=seq(min(time.stamp[start:end]), max(time.stamp[start:end]), length.out=6), format="%Y-%m-%d %H:%M:%S", labels=FALSE)
			else axis.POSIXct(1, at=seq(min(time.stamp[start:end]), max(time.stamp[start:end]), length.out=6), format="%Y-%m-%d %H:%M:%S", cex.axis=cex-0.2)
		
			if(length(sets)>1) {
				for(j in 2:length(sets)) {
					lines(time.stamp[start:end], mast$sets[[sets[j]]]$data[[which(names(mast$sets[[sets[j]]]$data)==signal[i])]][start:end], col=col[sets[j]], lty=lty[sets[j]])
				}
			}
		} else {
			plot(0, type="n", axes=FALSE, xlab="", ylab="")
			text(0, labels=paste(signal[i], "not found!"))
		}
	}
	
	set.idx <- unique(unlist(set.idx)[!is.na(unlist(set.idx))])
	if(length(set.idx)>1) {
		heights <- names <- NULL
		for(i in 1:length(set.idx)) {
			heights <- append(heights, mast$sets[[set.idx[i]]]$height)
			names <- append(names, names(mast$sets)[set.idx[i]])
		}
		plot(0, type="n", axes=FALSE, xlab="", ylab="")
		par(mar=c(0,5,0,1))
		plot(0, type="n", axes=FALSE, xlab="", ylab="")
		legend("bottom", legend=paste(names, " (", heights, h.unit, ")", sep=""), col=col[set.idx], lty=lty[set.idx], ncol=length(set.idx), bty="n", cex=cex-0.2, x.intersp=0.4, y.intersp=0.8)
	}
}

plotMonthStats <- 
function(month.stats, set, ...) {
### plotting monthly data
	
	stopifnot(require(RColorBrewer))
	
	if(is.data.frame(month.stats)) num.sets <- 1
	else num.sets <- length(month.stats)
	if(is.null(attr(month.stats, "call")$func)) stop(paste(substitute(month.stats), "is no monthStats object\n"))
	if(attr(month.stats, "call")$func!="monthStats") stop(paste(substitute(month.stats), "is no monthStats object\n"))
	if(missing(set)) set <- 1:num.sets
	n.set <- length(set)
	if(any(!is.numeric(set))) stop("'set' must be numeric\n")
	if(any(set<1) | any(set>num.sets)) stop("'set' not found\n")
	unit <- attr(month.stats, "unit")
	years <- length(month.stats[[1]])-2
	
	# prepare plot
	old.par <- par(no.readonly=TRUE)
	on.exit(par(old.par))
	
	plot.param <- list(...)
	if(any(names(plot.param)=="col")) col <- plot.param$col
	else {
		if(n.set<=9) {
			col <- col1 <- brewer.pal(3, "Paired")
			if(years>3) col <- col1 <- brewer.pal(years, "Paired")
			col[1] <- col1[2]
			col[2] <- col1[1]
		} else col <- c("blue", "green", "red", "cyan", "magenta", "orange", "brown", "violet", "yellow", "pink", colors())
	}
	if(any(names(plot.param)=="cex")) cex <- plot.param$cex
	else cex <- 1
	if(any(names(plot.param)=="ylab")) ylab <- plot.param$ylab
	else ylab <- list(names(month.stats)[set], rep(paste("Wind speed [", unit, "]", sep=""), n.set))
		
	# plot
	if(n.set==1 | num.sets==1) {
		lo <- layout(matrix(c(2,1), 2, 1), heights=c(1,8))
		par(mar=c(4,5,0,1), las=1)
		ylim <- c(-0.1, ceiling(max(month.stats[[set]][1:12,1:(length(month.stats[[set]])-2)], na.rm=TRUE))+0.3)
		barplot(t(as.matrix(month.stats[[set]][1:12,1:(length(month.stats[[set]])-2)])), beside=TRUE, ylab=NULL, cex.lab=cex, cex.axis=cex-0.1, axisnames=FALSE, col=col[1:years], border=NA, ylim=ylim, xpd=FALSE)
		box(bty="o")
		bxp <- barplot(t(as.matrix(month.stats[[set]][1:12,1:(length(month.stats[[set]])-2)])), beside=TRUE, plot=FALSE)
		at <- apply(bxp, 2, mean)
		mtext(toupper(row.names(month.stats[[set]])[1:12]), side=1, line=0.5, at=at, cex=cex-0.3)
		mtext("Months", side=1, line=1.8, at=mean(at), cex=cex)
		mtext(ylab[[1]], side=2, line=3.1, las=0, cex=cex)
		mtext(ylab[[2]], side=2, line=2.2, las=0, cex=cex)
		par(mar=c(0,5,0,2))
		plot(0, type="n", axes=FALSE, xlab="", ylab="")
		legend("bottom", legend=names(month.stats[[1]])[1:years], fill=col[1:years], ncol=years, bty="n", cex=cex-0.2, x.intersp=0.4, y.intersp=0.8)
	} else {
		lo <- layout(matrix(c(n.set+2, 1:(n.set+1)), n.set+2, 1), heights=c(1, rep(5, n.set), 1))
		par(mar=c(1,5,0,1), las=1)
		dat.max <- ceiling(max(unlist(month.stats), na.rm=TRUE))
		for(i in 1:n.set) {
			ylim <- c(-0.1, dat.max+0.3)
			barplot(t(as.matrix(month.stats[[i]][1:12,1:years])), beside=TRUE, ylab=NULL, cex.lab=cex, cex.axis=cex, axisnames=FALSE, col=col[1:years], border=NA, ylim=ylim, xpd=FALSE)
			box(bty="o")
			mtext(ylab[[1]][set[i]], side=2, line=3.5, las=0, cex=cex-0.3)
			mtext(ylab[[2]][set[i]], side=2, line=2.6, las=0, cex=cex-0.3)
		}
		bxp <- barplot(t(as.matrix(month.stats[[1]][1:12,1:(length(month.stats[[1]])-2)])), beside=TRUE, plot=FALSE)
		at <- apply(bxp, 2, mean)
		mtext(toupper(row.names(month.stats[[1]])[1:12]), side=1, line=0.5, at=at, cex=cex-0.4)
		mtext("Months", side=1, line=1.8, at=mean(at), cex=cex-0.3)
		plot(0, type="n", axes=FALSE, xlab="", ylab="")
		par(mar=c(0,5,0,1))
		plot(0, type="n", axes=FALSE, xlab="", ylab="")
		legend("bottom", legend=names(month.stats[[1]])[1:years], fill=col[1:years], ncol=years, bty="n", cex=cex-0.2, x.intersp=0.4, y.intersp=0.8)
	}
}

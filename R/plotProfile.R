plotProfile <-
function(profile, sector, measured=TRUE, ...) {
###	plotting profile
		
	if(is.null(attr(profile, "call")$func)) stop(paste(substitute(profile), "is no profile object\n"))
	if(attr(profile, "call")$func!="profile") stop(paste(substitute(profile), "is no profile object\n"))
	
	if(is.null(attr(profile, "call")$mast)) stop(paste("Source mast object of", substitute(profile), "could not be found\n"))
	mast <- get(attr(profile, "call")$mast)
	v.set <- attr(profile, "call")$v.set
	dir.set <- attr(profile, "call")$dir.set
	num.sectors <- attr(profile, "call")$num.sectors
	h.ref <- profile$h.ref
	sector.names <- row.names(profile$profile)
	
	if(missing(sector)) sector <- NULL
	if(length(sector)>1) stop("Please choose only one 'sector' by name or index\n")
	if(is.numeric(sector)) {
		if(sector<1 | sector>num.sectors+1) stop("Sector not found\n")
	} else if(is.character(sector)) {
		sector <- match(sector, sector.names)
		if(is.na(sector)) stop("Sector not found\n")
	} else {
		if(!is.null(sector)) stop("Sector not found - please choose 'sector' by name or index\n")
	}
	
	# prepare plot
	old.par <- par(no.readonly=TRUE)
    on.exit(par(old.par))
	
	plot.param <- list(...)
	if(any(names(plot.param)=="col")) {
		col <- plot.param$col
		if(length(col)==1 & is.null(sector)) col <- rep(col, num.sectors+1)
	} else {
		if(num.sectors==4) col <- c("#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#E41A1C")
		if(num.sectors==8) col <- c("#377EB8", "#41B6C4", "#4DAF4A", "#9970AB", "#984EA3", "#F781BF", "#FF7F00", "#A6761D", "#E41A1C")
		if(num.sectors==12) col <- c("#08519C", "#3182BD", "#74C476", "#006D2C", "#31A354", "#9E9AC8", "#54278F", "#756BB1", "#FED976", "#FD8D3C", "#FEB24C", "#6BAED6", "#E41A1C")
		if(num.sectors==16) col <- c("#08519C", "#3182BD", "#41B6C4", "#74C476", "#006D2C", "#31A354", "#9970AB", "#9E9AC8", "#54278F", "#756BB1", "#F781BF", "#FED976", "#FD8D3C", "#FEB24C", "#A6761D", "#6BAED6", "#E41A1C")
		if(!is.null(sector)) col <- col[sector]
	}
	if(any(names(plot.param)=="lty")) {
		lty <- plot.param$lty
		if(length(lty)==1 & is.null(sector)) lty <- rep(lty, num.sectors+1)
	} else {
		if(num.sectors==4) lty <- c(5, 5, 5, 5, 1)
		if(num.sectors==8) lty <- c(5, 3, 5, 3, 5, 3, 5, 3, 1)
		if(num.sectors==12) lty <- c(5, 4, 3, 5, 4, 3, 5, 4, 3, 5, 4, 3, 1)
		if(num.sectors==16) lty <- c(5, 4, 2, 3, 5, 4, 2, 3, 5, 4, 2, 3, 5, 4, 2, 3, 1)
		if(!is.null(sector)) lty <- lty[sector]
	}	
	if(any(names(plot.param)=="lwd")) {
		lwd <- plot.param$lwd
		if(length(lwd)==1 & is.null(sector)) lwd <- rep(lwd, num.sectors+1)
	} else {
		lwd <- c(rep(1.2, num.sectors), 2)
		if(!is.null(sector)) lwd <- lwd[sector]
	}
	if(any(names(plot.param)=="pch")) pch <- plot.param$pch
	else pch <- 0
	if(any(names(plot.param)=="cex")) cex <- plot.param$cex
	else cex <- 1
	if(any(names(plot.param)=="xlim")) xlim <- plot.param$xlim
	else xlim <- c(0, 1.75*ceiling(max(frequency(mast, v.set=v.set[1], dir.set=dir.set, bins=NULL)$wind.speed, na.rm=TRUE)))
	if(any(names(plot.param)=="ylim")) ylim <- plot.param$ylim
	else ylim <- c(0,200)
	
	# calculate and plot
	h.range <- c(1:max(ylim))
	v.range <- seq(xlim[1],xlim[2],0.1)
	v.mean <- data.frame(matrix(NA, ncol=1, nrow=num.sectors+1))
	h <- NULL
	if(measured) {		
		for(i in 1:length(v.set)) {
			if(!is.null(mast$sets[[v.set[i]]]$data$v.avg)) {
				v.mean <- data.frame(v.mean, cbind(frequency(mast, v.set=v.set[i], dir.set=dir.set, num.sectors=num.sectors, bins=NULL)$wind.speed))
				h <- append(h, mast$sets[[v.set[i]]]$height)
				names(v.mean)[i] <- names(mast$sets)[v.set[i]]
			}
		}
	}
	v.mean[1] <- NULL
	
	par(mar=c(5,5,1,1), las=1)
	if(is.null(sector)) { # all sectors
		v.over.h <- profile$profile$v.ref[1] * exp(profile$profile$alpha[1] * log(h.range / h.ref))
		h.over.v <- spline(x=v.over.h, y=h.range, method="natural", xout=v.range)
		h.over.v[[2]][h.over.v[[2]]<0] <- 0	
		plot(h.over.v, type="l", xlim=xlim, ylim=ylim, lty=lty[1], lwd=lwd[1], col=col[1], xlab="Wind speed [m/s]", ylab="Height [m]", cex.lab=cex, cex.axis=cex)
		if(measured) for(j in 1:length(v.mean)) points(x=v.mean[1,j], y=h[j], col=col[1], pch=pch, cex=cex-0.2)
		
		for(i in 2:num.sectors) {
			v.over.h <- profile$profile$v.ref[i] * exp(profile$profile$alpha[i] * log(h.range / h.ref))
			h.over.v <- spline(x=v.over.h, y=h.range, method="natural", xout=v.range)
			h.over.v[[2]][h.over.v[[2]]<0] <- 0
			lines(h.over.v, lty=lty[i], lwd=lwd[i], col=col[i])
			if(measured) for(j in 1:length(v.mean)) points(x=v.mean[i,j], y=h[j], col=col[i], pch=pch, cex=cex-0.2)
		}
		
		v.over.h <- profile$profile$v.ref[num.sectors+1] * exp(profile$profile$alpha[num.sectors+1] * log(h.range / h.ref))
		h.over.v <- spline(x=v.over.h, y=h.range, method="natural", xout=v.range)
		h.over.v[[2]][h.over.v[[2]]<0] <- 0
		lines(h.over.v, lty=lty[num.sectors+1], lwd=lwd[num.sectors+1], col=col[num.sectors+1])
		if(measured) for(j in 1:length(v.mean)) points(x=v.mean[num.sectors+1,j], y=h[j], col=col[num.sectors+1], pch=pch, cex=cex-0.2)
		
		if(measured) legend("topright", legend=c(sector.names, "measured"), col=c(col,"darkgrey"), lty=c(lty,NA), lwd=c(lwd,NA), pch=c(rep(NA,num.sectors+1),pch), bty="n", cex=cex-0.2, x.intersp=0.4, y.intersp=0.8)
	else legend("topright", legend=sector.names, col=col, lty=lty, lwd=lwd, bty="n", cex=cex-0.2, x.intersp=0.4, y.intersp=0.8)
	} else { # one sector
		v.over.h <- profile$profile$v.ref[sector] * exp(profile$profile$alpha[sector] * log(h.range / h.ref))
		h.over.v <- spline(x=v.over.h, y=h.range, method="natural", xout=v.range)
		h.over.v[[2]][h.over.v[[2]]<0] <- 0
		plot(h.over.v, type="l", xlim=xlim, ylim=ylim, lty=lty, lwd=lwd, col=col, xlab="Wind speed [m/s]", ylab="Height [m]", cex.lab=cex, cex.axis=cex)
		if(measured) for(j in 1:length(v.mean)) points(x=v.mean[sector,j], y=h[j], col=col, pch=pch, cex=cex-0.2)
		
		if(measured) legend("topright", legend="measured", col=col, pch=pch, bty="n", cex=cex-0.2, y.intersp=0.5)
	}
}

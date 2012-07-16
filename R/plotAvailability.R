plotAvailability <-
function(avail, set, ...) {
### plotting 	availability data
		
	num.sets <- length(avail)
	if(is.null(attr(avail, "call")$func)) stop(paste(substitute(availability), "is no availability object\n"))
	if(attr(avail, "call")$func!="availability") stop(paste(substitute(availability), "is no availability object\n"))
	if(is.null(attr(avail[[1]]$daily, "num.daily.samples"))) stop("Cannot use 'avail' - please use output object of availability\n")
	if(missing(set)) set <- 1:num.sets
	n.set <- length(set)
	if(any(!is.numeric(set))) stop("'set' must be numeric\n")
	if(any(set<1) | any(set>num.sets)) stop("'set' not found\n")
	
	# prepare plot
	old.par <- par(no.readonly=TRUE)
	on.exit(par(old.par))
	
	plot.param <- list(...)
	if(any(names(plot.param)=="col")) color <- plot.param$col
	else color <- c("#B3DE69", "#FFED6F", "#FB8072")
	if(any(names(plot.param)=="cex")) c.cex <- plot.param$cex
	else c.cex <- 1
	if(any(names(plot.param)=="border")) border <- plot.param$border
	else border <- "black"
	
	# plot
	par(mfrow=c(n.set,1), mar=c(0,4,0,0), oma=c(0,0,2,0))
	for(s in 1:n.set) {
		plot.new()
		m <- dim(avail[[set[s]]]$daily)[1]
		d.s <- attr(avail[[set[s]]]$daily, "num.daily.samples")
	
		for (i in 1:m) {
			d <- length(avail[[set[s]]]$daily[i, !is.na(avail[[set[s]]]$daily[i,])])-1
			for (j in 1:d) {
				value <- avail[[set[s]]]$daily[i,j+1]
				if(value==d.s) col <- color[1]
				if(value<d.s & value>0) col <- color[2]
				if(value==0) col <- color[3]
				rect(j/31,1-i/m, (j-1)/31, 1-(i-1)/m, col=col, border=border)
				text((j-0.5)/31, 1-(i-0.5)/m, value, cex=0.4*c.cex)
			}
		}
		if(s==1) {
			mtext("Days", side=3, line=0.7, at=0.5, cex=0.8*c.cex)
			mtext(names(avail[[set[s]]]$daily)[2:32], side=3, line=-0.2, at=((1:31)-0.5)/31, las=1, cex=0.6*c.cex)
		}
		mtext(names(avail)[set[s]], side=2, line=2.6, cex=0.8*c.cex)
		mtext("Months", side=2, line=1.8, cex=0.8*c.cex)
		mtext(row.names(avail[[set[s]]]$daily)[m:1], side=2, line=-0.4, at=((1:m)-0.5)/m, las=1, cex=0.6*c.cex)
	}
}

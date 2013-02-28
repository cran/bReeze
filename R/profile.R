profile <-
function(mast, v.set, dir.set, num.sectors=12, alpha=NULL, digits=3, print=TRUE) {
###	computing profile from mast data
	
	if(is.null(attr(mast, "call"))) stop(paste(substitute(mast), "is no mast object"))
	if(attr(mast, "call")$func!="createMast") stop(paste(substitute(mast), "is no mast object"))
	num.sets <- length(mast$sets)
	if(missing(v.set)) stop("Please choose one or two sets in 'v.set'\n")
	if(any(!is.numeric(v.set)==TRUE)) stop("'v.set' must be numeric\n")
	if(any(v.set<=0) || any(v.set>num.sets)) stop("'v.set' not found\n")
	for(i in 1:length(v.set)) if(is.null(mast$sets[[v.set[i]]]$data$v.avg)) stop(paste("Set", v.set[i], "does not contain average wind speed data\n"))
	if(length(dir.set)>1 || !is.numeric(dir.set)) stop("'dir.set' must be one numeric value\n")
	if(dir.set<=0 || dir.set>num.sets) stop("'dir.set' not found\n")
	if(is.null(mast$sets[[dir.set]]$data$dir.avg)) stop(paste("'dir.set' does not contain average wind direction data\n"))
	if(!is.numeric(num.sectors)) stop("'num.sectors' must be numeric\n")
	if(num.sectors<=1) stop("There must be at least 2 sectors\n")
	
	sector.width <- 360/num.sectors
	sectors <- seq(0, 360-sector.width, by=sector.width)
	sector.edges <- c(sectors-sector.width/2, tail(sectors, n=1)+sector.width/2)%%360
	r.names <- c(paste("s", 1:num.sectors, sep=""),"all")
	if(num.sectors==4) r.names <- c("n","e","s","w","all")
	if(num.sectors==8) r.names <- c("n","ne","e","se","s","sw","w","nw","all")
	if(num.sectors==12) r.names <- c("n","nne","ene","e","ese","sse","s","ssw","wsw","w","wnw","nnw","all")
	if(num.sectors==16) r.names <- c("n","nne","ne","ene","e","ese","se","sse","s","ssw","sw","wsw","w","wnw","nw","nnw","all")
	
	profile <- NULL
	v1 <- mast$sets[[v.set[1]]]$data$v.avg
	h1 <- mast$sets[[v.set[1]]]$height
	dir <- mast$sets[[dir.set]]$data$dir.avg
	
	if(length(v.set)==1) {	# fixed alpha
		if(is.null(alpha)) alpha <- 0.2
		dir <- mast$sets[[dir.set]]$data$dir.avg
		profile <- data.frame(matrix(NA, nrow=num.sectors+1, ncol=2))
		
		#v.ref <- NULL
		for(i in 1:num.sectors) {
			low <- sector.edges[i]
			high <- sector.edges[i+1]
			if(low<high) sector.idx <- dir>=low & dir<high
			else sector.idx <- dir>=low | dir<high
			
			profile[i,1] <- alpha
			profile[i,2] <- mean(v1[sector.idx], na.rm=TRUE)
		}
		profile[num.sectors+1,1] <- alpha
		profile[num.sectors+1,2] <- mean(v1, na.rm=TRUE)
		names(profile) <- c("alpha", "v.ref")
		row.names(profile) <- r.names
		attr(profile, "units") <- c("-", attr(mast$sets[[v.set[1]]]$data$v.avg, "unit"))
	} else if(length(v.set)>1) {
		if(length(v.set)>2) {
			cat("Only one or two sets are supported to calculate the profile - the first two sets from 'v.set' are used\n")
			v.set <- v.set[1:2]
		}
		v2 <- mast$sets[[v.set[2]]]$data$v.avg
		h2 <- mast$sets[[v.set[2]]]$height
		
		if(h1==h2) stop("Sets have the same height - no extrapolation possible\n")
		
		profile <- data.frame(matrix(NA, ncol=num.sectors+1, nrow=length(v1)))
		v.ref <- NULL
		for(i in 1:num.sectors) {
			low <- sector.edges[i]
			high <- sector.edges[i+1]
			if(low<high) sector.idx <- dir>=low & dir<high
			else sector.idx <- dir>=low | dir<high
		
			idx <- !is.na(v1) & v1>0 & !is.na(v2) & v2>0 & sector.idx
			profile[idx,i] <- log(v2[idx] / v1[idx]) / log(h2 / h1)
			v.ref <- append(v.ref, mean(v1[sector.idx], na.rm=TRUE))
		}
		
		profile <- data.frame(cbind(colMeans(profile, na.rm=TRUE), c(v.ref, NA)))
		idx <- !is.na(v1) & v1>0 & !is.na(v2) & v2>0
		profile[num.sectors+1,1] <- mean(log(v2[idx] / v1[idx]) / log(h2 / h1), na.rm=TRUE)
		profile[num.sectors+1,2] <- mean(v1, na.rm=TRUE)
		names(profile) <- c("alpha", "v.ref")
		row.names(profile) <- r.names
		attr(profile, "units") <- c("-", attr(mast$sets[[v.set[1]]]$data$v.avg, "unit"))
	}
	
	profile <- list(profile=round(profile, digits), h.ref=h1)
	attr(profile, "call") <- list(func="profile", mast=deparse(substitute(mast)), v.set=v.set, dir.set=dir.set, num.sectors=num.sectors, alpha=alpha, digits=digits, print=print)
	
	if(print) printObject(profile)
	invisible(profile)
}

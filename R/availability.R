availability <- 
function(mast, v.set, dir.set) {
### check availability for pairs of windspeed and direction - effective data period
	
	if(is.null(attr(mast, "call"))) stop(paste(substitute(mast), "is no mast object"))
	if(attr(mast, "call")$func!="createMast") stop(paste(substitute(mast), "is no mast object"))
	num.sets <- length(mast$sets)
	if(missing(v.set) & missing(dir.set)) v.set <- "all"
	if(!missing(v.set) & missing(dir.set)) dir.set <- v.set
	if(missing(v.set) & !missing(dir.set)) v.set <- dir.set
	
	num.samples <- length(mast$time.stamp)
	start.year <- mast$time.stamp$year[1]+1900
	end.year <- mast$time.stamp$year[num.samples]+1900
	start.month <- mast$time.stamp$mon[1]+1
	end.month <- mast$time.stamp$mon[num.samples]+1
	start.day <- mast$time.stamp$mday[1]
	end.day <- mast$time.stamp$mday[num.samples]
	if(start.year==end.year) num.months <- end.month-start.month+1
	if(start.year!=end.year) num.months <- 13-start.month+end.month + 12*(end.year-start.year-1)
	period.days <- as.numeric(mast$time.stamp[num.samples]-mast$time.stamp[1])
	
	if(v.set!="all") { # one set
		if(!is.numeric(v.set) & !is.numeric(dir.set)) stop("'v.set' and 'dir.set' must be numeric\n") 
		if(v.set<0 | v.set>num.sets) stop("'v.set' not found\n")
		if(dir.set<0 | dir.set>num.sets) stop("'dir.set' not found\n")
		if(is.null(mast$sets[[v.set]]$data$v.avg)) stop("'v.set' does not contain average wind speed data\n")
		if(is.null(mast$sets[[dir.set]]$data$dir.avg)) stop("'dir.set' does not contain average wind direction data\n")
		if(is.null(attr(mast$sets[[v.set]]$data, "cleaned")) | is.null(attr(mast$sets[[dir.set]]$data, "cleaned"))) cat("Set(s) not cleaned - the use of clean is recommended to avoid overestimated availability\n")
		avail <- list(availabilityInt(mast$sets[[v.set]], mast$sets[[dir.set]], mast$time.stamp, start.year, start.month, num.months, period.days))
		if(v.set==dir.set) names(avail) <- names(mast$sets)[v.set]
		else names(avail) <- paste(names(mast$sets)[v.set], "_", names(mast$sets)[dir.set], sep="")
	} else { # all sets
		set.index <- NULL
		for(s in 1:num.sets) if(!is.null(mast$sets[[s]]$data$v.avg) & !is.null(mast$sets[[s]]$data$dir.avg)) set.index <- append(set.index, s)
		if(is.null(set.index)) stop("No pairs of wind speed and wind direction data found")
		avail <- total <- NULL
		uncleaned <- 0
		
		for(s in 1:length(set.index)) {
			avail.s <- availabilityInt(mast$sets[[set.index[s]]], mast$sets[[set.index[s]]], mast$time.stamp, start.year, start.month, num.months, period.days)
			if(!is.null(avail)) avail[[length(avail)+1]] <- avail.s
			if(is.null(avail)) avail <- list(avail.s)
			if(is.null(attr(mast$sets[[set.index[s]]]$data, "cleaned"))) uncleaned <- uncleaned+1
		}
		names(avail) <- names(mast$sets)[set.index]
		if(uncleaned>0) cat(paste(uncleaned, "of", length(set.index), "sets were not cleaned - the use of clean is recommended to avoid overestimated availability\n"))
	}
	
	attr(avail, "call") <- list(func="availability", mast=deparse(substitute(mast)), v.set=v.set, dir.set=dir.set)
	
	return(avail)
}

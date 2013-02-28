monthStats <-
function(mast, set, digits=3, print=TRUE) {
### calculating monthly means

	if(is.null(attr(mast, "call"))) stop(paste(substitute(mast), "is no mast object"))
	if(attr(mast, "call")$func!="createMast") stop(paste(substitute(mast), "is no mast object"))
	if(missing(set)) set <- "all"
	
	m.mean.l <- NULL
	years <- unique(mast$time.stamp$year+1900)
	num.sets <- length(mast$sets)
	unit <- NULL
	
	if(set!="all") { # one set
		if(!is.numeric(set)) stop("'set' must be numeric\n") 
		if(set<0 | set>num.sets) stop("Set not found\n")
		if(is.null(mast$sets[[set]]$data$v.avg)) stop("Specified set does not contain average wind speed data\n")
		m.mean.l <- list(monthStatsInt(mast$sets[[set]]$data$v.avg, mast$time.stamp, years, digits))
		names(m.mean.l) <- names(mast$sets)[set]
		unit <- attr(mast$sets[[set]]$data$v.avg, "unit")	
	} else { # all sets
		set.index <- NULL
		for(s in 1:num.sets) if(!is.null(mast$sets[[s]]$data$v.avg)) set.index <- append(set.index, s)
		
		m.mean.l <- list(monthStatsInt(mast$sets[[set.index[1]]]$data$v.avg, mast$time.stamp, years, digits))
		unit <- attr(mast$sets[[set.index[1]]]$data$v.avg, "unit")
		
		if(length(set.index) > 1) {
			for(s in 2:length(set.index)) {
				m.mean.df <- monthStatsInt(mast$sets[[set.index[s]]]$data$v.avg, mast$time.stamp, years, digits)
				m.mean.l[[length(m.mean.l)+1]] <- m.mean.df
			}
		}
		names(m.mean.l) <- names(mast$sets)[set.index]
	}

	attr(m.mean.l, "unit") <- unit
	attr(m.mean.l, "call") <- list(func="monthStats", mast=deparse(substitute(mast)), set=set, digits=digits)
	
	if(print) printObject(m.mean.l)
	invisible(m.mean.l)
}

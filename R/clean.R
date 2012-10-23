clean <-
function(mast, set, v.avg.min=0.4, v.avg.max=50, dir.clean=TRUE, turb.clean=4, icing=FALSE) {
### cleaning faulty values of mast, set or specified set of mast
	
	r <- NULL
	
	if(missing(mast) & missing(set)) stop("No data to clean - please specify mast and/or set\n")
	if(missing(mast) & !missing(set)) { # set
		if(is.null(attr(set, "call"))) stop(paste(substitute(set), "is no set object"))
		if(attr(set, "call")$func!="createSet") stop(paste(substitute(set), "is no set object"))
		set$data <- cleanInt(set$data, v.avg.min, v.avg.max, dir.clean, turb.clean, icing)
		r <- set
	} else if(!missing(mast) & missing(set)) { # mast
		if(is.null(attr(mast, "call"))) stop(paste(substitute(mast), "is no mast object"))
		if(attr(mast, "call")$func!="createMast") stop(paste(substitute(mast), "is no mast object"))
		num.sets <- length(mast$sets)
		for(s in 1:num.sets) {
			cat(paste("Cleaning set", s, "...\n"))
			mast$sets[[s]]$data <- cleanInt(mast$sets[[s]]$data, v.avg.min, v.avg.max, dir.clean, turb.clean, icing)
		}
		r <- mast
	} else if(!is.null(mast) & !is.null(set)) { # set of mast
		if(is.null(attr(mast, "call"))) stop(paste(substitute(mast), "is no mast object"))
		if(attr(mast, "call")$func!="createMast") stop(paste(substitute(mast), "is no mast object"))
		num.sets <- length(mast$sets)
		if(!is.numeric(set)) stop("Please specify 'set' as number\n")
		if(set<0 | set>num.sets) stop("Set not found\n")
		cat(paste("Cleaning set", set, "...\n"))
		mast$sets[[set]]$data <- cleanInt(mast$sets[[set]]$data, v.avg.min, v.avg.max, dir.clean, turb.clean, icing)
		r <- mast
	}
	
	return(r)
}

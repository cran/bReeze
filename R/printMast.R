printMast <- function(mast) {
### summarising mast information

	if(is.null(attr(mast, "call"))) stop(paste(substitute(mast), "is no mast object"))
	if(attr(mast, "call")$func!="createMast") stop(paste(substitute(mast), "is no mast object"))
	if(!is.null(mast$location)) loc <- mast$location
	if(!is.null(mast$description)) desc <- mast$description
	num.sets <- length(mast$sets)
	num.samples <- nrow(mast$sets[[1]]$data)
	heights <- mast$sets[[1]]$height
	h.unit <- attr(mast$sets[[1]]$height, "unit")
	period.start <- mast$time.stamp[1]
	period.end <- mast$time.stamp[num.samples]
	period.days <- as.numeric(period.end-period.start)
	signals <- names(mast$sets[[1]]$data)
	if(is.null(mast$sets[[1]]$data$v.avg)) wind.speed <- 0
	else wind.speed <- mean(mast$sets[[1]]$data$v.avg, na.rm=TRUE); v.unit <- attr(mast$sets[[1]]$data$v.avg, "unit")
	if(is.null(mast$sets[[1]]$data$v.avg) | is.null(mast$sets[[1]]$data$dir.avg)) avail <- 0 
	else avail <- sum(!is.na(mast$sets[[1]]$data$v.avg) & !is.na(mast$sets[[1]]$data$dir.avg)) * 100 / num.samples
	if(is.null(attr(mast$sets[[1]]$data, "cleaned"))) cleaned <- "not cleaned"
	else cleaned <- unlist(attr(mast$sets[[1]]$data, "cleaned"))
		
	if(num.sets>1) {
		signals <- list(signals)
		cleaned <- list(cleaned)
		for(i in 2:num.sets) {
			heights <- append(heights, mast$sets[[i]]$height)
			signals[[i]] <- names(mast$sets[[i]]$data)
			if(is.null(mast$sets[[i]]$data$v.avg)) wind.speed <- 0 
			else wind.speed <- append(wind.speed, mean(mast$sets[[i]]$data$v.avg, na.rm=TRUE)); v.unit <- attr(mast$sets[[i]]$data$v.avg, "unit")
			if(is.null(mast$sets[[i]]$data$v.avg) | is.null(mast$sets[[i]]$data$dir.avg)) avail <- append(avail, 0)
			else avail <- append(avail, sum(!is.na(mast$sets[[i]]$data$v.avg) & !is.na(mast$sets[[i]]$data$dir.avg)) * 100 / num.samples)
			if(is.null(attr(mast$sets[[i]]$data, "cleaned"))) cleaned[[i]] <- "not cleaned"
			else cleaned[[i]] <- unlist(attr(mast$sets[[i]]$data, "cleaned"))
		}
	}
	
	attr(heights, "unit") <- h.unit
	attr(avail, "unit") <- "%"
	attr(wind.speed, "unit") <- v.unit
	
	if(is.null(mast$location)) {
		if(is.null(mast$description)) r <- list(num.sets=num.sets, heights=heights, signals=signals, period=list(period.start=period.start, period.end=period.end, period.days=period.days), num.samples=num.samples, wind.speed=wind.speed, availability=avail, cleaned=cleaned)
		else r <- list(description=desc, num.sets=num.sets, heights=heights, signals=signals, period=list(period.start=period.start, period.end=period.end, period.days=period.days), num.samples=num.samples, wind.speed=wind.speed, availability=avail, cleaned=cleaned)
	} else {
		if(is.null(mast$description)) r <- list(location=loc, num.sets=num.sets, heights=heights, signals=signals, period=list(period.start=period.start, period.end=period.end, period.days=period.days), num.samples=num.samples, wind.speed=wind.speed, availability=avail, cleaned=cleaned)
		else r <- list(location=loc, description=desc, num.sets=num.sets, heights=heights, signals=signals, period=list(period.start=period.start, period.end=period.end, period.days=period.days), num.samples=num.samples, wind.speed=wind.speed, availability=avail, cleaned=cleaned)
	}
	
	return(r)
}

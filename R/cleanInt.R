cleanInt <-
function(dat, v.avg.min, v.avg.max, dir.clean, turb.clean, icing) {
###	internal function for cleaning data
	
	if(!is.null(v.avg.min) & !is.null(dat$v.avg)) {
		replaced <- length(dat$v.avg[dat$v.avg<v.avg.min & !is.na(dat$v.avg)])
		dat$v.avg[dat$v.avg<v.avg.min] <- NA
		if(replaced>0) cat(paste(replaced, "samples smaller than 'v.avg.min' replaced by 'NA' in average wind speed\n"))
	}
	if(!is.null(v.avg.max) & !is.null(dat$v.avg)) {
		replaced <- length(dat$v.avg[dat$v.avg>v.avg.max & !is.na(dat$v.avg)])
		dat$v.avg[dat$v.avg>v.avg.max] <- NA
		if(replaced>0) cat(paste(replaced, "samples larger than 'v.avg.max' replaced by 'NA' in average wind speed\n"))
	}
	if(dir.clean & !is.null(dat$dir.avg)) {
		replaced <- length(dat$dir.avg[dat$dir.avg<0 | dat$dir.avg>360  & !is.na(dat$dir.avg)])
		dat$dir.avg[dat$dir.avg<0 | dat$dir.avg>360] <- NA
		if(replaced>0) cat(paste(replaced, "samples outside the range of 0-360 replaced by 'NA' in average wind direction\n"))
	}
	if(!is.null(turb.clean) & !is.null(dat$turb.int)) {
		replaced <- length(dat$turb.int[dat$v.avg<turb.clean])
		dat$turb.int[dat$v.avg<turb.clean] <- NA
		if(replaced>0) cat(paste(replaced, "samples with average wind speed lower than", turb.clean, "m/s replaced by 'NA' in turbulence intensity\n"))
	}
	if(icing & !is.null(dat$dir.avg) & !is.null(dat$dir.std)) {
		replaced <- length(dat$dir.avg[dat$dir.std==0])
		dat$dir.avg[dat$dir.std==0] <- NA
		if(replaced>0) cat(paste(replaced, "samples with wind direction standard deviation = 0 replaced by 'NA' in average wind direction\n"))
	}
	
	attr(dat, "cleaned") <- list(v.avg.min=v.avg.min, v.avg.max=v.avg.max, dir.clean=dir.clean, turb.clean=turb.clean, icing=icing)
	
	return(dat)
}

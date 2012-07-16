formatTS <-
function(time.stamp, pattern) {
### formatting time stamp (lookup or with given pattern)
	
	if(anyDuplicated(time.stamp)) stop("'time.stamp' contains duplicates\n")
	ts <- nts <- NULL
	
	if(missing(pattern)) { # search for pattern
		pattern <- c("%d.%m.%Y %H:%M", "%Y.%d.%m %H:%M", "%Y.%d.%m %H:%M", "%d.%m.%Y %H:%M:%S", "%d.%m.%Y %H:%M:%S", "%Y.%d.%m %H:%M:%S", "%Y.%d.%m %H:%M:%S", "%d.%m.%y %H:%M", "%d.%m.%y %H:%M", "%y.%d.%m %H:%M", "%y.%d.%m %H:%M", "%d.%m.%y %H:%M:%S", "%d.%m.%y %H:%M:%S", "%y.%d.%m %H:%M:%S", "%y.%d.%m %H:%M:%S", "%d-%m-%Y %H:%M", "%d-%m-%Y %H:%M", "%Y-%d-%m %H:%M", "%Y-%d-%m %H:%M", "%Y-%m-%d %H:%M", "%d-%m-%Y %H:%M:%S", "%d-%m-%Y %H:%M:%S", "%Y-%m-%d %H:%M:%S", "%Y-%d-%m %H:%M:%S", "%d-%m-%y %H:%M", "%d-%m-%y %H:%M", "%y-%d-%m %H:%M", "%y-%d-%m %H:%M", "%d-%m-%y %H:%M:%S", "%d-%m-%y %H:%M:%S", "%y-%d-%m %H:%M:%S", "%y-%d-%m %H:%M:%S", "%d/%m/%Y %H:%M", "%d/%m/%Y %H:%M", "%Y/%d/%m %H:%M", "%Y/%d/%m %H:%M", "%d/%m/%Y %H:%M:%S", "%d/%m/%Y %H:%M:%S", "%Y/%d/%m %H:%M:%S", "%Y/%d/%m %H:%M:%S", "%d/%m/%y %H:%M", "%d/%m/%y %H:%M", "%y/%d/%m %H:%M", "%y/%d/%m %H:%M", "%d/%m/%y %H:%M:%S", "%d/%m/%y %H:%M:%S", "%y/%d/%m %H:%M:%S", "%y/%d/%m %H:%M:%S", "%d_%m_%Y %H:%M", "%d_%m_%Y %H:%M", "%Y_%d_%m %H:%M", "%Y_%d_%m %H:%M", "%d_%m_%Y %H:%M:%S", "%d_%m_%Y %H:%M:%S", "%Y_%d_%m %H:%M:%S", "%Y_%d_%m %H:%M:%S", "%d_%m_%y %H:%M", "%d_%m_%y %H:%M", "%y_%d_%m %H:%M", "%y_%d_%m %H:%M", "%d_%m_%y %H:%M:%S", "%d_%m_%y %H:%M:%S", "%y_%d_%m %H:%M:%S", "%y_%d_%m %H:%M:%S", "%d%m%Y %H:%M", "%d%m%Y %H:%M", "%Y%d%m %H:%M", "%Y%d%m %H:%M", "%d%m%Y %H:%M:%S", "%d%m%Y %H:%M:%S", "%Y%d%m %H:%M:%S", "%Y%d%m %H:%M:%S", "%d%m%y %H:%M", "%d%m%y %H:%M", "%y%d%m %H:%M", "%y%d%m %H:%M", "%d%m%y %H:%M:%S", "%d%m%y %H:%M:%S", "%y%d%m %H:%M:%S", "%y%d%m %H:%M:%S")
		
		for(i in 1:length(pattern)) {
			nts <- strptime(time.stamp[1], pattern[i])
			if(is.na(nts)) {
				if(substr(pattern[i], nchar(pattern[i]), nchar(pattern[i]))=="S") nts <- strptime(paste(time.stamp[1], "00:00:00"), pattern[i])
				if(substr(pattern[i], nchar(pattern[i]), nchar(pattern[i]))=="M") nts <- strptime(paste(time.stamp[1], "00:00"), pattern[i])
			}
			if(!is.na(nts) & substr(nts,1,2)!="00") {
				nts <- strptime(time.stamp, pattern[i])
				if(any(is.na(nts)==TRUE)) {
					if(substr(pattern[i], nchar(pattern[i]), nchar(pattern[i]))=="S") nts[which(is.na(nts)==TRUE)] <- strptime(paste(time.stamp[which(is.na(nts)==TRUE)], "00:00:00"), pattern[i])
					if(substr(pattern[i], nchar(pattern[i]), nchar(pattern[i]))=="M") nts[which(is.na(nts)==TRUE)] <- strptime(paste(time.stamp[which(is.na(nts)==TRUE)], "00:00"), pattern[i])
				}
				if(!any(is.na(nts)==TRUE)) {
					cat(paste("Pattern found:", pattern[i], "\n"))
					break
				}
			}
		}
		if(length(nts)==1) stop("No pattern found\n")
	} else { # pattern specified
		nts <- strptime(time.stamp, pattern)
		if(any(is.na(nts)==TRUE)) {
			if(substr(pattern, nchar(pattern), nchar(pattern))=="S") nts[which(is.na(nts)==TRUE)] <- strptime(paste(time.stamp[which(is.na(nts)==TRUE)], "00:00:00"), pattern)
			if(substr(pattern, nchar(pattern), nchar(pattern))=="M") nts[which(is.na(nts)==TRUE)] <- strptime(paste(time.stamp[which(is.na(nts)==TRUE)], "00:00"), pattern)
		}
		if(any(is.na(nts)==TRUE) | substr(nts[1],1,2)=="00") stop("Pattern does not match\n")
	}
	
	return(nts)
}

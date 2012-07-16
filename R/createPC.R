createPC <-
function(v, p, cp, ct, rho=1.225, rated.p) {
###	creating power curve object

	if(missing(v)) stop("Wind speed 'v' is mandatory\n")
	if(!is.vector(v)) stop("'v' requires numeric vector\n")
	if(missing(p)) stop("Power 'p' is mandatory\n")
	if(!is.vector(p)) stop("'p' requires numeric vector\n")
	if(length(v)!=length(p)) stop("Different vector length of 'v' and 'p'\n")
	if(missing(cp)) cp <- NULL
	if(!is.null(cp)) if(!is.vector(cp)) stop("'cp' requires numeric vector\n")
	if(!is.null(cp)) if(length(v)!=length(cp)) stop("Different vector length of 'v' and 'p'\n")
	if(missing(ct)) ct <- NULL
	if(!is.null(ct)) if(!is.vector(ct)) stop("'ct' requires numeric vector\n")
	if(!is.null(ct)) if(length(v)!=length(ct)) stop("Different vector length of 'v' and 'cp'\n")
	if(missing(rated.p)) rated.p <- NULL
	
	pc <- data.frame(cbind(v, p, cp, ct))
	names <- c("v", "P")
	if(!is.null(cp)) names <- append(names, "cp")
	if(!is.null(ct)) names <- append(names, "ct")
	names(pc) <- names
	attr(pc, "units") <- c("m/s", "kW", "-")
	attr(pc, "rho") <- rho
	if(!is.null(rated.p)) attr(pc, "rated.power") <- rated.p
	attr(pc, "call") <- list(func="createPC")
	
	return(pc)
}

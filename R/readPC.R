readPC <-
function(file) {
### importing power curve from WAsP .wgt file or WindPower program .pow file
	
	type <- substr(file, nchar(file)-3, nchar(file))
	if(!any(c(".pow", ".wtg")==type)) stop("Cannot handle file - only WAsP .wtg files and WindPower program .pow files are supported\n")
	
	r <- NULL
	if(type==".pow") {
		pow <- read.table(file, as.is=TRUE)
		cut.out <- as.numeric(pow[4,1])
		if(is.na(cut.out) | is.null(cut.out)) stop("Cannot handle file")
		v <- seq(1, cut.out, 1)
		p <- tail(pow, -1)
		options(warn=-1)
		if(is.na(as.numeric(tail(p, 1)))) p <- head(p, -1)
		options(warn=1)
		p <- as.numeric(p[5:(cut.out+4),1])
		r <- createPC(v, p, rho=1.225)
		attr(r, "call") <- list(func="readPC", file=file)
	} else if(type==".wtg") {
		stopifnot(require(XML))
		wtg <- xmlTreeParse(file, asTree=TRUE)
		if(is.null(wtg$doc$children$WindTurbineGenerator)) stop("Cannot handle file\n")
		n <- length(wtg$doc$children$WindTurbineGenerator)
		idx <- 3
		if(n>4) {
			rho <- NULL
			for(i in 3:(n-1)) rho <- append(rho, as.numeric(xmlAttrs(wtg$doc$children$WindTurbineGenerator[[i]])[["AirDensity"]]))
			idx <- which.min(abs(rho-1.225))+2
		}
		rho <- as.numeric(xmlAttrs(wtg$doc$children$WindTurbineGenerator[[idx]])[["AirDensity"]])
		n <- length(wtg$doc$children$WindTurbineGenerator[[idx]][["DataTable"]])
		v <- p <- ct <- NULL
		for(i in 1:n) {
			v <- append(v, as.numeric(xmlAttrs(wtg$doc$children$WindTurbineGenerator[[idx]][["DataTable"]][[i]])[["WindSpeed"]]))
			p <- append(p, as.numeric(xmlAttrs(wtg$doc$children$WindTurbineGenerator[[idx]][["DataTable"]][[i]])[["PowerOutput"]])/1000)
			ct <- append(ct, as.numeric(xmlAttrs(wtg$doc$children$WindTurbineGenerator[[idx]][["DataTable"]][[i]])[["ThrustCoEfficient"]]))
		}
		r <- createPC(v=v, p=p, ct=ct, rho=rho)
		attr(r, "call") <- list(func="readPC", file=file)
	}
		
	return(r)
}

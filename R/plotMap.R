plotMap <-
function(mast, type=c("sat", "map"), width=0.02, height=0.02, plot.coords=TRUE, ...) {
### plotting map or satellite image of mast location

	Sys.setenv(NOAWT=1)	# for OpenStreetMap on Mac
	stopifnot(require(OpenStreetMap))
	
	if(is.null(attr(mast, "call"))) stop(paste(substitute(mast), "is no mast object"))
	if(attr(mast, "call")$func!="createMast") stop(paste(substitute(mast), "is no mast object"))
	if(missing(type)) type <- "sat"
	type <- match.arg(type)
	if(type=="sat") type <- "bing"
	if(type=="map") type <- "osm"
	if(is.null(mast$location)) stop("No location found\n")
	lat <- mast$location[1]
	lon <- mast$location[2]
	if(missing(width)) width <- 0.02
	if(missing(height)) height <- width
	
	if(lat+height/2>85 | lat-height/2<(-85) | lon+width/2>179.9999999999 | lon-width/2<(-179.9999999999)) stop("Coordinates out of plot range\n")
	map <- openmap(c(lat+height/2,lon-width/2), c(lat-height/2,lon+width/2), type=type)
	
	plot.param <- list(...)
	old.par <- par(no.readonly=TRUE)
    on.exit(par(old.par))
    par(mar=c(0,0,0,0))
    
    if(any(names(plot.param)=="pch")) pch <- plot.param$pch
	else pch <- 8
	if(any(names(plot.param)=="col")) col <- plot.param$col
	else col <- "#E41A1C"
	if(any(names(plot.param)=="cex")) cex <- plot.param$cex
	else cex <- 1.2
		
	x1 <- map$bbox$p1[1]
	y1 <- map$bbox$p1[2]
	x2 <- map$bbox$p2[1]
	y2 <- map$bbox$p2[2]
	plot(map)
	points(mean(c(x1,x2)), mean(c(y1,y2)), pch=pch, col=col, cex=cex)
	if(plot.coords) text(mean(c(x1,x2)), mean(c(y1,y2)), paste(lat, lon, sep=","), pos=4, col=col, cex=cex-0.4)
}

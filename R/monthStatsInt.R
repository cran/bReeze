monthStatsInt <-
function(data, ts, years, digits) {
### internal function for calculation of month.stats

	m.mean <- matrix(nrow=14, ncol=length(years)+2)
	
	for(y in 1:length(years)) {
		for(m in 1:12) {
			m.mean[m,y] <- mean(data[ts$year==years[y]-1900 & ts$mon==m-1], na.rm=TRUE)
			m.mean[m,length(years)+1] <- mean(data[ts$mon==m-1], na.rm=TRUE)
			m.mean[m,length(years)+2] <- mean(m.mean[m,1:length(years)], na.rm=TRUE)
		}
		m.mean[13,y] <- mean(data[ts$year==years[y]-1900], na.rm=TRUE)
		m.mean[14,y] <- mean(m.mean[1:12,y], na.rm=TRUE)
	}
	
	m.mean[13,length(years)+1] <- mean(data, na.rm=TRUE)
	m.mean[14,length(years)+2] <- mean(m.mean[1:12,length(years)+2], na.rm=TRUE)
	
	m.mean.df <- data.frame(m.mean, row.names=c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec","mean","mean.of.months"))
	names(m.mean.df) <- c(years, "mean", "mean.of.months")
		
	for(i in 1:length(m.mean.df)) m.mean.df[,i][is.nan(m.mean.df[,i]) | m.mean.df[,i]==0] <- NA
	
	m.mean.df <- round(m.mean.df, digits)
	return(m.mean.df)
}

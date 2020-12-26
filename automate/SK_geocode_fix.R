# fix outlier latlong
require(mapboxapi)

dat <- read.delim("SK_schools_geocoded_201213.csv",sep=",",
	h=T,as.is=T)

zlat <- (dat$Latitude-mean(dat$Latitude))/dat$Latitude
zlong <- (dat$Longitude-mean(dat$Longitude))/dat$Longitude
outlier <- union(which(abs(zlat)>0.2),which(abs(zlong)>0.2))
message(sprintf("%i places with |Z| > 0.2",length(outlier)))
bad <- list()
for (k in 1:length(outlier)) {
	cur <- as.numeric(dat[outlier[k],c("Longitude","Latitude")])
 	bad[[k]] <- mb_reverse_geocode(coordinates=dat[outlier[k],c("Longitude","Latitude")])
}
bad <- unlist(bad)

bad2 <- dat[outlier,]
bad2$Bad.Location <- bad
write.table(bad2,file="SK_badgeocode_tofix.txt",sep="\t",col=T,row=F,quote=T)



# get latlong for all AB schools
rm(list=ls())

require(ggmap)
source("~/software/google.R")
register_google(getGoogleKey())

inFile <- "/Users/shraddhapai/Google_covidschools/SchoolInfo/AB/Alberta_Schools_with_geolocations.csv"
dat <- read.delim(inFile, sep = ",", h = T, as.is = T)
idx <- union(which(dat$Lat==""),which(dat$Long==""))
dat <- dat[idx,]
dat <- dat[-which(dat$School.Name==""),]


blah <- paste(dat$School.Address1, dat$School.Address2,sep = ",")
blah2 <- paste(blah, dat$School.City,sep=",")
blah3 <- paste(blah2, dat$School.Postal.Code, sep = ",")
blah4 <- paste(blah3, "Canada", sep = ",")
blah4 <- paste(dat$School.Name, blah4,sep = ",")
blah4 <- gsub(",,",",",blah4)
blah4 <- gsub("\\n","",blah4)

#out <- geocode(blah4)
out <- list()
for (k in 1:length(blah4)) {
	out[[k]] <- NA
	tryCatch({
		out [[k]] <- geocode(blah4[k])
	}, error=function(ex){
		print(ex)
	},finally={
	})
}
browser()
out2 <- do.call("rbind",out)
out2 <- as.data.frame(out)


dat$Lat <- out2$lat
dat$Long <- out2$lon
write.table(dat,file="AB_geocode_missing.txt",sep=",",col=T,row=F,quote=T)



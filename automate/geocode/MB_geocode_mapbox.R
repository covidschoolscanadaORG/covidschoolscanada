# get latlong for all AB schools
rm(list=ls())
require(mapboxapi)

token <- Sys.getenv("MAPBOX_PUBLIC_TOKEN")

inFile <- "/Users/shraddhapai/Google_covidschools/SchoolInfo/MB/MB_addresses_parsed.txt"
dat <- read.delim(inFile, sep = "\t", h = T, as.is = T)
for (k in 1:ncol(dat))
  dat[, k] <- stringr::str_trim(dat[, k])

blah <- paste(dat[,1],dat[,2],sep=",")

#mb_geocode()
out <- list()
for (k in 1:length(blah)) {
  out[[k]] <- NA
  tryCatch({
    message(sprintf("%i: %s", k, blah[k]))
    out[[k]] <- mb_geocode(blah[k], access_token = token)
  }, error = function(ex) {
    print(ex)
  }, finally = {

  })
}

browser()

# these steps were manually done after making sure there were no NULLs or NAs
out2 <- do.call("rbind", out)
dat$Latitude <- out2[, 2]
dat$Longitude <- out2[, 1]
write.table(dat, file = "MB_schools_geocoded_201205.csv", 
	sep = ",", col = T, row = F, quote = T)

message("FIND OUTLIER POINTS")


zlat <- (out2[,2]-mean(out2[,2]))/out2[,2]
zlong <- (out2[,1]-mean(out2[,1]))/out2[,1]
outlier <- union(which(abs(zlat)>0.2),which(abs(zlong)>0.2))
message(sprintf("%i places with |Z| > 0.2",length(outlier)))
bad <- list()
for (k in 1:length(outlier)) {
 	bad[[k]] <- mb_reverse_geocode(coordinates=out2[outlier[k],])
}
bad <- unlist(bad)

bad2 <- dat[outlier,]
bad2$Bad.Location <- bad
write.table(bad2,file="MB_badgeocode_tofix.txt",sep="\t",col=T,row=F,quote=T)


rm(list=ls())

require(mapboxapi)
token <- Sys.getenv("MAPBOX_PUBLIC_TOKEN")
inFile <- "CEQ_VOC.txt"
dat <- read.delim(inFile,sep=",",h=T,as.is=T,skip=3)

blah <- paste(dat[,2], dat[,4], sep = ",")
blah2 <- paste(blah, "Quebec", sep = ",")
blah2 <- paste(blah, "Canada", sep = ",")

out <- list()
for (k in 1:10)  { #length(blah4)) {
  out[[k]] <- NA
  tryCatch({
    message(sprintf("%i: %s", k, blah2[k]))
    out[[k]] <- mb_geocode(blah2[k], access_token = token)
  }, error = function(ex) {
    print(ex)
  }, finally = {

  })
}


# get latlong for all AB schools
require(mapboxapi)

token <- Sys.getenv("MAPBOX_PUBLIC_TOKEN")

inFile <- "/Users/shraddhapai/Google_covidschools/SchoolInfo/BC/excelSchoolContact.csv"
dat <- read.delim(inFile, sep = ",", h = T, as.is = T)
dat <- dat[, 1:7]
for (k in 1:7)
  dat[, k] <- stringr::str_trim(dat[, k])

blah <- paste(dat$Address, dat$City, sep = ",")
blah2 <- paste(blah, "British Columbia", sep = ",")
blah3 <- paste(blah2, dat$Postal.Code, sep = ",")
blah4 <- paste(blah3, "Canada", sep = ",")
blah4 <- paste(dat$School.Name, blah4, "Canada", sep = ",")

#mb_geocode()
out <- list()
for (k in 1:length(blah4)) {
  out[[k]] <- NA
  tryCatch({
    message(sprintf("%i: %s", k, blah4[k]))
    out[[k]] <- mb_geocode(blah4[k], access_token = token)
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
write.table(dat, file = "BC_schools_geocoded_201128.csv", sep = ",", col = T, row = F, quote = F)
write.table(dat[1:2000,], file = "BC_201128_till2000.csv", sep = ",", col = T, row = F, quote = T)
write.table(dat[2001:nrow(dat),], file = "BC_201128_2001_beyond.csv", sep = ",", col = T, row = F, quote = T)
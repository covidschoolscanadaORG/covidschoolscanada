# get latlong for all AB schools
require(mapboxapi)

token <- Sys.getenv("MAPBOX_PUBLIC_TOKEN")

inFile <- "/Users/shraddhapai/Google_covidschools/SchoolInfo/SK/Active_List_of_School_Division_Schools_2019_20.csv"
dat <- read.delim(inFile, sep = ",", h = T, as.is = T)
dat <- dat[-1, 1:9]
dat <- dat[1:796,]
#for (k in 1:7)
#  dat[, k] <- stringr::str_trim(dat[, k])

blah <- paste(dat$ADDRESS, dat$TOWN.CITY, sep = ",")
blah2 <- paste(blah, "Saskatchewan", sep = ",")
blah3 <- paste(blah2, dat$PC, sep = ",")
blah4 <- paste(dat$SCHOOL.PROGRAM.NAME, blah3, "Canada", sep = ",")

#mb_geocode()
out <- list()
for (k in 1:10)  { #length(blah4)) {
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
write.table(dat, file = "SK_schools_geocoded_201128.csv", sep = ",", col = T, row = F, quote = F)

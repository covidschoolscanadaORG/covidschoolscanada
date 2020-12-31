# get latlong for all AB schools

inFile <- "/Users/shraddhapai/Google_covidschools/SchoolInfo/BC/excelSchoolContact.csv"
dat <- read.delim(inFile, sep = ",", h = T, as.is = T)
dat <- dat[, 1:7]
for (k in 1:7)
  dat[, k] <- stringr::str_trim(dat[, k])

blah <- paste(dat$Address, dat$City, sep = ",")
blah2 <- paste(blah, "British Columbia", sep = ",")
blah3 <- paste(blah2, dat$Postal.Code, sep = ",")
blah4 <- paste(blah3, "Canada", sep = ",")
#blah4 <- paste(dat$School.Name, blah4, "Canada", sep = ",")

out <- list()
outraw <- list()
for (k in 1:20) {
  #length(blah4)) {
  message(sprintf("%i: %s", k, blah4[k]))
  out[[k]] <- NA
  outraw[[k]] <- NA
  tryCatch({
    tmp <- photon::geocode(blah4[[k]])
    if (nrow(tmp) == 1) {
      if (any(grep("Not found", tmp[length(tmp)]))) {
        message("\t not found")
        tmp <- NULL
      } else {
        message("\tfound!")
      }
    }
    outraw[[k]] <- tmp

    if (!is.na(tmp)) {
      if (nrow(tmp) > 1 | !is.na(tmp$city[1])) {
        idx <- which(tmp$city == dat$School.City[k])
        if (any(idx)) {
          tmp <- tmp[idx,, drop = FALSE]
        }

        idx <- which(tmp$osm_value %in% c("service", "retail", "seafood", "restaurant",
              "bar", "apartments", "residential", "house_number", "construction", "track",
              "quarter", "suburb", "dock", "commercial", "secondary", "college", "university",
              "motorway", "trunk", "monument", "highway", "supermarket"))
        if (any(idx)) {
          tmp <- tmp[-idx,, drop = FALSE]
        }
        idx <- which(tmp$osm_key %in% c("natural"))
        if (any(idx)) {
          tmp <- tmp[-idx,, drop = FALSE]
        }


        idx <- which(tmp$osm_value == "school")
        if (any(idx)) {
          tmp <- tmp[idx,, drop = FALSE]
        }
      }
    }

    if (nrow(tmp) < 1) tmp <- NULL
    out[[k]] <- tmp
  }, error = function(ex) {
    print(ex)
  })

  if (k %% 50 == 0) {
    message("Saving")
    save(out, blah4, file = sprintf("%s/AB_geocode_%i.rda", dirname(inFile), k))
  }
}

idx <- which(!is.null(out))


message("done")
outFile <- sprintf("%s_geocode.txt", inFile)


# get latlong for all AB schools

require(photon)
inFile <- "/Users/shraddhapai/Google_covidschools/Notes/AB/AB_authority_and_school.txt"
dat <- read.delim(inFile,sep="\t",h=T,as.is=T,skip=2)

pc <- paste(substr(dat$School.Postal.Code,1,3),
						substr(dat$School.Postal.Code,4,6),
						sep=" ")
blah <- paste(dat$School.Address1,dat$School.City,sep=",")
blah2 <- paste(blah,"Alberta",sep=",")
blah3 <- paste(blah2,pc,sep=",")
blah4 <- paste(blah3,"Canada",sep=",")

out <- list()
for (k in 1:length(blah4)) {
	message(sprintf("%i: %s",k,blah4[k]))
	out[[k]] <- NA
	tryCatch({
			tmp <- geocode(blah4[k])
			tmp$city <- sub("City of ","",tmp$city)
			tmp$city <- sub("Town of ","",tmp$city)
			if (nrow(tmp)>1 | !is.na(tmp$city[1])){
				idx <- which(tmp$city == dat$School.City[k])
				if (any(idx)) {
					tmp <- tmp[idx,,drop=FALSE]
				}

				tmp <- tmp[which(tmp$postcode == pc[k]),,drop=FALSE]
				idx <- which(tmp$osm_value %in% c("service","retail","seafood","restaurant",
							"bar","apartments","residential","house_number","construction","track",
							"quarter","suburb","dock","commercial","secondary","college","university",
							"motorway","trunk","monument"))
				if (any(idx)) {
					tmp <- tmp[-idx,,drop=FALSE]
				}
				idx <- which(tmp$osm_value=="school")
				if (any(idx)) {
					tmp <- tmp[idx,,drop=FALSE]
				}
			}
			if (nrow(tmp)<1) {
				tmp <- rep(NA,15)
			}
			out[[k]] <- tmp
	}, error=function(ex){
		print(ex)
	})

	if (k %% 50 == 0) {
		message("Saving")
		save(out,blah4,file=sprintf("%s/AB_geocode_%i.rda",dirname(inFile),k))
	}
}

message("done")
outFile <- sprintf("%s_geocode.txt",inFile)


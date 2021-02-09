# identifies missing letters
rm(list=ls())
inFile <- "/Users/shraddhapai/Google_covidschools/daily_data/Canada_COVID_tracker/export-201208AMfreeze/final_data/CanadaMap_QuebecMerge-201208.clean.csv"

dat <- read.delim(inFile,sep=",",h=T,as.is=T)
dat <- dat[-which(dat$Province %in% c("QC","BC","AB")),]

print(table(dat$Province))

bad <- list()
for (k in 1:nrow(dat)) {
	message(sprintf("%s",k))
	art <- dat$Article[k]
	cur <- unlist(strsplit(art,";"))
	for (n in 1:length(cur)){
		if (!url.exists(cur[n])){
			message(sprintf("\t%s",cur[n]))	
			message("BAD!")
			tmp <- dat[k,]
			tmp$BAD_ARTICLE <- cur[n]
			
			bad[[k]] <- tmp
		} 
	}
}

final <- do.call("rbind",bad)

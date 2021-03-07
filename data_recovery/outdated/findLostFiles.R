# identifies missing letters
rm(list=ls())
inFile <- "/Users/shraddhapai/Google_covidschools/daily_data/Canada_COVID_tracker/export-201208AMfreeze/final_data/CanadaMap_QuebecMerge-201208.clean.csv"

dat <- read.delim(inFile,sep=",",h=T,as.is=T)
dat <- dat[-which(dat$Province %in% c("QC","BC","AB")),]

print(table(dat$Province))

bad <- list()
for (k in setdiff(1287:nrow(dat),1302)) {
	message(sprintf("%s",k))
	art <- dat$Article[k]
	cur <- unlist(strsplit(art,";"))
	for (n in 1:length(cur)){
		if (!url.exists(cur[n])){
			message(sprintf("\t%s",cur[n]))	
			message("BAD!")
			tmp <- dat[k,]
				tryCatch({
					if (identical(cur[n],character(0))) {
						tmp$BAD_ARTICLE <- "EMPTY"
				} else if (is.na(cur[n])) {
					message("empty")
					tmp$BAD_ARTICLE <- "EMPTY"
				} else {
					tmp$BAD_ARTICLE <- cur[n]
				}}, error=function(ex){
					print(ex)
					browser()
				},finally={
						
				})
				bad[[k]] <- tmp
		} 
	}
}

final <- do.call("rbind",bad)
write.table(final,file="Missing_part2.txt",sep=",",col=T,row=F,quote=F)

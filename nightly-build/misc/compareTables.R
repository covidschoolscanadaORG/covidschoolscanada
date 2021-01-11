# compare tables from different days

getTable <- function(dt) {
	message(sprintf("Fetching %s",dt))
	inDir <- sprintf("/home/shraddhapai/Canada_COVID_tracker/export-%s",dt)
	inFile <- sprintf("%s/CanadaMap_QuebecMerge-%s.clean.csv",
		inDir,dt)
	dat <- read.delim(inFile,sep=",",h=T,as.is=T)
	return(dat)
}

today <- getTable("201015")
yest <- getTable("201014")

idx <- which(!yest$institute.name %in% today$institute.name)
browser()
print(yest[idx,])


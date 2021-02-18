# create QC cumgraph


getQC_cumGraph <- function() {
inDir <- "/Users/shraddhapai/Google_covidschools/daily_data/QC"
dtseq <- seq(from=as.Date("2020-10-12"),
	to=Sys.Date(),by=1)
#dtseq <- format(dtseq,"%y%m%d")

totcase <- list()
for (k in 1:length(dtseq)) {
	dt2 <- format(dtseq[k],"%y%m%d")
	inFile<- sprintf("%s/CEQ_annotated_clean_%s.csv",inDir,dt2)
#	message(dtseq[k])
	if (file.exists(inFile)) {
#			message("\tEXISTS")
			dat <- read.delim(inFile,sep=",",h=T,as.is=T)
			totcase[[k]] <- c(as.character(dtseq[k]),
				sum(dat$Total.cases.to.date))
	}
}

tot <- do.call("rbind",totcase)
blah <- as.data.frame(tot)
blah[,1] <- as.Date(blah[,1])
blah[,2] <- as.integer(blah[,2])
#plot(blah[,1],blah[,2])

return(blah)
}




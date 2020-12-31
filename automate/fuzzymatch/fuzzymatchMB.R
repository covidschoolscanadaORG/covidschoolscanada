# fuzzy-match bc Alberta data to us
rm(list=ls())
require(tibble)
require(fuzzyjoin)

dat <- read.delim("/Users/shraddhapai/Google_covidschools/daily_data/Canada_COVID_tracker/export-201207AMfreeze/final_data/CanadaMap_QuebecMerge-201207.clean.csv",
	sep=",",h=T,as.is=T)
dat <- subset(dat, Province=="MB")
dat$institute.name <- stringr::str_trim(dat$institute.name)

mb <- read.delim("/Users/shraddhapai/Google_covidschools/SchoolInfo/MB/MB_addresses_parsed.txt",
	sep="\t",h=T,as.is=T)
browser()

mb$School.Name <- stringr::str_trim(mb$School.Name)

dat_t <- as_tibble(dat)
mb_t<- as_tibble(mb)

good<- stringdist_join(x=dat_t,y=mb_t, by=c(institute.name="School.Name"),max_dist=0)
message(sprintf("0 MM = %i schools", nrow(good)))

the_rest <- dat_t[-which(dat_t$institute.name %in% good$institute.name),]

MAX_MISMATCH <- 13
mm <- 0
while (nrow(the_rest)>1 & mm < MAX_MISMATCH) {
	mm <- mm + 1
	cur <- stringdist_join(x=the_rest,y=mb_t, 
		by=c(institute.name="School.Name"),max_dist=mm)
	message(sprintf("%i MM = %i schools", mm,
		length(unique(cur$institute.name))))
	good <- rbind(good, cur)
	the_rest <- dat_t[-which(dat_t$institute.name %in% good$institute.name),]
}

write.table(good,file="mb_match.csv",sep=",",col=T,row=F,quote=T)
write.table(the_rest,file="mb_unmatch.csv",sep=",",col=T,row=F,quote=T)

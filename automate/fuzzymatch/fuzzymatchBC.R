# fuzzy-match bc Alberta data to us
rm(list=ls())
require(tibble)
require(fuzzyjoin)

dat <- read.delim("/Users/shraddhapai/Google_covidschools/daily_data/Canada_COVID_tracker/export-201127AMfreeze/final_data/CanadaMap_QuebecMerge-201127.clean.csv",sep=",",h=T,as.is=T)
dat <- subset(dat, Province=="BC")
dat$institute.name <- stringr::str_trim(dat$institute.name)

bc <- read.delim("/Users/shraddhapai/Google_covidschools/SchoolInfo/BC/excelSchoolContact.csv",
	sep=",",h=T,as.is=T)
bc <- bc[,1:7]

bc$School.Name <- stringr::str_trim(bc$School.Name)

dat_t <- as_tibble(dat)
bc_t<- as_tibble(bc)

good<- stringdist_join(x=dat_t,y=bc_t, by=c(institute.name="School.Name"),max_dist=0)
message(sprintf("0 MM = %i schools", nrow(good)))

the_rest <- dat_t[-which(dat_t$institute.name %in% good$institute.name),]

MAX_MISMATCH <- 13
mm <- 0
while (nrow(the_rest)>1 & mm < MAX_MISMATCH) {
	mm <- mm + 1
	cur <- stringdist_join(x=the_rest,y=bc_t, 
		by=c(institute.name="School.Name"),max_dist=mm)
	message(sprintf("%i MM = %i schools", mm,
		length(unique(cur$institute.name))))
	good <- rbind(good, cur)
	the_rest <- dat_t[-which(dat_t$institute.name %in% good$institute.name),]
}

write.table(good,file="bc_match.txt",sep="\t",col=T,row=F,quote=T)
write.table(the_rest,file="bc_unmatch.txt",sep="\t",col=T,row=F,quote=T)

# fuzzy-match SOS Alberta data to us
rm(list=ls())
require(tibble)
require(fuzzyjoin)

dat <- read.delim("CanadaMap_QuebecMerge-201105.clean.csv",sep=",",h=T,as.is=T)
dat <- subset(dat, Province=="AB")
dat$institute.name <- gsub(" \\| Calgary Board of Education","",
	dat$institute.name)
dat$institute.name <- stringr::str_trim(dat$institute.name)

sos <- read.delim("/Users/shraddhapai/Google_covidschools/Notes/AB/2020-11-05T154530.csv",sep=",",h=T,as.is=T,comment="#")
sos$School <- stringr::str_trim(sos$School)

dat_t <- as_tibble(dat)
sos_t<- as_tibble(sos)

good<- stringdist_join(x=dat_t,y=sos_t, by=c(institute.name="School"),max_dist=0)
message(sprintf("0 MM = %i schools", nrow(good)))

the_rest <- dat_t[-which(dat_t$institute.name %in% good$institute.name),]

MAX_MISMATCH <- 13
mm <- 0
while (nrow(the_rest)>1 & mm < MAX_MISMATCH) {
	mm <- mm + 1
	cur <- stringdist_join(x=the_rest,y=sos_t, 
		by=c(institute.name="School"),max_dist=mm)
	message(sprintf("%i MM = %i schools", mm,
		length(unique(cur$institute.name))))
	good <- rbind(good, cur)
	the_rest <- dat_t[-which(dat_t$institute.name %in% good$institute.name),]
}

write.table(good,file="SOS_match.txt",sep="\t",col=T,row=F,quote=T)
write.table(the_rest,file="SOS_unmatch.txt",sep="\t",col=T,row=F,quote=T)

# convert MB schools to new format
rm(list=ls())

# fuzzy match google map with provincial
fuzzyMatch_Prov <- function(dat) {
require(tibble)
require(fuzzyjoin)

dt <- format(Sys.Date(),"%y%m%d")
dat$institute.name <- stringr::str_trim(dat$institute.name)

mb <- read.delim("/Users/shraddhapai/Google_covidschools/SchoolInfo/MB/MB_addresses_parsed.txt",
	sep="\t",h=T,as.is=T)
mb$School.Name <- stringr::str_trim(mb$School.Name)

dat_t <- as_tibble(dat)
mb_t<- as_tibble(mb)
good<- stringdist_join(x=dat_t,y=mb_t, 
	by=c(institute.name="School.Name"),max_dist=0)
message(sprintf("0 MM = %i schools", nrow(good)))
good$mm <- 0

the_rest <- dat_t[-which(dat_t$institute.name %in% good$institute.name),]

MAX_MISMATCH <- 13
mm <- 0
while (nrow(the_rest)>1 & mm < MAX_MISMATCH) {
	mm <- mm + 1
	cur <- stringdist_join(x=the_rest,y=mb_t, 
		by=c(institute.name="School.Name"),max_dist=mm)
	message(sprintf("%i MM = %i schools", mm,
		length(unique(cur$institute.name))))
	cur$mm <- mm
	good <- rbind(good, cur)
	the_rest <- dat_t[-which(dat_t$institute.name %in% good$institute.name),]
}

good$institute.name <- stringi::stri_encode(good$institute.name,"iso-8859-1")

write.table(good,file=sprintf("mb_match_%s.csv",dt),sep=",",
	col=T,row=F,quote=T)

browser()
write.table(the_rest,file=sprintf("mb_unmatch_%s.csv",dt),
	sep=",",
	col=T,row=F,quote=T)
}

# read in current clean.csv
inFile <- "/Users/shraddhapai/Google_covidschools/daily_data/Canada_COVID_tracker/export-201224/final_data/CanadaMap_QuebecMerge-201224.clean.csv"
joinFile <- "/Users/shraddhapai/Google_covidschools/SchoolInfo/MB/MB_GoogleMap2Prov_FINAL.csv"

# do a join with the file that matches MB school codes+geocodes
# with the clean.csv
dat <- read.delim(inFile,sep=",",h=T,as.is=T)
dat <- subset(dat,Province=="MB")
matchoff <- read.delim(joinFile,sep=",",h=T,as.is=T,skip=1)

x <- merge(x=dat,y=matchoff,by="institute.name")
idx <- which(! dat$institute.name %in% matchoff$institute.name)
mism <- dat[idx,]
message("running fuzzy match on those still mismatching")
fuzzyMatch_Prov(mism)






# convert to google spreadsheet


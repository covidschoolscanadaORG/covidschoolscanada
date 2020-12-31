# fuzzy-match bc Alberta data to us
rm(list=ls())
require(tibble)
require(fuzzyjoin)
require(readxl)

dat <- read.delim("/Users/shraddhapai/Google_covidschools/daily_data/Canada_COVID_tracker/export-201224/final_data/CanadaMap_QuebecMerge-201224.clean.csv",
	sep=",",h=T,as.is=T)
dat <- subset(dat, Province=="SK")
dat$institute.name <- stringr::str_trim(dat$institute.name)

dat <- dat[,c("institute.name","School.board","City")]

sk <- read_excel("/Users/shraddhapai/Google_covidschools/SchoolInfo/SK/SK_schools_geocoded_201213.xlsx")
sk <- sk[,c(1:3,5)]
colnames(sk) <- c("School.District","School.Code","School.Name",
	"City")
sk$School.Name <- stringr::str_trim(sk$School.Name)
sk$City <- tools::toTitleCase(tolower(sk$City))

dat_t <- as_tibble(dat)
sk_t<- as_tibble(sk)

good<- stringdist_join(x=dat_t,y=sk_t, 
	by=c(institute.name="School.Name"),max_dist=0)
message(sprintf("0 MM = %i schools", nrow(good)))
good$mm <- 0

the_rest <- dat_t[-which(dat_t$institute.name %in% good$institute.name),]

MAX_MISMATCH <- 13
mm <- 0
while (nrow(the_rest)>1 & mm < MAX_MISMATCH) {
	mm <- mm + 1
	cur <- stringdist_join(x=the_rest,y=sk_t, 
		by=c(institute.name="School.Name"),max_dist=mm)
	message(sprintf("%i MM = %i schools", mm,
		length(unique(cur$institute.name))))
	cur$mm <- mm
	good <- rbind(good, cur)
	the_rest <- dat_t[-which(dat_t$institute.name %in% good$institute.name),]
	
}

good$City.x <- stringr::str_trim(good$City.x)
good$City.y <- stringr::str_trim(good$City.y)

good$city_match <- good$City.x == good$City.y

write.table(good,file="sk_match.csv",sep=",",col=T,row=F,quote=T)
write.table(the_rest,file="sk_unmatch.csv",sep=",",col=T,row=F,quote=T)

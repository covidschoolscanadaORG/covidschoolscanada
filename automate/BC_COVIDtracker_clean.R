# initially cleanup of BC covid school tracker
# match BC covid tracker school names to official list
rm(list=ls())
require(tibble)
require(fuzzyjoin)

inDir <- "/Users/shraddhapai/Google_covidschools/SchoolInfo/BC"
officialFile <- sprintf("%s/excelSchoolContact.csv",inDir)
inFile <- sprintf("%s/BC_Schools_CovidTracker_201202.csv",inDir)

# official link
bc <- read.delim(officialFile,sep=",",h=T,as.is=T)
bc <- bc[,1:7]
bc$School.Name <- stringr::str_trim(bc$School.Name)

dat <- read.delim(inFile,sep=",",h=T,as.is=T,encoding="UTF-8")
dat <- dat[,c("School","City",
	"School.District",
	"Health.Region")]
dat <- dat[-which(dat$School==""),]
for (k in colnames(dat)) {
	dat[,k] <- iconv(dat[,k],"UTF-8","UTF-8",sub="")
	dat[,k] <- stringr::str_trim(dat[,k])
}
dat <- dat[!duplicated(dat),]
dat_t <- as_tibble(dat)
bc_t<- as_tibble(bc)

good<- stringdist_join(x=dat_t,y=bc_t, 
	by=c(School="School.Name"),max_dist=0)
message(sprintf("0 MM = %i schools", nrow(good)))

the_rest <- dat_t[-which(dat_t$School %in% good$School),]

MAX_MISMATCH <- 13
mm <- 0
while (nrow(the_rest)>1 & mm < MAX_MISMATCH) {
	mm <- mm + 1
	cur <- stringdist_join(x=the_rest,y=bc_t, 
		by=c(School="School.Name"),max_dist=mm)
	message(sprintf("%i MM = %i schools", mm,
		length(unique(cur$School))))
	good <- rbind(good, cur)
	the_rest <- dat_t[-which(dat_t$School %in% good$School),]
}

write.table(good,
	file=sprintf("%s/BC_COVIDSchoolTracker_match.txt",inDir),
	sep="\t",col=T,row=F,quote=T)

write.table(the_rest,
	file=sprintf("%s/BC_COVIDSchoolTracker_unmatch.txt",inDir),
	sep="\t",col=T,row=F,quote=T)


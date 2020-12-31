# second round of cleanup to reconcile bc tarcker names to official
# after first round of automated matching, SP manually reconciles 
# differences. this step cleans up minor things like missing school district
rm(list=ls())

require(readxl)
dat <- read_excel("/Users/shraddhapai/Google_covidschools/SchoolInfo/BC/BC_COVIDSchoolTracker_2_official/Late_December_match/BC_COVIDSchoolTracker_match_201227.xls")
dat <- as.data.frame(dat)
#print(head(dat[sort(union(idx-1,idx)),]))

dat <- dat[,-which(colnames(dat)=="District.Number")]
dat$City.x <- stringr::str_trim(dat$City.x)

off <- read.delim("/Users/shraddhapai/Google_covidschools/SchoolInfo/BC/BC_Official/excelSchoolContact.csv",sep=",",h=T,as.is=T)
off <- off[,c("School.Code","School.Name",
	"District.Number","City")]

midx <- match(dat$School.Code, off$School.Code)
if(all.equal(off$School.Code[midx],dat$School.Code)!=TRUE){
	message("codes don't match")
	browser()
}

message("merging")
off <- off[midx,]
dat$School.District <- off$District.Number
dat$City.y <- off$City
dat$School.Name2 <- off$School.Name
dat$City.y <- stringr::str_trim(dat$City.y)

message("city mismatch check")
dat2 <- dat[which(!is.na(dat$City.x)),]
idx <- which(dat2$City.x!=dat2$City.y)
message(sprintf("# city mismatches = %i", length(idx)))
print(head(dat2[idx,]))
browser()

message("remove duplicate school mappings")
dat <- dat[!duplicated(dat$School),]

message("now see how many schools were mapped to, twice.")
message("these reflect variation in how bc school tracker names")
message("schools and we want to keep these")
dat <- dat[order(dat$School.Code),]
idx <- which(duplicated(dat$School.Code))
message(sprintf("Duplicated school code = %i", length(idx)))
tmp <- dat[sort(union(idx-1,idx)),]
#print(tmp[,c("School","City.x","School.Code","City.y")])

message(sprintf("writing final mapping = %i schools", nrow(dat)))
dt <- format(Sys.Date(),"%y%m%d")
write.table(dat,
	file=sprintf("BC_COVIDTracker_%s.csv",dt), sep=",",
	col=T,row=F,quote=T)
message("done")




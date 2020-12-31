# identify mismatched city names
rm(list=ls())
require(readxl)

dat <- read_excel("/Users/shraddhapai/Google_covidschools/SchoolInfo/BC/BC_COVIDSchoolTracker_2_official/Early_December_match/BC_COVIDSchoolTracker_match_201202_RA_SP.xlsx",skip=1)
dat$City.x <- stringr::str_trim(dat$City.x)
dat$City.y <- stringr::str_trim(dat$City.y)
dat$city.match <- dat$City.x == dat$City.y

write.table(dat,file="BC_COVIDTracker_2_official_earlyDec_cityamatch_201230.csv",sep=",",col=T,row=F,quote=T)


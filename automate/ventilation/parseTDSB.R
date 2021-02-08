# parse TDSB schools with air purifiers
rm(list=ls())
require(readxl)
require(gsheet)
require(fuzzyjoin)

tdsbF <- "/Users/shraddhapai/Google_covidschools/Ventilation/TDSB_22Nov_2020.xlsx"
ON_URL <- "https://docs.google.com/spreadsheets/d/1U-HFYvLch6PkOsITpI2wA7_MIsT1ocR6tv9XVr5jfPw/edit#gid=1526203723"

tdsb <- read_excel(tdsbF)
colnames(tdsb)[2] <- "School.Name"
tdsb <- tdsb[-which(is.na(tdsb$School.Name)),]

onloc <- gsheet2tbl(ON_URL)
colnames(onloc)[2] <- "Board.Name"
colnames(onloc)[1] <- "School.NameON"
onloc <- onloc[grep("Toronto DSB",onloc$Board.Name),]

good <- stringdist_join(x=tdsb,y=onloc,
	by=c(School.Name="School.NameON"),max_dist=4)
good$MM <- 2
message(sprintf("0 MM = %i schools", nrow(good)))

the_rest <- tdsb[-which(tdsb$School.Name %in% good$School.Name),]
message(sprintf("the rest = %i schools", nrow(the_rest)))

write.table(good,file="TDSB_match.txt",sep="\t",
	col=T,row=F,quote=F)
write.table(the_rest,file="TDSB_unmatch.txt",sep="\t",col=T,row=F,quote=F)

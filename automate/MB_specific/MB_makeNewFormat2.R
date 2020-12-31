rm(list=ls())

require(readxl)

message("* reconcile 2")
dat <- read_excel("/Users/shraddhapai/Google_covidschools/SchoolInfo/MB/mb_match_201227_SPreconciled_2.xlsx")
print(head(dat$institute.name))

message("* reconcile 1")
dat2 <- read.delim("/Users/shraddhapai/Google_covidschools/SchoolInfo/MB/MB_GoogleMap2Prov_FINAL.csv",
	sep=",",h=T,as.is=T,skip=1)

found <- c(dat$institute.name,dat2$institute.name)
message("* now looking at map")
inFile <- "/Users/shraddhapai/Google_covidschools/daily_data/Canada_COVID_tracker/export-201224/final_data/CanadaMap_QuebecMerge-201224.clean.csv"
map <- read.delim(inFile,sep=",",h=T,as.is=T)
map <- subset(map,Province=="MB")
message("getting setdiff")
not_found <- which(! map$institute.name %in% found)
dt <- format(Sys.Date(),"%y%m%d")
browser()
message("writing")
write.table(map[not_found,],
	file=sprintf("MB_finalleftover_%s.csv",dt),sep=",",
	col=T,row=F,quote=T)


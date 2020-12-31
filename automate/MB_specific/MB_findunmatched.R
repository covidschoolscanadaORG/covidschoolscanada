# make list of all google map names not matched to prov after fuzzy match +
# reconciliation
rm(list=ls())

inDir <- "/Users/shraddhapai/Google_covidschools/SchoolInfo/MB/"
dat <- read.delim("/Users/shraddhapai/Google_covidschools/daily_data/Canada_COVID_tracker/export-201207AMfreeze/final_data/CanadaMap_QuebecMerge-201207.clean.csv",
	sep=",",h=T,as.is=T)
dat <- subset(dat, Province=="MB")
dat$institute.name <- stringr::str_trim(dat$institute.name)

recon <- read.delim("/Users/shraddhapai/Google_covidschools/SchoolInfo/MB/MB_GoogleMap2Prov_SPreconciled.csv",sep=",",h=T,as.is=T,skip=1)

idx <- which(!dat$institute.name %in% recon$institute.name)
write.table(dat[idx,],file=sprintf("%s/MB_to_be_reconciled.csv",inDir),
	sep=",",col=T,row=F,quote=T)


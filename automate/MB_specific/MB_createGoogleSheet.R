# create Google sheet for MB
rm(list=ls())
require(readxl)

curMap <- read_excel("/Users/shraddhapai/Google_covidschools/SchoolInfo/MB/clean.csv_MB_201224_FREEZE_CLEAN.xlsx")
map2prov <- read_excel("/Users/shraddhapai/Google_covidschools/SchoolInfo/MB/MB_GoogleMap2Prov_FinalMerge_pre.xlsx")

if (sum(duplicated(map2prov$institute.name))>0) {
	message("found duplicates")
	browser()
}

map2prov <- map2prov[,c("institute.name","School.Code")]

message("merging")
fin <- merge(x=curMap,y=map2prov,by="institute.name",all.y=TRUE)

message("checking for gaps")
not_inc <- setdiff(curMap$institute.name, fin$institute.name)
if (length(not_inc)>0){
	message("merge excluded rows - why?")
	browser()
}

message("writing out")
outFile <- paste("/Users/shraddhapai/Google_covidschools/SchoolInfo/MB",
		"MB_NewDataTable.csv",sep="/")
write.table(fin,file=outFile,sep=",",col=T,row=F,quote=T)

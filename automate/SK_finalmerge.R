# after fuzzymatch + sp reconcile final merge
rm(list=ls())
require(readxl)

map <- read.delim("/Users/shraddhapai/Google_covidschools/daily_data/Canada_COVID_tracker/export-201224/final_data/CanadaMap_QuebecMerge-201224.clean.csv",sep=",",h=T,as.is=T)
off <- read_excel("/Users/shraddhapai/Google_covidschools/SchoolInfo/SK/sk_match_SPreconciled_201230.xlsx")

map <- subset(map,Province=="SK")
message(sprintf("SK: %i rows",nrow(map)))

off <- off[,c("institute.name","City.x","School.Code")]
off <- as.data.frame(off)
colnames(off)[2] <- "City"

x <- merge(x=map,y=off,by=c("institute.name","City"),
	all.x=TRUE)
message(sprintf("after merge: %i rows", nrow(x)))

x <- x[!duplicated(x),]
x <- x[order(x$institute.name),]

write.table(x,file="SK_finalmerge_201231.csv",sep=",",col=T,row=F,quote=T)



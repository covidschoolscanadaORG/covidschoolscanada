# add city/country, school board info
rm(list=ls())

inFile <- "/home/shraddhapai/Canada_COVID_tracker/export-200920/CanadaMap_QuebecMerge-200920.csv"
###onFile <- "/home/shraddhapai/Canada_COVID_tracker/official_school_info/ON_publicly_funded_schools_xlsx_september_2020_en.xlsx"
onFile <- "/home/shraddhapai/Canada_COVID_tracker/school_annotations/CanadaMap_QuebecMerge-200920_schoolboards.tsv"

dat <- read.delim(inFile,sep=",",h=T,as.is=T)
dat$both <- paste(dat$institute.name,dat$Province,sep="_")

#tsc <- read.delim(onFile,sep="\t",h=T,as.is=T)
sc <- read.delim(onFile,sep="\t",h=T,as.is=T)
sc <- sc[,c("institute.name","Province","School.board")]
sc$both <- paste(sc$institute.name,sc$Province,sep="_")

midx <- match(dat$both, sc$both)
if (all.equal(sc$both[midx],dat$both)!=TRUE) {
	cat("merge didn't work")
	browser()
}
dat$School.board <- sc$School.board
dat  <- dat[,-which(colnames(dat)=="both")]
dat$Lat2 <- dat$Latitude
dat$Long2 <- dat$Longitude
dat <- dat[,-which(colnames(dat)=="Latitude")]
dat <- dat[,-which(colnames(dat)=="Longitude")]
colnames(dat)[which(colnames(dat)=="Lat2")] <- "Latitude"
colnames(dat)[which(colnames(dat)=="Long2")] <- "Longitude"


write.table(dat,file="/home/shraddhapai/Canada_COVID_tracker/export-200920/CanadaMap_QuebecMerge-200920_schoolboardadded.csv",sep=",",col=TRUE,row=FALSE,quote=TRUE)



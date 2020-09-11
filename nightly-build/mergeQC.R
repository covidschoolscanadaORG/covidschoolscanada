# merge canada-wide and quebec

dt <- "200911"
dt_folder <- "20200911"
rootDir <- "/home/shraddhapai/Canada_COVID_tracker/export"

inDir <- sprintf("%s-%s",rootDir,dt_folder)

can <- read.delim(sprintf("%s/CanadaMap_clean3.kml-%s.txt",
	inDir,dt),fileEncoding="UTF-8",sep="\t",h=T,as.is=T,
	stringsAsFactors=FALSE)
colnames(can) <- sub("^X.","",colnames(can))
colnames(can) <- sub("\\.$","",colnames(can))
qc <- read.delim(sprintf("%s/COVIDEcolesQuebec_clean.kml-%s.txt",
	inDir,dt),sep="\t",h=T,as.is=T)

can <- subset(can, Province=="Québec")



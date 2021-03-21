# compile VOC from Province-specific sources, convert into
# geojson
rm(list=ls())
require(gsheet)

dt <- format(Sys.Date(),"%y%m%d")
localDir <- sprintf("/Users/shraddhapai/Google_covidschools/daily_data/Canada_COVID_tracker/export-%s",dt)
mapDir <- "/Users/shraddhapai/software/covidschoolscanada.github.io/maps/"

qcFile <- "https://docs.google.com/spreadsheets/d/1cU9_JiWJ4NdvCWAleivb3pNweYgaqt4GBRIWPl3MyxM/edit?ts=60553d34#gid=0"
bcFile <- sprintf("%s/BC_VOC_%s.csv",localDir,dt)

message("Fetch QC")
qc <- gsheet2tbl(qcFile)
colnames(qc) <- qc[3,]
qc <- as.data.frame(qc[-(1:3),])
qc <- qc[,-1]
# school, city, date, latitude, longitude
colnames(qc)[c(1,3,6)] <- c("School","City","Date")
qc <- qc[,c(1,3,6:8)]
qc$Article <- qcFile
qc$Province <- "QC"
qc$Latitude <- sub(",",".",as.character(qc$Latitude))
qc$Longitude <- sub(",",".",as.character(qc$Longitude))
qc$Latitude <- as.numeric(qc$Latitude)+0.0003
qc$Longitude <- as.numeric(qc$Longitude)+0.0030

message("Fetch BC")
source("VOC_BC.R")
bc  <- read.delim(bcFile,sep=",",h=T,as.is=T)
bc <- bc[,c("School","City","Date3","Latitude","Longitude","Article")]
colnames(bc)[3] <- "Date"
bc$Province <- "BC"

message("Merging all Prov")
voc <- rbind(qc,bc)

# have "last reported date" column
message("* Adding last reported date column")
dt2 <- strsplit(voc$Date,";")
for (k in 1:length(dt2)){
    dt3 <- as.Date(dt2[[k]])
    dt2[[k]] <- as.character(dt3[which.max(dt3)])
}
dt2 <- unlist(dt2)
voc$Last_Reported_Date <- dt2

outCSV <- sprintf("%s/VOC.csv",localDir)
outJSON <- basename(sub(".csv",".geojson",outCSV))
write.table(voc,file=outCSV,sep=",",col=T,row=F,quote=T)

message("* Converting to geojson")
system2("csv2geojson", args=c(outCSV,">",outJSON))

message("* Move to live web folder")
tgtFile <- sprintf("%s/%s",mapDir,outJSON)
file.rename(outJSON, sprintf("%s/%s",mapDir,outJSON))

cwd <- getwd()
setwd(mapDir)
system2("git","pull")

message("CHECK")

message("* git commit")
system2("git",args=c("commit","-m","\"map update\"", tgtFile))
#system2("git","push")
setwd(cwd)


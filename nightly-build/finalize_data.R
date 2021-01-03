# Run this script when you're happy results look good and 
# all data cleaning is done.

require(rdrop2)
require(dplyr)
#dbox <- "/home/shraddhapai/Canada_COVID_tracker/misc/dbox.rds"
dbox <- "dbox.rds"

autoGen_boards <- c("Peel DSB", "Toronto DSB",
	"York Region DSB","York CDSB",
	"Ottawa-Carleton DSB","Ottawa CDSB")

dt <- format(Sys.Date(),"%y%m%d")
inDir <- sprintf("/Users/shraddhapai/Google_covidschools/daily_data/Canada_COVID_tracker/export-%s",dt)
logFile <- sprintf("%s/finalize_data_%s.log",inDir,dt)
baseF<- sprintf("CanadaMap_QuebecMerge-%s.clean.csv",dt)
inFile <- sprintf("%s/%s",inDir,baseF)
if (file.exists(logFile)) unlink(logFile)

counts <- list()
# Google My Maps has a max capacity of 2000 schools.
dat <- read.delim(inFile,sep=",",h=T,as.is=T)
counts$total <- nrow(dat)
cat(sprintf("Full = %i records\n",nrow(dat)),file=logFile,
	append=TRUE)

# upload final data to dropbox
token <- readRDS(dbox)
message("Authorizing dropbox")
drop_acc(dtoken=token)
odir <- sprintf("daily_data/export-%s/final_data",dt)
if (!drop_exists(path=odir,dtoken=token)) {
	message("Making Dropbox folder")
	drop_create(path=odir,dtoken=token)
} 
	message("\tMoving final file to Dropbox")
	drop_upload(file=inFile,path=odir,dtoken=token)
	message("Upload successful!\n")

# ---------------------------------------------------------
# Quebec layer
# ---------------------------------------------------------
qc <- subset(dat,Province=="QC")
counts$qc <- nrow(qc)
message("* Writing QC")
cat(sprintf("QC = %i records\n",nrow(qc)),file=logFile,
	append=TRUE)
write.table(qc[1:2000,],
	file=sprintf("%s/CanadaMap_QuebecMerge-%s.clean.QC_1.csv",
		inDir,dt),
	sep=",",col=T,row=F,quote=T)
write.table(qc[2001:nrow(qc),],
	file=sprintf("%s/CanadaMap_QuebecMerge-%s.clean.QC_2.csv",
		inDir,dt),
	sep=",",col=T,row=F,quote=T)
###write.table(qc[2001:nrow(qc),],
###	file=sprintf("%s/CanadaMap_QuebecMerge-%s.clean.QC2.csv",
###		inDir,dt),
###	sep=",",col=T,row=F,quote=T)
dat <- dat[-which(dat$Province=="QC"),]

# ---------------------------------------------------------
# Autogen
# ---------------------------------------------------------
message("* Writing autogen file")
autogen_idx <-which(dat$School.board %in% autoGen_boards)
counts$autogen <- length(autogen_idx)
autogen <- dat[autogen_idx,]
if (nrow(autogen)>2000) {
message("autogen over 2000")
browser()
}
print(table(autogen$School.board))
cat(sprintf("Autogen = %i records\n",nrow(autogen)),
	file=logFile,append=TRUE)
write.table(autogen,
	file=sprintf("%s/CanadaMap_QuebecMerge-%s.clean.Autogen.csv",
		inDir,dt),
	sep=",",col=T,row=F,quote=T)

dat <- dat[-autogen_idx,]
# ---------------------------------------------------------
# Ontario
# ---------------------------------------------------------
ont_idx <- which(dat$Province == "ON")
ont <- dat[ont_idx,]
counts$ont <- nrow(ont)
if (nrow(ont)>2000) {
message("ont over 2000")
browser()
}
message("* Writing ON")
cat(sprintf("ON = %i records\n",nrow(ont)),file=logFile,
	append=TRUE)
write.table(ont,
	file=sprintf("%s/CanadaMap_QuebecMerge-%s.clean.ON.csv",
		inDir,dt),
	sep=",",col=T,row=F,quote=T)

dat <- dat[-ont_idx,]
# ---------------------------------------------------------
# Alberta
# ---------------------------------------------------------
ab_idx <- which(dat$Province == "AB")
ab <- dat[ab_idx,]
counts$ab <- nrow(ab)
if (nrow(ab)>2000) {
message("AB over 2000")
browser()
}
message("* Writing AB")
cat(sprintf("AB = %i records\n",nrow(ab)),file=logFile,
	append=TRUE)
write.table(ab,
	file=sprintf("%s/CanadaMap_QuebecMerge-%s.clean.AB.csv",
		inDir,dt),
	sep=",",col=T,row=F,quote=T)

dat <- dat[-ab_idx,]
# ---------------------------------------------------------
# Other Provinces
# ---------------------------------------------------------
message("* Writing Other Provinces")
counts$other <- nrow(dat)
if (nrow(dat)>2000) {
	message("other over 2000")
browser()
}
cat(sprintf("Other = %i records\n",nrow(dat)),file=logFile,
	append=TRUE)
write.table(dat,
	file=sprintf("%s/CanadaMap_QuebecMerge-%s.clean.nonQC.csv",inDir,dt),
	sep=",",col=T,row=F,quote=T)
message("* Layer write done")

message("Totalling...")
tot2 <- counts$qc + counts$ont + counts$ab + counts$autogen + counts$other
cat(sprintf("Tally QC + ON + autogen + other = %i\n", tot2),
	file=logFile,append=TRUE)

system2("cat",logFile)


dir.create(sprintf("%s/final_data",inDir))
file.rename(sprintf("%s/%s",inDir,baseF),
		sprintf("%s/final_data/%s",inDir,baseF))
# upload clean.csv to covidschoolscanada dropbox account


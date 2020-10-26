# Run this script when you're happy results look good and 
# all data cleaning is done.

require(rdrop2)
require(dplyr)
dbox <- "/home/shraddhapai/Canada_COVID_tracker/misc/dbox.rds"

autoGen_boards <- c("Peel DSB", "Toronto DSB")

dt <- format(Sys.Date(),"%y%m%d")
inDir <- sprintf("/home/shraddhapai/Canada_COVID_tracker/export-%s",dt)
logFile <- sprintf("%s/finalize_data_%s.log",inDir,dt)
inFile <- sprintf("%s/CanadaMap_QuebecMerge-%s.clean.csv",
	inDir,dt)
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

qc <- subset(dat,Province=="QC")
counts$qc <- nrow(qc)
message("* Writing QC")
cat(sprintf("QC = %i records\n",nrow(qc)),file=logFile,
	append=TRUE)
write.table(qc,
	file=sprintf("%s/CanadaMap_QuebecMerge-%s.clean.QC.csv",
		inDir,dt),
	sep=",",col=T,row=F,quote=T)

message("* Writing autogen file")
autogen_idx <-which(dat$School.board %in% autoGen_boards)
counts$autogen <- length(autogen_idx)
autogen <- dat[autogen_idx,]
cat(sprintf("Autogen = %i records\n",nrow(autogen)),
	file=logFile,append=TRUE)
write.table(autogen,
	file=sprintf("%s/CanadaMap_QuebecMerge-%s.clean.Autogen.csv",
		inDir,dt),
	sep=",",col=T,row=F,quote=T)

message("* Writing Other Provinces")
dat <- dat[-autogen_idx,]
dat <- subset(dat,Province!="QC")
counts$other <- nrow(dat)
cat(sprintf("Other = %i records\n",nrow(dat)),file=logFile,
	append=TRUE)
write.table(dat,file=sprintf("%s/CanadaMap_QuebecMerge-%s.clean.nonQC.csv",inDir,dt),sep=",",col=T,row=F,quote=T)
message("* Layer write done")

message("Totalling...")
tot2 <- counts$qc + counts$autogen + counts$other
cat(sprintf("Tally QC + autogen + other = %i\n", tot2),
	file=logFile,append=TRUE)

# upload clean.csv to covidschoolscanada dropbox account


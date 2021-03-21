# compile data from all data entry google sheets
rm(list=ls())
require(gsheet)
require(rdrop2)
dbox <- "dbox.rds"

args <- commandArgs(TRUE)
outDir <- args[1] #"/Users/shraddhapai/Google_covidschools/daily_data/Canada_COVID_tracker/export-210118" #args[1]
dtRun <- args[2] #"210118" #args[2]

abDate <- format(Sys.Date()-2,"%Y-%m-%d")
ABfile <- sprintf("/Users/shraddhapai/Google_covidschools/daily_data/AB/AB_Automated_boards_%s.csv",abDate)
#MB="https://docs.google.com/spreadsheets/d/1a1Rzn7tDVrTc976UAyHFk-WcSz9RPumRQELVd6lnac8/edit#gid=20331003",
#SK="https://docs.google.com/spreadsheets/d/10Y2N2wq0vzW6BAB3d0BQgAdpZZrpZlU7xKQhKfNeuBE/edit#gid=53224925",
#ON="https://docs.google.com/spreadsheets/d/1U-HFYvLch6PkOsITpI2wA7_MIsT1ocR6tv9XVr5jfPw/edit#gid=0",
sheetURLs <- list(
	MB="https://docs.google.com/spreadsheets/d/1a1Rzn7tDVrTc976UAyHFk-WcSz9RPumRQELVd6lnac8/edit#gid=1931233338",
	SK="https://docs.google.com/spreadsheets/d/10Y2N2wq0vzW6BAB3d0BQgAdpZZrpZlU7xKQhKfNeuBE/edit#gid=1394162486",
	NS_PEI="https://docs.google.com/spreadsheets/d/1f7EJGFzr8OiRB_GtlbhDs0ZWbA8BDbEM_bSps6StONA/edit#gid=0",
	NB="https://docs.google.com/spreadsheets/d/1kKEQu7O-lDYq9D9o1P19yEPttBcj8_xxGS1Y2IAILj4/edit#gid=0",
	ON="https://docs.google.com/spreadsheets/d/1U-HFYvLch6PkOsITpI2wA7_MIsT1ocR6tv9XVr5jfPw/edit#gid=551585214",
	ON_AUTO="https://docs.google.com/spreadsheets/d/14-CM4FX666eRcMCTpKJoNViUpW50P4OAH433dQxn9Yg/edit#gid=0"
)

out <- list()
for (k in 1:length(sheetURLs)){
	nm <- names(sheetURLs)[k]
	message(nm)
	x <- gsheet2tbl(sheetURLs[[k]])
	x$Date <- as.character(x$Date)
	x$Outbreak.dates <- as.character(x$Outbreak.dates)
	message(sprintf("\t%i rows", nrow(x)))
	out[[nm]] <- as.data.frame(x)
}

message("merging")
dat<- do.call("rbind",out)

message("fetching AB")
# -----------------------------------------
# ADD ALBERTA AUTOGEN
AB <- read.delim(ABfile,sep=",",h=T,as.is=T)
AB$School.Code <- NA
AB <- AB[,colnames(dat)]
AB$Article <- "https://www.supportourstudents.ca/alphabeticaltrackerlist.html"
message("* Adding AB autogen")
#dat <- dat[-which(dat$Province=="AB"),]
old <- nrow(dat)
dat <- rbind(dat,AB)
message(sprintf("Added AB autogen: %i to %i rows", old, nrow(dat)))

# -----------------------------------------
# ADD BC AUTOGEN
token <- readRDS(dbox)
message("Authorizing dropbox")
suppressMessages(drop_acc(dtoken=token))

dt <- format(Sys.Date()-1,"%Y%m%d")
odir <- sprintf("BC_automation/daily_update/export-%s",dt)
ofile <- sprintf("%s/CanadaMap_BC-%s.clean.csv",odir,dt)
message("Downloading BC")
lpath <- "."
if (file.exists(basename(ofile))) unlink(basename(ofile))
drop_download(path=ofile,dtoken=token,overwrite=TRUE)
bc <- read.delim(basename(ofile),sep=",",h=T,as.is=T)
colnames(bc)[which(colnames(bc)=="School.Name")] <- "institute.name"
bc <- bc[,colnames(dat)]
message(sprintf("BC update: Added %i rows",nrow(bc)))
dat <- rbind(dat,bc)

message("Writing to file")
oFile <- sprintf("%s/CanadaMap-%s.csv",outDir,dtRun)
write.table(dat,file=oFile,sep=",",col=T,row=F,quote=T)

message("done collecting")

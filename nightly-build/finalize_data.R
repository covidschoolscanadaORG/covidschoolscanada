# Run this script when you're happy results look good and 
# all data cleaning is done.


dt <- format(Sys.Date(),"%y%m%d")
inDir <- sprintf("/home/shraddhapai/Canada_COVID_tracker/export-%s",dt)
inFile <- sprintf("%s/CanadaMap_QuebecMerge-%s.clean.csv",
	inDir,dt)
dat <- read.delim(inFile,sep=",",h=T,as.is=T)

# Google My Maps has a max capacity of 2000 schools.
qc <- subset(dat,Province=="QC")
nonqc <- subset(dat,Province!="QC")
message("* Writing QC")
write.table(qc,file=sprintf("%s/CanadaMap_QuebecMerge-%s.clean.QC.csv",inDir,dt),sep=",",col=T,row=F,quote=T)
message("* Writing Other Provinces")
write.table(nonqc,file=sprintf("%s/CanadaMap_QuebecMerge-%s.clean.nonQC.csv",inDir,dt),sep=",",col=T,row=F,quote=T)
message("* Layer write done")

# upload clean.csv to covidschoolscanada dropbox account


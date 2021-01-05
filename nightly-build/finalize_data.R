# Run this script when you're happy results look good and 
# all data cleaning is done.

require(rdrop2)
require(dplyr)
#dbox <- "/home/shraddhapai/Canada_COVID_tracker/misc/dbox.rds"
dbox <- "dbox.rds"

#autoGen_boards <- c("Peel DSB", "Toronto DSB",
#	"York Region DSB","York CDSB",
#	"Ottawa-Carleton DSB","Ottawa CDSB")

dt <- format(Sys.Date(),"%y%m%d")
inDir <- sprintf("/Users/shraddhapai/Google_covidschools/daily_data/Canada_COVID_tracker/export-%s",dt)
logFile <- sprintf("%s/finalize_data_%s.log",inDir,dt)
baseF<- sprintf("CanadaMap_QuebecMerge-%s.clean.csv",dt)
inFile <- sprintf("%s/%s",inDir,baseF)
if (file.exists(logFile)) unlink(logFile)

###counts <- list()
#### Google My Maps has a max capacity of 2000 schools.
###dat <- read.delim(inFile,sep=",",h=T,as.is=T)
###counts$total <- nrow(dat)
###cat(sprintf("Full = %i records\n",nrow(dat)),file=logFile,
###	append=TRUE)
###
#### upload final data to dropbox
###token <- readRDS(dbox)
###message("Authorizing dropbox")
###drop_acc(dtoken=token)
###odir <- sprintf("daily_data/export-%s/final_data",dt)
###if (!drop_exists(path=odir,dtoken=token)) {
###	message("Making Dropbox folder")
###	drop_create(path=odir,dtoken=token)
###} 
###	message("\tMoving final file to Dropbox")
###	drop_upload(file=inFile,path=odir,dtoken=token)
###	message("Upload successful!\n")
###
# move clean.csv to final_data
dir.create(sprintf("%s/final_data",inDir))
file.rename(sprintf("%s/%s",inDir,baseF),
		sprintf("%s/final_data/%s",inDir,baseF))

# create geojson for mapbox
source("makeInteractiveMap.R")
message("making geojson")
dt <- format(Sys.Date(),"%y%m%d")
makeGeoJSON(inDir,dt)





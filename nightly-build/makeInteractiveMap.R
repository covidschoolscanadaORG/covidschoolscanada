# set up map

source("flattenCaseCol.R")

makeGeoJSON <- function(inDir,dt) {
mapDir <- "/Users/shraddhapai/software/covidschoolscanada.github.io/maps/"

inFile <- sprintf("%s/final_data/CanadaMap_QuebecMerge-%s.clean.csv",
	inDir,dt)
qcFile <- sprintf("%s/CEQ_annotated_clean_%s.csv",inDir,dt)

dat <- flattenCaseCol(inFile,qcFile)
outCSV <- "CanadaMap_QuebecMerge-current.clean.csv"
outJSON <- sub(".csv",".geojson",outCSV)
write.table(dat,file=outCSV,sep=",",col=T,row=F,quote=T)

message("* Converting to geojson")
system2("csv2geojson", args=c(outCSV,">",outJSON))

message("* Move to live web folder")
tgtFile <- sprintf("%s/%s",mapDir,outJSON)
file.rename(outJSON, sprintf("%s/%s",mapDir,outJSON))

cwd <- getwd()
setwd(mapDir)
system2("git","pull")

message("* git commit")
system2("git",args=c("commit","-m","\"map update\"", tgtFile))
system2("git","push")
setwd(cwd)
}


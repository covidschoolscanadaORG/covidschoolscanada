require(XML)

print ("Am I in here?")

args <- commandArgs(TRUE)
Sys.setenv(TZ="America/Toronto")
dt <- format(Sys.Date(),"%y%m%d")

inFile <- args[1] #"/Users/shraddhapai/Google_covidschools/daily_data/Canada_COVID_tracker/export-201215/COVIDEcolesQuebec_layer2_clean3.kml" #args[1]#sprintf("%s-%s/COVIDEcolesQuebec_clean3.kml",
	#args[1],dt)

#args[1]#"/home/shraddhapai/Canada_COVID_tracker/export-200923/CanadaMap_clean3.kml"#args[1]
print(inFile)

dt <- format(Sys.Date(),"%y%m%d")
tagsXML <- xmlParse(inFile)
outFile <-  sprintf("%s-%s.txt",inFile,dt)

# Convert to List
tagsList <- xmlToList(tagsXML)
# get into the level where the school data is stored

if (any(grep("layer2",inFile))) {
	schools <- tagsList$Document
} else {
	schools <- tagsList$Document$Folder
}
schools[[1]] <- NULL

# parse each placeholder
parseRec <- function(rec) {
	cur <- rec$ExtendedData
	for (k in 1:length(cur)) {
		if (is.null(cur[[k]]$value)) cur[[k]]$value <- ""
	}
	attrs <- unlist(lapply(cur,function(x) x[[2]]))
	vals <- unlist(lapply(cur,function(x) x[[1]]))
	names(vals) <- attrs
	return(vals)
}

sname <- unlist(lapply(schools,function(x) x$name))
coords <- unlist(lapply(schools, function(x) x$Point$coordinates))

latitude <- c()
longitude <- c()
third <- c()

message("* Adding coordinates")
for (k in coords){
	cur <- gsub("\n","",k)
	cur <- trimws(cur)
	cur <- unlist(strsplit(cur,","))
	latitude <- c(latitude,as.numeric(cur[1]))
	longitude <- c(longitude,as.numeric(cur[2]))
	third <- c(third,as.numeric(cur[3]))
}
tmp <- latitude
latitude <- longitude
longitude <- tmp

style <- unlist(lapply(schools,function(x) x$styleUrl))
status <- rep("Single/unlinked cases",length(style))
status[which(style=="#icon-1899-E65100-labelson-nodesc")] <- "Declared outbreak"

message("* Compiling table")
df <- data.frame(institute.name=sname,Latitude=latitude,
	Longitude=longitude,Outbreak.status=status)

message("* Annotate")


write.table(df,file=outFile,sep="\t",col=T,row=F,quote=T)


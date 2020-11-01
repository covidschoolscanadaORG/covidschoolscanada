require(XML)

args <- commandArgs(TRUE)
#inFile <- args[1] #"/home/shraddhapai/Canada_COVID_tracker/export-201004/CanadaMap_clean3.kml"# args[1]#"/home/shraddhapai/Canada_COVID_tracker/export-200923/CanadaMap_clean3.kml"#args[1]
###inFile <- "/home/shraddhapai/Canada_COVID_tracker/export-201029/CanadaMap_clean3.kml"
inFile <- args[1]
print(inFile)

dt <- format(Sys.Date(),"%y%m%d")
tagsXML <- xmlParse(inFile)
outFile <-  sprintf("%s-%s.txt",inFile,dt)

# Convert to List
tagsList <- xmlToList(tagsXML)
# get into the level where the school data is stored
mega <- tagsList$Document
idx <- which(names(mega)=="Folder")
message(sprintf("* layers found", length(idx)))

schools <- list()
for (k in 1:length(idx)) {
	cur <- mega[[idx[k]]]
	nm <- cur$name
	pl <- cur[which(names(cur)=="Placemark")] # get points
	message(sprintf("Layer: %s: %i schools",nm, length(pl)))
	schools <- c(schools,pl)
}

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

other <- list()
message("* Adding other fields")
field_names <- c()
for (k in 1:length(schools)) {
	other[[k]] <- t(as.matrix(parseRec(schools[[k]])))
}
field_names <- trimws(colnames(other[[1]]))
#ln <- unlist(lapply(other,length))
#other_long <- do.call("rbind",other[which(ln > 20)])
#other_long <- other_long[,1:15]

#other2 <- do.call("rbind",other[which(ln==15)])
#other2 <- rbind(other2,other_long)
other <- do.call("rbind",other)
colnames(other) <- field_names

message("* Compiling table")
df <- data.frame(institute.name=sname)
df <- cbind(df,other)
#df$latitude <- latitude
#df$longitude <- longitude

df$Article <- gsub("\n","; ",df$Article)
table(df$Province)


write.table(df,file=outFile,sep="\t",col=T,row=F,quote=T)
message("* Table compiled")


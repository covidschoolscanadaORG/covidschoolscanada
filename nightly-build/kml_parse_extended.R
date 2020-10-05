require(XML)

args <- commandArgs(TRUE)
inFile <- args[1] #"/home/shraddhapai/Canada_COVID_tracker/export-201004/CanadaMap_clean3.kml"# args[1]#"/home/shraddhapai/Canada_COVID_tracker/export-200923/CanadaMap_clean3.kml"#args[1]
print(inFile)

dt <- format(Sys.Date(),"%y%m%d")
tagsXML <- xmlParse(inFile)
outFile <-  sprintf("%s-%s.txt",inFile,dt)

# Convert to List
tagsList <- xmlToList(tagsXML)
# get into the level where the school data is stored
schools <- tagsList$Document$Folder
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

other <- list()
message("* Adding other fields")
field_names <- c()
for (k in 1:length(schools)) {
#print(k)
#if (k==617) browser()
	other[[k]] <- t(as.matrix(parseRec(schools[[k]])))
	
}
field_names <- trimws(colnames(other[[1]]))
other2 <- do.call("rbind",other)
colnames(other2) <- field_names

message("* Compiling table")
df <- data.frame(institute.name=sname)
df <- cbind(df,other2)
#df$latitude <- latitude
#df$longitude <- longitude

df$Article <- gsub("\n","; ",df$Article)
table(df$Province)


write.table(df,file=outFile,sep="\t",col=T,row=F,quote=T)


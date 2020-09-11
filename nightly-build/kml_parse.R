require(XML)

args <- commandArgs(TRUE)
inFile <- args[1]
print(inFile)

dt <- format(Sys.Date(),"%y%m%d")

tagsXML <- xmlParse(inFile)
outFile <-  sprintf("%s-%s.txt",inFile,dt)

# Convert to List
tagsList <- xmlToList(tagsXML)
# get into the level where the school data is stored
schools <- tagsList$Document$Folder
schools[[1]] <- NULL

sname <- unlist(lapply(schools,function(x) x$name))
coords <- unlist(lapply(schools, function(x) x$Point$coordinates))

latitude <- c()
longitude <- c()
third <- c()

for (k in coords){
	cur <- gsub("\n","",k)
	cur <- trimws(cur)
	cur <- unlist(strsplit(cur,","))
	latitude <- c(latitude,as.numeric(cur[1]))
	longitude <- c(longitude,as.numeric(cur[2]))
	third <- c(third,as.numeric(cur[3]))
}

df <- data.frame(institute.name=sname,latitude=latitude,longitude=longitude)
write.table(df,file=outFile,sep="\t",col=T,row=F,quote=F)


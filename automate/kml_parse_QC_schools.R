require(XML)

print ("Am I in here?")

args <- commandArgs(TRUE)
Sys.setenv(TZ="America/Toronto")
dt <- format(Sys.Date(),"%y%m%d")

inFile <- "/Users/shraddhapai/Google_covidschools/SchoolInfo/QC/etablissements-meq-mes.kml"
print(inFile)

tagsXML <- xmlParse(inFile)
outFile <-  "qc_schools.txt"

# Convert to List
tagsList <- xmlToList(tagsXML)
# get into the level where the school data is stored

#blah1 <- tagsList$Document[[16]][[2]][[2]][-1]
#blah2 <- tagsList$Document[[16]][[3]]

collapsed <- list()
tagsList <- tagsList$Document
message("about to go in")
for (k in which(names(tagsList)=="Folder")) {
	cur <- tagsList[[k]]
	message(sprintf("level 1: %i",k))
	for (m in which(names(cur) == "Folder")){
		cur2 <- cur[[m]]
		message(sprintf("\tlevel 2: %i",m))
		for (n in which(names(cur2) == "Folder")) {
			message(sprintf("\t\tlevel 3: %i",n))
			cur3 <- cur2[[n]]
			 idx <- which(names(cur3)=="Placemark")
			collapsed <- c(collapsed,cur3[idx])
		}
			 idx <- which(names(cur2)=="Placemark")
			collapsed <- c(collapsed,cur2[idx])
	}
			 idx <- which(names(cur)=="Placemark")
			collapsed <- c(collapsed,cur[idx])
}

parseRec <- function(rec) {
	rec <- rec$ExtendedData$SchemaData
	idx <- which(names(rec)=="SimpleData")
	vec <- c()
	nm <- c()
	for (k in idx) {
		vec <- c(vec, rec[[k]]$text)
		nm <- c(nm, rec[[k]]$.attrs)
	}
	names(vec) <- nm
	return(vec)
}

blah <- lapply(collapsed, parseRec)
ln <- unlist(lapply(blah,length))
browser()

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


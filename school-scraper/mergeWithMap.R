
dt <- format(Sys.Date(),"%y%m%d")
mapDir <- sprintf("/Users/shraddhapai/Google/Advocacy/COVID-19/daily_data/Canada_COVID_tracker/export-%s",dt)

dsbDir <- "/Users/shraddhapai/Google_covidschools/SchoolBoard_daily_snapshot"

getFiles <- function() {
	message("* Fetching new records for today")
	boards <- c("Peel_DSB","HDSB","HCDSB","DDSB","DPCDSB","YRDSB")
	fileSet <- c()
	fileList <- list()
	for (sb in boards) {
		x <- file.info(dir(sprintf("%s/%s",dsbDir,sb),
			pattern="annot.csv$",recursive=TRUE))
		if (nrow(x) > 0){
			if (any(grep("outdated",rownames(x)))) x <- x[-grep("outdated",rownames(x)),]
			 x <- x[order(x$ctime,decreasing=TRUE),]
			 fileSet <- c(fileSet, rownames(x)[1])
				fileList[[sb]] <- read.delim(sprintf("%s/%s/%s",dsbDir,sb,rownames(x)[1]),
						sep=",",h=T,as.is=T)
				message(sprintf("\t%s: %i records", sb, nrow(fileList[[sb]])))
		}
	return(fileList)
}
}

y <- getFiles()
today <- do.call("rbind",y)

message("* Reading current")
cleanFile <- sprintf("%s/CanadaMap_QuebecMerge-%s.clean.csv",mapDir,dt)
orig <- read.delim(cleanFile,sep=",",h=T,as.is=T)
orig$isNew <- FALSE
if (!("Entry.Date" %in% colnames(orig))){
	orig$Entry.Date <- "2020-09-22" # initialize it
}

today <- today[,colnames(orig)]

message("* Merging")
merged <- rbind(orig,today)

message(sprintf("Orig = %i rows",nrow(orig)))
message(sprintf("Today = %i rows",nrow(today)))
print(table(today$School.board))
print(today$institute.name)
message(sprintf("Merged = %i rows",nrow(merged)))

# Final map collapse for online
message("* Merging duplicates")
comp <- paste(merged$institute.name,merged$City,merged$School.board,sep="_")
dupset <- which(duplicated(comp))
print(dupset)
for (idx in dupset){
	message(sprintf("\t%s",merged$institute.name[idx]))
	blah <- merged[which(merged$institute.name %in% merged$institute.name[idx]),]
print(blah)
message("----")
	browser()
}


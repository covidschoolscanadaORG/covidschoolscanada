# new case entries to map

args <- commandArgs(TRUE)
boardName <- args[1] #"DDSB"#"Toronto_DSB" #"HDSB" # args[1]
boardNameTable <- args[2]## "Durham DSB" #"Toronto DSB" #"Halton DSB" #args[2]

message(boardName)

annotFile <- "/Users/shraddhapai/Google/Advocacy/COVID-19/daily_data/Canada_COVID_tracker/school_annotations/ONnew_sif_data_table_2018_2019prelim_en_august.tsv"
inRoot <- "/Users/shraddhapai/Google_covidschools/SchoolBoard_daily_snapshot"

inDir <- sprintf("%s/%s",inRoot,boardName)
cur <- dir(path=inDir,pattern="newentries.txt$",full.names=TRUE)
cur <- file.info(cur)
cur <- cur[order(cur$ctime,decreasing=TRUE),]
df <- difftime(Sys.time(),cur$ctime[1],units="hours")
if (df >= 20) {
	message("Last update about a day ago. No changes...")
  stop("")
}

inFile <- rownames(cur)[1]
dat <- read.delim(inFile,sep=",",h=T,as.is=T)
if (nrow(dat)<1) {
	stop("No new records")
}
outFile <- sprintf("%s.annot.csv",sub(".txt","",inFile))
dat <- dat[,c("School.Name","case_change")]

locs <- read.delim(annotFile,sep="\t",h=T,as.is=T)
locs <- locs[,c("School.Name","Board.Name","Latitude","Longitude","City","School.Level")]
locs <- subset(locs,Board.Name==boardNameTable)

x <- merge(x=dat,y=locs,by.x="School.Name",by.y="School.Name")
notthere <- setdiff(dat$School.Name,x$School.Name)
if (length(notthere)>0) {
	print("school names don't match")
	browser()
}

x$Entry.Date <- format(Sys.Date(),"%Y-%m-%d")
today <- x

today$institute.name <- today$School.Name
today$Province <- "ON"
if (boardName == "Toronto_DSB") {
	today$Total.cases.to.date <- today$case_change
	browser()
	today$Total.students.to.date <- NA
	today$Total.staff.to.date <- NA
} else {
	today$Total.cases.to.date <- today$case_change
	today$Total.students.to.date <- NA
	today$Total.staff.to.date <- NA
}
today$Article <- "https://tinyurl.com/covidschoolsCA-ONsnapshots"
today$Date <- format(Sys.Date(),"%Y-%m-%d")
today$Total.outbreaks.to.date <- 0
today$Outbreak.dates <- NA
today$Outbreak.Status <- "Single/unlinked cases"
today$Type_of_school <- today$School.Level
today$School.board <- today$Board.Name
today$isNew <- TRUE

today <- today[,c("institute.name","Province",
"Total.cases.to.date","Total.students.to.date",
"Total.staff.to.date","Article",
"Date","Total.outbreaks.to.date",
"Outbreak.dates","Outbreak.Status",
"Type_of_school","School.board",
"City","Latitude",
"Longitude","isNew",
"Entry.Date")]

write.table(today,file=outFile,sep=",",col=TRUE,row=FALSE,quote=TRUE)

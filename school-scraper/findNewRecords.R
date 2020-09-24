# compare recent two snapshots find new cases

args <- commandArgs(TRUE)
sb <-args[1]#"DDSB" #"HDSB" #args[1] #"Peel_DSB"
message(sb)

inDir <- "/Users/shraddhapai/Google_covidschools/SchoolBoard_daily_snapshot"
curd <- sprintf("%s/%s", inDir,sb)
paths <- dir(curd,pattern="csv",full.names=TRUE)
if (any(grep("newentries",paths))) {
	paths <- paths[-grep("newentries",paths)]
}
if (any(grep("compare",paths))) {
	paths <- paths[-grep("compare",paths)]
}
info <- file.info(paths)
info <- info[order(info$ctime,decreasing=TRUE),]

curShort <- sub(".csv","",basename(rownames(info)[2]))
prevShort <- sub(".csv","",basename(rownames(info)[3]))
logFile <- sprintf("%s/%s-%s.compare.txt",curd,curShort,prevShort)
outFile <- sprintf("%s/%s-%s.newentries.txt",curd,curShort,prevShort)


sink(logFile,split=TRUE)
print(sprintf("Cur = %s",rownames(info)[2]))
print(sprintf("Prev = %s",rownames(info)[3]))

cur <- read.delim(rownames(info)[2],sep=",",h=T,as.is=T,comment="#")
prev <- read.delim(rownames(info)[3],sep=",",h=T,as.is=T,comment="#")

tokeep <- list(
	Peel_DSB=c("School.Name","Confirmed.Cases"),
	HDSB=c("School.Name","Confirmed.Cases"),
	HCDSB=c("School.Name","Confirmed.Cases"),
	Toronto_DSB=c("School","Confirmed.Cases.Among.Students","Confirmed.Cases.Among.Staff"),
	DDSB=c("School","Confirmed.Cases..Active."),
	DPCDSB=c("School.Name","Confirmed.Cases"),
	YRDSB=c(1,2)
)

cur <- cur[,tokeep[[sb]]]
prev <- prev[,tokeep[[sb]]]
for (k in 2:ncol(cur)) {
	cur[,k] <- suppressWarnings(as.integer(cur[,k]))
	prev[,k] <- suppressWarnings(as.integer(prev[,k]))
}
colnames(cur)[1] <- "School.Name"
colnames(prev)[1] <- "School.Name"
if (sb %in% c("YRDSB","DDSB")) {
	colnames(cur)[2] <- "Confirmed.Cases"
	colnames(prev)[2] <- "Confirmed.Cases"
}

idx <- union(which(is.na(prev[,2])),which(is.na(cur[,2])))
if (any(idx)) {
	warning(sprintf("%i records had NA entries ***** removing",length(idx)))
	cur <- cur[-idx,]
	prev <- prev[-idx,]
}

both <- merge(x=prev,y=cur,by="School.Name")
if (sb == "Toronto_DSB") {
	chstaff <- both$Confirmed.Cases.Among.Staff.x - both$Confirmed.Cases.Among.Staff.y
	chstud <- both$Confirmed.Cases.Among.Students.x - both$Confirmed.Cases.Among.Students.y
  both$case_change <- chstaff + chstud
	both$changed <- (chstaff+chstud)>0
	both$changed_staff <- chstaff
	both$changed_students <- chstud
} else {
	both$changed <- both$Confirmed.Cases.y > both$Confirmed.Cases.x
	both$case_change <- both$Confirmed.Cases.y - both$Confirmed.Cases.x
}


#print(both[which(both$changed),])
if (sum(both$changed)>0) {
	message(sprintf("%s: %i changes today", sb, sum(both$changed)))
	write.table(both[which(both$changed),],file=outFile,sep=",",col=T,row=F,quote=T)
} else {
	message(sprintf("%s: No changes today",sb))
}

sink(NULL)



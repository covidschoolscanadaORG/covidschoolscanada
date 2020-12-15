# merge canada-wide and quebec
source("utils.R")
#options(warn=2)

args <- commandArgs(TRUE)
dt <- args[1]
rootDir <- args[2]
#rootDir <- "/home/shraddhapai/Canada_COVID_tracker/export"
#rootDir <- "/home/shraddhapai/Canada_COVID_tracker/export"

inDir <- sprintf("%s-%s",rootDir,dt)
failFile <- sprintf("%s/fail_mergeQC.txt",inDir)
#fail <- open(failFile,"w")
outFile <- sprintf("%s/CanadaMap_QuebecMerge-%s.csv",
	inDir,dt)
statFile <- sprintf("%s/CanadaMap_QuebecMerge-%s.stats.txt",
	inDir,dt)

if (file.exists(outFile)) unlink(outFile)
if (file.exists(statFile)) unlink(statFile)

can <- read.delim(sprintf("%s/CanadaMap_clean3.kml-%s.txt",
	inDir,dt),fileEncoding="UTF-8",sep="\t",h=T,as.is=T,
	stringsAsFactors=FALSE)
colnames(can) <- sub("^X.","",colnames(can))
colnames(can) <- sub("\\.$","",colnames(can))

rest_of_canada <- subset(can, !Province %in% c( "QC","Québec"))
can <- rest_of_canada



message("* Provincial breakdown")
print(table(can$Province,useNA="always"))
can$Province <- sub("Ontario","ON",can$Province)

message("* Reading Quebec")
qc <- read.delim(sprintf("%s/COVIDEcolesQuebec_clean3.kml-%s.txt",
	inDir,dt),sep="\t",h=T,as.is=T)
colnames(qc)[4] <- "Outbreak.Status"
qc$Total.outbreaks.to.date <- rep(0,nrow(qc))
qc$Total.outbreaks.to.date[grep("outbreak",qc[,4])]<- 1
qc$Province <- "QC"

qc2 <- read.delim(sprintf("%s/COVIDEcolesQuebec_layer2_clean3.kml-%s.txt",
	inDir,dt),sep="\t",h=T,as.is=T)
colnames(qc2)[4] <- "Outbreak.Status"
qc2$Total.outbreaks.to.date <- rep(0,nrow(qc2))
qc2$Total.outbreaks.to.date[grep("outbreak",qc2[,4])]<- 1
qc2$Province <- "QC"
qc2 <- qc2[,colnames(qc)]
old <- nrow(qc)
qc <- rbind(qc,qc2)
message(sprintf("QC before, after merging: %i to %i rows",
	old,nrow(qc)))

message("")
message("* Merging QC data")

new2 <- setdiff(colnames(can),colnames(qc))
for (dont_have in new2) {
	qc[,dont_have] <- NA
}
midx <- match(colnames(qc),colnames(can))
if (all.equal(colnames(can)[midx],colnames(qc))!=TRUE) {
	stop("MERGE TABLE FAIL - column name order")
}
can <- can[,midx]
if (all.equal(colnames(can),colnames(qc))!=TRUE) {
	stop("MERGE TABLE FAIL - column name order 2")
}

x <- rbind(can,qc)

message("Table sizes")
xqc <- subset(x,Province=="QC")
non <- subset(x,Province!="QC")
message(
	sprintf("BEFORE:\t%i QC\t%i other",nrow(qc),nrow(can))
	)
message(
	sprintf("MERGED:\t%i QC\t%i other",nrow(xqc),nrow(non))
	)

if (nrow(x) != (nrow(can) + nrow(qc))) {
	message("")
	message("****************************************")
	message("")
	message("ERRORS WHILE MERGING QUEBEC DATA")
	message("Resolve first")
	message("")
	message("****************************************")
	stop("")
}

x$Lat2 <- x$Latitude
x$Long2 <- x$Longitude
x <- x[,-which(colnames(x)%in% c("Latitude","Longitude"))]
colnames(x)[(ncol(x)-1):ncol(x)] <- c("Latitude","Longitude")
final <- x

# clean text
final$Total.cases.to.date <- stringr::str_trim(final$Total.cases.to.date)

final$Total.outbreaks.to.date <- stringr::str_trim(final$Total.outbreaks.to.date)
####tryCatch({
###idx <- suppressWarnings(which(is.na(as.integer(final$Total.outbreaks.to.date))))
###if (any(idx)) {
###	message("Converting outbreaks to integer failed")
###	write.table(final[idx,],file=failFile,sep="\t",
###		col=T,row=F,quote=F)
###	message(sprintf("\tFAILED: %i rows > excluding",length(idx)))
###	final <- final[-idx,]
###}
###final$Total.outbreaks.to.date <- as.integer(
###		final$Total.outbreaks.to.date)

if (length(grep("Henry Wise Wood High School", 
	final$Type_of_school))>0) {
	final$Type_of_school[which(final$Type_of_school== "Henry Wise Wood High School")] <- "High School"
}
final$Type_of_school <- stringr::str_trim(final$Type_of_school)
final$Type_of_school <- tools::toTitleCase(trimws(final$Type_of_school))

message("")
message("*** FINAL ***")
message("")
message(sprintf("# institutions = %i rows", nrow(final)))
#message(sprintf("# outbreaks = %i rows", sum(final$Total.outbreaks.to.date,
#		na.omit=TRUE)))
message("")
message("-------------------------------------")
message("* Num institutions: PROVINCE")
message("-------------------------------------")
print(getTable_dec(final$Province))
message("")

###message("-------------------------------------")
###message("* Num outbreaks: PROVINCE")
###message("-------------------------------------")
###tmp <- aggregate(final$Total.outbreaks.to.date, by=list(final$Province),FUN=sum)
###tmp2 <- tmp[,2]; names(tmp2) <- tmp[,1]; print(tmp2[order(tmp2,decreasing=TRUE)])

###message("-------------------------------------")
###message("* Type of schools: CANADA")
###message("-------------------------------------")
###final2 <- subset(final, Province != "Québec")
###print(getTable_dec(final$Type_of_school))
###message("")
###message("-------------------------------------")
###message("* Case type breakdown (except Quebec)")
###message("-------------------------------------")
###message("Total (confirmed reports)")
###print(table(final2$Total.cases.to.date,useNA="always"))
###message("Students (confirmed reports)")
###print(table(final2$Total.students.to.date,useNA="always"))
###message("Staff (confirmed reports)")
###print(table(final2$Total.staff.to.date,useNA="always"))
###message("")

out <- final$Total.outbreaks.to.date
out[which(out=="")] <- NA
str2 <- rep("Outbreak status unknown",nrow(final))
str2[which(out<1)] <- "Single/unlinked cases"
str2[which(out>0.5)] <- "Declared outbreak"
final$Outbreak.Status <- str2
print(table(final$Outbreak.Status,useNA="always"))

message("* Writing file")
write.table(final,file=outFile,sep=",",col=TRUE,row=F,quote=T)

message("* Writing stats")
if (file.exists(statFile)) unlink(statFile)


cat("",file=statFile,append=TRUE)
cat("Tally\n",file=statFile,append=TRUE)
ct <- getTable_dec(final$Province)
for (k in 1:length(ct)) {
	cat(sprintf("%s\t%s\n",names(ct)[k],ct[k]),
			file=statFile,append=TRUE)
}

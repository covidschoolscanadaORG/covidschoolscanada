# merge canada-wide and quebec
source("utils.R")

args <- commandArgs(TRUE)
dt <- args[1]
rootDir <- "/home/shraddhapai/Canada_COVID_tracker/export"

inDir <- sprintf("%s-%s",rootDir,dt)
outFile <- sprintf("%s/CanadaMap_QuebecMerge-%s.csv",
	inDir,dt)
statFile <- sprintf("%s/CanadaMap_QuebecMerge-%s.stats.txt",
	inDir,dt)

message("* Reading Canada-wide")
can <- read.delim(sprintf("%s/CanadaMap_clean3.kml-%s.txt",
	inDir,dt),fileEncoding="UTF-8",sep="\t",h=T,as.is=T,
	stringsAsFactors=FALSE)
colnames(can) <- sub("^X.","",colnames(can))
colnames(can) <- sub("\\.$","",colnames(can))
rest_of_canada <- subset(can, Province != "Québec")

message("* Provincial breakdown")
print(table(can$Province,useNA="always"))
can <- subset(can, Province=="Québec")

message("* Reading Quebec")
qc <- read.delim(sprintf("%s/COVIDEcolesQuebec_clean.kml-%s.txt",
	inDir,dt),sep="\t",h=T,as.is=T)
colnames(qc)[2:3] <- c("Latitude","Longitude")
qc$coord <- paste(qc$Latitude,qc$Longitude,sep="_")
can$coord <- paste(can$Latitude,can$Longitude,sep="_")

message("")
message("* Merging QC data")
x <- merge(qc,can,by="coord",all.x=TRUE)

message("Table sizes")
message(sprintf("\tCanada\t%i rows", nrow(can)))
message(sprintf("\tCEQ\t%i rows", nrow(qc)))
message(sprintf("\tMERGED\t%i rows",nrow(x)))
message("-----")
message("* Canada entries which didn't merge correctly")
print(setdiff(can$institute.name, x$institute.name.y))
message("-----")

x <- x[,-which(colnames(x) %in% c("coord","Latitude.y","Longitude.y","institute.name.y"))]
colnames(x) <- sub("\\.[xy]$","",colnames(x))
x$Lat2 <- x$Latitude
x$Long2 <- x$Longitude
x <- x[,-c(2:3)]
colnames(x)[(ncol(x)-1):ncol(x)] <- c("Latitude","Longitude")
x$Province <- "Québec"

if (all.equal(colnames(x),colnames(rest_of_canada))!=TRUE) {
	print("when merging colnames not in right order")
	browser()
}
final <- rbind(rest_of_canada,x)

if (any(final$Type_of_school == "Henry Wise Wood High School")) {
	final$Type_of_school[which(final$Type_of_school== "Henry Wise Wood High School")] <- "High School"
}
final$Type_of_school <- tools::toTitleCase(trimws(final$Type_of_school))

message("")
message("*** FINAL ***")
message("")
message(sprintf("# institutions = %i rows", nrow(final)))
message(sprintf("# outbreaks = %i rows", sum(final$Total.outbreaks.to.date,
		na.omit=TRUE)))
message("")
message("-------------------------------------")
message("* Num institutions: PROVINCE")
message("-------------------------------------")
print(getTable_dec(final$Province))
message("")

message("-------------------------------------")
message("* Num outbreaks: PROVINCE")
message("-------------------------------------")
tmp <- aggregate(final$Total.outbreaks.to.date, by=list(final$Province),FUN=sum)
tmp2 <- tmp[,2]; names(tmp2) <- tmp[,1]; print(tmp2[order(tmp2,decreasing=TRUE)])

message("-------------------------------------")
message("* Type of schools: CANADA")
message("-------------------------------------")
final2 <- subset(final, Province != "Québec")
print(getTable_dec(final$Type_of_school))
message("")
message("-------------------------------------")
message("* Case type breakdown (except Quebec)")
message("-------------------------------------")
message("Total (confirmed reports)")
print(summary(final2$Total.cases.to.date,useNA="always"))
message("Students (confirmed reports)")
print(summary(final2$Total.students.to.date,useNA="always"))
message("Staff (confirmed reports)")
print(summary(final2$Total.staff.to.date,useNA="always"))
message("")


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

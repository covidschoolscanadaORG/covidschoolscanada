# merge canada-wide and quebec

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

message("")
message("* Post-merge: Provincial breakdown")
print(table(final$Province,useNA="always"))
message(sprintf("Total cases = %i rows", nrow(final)))

message("* Writing file")

write.table(final,file=outFile,sep=",",col=TRUE,row=F,quote=T)

message("* Writing stats")
if (file.exists(statFile)) unlink(statFile)
ct <-table(final$Province)
nm <- names(ct)
ct <- as.numeric(ct); names(ct) <- nm
ct <- ct[order(ct,decreasing=TRUE)]
print(ct)

cat("",file=statFile,append=TRUE)
cat("Tally\n",file=statFile,append=TRUE)
for (k in 1:length(ct)) {
	cat(sprintf("%s\t%s\n",names(ct)[k],ct[k]),
			file=statFile,append=TRUE)
}

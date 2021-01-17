# set up map

makeGeoJSON <- function(inDir,dt) {
mapDir <- "/Users/shraddhapai/software/covidschoolscanada.github.io/maps/"

dat <- read.delim(sprintf("%s/final_data/CanadaMap_QuebecMerge-%s.clean.csv",inDir,dt),sep=",",h=T,as.is=T)
qc <- read.delim(sprintf("%s/CEQ_annotated_clean_%s.csv",inDir,dt),sep=",",h=T,as.is=T)

qc <- qc[,colnames(dat)]
qc$Longitude <- -1*qc$Longitude

idx <- union(which(is.na(qc$Latitude)), 
	which(is.na(qc$Longitude)))
message(sprintf("* %i entries missing lat/long", length(idx)))
if (any(idx)) qc <- qc[-idx,]

qc$Outbreak.Status[grep("outbreak",qc$Outbreak.Status)] <- "Declared outbreak"

dat <- dat[-which(dat$Province=="QC"),]
dat <- rbind(dat,qc)

# prettify link text
links <- strsplit(dat$Article,";")
links2 <- list()
for (k in 1:length(links)) {
	ln <- length(links[[k]])
	links[[k]] <- paste('<a href="',links[[k]],">Link",1:ln,
			"</a>",sep="")
	links2[[k]] <- paste(links[[k]],collapse=";")
}
links2 <- unlist(links2)
#dat$Article_Pretty <- links2

# move each status type to own column
message("* Adding status columns")
x <- rep(0,nrow(dat))
x[which(dat$Outreak.Status=="Single/unlinked cases")] <- 1
dat$Is_Single <- x

x <- rep(0,nrow(dat))
x[which(dat$Outreak.Status=="Declared outbreak")] <- 1
dat$Is_Outbreak <- x

x <- rep(0,nrow(dat))
x[grep("Clusters",dat$Outbreak.Status)] <- 1
dat$Is_Cluster <- x

# have "last reported date" column
message("* Adding last reported date column")
dt2 <- lapply(strsplit(dat$Date,";"),function(x) {
    x <- stringr::str_trim(x)
    x[length(x)]
})
dt2 <- unlist(dt2)
dat$Last_Reported_Date <- dt2

# calculate date diff relative to today
newd <- rep(NA,nrow(dat))
dateDiff <- rep(NA,nrow(dat))
for (k in 1:length(dat$Last_Reported_Date)){
    cur <- dat$Last_Reported_Date[k]
    if (!is.na(cur)){
        pos <- gregexpr("-",cur)
        yyyy <- substr(cur,1,pos[[1]][1]-1)
        mm <- substr(cur,pos[[1]][1]+1,pos[[1]][2]-1)
        dd <- substr(cur,pos[[1]][2]+1,nchar(cur))
        newd[k] <- sprintf("%s-%s-%s",mm,dd,yyyy)
    }
    dateDiff
}
dat$Difference_In_Days <- Sys.Date() - as.Date(dat$Last_Reported_Date)


# have total case column
message("* Adding total case column")
cs <- lapply(strsplit(dat$Total.cases.to.date,";"),function(x){
    x <- stringr::str_trim(x)
    x <- sum(as.integer(x),na.rm=TRUE)
})
cs <- unlist(cs)
dat$Total_Cases_Summed <- cs

# convert all dots to underscores in column names
colnames(dat) <- gsub("\\.","_",colnames(dat))
print(colnames(dat))

idx <- which(is.na(dat$Last_Reported_Date))
if (length(idx)>0) dat <- dat[-idx,]
outCSV <- "CanadaMap_QuebecMerge-current.clean.csv"
outJSON <- sub(".csv",".geojson",outCSV)
write.table(dat,file=outCSV,sep=",",col=T,row=F,quote=T)

message("* Converting to geojson")
system2("csv2geojson", args=c(outCSV,">",outJSON))

message("* Move to live web folder")
tgtFile <- sprintf("%s/%s",mapDir,outJSON)
file.rename(outJSON, sprintf("%s/%s",mapDir,outJSON))

cwd <- getwd()
setwd(mapDir)
system2("git","pull")

#message("* git commit")
#system2("git",args=c("commit","-m","\"map update\"", tgtFile))
#system2("git","push")
setwd(cwd)
browser()
}


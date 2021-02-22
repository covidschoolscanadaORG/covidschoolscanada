# flatten cases creating input file for map.

flattenCaseCol <- function(inFile,qcFile) {

dat <- read.delim(inFile,sep=",",h=T,as.is=T)
qc <- read.delim(qcFile,sep=",",h=T,as.is=T)
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
dt2 <- strsplit(dat$Date,";")
for (k in 1:length(dt2)){
    dt3 <- as.Date(dt2[[k]])
    dt2[[k]] <- as.character(dt3[which.max(dt3)])
}
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

return(dat)
}

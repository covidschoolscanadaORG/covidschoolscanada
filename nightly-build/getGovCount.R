require(googlesheets4)

# get official govt counts
getGovCounts <- function(inDir,dt){
govt_count <-list()
message("Reading MB")
mb_url <- "https://docs.google.com/spreadsheets/d/1FYgmi2S_Tjhn1qfem7sqdY9NzvHaC5YLuZeFiSVW838/edit#gid=279193410"
googlesheets4::gs4_deauth()
x <- as.data.frame(googlesheets4::read_sheet(mb_url))
idx <- which(x[,1] == "GRAND TOTAL OF ALL KNOWN K-12 CASES") #TOTAL MB K-12 CASES")
mb <- x[idx,3]
govt_count[["MB"]] <- 2997#as.integer(mb)

message("Reading QC")
QCfile <- sprintf("%s/CEQ_annotated_%s.csv",inDir,dt)
tmp <- read.delim(QCfile,sep=",",h=T,as.is=T)
tmp <- tmp[nrow(tmp),2]
tmp <- sub("^DO NOT ERASE ","",tmp)
spos <- regexpr(" ",tmp)
tmp <- substr(tmp,1,spos-1)
govt_count[["QC"]] <- as.integer(tmp)

return(govt_count)
}

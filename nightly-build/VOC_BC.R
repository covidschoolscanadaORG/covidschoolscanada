require(rdrop2)
dbox <- "dbox.rds"
token <- readRDS(dbox)
message("Authorizing dropbox")
suppressMessages(drop_acc(dtoken=token))

dt <- format(Sys.Date()-1,"%Y%m%d")
odir <- sprintf("BC_automation/daily_update/export-%s",dt)

dt2 <- format(Sys.Date(),"%y%m%d")
localDir <- sprintf("/Users/shraddhapai/Google_covidschools/daily_data/Canada_COVID_tracker/export-%s",dt2)
outFile <- sprintf("%s/BC_VOC_%s.csv",localDir,dt2)

rfile <- sprintf("%s/BCTracker-raw-%s.csv",odir,dt)
ofile <- sprintf("%s/CanadaMap_BC-%s.clean.csv",odir,dt)
message("Downloading BC")
lpath <- "."
if (file.exists(basename(rfile))) unlink(basename(rfile))
drop_download(path=rfile,dtoken=token,overwrite=TRUE)
bc <- read.delim(basename(rfile),sep=",",h=T,as.is=T)
bc <- subset(bc,(Variant.of.Concern)!="")
colnames(bc)[1] <- "Date"
tmp <- strsplit(bc$Date,"/")
tmp2 <- lapply(tmp, function(x) {
		paste(x[3],x[1],x[2],sep="-",collapse="-")
	}
)
bc$Date2 <- unlist(tmp2)

tbl <- table(bc$School); 
tbl_int <- as.integer(tbl)
once <- names(tbl)[which(tbl_int < 2)];
often <- names(tbl)[which(tbl_int>1)]; 

bc_once <- bc[which(bc$School %in% once),]
bc_once$Date3 <- bc_once$Date2
multi_list <- list()
for (k in often) {
	cur <- subset(bc, School %in% k)
	tmp <- cur[1,]
	tmp$Date3 <- paste(cur$Date2,sep=";",collapse=";")
	multi_list[[k]] <- tmp
}
multi2 <- do.call("rbind",multi_list)
rownames(multi2) <- NULL

bc_collap <- rbind(bc_once,multi2)

if (file.exists(basename(ofile))) unlink(basename(ofile))
drop_download(path=ofile,dtoken=token,overwrite=TRUE)
bc_clean <- read.delim(basename(ofile),sep=",",h=T,as.is=T)
midx <- match(bc_collap$School, bc_clean$Tracker.Name)
if (all.equal(bc_collap$School,bc_clean$Tracker.Name[midx])!=TRUE) {
	message("match incomplete")
	browser()
	}

bc_collap$Latitude <- bc_clean$Latitude[midx]+0.0005
bc_collap$Longitude <- bc_clean$Longitude[midx]+0.0005
bc_collap$Article <- "https://bcschoolcovidtracker.knack.com/bc-school-covid-tracker"
write.table(bc_collap,file=outFile,sep=",",col=T,row=F,quote=T)


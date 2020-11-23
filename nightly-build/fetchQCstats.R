# download CovidEcoles sheet with school annotation

args <- commandArgs(TRUE)
outRoot <- args[1]
sheetURL <- "https://docs.google.com/spreadsheets/d/1Il2Xhz-VKhME4B9EqKkzbmT5APf3IaKktyk80SQi6JA/edit#gid=154085736"

Sys.setenv(TZ="America/Toronto")
dt <- format(Sys.Date(),"%y%m%d")
outDir <- sprintf("%s-%s",args[1], dt)

require(gsheet)
message("* Fetching CEQ annotation")
x <- gsheet2tbl(sheetURL)
y <- as.data.frame(x)
outFile <- sprintf("%s/CEQ_annotated_%s.csv",outDir,dt)
message("* Writing local copy")
write.table(y,file=outFile,sep=",",col=TRUE,row=FALSE,
	quote=TRUE)



rm(list=ls())
require(rdrop2)

dbox <- "dbox.rds"
token <- readRDS(dbox)
message("Authorizing dropbox")
drop_acc(dtoken=token)

srcDir <- "SchoolBoard_daily_snapshot"
tgtDir <- "/Users/shraddhapai/Google_covidschools/SchoolBoard_daily_snapshot"
if (!file.exists(tgtDir)) dir.create(tgtDir)

existDir <- "York_CDSB" #setdiff(dir(tgtDir),"York_CDSB")

x <- drop_dir(srcDir)
for (y in existDir) { #d$x$path_display[1:35]) {
	baseF <- basename(y)
	message(baseF)
	tgt2 <- sprintf("%s/%s",tgtDir,baseF)
	if (!file.exists(tgt2)) dir.create(tgt2)
	z <- drop_dir(sprintf("%s/%s",srcDir,y))

	base2 <- basename(z$path_display)
	idx <- 1:length(base2)#grep("^2021-", base2)
	for (w in idx) { #1:length(base2))	{
		baseD <-basename(z$path_display[w])
		tgtPath <- sprintf("%s/%s",tgt2,baseD)
		if (!file.exists(tgtPath)) {
		message(sprintf("getting %s", z$path_display[w]))
		drop_download(z$path_display[w], local_path=tgt2,dtoken=token,
			overwrite=TRUE)
		}
	}
}



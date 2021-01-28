
rm(list=ls())
require(rdrop2)

dbox <- "dbox.rds"
token <- readRDS(dbox)
message("Authorizing dropbox")
drop_acc(dtoken=token)

srcDir <- "SchoolBoard_daily_snapshot"
tgtDir <- "/Users/shraddhapai/Google_covidschools/SchoolBoard_daily_snapshot"
if (!file.exists(tgtDir)) dir.create(tgtDir)

x <- drop_dir(srcDir)
existDir <- dir(tgtDir)

for (y in existDir) {
		fullD <- sprintf("%s/%s",srcDir,y)
		message(fullD)
		if (drop_exists(fullD)) {
		drop_delete(fullD,dtoken=token)
		}
}



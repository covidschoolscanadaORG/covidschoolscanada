require(rdrop2)
require(dplyr)
require(googlesheets4)

dbox <- "dbox.rds"

message("BC")
bcGS <- "https://docs.google.com/spreadsheets/d/1MYDtPm_iaVHiiDtFBlToPD0a9JQF0CkXXSgrPL9w-Ko"

token <- readRDS(dbox)
message("Authorizing dropbox")
suppressMessages(drop_acc(dtoken=token))
dt <- format(Sys.Date(),"%Y%m%d")
odir <- sprintf("BC_automation/daily_update/export-%s",dt)
ofile <- sprintf("%s/CanadaMap_BC-%s.clean.csv",odir,dt)
message("Downloading BC")
lpath <- "."
if (file.exists(basename(ofile))) unlink(basename(ofile))
drop_download(path=ofile,dtoken=token,overwrite=TRUE)

dat <- read.delim(basename(ofile),sep=",",h=T,as.is=T)

message("Overwriting online autogen copy")
sheet_write(dat,bcGS,1)

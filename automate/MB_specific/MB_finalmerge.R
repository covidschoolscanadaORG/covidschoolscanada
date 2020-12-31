# final merge of MB google map to prov mappings

rm(list=ls())
require(readxl)
###MB1 <- read_excel("/Users/shraddhapai/Google_covidschools/SchoolInfo/MB/MB_GoogleMap2Prov.xlsx",skip=1)
###MB2 <- read_excel("/Users/shraddhapai/Google_covidschools/SchoolInfo/MB/mb_match_201227_SPreconciled_2.xlsx")
###
###blah <- rbind(MB1,MB2)
###blah <- blah[!duplicated(blah[,c("institute.name","City")]),]
###options("encoding"="TF-8")
###write.table(blah,file="MB_GoogleMap2Prov_FinalMerge_pre.csv",sep=",",col=T,row=F,quote=T)
###

map2prov <- read_excel("/Users/shraddhapai/Google_covidschools/SchoolInfo/MB/MB_GoogleMap2Prov_FinalMerge_pre.xlsx")
dat <- read_excel("/Users/shraddhapai/Google_covidschools/SchoolInfo/MB/clean.csv_MB_201224_FREEZE_CLEAN.xlsx")

idx <- which(!dat$institute.name %in% map2prov$institute.name)

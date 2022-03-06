# replace ON autogen with latest table
rm(list=ls())

Sys.setenv("R_BROWSER"="/usr/bin/open -a '/Applications/Firefox.app'")
options(browser = as.vector(Sys.getenv("R_BROWSER")))

#dt <- format(Sys.Date()-1,"%Y-%m-%d")
dt <- format(Sys.Date(),"%Y-%m-%d")
#dt <- "2021-06-24"
#dt <- "2021-06-18"
fName <- sprintf("/Users/shraddhapai/Google_covidschools/daily_data/ON/Automated_boards_%s.csv",dt)

message("Reading latest ON file")
dat <- read.delim(fName,sep=",",h=T,as.is=T)

targetURL <- "https://docs.google.com/spreadsheets/d/14-CM4FX666eRcMCTpKJoNViUpW50P4OAH433dQxn9Yg"
require(googlesheets4)
message("Overwriting online autogen copy")
sheet_write(dat,targetURL,1)


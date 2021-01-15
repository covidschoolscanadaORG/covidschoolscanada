
source("utils.R")
abDir <- "/Users/shraddhapai/Google_covidschools/daily_data/AB/"

for (dt in c("2021-01-12")) {
	abFile <- sprintf("%s/AB_Automated_boards_%s.csv",abDir,dt)
	dat <- read.delim(abFile,sep=",",h=T,as.is=T)
browser()
	dat <- flattenCases(dat)
browser()
	dat$Total.cases.to.date <- as.integer(dat$Total.cases.to.date)
	cs <- sum(dat$Total.cases.to.date,na.rm=TRUE)
	message(sprintf("%s: %i cases",dt,cs))
}

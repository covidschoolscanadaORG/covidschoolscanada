

inDir <- "/Users/shraddhapai/Google_covidschools/daily_data/AB/AB_Automated_boards_2021-01-"

for (k in c("06","07","08","09","10","11","12","13")) {
		fname <- sprintf("%s%s.csv",inDir,k)
	dat <- read.delim(fname,sep=",",h=T,as.is=T)
	cs <- stringr::str_trim(dat$Total.cases.to.date)
	cs <- gsub(" ","",cs)
	cs2 <- strsplit(cs,";")
	cs3 <- lapply(cs2,function(x) {
					return(sum(as.integer(x),na.rm=TRUE))
			})
	cs3 <- unlist(cs3)
	message(sprintf("%s: %i schools, %i cases",k,nrow(dat),sum(cs3,na.rm=TRUE)))
	
}

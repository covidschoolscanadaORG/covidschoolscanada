# fuzzy-match bc Alberta data to us
rm(list=ls())
require(tibble)
require(fuzzyjoin)
require(readxl)

dat <- read.delim("/Users/shraddhapai/Google_covidschools/daily_data/Canada_COVID_tracker/export-201224/final_data/CanadaMap_QuebecMerge-201224.clean.csv",
	sep=",",h=T,as.is=T)

autoGen_boards <- c("Peel DSB", "Toronto DSB",
	"York Region DSB","York CDSB",
	"Ottawa-Carleton DSB","Ottawa CDSB")

dat <- subset(dat, Province=="ON")
message(sprintf("%i ON records",nrow(dat)))
dat <- dat[-which(dat$School.board %in% autoGen_boards),]
message(sprintf("%i left after removing autogen",nrow(dat)))
dat$institute.name <- stringr::str_trim(dat$institute.name)

write.table(dat,file="ON_board_201231.csv",sep=",",col=T,row=F,quote=T)

on <- read_excel("/Users/shraddhapai/Google_covidschools/SchoolInfo/ON/Master File of Schools.xlsx")
on <- as.data.frame(on)
colnames(on) <- c("Board.Name","School.Code","School.Name","Level","Street","City")
on <- on[,c("School.Code","School.Name","Board.Name","City")]
on$School.Name <- stringr::str_trim(on$School.Name)

dat_t <- as_tibble(dat)
on_t<- as_tibble(on)

good<- stringdist_join(x=dat_t,y=on_t, 
	by=c(institute.name="School.Name"),max_dist=0)
good <- subset(good, good$City.x==good$City.y)
good$mm <- 0
message(sprintf("0 MM = %i schools", nrow(good)))

the_rest <- dat_t[-which(dat_t$institute.name %in% good$institute.name),]

MAX_MISMATCH <- 13
mm <- 0
while (nrow(the_rest)>1 & mm < MAX_MISMATCH) {
	mm <- mm + 1
	cur <- stringdist_join(x=the_rest,y=on_t, 
		by=c(institute.name="School.Name"),max_dist=mm)
	message(sprintf("%i MM = %i schools", mm,
		length(unique(cur$institute.name))))
	cur$mm <- mm
	good <- rbind(good, cur)
	the_rest <- dat_t[-which(dat_t$institute.name %in% good$institute.name),]
}

good$city_match <- good$City.x == good$City.y
good <- good[,c("institute.name","School.board","City.x",
	"School.Code","School.Name","Board.Name","City.y",
	"mm","city_match")]
write.table(good,file="on_match.csv",sep=",",col=T,row=F,quote=T)
write.table(the_rest,file="on_unmatch.csv",sep=",",col=T,row=F,quote=T)

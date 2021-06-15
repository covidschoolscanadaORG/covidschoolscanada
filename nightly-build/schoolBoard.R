# generates graphs showing case breakdown by school board

rm(list=ls())
suppressMessages(require(dplyr))
suppressMessages(require(ggplot2))
suppressMessages(require(gtable))  # annotate page
suppressMessages(require(cowplot)) # ggplot to gtable
suppressMessages(require(grid))	 # annotate page
suppressMessages(require(showtext))
source("utils.R")

font_add_google(name = "Yantramanav", family = "yantramanav")
font_add_google(name = "Source Sans Pro", family = "source-sans-pro")
showtext_auto()

provFull <- list(
	BC="British Columbia",
	ON="Ontario",
	AB="Alberta",
	MB="Manitoba",
	SK="Saskatchewan",
  NB="New Brunswick",
	NS="Nova Scotia",
	PEI="Prince Edward Island",
	NU="Nunavut"
)

school_th <-  theme(
	panel.background = element_rect(fill="#ffefef"),
	plot.background = element_rect(fill = "white"),
	panel.grid.minor = element_blank(), 
	panel.grid.major = element_line(
		color = "gray90", size = 0.5,
		linetype="dashed"),
	panel.grid.major.x = element_blank(),
	axis.ticks = element_line(colour = "#68382c"),
	axis.text = element_text(family="source-sans-pro",
		colour="#68382C",size=14),
	axis.title.y = element_text(family="source-sans-pro",
		colour="#68382C",size=14),
	axis.text.x = element_text(angle=90,
		hjust=1,vjust=0.2),
	plot.title = element_text(family="yantramanav",
		hjust = 0.5,size=20,colour="red",face="bold"),
	plot.subtitle = element_text(family="yantramanav",hjust=0.5,
		size=14,colour="red",face="bold"),
	plot.caption = element_text(family="source-sans-pro",size=16,
		face="bold",hjust=0,colour="red")
	#plot.margin = unit(c(60,20,30,20),"pt"),
)

# --------------------------------------
# Start processing
Sys.setenv(TZ="America/Toronto")

dt <- format(Sys.Date(),"%y%m%d")
inDir <- sprintf("/Users/shraddhapai/Google_covidschools/daily_data/Canada_COVID_tracker/export-%s",dt)
inFile <- sprintf("%s/CanadaMap_QuebecMerge-%s.clean.csv_dat2.csv",
	inDir,dt)
dat <- read.delim(inFile,sep=",",h=T,as.is=T)
dat <- subset(dat, Province!="QC")

dat$School.board <- sub("Hamilton-Wentowrth","Hamilton-Wentworth",dat$School.board)
dat$School.board <- sub("Arch. of Winnipeg","Arch. Of Winnipeg",dat$School.board)
dat$School.board[grep("Calvin Christian School Society",dat$School.board)] <- "Calvin Christian Schools"

qcStats <- sprintf("%s/CEQ_annotated_clean_%s.csv",
	inDir,dt)
qcStats <- read.delim(qcStats,sep=",",h=T,as.is=T)
#qcStats <- qcStats[,c("Date","Province","Total.cases.to.date","institute.name","School.board")]

dat <- rbind(dat,qcStats)
#dat$tstamp <- as.Date(dat$Date)
#dat <- subset(dat, tstamp > Sys.Date()-14)
#dat <- dat[,c("Total.cases.to.date","School.board","Province")]
#colnames(dat)[2] <- "Board"
#bloo <- aggregate(dat$Total.cases.to.date, 
#	by=list(Board=dat$Board, Province=dat$Province),
#		FUN=sum)

dat$ct <- 1
df2 <- aggregate(dat$ct, 
	by=list(Province=dat$Province,Board=dat$School.board),
FUN=sum)
df2$Province <- factor(df2$Province)
df2$Board <- factor(df2$Board)

colnames(df2)[3] <- "ct"
pList <- list()
for (prov in unique(df2$Province)) {
	message("------------------------")
	message(prov)
	message("")
	df3 <- subset(df2,Province==prov)
	message("")
	p <- ggplot(data = df3, aes(x=Board,y=ct))
	p <- p + geom_bar(stat="identity",fill="#FF6666")
	if (prov=="QC") {
		ttl <- sprintf("%s: CONFIRMED SCHOOLS, by REGION",
				prov)
	} else {
		ttl <- sprintf("%s: CONFIRMED SCHOOLS, by SCHOOL BOARD",
				prov)
	}
	p <- p + labs(
		title = ttl,
      	subtitle = "CANADA COVID-19 SCHOOL TRACKER",
       	caption = sprintf("@covidschoolsCA | Updated %s ",
					footerDate())
	)
	p <- p + school_th
	if (prov %in% c("QC","ON","BC")) {
		p <- p + theme(axis.text.x=element_text(size=10))
	}
	if (prov %in% c("AB")) {
		p <- p + theme(axis.text.x=element_text(size=8))
	}
	p <- p + ylim(0,max(df3$ct)+ceiling(0.15*max(df3$ct))) 
	p <- p + ylab("Number of affected schools") + xlab("")
	pdfFile <- sprintf("%s/%s_schoolboard.pdf",inDir,prov)
	pdf(pdfFile,width=9,height=5.5)
	print(p)
	dev.off()

	message("* Converting to jpg")
	system2("convert",args=c("-density","400","-quality","100",
		pdfFile,sprintf("%s/social_media/%s_schoolboard.jpg",inDir,prov)))
}


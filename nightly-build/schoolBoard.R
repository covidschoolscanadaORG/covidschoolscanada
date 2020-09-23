
suppressMessages(require(dplyr))
suppressMessages(require(ggplot2))
suppressMessages(require(gtable))  # annotate page
suppressMessages(require(cowplot)) # ggplot to gtable
suppressMessages(require(grid))	 # annotate page
source("utils.R")

provFull <- list(
	BC="British Columbia",
	ON="Ontario",
	AB="Alberta",
	MB="Manitoba",
	SK="Saskatchewan"
)

school_th <-  theme(
	panel.background = element_blank(),
	panel.grid.minor = element_blank(), 
	panel.grid.major = element_line(color = "gray90", size = 1,
		linetype="dashed"),
	panel.grid.major.x = element_blank(),
	axis.ticks = element_line(colour = 'gray50'),
	axis.text.x = element_text(angle=90,
		colour="#68382C",size=14,hjust=0.95,vjust=0.2),
	axis.text.y = element_text(colour="#68382C", size = 18),
	axis.title=element_text(size=20),
	plot.title = element_text(hjust = 0.5,size=20),
	panel.border = element_blank(),
	plot.margin = unit(c(60,20,30,20),"pt")
)

dt <- format(Sys.Date(),"%y%m%d")
inDir <- sprintf("/home/shraddhapai/Canada_COVID_tracker/export-%s",dt)
inFile <- sprintf("%s/CanadaMap_QuebecMerge-%s.clean.csv",
	inDir,dt)
dat <- read.delim(inFile,sep=",",h=T,as.is=T)

dat$School.board <- sub(" Kootenay/Columbia"," Kootenay-Columbia",dat$School.board)

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
	#p <- ggplot(data = df3, aes(x=reorder(Board,-ct),y=ct))
	p <- ggplot(data = df3, aes(x=Board,y=ct))
	p <- p + geom_bar(stat="identity",fill="#FF6666")
	p <- p + school_th
	p <- p + ylim(0,max(df3$ct)+ceiling(0.15*max(df3$ct))) 
	p <- p + ylab("") + xlab("")
	g <- annotPage(p,"schoolboard",
		sprintf("%s: Schools with confirmed COVID-19, by school board",
			provFull[[prov]]))
	pdf(sprintf("%s/%s_schoolboard.pdf",inDir,prov),
		width=9,height=5.5)
	grid.newpage()
	grid.draw(g)
	dev.off()
}

###outFile <- sprintf("%s/schoolboard.pdf",inDir)
###pdf(outFile,width=11,height=5)
###for (g in pList) {
###	grid.newpage()
###	grid.draw(g)
###}
###dev.off()

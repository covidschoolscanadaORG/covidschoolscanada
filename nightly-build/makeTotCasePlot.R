
suppressMessages(require(png))
suppressMessages(require(dplyr))
suppressMessages(require(ggplot2))
suppressMessages(require(gtable))  # annotate page
suppressMessages(require(cowplot)) # ggplot to gtable
suppressMessages(require(grid))	 # annotate page
suppressMessages(require(gridExtra))	 # annotate page
suppressMessages(require(showtext))
suppressMessages(require(scales)) # date axis for cumulative
suppressMessages(require(zoo))
suppressMessages(require(tidyr))
source("utils.R")

# add Google fonts
#font_add_google(name = "Yantramanav", family = "yantramanav")
#font_add_google(name = "Source Sans Pro", family = "source-sans-pro")
#showtext_auto()

require(padr)

getRollAvg <- function(indat,ylim,plotTitle,yvals=NULL) {
lv <- c("AB","BC","MB","NB","NL","NS","ON","PEI","QC","SK","NWT","NU","YT")

blah <- indat[,c("tstamp","Province","Total.cases.to.date")]
agg <- aggregate(blah$Total.cases.to.date, 
	by=list(Province=blah$Province,tstamp=blah$tstamp),
	FUN=sum)
agg2 <- list()

colnames(agg)[3] <- "totcase"
agg$tstamp <- as.Date(agg$tstamp)

# pad blank days
for (k in unique(agg$Province)) {
	tmp <- subset(agg, Province==k)
	tmp <- pad(tmp,interval="day",start_val=min(agg$tstamp),
		end_val=Sys.Date()-1)
	tmp[,1] <- k
	idx <- which(is.na(tmp[,3]))
	if (any(idx)) tmp[idx,3] <- 0 #fill missing dates with 0 cases
	agg2[[k]] <- tmp
}

agg <- do.call("rbind",agg2)

# calculate rolling average 14d
tmp2 <- agg %>%
	group_by(Province) %>%
	mutate(roll_mean = rollmean(totcase,14,align="right",fill=NA)) %>%
	gather(metric, value, totcase:roll_mean)  %>%
	filter(metric == "roll_mean")

p <- tmp2 %>%
	ggplot(aes(tstamp,value,color=Province)) + geom_line()
	p <- p + scale_colour_brewer(palette="Spectral")
	p <- p + coord_cartesian(
			xlim=c(as.Date("2020-09-10"),Sys.Date()+35),
			ylim=ylim)
	p <- p + geom_line(lwd=1.9)
	p <- p + geom_vline(xintercept=as.Date(
		c("2020-09-01","2020-10-01",
			"2020-11-01","2020-12-01",
			"2021-01-01","2021-02-01","2021-03-01","2021-04-01")),linetype=3,
		colour="#ffffff")
	p <- p + geom_hline(yintercept=0,linetype=1,colour="#ffffff")
	p <- p + xlab("")
	p <- p + ylab("")
	p <- p + ggtitle(plotTitle)
	p <- p + annotate("text",x=as.Date("2020-09-15"),
		y=ylim[2]*0.95,
		hjust=0,vjust=0,
		label="y-axis truncated for clarity",colour="#ffffff",size=6,
		fontface=3)

# annotate with case count
cur <- as.data.frame(tmp2)
cur <- subset(cur,tstamp==Sys.Date()-1)
cols <- RColorBrewer::brewer.pal(n=10,"Spectral")
caseText <- c()
cur <- cur[order(cur$Province),]
for (k in lv) {
	if (k %in% cur[,1]){
		curtxt <- sprintf("%s: %s",k,
			prettyNum(round(cur$value[which(cur$Province==k)],1),big.mark=","))
		caseText <- c(caseText, curtxt)
	}
}

yv <- cur$value
if (!is.null(yvals)) {
	for (nm in names(yvals)) {
		yv[which(cur$Province==nm)] <- yvals[[nm]] 
	}
}

	p <- p + annotate("text",x=Sys.Date()+3,
			y=yv,label=caseText,
			colour=cols,size=6,
			fontface=2,
			vjust=0,hjust=0,fill="white")
	
school_th <-  theme(
	panel.background = element_rect(fill="#ffefef"),
	panel.grid.minor = element_blank(), 
	panel.grid.major = element_line(color = "gray90", 
		size = 2,linetype="dashed"),
	panel.grid.major.x = element_blank(),
	#legend.text=element_text(size=24,colour="#550000",
	#		family="source-sans-pro"),
	#legend.key.size=unit(0.8,"cm"),
	#legend.key=element_rect(fill="black"),
	legend.position = "none",
	axis.ticks = element_line(colour = 'gray50'),
	axis.text = element_text(family="source-sans-pro",
		colour="#68382C",size=30),
	axis.title=element_text(size=20),
	plot.title = element_text(family="source-sans-pro",
		hjust = 0.5,size=30,colour="#68382C",face="bold"),
	panel.border = element_blank(),
	plot.margin = unit(c(10,0,0,5),"pt")
)
	p <- p + school_th
	p <- p + theme(
		panel.background = element_rect(fill="#111111"),
		panel.grid.major=element_blank(),
		axis.text.x = element_text(angle = 20,
			size=30,vjust=0.5)
	)

return(p)
}

getRollAvg_Norm <- function(x,...) {
# normalized by pop
lv <- c("AB","BC","MB","NB","NL","NS","ON","PEI","QC","SK","NWT","NU","YT")
provPop <- getSchoolPop()
norm_sorted <- x
for (curProv in lv) {
	idx <- which(norm_sorted$Province == curProv)
	curPop <- provPop[[curProv]]
	sc <- (10^4)/curPop
	if (any(idx)) norm_sorted$Total.cases.to.date[idx] <- norm_sorted$Total.cases.to.date[idx] * sc
}
p2 <- getRollAvg(norm_sorted,
	plotTitle="COVID-19 School Cases (per 10K, 14d roll.avg.)",
	...)

return(p2) #list(plot=p2,data=norm_sorted))
}

###dat <- read.delim("sorted.txt",sep=",",h=T,as.is=T)
###
###yvals <- list(
###	QC=180,
###	PEI=-70,
###	NB=-30,
###	NL=-50,
###	NS=-10
###)
###prollavg <- getRollAvg(sorted,ylim=c(-70,200),
###	plotTitle="COVID-19 School Cases (14d roll.avg.)",
###	yvals=yvals)
###
###yvals <- list(
###	SK=0.3,
###	NS=0.5,
###  ON=0.7,
###	PEI=-0.2,
###	NL=0.2,
###	NB=0,
###	QC=2.5
###)
###prollavg_norm <- getRollAvg_Norm(sorted,ylim=c(-0.3,2.5), yvals=yvals)


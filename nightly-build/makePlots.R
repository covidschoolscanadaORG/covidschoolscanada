# tally graphs
rm(list=ls())
suppressMessages(require(png))
suppressMessages(require(dplyr))
suppressMessages(require(ggplot2))
suppressMessages(require(gtable))  # annotate page
suppressMessages(require(cowplot)) # ggplot to gtable
suppressMessages(require(grid))	 # annotate page
suppressMessages(require(gridExtra))	 # annotate page
suppressMessages(require(showtext))
options(warn=2)
source("utils.R")

# add Google fonts
font_add_google(name = "Yantramanav", family = "yantramanav")
font_add_google(name = "Source Sans Pro", family = "source-sans-pro")
showtext_auto()

provFull <- list(
	BC="British Columbia",
	ON="Ontario",
	AB="Alberta",
	MB="Manitoba",
	SK="Saskatchewan",
	QC="Quebec"
)

dt <- format(Sys.Date(),"%y%m%d")
reportDate <- format(Sys.Date(),"%d %B %Y")
inDir <- sprintf("/home/shraddhapai/Canada_COVID_tracker/export-%s",dt)

logfile <- sprintf("%s/makePlotslog.txt",inDir)
if (file.exists(logfile)) unlink(logfile)
con <- file(logfile)
#sink(con,append=TRUE)
#sink(con,append=TRUE,type="message")
tryCatch({

prov <- c("AB","BC","MB","NB","NL","NS","ON","PEI","QC","SK","NWT","NU","YT")

# ----------------------------
# theme
school_th <-  theme(
	panel.background = element_rect(fill="#ffefef"),
	panel.grid.minor = element_blank(), 
	panel.grid.major = element_line(color = "gray90", 
		size = 2,linetype="dashed"),
	panel.grid.major.x = element_blank(),
	axis.ticks = element_line(colour = 'gray50'),
	axis.text = element_text(family="source-sans-pro",
		colour="#68382C",size=36),
	axis.title.y = element_text(family="source-sans-pro",
		colour="#68382C",size=36),
	axis.title=element_text(size=20),
	plot.title = element_text(family="source-sans-pro",
		hjust = 0.5,size=48,colour="#68382C",face="bold"),
	panel.border = element_blank(),
	plot.margin = unit(c(20,0,0,5),"pt")
)

inFile <- sprintf("%s/CanadaMap_QuebecMerge-%s.clean.csv",
	inDir,dt)
tryCatch({
	dat <- read.delim(inFile,sep=",",h=T,as.is=T)
},error=function(ex){
print(ex)
},finally={
})
message("---------")
message(sprintf("Total cases = %i",nrow(dat)))
message(sprintf("Total outbreaks = %i",
	sum(dat$Total.outbreaks.to.date,na.rm=TRUE)))
dat$Province <- factor(dat$Province, 
	level=prov) 

# plot case by prov
tbl <- table(dat$Province)
print(tbl,useNA="always")
ct <- as.numeric(tbl)
nm <- names(tbl)
df2 <- data.frame(Province=nm,Count=ct)
df2 <- df2[order(df2$Count,decreasing=TRUE),]
print(df2)

total_schools <- sum(df2$Count)


message("*PLOT: Count by Province")
idx <- which(df2$Count < 1)
if (any(idx)) df2 <- df2[-idx,]
df2$Province <- factor(df2$Province)
p <- ggplot(data=df2,aes(x=reorder(Province,-Count),y=Count))
p <- p + geom_bar(stat="identity", fill="#FF6666")
p <- p + geom_text(aes(label=df2$Count),
		position=position_dodge(width=0.9),
		vjust=-0.25,size=20,col="#FF6666")
p <- p + scale_x_discrete(drop=F)
p <- p + xlab("") + ylab("")
p <- p + ylim(0,max(df2$Count)*1.50)
p <- p + school_th
p <- p + theme(axis.ticks.x=element_blank(), 
		axis.text.x=element_text(size=40,
		face="bold",colour="#550000"),
		axis.text.y=element_text(size=48))
p  <- p + ggtitle("Schools with 1+ confirmed case")
p1 <- p

#g <- annotPage(p,"Province",plotTitle=ttl)

message("*PLOT: Outbreaks by Province")
df3 <- aggregate(dat$Total.outbreaks.to.date,
	by=list(Province=dat$Province),
	FUN=sum,na.rm=TRUE)
df3 <- df3[order(df3$x,decreasing=TRUE),]
colnames(df3)[2] <- "Outbreaks"
message("Total outbreaks, by Province")
outbreaks <- df3
total_outbreaks <- sum(df3$Outbreaks)

message("* PLOT: Type of school")
dat2 <- subset(dat, Province!="QC")
print(table(dat2$Type_of_school,useNA="always"))
dat2$Type_of_school <- factor(dat2$Type_of_school,
	levels=schoolLevels())
if (any(is.na(dat2$Type_of_school))) {
	message("converting school to factor gave NA")
	idx <- which(is.na(dat2$Type_of_school))
	browser()
	print(dat2[idx,])
}

p2 <- ggplot(dat2, aes(Province,fill=Type_of_school))
p2 <- p2 + geom_bar(position="dodge")
p2 <- p2 + school_th
p2 <- p2 + theme(axis.text.x=element_text(
					size=42,face="bold",
					color="#550000"),
		axis.text.y=element_text(size=48),
		legend.text=element_text(size=24,colour="#550000",
				family="source-sans-pro"),
		legend.key.size=unit(0.8,"cm"),
		legend.title=element_blank(),
		legend.position = c(0.85,0.7)) # 0,0 -> bottom-left; 1,1 -> top,right
p2 <- p2 + guides(fill=guide_legend(ncol=2))
p2 <- p2 + xlab("") + ylab("")
p2 <- p2 + ggtitle("Type of School")

message("* PLOT: Cumulative cases")
mondays <- getAllMondays(2020)
idx1 <- grep(";",dat2$Date)
idx2 <- grep(";",dat2$Total.cases.to.date)
bad <- setdiff(idx1,idx2)
if (length(bad)>0) {
	print("bad rows")
browser()
}
#dat2$Date[bad] <- sub("; 2020-09-21","",dat2$Date[bad])
dat2 <- dat2[,c("Date","Province","Total.cases.to.date")]
lv <- levels(dat2$Province)
dat2$Province <- as.character(dat2$Province)
dat2 <- flattenCases(dat2)
#dat2 <- na.omit(dat2)
dat2$Province <- factor(dat2$Province, levels=lv)
dat2$Date <- stringr::str_trim(dat2$Date)
dat2$tstamp <- as.POSIXct(dat2$Date)

dat2$Total.cases.to.date <- stringr::str_trim(
		dat2$Total.cases.to.date)
dat2 <- na.omit(dat2)
dat2$Total.cases.to.date <- as.integer(
		dat2$Total.cases.to.date)
cur <- dat2 %>%
	group_by(Province) %>%
	arrange(tstamp) %>% 
	mutate(cs = cumsum(Total.cases.to.date))

cur <- as.data.frame(cur)
cur$Province <- factor(cur$Province)
cur <- aggregate(cur$cs,
	by=list(tstamp=cur$tstamp,Province=cur$Province),
	FUN=max)

cur2 <- cur
p3 <- ggplot(cur2,aes(x=tstamp,y=x,colour=Province))
p3 <- p3 + geom_line(lwd=2)#geom_p#oint() + geom_line()
p3 <- p3 + geom_vline(xintercept=mondays,col="#ff6666",
		linetype="dashed",lwd=2)
p3 <- p3 + xlab("")
p3 <- p3 + ylab("")
p3 <- p3 + ggtitle("Number of cases, cumulative (conservative estimate)")
# annotate
p3 <- p3 + school_th
p3 <- p3 + theme(
	axis.text.x = element_text(angle = 20,size=40,vjust=0.5),
	axis.text.y = element_text(size=48),
	legend.text=element_text(size=36,colour="#550000"),
	legend.title=element_blank(),
	legend.key.size=unit(1.5,"cm"),
	legend.background=element_rect(fill="white"),
	legend.position = c(0.07,0.58)  # 0,0 -> bottom-left; 1,1 -> top,right
)

# image of map + outbreak table
tt3 <- ttheme_minimal(
  core=list(bg_params = list(fill = "#A50000"),
            fg_params=list(fontface=2,fontsize=36,col="white",
				fontfamily="yantramanav",hjust=1,x=0.95),
			padding=unit(c(1.5, 1), "cm")
	)
)
outbreaks[,1] <- as.character(outbreaks[,1])
outbreaks <- outbreaks[which(outbreaks[,2]>0),]
for (k in 1:nrow(outbreaks)) {
	full <- provFull[[outbreaks[k,1]]]
	outbreaks[k,1] <- full
}
ttlout <-  grobTree(
	rectGrob(gp=gpar(fill="#A50000",col="#A50000",lwd=5,
			width=unit(1,"npc"),height=unit(1,"npc"))),
	textGrob("Declared Outbreaks",
		gp = gpar(fontsize =40,col="white",fontface=2,
		fontfamily="source-sans-pro",fill="white"
	))
)
anno2 <-  grobTree(
	rectGrob(gp=gpar(fill="white",col="white",lwd=8)),
	textGrob("View the map. Submit a case.\nmasks4canada.org",
		gp = gpar(fontsize=36,col="#68382C",
		fontfamily="source-sans-pro",fill="white",
		hjust=-5
	))
)

tgrob <- tableGrob(outbreaks,rows=NULL,cols=NULL,theme=tt3)
ttlGrob <- grobTree(ttlout, tgrob)
mapImage 	<- rasterGrob(png::readPNG("../images/map.png"),
	width=unit(1,"npc"), height=unit(1,"npc"))
obImage		<- rasterGrob(png::readPNG("../images/outbreak.png"))
mapPlot <- ggplot() + geom_blank() + 
    annotation_custom(mapImage, 
		xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + 
	annotation_custom(tgrob,
		xmin = 0.68, xmax=0.9, ymin=0.6,ymax=0.95) + 
#	annotation_custom(ttlout,
#		xmin = 0.68, xmax=0.9, ymin=0.9,ymax=0.95) +
	annotation_custom(anno2,
		xmin = 0.01, xmax=0.6, ymin=0.02,ymax=0.2)

		#tableGrob(table,rows=NULL, cols=NULL, theme=tt3))
mapPlot <- mapPlot + theme(
	plot.margin = unit(c(0,0,0,0),"pt")
)

# title at top
top <-  textGrob(
	"CANADA COVID-19 SCHOOL TRACKER",
	gp = gpar(fontsize = 70,col="red",fontface=2,hjust = -1,
	fontfamily="yantramanav",fill="white"
	))
rt <-  textGrob(
	sprintf("%s", prettyNum(total_schools,big.mark=",")),
	gp = gpar(fontsize = 80,col="#68382C",fontface=2,
	fontfamily="yantramanav",fill="white"
	))
rt3 <-  textGrob(
	"SCHOOLS",
	gp = gpar(fontsize = 36,col="#68382C",fontface=2,
	fontfamily="yantramanav",fill="white"
	))
rt2 <-  textGrob(
	sprintf("%s", prettyNum(total_outbreaks,big.mark=",")),
	gp = gpar(fontsize = 80,col="red",fontface=2,
	fontfamily="yantramanav",fill="white"
	))
rt4 <-  textGrob(
	"OUTBREAKS",
	gp = gpar(fontsize = 36,col="red",fontface=2,
	fontfamily="yantramanav",fill="white"
	))

pdfFile <- sprintf("%s/arranged.pdf",inDir)
pdf(pdfFile,width=28,height=16)
tryCatch({
grid.arrange(
  p1,p2,p3,mapPlot,top,rt,rt2,rt3,rt4,
  widths = c(0.07,1, 1, 1, 1, 1, 1, 1,0.07),
  heights = c(0.05,0.5,0.3,2,2,3),
  layout_matrix = rbind(rep(NA,9),
						c(NA, 5, 5, 5, 5, 5, 6, 7,NA),
						c(NA, 5, 5, 5, 5, 5, 8, 9,NA),
						c(NA, 4, 4, 4, 1, 1, 1, 1,NA),
						c(NA, 4, 4, 4, 2, 2, 2, 2,NA),
                        c(NA, 3, 3, 3, 3, 3, 3, 3,NA)),
  	bottom = textGrob(
    	sprintf("@covidschoolsCA | Updated %s",footerDate()),
    	gp = gpar(fontface = 2, fontsize = 40,
			fontfamily="source-sans-pro",col="red"),
    	hjust = 0,vjust=0,
    	x = 0.05 
  )
)
print("done")
},error=function(ex){
	print(ex)
},finally={
	dev.off()
})

},error=function(ex){
},finally={
	sink(NULL)
	sink(NULL)
})

# creates daily am report graphs

rm(list=ls())
suppressMessages(require(png))
suppressMessages(require(dplyr))
suppressMessages(require(ggplot2))
suppressMessages(require(gtable))  # annotate page
suppressMessages(require(cowplot)) # ggplot to gtable
suppressMessages(require(grid))	 # annotate page
suppressMessages(require(gridExtra))	 # annotate page
suppressMessages(require(showtext))
suppressMessages(require(scales)) # date axis for cumulative
options(warn=2)
source("utils.R")
source("genTweets.R")

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

# results to be compiled into tweet
tweetRes <- list()

tweetRes[["date"]] <- Sys.Date()
reportDate <- format(Sys.Date(),"%d %B %Y")
inDir <- sprintf("/home/shraddhapai/Canada_COVID_tracker/export-%s",dt)
pdfFile <- sprintf("%s/arranged.pdf",inDir)
if (file.exists(pdfFile)) unlink(pdfFile)

logfile <- sprintf("%s/makePlotslog.txt",inDir)
if (file.exists(logfile)) unlink(logfile)
con <- file(logfile)
#sink(con,append=TRUE)
#sink(con,append=TRUE,type="message")
tryCatch({
prov <- c("AB","BC","MB","NB","NL","NS","ON","PEI","QC","SK","NWT","NU","YT")

# ----------------------------
# Get school plot
#' @param dat (data.frame) input table
#' @param th (theme)
#' @param addLabels (logical)
getSchoolPlot <- function(dat, th, addLabels=FALSE) {
p2 <- ggplot(dat, aes(group=Province,
	x=Type_of_school))
p2 <- p2 + geom_bar(
	aes(y = ..prop.., fill = factor(..x..)), stat="count")
p2 <- p2 + scale_fill_brewer(palette="Set1",
		labels=levels(dat2$Type_of_school))

if (addLabels) { 
 p2 <- p2 +  geom_text(aes( 
		label = scales::percent(..prop.., accuracy=1),
        y= ..prop.. ), stat= "count", vjust = -.5,
		size=8.6,colour="#550000") 
}
p2 <- p2 + facet_grid(~Province,switch="x")
p2 <- p2 + th
p2 <- p2 + theme(
		axis.text.x=element_blank(),
		axis.ticks.x = element_blank(),
		strip.background=element_rect(fill="#ffffff"),
		strip.text=element_text(size=28,colour="#550000",
			face="bold"),
		legend.text=element_text(size=24,colour="#550000",
				family="source-sans-pro"),
		legend.key.size=unit(0.8,"cm"),
		legend.title=element_blank())
p2 <-p2 + ylim(0,0.75)
p2 <- p2 + scale_y_continuous(
	labels=scales::percent_format(accuracy=1L))
p2 <- p2 + guides(fill=guide_legend())
p2 <- p2 + xlab("") + ylab("")
p2 <- p2 + ggtitle("Affected Schools, by Type (%)")

p2
}

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
		colour="#68382C",size=30),
	axis.title.y = element_text(family="source-sans-pro",
		colour="#68382C",size=36),
	axis.title=element_text(size=20),
	plot.title = element_text(family="source-sans-pro",
		hjust = 0.5,size=40,colour="#68382C",face="bold"),
	panel.border = element_blank(),
	plot.margin = unit(c(10,0,0,5),"pt")
)


# ----------------------------
# Start processing

inFile <- sprintf("%s/CanadaMap_QuebecMerge-%s.clean.csv",
	inDir,dt)
tryCatch({
	dat <- read.delim(inFile,sep=",",h=T,as.is=T)
},error=function(ex){
	print(ex)
},finally={
})

qcStats <- sprintf("%s/CEQ_annotated_clean_%s.csv",inDir,dt)
qcStats <- read.delim(qcStats,sep=",",h=T,as.is=T)

message("---------")
message(sprintf("Total cases = %i",nrow(dat)))
ob <- sum(dat$Total.outbreaks.to.date,na.rm=TRUE)
dat$Province <- factor(dat$Province, 
	level=prov) 
message(sprintf("Total outbreaks = %i", ob))

tweetRes[["total_school"]] <- nrow(dat)
tweetRes[["total_outbreak"]] <- ob

# plot case by prov
tbl <- table(dat$Province)
print(tbl,useNA="always")
ct <- as.numeric(tbl)
nm <- names(tbl)
df2 <- data.frame(Province=nm,Count=ct)
df2 <- df2[order(df2$Count,decreasing=TRUE),]
print(df2)

tweetRes[["num_school"]] <- df2
total_schools <- sum(df2$Count)

message("*PLOT: Count by Province")
idx <- which(df2$Count < 1)
if (any(idx)) df2 <- df2[-idx,]
df2$Province <- factor(df2$Province)
p <- ggplot(data=df2,aes(x=reorder(Province,-Count),y=Count))
p <- p + geom_bar(stat="identity", fill="#FF6666")
p <- p + geom_text(aes(label=df2$Count),
		position=position_dodge(width=0.9),
		vjust=-0.25,size=16,col="#FF6666")
p <- p + scale_x_discrete(drop=F)
p <- p + xlab("") + ylab("")
p <- p + school_th
p <- p + scale_y_continuous(
	labels=scales::number_format(big.mark=","))
p <- p + ylim(0,max(df2$Count)*1.50)
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
df3 <- df3[-which(df3$Province == "BC"),]
colnames(df3)[2] <- "Outbreaks"
# For BC put in number of clusters
numc <- length(intersect(which(dat$Province=="BC"),
	grep("Cluster",dat$Outbreak.Status)))
tweetRes[["total_outbreak"]] <- ob+numc # add clusters to total
tmp <- data.frame(Province="BC",Outbreaks=numc)
df3 <- rbind(df3,tmp)
df3 <- df3[order(df3$Outbreaks,decreasing=TRUE),]

tweetRes[["outbreaks"]] <- df3
message("Total outbreaks, by Province")
outbreaks <- df3
total_outbreaks <- sum(df3$Outbreaks)

message("* PLOT: Type of school")
dat2 <- subset(dat, Province!="QC")
if (!is.null(qcStats)) {
	message("\tAdding annotated QC data")
	dat2 <- rbind(dat2,qcStats)
}
tweetRes[["dat_qcStats"]] <- dat2

dat2$Type_of_school[which(dat2$Type_of_school=="Field Office")] <- "Office"
dat2$Type_of_school[which(dat2$Type_of_school=="Middle School")] <- "Elementary"
dat2$Type_of_school[which(dat2$Type_of_school=="Post-secondary")] <- "PostSec"
print(table(dat2$Type_of_school,useNA="always"))

dat2$Type_of_school <- factor(dat2$Type_of_school,
	levels=schoolLevels())
dat2$Schoolstr <- as.character(dat2$Type_of_school)
if (any(is.na(dat2$Type_of_school))) {
	message("converting school to factor gave NA")
	idx <- which(is.na(dat2$Type_of_school))
	browser()
	print(dat2[idx,])
}
dat2Full <- dat2

p2 <- getSchoolPlot(dat2,school_th,FALSE)
pschlb <- getSchoolPlot(dat2,school_th,TRUE)
pschlb <- pschlb + labs(caption =sprintf("@covidschoolsCA | Updated %s ",
	footerDate()))
pschlb <- pschlb + theme(
		plot.caption = element_text(family="source-sans-pro",size=30,
		face="bold",hjust=0,colour="red")
	)

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
dat2 <- dat2[,c("Date","Province","Total.cases.to.date",
	"institute.name")]
lv <- levels(dat2$Province)
dat2$Province <- as.character(dat2$Province)
dat2 <- flattenCases(dat2)
message("...done flattening")
#dat2 <- na.omit(dat2)
dat2$Province <- factor(dat2$Province, levels=lv)
dat2$Date <- stringr::str_trim(dat2$Date)
tryCatch({
	dat2$tstamp <- as.POSIXct(dat2$Date)
},error=function(ex){
	message("posix conversion of date failed")
	print(ex)
	browser()
},finally={
})

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
cur2$tstamp <- as.Date(cur2$tstamp)
p3 <- ggplot(cur2,aes(x=tstamp,y=x,colour=Province))
p3 <- p3 + geom_line(lwd=1.7)#geom_p#oint() + geom_line()
p3 <- p3 + geom_vline(xintercept=mondays,col="#ff6666",
		linetype="dashed",lwd=2)
p3 <- p3 + xlab("")
p3 <- p3 + ylab("")
p3 <- p3 + ggtitle("Number of cases, cumulative (conservative estimate)")
p3 <- p3 + scale_x_date(date_breaks = "weeks" , date_labels = "%y-%m-%d")
p3 <- p3 + ylim(1,max(cur2$x)*1.02)
p3 <- p3 + scale_y_sqrt(breaks=c(0,25,100,200,500,1000,2000))

# annotate
p3 <- p3 + school_th
p3 <- p3 + theme(
	axis.text.x = element_text(angle = 20,
		size=30,vjust=0.5),
	axis.text.y = element_text(size=48),
	legend.text=element_text(size=30,colour="#550000"),
	legend.title=element_blank(),
	legend.key.size=unit(1.5,"cm"),
	legend.background=element_rect(fill="white"),
	legend.position = c(0.07,0.58)  # 0,0 -> bottom-left; 1,1 -> top,right
)

###cur4 <- aggregate(dat2$Total.cases.to.date,
###	by=list(school=dat2$institute.name,Province=dat2$Province
###		),
###	FUN=sum)
###pobcum <- ggplot(cur4,aes(x=x,group=Province))
###pobcum <- p4 + geom_line(
###	aes(y = ..count..), stat="count")

#### now repeat for outbreaks
###dat2 <- subset(dat2,Province!="QC")
###dat2 <- dat[,c("Province","Date","Total.outbreaks.to.date",
###	"Outbreak.dates","Outbreak.Status")]
###dat2 <- dat2[union(which(dat2$Total.outbreaks.to.date>0),
###		grep("Cluster",dat2$Outbreak.Status)),]
###dat2$Outbreak.dates[which(dat2$Outbreak.dates %in% c(NA,""))] <- NA
###for (k in grep(";",dat2$Date)){
###	x <- unlist(strsplit(dat2$Date[k],";"))
###	x <- stringr::str_trim(x[length(x)])
###	dat2$Outbreak.dates[k] <- x	
###}
###dat2$tstamp <- as.Date(dat2$Outbreak.dates)
###cur3 <- dat2 %>%
###	group_by(Province) %>%
###	arrange(tstamp) %>% 
###	mutate(cs = cumsum(Total.outbreaks.to.date))
###pobcum <- ggplot(cur3,aes(x=tstamp,y=x,colour=Province))
###pobcum <- pobcum + geom_line(lwd=1)#geom_p#oint() + geom_line()
###pobcum <- pobcum + geom_vline(xintercept=mondays,col="#ff6666",
###		linetype="dashed",lwd=2)
###pobcum <- pobcum + xlab("")
###pobcum <- pobcum + ylab("")





message("* putting together grobs")
# image of map + outbreak table
tt3 <- ttheme_minimal(
  core=list(bg_params = list(fill = "#ff0000"),
            fg_params=list(fontface=2,fontsize=30,
				col="white",
				fontfamily="yantramanav",hjust=1,x=0.95),
			padding=unit(c(1.5, 1), "cm")
	)
)

message("* template for outbreak grob")
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
	textGrob("View the map. Submit a case. masks4canada.org",
		gp = gpar(fontsize=30,col="#68382C",
		fontfamily="source-sans-pro",fill="white"
	))
)
message("* outbreak grob")
outbreaks[,1] <- as.character(outbreaks[,1])
outbreaks <- outbreaks[which(outbreaks[,2]>0),]
for (k in 1:nrow(outbreaks)) {
	full <- provFull[[outbreaks[k,1]]]
	outbreaks[k,1] <- full
}
tgrob <- tableGrob(outbreaks,rows=NULL,cols=NULL,theme=tt3)
ttlGrob <- grobTree(ttlout, tgrob)
message("* map grob")
mapImage 	<- rasterGrob(png::readPNG("../images/map.png"))
	#width=unit(1,"npc"), height=unit(1,"npc"))
obImage	<- rasterGrob(png::readPNG("../images/outbreak.png"))
mapPlot <- ggplot() + geom_blank() + 
    annotation_custom(mapImage, 
		xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + 
	annotation_custom(tgrob,
		xmin = 0.68, xmax=0.9, ymin=0.4,ymax=0.8) + 
#	annotation_custom(ttlout,
#		xmin = 0.68, xmax=0.9, ymin=0.9,ymax=0.95) +
	annotation_custom(anno2,
		xmin = 0.4, xmax=0.8, ymin=0,ymax=0.1)

		#tableGrob(table,rows=NULL, cols=NULL, theme=tt3))
mapPlot <- mapPlot + theme(
	plot.margin = unit(c(0,0,0,0),"pt")
)

message("* title grob")
# title at top
top <-  textGrob(
	"CANADA COVID-19 SCHOOL TRACKER",
	gp = gpar(fontsize = 80,col="red",
		fontface=2,hjust = -1,
		fontfamily="yantramanav",fill="white"
	))
rt <-  textGrob(
	sprintf("%s", prettyNum(total_schools,big.mark=",")),
	gp = gpar(fontsize = 72,col="#68382C",fontface=2,
	fontfamily="yantramanav",fill="white"
	))
rt3 <-  textGrob(
	"SCHOOLS",
	gp = gpar(fontsize = 30,col="#68382C",fontface=2,
	fontfamily="yantramanav",fill="white"
	))
rt2 <-  textGrob(
	sprintf("%s", prettyNum(total_outbreaks,big.mark=",")),
	gp = gpar(fontsize = 72,col="red",fontface=2,
	fontfamily="yantramanav",fill="white"
	))
rt4 <-  textGrob(
	"OUTBREAKS/CLUSTERS",
	gp = gpar(fontsize = 30,col="red",fontface=2,
	fontfamily="yantramanav",fill="white"
	))

# -----------------------------------------
# Make PDFs
# -----------------------------------------
# Create Twitter thread
message("Making tweets")
tweetDir <- sprintf("%s/social_media",inDir)
if (!file.exists(tweetDir)) dir.create(tweetDir)
genTweet(tweetDir,tweetRes)

pdf(pdfFile,width=28,height=16)
tryCatch({
	grid.arrange(
	  p1,p2,p3,mapPlot,top,rt,rt2,rt3,rt4,
	  widths = c(0.07,1, 1, 1, 1, 1, 1, 1,0.13),
	  heights = c(0.05,0.5,0.3,2,2,3,0.07),
	  layout_matrix = rbind(rep(NA,9),
							c(NA, 5, 5, 5, 5, 5, 6, 7,NA), #header
							c(NA, 5, 5, 5, 5, 5, 8, 9,NA),
							c(NA, 1, 1, 1, 3, 3, 3, 3,NA),
							c(NA, 2, 2, 2, 3, 3, 3, 3, NA),
							c(NA, 4, 4, 4, 3, 3, 3, 3, NA),
							rep(NA,9)),
###	  layout_matrix = rbind(rep(NA,9),
###							c(NA, 5, 5, 5, 5, 5, 6, 7,NA),
###							c(NA, 5, 5, 5, 5, 5, 8, 9,NA),
###							c(NA, 4, 4, 4, 1, 1, 1, 1,NA),
###							c(NA, 4, 4, 4, 2, 2, 2, 2,NA),
###	                        c(NA, 3, 3, 3, NA, NA, NA, NA,NA)),
	  	bottom = textGrob(
	    	sprintf("@covidschoolsCA | Updated %s",footerDate()),
	    	gp = gpar(fontface = 2, fontsize = 40,
				fontfamily="source-sans-pro",col="red"),
	    	hjust = 0,vjust=0,
	    	x = 0.05 
	  )
	)
	
	# % schools by type	
	pdf(sprintf("%s/schoolPct.pdf",inDir),width=28,height=14)
	print(pschlb)
	dev.off()
	
	system2("convert",args=c("-density","400","-quality","100",
		sprintf("%s/schoolPct.pdf",inDir),
		sprintf("%s/social_media/schoolPct.jpg",inDir)))

	message("* Finished successfully.")
	
},error=function(ex){
	print(ex)
},finally={
	dev.off()
})

},error=function(ex){
},finally={
	message("* Done making plots")
	#sink(NULL)
	#sink(NULL)
})

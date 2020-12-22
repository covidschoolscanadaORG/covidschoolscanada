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
#options(warn=2)
source("utils.R")
source("genTweets.R")

suppressMessages(require(googledrive))

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
#inDir <- sprintf("/home/shraddhapai/Canada_COVID_tracker/export-%s",dt)
inDir <- sprintf("/Users/shraddhapai/Google_covidschools/daily_data/Canada_COVID_tracker/export-%s",dt)
pdfFile <- sprintf("%s/arranged.pdf",inDir)
if (file.exists(pdfFile)) unlink(pdfFile)
pdfinsta <- sprintf("%s/arranged_insta.pdf",inDir)
if (file.exists(pdfinsta)) unlink(pdfinsta)

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
failFile <- sprintf("%s/fail_makePlots.txt",inDir)
if (file.exists(failFile)) unlink(failFile)
tryCatch({
	dat <- read.delim(inFile,sep=",",h=T,as.is=T)
},error=function(ex){
	print(ex)
},finally={
})


qcStats <- sprintf("%s/CEQ_annotated_clean_%s.csv",inDir,dt)
qcStats <- read.delim(qcStats,sep=",",h=T,as.is=T)
idx <- which(is.na(qcStats$institute.name))
if (any(idx)) qcStats <- qcStats[-idx,]

qcStats$Total.cases.to.date <- as.integer(qcStats$Total.cases.to.date)
qcOB <- sum(qcStats$Total.cases.to.date >=5)

dat$Total.outbreaks.to.date <- as.integer(dat$Total.outbreaks.to.date)
tmp <- subset(dat, Province!="QC")

message("---------")
message(sprintf("Total cases = %i",nrow(dat)))
ob <- sum(tmp$Total.outbreaks.to.date,na.rm=TRUE) + qcOB
dat$Province <- factor(dat$Province, 
	level=prov) 
message(sprintf("Total outbreaks = %i", ob))

if (any(is.na(dat$Province))) {
	message("found NA province - check")
	table(dat$Province,useNA="always")
	print(dat[which(is.na(dat$Province)),])
browser()
}

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
		vjust=-0.25,size=12,col="#FF6666")
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
colnames(df3)[2] <- "Outbreaks"

# For BC put in number of clusters
numc <- length(intersect(which(dat$Province=="BC"),
			grep("Cluster",dat$Outbreak.Status,ignore.case=TRUE))
)
tweetRes[["total_outbreak"]] <- ob+numc # add clusters to total
ob_bc <- length(intersect(which(dat$Province=="BC"),
		grep("outbreak",dat$Outbreak.Status,ignore.case=TRUE)))
numc2 <- numc+ob_bc
df3[which(df3$Province=="BC"),"Outbreaks"] <- numc2
df3 <- df3[order(df3$Outbreaks,decreasing=TRUE),]
# For QC num schools with cases >=5 
df3[which(df3$Province=="QC"),"Outbreaks"] <- qcOB

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

dat_qcStats <- dat2
tweetRes[["dat_qcStats"]] <- dat_qcStats

dat2$Type_of_school[which(dat2$Type_of_school=="Field Office")] <- "Office"
dat2$Type_of_school[which(dat2$Type_of_school=="Middle School")] <- "Elementary"
dat2$Type_of_school[which(dat2$Type_of_school=="Post-secondary")] <- "PostSec"
print(table(dat2$Type_of_school,useNA="always"))

###dat2$Type_of_school <- factor(dat2$Type_of_school,
###	levels=schoolLevels())
###dat2$Schoolstr <- as.character(dat2$Type_of_school)
###if (any(is.na(dat2$Type_of_school))) {
###	message("converting school to factor gave NA")
###	idx <- which(is.na(dat2$Type_of_school))
###	print(dat2[idx,])
###	message(sprintf("\tFAIL: School factor conversion: excluded %i",
###		length(idx)))
###	write.table(dat2[idx,],file=failFile,
###		sep="\t",col=T,row=F,quote=F)
###	dat2 <- dat2[-idx,]
###}
dat2Full <- dat2

###p2 <- getSchoolPlot(dat2,school_th,FALSE)
###pschlb <- getSchoolPlot(dat2,school_th,TRUE)
###pschlb <- pschlb + labs(caption =sprintf("@covidschoolsCA | Updated %s ",
###	footerDate()))
###pschlb <- pschlb + theme(
###		plot.caption = element_text(family="source-sans-pro",size=30,
###		face="bold",hjust=0,colour="red")
###	)

message("* PLOT: Cumulative cases")
mondays <- getAllMondays(2020)
idx1 <- grep(";",dat2$Date)
idx2 <- grep(";",dat2$Total.cases.to.date)
bad <- setdiff(idx1,idx2)
if (length(bad)>0) {
	print("bad rows")
	message(sprintf("FAILED: BAD DATES: excluding %i rows",
		length(bad)))
browser()
	write.table(dat2[bad,],file=failFile,sep="\t",
		col=F,row=F,quote=F,append=TRUE)
	dat2 <- dat2[-bad,]
}

#dat2$Date[bad] <- sub("; 2020-09-21","",dat2$Date[bad])
dat2 <- dat2[,c("Date","Province","Total.cases.to.date",
	"institute.name")]

lv <- levels(dat2$Province)
dat2$Province <- as.character(dat2$Province)
dat2 <- flattenCases(dat2)
message("...done flattening")

# distribution of num cases per school
dat2gp <- aggregate(as.integer(dat2$Total.cases.to.date),
	by=list(institute=dat2$institute.name,
			Province=dat2$Province), FUN=sum,na.rm=TRUE)

top <- dat2gp %>% group_by(Province) %>% filter(x==max(x)) %>% arrange(institute, Province,x)
top <- as.data.frame(top)
top <- top[order(top$x,decreasing=TRUE),]
colnames(top) <- c("School","Province","Cases")
topTbl <- grobTree(tableGrob(top,rows=NULL,
	theme=ttheme_default(base_size=8)))

p <- ggplot(dat2gp,aes(x=Province,y=x)) 
p <- p + geom_violin(aes(fill=Province),colour=NA)
p <- p + geom_dotplot(fill="red",colour=NA,
	binaxis='y',stackdir='center',dotsize=0.24,
	stackratio=0.01,alpha=0.8,
	position = position_jitter(0.2))	
p <- p +scale_y_continuous(trans='log2',breaks=c(1,2,4,8,16,32,64))
#p <- p + xlim(-3,length(unique(dat2gp$Province))+2)
p <- p + school_th
p <- p + ggtitle("Number +ve cases per school")
p <- p + ylab("")
p <- p + theme(axis.text.y=element_text(size=30),
	legend.position='none')
p <- p + annotation_custom(topTbl,
	xmin=1,xmax=2.5,ymin=log2(32),ymax=log2(64))

pdf(sprintf("%s/dots.pdf",inDir),width=13,height=5); print(p); dev.off()

###pgp <- ggplot(dat2gp,aes(x,colour=Province)) + stat_ecdf()
###pgp <- pgp + ylim(0.5,1)
###tryCatch({
###pdf(sprintf("%s/ecdf.pdf",inDir));print(pgp); dev.off()
###}, error=function(ex) {
###	print(ex)
###},finally={
###	# ignore errors in this plot
###})

#dat2 <- na.omit(dat2)
idx <- which(is.na(dat2$institute.name))
if (any(idx)) dat2 <- dat2[-idx,]

dat2$Province <- factor(dat2$Province, levels=lv)
dat2$Date <- stringr::str_trim(dat2$Date)
#dat2$Date <- gsub("2020-10-13\\.","2020-10-13",dat2$Date)
#dat2$Date <- gsub("2020-1028","2020-10-28",dat2$Date)
if (length(idx)>0) {
	message("* found NA dates after flattening")
	print(dat2[idx,])
	dat2 <- na.omit(dat2)
}
tryCatch({
	# you were going to put a check here for dates earlier than
	# 2020 or even august 2020
###IsDate <- function(mydate, date.format = "%Y-%m-%d") {
###  tryCatch(!is.na(as.Date(mydate, date.format)),  
###           error = function(err) {FALSE})  
###}
	isd <- IsDate(dat2$Date)
	if (any(isd==FALSE)) {
		message("found malformed date")
		print(dat2[which(!isd),])
	browser()
	#dat2$Date[which(!isd)] <- rep("2020-12-04",sum(!isd))
	}
	dat2$tstamp <- as.POSIXct(dat2$Date)
},error=function(ex){
	message("posix conversion of date failed")
	print(ex)
	browser()
},finally={
})

idx <- which(as.Date(dat2$Date) < as.Date("2020-08-15"))
idx <- c(idx,which(as.Date(dat2$Date) > Sys.Date()))

if (any(idx)) {
	print(dat2[idx,])

}

dat2$Total.cases.to.date <- stringr::str_trim(
		dat2$Total.cases.to.date)
dat2 <- na.omit(dat2)
dat2$Total.cases.to.date <- as.integer(
		dat2$Total.cases.to.date)

totcase <- aggregate(dat2$Total.cases.to.date,
	by=list(Province=dat2$Province),FUN=sum,na.rm=TRUE)
if (any(is.na(totcase$x))){
idx <- which(is.na(dat2$Total.cases.to.date))
idx2 <- which(dat$Province!="QC")
idx <- intersect(idx,idx2)
print(dat2[idx,])
	message("totcase NA")
browser()
	#dat2$Total.cases.to.date[idx] <- 1
#totcase <- aggregate(dat2$Total.cases.to.date,
 # by=list(Province=dat2$Province),FUN=sum)	
}
totcase$Province <- factor(totcase$Province,levels=lv)
tweetRes$totcase <- totcase
mega_totcase <- sum(totcase$x)

cur <- dat2 %>%
	group_by(Province) %>%
	arrange(tstamp) %>% 
	mutate(cs = cumsum(Total.cases.to.date))

cur <- as.data.frame(cur)

cur$Province <- factor(cur$Province,levels=lv)
cur <- aggregate(cur$cs,
	by=list(tstamp=cur$tstamp,Province=cur$Province),
	FUN=max)

cur2 <- cur
cur2$tstamp <- as.Date(cur2$tstamp)
p3 <- ggplot(cur2,aes(x=tstamp,y=x,colour=Province))
p3 <- p3 + geom_line(lwd=1.9)#geom_p#oint() + geom_line()
p3 <- p3 + geom_vline(xintercept=mondays,col="#ff6666",
		linetype="dashed",lwd=2)
p3 <- p3 + xlab("")
p3 <- p3 + ylab("")
p3 <- p3 + ggtitle("Number of cases, cumulative (conservative estimate)")
#p3 <- p3 + scale_x_date(date_breaks = "weeks" , date_labels = "%y-%m-%d")
p3 <- p3 + ylim(-500,max(cur2$x)*1.02)
p3 <- p3 + scale_y_continuous(breaks=c(0,500,1000,2000,3000,4000,5000,6000,7000))

caseText <- c()
for (k in 1:length(lv)) {
	i <- which(totcase$Province==lv[k])
	if (lv[k] == "AB") str <- "*" else str <- ""
	caseText <- c(caseText,sprintf("%s:%s%s", 
		totcase$Province[i],
		prettyNum(totcase$x[i],big.mark=","),str))
}

yvals <- totcase$x
yvals[which(totcase$Province=="PEI")] <- -250
xvals <- rep(Sys.Date()+1, nrow(totcase))
yvals[which(totcase$Province=="NS")] <- -500#Sys.Date()+16
#idx <- which(totcase$Province %in% c("BC","MB","SK"))
#yvals[idx] <- yvals[idx] + 350

cols <- scales::hue_pal()(nrow(totcase))
p3 <- p3 + expand_limits(x=Sys.Date()+20)
p3 <- p3 + annotate("text",x=xvals,
		y=yvals,label=caseText,
		colour=cols,size=11,fontface=2,
		vjust=0,hjust=0,fill="white")
p3 <- p3 + annotate("text",x=as.Date("2020-08-17"),
	y=7400,
	hjust=0,vjust=0,
	label="Linear scale",colour="#68382C",size=12,
	fontface=4)
p3 <- p3 + annotate("text",x=as.Date("2020-08-17"),
	y=7000,
	hjust=0,vjust=0,
	label="* Pending update",colour="#ff0000",size=12,
	fontface=4)

# annotate
p3 <- p3 + school_th
p3 <- p3 + theme(
	axis.text.x = element_text(angle = 20,
		size=20,vjust=0.5),
	axis.text.y = element_text(size=48),
	legend.text=element_text(size=30,colour="#550000"),
	legend.title=element_blank(),
	legend.key.size=unit(1.5,"cm"),
	legend.background=element_rect(fill="white"),
	legend.position = c(0.07,0.58)  # 0,0 -> bottom-left; 1,1 -> top,right
)
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

#### ------------------------------------------------------
#### cumulative outbreaks
###message("* OUTBREAKS")
###dat3 <- dat2Full[,c("Province","Total.outbreaks.to.date",
###	"Outbreak.dates")]
###dat3 <- dat3[which(dat3$Outbreak.dates!=""),]
###dat3 <- flattenCases(dat3,"outbreaks")
###message("... done flattening")
###dat3$Province <- factor(dat3$Province, levels=lv)
###dat3$Outbreak.dates <- stringr::str_trim(dat3$Outbreak.dates)
###tryCatch({
###	dat3$tstamp <- as.POSIXct(dat3$Outbreak.dates)
###},error=function(ex){
###	message("OUTBREAKS: posix conversion of date failed")
###	print(ex)
###	browser()
###},finally={
###})
###dat3$Total.outbreaks.to.date <- stringr::str_trim(
###		dat3$Total.outbreaks.to.date)
###dat3 <- na.omit(dat3)
###dat3$Total.outbreaks.to.date <- as.integer(
###		dat3$Total.outbreaks.to.date)
###cur <- dat3 %>%
###	group_by(Province) %>%
###	arrange(tstamp) %>% 
###	mutate(cs = cumsum(Total.outbreaks.to.date))
###
###cur <- as.data.frame(cur)
###cur$Province <- factor(cur$Province)
###cur <- aggregate(cur$cs,
###	by=list(tstamp=cur$tstamp,Province=cur$Province),
###	FUN=max)
###
###cur3 <- cur
###cur3$tstamp <- as.Date(cur3$tstamp)
###p4 <- ggplot(cur3,aes(x=tstamp,y=x,colour=Province))
###p4 <- p4 + geom_line(lwd=1.7)#geom_p#oint() + geom_line()
###p4 <- p4 + geom_vline(xintercept=mondays,col="#ff6666",
###		linetype="dashed",lwd=2)
###p4 <- p4 + xlab("date")
###p4 <- p4 + ylab("cumulative outbreaks")
###p4 <- p4 + ggtitle("Cumulative outbreaks by Province")
###
###dat3$week <- cut.Date(as.Date(dat3$tstamp),breaks="1 week",
###	labels=FALSE)
###cur4 <- aggregate(dat3$Total.outbreaks.to.date, 
###	by=list(Province=dat3$Province,
###		week=dat3$week),FUN=sum)
###pobwk <- ggplot(cur4,aes(x=week,y=x,colour=Province))
###pobwk <- pobwk + geom_line(lwd=2)
###pobwk <- pobwk + xlab("weeks")+ ylab("# outbreaks declare")
###pobwk <- pobwk + ggtitle("Outbreaks declared per week")
###
###pdf(sprintf("%s/outbreaks.pdf",inDir))
###print(p4); 
###print(pobwk)
###dev.off()

message("* putting together grobs")
# image of map + outbreak table
tt3 <- ttheme_minimal(
  core=list(
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
	rectGrob(gp=gpar(fill="white",col="white",
			width=unit(1,"npc"),height=unit(1,"npc"))),
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
mapImage 	<- rasterGrob(png::readPNG("../images/map2.png"))
	#width=unit(1,"npc"), height=unit(1,"npc"))
obImage	<- rasterGrob(png::readPNG("../images/outbreak.png"))
mapPlot <- ggplot() + geom_blank() + 
    annotation_custom(mapImage, 
		xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + 
	annotation_custom(tgrob,
		xmin = 0.58, xmax=0.8, ymin=0.3,ymax=0.7) + 
#	annotation_custom(ttlout,
#		xmin = 0.68, xmax=0.9, ymin=0.9,ymax=0.95) +
	annotation_custom(anno2,
		xmin = 0.1, xmax=0.9, ymin=0,ymax=0.1)

		#tableGrob(table,rows=NULL, cols=NULL, theme=tt3))
mapPlot <- mapPlot + theme(
	plot.margin = unit(c(0,0,0,0),"pt"),
	panel.background=element_rect(fill="white")
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


pdfSet <- list(
	main=list(pdfFile,28,16)
	##insta=list(pdfinsta,28,28)
)

for (pdfI in 1:length(pdfSet)){
curset <- pdfSet[[pdfI]]

pdf(curset[[1]],width=curset[[2]],height=curset[[3]])
tryCatch({
	suppressWarnings(grid.arrange(
	  p1,p3,mapPlot,top,rt,rt2,rt3,rt4,
	  widths = c(0.03,1, 1, 1, 1, 1, 1, 1,0.13),
	  heights = c(0.07,0.5,0.3,2,2,3,0.07),
	  layout_matrix = rbind(rep(NA,9),
							c(NA, 4, 4, 4, 4, 4, 5, 6,NA), #header
							c(NA, 4, 4, 4, 4, 4, 7, 8,NA),
							c(NA, 1, 1, 1, 2, 2, 2, 2,NA),
							c(NA, 1, 1, 1, 2, 2, 2, 2, NA),
							c(NA, 3, 3, 3, 2, 2, 2, 2, NA),
							rep(NA,9)),
###	  layout_matrix = rbind(rep(NA,9),
###							c(NA, 5, 5, 5, 5, 5, 6, 7,NA),
###							c(NA, 5, 5, 5, 5, 5, 8, 9,NA),
###							c(NA, 4, 4, 4, 1, 1, 1, 1,NA),
###							c(NA, 4, 4, 4, 2, 2, 2, 2,NA),
###	                        c(NA, 3, 3, 3, NA, NA, NA, NA,NA)),
	  	bottom = textGrob(
	    	sprintf("@covidschoolsCA | Updated %s",
				footerDate()),
	    	gp = gpar(fontface = 2, fontsize = 36,
				fontfamily="source-sans-pro",col="red"),
	    	hjust = 1,vjust=-0.5,
	    	x = 0.99 
	  )
	))
	grid.rect(width = 0.99, height = 0.98, 
		gp = gpar(lwd = 11, col = "red", fill = NA))
},error=function(ex){
	print(ex)
},finally={
	dev.off()
})
}
###	# % schools by type	
###	pdf(sprintf("%s/schoolPct.pdf",inDir),width=28,height=14)
###	suppressWarnings(print(pschlb))
###	dev.off()
	
###	system2("convert",args=c("-density","400","-quality","100",
###		sprintf("%s/schoolPct.pdf",inDir),
###		sprintf("%s/social_media/schoolPct.jpg",inDir)))
	message("* Finished successfully.")
	system2("convert",args=c("-density","400","-quality","100",
		sprintf("%s/arranged.pdf",inDir),
		sprintf("%s/social_media/arranged.png",inDir)))

},error=function(ex){
},finally={
	message("* Done making plots")
	#sink(NULL)
	#sink(NULL)
})

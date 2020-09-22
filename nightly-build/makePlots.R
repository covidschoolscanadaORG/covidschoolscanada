# tally graphs
suppressMessages(require(dplyr))
suppressMessages(require(ggplot2))
suppressMessages(require(gtable))  # annotate page
suppressMessages(require(cowplot)) # ggplot to gtable
suppressMessages(require(grid))	 # annotate page
options(warn=2)
source("utils.R")

dt <- format(Sys.Date(),"%y%m%d")
reportDate <- format(Sys.Date(),"%d %B %Y")
inDir <- sprintf("/home/shraddhapai/Canada_COVID_tracker/export-%s",dt)

logfile <- sprintf("%s/makePlotslog.txt",inDir)
con <- file(logfile)
sink(con,append=TRUE)
sink(con,append=TRUE,type="message")
tryCatch({

#prov <- c("AB","BC","MB","NB","NL","NS","ON","PEI","QC","SK","NWT","NU","YT")

# ----------------------------
# theme
school_th <-  theme(
	panel.background = element_blank(),
	panel.grid.minor = element_blank(), 
	panel.grid.major = element_line(color = "gray90", size = 2,
		linetype="dashed"),
	panel.grid.major.x = element_blank(),
	axis.ticks = element_line(colour = 'gray50'),
	axis.text.x = element_text(
		colour="black",size=14),
	axis.text.y = element_text(colour="#68382C", size = 18),
	axis.title=element_text(size=20),
	plot.title = element_text(hjust = 0.5,size=20),
	panel.border = element_blank(),
	plot.margin = unit(c(50,20,40,20),"pt")
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

message("*PLOT: Count by Province")
idx <- which(df2$Count < 1)
if (any(idx)) df2 <- df2[-idx,]
df2$Province <- factor(df2$Province)
p <- ggplot(data=df2,aes(x=reorder(Province,-Count),y=Count))
p <- p + geom_bar(stat="identity", fill="#FF6666")
p <- p + geom_text(aes(label=df2$Count),
		position=position_dodge(width=0.9),
		vjust=-0.25,size=32,col="#FF6666")
p <- p + scale_x_discrete(drop=F)
p <- p + xlab("") + ylab("")
p <- p + ylim(0,max(df2$Count)*1.50)
p <- p + school_th
p <- p + theme(axis.ticks.x=element_blank(), 
		axis.text.x=element_text(size=70,
		face="bold",colour="#550000"),
		axis.text.y=element_text(size=56))
ttl <- "Schools with confirmed COVID-19 cases, by Province"

g <- annotPage(p,"Province",plotTitle=ttl)

message("*PLOT: Outbreaks by Province")
df2 <- aggregate(dat$Total.outbreaks.to.date,
	by=list(Province=dat$Province),
	FUN=sum,na.rm=TRUE)
df2 <- df2[order(df2$x,decreasing=TRUE),]
colnames(df2)[2] <- "Outbreaks"
message("Total outbreaks, by Province")
p4 <- ggplot(data=df2,aes(
	x=reorder(Province,-Outbreaks),y=Outbreaks)
)
p4 <- p4 + geom_bar(stat="identity", fill="#FF6666")
p4 <- p4 + geom_text(aes(label=df2$Outbreaks),
		position=position_dodge(width=0.9),
		vjust=-0.25,size=7,col="#FF6666")
p4 <- p4 + scale_x_discrete(drop=F) 
p4 <- p4+ scale_y_continuous(
	breaks=seq(1,max(df2$Outbreaks)*2,1))
p4 <- p4 + xlab("")
p4 <- p4 + ylab("Number of schools")
p4 <- p4 + school_th
p4 <- p4 + theme(axis.ticks.x=element_blank(), 
		axis.text.x=element_text(size=16,face="bold"))
p4 <- p4 + ylim(0,max(df2$Outbreaks*1.5))
ttl <- "Schools with declared COVID-19 outbreaks, by Province"
g4 <- annotPage(p4,"Province",plotTitle=ttl)

message("* PLOT: Type of school")
dat2 <- subset(dat, Province!="QC")
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
					size=64,face="bold",
					color="#550000"),
		axis.text.y=element_text(size=56))
p2 <- p2 + xlab("") + ylab("")
g2 <- annotPage(p2,"Type of school",ttl)

message("* PLOT: Cumulative cases")
mondays <- getAllMondays(2020)
dat2 <- dat2[,c("Date","Province","Total.cases.to.date")]
lv <- levels(dat2$Province)
dat2$Province <- as.character(dat2$Province)
dat2 <- flattenCases(dat2)
dat2$Province <- factor(dat2$Province, levels=lv)
dat2$tstamp <- as.POSIXct(dat2$Date)

dat2$Total.cases.to.date <- stringr::str_trim(
		dat2$Total.cases.to.date)
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

p3 <- ggplot(cur,aes(x=tstamp,y=x,colour=Province))
p3 <- p3 + geom_line(lwd=5)#geom_p#oint() + geom_line()
p3 <- p3 + geom_vline(xintercept=mondays,col="#ff6666",
		linetype="dashed",lwd=2)
p3 <- p3 + xlab("")
p3 <- p3 + ylab("")
# annotate
p3 <- p3 + school_th
p3 <- p3 + theme(
	axis.text.x = element_text(angle = 20,size=56,
		face="bold",color="#550000"),
	axis.text.y = element_text(size=60),
	legend.text=element_text(size=48,colour="#550000"),
	legend.title=element_blank(),
	legend.key.size=unit(2.5,"cm"),
	legend.background=element_rect(fill="white"),
	legend.position = c(0.07,0.65)  # 0,0 -> bottom-left; 1,1 -> top,right
)
ttl <- "Schools with confirmed COVID-19 cases,cumulative"

message("* Making pdf")

pdfFile <- sprintf("%s/numoutbreaks.pdf",inDir)
pdf(pdfFile,width=8,height=8)
message("\tplot 4")
grid.newpage()
grid.draw(g4)
dev.off()

message("\tcumulative cases")
pdfFile <- sprintf("%s/cumulative.pdf",inDir)
pdf(pdfFile,width=48,height=8)
	print(p3)
dev.off()

message("\tplot 3")
pdfFile <- sprintf("%s/school_type.pdf",inDir)
pdf(pdfFile,width=24,height=8)
tryCatch({
	grid.newpage()
	grid.draw(g2)
},error=function(ex){
	print(ex)
},finally={
	dev.off()
})

},error=function(ex){
},finally={
#	sink(NULL)
#	sink(NULL)
})

# tally graphs

require(dplyr)
require(ggplot2)
require(gtable)  # annotate page
require(cowplot) # ggplot to gtable
require(grid)	 # annotate page
source("utils.R")

dt <- format(Sys.Date(),"%y%m%d")
reportDate <- format(Sys.Date(),"%d %B %Y")
inDir <- sprintf("/home/shraddhapai/Canada_COVID_tracker/export-%s",dt)

#prov <- c("Alberta","British Columbia","Manitoba","New Brunswick","Newfoundland and Labrador","Nova Scotia","Ontario","Prince Edward Island","Québec","Saskatchewan","Northwest Territories","Nunavut","Yukon")
prov <- c("AB","BC","MB","NB","NL","NS","ON","PEI","QC","SK","NWT","NU","YT")

# ----------------------------
# theme
school_th <-  theme(
	panel.background = element_blank(),
	panel.grid.minor = element_blank(), 
	panel.grid.major = element_line(color = "gray90", size = 2,linetype="dashed"),
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

inFile <- sprintf("%s/CanadaMap_QuebecMerge-%s.csv",
	inDir,dt)
dat <- read.delim(inFile,sep=",",h=T,as.is=T)
message("---------")
message(sprintf("Total cases = %i",nrow(dat)))
message(sprintf("Total outbreaks = %i",
	sum(dat$Total.outbreaks.to.date,na.rm=TRUE)))

if(any(dat$Province %in% "Newfoundland and Labrador")) {
		dat$Province[which(dat$Province == "Newfoundland and Labrador")] <- "NL"
}
if(any(dat$Province %in% "Northwest Territories")){ 
		dat$Province[which(dat$Province == "Northwest Territories")] <- "NWT"
}
if(any(dat$Province %in% "Prince Edward Island")){
		dat$Province[which(dat$Province == "Prince Edward Island")] <- "PEI"
}
if(any(dat$Province %in% "Saskatchewan")){
		dat$Province[which(dat$Province == "Saskatchewan")] <- "SK"
}
if(any(dat$Province %in% "New Brunswick")) {
		dat$Province[which(dat$Province == "New Brunswick")] <- "NB"
}
if(any(dat$Province %in% "Québec")) {
		dat$Province[which(dat$Province == "Québec")] <- "QC"
}
if(any(dat$Province %in% "Alberta")) {
		dat$Province[which(dat$Province == "Alberta")] <- "AB"
}
if(any(dat$Province %in% "Ontario")) {
		dat$Province[which(dat$Province == "Ontario")] <- "ON"
}
dat$Province[grep("British Columbia",dat$Province)] <- "BC"

if(any(dat$Province %in% "Manitoba")) {
		dat$Province[which(dat$Province == "Manitoba")] <- "MB"
}
if(any(dat$Province %in% "Nova Scotia")) {
		dat$Province[which(dat$Province == "Nova Scotia")] <- "NS"
}
if(any(dat$Province %in% "Nunavut")) {
		dat$Province[which(dat$Province == "Nova Scotia")] <- "NU"
}
if(any(dat$Province %in% "Yukon")) {
		dat$Province[which(dat$Province == "Nova Scotia")] <- "YT"
}

dat$Province <- factor(dat$Province, 
	level=prov) 

# plot case by prov
tbl <- table(dat$Province)
ct <- as.numeric(tbl)
nm <- names(tbl)
df2 <- data.frame(Province=nm,Count=ct)
df2 <- df2[order(df2$Count,decreasing=TRUE),]

plotList <- list()
grobList <- list()

# counts by Province
idx <- which(df2$Count < 1)
if (any(idx)) df2 <- df2[-idx,]
df2$Province <- factor(df2$Province)
p <- ggplot(data=df2,aes(x=reorder(Province,-Count),y=Count))
p <- p + geom_bar(stat="identity", fill="#FF6666")
p <- p + geom_text(aes(label=df2$Count),position=position_dodge(width=0.9),
		vjust=-0.25,size=24,col="#FF6666")
p <- p + scale_x_discrete(drop=F)
p <- p + xlab("") + ylab("")
#p <- p + ylab("Number of schools")
p <- p + ylim(0,max(df2$Count)*1.15)
p <- p + school_th
p <- p + theme(axis.ticks.x=element_blank(), 
		axis.text.x=element_text(size=52,face="bold"),
		axis.text.y=element_text(size=48))
ttl <- "Schools with confirmed COVID-19 cases, by Province"
g <- annotPage(p,"Province",plotTitle=ttl)

# outbreaks by Province
df2 <- aggregate(dat$Total.outbreaks.to.date,by=list(Province=dat$Province),
	FUN=sum,na.rm=TRUE)
df2 <- df2[order(df2$x,decreasing=TRUE),]
colnames(df2)[2] <- "Outbreaks"
p4 <- ggplot(data=df2,aes(x=reorder(Province,-Outbreaks),y=Outbreaks))
p4 <- p4 + geom_bar(stat="identity", fill="#FF6666")
p4 <- p4 + geom_text(aes(label=df2$Outbreaks),position=position_dodge(width=0.9),
		vjust=-0.25,size=7,col="#FF6666")
p4 <- p4 + scale_x_discrete(drop=F) + scale_y_continuous(breaks=seq(1,max(df2$Outbreaks)*2,1))
p4 <- p4 + xlab("")
p4 <- p4 + ylab("Number of schools")
p4 <- p4 + school_th
p4 <- p4 + theme(axis.ticks.x=element_blank(), 
		axis.text.x=element_text(size=16,face="bold"))
p4 <- p4 + ylim(0,max(df2$Outbreaks*1.5))
ttl <- "Schools with declared COVID-19 outbreaks, by Province"
g4 <- annotPage(p4,"Province",plotTitle=ttl)

# type of school
dat2 <- subset(dat, Province!="QC")
idx <- which(dat2$Type_of_school=="")
if (any(idx)) dat2$Type_of_school[idx] <- "?"

idx <- grep(";",dat2$Type_of_school)
if (length(idx)>0) {
	dat2$Type_of_school[idx] <- gsub(" *; *",";",dat2$Type_of_school[idx],
			perl=TRUE)
	dat2$Type_of_school[idx] <- gsub(" School","",dat2$Type_of_school[idx])
	dat2$Type_of_school[idx] <- gsub(";"," & ",dat2$Type_of_school[idx])
}
idx <- which(dat2$Type_of_school=="Secondary")
if (any(idx)) {
	dat2$Type_of_school[idx] <- "High School"
}

message("* Type of school")
dat2$Type_of_school[grep("&",dat2$Type_of_school)] <- "Mixed"
dat2$Type_of_school <- factor(dat2$Type_of_school,levels=schoolLevels())
if (any(is.na(dat2$Type_of_school))) {
	message("converting school to factor gave NA")
	idx <- which(is.na(dat2$Type_of_school))
	browser()
	print(dat2[idx,])
}

p2 <- ggplot(dat2, aes(Province,fill=Type_of_school))
p2 <- p2 + geom_bar(position="dodge")
p2 <- p2 + guides(fill=guide_legend(title="Type of School"))
ttl <- "Schools with confirmed COVID-19 cases, Type of school & Province"
p2 <- p2 + school_th
p2 <- p2 + theme(axis.text.x=element_text(size=30,face="bold"),
				axis.text.y=element_text(size=50))
p2 <- p2 + xlab("") + ylab("")
g2 <- annotPage(p2,"Type of school",ttl)

# staff/students/other
message("* Staff/students")

# trend with time
mondays <- getAllMondays(2020)
dat2 <- dat2[,c("Date","Province","Total.cases.to.date")]
lv <- levels(dat2$Province)
dat2$Province <- as.character(dat2$Province)
idx <- grep(";", dat2$Total.cases.to.date)
simple <- setdiff(1:nrow(dat2),idx)
simple <- dat2[simple,]
multi <- dat2[idx,]
for (i in 1:length(multi)) {
	x <- as.integer(trimws(unlist(strsplit(multi$Total.cases.to.date[i],";"))))
	y <- trimws(unlist(strsplit(multi$Date[i],";")))
	for (j in 1:length(x)) {
		simple <- rbind(simple, c(y[j],multi$Province[i],x[j]))
	}
}
dat2 <- simple
dat2$Province <- factor(dat2$Province, levels=lv)
dat2$tstamp <- as.POSIXct(dat2$Date)
dat2$Total.cases.to.date <- as.integer(dat2$Total.cases.to.date)
cur <- dat2 %>% group_by(Province) %>% arrange(tstamp) %>% mutate(cs = cumsum(Total.cases.to.date))
cur <- as.data.frame(cur)
cur$Province <- factor(cur$Province)
cur <- aggregate(cur$cs,by=list(tstamp=cur$tstamp,Province=cur$Province),FUN=max)
p3 <- ggplot(cur,aes(x=tstamp,y=x,colour=Province))
p3 <- p3 + geom_line(lwd=5)#geom_p#oint() + geom_line()
p3 <- p3 + geom_vline(xintercept=mondays,col="#ff6666",linetype="dashed",
		lwd=2)
p3 <- p3 + xlab("")
p3 <- p3 + ylab("")
# annotate
#p3 <- p3 + annotate(geom="text", x=as.Date("2020-08-20"),
#		y=38,label="Mondays",colour="#ff6666")
p3 <- p3 + school_th
p3 <- p3 + theme(
	axis.text.x = element_text(angle = 20,size=56,face="bold"),
	axis.text.y = element_text(size=60),
	legend.text=element_text(size=48,colour="#550000"),
	legend.title=element_blank(),
	legend.key.size=unit(2.5,"cm"),
	legend.background=element_rect(fill="white"),
	legend.position = c(0.07,0.65)  # 0,0 -> bottom-left; 1,1 -> top,right
)
ttl <- "Schools with confirmed COVID-19 cases,cumulative"

message("* Making pdf")
message("\tplot 1")
pdfFile <- sprintf("%s/num-schools-by-prov.pdf",inDir)
pdf(pdfFile,width=32,height=8)
grid.newpage()
grid.draw(g)
dev.off()

pdfFile <- sprintf("%s/numoutbreaks.pdf",inDir)
pdf(pdfFile,width=8,height=8)
message("\tplot 4")
grid.newpage()
grid.draw(g4)
dev.off()

message("\tcumulative cases")
pdfFile <- sprintf("%s/cumulative.pdf",inDir)
pdf(pdfFile,width=40,height=8)
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



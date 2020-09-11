# tally graphs

require(ggplot2)
source("utils.R")

dt <- format(Sys.Date(),"%y%m%d")
reportDate <- format(Sys.Date(),"%d %B %Y")
inDir <- sprintf("/home/shraddhapai/Canada_COVID_tracker/export-%s",dt)

prov <- c("Alberta","British Columbia","Manitoba","New Brunswick","Newfoundland and Labrador","Nova Scotia","Ontario","Prince Edward Island","Québec","Saskatchewan","Northwest Territories","Nunavut","Yukon")

# ----------------------------
# theme
school_th <-  theme(axis.text = element_text(size=16),
			axis.text.x = element_text(
				angle = 90, vjust = 0.5, hjust=1),
			axis.title=element_text(size=18),
			plot.title = element_text(hjust = 0.5,size=20))

inFile <- sprintf("%s/CanadaMap_QuebecMerge-%s.csv",
	inDir,dt)
dat <- read.delim(inFile,sep=",",h=T,as.is=T)
dat$Province <- factor(dat$Province, 
	level=prov) 

# plot case by prov
tbl <- table(dat$Province)
ct <- as.numeric(tbl)
nm <- names(tbl)
df2 <- data.frame(Province=nm,Count=ct)
df2 <- df2[order(df2$Count,decreasing=TRUE),]

# counts by Province
p <- ggplot(data=df2,aes(x=reorder(Province,-Count),y=Count))
p <- p + geom_bar(stat="identity", fill="#FF6666")
p <- p + geom_text(aes(label=df2$Count),position=position_dodge(width=0.9),
		vjust=-0.25,size=7,col="#FF6666")
p <- p + scale_x_discrete(drop=F)
p <- p + xlab("")
p <- p + ylab("Number of schools")
p <- p + ylim(0,max(df2$Count)*1.3)
p <- p + ggtitle(sprintf("Schools with confirmed COVID-cases\n%s",
		reportDate))
p <- p + school_th

# type of school
dat2 <- subset(dat, Province!="Québec")
idx <- which(dat2$Type_of_school=="")
if (any(idx)) dat2$Type_of_school[idx] <- "?"

dat2$Type_of_school <- factor(dat2$Type_of_school)
p2 <- ggplot(dat2, aes(Province,fill=Type_of_school))
p2 <- p2 + geom_bar(position="dodge")
p2 <- p2 + ggtitle(sprintf("Confirmed COVID-19 School Reports, by type of school\n%s",
		reportDate))
p2 <- p2 + school_th

# staff/students/other

# type of school
dat2 <- subset(dat, Province!="Québec")
idx <- which(dat2$Type_of_school=="")
if (any(idx)) dat2$Type_of_school[idx] <- "?"

dat2$Type_of_school <- factor(dat2$Type_of_school)
p2 <- ggplot(dat2, aes(Province,fill=Type_of_school))
p2 <- p2 + geom_bar(position="dodge")
p2 <- p2 + ggtitle(sprintf("Confirmed COVID-19 School Reports, by type of school\n%s",
		reportDate))
p2 <- p2 + school_th

# trend with time
mondays <- getAllMondays(2020)
dat2 <- subset(dat, Province=="Alberta")
dat2$tstamp <- as.POSIXct(dat2$Date)
cur <- aggregate(dat2$Total.cases.to.date,
	by=list(x=dat2$tstamp),FUN=sum)
colnames(cur) <- c("Date","reports") 
p3 <- ggplot(cur,aes(x=Date,y=cumsum(reports))) + geom_point() + geom_line()
p3 <- p3 + ylab("# reports (cumulative)")
p3 <- p3 + geom_hline(yintercept=seq(10,40,10),col="#555555",linetype="dashed")
p3 <- p3 + geom_vline(xintercept=mondays,col="#6666ff")
p3 <- p3 + ggtitle(sprintf("Confirmed COVID-19 School Reports,cumulative: Alberta\n%s",reportDate))
p3 <- p3 + school_th

pdfFile <- sprintf("%s/numschools_tally.pdf",inDir)
pdf(pdfFile,width=11,height=6)
print(p)
print(p2)
print(p3)
dev.off()

# tally graphs

require(ggplot2)


dt <- format(Sys.Date(),"%y%m%d")
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

tbl <- table(dat$Province)
ct <- as.numeric(tbl)
nm <- names(tbl)
df2 <- data.frame(Province=nm,Count=ct)
df2 <- df2[order(df2$Count,decreasing=TRUE),]

p <- ggplot(df2,aes(x=reorder(Province,-Count),y=Count))
p <- p + geom_bar(stat="identity", fill="#FF6666")
p <- p + scale_x_discrete(drop=F)
p <- p + xlab("")
p <- p + ylab("Number of schools")
p <- p + ggtitle("Schools with confirmed COVID-cases")
p <- p + school_th

pdfFile <- sprintf("%s/numschools_tally.pdf",inDir)
print(p)


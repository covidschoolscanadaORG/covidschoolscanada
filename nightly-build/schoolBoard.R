
suppressMessages(require(dplyr))
suppressMessages(require(ggplot2))

school_th <-  theme(
	panel.background = element_blank(),
	panel.grid.minor = element_blank(), 
	panel.grid.major = element_line(color = "gray90", size = 1,
		linetype="dashed"),
	panel.grid.major.x = element_blank(),
	axis.ticks = element_line(colour = 'gray50'),
	axis.text.x = element_text(angle=90,
		colour="black",size=14,hjust=0.95,vjust=0.2),
	axis.text.y = element_text(colour="#68382C", size = 18),
	axis.title=element_text(size=20),
	plot.title = element_text(hjust = 0.5,size=20),
	panel.border = element_blank()
#	plot.margin = unit(c(50,20,40,20),"pt")
)

dt <- format(Sys.Date(),"%y%m%d")
inDir <- sprintf("/home/shraddhapai/Canada_COVID_tracker/export-%s",dt)
inFile <- sprintf("%s/CanadaMap_QuebecMerge-%s.csv.cleanProv.txt",
	inDir,dt)
dat <- read.delim(inFile,sep=",",h=T,as.is=T)
dat$ct <- 1

idx <- which(dat$Province == "QC")
dat <- dat[-idx,]
dat$School.board <- stringr::str_trim(dat$School.board)
dat$School.board <- sub("Catholic DSB", "CDSB",dat$School.board)
dat$School.board <- sub("Catholic SD", "CSD",dat$School.board)
dat$School.board <- sub("School Division", "SD",dat$School.board)
dat$School.board <- sub("School District", "SD",dat$School.board)
dat$School.board <- sub("Conseil scolaire", "CS",dat$School.board)
dat$School.board <- sub("Board of Education", "BofEd",dat$School.board)
dat$School.board <- sub(" PS$", " PSD",dat$School.board)
dat$School.board <- sub(" PSB$", " PSD",dat$School.board)
dat$School.board <- sub("CÉP de l'Est de l'Ontario", "CEPEO",
	dat$School.board)
dat$School.board <- sub("Public Schools", "PS",
	dat$School.board)
dat$School.board[grep("CECCE",dat$School.board)] <- "CECCE"
dat$School.board <- sub("CSDC du Centre-Est de l'Ontario",
	"CSDCCEO",
	dat$School.board)
dat$School.board <- sub("CSDC du Nouvel-Ontario",
	"CSDCNO",
	dat$School.board)
dat$School.board <- sub("CS Viamond","CS Viamonde",
	dat$School.board)
dat$School.board <- sub("CS Viamondee","CS Viamonde",
	dat$School.board)
dat$School.board <- sub("Waterloo Region DSB","WRDSB",
	dat$School.board)
dat$School.board <- sub("WRDSB","Waterloo Reg.",
	dat$School.board)
dat$School.board <- sub("Independent","Indep.",
	dat$School.board)
dat$School.board <- sub("DSB","",
	dat$School.board)
dat$School.board <- sub("Durham-Peel","Dufferin-Peel",
	dat$School.board)
dat$School.board <- stringr::str_trim(dat$School.board)

idx <- which(dat$School.board=="")
if (any(idx)) dat$School.board[idx] <- "TBA"#"other/uncurated"

df2 <- aggregate(dat$ct, 
	by=list(Province=dat$Province,Board=dat$School.board),
	FUN=sum)

df2$Province <- factor(df2$Province)
colnames(df2)[3] <- "ct"

df2$Board <- factor(df2$Board)
pList <- list()
for (prov in unique(df2$Province)) {
	message("------------------------")
	message(prov)
	message("")
	df3 <- subset(df2,Province==prov)
	message("")
	p <- ggplot(data = df3, aes(x=reorder(Board,-ct),y=ct))
	p <- p + geom_bar(stat="identity")
	p <- p + ggtitle(prov)
	p <- p + school_th
	p <- p + ylim(0,max(df3$ct)*1.05) + ylab("") + xlab("")
	pList[[prov]] <- p
}

outFile <- sprintf("%s/schoolboard.pdf",inDir)
pdf(outFile,width=11,height=5)
for (p in pList) {
	print(p)
}
dev.off()

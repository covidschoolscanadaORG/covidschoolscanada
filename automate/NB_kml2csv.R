require(XML)
require(fuzzyjoin)
require(dplyr)
# downloaded from http://www.snb.ca/geonb1/e/DC/catalogue-E.asp and unzipped kmz
inFile <- "/Users/shraddhapai/Google_covidschools/SchoolInfo/NB/geonb_nbps-epnb_kml/doc.kml"

tagsXML <- xmlParse(inFile)
tagsList <- xmlToList(tagsXML)
locs <- tagsList$Document$Folder
locs <- locs[-(1:2)]
locs <- locs[-296] # hack for NB specific file

schools <- list()
for (k in 1:length(locs)) {
	x <- locs[[k]]$Point$coordinates
	x <- unlist(strsplit(x,","))
	long <- as.numeric(x[1])
	lat <- as.numeric(x[2])
print(k)
	schools[[k]] <- cbind(locs[[k]]$name, lat, long)
}

schools <- do.call("rbind",schools)
schools <- as.data.frame(schools)
colnames(schools) <- c("Schools","Latitude","Longitude")
schools[,1] <- stringi::stri_encode(schools[,1],"","UTF-8")
schools_t <- as_tibble(schools)

datFile <- "/Users/shraddhapai/Google_covidschools/SchoolInfo/NB/All_NB_schools_201226.csv"
dat <- read.delim(datFile,sep=",",h=T,as.is=T)
dat <- dat[-which(is.na(dat$School.Number)),]
dat <- dat[,1:3]
dat$Schools <- stringi::stri_encode(dat$Schools,"","UTF-8")
dat_t <- as_tibble(dat)

good <- stringdist_join(x=schools_t,y=dat_t,
	by=c(Schools="Schools"),max_dist=2)
the_rest <- schools_t[-which(schools_t$Schools %in% good$Schools.x),]

write.table(good,file="nb_match.txt",sep="\t",col=T,row=F,quote=T)
write.table(the_rest,file="nb_unmatch.txt",sep="\t",col=T,row=F,quote=T)




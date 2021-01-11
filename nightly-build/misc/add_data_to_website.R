# compile bullet list of links to KML files and CSV to add to covidschoolsca
# website

require(googledrive)

gDir <- "/Users/shraddhapai/Google_covidschools/daily_data/Canada_COVID_tracker"
repoFile <- "/Users/shraddhapai/software/covidschoolscanada.github.io/data.md"
presaveFile <- "/Users/shraddhapai/software/covidschoolscanada.github.io/filelist.txt"
presaveNew <- "/Users/shraddhapai/software/covidschoolscanada.github.io/filelist_updated.txt"


tryCatch({
# -----------------------
# insert front matter
# -----------------------
con <- file(repoFile,"w")					# content for webpage with links
cat("---\n",file=con)
cat("layout: page\n",file=con)
cat("title: Get the Data\n",file=con)
cat("permalink: /data/\n",file=con)
cat("---\n\n\n",file=con)

if (file.exists(presaveNew)) unlink(presaveNew)

if (file.exists(presaveFile)) {
	file.copy(from=presaveFile,to=presaveNew) # copy existing records
	# old listings - speeds up process
	dat <- read.delim(presaveFile,sep="\t",h=T,as.is=T)
} else {
	dat <- NULL
}

buf <- file(presaveNew,"a")		# and add new ones
dt <- seq(as.Date("2020-09-23"),Sys.Date(),by="days")

for (i in rev(1:length(dt))) {
	cur <- dt[i]
	yymmdd <- format(cur, "%y%m%d")
	message(yymmdd)

	if (yymmdd %in% dat$date) {
			message("\tfound - using")
			idx <- which(dat$Date == yymmdd)
			tbl <- dat$csv[idx]
			rawkml <- dat$kml_raw[idx]
			cleankml <- dat$kml_clean[idx]

		# add line to webpage
		t1 <- format(cur,"%a %b %d %Y")
		cat(sprintf("* %s: [**[CSV](%s)**] [**[KML, RAW](%s)**] [**[KML, CLEAN](%s)**]\n",
			t1,tbl,rawkml, cleankml),
			file=con)
	} else {
		x <- NULL
		tryCatch({
			x <- drive_ls(sprintf("export-%s",yymmdd))
		},error=function(ex){ 
			print(ex)
		},finally={
		})

		if (is.null(x)) {
			message("\tcouldn't find record")
		} else {
			message("\tadding new record")
			# for each page, first add share permission, then get link
			tmp <- sprintf("CanadaMap_QuebecMerge-%s.clean.csv",yymmdd)
			tmp <- x[which(x$name==tmp),]
			tmp <- tmp %>% drive_share(role="reader",type="anyone")
			tbl <- drive_link(tmp)
	
			tmp <- x[which(x$name=="CanadaMap.kml"),]
			tmp <- tmp %>% drive_share(role="reader",type="anyone")
			rawkml <- drive_link(tmp)
	
			tmp <- x[which(x$name=="CanadaMap_clean3.kml"),]
			tmp <- tmp %>% drive_share(role="reader",type="anyone")
			cleankml <- drive_link(tmp)
	
			message("\tadd new record")
			cat(sprintf("%s\t%s\t%s\t%s\n",yymmdd,tbl,rawkml,cleankml),
				file=buf)

		# add line to webpage
		t1 <- format(cur,"%a %b %d %Y")
		cat(sprintf("* %s: [**[CSV](%s)**] [**[KML, RAW](%s)**] [**[KML, CLEAN](%s)**]\n",
			t1,tbl,rawkml, cleankml),
			file=con)
		}
	}

}
close(buf)

message("Updating master listings file")
if (file.exists(presaveFile)) unlink(presaveFile) 
file.copy(from=presaveNew,to=presaveFile)
unlink(presaveNew)

message("Done updating links")

}, error=function(ex){
	print(ex)
}, finally={
	message("* Closing file")
	close(con)
})




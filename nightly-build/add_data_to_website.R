# compile bullet list of links to KML files and 

require(googledrive)

gDir <- "/Users/shraddhapai/Google_covidschools/daily_data/Canada_COVID_tracker"
repoFile <- "/Users/shraddhapai/software/covidschoolscanada.github.io/data.md"
presaveFile <- "/Users/shraddhapai/software/covidschoolscanada.github.io/filelist.txt"

con <- file(repoFile,"w")
buf <- file(presaveFile,"w")

tryCatch({
# -----------------------
# insert front matter
# -----------------------
cat("---\n",file=con)
cat("layout: page\n",file=con)
cat("title: Get the Data\n",file=con)
cat("permalink: /data/\n",file=con)
cat("---\n\n\n",file=con)

# Data
# change permissions to share-link
message("* Making shareable (kml) ...")
#x <- drive_find("CanadaMap_clean3.kml$")
#x <- x %>% drive_share(role="reader",type="anyone")

dt <- seq(as.Date("2020-10-01"),Sys.Date(),by="days")
for (i in 1:3) {#length(dt)) {
	cur <- dt[i]
	yymmdd <- format(cur, "%y%m%d")
	message(yymmdd)
	tmp <- sprintf("CanadaMap_QuebecMerge-%s.clean.csv",yymmdd)
	x <- drive_ls(sprintf("export-%s",yymmdd))
	tbl <- drive_link(x[which(x$name==tmp),])
	rawkml <- drive_link(x[which(x$name=="CanadaMap.kml"),])
	cleankml <- drive_link(x[which(x$name=="CanadaMap_clean3.kml"),])

	t1 <- format(cur,"%a %b %d %Y")
	cat(sprintf("* %s: [**[CSV](%s)**] [**[KML, RAW](%s)**] [**[KML, CLEAN](%s)**]\n",
		t1,tbl,rawkml, cleankml),
		file=con)

}
}, error=function(ex){
	print(ex)
}, finally={
	message("* Closing file")
	close(con)
})




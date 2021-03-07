# removes bad links
require(gsheet)
require(rvest)
require(xml2)
require(stringr)


# uses casperjs headless browser
# https://www.r-bloggers.com/2019/11/when-rvest-is-not-enough/
flagMissingPDF <- function(myURL,jsTemp) {
	# update casper template to use myURL as the input URL
	js <- str_interp(jsTemp, env=list(url=myURL))
	cat(js,file='tmp_script.js')

	cmd <- 'casperjs tmp_script.js'
	html <- system(cmd, intern = TRUE)
	html <- paste(html, collapse = '\n')
	html <- xml2::read_html(html)
	links <- html %>% html_nodes(xpath="//*[@class='pdfemb-errormsg']") %>% html_text()	
	return(links)
}

removeBadLinks <- function(dat,outFile) {
	bad_links <- rep(NA, nrow(dat))
	bad_count <-rep(0,nrow(dat))

	# read js template once
 	f <- file('fetchTemplate.js')
  jsMaster <- readLines(f)
  jsMaster <- paste0(jsMaster,collapse="\n")
	close(f)

	for (k in 1:nrow(dat)) { # nrow(dat)) {
		art <- unlist(strsplit(dat$Article[k],";"))
		isgood <- unlist(lapply(art,function(x) {
			x <- trimws(x)

			# only run pdf flagger if M4C link
			if (any(grep("masks4canada.org",x))) {
		 		pdfs <- flagMissingPDF(x,jsMaster)
				pdfs <- pdfs[grep("Missing PDF",pdfs)]
				if (url.exists(x) && length(pdfs)<1) return(TRUE)
				else return(FALSE)
			} else {
				pdfs <- TRUE
			}
		}
		))
		bad_links[k] <- paste(art[!isgood],collapse=";")
		bad_count[k] <- sum(!isgood)
		if (sum(!isgood)>0) art <- art[-which(!isgood)]
		art <- paste(art,collapse=";")
		message(sprintf("%s: %i bad",k,sum(!isgood)))
		dat$Article[k] <- art
	}
	dat$broken_links <- bad_links
	dat$broken_link_count <- bad_count
	write.table(dat,file=outFile,sep=",",col=T,row=F,quote=T)
}

# --------------------

provs <- list(
	#MB="https://docs.google.com/spreadsheets/d/1a1Rzn7tDVrTc976UAyHFk-WcSz9RPumRQELVd6lnac8/edit#gid=20331003",
	#SK="https://docs.google.com/spreadsheets/d/10Y2N2wq0vzW6BAB3d0BQgAdpZZrpZlU7xKQhKfNeuBE/edit?usp=sharing"
	ON="https://docs.google.com/spreadsheets/d/1U-HFYvLch6PkOsITpI2wA7_MIsT1ocR6tv9XVr5jfPw/edit?usp=sharing"
	
)

for (nm in names(provs)) {
	message(nm)
	message("Fetching sheet")
	x <- gsheet2tbl(provs[[nm]])
	y <- as.data.frame(x)
	outFile <- sprintf("%s_cleaned.csv",nm)
	message("running link cleaner")
	t0 <- Sys.time()
	bad_count <- removeBadLinks(y,outFile)
	message("finished running link cleaner")
	print(Sys.time()-t0)
}


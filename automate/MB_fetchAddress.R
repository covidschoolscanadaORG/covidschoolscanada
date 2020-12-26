# parse MB school website to get addresses
rm(list=ls())
require(httr)
require(rvest)

inDir <- "/Users/shraddhapai/Google_covidschools/SchoolInfo/MB"
dat <- read.delim(sprintf("%s/MB_schools_raw.txt",inDir),
	sep="\t",h=T,as.is=T)
Encoding(dat[,2]) <- "latin1"

urlBase <- "https://web36.gov.mb.ca/school/school?action=singleschool&name="
message("fetching html")
out <- list()
outFile <- sprintf("%s/MB_addresses_processed.txt",inDir)
if (file.exists(outFile)) unlink(outFile)
for (k in 1:nrow(dat)) {
	num <- dat[k,1]
	url <- sprintf("%s%i",urlBase,num)
	message(url)
	
	x <- httr::GET(url)
	txt <- content(x,"text")
	cat(txt,file="output.txt")
	html <- read_html("output.txt")
	
	# get school address
	school <- html %>% html_nodes("table") %>% html_nodes(xpath = '//*[@id="school"]') %>% html_nodes(xpath='//*[@class="sc_address"]')
	school <- as.character(school)
	school <- sub("<div class=\\\"sc_address\\\">\\n<div class=\\\"sc_name\\\">","",school) 
	school <- sub("<div class=\\\"sc_div\\\">","",school) 
	school <- gsub("\n","\t",school)
	school <- gsub("</div>","",school)
	school <- gsub("<div>","",school)
	school <- gsub("Â","",school)
	school <- gsub(",<strong>","\t", school)
	school <- gsub("<strong>","\t", school)
	school <- gsub("</strong>","", school)
	school <- gsub("<br>",",", school)
	school <- gsub(",\t","\t", school)
	school <- sub("\t$","\n", school)
	print(school)
	schools <- stringr::str_trim(unlist(strsplit(school,"\t")))
	hpos <- regexpr("#",dat[k,2])
	schools[1] <- substr(dat[k,2],1,hpos-2)
	schools <- c(schools,substr(dat[k,2],hpos+1,nchar(dat[k,2])))
	cat(school,file=outFile,append=TRUE)
	out[[k]] <- schools
}

out2 <- do.call("rbind",out)

colnames(out2) <- c("School.Name","School.Address","Phone","Fax",
	"Grade","Program",
	"School.Division","School.Code")
write.table(out2,file=sprintf("%s/MB_addresses_parsed.txt",inDir),
	sep="\t",col=T,row=F,quote=T)

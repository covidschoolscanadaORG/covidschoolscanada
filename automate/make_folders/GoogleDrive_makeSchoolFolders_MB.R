# make folders for each province
# dir structure is 
# /SupportingDocs/Province_Name/SchoolNumber_SchoolName
require(googledrive)

# Create folders for BC
mb <- read.delim("/Users/shraddhapai/Google_covidschools/SchoolInfo/MB/MB_schools_raw.txt",sep="\t",h=T,as.is=T)
mb$School <- stringi::stri_encode(mb$School,"","UTF-8")

hpos <- gregexpr("#", mb$School)
ln <- unlist(lapply(hpos,length))
if (any(ln>1)) browser()
hpos <- regexpr("#", mb$School)
if (any(hpos<0)) {
	message("no hash")
	browser()
}
mb$School <- substr(mb$School,1,hpos-2)
mb$School <- gsub("\\/"," ",mb$School)

##tmp <- sprintf("SupportingDocs/BC/%i_%s",
##	bc$School.Code[idx],bc$School.Name[idx])
##browser()
for (k in 236:nrow(mb)){
	str <- sprintf("%i_%s",mb$Code[k],mb$School[k])
 print(str)
	#drive_mkdir(sprintf("SupportingDocs/MB/%s",str))
	dir.create(sprintf("/Users/shraddhapai/Google_covidschools/SupportingDocs/MB/%s",str))
}



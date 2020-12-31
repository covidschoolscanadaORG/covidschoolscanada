# make folders for each province
# dir structure is 
# /SupportingDocs/Province_Name/SchoolNumber_SchoolName
require(googledrive)

# Create folders for BC
sk <- read.delim("/Users/shraddhapai/Google_covidschools/SchoolInfo/SK/Active_List_of_School_Division_Schools_2019_20.csv",
	sep=",",h=T,as.is=T)
sk <- sk[-which(is.na(sk$DAN..)),]
colnames(sk)[2:3] <- c("Code","School")
sk$School <- stringi::stri_encode(sk$School,"","UTF-8")
sk$School <- gsub("\\/"," ",sk$School)

##tmp <- sprintf("SupportingDocs/BC/%i_%s",
##	bc$School.Code[idx],bc$School.Name[idx])
##browser()
for (k in 65:nrow(sk)) {
	str <- sprintf("%i_%s",sk$Code[k],sk$School[k])
 print(str)
	dir.create(sprintf("/Users/shraddhapai/Google_covidschools/SupportingDocs/SK/%s",str))
	#drive_mkdir(sprintf("SupportingDocs/SK/%s",str))
	
}



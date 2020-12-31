# make folders for each province
# dir structure is 
# /SupportingDocs/Province_Name/SchoolNumber_SchoolName
require(googledrive)

# Create folders for BC
bc <- read.delim("/Users/shraddhapai/Google_covidschools/SchoolInfo/BC/excelSchoolContact.csv",sep=",",h=T,as.is=T)
bc <- bc[,1:3]
bc$School.Name <- gsub("\\/"," ",bc$School.Name)

##tmp <- sprintf("SupportingDocs/BC/%i_%s",
##	bc$School.Code[idx],bc$School.Name[idx])
##browser()
for (k in 326:nrow(bc)) {
	str <- sprintf("%i_%s",bc$School.Code[k],bc$School.Name[k])
 print(str)
	dir.create(sprintf("/Users/shraddhapai/Google_covidschools/SupportingDocs/BC/%s",str))
	#drive_mkdir(sprintf("SupportingDocs/BC/%s",str))
}



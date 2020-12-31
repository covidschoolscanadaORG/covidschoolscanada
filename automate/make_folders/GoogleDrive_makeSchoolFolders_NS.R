# make folders for each province
# dir structure is 
# /SupportingDocs/Province_Name/SchoolNumber_SchoolName
require(readxl)

# Create folders for BC
ns <- read_excel("/Users/shraddhapai/Google_covidschools/SchoolInfo/NS_PEI/NSschools_latlong.xlsx")
ns <- as.data.frame(ns)
ns <- ns[,c("School.Code","School.Name")]
colnames(ns)<- c("Code","School")
ns$School <- stringi::stri_encode(ns$School,"","UTF-8")
ns$School <- gsub("\\/"," ",ns$School)


##tmp <- sprintf("SupportingDocs/BC/%i_%s",
##	bc$School.Code[idx],bc$School.Name[idx])
##browser()
for (k in 1:nrow(ns)) {
	str <- sprintf("%s_%s",ns$Code[k],ns$School[k])
 print(str)
	dir.create(sprintf("/Users/shraddhapai/Google_covidschools/NS/%s",str))
	#drive_mkdir(sprintf("SupportingDocs/NB/%s",str))
	
}



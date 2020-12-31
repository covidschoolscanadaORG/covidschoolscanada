# make folders for each province
# dir structure is 
# /SupportingDocs/Province_Name/SchoolNumber_SchoolName
require(readxl)

# Create folders for BC
pei <- read.delim("/Users/shraddhapai/Google_covidschools/SchoolInfo/NS_PEI/PEI_OD0055_Public_School_Locations.csv",sep=",",
	h=T,as.is=T)
colnames(pei)[1:2] <- c("Code","School")
colnames(pei)<- c("Code","School")
pei$School <- stringi::stri_encode(pei$School,"","UTF-8")
pei$School <- gsub("\\/"," ",pei$School)

##tmp <- sprintf("SupportingDocs/BC/%i_%s",
##	bc$School.Code[idx],bc$School.Name[idx])
##browser()
for (k in 1:nrow(pei)) {
	str <- sprintf("%s_%s",pei$Code[k],pei$School[k])
 print(str)
	dir.create(sprintf("/Users/shraddhapai/Google_covidschools/PEI/%s",str))
	#drive_mkdir(sprintf("SupportingDocs/NB/%s",str))
	
}



# make folders for each province
# dir structure is 
# /SupportingDocs/Province_Name/SchoolNumber_SchoolName
require(readxl)

# Create folders for BC
nb <- read_excel("/Users/shraddhapai/Google_covidschools/SchoolInfo/NB/NB_SchoolCoords_withCity_201231.xlsx")
nb <- as.data.frame(nb)
nb <- nb[,c("School.Number","Schools")]
browser()
colnames(nb)<- c("Code","School")
nb$School <- stringi::stri_encode(nb$School,"","UTF-8")
nb$School <- gsub("\\/"," ",nb$School)


##tmp <- sprintf("SupportingDocs/BC/%i_%s",
##	bc$School.Code[idx],bc$School.Name[idx])
##browser()
for (k in 1:nrow(nb)) {
	str <- sprintf("%s_%s",nb$Code[k],nb$School[k])
 print(str)
	dir.create(sprintf("/Users/shraddhapai/Google_covidschools/NB/%s",str))
	#drive_mkdir(sprintf("SupportingDocs/NB/%s",str))
	
}



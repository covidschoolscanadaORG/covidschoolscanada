# make folders for each province
# dir structure is 
# /SupportingDocs/Province_Name/SchoolNumber_SchoolName
require(googledrive)

# Create folders for AB
ab <- read.delim("/Users/shraddhapai/Google_covidschools/SchoolInfo/AB/Alberta_Schools_with_geolocations.csv",
	sep=",",h=T,as.is=T)
ab <- ab[,c("School.Code","School.Name")]
colnames(ab) <- c("Code","School")
ab$School <- stringi::stri_encode(ab$School,"","UTF-8")
ab$School <- gsub("\\/"," ",ab$School)

for (k in 45:nrow(ab)) {
	str <- sprintf("%i_%s",ab$Code[k],ab$School[k])
 print(str)
	#drive_mkdir(sprintf("SupportingDocs/AB/%s",str))
	dir.create(sprintf("/Users/shraddhapai/Google_covidschools/SupportingDocs/AB/%s",str))
}



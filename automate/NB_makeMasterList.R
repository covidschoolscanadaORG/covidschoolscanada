# merge NB geoloc with school city via code
require(readxl)

schools <- read_excel("/Users/shraddhapai/Google_covidschools/SchoolInfo/NB/All_NB_schools_201226.xlsx")
schools <- as.data.frame(schools)
schools <- schools[,c("School Number","Municipality","Schools")]
colnames(schools) <- c("School.Number","City","Schools")

loc <- read.delim("/Users/shraddhapai/Google_covidschools/SchoolInfo/NB/NB_Geocoded_Schools_201226 - nb_match.csv",sep=",",h=T,as.is=T)
x <- merge(x=schools,y=loc,by="School.Number",all.x=TRUE)

not_there <- which(!schools$School.Number %in% x$School.Number)
print(head(schools[not_there,]))

write.table(x,file="NB_SchoolCoords_withCity_201231.csv",sep=",",col=T,row=F,quote=T)




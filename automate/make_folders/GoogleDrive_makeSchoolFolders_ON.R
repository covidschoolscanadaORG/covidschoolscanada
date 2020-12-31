# make folders for each province
# dir structure is 
# /SupportingDocs/Province_Name/SchoolNumber_SchoolName
require(readxl)

# Create folders for BC
on <- read_excel("/Users/shraddhapai/Google_covidschools/SchoolInfo/ON/Master File of Schools.xlsx")
on <- as.data.frame(on)
colnames(on) <- c("Board.Name","School.Code","School.Name","Level","Street","City")

# make folder for each school board
divs <- unique(on$Board.Name)
on$School.Name <- gsub("\\/"," ",on$School.Name)
message(sprintf("%i school boards", length(divs)))
for (k in divs) {
	divdir <-sprintf("/Users/shraddhapai/Google_covidschools/ON/%s",k)
	message(divdir)
	dir.create(divdir)

	dat2 <- subset(on, Board.Name == k)

	for (m in 1:nrow(dat2)) {
			str <- sprintf("%s_%s",
				dat2$School.Code[m],dat2$School.Name[m])
			message(sprintf("\t%s",str))
			divdir2 <- sprintf("%s/%s",divdir,str)
			dir.create(divdir2)
	}
}

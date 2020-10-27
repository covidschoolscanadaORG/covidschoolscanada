# convert qc update excel file into format compatible
# with covidschoolsCA

source("utils.R")

date2use <- Sys.Date()
dt <- format(date2use,"%y%m%d")
outDir <- sprintf("/home/shraddhapai/Canada_COVID_tracker/export-%s",dt)

inFile <- sprintf("%s/CEQ_annotated_%s.csv",outDir,dt)
outFile <- sprintf("%s/CEQ_annotated_clean_%s.csv",outDir,dt)

dat <- read.delim(inFile,sep=",",h=T,as.is=T)

for (k in 1:ncol(dat)) {
	dat[,k] <- stringr::str_trim(dat[,k])
}

map_colname <- list(
	"École"="institute.name",
	"Nombre"="Total.cases.to.date",
	"Ville"="City",
	"Type.d.établissement"="Type_of_school",
	"Centre.de.Service"="School.board",
	"latitude"="Latitude",
	"longitude"="Longitude")
for (nm in names(map_colname)){
	colnames(dat)[which(colnames(dat)==nm)] <- map_colname[[nm]]
}
dat$Total.students.to.date <- NA
dat$Total.staff.to.date <- NA

dat$Total.outbreaks.to.date <- NA
dat$Outbreak.dates <- NA
dat$Outbreak.Status <- "Single/unlinked cases"

idx <- which(dat$Total.cases.to.date >=5)
dat$Total.outbreaks.to.date[idx] <- 1L
dat$Outbreak.Status[idx] <- "Declared outbreaks"
dat$Outbreak.dates[idx] <- dat$Date[idx]

dat$Province <- "QC"

finalorder <- c("institute.name","Total.cases.to.date",
	"Total.students.to.date","Total.staff.to.date",
	"Date","Article",
	"Total.outbreaks.to.date","Outbreak.dates","Outbreak.Status",
	"Type_of_school","School.board",
	"City","Province",
	"Latitude","Longitude")


dat$Article <- "https://www.covidecolesquebec.org/nouvelles-closions"
dat  <- dat[,finalorder]

# --------------------------------------------
# Clean school type
# --------------------------------------------
dat$Type_of_school[which(dat$Type_of_school %in% 
	c("Secondaire, CEGEP, Professionnel",
	"Professionnel",
	"professionnel","Université","CEGEP"))] <- "Post-secondary"
dat$Type_of_school <- sub("Primaire et Secondaire","Mixed",dat$Type_of_school)
dat$Type_of_school[grep("et secondaire",dat$Type_of_school)] <- "Mixed"
dat$Type_of_school <- sub("Primaire","Elementary",dat$Type_of_school)
dat$Type_of_school <- sub("Secondaire","Secondary",dat$Type_of_school)
dat$Type_of_school <- sub("Commission scolaire","Field Office",dat$Type_of_school)
dat$Type_of_school[grep("^Secondary s",dat$Type_of_school)] <- "Secondary"
print(table(dat$Type_of_school,useNA="always"))
idx <- which(is.na(dat$Type_of_school))
if (any(idx)) {
	message("Found schools with no type - excluding records")
	message(sprintf("Excluding %i records",length(idx)))
	dat <- dat[-idx,]
}

# --------------------------------------------
# Clean region
# --------------------------------------------
dat$School.board <- sub("Decouvreurs","Découvreurs",dat$School.board)
dat$School.board <- sub("Energie","Énergie",dat$School.board)
dat$School.board <- sub("Jonquiere","Jonquière",dat$School.board)
dat$School.board <- sub("Vallee des Tisserands","Vallée-des-Tisserands",
	dat$School.board)
dat$School.board <- sub("Pays-de-Bluets","Pays-de-Bleuets",
	dat$School.board)

# add active/resolved column
dat$ActiveOrResolved <- addActiveResolved(dat,date2use)

write.table(dat,file=outFile,sep=",",col=T,row=F,quote=TRUE)
message("Done QC annotation processing")


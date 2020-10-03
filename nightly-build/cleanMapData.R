# first-pass map clean before running plots
source("utils.R")

message("-------------------------------------")
message("Cleaning map data")
message("-------------------------------------")

dt <- format(Sys.Date(),"%y%m%d")
inDir <- sprintf("/home/shraddhapai/Canada_COVID_tracker/export-%s",dt)
inFile <- sprintf("%s/CanadaMap_QuebecMerge-%s.csv",
	inDir,dt)
outFile <- sprintf("%s/CanadaMap_QuebecMerge-%s.clean.csv",
	inDir,dt)

tryCatch({
dat <- read.delim(inFile,sep=",",h=T,as.is=T)
},error=function(ex){
print(ex)
},finally={
})

# -----------------------------------------
# WHITESPACE REMOVE
message("* Removing trailing whitespace")
for (k in 1:ncol(dat)) {
	dat[,k] <- stringr::str_trim(dat[,k])
	dat[,k] <- trimws(dat[,k])
}

# -----------------------------------------
# CLEAN PROVINCE, CODE AS FACTOR
message("------------------------------------")
message("* Cleaning Province")
message("------------------------------------")
prov <- c("AB","BC","MB","NB","NL","NS","ON","PEI","QC","SK","NWT","NU","YT")
idx <- which(dat$Province=="Edmonton")
if (any(idx)){
	dat$Province[idx] <- "Alberta"
	dat$City[idx] <- "Edmonton"
}
print(table(dat$Province,useNA="always"))
if(any(dat$Province %in% "Newfoundland and Labrador")) {
		dat$Province[which(dat$Province == "Newfoundland and Labrador")] <- "NL"
}
if(any(dat$Province %in% "Northwest Territories")){ 
		dat$Province[which(dat$Province == "Northwest Territories")] <- "NWT"
}
if(any(dat$Province %in% "Prince Edward Island")){
		dat$Province[which(dat$Province == "Prince Edward Island")] <- "PEI"
}
if(any(dat$Province %in% "Saskatchewan")){
		dat$Province[which(dat$Province == "Saskatchewan")] <- "SK"
}
if(any(dat$Province %in% "New Brunswick")) {
		dat$Province[which(dat$Province == "New Brunswick")] <- "NB"
}
if(any(dat$Province %in% "Québec")) {
		dat$Province[which(dat$Province == "Québec")] <- "QC"
}
if(any(dat$Province %in% "Alberta")) {
		dat$Province[which(dat$Province == "Alberta")] <- "AB"
}
if(any(dat$Province %in% "Ontario")) {
		dat$Province[which(dat$Province == "Ontario")] <- "ON"
}
dat$Province[grep("British Columbia",dat$Province)] <- "BC"

if(any(dat$Province %in% "Manitoba")) {
		dat$Province[which(dat$Province == "Manitoba")] <- "MB"
}
if(any(dat$Province %in% "Nova Scotia")) {
		dat$Province[which(dat$Province == "Nova Scotia")] <- "NS"
}
if(any(dat$Province %in% "Nunavut")) {
		dat$Province[which(dat$Province == "Nova Scotia")] <- "NU"
}
if(any(dat$Province %in% "Yukon")) {
		dat$Province[which(dat$Province == "Nova Scotia")] <- "YT"
}

dat$Province <- factor(dat$Province, 
	level=prov) 
print(table(dat$Province,useNA="always"))

# -----------------------------------------
# CLEAN TYPE OF SCHOOL
message("------------------------------------")
message("* Cleaning Type_of_school")
message("------------------------------------")
# missing
idx <- union(which(is.na(dat$Type_of_school)),
	which(dat$Type_of_school==""))
if (any(idx)) dat$Type_of_school[idx] <- "TBA"
idx <- which(dat$Type_of_school=="?")
if (any(idx)) dat$Type_of_school[idx] <- "TBA"
idx <- which(dat$Type_of_school=="HIgh School")
if (any(idx)) dat$Type_of_school[idx] <- "High School"

# mixed school
idx <- grep(";",dat$Type_of_school)
if (length(idx)>0) {
	dat$Type_of_school[idx] <- "Mixed"
}
# code high school as secondary
idx <- which(dat$Type_of_school=="High School")
if (any(idx)) {
	dat$Type_of_school[idx] <- "Secondary"
}
idx <- grep("Elementary school", dat$Type_of_school,
		ignore.case=TRUE)
if (any(idx)) dat$Type_of_school[idx] <- "Elementary"

dat$Type_of_school <- factor(dat$Type_of_school,
	levels=schoolLevels())

if (any(is.na(dat$Type_of_school))) {
	message("converting school to factor gave NA")
	idx <- which(is.na(dat$Type_of_school))
	print(dat[idx,])
	stop("")
	browser()
}
print(table(dat$Type_of_school,useNA="always"))

# -----------------------------------------
# CLEAN SCHOOL BOARD
message("------------------------------------")
message("* Cleaning School board")
message("------------------------------------")
dat$School.board <- sub("District School Board", 
	"DSB",dat$School.board)
dat$School.board <- sub("Catholic DSB", "CDSB",dat$School.board)
dat$School.board <- sub("School Division", "SD",dat$School.board)
dat$School.board <- sub("School District", "SD",dat$School.board)
dat$School.board <- sub("Catholic SD", "CSD",dat$School.board)
dat$School.board <- sub("Conseil scolaire", "CS",dat$School.board)
dat$School.board <- sub("Board of Education", "BofEd",dat$School.board)
dat$School.board <- sub(" PS$", " PSD",dat$School.board)
dat$School.board <- sub(" PS$", " PSD",dat$School.board)
dat$School.board <- sub(" PSB$", " PSD",dat$School.board)
dat$School.board <- sub(" PSD$", " SD",dat$School.board)
dat$School.board <- sub("CÉP de l'Est de l'Ontario", "CEPEO",
	dat$School.board)
dat$School.board <- sub("Public Schools", "PS",
	dat$School.board)
dat$School.board[grep("CECCE",dat$School.board)] <- "CECCE"
dat$School.board <- sub("CSDC du Centre-Est de l'Ontario",
	"CSDCEO",
	dat$School.board)
dat$School.board <- sub("CSDC du Nouvel-Ontario",
	"CSDCNO",
	dat$School.board)
dat$School.board <- sub("CS Viamond","CS Viamonde",
	dat$School.board)
dat$School.board <- sub("CS Viamondee","CS Viamonde",
	dat$School.board)
dat$School.board <- sub("Waterloo Region DSB","WRDSB",
	dat$School.board)
dat$School.board <- sub("Independent","Indep.",
	dat$School.board)
dat$School.board <- sub("Durham-Peel","Dufferin-Peel",
	dat$School.board)
dat$School.board <- sub("Grande Prairie SD", "Grand Prairie SD",dat$School.board)
dat$School.board <- sub("TCDSB", "Toronto CDSB",dat$School.board)
dat$School.board <- sub(" Kootenay/Columbia"," Kootenay-Columbia",dat$School.board)
dat$School.board <- sub("Ottawa-Carleton ", "OC", 
	dat$School.board)
dat$School.board <- sub("Ottawa CDSB", "OCSB", 
	dat$School.board)
dat$School.board <- sub("OCDSB", "Ottawa-Carleton DSB", 
	dat$School.board)
dat$School.board <- sub("OCSB", "Ottawa CDSB", 
	dat$School.board)
dat$School.board <- sub("WRDSB", "Waterloo Regional DSB", 
	dat$School.board)
dat$School.board <- sub("HCDSB", "Halton CDSB", 
	dat$School.board)
dat$School.board <- sub("RCCDSB", "Renfrew County CDSB",
	dat$School.board)
dat$School.board <- sub("SMCDSB", "Simcoe Muskoka CDSB",
	dat$School.board)
dat$School.board <- sub("DSB Niagara", "Niagara DSB",
	dat$School.board)
dat$School.board <- sub("York Regional DSB", "York Region DSB",
	dat$School.board)
dat$School.board <- sub("YRDSB", "York Region DSB",
	dat$School.board)
dat$School.board <- sub("HDSB", "Halton DSB",
	dat$School.board)
dat$School.board <- sub("DSB of Niagara", "Niagara DSB",
	dat$School.board)
dat$School.board <- sub("Division Scolaire Franco-Manitobaine", "Franco-Manitobaine SD",
	dat$School.board)
dat$School.board <- sub("Franco-manitobian SD", "Franco-Manitobaine SD",
	dat$School.board)
idx <- which(dat$School.board == "Thames Valley")
if (any(idx)) 
	dat$School.board[idx] <- "Thames Valley DSB"
dat$School.board <- sub("Kawartha Pine DSB", "Kawartha Pine Ridge DSB",
	dat$School.board)

dat$School.board <- stringr::str_trim(dat$School.board)
idx <- which(dat$School.board=="")
if (any(idx)) dat$School.board[idx] <- "TBA"#"other/uncurated"

dat$ct <- 1
df2 <- aggregate(dat$ct, 
	by=list(Province=dat$Province,Board=dat$School.board),
	FUN=sum)
df2$Province <- factor(df2$Province)
df2$Board <- factor(df2$Board)
colnames(df2)[3] <- "ct"
for (prov in unique(df2$Province)) {
	message("------------------------")
	message(prov)
	message("")
	df3 <- subset(df2,Province==prov)
	df3 <- df3[order(-df3$ct),]
	print(df3)
}
dat <- dat[,-which(colnames(dat)=="ct")]

finalorder <- c("institute.name","Total.cases.to.date",
	"Total.students.to.date","Total.staff.to.date",
	"Date","Article",
	"Total.outbreaks.to.date","Outbreak.dates","Outbreak.Status",
	"Type_of_school","School.board",
	"City","Province",
	"Latitude","Longitude")
dat  <- dat[,finalorder]

message("* Writing output file")
write.table(dat,file=outFile,sep=",",
	col=TRUE,row=FALSE,quote=TRUE)

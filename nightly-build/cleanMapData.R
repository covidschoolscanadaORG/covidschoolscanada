# first-pass map clean before running plots
source("utils.R")
options(warn=2)

message("-------------------------------------")
message("Cleaning map data")
message("-------------------------------------")

date2use <- Sys.Date()
dt <- format(date2use,"%y%m%d")
baseDir <- "/home/shraddhapai/Canada_COVID_tracker/"
inDir <- sprintf("%s/export-%s",baseDir,dt)
inFile <- sprintf("%s/CanadaMap_QuebecMerge-%s.csv",
	inDir,dt)
outFile <- sprintf("%s/CanadaMap_QuebecMerge-%s.clean.csv",
	inDir,dt)

if (file.exists(outFile)) unlink(outFile)

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

if (any(!dat$Province %in% c("AB","BC","ON","QC","MB","SK","YT","NB",
	"NS","NL")) || any(is.na(dat$Province))) {
	idx <- which(dat$Province=="")
	dat <- dat[-idx,]
	print(table(dat$Province,useNA="always"))
	browser()
	#stop("Strange Province. Take a look")
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
idx <- which(dat$Type_of_school=="Elementary ; Middle School")
if (any(idx)) dat$Type_of_school[idx] <- "Mixed"

idx <- which(dat$Type_of_school=="Middle")
if (any(idx)) dat$Type_of_school[idx] <- "Middle School"

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


tryCatch({
	dat$Type_of_school <- factor(dat$Type_of_school,
		levels=schoolLevels_full())
},error=function(ex){
	stop("error while converting type of school")
	print(ex)
},finally={
})

if (any(is.na(dat$Type_of_school))) {
	message("converting school to factor gave NA")
	idx <- which(is.na(dat$Type_of_school))
	print(dat[idx,])
	browser()
	stop("")
}
print(table(dat$Type_of_school,useNA="always"))
# -----------------------------------------
# CLEAN DATE
dat$Date <- gsub(":",";",dat$Date)
dat$Date <- gsub("^20-","2020-",dat$Date)

# -----------------------------------------
# CLEAN CASES
dat$Total.cases.to.date <- gsub(":",";",dat$Total.cases.to.date)
cs <- strsplit(dat$Total.cases.to.date,";")
tryCatch({
cs <- lapply(cs,function(x) { 
	x <- stringr::str_trim(x)
	x <- as.integer(x); 
	sum(x,na.rm=TRUE)
})
cs <- unlist(cs)
}, error=function(ex){
	print("Error while processing # cases")
	browser()
},finally={
})

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
dat$School.board <- sub("Franco-Manitoban SD", "Franco-Manitobaine SD",
	dat$School.board)
idx <- which(dat$School.board == "Thames Valley")
if (any(idx)) 
	dat$School.board[idx] <- "Thames Valley DSB"
dat$School.board <- sub("Kawartha Pine DSB", "Kawartha Pine Ridge DSB",
	dat$School.board)
dat$School.board <- sub("SD 59 Peace River South", 
	"SD59 Peace River South",
	dat$School.board)
dat$School.board[grep("SD45",dat$School.board)] <- "SD45 West Vancouver"
dat$School.board[grep("SD34",dat$School.board)] <- "SD34 Abbotsford"
dat$School.board[grep("SD43",dat$School.board)] <- "SD43 Coquitlam"
dat$School.board[grep("SD44",dat$School.board)] <- "SD44 North Vancouver"
dat$School.board[grep("SD36",dat$School.board)] <- "SD36 Surrey"
dat$School.board[grep("SD39",dat$School.board)] <- "SD39 Vancouver" 
dat$School.board[grep("SD38",dat$School.board)] <- "SD38 Richmond"
dat$School.board[grep("SD41",dat$School.board)] <- "SD41 Burnaby" 
dat$School.board[grep("SD37",dat$School.board)] <- "SD37 Delta" 
dat$School.board[grep("SD35",dat$School.board)] <- "SD35 Langley" 
dat$School.board[which(dat$School.board=="DDSB")] <- "Durham DSB"

dat$School.board <- sub("Franco-Manitobaine SD SD",
	"Franco-Manitobaine SD",
	dat$School.board)
dat$School.board <- sub("Indep\\. ","Indep ",
	dat$School.board)
dat$School.board[grep("Indep  Schools",dat$School.board)] <- 
	"Indep Schools"
dat$School.board <- sub("Rocky View Schools","Rocky View SD",
	dat$School.board)
##dat$School.board <- sub("Indep. Vancouver","IndepVancouver",
###	dat$School.board)
dat$School.board[grep("Indep  Surrey",dat$School.board)] <- "Indep Surrey"
dat$School.board[which(dat$School.board=="DCDSB")] <- "Dufferin-Peel CDSB"
dat$School.board[grep("Manitoba Catholic School",dat$School.board)] <- "Manitoba Catholic Schools"
dat$School.board[grep("Portage la Prairie",dat$School.board)] <- "Portage La Praire"


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

# ----------------------------------------
# BC - Label clusters
# ----------------------------------------
sumCases <- function(x) {
}
idx <- intersect(which(dat$Province %in% "BC"), 
	grep(";", dat$Total.cases.to.date))
csum <- c() 
totcase <- dat$Total.cases.to.date[idx]
for (k in 1:length(idx)) {
	cur <- unlist(strsplit(dat$Total.cases.to.date[idx[k]],";"))
	cur <- as.integer(stringr::str_trim(cur))
	totcase[k] <- sum(cur)
}
dat$Outbreak.Status[idx[which(totcase > 1)]] <- "Cluster (BC)"

message("------------------------------------")
message("* Dates cleanup")
message("------------------------------------")
dat$Date <- gsub(":",";",dat$Date)
dat$Date <- gsub("--","-",dat$Date)
dat$Date <- gsub("^20-","2020-",dat$Date)

finalorder <- c("institute.name","Total.cases.to.date",
	"Total.students.to.date","Total.staff.to.date",
	"Date","Article",
	"Total.outbreaks.to.date","Outbreak.dates","Outbreak.Status",
	"Type_of_school","School.board",
	"City","Province",
	"Latitude","Longitude")
dat  <- dat[,finalorder]

# -----------------------------------------
# ADD AUTOGEN TABLE
dt2 <- format(date2use+1,"%Y-%m-%d")
autoFile <- sprintf("%s/AutoGen/Peel_%s.csv",baseDir,dt2)
autoDat <- read.delim(autoFile,sep=",",h=T,as.is=T)
midx <- match(colnames(dat),colnames(autoDat))
if (all.equal(colnames(autoDat)[midx],colnames(dat))!=TRUE) {
	stop("colnames don't match")
}
autoDat <- autoDat[,midx]
rmidx <- which(dat$School.board %in% unique(autoDat$School.board))
message(sprintf("Removing %i entries for {%s}", length(rmidx),
	paste(unique(autoDat$School.board),collapse=",")))

message(sprintf("Adding %i auto-gen entries",nrow(autoDat)))
dat <- rbind(dat,autoDat)
dat$Type_of_school[grep("Partner organizations", dat$Type_of_school)] <- "Field Office"

message("* Add active/resolved status")
dat$ActiveOrResolved <- addActiveResolved(dat,date2use)

message("* Writing output file")
write.table(dat,file=outFile,sep=",",
	col=TRUE,row=FALSE,quote=TRUE)

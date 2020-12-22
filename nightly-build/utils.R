# misc functions
Sys.setenv(TZ="America/Toronto")

#' convert Prov to abbrev
prov2abbrev <- function(x) {
	if(any(x %in% "Newfoundland and Labrador")) {
			x[which(x == "Newfoundland and Labrador")] <- "NL"
	}
	if(any(x %in% "Northwest Territories")){ 
			x[which(x == "Northwest Territories")] <- "NWT"
	}
	if(any(x %in% "Prince Edward Island")){
			x[which(x == "Prince Edward Island")] <- "PEI"
	}
	if(any(x %in% "Saskatchewan")){
			x[which(x == "Saskatchewan")] <- "SK"
	}
	if(any(x %in% "New Brunswick")) {
			x[which(x == "New Brunswick")] <- "NB"
	}
	if(any(x %in% "Québec")) {
			x[which(x == "Québec")] <- "QC"
	}
	if(any(x %in% "Alberta")) {
			x[which(x == "Alberta")] <- "AB"
	}
	if(any(x %in% "Ontario")) {
			x[which(x == "Ontario")] <- "ON"
	}
	x[grep("British Columbia",x)] <- "BC"
	
	if(any(x %in% "Manitoba")) {
			x[which(x == "Manitoba")] <- "MB"
	}
	if(any(x %in% "Nova Scotia")) {
			x[which(x == "Nova Scotia")] <- "NS"
	}
	if(any(x %in% "Nunavut")) {
			x[which(x == "Nunavut")] <- "NU"
	}
	if(any(x %in% "Yukon")) {
			x[which(x == "Yukon")] <- "YT"
}
x
}

# checks date format
IsDate <- function(mydate, date.format = "%Y-%m-%d") {
  tryCatch(!is.na(as.Date(mydate, date.format)),  
           error = function(err) {FALSE})  
}

# schools with cases < 14 days apart have clusters
findClusters <- function(x) {

x$Province <- as.character(x$Province)
x <- x[,c("Date","Province","Total.cases.to.date","institute.name")]
y <- flattenCases(x)
isd <- IsDate(y$Date)
if (any(isd==FALSE)) {
	message("found malformed date")
	print(y[which(!isd),])
	browser()
}
y$tstamp <- as.POSIXct(y$Date)

tbl <- table(y$institute.name)
multi <- names(tbl)[which(as.integer(tbl)>=2)]
cluster <- list()
for (k in multi) {
 dt <- sort(y$tstamp[which(y$institute.name==k)])
	message(sprintf("%s: %i entries",k,length(dt)))
 dtdiff <- as.integer(diff(dt))
 if (any(dtdiff <= 14)) {
		cluster <- c(cluster, k)
		message("> cluster!")
	}
}
return(cluster)
}


#' returns active/resolved column for cases
#'
#' @param dat (data.frame) cleaned school report table
#' @param cur (Date) current date
#' @param num_days (integer) max difference between cur and
#' last reported date, to count as active. 
#' @return (logical) TRUE if case is active; FALSE if resolved
addActiveResolved <- function(dat,cur,num_days=14) {
	# last date on case
	lastDate <- sapply(dat$Date,function(x){
			x <- gsub(" ","",x)
			y <- unlist(strsplit(x,";"))
			y <- y[length(y)]
			y <- stringr::str_trim(y)
	})
	timelapse <- as.integer(cur-as.Date(lastDate))
	stt <- timelapse > num_days
	#pdf("test.pdf");plot(as.Date(lastDate),stt);dev.off()
	return(stt)
}

getTable_dec <- function(x) {
	tb <- table(x)
	nm <- names(tb)
	val <- as.numeric(tb)
	names(val) <- nm
	val <- val[order(val,decreasing=TRUE)]
	val
}

getProvTerr <- function() {
	c("Alberta","British Columbia","Manitoba","New Brunswick","Newfoundland and Labrador","Nova Scotia","Ontario","Prince Edward Island","Québec","Saskatchewan","Northwest Territories","Nunavut","Yukon")
}

# convert each entry of 1;1; cases into one record per case
flattenCases <- function(curd,type="case") {
curd$Province <- as.character(curd$Province)
	if (type=="case"){ 
		type <- "Total.cases.to.date"
		dtype <- "Date"
	} else {
		type <- "Total.outbreaks.to.date"
		dtype <- "Outbreak.dates"
		curd <- curd[,c(dtype,"Province",type)]
	}

	message("Flattening cases...")
	idx <- grep(";", curd[,dtype])
	simple <- setdiff(1:nrow(curd),idx)
	simple 	<- curd[simple,]
	multi 	<- curd[idx,]
tryCatch({
	multi[,type] <- stringr::str_trim(multi[,type])
	multi[,type] <- gsub(" ","",multi[,type])
	multi[,dtype] <- gsub(" ","",multi[,dtype])
	
	for (i in 1:nrow(multi)) {
		if (type=="Total.outbreaks.to.date"){
			tmp <- paste(rep(1,multi[i,type]),collapse=";")
			multi[i,type] <- tmp
		}
		tryCatch({
		x <- as.integer(unlist(
			strsplit(multi[i,type],";")))
		},error=function(ex){ 
			browser()
			print(ex)
			message("Error while flattening")
		})	
		y <- trimws(unlist(strsplit(multi[i,dtype],";")))
		if (any(is.na(x))|| any(is.na(y))) {
			message("NA case/date")
			print(multi[i,])
			#browser()
		}
		for (j in 1:length(x)) {
			simple <- rbind(simple, 
				c(y[j],multi$Province[i],x[j],
					multi$institute.name[i]))
		}
	     cat(sprintf("%s: {%s}",multi[i,type],
			paste(x,collapse=",")),file="test.txt",append=TRUE)
	}
	curd[,dtype] <- stringr::str_trim(curd[,dtype])

	# shared cases have NA dates
	if (any(is.na(curd[,type]))) {
		curd[which(is.na(curd[,type])),type] <- 0
	}
	idx <- which(is.na(curd$institute.name))	
	if (any(idx)) curd <- curd[-idx,]
},error=function(ex){
	print(ex)
	browser()
},finally={
	
})
	curd <- simple
	curd
}


getAllMondays <- function(year) {
    days <- as.POSIXlt(paste(year, 1:366, sep="-"), format="%Y-%j")
    Ms <- days[days$wday==1]
    Ms[!is.na(Ms)]  # Needed to remove NA from day 366 in non-leap years
}

schoolLevels <- function() {
	return(c("Elementary",
		"Secondary","Mixed",
		"Office","PostSec","TBA"))
	}

schoolLevels_full <- function() {
	return(c("Elementary",
		"Secondary","Middle School","Mixed",
		"Field Office","TBA"))
	}

### get background coordinates by plotting grob that serves as border of plot
###TableGrob (12 x 9) "layout": 23 grobs
###    z         cells       name                                      grob
###1   0 ( 1-12, 1- 9) background          rect[plot.background..rect.1146]
###2   5 ( 6- 6, 4- 4)     spacer                            zeroGrob[NULL]
###3   7 ( 7- 7, 4- 4)     axis-l      absoluteGrob[GRID.absoluteGrob.1132]
###4   3 ( 8- 8, 4- 4)     spacer                            zeroGrob[NULL]
getBG <- function(plotType) {
	if (plotType=="Province") {
		return(list(top=1,bottom=12,left=1,right=9))
	} else if (plotType == "Type of school") {
		return(list(top=1,bottom=12,left=1,right=11))
	} else if (plotType == "cumuCases") {
		return(list(top=1,bottom=12,left=1,right=9))
	} else if (plotType == "schoolboard") {
		return(list(top=1,bottom=12,left=1,right=9))
	}
}

# add margin and footnotes around page
# p - ggplot object
annotPage <- function(p,plotType,plotTitle) {
	# instructions from: https://drawar.github.io/posts/add-borders-annotate-outside-ggplot/
	g1 <- cowplot::as_gtable(p)
if (plotType=="Total_outbreaks") browser()
	marg <- getBG(plotType)

	# add fill border
	rect <- grid::rectGrob(gp = grid::gpar(col = NA, fill = "red"))
	# add to the left and right
	for(i in c(1,marg$right)) 
		g1 = gtable_add_grob(g1, rect, t = marg$top, b = marg$bottom, l=i)
	# add to the top and bottom
	for(i in c(1,marg$bottom)) 
		g1 = gtable_add_grob(g1, rect, t = i, l = marg$left, r=marg$right)

	g1 <- addFooter(g1,plotType)
	g1 <- addHeader(g1,plotType,plotTitle)

	return(g1)
}

addHeader <- function(g,plotType,plotTitle) {
	marg <- getBG(plotType)	
	left.title = textGrob(plotTitle, x = 0.5, y = 0.65, 
		just = c("center", "top"), 
		gp = gpar(fontsize = 18, 
		col =  "white"))
	labs.title = gTree("LabsTitle", children = gList(left.title))

	left.sub = textGrob("", x = 0, y = 0.9, 
		just = c("left", "top"), 
		gp = gpar(fontsize = 14, col =  "black"))
	labs.sub = gTree("LabsSub", children = gList(left.sub))

	left.head = matrix(list(left.title, left.sub), nrow = 2)
		head = gtable_matrix(name = "Heading", grobs = left.head, 
		widths = unit(1, "null"), 
		heights = unit.c(
				unit(1.1, "grobheight", left.title) + unit(0.5, "lines"), 
				unit(1.1,  "grobheight", left.sub) + unit(0.5, "lines")))

	marg <- getBG(plotType)	
	g <- gtable_add_grob(g, head, t=1, l=marg$left+1, r=marg$right)
}

footerDate <- function() {
	t1 <- format(Sys.time(), "%b %d %Y")
	t2 <- format(Sys.time(), "%X")
	t2 <- substr(t2,1,regexpr(":",t2)-1)
	t3 <- format(Sys.time(), "%X")
	t3 <- substr(t3,nchar(t3)-1,nchar(t3))
	t4 <- sprintf("%s %s:00 %s EDT",t1,t2,t3)
	t4
}

addFooter <- function(g,plotType) {
	marg <- getBG(plotType)
	dt <- footerDate()
	footerText <- sprintf("@covidschoolscanada")
	# left footer
	left.foot = textGrob(footerText,
		x = 0, y = 0.8, just = c("left", "top"), 
		gp = gpar(fontsize = 20, col =  "white"))
	labs.foot = gTree("LabsFoot", children = gList(left.foot))

	g <- gtable_add_grob(g, labs.foot, 
		t=marg$bottom, l=2, r=marg$right)

	# right footer
	right.foot = textGrob(format(Sys.Date(),"%y-%m-%d"),
		x = 0, y = 0.8, just = c("center", "top"), 
		gp = gpar(fontsize = 24, col =  "white"))
	labs.foot = gTree("LabsFoot", children = gList(right.foot))
	g <- gtable_add_grob(g, labs.foot, 
		t=marg$bottom, l=2, r=marg$right)
g
}

#' reverse geolocate - fetch city/prov given latlong
#'
#' @details Uses Photon Map API
#' @param x (data.frame) school table with Latitude and Longitude columns
#' @return (data.frame) columns Latitude, Longitude, City, Province
#' @importFrom RCurl getURIAsynchronous
#' @importFrom jsonlite fromJSON
revGeo <- function(x) {

require(mapboxapi)
message("Reverse geo-locating")

out <- list()
### urlbase <- "https://photon.komoot.io/reverse?"
for (k in 1:nrow(x)) {
	if (is.na(x$Latitude[k])) {
		out[[k]] <- rep(NA,4)
	} else {
###	urlFull <- sprintf("%slon=%s&lat=%s",urlbase,
###		as.character(x$Longitude[k]),as.character(x$Latitude[k]))
###	message(sprintf("\tFetching %s", urlFull))
message(sprintf("Mapbox: RevGeo: %s: %s, %s", 
	x$institute.name[k],
	as.character(x$Latitude[k]),
	as.character(x$Longitude[k])))
	tryCatch({
		cur <- as.numeric(x[k,c("Latitude","Longitude")])
		cur <- mb_reverse_geocode(coordinates=x[k,c("Longitude","Latitude")])
		blah <- stringr::str_trim(unlist(strsplit(cur,",")))
		city <- blah[length(blah)-2]
		prov <- blah[length(blah)-1]
	sp <- gregexpr(" ", prov)[[1]]
	if (sp[1]>0) {
		fr <- stringr::str_trim(substr(prov,1,sp[1]-1))
		if (fr %in% c("Ontario","Alberta","Manitoba","Saskatchewan")) {
			prov <- fr
		} else {
			prov <- substr(prov,1,sp[2]-1)
		}
	}

###		m <- RCurl::getURIAsynchronous(urlFull)
###		m2 <- RJSONIO::fromJSON(m)
###		m3 <- m2$features[[1]]$properties
###		city <- m3$city
###		if (is.null(city)) city <- m3$district
###		if (is.null(city)) city <- NA
###		prov <- m3$state
###		if (is.null(prov)) prov <- NA
		message(sprintf("\t%s , %s",city, prov))
		out[[k]] <- c(x$Longitude[k],x$Latitude[k],city,prov)
	}, error=function(ex) {
		print(ex)
		out[[k]] <- c(x$Longitude[k],x$Latitude[k],NA,NA)
	},finally={
	})
}
}
	out2 <- do.call("rbind",out)
	out2[,4] <- prov2abbrev(out2[,4])

	# make sure record order not mixed up
	if (any(is.na(out2[,1]))) {
		cat("found NA lat/long")
		browser()
	} else {
	if (any(abs(floor(as.numeric(out2[,2])-as.numeric(x$Latitude)))>.Machine$double.eps)||
			any(abs(floor(as.numeric(out2[,1])-as.numeric(x$Longitude)))>.Machine$double.eps)) {
			message("lat/long order doesn't match")
			browser()
	}
	}

	if (any(is.na(out2[,3]))) {
		cat("Found NA city/Province")
		print(out2[which(is.na(out2[,3])),])
	}
	out2
}

# tests revgeo
revGeo_test <- function() {
	inFile <- "/Users/shraddhapai/Google_covidschools/daily_data/Canada_COVID_tracker/export-201101/final_data/CanadaMap_QuebecMerge-201101.clean.csv"
	dat <- read.delim(inFile,sep=",",h=T,as.is=T)
	set.seed(123)
	blah <- dat[sample(1:nrow(dat),50,F),]	
	y <- revGeo(blah)
browser()
}

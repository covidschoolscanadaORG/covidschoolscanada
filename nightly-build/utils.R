# misc functions

# num new values
# 

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
flattenCases <- function(curd) {
	message("Flattening cases...")
	idx <- grep(";", curd$Total.cases.to.date)
	simple <- setdiff(1:nrow(curd),idx)
	simple 	<- curd[simple,]
	multi 	<- curd[idx,]
	multi$Total.cases.to.date <- stringr::str_trim(multi$Total.cases.to.date)
	multi$Total.cases.to.date <- gsub(" ","",
		multi$Total.cases.to.date)
	
	for (i in 1:length(multi)) {
		x <- as.integer(unlist(
			strsplit(multi$Total.cases.to.date[i],";")))
		y <- trimws(unlist(strsplit(multi$Date[i],";")))
		for (j in 1:length(x)) {
			simple <- rbind(simple, 
				c(y[j],multi$Province[i],x[j]))
		}
	}
	curd$Date <- stringr::str_trim(curd$Date)

	# shared cases have NA dates
	if (any(is.na(curd$Total.cases.to.date))) {
		curd$Total.cases.to.date[which(is.na(curd$Total.cases.to.date))] <- 0
	}

	curd <- simple
	curd
}


getAllMondays <- function(year) {
    days <- as.POSIXlt(paste(year, 1:366, sep="-"), format="%Y-%j")
    Ms <- days[days$wday==1]
    Ms[!is.na(Ms)]  # Needed to remove NA from day 366 in non-leap years
}

schoolLevels <- function() {
	return(c("Elementary","Middle School",
		"Secondary","Mixed","Cegep",
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


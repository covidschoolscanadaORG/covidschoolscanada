# misc functions

# num new values
# 
getDiff_Prov <- function(prov,val_today,val_yesterday) {

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


getAllMondays <- function(year) {
    days <- as.POSIXlt(paste(year, 1:366, sep="-"), format="%Y-%j")
    Ms <- days[days$wday==1]
    Ms[!is.na(Ms)]  # Needed to remove NA from day 366 in non-leap years
}

schoolLevels <- function() {
	#return(c("Elementary","Middle School","High School", "Elementary & Middle",
	#"Middle & High", "Elementary & Middle & High","Cegep","Field Office","?"))
	return(c("Elementary","Middle School","High School","Mixed","Cegep",
		"Field Office","?"))
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
	rect <- grid::rectGrob(gp = grid::gpar(col = NA, fill = "grey90"))
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
	left.title = textGrob(plotTitle, x = 0, y = 0.9, 
		just = c("left", "top"), 
		gp = gpar(fontsize = 18, 
		col =  "black"))
	labs.title = gTree("LabsTitle", children = gList(left.title))

	left.sub = textGrob("Canada COVID-19 School Tracker", x = 0, y = 0.9, 
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
	t4 <- sprintf("%s,%s %s",t1,t2,t3)
	t4
}

addFooter <- function(g,plotType) {
	marg <- getBG(plotType)
	dt <- footerDate()
	footerText <- sprintf("Source: COVID-19 School Tracker: %s \n@spaiglass ; masks4canada.org",dt)
	# left footer
	left.foot = textGrob(footerText,
		x = 0, y = 0.8, just = c("left", "top"), 
		gp = gpar(fontsize = 11, col =  "black"))
	labs.foot = gTree("LabsFoot", children = gList(left.foot))

	g <- gtable_add_grob(g, labs.foot, t=marg$bottom, l=2, r=marg$right)

	# right footer
	right.foot = textGrob(format(Sys.Date(),"%y-%m-%d"),
		x = 0, y = 0.8, just = c("right", "top"), 
		gp = gpar(fontsize = 11, col =  "black"))
	labs.foot = gTree("LabsFoot", children = gList(right.foot))

	g <- gtable_add_grob(g, labs.foot, t=marg$bottom, l=2, r=marg$right)
g
}



#' make cumulative case plot
#' 
#' @param indat (data.frame) case table flattened to have one report per row
#' @param provLvl (char) levels of Province in indat
#' @param totCase (list) total case count in provs
#' @param yposAdjust (list) yposition for Provs to avoid text collision. Keys are Provs, values are ypos.
#' @param ymin (integer) min of y-axis
#' @param xmaxAdj (integer) 
makeCumPlot <- function(indat,provLvl,totC,yposAdjust,ymin,
	xmaxAdj=30,font_scale=1,
	title="",suppText=FALSE) {
	p3 <- ggplot(indat,aes(x=tstamp,y=x,colour=Province))
	p3 <- p3 + scale_colour_brewer(palette="Spectral")
	p3 <- p3 + geom_line(lwd=1.9)
	p3 <- p3 + geom_vline(xintercept=mondays,col="#ff6666",
			linetype="dashed",lwd=2)
	p3 <- p3 + annotate("rect",
				xmin=as.Date("2020-12-18"),
				xmax=as.Date("2021-01-04"),
				ymin=-Inf,ymax=Inf,
				fill="#ffefef",alpha=0.1)
	p3 <- p3 + xlab("")
	p3 <- p3 + ylab("")
	p3 <- p3 + ggtitle(title)
	p3 <- p3 + scale_x_date(breaks=as.Date(
		c("2020-09-01","2020-10-01",
			"2020-11-01","2020-12-01",
			"2021-01-01","2021-02-01"),format="%b"))
	if (missing(ymin)) ymin <- 0
	p3 <- p3 + ylim(ymin,max(indat$x)*1.04)
	
	caseText <- c()
	for (k in 1:length(provLvl)) {
		i <- which(totC$Province==provLvl[k])
		val <- round(totC$x[i])
		caseText <- c(caseText,sprintf("%s:%s", 
			totC$Province[i],
			prettyNum(val,big.mark=",")))
	}
	
	yvals <- totC$x
	if (!is.null(ypos)) {
		for (nm in names(ypos)){
			yvals[which(totC$Province==nm)] <- ypos[[nm]]
		}
	}
		#yvals[which(totC$Province=="QC")] <- yvals[which(totC$Province=="QC")] + 200
		xvals <- rep(Sys.Date()+1, nrow(totC))
	
	cols <- RColorBrewer::brewer.pal(n=10,"Spectral")
	p3 <- p3 + expand_limits(x=Sys.Date()+xmaxAdj)
	p3 <- p3 + annotate("text",x=xvals,
			y=yvals,label=caseText,
			colour=cols,size=11*font_scale,
			fontface=2,
			vjust=0,hjust=0,fill="white")
if (!suppText) {
	p3 <- p3 + annotate("text",x=as.Date("2020-11-13"),
		y=max(indat$x)*1.035,
		hjust=0,vjust=0,
		label="Dec 18-Jan 4",colour="#ffffff",size=9,
		fontface=3)
}
	
	# annotate
	p3 <- p3 + school_th
	p3 <- p3 + theme(
		panel.background = element_rect(fill="#111111"),
		panel.grid.major=element_blank(),
		axis.text.x = element_text(angle = 20,
			size=30,vjust=0.5),
		axis.text.y = element_text(size=48*font_scale),
		legend.position = "none" 
	)
	
	return(p3)
}

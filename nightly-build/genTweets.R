
require(emo)
require(googledrive)

tweetDate <- function(){
t1 <- toupper(format(Sys.time(), "%d %b %Y"))
t2 <- format(Sys.time(), "%X")
t2 <- substr(t2,1,regexpr(":",t2)-1)
t3 <- format(Sys.time(), "%X")
t3 <- substr(t3,nchar(t3)-7,nchar(t3)-6)
t4 <- toupper(format(Sys.time(),"%p"))
t5 <- sprintf("%s, %s:%s %s",t1,t2,t3,t4)
t5
}

# merge daily with overall for all provinces
# @param df (data.frame) must have "Province" column with 
# provincial abbreviations (AB, ON, QC, etc.,)
getSchoolTotals <- function(df) {

# totals from https://docs.google.com/spreadsheets/d/1KZT5lwAeR0diOfQYtOvcrmvPOcPG0i9gkt8rFAOgBqU/edit?usp=sharing

# public only
public_sch <- list(
	QC=2997,
	ON=4828,
	AB=2080,
	BC=1578,
	MB=938, # combines public and private, maybe separate at some point
	SK=769,
	NB=302, # GNB Policy & Planning, Dept Ed & ECDev April 2017
	NS=370,
	PEI=62
)
# private only
private_sch <- list(
	ON=1943,
	AB=402,
	BC=365
)

total_sch <- list(
	ON=public_sch$ON + private_sch$ON,
	AB=public_sch$AB + private_sch$AB,
	BC=public_sch$BC + private_sch$BC
)

pubdf	<- as.data.frame(do.call("rbind",public_sch))
pubdf$Province <- rownames(pubdf)
colnames(pubdf)[1] <- "PUBLIC"
prdf	<- as.data.frame(do.call("rbind",private_sch))
prdf$Province <- rownames(prdf)
colnames(prdf)[1] <- "PRIVATE"
totdf	<- as.data.frame(do.call("rbind",total_sch))
totdf$Province <- rownames(totdf)

tot <- merge(x=pubdf,y=prdf,by="Province",all.x=TRUE)
tot$PRIVATE[which(is.na(tot$PRIVATE))] <- 0
tot$TOTAL <- tot$PUBLIC + tot$PRIVATE

# combine with daily school count
df$Province <- as.character(df$Province)
x <- merge(x=df,y=tot,by="Province")
x$PUBLIC_PCT <- round((x$Count/x$PUBLIC)*100)
x$TOTAL_PCT <- round((x$Count/x$TOTAL)*100)
x <- x[order(x$Count,decreasing=TRUE),]

x
}

getCoords <- function() {
	coords <- list(
		AB="&ll=55.11842014707787%2C-115.0556102952719&z=6",
		BC="&ll=53.132679251371506%2C-128.97453106268213&z=5",
		SK="&ll=54.68857226971919%2C-110.27476094987723&z=6",
		MB="&ll=53.74476051758362%2C-99.17430471694146&z=6",
		ON="&ll=47.84454183589612%2C-88.18797659194146&z=6",
		QC="&ll=48.39486655697718%2C-74.28622180721197&z=6",
		NB="&ll=46.52185832861437%2C-65.72942947682745&z=7",
		NS="&ll=45.82732191451751%2C-66.61078319517088&z=7",
		PEI="&ll=46.29119004736826%2C-63.9002120375&z=8"
	)
	coords
}

mapLink <- function() {
	return("https://google.com/maps/d/edit?mid=1blA_H3Hv5S9Ii_vyudgDk-j6SfJQil9S&usp=sharing")
}


testIt <- function() {

	outDir <- "/Users/shraddhapai/Documents/Software/covidschoolscanada/nightly-build"
	tweetRes <- list()
	tweetRes$date <- Sys.Date()
	tweetRes[["total_school"]] <- 100
	tweetRes[["total_outbreak"]] <- 50
	df <- data.frame(
		Province=c("AB","ON","QC","MB","SK"),
		Count=c(183,396,678,14,12))
	tweetRes[["num_school"]] <- df
	
	df2 <- data.frame(
		Province=c("AB","ON","QC","MB","SK"),
		Count=c(5,10,15,2,1))
	tweetRes[["outbreaks"]] <- df2

	message("* Testing")
	genTweet(outDir,tweetRes)
	
}

#' generate daily tweet report for covidschoolsCA
#' @param outDir (char) directory to generate tweet
#' @param res (list) data with num schools, outbreaks etc
genTweet <- function(outDir,res) {
	dt <- format(res$date,"%y%m%d")
	pr <- function(i) prettyNum(i,big.mark=",")
	provFull <- list(
		BC="British Columbia",
		ON="Ontario",
		AB="Alberta",
		MB="Manitoba",
		SK="Saskatchewan",
		QC="Québec",
		NB="New Brunswick",
		NS="Nova Scotia",
		PEI="Prince Edward Island"
	)
	outFile <- sprintf("%s/tweetgen_%s.txt",outDir,dt)

	if (file.exists(outFile)) unlink(outFile)
	twf <- file(outFile,"w")
	tryCatch({

message("started tweet")
		# -------------------------
		# TWEET: Overall
		# -------------------------
		x <- sprintf("Good morning. %s\n", emo::ji("coffee"))
		cat(x,file=twf)
		cat("Here is our cumulative view of #COVID19 in Canadian schools.\n\n",
			file=twf)

		cat(sprintf("%s\n",tweetDate()),file=twf)
		cat(sprintf("%s %s schools\n",emo::ji("school"),
			pr(res$total_school)),
			file=twf)
		cat(sprintf("%s %s clusters & outbreaks\n",
			emo::ji("rotating_light"),
			pr(res$total_outbreak)),
			file=twf)
		cat("\n",file=twf)

		cat(sprintf("%s Map: https://covidschoolscanada.org/maps/",
			emo::ji("round_pushpin")),file=twf)
		cat(sprintf("%s Report cases/errors: https://covidschoolscanada.org/submit.html\n", 
			emo::ji("incoming_envelope")),file=twf)
		cat(sprintf("%s%s More CHARTS in thread: \n",
			emo::ji("chart_with_upwards_trend"),
			emo::ji("right_arrow_curving_down")),file=twf)

		cat("/1\n",file=twf)
		cat("\n------------\n",file=twf) # separator

		tweet_ct <-2 

		# -------------------------
		# TWEET: Data
		# -------------------------
		cat("Download everything on the map to date\n",file=twf)
		cat(sprintf("%s\n",
			emo::ji("chart_with_upwards_trend")),file=twf)
		cat("/2\n",file=twf)
		cat("\n------------\n",file=twf) # separator
		tweet_ct <-3 


		# -------------------------
		# TWEET: % Schools
		# -------------------------
		sch <- getSchoolTotals(res$num_school)
		cat(sprintf("%s SCHOOLS, by %% %s\n",
			emo::ji("school"),emo::ji("school")),file=twf)
		for (k in 1:nrow(sch)) {
message(sch$Province[k])
			cat(sprintf("%s - %s / %s (%i%%)",
				sch$Province[k],pr(sch$Count[k]),
				pr(sch$TOTAL[k]),
				sch$TOTAL_PCT[k]),
				file=twf)
			if (sch$Province[k] %in% c("AB","ON")){
				cat(sprintf("%s %i%% / %s public", 
					emo::ji("right_arrow"), sch$PUBLIC_PCT[k],
					pr(sch$PUBLIC[k])),
					file=twf)
			}
			cat("\n",file=twf)
		}
		cat(sprintf("\n/%i",tweet_ct),file=twf)
		cat("\n------------\n",file=twf) # separator
		tweet_ct <- tweet_ct+1

###		# -------------------------
###		# TWEET: School type %
###		# -------------------------
###		dat2 <- tweetRes[["dat_qcStats"]] #subset(dat, Province != "QC")
###		dat2[which(dat2[,"Type_of_school"]=="Middle School"),"Type_of_school"] <- "Elementary"
###		tbl <- table(dat2[,"Type_of_school"])
###		tbl <- data.frame(Type=names(tbl),Count=as.integer(tbl))
###		tbl$Pct <- (tbl$Count/sum(tbl$Count))*100
###		tbl$Type <- as.character(tbl$Type)
###		
###		fun <- function(emostr,x) {
###			p <-tbl$Pct[which(tbl$Type==x)]
###			if (p < 1) p <- "< 1%" else p  <- sprintf("%1.0f %%",p)
###			cat(sprintf("%s %s %s (%s)\n",
###				emo::ji(emostr),
###				pr(tbl$Count[which(tbl$Type==x)]),x,p),
###				file=twf)
###		}
###		cat(sprintf("%s AFFECTED SCHOOL TYPE %s\n",
###			emo::ji("green_apple"),
###			emo::ji("green_apple")),file=twf)
###		cat("Num (% of all affected)\n",file=twf)
###		fun("apple","Elementary")
###		fun("books","Secondary")
###		fun("children_crossing","Mixed")
###		fun("office","Field Office")
###		fun("mortar_board","Post-secondary")
###		fun("question","TBA")
###		cat(sprintf("\n%s By Province (QC updates on Mondays)\n",
###				emo::ji("down_arrow")),file=twf)
###		cat(sprintf("/%i",tweet_ct),file=twf)
###			cat("\n------------\n",file=twf) # separator
###		
###		tweet_ct <- tweet_ct+1

		# -------------------------
		# TWEET: PROVINCE-LEVEL
		# -------------------------

		cur <- res$outbreaks; colnames(cur)[2] <- "Outbreaks"
		totcase <- res$totcase
		colnames(totcase)[2] <- "totcase"
		sch <- merge(x=sch,y=cur,by="Province")
		sch <- sch[order(sch$Count,decreasing=TRUE),]
		coords <- getCoords()
		for (k in 1:nrow(sch)) {
			print(k)
			cat("Schools with 1+ confirmed COVID-19 case\n",
				file=twf)
			cat(sprintf("%s %s %s\n", emo::ji("star"), 
				toupper(provFull[[sch$Province[k]]]),emo::ji("star")),
				file=twf)
			cat(sprintf("%s %s CASES\n", emo::ji("microbe"),
				pr(totcase$totcase[which(totcase$Province==sch$Province[k])])),
				file=twf)
			str <- "OUTBREAKS"
			if (sch$Province[k]=="BC") {
				str <- "CLUSTERS & OUTBREAKS"
			} else  if (sch$Province[k]=="QC") {
				str <- "OUTBREAKS (5+ cases)"
			}
			cat(sprintf("%s %s SCHOOLS (%1.1f%% of all)\n%s %s %s\n",
				emo::ji("school"),pr(sch$Count[k]),sch$TOTAL_PCT[k],
				emo::ji("rotating_light"),pr(sch$Outbreaks[k]),str),
				file=twf)
			if (sch$Province[k]=="QC") {
				cat("Source: @CovidEcoles\n",file=twf)
			} else if (sch$Province[k]=="AB"){
				cat("Source: @SOSAlberta\n",file=twf)
			} else if (sch$Province[k]=="BC"){
				cat("Source: BC Schools COVID Tracker\n",file=twf)
			}

			cat(sprintf("\n/%i", tweet_ct),file=twf)
			tweet_ct <- tweet_ct+1
			cat("\n------------\n",file=twf) # separator
		} 

		# -------------------------
		# TWEET: MAP INFO
		# -------------------------
		cat("Points show COVID-19 cases and outbreaks. Reports first confirmed, then added - never any personal info.\n",file=twf)
		cat(sprintf("%s Google Map: %s\n", 
			emo::ji("round_pushpin"), mapLink()),file=twf)
		cat("Yellow:   1+ unlinked case\n",file=twf)
		cat("Orange: Declared outbreak (PHU; 2+ cases <14 days, linked)\n",
				file=twf)
		cat("Purple: Clusters (BC)\n",file=twf)
		cat(sprintf("\n/%i", tweet_ct),file=twf)
		tweet_ct <- tweet_ct+1
		cat("\n------------\n",file=twf) # separator

		# -------------------------
		# TWEET: E-MAIL DISCLAIMER
		# -------------------------
		cat("We ask for e-mail addresses in case we need to contact you to verify info (e.g. get letter from school or PHU).\n\n",
			file=twf)
		cat("We don't mention your email address in the posted case (anonymous tip), and we do NOT sell your email address.\n\n",
			file=twf)
		cat("We just want 100% transparency in our schools.\n",
			file=twf)

		cat(sprintf("\n/%i", tweet_ct),file=twf)
		tweet_ct <- tweet_ct+1
		cat("\n------------\n",file=twf) # separator

		# -------------------------
		# TWEET: NOTES
		# -------------------------
		cat(sprintf("Today's notes: %s\n", emo::ji("pencil")),file=twf)
		cat("\n\n/END\n",file=twf)

	}, error=function(ex){
	}, finally={
		message("Closing file")
		close(twf)
	})
	
}

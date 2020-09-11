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

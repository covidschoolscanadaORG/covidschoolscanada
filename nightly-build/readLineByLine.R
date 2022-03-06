options(warn=2)

inFile <- "/Users/shraddhapai/Google_covidschools/daily_data/Canada_COVID_tracker/export-210330/CanadaMap_QuebecMerge-210330.csv"

con=file(inFile,open="r")
line=readLines(con) 
long=length(line)
for (i in 1:long){
		dat <- read.delim(inFile,sep=",",h=F,as.is=T,nrow=i)
	print(i)
}
close(con)

require(httr)
require(rvest)

dat <- read.delim("MB_schools_raw.txt",sep="\t",h=T,as.is=T)

urlBase <- "https://web36.gov.mb.ca/school/school?action=singleschool&name="


message("fetching html")
num <- dat[5,1]
url <- sprintf("%s%i",urlBase,num)
message(url)

x <- httr::GET(url)
txt <- content(x,"text")
cat(txt,file="output.txt")
html <- read_html("output.txt")

# get school address
school <- html %>% html_nodes("table") %>% html_nodes(xpath = '//*[@id="school"]') %>% html_nodes(xpath='//*[@class="sc_address"]')
school <- as.character(school)
school <- sub("<div class=\\\"sc_address\\\">\\n<div class=\\\"sc_name\\\">","",school) 
school <- sub("<div class=\\\"sc_div\\\">","",school) 
school <- sub("\n","\t",school)
school <- gsub("</div>","",school)
school <- gsub("<div>","",school)
school <- gsub(",<strong>","\t", school)
school <- gsub("<strong>","\t", school)
school <- gsub("</strong>","", school)
school <- gsub("<br>",",", school)
print(school)


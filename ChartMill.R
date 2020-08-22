library(rvest)

#https://www.chartmill.com/stock/quote/GOOG/analyst-ratings
stock_sign = "GOOG"

stock_sign <- as.character(stock_sign)
cm_url <- paste0("https://www.chartmill.com/stock/quote/", stock_sign, "/profile")
# recommendation for stock symbol
stock_rec <- htmltab(doc = cm_url, which = 1, header = 0)


#/html/body/app-root/app-menu/div/mat-sidenav-container/mat-sidenav-content/app-stock/div[2]/div[1]/app-property-display/div/mat-card/mat-card-content/div/div/div/div[5]/div[2]/table
url <- "https://www.chartmill.com/stock/quote/GOOG/profile"
population <- url %>%
  html() %>%
  html_nodes(xpath='/html/body/app-root/app-menu/div/mat-sidenav-container/mat-sidenav-content/app-stock/div[2]/div[1]/app-property-display/div/mat-card/mat-card-content/div/div/div/div[5]/div[2]/table/tbody/tr[4]/td[2]/span/span/span')

head(population)

print("Chartmill Investment Research")
print(stock_rec)

chartmill_stock_price <- function(stock_sign) {
  stock_sign <- as.character(stock_sign)
  cm_url <- paste0("https://www.chartmill.com/stock/quote/", stock_sign, "/technical-analysis")
  stock_rec <- htmltab(doc = cm_url, which = 1, header = 0)
  print("Chartmill Investment Research")
  print(stock_rec)
}
chartmill_stock_price("FINV")

#<div _ngcontent-wpx-c299="">The short term is neutral, but the long term trend is still positive. Not much to worry about for now.</div>
# mypattern = '<p>([^<]*)<span class="posData">([^<]*)</span>([^<]*)</p>'
# Overall <strong>GOOG</strong> gets a technical rating of <strong>8</strong> out of 10. In the last year, <strong>GOOG</strong> was one of the better performers in the market. There are positive signs in the very recent evolution, but the medium term picture is slightly mixed.</p>
stock_sign = "GOOG"
chartmill_url <- "https://www.chartmill.com/stock/quote/GOOG/profile"
thepage = readLines(chartmill_url)
mypattern = '<div _ngcontent-ebw-c289([^<]*)</div>'
datalines = grep(mypattern, thepage, value = TRUE)
getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
gg = gregexpr(mypattern,datalines)
matches = mapply(getexpr,datalines,gg)
result = gsub(mypattern,'\\1', matches)
names(result) = NULL
result1 <- result[1]

chartmill_stock_price("GOOG")
# getting first part of sentence

datalines = grep(mypattern, thepage, value = TRUE)
#names(datalines) = NULL
#print(datalines[1])

chartmill_stock_price("GOOG")


url <- read_html(chartmill_url)


url <- "https://www.chartmill.com/stock/quote/GOOG/technical-analysis"
population <- url %>%
  xml2::read_html() %>%
  html_nodes(xpath='/html/body/app-root/app-menu/div/mat-sidenav-container/mat-sidenav-content/app-stock/div[2]/mat-card[1]/app-ta-report/div[2]/div[1]/div[2]/div')
population <- population[[1]]

head(population)
chartmill_url = "https://www.chartmill.com/stock/quote/GOOG/profile"
cm_url <- read_html(chartmill_url)


words <- url %>%
  html_node("div")

install.packages("htm2txt")
library(htm2txt)
url <- 'https://www.chartmill.com/stock/quote/GOOG/profile'
text <- gettxt(url)
view(text)


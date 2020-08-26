# SCRATCH CODE

#MarketWatch####
market_watch2 <- function(stock_sign) {
  
  url <- paste0("https://www.marketwatch.com/investing/stock/", stock_sign, "/analystestimates")
  stock_data <- htmltab(doc = url, which = 1, header = 0)
  
  rec <- stock_data[1,2]
  rec <- paste0("Analyst Recommendation: ", rec)
  
  pt <- stock_data[1,4]
  pt <- paste0("Price Target: $", pt)
  
  url2 <- paste0("https://www.marketwatch.com/investing/stock/", stock_sign, "/profile")
  thepage = readLines(url2)
  mypattern = '<p class="data lastcolumn">([^<]*)</p>'
  
  pe = grep(mypattern, thepage, value = TRUE)
  pe <- pe[2]
  pe <- pe %>% str_squish()
  pe <- gsub("<p class=\"data lastcolumn\"",'',pe)
  pe <- gsub(">",'', pe)
  pe <- gsub("</p",'', pe)
  pe <- paste0("PE = ", pe)
  
  url3 <- paste0("https://www.marketwatch.com/investing/stock/", stock_sign, "/financials")
  eps <- htmltab(doc = url3, which = 2, header = 0)
  eps <- eps[38,6]
  eps <- paste0("EPS = ", eps)
  
  print("Market Watch")
  print(pe)
  print(eps)
  print(rec)
  print(pt)
  
}


#Financhill####

stock_sign = "mfc"
financhill_url <- paste0("https://financhill.com/stock-forecast/", stock_sign, "-stock-prediction")
url <- read_html(financhill_url)

words <- url %>%
  html_nodes("strong") %>%
  html_text() %>%
  as.data.frame()

view(words)

rec <- words[c(5),]
rec <- as.character(rec)
rec <- unlist(strsplit(rec, split = "\t", fixed = TRUE))[9]

print("Financhill Recommendation")
print(rec)


mfc

tipranks_url <- paste0("https://www.tipranks.com/stocks/", stock_sign, "/forecast")
url <- read_html(tipranks_url)

words <- url %>%
  html_nodes(".paragraph") %>%
  html_text() %>%
  as.data.frame()


#YAHOO FINANCE####

yahoo("sam")

yahoo_url = "https://finance.yahoo.com/quote/goog?p=goog"

url <- read_html(yahoo_url)
words <- url %>%
  html_nodes(".IbBox") %>%
  html_text() %>%
  as.data.frame()

est_return <- words[20,1]
est_return <- as.character(est_return)

# side 1 of info
est_return <- gsub(".*XX", "\\1", est_return)
# side 2 of info
est_return <- gsub("Premium.*", "\\1", est_return)

est_return <- ifelse(grepl(est_return, "-", fixed = FALSE) == TRUE, 
                     gsub("((\\d*))%"," i \\1%", est_return),
                     gsub("((\\d*))-"," i -\\1", est_return))

# isolating fair value from % return
value_ <-  sub(" i.*", "", est_return)    
perc_return <-  sub(".*i ", "", est_return)    
estimates <- paste0(value_, ' (', perc_return, ')')




fair_value_eval <- gsub(".*([[:digit:]])", "\\1", est_return)
fair_value_eval <- gsub(" ([[:digit:]]).*", "\\1", est_return)



fair_value_eval <- gsub(".*XX", "\\1", est_return1)

gsub("(\\d*)(\\D*)\\s*(\\d*)",
     "Floor \\1 Ward \\2 and Bed \\3.",
     gsub(" ", "", "1ED 34"))

#est_return2 <- gsub("[[:digit:]]","", est_return1)


gsub("(\\d*)(\\D*)\\s*(\\d*)",
     "Floor \\1 Ward \\2 and Bed \\3.",
     gsub(" ", "", "1ED 34"))
#a <- gsub("(\\d*)$"," \\1", toyraw$location)

# gsub("(?<=\\d)\\$", " $", mystring, perl=T)

gsub("[[:digit:]]","",my.data)

# gsub("(\\d*)(\\D*)\\s*(\\d*)",
#"Floor \\1 Ward \\2 and Bed \\3.",
#gsub(" ", "", "1ED 34"))



est_return_num <- gsub("% Est. Return", "", est_return)
est_return_num <- as.numeric(est_return_num)

value <- words[19,1]
value <- as.character(value)
value <- gsub(".*XX", "", value)
value <- gsub("1.*","",value)
value <- gsub("2.*","",value)
value <- gsub("3.*","",value)
value <- gsub("4.*","",value)
value <- gsub("5.*","",value)
value <- gsub("6.*","",value)
value <- gsub("7.*","",value)
value <- gsub("8.*","",value)
value <- gsub("9.*","",value)
value <- gsub("0.*","",value)
value <- gsub("Premium.*","",value)


# <span data-reactid="53">Bearish</span>
thepage = readLines(yahoo_url)
mypattern = '<span data-reactid="53">([^<]*)</span>'
datalines = grep(mypattern, thepage, value = TRUE)
view(datalines)

PE <- datalines[2]
PE <- PE %>% str_squish()
PE <- gsub("<p class=\"data lastcolumn\"",'',PE)
PE <- gsub(">",'',PE)
PE <- gsub("</p",'',PE)
pe <-  paste0("PE = ", PE)





url <- read_html(yahoo_url)
words <- url %>%
  html_nodes("") %>%
  html_text()

view(words)

value <- words2[103,1]
returns <- words2[104,1]
values = paste0(value, ' (', returns, ')')

fair_value1_div <- url %>%
  html_nodes("div") %>%
  html_text()

yahoo_url2 = "https://finance.yahoo.com/quote/GOOG?p=GOOG"

url2 <- read_html(yahoo_url2)

fair_value2_div <- url2 %>%
  html_nodes("div") %>%
  html_text()

view(fair_value2)

fv <- fair_value[93]
fv <- gsub(".*Est","",fv)
fv <- gsub(",","",fv)
fv <- as.numeric(fv)

fv <- paste0("Price Target: $", fv)





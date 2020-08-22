library(rvest)
library(dplyr)
library(tidyverse)

market_watch <- function(stock_sign) {
  
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

market_watch("GOOG")


#FASTER VERSION####

market_watch <- function(stock_sign) {
   url <- paste0("https://www.marketwatch.com/investing/stock/", stock_sign, "/analystestimates")
   url <- read_html(url)

   stock_data <- url %>%
      html_nodes("td") %>%
      html_text() %>%
      str_squish() %>%
      as.data.frame()

   rec <- stock_data[2,1]
   rec <- sub(" .*", "", rec)
   print("Market Watch")
   print("Analyst Recommendation")
   print(rec)
  
}

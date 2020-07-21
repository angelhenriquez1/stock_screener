#rm(list = ls())
library(rvest)
library(dplyr)
library(tidyverse)

market_watch <- function(stock_sign) {
  
  stock_sign <- as.character(stock_sign)
  market_watch_url <- paste0("https://www.marketwatch.com/investing/stock/", stock_sign, "/analystestimates")
  stock_data <- htmltab(doc = market_watch_url, which = 1, header = 0)
  stock_rec <- stock_data[1,2]
  stock_target <- stock_data[1,4]
  market_watch_rec <- paste0("Analyst Recommendation: ", stock_rec)
  market_watch_price <- paste0("Price Target: $", stock_target)
  
  market_watch_url2 <- paste0("https://www.marketwatch.com/investing/stock/", stock_sign, "/profile")
  thepage = readLines(market_watch_url2)
  mypattern = '<p class="data lastcolumn">([^<]*)</p>'
  datalines = grep(mypattern, thepage, value = TRUE)
  PE <- datalines[2]
  PE <- PE %>% str_squish()
  PE <- gsub("<p class=\"data lastcolumn\"",'',PE)
  PE <- gsub(">",'',PE)
  PE <- gsub("</p",'',PE)
  pe <-  paste0("PE = ", PE)
  
  market_watch_url <- paste0("https://www.marketwatch.com/investing/stock/", stock_sign, "/financials")
  stock_data <- htmltab(doc = market_watch_url, which = 2, header = 0)
  EPS <- stock_data[38,6]
  eps <- paste0("EPS = ", EPS)
  
  print("Market Watch")
  print(pe)
  print(eps)
  print(market_watch_rec)
  print(market_watch_price)
  
}

market_watch("GOOG")

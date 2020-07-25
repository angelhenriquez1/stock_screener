library(htmltab)
library(tidyverse)
library(rvest)

finviz_stock_price <- function(stock_sign) {
  
  stock_sign <- as.character(stock_sign)
  fv_url <- paste0("https://finviz.com/quote.ashx?t=", stock_sign)
  stock_rec <- htmltab(doc = fv_url, which = 9, header = 0)
  rec <- stock_rec[12,2]
  pe <- stock_rec[1,4]
  PE <- paste0("PE = ", pe)
  eps <- stock_rec[1,6]
  EPS <- paste0("EPS = ", eps)
  target_price <- stock_rec[5,10]
  price_target <- paste0("Price Target = $", target_price)
  
  print("finviz")
  print("Analyst Recommendation")
  print("1-5 (1 Strong Buy | 5 Strong Sell)")
  print(rec)
  print(PE)
  print(EPS)
  print(price_target)
  
}

finviz_stock_price("GOOG")

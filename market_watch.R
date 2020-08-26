library(rvest)
library(dplyr)
library(tidyverse)

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
  rec <- as.character(rec)
  rec <- ifelse(is.na(rec), 'None', rec)
  
  print("Market Watch")
  print("Analyst Recommendation")
  print(rec)
  
}

market_watch("baba")

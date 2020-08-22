rm(list=ls())
library(tidyverse)
library(htmltab)
library(tidyr)


#website is down
zacks_stock_price <- function(stock_sign) {
  
  stock_sign <- as.character(stock_sign)
  mw_url <- paste0("https://www.zacks.com/stock/quote/", stock_sign)
  stock_rec <- htmltab(doc = mw_url, which = 6, header = 0)
  stock_rec <- stock_rec[1,2]
  stock_rec <- gsub('[[:digit:]]+', '', stock_rec)
  
  zack_url <- paste0("https://www.zacks.com/stock/quote/", stock_sign, "/detailed-estimates")
  z_url <- read_html(zack_url)
  words <- z_url %>%
    html_nodes("tr") %>%
    html_text()
  
  words <- words %>% str_squish()
  
  eps <- words[15]
  eps_num <- gsub('EPS', '', eps)
  eps_num <- gsub('TTM', '', eps_num)
  eps_num <- gsub(' ', '', eps_num)
  eps_num <- gsub("\\(|\\)", "", eps_num)
  eps_num <- as.numeric(eps_num)
  
  pe <- words[16]
  pe_num <- gsub('P/E', '', pe)
  pe_num <- gsub('F1', '', pe_num)
  pe_num <- gsub(' ', '', pe_num)
  pe_num <- gsub("\\(|\\)", "", pe_num)
  pe_num <- as.numeric(pe_num)
  
  zack_eps <- paste0("EPS = ", eps_num)
  zack_pe <- paste0("PE = ", pe_num)
  
  mw_url2 <- paste0("https://www.zacks.com/stock/research/", stock_sign, "/industry-comparison")
  stock_avg_rec <- htmltab(doc = mw_url2, which = 4, header = 0)
  stock_avg_rec_text <- "Analyst Recommendation (1=Buy, 5=Sell)"
  stock_avg_rec <- stock_avg_rec[1,2]
  
  print(stock_sign)
  print("Zack's Investment Research Recommendation")
  print(stock_rec)
  print(stock_avg_rec_text)
  print(stock_avg_rec)
  print(zack_pe)
  print(zack_eps)
  
}

zacks_stock_price("goog")

#rm(list = ls())
setwd("~/Desktop/Stock_Calculator")
library(tidyverse)
library(htmltab)
library(rvest)

stock_data <- function(stock_sign) {
  
  # ZACKS STOCK PRICE
  stock_sign <- as.character(stock_sign)
  mw_url <- paste0("https://www.zacks.com/stock/quote/", stock_sign)
  # recommendation for stock symbol
  stock_zack <- htmltab(doc = mw_url, which = 6, header = 0)
  stock_zack <- stock_zack[1,2]
  stock_zack <- gsub('[[:digit:]]+', '', stock_zack)
  
  zack_url <- paste0("https://www.zacks.com/stock/quote/", stock_sign, "/detailed-estimates")
  z_url <- read_html(zack_url)
  words <- z_url %>%
    html_nodes("tr") %>%
    html_text()
  
  words <- words %>% str_squish()
  
  zack_eps <- words[15]
  zack_eps_num <- gsub('EPS', '', zack_eps)
  zack_eps_num <- gsub('TTM', '', zack_eps_num)
  zack_eps_num <- gsub(' ', '', zack_eps_num)
  zack_eps_num <- gsub("\\(|\\)", "", zack_eps_num)
  zack_eps_num <- as.numeric(zack_eps_num)
  
  zack_pe <- words[16]
  zack_pe_num <- gsub('P/E', '', zack_pe)
  zack_pe_num <- gsub('F1', '', zack_pe_num)
  zack_pe_num <- gsub(' ', '', zack_pe_num)
  zack_pe_num <- gsub("\\(|\\)", "", zack_pe_num)
  zack_pe_num <- as.numeric(zack_pe_num)
  
  zack_pos_neg_eps <- ifelse(zack_eps_num > 0, "Positive Returns", "Negative Returns")
  zack_pos_neg_pe <- ifelse(zack_pe_num > 0, "Positive Returns", "Negative Returns")
  
  zack_eps <- paste0("EPS = ", zack_eps_num," | ", zack_pos_neg_eps)
  zack_pe <- paste0("PE = ", zack_pe_num, " | ", zack_pos_neg_pe)
  print(stock_sign)
  print("Zack's Investment Research Recommendation")
  print(stock_zack)
  print(zack_pe)
  print(zack_eps)
  
  # FINVIZ STOCK PRICE
  fv_url <- paste0("https://finviz.com/quote.ashx?t=", stock_sign)
  # recommendation for stock symbol
  fv_stock_rec <- htmltab(doc = fv_url, which = 9, header = 0)
  fv_rec <- fv_stock_rec[12,2]
  fv_pe <- fv_stock_rec[1,4]
  fv_eps <- fv_stock_rec[1,6]
  fv_target_price <- fv_stock_rec[5,10]
  
  print("finviz")
  print("Recommendation")
  print("1-5 (1 Strong Buy | 5 Strong Sell)")
  print(fv_rec)
  # PE
  fv_PE_sign <- ifelse(fv_pe > 0, " Positive PE", " Negative PE")
  fv_PE <- paste0("PE = ", fv_pe, " |", fv_PE_sign)
  print(fv_PE)
  # EPS
  fv_EPS_sign <- ifelse(fv_eps > 0, " Positive EPS", " Negative EPS")
  fv_EPS <- paste0("EPS = ", fv_eps, " |", fv_EPS_sign)
  print(fv_EPS)
  # target price
  fv_tp <- paste0("Target Price = ", fv_target_price)
  print(fv_tp)
  
  # MARKETWATCH STOCK PRICE
  market_watch_url <- paste0("https://www.marketwatch.com/investing/stock/", stock_sign, "/analystestimates")
  # creates variable for stock url
  marketwatch_stock_data <- htmltab(doc = market_watch_url, which = 1, header = 0)
  # gives target price
  marketwatch_stock_rec <- marketwatch_stock_data[1,2]
  marketwatch_stock_target <- marketwatch_stock_data[1,4]
  market_watch_rec <- paste0("Average Recommendation: ", marketwatch_stock_rec)
  market_watch_price <- paste0("Average Target Price: ", marketwatch_stock_target)
  print("Market Watch")
  print(market_watch_rec)
  print(market_watch_price)
  
  # gives pe ratio
  market_watch_url2 <- paste0("https://www.marketwatch.com/investing/stock/", stock_sign, "/profile")
  marketwatch_thepage = readLines(market_watch_url2)
  marketwatch_mypattern = '<p class="data lastcolumn">([^<]*)</p>'
  marketwatch_datalines = grep(marketwatch_mypattern, marketwatch_thepage, value = TRUE)
  marketwatch_PE <- marketwatch_datalines[2]
  marketwatch_PE <- marketwatch_PE %>% str_squish()
  marketwatch_PE <- gsub("<p class=\"data lastcolumn\"",'',marketwatch_PE)
  marketwatch_PE <- gsub(">",'',marketwatch_PE)
  marketwatch_PE <- gsub("</p",'',marketwatch_PE)
  marketwatch_PE_sign <- ifelse(marketwatch_PE > 0, " PE Positive", " PE Negative")
  marketwatch_pe <-  paste0("PE = ", marketwatch_PE, " |", marketwatch_PE_sign)
  print(marketwatch_pe)
  
  # gives EPS ratio
  market_watch_url2 <- paste0("https://www.marketwatch.com/investing/stock/", stock_sign, "/analystestimates")
  market_watch_eps <- market_watch_url2 %>%
    html() %>%
    html_nodes(xpath='//*[@id="maincontent"]/div[1]/table[2]') %>%
    html_table()
  market_watch_eps <- market_watch_eps[[1]]
  market_watch_eps <- market_watch_eps[41,6]
  marketwatch_eps_sign <- ifelse(market_watch_eps > 0, "EPS Positive", "EPS Negative")
  market_watch_eps <- paste0("EPS = ", market_watch_eps, " | ", marketwatch_eps_sign)
  print(market_watch_eps)
  
  # YAHOO STOCK PRICE
  yahoo_url <- paste0("https://finance.yahoo.com/quote/", stock_sign, "?p=", stock_sign)
  # creates variable for stock url
  stock <- htmltab(doc = yahoo_url, which = 2, header = 0, colNames = c("N.A.", "PE"), length())
  print("Yahoo Finance")
  # gives PE Ratio
  stock_PE <- stock[3,2]
  PE_sign <- ifelse(stock_PE > 0, " PE Positive", " PE Negative")
  PE <- paste0("PE = ", stock_PE, " |", PE_sign)
  print(PE)
  # gives EPS Ratio
  stock_EPS <- stock[4,2]
  EPS_sign <- ifelse(stock_EPS > 0, " EPS Positive", " EPS Negative")
  EPS <- paste0("EPS = ", stock_EPS, " |", EPS_sign)
  print(EPS) 
  
  yahoo_url <- paste0("https://finance.yahoo.com/quote/", stock_sign, "?p=", stock_sign)
  url <- read_html(yahoo_url)
  words <- url %>%
    html_nodes(".IbBox") %>%
    html_text() %>%
    as.data.frame()
  
  est_return <- words[21,1]
  est_return <- as.character(est_return)
  est_return_num <- gsub("% Est. Return", "", est_return)
  est_return_num <- as.numeric(est_return_num)
  pos_neg <- ifelse(est_return_num > 0, "Positive Returns", "Negative Returns")
  est_returns <- paste0(est_return, " | ", pos_neg)
  print(est_returns)
  
  fair_value <- words[19,1]
  fair_value <- as.character(fair_value)
  fair_value <- gsub(".*XX", "", fair_value)
  fair_value <- gsub("1.*","",fair_value)
  fair_value <- gsub("2.*","",fair_value)
  fair_value <- gsub("3.*","",fair_value)
  fair_value <- gsub("4.*","",fair_value)
  fair_value <- gsub("5.*","",fair_value)
  fair_value <- gsub("6.*","",fair_value)
  fair_value <- gsub("7.*","",fair_value)
  fair_value <- gsub("8.*","",fair_value)
  fair_value <- gsub("9.*","",fair_value)
  fair_value <- gsub("0.*","",fair_value)
  fair_value <- gsub("Premium.*","",fair_value)
  print(fair_value)
  # CNN STOCK PRICE
  print("CNN Money")
  #stock_sign <- as.character(stock_sign)
  cnn_url <- paste0("https://money.cnn.com/quote/forecast/forecast.html?symb=", stock_sign)
  thepage = readLines(cnn_url)
  
  # getting first part of sentence
  mypattern = '<p>([^<]*)<span class="posData">([^<]*)</span>([^<]*)</p>'
  datalines = grep(mypattern, thepage, value = TRUE)
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(mypattern,datalines)
  matches = mapply(getexpr,datalines,gg)
  result = gsub(mypattern,'\\1', matches)
  names(result) = NULL
  result <- result[1]
  
  # Positive or Negative Stock Evaluation Percentage
  mypattern2 = '<span class="posData">([^<]*)</span>([^<]*)</p>'
  datalines2 = grep(mypattern2, thepage, value = TRUE)
  
  getexpr2 = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg2 = gregexpr(mypattern2,datalines2)
  matches2 = mapply(getexpr2,datalines2,gg2)
  result2 = gsub(mypattern2,'\\1',matches2)
  names(result2) = NULL
  result2 <- result2[1]
  
  # getting last part of sentence
  mypattern3 = '</span>([^<]*)</p>'
  datalines3 = grep(mypattern3, thepage, value = TRUE)
  
  getexpr3 = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg3 = gregexpr(mypattern3,datalines3)
  matches3 = mapply(getexpr3,datalines3,gg3)
  result3 = gsub(mypattern3,'\\1',matches3)
  names(result3) = NULL
  result3 <- result3[1]
  
  # first part of analyst recommendations
  mypattern4 = '<p>([^<]*)<strong'
  datalines4 = grep(mypattern4, thepage, value = TRUE)
  getexpr4 = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg4 = gregexpr(mypattern4,datalines4)
  matches4 = mapply(getexpr4,datalines4,gg4)
  result4 = gsub(mypattern4,'\\1', matches4)
  names(result4) = NULL
  result4 <- result4[1]
  
  # analyst recommendation
  mypattern5 = '<strong class="wsod_rating">([^<]*)</strong>'
  
  datalines5 = grep(mypattern5, thepage, value = TRUE)
  getexpr5 = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg5 = gregexpr(mypattern5,datalines5)
  matches5 = mapply(getexpr5,datalines5,gg5)
  result5 = gsub(mypattern5,'\\1', matches5)
  names(result5) = NULL
  result5 <- result5[1]
  
  # rating changes
  mypattern6 = '</strong>([^<]*)<span class="wsod_rating">'
  
  datalines6 = grep(mypattern6, thepage, value = TRUE)
  getexpr6 = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg6 = gregexpr(mypattern6,datalines6)
  matches6 = mapply(getexpr6,datalines6,gg6)
  result6 = gsub(mypattern6,'\\1', matches6)
  names(result6) = NULL
  result6 <- result6[1]
  
  # past rating
  mypattern7 = '<span class="wsod_rating">([^<]*)</span>'
  datalines7 = grep(mypattern7, thepage, value = TRUE)
  getexpr7 = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg7 = gregexpr(mypattern7,datalines7)
  matches7 = mapply(getexpr7,datalines7,gg7)
  result7 = gsub(mypattern7,'\\1', matches7)
  names(result7) = NULL
  result7 <- result7[1]
  
  # cnn pe
  cnn_url2 <- paste0("https://money.cnn.com/quote/quote.html?symb=", stock_sign)
  cnn_PE <- htmltab(doc = cnn_url2, which = 4, header = 0)
  cnn_PE <- cnn_PE[5,2]
  cnn_PE <- as.numeric(cnn_PE)
  cnn_pos_neg_pe <- ifelse(cnn_PE > 0, "Positive Returns", "Negative Returns")
  cnn_pe <- paste0("PE = ", cnn_PE, " | ", cnn_pos_neg_pe)
  
  # combining sentences
  results1 <- paste0(result, result2, result3) 
  results2 <- paste0(result4, result5, result6, result7, " rating.")
  print(cnn_pe)
  print(results1)
  print(results2)
  
}

stock_data("faf")

stock_data("goog")

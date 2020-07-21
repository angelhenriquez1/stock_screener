#rm(list = ls())
setwd("~/Desktop/Stock_Calculator")
library(tidyverse)
library(htmltab)
library(rvest)

stock_data <- function(stock_sign) {
  
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
  
  yahoo <- function(stock_sign){
    
    yahoo_url <- paste0("https://finance.yahoo.com/quote/", stock_sign, "?p=", stock_sign)
    stock <- htmltab(doc = yahoo_url, which = 2, header = 0, colNames = c("N.A.", "PE"), length())
    
    pe <- stock[3,2]
    pe <- paste0("PE = ", pe)
    
    eps <- stock[4,2]
    eps <- paste0("EPS = ", eps)
    
    url <- read_html(yahoo_url)
    words2 <- url %>%
      html_nodes("div") %>%
      html_text() %>%
      as.data.frame()
    
    patternA <- words2[73,1]
    patternA <- gsub("(ish).*", "\\1", patternA)
    
    patternB <- words2[74,1]
    
    pattern <- paste0("Pattern = ", patternA, ' (', patternB, ')')
    
    value <- words2[103,1]
    return <- words2[104,1]
    value = paste0(value, ' (', return, ')')
    
    fair_value <- url %>%
      html_nodes("div") %>%
      html_text()
    
    fv <- fair_value[93]
    fv <- gsub(".*Est","",fv)
    fv <- gsub(",","",fv)
    fv <- as.numeric(fv)
    
    fv <- paste0("Price Target: $", fv)
    
    print("Yahoo Finance")
    print(pe)
    print(eps)
    print(fv)
    print(pattern)
    print(value)
    
  }
  
  cnn_money <- function(stock_sign) {
    
    cnn_url <- paste0("https://money.cnn.com/quote/forecast/forecast.html?symb=", stock_sign)
    url <- read_html(cnn_url)
    
    words1 <- url %>%
      html_nodes("p") %>%
      html_text() %>%
      as.data.frame()
    
    forecast <- words1[2,1]
    forecast <- gsub("rating..*","\\2",forecast)
    
    rec <- words1[3,1] 
    rec <- gsub("Move.*","\\1",rec)
    
    print("CNN Money")
    print(forecast)
    print(rec)
    
  }
  
  stock_analysis <- function(stock_sign){
    zacks_stock_price(stock_sign)
    finviz_stock_price(stock_sign)
    market_watch(stock_sign)
    yahoo(stock_sign)
    cnn_money(stock_sign)
    
  }
  
  stock_analysis(stock_sign)
  
}

stock_data("mbt")

stock_data("este")

stock_data("sbow")

stock_data("qd")

stock_data("finv")

stock_data("zm")

stock_data("azek")

stock_data("xyf")

stock_data("chu")

stock_data("finv")

# long term
stock_data("mbt")

stock_data("qd")

stock_data("doyu")

stock_data("dao")

stock_data("nkla")

stock_data("alb")

stock_data("stmp")

stock_data("knsa")

stock_data("cemi")

stock_data("osk")

stock_data("cece")

stock_data("curo")

stock_data("mwa")

stock_data("nvln")

stock_data("wmt")

stock_data("phio")

stock_data("baba")

stock_data("hx")

stock_data("jfin")

# Retrieves Analyst Recommendations

setwd("~/Desktop/Stock_Calculator")
library(tidyverse)
library(htmltab)
library(rvest)

stock_recs <- function(stock_sign) {
  
  financhill <- function(stock_sign) {
    
    financhill_url <- paste0("https://financhill.com/stock-forecast/", stock_sign, "-stock-prediction")
    url <- read_html(financhill_url)
    
    words <- url %>%
      html_nodes("h4") %>%
      html_text() %>%
      as.data.frame()
    
    rec <- words[c(5),]
    rec <- as.character(rec)
    rec <- unlist(strsplit(rec, split = "\t", fixed = TRUE))[9]
    
    print("Financhill")
    print(rec)
    
  }
  
  zacks_stock_price <- function(stock_sign) {
    
    stock_sign <- as.character(stock_sign)
    mw_url <- paste0("https://www.zacks.com/stock/quote/", stock_sign)
    stock_rec <- htmltab(doc = mw_url, which = 6, header = 0)
    stock_rec <- stock_rec[1,2]
    stock_rec <- gsub('[[:digit:]]+', '', stock_rec)
    
    mw_url2 <- paste0("https://www.zacks.com/stock/research/", stock_sign, "/industry-comparison")
    stock_avg_rec <- htmltab(doc = mw_url2, which = 4, header = 0)
    stock_avg_rec <- stock_avg_rec[1,2]
    
    print("Zack's Investment")
    print(stock_rec)
    print("Analyst Recommendation (1 = Strong Buy, 5 = Strong Sell)")
    print(stock_avg_rec)
    
  }
  
  finviz_stock_price <- function(stock_sign) {
    
    stock_sign <- as.character(stock_sign)
    fv_url <- paste0("https://finviz.com/quote.ashx?t=", stock_sign)
    stock_rec <- htmltab(doc = fv_url, which = 9, header = 0)
    rec <- stock_rec[12,2]
    
    print("finviz")
    print("Analyst Recommendation")
    print("1-5 (1 Strong Buy | 5 Strong Sell)")
    print(rec)
    
  }
  
  market_watch <- function(stock_sign) {
    
    url <- paste0("https://www.marketwatch.com/investing/stock/", stock_sign, "/analystestimates")
    stock_data <- htmltab(doc = url, which = 1, header = 0)
    
    rec <- stock_data[1,2]
    
    print("Market Watch")
    print("Analyst Recommendation")
    print(rec)

  }
  
  yahoo_finance <- function(stock_sign){
    
    yahoo_url <- paste0("https://finance.yahoo.com/quote/", stock_sign, "?p=", stock_sign)

    url <- read_html(yahoo_url)
    words <- url %>%
      html_nodes(".IbBox") %>%
      html_text() %>%
      as.data.frame()
    
    est_return <- words[20,1]
    est_return <- as.character(est_return)
    
    # removing left side
    est_return <- gsub(".*XX", "\\1", est_return)
    # removing right side
    est_return <- gsub("Premium.*", "\\1", est_return)
    
    est_return <- ifelse(grepl(est_return, "-", fixed = FALSE) == TRUE, 
                         gsub("((\\d*))%"," i \\1%", est_return),
                         gsub("((\\d*))-"," i -\\1", est_return))
    
    # isolating fair value from % return
    value_ <-  sub(" i.*", "", est_return)    
    perc_return <-  sub(".*i ", "", est_return)    
    estimates <- paste0(value_, ' (', perc_return, ')')
    
    print("Yahoo Finance")
    print(estimates)
    
  }
  
  cnn_money <- function(stock_sign) {
    
    cnn_url <- paste0("https://money.cnn.com/quote/forecast/forecast.html?symb=", stock_sign)
    url <- read_html(cnn_url)
    
    words1 <- url %>%
      html_nodes("p") %>%
      html_text() %>%
      as.data.frame()
    
    rec <- words1[3,1] 
    rec <- gsub("Move.*","\\1",rec)
    
    print("CNN Money")
    print(rec)
    
  }
  
  stock_analysis <- function(stock_sign){
    
    print(stock_sign)
    financhill(stock_sign)
    zacks_stock_price(stock_sign)
    finviz_stock_price(stock_sign)
    yahoo_finance(stock_sign)
    market_watch(stock_sign)
    cnn_money(stock_sign)
    
  }
  
  stock_analysis(stock_sign)
  
}

stock_recs("iivi")

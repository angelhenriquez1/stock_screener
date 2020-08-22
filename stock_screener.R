#rm(list = ls())
setwd("~/Desktop/Stock_Calculator")
library(tidyverse)
library(htmltab)
library(rvest)

stock_data <- function(stock_sign) {

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
    
    print("Financhill Recommendation")
    print(rec)
    
  }
  
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
  
  yahoo_finance <- function(stock_sign){
    
    yahoo_url <- paste0("https://finance.yahoo.com/quote/", stock_sign, "?p=", stock_sign)
    stock <- htmltab(doc = yahoo_url, which = 2, header = 0, colNames = c("N.A.", "PE"), length())
    
    pe <- stock[3,2]
    pe <- paste0("PE = ", pe)
    
    eps <- stock[4,2]
    eps <- paste0("EPS = ", eps)
    
    yr_est <- stock[8,2]
    yr_est <- paste0("1 Year Price Target: $", yr_est)
    
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
    
    print("Yahoo Finance")
    print(pe)
    print(eps)
    print(yr_est)
    print(estimates)
    
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
    
    financhill(stock_sign)
    #zacks_stock_price(stock_sign) #website is down
    finviz_stock_price(stock_sign)
    market_watch(stock_sign)
    yahoo_finance(stock_sign)
    cnn_money(stock_sign)
    
  }
  
  stock_analysis(stock_sign)
  
}

stock_data("doyu")

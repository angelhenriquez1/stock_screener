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
  
  yahoo_finance <- function(stock_sign) {
    
    yahoo_PE_EPS <- function(stock_sign){
      stock_sign <- as.character(stock_sign)
      yahoo_url <- paste0("https://finance.yahoo.com/quote/", stock_sign, "?p=", stock_sign)
      stock <- htmltab(doc = yahoo_url, which = 2, header = 0, colNames = c("N.A.", "PE"), length())
      print("Yahoo Finance")
      stock_PE <- stock[3,2]
      PE <- paste0("PE = ", stock_PE)
      print(PE)
      stock_EPS <- stock[4,2]
      EPS <- paste0("EPS = ", stock_EPS)
      print(EPS) 
    }
    
    yahoo_fair_value <- function(stock_sign){
      
      stock_sign <- as.character(stock_sign)
      yahoo_url <- paste0("https://finance.yahoo.com/quote/", stock_sign, "?p=", stock_sign)
      
      yahoo_url <- read_html(yahoo_url)
      yahoo_url <- yahoo_url %>%
        html_nodes("div") %>%
        html_text()
      
      fair_value <- yahoo_url[93]
      fair_value <- gsub(".*Est","",fair_value)
      fair_value <- gsub(",","",fair_value)
      fair_value <- as.numeric(fair_value)
      
      yahoo_fair_value <- paste0("Price Target: $", fair_value)
      
      print(yahoo_fair_value)
      
    }
    
    yahoo_returns <- function(stock_sign){
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
      
      print(est_return)
      print(value)
      
    }
    
    yahoo_estimates <- function(stock_sign){
      
      yahoo_PE_EPS(stock_sign)
      yahoo_fair_value(stock_sign)
      yahoo_returns(stock_sign)
      
    }
    
    yahoo_estimates(stock_sign)
    
  }
  
  cnn_money <- function(stock_sign) {
    print("CNN Money")
    cnn_url <- paste0("https://money.cnn.com/quote/forecast/forecast.html?symb=", stock_sign)
    thepage = readLines(cnn_url)
    
    # first part of sentence
    mypattern = '<p>([^<]*)<span class="posData">([^<]*)</span>([^<]*)</p>'
    datalines = grep(mypattern, thepage, value = TRUE)
    getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
    gg = gregexpr(mypattern,datalines)
    matches = mapply(getexpr,datalines,gg)
    result = gsub(mypattern,'\\1', matches)
    names(result) = NULL
    result1 <- result[1]
    
    # Evaluation Percentage
    mypattern2 = '<span class="posData">([^<]*)</span>([^<]*)</p>'
    datalines2 = grep(mypattern2, thepage, value = TRUE)
    getexpr2 = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
    gg2 = gregexpr(mypattern2,datalines2)
    matches2 = mapply(getexpr2,datalines2,gg2)
    result2 = gsub(mypattern2,'\\1',matches2)
    names(result2) = NULL
    result2 <- result2[1]
    
    # last part of sentence
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
    
    cnn_url2 <- paste0("https://money.cnn.com/quote/quote.html?symb=", stock_sign)
    cnn_PE <- htmltab(doc = cnn_url2, which = 4, header = 0)
    cnn_PE <- cnn_PE[5,2]
    cnn_PE <- as.numeric(cnn_PE)
    cnn_pe <- paste0("PE = ", cnn_PE)
    
    # combining sentences
    results1 <- paste0(result1, result2, result3) 
    results2 <- paste0(result4, result5, result6, result7, " rating.")
    print(cnn_pe)
    print(results1)
    print(results2)
    
  }
  
  stock_analysis <- function(stock_sign){
    zacks_stock_price(stock_sign)
    finviz_stock_price(stock_sign)
    market_watch(stock_sign)
    yahoo_finance(stock_sign)
    cnn_money(stock_sign)
    
  }
  
  stock_analysis(stock_sign)
  
}

stock_data("qd")


stock_data("sbow")

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


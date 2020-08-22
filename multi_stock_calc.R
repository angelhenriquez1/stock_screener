setwd("~/Desktop/Stock_Calculator")
library(tidyverse)
library(htmltab)
library(rvest)

stock_data <- function(stock_list) {
  
  change_stocks <- function(stock_list){
    stock_list <- as.list(stock_list)
    stock_list <- as.data.frame(t(stock_list))
    
  }
  
  cnn_money_stock_price_target <- function(stock_list) {
    stock_sign <- as.character(stock_list)
    cnn_url <- paste0("https://money.cnn.com/quote/forecast/forecast.html?symb=", stock_sign)
    thepage = readLines(cnn_url)
    
    # getting first part of sentence
    mypattern = '<p>([^<]*)<span class="posData">([^<]*)</span>([^<]*)</p>'
    datalines = grep(mypattern, thepage, value = TRUE)
    getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
    gg = gregexpr(mypattern,datalines)
    matches = mapply(getexpr,datalines,gg)
    result = gsub(mypattern,'\\1',matches)
    names(result) = NULL
    print(result[1])
    
    # Positive or Negative Stock Evaluation Percentage
    mypattern2 = '<span class="posData">([^<]*)</span>'
    datalines2 = grep(mypattern2, thepage, value = TRUE)
    
    getexpr2 = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
    gg2 = gregexpr(mypattern2,datalines2)
    matches2 = mapply(getexpr2,datalines2,gg2)
    result2 = gsub(mypattern2,'\\1',matches2)
    names(result2) = NULL
    print(result2[1])
    
    # getting last part of sentence
    mypattern3 = '</span>([^<]*)</p>'
    datalines3 = grep(mypattern3, thepage, value = TRUE)
    
    getexpr3 = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
    gg3 = gregexpr(mypattern3,datalines3)
    matches3 = mapply(getexpr3,datalines3,gg3)
    result3 = gsub(mypattern3,'\\1',matches3)
    names(result3) = NULL
    print(result3[1])
    
  }
  
  zacks_stock_price <- function(stock_list) {
    stock_list <- as.character(stock_list)
    mw_url <- paste0("https://www.zacks.com/stock/quote/", stock_list)
    # recommendation for stock symbol
    stock_rec <- htmltab(doc = mw_url, which = 6, header = 0)
    stock_rec <- stock_rec[1,2]
    stock_rec <- gsub('[[:digit:]]+', '', stock_rec)
    # line 1 column 2 is the recommendation for the stock
    print(stock_list)
    print(stock_rec)
  }
  
  calculate_PE <- function(stock_list) {
    #list_of_all_stocks <- as.character(list_of_all_stocks)
    yahoo_url <- paste0("https://finance.yahoo.com/quote/", stock_list, "?p=", stock_list)
    # creates variable for stock url
    stock_PE <- htmltab(doc = yahoo_url, which = 2, header = 0, colNames = c("N.A.", "PE"), length())
    # gives PE Ratio
    stock_PE <- stock_PE[3,2]
    print("PE = ") 
    print(stock_PE)
    print(ifelse(stock_PE > 0, "PE Positive", "PE Negative"))
  }
  
  calculate_EPS <- function(stock_list) {
    stock_list <- as.character(stock_list)
    yahoo_url <- paste0("https://finance.yahoo.com/quote/", stock_list, "?p=", stock_list)
    # creates variable for stock url
    stock_EPS <- htmltab(doc = yahoo_url, which = 2, header = 0, colNames = c("NA", "EPS"))
    # gives EPS Ratio
    stock_EPS <- stock_EPS[4,2]
    print("EPS = ") 
    print(stock_EPS)
    print(ifelse(stock_EPS > 0, "EPS Positive", "EPS Negative"))
  }
  
  zacks_stock_price(stock_list)
  cnn_money_stock_price_target(stock_list)
  calculate_PE(stock_list)
  calculate_EPS(stock_list)
  
}

stock_data("GOOG")

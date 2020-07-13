#rm(list = ls())
library(tidyverse)
library(htmltab)
library(rvest)
setwd("~/Desktop/Stock_Calculator")

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

yahoo_finance("mbt")

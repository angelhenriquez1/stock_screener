#rm(list = ls())
library(tidyverse)
library(htmltab)
library(rvest)
setwd("~/Desktop/Stock_Calculator")

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


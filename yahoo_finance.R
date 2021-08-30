library(tidyverse)
library(htmltab)
library(rvest)

yahoo <- function(stock_sign){
  #Pulling stock data from Yahoo Finance website
  yahoo_url <- paste0("https://finance.yahoo.com/quote/", stock_sign, "?p=", stock_sign)
  stock <- htmltab(doc = yahoo_url, which = 2, header = 0, colNames = c("N.A.", "PE"), length())
  
  #Price to earnings ratio data
  pe <- stock[3,2]
  pe <- paste0("PE = ", pe)
  
  #Earnings per share ratio data
  eps <- stock[4,2]
  eps <- paste0("EPS = ", eps)
  
  #1 year price target
  yr_est <- stock[8,2]
  yr_est <- paste0("1 Year Price Target: $", yr_est)
  
  #Getting estimated return and fair value from Yahoo Finance website
  url <- read_html(yahoo_url)
  words <- url %>%
    html_nodes(".IbBox") %>%
    html_text() %>%
    as.data.frame()
  
  #Estimated return
  est_return <- words[20,1]
  est_return <- as.character(est_return)  
  est_return <- gsub(".*XX", "\\1", est_return)
  est_return <- gsub("Premium.*", "\\1", est_return)
  
  est_return <- ifelse(grepl(est_return, "-", fixed = FALSE) == TRUE, 
                       gsub("((\\d*))%"," i \\1%", est_return),
                       gsub("((\\d*))-"," i -\\1", est_return))
  
  # Isolating fair value from % return
  value_ <-  sub(" i.*", "", est_return)    
  perc_return <-  sub(".*i ", "", est_return)    
  estimates <- paste0(value_, ' (', perc_return, ')')
  
  #Displaying price to earnings, earnings per share, 1 year price forecast, and estimated return
  print("Yahoo Finance")
  print(pe)
  print(eps)
  print(yr_est)
  print(estimates)

}

yahoo("LULU")

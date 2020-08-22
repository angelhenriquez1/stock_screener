library(tidyverse)
library(htmltab)
library(rvest)

yahoo <- function(stock_sign){
  
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

yahoo("LULU")

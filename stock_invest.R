# stockinvest
library(htmltab)
library(tidyverse)

stock_invest <- function(stock_sign){
  
  url <- paste0("https://stockinvest.us/technical-analysis/", stock_sign)
  url <- read_html(url)
  
  rec <- url %>%
    html_nodes("span") %>%
    html_text() %>%
    as.data.frame()
  
  names(rec)[1] <- "words"
  
  rec <- rec %>% filter(grepl("candidate", words))
  
  rec <-   as.data.frame(rec)
  rec <- gsub("candidate.*", "", rec$words)
  print(rec)
  
}

stock_invest("baba")


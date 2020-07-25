library(rvest)
library(htmltab)

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

cnn_money("baba")


markets_insider <- function(stock_sign){
  
  url <- paste0("https://markets.businessinsider.com/analyst/", stock_sign, "/all")
  url <- read_html(url)
  
  rec <- url %>%
    html_nodes("span") %>%
    html_text() %>%
    as.data.frame()
  
  names(rec)[1] <- "list"
  
  rec <- rec[!(rec$list == ""), ]
  rec <- as.data.frame(rec)
  rec <- rec[!(rec$rec == "Futures"), ]
  rec <- sub(" .*", "", rec)
  rec <- rec[1]
  rec <- ifelse(rec == "×", "No Data", rec)
  print("Markets Insider")
  print("1 = Buy | 5 = Sell")
  print(rec)
  
}

markets_insider("baba")


markets_insider <- function(stock_sign){
  
  url <- paste0("https://markets.businessinsider.com/analyst/", stock_sign, "/all")
  url <- read_html(url)
  
  rec <- url %>%
    html_nodes("span") %>%
    html_text() %>%
    as.data.frame()
  
  rec <- rec[!(rec$. == ""), ]
  rec <- as.data.frame(rec)
  rec <- rec[2,1]
  rec <- sub(" .*", "", rec)
  rec <- ifelse(rec == "×", "NA", rec)
  print("Markets Insider")
  print("1 = Buy | 5 = Sell")
  print(rec)
  
}

markets_insider("baba")

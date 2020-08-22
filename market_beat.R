# market beat
stock_sign = "BABA"

market_beat <- function(stock_sign){
url <- paste0("https://www.marketbeat.com/stocks/NYSE/", stock_sign, "/price-target/")
url <- read_html(url)

rec <- url %>%
  html_nodes("td") %>%
  html_text() %>%
  as.data.frame()

rec <- rec[2,1]
rec <- sub(" .*", "", rec)
rec <- ifelse(rec == "Read", "Not NYSE", rec)
print("Market Beat")
print(rec)
}

market_beat("baba")

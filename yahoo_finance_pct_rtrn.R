# yahoo finance, percent return

yahoo_finance <- function(stock_sign){
  
  yahoo_url <- paste0("https://finance.yahoo.com/quote/", stock_sign, "?p=", stock_sign)
  
  url <- read_html(yahoo_url)
  words <- url %>%
    html_nodes(".IbBox") %>%
    html_text() %>%
    as.data.frame()
  
  est_return <- words[20,1]
  est_return <- as.character(est_return)
  
  # removing left side
  est_return <- gsub(".*XX", "\\1", est_return)
  # removing right side
  est_return <- gsub("Premium.*", "\\1", est_return)
  
  est_return <- ifelse(grepl(est_return, "-", fixed = FALSE) == TRUE, 
                       gsub("((\\d*))%"," i \\1%", est_return),
                       gsub("((\\d*))-"," i -\\1", est_return))
  
  # isolating fair value from % return
  value_ <-  sub(" i.*", "", est_return)    
  perc_return <-  sub(".*i ", "", est_return)    
  estimates <- paste0(value_, ' (', perc_return, ')')
  
  print("Yahoo Finance")
  print(estimates)
  
}

data <- c("GOOG")

yahoo_finance(data)




stock_sign = "aapl"
yahoo_url <- paste0("https://finance.yahoo.com/quote/", stock_sign, "?p=", stock_sign)

url <- read_html(yahoo_url)
words <- url %>%
  html_nodes(".IbBox") %>%
  html_text() %>%
  as.data.frame()

est_return <- words[20,1]
est_return <- as.character(est_return)

# removing left side
est_return <- gsub(".*XX", "\\1", est_return)
# removing right side
est_return <- gsub("Premium.*", "\\1", est_return)

print("Yahoo Finance")
print(est_return)






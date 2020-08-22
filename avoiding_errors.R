Data <- data.frame(Pages=c(
  "https://walletinvestor.com/stock-forecast/baba-stock-prediction",
  "https://walletinvestor.com/stock-forecast/goog-stock-prediction",
  "https://walletinvestor.com/stock-forecast/finv-stock-prediction"))


sapply(Data$Pages, function(url){
  tryCatch(
    url <- paste0("https://walletinvestor.com/stock-forecast/", stock_sign, "-stock-prediction"),
    url <- as.character(url),
    url <- read_html(url),
    words <- url %>%
      html_nodes("strong") %>%
      html_text() %>%
      as.data.frame(),
    
    stock_name <- words[3,1],
    recommendation <- words[4,1],
    
    recommendation <- paste0(stock_name, "is a ", recommendation, " long term (1 year) investment."),
    print(recommendation),
    
    error = function(e){NA}    # a function that returns NA regardless of what it's passed
  )
})


########THIS WORKS#####################3
########THIS WORKS#####################3
########THIS WORKS#####################3
data <- sapply(Data$Pages, function(url){
  tryCatch(
    url %>%
      as.character() %>% 
      read_html() %>% 
      html_nodes('strong') %>% 
      html_text(),
    error = function(e){NA},
    na.omit(url)
  )

})
########THIS WORKS#####################3
########THIS WORKS#####################3
########THIS WORKS#####################3





#Data <- data.frame(symbol=c(
#  "https://walletinvestor.com/stock-forecast/baba-stock-prediction",
#  "https://walletinvestor.com/stock-forecast/goog-stock-prediction",
#  "https://walletinvestor.com/stock-forecast/finv-stock-prediction"))


a <- sapply(Data$symbol, function(url){
  tryCatch(
url <- as.character(url),
url <- read_html(url),
words <- url %>%
  html_nodes("strong") %>%
  html_text() %>%
  as.data.frame())

error = function(e){NA}
})

sapply(Data$Pages, function(url){
  tryCatch(
    url %>%
      as.character() %>% 
      read_html() %>% 
      html_nodes('h1') %>% 
      html_text(), 
    error = function(e){NA}    # a function that returns NA regardless of what it's passed
  )
})

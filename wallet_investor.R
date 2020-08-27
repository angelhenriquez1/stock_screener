# stocks.com 

# this works

wallet_investor <- function(stock_sign){
  
  url <- paste0("https://walletinvestor.com/stock-forecast/", stock_sign, "-stock-prediction")
  url <- read_html(url)
  
  words <- url %>%
    html_nodes("strong") %>%
    html_text() %>%
    as.data.frame()
  
  stock_name <- words[3,1]
  recommendation <- words[4,1]
  
  recommendation <- paste0(stock_name, "is a ", recommendation, " long term (1 year) investment.")
  print("Wallet Investor")
  print(recommendation)
  
}





# this one has problem with wording positioning

wallet_investor <- function(stock_sign){
  
  url <- paste0("https://walletinvestor.com/stock-forecast/", stock_sign, "-stock-prediction")
  url <- read_html(url)
  
  words <- url %>%
    html_nodes("p") %>%
    html_text() %>%
    str_squish() %>%
    as.data.frame()
  
  stock_name <- words[3,1]
  recommendation <- words[4,1]
  
  recommendation <- paste0(stock_name, "is a(n) ", recommendation, " long term (1 year) investment.")
  print("Wallet Investor")
  print(recommendation)
  
}

wallet_investor("baba")


# condition 1
stock_sign = "yy"
url <- paste0("https://stocks2.com/", stock_sign, "-stock/buy-or-sell/")
url <- read_html(url)

words <- url %>%
  html_nodes("strong") %>%
  html_text() %>%
  str_squish() %>%
  as.data.frame()

names(words)[1] <- "words"

words$words <- as.character(words$words)
view(words)


# condition 2

stock_sign2 = "goog"
url2 <- paste0("https://stocks2.com/", stock_sign2, "-stock/buy-or-sell/")
url2 <- read_html(url2)

words2 <- url2 %>%
  html_nodes("p") %>%
  html_text() %>%
  str_squish() %>%
  as.data.frame()

names(words2)[1] <- "words"

words2$words <- as.character(words2$words)
view(words2)

relevant_data <- words2[words2$words == "trading"]

words_data <- words2[!apply(words2 == "", 1, all),]

view(words_data)

data <- words_data[2]

view(data)



view(words)
words1 <- as.data.frame.character(words)

rec <- rec %>% filter(grepl("Currently", words))


rec <- words[3,1]
view(rec)
rec1 <- sub("now.*", "", rec)
rec2 <- sub(".*is", "", rec1)

view(rec2)



view(rec)
rec <- rec[2,1]
rec <- sub(" .*", "", rec)
rec <- ifelse(rec == "Read", "Not NYSE", rec)

rec <- words1[3,1]
rec <- gsub("Move.*","\\1",rec)

stock_name <- words[3,1]
recommendation <- words[4,1]

recommendation <- paste0(stock_name, "is a(n) ", recommendation, " long term (1 year) investment.")
print("Wallet Investor")
print(recommendation)


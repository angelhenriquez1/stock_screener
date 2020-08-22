# stocks.com 

# problem is the different wording and positioning of results

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
stock_sign = "baba"
url <- paste0("https://stocks2.com/", stock_sign, "-stock/buy-or-sell/")
url <- read_html(url)

words <- url %>%
  html_nodes("p") %>%
  html_text() %>%
  str_squish() %>%
  as.data.frame()

names(words)[1] <- "words1"

view(words)
words$words <- as.data.frame(words$words)

rec1 <- words$words %>% filter(grepl('Currently', words$words))

# condition 2

stock_sign2 = "virt"
url2 <- paste0("https://stocks2.com/", stock_sign2, "-stock/buy-or-sell/")
url2 <- read_html(url2)

words2 <- url2 %>%
  html_nodes("p") %>%
  html_text() %>%
  str_squish() %>%
  as.data.frame()

names(words2)[1] <- "words"

words2$words <- as.character(words2$words)

rec2 <- words2 %>% filter(grepl('Currently', words2$words))

view(rec1)

view(rec2)








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


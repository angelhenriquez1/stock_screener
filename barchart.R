#https://www.barchart.com/stocks/quotes/BABA
# BAR CHART DATA

bar_chart <- function(stock_sign){

  url <-  paste0("https://www.barchart.com/stocks/quotes/", stock_sign)
  url <- read_html(url)
  
  words <- url %>%
    html_nodes("a") %>%
    html_text() %>%
    str_squish() %>%
    as.data.frame()
  
  names(words)[1] = "rec"
  
  # removing blank rows
  words1 <- words[!apply(is.na(words) | words == "", 1, all),]
  words1 <- as.data.frame(words1)
  
  strong_buy <- subset(words1$words1, words1$words1=="Strong buy")
  strong_buy <- as.character(strong_buy)
  
  buy <- subset(words1$words1, words1$words1=="buy")
  buy <- as.character(buy)
  
  weak_buy <- subset(words1$words1, words1$words1=="Weak buy")
  weak_buy <- as.character(weak_buy)
  
  hold <- subset(words1$words1, words1$words1=="hold")
  hold <- as.character(hold)
  
  weak_sell <- subset(words1$words1, words1$words1=="Weak sell")
  weak_sell <- as.character(weak_sell)
  
  sell <- subset(words1$words1, words1$words1=="sell")
  sell <- as.character(sell)
  
  strong_sell <- subset(words1$words1, words1$words1=="Strong sell")
  strong_sell <- as.character(strong_sell)
  
  opinion <- rbind(strong_buy, buy, all = TRUE)
  opinion <- rbind(opinion, weak_buy, all = TRUE)
  opinion <- rbind(opinion, hold, all=TRUE)
  opinion <- rbind(opinion, weak_sell, all = TRUE)
  opinion <- rbind(opinion, sell, all = TRUE)
  opinion <- rbind(opinion, strong_sell, all = TRUE)
  
  opinion <- as.data.frame(opinion)
  
  opinion <- opinion %>% 
    filter(!grepl('TRUE', opinion$V1))
  
  print("Barchart Recommendation")
  print(opinion)

}

bar_chart("baba")

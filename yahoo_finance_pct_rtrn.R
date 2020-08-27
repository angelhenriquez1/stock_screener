
# finding list of all stock symbols
other <- read_delim("http://ftp.nasdaqtrader.com/dynamic/SymDir/otherlisted.txt",
                          "|", escape_double = FALSE, trim_ws = TRUE)

nasdaq <- read_delim("http://ftp.nasdaqtrader.com/dynamic/SymDir/nasdaqtraded.txt",
                          "|", escape_double = FALSE, trim_ws = TRUE)

stock_names1 <- other[1]
names(stock_names1)[1] <- "stock_symbols"
stock_names2 <- nasdaq[1]
names(stock_names2)[1] <- "stock_symbols"

stock_names <- rbind(stock_names1, stock_names2)
rm(other)
rm(nasdaq)
rm(stock_names1)
rm(stock_names2)

# yahoo finance, percent return
yahoo_finance <- function(stock_sign){
  
  stock_sign <- as.character(stock_sign)
  stock_sign <- tolower(stock_sign)
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
  print(stock_sign)
  ifelse(perc_return > 25, estimates, "")
  #print(estimates)
  
}

yahoo_finance("baba")

data1 <- as.character(stock_names$stock_symbols)

for ( i in data1 ){

  yahoo_finance(i)
  print(" ")

}

# use transmute instead of subsets
a <- transmute(data)

pick_perc_return <- function(stock_sign){
  
  stock_sign <- as.character(stock_sign)
  stock_sign <- tolower(stock_sign)
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

  est_return1 <- regmatches(est_return, gregexpr("[[:digit:]]+", est_return))
  est_return1 <- as.numeric(est_return1)

  #print(stock_sign)
  ifelse(est_return1 > 25, paste0(stock_sign, ": ", est_return1,"%"), "")

}
pick_perc_return("tsla")

data1 <- as.character(stock_names$stock_symbols)

for ( i in data1 ){
  
  more_than_25_percent_return(i)
  print(" ")
  
}





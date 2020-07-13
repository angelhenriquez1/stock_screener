library(rvest)
library(htmltab)

cnn_money <- function(stock_sign) {
  print("CNN Money")
  cnn_url <- paste0("https://money.cnn.com/quote/forecast/forecast.html?symb=", stock_sign)
  thepage = readLines(cnn_url)
  
  # first part of sentence
  mypattern = '<p>([^<]*)<span class="posData">([^<]*)</span>([^<]*)</p>'
  datalines = grep(mypattern, thepage, value = TRUE)
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(mypattern,datalines)
  matches = mapply(getexpr,datalines,gg)
  result = gsub(mypattern,'\\1', matches)
  names(result) = NULL
  result1 <- result[1]
  
  # Evaluation Percentage
  mypattern2 = '<span class="posData">([^<]*)</span>([^<]*)</p>'
  datalines2 = grep(mypattern2, thepage, value = TRUE)
  getexpr2 = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg2 = gregexpr(mypattern2,datalines2)
  matches2 = mapply(getexpr2,datalines2,gg2)
  result2 = gsub(mypattern2,'\\1',matches2)
  names(result2) = NULL
  result2 <- result2[1]
  
  # last part of sentence
  mypattern3 = '</span>([^<]*)</p>'
  datalines3 = grep(mypattern3, thepage, value = TRUE)
  getexpr3 = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg3 = gregexpr(mypattern3,datalines3)
  matches3 = mapply(getexpr3,datalines3,gg3)
  result3 = gsub(mypattern3,'\\1',matches3)
  names(result3) = NULL
  result3 <- result3[1]
  
  # first part of analyst recommendations
  mypattern4 = '<p>([^<]*)<strong'
  datalines4 = grep(mypattern4, thepage, value = TRUE)
  getexpr4 = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg4 = gregexpr(mypattern4,datalines4)
  matches4 = mapply(getexpr4,datalines4,gg4)
  result4 = gsub(mypattern4,'\\1', matches4)
  names(result4) = NULL
  result4 <- result4[1]
  
  # analyst recommendation
  mypattern5 = '<strong class="wsod_rating">([^<]*)</strong>'
  datalines5 = grep(mypattern5, thepage, value = TRUE)
  getexpr5 = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg5 = gregexpr(mypattern5,datalines5)
  matches5 = mapply(getexpr5,datalines5,gg5)
  result5 = gsub(mypattern5,'\\1', matches5)
  names(result5) = NULL
  result5 <- result5[1]
  
  # rating changes
  mypattern6 = '</strong>([^<]*)<span class="wsod_rating">'
  datalines6 = grep(mypattern6, thepage, value = TRUE)
  getexpr6 = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg6 = gregexpr(mypattern6,datalines6)
  matches6 = mapply(getexpr6,datalines6,gg6)
  result6 = gsub(mypattern6,'\\1', matches6)
  names(result6) = NULL
  result6 <- result6[1]
  
  # past rating
  mypattern7 = '<span class="wsod_rating">([^<]*)</span>'
  datalines7 = grep(mypattern7, thepage, value = TRUE)
  getexpr7 = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg7 = gregexpr(mypattern7,datalines7)
  matches7 = mapply(getexpr7,datalines7,gg7)
  result7 = gsub(mypattern7,'\\1', matches7)
  names(result7) = NULL
  result7 <- result7[1]
  
  cnn_url2 <- paste0("https://money.cnn.com/quote/quote.html?symb=", stock_sign)
  cnn_PE <- htmltab(doc = cnn_url2, which = 4, header = 0)
  cnn_PE <- cnn_PE[5,2]
  cnn_PE <- as.numeric(cnn_PE)
  cnn_pe <- paste0("PE = ", cnn_PE)
  
  # combining sentences
  results1 <- paste0(result1, result2, result3) 
  results2 <- paste0(result4, result5, result6, result7, " rating.")
  print(cnn_pe)
  print(results1)
  print(results2)
  
}

cnn_money("GOOG")
cnn_money("BABA")


cnn_money2 <- function(stock_sign) {
  print("CNN Money")
  cnn_url <- paste0("https://money.cnn.com/quote/forecast/forecast.html?symb=", stock_sign)
  thepage = readLines(cnn_url)
  
  # first part of sentence
  mypattern = '<p>([^<]*)<span class="posData">([^<]*)</span>([^<]*)</p>'
  datalines = grep(mypattern, thepage, value = TRUE)
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(mypattern,datalines)
  matches = mapply(getexpr,datalines,gg)
  result = gsub(mypattern,'\\1', matches)
  names(result) = NULL
  result1 <- result[1]
  
  # Evaluation Percentage
  mypattern2 = '<span class="posData">([^<]*)</span>([^<]*)</p>'
  datalines2 = grep(mypattern2, thepage, value = TRUE)
  getexpr2 = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg2 = gregexpr(mypattern2,datalines2)
  matches2 = mapply(getexpr2,datalines2,gg2)
  result2 = gsub(mypattern2,'\\1',matches2)
  names(result2) = NULL
  result2 <- result2[1]
  
  # last part of sentence
  mypattern3 = '</span>([^<]*)</p>'
  datalines3 = grep(mypattern3, thepage, value = TRUE)
  getexpr3 = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg3 = gregexpr(mypattern3,datalines3)
  matches3 = mapply(getexpr3,datalines3,gg3)
  result3 = gsub(mypattern3,'\\1',matches3)
  names(result3) = NULL
  result3 <- result3[1]
  
  # first part of analyst recommendations
  mypattern4 = '<p>([^<]*)<strong'
  datalines4 = grep(mypattern4, thepage, value = TRUE)
  getexpr4 = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg4 = gregexpr(mypattern4,datalines4)
  matches4 = mapply(getexpr4,datalines4,gg4)
  result4 = gsub(mypattern4,'\\1', matches4)
  names(result4) = NULL
  result4 <- result4[1]
  
  # analyst recommendation
  mypattern5 = '<strong class="wsod_rating">([^<]*)</strong>'
  datalines5 = grep(mypattern5, thepage, value = TRUE)
  getexpr5 = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg5 = gregexpr(mypattern5,datalines5)
  matches5 = mapply(getexpr5,datalines5,gg5)
  result5 = gsub(mypattern5,'\\1', matches5)
  names(result5) = NULL
  result5 <- result5[1]
  
  # rating changes
  mypattern6 = '</strong>([^<]*)<span class="wsod_rating">'
  datalines6 = grep(mypattern6, thepage, value = TRUE)
  getexpr6 = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg6 = gregexpr(mypattern6,datalines6)
  matches6 = mapply(getexpr6,datalines6,gg6)
  result6 = gsub(mypattern6,'\\1', matches6)
  names(result6) = NULL
  result6 <- result6[1]
  
  # past rating
  mypattern7 = '<span class="wsod_rating">([^<]*)</span>'
  datalines7 = grep(mypattern7, thepage, value = TRUE)
  getexpr7 = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg7 = gregexpr(mypattern7,datalines7)
  matches7 = mapply(getexpr7,datalines7,gg7)
  result7 = gsub(mypattern7,'\\1', matches7)
  names(result7) = NULL
  result7 <- result7[1]
  
  cnn_url2 <- paste0("https://money.cnn.com/quote/quote.html?symb=", stock_sign)
  cnn_PE <- htmltab(doc = cnn_url2, which = 4, header = 0)
  cnn_PE <- cnn_PE[5,2]
  cnn_PE <- as.numeric(cnn_PE)
  cnn_pe <- paste0("PE = ", cnn_PE)
  
  # combining sentences
  results1 <- paste0(result1, result2, result3) 
  results2 <- paste0(result4, result5, result6, result7, " rating.")
  
  print(a)
  print(cnn_pe)
  print(results1)
  print(results2)
  
}
cnn_money2("BABA")
no_works = "BABA"
works = "GOOG"

if (x < 0) {
  print("Negative number")
} else if (x > 0) {
  print("Positive number")
} else print("Zero")

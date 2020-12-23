#webscraping resources:
#https://awesomeopensource.com/project/yusuzech/r-web-scraping-cheat-sheet#rvest4

library(rvest)
library(reshape2)
library(lubridate)
library(httr)

symbols <- readRDS(file = 'Nasdaq/data/symbols_extra_clean.RDS')
head(symbols)

read_html(curl('http://google.com', handle = curl::new_handle("useragent" = "Mozilla/5.0")))
symbol.list <- symbols[,1]

uastring <- c(
"Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36",
"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36",
"Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_3) AppleWebKit/537.75.14 (KHTML, like Gecko) Version/7.0.3 Safari/7046A194A",
"Opera/9.80 (Windows NT 6.1; Opera Tablet/15165; U; en) Presto/2.8.149 Version/11.1",
"Mozilla/5.0 (Windows NT 10.0; WOW64; rv:77.0) Gecko/20100101 Firefox/77.0",
"Mozilla/5.0 (compatible, MSIE 11, Windows NT 6.3; Trident/7.0;  rv:11.0) like Gecko",
"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36",
"Mozilla/5.0")

session <- html_session("https://finance.yahoo.com/quote/", user_agent(uastring[1]))



which(symbol.list=='AAPL')
i=7
temp.df <- data.frame()
temp.df.finc <- data.frame()
startTime <- Sys.time()
for(i in 1:length(symbol.list)){
  cat('.')
  symbol.text <- symbol.list[i]
  robust1 <- 'a'
  skip=0
  while(robust1!='Var.1' & skip<5){
    t0 <- Sys.time()
    url <- paste0("https://finance.yahoo.com/quote/",symbol.text,"/key-statistics?p=",symbol.text)
    # session <- html_session(url, user_agent("Mozilla/5.0"))
    # session <- html_session(url, user_agent(uastring[1]))
    session <- session%>%jump_to(url)
    t1 <- Sys.time()
    response_delay <- as.numeric(t1-t0)
    table <- session%>%html_nodes("table" )%>%.[[1]]%>%html_table(fill=TRUE)
    
    aux <- data.frame(table)  
    robust1 <- colnames(aux)[1]
    skip=skip+1

    if(robust1!='Var.1') {
      
      Sys.sleep(sample(40, 1) * 0.1)
      cat('-')
      if(skip > 3) session <- html_session("https://finance.yahoo.com/quote/", user_agent(uastring[
        sample(1:length(uastring),1)
        ]))
    }
  }
  

  if(skip==5) {
    cat('@')
    Sys.sleep(sample(10, 1) )
    session <- html_session("https://finance.yahoo.com/quote/", user_agent(uastring[
      sample(1:length(uastring),1)
    ]))
    } else {
  
    a <- melt(aux,id.vars = 'Var.1')
    b <- dcast(a,variable~Var.1)
    colnames(b) <- c('Finc_dt','Value','Value_EBITDA','Value_Revenue','F_P_E','Market_Cap','PEG','Price_book','Price_sales','P_E')
    b$Finc_dt <- substring(b$Finc_dt, 2)
    b$Finc_dt <- gsub("\\.","-",b$Finc_dt) 
    b$Finc_dt <- parse_date_time(b$Finc_dt, orders="mdy")
    b$Symbol <- symbol.text
    
    temp.df.finc <- rbind(temp.df.finc,data.frame(b))
    
    t2 <- Sys.time()
    waiting_delay <- as.numeric(t2-t1)
    final_delay=pmax(response_delay-waiting_delay,0)
    Sys.sleep(2*final_delay)
    }
  
  
  if(i%%10==0) {
    Sys.sleep(sample(10, 1) * 0.1)
    print(paste0(i,'  ',Sys.time()-startTime))
    startTime <- Sys.time()
    session <- html_session("https://finance.yahoo.com/quote/", user_agent(uastring[
      sample(1:length(uastring),1)
    ]))
  }
}
  
temp.df.finc

  
tail(temp.df.finc)
# Get Ex-dividend
# cat('.')
# url <- paste0("https://finance.yahoo.com/quote/",symbol.text,"?p=",symbol.text)
# webpage <- read_html(url)
# table <- data.frame(webpage %>%   html_nodes("table" )%>%.[[2]]%>%html_table(fill=TRUE))
# a <- dcast(table,symbol.text~X1,value.var = 'X2')
# a <- a[c(1,3,6,7)]
# colnames(a) <- c('Symbol','Beta','Div_dt','Div_pct')
# a$Div_pct <-  qdapRegex::ex_between(a$Div_pct, "(", ")")[[1]]
# a$Div_dt <- parse_date_time(a$Div_dt,orders = 'mdy')
#   
# 
#   
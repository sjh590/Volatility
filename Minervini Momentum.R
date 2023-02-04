#Minervini Stock Selection Criteria

#Test for TWSE
#TWSE tickers
library(rvest)
twse_raw <- read_html("https://isin.twse.com.tw/isin/C_public.jsp?strMode=2")
twse_raw
temp <- html_table(twse_raw)
twse_tickers_raw <- temp[[2]]$X1
twse_tickers_raw


#clean html using stringr
library(stringr)
tickers <- str_extract(twse_tickers_raw,"^[1-9][0-9]{3}") #Extract Common Stock tickers
tickers <- tickers[!is.na(tickers)] %>% paste(".TW", sep = "") #Add .TW to fetch data from yfinance
write.csv(tickers, file = "twse_tickers.csv", row.names = FALSE)

tickers <- read.csv("twse_tickers.csv", header = TRUE)$x




#S&P 500 tickers
library(rvest)
sp500_raw <- read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies")
temp <- html_table(sp500_raw)
temp <- temp[[1]]
tickers <- temp$Symbol

#Russell 3000
Russell_df <- read.csv("IWV_holdings.csv", header = TRUE)
names(Russell_df) <- "tickers"
tickers <- Russell_df$tickers

#time variables
library(quantmod)
library(lubridate)

enddate <- today()-1  
startdate <- enddate %m-% years(10)
end_n <- as.numeric(enddate-startdate-365)
time <- sort(c(0, sample(end_n, 98), end_n)) #important, N testing periods


#get financial data from yfinance
#get index return first
#index <- "^TWII" #Taiwan Stock Index
index <- "^GSPC"
#need to set env = NULL, so the function will retun data directly
#the Adjusted Price is the 6th column, need to check
index_df <- getSymbols(index, from = startdate, end = enddate, src = "yahoo", env = NULL)[,6] 

#get cumulative return subsetted by time
index_cum_percentage <- c()
for (i in 1:length(time)) {
  temp_df <- index_df[startdate + time[i] <= index(index_df) & startdate + time[i] +365 >= index(index_df)]
  index_cum_percentage[i] <- tail(temp_df,1)[[1]]/head(temp_df,1)[[1]]-1 
}



#next, then get individual stock return 
for (i in 1:length(tickers)){
  try({
    df <- getSymbols(tickers[i], from = startdate, end = enddate, src = "yahoo", env = NULL)
    df <- df[,c(1,2,3,6)] #only need Open, High, Low, Adjusted Columns
    colnames(df) <- c("Open","High", "Low", "Price")
    write.zoo(df, paste(tickers[i], "csv", sep = "."))
  })
}

#initiate empty list to store individual stock dfs by time 
df_list <- list()
for (i in 1:length(time)) {
  temp_df <- data.frame(matrix(ncol = 2))
  names(temp_df) <- c("tickers", "return_multiple")
  
  df_list[[i]] <- temp_df
}

#subset individual stock dfs by time 
for (i in 1:length(tickers)){
  try({
    df<- read.zoo(paste(tickers[i], "csv", sep = "."), header = TRUE)
    for (j in 1:length(time)) {
      temp_df <- df[startdate + time[j] <= index(df) & startdate + time[j] +365 >= index(df)]
      
      if (length(temp_df)==0) {
        df_list[[j]][i,1] <- tickers[i]
        df_list[[j]][i,2] <- NA
      }else{
        stock_cum_percentage <- tail(temp_df$Price,1)[[1]]/head(temp_df$Price,1)[[1]]-1
        return_multiple <- round((1+stock_cum_percentage)/(1+index_cum_percentage[j]),2)
        
        df_list[[j]][i,1] <- tickers[i]
        df_list[[j]][i,2] <- return_multiple
      }
    }
  })  
}
#Sys.sleep(2) use this if working on a old computer 



#filter top 10% of stocks by RS Rating
library(dplyr)
#need to re-initiate list of dfs when re-running
input_list <- df_list
for (i in 1:length(time)) {
  input_df <- df_list[[i]] %>% mutate(RS_rating = percent_rank(return_multiple)) %>%
    filter(RS_rating >= 0.9) %>%
    arrange(desc(RS_rating))
  input_list[[i]] <- input_df 
}

#initiate empty list to store selections
selection <- lapply(seq(length(time)), c)
failed <- lapply(seq(length(time)), c)
iter <- 0
#check filter conditions
for (i in 1:length(time)) {
  input_df <- input_list[[i]]
  iter <- iter +1
  print(paste("Iteration", iter, sep = " "))
  
  n <- 0 #important, initiate selected count
  f <- 0 #important, initiate failed count
  for (j in input_df$tickers) {

    try({
      df <- NULL
      df <- read.zoo(paste(j, "csv", sep = "."), header = TRUE)  
      df <- df[startdate + time[i] <= index(df) & startdate + time[i] +365 >= index(df)]
      
      #store needed values
      currentClose <- tail(df$Price,1)[[1]]
      current_sma_20 <- tail(SMA(df$Price,20), 1)[[1]]
      current_sma_50 <- tail(SMA(df$Price,50), 1)[[1]]
      current_sma_100 <- tail(SMA(df$Price,100), 1)[[1]]
      low_of_52week <- min(df$Low)
      high_of_52week <- max(df$High)
      
      
      #trending moving average for 100 SMA
      trending_sma_100 <- SMA(df$Price, 100)[length(df$Price)-20][[1]]
      
      #Condition 1: Current Price > 20, 50 and 100 SMA
      condition1 <- currentClose > current_sma_20 & current_sma_50 & current_sma_100
      #Condition 2: 50 SMA > 100 SMA
      condition2 <- current_sma_50 > current_sma_100
      #Condition 3: 150 SMA trending upwards 
      condition3 <- current_sma_100 > trending_sma_100
      #Condition 4: 20 MA > 50 SMA and > 100 SMA
      condition4 <- current_sma_20 > current_sma_50 & current_sma_100
      #Condition 5: Current Price >= 30% above 52 week low
      condition5 <- currentClose >= 1.3*low_of_52week
      #Condition 6: Current Price >= 75% of 52 week high
      condition6 <- currentClose >= 0.75*high_of_52week
      
      
      
      if(condition1 & condition2 & condition3 & condition4 & condition5 & condition6){
        n <- n + 1
        selection[[i]][n] <- j
        #print(paste(j, "met the Minervini conditions", sep = " "))
        
      } else {
        f <- f + 1
        failed[[i]][f] <- j
        #print(paste(j, "failed the Minervini conditions", sep = " "))
      }
    })
  }
}

#selection <- failed

#backtest forward 3mo & 6mo performance
#initiate empty list to store backtest dfs
backtest_list <- list()
for (i in 1:(length(time)-1)) {#can not test the last one, since no data for future next 3 month
  temp_df <- data.frame(matrix(ncol = 7))
  names(temp_df) <- c("tickers", "Stock_1mo_Return", "Stock_3mo_Return", "Stock_6mo_Return", "Index_1mo_Return", "Index_3mo_Return", "Index_6mo_Return")
  
  backtest_list[[i]] <- temp_df
}

for (i in 1:(length(time)-1)) { #can not test the last one, since no data for future next 3 month
  n <- 0 #initiate counter to save ticker
  try({
    for (j in selection[[i]]) {
      df3 <- read.zoo(paste(j, "csv", sep = "."), header = TRUE)  
      df_1mo <- df3[startdate + time[i] +365 <= index(df3) & startdate + time[i] +365 + 30 >= index(df3)]
      stock_1mo_percentage <- tail(df_1mo$Price,1)[[1]]/head(df_1mo$Price,1)[[1]]-1
      
      
      df_3mo <- df3[startdate + time[i] +365 <= index(df3) & startdate + time[i] +365 + 90 >= index(df3)]
      stock_3mo_percentage <- tail(df_3mo$Price,1)[[1]]/head(df_3mo$Price,1)[[1]]-1
      
      df_6mo <- df3[startdate + time[i] +365 <= index(df3) & startdate + time[i] +365 + 180 >= index(df3)]
      stock_6mo_percentage <- tail(df_6mo$Price,1)[[1]]/head(df_6mo$Price,1)[[1]]-1
      
      
      index_1mo_df <- index_df[startdate + time[i] +365 <= index(index_df) & startdate + time[i] +365 + 30 >= index(index_df)]
      index_1mo_percentage <- tail(index_1mo_df,1)[[1]]/head(index_1mo_df,1)[[1]]-1
      
      index_3mo_df <- index_df[startdate + time[i] +365 <= index(index_df) & startdate + time[i] +365 + 90 >= index(index_df)]
      index_3mo_percentage <- tail(index_3mo_df,1)[[1]]/head(index_3mo_df,1)[[1]]-1
      
      index_6mo_df <- index_df[startdate + time[i] +365 <= index(index_df) & startdate + time[i] +365 + 180 >= index(index_df)]
      index_6mo_percentage <- tail(index_6mo_df,1)[[1]]/head(index_6mo_df,1)[[1]]-1
      
      n <- n + 1
      backtest_list[[i]][n,1] <- j
      backtest_list[[i]][n,2] <- stock_1mo_percentage
      backtest_list[[i]][n,3] <- stock_3mo_percentage
      backtest_list[[i]][n,4] <- stock_6mo_percentage
      backtest_list[[i]][n,5] <- index_1mo_percentage
      backtest_list[[i]][n,6] <- index_3mo_percentage
      backtest_list[[i]][n,7] <- index_6mo_percentage
    }
  })
}


backtest_output_df <- data.frame(matrix(ncol = 6))
names(backtest_output_df) <- c("Average_1mo_Return", "Average_3mo_Return", "Average_6mo_Return", "Benchmark_1mo_Return", "Benchmark_3mo_Return", "Benchmark_6mo_Return")
for (i in 1:(length(time)-1)) {
  temp_df <- lapply(backtest_list[[i]][2:7], mean)
  names(temp_df) <- c("Average_1mo_Return", "Average_3mo_Return", "Average_6mo_Return", "Benchmark_1mo_Return", "Benchmark_3mo_Return", "Benchmark_6mo_Return")
  backtest_output_df <- rbind(backtest_output_df,temp_df)
}

backtest_output_df <- backtest_output_df %>%
  mutate("1mo_Outperform" = Average_1mo_Return - Benchmark_1mo_Return) %>%
  mutate("3mo_Outperform" = Average_3mo_Return - Benchmark_3mo_Return) %>%
  mutate("6mo_Outperform" = Average_6mo_Return - Benchmark_6mo_Return) %>%
  na.omit()

summary(backtest_output_df)

selection[[length(time)]]

#plot selection, only need to plot the last time period, since rest is irrelevant
library(ggplot2)
library(tidyquant) #for advanced stock charting
library(tibble)
library(ggthemes)
for (i in selection[[length(time)]]){ #the last selection
  df2 <- read.zoo(paste(i, "csv", sep = "."), header = TRUE)
  df2 <- df2 %>% 
    as.data.frame() %>%
    rownames_to_column("Date") %>%
    mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
    mutate(Year = year(Date)) %>%
    filter(Year == 2022)
  
  
  p <- ggplot(data = df2, aes(x = Date, y = Price, open = Open, high = High, low = Low, close = Price)) +
    geom_candlestick(fill_up = "darkgreen", fill_down = "red") +
    geom_ma(ma_fun = SMA, n = 50, color = "lightblue", linetype = "solid", lwd = 0.3) +
    geom_ma(ma_fun = SMA, n = 10, color = "blue", linetype = "solid", lwd = 0.3) +
    geom_bbands(ma_fun = SMA, sd = 2, n = 20, color_bands = "grey") +
    labs(title = i, y = "Price", x = "") + 
    scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m-%d") +
    theme_tq()
  
  png_name <- paste(i, tail(time,1), sep = "_") 
  ggsave(paste(png_name, "png" , sep = "."))
}

a <- 1-sum(backtest_output_df$`1mo_Outperform` < 0)/length(backtest_output_df$`1mo_Outperform`)
b <- 1-sum(backtest_output_df$`3mo_Outperform` < 0)/length(backtest_output_df$`3mo_Outperform`)
c <- 1-sum(backtest_output_df$`6mo_Outperform` < 0)/length(backtest_output_df$`6mo_Outperform`)


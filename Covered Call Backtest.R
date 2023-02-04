#SPY Covered Call Strategy

#Libraries
library(quantmod)
library(PerformanceAnalytics)
library(lubridate)
library(dplyr)

#The Strategy
#1. Buy 100 shares of SPY
#2. Sell 1 OTM monthly call
#3. Buy 1 OTM montthly put

#Load Data
enddate <- today()
startdate <- enddate %m+% months(-60)

#Index data
index_df <- getSymbols("SPY", from = startdate, to = enddate, src = "yahoo", env = NULL, periodicity = "monthly")[,6]
#create two dfs, one lagged
n <- length(index_df)
S_t <- coredata(index_df[-n+1:-n])
S_t1 <- coredata(index_df[c(-1, -n)])
  

#VIX data, as a proxy for volatility
vix_df <- getSymbols("^VIX", from = startdate, to = enddate, src = "yahoo", env = NULL, periodicity = "monthly")[,6]
sigma_t <- coredata(vix_df[-n+1:-n])/100

#Risk-free rate
tbill <- getSymbols("TB3MS", src = "FRED", env = NULL, periodicity = "monthly")
tbill_tmp <- tbill[paste(startdate, enddate, sep = "/")] #has to manually subset by date for FRED data
tbill_tmp <- coredata(tbill_tmp/100) #turn into %
P_t <- 100*(1-91/360*tbill_tmp) #caluclate price
r_t <- 4*log(100/P_t) #turn price into annualized yield


#Payout Functions
#Since we don't have access to Options Historical Data, we can only use BS model to approximate option prices.
C <- 0.03 #2% OTM Call
P <- 0.05 #5% OTM Putt
Put_t <- function(S_t, P, r_t, sigma_t){
  d1 <- (log(1/(1-P)) + (r_t + sigma_t^2/2)*1/12) / (sigma_t*sqrt(1/12)) #monthly
  d2 <- d1 - sigma_t*sqrt(1/12) #monthly
  (1-P)*S_t*exp(-r_t/12)*pnorm(-d2)-S_t*pnorm(-d1)
}
PutPayoff_t <- function(S, S_1, P){
  pmax(0, (1-P)*S-S_1)
}
Call_t <- function(S_t, C, r_t, sigma_t){
  d1 <- (log(1/(1+C)) + (r_t + sigma_t^2/2)*1/12) / (sigma_t*sqrt(1/12)) #monthly
  d2 <- d1 - sigma_t*sqrt(1/12) #monthly
  S_t*pnorm(d1)-(1+C)*S_t*exp(-r_t/12)*pnorm(d2)
}

CallPayoff_t <- function(S, S_1, C){
  pmax(0, S_1 -(1+C)*S)
}



ROR_t <- function(S_t, S_t1, P, C, r_t, sigma_t){
  (S_t1 + PutPayoff_t(S_t, S_t1, P) - CallPayoff_t(S_t, S_t1, C)) / (S + Put_t(S_t, P, r_t, sigma_t) - Call_t(S_t, C, r_t, sigma_t))
}


t1 <- S_t1 + PutPayoff_t(S_t, S_t1, P) - CallPayoff_t(S_t, S_t1, C)
t11 <- pmax(0, (1-P)*S_t-S_t1)
t2 <- S_t + Put_t(S_t, P, r_t, sigma_t) - Call_t(S_t, C, r_t, sigma_t)
t22 <- pmax(0, S_t1 -(1+C)*S_t)
t3 <- t2-t1
test <- cbind(S_t, t1, t11, t2, t22, t3)

#Backtest
strategy_ret <- ROR_t(S_t, S_t1, P, C, r_t, sigma_t) - 1
benchmark_return <- (S_t1/S_t) - 1


#Create a final df by adding back the dates
months <- index(index_df)
months <- months[-n+1:-n]
final_df <- data.frame(months, strategy_ret, benchmark_return)

#convert back to xts
final_df <- xts(final_df[,-1], order.by = final_df[, 1])
names(final_df) <- c("Strategy", "Benchmark")

#Results
table.Stats(final_df)
table.DownsideRisk(final_df)
charts.PerformanceSummary(final_df, main = "100現股 + 1x 5%價外PUT - 1x 3%價外call")
chart.RelativePerformance(final_df[,1], final_df[,2])
chart.RiskReturnScatter(final_df)





BlackScholes <- function(S, K, r, T, sig, type){
  
  if(type=="C"){
    d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
    d2 <- d1 - sig*sqrt(T)
    
    value <- S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
    return(value)}
  
  if(type=="P"){
    d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
    d2 <- d1 - sig*sqrt(T)
    
    value <-  (K*exp(-r*T)*pnorm(-d2) - S*pnorm(-d1))
    return(value)}
}

call <- BlackScholes(S_t, S_t*(1+C), r_t, 1/12, sigma_t, "C")
call
head(S_t)
head(S_t*(1+C))
head(r_t)
head(sigma_t)

tail(S_t)
t <- Call_t(S_t, C, r_t, sigma_t)
t2 <- Put_t(S_t, P, r_t, sigma_t)
tail(t)
tail(t2)

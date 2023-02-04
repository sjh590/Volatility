#Code to forecast Volatility using GJR-GARCH and YZ Vol Estimators

simVOL <- function(x, t0, t1){
  library(quantmod)
  
  #Load Data
  df <- getSymbols(x, from = t0, to = t1, env = NULL)[,1:4] #env = NULL is needed to save it directly as df
  
  #Clean Data. Yang-Zhang Vol calculation accounts for the Overnight + Open-to-Close price action. RSY Vol is also inside YZ Vol.
  names(df) <- c("Open", "High", "Low", "Close")
  df$Lag.Close <- Lag(df$Close, 1)
  df <- df[-1,]
  
  #Calcualte the terms of RSY
  rsy1 <- as.numeric(log(df$High/df$Close))
  rsy2 <- as.numeric(log(df$High/df$Open))
  rsy3 <- as.numeric(log(df$Low/df$Close))
  rsy4 <- as.numeric(log(df$Low/df$Open))
  
  #Calcualte RSY Vol
  rsy <- sqrt(1/nrow(df)*sum(rsy1*rsy2 + rsy3*rsy4))
  
  
  #Calculate the first term of yz
  t1 <- log(df$Open/df$Lag.Close)
  yz1 <- 1/(nrow(df) - 1)*sum((t1 - mean(t1))^2)
  
  #Calculate the second term of yz
  t2 <- log(df$Close/df$Open)
  yz2 <- 1/(nrow(df) - 1)*sum((t2 - mean(t2))^2)
  
  #Calculate k
  k <- 0.34/(1.34 + (nrow(df)+1)/(nrow(df)-1))
  
  #Calculate YZ Vol
  annual.vol <- sqrt(yz1 + k*yz2 + (1-k)*rsy^2)*sqrt(252) #Annualize
  
  print(annual.vol)
}


library(TTR)
library(lubridate)
#Compare my function to TTR
t1 <- as.Date("2023-2-4")
t0 <- t1 - years(1)

getSymbols("AAPL", from = t0, to = t1)
volatility(AAPL, n =250, N=250, calc = "yang.zhang")
simVOL("AAPL", t0, t1)


#Main Strategy
library(rugarch)
library(quantmod)
library(TTR)
library(PerformanceAnalytics)

#Load Data: SPY
getSymbols("SPY", from = "1990-1-1")
SPY.rets <- na.omit(Delt(Ad(SPY)))

#Specify GARCH model 
gjrSpec <- ugarchspec(mean.model = list(armaOrder = c(1,0)),
                      variance.model = list(model = "gjrGARCH", variance.targeting = TRUE),
                      distribution.model = "sstd") #Skewed Student T

#Run Estimations
garchroll <- ugarchroll(gjrSpec, data = SPY.rets, n.start = 502, refit.window = "moving", refit.every = 22) #502-day rolling window, refit every 22 days (1 month) 
garchroll <- as.data.frame(garchroll)

#Compare predictions to VIX
#GARCH predictions are "Realized Volatility (Current)" and VIX is and "Implied Volatility (Future).
#Whenever GARCH > VIX, it implies that people are in panic and we should long VOL
#Conversely, whenever GARCH < VIX, this is considered to be a normal market condition, therefore we short VOL (Selling insurance to people overpaying for VOL)

#Load Data: VIX, ZIV, VXZ. The latter two are short and long VOL ETFs.
tickers <- c("^VIX", "ZIV", "VXZ")
getSymbols(tickers, from = "1990-1-1")

#Create Long/Short signals
#Clean predictions data frame. Annualize and *100 to match VIX.
garchPreds <- xts(garchroll$Sigma*sqrt(252)*100, order.by = as.Date(rownames(garchroll)))
diff <- garchPreds - Ad(VIX)

ZIV.rets <- na.omit(Delt(Ad(ZIV)))
VXZ.rets <- na.omit(Delt(Ad(VXZ)))

ZIV.signal <- diff < 0 
VXZ.signal <- diff > 0

#lag the signals by 2 periods. 
#Must lag by two. For example, get signal at T, buy at T+1, and return is T+2/T+1-1
Strat.Ret <- lag(ZIV.signal, 2) * ZIV.rets + lag(VXZ.signal, 2) * VXZ.rets

#Compare it with using historical Vol
SPY.sd <- runSD(SPY.rets, n =22, sample = FALSE) * sqrt(252) *100
SPY.diff <- SPY.sd - Ad(VIX)

ZIV.signal <- SPY.diff < 0 
VXZ.signal <- SPY.diff > 0

Strat2.Ret <- lag(ZIV.signal, 2) * ZIV.rets + lag(VXZ.signal, 2) * VXZ.rets

#Compare both strategies
compare <- cbind(Strat.Ret, Strat2.Ret)
colnames(compare) <- c("gjrGARCH", "Historical")

charts.PerformanceSummary(compare)

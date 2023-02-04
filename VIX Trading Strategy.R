#GJR-GARCH Volatility Trading Strat

#Libraries
library(quantmod)
library(rugarch)
library(PerformanceAnalytics)
library(lubridate)

#Load Data
enddate <- today()-1
startdate <- enddate %m+% months(-48)

getSymbols("SPY", from = startdate, to = enddate, src = "yahoo")

SPY_ret <- na.omit(Return.calculate(Ad(SPY)))
head(SPY_ret)


#GJR GARCH model variables
gjrSpecs <- ugarchspec(mean.model = list(armaOrder = c(1,0)),
                       variance.model = list(model = "gjrGARCH",
                                             #Variance Targeting uses unconditional variance as the asymptotic variance
                                             variance.targeting = TRUE), 
                       distribution.model = "sstd")

#Rolling windows of 500 days, and refit every 22 days
#If the rolling windows are too narrow, the model might not converge
garchroll = ugarchroll(gjrSpecs, data = SPY_ret, 
                       n.start = 500, refit.window = "moving", refit.every = 22)

#Covert into df
garch_df <- as.data.frame(garchroll)
head(garch_df)


#Get VIX (implied volatility) data
getSymbols("^VIX", from = startdate, to = enddate, src = "yahoo")
#Annualized GARCH predictions to match VIX
garchPreds <- xts(garch_df$Sigma*sqrt(252)*100, order.by = as.Date(rownames(garch_df)))
diff <- garchPreds - Ad(VIX)

#Get VIXY (Long Vix) and SVXY (Short VIX) ETF data
getSymbols("VIXY", from = startdate, to = enddate, src = "yahoo")
getSymbols("SVXY", from = startdate, to = enddate, src = "yahoo")
VIXY_ret <- na.omit(Return.calculate(Ad(VIXY)))
SVXY_ret <- na.omit(Return.calculate(Ad(SVXY)))

#Create buy signals
SVXYsignal <- Ad(VIX) > 10 & Ad(VIX) < 20 & diff < -0 #Go short vol when VIX is higher than predictions
VIXYsignal <- diff > 0 #Go long vol when VIX is lower than predictions

#Returns from buy signals
#Must lag by two. For example, get signal at T, buy at T+1, and return is T+2/T+1-1
garch_Ret<- lag(SVXYsignal, 2) * SVXY_ret + lag(VIXYsignal, 2) * VIXY_ret


#Alternative method: Use historical volatility instead of GARCH predictions
#Get SPY histroical volatility (realzied volatility)
SPY_histvol <- runSD(SPY_ret, n = 21, sample = FALSE) * sqrt(252) *100
histvol_diff <- SPY_histvol - Ad(VIX)
#Create a second set of buy signals
SVXYsignal <- Ad(VIX) > 10 & Ad(VIX) < 20 & histvol_diff < 0 #Go short vol when VIX is higher than historical
VIXYsignal <- histvol_diff > 0 #Go long vol when VIX is lower than historical

SPY_histvol_Ret<- lag(SVXYsignal, 2) * SVXY_ret + lag(VIXYsignal, 2) * VIXY_ret

#comparison
final_df <- cbind(garch_Ret, SPY_histvol_Ret, SPY_ret)
colnames(final_df) <- c("gjrGARCH", "histVol", "Benchmark")

charts.PerformanceSummary(final_df)



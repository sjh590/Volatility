#Hold when VIX% is within 95% normal distribution
#Sell when VIX% is outside of 95% normal distribution

options(scipen = 999)
#Libraries
library(quantmod)
library(PerformanceAnalytics)
library(lubridate)

#Load Data
enddate <- today()-1
startdate <- enddate %m+% months(-240)

getSymbols("SPY", from = startdate, to = enddate, src = "yahoo")
SPY_ret <- na.omit(Return.calculate(Ad(SPY)))
head(SPY_ret)

getSymbols("SPDN", from = startdate, to = enddate, src = "yahoo")
SPDN_ret <- na.omit(Return.calculate(Ad(SPDN)))
head(SPDN_ret)


#Get VIX (implied volatility) data
getSymbols("^VIX", from = startdate, to = enddate, src = "yahoo")
VIX_ret <- na.omit(Return.calculate(Ad(VIX)))
right_criteria <- rollapply(VIX_ret, 50, mean) + 2*rollapply(VIX_ret, 50, sd)
left_criteria <- rollapply(VIX_ret, 50, mean) - 2*rollapply(VIX_ret, 50, sd)

#Create Buy signals
Buysignal <- VIX_ret < lag(right_criteria, 1) & VIX_ret > lag(left_criteria, 1) #Buy when VIX is within 95% normal distribution
#In addition, buy SQQQ when Buysignal is 0
SPDN_Buysignal <- ifelse(Buysignal == 0, 1, 0)

#Returns from buy signals
#Must lag by two. For example, get signal at T, buy at T+1, and return is T+2/T+1-1
Ret<- lag(Buysignal, 2) * SPY_ret +  lag(SPDN_Buysignal, 2) * SPDN_ret
Ret <- Ret[-1]


#comparison
final_df <- cbind(Ret, SPY_ret)
colnames(final_df) <- c("Strategy", "Benchmark")

table.Stats(final_df)
table.DownsideRisk(final_df)
charts.PerformanceSummary(final_df, main = "VIX漲跌幅超過2標準差就觀望 vs. S&P 500")
chart.RelativePerformance(final_df[,1], final_df[,2])
chart.RiskReturnScatter(final_df)

h1 <- hist(final_df$Benchmark, breaks = 50)
h2 <- hist(final_df$Strategy, breaks = 50)


par(xpd = TRUE)
plot(h1, col = "Red", main = NULL)
plot(h2, col = "Blue", add = T)

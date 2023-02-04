#Chapter 5 
#Factor Models
library(quantmod)
library(dplyr)
library(zoo)
library(lubridate)
library(lmtest)
library(moments)
#CAPM
#Load data, use AAPL as proxy portfolio against SPY
tickers <- c("AAPL", "SPY")
getSymbols(tickers, from = "2014-12-1" , to = "2020-2-1", periodicity = "monthly")
getSymbols("DGS3MO", from = "2014-12-1" , to = "2020-2-1", periodicity = "monthly", src = "FRED")
market <- Delt(SPY$SPY.Close)
portfolio <- Delt(AAPL$AAPL.Close)

DGS3MO <- na.omit(DGS3MO)
DGS3MO <- to.monthly(DGS3MO)

#Convert 3-month yield to monthy yield. Raw data is annual yield
DGS3MO <- (1+DGS3MO/100)^(1/12)-1
rf <- DGS3MO["2014-12-31::2020-01-01"]

#Combine date into one data frame
combo <- cbind(market, portfolio, rf$DGS3MO.Close)
names(combo) <- c("market", "portfolio", "rf")
combo <- combo[-1]

#Calculate excess returns
combo$ex.ret <- combo$portfolio - combo$rf
combo$ex.mkt <- combo$market - combo$rf

#Linear regression
m <- lm(ex.ret ~ ex.mkt, data = combo)
summary(m)

#Market model, without subtracting risk-free rate
m2 <- lm(portfolio ~ market, data = combo)
summary(m2)

#Rolling Window Regressions
getSymbols(tickers, from = "2014-12-31" , to = "2020-1-1")
market2 <- Delt(SPY$SPY.Close)
portfolio2 <- Delt(AAPL$AAPL.Close)

rets <- cbind(market2, portfolio2)
rets <- rets[-1]
names(rets) <- c("SPY", "AAPL")

coeffs <- rollapply(rets, width = 252, 
                    FUN = function(X)
                      {
                      roll.reg = lm(AAPL ~ SPY, data = as.data.frame(X))
                      return(roll.reg$coef)
                      },
                      by.column = FALSE)
coeffs <- na.omit(coeffs)
names(coeffs) <- c("Alpha", "Beta")

par(mfrow = c(2,1))
plot(y = coeffs$Alpha, x = index(coeffs), xlab = "", ylab = "Alpha", type = "l", col = "blue", main = "AAPL Alpha, 252-day Rolling Window")
plot(y = coeffs$Beta, x = index(coeffs), xlab = "", ylab = "Beta", type = "l", col = "red", main = "AAPL Beta, 252-day Rolling Window")
par(mfrow = c(1,1))

#Betas on different days
getSymbols(tickers, from = "2014-12-31" , to = "2020-1-1")
prices <- cbind(AAPL$AAPL.Adjusted, SPY$SPY.Adjusted)
head(prices)
prices <- cbind(as.data.frame(index(prices)), prices)
names(prices) <- c("date", "AAPL", "SPY")

all.dates <- seq(as.Date("2017-12-01"), as.Date("2019-12-31"), by = "day")
all.dates <- as.data.frame(all.dates)
names(all.dates) <- "date"

combo <- merge(all.dates, prices, by = "date", all = TRUE)
combo <- na.locf(combo) #fill in NA with prior price
combo$wkday <- weekdays(combo$date)

#Create empty vectors to store Betas
beta <- as.data.frame(rep(999,5)) #repeat five times for each weekday
colnames(beta) <- "beta"
day.week <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
rownames(beta) <- day.week

#calculate betas for every day of the week
for(i in 1:5){
  new.data <- subset(combo, wkday == paste(day.week[i]))
  stock.ret <- new.data[,2] / Lag(new.data[,2], k = 1) - 1
  mkt.ret <- new.data[,3] / Lag(new.data[,3], k = 1) - 1
  
  combo.ret <- cbind(stock.ret, mkt.ret)
  combo.ret <- as.data.frame(combo.ret)
  colnames(combo.ret) <- c("stock.ret", "mkt.ret")
  combo.ret <- combo.ret[(nrow(combo.ret) - 104 +1):nrow(combo.ret), ] #only calculating betas for the last 2 years, 104 weeks
  beta[i,1] <- summary(lm(stock.ret ~ mkt.ret, data = combo.ret))$coeff[2]

}
beta

#Plot
vals <- round(beta$beta, 2)
p <- barplot(vals, 
             names.arg = day.week,
             ylim = c(0, max(beta) + 0.5),
             ylab = "Beta",
             border = 0,
             col = c("gray", "red", "blue", "darkgreen", "black"),
             main = "AAPL Betas Based on Different Weekdays")
text(p, vals, labels = vals, pos = 3)

#Fama-French Three Factor Model
data.ff <- read.csv(file = "F-F_Research_Data_Factors.csv", skip = 3, header = TRUE, nrows = 1122)
str(data.ff)

data.ff$dt <- paste(data.ff$X, "01", sep = " ")
data.ff$Date <- ymd(data.ff$dt)
data.ff <- data.ff[, -c(1,6)]
data.ff[, 1:4] <- data.ff[, 1:4]/100

ff.sub <- subset(data.ff, Date >= "2015-01-01" & Date <= "2019-12-31")
rownames(ff.sub) <- seq(1, nrow(ff.sub), 1)
head(ff.sub)
getSymbols(tickers, from = "2014-12-1" , to = "2020-1-1", periodicity = "monthly")
port.ret <- Delt(AAPL$AAPL.Adjusted)[-1]

ff.sub$port.ret <- port.ret - ff.sub$RF

ff.reg <- lm(port.ret ~ Mkt.RF + SMB +HML, data = ff.sub)
summary(ff.reg)
capm.reg <- lm(port.ret ~ Mkt.RF, data = ff.sub)
summary(capm.reg)

#Testing for Heteroskedasticity
bptest(formula(ff.reg), data = ff.sub, studentize = FALSE)
bptest(formula(ff.reg), data = ff.sub, studentize = TRUE)
#White Correction if there is heteroskedasticity
#coeftest(ff.reg, vcov. = vcovHC(ff.reg, type = "HC0"))

#Testing for Non-Normality
skewness(ff.reg$residuals)
kurtosis(ff.reg$residuals)
agostino.test(ff.reg$residuals)
anscombe.test(ff.reg$residuals)

#Testing for autocorrelation
#dwtest(capm.reg)

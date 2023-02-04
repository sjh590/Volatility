library(quantmod)

getSymbols("AAPL", from = "2014-12-31", to = "2020-1-1")

#simple return
ret <- AAPL$AAPL.Adjusted
head(ret)
tail(ret)
names(ret) <- c("price")
ret$lag.price <- lag(ret$price, k = 1)
ret$price.ret <- ret$price / ret$lag.price - 1

#alternatively
ret2 <- Delt(ret$price)
head(ret2)

ret <- ret[-1,3]


#total return
tot.ret <- AAPL[,6]
tot.ret <- Delt(tot.ret)
tot.ret <- tot.ret[-1]
head(tot.ret)

#log return
log.ret <- diff(log(AAPL[,6]))
log.ret <- log.ret[-1]
log.ret2 <- ROC(AAPL[,6])
log.ret2 <- log.ret2[-1]
(cbind(log.ret, log.ret2))

#winsorize, make outliers 5% quantile
(upper <- as.numeric(quantile(tot.ret, 0.995)))
(lower <- as.numeric(quantile(tot.ret, 0.005)))
winsorize.ret <- ifelse(tot.ret <= lower, lower,
                        ifelse(tot.ret >= upper, upper, tot.ret))
summary(tot.ret)
summary(winsorize.ret)

#truncate
truncate.ret <- subset(tot.ret, tot.ret <= upper & tot.ret >= lower)

#gross return
#sumprod
gross.ret <- 1 + tot.ret
cum.arith <- cumprod(gross.ret)
head(cum.arith)
as.numeric(cum.arith[nrow(cum.arith)])-1
#log
(cum.log <- exp(sum(log.ret))-1)

#normalize and plot standard vs total
par(mar = c(5,5,3,5))
norm.ret <- AAPL[,4]/as.numeric(AAPL[,4][1])
norm.tot.ret <- AAPL[,6]/as.numeric(AAPL[,6][1])
Date <- index(norm.ret)
(y.range <- range(norm.ret, norm.tot.ret))
plot(x = Date, y = norm.ret, ylim = y.range, xlab = "Date", ylab = "Normalized Price", type = "l", col = "blue", main = "Normalized Standard vs Total Return")
lines(x = Date, y = norm.tot.ret, col = "dark green")
abline(h = 1)
legend("topleft", inset = 0.01 ,c("Standard Return", "Total Return"), col = c("blue", "dark green"), lwd = c(1,1))

#change to weekly returns
AAPL_weekly <- to.weekly(AAPL)
AAPL_weekly <- Ad(AAPL_weekly)
weekly_ret <- Delt(AAPL_weekly)

#change to monthly retursn
AAPL_monthly <- to.monthly(AAPL)
AAPL_monthly <- Ad(AAPL_monthly)
monthly_ret <- Delt(AAPL_monthly)



#Comparison for multiple tickers
tickers <- c("AAPL", "GOOG", "AMZN", "SPY")
getSymbols(tickers, from = "2014-12-31", to = "2020-1-1")

#Calculation returns by using prices and cumulative return
#Using Prices
#Create normalized df
Norm.Price_df <- cbind(Ad(AMZN)/as.numeric(Ad(AMZN)[1]), Ad(GOOG)/as.numeric(Ad(GOOG)[1]), Ad(AAPL)/as.numeric(Ad(AAPL)[1]), Ad(SPY)/as.numeric(Ad(SPY)[1]))
names(Price_df) <- c("AMZN", "GOOG", "AAPL", "SPY")

tail(Norm.Price_df,1)

#Using cumulative return
AMZN_ret <- 1 + Delt(Ad(AMZN))
GOOG_ret <- 1 + Delt(Ad(GOOG))
AAPL_ret <- 1 + Delt(Ad(AAPL))
SPY_ret <- 1 + Delt(Ad(SPY))

#Create combined df
Ret_df<- cbind(AMZN_ret, GOOG_ret, AAPL_ret, SPY_ret)
names(Ret_df) <- c("AMZN", "GOOG", "AAPL", "SPY")
Ret_df[1,] <- c(1,1,1,1)

Cum.Ret_df <- cumprod(Ret_df)

tail(Cum.Ret_df,1)


#Plot
plot(x = index(Cum.Ret_df),
     y = Cum.Ret_df$AMZN,
     ylim = c(0,round(max(Cum.Ret_df))),
     xlab = "Date",
     ylab = "Value of Investment",
     type = "l",
     lwd = 3,
     main = "Value of $1 Investment in AMZN, GOOG, AAPL, SPY")
lines(x = index(Cum.Ret_df), y = Cum.Ret_df$GOOG, col = "blue")
lines(x = index(Cum.Ret_df), y = Cum.Ret_df$AAPL, col = "red")
lines(x = index(Cum.Ret_df), y = Cum.Ret_df$SPY, col = "darkgreen")
abline(h=1)
legend("topleft", inset = 0.01, 
       c("Amazon", "Google", "Apple", "S&P 500"),
       col = c("black", " blue", "red", "darkgreen"),
       lwd = c(3,2,2,1))

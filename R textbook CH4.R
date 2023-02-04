#Chapter 4
#Libraries
library(quantmod)
library(lubridate)
library(ggplot2)
library(dplyr)

#Date
startdate <- "2014-12-31"
enddate <- "2020-1-1" 

#Data
tickers <- c("SPY", "BIL")
getSymbols(tickers, from = startdate, to = enddate)

#daily returns
ret.SPY <- Delt(SPY$SPY.Adjusted)
ret.BIL <- Delt(BIL$BIL.Adjusted)

#gross returns
rets <- cbind(ret.SPY, ret.BIL)
rets[1, ] <- c(0,0)
names(rets) <- c("SPY", "BIL")

gross.rets <- 1 + rets
cum.ret <- cumprod(gross.rets)
tail(cum.ret)

#Plot
plot(x = index(cum.ret),
     y = cum.ret$SPY,
     xlab = "Date",
     ylab = "Value of Investments",
     type = "l",
     col = "darkgreen",
     main = "Value of $1 Invested in the S&P 500 index and T-Bills 2015-2019")
lines(x = index(cum.ret),
      y = cum.ret$BIL,
      col = "blue")
legend("topleft", c("SPY", "BIL"), lty = 1, col = c("darkgreen", "blue"))

plot(x = rets$SPY, col = "red", main = "Volatility of S&P 500 and T-Bilss")
lines(rets$BIL, col = "blue")

#Volatility
rets <- rets[-1,]
sd(rets$SPY)
sd(rets$BIL)

Year <- as.Date(index(rets))
Year <- format(Year, "%Y")
rets <- cbind(Year, rets)
rets.2015 <- subset(rets, rets$Year==2015)
(sd.2015 <- apply(rets.2015[,-1], 2, sd))

#Calculate for each year
sd.all <- aggregate(rets[,-1], list(rets$Year), FUN = sd)
sd.all <- sd.all*sqrt(252) #annualize
sd.all <- as.data.frame(sd.all)
transpose.sd.all <- t(sd.all) #transpose

#Plot
barplot(transpose.sd.all,
        beside = TRUE,
        main = "Annualized Standard Deviation of SPY and BIL Returns 2015-2019",
        ylim = c(0,0.2),
        col = c("blue", "red"),
        border = c(0,0),
        legend.text = c("SPY", "BIL"))


#Var-Covar Variance Portfolio Risk
tickers <- c("AMZN", "GOOG", "AAPL", "SPY")
getSymbols(tickers, from = startdate, to = enddate)

rets <- cbind(AMZN$AMZN.Adjusted, GOOG$GOOG.Adjusted, AAPL$AAPL.Adjusted, SPY$SPY.Adjusted)
rets <- apply(rets, 2, Delt)
rets <- as.data.frame(rets)
rownames(rets) <- index(SPY$SPY.Adjusted)
rets <- rets[-1,]

weights <- as.matrix(c(0.3,0.3,0.2,0.2), 1)

mat.rets <- as.matrix(rets)

vcov <- cov(mat.rets)*252

mat.port.var <- t(weights) %*% vcov %*% (weights)
mat.port.var
(mat.port.sd <- sqrt(mat.port.var))


#Value at Risk
#Var-Covar Variance Portfolio Risk
rets <- cbind(AMZN$AMZN.Adjusted, GOOG$GOOG.Adjusted, AAPL$AAPL.Adjusted, SPY$SPY.Adjusted)
rets <- apply(rets, 2, Delt)
rets <- as.data.frame(rets)
rownames(rets) <- index(SPY$SPY.Adjusted)
rets <- rets[-1,]
#Portfolio
portfolio.rets <- t(weights)*rets
portfolio.rets$total <- rowSums(portfolio.rets)

var.pnl <- portfolio.rets[c(-1:-4)]
var.pnl$amount <- cumprod(1 + portfolio.rets$total)*1000000
var.pnl$daily <- var.pnl$amount - lag(var.pnl$amount, 1)
var.pnl$daily[1] <- 1000000-var.pnl$amount[1]

(port.mean <- mean(var.pnl$total))
(port.sd <- sd(var.pnl$total))


#Gauss0.1
(var01.gauss <- -(port.mean + qnorm(0.01)*port.sd)*tail(var.pnl$amount,1))
#Gauss0.5
(var05.gauss <- -(port.mean + qnorm(0.05)*port.sd)*tail(var.pnl$amount,1))
#Hist0.1
(var01.hist <- as.numeric(quantile(var.pnl$daily, 0.99)))
#Hist0.5
(var05.hist <- as.numeric(quantile(var.pnl$daily, 0.95)))

#Plot
(ret.d <- density(portfolio.rets$total))
plot(ret.d,
     xlab = "Profit & Loss",
     ylab = "",
     yaxt = "n",
     main = "Density of Simulated Portfolio P&L and 1% and 5% VaR")
abline(v = -quantile(portfolio.rets$total, 0.99), col = "red")
abline(v = -quantile(portfolio.rets$total, 0.95), col = "blue")

x <- seq(min(portfolio.rets$total), max(portfolio.rets), length = 1000)
y <- dnorm(x, port.mean, port.sd)
lines(x, y, type = "l", col = "darkgreen")
legend("topright",
       c("P&L", "5% VAR", "1% VAR", "Normal Distribution"),
       col = c("black", "blue", "red", "darkgreen"),
       lty = 1)

#Expected Shortfall
#Gauss0.1
(es01.gauss <- ((port.mean + port.sd*(dnorm(qnorm(0.01)))/0.01)*tail(var.pnl$amount,1)))
#Gauss0.5
(es05.gauss <- ((port.mean + port.sd*(dnorm(qnorm(0.05)))/0.05)*tail(var.pnl$amount,1)))
#Hist0.1
var01.limit <- -var01.hist
es.pnl <- var.pnl
es.pnl$d01 <- ifelse(es.pnl$daily < var01.limit, 1, 0)
#Hist0.5
var05.limit <- -var05.hist
es.pnl$d05 <- ifelse(es.pnl$daily < var05.limit, 1, 0)

#extract shortfall
shortfall01 <- subset(es.pnl[,3:4], d01 == 1)
shortfall05 <- subset(es.pnl[,c(3,5)], d05 == 1)
(es01.hist <- -mean(shortfall01$daily))
(es05.hist <- -mean(shortfall05$daily))

combo <- rbind(cbind(var01.gauss, var01.hist, es01.gauss, es01.hist), cbind(var05.gauss, var05.hist, es05.gauss, es05.hist))
colnames(combo) <- c("Gaussian VaR", "Historical VaR", "Gaussian ES", "Historical ES")
rownames(combo) <- c("1% 1-Day", "5% 1-Day")

combo


#Alternative Risk Measures
#Parkinson
parkinson <- AMZN[-1,2:3]
parkinson$log.hi.low <- log(parkinson$AMZN.High / parkinson$AMZN.Low)
parkinson$log.square <- parkinson$log.hi.low^2
head(parkinson)
(parkinson.sum <- sum(parkinson$log.square))
(parkinson.vol <- sqrt(1/(4*nrow(parkinson)*log(2))*parkinson.sum)*sqrt(252))


#Garman-Klass
gk <- AMZN[-1, 1:4]
(gk1 <- (1/(2*nrow(gk)))*parkinson.sum)
(gk2 <- ((2*log(2)-1)/nrow(gk))*sum(log(gk$AMZN.Close/gk$AMZN.Open)^2))

(gk.vol <- sqrt(gk1 - gk2)*sqrt(252))

#Rogers, Satchell, and Yoon
rsy <- AMZN[-1, 1:4]
(rsy1 <- as.numeric(log(rsy$AMZN.High/rsy$AMZN.Close)*log(rsy$AMZN.High/rsy$AMZN.Open)))
(rsy2 <- as.numeric(log(rsy$AMZN.Low/rsy$AMZN.Close)*log(rsy$AMZN.Low/rsy$AMZN.Open)))

(rsy.vol <- sqrt((1/nrow(rsy))*sum(rsy1 + rsy2))*sqrt(252))

#Yang and Zhang
yz <- AMZN[-1, 1:4]
yz$lag.Close <- Lag(yz$AMZN.Close, 1) 
yz <- yz[-1]
(yz1.mean <- mean(log(yz$AMZN.Open/yz$lag.Close)))
(yz1 <- 1/(nrow(yz)-1)*sum((log(yz$AMZN.Open/yz$lag.Close)-yz1.mean)^2)) 
(yz2.mean <- mean(log(yz$AMZN.Close/yz$AMZN.Open)))
(yz2 <- 1/(nrow(yz)-1)*sum((log(yz$AMZN.Close/yz$AMZN.Open)-yz2.mean)^2)) 
(k <- 0.34/(1.34 + (nrow(yz)+1)/(nrow(yz)-1)))

(yz.vol <- sqrt(yz1 + k*yz2 + (1-k)*(rsy.vol/sqrt(252))^2)*sqrt(252))

#Normal Arithmetic
rets <- diff(log(AMZN$AMZN.Adjusted))
rets <- rets[-1]
names(rets) <- "AMZN"
sd <- sd(rets)*sqrt(252)

#Combine
vols <- rbind(sd, parkinson.vol, gk.vol, rsy.vol, yz.vol)
rownames(vols) <- c("Normal", "Parkinson", "Garman-Klass", "RSY", "YZ")
colnames(vols) <- "Annualized Vol"

vols

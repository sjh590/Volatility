#Libraries
library(quantmod)
library(lubridate)

#Date
startdate <- "2014-12-31"
enddate <- "2020-1-1" 

#Data
tickers <- c("AMZN", "GOOG", "AAPL")
getSymbols(tickers, from = startdate, to = enddate)

#Returns
ret.AMZN <- Delt(Ad(AMZN))
ret.GOOG <- Delt(Ad(GOOG))
ret.AAPL <- Delt(Ad(AAPL))

returns <- cbind(ret.AMZN, ret.GOOG, ret.AAPL)
names(returns) <- c("AMZN", "GOOG", "AAPL")

#Weights
i.AMZN <- 50000
i.GOOG <- 30000
i.AAPL <- 20000
i.Total <- i.AMZN + i.GOOG + i.AAPL

w.AMZN <- i.AMZN/i.Total
w.GOOG <- i.GOOG/i.Total
w.AAPL <- i.AAPL/i.Total

#Portfolio returns
port.rets <- 1 + returns
port.rets <- port.rets[-1] #remove first row NA
cum.rets <- cumprod(port.rets)
(tail(cum.rets,1)-1)
cum.port <- w.AMZN*cum.rets$AMZN + w.GOOG*cum.rets$GOOG + w.AAPL*cum.rets$AAPL
(tail(cum.port,1)-1)


#Same thing but using matrices
weights <- c(w.AMZN, w.GOOG, w.AAPL)
mat.weight <- matrix(weights, 1)
mat.returns <- matrix(tail(cum.rets,1)-1, 3)
(port.rets2 <- mat.weight %*% mat.returns)


#Quarterly Rebalancing
#Q1
rets.q1 <- returns["2019-01-01/2019-03-31"]
grets.q1<- 1 + rets.q1
crets.q1 <- apply(grets.q1, 2, cumprod)
crets.q1 <- tail(crets.q1,1)-1

#Q2
rets.q2 <- returns["2019-04-01/2019-06-30"]
grets.q2<- 1 + rets.q2
crets.q2 <- apply(grets.q2, 2, cumprod)
crets.q2 <- tail(crets.q2,1)-1

#Q3
rets.q3 <- returns["2019-07-01/2019-09-30"]
grets.q3<- 1 + rets.q3
crets.q3 <- apply(grets.q3, 2, cumprod)
crets.q3 <- tail(crets.q3,1)-1

#Q4
rets.q4 <- returns["2019-10-01/2019-12-31"]
grets.q4 <- 1 + rets.q4
crets.q4 <- apply(grets.q4, 2, cumprod)
crets.q4 <- tail(crets.q4,1)-1

#quarterly short way
qtr.AMZN <- to.quarterly(AMZN)
qtr.GOOG <- to.quarterly(GOOG)
qtr.AAPL <- to.quarterly(AAPL)
rqtr <- cbind(Delt(Ad(qtr.AMZN)), Delt(Ad(qtr.GOOG)), Delt(Ad(qtr.AAPL)))


#Equal-Weighted Portfolio
ew.i0 <- 1000
(ew.i1 <- ew.i0 * (1 + mean(crets.q1)))
(ew.i2 <- ew.i1 * (1 + mean(crets.q2)))
(ew.i3 <- ew.i2 * (1 + mean(crets.q3)))
(ew.i4 <- ew.i3 * (1 + mean(crets.q4)))

#Value-Weighted Portfolio
#Market-Cap from web
mc1.AMZN <- 737.47
mc1.GOOG <- 720.32
mc1.AAPL <- 746.08
(mc1.total <- mc1.AMZN + mc1.GOOG + mc1.AAPL)

(w1.AMZN <- mc1.AMZN/mc1.total)
w1.GOOG <- mc1.GOOG/mc1.total
w1.AAPL <- mc1.AAPL/mc1.total

vw.i0 <- 1000
(vw.i0.AMZN <- vw.i0 * w1.AMZN)
(vw.i0.GOOG <- vw.i0 * w1.GOOG)
(vw.i0.AAPL <- vw.i0 * w1.AAPL)

#Q1
(vw.i1.AMZN <- vw.i0.AMZN * (1 + crets.q1[1]))
(vw.i1.GOOG <- vw.i0.GOOG * (1 + crets.q1[2]))
(vw.i1.AAPL <- vw.i0.AAPL * (1 + crets.q1[3]))
(vw.i1 <- sum(vw.i1.AMZN, vw.i1.GOOG, vw.i1.AAPL))

#Q2
mc2.AMZN <- 876.22
mc2.GOOG <- 815.67
mc2.AAPL <- 895.67
(mc2.total <- mc2.AMZN + mc2.GOOG + mc2.AAPL)

(w2.AMZN <- mc2.AMZN/mc2.total)
w2.GOOG <- mc2.GOOG/mc2.total
w2.AAPL <- mc2.AAPL/mc2.total

(vw.i2.AMZN <- vw.i1 * w2.AMZN * (1 + crets.q2[1]))
(vw.i2.GOOG <- vw.i1 * w2.GOOG * (1 + crets.q2[2]))
(vw.i2.AAPL <- vw.i1 * w2.AAPL * (1 + crets.q2[3]))
(vw.i2 <- sum(vw.i2.AMZN, vw.i2.GOOG, vw.i2.AAPL))

#Q3
mc3.AMZN <- 939.29
mc3.GOOG <- 750.42
mc3.AAPL <- 910.64
(mc3.total <- mc3.AMZN + mc3.GOOG + mc3.AAPL)

(w3.AMZN <- mc3.AMZN/mc3.total)
w3.GOOG <- mc3.GOOG/mc3.total
w3.AAPL <- mc3.AAPL/mc3.total

(vw.i3.AMZN <- vw.i2 * w3.AMZN * (1 + crets.q3[1]))
(vw.i3.GOOG <- vw.i2 * w3.GOOG * (1 + crets.q3[2]))
(vw.i3.AAPL <- vw.i2 * w3.AAPL * (1 + crets.q3[3]))
(vw.i3 <- sum(vw.i3.AMZN, vw.i3.GOOG, vw.i3.AAPL))

#Q3
mc4.AMZN <- 859.28
mc4.GOOG <- 842.21
mc4.AAPL <- 995.15
(mc4.total <- mc4.AMZN + mc4.GOOG + mc4.AAPL)

(w4.AMZN <- mc4.AMZN/mc4.total)
w4.GOOG <- mc4.GOOG/mc4.total
w4.AAPL <- mc4.AAPL/mc4.total

(vw.i4.AMZN <- vw.i3 * w4.AMZN * (1 + crets.q4[1]))
(vw.i4.GOOG <- vw.i3 * w4.GOOG * (1 + crets.q4[2]))
(vw.i4.AAPL <- vw.i3 * w4.AAPL * (1 + crets.q4[3]))
(vw.i4 <- sum(vw.i4.AMZN, vw.i4.GOOG, vw.i4.AAPL))


#Equal-Weighted Quarterly Reblance Returns
#Q1
ew.val1 <- ew.i0 / 3 * cumprod(grets.q1)
ew.val1$tot <- rowSums(ew.val1)
tail(ew.val1)

#Q2
ew.val2 <- as.numeric(ew.val1[nrow(ew.val1),4]) / 3 * cumprod(grets.q2)
ew.val2$tot <- rowSums(ew.val2)
tail(ew.val2)

#Q3
ew.val3 <- as.numeric(ew.val2[nrow(ew.val2),4]) / 3 * cumprod(grets.q3)
ew.val3$tot <- rowSums(ew.val3)
tail(ew.val3)

#Q4
ew.val4 <- as.numeric(ew.val3[nrow(ew.val3),4]) / 3 * cumprod(grets.q4)
ew.val4$tot <- rowSums(ew.val4)
tail(ew.val4)

ew.port <- rbind(ew.val1, ew.val2, ew.val3, ew.val4)
tail(ew.port)

#Value-Weighted Quarteryl Rebalance Returns
#Q1
vw.val1 <- cumprod(grets.q1)
vw.val1$AMZN <- vw.val1$AMZN * vw.i0.AMZN
vw.val1$GOOG <- vw.val1$GOOG * vw.i0.GOOG
vw.val1$AAPL <- vw.val1$AAPL * vw.i0.AAPL
vw.val1$tot <- rowSums(vw.val1)
tail(vw.val1)

#Q2
vw.i1tot <- as.numeric(tail(vw.val1$tot,1))
vw.val2 <- cumprod(grets.q2)
vw.val2$AMZN <- vw.val2$AMZN * w2.AMZN * vw.i1tot
vw.val2$GOOG <- vw.val2$GOOG * w2.GOOG * vw.i1tot
vw.val2$AAPL <- vw.val2$AAPL * w2.AAPL * vw.i1tot
vw.val2$tot <- rowSums(vw.val2)
tail(vw.val2)

#Q3
vw.val3 <- cumprod(grets.q3)
vw.i2tot <- as.numeric(tail(vw.val2$tot,1))
vw.val3$AMZN <- vw.val3$AMZN * w3.AMZN * vw.i2tot
vw.val3$GOOG <- vw.val3$GOOG * w3.GOOG * vw.i2tot
vw.val3$AAPL <- vw.val3$AAPL * w3.AAPL * vw.i2tot
vw.val3$tot <- rowSums(vw.val3)
tail(vw.val3)

#Q4
vw.val4 <- cumprod(grets.q4)
vw.i3tot <- as.numeric(tail(vw.val3$tot,1))
vw.val4$AMZN <- vw.val4$AMZN * w4.AMZN * vw.i3tot
vw.val4$GOOG <- vw.val4$GOOG * w4.GOOG * vw.i3tot
vw.val4$AAPL <- vw.val4$AAPL * w4.AAPL * vw.i3tot
vw.val4$tot <- rowSums(vw.val4)
tail(vw.val4)

vw.port <- rbind(vw.val1, vw.val2, vw.val3, vw.val4)
porfolios <- cbind(ew.port, vw.port)


#Plot
par(mfrow = c(2,2))
(Q1.pie.values <- round((c(mc1.AMZN, mc1.GOOG, mc1.AAPL) / mc1.total)*100, digits = 1))
(Q1.pie.labels <- c(paste("AMZN (", Q1.pie.values[1], "%)", sep = ""), paste("GOOG (", Q1.pie.values[2], "%)", sep = ""), paste("AAPL (", Q1.pie.values[3], "%)", sep = "")))
pie(Q1.pie.values, labels = Q1.pie.labels, col = c("black", "blue", "red"), main = "Q1 Value Weighting")

(Q2.pie.values <- round((c(mc2.AMZN, mc2.GOOG, mc2.AAPL) / mc2.total)*100, digits = 1))
(Q2.pie.labels <- c(paste("AMZN (", Q2.pie.values[1], "%)", sep = ""), paste("GOOG (", Q2.pie.values[2], "%)", sep = ""), paste("AAPL (", Q2.pie.values[3], "%)", sep = "")))
pie(Q2.pie.values, labels = Q2.pie.labels, col = c("black", "blue", "red"), main = "Q2 Value Weighting")

(Q3.pie.values <- round((c(mc3.AMZN, mc3.GOOG, mc3.AAPL) / mc3.total)*100, digits = 1))
(Q3.pie.labels <- c(paste("AMZN (", Q3.pie.values[1], "%)", sep = ""), paste("GOOG (", Q3.pie.values[2], "%)", sep = ""), paste("AAPL (", Q3.pie.values[3], "%)", sep = "")))
pie(Q3.pie.values, labels = Q3.pie.labels, col = c("black", "blue", "red"), main = "Q3 Value Weighting")  

(Q4.pie.values <- round((c(mc4.AMZN, mc4.GOOG, mc4.AAPL) / mc4.total)*100, digits = 1))
(Q4.pie.labels <- c(paste("AMZN (", Q4.pie.values[1], "%)", sep = ""), paste("GOOG (", Q4.pie.values[2], "%)", sep = ""), paste("AAPL (", Q4.pie.values[3], "%)", sep = "")))
pie(Q4.pie.values, labels = Q4.pie.labels, col = c("black", "blue", "red"), main = "Q4 Value Weighting")  

#Time-Weighted Rate of Return
dates <- as.Date(c("2018-12-31", "2019-03-31", "2019-06-30", "2019-07-31", "2019-09-30", "2019-12-31"))
mv <- c(2000000, 1950000, 2000000, 2220000, 2400000, 2500000)
cf <- c(0, 0, 0, 20000, 0, -5000)
cbind(data.frame(dates), mv, cf)

hpr <- rep(0, length(cf))
for (i in 2:length(cf)) {
  hpr[i] <- (mv[i] - mv[i-1] + cf[i]) / mv[i-1]
}
cbind(data.frame(dates), mv, cf, hpr)
(gross.ret <- 1 + hpr)
(cum.ret <- tail(cumprod(gross.ret),1)-1)

#Money-Weighted Rate of Return (IRR)
pv <- function(cf, dates, r){
  t <- as.numeric((dates - dates[1]) / 365)
  pv_factor <- (1 + r) ^ -t
  pv <- cf * pv_factor
  value <- sum(pv)
  return(value)
}
mwrr <- function(cf, dates, guess){
  delta.x <- 0.01
  tol <- 0.0000001
  cur.x <- guess
  iter <- 0
  for (i in 1:1000) {
    fx <- pv(cf, dates, cur.x)
    cur.x.delta <- cur.x - delta.x
    fx.delta <- pv(cf, dates, cur.x.delta)
    dx <- (fx - fx.delta) / (delta.x)
    cur.x <- cur.x - (fx / dx)
    iter <- iter + 1
    cat("At iteration", iter, "MWRR equals", cur.x, "\n")
    if (abs(fx) < tol) break
  }
}  

#Example 1
cf <- c(-100000, 120000)
dates <- as.Date(c("2018-12-31", "2019-12-31"))
mwrr(cf, dates, 0.1)

#Example 2
cf <- c(-200000, 20000, 220000)
dates <- as.Date(c("2018-12-31", "2019-06-30", "2019-12-31"))
mwrr(cf, dates, 0.2)

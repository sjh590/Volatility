#Chapter 1

library(quantmod)

tickers <- c("AMZN", "GOOG", "AAPL", "SPY")

getSymbols(tickers, from = "2014-12-31", to = "2020-1-1")

#create df with only adj. prices
df <- cbind(AAPL$AAPL.Adjusted, AMZN$AMZN.Adjusted, GOOG$GOOG.Adjusted, SPY$SPY.Adjusted)
#normalize price using first row
df <- apply(df[,1:4], 2, function(x) (x/x[1]))


library(ggplot2)
library(tidyquant)
library(reshape2) 
#need to recreate df, since ggplot doesn't work well with xts objects
plot_df <- as.data.frame(df)
plot_df$Date <- as.Date(rownames(df))
#melt df so it would be easier to plot multiple columns on same graph
melted_plot_df <- melt(plot_df, id.vars = "Date", variable.name = "Series")
#plot 
p <- ggplot(melted_plot_df, aes(x = Date, y = value)) +
  geom_line(aes(color = Series), size = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Comparison", x = "Date", y = "Return Multiple", color = "Tickers") +
  geom_line(aes(y=1), linetype = "dashed") + 
  theme(legend.position = c(0.2,0.8)) 

library(gridExtra)
#plot four together and highlight one
p1 <- ggplot(plot_df) +
  geom_line(aes(x = Date, y = plot_df[,1]), color = "blue", size = 1) +
  geom_line(aes(x = Date, y = plot_df[,2]), color = "grey", size = 0.5) +
  geom_line(aes(x = Date, y = plot_df[,3]), color = "grey", size = 0.5) +
  geom_line(aes(x = Date, y = plot_df[,4]), color = "grey", size = 0.5) +
  labs(title = names(plot_df)[1], x = "Date", y = "Return Multiple")
p2 <- ggplot(plot_df) +
  geom_line(aes(x = Date, y = plot_df[,2]), color = "blue", size = 1) +
  geom_line(aes(x = Date, y = plot_df[,1]), color = "grey", size = 0.5) +
  geom_line(aes(x = Date, y = plot_df[,3]), color = "grey", size = 0.5) +
  geom_line(aes(x = Date, y = plot_df[,4]), color = "grey", size = 0.5) +
  labs(title = names(plot_df)[2], x = "Date", y = "Return Multiple")
p3 <- ggplot(plot_df) +
  geom_line(aes(x = Date, y = plot_df[,3]), color = "blue", size = 1) +
  geom_line(aes(x = Date, y = plot_df[,1]), color = "grey", size = 0.5) +
  geom_line(aes(x = Date, y = plot_df[,2]), color = "grey", size = 0.5) +
  geom_line(aes(x = Date, y = plot_df[,4]), color = "grey", size = 0.5) +
  labs(title = names(plot_df)[3], x = "Date", y = "Return Multiple")
p4 <- ggplot(plot_df) +
  geom_line(aes(x = Date, y = plot_df[,4]), color = "blue", size = 1) +
  geom_line(aes(x = Date, y = plot_df[,1]), color = "grey", size = 0.5) +
  geom_line(aes(x = Date, y = plot_df[,2]), color = "grey", size = 0.5) +
  geom_line(aes(x = Date, y = plot_df[,3]), color = "grey", size = 0.5) +
  labs(title = names(plot_df)[4], x = "Date", y = "Return Multiple")
grid.arrange(p1,p2,p3,p4, ncol = 2, top = "Side-by-Side Comparison")

#second method, use melted df
library(dplyr)
melted_plot_df$Series2 <- melted_plot_df$Series
p5 <- ggplot() +
  geom_line(data = melted_plot_df %>% select(-Series), aes(x = Date, y = value, group = Series2), color = "grey", size = 0.5) +
  geom_line(data = melted_plot_df, aes(x = Date, y = value, color = Series), color = "blue", size = 1) +
  facet_wrap(~Series) + #separate plots by factor 
  labs(title = "Side-by-Side Comparison", x = "Date", y = "Return Multiple")
p5


ma_20d <- rollapply(df$AMZN.Adjusted, 20, mean)
ema_20d <- EMA(df$AMZN.Adjusted, 20)
data <- cbind(df$AMZN.Adjusted, ma_20d, ema_20d)
names(data) <- c("Price", "MA_20","EMA_20")
data[18:22,]

data_2019 <- data["2019-01-01/2019-12-31"]
head(data_2019)
#need to recreate df, since ggplot doesn't work well with xts objects
plot_df <- as.data.frame(data_2019)
plot_df$Date <- as.Date(index(data_2019))
#melt df so it would be easier to plot multiple columns on same graph
melted_plot_df <- melt(plot_df, id.vars = "Date", variable.name = "Series")

p <- ggplot(melted_plot_df, aes(x = Date, y = value)) +
  geom_line(aes(color = Series, size = Series, linetype = Series)) +
  scale_color_manual(values = c("black", "blue", "red")) +
  scale_size_manual(values = c(1.2,1,1)) + 
  scale_linetype_manual(values = c("solid", "longdash", "longdash")) +
  labs(title = "AMZN Price and 20-day Moving Averages")+
  theme(legend.position = c(0.1,0.8)) 
p


getSymbols("AMZN", from = "2014-12-31", to = "2020-1-1")
head(AMZN)
AMZN_last30 <- AMZN[(nrow(AMZN)-30+1):nrow(AMZN)]
total_volume <- sum(AMZN_last30$AMZN.Volume)
AMZN_last30$Volume_Weight <- AMZN_last30$AMZN.Volume/total_volume
AMZN_last30$Price_Weighted <- AMZN_last30$AMZN.Adjusted*AMZN_last30$Volume_Weight
vwap <- sum(AMZN_last30$Price_Weighted)

#Candlestick chart
head(AMZN)
ohlc <- to.monthly(AMZN)
ohlc <- ohlc[-1,-4]
class(ohlc)
amzn.ohlc <- as.quantmod.OHLC(ohlc, 
                              col.names = c("Open", "High", "Low", "Volume", "Close"))
chartSeries(amzn.ohlc, theme = "white", name = "AMZN OHLC")

#2 axis chart
prc.vol <- AMZN[-1,5:6]
head(prc.vol)
date <- as.Date(index(prc.vol))
prc.vol <- data.frame(date,prc.vol)
names(prc.vol) <- c("Date", "Volume", "Close")
rownames(prc.vol) <- seq(1,nrow(prc.vol),1)
prc.vol$Volume <- prc.vol$Volume/10^6

par(mar = c(5,5,3,5))
with(prc.vol,
     barplot(prc.vol$Volume, ylim = c(0,500), border = NA, ylab = "Volume (mn)", xlab = "", col = "red", main = "AMZN Price and Volume \n 2015-2019"))
par(new = TRUE)
with(prc.vol,
     plot(x = prc.vol$Date, y = prc.vol$Close, ylim = c(0, 120), ylab = "", xlab = "",, type = "l", lwd = 2, col = "blue", yaxt = "n"))
axis(side = 4)
mtext(side = 4, line =3 , "Price")

#Leverage vs Double Capital

library(quantmod)
library(lubridate)
library(ggplot2)
library(dplyr)
library(cowplot)

#Load Data
enddate <- today()
startdate <- enddate - years(30)
getSymbols("SPY", from = startdate, to = enddate)

#Clean Data
ret <- SPY
ret <- ret[,6]
ret$ret <- Delt(ret$SPY.Adjusted)*100 #Change unit into %
ret <- ret[-1]
names(ret) <- c("SPY", "Returns")

windowsFonts(BL = windowsFont("微軟正黑體"))
#plot distribution of returns
p<- ret %>% ggplot(aes(x = Returns)) +
  geom_histogram(binwidth = 0.5, col = "black", fill = "lightblue") +
  stat_function(fun = function(x) 
    dnorm(x, mean = mean(ret$Returns), sd = sd(ret$Returns)) * 0.5 * nrow(ret))+
  ggtitle("S&P 500 過去30年報酬分布")+
  xlab("報酬 (%)") +
  ylab("次數") +
  theme(axis.title = element_text(family = "BL", size = 14))+
  theme(plot.title = element_text(size = 24, family = "BL", face = "bold")) 
p

#subset returns into base, bull, and bear
base <- ret[ret$Returns < quantile(ret$Returns, 0.75) & ret$Returns > quantile(ret$Returns, 0.25)]
bear <- ret[ret$Returns < (mean(ret$Returns) - sd(ret$Returns))]
#bull <- ret[ret$Returns > quantile(ret$Returns, 0.25)]



#Simulate P&L
i <- 0
n <- 10000
cash <- 1000
tmp.pos <- c()
tmp.neg <- c()

tmp1 <- c()
tmp2 <- c()
while (i < n+1) {
  df <- base[sample(nrow(base), 1000)]
  df$cumret1 <- cumprod(1+ df$Returns/100)
  amount1 <- 2 * cash * as.numeric(tail(df$cumret1,1) - 1) - cash * 0.05 *4
    
  df$cumret2 <- cumprod(1+ 2*df$Returns/100)
  amount2 <- cash * as.numeric(tail(df$cumret2,1) - 1)
  
  
  tmp.pos <- c(tmp.pos, amount2/amount1)

  df2 <- bear[sample(nrow(bear), 0.05*nrow(bear))]
  df2$cumret1 <- cumprod(1+ df2$Returns/100)
  amount3 <- 2 * cash * as.numeric(tail(df2$cumret1,1) - 1) - cash * 0.05 *4
  
  df2$cumret2 <- cumprod(1+ 2*df2$Returns/100)
  amount4 <- cash * as.numeric(tail(df2$cumret2,1) - 1)
  
  tmp1 <- c(tmp1, amount3)
  tmp2 <- c(tmp2, amount4)
  
  tmp.neg <- c(tmp.neg, -amount4/amount3)
  
  i <- i + 1
}

#Plot
title <- ggdraw() + 
  draw_label(
    "兩倍槓桿 vs. 兩倍本金(融資) 盈虧比值",
    fontface = 'bold',
    x = 0,
    hjust = 0,
    size = 20
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 20)
  )

p1 <- ggplot(mapping = aes(tmp.pos)) + 
  geom_histogram(col = "black", fill = "lightblue") + 
  xlab("一般情況") + ylab("次數") +
  theme(axis.title = element_text(family = "BL", size = 16, face = "bold")) +
  geom_vline(aes(xintercept = mean(tmp.pos)), col = "red") 

p2 <- ggplot(mapping = aes(tmp.neg)) + 
  geom_histogram(col = "black", fill = "lightgrey") + 
  xlab("熊市") + 
  ylab("次數") +
  theme(axis.title = element_text(family = "BL", size = 16, face = "bold")) +
  geom_vline(aes(xintercept = mean(tmp.neg)), col = "blue")

plot_row <- plot_grid(p2,p1)

plot_grid(title,plot_row, ncol =  1, rel_heights = c(0.1, 1))

mean(tmp.pos)
mean(tmp.neg)

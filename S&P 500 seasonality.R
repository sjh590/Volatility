#Plot stock market seasonality

options(scipen = 999)
#Libraries
library(quantmod)
library(PerformanceAnalytics)
library(lubridate)
library(dplyr)
library(ggplot2)

#Load Data
enddate <- today()-1
startdate <- enddate %m+% months(-480)

getSymbols("SPY", from = startdate, to = enddate, src = "yahoo")
weekly_SPY <- to.period(SPY, period = "weeks") 
SPY_ret <- na.omit(Return.calculate(Ad(weekly_SPY)))
head(SPY_ret)
SPY_ret_df <- as.data.frame(SPY_ret)
names(SPY_ret_df) <- "Return"
SPY_ret_df$Date <- as.Date(index(SPY_ret))




p <- SPY_ret_df %>% 
  mutate(
    Year = factor(year(Date)),     # use year to define separate curves
    Date = update(Date, year = 1)  # use a constant year for the x-axis
  ) %>% 
  ggplot(aes(Date, Return, color = Year)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(labels = scales::percent, breaks = scales::pretty_breaks(n = 10))

windowsFonts(BL = windowsFont("微軟正黑體"))
# Raw daily data
p + geom_point(aes(group = Year)) +
  geom_point(data = function(x) filter(x, Year == 2020), size = 3) + 
  geom_point(data = function(x) filter(x, Year == 2008), size = 3) +
  ggtitle("S&P 500 報酬季節性分布") +
  theme(text = element_text(size = 18, family = "BL")) +
  geom_hline(yintercept=0.05, linetype="dashed") +
  geom_hline(yintercept=-0.05, linetype="dashed") +
  geom_hline(yintercept=0.03, linetype="solid") +
  geom_hline(yintercept=-0.03, linetype="solid")



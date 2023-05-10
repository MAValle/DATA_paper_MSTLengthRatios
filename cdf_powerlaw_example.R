# See the power-low asymptotic behaviour 
# We take as an example the DowJones.

# some ideas in:
# https://stackoverflow.com/questions/23457990/how-to-plot-ccdf-graph-on-a-logarithmic-scale
# https://rfmw.em.keysight.com/wireless/helpfiles/n7617/Content/Main/Understanding_CCDF_Curves.htm
# https://stackoverflow.com/questions/14736038/log-log-probability-chart-in-r
# https://stackoverflow.com/questions/28709331/logarithmic-grid-for-plot-with-ggplot2

rm(list = ls())
library(quantmod)
library(tidyverse)
library(MASS)
library(scales)

# get DJIA from the FED https://fred.stlouisfed.org/   / YEAR-month-day
# max from 2012-06-04
getSymbols("DJIA" , src="FRED", from="2012-06-04", to="2022-06-1", warnings = FALSE, auto.assign = TRUE)
prices <- DJIA
length(prices) # 2610

# plot
plot(DJIA, main="DJIA")


# daily returns: 
d_ret = periodReturn(prices, period="daily") # 2518
plot(d_ret, type='l')
hist(d_ret, 40)

# analyze positive and negative returns in absolute value
d_ret_pos = d_ret[d_ret >= 0]; plot(d_ret_pos)
d_ret_neg = abs( d_ret[d_ret < 0] ) ; plot(d_ret_neg)

all_ret <- c(as.numeric(d_ret_pos), as.numeric(d_ret_neg))
hist(all_ret, 40)

# cumulative distribution function (cdf) al all returns
f <- ecdf(all_ret)  # ecdf is a function
ls(environment(f))
f
plot(f)
# however I want a log-log plane of complementary cdf (1-Prob(X<x) = Prob(X>x))
# plot(sort(all_ret) , 1-ecdf(all_ret)(sort(all_ret) ), log="xy", ylab="log[P(X > x)]", xlab="log(x)",main="CCDF: log-log")

x <- environment(f)$x
y <- environment(f)$y
plot(x, y) # equivalent to plot(f)
y2 <- 1- environment(f)$y # ccdf  P(X>x)
plot(x, y2)
plot(log(x), log(y2))

# Let's create a dataframe for ggplot2 
df = data.frame(x=x, y=y2)
breaksy <- 10^(-6:1)
breaksx <- 10^(-6:1)
p_ccdf <- ggplot(df, aes(x, y)) + geom_point() + 
  scale_x_log10(breaks = breaksx, labels = trans_format("log10", math_format(10^.x)), limits = c( NA, 1)) +
  scale_y_log10(breaks = breaksy, labels = trans_format("log10", math_format(10^.x)), limits = c(0.0001, NA)) +
  annotation_logticks() +
  #coord_equal() + 
  annotation_logticks(base = 10) +
  theme_bw() + theme(panel.grid.minor = element_line(color="blue", linetype="dotted"), panel.grid.major = element_line(color="blue", linetype="dotted"))
p_ccdf


# Let's take the tail from x=0.01 for a linear regression
df2 <- subset(df, x >= 0.01)
df2 <- df2[-nrow(df2), ]   # drop last row because of a zero value
plot(log(df2$x), log(df2$y) )

fit <- lm( log(df2$y) ~ log(df2$x) )
summary(fit)
new = data.frame(x = log(df2$x))
y_hat=predict(fit, newdata = new, type='response')
lines(log(df2$x), y_hat, type='l', col='red')

# results indicate an estimated alpha of -2.44
# y = -12.52 - 2.44*x

# add predicted values to df and create new plot with the line
df$y_hat <- c(  rep(NA, nrow(df)-nrow(df2) )     , y_hat)
p_ccdf2 <- ggplot(df, aes(x, y)) + geom_point() + 
  scale_x_log10(breaks = breaksx, labels = trans_format("log10", math_format(10^.x)), limits = c( NA, 1)) +
  scale_y_log10(breaks = breaksy, labels = trans_format("log10", math_format(10^.x)), limits = c(0.0001, NA)) +
  annotation_logticks() +
  #coord_equal() + 
  annotation_logticks(base = 10) +
  theme_bw() + theme(panel.grid.minor = element_line(color="blue", linetype="dotted"), panel.grid.major = element_line(color="blue", linetype="dotted"))
p_ccdf2 + geom_segment(aes(x = 1.15*df2[1,1], y = df2[1,2], xend = 1.15*df2[nrow(df2),1], yend = df2[nrow(df2),2]), color="red", size=0.6) +
  ggtitle("CCDF DowJones") + xlab("Returns") + ylab("P(X > x")

# Note: the line has a tiny displacement onx-axis to see it better.

# Permutation entropy
# Script to compute permutation entropy from a tiem series based
# on paper of Band and Pompe "Permutation entropy: A natural complexity 
# measure for time series".


rm(list = ls())
library(ggplot2)
library(quantmod)

# FUNCTION DEFINITION  - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# function to compute permutation entropy
# inputs:
# time_series: 
# m : number of values to take (dimension embedding)
# output: entropy of time_series.
permutation_entropy <- function(time_series, m) {
  permutations <- matrix(0, ncol = m, nrow = length(time_series) - m + 1)
  
  for (i in 1:(length(time_series) - m + 1) ) {
    state = numeric(length=m)
    time_series_m <- time_series[i:(i + m - 1)]
    #time_series_m <- time_series[i:(i + m )]
    new = sort(time_series_m)
    state = match(new, time_series_m) - 1
    permutations[i, ] <- state
  }
  
  # Calculate the counts of each permutation
  permutation_counts <- table(apply(permutations, 1, paste, collapse = ""))
  
  # Calculate the probabilities
  probabilities <- permutation_counts / sum(permutation_counts)
  
  # Compute permutation entropy
  entropy <- -sum(probabilities * log(probabilities)) #m=2 da -0.636
  entropy <- entropy/log(factorial(m))
  #cat('entropy is: ', entropy)
  
  return(entropy)
}
# example
# entropy <- permutation_entropy(time_series, m=2)

# function to compute PermEntrop in roll windows
# inputs
# tsr: time series
# length_windows : size of the windows
# lag : overlapping times 
# output: vector with PermEntrop in each windows
ts_permentrop <- function(tsr, length_windows, lag, m) {
  entropy = numeric(length=0)
  start=1
  for ( i in 1:c(length(rw) - length_window)  ) {
    ini = start
    fin = i + length_window - 1
    series = tsr[ini:fin]
    s = permutation_entropy(time_series = series, m=m)
    entropy = c(entropy, s)
    start = start + lag
  }
  return(entropy)
}
# example
# entrp <- ts_permentrop(tsr = rw, length_windows=20, lag=1, m=2)
# FUNCTION DEFINITION  - - - - - - - - - - - - - - - - - - - - - - - - - - - 




# - - - - - - - - - - Entropy of random brownian RW
# https://www.stat.berkeley.edu/~aldous/Research/Ugrad/ZY1.pdf
N=1431 # number of points
rw = rnorm(N, 0, 1);  # like a return distributions with constant variance.
W = cumsum(rw);
#plot(W, type= 'l', main='cumRW', xlab='t', ylab='W(t)')
#permutation_entropy(time_series = W, m=2)

# PermEntr for whithe noise in roll windows
entropy <- ts_permentrop(tsr = rw, length_windows=20, lag=1, m=2)


# https://fhernanb.github.io/Graficos-con-R/par.html
# PLots
par(mar=c(1,2,1,1))
par(mfrow = c(2, 1))  # 3 rows and 2 columns
plot(c(1:length(rw)), rw, type= 'l', main=NULL, xlab='time', ylab='W(t)')
plot(c(1:length(entropy)), entropy, type= 'l', main=NULL, xlab='time', ylab='PermEntr(t)')
# - - - - - - - - - - Entropy of random brownian RW



# - - - - - - - - - - Entropy of a real time series  
getSymbols("AAPL", from='2017-12-25',to='2023-09-25', source='yahoo')
rets = dailyReturn(AAPL$AAPL.Adjusted, type='arithmetic')
length(rets)
date <- time(rets)
date <- as.Date(date, "%Y-%m-%d")

# PermEntr for AAPL returns
entropy <- ts_permentrop(tsr = as.numeric(rets), length_windows=20, lag=1, m=2)
# PermEntr for AAPL returns
entropy_prices <- ts_permentrop(tsr = as.numeric(AAPL$AAPL.Adjusted), length_windows=20, lag=1, m=2)


# https://fhernanb.github.io/Graficos-con-R/par.html
# PLots
par(mar=c(1,2,1,1))
par(mfrow = c(2, 1))  # 3 rows and 2 columns
#plot(date, rets, type= 'l', main=NULL, xlab='time', ylab='W(t)')
#plot(date[c(1:length(entropy))], entropy, type= 'l', main=NULL, xlab='time', ylab='PermEntr(t)')

plot(date, as.numeric(AAPL$AAPL.Adjusted), type= 'l', main=NULL, xlab='time', ylab='W(t)')
plot(date[c(1:length(entropy_prices))], entropy_prices, type= 'l', main=NULL, xlab='time', ylab='PermEntr(t)')
# - - - - - - - - - - Entropy of a real time series  

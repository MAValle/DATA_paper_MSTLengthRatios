# This script calculates Fisher-Information based on paper from 
# "Fisher-Information and nonlinear dynamics". 
# Martin, M. T., Perez, J., & Plastino, A. (2001). Fisher information and nonlinear dynamics. Physica A: Statistical Mechanics and its Applications, 291(1-4), 523-532.

# We take as an example the price of gold

# date creation: 03-abr-23
# name: fi_test02.R

# notes:
# 31-03-23: creation


library(quantmod)
library(tidyverse)

# retrieving gold prices from 2019.
gold <- getSymbols ('GC=F', src = 'yahoo', from = '2019-01-01', auto.assign = FALSE)
barChart(gold, theme = chartTheme('black'))

# returns
rets <- data.frame(round(dailyReturn(gold)*100,4))
hist(rets$daily.returns,30)


# function to compute number of times returs are inside a box
sumvalues <- function(lims) {
  vls <- vector()
  for ( i in 2:length(lims)) {
    vls[i-1] <- sum(rets >= lims[i-1] & rets < lims[i])
    #print(vls)
  }
  return(vls)
}


# Choose the number of intervals in which we split all the values
n <- 10
# Compute the interval widths
w <- diff(range(rets)) / n
# Compute the interval limits
lims <- seq(min(rets), max(rets), w)

# Compute the interval probabilities
p <- sumvalues(lims = lims)
p <- p / sum(p)



# Exc. 2.
# We analyze the calculation with different interval values.
# Choose the number of intervals in which we split all the values
splits  <- c(3:12)
sims <- 1 # numero de simulaciones para cada split
almacen2 <- matrix(data=NA, nrow=0, ncol=2);

# start
for (s in 1:sims) {
  almacen_split2 <- matrix(NA, ncol=2, nrow=length(splits))
  for ( i in 1:length(splits) ) {
    n = splits[i]
    # Compute the interval widths
    w <- diff(range(rets)) / n
    # Compute the interval limits
    lims <- seq(min(rets), max(rets), w)
    # Compute the interval probabilities
    p <- sumvalues(lims = lims)
    p <- p / sum(p)
    
    # calculo FI  segun "Fisher Information and nonlinear dynamics"
    fi2 <- sum (  ((diff(p))^2)/p[-1]  )
    # sacamos valores en que p=0
    drops <- which(p == 0)
    if (length(drops) > 0) {
      print(drops)
      p <- p[-drops]
    }
    
    # save data
    almacen_split2[i, ] <- c(n, fi2)
  }
  almacen2 <- rbind(almacen2, almacen_split2)
}

colnames(almacen2) <- c('split', 'fi')
almacen2 <- as.data.frame(almacen2)


library(ggplot2)
pl2 <- ggplot(almacen2, aes(x=factor(split), y=fi)) + geom_boxplot(fill='#A4A4A4', color="black") + labs(title="Fisher-Information",x="Split window", y = "FI")+
  theme_classic()
pl2

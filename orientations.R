# Calculo de las orientaciones



# Creation date: 25-abr-19
# name: orientations.R



rm(list = ls())
library(igraph)
library(ggplot2)
library(purrr)
source("get_orientation_function.R")
modo ="w" # w es weekly, d es daily
if (modo == "w") {
  data <- read.csv("data170419.csv") #data weekly
} else {
  data <- read.csv("data150419.csv") #data daily
}
data <- data[complete.cases(data), ]

# The magic
x <- get_windows_orientation(data[, c(2:ncol(data))], desfase = 5, size = 39, ini = -4, fin = 0)


# beauty of the final data before to send
df <- data.frame(block = seq_along(x), qt=x )
if (modo == "w") {
  write.csv(df,'orientations_weekly_1250419.csv')
} else {
  write.csv(df,'orientations_daily_250419.csv')
}
# # # ## ## ## ## ## ## ## # CODE # ## ## ## ## ## ## ## #

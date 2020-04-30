# Script que calcula la entropia u orientaciones netas
# por mes basado en solo dos estados 0 (baja), 1(sube)




# file: entropy_serieV3.R
# creation date: oct 23, 2019

# NOTES:
# 23-ago-19: creation 

rm(list = ls())
library(entropy)
library(purrr)
library(moments)
library(ggplot2)
source("get_orientation_function.R")



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 28.ago.2019  LOADING DATA
# loading data de los retornos de los indices
modo ="d" # w es weekly, d es daily
if (modo == "w") {
  data <- read.csv("data170419.csv") #data weekly
} else {
  #data <- read.csv("data150419.csv") #data daily
  data <- read.csv("data231019.csv") #data daily
}
data <- data[complete.cases(data), ]
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 04-sep-19
# estoy viendo como extraer las fechas para poder calcular matriz de correlacion
# por mes.
#https://stackoverflow.com/questions/17496358/r-help-converting-factor-to-date
data[,"Date"] <- as.Date(data[,"Date"], format = "%m/%d/%y")
mandy <- format(data$Date, "%m/%Y")
data$mandy <- mandy
library(igraph)
mst_largos <- c()
lasfechas_en_meses <- unique(format(data$Date, "%m/%Y"))
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# BINARIZATION DE LOS DATOS DE RENTABILIDAD
# dado que tenemos los retornos de los indices, no es necesaria aplicar la 
# funcion binarizationSerie (esto es solo para los indices). Por lo 
# tanto necesitamos solo binarizar indicando:
# si rentabilidad >= 0 , poner un 1, si rentabilidad < 0, poner un 0.
df <- data[, -c(1)]
n <- ncol(df) -1
df[, 1:n ] <- ifelse(df[, 1:n ] >= 0, 1 ,0)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# la magia
# Funcion que calcula todas las series de tiempo entropia de orientaciones
# o de estados 0/1 para cada indice.
# Input: df: dataframe con las columnas binarizada que queremos calcular la serie de tiempo entropica
# output: matriz con las mismas dimensiones de df con las series de tiempo entropicas 
# calculadas para cada MES!
compute_all_orientations <- function(df, fechas) {
  ncols <- ncol(df)
  M <- matrix(NA, ncol=ncols, nrow=length(lasfechas_en_meses))
  for (c in 1:ncols) {
    for (i in 1:length(lasfechas_en_meses) ) {
      fecha <- lasfechas_en_meses[i]
      df_temporal <- subset(df, mandy==fecha)
      mean_orientation <- mean(df_temporal[,c])
      if (mean_orientation == 0 || mean_orientation == 1) {
        entropy_o = 0
      } else {
        entropy_o <- (mean_orientation*log(mean_orientation) + (1-mean_orientation)*log(1-mean_orientation))
      }
      #temp <- getFrecuencyEntropy(states= states, dtau = dtau, TT = nrow(df_temporal), vecT = df_temporal[, c], overlap = 0)
      # temp$tabla
      M[i, c] <- entropy_o
    }
  }
  return(M)
}
salida <- compute_all_orientations(df = df[, c(1:34)], fechas = lasfechas_en_meses )
salida <- as.data.frame(salida)
salida <- cbind(salida, lasfechas_en_meses)
colnames(salida) <- colnames(df)
write.csv(salida, "entropies_from_entropy_serieV3_231019.csv", row.names = FALSE)


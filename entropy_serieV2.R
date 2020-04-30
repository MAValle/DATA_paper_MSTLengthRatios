# En este script intentamos calculamos la serie de tiempo de entropia
# para los indices tomando dtau=2 y bloques de segmento de tiempo TT = 1 mes.

# las funciones utilizadas aqui y la operatoria para calcular la serie
# proviene de entropy_serieV1.R

# Cabe aclarar que dado que se va calculando la entropia de cada mes, 
# utilizando como input los indices diarios, veremos que el segmento de tiempo T
# no es constantes. A veces es de 20, otras veces de 18. 
# Esto se debe tomar en cuenta dado esto implica que el NUMERO DE WORDS en cada
# segmento de tiempo sera variable (numero de words = T/dtau)

# Se elige dtau=2, puesto que con dtau=3, los requisitos de numero de datos 
# se hacen muy exigente para lograr un calculo de entropia que no quede tan sesgado.



# file: entropy_serieV2.R
# creation date: oct 19, 2019

# NOTES:
# 29-ago-19: 

rm(list = ls())
library(entropy)
library(purrr)
library(moments)
library(ggplot2)
source("get_orientation_function.R")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # #FUNCIONES NECESARIAS # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Funcion para crear estados:
# ORIGEN: entropy_serieV1.R
# inputs: n : numero de spins
# signos: posibles signos del spin, signo=1 0,1, signo=2 0,1,3
# output: dataframe con 2^n estados posibles y n columnas
defineStates <- function(n, signos) {
  states <- expand.grid(replicate(n, 0:signos, simplify = FALSE))
  #states$ss <- letters[1:nrow(states)]
  return(states)
}
# ejemplo
#states <- defineStates(n=2, signos=1)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# BINARIZATION OF A TIME SERIES
# ORIGEN: entropy_serieV1.R
# Funcion para detectar subida o bajada del indice. Ojo del indice, no de la rentabilidad!
# Generamos un tren de pulsos de 0 y 1: 1 cuando x(t-1) < x(t)  (up) y 0 cuando x(t-1) > x(t) down
# input: rw indice (debe ser el indice, no la rentabilidad)
# output: vector binario de 0 y 1 de largo T-1 donde T es el largo de rw.
binarizationSerie <- function(rw) {
  n <- length(rw) - 1
  b <- c()
  for (i in 1:n) {
    if (rw[i] < rw[i+1]  ) {
      b <- c(b, 1) 
    } else {
      b <- c(b, 0)
    }
  }
  return(b)
}
# ejemplo
# b <- binarizationSerie(rw)
# table(b)
# plot(b, type = "l")
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# FUNCION PARA CALCULAR LA ENTROPIA DE UN SEGMENTO DE SERIE DE TIEMPO BINARIZADA.
# funcion para calcular la entropia de una ventana de largo T y la frecuencia de palabras (estados)
# en la ventana.
# ORIGEN: entropy_serieV1.R
# inputs: 
# states: dataframe que viene de funcion defineStates que debine los estados o palabras
# dtau : es el delta tau en el paper entropy and information in neural spike trains
# TT: es la ventana de tiempo a tomar equivalente a T en el paper entropy and information in neural spike trains
# numero maximo de palabras sera TT/dtau 
# cada palabra dtau letras y por lo tanto 2^dtau combinaciones posible
# vecT = es el vector de tren de pulsos de largo T que viene del vector binario b (salida de binarizationSerie)
# overlap: es el numero de unidades de tiempo de overlap entre una palabra y otra. 
#      overlap debe ser menor o igual que dtau-1.
# Output:
# una lista con la frecuencia de las palabras acorde a states
# y con la entropia para la ventana de tiempo T
# Nota: TT debe ser igual que el largo de vecT.
# Nota: dtau debe ser igual al numero de spins o nodos en states.
getFrecuencyEntropy <- function(states, dtau, TT, vecT, overlap=0) {
  if (overlap >= dtau) {
    stop("Not computable!, overlap must be lower or igual than dtau - 1")
  }
  library(prodlim)
  x <- c()
  dtau_prima <- dtau - overlap
  number_of_words <- floor(TT/dtau_prima)
  i = 1
  for (j in 1:number_of_words) {
    fin = i + dtau - 1
    word <- vecT[i:fin]
    x <- c(x,row.match(word, states[,1:dtau]))
    i <- fin + 1 - overlap  # nos da la fila de states en que se reconoce el estado encontrado en word
  }
  tabla <- table(x) # nos da la distribucion de cada estado
  
  library(entropy)
  S <- -entropy(tabla) # 0.2*log(0.2) + 0.1*log(0.1) + 0.4*log(0.4) + 0.2*log(0.2) + 0.1*log(0.1)
  return(list(tabla = tabla, entropia = S))
}
#ejemplo
# vecT <- b[1:30]
# temp <- getFrecuencyEntropy(states= states, dtau = 2, TT = 30, vecT = vecT, overlap = 0)
# temp$tabla
# temp$entropia
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# FUNCION PARA CALCULAR LA SERIE DE TIEMPO DE ENTROPIA ENTERA, DE LA SERIE BINARIZADA.
# 30-ago-19
# ORIGEN: entropy_serieV1.R
# funcion para calcular la serie de entropia completa a partir de un tren de pulsos
# input: b  tren binario del indice discretizado (viene de funcion binarizationSerie)
# inputs: TT, dtau, overlap, states, b (tren de pulsos binarios)
# output: vector serie de tiempo de la entropia. 
# Nota: es necesario tener cargada la funcion "getFrecuencyEntropy"
get_seriesEntropy <- function(TT, dtau, overlap, states, b) {
  number_of_periods <- floor(length(b)/TT)
  i = 1
  S <- c()
  for (p in 1:number_of_periods) {
    fin = i + TT - 1
    vecT <- b[i:fin]
    temp <- getFrecuencyEntropy(states= states, dtau = dtau, TT = TT, vecT = vecT, overlap = overlap)
    #temp$tabla
    S <- c(S, temp$entropia)
    i <- fin + 1
  }
  return(S)
}
# ejemplo
# dtau=2
# states <- defineStates(n=dtau, signos=1)
# h_sn <- get_seriesEntropy(TT = 20, dtau = dtau, overlap = 0, states = states, b = binarizationSerie(sn))
# h_rw <- get_seriesEntropy(TT = 20, dtau = dtau, overlap = 0, states = states, b = binarizationSerie(rw))
# bc <- binarizationSerieChanges(rw, umbral=0.5)
# b <- bc$b
# h_rw_c <- get_seriesEntropy(TT = 20, dtau = dtau, overlap = 0, states = states, b = b )
# bc <- binarizationSerieChanges(sn, umbral=0.5)
# b <- bc$b
# h_sn_c <- get_seriesEntropy(TT = 20, dtau = dtau, overlap = 0, states = states, b = b )
# plot(h_sn, type='l', col=2)
# plot(h_sn_c, type='l', col=2)
# plot(h_rw, type='l')
# plot(h_rw_c, type='l')
# lines(h_rw, col=2)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 





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



matplot(data[,c("SPX", "IPSA", "IBOV")], type = c("l"), lty=1, col = c(3,1,2)) #plot

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


# step 1: define the states
states <- defineStates(n=2, signos=1)
# states <- defineStates(n=1, signos=1)
dtau=1
col = 1  # columna binatizada a la cual encontramos la seri de tiempo entropiaca
# la magia
# Funcion que calcula todas las series de tiempo entropia para cada indice
# Input: df: dataframe con las columnas binarizada que queremos calcular la serie de tiempo entropica
# output: matriz con las mismas dimensiones de df con las series de tiempo entropicas 
# calculadas para cada MES!
compute_all_entropies_series <- function(df, fechas) {
  ncols <- ncol(df)
  M <- matrix(NA, ncol=ncols, nrow=length(lasfechas_en_meses))
  for (c in 1:ncols) {
    for (i in 1:length(lasfechas_en_meses) ) {
      fecha <- lasfechas_en_meses[i]
      df_temporal <- subset(df, mandy==fecha)
      temp <- getFrecuencyEntropy(states= states, dtau = dtau, TT = nrow(df_temporal), vecT = df_temporal[, c], overlap = 0)
      # temp$tabla
      M[i, c] <- temp$entropia
    }
  }
  return(M)
}
# ejemplo
salida <- compute_all_entropies_series(df = df[, c(1:34)], fechas = lasfechas_en_meses )
salida <- as.data.frame(salida)
salida <- cbind(salida, lasfechas_en_meses)
colnames(salida) <- colnames(df)
write.csv(salida, "entropies_from_entropy_serieV2_231019.csv", row.names = FALSE)
plot(salida[,2], type="l")


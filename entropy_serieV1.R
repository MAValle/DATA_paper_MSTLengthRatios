# En este script intentamos calcular la entropia de una serie de tiempo
# financiero siguiendo los pasos y literatura de :
# "entropy and information in neural spike trains" y 
# "temporal coding of visual information in the thalamus"


# file: entropy_serieV1.R
# creation date: aug 28, 2019

# NOTES:
# 29-ago-19: creamos funcion par agenerar una sinusoide con y son ruido, 
#             funcion : binarizationSerie
# 30-AGO-19: intentamos una funcion que discrtiza la serie en tres estados
#           -1, 0 y 1, pero esto no prospero. Ver motivos mas abajo.
# 30-ago-19: pienso que si utilizo un dtau de 3, entences tendre 2^3=9 estados o words.
#           posibles. Si utilizo T=30, entonces en ese periodo doy espacio
#           para tener solo 10 words. Es decir, le doy poca oportunidad al sistema
#           para expresar las posibles 9 estados en 10 oportunidades. Lo que 
#           tendria que hacer es aumentar T (a costa de la serie) o disminuir dtau
#           (a costa de disminuir precision o sensibilidad de la senal)
# 30-ago-19: pienso que si utilizo dtau=2, entonces tendre 4 posibles estados. 
#           De esta forma estoy viendo solo lo que pasa dia a dia o en una 
            #resolucion muy baja de tiempo. Pero de esta forma me aseguro de 
#           tener sampleo de los 4 words en el periodo de T.
# 18-oct-19: Hacemos pruebas de calculo de series de entropia de un random walk
#          con distintos segmentos de tiempo T, pero con un mismo dtau = 2 y dtau=3.
# 19-oct-19: Hacemos pruebas de calculo de series de entropia de un white noise time series
#          con distintos segmentos de tiempo T, pero con un mismo dtau = 2 y dtau=3

rm(list = ls())
library(entropy)
library(purrr)
library(moments)
library(ggplot2)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # #FUNCIONES NECESARIAS # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Funcion para crear estados:
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
# 18-oct-19
# Hacemos pruebas de calculo de series de entropia de un random walk
# con distintos segmentos de tiempo T, pero con un mismo dtau = 2.
# step 1: define the states
states <- defineStates(n=2, signos=1)
states3 <- defineStates(n=3, signos=1)
# step 2: generar un random walk (como si fuesen rentabilidades)
set.seed(123)
GenerateRandomWalk <- function(k = 250,initial.value = 0) {
  # Add a bionomial at each step
  samples = rbinom(k,1,0.5)
  samples[samples==0] = -1
  initial.value + c(0, cumsum(samples))
}
rw <- GenerateRandomWalk(k=10000, initial.value = 0.1)
plot(rw, type = "l")
# step 3: Binarization
b <- binarizationSerie(rw)
# step 4: get the entropy series
dtau=2
time_segments <- seq(from=10, to=200, 1)
#overlaps <- length(seq(from=0, to=190, 1)) #overlap debe ser menor o igual que dtau-1.
#time_segment = 20
# Nota: k=10000 / TT=20  = debe ser entero.
# numero maximo de palabras en cada segmento sera TT/dtau 
output <- matrix(NA, ncol=5, nrow=length(time_segments))
for (i in 1:length(time_segments) ) {
  entropy_serie <- get_seriesEntropy(TT = time_segments[i], dtau = 2, overlap = 0, states = states, b = b)
  output[i,] <- c(time_segments[i], mean(entropy_serie), var(entropy_serie), skewness(entropy_serie), kurtosis(entropy_serie))
}
colnames(output) <- c("TT", "mean", "var", "ske", "kur")
plot(output[,1], output[,2], type = "l")
# lo mismo pero utilizando un dtau=3
output3 <- matrix(NA, ncol=5, nrow=length(time_segments))
for (i in 1:length(time_segments) ) {
  entropy_serie <- get_seriesEntropy(TT = time_segments[i], dtau = 3, overlap = 0, states = states3, b = b)
  output3[i,] <- c(time_segments[i], mean(entropy_serie), var(entropy_serie), skewness(entropy_serie), kurtosis(entropy_serie))
}
colnames(output3) <- c("TT", "mean", "var", "ske", "kur")
plot(output3[,1], output3[,2], type = "l", col="red")
# Grafica de mean of entropy of the time series in function of time segmentes for dtau=2 and 3
tsegments <- rep(1:length(time_segments), 2)
dtau <- (c(rep(2, length(time_segments)), rep(3, length(time_segments))))
entropy_mean <- c(output[,2], output3[,2])
dfplot <- data.frame(tsegments= tsegments , dtau = dtau, entropy_mean= entropy_mean)
ggplot(dfplot) +
  geom_line(aes(x = tsegments, y = entropy_mean, color = as.factor(dtau))) + 
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50") )  +
  theme(legend.position="bottom") + ylab("Mean entropy of time series") +
  xlab("Time Segment") 
# plot de entropy time series para TT=25, para dtau=2 y dtau=3
entropy_serie_dtau2 <- get_seriesEntropy(TT = 25, dtau = 2, overlap = 0, states = states, b = b)
entropy_serie_dtau3 <- get_seriesEntropy(TT = 25, dtau = 3, overlap = 0, states = states3, b = b)
time <- rep(1:length(entropy_serie_dtau2), 2)
entrop <- c(entropy_serie_dtau2, entropy_serie_dtau3)
dtau <- c(rep(2, length(entropy_serie_dtau2)), rep(3, length(entropy_serie_dtau3)))
plotseries <- data.frame(time = time, dtau = dtau, entrop = entrop)
par(mfrow=c(2,1))
hist(plotseries$entrop[plotseries$dtau==2])
hist(plotseries$entrop[plotseries$dtau==3])

ggplot(plotseries) +
  geom_line(aes(x = time, y = entrop, color = as.factor(dtau))) + 
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50") )  +
  theme(legend.position="bottom") + ylab("Entropy of time series") +
  xlab("Time Segment") + ggtitle("Time segment = 25")
# plot de entropy time series para TT=100, para dtau=2 y dtau=3
entropy_serie_dtau2 <- get_seriesEntropy(TT = 100, dtau = 2, overlap = 0, states = states, b = b)
entropy_serie_dtau3 <- get_seriesEntropy(TT = 100, dtau = 3, overlap = 0, states = states3, b = b)
time <- rep(1:length(entropy_serie_dtau2), 2)
entrop <- c(entropy_serie_dtau2, entropy_serie_dtau3)
dtau <- c(rep(2, length(entropy_serie_dtau2)), rep(3, length(entropy_serie_dtau3)))
plotseries <- data.frame(time = time, dtau = dtau, entrop = entrop)
par(mfrow=c(2,1))
hist(plotseries$entrop[plotseries$dtau==2])
hist(plotseries$entrop[plotseries$dtau==3])

ggplot(plotseries) +
  geom_line(aes(x = time, y = entrop, color = as.factor(dtau))) + 
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50") )  +
  theme(legend.position="bottom") + ylab("Entropy of time series") +
  xlab("Time Segment") + ggtitle("Time segment = 100") 


# heatmap: en realidad el heatmap no nos interesa mucho.
#https://stackoverflow.com/questions/11531059/creating-a-continuous-heat-map-in-r
library(akima)
resolution <- 0.05 # you can increase the resolution by decreasing this number (warning: the resulting dataframe size increase very quickly)
a <- interp(x=output[,1], y=output[,2], z=output[,3], 
            xo=seq(min(output[,1]),max(output[,1]),by=resolution), 
            yo=seq(min(output[,2]),max(output[,2]),by=resolution), duplicate="mean")
image(a) #you can of course modify the color palette and the color categories. See ?image for more explanation
filled.contour(a, color.palette=heat.colors)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 19-oct-19
# Hacemos pruebas de calculo de series de entropia de un white noise random time series
# con distintos segmentos de tiempo T, pero con un mismo dtau = 2.
# step 1: define the states
states <- defineStates(n=2, signos=1)
states3 <- defineStates(n=3, signos=1)
# step 2: generar un random walk (como si fuesen rentabilidades)
# funcion para generar serie de ruido blanco
set.seed(123)
whitheNoise <- function(k) {
  x=NULL
  x[1]=0
  for (i in 2:k) {
    x[i] = x[i-1] + rnorm(1,0,1)
  }
  return(x)
}
wn <- whitheNoise(k=10000)
plot(wn, type='l')
# step 3: Binarization
b <- binarizationSerie(wn)
# step 4: get the entropy series
dtau=2
time_segments <- seq(from=10, to=200, 1)
#overlaps <- length(seq(from=0, to=190, 1)) #overlap debe ser menor o igual que dtau-1.
#time_segment = 20
# Nota: k=10000 / TT=20  = debe ser entero.
# numero maximo de palabras en cada segmento sera TT/dtau 
output <- matrix(NA, ncol=5, nrow=length(time_segments))
for (i in 1:length(time_segments) ) {
  entropy_serie <- get_seriesEntropy(TT = time_segments[i], dtau = 2, overlap = 0, states = states, b = b)
  output[i,] <- c(time_segments[i], mean(entropy_serie), var(entropy_serie), skewness(entropy_serie), kurtosis(entropy_serie))
}
colnames(output) <- c("TT", "mean", "var", "ske", "kur")
plot(output[,1], output[,2], type = "l")
# lo mismo pero utilizando un dtau=3
output3 <- matrix(NA, ncol=5, nrow=length(time_segments))
for (i in 1:length(time_segments) ) {
  entropy_serie <- get_seriesEntropy(TT = time_segments[i], dtau = 3, overlap = 0, states = states3, b = b)
  output3[i,] <- c(time_segments[i], mean(entropy_serie), var(entropy_serie), skewness(entropy_serie), kurtosis(entropy_serie))
}
colnames(output3) <- c("TT", "mean", "var", "ske", "kur")
plot(output3[,1], output3[,2], type = "l", col="red")
# Grafica de mean of entropy of the time series in function of time segmentes for dtau=2 and 3
tsegments <- rep(1:length(time_segments), 2)
dtau <- (c(rep(2, length(time_segments)), rep(3, length(time_segments))))
entropy_mean <- c(output[,2], output3[,2])
dfplot <- data.frame(tsegments= tsegments , dtau = dtau, entropy_mean= entropy_mean)
ggplot(dfplot) +
  geom_line(aes(x = tsegments, y = entropy_mean, color = as.factor(dtau))) + 
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50") )  +
  theme(legend.position="bottom") + ylab("Mean entropy of time series") +
  xlab("Time Segment") 
# plot de entropy time series para TT=25, para dtau=2 y dtau=3
entropy_serie_dtau2 <- get_seriesEntropy(TT = 25, dtau = 2, overlap = 0, states = states, b = b)
entropy_serie_dtau3 <- get_seriesEntropy(TT = 25, dtau = 3, overlap = 0, states = states3, b = b)
time <- rep(1:length(entropy_serie_dtau2), 2)
entrop <- c(entropy_serie_dtau2, entropy_serie_dtau3)
dtau <- c(rep(2, length(entropy_serie_dtau2)), rep(3, length(entropy_serie_dtau3)))
plotseries <- data.frame(time = time, dtau = dtau, entrop = entrop)
par(mfrow=c(2,1))
hist(plotseries$entrop[plotseries$dtau==2])
hist(plotseries$entrop[plotseries$dtau==3])

ggplot(plotseries) +
  geom_line(aes(x = time, y = entrop, color = as.factor(dtau))) + 
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50") )  +
  theme(legend.position="bottom") + ylab("Entropy of time series") +
  xlab("Time Segment") + ggtitle("Time segment = 25")
# plot de entropy time series para TT=100, para dtau=2 y dtau=3
entropy_serie_dtau2 <- get_seriesEntropy(TT = 100, dtau = 2, overlap = 0, states = states, b = b)
entropy_serie_dtau3 <- get_seriesEntropy(TT = 100, dtau = 3, overlap = 0, states = states3, b = b)
time <- rep(1:length(entropy_serie_dtau2), 2)
entrop <- c(entropy_serie_dtau2, entropy_serie_dtau3)
dtau <- c(rep(2, length(entropy_serie_dtau2)), rep(3, length(entropy_serie_dtau3)))
plotseries <- data.frame(time = time, dtau = dtau, entrop = entrop)
par(mfrow=c(2,1))
hist(plotseries$entrop[plotseries$dtau==2])
hist(plotseries$entrop[plotseries$dtau==3])

ggplot(plotseries) +
  geom_line(aes(x = time, y = entrop, color = as.factor(dtau))) + 
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50") )  +
  theme(legend.position="bottom") + ylab("Entropy of time series") +
  xlab("Time Segment") + ggtitle("Time segment = 100") 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 20-oct-19
# Hacemos pruebas de calculo de series de entropia de un AR1 
# http://www.utstat.utoronto.ca/hadas/STA457/Lecture%20notes/R_armasimulation.pdf
# https://kevinkotze.github.io/ts-2-tut/
# https://nwfsc-timeseries.github.io/atsa-labs/sec-tslab-autoregressive-ar-models.html
# con distintos segmentos de tiempo T, pero con un mismo dtau = 2.
# step 1: define the states
states <- defineStates(n=2, signos=1)
states3 <- defineStates(n=3, signos=1)
# step 2: generar aurotegresive process (como si fuesen rentabilidades) OJO
set.seed(123)
ar <- arima.sim(model = list(ar = 0.8), n = 10000)
plot(ar, type="l")
# # step 3: Binarization
b <- ifelse(ar >= 0, 1 ,0)
# step 4: get the entropy series
dtau=2
time_segments <- seq(from=10, to=200, 1)
#overlaps <- length(seq(from=0, to=190, 1)) #overlap debe ser menor o igual que dtau-1.
#time_segment = 20
# Nota: k=10000 / TT=20  = debe ser entero.
# numero maximo de palabras en cada segmento sera TT/dtau 
output <- matrix(NA, ncol=5, nrow=length(time_segments))
for (i in 1:length(time_segments) ) {
  entropy_serie <- get_seriesEntropy(TT = time_segments[i], dtau = 2, overlap = 0, states = states, b = b)
  output[i,] <- c(time_segments[i], mean(entropy_serie), var(entropy_serie), skewness(entropy_serie), kurtosis(entropy_serie))
}
colnames(output) <- c("TT", "mean", "var", "ske", "kur")
plot(output[,1], output[,2], type = "l")
# lo mismo pero utilizando un dtau=3
output3 <- matrix(NA, ncol=5, nrow=length(time_segments))
for (i in 1:length(time_segments) ) {
  entropy_serie <- get_seriesEntropy(TT = time_segments[i], dtau = 3, overlap = 0, states = states3, b = b)
  output3[i,] <- c(time_segments[i], mean(entropy_serie), var(entropy_serie), skewness(entropy_serie), kurtosis(entropy_serie))
}
colnames(output3) <- c("TT", "mean", "var", "ske", "kur")
plot(output3[,1], output3[,2], type = "l", col="red")
# Grafica de mean of entropy of the time series in function of time segmentes for dtau=2 and 3
tsegments <- rep(1:length(time_segments), 2)
dtau <- (c(rep(2, length(time_segments)), rep(3, length(time_segments))))
entropy_mean <- c(output[,2], output3[,2])
dfplot <- data.frame(tsegments= tsegments , dtau = dtau, entropy_mean= entropy_mean)
ggplot(dfplot) +
  geom_line(aes(x = tsegments, y = entropy_mean, color = as.factor(dtau))) + 
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50") )  +
  theme(legend.position="bottom") + ylab("Mean entropy of time series") +
  xlab("Time Segment") 
# plot de entropy time series para TT=25, para dtau=2 y dtau=3
entropy_serie_dtau2 <- get_seriesEntropy(TT = 25, dtau = 2, overlap = 0, states = states, b = b)
entropy_serie_dtau3 <- get_seriesEntropy(TT = 25, dtau = 3, overlap = 0, states = states3, b = b)
time <- rep(1:length(entropy_serie_dtau2), 2)
entrop <- c(entropy_serie_dtau2, entropy_serie_dtau3)
dtau <- c(rep(2, length(entropy_serie_dtau2)), rep(3, length(entropy_serie_dtau3)))
plotseries <- data.frame(time = time, dtau = dtau, entrop = entrop)
par(mfrow=c(2,1))
hist(plotseries$entrop[plotseries$dtau==2])
hist(plotseries$entrop[plotseries$dtau==3])

ggplot(plotseries) +
  geom_line(aes(x = time, y = entrop, color = as.factor(dtau))) + 
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50") )  +
  theme(legend.position="bottom") + ylab("Entropy of time series") +
  xlab("Time Segment") + ggtitle("Time segment = 25")
# plot de entropy time series para TT=100, para dtau=2 y dtau=3
entropy_serie_dtau2 <- get_seriesEntropy(TT = 100, dtau = 2, overlap = 0, states = states, b = b)
entropy_serie_dtau3 <- get_seriesEntropy(TT = 100, dtau = 3, overlap = 0, states = states3, b = b)
time <- rep(1:length(entropy_serie_dtau2), 2)
entrop <- c(entropy_serie_dtau2, entropy_serie_dtau3)
dtau <- c(rep(2, length(entropy_serie_dtau2)), rep(3, length(entropy_serie_dtau3)))
plotseries <- data.frame(time = time, dtau = dtau, entrop = entrop)
par(mfrow=c(2,1))
hist(plotseries$entrop[plotseries$dtau==2])
hist(plotseries$entrop[plotseries$dtau==3])

ggplot(plotseries) +
  geom_line(aes(x = time, y = entrop, color = as.factor(dtau))) + 
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50") )  +
  theme(legend.position="bottom") + ylab("Entropy of time series") +
  xlab("Time Segment") + ggtitle("Time segment = 100") 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 





























# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 28.ago.2019  LOADING DATA
# loading data de los retornos de los indices
rm(list = ls())
library(igraph)
library(ggplot2)
library(purrr)
source("get_orientation_function.R")
modo ="d" # w es weekly, d es daily
if (modo == "w") {
  data <- read.csv("data170419.csv") #data weekly
} else {
  data <- read.csv("data150419.csv") #data daily
}
data <- data[complete.cases(data), ]
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


matplot(data[,c("SPX", "IPSA", "IBOV")], type = c("l"), lty=1, col = c(3,1,2)) #plot


# procedimiento:
# 1. cargar series de rentabilidad
# 2.definir para cada serie el umbral, ej. 2 des. estandar. 
#      si rentabilidad >0, s=1, si no es 0. 
# 3. armar matriz de estados.
# 4. definir el periodo delta
# 5. para cada delta de tiempo, calcular entropia

# paso 1
df <- data[,c("SPX", "IPSA", "IBOV")]*100
sum(data[, "SPX"] > sd(data[, "SPX"]) ) # 104
sum(data[, "IPSA"] > sd(data[, "SPX"]) ) # 126
sum(data[, "IBOV"] > sd(data[, "SPX"]) ) # 251

# paso 2 y 3
mt <- matrix(NA, ncol=3, nrow=nrow(df))
mt[,1] <- ifelse(data[, "SPX"] > sd(data[, "SPX"]), 1, 0)
mt[,2] <- ifelse(data[, "IPSA"] > sd(data[, "IPSA"]), 1, 0)
mt[,3] <- ifelse(data[, "IBOV"] > sd(data[, "IBOV"]), 1, 0)

# paso 4
size <- 10
numero_de_slots <- nrow(df)/size

# paso 5
temp <- mt[c(11:21),]

# ahora intento contar el numero de palabras
word_count <- function(temp) {
  library(prodlim)
  x <- vector(mode="numeric", length=nrow(temp))
  for (i in seq_along(1:nrow(temp))){  
    #print(i)
    x[i] <- row.match(temp[i,], states[,1:3])
    #x <- c(x, row.match(temp[i,], states[,1:3])) #1)
  }
  return(x)
}
# ejemplo:
#x <- word_count(temp)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 









rm(list = ls())
# 29-ago-19
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Funcion para generar un random walk
# https://stackoverflow.com/questions/21991130/simulating-a-random-walk
# inputs: k : numero de puntos
# OUTPUT: vector de largo k con los valores del indice
set.seed(123)
GenerateRandomWalk <- function(k = 250,initial.value = 0) {
  # Add a bionomial at each step
  samples = rbinom(k,1,0.5)
  samples[samples==0] = -1
  initial.value + c(0, cumsum(samples))
}
rw <- GenerateRandomWalk(k=3600, initial.value = 0)
plot(rw, type = "l")
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 30-ago-19
# funcion para generar serie de ruido blanco
set.seed(123)
whitheNoise <- function(k) {
  x=NULL
  x[1]=0
  for (i in 2:k) {
    x[i] = x[i-1] + rnorm(1,0,1)
  }
  return(x)
}
wn <- whitheNoise(k=3600)
plot(wn, type='l')
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 30-ago-19
# Funcion para binarizar en funcion de cambios en valor absoluto. Aqui
# solo nos interesa su hubo cambios por arriba o por sobre un umbral dado.
# Regla: if ( abs (v(t+1) )  > umbral * abs(v(t) )  ) entonces colocar 1.
# inputs: rw  vector de la serie de tiempo
# input: umbral: umbral de deteccion de cambio: por ejemplo 0.5 detectar 50% de cambio
# outputs: lista con changes (vector de cambios) y b = tren de pulsos
binarizationSerieChanges <- function(rw, umbral) {
  n <- length(rw) - 1
  b <- c()
  changes <- c()
  for (i in 1:n) {
    cambio <- (abs (rw[i+1] - rw[i] ) )/abs(rw[i])
    changes <- c(changes, cambio)
    if ( cambio > umbral  ) {
      b <- c(b, 1) 
    } else {
      b <- c(b, 0)
    }
  }
  return(list(b=b, changes=changes))
}
bc <- binarizationSerieChanges(rw, umbral=0.5)
table(bc$b)
plot(bc$b, type = "l")
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 




# 30-ago-19
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# funcion para generar una sinusoide con ruido
### Equation: y=a*sin(b*t)+c.unif*amp
# https://stackoverflow.com/questions/32111941/r-how-to-generate-a-noisy-sine-function
GenerateSinusoidal <- function(k=3600, a=3, b=2, amp=2, fr=4) {
  t <- seq(from=0, to=fr*pi, length.out=k)
  #c.unif <- runif(k)
  c.norm <- rnorm(k)
  #y1 <- a*sin(b*t)+c.unif*amp # uniform error
  y2 <- a*sin(b*t) + c.norm*amp # Gaussian/normal error
  return(y2)
}
sn <- GenerateSinusoidal(k=3600, a=3, b=2, amp=0.1, fr=6) # amp=0 sin ruido
plot(sn, t="l")
bc <- binarizationSerieChanges(sn, umbral=10)
table(bc$b)
# plot results rn and sn
plot(rw, type='l')
plot(bc$b, type = 'l', col=1)
lines(sn, col=2)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

















# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 30-ago-19
# NOTA: esto no prospero porque por algun motivo se queda
# pegado en valor 1 y ademas con este metodo no detectamos
# si la serie sube o baja al aplicar umbrables.

# funcion que discretiza serie de tiempo en tres estados:
# 1 cuando sube por cierto nivel de umbral v(t) > umbral*v(t-1) 
# 0 cuando no hay variacion v(t) esta dentro de +- umbral*v(t-1)
# -1 cuando baja por un cierto nivel de umbral v(t) < umbral*v(t-1)
# input: rw    vector con valores de la serie de tiempo
# input: umbral: % de cambio adminido para generar un spike positivo o negativo
# output: vector con tren de pulsos.
umbral = 1.02
n <- length(rw) - 1
b <- c()
for (i in 1:n) {
  y <- ifelse (umbral*rw[i] < rw[i+1], 1,
               ifelse( umbral*rw[i] > rw[i+1], -1, 0 ) ) 
  b <- c(b, y)
}
table(b)
plot(rw, type='l', col=2)
plot(b, type='l')
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 




# De manera similar a trying_v3.R, calculamos el LArgo de la red PMFG 
# de todos los activos.

# NOTA: El largo del camino necesario dentro del PMFG
# para recorrer los nodos de un grupo o continente prefefinido.
# NO se calcula porque corresponde a la solucion encontrada en el MST. 
# Hay que recordar que el PMFG incluye al MST, por lo tanto encontrar
# estos recorridos en el PMFG no es necesario.


# Se calcula el PMFG
# Se calcula el largo del PMFG definido como la suma de todos los edges (distancias)
# del PMFG


# Creation date: 30.abr.20
# name: trying_v7.R

# Notes:
# 30-abr-20: creacion

# # # # # # # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # #
# 04.sep.19
# lectura de datos de Retornos acciones + bonos_daily_5y.csv

rm(list = ls())
#source("find_mst_barrier_function.R")
library(igraph)
library(ggplot2)
library(purrr)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 28.ago.2019  LOADING DATA
# loading data de los retornos de los indices
modo ="d" # w es weekly, d es daily
if (modo == "w") {
  data <- read.csv("data170419.csv") #data weekly
} else {
  #data <- read.csv("data150419.csv") #data daily
  data <- read.csv("data251019.csv") #data daily
}
data <- data[complete.cases(data), ]
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# 17-abr-19
# aqui parte lo nuevo

#input:
#columnas en data en las que se encuentra cada tipo de instrumento
columnas <- list("america" = c(2:10),
                 "namerica" = c(2:4), 
                 "latam" = c(5:10), 
                 "europe" = c(11:19), 
                 "asiaocea" = c(20:28),
                 "all_indices" = c(2:28)
)
#ejemplo: colsy$america
# length(colsy) =  6
# # # # # # # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 04-sep-19
# estoy viendo como extraer las fechas para poder calcular matriz de correlacion
# por mes.
#https://stackoverflow.com/questions/17496358/r-help-converting-factor-to-date
data[,"Dates"] <- as.Date(data[,"Dates"], format = "%m-%d-%y")
mandy <- format(data$Date, "%m/%Y")
data$mandy <- mandy
lasfechas_en_meses <- unique(format(data$Date, "%m/%Y"))
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 



# # # # # # # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # #
#30-abr-20
# FINDING THE PMFG

# # # # # # # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # #
# FUNCION BASADA EN trying_v3.R
#30-abr-20
# funcion que genera los largos de PMFG POR MES, segun las fechas de cada dia en data
# inputs: data: data con los datos DIARIOS, debe ser procesada antes con fechar en formato date.
# inputs: lasfechas_en_meses  vector con las fechas en meses
# inputs: numcolu: vector con el numero de columnas de data que se desean considerar
#     para hacer el MST. Ver vector columnas mas arriba.
# outputs: mst_largos: vector con los largos de MST para CADA MES.
# Nota: las libraris igrpah y purrr deben ejecutarse antes de correr la funcion.
monthly_pmfglength <- function(data, lasfechas_en_meses, numcolu) {
  library(NetworkToolbox)
  pmfg_largos <- vector(mode="numeric", length=length(lasfechas_en_meses))
  for (i in 1:length(lasfechas_en_meses) ) {
    fecha <- lasfechas_en_meses[i]
    df_temporal <- subset(data, mandy==fecha)
    # calcula de la matriz de correlacion
    #rho_matrix <- cor(df_temporal[,c(2:34)],  method="pearson")
    rho_matrix <- cor(df_temporal[,numcolu],  method="pearson")
    pmfg <- TMFG(rho_matrix)
    rho_pmfg <- pmfg$A # matriz de correlacion filtrada
    d_matrix_pmfg <- sqrt(1 - rho_pmfg) # los valores iguales a 0 quedaron igual a 1, hay que volver a dejarlos en cero.
    d_matrix_pmfg[d_matrix_pmfg  == 1] <- 0
    # pmfg netqork creation
    g_pmfg  <- graph_from_adjacency_matrix(d_matrix_pmfg , mode="upper", weighted=TRUE, diag=FALSE)
    # capturar el largo del pmfg
    # E(mst_g)$weight  aqui estan las distancias
    pmfg_length <- sum(E(g_pmfg)$weight)
    #mst_largos <- c(mst_largos, mst_length) # este es el output: msl length por mes.
    pmfg_largos[i] <- pmfg_length 
  }
  return(pmfg_largos)
}
# ejemplo:
vv <- monthly_pmfglength(data = data, lasfechas_en_meses = lasfechas_en_meses, numcolu = columnas$all_indices)
#creacion del aoutput: dates en una columna y pmfg largo en la otra
pmfgL <- data.frame(date=lasfechas_en_meses, PMFG_length=vv, stringsAsFactors =FALSE)
write.csv(pmfgL, "PMFG_length_from_trying_v7_300420.csv", row.names = FALSE)
# # # # # # # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # #
# De manera similar a trying_v3.R, calculamos el LArgo de la red PMFG 
# de todos los activos.

# NOTA: El largo del camino necesario dentro del PMFG
# para recorrer los nodos de un grupo o continente predefinido.
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
# 01-nov-20: la base en excel original  excel060520XX.xlsx tenia el problema de que
#           las rentabilidades de australia y reino unido eran casi las mismas. 
#           Este error fue corregido, de manra que cargamos asx200 vs ukx_daily returns_02072001_30042020_stata11.dta
#           que contiene las rentabilidades correctas y las reemplazados en data.
#           Lo anterior fue corregido en code_050820_ising.R del proyecto  MST-ISING-VALLE.

# # # # # # # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # #
# 04.sep.19
# lectura de datos de Retornos acciones + bonos_daily_5y.csv

rm(list = ls())
#source("find_mst_barrier_function.R")
library(igraph)
library(ggplot2)
library(purrr)
library(NetworkToolbox)

# 06-may-2020: esto ya no seria necesario porque trabajamos todo con retornos diarios
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 28.ago.2019  LOADING DATA
# loading data de los retornos de los indices
# modo ="d" # w es weekly, d es daily
# if (modo == "w") {
#   data <- read.csv("data170419.csv") #data weekly
# } else {
#   #data <- read.csv("data150419.csv") #data daily
#   #data <- read.csv("data251019.csv") #data daily
#   data <- read.csv("data050520.csv") #data daily
# }
# data <- data[complete.cases(data), ]
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


data <- read.csv("financial_data_011020_returns.csv")
# 4913 rentabilidades diarias de 27 indices de paises.
# no es necesario hacer tratamiento de fechas porque esa ya viene listo de code_050820_ising.R del proyecto  MST-ISING-VALLE.
data$X <- NULL

# columns:
1 "Dates"    
2 "SPX"       NA    
3 "CCMP"      NA
4 "SPTSX"     NA
5 "MEXBOL"    latam
6 "IBOV"      latam
7 "IPSA"      latam
8 "MERVAL"    latam
9 "SPBLPGPT"  latam
10 "COLCAP"   latam   
11 "UKX"      europe
12 "CAC"      europe
13 "DAX"      europe
14 "IBEX"     europe
15 "FTSEMIB"  europe
16 "AEX"      europe
17 "OMX"      europe
18 "RTSI."    europe
19 "SMI"      europe
20 "NKY"      asia
21 "HSI"      asia
22 "KOSPI"    asia
23 "TWSE"     asia
24 "JCI"      asia
25 "FBMKLCI"  asia
25 "STI"      asia
27 "ASX"      asia
28 "NZSE"     asia
29 "mandy"  




# 17-abr-19 / actualizado el 050520
# aqui parte lo nuevo
#input:
#columnas en data en las que se encuentra cada tipo de instrumento
columnas <- list("namer" = c(2,4),
                 "latam" = c(5:10),
                 "america" = c(2:10),
                 "europe" = c(11:19), 
                 "asiaoc" = c(20:28),
                 #"commodities" = c(31,32,33,34), 
                 #"bonds" = c(3,4,5),
                 "all_indices" = c(2:28))
#"continents" = c(2,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,28,29,30 ) )
#ejemplo: colsy$america
# length(colsy) =  6
# # # # # # # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # #







# # # # # # # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # #
# 04-sep-19/ actualizado el 050520
# estoy viendo como extraer las fechas para poder calcular matriz de correlacion
# por mes.
#https://stackoverflow.com/questions/17496358/r-help-converting-factor-to-date
#data[,"Dates"] <- as.Date(data[,"Dates"], format = "%m-%d-%y")
#mandy <- format(data$Dates, "%m/%Y")
#data$mandy <- mandy
library(igraph)
#mst_largos <- c()
#lasfechas_en_meses <- unique(format(data$Date, "%m/%Y"))
lasfechas_en_meses <- unique(data$mandy)
# # # # # # # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # #



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
write.csv(pmfgL, "PMFG_length_from_trying_v7_011020.csv", row.names = FALSE)
# # # # # # # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # #

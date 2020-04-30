# Lo que hacemos aqui es obtener:
# 1. el degree medio de los nodos del MST para cada periodo
# 2. the leaf number o numero de hojas del MST (nodos con degre = 1)
# 3. diameter of the tree
# 4. the tree hierarchy 

# En particular nos concentramos en Th = leaf number / 2mBmax
# donde m = numero de edges del tree y Bmax es el maximo betweenness de cualquier
# nodo del arbol.

# Para un arbol equivalente a un simple path, Th = 2/m, y se aproxima a 0.5 (con large N)
# para un arbol consistente en una estrella.
# Th toma valores entre 2/m y 1 para arboles con topologia entre un simple path
# y estrella

# la Referencia es "The trees and teh forest: characterization of complex
# brain netqorks with MST" de Stam.

# Creation date: 29.nov.19
# name: trying_v6.R

# Notes:
# 29-nov-19: creation

# # # # # # # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # #
# 04.sep.19
# lectura de datos de Retornos acciones + bonos_daily_5y.csv

rm(list = ls())
source("find_mst_barrier_function.R")
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
# 29-nov-19
# Funcion para calcular el tree hiearchy:
# Pasos:
# 1. obtener el MST
# 2. contar numero de edges = m
# 3. contar el numero de nodos con degree = 1
# 4. calcular el betweennes maximo
# 5. calcular el Th.
# Inputs:
#     *data = la data original en donde se encuentra la informacion
#     *numcolu numero de columnas de data que representan los nodos del grafo
#     *lasfechas_en_meses: vector con las fechas en meses de data. 
# Outputs:
#     * Th: tree hierarchy
#     * leafs = numero de hojas (nodos con degree 1) / numero total de nodos
tree_hiearchy_index <- function(data) {
  rho_matrix <- cor(data,  method="pearson")
  # conversion de correlaciones a distancias
  d_matrix <- sqrt(1 - rho_matrix)
  # net creation from distances
  g_net <- graph_from_adjacency_matrix(d_matrix, mode="upper", weighted=TRUE, diag=FALSE)
  mst_g <- minimum.spanning.tree(g_net, algorithm="prim")
  # step 2
  m <- sum(E(mst_g))
  # step 3
  dg <- degree(mst_g)
  leafs <- sum(dg == 1)
  # step 4
  # se calcula al betweenness de cada nodo considerando los pesos
  bt <- betweenness(mst_g, directed = FALSE, normalized = FALSE)
  maxbt <- max(bt)
  # step 5
  Th <- leafs/(2*m*maxbt)
  median_dg <- median(dg)
  
  return(list(Th = Th, leafs = leafs/sum(V(mst_g)), dg = median_dg ) )
}
# ejemplo
# tree_hiearchy_index(data = data)
# # # # # # # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # #

# # # # # # # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # #
# 29-nov-19
# Se calcula la serie de tiempo del tree hiearchy para la serie mensual 
# del MST.
# Nota: tiene que estar data cargada y columnas y lasfechas_en_meses
library(dplyr)
iteraciones <- length(lasfechas_en_meses) - 1
tree_hier <- vector(mode = "numeric", length = iteraciones)
leafs <- vector(mode = "numeric", length = iteraciones)
dates <- vector(mode = "numeric", length = iteraciones)
for (t in 1:iteraciones ) {
  fecha <- lasfechas_en_meses[t]
  df_temporal <- subset(data, mandy==fecha)
  df_temporal <- select_if(df_temporal, is.numeric)
  R <- tree_hiearchy_index(data = df_temporal)
  dates[t] <- lasfechas_en_meses[t]
  tree_hier[t] <- R$Th
  leafs[t] <- R$leafs
  dg[t] <- R$dg
}
salida <- data.frame(dates = dates, treehier = tree_hier, leafs = leafs, median_degree=dg)
plot(1:iteraciones, log(salida$treehier), type='l')
plot(1:iteraciones, (salida$leafs), type='l')
plot(1:iteraciones, (salida$median_degree), type='l')

write.csv(salida, "tree_hierarchy_from_trying_v6_291119.csv", row.names = FALSE)
# # # # # # # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # #




























# # # # # # # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # #
# 15-nov-19
# Funcion para crear el survival de los edges de la red.
# steps:
# 1. obtener el MST en t-1 e identificar los edges que conectan cada nodo
# 2. obtener el MST en t e identificar los esges de conectan cada nodo
# 3. identificar los edges que siguen conectados entre las dos redes.
# Funcion para calcular la sovrevivencia de los edges de una red
# Inputs: 
#     *t0, te: tiempo de inicio, tiempo fin para comparar las dos redes
#     *data = la data original en donde se encuentra la informacion
#     *numcolu numero de columnas de data que representan los nodos del grafo
#     *lasfechas_en_meses: vector con las fechas en meses de data. 
# Output:
#     * un vector que indica el indice S = numero de edges que sobrevivieron / N
#       donde N = numero total de edges que siempre es es constante.
edge_survival <- function(t0, te, data, numcolu, lasfechas_en_meses) {
  # step 1 y : obtener el MST
  fecha1 <- lasfechas_en_meses[t0]
  fecha2 <- lasfechas_en_meses[te]
  df_temporal1 <- subset(data, mandy==fecha1)
  df_temporal2 <- subset(data, mandy==fecha2)
  # calcula de la matriz de correlacion
  rho_matrix1 <- cor(df_temporal1[,numcolu],  method="pearson")
  rho_matrix2 <- cor(df_temporal2[,numcolu],  method="pearson")
  # conversion de correlaciones a distancias
  d_matrix1 <- sqrt(1 - rho_matrix1)
  d_matrix2 <- sqrt(1 - rho_matrix2)
  # net creation from distances
  g_net1 <- graph_from_adjacency_matrix(d_matrix1, mode="upper", weighted=TRUE, diag=FALSE)
  g_net2 <- graph_from_adjacency_matrix(d_matrix2, mode="upper", weighted=TRUE, diag=FALSE)
  mst_g1 <- minimum.spanning.tree(g_net1, algorithm="prim")
  mst_g2 <- minimum.spanning.tree(g_net2, algorithm="prim")
  
  # step 3: identificar los edges que siguen conectados entre las dos redes.
  #https://stackoverflow.com/questions/39042814/identify-number-of-same-different-edges-between-two-igraph-objects-r
  same = length(E(intersection(mst_g2, mst_g1)))
  S <- same/length(numcolu)
  return(S)
}
# ejemplo
S <- edge_survival(t0=1, te=2, data = data, numcolu = columnas$all_indices,
                   lasfechas_en_meses = lasfechas_en_meses) 
# # # # # # # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # #




# # # # # # # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # #
# 15-nov-19
# creamos serie de tiempo con los survival de los edges
# Nota: tiene que estar data cargada y columnas y lasfechas_en_meses
iteraciones = length(lasfechas_en_meses) - 1
survivals <- vector(mode = "numeric", length = iteraciones)
dates <- vector(mode = "numeric", length = iteraciones)
# Si la base de datos tiene T fechas, el vector survivals tendra T-1 datos
# y dates comanzara de la fecha 2.
for (t in 1:iteraciones ) {
  S <- edge_survival(t0=t, te=t+1, data = data, numcolu = columnas$all_indices,
                     lasfechas_en_meses = lasfechas_en_meses) 
  dates[t] <- lasfechas_en_meses[t+1]
  survivals[t] <- S
}
salida <- data.frame(dates = dates, survivals = survivals)
write.csv(salida, "edgesurvivals_from_trying_v5_151119.csv", row.names = FALSE)

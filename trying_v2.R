# Lo que hacemos aqui es leer la base de datos de indices de bonos y acciones
# de la data que viene de data150419.csv original de data_xlxs_150419.xlxs
# y sacamos las correlaciones entre los indices de cada tipo para luego calcular
# el MST asociado, y finalmente encontrar el largo del MST L.
# El largo del MST lo calculamos tomando datos correlativos de 40 dias, es decir
#con ventanas de 40 dias y con 10 dias de datos repetidos entre ventana y ventana.

# 15-abr-19
# lectura de datos de Retornos acciones + bonos_daily_5y.csv

rm(list = ls())
library(igraph)
library(ggplot2)
library(purrr)
data <- read.csv("data150419.csv")
data <- data[complete.cases(data), ]


# 17-abr-19
# aqui parte lo nuevo

#input:
#columnas en data en las que se encuentra cada tipo de instrumento
columnas <- list("america" = c(2:9), "europe" = c(10:18), "asia" = c(19:25),
              "oceania" = c(29:30), "commodities" = c(31:34), "bonds" = c(26:28),
              "all_indices" = c(2:34))
#ejemplo: colsy$america
# length(colsy) = 6

# # # ## ## ## ## ## ## ## #Funcion para seleccionar muestras # ## ## ## ## ## ## ## #
#crear una funcion extraer datos de fila ini:fin con columnas deseadas para 
#cada tipo de insturmento y cada datos quede en una lista distinta
# input:
#data # donde se encuentra la data
# input:
#ini : inicio de fila a considerar
#fin : termino de fila a considerar
# output
# listas con dataframes de grupos de mismos instrumentos con filas de ini a fin
get_subdt <- function(data, colsy, ini, fin) {
  output <- vector("list", length(colsy))
  #filas <- c(ini:fin)
  for (i in seq_along(colsy)) {
    columns <- colsy[[i]]
    #print(columns)
    output[[i]] <- data[ c(ini:fin), columns]
  }
  return(output)
}
# ejemplo
#df <- get_subdt(data=data, colsy=columnas, ini=1, fin=40)
# # # ## ## ## ## ## ## ## #F uncion para seleccionar muestras # ## ## ## ## ## ## ## #

# # # ## ## ## ## ## ## ## # CODE # ## ## ## ## ## ## ## #
desfase <- 5
ini <- -4
fin <- 0
cont <- 1
storage_data <- matrix(NA, ncol=length(columnas)+1, nrow=nrow(data))
while ( fin <= nrow(data) ) {
  cat("\r", "Processing data..Iteration number:", cont)
  ini <- ini + desfase
  #print(ini)
  fin <- ini + 39
  # get teh sub-data
  df <- get_subdt(data=data, colsy=columnas, ini=ini, fin=fin)
  # calculo de las correlaciones para cada dataframe contenida en la lista
  rho_list <- map(df, cor, method="pearson")
  # conversion a distancias
  distances_list <- map(rho_list, function(df) sqrt(1-df) )
  # net creation from distances
  g_list <- map(distances_list, graph_from_adjacency_matrix, mode="upper", weighted=TRUE, diag=FALSE)
  mst_g_list <- map(g_list, minimum.spanning.tree, algorithm="prim")
  # E(mst_g)$weight  aqui estan las distancias
  largos_list <- map(mst_g_list, function(net) sum(E(net)$weight) )
  all_lengths <- unlist(largos_list)
  content <- c(cont, all_lengths)
  storage_data[cont, ] <- content
  cont <- cont + 1
}
# # # ## ## ## ## ## ## ## # CODE # ## ## ## ## ## ## ## #
# beauty of the final data before to send
storage_data <- as.data.frame(storage_data)
colnames(storage_data) <- c("bloack", names(columnas))
storage_data <- storage_data[complete.cases(storage_data), ]
write.csv(storage_data,'mst_lengths_170419.csv')
# # # ## ## ## ## ## ## ## # CODE # ## ## ## ## ## ## ## #

plot(storage_data[,7], type="l" )

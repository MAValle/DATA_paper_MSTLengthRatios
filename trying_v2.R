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
modo ="d" # w es weekly, d es daily
if (modo == "w") {
  data <- read.csv("data170419.csv") #data weekly
} else {
  data <- read.csv("data150419.csv") #data daily
}
data <- data[complete.cases(data), ]


# 17-abr-19
# aqui parte lo nuevo

#input:
#columnas en data en las que se encuentra cada tipo de instrumento
columnas <- list("america" = c(2,6,7,8,9,10,11,12), 
                 "europe" = c(13,14,15,16,17,18,19,20,21), 
                 "asia" = c(22,23,24,25,26,27,28,29,30),
                 #"oceania" = c(28,29), 
                 "commodities" = c(31,32,33,34), 
                 "bonds" = c(3,4,5),
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
colnames(storage_data) <- c("block", names(columnas))
storage_data <- storage_data[complete.cases(storage_data), ]
if (modo == "w") {
  write.csv(storage_data,'mst_lengths_weekly_170419.csv')
} else {
  write.csv(storage_data,'mst_lengths_daily_170419.csv')
}
# # # ## ## ## ## ## ## ## # CODE # ## ## ## ## ## ## ## #

plot(storage_data[,7], type="l" )



# 24-abr-19
# viendo como calcular el largo que hay entre isntrumentos de un mismo tipo 
# dentro de un MST.
# un plot de mst de un periodo cualquiera
mstnet <- mst_g_list[[7]] # mst
coords <- layout_nicely(mstnet, dim = 2)
plot(mstnet, layout = coords,
     vertex.size = 10,
     edge.width = 3,
     vertex.label.cex=0.6,
     #vertex.color="white",
     vertex.color="orange", 
     vertex.frame.color="#ffffff",
     vertex.label.color="black")

names_america <- names(data[,c(2,6,7,8,9,10,11,12)]) # select the nodes having these names
names_oceania <- names(data[,c(28,29)])

# https://www.biostars.org/p/100850/
#g2 <- induced.subgraph(graph=mstnet,vids=unlist(neighborhood(graph=mstnet,order=2,nodes=names_oceania))) # no sirve
#https://ourednik.info/maps/2018/09/21/create-a-subgraph-from-the-neighborhood-of-specific-vertices-in-igraph/
#selegoV <- ego(mstnet, order=1, nodes = names_america, mode = "all", mindist = 0) # me dan grafos desconectados
#g2 <- subgraph(mstnet, names_america) # no sirve, deja los nodos aislados
#g2 <- induced_subgraph(mstnet, vids=names(data[,c(2:9)])) #no sirve deja los nodos aislados
#g2  <- induced_subgraph(mstnet,unlist(selegoV))
plot(g2,
     vertex.size = 10,
     edge.width = 3,
     vertex.label.cex=0.6,
     #vertex.color="white",
     vertex.color="orange", 
     vertex.frame.color="#ffffff",
     vertex.label.color="black")




plot.igraph(g, layout = -coords[,2:1],
            vertex.shape="rectangle",
            vertex.size=10,
            vertex.size2=1,
            vertex.color=NA,
            vertex.frame.color=NA,
            vertex.label.color="black",
            vertex.label.family="sans",
            edge.label.color="white",
            edge.arrow.mode=0,
            edge.width=3,
            asp=5)





https://stackoverflow.com/questions/24597523/how-can-one-set-the-size-of-an-igraph-plot
https://stackoverflow.com/questions/48746804/adjusting-visualization-of-igraph-plot
https://www.r-graph-gallery.com/248-igraph-plotting-parameters/
https://rstudio-pubs-static.s3.amazonaws.com/337696_c6b008e0766e46bebf1401bea67f7b10.html
https://stackoverflow.com/questions/23367765/extract-a-connected-subgraph-from-a-subset-of-vertices-with-igraph

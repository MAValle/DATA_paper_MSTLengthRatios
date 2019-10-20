# Lo que hacemos aqui es lo mismo que en trying_v2.R que es leer la base de 
# datos de indices de bonos y acciones
# de la data que viene de data150419.csv original de data_xlxs_150419.xlxs
# y sacamos las correlaciones POR MES. 
# Para esto tenemos que ir subseteando los datos POR MES.
# El output sera una matriz de correlaciones POR MES y en consecuencia
# la serie de MST L por MES. 


# 04.sep.19
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
                 "oceania" = c(28,29), 
                 "commodities" = c(31,32,33,34), 
                 "bonds" = c(3,4,5),
                 "all_indices" = c(2:34),
                 "continents" = c(2,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,28,29 ) )
#ejemplo: colsy$america
# length(colsy) =  6


# # # # # # # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # #
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
# # # # # # # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # #

# # # # # # # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # #
#06-sep-19
# funcion que genera los largos de MST POR MES, segun las fechas de cada dia en data
# inputs: data: data con los datos DIARIOS, debe ser procesada antes con fechar en formato date.
# inputs: lasfechas_en_meses  vector con las fechas en meses
# inputs: numcolu: vector con el numero de columnas de data que se desean considerar
#     para hacer el MST. Ver vector columnas mas arriba.
# outputs: mst_largos: vector con los largos de MST para CADA MES.
# Nota: las libraris igrpah y purrr deben ejecutarse antes de correr la funcion.
monthly_mstlength <- function(data, lasfechas_en_meses, numcolu) {
  mst_largos <- vector(mode="numeric", length=length(lasfechas_en_meses))
  for (i in 1:length(lasfechas_en_meses) ) {
    fecha <- lasfechas_en_meses[i]
    df_temporal <- subset(data, mandy==fecha)
    # calcula de la matriz de correlacion
    #rho_matrix <- cor(df_temporal[,c(2:34)],  method="pearson")
    rho_matrix <- cor(df_temporal[,numcolu],  method="pearson")
    # conversion de correlaciones a distancias
    d_matrix <- sqrt(1 - rho_matrix)
    # net creation from distances
    g_net <- graph_from_adjacency_matrix(d_matrix, mode="upper", weighted=TRUE, diag=FALSE)
    mst_g <- minimum.spanning.tree(g_net, algorithm="prim")
    # capturar el largo del mst
    # E(mst_g)$weight  aqui estan las distancias
    mst_length <- sum(E(mst_g)$weight)
    #mst_largos <- c(mst_largos, mst_length) # este es el output: msl length por mes.
    mst_largos[i] <- mst_length
  }
  return(mst_largos)
}
# ejemplo:
#vv <- monthly_mstlength(data = data, lasfechas_en_meses = lasfechas_en_meses, numcolu = columnas$continents)
#creacion del aoutput: dates en una columna y mst largo en la otra
#mstL <- data.frame(date=lasfechas_en_meses, mst_length=vv, stringsAsFactors =FALSE)
# # # # # # # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # #


# # # # # # # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # #
# 06-sep-19
# generamos los mst lenght para todas las combinaciones: asua, europa, etc... de un viaje
mstL <- matrix(NA, ncol = length(columnas)+1, nrow=length(lasfechas_en_meses))
mstL[,1] <- lasfechas_en_meses
for (j in 1:length(columnas) ) {
  vv <- monthly_mstlength(data = data, lasfechas_en_meses = lasfechas_en_meses, numcolu = columnas[[j]])
  mstL[,j+1] <- vv
}
colnames(mstL) <- c("date", names(columnas))
mstL <- as.data.frame(mstL)
N=ncol(mstL)-1
for (i in 1:N){
  mstL[,i+1]  <- as.numeric(as.character( mstL[,i+1] ))
}
mstL$date <- ( as.character(mstL[,1]))


#escritura de la dataframe
write.csv(mstL, "mstlength_from_trying_v3_181019.csv", row.names = FALSE)
plot(factor(mstL$date), mstL$mst_length)
# # # # # # # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # #




# trato de graficar la serie con eje x manteniendo formato date.
mstL$Date <- as.Date(mstL$date, format = "%d/%m/%y") no funciona
require(ggplot2)
ggplot( data = mstL, aes( 1:nrow(mstL), mst_length )) + geom_line()




# # # # # # # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # #








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
size <- 39
storage_data <- matrix(NA, ncol=length(columnas)+1, nrow=nrow(data))
while ( fin <= nrow(data) ) {
  cat("\r", "Processing data..Iteration number:", cont)
  ini <- ini + desfase
  #print(ini)
  fin <- ini + size
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


# aqui voy
# 24-abr-19
# viendo como calcular el largo que hay entre instrumentos de un mismo tipo 
# dentro de un MST.
# un plot de mst de un periodo cualquiera
mstnet <- mst_g_list[[6]] # mst
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
x <- vector() # aqui vamos almacenando los edges id

pair_nodes <- combn(names_america,2)
i=2
par <- pair_nodes[,i]
p <- get.shortest.paths(mstnet, from=par[1], to=par[2])
p <- p$vpath[[1]] # path between nodes (get the nodes all passing them)
# ahora necesito los edges id que conectan lso vertices en p, y los meto en x
x <- c(x, E(mstnet, path = p) ) # esta es la clave: https://igraph.org/r/doc/E.html
.....

#finalmente
x <- unique(x)
sum(E(mstnet)$weight[x])














edgl <- get.edgelist(mstnet)
E(mstnet)
E(mstnet)$weight # problema aparecen solo NAs
summary(mstnet)
#find the id row in edgl that match par[1] y par[2]


which( (edgl[,1] == par[1]) &&  (edgl[,2] == par[2]) )
which( (edgl[,1] == par[2]) && (edgl[,2] == par[1]) )






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
https://stackoverflow.com/questions/7931504/find-all-paths-between-two-vertices-nodes
# para el paper de Magner
# En este script, al igual que en los otros, se calcula el MST, 
# pero no el largo, tambien otros elementos del MST tales como:
# numero de hojas
# average shortest path lenght
# Diameter (maximun distance between any two edges of the tree)
# Lf = number of leaves divided by the maximum number of potential leaves of the tree (a star-like tree)
# Dn =  diameter divided by the total number of edges.


# name: mst_props.R

# Notes
# 24-sep-20: creation, 
# 08-oct-20: se agrega el degree mean del mst, la mediana del degree y el strenght del mst




# # # # carga de paquetes y data  (son 27 indices)
rm(list = ls())
library(igraph)
library(ggplot2)
library(purrr)
source("monthly_mstlength_function.R")
#data <- read.csv("ipsa_230620_nmagner_csv.csv") #data daily de ReNTABILIDADES
data <- read.csv("data_paper_magner_jul20.csv") #data daily de ReNTABILIDADES
colSums(is.na(data)) # para sumar cuantos datos NA hay en cada columna.



# # # # Tratamiento de las fechas
fechas <- data$Date  # las fechas vienen en formato mes/dia/ano
#fechas <- droplevels(fechas)
fechas2 <- data.frame(as.Date(fechas, format = "%m/%d/%y"))
#fechas2 <- data.frame(as.Date(fechas, format = "%d/%m/%y"))
#fechas2 <- data.frame(as.Date(fechas, format = "%d-%m-%Y"))
colnames(fechas2) <- "fechas2"

data$fechitas <- fechas2$fechas2
mandy <- format(data$fechitas, "%m/%Y")
data$mandy <- mandy
as.data.frame(table(data$mandy))  # chequeo de la distribucions de dias por meses
data$fechitas <- NULL
# # # # Tratamiento de las fechas


# # EN CASO que quiera convertir los NA en rentabilidad 0:
data[is.na(data)] <- 0
colSums(is.na(data)) # para sumar cuantos datos NA hay en cada columna.
data$IGBC <- NULL  # el IGBC tiene muchos datos perdidos y no podemos calcularla correlacion despues.
all_indices = c(2:27)
# # EN CASO que quiera convertir los NA en rentabilidad 0:



columnas <- list("namer" = c(2:4),
                 "latam" = c(5:10),
                 "america" = c(2:10),
                 "europe" = c(11:19), 
                 "asiaoc" = c(20:28),
                 #"commodities" = c(31,32,33,34), 
                 #"bonds" = c(3,4,5),
                 "all_indices" = c(2:28))


# # # # calculo de mst lengths
# generamos los mst lenght para todas las combinaciones: asia, europa, etc... de un viaje
lasfechas_en_meses <- unique(data$mandy)
#mstL <- matrix(NA, ncol = length(columnas)+1, nrow=length(lasfechas_en_meses))
#mstL <- matrix(NA, ncol = 2, nrow=length(lasfechas_en_meses))
#mstL[,1] <- lasfechas_en_meses
#for (j in 1:length(columnas) ) {
vv <- monthly_props(data = data, lasfechas_en_meses = lasfechas_en_meses, numcolu = columnas$all_indices  )
#  mstL[,j+1] <- vv
#}
num_leaves <- vv[[1]]
diam <- vv[[2]]
avpl <- vv[[3]]
mean_dg <- vv[[4]]
median_degree <- vv[[5]]
st <- vv[[6]]

salida <- data.frame(date = lasfechas_en_meses, num_leaves = num_leaves, diam = diam, 
                     avpl = avpl, mean_degree = mean_dg, median_degree = median_degree, 
                     strength = st)
colnames(salida) <- c("date", "leaves", "diameter", "avpl", "mean_degree", 'median_degrre', 'strength' )
write.csv(salida,"magner081020b_from_mst_props.csv", row.names = FALSE)





# # #
# funcion
monthly_props <- function(data, lasfechas_en_meses, numcolu) {
  num_leaves <- vector(mode="numeric", length=length(lasfechas_en_meses)) #num de hojas
  diam <- vector(mode="numeric", length=length(lasfechas_en_meses)) #diametro
  avpl <- vector(mode="numeric", length=length(lasfechas_en_meses)) #Average path length
  mean_dg <- vector(mode="numeric", length=length(lasfechas_en_meses)) # mean degree
  median_dg <- vector(mode="numeric", length=length(lasfechas_en_meses)) # median degree
  st <- vector(mode="numeric", length=length(lasfechas_en_meses)) # strength degree
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
    V(mst_g)$degree <- degree(mst_g)
    num_leaves[i] <- sum(V(mst_g)$degree==1)
    diam[i] <-diameter(mst_g, directed = FALSE, unconnected = TRUE)
    avpl[i] <- mean_distance(mst_g, directed = FALSE, unconnected = TRUE)
    mean_dg[i] <- mean(V(mst_g)$degree)
    median_dg[i] <- median(V(mst_g)$degree)
    st[i] <- mean(E(mst_g)$weight)
  }
  return(list(num_leaves= num_leaves, diam = diam, avpl = avpl, mean_degree = mean_dg,
              median_degree = median_dg, mean_strength = st))
}

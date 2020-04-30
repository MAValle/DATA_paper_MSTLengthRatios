# En este script hacemos las graficas de MST
# para el paper para epoca de pre, crisis y post crisis.


# name: mst_figures_v1.R
# creation date: 6, nov, 2019.

# Notes:
# 06-nov-19: creation
# 08-nov-19: hacemos las graficas para lso tres periodos en una misma ventana.


# precrisis: ene y feb  y mar 08
# crisis: sep oct nov  08
# post : mar abr may 09 



rm(list = ls())
library(entropy)
library(purrr)
library(moments)
library(ggplot2)
library(igraph)
source("get_orientation_function.R")



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


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 04-sep-19
# estoy viendo como extraer las fechas para poder calcular matriz de correlacion
# por mes.
#https://stackoverflow.com/questions/17496358/r-help-converting-factor-to-date
data[,"Dates"] <- as.Date(data[,"Dates"], format = "%m-%d-%y")
mandy <- format(data$Date, "%m/%Y")
data$mandy <- mandy
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 06.nov.19
# para las fechas determinadas generamos un plot del MST
# # # # # # # # # # # # # # # # # # # #  PRECRISIS
fecha <- c("01/2008", "02/2008", "03/2008")   # ene feb mar 2008
numcolu <- c(2:28)
df_temporal <- subset(data, mandy %in% fecha)   #%in% c("v1", "v2", "v3")
# calcula de la matriz de correlacion
#rho_matrix <- cor(df_temporal[,c(2:34)],  method="pearson")
rho_matrix <- cor(df_temporal[,numcolu],  method="pearson")
# conversion de correlaciones a distancias
d_matrix <- sqrt(1 - rho_matrix)
# net creation from distances
g_net <- graph_from_adjacency_matrix(d_matrix, mode="upper", weighted=TRUE, diag=FALSE)
mst_g1 <- minimum.spanning.tree(g_net, algorithm="prim")
V(mst_g1)$colors <- c("antiquewhite1", "antiquewhite1", "antiquewhite1",
                     "azure2", "azure2", "azure2", "azure2", "azure2", "azure2",
                     "palegoldenrod", "palegoldenrod", "palegoldenrod", "palegoldenrod", "palegoldenrod", "palegoldenrod", "palegoldenrod", "palegoldenrod", "palegoldenrod",
                     "skyblue", "skyblue", "skyblue", "skyblue", "skyblue", "skyblue", "skyblue", "skyblue", "skyblue")
# antiquewhite1: North america (cafe leche claro)
# azure2: latam (celeste-blanco)
# palegoldenrod: europe  (color crema)
# skyblue: asia + oceania (celeste)


# # # # # # # # # # # # # # # # # # # # CRISIS
fecha <- c("09/2008", "10/2008", "11/2008")   # sep oct nov 2008
numcolu <- c(2:28)
df_temporal <- subset(data, mandy %in% fecha)   #%in% c("v1", "v2", "v3")
# calcula de la matriz de correlacion
#rho_matrix <- cor(df_temporal[,c(2:34)],  method="pearson")
rho_matrix <- cor(df_temporal[,numcolu],  method="pearson")
# conversion de correlaciones a distancias
d_matrix <- sqrt(1 - rho_matrix)
# net creation from distances
g_net <- graph_from_adjacency_matrix(d_matrix, mode="upper", weighted=TRUE, diag=FALSE)
mst_g2 <- minimum.spanning.tree(g_net, algorithm="prim")
V(mst_g2)$colors <- c("antiquewhite1", "antiquewhite1", "antiquewhite1",
                     "azure2", "azure2", "azure2", "azure2", "azure2", "azure2",
                     "palegoldenrod", "palegoldenrod", "palegoldenrod", "palegoldenrod", "palegoldenrod", "palegoldenrod", "palegoldenrod", "palegoldenrod", "palegoldenrod",
                     "skyblue", "skyblue", "skyblue", "skyblue", "skyblue", "skyblue", "skyblue", "skyblue", "skyblue")
# antiquewhite1: North america (cafe leche claro)
# azure2: latam (celeste-blanco)
# palegoldenrod: europe  (colro crema)
# skyblue: asia + oceania (celeste)


# # # # # # # # # # # # # # # # # # # # POST-CRISIS
fecha <- c("03/2009", "04/2009", "05/2009")   # sep oct nov 2008
numcolu <- c(2:28)
df_temporal <- subset(data, mandy %in% fecha)   #%in% c("v1", "v2", "v3")
# calcula de la matriz de correlacion
#rho_matrix <- cor(df_temporal[,c(2:34)],  method="pearson")
rho_matrix <- cor(df_temporal[,numcolu],  method="pearson")
# conversion de correlaciones a distancias
d_matrix <- sqrt(1 - rho_matrix)
# net creation from distances
g_net <- graph_from_adjacency_matrix(d_matrix, mode="upper", weighted=TRUE, diag=FALSE)
mst_g3 <- minimum.spanning.tree(g_net, algorithm="prim")
V(mst_g3)$colors <- c("antiquewhite1", "antiquewhite1", "antiquewhite1",
                     "azure2", "azure2", "azure2", "azure2", "azure2", "azure2",
                     "palegoldenrod", "palegoldenrod", "palegoldenrod", "palegoldenrod", "palegoldenrod", "palegoldenrod", "palegoldenrod", "palegoldenrod", "palegoldenrod",
                     "skyblue", "skyblue", "skyblue", "skyblue", "skyblue", "skyblue", "skyblue", "skyblue", "skyblue")
# antiquewhite1: North america (cafe leche claro)
# azure2: latam (celeste-blanco)
# palegoldenrod: europe  (colro crema)
# skyblue: asia + oceania (celeste)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 06-nov-19
# Ploteo del MST
# https://kateto.net/netscix2016.html
# https://kateto.net/wp-content/uploads/2016/01/NetSciX_2016_Workshop.pdf
# https://mariliagaiarsa.weebly.com/uploads/3/8/6/2/38628397/igraphtutorialeng.html

library(igraph)
par(mfrow=c(1,2))
g1 <- barabasi.game(10)
g2 <- barabasi.game(5)
plot(g1)
plot(g2,add=TRUE)

par(mfrow=c(1,3))

cairo_ps("mst_precrisis_from_mst_figures_v1_eps_310120.eps")
par(mar = c(1.5, 1.5, 1.5, 1.5))
plot(mst_g1, 
     edge.arrow.size =.3, 
     edge.curved = 0,
     vertex.color = V(mst_g1)$colors, 
     vertex.frame.color="#555555",
     vertex.label = V(mst_g1)$name, 
     vertex.label.color = "black",
     vertex.label.cex=.6)
#title("Pre-crisis", cex.main=0.5, col.main="black")
dev.off()

cairo_ps("mst_crisis_from_mst_figures_v1_eps_310120.eps")
par(mar = c(1.5, 1.5, 1.5, 1.5))
plot(mst_g2, 
     edge.arrow.size =.3, 
     edge.curved = 0,
     vertex.color = V(mst_g2)$colors, 
     vertex.frame.color="#555555",
     vertex.label = V(mst_g2)$name, 
     vertex.label.color = "black",
     vertex.label.cex=.6)
#title("Crisis", cex.main=0.5, col.main="black")
dev.off()


cairo_ps("mst_postcrisis_from_mst_figures_v1_eps_310120.eps")
par(mar = c(1.5, 1.5, 1.5, 1.5))
plot(mst_g3, 
     edge.arrow.size =.3, 
     edge.curved = 0,
     vertex.color = V(mst_g3)$colors, 
     vertex.frame.color="#555555",
     vertex.label = V(mst_g3)$name, 
     vertex.label.color = "black",
     vertex.label.cex=.6)
#title("Post-crisis", cex.main=0.5, col.main="black")
dev.off()
#mtext("MST for three different periods", side = 3, outer = TRUE)
#mtext("MST for three different periods", side = 3)
#text(-1.3,2.9,"First title",cex=1,font=1)



# Caption:
#The figure shows the resulting MST for three 
# different periods under the names: Pre-crisis (January, February and March 2008),
# the crisis (September, October and November 2008),
# and Post-crisis (March, April and May 2009).
# The colors of the vertex represents different regions:
# Blue: Asia-Oceania   Light-blue: LatinAmerica    Light-Yellow: Europa   Light-Salmon:NorthAmerica

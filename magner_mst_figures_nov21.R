
# este script utiliza la database del paper de magner para graficar
# los MST en distintos periodos.
# La bbdd de magner que se utiliza es la entregada por magner el 13 de octubre
# 2021 bajo el nombre data_magner_131021.xlsx (formato excel :( !!))

# Magner desea 14 graficas de mst 
# en noviembre 2017 / julio 2015 / marzo 2020 
# en 4 cuartiles que vienen en hojas de excel separadas!!

# Procedimiento:
# Transformar cada hoja de excel en csv
# cargarla en R
# subset de los datos para el mes deseado
# graficar mst.

# codigo fuente: magner_mst_figures.R

# name: magner_mst_figures_nov21.R

# para cada bbdd g1, g2, g3 y g4, se hara un grafico de MST en dos fechas 
# distintas para : una en periodo normal (December 17, 2019 to February 12, 2020) 
# y otra en periodo de crisis pandemia (February 13, 2020 to April 03, 2020)

# Notes
# 004-nov-21: creation, 



# # # # carga de paquetes y data  (son 27 indices)
rm(list = ls())
library(igraph)
library(ggplot2)
library(purrr)
setwd("~/Dropbox/research/Papers con JL-NM/Paper MST length indexs/Manuscript_magner_esg")
#source("monthly_mstlength_function.R")


data <- read.csv("etf_q1_csv.csv") #data daily de ReNTABILIDADES de cuartil q1
#colSums(is.na(data)) # para sumar cuantos datos NA hay en cada columna.

# borrar todas columnas con solo NA
data <- data[, colSums(is.na(data)) < nrow(data)]
str(data)
#data$Date <- as.Date(data$Date, format = "%m/%d/%y")

data$Date <- as.POSIXlt(data$Date, format = "%m/%d/%y")



# seleccionar mes
#  noviembre 2017 / julio 2015 / marzo 2020 
#subset(data, format.Date(Date, "%m")=="11" & format.Date(Date, "%y")=="2017")
subdata_nov17 <- data[format.Date(data$Date, "%m")=="11" & format.Date(data$Date, "%Y")=="2017"   &   !is.na(data$Date),]
subdata_jul15 <- data[format.Date(data$Date, "%m")=="07" & format.Date(data$Date, "%Y")=="2015"   &   !is.na(data$Date),]
subdata_mar20 <- data[format.Date(data$Date, "%m")=="03" & format.Date(data$Date, "%Y")=="2020"   &   !is.na(data$Date),]


# pasar subsets a una lista
my_list <- list()
my_list[[1]] <- subdata_nov17[, c(2:26)]
my_list[[2]] <- subdata_jul15[, c(2:26)]
my_list[[3]] <- subdata_mar20[, c(2:26)]


# mst build:
rho <- map(my_list, cor, use="pairwise.complete.obs")
d_matrix <- map(rho, function(R) sqrt(1 - R))
g_net <- map(d_matrix, graph_from_adjacency_matrix, mode="upper", weighted=TRUE, diag=FALSE)
mst_g <- map(g_net, minimum.spanning.tree, algorithm="prim")
# capturar el largo del mst
# E(mst_g)$weight  aqui estan las distancias
mst_length <- map(mst_g, function(net)  sum(E(net)$weight ) ) # largos


# plots
par(mar = c(0.1, 0.5, 1.5, 0.5)) # bottom, left, top, and right.
par(mfrow=c(1,3))
plot(mst_g[[1]], 
     edge.color = "gray47",
     #edge.width = 10*sqrt(E(mst_g)$weight),
     edge.width = 4,
     edge.arrow.size =.3, 
     edge.curved = 0,
     vertex.color = "white", 
     vertex.size = 12,
     vertex.frame.color="black",
     #vertex.label = V(mst_normal)$name, 
     vertex.label.color = "black",
     vertex.label.cex=.5
)
title("Nov17", adj = 0, line = 0)
plot(mst_g[[2]], 
     edge.color = "gray47",
     #edge.width = 10*sqrt(E(mst_g)$weight),
     edge.width = 4,
     edge.arrow.size =.3, 
     edge.curved = 0,
     vertex.color = "white", 
     vertex.size = 12,
     vertex.frame.color="black",
     #vertex.label = V(mst_normal)$name, 
     vertex.label.color = "black",
     vertex.label.cex=.5
)
title("Jul15", adj = 0, line = 0)
plot(mst_g[[2]], 
     edge.color = "gray47",
     #edge.width = 10*sqrt(E(mst_g)$weight),
     edge.width = 4,
     edge.arrow.size =.3, 
     edge.curved = 0,
     vertex.color = "white", 
     vertex.size = 12,
     vertex.frame.color="black",
     #vertex.label = V(mst_normal)$name, 
     vertex.label.color = "black",
     vertex.label.cex=.5
)
title("Mar20", adj = 0, line = 0)

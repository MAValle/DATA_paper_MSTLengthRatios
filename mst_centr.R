# para el paper de Magner
# En este script, calculamos el strength de los nodos de la red COMPLETA
# calculada a partir de las correlaciones y luego distancias.



# name: mst_centr.R

# Notes
# 22-dic-20: creation, 
# 11-ene-21: MAGNER me envia nuevos datos de rentabilidades de ETF en magner_data_110121.csv. Es necesario cambiar
#             todos los nombres de columnas, y eliminar todas las columnas 


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # carga de paquetes y data  (son 27 indices)
rm(list = ls())
library(igraph)
library(ggplot2)
library(purrr)
source("get_monthly_centrality_function.R")
#data <- read.csv("ipsa_230620_nmagner_csv.csv") #data daily de ReNTABILIDADES
# data <- read.csv("data_paper_magner_jul20.csv") #data daily de ReNTABILIDADES (carga de datos original)

# # # # # # # # 
# 11-ene-20
# Pre-procesamiento de datos de MAGNER enviados el 11 enero 2021
# crar nombre de columnas decentes
nc <- seq(from = 1, to = 112, by = 1)
nc <- paste("V", nc, sep = "")
nc <- c("Date", nc)

data <- read.csv("magner_data_110121.csv")
colnames(data) <- nc
data <- data[-1, ]
colSums(is.na(data)) # para sumar cuantos datos NA hay en cada columna.
# # # # # # # # 


# # # # Tratamiento de las fechas
fechas <- data$Date  # las fechas vienen en formato mes/dia/ano
#fechas <- droplevels(fechas)
# fechas2 <- data.frame(as.Date(fechas, format = "%m/%d/%y")) # para data_paper_magner_jul20.csv (fechas deparada por /)
#fechas2 <- data.frame(as.Date(fechas, format = "%d/%m/%y"))
fechas2 <- data.frame(as.Date(fechas, format = "%d-%m-%Y")) # para magner_data_110121.csv (fechas deparada por -)
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
# data$IGBC <- NULL  # el IGBC tiene muchos datos perdidos y no podemos calcularla correlacion despues.
# all_indices = c(2:27) # para data_paper_magner_jul20.csv
all_indices = c(2:113) # para magner_data_110121.csv 
# # EN CASO que quiera convertir los NA en rentabilidad 0:



columnas <- list("namer" = c(2:4),
                 "latam" = c(5:10),
                 "america" = c(2:10),
                 "europe" = c(11:19), 
                 "asiaoc" = c(20:28),
                 #"commodities" = c(31,32,33,34), 
                 #"bonds" = c(3,4,5),
                 "all_indices" = c(2:113))


# # # # calculo strengths por mes
lasfechas_en_meses <- unique(data$mandy)
vv <- get_monthly_centrality(data = data, lasfechas_en_meses = lasfechas_en_meses, numcolu = all_indices)
write.csv(vv, file = "magner120121_from_mst_centr.csv", row.names = TRUE)

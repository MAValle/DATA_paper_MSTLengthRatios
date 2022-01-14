# # # # # # # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # #
#22-dic-20
# funcion que calcula el strength de cada nodo de la red completa de distancias calculadas
# a partir de la correlacion entre activos. 
# Tomamos como ejemplo, la funcion monthly_mstlength en monthly_mstlength_function.R


# inputs: data: data con las rentabilidades DIARIAS, debe ser procesada antes con fechar en formato date.
#         y debe venor con una columna llamada mandy del estilo "01/2001" "01/2001" "01/2001" "01/2001"...
# inputs: lasfechas_en_meses  vector con las fechas en meses
# inputs: numcolu: vector con el numero de columnas de data que se desean considerar
#     para hacer la red completa. 
# outputs: una matriz con N filas (numero de activos) y m columnas que son los meses.
#           La matriz contiene los strength de cada activo (nodo) en cada mes.
# Nota: las libraris igrpah y purrr deben ejecutarse antes de correr la funcion.


get_monthly_centrality <- function(data, lasfechas_en_meses, numcolu) {
  st_matrix <- matrix(NA, nrow = length(numcolu), ncol = length(lasfechas_en_meses) )
  #mst_largos <- vector(mode="numeric", length=length(lasfechas_en_meses))
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
    #mst_g <- minimum.spanning.tree(g_net, algorithm="prim")
    
    # antes de calcular el strength tenemos que asegurar que no hayan links con peso NA, 
    # y en ese caso, los dejamos en cero.
    E(g_net)$weight <- ifelse(is.na(E(g_net)$weight), 0, E(g_net)$weight)
    
    # calcula el strength de cada nodo
    st <- strength(g_net, mode = "all", loops = TRUE)
    
    # poner la centralidad en colimna i
    st_matrix[, i] <- st
  }
  # enchular la matriz
  st_matrix <- as.data.frame(st_matrix)
  colnames(st_matrix) <- lasfechas_en_meses
  st_matrix$nodes <- colnames(data[numcolu])
  return(st_matrix)
}
# ejemplo:
# vv <- get_monthly_centrality(data = data, lasfechas_en_meses = lasfechas_en_meses, numcolu = all_indices)
# write.csv(vv, file = "magner221220_from_mst_centr.csv", row.names = TRUE)
# # # # # # # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # ## # # # #
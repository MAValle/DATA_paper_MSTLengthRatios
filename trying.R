# Lo que hacemos aqui es leer la base de datos de indices de bonos y acciones 
# y sacamos las correlaciones entre los indices de cada tipo para luego calcular
# el MST asociado, y finalmente encontrar el largo del MST L.
# El largo del MST lo calculamos tomando datos correlativos de 40 dias, es decir
#con ventanas de 40 dias y con 10 dias de datos repetidos entre ventana y ventana.

# 09-mar-19
# lectura de datos de Retornos acciones + bonos_daily_5y.csv

library(igraph)
library(ggplot2)
data <- read.csv("data070319.csv")
data <- data[complete.cases(data), ]
data_acc <- data[, c(2:12)]
data_bon <- data[, c(13:ncol(data))]

# codigo para ir tomando de a 40 datos con traslapes de a 5
desfase <- 5
ini <- -4
fin <- 0
cont <- 1
largos_acc <- vector(mode="numeric", length=0)
largos_bon <- vector(mode="numeric", length=0)
corrs_acc <- vector(mode="numeric", length=0)
corrs_bon <- vector(mode="numeric", length=0)
corrs_all <- vector(mode="numeric", length=0)

while ( fin <= nrow(data) ) {
  cat("\r", "Processing data..Iteration number:", cont)
  ini <- ini + desfase
  #print(ini)
  fin <- ini + 39
  #print(fin)
  df <- data[c(ini:fin), ]
  df_acc <- data_acc[c(ini:fin), ]
  df_bon <- data_bon[c(ini:fin), ]
  
  rho_all <- cor(df[, c(2:ncol(df))], method="pearson")
  mean_corr_all <- mean(rho_all[lower.tri(rho_acc, diag=FALSE)])
  rho_acc <- cor(df_acc[, c(2:ncol(df_acc))], method="pearson")
  d_acc <- sqrt(1-rho_acc)
  mean_corr_acc <- mean(rho_acc[lower.tri(rho_acc, diag=FALSE)])
  rho_bon <- cor(df_bon[, c(2:ncol(df_bon))], method="pearson")
  d_bon <- sqrt(1-rho_bon)
  mean_corr_bon <- mean(rho_bon[lower.tri(rho_bon, diag=FALSE)])
  
  # net creation from distances
  g_acc <- graph_from_adjacency_matrix(d_acc, mode="upper", weighted=TRUE, diag=FALSE)
  mst_g_acc <- minimum.spanning.tree(g_acc, algorithm="prim")
  g_bon <- graph_from_adjacency_matrix(d_bon, mode="upper", weighted=TRUE, diag=FALSE)
  mst_g_bon <- minimum.spanning.tree(g_bon, algorithm="prim")
  # E(mst_g)$weight  aqui estan las distancias
  largos_acc <- c(largos_acc, sum(E(mst_g_acc)$weight))
  largos_bon <- c(largos_bon, sum(E(mst_g_bon)$weight))
  
  corrs_acc <- c(corrs_acc, mean_corr_acc)
  corrs_bon <- c(corrs_bon, mean_corr_bon)
  corrs_all <- c(corrs_all, mean_corr_all)
  
  cont <- cont + 1
}
Largos <- c(largos_acc, largos_bon)
correlations <- c(corrs_acc, corrs_bon)
Block <- rep(c(1:length(largos_acc)), 2)
Type <- factor(c(rep("Shares",length(largos_acc)), rep("Bonds",length(largos_acc))))
#largos <- data.frame(largos_acc=largos_acc, largos_bon=largos_bon)
#largos$block <- c(1:length(largos_acc))
dtf <- data.frame(Length=Largos, Block=Block, Type=Type, Correlations=correlations)
corrs_combined <- data.frame(combined_correlations=corrs_all)

ggplot(dtf, aes(x = Block, y = Length)) + geom_line(aes(color = Type), size = 1) +
  xlab("Block") + ylab("MST Length") + 
  labs(caption = "(window of 40 trading days shifted by 10 each time)") + 
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()

write.csv(data,'data.csv')
write.csv(dtf,'dtf.csv')
write.csv(corrs_combined,'correlaciones_bond_and_shares.csv')
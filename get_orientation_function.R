# Lo que hacemos aqui es calcular el net orientations q_t para cada periodo
# q_t = 1*n + (1)*sum_(N-n) donde N es el numero de neuronas del sistema (o indices)
# n es el numero de neuronas activadas, y N-n es el numero de neuronas en off.
# Otra forma de escribir es simplemente q_t = <q_i> donde q_i es la orientacion negativa
# o positiva del spin.
# q_i = 1 si rentabilidad es positiva, -1 cuando es negativa, es decir, q_i = sgn(R_i)


# Creation date: 25-abr-19
# name: get_orientation_function.R



# Funcion que calcula la orientacion promedio de un estado (conjunto de N spins) de una fila.
# input:
#   data
# output
#   vector de largo = nrow(data) indicando orientacion de cada una de las nrow(data) filas.
get_simple_orientation <- function(data) {
  sgn_data <- sign(data)
  x <- rowMeans(sgn_data)
  return(x)
}
# ejemplo
#so <- get_simple_orientation(data[,c(2:nrow(data))])
#hist(so,20)
#plot(1:nrow(data), so, type="l")



# Funcion que calcula la orientacion neta promedio para un un conjunto de size+1 estados (o filas)
# input:
#   data: en la primera columan va la fecha
# output
#   vector con todas las orientaciones promedio.
get_windows_orientation <- function(data, desfase, size, ini, fin) {
  cont <- 1
  temporal <- vector()
  while ( fin <= nrow(data) ) {
    cat("\r", "Processing data..Iteration number:", cont)
    ini <- ini + desfase
    #print(ini)
    fin <- ini + size
    subdata <- data[c(ini:fin), ]
    x <- get_simple_orientation(subdata)
    temporal <- c(temporal, mean(x))
    cont <- cont + 1
  }
  return(temporal)
}
# ejemplo
#x <- get_windows_orientation(data[,c(2:ncol(data))], desfase = 5, size = 39, ini = -4, fin = 0)
#plot(seq_along(x), temporal, type="l")


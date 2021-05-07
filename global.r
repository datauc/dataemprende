#importar variables y listas necesarias
source("variables.r")

#funciones
puntos <- function(x) {
  y <- format(x, big.mark = ".", decimal.mark = ",")
  return(y)
}

pesos <- function(x) {
  y <- paste0("$", x)
  return(y)
}
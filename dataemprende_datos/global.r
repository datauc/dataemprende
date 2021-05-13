#importar variables y listas necesarias
source("variables.r")

#cargar datos
load("datos_precalculados.rdata")

#funciones
puntos <- function(x) {
  y <- format(x, big.mark = ".", decimal.mark = ",")
  return(y)
}

pesos <- function(x) {
  y <- paste0("$", x)
  return(y)
}


ninguna <- function(x, palabra = "ninguna") {
  #si es null o numeric(0)
  if (isTruthy(x) == FALSE) {
    y = palabra}
  #si es cero
  else if (x == 0) {
    y = palabra}
  #si es caracter vacío
  else if (x == "") {
    y = palabra}
  else {y = x}
  return(y)
}
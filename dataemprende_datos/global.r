library(dplyr)
library(ggplot2)
library(aos)


#importar variables y listas necesarias
source("variables.r")

#cargar datos
load("datos_precalculados.rdata")

#funciones ----
#pone puntos de miles a una cifra
puntos <- function(x) {
  y <- format(x, big.mark = ".", decimal.mark = ",")
  return(y)
}

#antepone signo peso a una cifra
pesos <- function(x) {
  y <- paste0("$", x)
  return(y)
}

#pone la palabra "ninguna" si al cifra es cero o nula
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

#función para hacer un gráfico de 10 personitas en base a dos proporciones
graficar_genero <- function(dato_hombres = 0.5,
                            dato_mujeres = 0.5){
  
  cant_hombres <- dato_hombres %>% round(1)*10
  cant_mujeres <- dato_mujeres %>% round(1)*10
  
  distribucion <- c(rep("Hombre", cant_hombres),
                    rep("Mujer", cant_mujeres)
  )
  
  datos <- tibble(genero = distribucion, id = 1:10)
  
  p <- datos %>%
    mutate(logo = ifelse(genero == "Hombre", "\uF183", "\uF182")) %>%
    ggplot(aes(x = id, y = 1, label = logo, fill = genero, col = genero)) +
    geom_text(size = 8, family = 'FontAwesome', show.legend=F) +
    #geom_point(size = 5, show.legend = F) +
    theme_void()
  
  return(p)
}

#grafica empresas por comuna
graficar_empresas <- function(input_comuna = "Iquique") {
  
  cifras <- datos$tramos_comuna %>%
    filter(comuna == input_comuna,
           #filter(comuna == "Pica",
           tramo != "Grande") %>%
    mutate(cant = round(porcentaje, 1) * 10,
           cant = ifelse(porcentaje > 0.01 & porcentaje <= 0.1, 1, cant)) #poner una aunque sean menos del 10%
  
  if (any(cifras$tramo == "Mediana") == FALSE) { 
    cifras <- cifras %>%
      bind_rows(tibble(tramo = "Mediana", cant = 0))
  }
  
  distribucion <- c(rep("Micro", cifras$cant[cifras$tramo == "Micro"]),
                    rep("Pequeña", cifras$cant[cifras$tramo == "Pequeña"]),
                    rep("Mediana", cifras$cant[cifras$tramo == "Mediana"])) %>%
    tibble(tramo = .) %>%
    mutate(tramo = as.factor(tramo),
           tramo = forcats::fct_expand(tramo, "Mediana"),
           tramo = forcats::fct_relevel(tramo, 
                                        "Micro",
                                        "Pequeña",
                                        "Mediana")) 
  
  distribucion2 <- distribucion %>%
    group_by(tramo) %>%
    mutate(orden = 1:n()) %>%
    mutate(logo = case_when(tramo == "Micro" ~ "\uF54F", #tienda casa
                            tramo == "Pequeña" ~ "\uF54E", #tienda con techo
                            tramo == "Mediana" ~ "\uF1AD")) #edificio
  
  p <- distribucion2 %>%
    ggplot(aes(x = orden, y = tramo, label = logo, fill = tramo, col = tramo)) +
    geom_text(size = 7, family = 'FontAwesome', show.legend=F) +
    #geom_point(size = 5, show.legend = F) +
    theme_void() +
    scale_y_discrete(drop=F) +
    scale_x_continuous(limits = c(1, 9)) +
    coord_cartesian(clip = "off") +
    theme(axis.text.y = element_text(hjust = 1, margin = margin(r = 10)))
  
  return(p)
}

# graficar_empresas("Iquique")
# graficar_empresas("Alto Hospicio")
# graficar_empresas("Colchane")
# graficar_empresas("Pica")
# graficar_empresas("Huara")
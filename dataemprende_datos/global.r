library(dplyr)
library(ggplot2)
library(aos)
#library(grid)

#importar variables y listas necesarias
source("variables.r")

#cargar datos
load("datos_precalculados.rdata")
load("puntos_empresas.rdata")
load("datos_mapas.rdata")

color_fondo <- "#457B9D"
color_claro <- "#A8DADC"
color_blanco <- "#F1FAEE"
color_oscuro <- "#2D668E"
color_negro <- "#1D3557"

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

#pone los porcentajes en el formato correcto
porcentaje <- function(x, z = 0.1) {
 y <- scales::percent(x, decimal.mark = ",", big.mark =".", accuracy = z)
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
    theme_void() +
    theme(plot.background = element_rect(fill = color_fondo, color = color_fondo))
  
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
    theme(axis.text.y = element_text(hjust = 1, margin = margin(r = 10))) +
    theme(plot.background = element_rect(fill = color_fondo, color = color_fondo))
  
  return(p)
}

# graficar_empresas("Iquique")
# graficar_empresas("Alto Hospicio")
# graficar_empresas("Colchane")
# graficar_empresas("Pica")
# graficar_empresas("Huara")


#agrega el estilo css al texto
cifra <- function(x) {
  y <- paste0("<span class='cifra'>", x , "</span>")
  return(y)
}


espaciador <- function() {
 
  y <- list(br(),br(),
  br(),br(),
  br(),br(),
  p(" "))
  return(y)
}


#https://stackoverflow.com/questions/30136725/plot-background-colour-in-gradient
gg.background.fill <- function(gg.plot, cols = "white", which = "x") {
  #does not work with facets
  
  stopifnot(which %in% c("x", "y"))
  which1 <- if (which == "x") "width" else "height"
  
  require(gridExtra)
  
  g <- ggplotGrob(gg.plot)
  #g <- ggplotGrob(p)
  gg <- g$grobs      
  findIt <- vapply(gg, function(x) grepl("GRID.gTree", x$name, fixed = TRUE), TRUE)
  n1 <- getGrob(gg[findIt][[1]], "grill.gTree", grep=TRUE)$name
  n2 <- getGrob(gg[findIt][[1]], "panel.background.rect", grep=TRUE)$name
  gg[findIt][[1]]$children[[n1]]$children[[n2]]$gp$fill <- cols
  x <- gg[findIt][[1]]$children[[n1]]$children[[n2]][[which]]
  w <- gg[findIt][[1]]$children[[n1]]$children[[n2]][[which1]]
  attr <- attributes(x)
  x <- seq(0 + c(w)/length(cols)/2, 1 - c(w)/length(cols)/2, length.out = length(cols))
  attributes(x) <- attr
  gg[findIt][[1]]$children[[n1]]$children[[n2]][[which]] <- x
  w <- c(w)/length(cols) 
  attributes(w) <- attr
  gg[findIt][[1]]$children[[n1]]$children[[n2]][[which1]] <- w
  g$grobs <- gg
  class(g) = c("arrange", "ggplot", class(g)) 
  g
}




graficar_lineas_degradado <- function(data, texto_y = "Cantidad de empresas"){
  #fondo degradado https://r.789695.n4.nabble.com/plot-background-excel-gradient-style-background-td4632138.html#a4634954
  colores_degradado <- colorRampPalette(c(color_claro, color_fondo))
  fondo_degradado <- grid::rasterGrob(colores_degradado(5), width=unit(1,"npc"), height = unit(1,"npc"), interpolate = T) 
  
  #graficar
  p <- data %>%
    ggplot(aes(año, empresas, col=rubro)) +
    #fondo degradado
    annotation_custom(fondo_degradado, xmin=2005, xmax=2019, ymin=0, ymax=Inf) +
    geom_segment(col = color_fondo, size=1, aes(x=año, y=Inf, yend=empresas, xend=año), show.legend = F) + #parche de líneas hacia arriba para tapar el fondo de degradado que se le escapa al geom_ribbon
    #líneas de fondo
    geom_segment(col = color_claro, alpha = 0.3, aes(x=año, y=0, yend=empresas, xend=año), show.legend = F) +
    #colores de fondo arriba/abajo
    geom_ribbon(fill = color_fondo, col=color_fondo, alpha = 1, aes(ymin = empresas, ymax = Inf), show.legend = F) + #fondo oscuro (arriba) para tapar el degradado
    geom_area(fill = color_fondo, alpha = 0.2, show.legend = F) + #fondo claro (abajo)
    #línea
    geom_line(color = color_claro, size = 1.2, show.legend = F, ) +
    #punto
    #geom_point(col=color_negro, alpha=0.6, size=3) + #punto chico negro
    geom_point(color = color_blanco, size=1.5) + #punto chico claro
    geom_point(color = color_blanco, size=6, data = . %>% filter(año == 2019), aes(x = max(año), y=max(empresas))) + #punto grande claro
    geom_point(color = color_claro, size=4, data = . %>% filter(año == 2019), aes(x = max(año), y=max(empresas))) + #punto grande blanco
    #líneas del gráfico
    #geom_segment(inherit.aes = F, color = color_negro, y=0, aes(x=min(año), xend=min(año), yend=max(empresas))) +
    #geom_segment(inherit.aes = F, color = color_negro, y=0, aes(x=min(año), xend=max(año)+0.2, yend=0)) +
    scale_x_continuous(breaks = años_sii, expand = expansion(add=c(0, 2))) +
    #scale_y_continuous(expand = expansion(mult=c(0, 0.15))) +
    #texto
    geom_text(color = color_blanco, aes(label = paste0(" ", max(empresas)), x = max(año)+0.5, y=max(empresas)),
              hjust=0, inherit.aes = F, check_overlap = T, data = . %>% filter(año == 2019)) +
    #tema
    coord_cartesian(clip="off") +
    labs(y = texto_y) +
    theme(axis.text.x = element_text(angle=90, vjust=0.5)) +
    theme(plot.background = element_rect(fill = color_fondo, color = color_fondo),
          panel.background = element_rect(fill = color_fondo, color = color_fondo),
          text = element_text(color = color_oscuro),
          axis.text = element_text(color = color_negro),
          axis.ticks = element_blank(), panel.grid = element_blank(), axis.title.x = element_blank(),
          axis.text.x = element_text(margin=margin(t = -8)),
          axis.text.y = element_text(margin=margin(r = 4)))
  
  return(p)
}

# datos_sii$empresas %>%
#   filter(comuna == comunas_sii[5]) %>% #picker
#   filter(rubro == rubros_sii[3]) %>% #picker
#   graficar_lineas_degradado()


#graficar mapa rubros 
graficar_mapa_rubros <- function(datos_filtrados) {
  p <- ggplot() +
    #mapa de base
    geom_sf(data = datos_mapas$región, aes(geometry = geometry),
            fill = color_oscuro, col = color_oscuro) +
    #agua y tierra
    #geom_sf(data = datos_mapas$tierra, colour = "transparent", fill="grey90") + #tierra
    #geom_sf(data = datos_mapas$mar, colour = "transparent", fill = "lightblue1") + #mar
    #calles
    geom_sf(data = datos_mapas$calles_medianas$osm_lines,
            color = color_claro, size = .3, alpha = .2, inherit.aes = F) +
    geom_sf(data = datos_mapas$calles_chicas$osm_lines,
            color = color_negro, size = .2, alpha = .3, inherit.aes = F) +
    geom_sf(data = datos_mapas$calles_grandes$osm_lines,
            color = color_negro, size = .5, alpha = .8, inherit.aes = F) +
    #puntos
    geom_point(data = datos_filtrados, aes(x=x, y=y),
              alpha = 0.3, size = 1, col = color_claro, show.legend = F) +
    #zoom en iquique y alto hospicio
    coord_sf(xlim = c(-70.17, -70.06),
             ylim = c(-20.31, -20.195),
             expand = FALSE) +
    theme_void() +
    theme(plot.background = element_rect(fill = color_fondo, color = color_fondo),
          panel.background = element_rect(fill = color_fondo, color = color_fondo))
  
  #arreglar fondo con barras blancas por haber usado coord_sf: https://gis.stackexchange.com/questions/269224/ggplot2-map-with-colored-background-and-coord-map
  #p <- cowplot::ggdraw(p) + 
  #  theme(panel.background = element_rect(fill = color_fondo, color = color_fondo))
  return(p)
}

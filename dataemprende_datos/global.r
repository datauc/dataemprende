suppressPackageStartupMessages(library(dplyr))
library(ggplot2)
library(aos)
#library(grid)

cat("cargando datos...", fill=T)
#importar variables y listas necesarias
source("variables.r")

#cargar datos
load("datos_precalculados.rdata")
load("puntos_empresas.rdata")
load("datos_mapas.rdata")
load("datos_mapa_regional.rdata")

cat("datos cargados", fill=T)

color_fondo <- "#457B9D"
color_claro <- "#A8DADC"
color_blanco <- "#F1FAEE"
color_oscuro <- "#2D668E"
color_negro <- "#1D3557"

#funciones ----
cat("cargando funciones...", fill=T)
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
  
  cant_hombres_g <- dato_hombres %>% round(1)*10
  cant_mujeres_g <- dato_mujeres %>% round(1)*10
  
  distribucion_g <- c(rep("Hombres", cant_hombres_g),
                    rep("Mujeres", cant_mujeres_g)
  )
  
  datos_g <- tibble(genero = distribucion_g, id = 1:10) %>%
    #posicion del texto
    group_by(genero) %>%
    mutate(id = as.numeric(id)) %>%
    mutate(posicion = mean(id)) %>%
    #porcentajes
    mutate(porcentaje = case_when(genero == "Hombres" ~ dato_hombres,
                                  genero == "Mujeres" ~ dato_mujeres))
  
  p <- datos_g %>%
    mutate(logo = ifelse(genero == "Hombres", "\uF183", "\uF182")) %>%
    ggplot(aes(x = id, y = 1, label = logo, fill = genero, col = genero, alpha = genero)) +
    geom_text(size = 8, family = 'FontAwesome', col = color_claro, show.legend=F) +
    geom_text(family = "Montserrat", y = 1.02*1.01, aes(x = posicion, label = genero), 
              col = color_negro, alpha = 1, show.legend = F, check_overlap = T) +
    geom_text(family = "Dosis ExtraLight SemiBold", y = 0.975*0.99, aes(x = posicion, label = scales::percent(porcentaje, 0.1)),
              col = color_negro, alpha = 1, size = 5, show.legend = F, check_overlap = T) +
    scale_alpha_manual(values = c(0.5, 1)) +
    theme_void() +
    coord_cartesian(clip = "off") +
    theme(plot.background = element_rect(fill = color_fondo, color = color_fondo)) +
    theme(plot.margin = margin(7, 14, 7, 14))
  
  #p
  return(p)
}

#graficar_genero()




#grafica empresas por comuna
graficar_empresas <- function(input_comuna = "Iquique") {
  
  cifras <- datos$tramos_comuna %>%
    filter(comuna == input_comuna,
           #comuna == "Pica",
           tramo != "Grande") %>%
    #condición para agregar casitas para números chicos pero no tan chicos
    mutate(cant = round(porcentaje, 1) * 10,
           cant = ifelse(porcentaje > 0.01 & porcentaje <= 0.1, 1, cant)) #poner una aunque sean menos del 10%
  
  #condición para agregar tramo mediano si no está en los datos
  if (any(cifras$tramo == "Mediana") == FALSE) { 
    cifras <- cifras %>%
      bind_rows(tibble(tramo = "Mediana", cant = 0))
  }
  
  #crear dataframe repitiendo hacia abajo una casa por cada 10%
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
  
  #agregar logotipos
  distribucion2 <- distribucion %>%
    group_by(tramo) %>%
    mutate(orden = 1:n()) %>%
    mutate(logo = case_when(tramo == "Micro" ~ "\uF54F", #tienda casa
                            tramo == "Pequeña" ~ "\uF54E", #tienda con techo
                            tramo == "Mediana" ~ "\uF1AD")) #edificio
  
  #agregar porcentajes
  distribucion3 <- distribucion2 %>%
    mutate(porcentaje = case_when(tramo == "Micro" ~ cifras$porcentaje[cifras$tramo=="Micro"],
                                  tramo == "Pequeña" ~ cifras$porcentaje[cifras$tramo=="Pequeña"],
                                  tramo == "Mediana" ~ cifras$porcentaje[cifras$tramo=="Mediana"],
                                  TRUE ~ 0)) %>%
    #agregar orden para el texto
    group_by(tramo) %>%
    mutate(posicion = max(orden)+0.6)

  p <- distribucion3 %>%
    ggplot(aes(x = orden, y = tramo, label = logo, fill = tramo, col = tramo)) +
    geom_text(size = 7, family = 'FontAwesome', col = color_claro, show.legend=F) +
    geom_text(aes(x = posicion, label=scales::percent(porcentaje, accuracy = 0.1)), 
              col=color_negro, hjust=0, family = "Dosis ExtraLight SemiBold", size = 5, check_overlap = T, show.legend=F) +
    theme_void() +
    scale_y_discrete(drop=F) +
    scale_x_continuous(limits = c(1, 10.5)) +
    coord_cartesian(clip = "off") +
    theme(axis.text.y = element_text(hjust = 1, 
                                     color = color_blanco,
                                     family = "Montserrat",
                                     margin = margin(r = 10))) +
    theme(plot.background = element_rect(fill = color_fondo, color = color_fondo))
  
  #p
  return(p)
}

# graficar_empresas("Iquique")
# graficar_empresas("Alto Hospicio")
# graficar_empresas("Pozo Almonte")
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
  br(),br(),
  br(),br(),
  p(" "))
  return(y)
}

espaciador_interior <- function() {
  
  y <- list(br(),br(),
            br(),br(),
            br(),
            p(" "))
  return(y)
}


# #https://stackoverflow.com/questions/30136725/plot-background-colour-in-gradient
# gg.background.fill <- function(gg.plot, cols = "white", which = "x") {
#   #does not work with facets
#   
#   stopifnot(which %in% c("x", "y"))
#   which1 <- if (which == "x") "width" else "height"
#   
#   require(gridExtra)
#   
#   g <- ggplotGrob(gg.plot)
#   #g <- ggplotGrob(p)
#   gg <- g$grobs      
#   findIt <- vapply(gg, function(x) grepl("GRID.gTree", x$name, fixed = TRUE), TRUE)
#   n1 <- getGrob(gg[findIt][[1]], "grill.gTree", grep=TRUE)$name
#   n2 <- getGrob(gg[findIt][[1]], "panel.background.rect", grep=TRUE)$name
#   gg[findIt][[1]]$children[[n1]]$children[[n2]]$gp$fill <- cols
#   x <- gg[findIt][[1]]$children[[n1]]$children[[n2]][[which]]
#   w <- gg[findIt][[1]]$children[[n1]]$children[[n2]][[which1]]
#   attr <- attributes(x)
#   x <- seq(0 + c(w)/length(cols)/2, 1 - c(w)/length(cols)/2, length.out = length(cols))
#   attributes(x) <- attr
#   gg[findIt][[1]]$children[[n1]]$children[[n2]][[which]] <- x
#   w <- c(w)/length(cols) 
#   attributes(w) <- attr
#   gg[findIt][[1]]$children[[n1]]$children[[n2]][[which1]] <- w
#   g$grobs <- gg
#   class(g) = c("arrange", "ggplot", class(g)) 
#   g
# }




graficar_lineas_degradado <- function(data, variable = "rubro", #no hace nada creo
                                      variable_y_elegida="empresas", #columna con valores y
                                      texto_y = "Cantidad de empresas",
                                      numero_largo = 1 #multiplicador del espacio para los numeros
                                      ){
  #fondo degradado https://r.789695.n4.nabble.com/plot-background-excel-gradient-style-background-td4632138.html#a4634954
  colores_degradado <- colorRampPalette(c(color_claro, color_fondo))
  fondo_degradado <- grid::rasterGrob(colores_degradado(5), width=unit(1,"npc"), height = unit(1,"npc"), interpolate = T) 
  
  #calcular año mínimo y máximo
  año_minimo <- min(data$año, na.rm = T)
  año_maximo <- max(data$año, na.rm = T)
  
  #graficar
  p <- data %>%
    rename(variable_y = all_of(variable_y_elegida)) %>%
    ggplot(aes(año, variable_y, col=variable)) +
    #fondo degradado
    annotation_custom(fondo_degradado, xmin=año_minimo, xmax=año_maximo, ymin=0, ymax=Inf) +
    geom_segment(col = color_fondo, size=1, aes(x=año, y=Inf, yend=variable_y, xend=año), show.legend = F) + #parche de líneas hacia arriba para tapar el fondo de degradado que se le escapa al geom_ribbon
    #líneas de fondo
    geom_segment(col = color_claro, alpha = 0.3, aes(x=año, y=0, yend=variable_y, xend=año), show.legend = F) +
    #colores de fondo arriba/abajo
    geom_ribbon(fill = color_fondo, col=color_fondo, alpha = 1, aes(ymin = variable_y, ymax = Inf), show.legend = F) + #fondo oscuro (arriba) para tapar el degradado
    geom_area(fill = color_fondo, alpha = 0.2, show.legend = F) + #fondo claro (abajo)
    #línea
    geom_line(color = color_claro, size = 1.2, show.legend = F, ) +
    #punto
    #geom_point(col=color_negro, alpha=0.6, size=3) + #punto chico negro
    geom_point(color = color_blanco, size=1.5) + #punto chico claro
    geom_point(color = color_blanco, size=6, data = . %>% filter(año == 2019), aes(x = max(año), y=max(variable_y))) + #punto grande claro
    geom_point(color = color_claro, size=4, data = . %>% filter(año == 2019), aes(x = max(año), y=max(variable_y))) + #punto grande blanco
    #líneas del gráfico
    #geom_segment(inherit.aes = F, color = color_negro, y=0, aes(x=min(año), xend=min(año), yend=max(empresas))) +
    #geom_segment(inherit.aes = F, color = color_negro, y=0, aes(x=min(año), xend=max(año)+0.2, yend=0)) +
    scale_x_continuous(breaks = años_sii, expand = expansion(add=c(0, 2*numero_largo))) +
    #scale_y_continuous(expand = expansion(mult=c(0, 0.15))) +
    #texto
    geom_text(color = color_blanco, family = "Dosis ExtraLight SemiBold", size = 5, aes(label = paste0(" ", max(variable_y)), x = max(año)+0.5, y=max(variable_y)),
              hjust=0, inherit.aes = F, check_overlap = T, data = . %>% filter(año == 2019)) +
    #tema
    coord_cartesian(clip="off") +
    labs(y = texto_y) +
    theme(axis.text.x = element_text(angle=90, vjust=0.5)) +
    theme(plot.background = element_rect(fill = color_fondo, color = color_fondo),
          panel.background = element_rect(fill = color_fondo, color = color_fondo),
          text = element_text(color = color_oscuro, family = "Montserrat"),
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

# datos$empresas_año_rubro_region %>%
#   filter(rubro ==rubros_sii[2]) %>%
#   graficar_lineas_degradado()


#graficar mapa rubros 
graficar_mapa_rubros <- function(datos_filtrados, 
                                 mover_x = 0,
                                 mover_y = 0,
                                 zoom = 0) {

  p <- ggplot() +
    #mapa de base
    geom_sf(data = datos_mapas$región, aes(geometry = geometry),
            fill = color_oscuro, col = color_oscuro) +
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
    # #zoom en iquique y alto hospicio
    # coord_sf(xlim = c(-70.17, -70.06),
    #          ylim = c(-20.31, -20.195),
    #          expand = FALSE) +
    #zoom variable
    coord_sf(xlim = c((-70.17+mover_x)+zoom, (-70.06+mover_x)-zoom),
             ylim = c((-20.31+mover_y)+zoom, (-20.195+mover_y)-zoom),
             expand = FALSE) +
    theme_void() +
    theme(plot.background = element_rect(fill = color_fondo, color = color_fondo),
          panel.background = element_rect(fill = color_fondo, color = color_fondo))
  
  #arreglar fondo con barras blancas por haber usado coord_sf: https://gis.stackexchange.com/questions/269224/ggplot2-map-with-colored-background-and-coord-map
  p <- cowplot::ggdraw(p) +
   theme(panel.background = element_rect(fill = color_fondo, color = color_fondo))
  return(p)
}



graficar_barras_horizontales <- function(data, variable_categorica="subrubro", variable_numerica="empresas", slice=8, str_trunc=80, str_wrap=40) {
  variable_categorica <- sym(variable_categorica)
  
  p <- data %>%
    rename(valor = all_of(variable_numerica)) %>%
    arrange(desc(valor)) %>%
    slice(1:slice) %>%
    mutate(!!variable_categorica := stringr::str_trunc(as.character(!!variable_categorica), str_trunc),
           !!variable_categorica := stringr::str_wrap(as.character(!!variable_categorica), str_wrap),
           !!variable_categorica := as.factor(!!variable_categorica),
           !!variable_categorica := forcats::fct_reorder(!!variable_categorica, valor),
           id = 1:n()) %>%
    ggplot(aes(x=valor, y = !!variable_categorica)) +
    geom_col(fill = color_claro, width = 0.3, aes(alpha = id), show.legend = F) +
    geom_text(aes(label = paste0(" ", valor)), size = 5, family = "Dosis ExtraLight SemiBold", col = color_blanco, 
              hjust = 0) +
    #scale_fill_gradient(high = color_claro, low= color_blanco) +
    scale_alpha_continuous(range = c(1, 0.2)) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.4))) +
    theme_void() +
    theme(axis.text.y = element_text(hjust = 1, color = color_negro, family = "Montserrat", size = 9, margin = margin(r = 10))) +
    theme(plot.background = element_rect(fill = color_fondo, color = color_fondo))
  
  return(p)
}


graficar_mapa_comunas <- function(data, variable){
  
  #agregar datos a puntos
  lugares_tarapaca_datos <- datos_mapa_regional$lugares %>%
    left_join(data, 
              by = c("name" = "comuna")) %>%
    rename(empresas = all_of(variable))
  
  m <- ggplot() +
   #mapa de base
   # geom_point(aes(x=-70.17, y=-18.95), col = "red", size=14) +
   # geom_point(aes(x=-70.24, y=-19.055), col = "orange", size=10) +
   # geom_point(aes(x=-68.45, y=-19.05), col = "red", size=15) +
   # geom_point(aes(x=-68.57, y=-19.23), col = "green", size=4) +
   # geom_point(aes(x=-70.1, y=-21.5), col = "blue", size=10) +
   # geom_point(aes(x=-68.47, y=-20.77), col = "pink", size=5) +
   #mapa regional
   geom_sf(data = datos_mapa_regional$región, aes(geometry = geometry),
           fill = color_oscuro, col = color_fondo) +
   #carreteras
   geom_sf(data = datos_mapa_regional$carreteras %>% 
             filter(name != "Avenida La Tirana" &
                      name != "Avenida Arturo Prat Chacón" &
                      name != "Segundo Acceso" &
                      name != "Las Cabras" &
                      name != "Ruta 16" &
                      name != "Avenida Circunvalación"),
           color = color_negro, size = .3, alpha = 0.4) +
   # #calles medianas
   # geom_sf(data = datos_mapa_regional$calles$osm_lines,
   #         inherit.aes = FALSE,
   #         color = color_negro, size = .2, alpha = 0.2) +
   #puntos
   geom_point(data = lugares_tarapaca_datos,
              aes(geometry = geometry, size = empresas), 
              stat = "sf_coordinates", col = color_claro, alpha = 0.6,
              show.legend = F) +
   #texto
   ggrepel::geom_text_repel(data = lugares_tarapaca_datos, aes(geometry = geometry, label = name), 
                            stat = "sf_coordinates", seed = 1993, point.padding = 0.2, min.segment.length = 9,
                            size = 3, col = "black", family = "Montserrat", alpha = 0.7) +
   #zoom region
   coord_sf(xlim = c(-70.4, -68.35),
            ylim = c(-21.7, -18.9), expand = FALSE, clip = "off") +
   # #zoom iquique y alto hospicio
   # coord_sf(xlim = c(-70.17, -70.06),
   #          ylim = c(-20.31, -20.195), expand = FALSE) +
   scale_size_continuous(range = c(0, 15)) +
   theme_void() +
   theme(plot.background = element_rect(fill = color_fondo, color = color_fondo), panel.background = element_rect(fill = color_fondo, color = color_fondo))
 
 #arreglar fondo con barras blancas por haber usado coord_sf: https://gis.stackexchange.com/questions/269224/ggplot2-map-with-colored-background-and-coord-map
 m <- cowplot::ggdraw(m) +
   theme(panel.background = element_rect(fill = color_fondo, color = color_fondo))
 
 return(m)
}

graficar_circular <- function(data, variable_categorica, variable_numerica) {
  grosor=0.7
  transparencia=0.5
  
  p <- data %>%
    rename(variable_categorica = all_of(variable_categorica),
           variable_numerica = all_of(variable_numerica)) %>%
    ggplot(aes(fill = variable_categorica, col=variable_categorica, alpha=variable_categorica)) +
    ggforce::geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = grosor, r = 1, 
                              amount = variable_numerica), 
                          stat = "pie", col = color_fondo, size=2, fill= color_claro) +
    theme_void() +
    scale_alpha_manual(values = c(transparencia, 1)) +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    theme(plot.background = element_rect(fill = color_fondo, color = color_fondo),
          text = element_text(family = "Montserrat", color=color_blanco),
          legend.text = element_text(margin = margin(r=20))) +
    guides(alpha = guide_legend(reverse = TRUE)) +
    coord_fixed()
  return(p)
}
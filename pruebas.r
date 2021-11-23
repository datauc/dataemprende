source("~/Otros/fa_svg_icons.r")
library(dplyr)
library(ggplot2)
library(ggimage)

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
    # mutate(logo = case_when(tramo == "Micro" ~ "\uF015", #"\uF54F", #tienda casa #store-alt
    #                         tramo == "Pequeña" ~ "\uF54E", #tienda con techo #store
    #                         tramo == "Mediana" ~ "\uF1AD")) #edificio #building
    mutate(logo = case_when(tramo == "Micro" ~ fa_icon("store-alt"), #"\uF54F", #tienda casa #store-alt
                            tramo == "Pequeña" ~ fa_icon("store"), #tienda con techo #store
                            tramo == "Mediana" ~ fa_icon("building"))) #edificio #building
  
  #agregar porcentajes
  distribucion3 <- distribucion2 %>%
    mutate(porcentaje = case_when(tramo == "Micro" ~ cifras$porcentaje[cifras$tramo=="Micro"],
                                  tramo == "Pequeña" ~ cifras$porcentaje[cifras$tramo=="Pequeña"],
                                  tramo == "Mediana" ~ cifras$porcentaje[cifras$tramo=="Mediana"],
                                  TRUE ~ 0)) %>%
    #agregar orden para el texto
    group_by(tramo) %>%
    mutate(posicion = max(orden)+0.6)
  
  devtools::install_github("GuangchuangYu/ggimage")
  
  #p <- 
    distribucion3 %>%
    ggplot(aes(x = orden, y = tramo, label = logo, fill = tramo, col = tramo)) +
    geom_image(aes(image = logo),
               size = 0.08,
               asp = 1.2,
               by="width",
              col = color_claro,
              ) +
      #scale_size_identity() +
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
  
  p
  return(p)
}



#—----


d <- puntos_empresas %>%
  filter(rubro == rubros_sii[10]) #%>% 
  #filter(rubro == input$rubro)
#graficar_mapa_rubros()


datos_filtrados <- d
mover_x = 1
mover_y = 1
zoom = 1

mover_x_elegido = -0.02; mover_y_elegido = 0; zoom_elegido = 0.039

datos_filtrados %>% 
  ggplot() +
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
             expand = T) +
    theme_void() +
    theme(plot.background = element_rect(fill = color_fondo, color = color_fondo),
          panel.background = element_rect(fill = color_fondo, color = color_fondo))
  
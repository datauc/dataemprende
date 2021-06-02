#en base a los datos scrappeados por Diego y la transformación de coordenadas de Claudio
#genera un mapa con puntos por cada empresa
library(tidyverse)

#importar ----
#importar datos de Diego
#load("datos diego/Empresas_Taparaca.Rdata")

base_puntos_empresas_tarapaca <- readxl::read_xlsx("mapa claudio/Empresas_Tarapaca.xlsx")

source("subrubros y ciiu.r")

#preparar base ----

puntos_empresas <- base_puntos_empresas_tarapaca %>%
  mutate(across(c(X, Y), ~ as.numeric(.x))) %>%
  sgs::sgs_points(coords = c("X", "Y"), epsg=3857) %>%
  sgs::sgs_en_wgs84() %>%
  as_tibble() %>%
  rename_all(~ tolower(.)) %>%
  mutate(comuna = stringr::str_to_sentence(nom_comuna)) %>%
  filter(!is.na(x),
         !is.na(y)) %>%
  #agregar nombres de rubros correctos
  rename(letra = seccion_ciiu4cl) %>%
  left_join(rubros_sii_letra) %>%
  #anexar subrubros del sii
  mutate(division_ciiu4cl = as.numeric(division_ciiu4cl)) %>%
  left_join(cruce_ciiuu_sii) %>%
  select(rubro, glosa_division, subrubro, x, y)

save(puntos_empresas, file = "dataemprende_datos/puntos_empresas.rdata")

#importar mapa y agregarle datos
mapa_tarapaca <- chilemapas::mapa_comunas %>%
  left_join(
    chilemapas::codigos_territoriales %>%
      select(matches("comuna"))
  ) %>%
  filter(codigo_region=="01")

# puntos_empresas_2 <- sf::st_as_sf(puntos_empresas, coords = c("x", "y"), 
#                crs = 4326, agr = "constant")


puntos_empresas %>% glimpse()

ggplot() +
  geom_sf(data = mapa_tarapaca, aes(geometry = geometry)) +
  geom_point(data = puntos_empresas %>% filter(seccion_ciiu4cl == "D"), 
                                               aes(x=x, y=y, col = glosa_seccion), 
             alpha = 0.1, show.legend = F) +
# #zoom en iquique y alto hospicio
# coord_sf(xlim = c(-70.18, -70.07),
#          ylim = c(-20.31, -20.19),
#          expand = FALSE)
#zoom en tarapacá
    coord_sf(xlim = c(-70.5, -68.25),
             ylim = c(-21.7, -18.8),
             expand = FALSE)
  


ggplot() +
  geom_sf(data = mapa_tarapaca, aes(geometry = geometry)) +
  geom_point(data = puntos_empresas %>% filter(seccion_ciiu4cl == "D"), 
             aes(x=x, y=y, col = glosa_seccion), 
             alpha = 0.1, show.legend = F) +
  # #zoom en iquique y alto hospicio
  # coord_sf(xlim = c(-70.18, -70.07),
  #          ylim = c(-20.31, -20.19),
  #          expand = FALSE)
  #zoom en tarapacá
  coord_sf(xlim = c(-70.5, -68.25),
           ylim = c(-21.7, -18.8),
           expand = FALSE)


#—----
#stamen ----
#funciona mal, descarga un puro tile

# mapa_tarapaca %>%
#   mutate(
#     CENTROID = map(geometry, st_centroid),
#     COORDS = map(CENTROID, st_coordinates),
#     long = map_dbl(COORDS, 1),
#     lat = map_dbl(COORDS, 2)
#   ) %>%
# ggplot(aes(geometry = geometry, long, lat)) +
#   geom_sf() +
#   # geom_point(data = puntos_empresas %>% filter(seccion_ciiu4cl == "D"), 
#   #            aes(x=x, y=y, col = glosa_seccion), 
#   #            alpha = 0.1, show.legend = F) +
#     # coord_sf(xlim = c(-70.5, -68.25),
#     #          ylim = c(-21.7, -18.8),
#     #          expand = FALSE)
#   stat_maptiles(zoom = 7,
#                 url = "http://tile.stamen.com/terrain/%d/%d/%d.png",
#                 #url = "http://tile.stamen.com/watercolor/%d/%d/%d.jpg",
#                 force = T) +
#   coord_sf(clip = "off", expand = F,
#            xlim = c(-70.5, -68.25),
#            ylim = c(-21.7, -16.8)) +
#   mapview()

#—----

#rmapzen ----
#agregar calles, mar, y zonas protegidas al gráfico

##https://www.dshkol.com/2018/better-maps-with-vector-tiles/
##install.packages("rmapzen")
# library(rmapzen)
# options(nextzen_API_key="wkUP5UE4TDu92Vg14jut9A")
# mz_set_tile_host_nextzen(key = getOption("nextzen_API_key"))
# 
# get_vector_tiles <- function(bbox){
#   mz_box=mz_rect(bbox$xmin,bbox$ymin,bbox$xmax,bbox$ymax)
#   mz_vector_tiles(mz_box)
# }
# 
# library(sf)
# bbox <- st_bbox(mapa_tarapaca$geometry)
# bbox$xmin <- bbox$xmin-0.2
# vector_tiles <- get_vector_tiles(bbox)
# 
# names(vector_tiles)
# 
# water <- as_sf(vector_tiles$water)
# roads <- as_sf(vector_tiles$roads)
# earth <- as_sf(vector_tiles$earth)
# land <- as_sf(vector_tiles$landuse) #uso de tierra (parques y zonas protegidas)
# boundaries <- as_sf(vector_tiles$boundaries) #son muy aproximados
# 
# table(land$kind)
# 
# ggplot(boundaries) + 
#   geom_sf() + 
#   theme_void() + 
#   coord_sf(datum = NA)
# 
# 
# names(roads)
# 
# roads$kind
# 
# ggplot() + 
#   #geom_sf(data = roads %>% filter(kind == "ferry"), colour = "red") +
#   #geom_sf(data = roads %>% filter(kind == "highway"), colour = "blue") +
#   geom_sf(data = roads %>% filter(kind == "minor_road"), colour = "green") +
#   geom_sf(data = roads %>% filter(kind == "major_road"), colour = "darkgrey") +
#   #geom_sf(data = roads %>% filter(cycleway == "lane"), colour = "orange") +
#   theme_void() + 
#   coord_sf(datum = NA)
# 
# ggplot() +
#   geom_sf(data = earth, colour = "transparent", fill="grey90") + #tierra
#   geom_sf(data = water, colour = "transparent", fill = "lightblue1") + #mar
#   geom_sf(data = mapa_tarapaca, aes(geometry=geometry), fill = "grey95", col = NA) + #fondo región
#   geom_sf(data = roads %>% filter(kind == "minor_road"), colour = "grey80") +
#   geom_sf(data = roads %>% filter(kind == "major_road"), colour = "grey70") +
#   geom_sf(data = mapa_tarapaca, aes(geometry=geometry), fill = NA, col = "grey30", alpha=0.5, size = 0.2) + #bordes región
#   geom_point(data = puntos_empresas, aes(x=x, y=y), size = 1, alpha = 0.1) +
#   geom_sf(data = land %>% filter(kind != "urban_area"), alpha = 0.2, 
#           fill = "forestgreen", col = NA) +
#   scale_fill_viridis_c() +
#   guides(fill = guide_legend()) + 
#   #coord_sf(datum = NA) +
#   coord_sf(xlim = c(-71, -68.35),
#           ylim = c(-21.7, -18.9),
#                         expand = FALSE) +
#   theme_void()





#—----
#OpenStreetMap ----
#remotes::install_github("ropensci/osmdata")

library(osmdata) # package for working with streets
library(ggmap)
library(rvest)

#explorar etiquetas disponibles
available_tags("highway")

#definir ciudad a obtener
ciudad <- "Iquique"
getbb(ciudad)


#obtener calles
big_streets <- getbb(ciudad)%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "motorway_link", "primary_link")) %>%
  osmdata_sf()

med_streets <- getbb(ciudad)%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
  osmdata_sf()


small_streets <- getbb(ciudad)%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway"
                  )) %>%
  osmdata_sf()

river <- getbb(ciudad)%>%
  opq()%>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()


#asignar crs
library(sf)
st_crs(river$osm_lines) <- 4326
st_crs(med_streets$osm_lines) <- 4326
st_crs(big_streets$osm_lines) <- 4326
st_crs(small_streets$osm_lines) <- 4326


#graficar
ggplot() +
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size = .8,
          alpha = .3) +
  geom_sf(data = med_streets$osm_lines %>% st_transform(crs = 4326),
          inherit.aes = FALSE,
          color = "black",
          size = .3,
          alpha = .5) +
  geom_sf(data = small_streets$osm_lines, #%>% st_transform(crs = 4326),
          inherit.aes = FALSE,
          color = "#666666",
          size = .2,
          alpha = .3) +
  geom_sf(data = big_streets$osm_lines %>% st_transform(crs = 3031),
          inherit.aes = FALSE,
          color = "black",
          size = .5,
          alpha = .6) +
# #zoom en iquique y alto hospicio
# coord_sf(xlim = c(-70.18, -70.07),
#          ylim = c(-20.31, -20.19),
#          expand = FALSE)
  #zoom tarapaca
  coord_sf(xlim = c(-71, -68.35),
           ylim = c(-21.7, -18.9),
           expand = FALSE)


#—----


#graficar mezclando capas
ggplot() +
  #mapa de base
  geom_sf(data = mapa_tarapaca, aes(geometry = geometry)) +
  #agua y tierra
  geom_sf(data = earth, colour = "transparent", fill="grey90") + #tierra
  geom_sf(data = water, colour = "transparent", fill = "lightblue1") + #mar
  #calles
  geom_sf(data = med_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black", size = .3, alpha = .5) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "#666666", size = .2, alpha = .3) +
  geom_sf(data = big_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black", size = .5, alpha = .6) +
  #puntos
  geom_point(data = puntos_empresas %>% filter(seccion_ciiu4cl == "D"), 
             aes(x=x, y=y, col = glosa_seccion), 
             alpha = 0.9, show.legend = F) +
  #zoom en iquique y alto hospicio
  coord_sf(xlim = c(-70.18, -70.07),
           ylim = c(-20.31, -20.19),
           expand = FALSE)


#compilar mapas ----
datos_mapas <- list(#"mar" = water,
                    #"tierra" = earth,
                    "región" = mapa_tarapaca,
                    "calles_chicas" = small_streets,
                    "calles_medianas" = med_streets,
                    "calles_grandes" = big_streets)

save(datos_mapas, file = "dataemprende_datos/datos_mapas.rdata")

load("dataemprende_datos/datos_mapas.rdata")


# fondo <- "#2D668E"
# degradado <- colorRampPalette(c("#2D668E", "#457B9D"))
# color_terreno <- degradado(3)[2]

#graficar
ggplot() +
  #mapa de base
  geom_sf(data = datos_mapas$región, aes(geometry = geometry),
          fill = color_oscuro, col = "red") +
  #agua y tierra
  #geom_sf(data = datos_mapas$tierra, colour = "transparent", fill="grey90") + #tierra
  #geom_sf(data = datos_mapas$mar, colour = "transparent", fill = "lightblue1") + #mar
  #calles
  geom_sf(data = datos_mapas$calles_medianas$osm_lines,
          color = color_claro, size = .3, alpha = .3, inherit.aes = F) +
  geom_sf(data = datos_mapas$calles_chicas$osm_lines,
          color = color_negro, size = .2, alpha = .3, inherit.aes = F) +
  geom_sf(data = datos_mapas$calles_grandes$osm_lines,
          color = color_negro, size = .5, alpha = .8, inherit.aes = F) +
  #puntos
  # geom_point(data = puntos_empresas %>%
  #              filter(letra == "H"), aes(x=x, y=y, col = glosa_seccion),
  #            alpha = 0.4, size = 1, col = color_claro, show.legend = F) +
  # #zoom en iquique y alto hospicio
  # coord_sf(xlim = c(-70.17, -70.06),
  #          ylim = c(-20.31, -20.195),
  #          expand = FALSE) +
  #zoom en alto hospicio
  coord_sf(xlim = c((-70.17+mover_x)+zoom, (-70.06+mover_x)-zoom),
           ylim = c((-20.31+mover_y)+zoom, (-20.195+mover_y)-zoom),
           expand = FALSE) +
  theme_void() +
  theme(plot.background = element_rect(fill = color_fondo, color = color_fondo), panel.background = element_rect(fill = color_fondo, color = color_fondo))

#iquique y alto hospicio
mover_x = 0
mover_y = 0
zoom = 0

# #centrar en alto hospicio
# mover_x = 0.021
# mover_y = -0.022
# zoom = 0.025

# # #centrar en iquique arriba
# mover_x = -0.029
# mover_y = 0.03
# zoom = 0.039

# #centrar en iquique al medio
# mover_x = -0.02
# mover_y = 0
# zoom = 0.039

#centrar en iquique abajo
mover_x = -0.014
mover_y = -0.016
zoom = 0.039

#seleccionar letra para filtrar datos
rubro_elegido <- rubros_sii[1]

puntos_empresas %>% 
  filter(rubro == rubro_elegido) %>%
  graficar_mapa_rubros()

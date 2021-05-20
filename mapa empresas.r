#en base a los datos scrappeados por Diego y la transformación de coordenadas de Claudio
#genera un mapa con puntos por cada empresa
library(tidyverse)
library(sgs)

#importar datos de Diego
#load("datos diego/Empresas_Taparaca.Rdata")

df <- readxl::read_xlsx("mapa claudio/Empresas_Tarapaca.xlsx")

df


puntos_empresas <- df %>%
  mutate(across(c(X, Y), ~ as.numeric(.x))) %>%
  sgs_points(coords = c("X", "Y"), epsg=3857) %>%
  sgs_en_wgs84() %>%
  as_tibble() %>%
  rename_all(~ tolower(.)) %>%
  mutate(comuna = stringr::str_to_sentence(nom_comuna)) %>%
  filter(!is.na(x),
         !is.na(y))

puntos_empresas %>%
  count(comuna)

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

p <- ggplot() +
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



mapa_tarapaca %>%
  mutate(
    CENTROID = map(geometry, st_centroid),
    COORDS = map(CENTROID, st_coordinates),
    long = map_dbl(COORDS, 1),
    lat = map_dbl(COORDS, 2)
  ) %>%
ggplot(aes(geometry = geometry, long, lat)) +
  geom_sf() +
  # geom_point(data = puntos_empresas %>% filter(seccion_ciiu4cl == "D"), 
  #            aes(x=x, y=y, col = glosa_seccion), 
  #            alpha = 0.1, show.legend = F) +
    # coord_sf(xlim = c(-70.5, -68.25),
    #          ylim = c(-21.7, -18.8),
    #          expand = FALSE)
  stat_maptiles(zoom = 7,
                url = "http://tile.stamen.com/terrain/%d/%d/%d.png",
                #url = "http://tile.stamen.com/watercolor/%d/%d/%d.jpg",
                force = T) +
  coord_sf(clip = "off", expand = F,
           xlim = c(-70.5, -68.25),
           ylim = c(-21.7, -16.8)) +
  mapview()

#—----
#agregar calles, mar, y zonas protegidas al gráfico

#https://www.dshkol.com/2018/better-maps-with-vector-tiles/
install.packages("rmapzen")
library(rmapzen)
options(nextzen_API_key="wkUP5UE4TDu92Vg14jut9A")
mz_set_tile_host_nextzen(key = getOption("nextzen_API_key"))

get_vector_tiles <- function(bbox){
  mz_box=mz_rect(bbox$xmin,bbox$ymin,bbox$xmax,bbox$ymax)
  mz_vector_tiles(mz_box)
}

library(sf)
bbox <- st_bbox(mapa_tarapaca$geometry)
bbox$xmin <- bbox$xmin-0.2
vector_tiles <- get_vector_tiles(bbox)

names(vector_tiles)

water <- as_sf(vector_tiles$water)
roads <- as_sf(vector_tiles$roads)
earth <- as_sf(vector_tiles$earth)
land <- as_sf(vector_tiles$landuse) #uso de tierra (parques y zonas protegidas)
boundaries <- as_sf(vector_tiles$boundaries) #son muy aproximados

table(land$kind)

ggplot(boundaries) + 
  geom_sf() + 
  theme_void() + 
  coord_sf(datum = NA)


names(roads)

roads$kind

ggplot() + 
  #geom_sf(data = roads %>% filter(kind == "ferry"), colour = "red") +
  #geom_sf(data = roads %>% filter(kind == "highway"), colour = "blue") +
  geom_sf(data = roads %>% filter(kind == "minor_road"), colour = "green") +
  geom_sf(data = roads %>% filter(kind == "major_road"), colour = "darkgrey") +
  #geom_sf(data = roads %>% filter(cycleway == "lane"), colour = "orange") +
  theme_void() + 
  coord_sf(datum = NA)

ggplot() +
  geom_sf(data = earth, colour = "transparent", fill="grey90") + #tierra
  geom_sf(data = water, colour = "transparent", fill = "lightblue1") + #mar
  geom_sf(data = mapa_tarapaca, aes(geometry=geometry), fill = "grey95", col = NA) + #fondo región
  geom_sf(data = roads %>% filter(kind == "minor_road"), colour = "grey80") +
  geom_sf(data = roads %>% filter(kind == "major_road"), colour = "grey70") +
  geom_sf(data = mapa_tarapaca, aes(geometry=geometry), fill = NA, col = "grey30", alpha=0.5, size = 0.2) + #bordes región
  geom_point(data = puntos_empresas, aes(x=x, y=y), size = 1, alpha = 0.1) +
  geom_sf(data = land %>% filter(kind != "urban_area"), alpha = 0.2, 
          fill = "forestgreen", col = NA) +
  scale_fill_viridis_c() +
  guides(fill = guide_legend()) + 
  #coord_sf(datum = NA) +
  coord_sf(xlim = c(-71, -68.35),
          ylim = c(-21.7, -18.9),
                        expand = FALSE) +
  theme_void()

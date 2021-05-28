#OpenStreetMap ----
#remotes::install_github("ropensci/osmdata")

library(osmdata) # package for working with streets
library(ggmap)
library(rvest)
library(sf)

#explorar etiquetas disponibles
available_tags("place") #highway

#definir ciudad a obtener
ciudad <- "Tarapaca"
#getbb("Iquique")
getbb(ciudad)


#obtener calles y carreteras
big_streets <- getbb(ciudad)%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "motorway_link", "primary_link",
                            "trunk")) %>%
  osmdata_sf()


med_streets <- getbb(ciudad)%>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
  osmdata_sf()

# estas no porque son muy chicas
# small_streets <- getbb(ciudad)%>%
#   opq()%>%
#   add_osm_feature(key = "highway", 
#                   value = c("residential", "living_street",
#                             "unclassified",
#                             "service", "footway"
#                   )) %>%
#   osmdata_sf()
#
# river <- getbb(ciudad) %>%
#   opq()%>%
#   add_osm_feature(key = "waterway", value = "river") %>%
#   osmdata_sf()



#obtener puntos de las comunas
available_tags_place <- available_tags("place")

lugares_tarapaca <- getbb("Tarapacá")%>%
  opq() %>%
  add_osm_feature(key = "place", 
                  value = available_tags_place) %>% #todos los lugares no más
  osmdata_sf()


lugares_tarapaca$osm_points %>% count(place)

lugares_tarapaca$osm_points %>% filter(place == "town")

#filtrar puntos (ejemplo)
# lugares_tarapaca$osm_points %>% filter(name %in% c("Pica", "Huara", "Colchane", "Camiña",
#                                                    "Alto Hospicio", "Iquique", "Pozo Almonte")) %>%
#   filter(place != "isolated_dwelling") #porque colchane sale dos veces

#filtrar lugares relevantes
lugares_tarapaca$osm_points <- lugares_tarapaca$osm_points %>% 
  filter(name %in% c("Pica", "Huara", "Colchane", "Camiña",
                     "Alto Hospicio", "Iquique", "Pozo Almonte")) %>%
  filter(place != "isolated_dwelling")




#asignar crs


st_crs(big_streets$osm_lines) <- 4326
#st_crs(river$osm_lines) <- 4326
st_crs(med_streets$osm_lines) <- 4326
#st_crs(small_streets$osm_lines) <- 4326
st_crs(lugares_tarapaca$osm_points) <- 4326







#cargar polígono regional
mapa_tarapaca <- chilemapas::mapa_comunas %>%
  left_join(
    chilemapas::codigos_territoriales %>%
      select(matches("comuna"))) %>%
  filter(codigo_region=="01")


#compilar mapas ----
datos_mapa_regional <- list(#"mar" = water,
  #"tierra" = earth,
  "región" = mapa_tarapaca,
  #"calles" = med_streets,
  "carreteras" = big_streets$osm_lines,
  "lugares" = lugares_tarapaca$osm_points)

#save(datos_mapa_regional, file = "dataemprende_datos/datos_mapa_regional.rdata")

#agregar datos a puntos
lugares_tarapaca_datos <- datos_mapa_regional$lugares %>%
  left_join(datos$empresas_comunas, 
            by = c("name" = "comuna")) %>%
  rename(empresas = n)


#graficar mezclando capas
ggplot() +
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
             stat = "sf_coordinates", col = color_negro,
             show.legend = F) +
  #texto
  ggrepel::geom_text_repel(data = datos_mapa_regional$lugares, aes(geometry = geometry, label = name), 
                           stat = "sf_coordinates", seed = 1993, point.padding = 0.2, min.segment.length = 9,
                           size = 3, col = color_negro, family = "Montserrat", alpha = 1) +
  #zoom region
  coord_sf(xlim = c(-70.4, -68.35),
           ylim = c(-21.7, -18.9), expand = FALSE) +
  # #zoom iquique y alto hospicio
  # coord_sf(xlim = c(-70.17, -70.06),
  #          ylim = c(-20.31, -20.195), expand = FALSE) +
  theme_void() +
  theme(plot.background = element_rect(fill = color_fondo, color = color_fondo), panel.background = element_rect(fill = color_fondo, color = color_fondo))

#system("git commit -m 'boceto mapa regional con comunas'")
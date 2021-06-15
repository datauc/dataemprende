library(tidyverse)
library(ggplot2)
library(tidytext)
fecha_s <- "11-jun-2021"
fecha_a <- "2021-06-11"
load(paste0("scrapping/yapo.cl/bases/base_yapo_", fecha_s, ".rdata"))

#base_yapo %>% count(categoria) %>% arrange(desc(n)) %>% distinct(categoria) %>% pull() %>% as.character() %>% dput()
categorias <- c("articulos-del-hogar", "bolsos-bisuteria-accesorios", "celulares", 
                "computadores", "jardin_herramientas", "moda-vestuario", "muebles", 
                "salud-belleza", "electrodomesticos", "vestuario-futura-mama-ninos", 
                "calzado", "consolas_videojuegos", "hobbies_outdoor", "servicios", 
                "television_camaras", "coches-articulos-infantiles", "deportes_gimnasia", 
                "juguetes", "animales_accesorios", "arte_antiguedades_colecciones", 
                "libros_revistas", "otros_productos", "bicicletas_ciclismo", 
                "musica_peliculas", "instrumentos_musicales")

categorias[c(1:8)]


load("~/Texto/stopwords/stopwords.Rdata")

#análisis unigrama ----
#tokenizar y filtrar stopwords
base_yapo_palabras <- base_yapo %>%
  select(producto, precio, categoria, pagina, fecha, hora, comuna) %>%
  #tokenizar
  unnest_tokens(palabra, producto, drop = F, to_lower = T) %>%
  select(palabra, everything()) %>%
  #limpiar
  filter(nchar(palabra) > 3) %>%
  filter(!(palabra %in% stopwords))
  
#graficar precio
base_yapo_palabras %>%
  ggplot() +
  geom_jitter(aes(x = 1, y = precio, col = categoria), alpha = 0.2) +
  theme_void() +
  theme(legend.position = "none")

#palabras obvias que no aportan al estar dentro de sus categorías respectivas
palabras_obvias <- c("bicicleta", "bicicletas", "bici", "bike",
                     "perros", "perro", "gatos", "mascotas", "mascota",
                     "zapatos", "zapatillas",
                     "ropa", "talla",
                     "juego", "piezas",
                     "coleccion",
                     "celular",
                     "kilos", "litro", "litros",
                     "niña", "niño", "niños", "bebé", "bebe", "meses",
                     "libro", "libros", "lectura")

#palabras relacionadas a la venta
palabras_meta <- c("busco", "oferta", "ofertas",
                   "Iquique", "Alto Hospicio",
                   "original", "originales")

#top conceptos por categoría
base_yapo_palabras_top <- base_yapo_palabras %>%
  group_by(categoria) %>%
  count(palabra) %>%
  filter(!(palabra %in% palabras_obvias),
         !(palabra %in% palabras_meta)) %>%
  arrange(categoria, desc(n)) %>%
  slice(1:10) %>%
  mutate(id = 1:n())



#—----

#outputs ----

#cantidad productos ----
#Categorías ordenadas por cantidad de productos en la última semana
base_yapo_resumen <- base_yapo %>%
  filter(precio > 1000) %>% 
  filter(fecha >= ymd(fecha_a) - weeks(1)) %>% #última semana
  group_by(categoria) %>% 
  summarize(cantidad = n(),
            precio_promedio = mean(precio, na.rm=T),
            precio_mediana = median(precio, na.rm=T),
            precio_min = min(precio, na.rm=T),
            precio_max = max(precio, na.rm=T)) %>% 
  arrange(desc(cantidad))

base_yapo_resumen %>% select(categoria, cantidad)


#precios promedio ----
#Promedio de precios por categoría
base_yapo_resumen %>% select(-cantidad)

#evolucion productos ----
#Evolución de cantidad de productos vendidos por categoría
base_yapo_evolucion_cantidad <- base_yapo %>%
  group_by(categoria, fecha) %>% 
  summarize(cantidad = n())

base_yapo_evolucion_cantidad %>% 
  ggplot(aes(fecha, cantidad, fill = categoria, col = categoria)) +
  geom_line() +
  theme(legend.position = "none")


#proyección de ventas ----


#productos más vendidos ----
base_yapo_palabras_top %>%
  filter(categoria == categorias[8]) %>% 
  ggplot() +
  geom_col(aes(x = n, y = fct_reorder(palabra, id, .desc=T))) +
  theme_minimal() +
  facet_wrap(~categoria, scales = "free_y", ncol = 5) +
  coord_cartesian(clip="off")


#evolución de cantidad de productos por rango de precios por categoría
#grafico por semanas, con puntos jitter en la cantidad?
base_yapo %>% 
  filter(precio > 15000,
         precio < 25000) %>% 
  group_by(categoria, fecha) %>% 
  summarize(cantidad = n()) %>% 
  ggplot(aes(fecha, cantidad, fill = categoria, col = categoria)) +
  geom_line() +
  theme(legend.position = "none")




#horas del día que se publican productos
base_yapo %>% 
  filter(fecha >= ymd(fecha_a) - months(1)) %>% #último mes
  mutate(horas = hour(hora)) %>% 
  group_by(horas) %>% 
  summarize(cantidad = n()) %>% 
  ggplot(aes(horas, cantidad)) +
  geom_point() +
  geom_smooth()
#poner fondo de día y de noche! poner sol y luna 
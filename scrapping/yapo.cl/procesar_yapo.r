library(dplyr)
library(stringr)
library(ggplot2)
library(tidytext)
library(lubridate)

#fecha_s <- "11-jun-2021"
#fecha_a <- "2021-06-11"
#load(paste0("scrapping/yapo.cl/bases/base_yapo_", fecha_s, ".rdata"))
load("scrapping/yapo.cl/bases_yapo.rdata")

fecha_a <- max(bases_yapo$fecha) #fecha maxima

#categorias_yapo[c(1:8)]

load("scrapping/stopwords.rdata")

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


#precalcular ----

#tokenizar nombres de productos y filtrar stopwords
base_yapo_palabras <- bases_yapo %>%
  select(producto, precio, categoria, pagina, fecha, hora, comuna) %>%
  #tokenizar
  unnest_tokens(palabra, producto, drop = F, to_lower = T) %>%
  select(palabra, everything()) %>%
  #limpiar
  filter(nchar(palabra) > 3) %>%
  filter(!(palabra %in% stopwords),
         !(palabra %in% palabras_obvias),
         !(palabra %in% palabras_meta))

#resumenes estadísticos de precios
base_yapo_resumen <- bases_yapo %>%
  filter(precio > 1000) %>% 
  filter(fecha >= ymd(fecha_a) - weeks(1)) %>% #última semana
  group_by(categoria) %>% 
  summarize(cantidad = n(),
            precio_promedio = mean(precio, na.rm=T),
            precio_mediana = median(precio, na.rm=T),
            precio_min = min(precio, na.rm=T),
            precio_max = max(precio, na.rm=T)) %>% 
  arrange(desc(cantidad))


#compilar ----

base_yapo_procesada <- list("entera" = bases_yapo,
                            "resumen" = base_yapo_resumen,
                            "palabras" = base_yapo_palabras)

save(base_yapo_procesada, file = "dataemprende/scrapping_yapo.rdata")
#load("dataemprende_datos/scrapping_yapo.rdata")

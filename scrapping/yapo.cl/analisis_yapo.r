library(tidyverse)
fecha_s <- "11-jun-2021"
load(paste0("scrapping/yapo.cl/bases/base_yapo_", fecha_s, ".rdata"))

base_yapo %>% count(categoria)

base_yapo %>% count(comuna)

library(ggplot2)

base_yapo %>%
  ggplot(aes(precio)) +
  geom_histogram(bins = 90) +
  geom_rug(alpha=0.1) +
  scale_x_continuous(n.breaks = 10, limits = c(0, 60000))


library(tidytext)
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
                     "busco",
                     "perros", "perro", "gatos", 
                     "zapatos", "zapatillas",
                     "ropa", "talla",
                     "kilos", "litro", "litros",
                     "niña", "niño", "niños", "bebé", "bebe", "meses",
                     "libro", "libros", "lectura")

#palabras relacionadas a la venta
palabras_meta <- c("busco", "oferta", "original", "Iquique",
                   "original", "originales")

#top 8 conceptos por categoría
base_yapo_palabras_top <- base_yapo_palabras %>%
  group_by(categoria) %>%
  count(palabra) %>%
  filter(!(palabra %in% palabras_obvias)) %>%
  arrange(categoria, desc(n)) %>%
  slice(1:8) %>%
  mutate(id = 1:n())

#graficar
base_yapo_palabras_top %>%
  ggplot() +
  geom_col(aes(x = n, y = fct_reorder(palabra, id, .desc=T))) +
  theme_minimal() +
  facet_wrap(~categoria, scales = "free_y", ncol = 5) +
  coord_cartesian(clip="off")


#análisis bigrama ----
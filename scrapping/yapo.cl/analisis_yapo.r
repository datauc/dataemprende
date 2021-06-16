library(dplyr)
library(stringr)
library(ggplot2)
library(tidytext)
fecha_s <- "11-jun-2021"
fecha_a <- "2021-06-11"
load(paste0("scrapping/yapo.cl/bases/base_yapo_", fecha_s, ".rdata"))

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

base_yapo_resumen %>% 
  select(categoria, cantidad) %>% 
  graficar_barras_horizontales(variable_categorica = "categoria", 
                               variable_numerica = "cantidad", slice = 10)


#precios promedio ----
#Promedio de precios por categoría
base_yapo_resumen %>% 
  select(-cantidad) %>% 
  #filter(categoria == categorias[8]) %>% 
  ggplot() +
  geom_segment(col = color_claro, size = 1, 
               aes(xend = 0, x = precio_promedio, 
                   y = forcats::fct_relevel(categoria, rev(categorias)), yend = forcats::fct_relevel(categoria, rev(categorias)))) +
  geom_point(size = 4, col = color_blanco, 
             aes(x = precio_promedio, y = forcats::fct_relevel(categoria, rev(categorias)))) +
  geom_point(size = 2.5, col = color_claro, 
             aes(x = precio_promedio, y = forcats::fct_relevel(categoria, rev(categorias)))) +
  #geom_text(aes(label = paste0("  $", stringr::str_trim(format(round(precio_promedio, -2), big.mark = ".", decimal.mark = ","))), 
  geom_text(size = 4, family = "Dosis ExtraLight SemiBold", col = color_blanco,
            aes(label = precio_promedio %>% round(-2) %>% 
                  format(big.mark = ".", decimal.mark = ",") %>% 
                  stringr::str_trim() %>% paste0("   $", .),
                x = precio_promedio, 
                y = forcats::fct_relevel(categoria, rev(categorias))),
            hjust = 0) +
  theme_minimal() +
  scale_x_continuous(expand = expansion(c(0, 0.5))) +
  coord_cartesian(clip="off") +
  theme_void() +
  theme(axis.text.y = element_text(hjust = 1, color = color_negro, family = "Montserrat", size = 9, margin = margin(r = 10))) +
  theme(plot.background = element_rect(fill = color_fondo, color = color_fondo))

#evolucion productos ----
#Evolución de cantidad de productos vendidos por categoría
base_yapo_evolucion_cantidad <- base_yapo %>%
  group_by(categoria, fecha) %>% 
  summarize(cantidad = n())

base_yapo_evolucion_cantidad %>% 
  #filtrar categoría
  filter(categoria == categorias[1]) %>% 
  #últimos x meses
  filter(fecha > lubridate::ymd(fecha_a) - months(3)) %>% 
  #suavizar datos
  mutate(cantidad = zoo::rollmean(cantidad, k = 4, fill = NA)) %>% 
  #graficar
 graficar_lineas_degradado_reg()





#productos más vendidos ----
base_yapo_palabras_top %>%
  filter(categoria == categorias[1]) %>% 
  mutate(palabra = str_to_sentence(palabra)) %>% 
  graficar_barras_horizontales(variable_categorica = "palabra",
                               variable_numerica = "n")

#(!) evolución rango ----
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



#horas del día ----
#horas del día que se publican productos
base_yapo %>% 
  filter(fecha >= ymd(fecha_a) - months(1)) %>% #último mes
  mutate(horas = hour(hora),
         horas = replace(horas, horas == 0, 24)) %>% 
  group_by(horas) %>% 
  summarize(cantidad = n()) %>% 
  mutate(punto_maximo = max(cantidad),
         punto_minimo = min(cantidad)) %>% 
  #mutate(horas_etiqueta = paste(horas, "hrs.")) %>%
  ggplot(aes(horas, cantidad)) +
  #noche
  annotate(geom = "rect", xmin = 0, xmax = 12, ymin = -Inf, ymax = Inf,
           fill = color_oscuro, alpha = 0.3) +
  #día
  # annotate(geom = "rect", xmin = 12, xmax = 23, ymin = -Inf, ymax = Inf,
  #           fill = color_claro, alpha = 0) +
  #laboral
  #annotate(geom = "rect", xmin = 8, xmax = 18, ymin = -Inf, ymax = Inf,
  #          fill = color_medio, alpha = 0.2) +
  geom_vline(xintercept = c(8, 18), col = color_claro, size = 1, alpha = 0.3) +
  #línea
  geom_smooth(fill = color_claro, size = 0, alpha = 0.3) +
  geom_line(col = color_claro, size = 1, alpha = 0.8, stat = "smooth", method = "loess", lineend = "round") +
  #puntos
  #geom_point(col = color_blanco, size = 5) +
  geom_point(col = color_blanco, size = 3) +
  #íconos
  geom_text(label = "\uF185", col = color_claro, alpha = 0.7, aes(x = 21+0.5, y = punto_maximo*0.1),
            size = 12, family = 'FontAwesome', show.legend=F, inherit.aes = F, check_overlap = T, hjust = 0.5) +
  geom_text(label = "\uF186", col = color_claro, alpha = 0.7, aes(x = 3-0.5, y = punto_maximo*0.9),
            size = 12, family = 'FontAwesome', show.legend=F, inherit.aes = F, check_overlap = T, hjust = 0.5) +
  scale_x_continuous(labels = function(x) paste(x, "hrs."), 
                     breaks = c(1, 6, 12, 18, 23), expand = expansion(add = c(0, 1.5))) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(axis.text.y = element_text(hjust = 1, color = color_negro, family = "Montserrat", size = 9, margin = margin(r = 5)),
        axis.text.x = element_text(hjust = 0.5, color = color_negro, family = "Montserrat", size = 9, margin = margin(t = 5, b=2))) +
  theme(plot.background = element_rect(fill = color_fondo, color = color_fondo))

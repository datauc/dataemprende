library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)

load("dataemprende_datos/scrapping_yapo.rdata")

#outputs ----

#productos categoria ----
#Categorías ordenadas por cantidad de productos en la última semana
base_yapo_procesada$resumen %>% 
  select(categoria, cantidad) %>% 
  graficar_barras_horizontales(variable_categorica = "categoria", 
                               variable_numerica = "cantidad", slice = 10)


#precios categoria ----
#Promedio de precios por categoría
base_yapo_procesada$resumen %>% 
  select(-cantidad) %>% 
  #filter(categoria == categorias_yapo[8]) %>% 
  ggplot() +
  geom_segment(col = color_claro, size = 1, 
               aes(xend = 0, x = precio_promedio, 
                   y = forcats::fct_relevel(categoria, rev(categorias_yapo)), yend = forcats::fct_relevel(categoria, rev(categorias_yapo)))) +
  geom_point(size = 4, col = color_blanco, 
             aes(x = precio_promedio, y = forcats::fct_relevel(categoria, rev(categorias_yapo)))) +
  geom_point(size = 2.5, col = color_claro, 
             aes(x = precio_promedio, y = forcats::fct_relevel(categoria, rev(categorias_yapo)))) +
  #geom_text(aes(label = paste0("  $", stringr::str_trim(format(round(precio_promedio, -2), big.mark = ".", decimal.mark = ","))), 
  geom_text(size = 4, family = "Dosis ExtraLight SemiBold", col = color_blanco,
            aes(label = precio_promedio %>% round(-2) %>% 
                  format(big.mark = ".", decimal.mark = ",") %>% 
                  stringr::str_trim() %>% paste0("   $", .),
                x = precio_promedio, 
                y = forcats::fct_relevel(categoria, rev(categorias_yapo))),
            hjust = 0) +
  theme_minimal() +
  scale_x_continuous(expand = expansion(c(0, 0.5))) +
  coord_cartesian(clip="off") +
  theme_void() +
  theme(axis.text.y = element_text(hjust = 1, color = color_negro, family = "Montserrat", size = 9, margin = margin(r = 10))) +
  theme(plot.background = element_rect(fill = color_fondo, color = color_fondo))

#tendencias productos ----
#Evolución de cantidad de productos vendidos por categoría
base_yapo_procesada$entera %>%
  group_by(categoria, fecha) %>% 
  summarize(cantidad = n()) %>% 
  #filtrar categoría
  filter(categoria == categorias_yapo[1]) %>% 
  #últimos x meses
  filter(fecha > max(fecha) - months(3)) %>% 
  #suavizar datos
  mutate(cantidad = zoo::rollmean(cantidad, k = 4, fill = NA)) %>% 
  #graficar
 graficar_lineas_degradado_reg()


# (!) evolución precios ----


#productos más vendidos ----
base_yapo_procesada$palabras %>%
  group_by(categoria) %>%
  count(palabra) %>%
  arrange(categoria, desc(n)) %>%
  slice(1:10) %>%
  mutate(id = 1:n()) %>%
  filter(categoria == categorias_yapo[1]) %>% 
  mutate(palabra = stringr::str_to_sentence(palabra)) %>% 
  graficar_barras_horizontales(variable_categorica = "palabra",
                               variable_numerica = "n")

#(!) evolución rango ----
#evolución de cantidad de productos por rango de precios por categoría
#grafico por semanas, con puntos jitter en la cantidad?
base_yapo_procesada$entera %>% 
  filter(precio > 15000,
         precio < 25000) %>% 
  group_by(categoria, fecha) %>% 
  summarize(cantidad = n()) %>% 
  ggplot(aes(fecha, cantidad, fill = categoria, col = categoria)) +
  geom_line() +
  theme(legend.position = "none")



#horas del día ----
#horas del día que se publican productos
base_yapo_procesada$entera %>% 
  filter(fecha >= max(fecha) - months(1)) %>% #último mes
  filter(categoria == categorias_yapo[3]) %>% 
  mutate(horas = lubridate::hour(hora),
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

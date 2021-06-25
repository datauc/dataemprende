library(dplyr)
library(stringr)
library(ggplot2)

load(paste0("scrapping/portalinmobiliario/bases/base_portal_", "2021-06-18", ".rdata"))

# #compilar ----
# base_portal_procesada <- list("entera" = base_portal,
#                             "resumen" = base_yapo_resumen,
#                             "palabras" = base_yapo_palabras)
# 
# save(base_portal_procesada, file = "dataemprende_datos/scrapping_portal.rdata")
#load("dataemprende_datos/scrapping_portal.rdata")


#—----
#precio por tipo ----
base_portal %>% 
  #mutate(precio = precio / 100000) %>% 
  group_by(tipo) %>% 
  summarize(promedio = mean(precio),
            min = min(precio),
            max = max(precio),
            q = list(quantile(precio))) %>% #calcular cuartiles
  tidyr::unnest_wider(q) %>% 
  #graficar
  ggplot(aes(y = tipo)) +
  #cuartiles
  geom_segment(col = color_claro, size=1.4, lineend = "round", aes(xend = `75%`, x = `25%`, yend = tipo)) +
  #promedio
  geom_point(aes(x = promedio), size = 6, col = color_blanco) +
  geom_point(aes(x = promedio), size = 4, col = color_claro) +
  #texto
  # geom_text(aes(x = promedio, label = paste("    ", round(promedio/1000000,1), "M")),
  #           col = color_claro, size = 4, angle = 30, hjust = 0, vjust = 0, family = "Dosis ExtraLight SemiBold") +
  geom_text(aes(x = promedio, label = paste(round(promedio/1000000,1), "M")), nudge_y = 0.15,
            col = color_claro, size = 5, angle = 0, hjust = 0.5, vjust = 0, family = "Dosis ExtraLight SemiBold") +
  geom_text(aes(x = `25%`, label = paste(round(`25%`/1000000,1), " ")),
            col = color_claro, size = 3.5, angle = 0, hjust = 1.1, vjust = 0.5, family = "Dosis ExtraLight SemiBold") +
  geom_text(aes(x = `75%`, label = paste(" ", round(`75%`/1000000,1))),
            col = color_claro, size = 3.5, angle = 0, hjust = -0.1, vjust = 0.5, family = "Dosis ExtraLight SemiBold") +
  theme_void() +
  scale_x_continuous(labels = function (x) paste(x/1000000, "M"), 
                     expand = expansion(mult = c(0.1, 0.2)),
                     breaks = c(1000000, 2000000, 3000000, 4000000, 5000000)) +
  theme(axis.text.y = element_text(hjust = 1, color = color_negro, family = "Montserrat", size = 9, margin = margin(r = 5)),
        axis.text.x = element_text(hjust = 0.5, color = color_negro, family = "Montserrat", size = 9, margin = margin(t = 5, b=2))) +
  theme(plot.background = element_rect(fill = color_fondo, color = color_fondo)) +
  theme(panel.grid.major.x = element_line(color = color_medio_2, size = 0.7))


#precio por metros ----
base_portal %>% 
  # mutate(metros = readr::parse_number(metros)) %>% 
  # #cortar en categorías
  # mutate(metros_cat = cut_width(metros, 100, boundary = 0)) %>% 
  # #límite superior
  # mutate(metros_cat = ifelse(metros > 500, "500+", as.character(metros_cat))) %>% 
  # #poner nombres a categorías
  # mutate(metros_cate = metros_cat,
  #        metros_cate = stringr::str_remove(metros_cate, "\\["),
  #        metros_cate = stringr::str_remove(metros_cate, "\\]"),
  #        metros_cate = stringr::str_remove(metros_cate, "\\("),
  #        metros_cate = stringr::str_replace(metros_cate, ",", " a "),
#        metros_cate = paste(metros_cate, "m²"),
#        metros_cate = as.factor(metros_cate)) %>% 
group_by(metros_cat) %>%
  summarize(promedio = mean(precio),
            min = min(precio),
            max = max(precio),
            n = n()) %>% 
  mutate(promedio = round(promedio/1000000, 2)) %>% 
  graficar_barras_horizontales(variable_categorica = "metros_cat",
                               variable_numerica = "promedio")


#relacion precio metros ----
base_portal %>% 
  ggplot(aes(as.numeric(metros), precio/1000000, col=tipo)) +
  geom_point(size = 3, alpha = 0.9) +
  xlim(0, 500) +
  ylim(0, 6) +
  scale_color_manual(values = colores_azules_3_grupos_2[c(1, 4, 6, 8)]) +
  theme_void() +
  labs(y = "Precio de arriendo (en millones)",
       x = "Metros cuadrados") +
  theme(axis.text.y = element_text(hjust = 1, color = color_negro, family = "Montserrat", size = 9, margin = margin(r = 5)),
        axis.text.x = element_text(hjust = 0.5, color = color_negro, family = "Montserrat", size = 9, margin = margin(t = 5, b=2))) +
  theme(axis.title.y = element_text(family = "Montserrat", color = color_oscuro, angle=90, margin=margin(r=6)),
        axis.title.x = element_text(family = "Montserrat", color = color_oscuro, angle=0, , margin=margin(t=6, b=4))) +
  theme(plot.background = element_rect(fill = color_fondo, color = color_fondo)) +
  theme(panel.grid.major.x = element_line(color = color_medio_2, size = 0.7),
        panel.grid.major.y = element_line(color = color_medio_2, size = 0.7)) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(family = "Montserrat", color = color_negro, margin = margin(r=16))) +
  guides(color = guide_legend(override.aes = list(size = 4)))

  

  
  #precio por zona (!) ----
#recodificar zonas
base_portal %>% 
  group_by(zona) %>% 
  summarize(promedio = mean(precio),
            min = min(precio),
            max = max(precio),
            n = n()) %>% 
  filter(n > 3)



#precio por dormitorio ----
base_portal %>% 
  # mutate(dormitorios_cat = case_when(dormitorios >= 10 ~ "10+",
  #                                    TRUE ~ as.character(dormitorios))) %>% 
  filter(!is.na(dormitorios)) %>% 
  mutate(dormitorios_cat = case_when(dormitorios == 1 ~ "1 dormitorio",
                                     dormitorios >= 10 ~"10+ dormitorios",
                                     TRUE ~ paste(dormitorios, "dormitorios"))) %>% 
   mutate(dormitorios_cat = as.factor(dormitorios_cat),
          dormitorios_cat = forcats::fct_relevel(dormitorios_cat, "10+ dormitorios", after = Inf),
          dormitorios_cat = forcats::fct_rev(dormitorios_cat)) %>% 
  group_by(dormitorios_cat) %>% 
  summarize(promedio = mean(precio),
            min = min(precio),
            max = max(precio)) %>% 
  mutate(promedio = round(promedio/1000000, 2)) %>% 
  graficar_barras_horizontales(variable_categorica = "dormitorios_cat",
                               variable_numerica = "promedio", rank = F)

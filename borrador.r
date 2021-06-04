datos_sii$tramos_comuna %>%
  select(comuna, año, tramo, empresas)

datos_sii$tramos_comuna_13 %>%
  ungroup() %>%
  select(comuna, tramo, empresas) %>%
  mutate(tramo4 = stringr::str_remove_all(tramo, " \\d")) 


tramos_comuna_13 %>%
  #excluir grandes empresas
  filter(tramo4 != "Grande") %>%
  droplevels() %>%
  #filtrar comuna
  filter(comuna == comunas_sii[2]) %>%
  #calcular punto más alto
  group_by(año) %>%
  mutate(punto_mas_alto = sum(empresas)) %>%
  #graficar
  ggplot(aes(año, empresas, fill = tramo, col=tramo)) +
  geom_area(show.legend = F) +
  geom_point(size=0, alpha=0) +
  scale_x_continuous(breaks = años_sii, expand = expansion(add=c(0.5, 2))) +
  scale_fill_manual(values = c(colorspace::desaturate("red", 0),
                               colorspace::desaturate("red", 0.2),
                               colorspace::desaturate("red", 0.4),
                               colorspace::desaturate("orange", 0),
                               colorspace::desaturate("orange", 0.2),
                               colorspace::desaturate("orange", 0.4),
                               colorspace::desaturate("purple", 0),
                               colorspace::desaturate("purple", 0.2),
                               # colorspace::desaturate("blue", 0),
                               # colorspace::desaturate("blue", 0.2),
                               # colorspace::desaturate("blue", 0.4),
                               # colorspace::desaturate("blue", 0.6),
                               "gray70"
                               ),
                    drop = F, aesthetics = c("col", "fill")) +
  ggrepel::geom_text_repel(family = "Dosis ExtraLight SemiBold", size = 5, 
                           aes(label = paste0("", empresas), 
                               x = max(año)+0.5), position = position_stack(vjust = 0.5),
                           hjust=0, show.legend=F, #check_overlap = T, 
                           direction = "y", seed=1993,
                           force = 1.5,
                           box.padding = 0,
                           segment.alpha = 0.2,
                           data = . %>% filter(año == 2019)) +
  geom_segment(col = color_blanco, alpha = 0.3, 
               aes(x=año, y=0, yend=punto_mas_alto, xend=año), show.legend = F,
               inherit.aes = F,
               data = . %>% select(año, punto_mas_alto) %>% distinct() %>% filter(año != 2005, año != 2019)) + 
  theme_void() +
  guides(col = guide_legend(override.aes = list(size=5, alpha=1, fill=NA, text=NA), ncol = 3)) +
  theme(plot.background = element_rect(fill = color_fondo, color = color_fondo),
        panel.background = element_rect(fill = color_fondo, color = color_fondo),
        text = element_text(color = color_oscuro, family = "Montserrat"),
        axis.text = element_text(color = color_negro),
        axis.text.x = element_text(angle=90, vjust=0.5, margin=margin(t = -8)),
        axis.ticks = element_blank(), panel.grid = element_blank(), axis.title.x = element_blank(),
        axis.text.y = element_blank()) + #element_text(margin=margin(r = 4), hjust = 0)) +
  theme(legend.position = "bottom",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(color = color_blanco, margin = margin(r=20)))


tramos_comuna_13 %>%
  #excluir grandes empresas
  filter(tramo4 != "Grande") %>%
  droplevels() %>%
  #filtrar comuna
  filter(comuna == comunas_sii[2])

graficar_area_aditiva <- function(data, 
                             variable_y = "empresas",
                             variable_categorica = "tramo",
                             n_columnas=3,
                             numero_largo=1){

  p <- data %>%
    rename(variable_y_elegida = all_of(variable_y),
           variable_categorica_elegida = all_of(variable_categorica)) %>%
  #calcular punto más alto
  group_by(año) %>%
  mutate(punto_mas_alto = sum(variable_y_elegida)) %>%
  #graficar
  ggplot(aes(año, variable_y_elegida, fill = variable_categorica_elegida, col = variable_categorica_elegida)) +
  geom_area(show.legend = F) +
  geom_point(size=0, alpha=0) +
  scale_x_continuous(breaks = años_sii, expand = expansion(add=c(0.5, 2*numero_largo))) +
  scale_fill_manual(values = c("#A8DADC", "#8CC4C5", "#55B1B5", 
                               "#6BA0B5", "#5290A7", "#32819A", 
                               "#2D668E", "#02537A",
                               "gray70"),
  drop = F, aesthetics = c("col", "fill")) +
  ggrepel::geom_text_repel(family = "Dosis ExtraLight SemiBold", size = 5, 
                           aes(label = paste0("", variable_y_elegida), 
                               x = max(año)+0.5), position = position_stack(vjust = 0.5),
                           hjust=0, show.legend=F, #check_overlap = T, 
                           direction = "y", seed=1993,
                           force = 1.5,
                           box.padding = 0,
                           segment.alpha = 0.2,
                           data = . %>% filter(año == 2019)) +
  geom_segment(col = color_blanco, alpha = 0.3, 
               aes(x=año, y=0, yend=punto_mas_alto, xend=año), show.legend = F,
               inherit.aes = F,
               data = . %>% select(año, punto_mas_alto) %>% distinct() %>% filter(año != 2005, año != 2019)) + 
  theme_void() +
  guides(col = guide_legend(override.aes = list(size=5, alpha=1, fill=NA, text=NA), ncol = n_columnas)) +
  theme(plot.background = element_rect(fill = color_fondo, color = color_fondo),
        panel.background = element_rect(fill = color_fondo, color = color_fondo),
        text = element_text(color = color_oscuro, family = "Montserrat"),
        axis.text = element_text(color = color_negro),
        axis.text.x = element_text(angle=90, vjust=0.5, margin=margin(t = -8)),
        axis.ticks = element_blank(), panel.grid = element_blank(), axis.title.x = element_blank(),
        axis.text.y = element_blank()) + #element_text(margin=margin(r = 4), hjust = 0)) +
  theme(legend.position = "bottom",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(color = color_blanco, margin = margin(r=20)))

return(p)
}

tramos_comuna_13 %>%
  #excluir grandes empresas
  filter(tramo4 != "Grande") %>%
  droplevels() %>%
  #filtrar comuna
  filter(comuna == comunas_sii[2]) %>%
  graficar_area_aditiva()


color_medio <- colorspace::hex(colorspace::mixcolor(0.5, 
                                                    colorspace::hex2RGB(color_claro), 
                                                    colorspace::hex2RGB(color_oscuro)))

scales::show_col(c(color_blanco,
                   color_claro,
                   color_medio,
                   color_oscuro,
                   color_negro))

scales::show_col(c(colorspace::desaturate(color_claro, 0),
                   colorspace::desaturate(color_claro, -0.4) %>% colorspace::darken(0.1),
                   colorspace::desaturate(color_claro, -0.6) %>% colorspace::darken(0.2),
                   colorspace::desaturate(color_medio, 0),
                   colorspace::desaturate(color_medio, -0.4) %>% colorspace::darken(0.1),
                   colorspace::desaturate(color_medio, -0.6) %>% colorspace::darken(0.2),
                   colorspace::desaturate(color_oscuro, 0),
                   colorspace::desaturate(color_oscuro, -0.15) %>% colorspace::darken(0.2)))

colores_azules_3_grupos <- c(colorspace::desaturate(color_claro, 0),
  colorspace::desaturate(color_claro, -0.4) %>% colorspace::darken(0.1),
  colorspace::desaturate(color_claro, -0.6) %>% colorspace::darken(0.2),
  colorspace::desaturate(color_medio, 0),
  colorspace::desaturate(color_medio, -0.4) %>% colorspace::darken(0.1),
  colorspace::desaturate(color_medio, -0.6) %>% colorspace::darken(0.2),
  colorspace::desaturate(color_oscuro, 0),
  colorspace::desaturate(color_oscuro, -0.15) %>% colorspace::darken(0.2))

dput(colores_azules_3_grupos)

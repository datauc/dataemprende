#el mapa animado que aparece de fondo en el header de la página

library(dplyr)
library(ggplot2)
library(gganimate)

load("dataemprende_datos/datos_precalculados.rdata")

load("datos_sii.rdata")

names(datos_sii)

#preparar datos
datos_mapa <- datos_sii$empresas %>%
  group_by(año, comuna) %>%
  summarize(empresas = sum(empresas))

comunas <- readxl::read_excel("~/Tarapacá/Demografía/Datos/comunas.xlsx", n_max=7) %>%
  select(comuna, poblacion)


datos_mapa <- datos_sii$empresas %>%
  filter(año==2019) %>%
  group_by(rubro) %>%
  mutate(porcentaje = empresas/sum(empresas))

datos_mapa <- datos_sii$trabajadores %>%
  mutate(comuna = as.factor(comuna)) %>%
  filter(año==2019,
         género == "total",
         calculo == "total",
         tipo == "dependencia") %>% 
  group_by(rubro, comuna) %>%
  summarize(empresas = mean(empresas),
            trabajadores = sum(valor)) %>%
  #completar comunas faltantes porque tienen cero, y rellenar missings con ceros
  tidyr::complete(rubro, comuna) %>%
  mutate(across(where(is.numeric), ~ tidyr::replace_na(.x, 0))) %>%
  #agregar población
  left_join(comunas) %>%
  ungroup() %>%
  #calcular
  group_by(rubro) %>%
  mutate(prop = (trabajadores/poblacion)*100) %>%
  ungroup() %>%
  mutate(tasa = (empresas/poblacion)*1000) %>%
  #destacar comuna mayor
  group_by(rubro) %>%
  mutate(rank = rank(tasa, ties.method = "first")) %>%
  mutate(top = if_else(rank == 7, "top", "otras"))

#calcular % de la población que es cada rubro, quizás así se vean comunas chicas
#poner color celeste a punto más alto en cada paso

#importar mapa y agregarle datos
mapa_regional <- chilemapas::mapa_comunas %>%
  left_join(
    chilemapas::codigos_territoriales %>%
      select(matches("comuna"))
  ) %>%
  rename(comuna = nombre_comuna) %>%
  mutate(comuna = recode(comuna,
                         "Camina"="Camiña")) %>%
  #datos
  left_join(datos_mapa) %>%
  filter(codigo_region=="01")


fondo <- "#2D668E"

degradado <- colorRampPalette(c("#2D668E", "#457B9D"))

terreno <- degradado(3)[2]

#graficar
p <- mapa_regional %>%
  ggplot(aes(geometry = geometry)) +
  geom_sf(data = mapa_regional %>% select(geometry:rubro) %>% distinct(), 
          col = fondo, fill = terreno, size = 0.3) +
  geom_point(aes(size = tasa, col = top,
                 group = interaction(tasa, top)),
                         alpha = 0.6,
                         stat = "sf_coordinates", show.legend=F) +
  scale_color_manual(values = c("#F1FAEE", "#A8DADC")) +
  scale_size_continuous(range = c(0, 30)) +
  #coord_sf(expand = T) +
  theme_void(base_size = 15) +
  theme(plot.background = element_rect(fill = fondo, color = fondo))



#el fondo oscuro es #2D668E
#el grupo es lo que define la animación
#si se pone data en uan capa, se excluye de la animación

anim <- p +
  #transition_manual(rubro) +
  transition_states(rubro,
                   transition_length = 3,
                   state_length = 2) +
  enter_grow() +
  exit_shrink() +
  #ggtitle("{rubro}") +
  ease_aes('linear')#ease_aes('cubic-in-out')

anim

anim_save(animation = anim, 
          filename = "anim.gif", 
          fps = 24,
          bg = fondo,
          duration = 20,
          rewind = T)

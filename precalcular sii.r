####
####
####
#en este archivo se sacan los cálculos generales que vamos a usar
#también se produce la lista datos.rdata que tiene los datos precalculados y listos para el shiny
####
####
####

library(tidyverse)
load("datos_sii_act.rdata")
#datos vienen del archivo "~/SII/SII 2019.R"

source("dataemprende_datos/variables.r")

comuna_elegida <- comunas_sii[2]
rubro_elegido <- rubros_sii[1]

subrubros_sii <- datos_sii$ventas_act %>%
  #filtrar subrubros sin ventas anuales
  #filter(ventas_anuales_uf > 0) %>%
  select(subrubro) %>%
  distinct() %>%
  pull()

#filtrar subrubros a partir de rubro
subrubros_filtrados <- datos_sii$ventas_act %>%
  filter(rubro == rubro_elegido) %>%
  #filtrar subrubros sin ventas anuales
  #filter(ventas_anuales_uf > 0) %>%
  select(subrubro) %>%
  distinct() %>%
  pull()

subrubro_elegido <- subrubros_filtrados[2]

#filtrar actividades a partir de subrubros
actividad_filtradas <- datos_sii$ventas_act %>%
  filter(subrubro == subrubro_elegido) %>%
  #filtrar subrubros sin ventas anuales
  #filter(ventas_anuales_uf > 0) %>%
  select(actividad) %>%
  distinct() %>%
  pull()

actividad_elegida <- actividad_filtradas[1]


#—----

#EMPRESAS ----

#región ----
#total empresas región
datos_sii$empresas %>%
  filter(año == 2019) %>%
  summarize(n = sum(empresas, na.rm = T))


#comunas ----
empresas_comunas <- datos_sii$empresas %>%
  filter(año == 2019) %>%
  group_by(comuna) %>%
  summarize(n = sum(empresas, na.rm = T))

#rubros ----
empresas_rubros <- datos_sii$empresas %>%
  filter(año == 2019) %>%
  group_by(rubro) %>%
  summarize(n = sum(empresas, na.rm = T))

#comuna elegida ----
#empresas comuna elegida
datos_sii$empresas %>%
  filter(comuna == comuna_elegida) %>%
  summarize(n = sum(empresas, na.rm = T))

#rubro elegido x comunas ----
#empresas por comuna del rubro elegido
datos_sii$empresas %>%
  filter(rubro == rubro_elegido,
         año == 2019) %>% 
  select(comuna, empresas) %>%
  arrange(desc(empresas))

#rubro elegido x comuna elegida ----
#empresas por comuna elegida y rubro elegido
datos_sii$empresas %>%
  filter(comuna == comuna_elegida, #picker
         rubro == rubro_elegido,
         año == 2019) %>% 
  select(empresas) %>%
  pull()

#subrubros----
empresas_subrubros <- datos_sii$empresas_act %>%
  filter(año == 2019) %>%
  group_by(rubro, subrubro) %>%
  summarize(n = sum(empresas, na.rm = T))

#subrubro elegido ----
#empresas subrubro región
datos_sii$empresas_act %>%
  filter(subrubro == subrubro_elegido) %>%
  summarize(n = sum(empresas, na.rm = T))

#subrubro elegido x comunas ----
#empresas por comuna por subrubro elegido
datos_sii$empresas_act %>%
  filter(subrubro == subrubro_elegido,
         año == 2019) %>% 
  select(comuna, empresas) %>%
  arrange(desc(empresas))

#subrubro elegido x comuna elegida ----
#empresas por comuna elegida y subrubro elegido
datos_sii$empresas_act %>%
  filter(comuna == comuna_elegida, #picker
         subrubro == subrubro_elegido,
         año == 2019) %>% 
  select(empresas) %>%
  pull()

#actividades ----
#empresas totales por actividad 
#(mostrar todas las actividades del subrubro)
datos_sii$empresas_act %>%
  filter(subrubro == subrubro_elegido,
         año == 2019) %>%
  group_by(actividad) %>%
  summarize(empresas = sum(empresas)) %>%
  arrange(desc(empresas))

#actividades x comunas ----
#empresas actividad por comuna
datos_sii$empresas_act %>%
  filter(actividad == actividad_elegida,
         año == 2019) %>%
  group_by(comuna, actividad) %>%
  summarize(empresas = sum(empresas)) %>%
  arrange(desc(empresas)) %>%
  select(-actividad)

#g evolución rubro ----
#gráfico aumento empresas del rubro
#pasado a global como funcion graficar_lineas_degradado()
#https://r.789695.n4.nabble.com/plot-background-excel-gradient-style-background-td4632138.html#a4634954
colores_degradado <- colorRampPalette(c(color_claro, color_fondo))

fondo_degradado <- grid::rasterGrob(colores_degradado(5), width=unit(1,"npc"), height = unit(1,"npc"), interpolate = T) 

empresas_año_rubro_region <- datos_sii$empresas %>%
  group_by(año, rubro) %>%
  summarize(empresas = sum(empresas))

empresas_año_rubro_comuna <- datos_sii$empresas

datos_sii$empresas %>%
  filter(comuna == comuna_elegida) %>% #picker
  filter(rubro == rubros_sii[2]) %>% #picker
  ggplot(aes(año, empresas, col=rubro)) +
  #fondo degradado
  annotation_custom(fondo_degradado, xmin=2005+0.008, xmax=2019-0.008, ymin=0, ymax=Inf) +
  #líneas de fondo
  geom_segment(col = color_claro, alpha = 0.15, aes(x=año, y=0, yend=empresas, xend=año), show.legend = F) +
  #colores de fondo arriba/abajo
  geom_ribbon(fill = color_fondo, col=color_fondo, alpha = 1, aes(ymin = empresas, ymax = Inf), show.legend = F) + #fondo oscuro (arriba)
  geom_area(fill = color_fondo, alpha = 0.1, show.legend = F) + #fondo claro (abajo)
  #línea
  geom_line(color = color_claro, size = 1.2, show.legend = F, ) +
  #punto
  #geom_point(col=color_negro, alpha=0.6, size=3) + #punto chico negro
  geom_point(color = color_blanco, size=1.5) + #punto chico claro
  geom_point(color = color_blanco, size=6, data = . %>% filter(año == 2019), aes(x = max(año), y=max(empresas))) + #punto grande claro
  geom_point(color = color_claro, size=2, data = . %>% filter(año == 2019), aes(x = max(año), y=max(empresas))) + #punto grande blanco
  #líneas del gráfico
  #geom_segment(inherit.aes = F, color = color_negro, y=0, aes(x=min(año), xend=min(año), yend=max(empresas))) +
  #geom_segment(inherit.aes = F, color = color_negro, y=0, aes(x=min(año), xend=max(año)+0.2, yend=0)) +
  scale_x_continuous(breaks = años_sii, expand = expansion(add=c(0, 3))) +
  #scale_y_continuous(expand = expansion(mult=c(0, 0.15))) +
  #texto
  geom_text(inherit.aes = F, aes(x = max(año)+0.3, y=max(empresas), label = max(empresas)), color=color_blanco,
            hjust=0, check_overlap = T) +
  #tema
  theme(axis.text.x = element_text(angle=90, vjust=0.5)) +
  theme(plot.background = element_rect(fill = color_fondo, color = color_fondo),
        panel.background = element_rect(fill = color_fondo, color = color_fondo),
        text = element_text(color = color_oscuro),
        axis.text = element_text(color = color_negro),
        axis.ticks = element_blank(), panel.grid = element_blank(), axis.title.x = element_blank(),
        axis.text.x = element_text(margin=margin(t=-5)),
        axis.text.y = element_text(margin=margin(r=1)))



datos_sii$empresas %>%
  filter(comuna == comunas_sii[5]) %>% #picker
  filter(rubro == rubros_sii[2]) %>% #picker
  graficar_lineas_degradado()


#g evolución subrubros ----
#gráfico empresas por subrubros, destacando el seleccionado
datos_sii$empresas_act %>%
  filter(comuna == comuna_elegida) %>% #picker
  filter(rubro == rubro_elegido) %>% #picker
  group_by(año, subrubro) %>%
  summarize(empresas = sum(empresas)) %>%
  mutate(tipo = case_when(subrubro == subrubro_elegido ~ "elegido",
                          TRUE ~ "no")) %>%
  ggplot(aes(año, empresas, col = subrubro, alpha = tipo)) +
  geom_line(size = 1, show.legend=F) +
  scale_x_continuous(breaks = años_sii) +
  scale_y_log10() +
  scale_alpha_manual(values = c(1, 0.5)) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))

#g evolución actividades ----
#gráfico empresas por actividades
datos_sii$empresas_act %>%
  filter(comuna == comuna_elegida) %>% #picker
  filter(rubro == rubro_elegido) %>% #picker
  group_by(año, actividad) %>%
  summarize(empresas = sum(empresas)) %>%
  ggplot(aes(año, empresas, col = actividad)) +
  geom_line(size = 1, show.legend=F) +
  scale_x_continuous(breaks = años_sii) +
  scale_y_log10() +
  scale_alpha_manual(values = c(1, 0.5)) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))


#gráfico empresas por rubro, subrubro y actividad
#no sirve porque actividad es muy chico
# datos_sii$empresas_act %>%
#   filter(comuna == comuna_elegida) %>% #picker
#   filter(rubro == rubro_elegido) %>% #picker
#   group_by(año, subrubro) %>%
#   summarize(empresas = sum(empresas)) %>%
#   mutate(tipo = "subrubro") %>%
#   bind_rows(datos_sii$empresas_act %>%
#               filter(comuna == comuna_elegida,
#                      rubro == rubro_elegido,
#                      actividad == actividad_elegida) %>%
#               group_by(año, actividad) %>%
#               summarize(empresas = sum(empresas)) %>%
#               rename(subrubro = actividad) %>%
#               mutate(tipo = "actividad")) %>%
#   ggplot(aes(año, empresas, col = subrubro, linetype = tipo)) +
#   geom_line(show.legend=F) +
#   scale_x_continuous(breaks = años_sii) +
#   scale_linetype
#   theme(axis.text.x = element_text(angle=90, vjust=0.5))


# library(ggraph)
# library(igraph)
# library(viridis)
# 
# # We need a data frame giving a hierarchical structure. Let's consider the flare dataset:
# edges <- flare$edges
# vertices <- flare$vertices
# mygraph <- graph_from_data_frame( edges, vertices=vertices )
# 
# # Control the size of each circle: (use the size column of the vertices data frame)
# ggraph(mygraph, layout = 'circlepack', weight= size) + 
#   geom_node_circle() +
#   theme_void()


#crecimiento subrubros region ----
datos_sii$empresas_act %>%
  #filter(comuna == comuna_elegida) %>% #picker
  filter(rubro == rubro_elegido) %>% #picker
  group_by(año, rubro, subrubro) %>%
  summarize(empresas = sum(empresas)) %>%
  filter(subrubro == subrubro_elegido) %>%
  ungroup() %>%
  arrange(desc(año)) %>%
  slice(1:2) %>%
  mutate(lag = lead(empresas),
         dif = empresas-lag,
         crecimiento = dif/empresas) %>%
  select(-año) %>%
  slice(1)
#las empresas del subrubro crecieron un xxx% en el último año

lista <- list()
for (i in subrubros_sii) {
  cat(i, fill=T)
  parte <- datos_sii$empresas_act %>%
    #filter(comuna == comuna_elegida) %>% #picker
    #filter(rubro == rubro_elegido) %>% #picker
    group_by(año, rubro, subrubro) %>%
    summarize(empresas = sum(empresas), .groups = "drop_last") %>%
    filter(subrubro == i) %>%
    ungroup() %>%
    arrange(desc(año)) %>%
    slice(1:2) %>%
    mutate(lag = lead(empresas),
           dif = empresas-lag,
           crecimiento = dif/empresas) %>%
    select(-año) %>%
    slice(1)
  #print(parte)
  
  lista[[i]] <- parte
}
crecimiento_subrubros_1_region <- bind_rows(lista)


#crecimiento 5 años
lista <- list()
for (i in subrubros_sii) {
  cat(i, fill=T)
parte <- datos_sii$empresas_act %>%
  #filter(comuna == comuna_elegida) %>% #picker
  #filter(rubro == rubro_elegido) %>% #picker
  group_by(año, rubro, subrubro) %>%
  summarize(empresas = sum(empresas), .groups = "drop_last") %>%
  filter(subrubro == i) %>%
  ungroup() %>%
  arrange(desc(año)) %>%
  slice(1, 6:7) %>% #2019-5= año 2014 como referencia
  mutate(lag = lead(empresas),
         dif = empresas-lag,
         crecimiento = dif/empresas) %>%
  select(-año) %>%
  slice(1)
#print(parte)

lista[[i]] <- parte
}
crecimiento_subrubros_5_region <- bind_rows(lista)


#crecimiento 10 años
lista <- list()
for (i in subrubros_sii) {
  cat(i, fill=T)
  parte <- datos_sii$empresas_act %>%
  #filter(comuna == comuna_elegida) %>% #picker
  #filter(rubro == rubro_elegido) %>% #picker
  group_by(año, rubro, subrubro) %>%
  summarize(empresas = sum(empresas), .groups = "drop_last") %>%
  filter(subrubro == i) %>%
  ungroup() %>%
  arrange(desc(año)) %>%
  slice(1, 10:11) %>% #2019 vs 2009
  mutate(lag = lead(empresas),
         dif = empresas-lag,
         crecimiento = dif/empresas) %>%
  select(-año) %>%
  slice(1)
#print(parte)

lista[[i]] <- parte
}
crecimiento_subrubros_10_region <- bind_rows(lista)


#combinar
crecimiento_subrubros_region <- crecimiento_subrubros_1_region %>%
  select(rubro, subrubro) %>%
  arrange(rubro) %>%
  left_join(crecimiento_subrubros_1_region %>%
              rename_with(where(is.numeric), .fn = ~ paste0(.x, "_1"))
            ) %>%
  left_join(crecimiento_subrubros_5_region %>%
              rename_with(where(is.numeric), .fn = ~ paste0(.x, "_5"))
            ) %>%
  left_join(crecimiento_subrubros_10_region %>%
              rename_with(where(is.numeric), .fn = ~ paste0(.x, "_10")))

#probar
(crecimiento_subrubros_region %>%
  filter(subrubro == subrubro_elegido))$crecimiento_5
#—----

#TRABAJADORES ----

#región ----
#trabajadores totales
datos_sii$trabajadores %>%
  filter(año == 2019,
         género == "total",
         tipo == "dependencia",
         calculo == "total") %>%
  summarize(n = sum(valor, na.rm = T)) %>%
  pull()

#comuna elegida ----
datos_sii$trabajadores %>%
  filter(comuna == comuna_elegida,
         año == 2019,
         género == "total",
         tipo == "dependencia",
         calculo == "total") %>%
  summarize(n = sum(valor, na.rm = T)) %>%
  pull() 

#rubros ----
trabajadores_rubros <- datos_sii$trabajadores %>%
  filter(año == 2019,
         género == "total",
         tipo == "dependencia",
         calculo == "total") %>%
  group_by(rubro) %>%
  summarize(n = sum(valor, na.rm = T))

#rubro elegido ----
datos_sii$trabajadores %>%
  filter(rubro == rubro_elegido,
         año == 2019,
         género == "total",
         tipo == "dependencia",
         calculo == "total") %>%
  summarize(n = sum(valor, na.rm = T))

#rubro elegido x comunas ----
datos_sii$trabajadores %>%
  filter(rubro == rubro_elegido,
         año == 2019,
         género == "total",
         tipo == "dependencia",
         calculo == "total") %>%
  group_by(comuna) %>%
  summarize(n = sum(valor, na.rm = T))

trabajadores_comuna_rubro <- datos_sii$trabajadores %>%
  filter(año == 2019,
         género == "total",
         tipo == "dependencia",
         calculo == "total") %>%
  group_by(comuna, rubro) %>%
  summarize(n = sum(valor, na.rm = T))


#subrubro elegido ----
datos_sii$trabajadores_act %>%
  filter(subrubro == subrubro_elegido,
         año == 2019,
         género == "total",
         tipo == "dependencia",
         calculo == "total") %>%
  summarize(n = sum(valor, na.rm = T)) %>%
  pull()

#subrubro elegido x comunas ----
datos_sii$trabajadores_act %>%
  filter(subrubro == subrubro_elegido,
         año == 2019,
         género == "total",
         tipo == "dependencia",
         calculo == "total") %>%
  group_by(comuna) %>%
  summarize(n = sum(valor, na.rm = T))

trabajadores_comuna_subrubro <- datos_sii$trabajadores_act %>%
  filter(año == 2019,
         género == "total",
         tipo == "dependencia",
         calculo == "total") %>%
  group_by(comuna, rubro, subrubro) %>%
  summarize(n = sum(valor, na.rm = T))
  
#subrubro elegido x género ----
datos_sii$trabajadores_act %>%
  filter(subrubro == subrubro_elegido,
         año == 2019,
         género != "total",
         tipo == "dependencia",
         calculo == "total") %>%
  group_by(género) %>%
  summarize(n = sum(valor, na.rm = T))

# dependencia: comunas ----
datos_sii$trabajadores %>%
  filter(año == 2019,
         género != "total",
         tipo == "dependencia",
         calculo == "total") %>%
  group_by(comuna, dependencia) %>%
  summarize(n = sum(valor, na.rm = T)) %>%
  pivot_wider(values_from = n, names_from = dependencia)


# dependencia: comuna elegida ----
#trabajadores comuna elegida por dependencia
datos_sii$trabajadores %>%
  filter(comuna == comuna_elegida,
         año == 2019,
         género == "total",
         tipo == "dependencia",
         calculo == "total") %>%
  group_by(dependencia) %>%
  summarize(n = sum(valor, na.rm = T)) %>%
  pivot_wider(values_from = n, names_from = dependencia)

# dependencia: rubro elegido ----
#trabajadores rubro elegido por dependencia
datos_sii$trabajadores %>%
  filter(rubro == rubro_elegido,
         año == 2019,
         género == "total",
         tipo == "dependencia",
         calculo == "total") %>%
  group_by(dependencia) %>%
  summarize(n = sum(valor, na.rm = T)) %>%
  pivot_wider(values_from = n, names_from = dependencia)

# g dependencia: rubro elegido ----
datos_sii$trabajadores %>%
  filter(rubro == rubro_elegido) %>%
  filter(tipo == "dependencia",
         calculo == "total",
         género == "total") %>%
  select(-dato, -tipo, -empresas, -calculo) %>%
  group_by(año, dependencia) %>%
  summarize(n = sum(valor, na.rm = T)) %>%
  ggplot(aes(año, n, col = dependencia)) +
  geom_line(show.legend = F) +
  scale_x_continuous(breaks = años_sii) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))

# dependencia: rubro elegido y comuna elegida ----
datos_sii$trabajadores %>%
  filter(comuna == comuna_elegida, #picker
         rubro == rubro_elegido,
         año == 2019) %>%
  filter(tipo == "dependencia",
         calculo == "total",
         género == "total") %>%
  select(-dato, -tipo, -empresas, -calculo) %>%
  pivot_wider(values_from = valor, names_from = dependencia)



# género: comunas ----
trabajadores_genero_comunas <- datos_sii$trabajadores %>%
  filter(año == 2019,
         género != "total",
         tipo == "dependencia",
         calculo == "total") %>%
  group_by(comuna, género) %>%
  summarize(n = sum(valor, na.rm = T)) %>%
  pivot_wider(values_from = n, names_from = género) %>%
  mutate(femenino_p = femenino/(femenino + masculino),
         masculino_p = masculino/(femenino + masculino))

# género: comuna elegida ----
#trabajadores rubro elegido por género
datos_sii$trabajadores %>%
  filter(comuna == comuna_elegida,
         año == 2019,
         género != "total",
         tipo == "dependencia",
         calculo == "total") %>%
  group_by(género) %>%
  summarize(n = sum(valor, na.rm = T)) %>%
  pivot_wider(values_from = n, names_from = género)



# género: rubro elegido ----
#trabajadores rubro elegido por género
datos_sii$trabajadores %>%
  filter(rubro == rubro_elegido,
         año == 2019,
         género != "total",
         tipo == "dependencia",
         calculo == "total") %>%
  group_by(género) %>%
  summarize(n = sum(valor, na.rm = T)) %>%
  pivot_wider(values_from = n, names_from = género)

#género: rubros ----
trabajadores_genero_rubros <- datos_sii$trabajadores %>%
  filter(año == 2019,
         género != "total",
         tipo == "dependencia",
         calculo == "total") %>%
  group_by(rubro, género) %>%
  summarize(n = sum(valor, na.rm = T)) %>%
  pivot_wider(values_from = n, names_from = género) %>%
  mutate(femenino_p = femenino/(femenino + masculino),
         masculino_p = masculino/(femenino + masculino))

#g género: rubro elegido ----
datos_sii$trabajadores %>%
  filter(rubro == rubro_elegido,
         género != "total",
         tipo == "dependencia",
         calculo == "total") %>%
  group_by(año, género) %>%
  summarize(n = sum(valor, na.rm = T)) %>%
  ggplot(aes(año, n, col = género)) +
  geom_line(show.legend = F) +
  scale_x_continuous(breaks = años_sii) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))



# género: rubro elegido y comuna elegida ----
datos_sii$trabajadores %>%
  filter(comuna == comuna_elegida, #picker
         rubro == rubro_elegido,
         año == 2019) %>%
  filter(tipo == "dependencia",
         calculo == "total",
         género != "total") %>%
  select(-dato, -tipo, -empresas, -calculo) %>%
  pivot_wider(values_from = valor, names_from = género) %>%
  group_by(año, comuna, rubro) %>%
  summarize(femenino = sum(femenino),
            masculino = sum(masculino))

#género: rubros y comunas ----
trabajadores_genero_rubros_comunas <- datos_sii$trabajadores %>%
  filter(año == 2019) %>%
  filter(tipo == "dependencia",
         calculo == "total",
         género != "total") %>%
  select(-dato, -tipo, -empresas, -calculo) %>%
  pivot_wider(values_from = valor, names_from = género) %>%
  group_by(comuna, rubro) %>%
  summarize(femenino = sum(femenino),
            masculino = sum(masculino)) %>%
  ungroup() %>%
  mutate(femenino_p = femenino/(femenino + masculino),
         masculino_p = masculino/(femenino + masculino))

#ponderados
#La columna “Trabajadores ponderados por meses trabajados” corresponde a la suma, por cada una de las empresas, de cada uno los trabajadores informados multiplicados por el número de meses trabajados y dividido por 12.
# datos_sii$trabajadores %>%
#   filter(comuna == comuna_elegida, #picker
#          rubro == rubro_elegido,
#          año == 2019) %>%
#   filter(tipo != "dependencia",
#          calculo == "total") %>%
#   select(dato)

#gráfico trabajadores
datos_sii$trabajadores %>%
  filter(comuna == comuna_elegida, #picker
         rubro == rubro_elegido) %>%
  filter(tipo == "dependencia",
         calculo == "total") %>%
  select(-dato, -tipo, -empresas, -calculo) %>%
  ggplot(aes(año, valor, col = género, linetype=dependencia)) +
  geom_line() +
  scale_x_continuous(breaks = años_sii) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))

#trabajdores por género
datos_sii$trabajadores %>%
  filter(comuna == comuna_elegida, #picker
         rubro == rubro_elegido) %>%
  filter(tipo == "dependencia",
         calculo == "total") %>%
  group_by(año, rubro, género) %>%
  summarize(valor = sum(valor, na.rm=T)) %>%
  ggplot(aes(año, valor, col = género)) +
  geom_line() +
  scale_x_continuous(breaks = años_sii) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))

#trabajdores por dependencia
datos_sii$trabajadores %>%
  filter(comuna == comuna_elegida, #picker
         rubro == rubro_elegido) %>%
  filter(tipo == "dependencia",
         calculo == "total") %>%
  group_by(año, rubro, dependencia) %>%
  summarize(valor = sum(valor, na.rm=T)) %>%
  ggplot(aes(año, valor, col = dependencia)) +
  geom_line() +
  scale_x_continuous(breaks = años_sii) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))

# (!) actividades ----

# (!) tamaño x comunas ----

#—----

#VENTAS ----

#rubros ----
datos_sii$ventas %>%
  filter(año == 2019) %>%
  group_by(rubro) %>%
  summarize(across(where(is.numeric), ~ sum(.x))) %>%
  arrange(ventas_anuales_uf)

#g rubros x comuna elegida ----
datos_sii$ventas %>%
  filter(comuna == comuna_elegida) %>%
  group_by(año, rubro) %>%
  summarize(across(where(is.numeric), ~ sum(.x))) %>%
  ggplot(aes(año, ventas_anuales/1000000, col = rubro)) +
  geom_line(show.legend = F) +
  scale_y_continuous(labels = function(x) paste(x, "M")) +
  scale_x_continuous(breaks = años_sii) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))






#rubro elegido ----
datos_sii$ventas %>%
  filter(rubro == rubro_elegido,
         año == 2019) %>%
  group_by(año, rubro) %>%
  summarize(across(where(is.numeric), ~ sum(.x)))

#rubro elegido x comunas  ----
datos_sii$ventas %>%
  filter(rubro == rubro_elegido,
         año == 2019) %>%
  group_by(año, rubro)

#g rubro elegido x comunas ----
datos_sii$ventas_act %>%
  filter(rubro == rubro_elegido) %>%
  filter(ventas_anuales_uf > 0) %>%
  group_by(año, comuna) %>%
  summarize(across(where(is.numeric), ~ sum(.x))) %>%
  ggplot(aes(año, ventas_anuales/1000000, col = comuna)) +
  geom_line(show.legend = F) +
  scale_x_continuous(breaks = años_sii) +
  scale_y_continuous(labels = function(x) paste(x, "M")) +
  scale_y_log10() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))


#subrubro elegido ----
datos_sii$ventas_act %>%
  filter(subrubro == subrubro_elegido,
         año == 2019) %>%
  group_by(año, subrubro) %>%
  summarize(across(where(is.numeric), ~ sum(.x)))

#subrubro elegido x comunas ----
datos_sii$ventas_act %>%
  filter(subrubro == subrubro_elegido,
         año == 2019)


#subrubros ----
datos_sii$ventas_act %>%
  filter(rubro == rubro_elegido,
         año == 2019) %>%
  group_by(año, subrubro) %>%
  summarize(across(where(is.numeric), ~ sum(.x)))

#g subrubros x comuna elegida ----
datos_sii$ventas_act %>%
  filter(rubro == rubro_elegido,
         comuna == comuna_elegida) %>%
  filter(ventas_anuales_uf > 0) %>%
  group_by(año, subrubro) %>%
  summarize(across(where(is.numeric), ~ sum(.x))) %>%
  ggplot(aes(año, ventas_anuales/1000000, col = subrubro)) +
  geom_line(show.legend = F) +
  scale_y_continuous(labels = function(x) paste(x, "M")) +
  scale_x_continuous(breaks = años_sii) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))


#actividades ----
datos_sii$ventas_act %>%
  filter(subrubro == subrubro_elegido,
         año == 2019) %>%
  mutate(actividad = replace_na(actividad, "Otras actividades del rubro")) %>%
  group_by(año, actividad) %>%
  summarize(across(where(is.numeric), ~ sum(.x)))

#actividad x comuna elegida ----
datos_sii$ventas_act %>%
  filter(subrubro == subrubro_elegido,
         comuna == comuna_elegida,
         año == 2019) %>%
  mutate(actividad = replace_na(actividad, "Otras actividades del rubro")) %>%
  group_by(comuna, actividad) %>%
  summarize(across(where(is.numeric), ~ sum(.x)))

# g ventas y empresas
datos_sii$ventas %>%
  filter(comuna == comuna_elegida, #selector
         rubro == rubro_elegido) %>%
  ggplot(aes(año, ventas_anuales)) +
  geom_line() +
  geom_col(aes(y=empresas*50000), alpha=0.2) +
  scale_y_continuous(labels = function(x) paste(x/1000000, "M"))

#—----

#RENTA ----

#rubros ----
datos_sii$renta %>%
  filter(año == 2019) %>%
  filter(género == "total",
         calculo == "total") %>%
  group_by(rubro) %>%
  summarize(valor = sum(valor, na.rm = T)) %>%
  arrange(desc(valor))
  

#rubro elegido ----
datos_sii$renta %>%
  filter(rubro == rubro_elegido,
         año == 2019) %>%
  filter(género == "total",
         calculo == "total") %>%
  group_by(rubro) %>%
  summarize(valor = sum(valor, na.rm = T)) %>%
  arrange(desc(valor))

#rubro elegido x comuna elegida ----
datos_sii$renta %>%
  filter(rubro == rubro_elegido,
         comuna == comuna_elegida,
         año == 2019) %>%
  filter(género == "total",
         calculo == "total") %>%
  group_by(comuna, rubro) %>%
  summarize(valor = sum(valor, na.rm = T)) %>%
  arrange(desc(valor))

#rubro elegido x comunas ----
datos_sii$renta %>%
  filter(rubro == rubro_elegido,
         año == 2019) %>%
  filter(género == "total",
         calculo == "total") %>%
  group_by(comuna, rubro) %>%
  summarize(valor = sum(valor, na.rm = T)) %>%
  arrange(desc(valor))

#rubro elegido x género ----
datos_sii$renta %>%
  filter(rubro == rubro_elegido,
         año == 2019) %>%
  filter(género != "total",
         calculo == "total") %>%
  group_by(género, rubro) %>%
  summarize(valor = sum(valor, na.rm = T)) %>%
  arrange(desc(valor))

#g evolución rubro elegido x género ----
datos_sii$renta %>%
  filter(rubro == rubro_elegido) %>%
  filter(género != "total",
         calculo == "total") %>%
  group_by(año, género, rubro) %>%
  summarize(valor = sum(valor, na.rm = T)) %>%
  ggplot(aes(año, valor/1000000, col = género)) +
  geom_line(show.legend = F) +
  scale_y_continuous(labels = function(x) paste(x, "M")) +
  scale_x_continuous(breaks = años_sii) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))
#habría que comparar con cantidad de trabajadores

# rubro elegido x comuna elegida x género ----
datos_sii$renta %>%
  filter(comuna == comuna_elegida, #selector
         rubro == rubro_elegido) %>%
  filter(calculo == "total") %>%
  ggplot(aes(año, valor, col=género, fill=género)) +
  geom_line() +
  scale_y_continuous(labels = function(x) paste(x/1000000, "M")) +
  scale_x_continuous(breaks = años_sii) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))


#subrubros ----
datos_sii$renta_act %>%
  filter(rubro == rubro_elegido,
         año == 2019) %>%
  filter(género == "total",
         calculo == "total") %>%
  group_by(subrubro) %>%
  summarize(valor = sum(valor, na.rm = T)) %>%
  arrange(desc(valor))


#subrubro elegido ----
datos_sii$renta_act %>%
  filter(subrubro == subrubro_elegido,
         año == 2019) %>%
  filter(género == "total",
         calculo == "total") %>%
  group_by(subrubro) %>%
  summarize(valor = sum(valor, na.rm = T)) %>%
  arrange(desc(valor))

#subrubro elegido x comuna elegida----
datos_sii$renta_act %>%
  filter(subrubro == subrubro_elegido,
         comuna == comuna_elegida,
         año == 2019) %>%
  filter(género == "total",
         calculo == "total") %>%
  group_by(comuna, subrubro) %>%
  summarize(valor = sum(valor, na.rm = T)) %>%
  arrange(desc(valor))


#g subrubros x comuna elegida ----
datos_sii$renta_act %>%
  filter(rubro == rubro_elegido,
         comuna == comuna_elegida) %>%
  filter(género == "total",
         calculo == "total") %>%
  group_by(año, comuna, subrubro) %>%
  summarize(valor = sum(valor, na.rm = T)) %>%
  filter(valor > 0) %>%
  ggplot(aes(año, valor/1000000, col = subrubro)) +
  geom_line(show.legend = F) +
  scale_y_continuous(labels = function(x) paste(x, "M")) +
  scale_x_continuous(breaks = años_sii) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))


#subrubro elegido x comunas ----
datos_sii$renta_act %>%
  filter(subrubro == subrubro_elegido,
         año == 2019) %>%
  filter(género == "total",
         calculo == "total") %>%
  group_by(comuna, subrubro) %>%
  summarize(valor = sum(valor, na.rm = T)) %>%
  arrange(desc(valor))

#subrubro elegido x género ----
datos_sii$renta_act %>%
  filter(subrubro == subrubro_elegido,
         año == 2019) %>%
  filter(género != "total",
         calculo == "total") %>%
  group_by(género, subrubro) %>%
  summarize(valor = sum(valor, na.rm = T)) %>%
  arrange(desc(valor))


#actividades ----
datos_sii$renta_act %>%
  filter(subrubro == subrubro_elegido,
         año == 2019) %>%
  filter(género == "total",
         calculo == "total") %>%
  group_by(actividad) %>%
  summarize(valor = sum(valor, na.rm = T)) %>%
  arrange(desc(valor))

#actividades x comuna elegida ----
datos_sii$renta_act %>%
  filter(subrubro == subrubro_elegido,
         comuna == comuna_elegida,
         año == 2019) %>%
  filter(género == "total",
         calculo == "total") %>%
  group_by(comuna, actividad) %>%
  summarize(valor = sum(valor, na.rm = T)) %>%
  arrange(desc(valor))

#g actividades x comuna elegida ----
datos_sii$renta_act %>%
  filter(subrubro == subrubro_elegido,
         comuna == comuna_elegida) %>%
  filter(género == "total",
         calculo == "total") %>%
  group_by(año, comuna, actividad) %>%
  summarize(valor = sum(valor, na.rm = T)) %>%
  filter(valor > 0) %>%
  ggplot(aes(año, valor/1000000, col = actividad)) +
  geom_line(show.legend = F) +
  scale_y_continuous(labels = function(x) paste(x, "M")) +
  scale_x_continuous(breaks = años_sii) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))



#barras horizontales de porcentaje, por ejemplo de género o dependencia

#idea de gráfico de 10 pelotitas o símbolos
#transformar porcentajes en 10 filas,
#asignar cada fila a un grupo, y colorear en base a eso

#—----
#TRAMOS ----

#tramos comuna  ----
tramos_comuna <- datos_sii$tramos_comuna %>%
  filter(año == 2019) %>%
  group_by(comuna) %>%
  mutate(porcentaje = empresas/sum(empresas)) %>%
  select(comuna, tramo, empresas, porcentaje)

#tramos región ----
tramos_region <- datos_sii$tramos_comuna %>%
  group_by(tramo) %>%
  summarize(empresas = sum(empresas)) %>%
  mutate(porcentaje = empresas/sum(empresas))

#tramos rubro ----
tramos_rubro <- datos_sii$tramos_rubro %>%
  filter(año == 2019) %>%
  group_by(rubro) %>%
  mutate(porcentaje = empresas/sum(empresas)) %>%
  select(rubro, tramo, empresas, porcentaje)

#—----

#COMPILAR ----
#E1 empresas region rubro elegido:            empresas_rubros
#E2 empresas rubro elegido comuna elegida:    empresas_comunas
#E3 empresas subrubros:                       empresas_subrubros
#P1 porcetaje region que son pequeñas
#P2 % de comuna e y rubro e que son pequeñas
#T1 trabajadores rubro e:                     trabajadores_rubros
#T2 trabajadores comuna e rubro e
#T3 trabajadores comuna e subrubro e
#T4 trabajadores rubro e mujeres %
#T5 trabajadores rubro e honorarios %
#V1 ventas rubro e region
#V2 ventas rubro e comuna e
#V3 ventas rubro e comuna e % del total de ventas


datos <- list("empresas_año_rubro_comuna" = empresas_año_rubro_comuna, #datos_sii$empresas
              "empresas_año_rubro_region" = empresas_año_rubro_region,
              ##
              "empresas_rubros" = empresas_rubros,
              "empresas_comunas" = empresas_comunas,
              "empresas_subrubros" = empresas_subrubros,
              ##
              "trabajadores_rubros" = trabajadores_rubros,
              "trabajadores_comuna_rubro" = trabajadores_comuna_rubro,
              "trabajadores_comuna_subrubro" = trabajadores_comuna_subrubro,
              "trabajadores_genero_comunas" = trabajadores_genero_comunas,
              "trabajadores_genero_rubros" = trabajadores_genero_rubros,
              "trabajadores_genero_rubros_comunas" = trabajadores_genero_rubros_comunas,
              ##
              "tramos_comuna" = tramos_comuna,
              "tramos_region" = tramos_region,
              "tramos_rubro" = tramos_rubro,
              ##
              "crecimiento_subrubros_region" = crecimiento_subrubros_region)

save(datos, file = "dataemprende_datos/datos_precalculados.rdata")
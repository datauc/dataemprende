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

source("variables.r")

comuna_elegida <- comunas_sii[2]
rubro_elegido <- rubros_sii[4]

#filtrar subrubros a partir de rubro
subrubros_filtrados <- datos_sii$ventas_act %>%
  filter(rubro == rubro_elegido) %>%
  #filtrar subrubros sin ventas anuales
  filter(ventas_anuales_uf > 0) %>%
  select(subrubro) %>%
  distinct() %>%
  pull()

subrubro_elegido <- subrubros_filtrados[2]

#filtrar actividades a partir de subrubros
actividad_filtradas <- datos_sii$ventas_act %>%
  filter(subrubro == subrubro_elegido) %>%
  #filtrar subrubros sin ventas anuales
  filter(ventas_anuales_uf > 0) %>%
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
datos_sii$empresas %>%
  filter(comuna == comuna_elegida) %>% #picker
  filter(rubro == rubro_elegido) %>% #picker
  ggplot(aes(año, empresas, col=rubro)) +
  geom_line(show.legend=F) +
  scale_x_continuous(breaks = años_sii) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))

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


datos <- list("empresas_rubros" = empresas_rubros,
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
              "tramos_rubro" = tramos_rubro)

save(datos, file = "datos_precalculados.rdata")

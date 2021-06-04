library(ggforce)

#evolución dependencia
trabajadores_año_dependencia_subrubro <- datos_sii$trabajadores_act %>%
  filter(#rubro == rubro_elegido,
         #año == 2019,
         género == "total",
         tipo == "dependencia",
         calculo == "total") %>%
  group_by(rubro, subrubro, año, dependencia) %>%
  summarize(trabajadores = sum(valor, na.rm = T))

#ejemplo de gráfico de línea
datos_sii$empresas %>%
  filter(comuna == comunas_sii[5]) %>% #picker
  filter(rubro == rubros_sii[3]) %>% #picker
  graficar_lineas_degradado()


#función para gráfico que tenga dos líneas
datos$trabajadores_año_genero_rubro %>%
  filter(rubro == rubros_sii[11]) %>%
  graficar_lineas_comparadas_degradado(variable_y = "trabajadores",
                                       numero_largo=1.5,
                                       texto_y = "Trabajadores por género",
                                       variable = "rubro",
                                       variable_categorica_elegida = "género")

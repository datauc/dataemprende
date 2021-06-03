library(ggforce)

#evolución dependencia
datos_sii$trabajadores_act %>%
  filter(#rubro == rubro_elegido,
         #año == 2019,
         género == "total",
         tipo == "dependencia",
         calculo == "total") %>%
  group_by(rubro, subrubro, año, dependencia) %>%
  summarize(trabajadores = sum(valor, na.rm = T))



datos$trabajadores_año_genero_rubro %>%
  filter(rubro == rubros_sii[3],
         género == "femenino") %>% #input
  graficar_lineas_degradado(variable_y_elegida = "trabajadores", numero_largo = 1.5)

prueba = "Mujer"

resultado <- switch(prueba,
       "Mujer" = 1,
         "Hombre" = 0)

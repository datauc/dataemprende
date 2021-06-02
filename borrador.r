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



#evolucion género
datos_sii$trabajadores_act %>%
  filter(#rubro == rubro_elegido,
    #año == 2019,
    género != "total",
    tipo == "dependencia",
    calculo == "total") %>%
  group_by(rubro, subrubro, año, género) %>%
  summarize(trabajadores = sum(valor, na.rm = T))

#luego same but subrubro



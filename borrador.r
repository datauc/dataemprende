
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




#—----
#rubros con mayores ventas anuales promedio en la comuna elegida
datos$ventas_año_subrubro_comuna %>%
  filter(año == 2019) %>%
  filter(comuna == comuna_elegida) %>%
  group_by(comuna, rubro) %>%
  summarize(ventas_anuales = sum(ventas_anuales, na.rm=T),
            empresas = sum(empresas)) %>%
  mutate(ventas_promedio = ventas_anuales/empresas,
         ventas_promedio = round(ventas_promedio/1000000, 2)) %>%
  graficar_barras_horizontales(variable_categorica = "rubro", 
                               variable_numerica = "ventas_promedio", 
                               slice=6, str_wrap=30, str_trunc=50)

#rubros con mayores ventas anuales promedio en la región
datos$ventas_año_subrubro_comuna %>%
  filter(año == 2019) %>%
  #filter(comuna == comuna_elegida) %>%
  group_by(rubro) %>%
  summarize(ventas_anuales = sum(ventas_anuales, na.rm=T),
            empresas = sum(empresas)) %>%
  mutate(ventas_promedio = ventas_anuales/empresas,
         ventas_promedio = round(ventas_promedio/1000000, 1)) %>%
  graficar_barras_horizontales(variable_categorica = "rubro", 
                               variable_numerica = "ventas_promedio", 
                               slice=6, str_wrap=30, str_trunc=50)


#subrubros con mayores ventas anuales promedio en la comuna elegida
datos$ventas_año_subrubro_comuna %>%
  filter(año == 2019) %>%
  filter(comuna == comuna_elegida) %>%
  filter(rubro == rubro_elegido) %>%
  group_by(comuna, subrubro) %>%
  summarize(ventas_anuales = sum(ventas_anuales, na.rm=T),
            empresas = sum(empresas)) %>%
  mutate(ventas_promedio = ventas_anuales/empresas,
         ventas_promedio = round(ventas_promedio/1000000, 2)) %>%
  graficar_barras_horizontales(variable_categorica = "subrubro", 
                               variable_numerica = "ventas_promedio", 
                               slice=6, str_wrap=30, str_trunc=50)

#subrubros con mayores ventas anuales promedio en la región
datos$ventas_año_subrubro_comuna %>%
  filter(año == 2019) %>%
  #filter(comuna == comuna_elegida) %>%
  filter(rubro == rubro_elegido) %>%
  group_by(subrubro) %>%
  summarize(ventas_anuales = sum(ventas_anuales, na.rm=T),
            empresas = sum(empresas)) %>%
  mutate(ventas_promedio = ventas_anuales/empresas,
         ventas_promedio = round(ventas_promedio/1000000, 1)) %>%
  graficar_barras_horizontales(variable_categorica = "subrubro", 
                               variable_numerica = "ventas_promedio", 
                               slice=6, str_wrap=30, str_trunc=50)

#ranking de rubros/subrubros
  #rubros top ventas comuna elegida
  #subrubros top ventas comuna elegida
  #selector: rubro/subrubro

#ranking de comunas
  #rubro elegido top ventas comunas
  #subrubro elegido top ventas comunas
  #selector: rubro/subrubro

datos$trabajadores_año_subrubro_comuna %>%
  filter(año == 2019) %>%
  filter(comuna == comuna_elegida) %>%
  arrange(desc(trabajadores))

#ranking de rubros/subrubros
#rubros top trabajadores comuna elegida
#subrubros top trabajadores comuna elegida
#selector: rubro/subrubro

#ranking de comunas
#rubro elegido top ventas comunas
#subrubro elegido top ventas comunas
#selector: rubro/subrubror



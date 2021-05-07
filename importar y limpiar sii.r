####
####
####
#este archivo importa las bases del sii (alternativamente las descarga) y limpia los datos
#también se genera la lista datos_sii.rdata que junta las bases en un solo objeto
####
####
####

library(tidyverse)

#obtener valor de la UF mediante webscrapping
#source("~/Scrapping/valor UF scrapping.r")

#obtener UF 2019
#valores_uf_2019 <- calcular_uf_anual(2019)

#uf2019 <- mean(valores_uf_2019$valor)
#27851
uf2019 <- 27851

# importar tablas
# url <- "https://www.sii.cl/sobre_el_sii/estadisticas/empresas/202010_PUB_COMU_RUBR.xlsb"
# 
# download.file(url = url,
#               destfile = "~/SII/Datos/202010_PUB_COMU_RUBR.xlsx")
# 
# sii_comu_rubr <- readxlsb::read_xlsb("~/SII/Datos/202010_PUB_COMU_RUBR.xlsb", range = "A1:Z999", sheet = 1)
# 
# glimpse(sii_comu_rubr)



#descargar todas las tablas ----
# download.file(url = "https://www.sii.cl/sobre_el_sii/estadisticas/empresas/Empresas_202010.zip",
#               destfile = "~/SII/Datos/Empresas_202010.zip")
# 
# unzip("~/SII/Datos/Empresas_202010.zip", 
#       exdir = "~/SII/Datos/SII oct 2020")


#importar ----
#sii_reg_rubr <- readr::read_tsv(file = "~/SII/Datos/SII oct 2020/PUB_REG_RUBR.txt", local = locale(encoding = "latin1"))
#sii_reg_subr <- readr::read_tsv(file = "~/SII/Datos/SII oct 2020/PUB_REG_SUBR.txt", local = locale(encoding = "latin1"))

sii_comu_rubr <- readr::read_tsv(file = "~/SII/Datos/SII oct 2020/PUB_COMU_RUBR.txt", local = locale(encoding = "latin1")) %>%
  mutate(`Rubro economico` = str_remove_all(`Rubro economico`, ". - "))

#sii_comu_subr <- readr::read_tsv(file = "~/SII/Datos/SII oct 2020/PUB_COMU_SUBR.txt", local = locale(encoding = "latin1"))

sii_comu_act <- readr::read_tsv(file = "~/SII/Datos/SII oct 2020/PUB_COMU_ACT.txt", local = locale(encoding = "latin1")) %>%
  mutate(`Rubro economico` = str_remove_all(`Rubro economico`, ". - "),
         `Subrubro economico` = str_remove_all(`Subrubro economico`, ".* - "))

#sii_tramo_rubr <- readr::read_tsv(file = "~/SII/Datos/SII oct 2020/PUB_TRAM_RUBR.txt", local = locale(encoding = "latin1"))


# glimpse(sii_reg_rubr)
# glimpse(sii_reg_subr)
# 
# glimpse(sii_comu_rubr)
# glimpse(sii_comu_subr)


#empresas ----
#permite sacar cantidad de empresas competidoras
sii_empresas <- sii_comu_rubr %>% 
  filter(`Region del domicilio o casa matriz` == "Región de Tarapacá") %>% 
  group_by(`Rubro economico`, `Comuna del domicilio o casa matriz`, `Año Comercial`) %>%
  summarize(n = sum(`Número de empresas`)) %>%
  ungroup() %>%
  pivot_wider(names_from = `Comuna del domicilio o casa matriz`, values_from = n) %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0))) %>%
  arrange(desc(Iquique)) %>%
  rename(rubro=1, año=2) %>%
  pivot_longer(cols = 3:length(.), names_to="comuna", values_to = "empresas") %>%
  print()

#base completa
sii_empresas_act <- sii_comu_act %>% 
  filter(`Region del domicilio o casa matriz` == "Región de Tarapacá") %>% 
  group_by(`Rubro economico`, `Subrubro economico`, `Actividad economica`, 
           `Comuna del domicilio o casa matriz`, `Año Comercial`) %>%
  summarize(n = sum(`Número de empresas`)) %>%
  ungroup() %>%
  pivot_wider(names_from = `Comuna del domicilio o casa matriz`, values_from = n) %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0))) %>%
  arrange(desc(Iquique)) %>%
  rename(rubro=1, subrubro=2, actividad=3, año=4) %>%
  filter(!is.na(rubro)) %>%
  pivot_longer(cols = 5:length(.), names_to="comuna", values_to = "empresas") %>%
  print()

sii_empresas_act

#ventas ----
sii_ventas <- sii_comu_rubr %>%
  #filtrar región y año
  filter(`Region del domicilio o casa matriz` == "Región de Tarapacá") %>%
  mutate(`Ventas anuales en UF` = readr::parse_number(`Ventas anuales en UF`)) %>%
  #dividir por cantidad de empresas
  mutate(`Ventas anuales en UF` = `Ventas anuales en UF` / `Número de empresas`) %>%
  #convertir uf a pesos
  mutate(`Ventas anuales en pesos` = `Ventas anuales en UF` * uf2019) %>%
  #convertir a meses
  mutate(`Ventas mensuales en pesos` = `Ventas anuales en pesos`/12) %>%
  #reemplazar missing
  select(1:6, starts_with("Venta"),
         -starts_with("Provincia"), -starts_with("Región")) %>%
  mutate(across(starts_with("Venta"), ~ replace_na(.x, 0))) %>%
  select(-3) %>%
  rename(año=1,
         comuna=2,
         rubro=3,
         empresas=4) %>% 
  rename(ventas_anuales_uf = 5,
         ventas_anuales = 6,
         ventas_mensuales = 7) %>%
  print()


#base completa
sii_ventas_act <- sii_comu_act %>%
  #filtrar región y año
  filter(`Region del domicilio o casa matriz` == "Región de Tarapacá") %>%
  mutate(`Ventas anuales en UF` = readr::parse_number(`Ventas anuales en UF`)) %>%
  #dividir por cantidad de empresas
  mutate(`Ventas anuales en UF` = `Ventas anuales en UF` / `Número de empresas`) %>%
  #convertir uf a pesos
  mutate(`Ventas anuales en pesos` = `Ventas anuales en UF` * uf2019) %>%
  #convertir a meses
  mutate(`Ventas mensuales en pesos` = `Ventas anuales en pesos`/12) %>%
  #reemplazar missing
  select(1:8, starts_with("Venta"),
         -starts_with("Provincia"), -starts_with("Región")) %>%
  mutate(across(starts_with("Venta"), ~ replace_na(.x, 0))) %>%
  select(-3) %>% #region
  rename(año=1,
         comuna=2,
         actividad=3,
         subrubro=4,
         rubro=5,
         empresas=6) %>% 
  rename(ventas_anuales_uf = 7,
         ventas_anuales = 8,
         ventas_mensuales = 9) %>%
  print()


#renta ----
sii_renta <- sii_comu_rubr %>%
  #filtrar región y año
  filter(`Region del domicilio o casa matriz` == "Región de Tarapacá") %>%
  select(1:6, starts_with("Renta")) %>%
  mutate(across(starts_with("Renta"), ~ readr::parse_number(.x))) %>%
  #dividir datos por cantidad de empresas para obtener promedios de empresas
  mutate(across(starts_with("Renta"),
                list("promedio" = ~ .x/`Número de empresas`), #poner sufijo a las columnas nuevas
                .names = "{col} {fn}")) %>% #formato del nombre nuevo
  #convertir uf a pesos
  mutate(across(starts_with("Renta"), ~ .x * uf2019)) %>%
  #mutate(`Rubro economico` = str_trunc(`Rubro economico`, 20)) %>%
  #pivotar a formato long
  pivot_longer(cols = 7:length(.), names_to = "dato", values_to = "valor") %>%
  #sacar columnas innecesarias
  select(-3, -4) %>%
  mutate(género = case_when(str_detect(dato, "femenino") ~ "femenino",
                            str_detect(dato, "masculino") ~ "masculino",
                            TRUE ~ "total")) %>%
  mutate(calculo = case_when(str_detect(dato, "promedio") ~ "promedio",
                             TRUE ~ "total")) %>%
  rename(año = 1,
         comuna = 2,
         rubro = 3,
         empresas = 4) %>%
  mutate(mensual = valor / 12) %>%
  print()

#intentar dividir renta neta por cantidad de trabajadores
sii_renta %>%
  filter(comuna == "Iquique",
         calculo == "promedio") %>%
  select(-dato) %>%
  left_join(sii_trabajadores %>%
              filter(comuna == "Iquique",
                     rubro == "Enseñanza") %>%
              filter(tipo == "dependencia") %>%
              filter(género == "total") %>%
              filter(calculo == "total") %>%
              group_by(comuna, rubro) %>%
              summarize(trabajadores = sum(valor))) %>%
  mutate(valor_trabajador = valor / trabajadores) %>% 
  print(n=300)


sii_renta_act <- sii_comu_act %>%
  #filtrar región y año
  filter(`Region del domicilio o casa matriz` == "Región de Tarapacá") %>%
  select(1:8, starts_with("Renta")) %>%
  mutate(across(starts_with("Renta"), ~ readr::parse_number(.x))) %>%
  #dividir datos por cantidad de empresas para obtener promedios de empresas
  mutate(across(starts_with("Renta"),
                list("promedio" = ~ .x/`Número de empresas`), #poner sufijo a las columnas nuevas
                .names = "{col} {fn}")) %>% #formato del nombre nuevo
  #convertir uf a pesos
  mutate(across(starts_with("Renta"), ~ .x * uf2019)) %>%
  #mutate(`Rubro economico` = str_trunc(`Rubro economico`, 20)) %>%
  select(-3, -4) %>%
  #pivotar a formato long
  pivot_longer(cols = 7:length(.), names_to = "dato", values_to = "valor") %>%
  #sacar columnas innecesarias
  mutate(género = case_when(str_detect(dato, "femenino") ~ "femenino",
                            str_detect(dato, "masculino") ~ "masculino",
                            TRUE ~ "total")) %>%
  mutate(calculo = case_when(str_detect(dato, "promedio") ~ "promedio",
                             TRUE ~ "total")) %>%
  rename(año = 1,
         comuna = 2,
         actividad = 3,
         subrubro = 4,
         rubro = 5,
         empresas = 6) %>%
  mutate(mensual = valor / 12) %>%
  print()


#trabajadores ----
sii_trabajadores <- sii_comu_rubr %>%
  #filtro inicial de región y año
  filter(`Region del domicilio o casa matriz` == "Región de Tarapacá") %>%
  #seleccionar columnas relevantes
  select(1:6, starts_with("Número de trabajadores"), starts_with("Trabajadores")) %>%
  #dividir datos por cantidad de empresas para obtener promedios de empresas
  mutate(across(c(starts_with("Número de trabajadores"), starts_with("Trabajadores")), 
                list("promedio" = ~ .x/`Número de empresas`), #poner sufijo a las columnas nuevas
                .names = "{col} {fn}")) %>% #formato del nombre nuevo
  #pivotar a formato long
  pivot_longer(cols = 7:length(.), names_to = "dato", values_to = "valor") %>%
  #sacar columnas innecesarias
  select(-3, -4) %>%
  #filter(`Comuna del domicilio o casa matriz` == "Huara") %>%
  #select(-3) %>%
  mutate(tipo = case_when(str_detect(dato, regex("ponderados", ignore_case = T)) ~ "ponderados",
                          str_detect(dato, regex("honorarios|dependientes", ignore_case = T)) ~ "dependencia",
                          str_detect(dato, regex("género", ignore_case = T)) ~ "género",
                          TRUE ~ "otros")) %>%
  mutate(género = case_when(str_detect(dato, "femenino") ~ "femenino",
                            str_detect(dato, "masculino") ~ "masculino",
                            TRUE ~ "total")) %>%
  mutate(dependencia = case_when(str_detect(dato, "honorario") ~ "honorarios",
                                 str_detect(dato, "dependiente") ~ "dependientes")) %>%
  mutate(calculo = case_when(str_detect(dato, "promedio") ~ "promedio",
                             TRUE ~ "total")) %>%
  rename(año = 1,
         comuna = 2,
         rubro = 3,
         empresas = 4) %>%
  print()

sii_trabajadores %>%
  filter(comuna == "Alto Hospicio",
         rubro == "Comercio al por mayor y al por menor; reparación de vehículos automotores y motocicletas",
         #tipo == "dependencia",
         dependencia == "dependientes") %>%
  select(-rubro)
#género == "masculino")

#comparar con fuerza de trabajo de la comuna
#los promedios son inútiles porque hay demasiadas empresas sin trabajadores

# #subrubro
# sii_trabajadores_subr <- sii_comu_subr %>%
#   #filtro inicial de región y año
#   filter(`Region del domicilio o casa matriz` == "Región de Tarapacá") %>%
#   #seleccionar columnas relevantes
#   select(1:7, starts_with("Número de trabajadores"), starts_with("Trabajadores")) %>%
#   #dividir datos por cantidad de empresas para obtener promedios de empresas
#   mutate(across(c(starts_with("Número de trabajadores"), starts_with("Trabajadores")), 
#                 list("promedio" = ~ .x/`Número de empresas`), #poner sufijo a las columnas nuevas
#                 .names = "{col} {fn}")) %>% #formato del nombre nuevo
#   #pivotar a formato long
#   pivot_longer(cols = 8:length(.), names_to = "dato", values_to = "valor") %>%
#   #sacar columnas innecesarias
#   select(-3, -4) %>%
#   #filter(`Comuna del domicilio o casa matriz` == "Huara") %>%
#   #select(-3) %>%
#   #mutate(`Rubro economico` = str_trunc(`Rubro economico`, 20)) %>%
#   mutate(tipo = case_when(str_detect(dato, regex("ponderados", ignore_case = T)) ~ "ponderados",
#                           str_detect(dato, regex("honorarios|dependientes", ignore_case = T)) ~ "dependencia",
#                           str_detect(dato, regex("género", ignore_case = T)) ~ "género",
#                           TRUE ~ "otros")) %>%
#   mutate(género = case_when(str_detect(dato, "femenino") ~ "femenino",
#                             str_detect(dato, "masculino") ~ "masculino",
#                             TRUE ~ "total")) %>%
#   mutate(dependencia = case_when(str_detect(dato, "honorario") ~ "honorarios",
#                                  str_detect(dato, "dependiente") ~ "dependientes")) %>%
#   mutate(calculo = case_when(str_detect(dato, "promedio") ~ "promedio",
#                              TRUE ~ "total")) %>%
#   rename(año = 1,
#          comuna = 2,
#          subrubro = 3,
#          rubro = 4,
#          empresas = 5) %>%
#   print()

sii_trabajadores_act <- sii_comu_act %>%
  #filtro inicial de región y año
  filter(`Region del domicilio o casa matriz` == "Región de Tarapacá") %>%
  #seleccionar columnas relevantes
  select(1:8, starts_with("Número de trabajadores"), starts_with("Trabajadores")) %>%
  #dividir datos por cantidad de empresas para obtener promedios de empresas
  mutate(across(c(starts_with("Número de trabajadores"), starts_with("Trabajadores")), 
                list("promedio" = ~ .x/`Número de empresas`), #poner sufijo a las columnas nuevas
                .names = "{col} {fn}")) %>% #formato del nombre nuevo
  #sacar columnas innecesarias
  select(-3, -4) %>%
  #pivotar a formato long
  pivot_longer(cols = 7:length(.), names_to = "dato", values_to = "valor") %>%
  #filter(`Comuna del domicilio o casa matriz` == "Huara") %>%
  #select(-3) %>%
  #mutate(`Rubro economico` = str_trunc(`Rubro economico`, 20)) %>%
  mutate(tipo = case_when(str_detect(dato, regex("ponderados", ignore_case = T)) ~ "ponderados",
                          str_detect(dato, regex("honorarios|dependientes", ignore_case = T)) ~ "dependencia",
                          str_detect(dato, regex("género", ignore_case = T)) ~ "género",
                          TRUE ~ "otros")) %>%
  mutate(género = case_when(str_detect(dato, "femenino") ~ "femenino",
                            str_detect(dato, "masculino") ~ "masculino",
                            TRUE ~ "total")) %>%
  mutate(dependencia = case_when(str_detect(dato, "honorario") ~ "honorarios",
                                 str_detect(dato, "dependiente") ~ "dependientes")) %>%
  mutate(calculo = case_when(str_detect(dato, "promedio") ~ "promedio",
                             TRUE ~ "total")) %>%
  rename(año = 1,
         comuna = 2,
         actividad = 3,
         subrubro = 4,
         rubro = 5,
         empresas = 6) %>%
  print()



#—----
# revisar ----
# sii_empresas
# sii_ventas
# sii_trabajadores
# sii_renta

datos_sii <- list("empresas" = sii_empresas,
                  #"empresas_subr" = sii_empresas_subr,
                  "empresas_act" = sii_empresas_act,
                  ##
                  "ventas" = sii_ventas,
                  "ventas_act" = sii_ventas_act,
                  ##
                  "trabajadores" = sii_trabajadores,
                  #"trabajadores_subr" = sii_trabajadores_subr,
                  "trabajadores_act" = sii_trabajadores_act,
                  ##
                  "renta" = sii_renta,
                  "renta_act" = sii_renta_act)

save(datos_sii, file = "datos_sii_act.rdata")

#rubros con letra ----
#importar para hacer join con la base de puntos, porque las letras de rubros coinciden pero los nombres no

# rubros_sii_letra <- readr::read_tsv(file = "~/SII/Datos/SII oct 2020/PUB_COMU_RUBR.txt", local = locale(encoding = "latin1")) %>%
#   rename(rubro = `Rubro economico`) %>%
#   select(rubro) %>%
#   distinct() %>%
#   mutate(letra = stringr::str_extract(rubro, "."),
#          rubro = stringr::str_remove_all(rubro, ". - "))

#datapasta::tribble_paste(rubros_sii_letra)

rubros_sii_letra <- tibble::tribble(
  ~rubro, ~letra,
  "Actividades de organizaciones y órganos extraterritoriales",    "U",
  "Actividades artísticas, de entretenimiento y recreativas",    "R",
  "Actividades financieras y de seguros",    "K",
  "Actividades de servicios administrativos y de apoyo",    "N",
  "Actividades de alojamiento y de servicio de comidas",    "I",
  "Actividades profesionales, científicas y técnicas",    "M",
  "Industria manufacturera",    "C",
  "Explotación de minas y canteras",    "B",
  "Construcción",    "F",
  "Actividades inmobiliarias",    "L",
  "Actividades de atención de la salud humana y de asistencia social",    "Q",
  "Comercio al por mayor y al por menor; reparación de vehículos automotores y motocicletas",    "G",
  "Transporte y almacenamiento",    "H",
  "Suministro de agua; evacuación de aguas residuales, gestión de desechos y descontaminación",    "E",
  "Enseñanza",    "P",
  "Agricultura, ganadería, silvicultura y pesca",    "A",
  "Información y comunicaciones",    "J",
  "Actividades de los hogares como empleadores; actividades no diferenciadas de los hogares",    "T",
  "Otras actividades de servicios",    "S",
  "Sin información",    "S",
  "Administración pública y defensa; planes de seguridad social de afiliación obligatoria",    "O",
  "Suministro de electricidad, gas, vapor y aire acondicionado",    "D"
)

#—----
#crear coincidencia entre ciiu (en base de puntos) y subrubros del sii (6 digitos, donde los 2 primeros son el ciiu4

#subrubros sii con numero de 6 digitos ----
actividades_sii <- readr::read_tsv(file = "~/SII/Datos/SII oct 2020/PUB_COMU_ACT.txt", local = locale(encoding = "latin1")) %>%
  rename(actividad = `Actividad economica`,
         rubro = `Rubro economico`,
         subrubro = `Subrubro economico`) %>%
  select(actividad, subrubro, rubro) %>%
  distinct() %>%
  mutate(codigo_actividades = stringr::str_extract(actividad, "\\d+"),
         codigo_actividades = as.numeric(codigo_actividades),
         numero = stringr::str_extract(actividad, ".."),
         numero = as.numeric(numero)) %>%
  arrange(codigo_actividades)

#actividades ciiu de 2 digitos ----
#presente en la base de puntos de empresas
actividades_ciiu4 <- base_puntos_empresas_tarapaca %>%
  rename_all(~ tolower(.)) %>%
  select(division_ciiu4cl, glosa_division) %>%
  distinct() %>%
  mutate(division_ciiu4cl = readr::parse_number(division_ciiu4cl)) %>%
  arrange(division_ciiu4cl) %>%
  mutate(numero = division_ciiu4cl) %>%
  arrange(numero)

actividades_sii
subrubros_sii_numero

#combinar ciiu con actividades sii ----
cruce_ciiuu_sii <- actividades_ciiu4 %>%
  left_join(actividades_sii) %>%
  mutate(codigo_subrubro = stringr::str_extract(subrubro, "\\d+"),
         subrubro = stringr::str_remove(subrubro, "\\d+ - "),
         codigo_rubro = stringr::str_extract(subrubro, "."),
         rubro = stringr::str_remove(subrubro, ". - ")) %>%
  select(division_ciiu4cl, subrubro, glosa_division) %>%
  distinct() %>%
  print(n=30)

cruce_ciiuu_sii

save(cruce_ciiuu_sii, file = "dataemprende_datos/cruce_ciiuu_sii.rdata")




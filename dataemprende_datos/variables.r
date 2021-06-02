####
####
####
#archivo que genera vectores para la app
####
####
####


#selectores y variables ----

#anos_sii <- distinct(sii_empresas, año) %>% arrange(año) %>% pull()
#dput(anos_sii)

años_sii <- c(2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 
              2015, 2016, 2017, 2018, 2019)
anos_sii <- c(2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 
              2015, 2016, 2017, 2018, 2019)

#comunas_sii <- distinct(datos_sii$empresas, comuna) %>% pull()
#dput(comunas_sii)
comunas_sii <- c("Iquique", "Alto Hospicio", "Camiña", 
                 "Colchane", "Huara",  
                 "Pica", "Pozo Almonte")

comunas_sii2 <- c("Alto Hospicio", "Iquique", "Huara", "Pica", "Pozo Almonte")

com_lat = c(-20.270372, -19.995317, -20.238531, -20.487102, -20.258318)
com_lng = c(-70.098489, -69.770744,-70.135417,-69.324798,-69.782779)

#rubros_sii <- distinct(datos_sii$empresas, rubro) %>% pull()
#dput(rubros_sii)
rubros_sii <- c("Comercio al por mayor y al por menor; reparación de vehículos automotores y motocicletas", 
                "Transporte y almacenamiento", "Actividades de alojamiento y de servicio de comidas", 
                "Construcción", "Industria manufacturera", "Otras actividades de servicios", 
                "Actividades profesionales, científicas y técnicas", "Actividades de servicios administrativos y de apoyo", 
                "Actividades inmobiliarias", 
                #"Sin información", 
                "Actividades de atención de la salud humana y de asistencia social", 
                "Enseñanza", "Información y comunicaciones", "Agricultura, ganadería, silvicultura y pesca", 
                "Actividades financieras y de seguros", "Actividades artísticas, de entretenimiento y recreativas", 
                "Explotación de minas y canteras", "Suministro de agua; evacuación de aguas residuales, gestión de desechos y descontaminación", 
                "Suministro de electricidad, gas, vapor y aire acondicionado", 
                "Administración pública y defensa; planes de seguridad social de afiliación obligatoria", 
                "Actividades de organizaciones y órganos extraterritoriales")

#generar subrubros ----
# subrubros_sii <- datos_sii$empresas_act %>%
#   select(rubro, subrubro) %>%
#   arrange(desc(subrubro)) %>%
#   distinct()
#save(subrubros_sii, file = "subrubros.rdata")

# subrubros_sii <- datos$empresas_subrubros %>%
#   group_by(rubro) %>%
#   arrange(desc(n)) %>%
#   ungroup() %>%
#   #filter(rubro == "Construcción") %>%
#   select(rubro, subrubro) %>%
#   distinct()
# 
# subrubros_sii %>%
#   filter(rubro == "Construcción")
# 
# save(subrubros_sii, file = "dataemprende_datos/subrubros.rdata")

load("subrubros.rdata")

#cargar cruce entre ciiu y subrubros del sii para mapa de puntos de empresas
load("cruce_ciiuu_sii.rdata")
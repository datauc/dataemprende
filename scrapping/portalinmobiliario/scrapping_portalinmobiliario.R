library(dplyr)
library(rvest)
library(lubridate)
library(stringr)

#función para descargar ----
descargar_portalinmobiliario <- function(direccion, tipo = "departamentos", fecha = lubridate::today()) {
  ruta <- paste0("scrapping/portalinmobiliario/paginas/", fecha, "/")
  archivo <- paste("portal", fecha, "arriendo", tipo, ".html", sep = "_")
  #crear carpeta si es que no existe
  dir.create(ruta)
  #descargar archivo
  download.file(direccion, destfile = paste0(ruta, archivo))
  cat("Archivos descargados en", paste0(ruta, archivo), fill = T)
}


#scrapping manual ----
# session <- rvest::session(url_p) #para url
# session <- rvest::read_html(paste0("scrapping/portalinmobiliario/paginas/",
#                                    "2021-06-18", "/",
#                                    paste("portal", fecha_scrapping, 
#                                          "arriendo", "oficinas", ".html", sep = "_"))) #para archivo
# 
# #ver todos los nodos
# session %>% html_nodes("*") %>% html_attr("class") %>% unique()
# 
# #son 36 elementos
# s_precio <- session %>% html_nodes(".price-tag-amount") %>% html_text() #precio tal cual
# s_ubicacion <- session %>% html_nodes(".ui-search-item__location") %>% html_text() #ubicación
# s_subtitulo <- session %>% html_nodes(".ui-search-item__subtitle") %>% html_text() #subtítulo (todos iguales)
# s_datos <- session %>% html_nodes(".ui-search-item__group--attributes") %>% html_text() #metros y dormitorios
# 
# base_portal <- tibble::tibble(subtitulo = s_subtitulo,
#                               precio = s_precio %>% str_remove_all("\\.") %>% readr::parse_number(),
#                               divisa = s_precio %>% str_remove_all("\\.") %>% str_remove("\\d+"),
#                               ubicacion = s_ubicacion,
#                               direccion = s_ubicacion %>% str_remove(", \\w+$") %>% str_extract("^[^\\)]*\\,") %>% str_remove(",$") %>% str_remove(", Iquique, Chile"),
#                               zona = s_ubicacion %>% str_remove(", \\w+$") %>% str_remove("^[^\\)]*\\, ") %>% replace(str_detect(., "\\d"), NA),
#                               comuna = s_ubicacion %>% str_extract("\\w+$"),
#                               metros = s_datos %>% str_extract("\\d+"),
#                               dormitorios = s_datos %>% str_remove("\\d+ ...") %>% str_remove("\\w+$") %>% readr::parse_number(),
# ) %>% 
#   mutate(fecha_scrapping = lubridate::ymd(fecha_scrapping)) %>% 
#   mutate(direccion = ifelse(is.na(direccion), ubicacion, direccion),
#          zona = ifelse(is.na(zona), comuna, zona)) %>% 
#   mutate(across(c(subtitulo, zona, comuna), ~ as.factor(.x)))


#función scrapping ----
scrapping_portalinmobiliario <- function(fecha = lubridate::today()) {
  archivos_s <- list.files(paste0("scrapping/portalinmobiliario/paginas/", fecha))
  
  for (archivo in archivos_s) {
    
    #iniciar loop: inicializar lista
    if(archivo == min(archivos_s)) { 
      cat("inicializando lista", fill=T) 
      base_portal_list <- list() 
    }
    
    #cargar archivo
    cat(archivo, fill=T)
    session <- rvest::read_html(paste0("scrapping/portalinmobiliario/paginas/", fecha, "/", archivo))
    
    #scrapping
    s_precio <- session %>% html_nodes(".price-tag-amount") %>% html_text() #precio tal cual
    s_ubicacion <- session %>% html_nodes(".ui-search-item__location") %>% html_text() #ubicación
    s_subtitulo <- session %>% html_nodes(".ui-search-item__subtitle") %>% html_text() #subtítulo (todos iguales)
    s_datos <- session %>% html_nodes(".ui-search-item__group--attributes") %>% html_text() #metros y dormitorios
    
    #meta
    s_tipo <- archivo %>% str_remove("_.html") %>% str_remove("portal_.*_") #casas o departamentos, desde la ruta del archivo
    
    #unir
    resultado <- tibble::tibble(subtitulo = s_subtitulo,
                                tipo = s_tipo,
                                precio = s_precio %>% str_remove_all("\\.") %>% str_replace_all("\\,", "\\.") %>% readr::parse_number(),
                                divisa = s_precio %>% str_remove_all("\\.") %>% str_remove_all("\\,") %>% str_remove("\\d+"),
                                ubicacion = s_ubicacion,
                                direccion = s_ubicacion %>% str_remove(", \\w+$") %>% str_extract("^[^\\)]*\\,") %>% str_remove(",$") %>% str_remove(", Iquique, Chile"),
                                zona = s_ubicacion %>% str_remove(", \\w+$") %>% str_remove("^[^\\)]*\\, ") %>% replace(str_detect(., "\\d"), NA),
                                comuna = s_ubicacion %>% str_extract("\\w+$"),
                                metros = s_datos %>% str_extract("\\d+"),
                                dormitorios = s_datos %>% str_remove("\\d+ ...") %>% str_remove("\\w+$") %>% readr::parse_number(),
    ) %>% 
      #limpiar
      mutate(fecha_scrapping = fecha) %>% 
      mutate(direccion = ifelse(is.na(direccion), ubicacion, direccion),
             zona = ifelse(is.na(zona), comuna, zona)) %>% 
      mutate(across(c(subtitulo, divisa, zona, comuna), ~ as.factor(.x))) %>% 
      #convertir precios en UF a pesos
      mutate(precio = case_when(divisa == "UF" ~ precio * 29500,
                                divisa == "U$S" ~ precio * 800,
                                divisa != "UF" ~ precio))
    
    #adjuntar a lista
    
    base_portal_list[[archivo]] <- resultado
    
    #terminar loop
    if(archivo == max(archivos_s)) { 
      cat("convirtiendo lista...", fill=T) 
      base_portal <- bind_rows(base_portal_list)
      cat("OK", fill=T)
    }
  }
  return(base_portal)
}

#—----

#descargar y scrappear ----
#departamentos
descargar_portalinmobiliario("https://www.portalinmobiliario.com/arriendo/departamento/tarapaca", "departamentos")

#casas
descargar_portalinmobiliario("https://www.portalinmobiliario.com/arriendo/casa/tarapaca", "casas")

#oficinas
descargar_portalinmobiliario("https://www.portalinmobiliario.com/arriendo/oficina/tarapaca", "oficinas")

#locales
descargar_portalinmobiliario("https://www.portalinmobiliario.com/arriendo/comercial/tarapaca", "locales")

#scrappear datos descargados
base_portal <- scrapping_portalinmobiliario()


#guardar
save(base_portal, file = paste0("scrapping/portalinmobiliario/bases/base_portal_", lubridate::today(), ".rdata"))

#guardar última base sin fecha para cargar desde shiny
save(base_portal, file = paste0("dataemprende_datos/scrapping_portal.rdata"))

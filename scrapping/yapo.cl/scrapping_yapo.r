setwd("~/Dataemprende/")

library(dplyr)
library(rvest)
library(lubridate)

cat("DEFINIENDO FUNCIONES...", fill=T)
#descargar páginas ----
#loop que genera todas las url de las categorías y páginas de resultados (50 publicaciones por página)

descargar_yapo <- function(paginas = 1:10, espera = 1.5, fecha = lubridate::today()) {
  #definir categorías (son todas menos las de busqueda de empleo)
  categorias <- c("computadores", "celulares", "consolas_videojuegos", "television_camaras",
                  "muebles", "electrodomesticos", "jardin_herramientas", "articulos-del-hogar",
                  "moda-vestuario", "bolsos-bisuteria-accesorios", "salud-belleza", "calzado",
                  "vestuario-futura-mama-ninos", "juguetes", "coches-articulos-infantiles",
                  "animales_accesorios", "deportes_gimnasia",
                  "bicicletas_ciclismo", "hobbies_outdoor", "instrumentos_musicales", "musica_peliculas",
                  "libros_revistas", "arte_antiguedades_colecciones",
                  "servicios", "otros_productos")
  #loop
  for (c in categorias) { #categorías por scrappear
    for (p in paginas) { #páginas de resultados por scrappear
      
      #generar la url
      url <- paste0("https://www.yapo.cl/tarapaca/", c, "?ca=2_s&o=", p)
      
      #generar ruta y nombre de archivo
      ruta <- paste0("scrapping/yapo.cl/paginas/", fecha, "/")
      archivo <- paste0(paste("yapo", fecha, c, p, sep = "_"), ".html")
      
      #crear carpeta si es que no existe
      dir.create(ruta)
      
      #intentar descargar (si falla, continúa el loop)
      try(
        download.file(url, 
                      destfile = paste0(ruta, archivo), 
                      quiet = F), silent = F)
      Sys.sleep(espera) #misericordia
    }
  }
}

# #scrapping individual
# 
# #cargar página descargada
# content <- read_html("scrapping/yapo.cl/temp/yapo_2021-06-10_muebles_4.html")
# content <- read_html("scrapping/yapo.cl/paginas/11-jun-2021/yapo_2021-06-11_animales_accesorios_10.html")
# 
# 
# 
# #prueba scrapping
# 
# #ver todos los nodos
# #content %>% html_nodes("*") %>% html_attr("class") %>% unique()
# 
# #contenido
# s_titulo <- content %>% html_nodes(".title") %>% html_text()
# s_enlace <- content %>% html_nodes(".title") %>% html_attr('href') #enlaces
# s_imagen <- content %>% html_nodes(".image") %>% html_attr('src')
# s_fecha <- content %>% html_nodes(".date") %>% html_text() 
# s_hora <- content %>% html_nodes(".hour") %>% html_text() 
# s_comuna <- content %>% html_nodes(".commune") %>% html_text() 
# 
# #obtiene los precios incluso si no hay
# s_precio <- content %>% html_nodes(".thumbs_subject") %>%
#   #busca adentro de la class anterior, retorna NA si no encuentra
#   lapply(. %>% html_nodes(".price") %>% html_text() %>%
#            ifelse(identical(., character(0)), NA, .)) %>% 
#   unlist()
# 
# #unir columnas
# resultado_1 <- tibble::tibble(titulo = s_titulo,
#                               precio = s_precio,
#                               enlace = s_enlace,
#                               #categoria = s_categoria,
#                               fecha = s_fecha,
#                               hora = s_hora,
#                               comuna = s_comuna,
#                               imagen = s_imagen)


#scrapping de todas las páginas ----

scrapping_yapo <- function(fecha = lubridate::today()) {
  fecha_a <- as.character(fecha)

  #lista de archivos
  ruta_scrapping <- paste0("scrapping/yapo.cl/paginas/", fecha_a, "/")
  archivos_s <- list.files(ruta_scrapping)
  
  #categorias formateadas
  categorias <- c("Artículos del hogar", "Bolsos y accesorios", "Celulares", "Computadores","Jardín y herramientas", "Moda y vestuario", 
                  "Muebles", "Salud y belleza", "Electrodomésticos", "Vestuario maternidad e infantil", 
                  "Calzado", "Consolas y videojuegos","Hobbies y outdoor", "Servicios", 
                  "Televisión y cámaras", "Artículos infantiles", "Deportes", "Juguetes", 
                  "Mascotas y accesorios", "Arte y antiguedades", "Libros y revistas", "Otros productos", 
                  "Ciclismo", "Música y películas", "Instrumentos musicales")
  
  #loop
  for(archivo in archivos_s) {
    
    #iniciar loop: inicializar lista
    if(archivo == min(archivos_s)) { 
      cat("inicializando lista", fill=T) 
      base_yapo_list <- list() 
    }
    
    nombre_archivo <- archivo %>% stringr::str_remove(paste0("yapo_", fecha_a, "_"))
    s_categoria <- nombre_archivo %>% stringr::str_remove(".\\d+.html")
    s_pagina <- nombre_archivo %>% stringr::str_extract("\\d+")
    
    #reconsrtruir url
    s_url <- paste0("https://www.yapo.cl/tarapaca/", s_categoria, "?ca=2_s&o=", s_pagina)
    
    #leer html
    cat("iniciando ", archivo, "...", fill=T)
    ruta_archivo <- paste0(ruta_scrapping, archivo)
    content <- rvest::read_html(ruta_archivo)
    
    #obtener contenido
    s_titulo <- content %>% html_nodes(".title") %>% html_text()
    s_enlace <- content %>% html_nodes(".title") %>% html_attr('href') #enlaces
    s_imagen <- content %>% html_nodes(".image") %>% html_attr('src')
    s_fecha <- content %>% html_nodes(".date") %>% html_text() 
    s_hora <- content %>% html_nodes(".hour") %>% html_text() 
    s_comuna <- content %>% html_nodes(".commune") %>% html_text() 
    
    #obtiene los precios incluso si no hay
    s_precio <- content %>% html_nodes(".thumbs_subject") %>%
      #busca adentro de la class anterior, retorna NA si no encuentra
      lapply(. %>% html_nodes(".price") %>% html_text() %>%
               ifelse(identical(., character(0)), NA, .)) %>% 
      unlist()
    
    if (length(s_titulo) < 2) { 
      warning(paste(nombre_archivo, "sin elementos")); 
      cat("error", fill=T)
      next() 
    }
    
    #unir columnas
    resultado <- tibble::tibble(producto = s_titulo %>% stringr::str_trim(),
                                precio = s_precio %>% stringr::str_remove("\\.") %>% readr::parse_number(),
                                enlace = s_enlace,
                                categoria = s_categoria,
                                pagina = s_pagina,
                                fecha = s_fecha,
                                hora = s_hora,
                                comuna = s_comuna,
                                imagen = s_imagen,
                                ruta = ruta_archivo,
                                url = s_url,
                                fecha_scrapping = fecha_a) %>%
      #limpieza
      mutate(across(c(categoria, fecha, comuna), ~ as.factor(.x))) %>%
      mutate(fecha_scrapping = lubridate::ymd(fecha_scrapping)) %>%
      mutate(pagina = readr::parse_number(pagina))
    
    base_yapo_list[[archivo]] <- resultado
    cat("OK ", archivo, fill=T) 
    
    #terminar loop: 
    if(archivo == max(archivos_s)) { 
      
      #transformar lista a tibble 
      cat("convirtiendo lista...", fill=T) 
      base_yapo <- bind_rows(base_yapo_list) 
      
      #ordenar categorías
      base_yapo <- base_yapo %>% mutate(categoria = recode(
        categoria,
        "articulos-del-hogar" = "Artículos del hogar",
        "bolsos-bisuteria-accesorios" = "Bolsos y accesorios",
        "celulares" = "Celulares",
        "computadores" = "Computadores",
        "jardin_herramientas" = "Jardín y herramientas",
        "moda-vestuario" = "Moda y vestuario",
        "muebles" = "Muebles",
        "salud-belleza" = "Salud y belleza",
        "electrodomesticos" = "Electrodomésticos",
        "vestuario-futura-mama-ninos" = "Vestuario maternidad e infantil",
        "calzado" = "Calzado",
        "consolas_videojuegos" = "Consolas y videojuegos",
        "hobbies_outdoor" = "Hobbies y outdoor",
        "servicios" = "Servicios",
        "television_camaras" = "Televisión y cámaras",
        "coches-articulos-infantiles" = "Artículos infantiles",
        "deportes_gimnasia" = "Deportes",
        "juguetes" = "Juguetes",
        "animales_accesorios" = "Mascotas y accesorios",
        "arte_antiguedades_colecciones" = "Arte y antiguedades",
        "libros_revistas" = "Libros y revistas",
        "otros_productos" = "Otros productos",
        "bicicletas_ciclismo" = "Ciclismo",
        "musica_peliculas" = "Música y películas",
        "instrumentos_musicales" = "Instrumentos musicales")) %>% 
        #ordenar factor de categoría
        mutate(categoria = as.factor(categoria),
               categoria = forcats::fct_relevel(categoria, categorias))
      
      #formatear fechas y hora
      cat("convirtiendo fecha y hora...", fill=T) 
      base_yapo <- base_yapo %>% 
        #detectar ayer/hoy de acuerdo a la fecha del scrapping
        mutate(fecha_f = case_when(fecha == "Hoy" ~ lubridate::ymd(fecha_a),
                                   fecha == "Ayer" ~ lubridate::ymd(fecha_a) - lubridate::days(1)),
               #detectar meses
               mes = case_when(stringr::str_detect(fecha, "Ene") ~ 01,
                               stringr::str_detect(fecha, "Feb") ~ 02,
                               stringr::str_detect(fecha, "Mar") ~ 03,
                               stringr::str_detect(fecha, "Abr") ~ 04,
                               stringr::str_detect(fecha, "May") ~ 05,
                               stringr::str_detect(fecha, "Jun") ~ 06,
                               stringr::str_detect(fecha, "Jul") ~ 07,
                               stringr::str_detect(fecha, "Ago") ~ 08,
                               stringr::str_detect(fecha, "Sep") ~ 09,
                               stringr::str_detect(fecha, "Oct") ~ 10,
                               stringr::str_detect(fecha, "Nov") ~ 11,
                               stringr::str_detect(fecha, "Dic") ~ 12),
               #detectar días
               dia = readr::parse_number(as.character(fecha))) %>% 
        #armar fechas
        mutate(fecha_p = lubridate::ymd(paste(2021, mes, dia, sep="-"))) %>% 
        #unir columnas de hoy/ayer con fechas armadas
        mutate(fecha = coalesce(fecha_p, fecha_f)) %>%
        #limpiar
        select(-mes, -dia, -fecha_p, -fecha_f) %>% 
        #corrección si la fecha es superior al día de scrapping, entonces sería 2020 y no 2021
        mutate(fecha = case_when(fecha > lubridate::ymd(fecha_a) ~ fecha - years(1),
                                 TRUE ~ fecha)) %>% 
        #crear columna con fecha+hora y otra con sólo hora
        mutate(fecha_hora = lubridate::parse_date_time(paste(fecha, hora), "%y-%m-%d %H:%M"),
               hora = hms::as_hms(fecha_hora)) 
    }
  }
  
  return(base_yapo)
}

#—----

#descargar páginas nuevas
cat("DESCARGANDO...", fill=T)
descargar_yapo()

#scrappear páginas descargadas
cat("SCRAPPING...", fill=T)
base_yapo <- scrapping_yapo()

#guardar base con fecha
save(base_yapo, file = paste0("scrapping/yapo.cl/bases/base_yapo_", lubridate::today(), ".rdata"))


# integrar bases ----
cat("INTEGRANDO BASES...", fill=T)

archivos_y <- list.files("scrapping/yapo.cl/bases/")

#crear lista
bases_yapo <- list()

#loop que carga todas las bases en una sola lista
for (arch in archivos_y) {
  bases_yapo[[arch]] <- get(load(paste0("scrapping/yapo.cl/bases/", arch)))
}

#convertir lista a data frame
bases_yapo <- dplyr::bind_rows(bases_yapo) %>% 
  distinct(producto, fecha, .keep_all = T)

#guardar base unificada
save(bases_yapo, file = "scrapping/yapo.cl/bases_yapo.rdata")

#—----
#procesar ----
cat("PROCESANDO BASES...", fill=T)
source("scrapping/yapo.cl/procesar_yapo.r", echo = T)



library(dplyr)
library(rvest)

# #dirección a scrappear
# url <- "https://www.yapo.cl/tarapaca/"
# url <- "yapo.cl"
# url <- "www.estrellaiquique.cl"
# url <- "https://www.yapo.cl/tarapaca/electrodomesticos/"
# url <- "https://www.yapo.cl/tarapaca/moda-belleza-salud?ca=2_s&l=0&w=1&cmn=" #cuando son pag 1
# "https://www.yapo.cl/tarapaca/moda-belleza-salud?ca=2_s&cg=4000&o=2"
# "https://www.yapo.cl/tarapaca/moda-belleza-salud?ca=2_s&cg=4000&o=3"
# "https://www.yapo.cl/tarapaca/muebles?ca=2_s&l=0&w=1&cmn=" #cuando son pag 1
# #https://www.yapo.cl/tarapaca/computadores?ca=2_s&o=3

##forma de construir la url
#paste0("https://www.yapo.cl/tarapaca/", categoria, "?ca=2_s&o=", pagina)


#descargar página manualmente
download.file(url, destfile = "scrapping/yapo.cl/temp/1.html", quiet=TRUE)

#definir categorías (son todas menos las de busqueda de empleo)
categorias <- c("computadores",
                "celulares",
                "consolas_videojuegos",
                "television_camaras",
                #
                "muebles",
                "electrodomesticos",
                "jardin_herramientas",
                "articulos-del-hogar",
                #
                "moda-vestuario",
                "bolsos-bisuteria-accesorios",
                "salud-belleza",
                "calzado",
                #
                "vestuario-futura-mama-ninos",
                "juguetes",
                "coches-articulos-infantiles",
                #
                "animales_accesorios",
                "deportes_gimnasia",
                "bicicletas_ciclismo",
                "hobbies_outdoor",
                "instrumentos_musicales",
                "musica_peliculas",
                "libros_revistas",
                "arte_antiguedades_colecciones",
                #
                "servicios",
                #
                "otros_productos")

#descargar páginas ----
#loop que genera todas las url de las categorías y páginas de resultados (50 publicaciones por página)
for (c in categorias) { #categorías por scrappear
  for (p in 1:10) { #páginas de resultados por scrappear
    
    #generar la url
    url <- paste0("https://www.yapo.cl/tarapaca/", c, "?ca=2_s&o=", p)
    
    #intentar descargar (si falla, continúa el loop)
    try(
      download.file(url, 
                    #nombre del archivo
                    destfile = paste0("scrapping/yapo.cl/paginas/11-jun-2021/", 
                                      paste("yapo",
                                            lubridate::today(), #fecha
                                            c, p, #categoría y página
                                            sep = "_"),
                                      ".html"), quiet = F), #extensión
      silent = F)
    Sys.sleep(1.5) #misericordia
  }
}

#scrapping individual ----

#cargar página descargada
content <- read_html("scrapping/yapo.cl/temp/yapo_2021-06-10_muebles_4.html")
content <- read_html("scrapping/yapo.cl/paginas/11-jun-2021/yapo_2021-06-11_animales_accesorios_10.html")



#scrapping ----

#ver todos los nodos
#content %>% html_nodes("*") %>% html_attr("class") %>% unique()

#contenido
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

#unir columnas
resultado_1 <- tibble::tibble(titulo = s_titulo,
                              precio = s_precio,
                              enlace = s_enlace,
                              #categoria = s_categoria,
                              fecha = s_fecha,
                              hora = s_hora,
                              comuna = s_comuna,
                              imagen = s_imagen)


#scrapping de todas las páginas ----
fecha_s <- "11-jun-2021"
fecha_a <- "2021-06-11"
ruta_scrapping <- paste0("scrapping/yapo.cl/paginas/", fecha_s, "/")
archivos_s <- list.files(ruta_scrapping)

#inicializar lista
#base_yapo <- list()

archivos_s[1]
min(archivos_s)

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
  content <- read_html(ruta_archivo)
  
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
  
  #terminar loop: transformar lista a tibble 
  if(archivo == max(archivos_s)) { 
    cat("convirtiendo lista...", fill=T) 
    base_yapo <- bind_rows(base_yapo_list) 
    }
}

#save(base_yapo, file = paste0("scrapping/yapo.cl/bases/base_yapo_", fecha_s, ".rdata"))
#load(paste0("scrapping/yapo.cl/bases/base_yapo_", fecha_s, ".rdata"))

base_yapo %>% count(categoria)

base_yapo %>% count(comuna)

library(ggplot2)

base_yapo %>%
  ggplot(aes(precio)) +
  geom_histogram(bins = 90) +
  geom_rug(alpha=0.1) +
  scale_x_continuous(n.breaks = 10, limits = c(0, 60000))

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
                  destfile = paste0("scrapping/yapo.cl/paginas/", 
                                    paste("yapo",
                                          lubridate::today(), #fecha
                                          c, p, #categoría y página
                                          sep = "_"),
                                    ".html"), quiet = F), #extensión
    silent = F)
    Sys.sleep(1.5) #misericordia
  }
}



#cargar página descargada
content <- read_html("scrapping/yapo.cl/temp/yapo_2021-06-10_muebles_4.html")


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
                                      
resultado_1

#agregar columnas del loop

#agregar a lista
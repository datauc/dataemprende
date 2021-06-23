# Contexto ----

# Se realiza un sistema de web scraping que pueda extraer información de los
# productos disponibles en Lider, para calcular la canasta básica familiar y ver
# como evoluciona en el tiempo


# Librerias ----

library(tidyverse)
library(rvest)

# Modelo

# Se carga la pagina principal del Lider, para extraer:
# Mundo (carnes, frutas, etc.)
# Departamento (Dentro de carnes: vacuno, pollo, cerdo, etc.)
# Categoría (Dentro de vacuno: deshuesado, al vacío, etc.)

Pagina_Lider <- read_html("https://www.lider.cl/supermercado/")


# Mundo  ----

# Nombre del Mundo (como se les muestra a los usuarios)
NOMBRE_MUNDO <- Pagina_Lider %>% 
  html_nodes(xpath = "//div[@class='department-list']/ul") %>% 
  html_text2()  %>% 
  str_replace_all("[:punct:]"," ") %>% 
  str_replace_all("\n\n","\n") %>% 
  str_split_fixed("\n",str_count(.,"\n")+1) %>% 
  str_trim()

# Nombre del Mundo (como se utiliza dentro del HTML)
mundo <- Pagina_Lider %>%
  html_nodes(xpath = "//div[@class='department-list']/ul/li/a") %>%
  html_attr('data-target') %>% 
  str_remove_all("#")


# Departamento ----

# Se crea un vector vacio donde se agregaran la lista de departamentos por cada mundo
Departamento = character(0)
for(i in mundo){
  aux = character(0)
  niveles <- Pagina_Lider %>% 
    html_node(xpath = paste0("//div[@id = '",i,"']/div/div", sep = "")) %>% 
    html_children() %>% 
    length()
  
  for(j in 1:niveles){
     aux[length(aux) + 1] <- Pagina_Lider %>% 
      html_node(xpath = paste0("//div[@id = '",i,"']/div/div/div[",j,"]/h3", sep = "")) %>% 
      html_text2()
     }
  Departamento[length(Departamento) +1] <- aux %>% list()
}

# Se crea un df, con el mundo y departamento
df <- tibble(Codigo_mundo = mundo,
             Mundo = NOMBRE_MUNDO,
             Departamento = Departamento) %>% 
  mutate(n = lapply(Departamento,length) %>% unlist()) 

# Categoría ----

# Se debe crear una lista contenida dentro de otra lista
# Esto debido a que para cada elemento de la lista de departamento, se crea la lista de categorias

categorias = character(0)
URL_CATEGORIA = character(0)
for(i in 1:16){
  aux1 = character(0)
  aux3 = character(0)
  for (j in 1:df$n[i]) {
    # Categoria ----
    x <- Pagina_Lider %>% 
      html_node(xpath = paste0("//div[@id = '",df$Codigo_mundo[i],"']/div/div/div[",j,"]/ul", sep = "")) %>% 
      html_children() %>% 
      html_text2()
    if(identical(x, character()) == FALSE){
      aux1[length(aux1) + 1] <- x  %>% 
        .[1:(length(.)-1)] %>% 
        list() 
      aux3[length(aux3) + 1] <- Pagina_Lider %>% 
        html_node(xpath = paste0("//div[@id = '",df$Codigo_mundo[i],"']/div/div/div[",j,"]/ul", sep = "")) %>% 
        html_children() %>% 
        html_children() %>% 
        html_attr('href') %>%
        str_remove("https://www.lider.cl") %>% 
        paste0("https://www.lider.cl",.) %>% 
        .[1:(length(.)-1)] %>% 
        list()
      
    }else{
      aux1[length(aux1) + 1] <- NA
      aux3[length(aux3) + 1] <- NA
      }
  }
  categorias[length(categorias) + 1] <- aux1 %>% list()
  URL_CATEGORIA[length(URL_CATEGORIA) + 1] <- aux3 %>% list()
}

# procesamiento de las listas ----

# Se añaden las listas contenidas dentro de otra lista al df
df <- df %>%
  add_column(Categorias = categorias, URL_categorias = URL_CATEGORIA) %>%
  select(-n)
  

# Se expanden el df, expandiendo las listas y se guarda en un df2
df2 <- df %>% 
  unnest(cols = c(Departamento,Categorias,URL_categorias), keep_empty = TRUE) %>% 
  unnest(cols = c(Categorias,URL_categorias), keep_empty = TRUE) %>% 
  drop_na() %>% 
  add_column(Nombre      = character(nrow(.)),
             Descripción = character(nrow(.)),
             Atributo    = character(nrow(.)),
             Precio      = character(nrow(.)),
             Precio_redondeado = character(nrow(.)),
             Enlace      = character(nrow(.)))

# El resultado es un df de 515 categorías


# Productos ----

# Proceso de extracción de información de los productos de cada categoría

for(i in 1:nrow(df2)){
  Sys.sleep(2) # Tiempo de espera para evitar bloqueo
  
  # Se selecciona la categoría i
  ENLACES <- df2$URL_categorias[i] %>% read_html()
  
  # Se calcula el número de productos para esa categoría
  N_Productos <- ENLACES %>% 
    html_node(xpath = "//small[@class = 'hidden-xs']") %>% 
    html_text2() %>% 
    str_remove_all('[:alpha:]') %>% 
    str_remove_all('[:punct:]') %>% 
    as.numeric() 
  
  # Se calcula el número de pagínas, suponiendo 80 productos por pag.
  N_Paginas <- ceiling(N_Productos/80)
  
  # Se realiza el scraping de los productos de la categoría i
  Sys.sleep(2) # tiempo de espera para evitar bloqueo
  
  # Se genera un df que se llena con los productos de la categoría i
  df_aux <- tibble(Nombre      = character(0),
                   Descripción = character(0),
                   Atributo    = character(0),
                   Precio      = character(0),
                   Precio_redondeado = character(0),
                   Enlace      = character(0))
  
  # Se salta la categoría si esta nos da NA, esto se debe a que algunas paginas del Lider
  # Tienen error 500
  if(is.na(N_Paginas) == TRUE){next}
  
  # Se realiza el scraping por página
  for(j in 1:N_Paginas){
    if(N_Paginas == 1){
      Pagina <- paste0(df2$URL_categorias[i],"?N=&No=0&Nrpp=80",sep = "") %>% read_html()
    }else{
      Pagina <- paste0(df2$URL_categorias[i],"?No=80&isNavRequest=Yes&Nrpp=80&page=",j,sep = "")  %>% read_html()
    }
    
    # Nombre del producto
    Nombre <- Pagina %>%
      html_nodes(xpath = "//span[@class='product-name']") %>% 
      html_text2()
    
    # Descripción del producto
    Descripción <- Pagina %>%
      html_nodes(xpath = "//span[@class='product-description js-ellipsis']") %>% 
      html_text2() 
    
    # Atributo del producto 
    Atributo <- Pagina %>%
      html_nodes(xpath = "//span[@class='product-attribute']") %>% 
      html_text2() 
    
    # Precio del producto
    Precio <- Pagina %>%
      html_nodes(xpath = "//span[@class='price-sell']") %>% 
      html_text2() 
    
    # Precio redondeado
    Precio_redondeado <- Pagina %>%
      html_nodes(xpath = "//span[@class='product-round']") %>% 
      html_text2()
    
    # Enlace del producto
    Enlace <- Pagina %>%
      html_nodes(xpath = "//a[@class='product-link']") %>% 
      html_attr('href') %>% 
      unique() %>% 
      paste0("https://www.lider.cl",.) 
    
    # Si el largo de nombre y precio es distinto, se debe a que un producto no tiene señalado
    # la información de precio, esto ocurre con productos descontinuados. Por lo cual se procede
    # a identificarlo y eliminarlo de la muestra
    if(length(Nombre) != length(Precio)){
      
      id <- Pagina %>% 
        html_node(xpath = "//div[@id = 'content-prod-boxes']") %>% 
        html_children() %>% 
        html_attr('id') %>% 
        .[-1]
      
      id_eliminar = integer(0)
      for(i in id){
        x <- Pagina %>% 
          html_node(xpath = paste0("//div[@id = '",i,"']/div[2]/div/div/span[2]/b", sep = "")) %>% 
          html_text2()
        if(is.na(x) == TRUE){
          id_eliminar[length(id_eliminar) + 1] = which(i == id)
        }
      }
      
      Nombre      <- Nombre[-id_eliminar]
      Descripción <- Descripción[-id_eliminar]
      Enlace      <- Enlace[-id_eliminar]
    }
    
    # Se asignan los productos de la página actual al df_aux y se itera el proceso con la
    # siguiente pagina
    
    df_aux <- df_aux %>% 
      add_row(Nombre      = Nombre,
              Descripción = Descripción,
              Atributo    = Atributo,
              Precio      = Precio,
              Precio_redondeado = Precio_redondeado,
              Enlace      = Enlace)
    
    print(paste0("Pagina ",j," de ",N_Paginas," scrapeada", sep = ""))
  }
  
  # Se asignan los productos de la categoría i al df final
  df2$Nombre[i]      <- df_aux$Nombre %>% list()
  df2$Descripción[i] <- df_aux$Descripción %>% list()
  df2$Atributo[i]    <- df_aux$Atributo %>% list()
  df2$Precio[i]      <- df_aux$Precio %>% list()
  df2$Precio_redondeado[i] <- df_aux$Precio_redondeado %>% list()
  df2$Enlace[i]      <- df_aux$Enlace %>% list()
  
  print(paste0("Termino de ejecución de la categoría número ",i," (504), Categoria: ",df2$Categorias[i],sep = ""))
}

# Se eliminan de la lista los productos que dieron error 500
Lider <- df2 %>% 
  unnest(cols = c(Nombre,Descripción,Atributo,Precio,Precio_redondeado,Enlace)) %>% 
  filter(Nombre != "")

save(Lider,file = "C:\\Users\\Diego Holiwis\\Documents\\GitHub\\dataemprende\\datos diego\\Web scraping - Lider\\Lider.Rdata")

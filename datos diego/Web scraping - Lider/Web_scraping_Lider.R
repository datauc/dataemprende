library(tidyverse)
library(rvest)

Pagina_Lider <- read_html("https://www.lider.cl/supermercado/")


# Departamento  ----

# Nombre del Mundo en el HTML (se utiliza para la base de datos)
NOMBRE_MUNDO <- Pagina_Lider %>% 
  html_nodes(xpath = "//div[@class='department-list']/ul") %>% 
  html_text2()  %>% 
  str_replace_all("[:punct:]"," ") %>% 
  str_replace_all("\n\n","\n") %>% 
  str_split_fixed("\n",str_count(.,"\n")+1) %>% 
  str_trim()

# Nombre del Mundo para los usuarios (se utiliza para moverse en el HTML)
mundo <- Pagina_Lider %>%
  html_nodes(xpath = "//div[@class='department-list']/ul/li/a") %>%
  html_attr('data-target') %>% 
  str_remove_all("#")

Departamento = character(0)
# Nombre del departamento por mundo
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

df <- tibble(Codigo_mundo = mundo,
             Mundo = NOMBRE_MUNDO,
             Departamento = Departamento) %>% 
  mutate(n = lapply(Departamento,length) %>% unlist()) 

# Nombre de la categoria por departamento
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

df <- df %>%
  add_column(Categorias = categorias, URL_categorias = URL_CATEGORIA) %>%
  select(-n)
  
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


for(i in 139:nrow(df2)){
  Sys.sleep(2)
  # Se ingresa a la primera categoría del departamento
  ENLACES <- df2$URL_categorias[i] %>% read_html()
  
  # Se calcula el número de paginas de productos de dicha categoría
  N_Productos <- ENLACES %>% 
    html_node(xpath = "//small[@class = 'hidden-xs']") %>% 
    html_text2() %>% 
    str_remove_all('[:alpha:]') %>% 
    str_remove_all('[:punct:]') %>% 
    as.numeric() 
  N_Paginas <- ceiling(N_Productos/80)
  
  # Se realiza el scraping de todas las paginas de una categoría del primer departamento
  Sys.sleep(2)
  
  df_aux <- tibble(Nombre      = character(0),
                   Descripción = character(0),
                   Atributo    = character(0),
                   Precio      = character(0),
                   Precio_redondeado = character(0),
                   Enlace      = character(0))
  
  if(is.na(N_Paginas) == TRUE){next}
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
    
    df_aux <- df_aux %>% 
      add_row(Nombre      = Nombre,
              Descripción = Descripción,
              Atributo    = Atributo,
              Precio      = Precio,
              Precio_redondeado = Precio_redondeado,
              Enlace      = Enlace)
    
    print(paste0("Pagina ",j," de ",N_Paginas," scrapeada", sep = ""))
  }
  
  df2$Nombre[i]      <- df_aux$Nombre %>% list()
  df2$Descripción[i] <- df_aux$Descripción %>% list()
  df2$Atributo[i]    <- df_aux$Atributo %>% list()
  df2$Precio[i]      <- df_aux$Precio %>% list()
  df2$Precio_redondeado[i] <- df_aux$Precio_redondeado %>% list()
  df2$Enlace[i]      <- df_aux$Enlace %>% list()
  
  print(paste0("Termino de ejecución de la categoría número ",i," (504), Categoria: ",df2$Categorias[i],sep = ""))
}





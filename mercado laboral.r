#—----
library(dplyr)
library(ggplot2)
load("/home/diego/Proyectos/Estudio mercado/RTarapaca.RData")

Trabajos %>% glimpse() 

Trabajos %>% 
  count(CategoriaE)


limpiar_campo_texto <- function(x){
    x <- as.character(x)
    x <- stringr::str_remove_all(x, "c\\(")
    x <- stringr::str_remove_all(x, "[:punct:]")
    x <- stringr::str_remove_all(x, "\\d+")
    x <- tolower(x)
  return(x)
}

trabajos <- Trabajos %>% 
  janitor::clean_names() %>% 
  mutate(fecha = stringr::str_remove(fecha_publicacion, "Publicado: "),
         fecha = lubridate::dmy(fecha)) %>% 
  mutate(descripcion = limpiar_campo_texto(descripcion),
         requisitos = limpiar_campo_texto(requisitos)) %>% 
  mutate(titulo = stringr::str_to_sentence(titulo)) 

trabajos %>% 
  #duplicar comunas
  mutate(comuna = stringr::str_replace_all(comuna, "\n", "-"),
         comuna = stringr::str_replace_all(comuna, "ueAl", "ue - Al"),
         comuna = stringr::str_replace_all(comuna, "character\\(0\\)", "")) %>% 
  tidyr::separate(comuna, into = c("comuna_1", "comuna_2", "comuna_3"), sep="-", remove = TRUE) %>%
  tidyr::pivot_longer(starts_with("comuna"), names_to = "comuna_original", values_to="comuna", values_drop_na = TRUE) %>% 
  mutate(comuna = stringr::str_trim(comuna),
         comuna = ifelse(comuna == "", "Sin información", comuna),
         comuna = ifelse(comuna == "Tarapacá", "Sin información", comuna))


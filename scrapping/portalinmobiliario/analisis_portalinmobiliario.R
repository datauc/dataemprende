library(dplyr)
library(stringr)
library(ggplot2)

load("dataemprende_datos/scrapping_portal.rdata")


base_portal %>% count(tipo)
base_portal %>% count(comuna)

#precio por metros cuadrados
base_portal %>% 
  ggplot(aes(as.numeric(metros), precio/1000, col=tipo)) +
  geom_point(size = 2, alpha = 0.5) +
  xlim(0, 600) +
  ylim(0, 10000) +
  theme(legend.position = "bottom")

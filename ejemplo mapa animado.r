
library(dplyr)
library(ggplot2)
library(gganimate)

mapa_regional <- readr::read_rds("mapa_regional.rds")

p2 <- mapa_regional %>%
  mutate(tasa = replace(tasa, tasa==0, NA)) %>% 
  ggplot(aes(geometry = geometry)) +
  geom_sf(data = mapa_regional,
          aes(fill = tasa), 
          col = "white",
          size = 0.8) +
  scale_fill_gradient(low = "#2D668E", high = "#F1FAEE", na.value = "white") +
  theme_void(base_size = 15)+
  theme(legend.title = element_blank())

anim2 <- p2 +
  transition_states(rubro,
                    transition_length = 2,
                    state_length = 3) +
  enter_grow() +
  exit_shrink() +
  ease_aes('linear')

anim2

anim_save(animation = anim2, 
          renderer = gifski_renderer(),
          filename = "anim4.gif", 
          fps = 30,
          duration = 20,
          rewind = T)
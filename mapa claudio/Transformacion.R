library(tidyverse)
# remotes::install_github("litos81/sgs")
# remotes::install_github("clozanoruiz/sgs")
# withr::with_envvar(c(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true"), 
#                    remotes::install_github("clozanoruiz/sgs")
# )
library(sgs)
library(leaflet)

df <- readxl::read_xlsx("mapa claudio/Empresas_Tarapaca_v2.xlsx")

options(digits=3)
start = list(as.numeric(df$X),as.numeric(df$Y))

p <- sgs_points(start, epsg=3857)
res <- sgs_en_wgs84(p)
X= res$x
Y= res$y

# df[,12] = X
# df[,13] = Y
# 
# names(df) = c(names(df)[1:11],"latitud","longitud")
# df
# 
# openxlsx::write.xlsx(df,"Empresas_Iquique_v2.xlsx")


m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=X[1:1000], lat=Y[1:1000])
m  # Print the map


m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=X[9000:10000], lat=Y[9000:10000])
m  # Print the map

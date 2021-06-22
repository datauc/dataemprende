library(tidyverse)
library(data.table)



df <- read_delim("datos diego/datas/base-gastos-viii-epf-(formato-csv).csv", 
                 ";", escape_double = FALSE, trim_ws = TRUE)

df_personas <- read_csv2("datos diego/datas/Personas.csv")




df <- rio::import("datos diego/datas/base-gastos-viii-epf-(spss).sav") %>% tibble()

df <- rio::import("datos diego/datas/Casen 2017.sav") %>% tibble()
save(df,file = "datos diego/datas/Casen.RData")


load("datos diego/datas/Casen.RData")

df <- df %>% 
  filter(region == 1)

df %>% str()


df[region = 1 & comuna == 1101,]


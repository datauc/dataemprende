graficar_empresas()

library(dplyr)
library(ggplot2)
library(emojifont)
load.fontawesome()

load.fontawesome(font = "~/Dataemprende/fa-regular-400.ttf")

emojifont::sample_fontawesome()
emojifont::search_fontawesome("store")

#https://stackoverflow.com/questions/46978992/cant-use-font-awesome-in-r
## Generate some data
DF <- data.frame(x=1:5, y=1:5)
ggplot(DF, aes(x=x, y=y)) + 
  geom_text(label = fontawesome('fa-building'), 
            family='fontawesome-webfont')


fontawesome('fa-building')

library(extrafont)
extrafont::loadfonts()
extrafont::fonts()

ggplot(DF, aes(x=x, y=y)) + 
  geom_text(label = "\uF54E", 
            #family='FontAwesome')
            #family="fontawesome-webfont")
            family='Font Awesome 5 Free Regular')


library(purrr)

loadfonts()

pdfFonts() %>% 
  map_chr("family") %>% 
  keep(~grepl(".*Font*", .))
#


#install.packages('extrafont')
#devtools::install_github("dreamRs/gfonts")
#para solucionar unos errores
#remotes::install_version("Rttf2pt1", version = "1.3.8")
#sudo apt-get install fonts-font-awesome
#sudo apt-get install libfreetype6-dev
#install.packages("showtext")
#devtools::install_github("GuangchuangYu/emojifont")

library(gfonts)
library(extrafont)
library(dplyr)

#primero importar todas las tipografías del sistema
extrafont::font_import()


#ver tipografías disponibles
(gfonts::get_all_fonts() %>% tibble() %>%
    filter(family == "Dosis"))$variants

(gfonts::get_all_fonts() %>% tibble() %>%
    filter(family == "Open Sans"))$variants

#crat carpeta
#sudo mkdir /usr/share/fonts/google/

#dar permisos para descargar ahí
#sudo chmod 777 /usr/share/fonts/google/

#descargar a la carpeta
ruta <- "/usr/share/fonts/google/"

gfonts::download_font("montserrat", output_dir = ruta)
gfonts::download_font("dosis", output_dir = ruta)
gfonts::download_font("dosis", variant = "regular", output_dir = ruta)
gfonts::download_font("open-sans", output_dir = ruta)
gfonts::download_font("open-sans", variant = "regular", output_dir = ruta)



#importar tipografías descargadas
extrafont::font_import(path = ruta, prompt = F)


#actualizar cache del sistema
system("fc-cache -fv")

#reiniciar r

#cargar tipografías
extrafont::loadfonts()

#revisar
extrafont::fonts()

#Luego confirmar nombres de tipografías:
extrafont::fonttable() %>% tibble() %>%
select(3:7) %>%
filter(stringr::str_detect(FontName, "Dosis"))

extrafont::fonttable() %>% tibble() %>%
  select(3:7) %>%
  filter(stringr::str_detect(FontName, "Open"))



#—----


#instalar FontAwesome
#instalar fontforge
#sudo apt install fontforge

#descargar tipografías
#https://fontawesome.com/v5.15/how-to-use/on-the-desktop/setup/getting-started
ruta_fa <- "~/Otros/Otros/fontawesome-free-5.15.4-desktop/otfs/"

#convertir de otf a ttf con fontforge

#fontforge -lang=ff -c 'Open($1); Generate($2); Close();' 'Font Awesome 5 Free-Regular-400.otf' 'Font Awesome 5 Free-Regular-400.ttf'
#fontforge -lang=ff -c 'Open($1); Generate($2); Close();' 'Font Awesome 5 Free-Solid-900.otf' 'Font Awesome 5 Free-Solid-900.ttf'
#fontforge -lang=ff -c 'Open($1); Generate($2); Close();' 'Font Awesome 5 Brands-Regular-400.otf' 'Font Awesome 5 Brands-Regular-400.ttf'

#importar tipografías descargadas
extrafont::font_import(path = ruta_fa, prompt = F)

#actualizar cache del sistema
system("fc-cache -fv")

#reiniciar r
library(dplyr)

#cargar tipografías
extrafont::loadfonts()

#revisar
extrafont::fonts()

#Luego confirmar nombres de tipografías:
extrafont::fonttable() %>% tibble() %>%
  select(3:7) %>%
  filter(stringr::str_detect(FontName, "Awesome")) %>% 
  glimpse()




#—----
#https://github.com/FortAwesome/Font-Awesome/blob/master/webfonts/fa-regular-400.ttf

#crat carpeta
#sudo mkdir /usr/share/fonts/fontawesome/

#dar permisos para descargar ahí
#sudo chmod 777 /usr/share/fonts/fontawesome/

#https://github.com/FortAwesome/Font-Awesome/raw/master/webfonts/fa-solid-900.ttf

#descargar a la carpeta
download.file(url = "https://github.com/FortAwesome/Font-Awesome/raw/master/webfonts/fa-regular-400.ttf",
              destfile = "fa-regular-400.ttf")

#importar tipografías descargadas
extrafont::font_import(path = "/usr/share/fonts/fontawesome/", prompt = F)


#actualizar cache del sistema
system("fc-cache -fv")

#reiniciar r

#cargar tipografías
extrafont::loadfonts()

#revisar
extrafont::fonts()



#—----


install.packages(c("waffle"))#, "emojifont", "showtext"))

library(waffle)
library(emojifont)
library(showtext)

font_add(family = "FontAwesome", 
         regular = "fa-regular-400.ttf")

emojifont::search_fontawesome("house")

waffle(c(5,12,18), rows = 5, use_glyph = "subway", glyph_size = 20, 
       title = "Subways!", legend_pos="right")

ggplot(DF, aes(x=x, y=y)) + 
  geom_text(label = "\uF1AD", 
            family='FontAwesome')
            #family="fontawesome-webfont")
            #family='Font Awesome 5 Free Regular')


DF <- data.frame(x=1:5, y=1:5)
ggplot(DF, aes(x=x, y=y)) + 
  geom_text(label = "\uF54E",#fontawesome('fa-building'), 
            family='FontAwesome')



#—----

#https://insileco.github.io/2017/05/23/add-icons-on-your-r-plot/
dir.create("fonts", showWarnings = FALSE)
##-- URLs
urls <- c(
  'https://github.com/jpswalsh/academicons/raw/master/fonts/academicons.ttf',
  'https://github.com/xdanaux/fontawesome-latex/raw/master/FontAwesome.otf',
  'https://github.com/ionic-team/ionicons/raw/master/docs/fonts/ionicons.ttf'
)
##-- download the fonts
for (i in 1:3){
  download.file(urls[i], destfile=paste0("fonts/", basename(urls[i])))
}

showtext_auto()

font_paths("fonts")

font_add(family = 'academicons', regular = 'fonts/academicons.ttf')
font_add(family = 'FontAwesome', regular = 'fonts/FontAwesome.otf')
font_add(family = 'FontAwesome', regular = 'fa-regular-400.ttf')
#font_add(family = 'ionicons', regular = 'ionicons.ttf')
##-- check the font families available
font_families()





DF <- data.frame(x=1:5, y=1:5)
ggplot(DF, aes(x=x, y=y)) + 
  geom_text(
    #label = "\uF54E",
    label = "\uF6D1",
            #fontawesome('fa-house'), 
            family='FontAwesome')
    #family = "Font Awesome 5 Free Regular")

extrafont::loadfonts()

extrafont::font_import("/usr/share/fonts/truetype/font-awesome/")

extrafont::font_import("fonts/")
extrafont::loadfonts()
extrafont::fonts()



#—----


sudo mkdir /usr/local/share/fonts/fuentes

sudo cp ~/Dataemprende/fonts/*.ttf /usr/local/share/fonts/fuentes/
  
sudo chown root:staff /usr/local/share/fonts/fuentes -R
sudo chmod 644 /usr/local/share/fonts/fuentes/* -R
sudo chmod 755 /usr/local/share/fonts/fuentes
sudo fc-cache -fv
- Luego reiniciar R
- Luego registrar con `extrafont`:
  extrafont::font_import(path="/usr/local/share/fonts/fuentes/")
extrafont::fonts()
extrafont::loadfonts()
extrafont::font_import()

Luego confirmar revisando:
  # Vector of font family names
  extrafont::fonts()

# Show entire table
extrafont::fonttable()

library(showtext)
#font_add(family = "FontAwesome5", regular = "/Users/fronori/Library/Fonts/fa-solid-900.ttf")
font_add(family = "FontAwesome5Free-Regular", regular = "/usr/local/share/fonts/fuentes/fa-regular-400.ttf")
#font_add(family = "FontAwesome5Brands-Regular", regular = "/Users/fronori/Library/Fonts/fa-brands-400.ttf")
showtext_auto()


DF <- data.frame(x=1:5, y=1:5)
ggplot(DF, aes(x=x, y=y)) + 
  geom_text(
    #label = "\uF54E",
    #label = "\uF6D1",
    label = "\uf1b9",
    #label = emojifont::fontawesome('fa-house'), 
    #family='FontAwesome')
#family = "FontAwesome5Free-Regular")
family = "fontawesome-webfont")

extrafont::fonttable() %>% 
  dplyr::filter(stringr::str_detect(FamilyName,"^Font.")) %>% 
  select(FontName, fontfile)



#—----

install.packages("extrafont")

library(extrafont)
library(dplyr)
font_import()  

extrafont::fonttable() %>% 
  dplyr::filter(stringr::str_detect(FamilyName,"^Font.")) %>% 
  select(FontName, fontfile)

loadfonts()
library(ggplot2)
DF <- data.frame(x=1:3, y=1:3, z=1:3)
ggplot(DF, aes(x=x, y=y, 
               #label = c("a", "b", "c"))) + 
               label = c("\uF54F", #"\uF015",
               "\uF54E",
               "\uF1AD"))) +
  geom_text(family = "FontAwesome")
    


#—----
#devtools::install_github("GuangchuangYu/ggimage")

library(dplyr)
library(ggplot2)
library(ggimage)


source("~/Otros/fa_svg_icons.r")

d <- data.frame(x = rnorm(10),
                y = rnorm(10),
                image = sample(fa_icons, size=10, replace = TRUE)
)

d

ggplot(d, aes(x, y)) + 
  geom_image(aes(image=image, col = "red"), size=.1,  asp = 1, alpha = 0.6)

ggsave(plot = last_plot(), filename = "plot.pdf")

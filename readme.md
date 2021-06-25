# DataEmprende

_En construcción_

Sitio interactivo que presenta información resumida y visualizaciones accesibles acerca de las empresas y trabajadores de la región de Tarapacá. Una persona emprendedora de la región puede elegir su comuna junto a su rubro y subrubro de desempeño, y recibirá información que detalla cómo se conforma la economía regional, quiénes son sus competidores, dónde se ubican, cuánto venden, cómo se componen sus empresas, entre otras.

La fuente de información principal es el sitio [estadísticas de empresas](https://www.sii.cl/sobre_el_sii/estadisticas_de_empresas.html) del Servicio de Impuestos internos.

## Funcionamiento

El archivo `importar y limpiar sii.r` permite descargar todos los datos estadísticos del SII directamente en R, y luego limpiar y consolidar estos datos en formato _tidy._

El archivo `precalcular sii.r` toma las bases que genera el archivo anterior, y calcula múltiples resúmenes de información que se guardan en una lista. El objetivo es resumir las bases de datos del SII y simplificar su información en varias bases más pequeñas y livianas. En la estrucura de este archivo se pueden calcular fácilmente la mayoría de los datos relevantes que entregan las bases del SII, efectivamente traduciendo las decenas de bases enormes y desordenadas del SII en dataframes pequeños y digeribles.

Posteriormente, estos datos precalculados, entre otros, alimentan un visualizador programado en Shiny que presenta la información de manera atractiva y eficiente. El visualizador (en constante desarrollo) puede verse en el siguiente enlace: http://dataintelligence.cl/shiny/dataemprende

## Dependencias

- `dplyr`
- `ggplot2`
- `shiny`
- `tidyverse` (sólo para el preprocesamiento de datos)
- `osmdata`, `sf` y `ggmap` para obtener información geográfica de la región desde Open Street Map
- `aos` (para las animaciones de los elementos del visualizador)
- `shinycssloaders` (animaciones mientras se cargan los gráficos)
- `shinyWidgets` (elementos de interfaz de usuario avanzados)

## Créditos

- Procesamiento de datos y programación: [Bastián Olea Herrera](http://bastian.olea.biz) (@bastimapache)
- Datos georreferenciados de empresas: Claudio Alarcón y Diego Muñoz
- Dirección: Alexis Alvear, Francisco Fortes
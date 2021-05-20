#http://dataintelligence.cl/shiny/dataemprende_datos

library(dplyr)

shinyUI(fluidPage(#shinymaterial::material_page(#
  #shinyanimate::withAnim(), #animación shinyanimate
  aos::use_aos(), #animación en scroll
  includeCSS("estilos.css"), #estilos css
  #tags$link(rel = "stylesheet", type = "text/css", href = "estilos.css"),
  
  fluidRow(
    column(12, class = "fondo",
           #style = "padding-bottom: 30px;",
           
           br(),br(),
           
           div(
             h1("DataEmprende", style = "display:inline-block;"),
             div("Tarapacá", class = "h1b", style = "display:inline-block;")
           ) %>% 
             aos(animation = "fade-down", duration = "2000"),
           
           br(),br(),
           
           p("Para asesorarte, necesitamos que respondas algunas preguntas...") %>% 
             aos(animation = "fade-down", duration = "2000", delay = 400),
           
           espaciador(),
    ),
    
  ),
  
  #preguntas ----
  
  espaciador(),
  
  fluidRow(
    
    column(12,
           
           div(id = "selectores",
               #input$comuna
               selectInput(inputId = "comuna", 
                           label = "Comuna donde se ubica su negocio o emprendimiento", 
                           choices = comunas_sii,
                           width = "100%"),
               
               #input$rubro
               selectInput(inputId = "rubro", 
                           label = "Rubro principal de su negocio o emprendimiento", 
                           choices = rubros_sii,
                           width = "100%"),
               #input$subrubro
               selectInput(inputId = "subrubro", 
                           label = "Subrubro de su negocio o emprendimiento", 
                           choices = NULL,
                           width = "100%"),
               
           )
    ) %>% aos(animation = "fade-down", duration = "2000", delay = 0),
    
  ),
  
  espaciador(),
  
  #resultados ----
  
  fluidRow(
    column(12,
           #párrafo empresas/rubros y tramos
           htmlOutput("parrafo1")%>% aos(animation = "fade-down", delay = "0"),
           br(),br(),
           #párrafo tramos/comuna, tramos/rubro
           htmlOutput("parrafo2")%>% aos(animation = "fade-down", delay = "100"),
           br(),br(),
           htmlOutput("parrafo3")%>% aos(animation = "fade-down", delay = "200"),
           br(),br(),
           htmlOutput("parrafo4")%>% aos(animation = "fade-down", delay = "300"),
           br(),br(),
           htmlOutput("parrafo5")%>% aos(animation = "fade-down", delay = "400"),
    ),
    
  ),
  
  espaciador(),
  
  # empresas ----
  fluidRow(
    column(12,
           h2("Empresas") %>% aos(animation = "fade-down", duration = "1000"),
           br(),
           
           #gráfico de logos de empresas en tres filas
           plotOutput("g_empresas_comuna", height = "200px") %>% 
             aos(animation = "fade-down"),
           
           br(),
           br(),
           
           #crecimiento del subrubro
             p("En comparación con", 
               style = "display:inline-block; vertical-align: middle;"),
             div(
               selectInput(inputId = "comparacion_crecimiento", 
                           label = NULL,
                           choices = c("el año pasado" = 1,
                                       "cinco años atrás" = 5,
                                       "diez años atrás" = 10),
                           width = "100%"),
               style = "display:inline-block; width: 160px; margin-left: 8px; margin-right: 8px; vertical-align: middle;"),
             
             div(
               htmlOutput("crecimiento_subrubros_empresas"),
               style = "display:inline-block; vertical-align: middle; margin-bottom: 15px;"),
           
           br(),
           br(),
           
           #grafico cantidad empresas ----
           plotOutput("g_cantidad_empresas", height = "300px") %>% 
             aos(animation = "fade-down"),
           
    ) 
  ),
  
  espaciador(),
  
  # trabajadores ----
  fluidRow(
    column(12,
           h2("Trabajadores") %>% aos(animation = "fade-down", duration = "1000"),
           br(),
           
           #gráfico de logos del género de trabajadores de la comuna
           plotOutput("g_trabajadores_comuna", height = "50px") %>% 
             aos(animation = "fade-down"),
           br(),br(),
           
           #gráfico de logos del género de trabajadores de la comuna por el rubro
           plotOutput("g_trabajadores_comuna_rubro", height = "50px") %>% 
             aos(animation = "fade-down"),
    )
  ),
  
  espaciador(),
  
  # ventas ----
  fluidRow(
    column(12,
           h2("Ventas") %>% aos(animation = "fade-down", duration = "1000"),
    )
  ),
  
  espaciador(),
  
)
)

#http://dataintelligence.cl/shiny/dataemprende_datos

library(dplyr)

shinyUI(fluidPage(#shinymaterial::material_page(#
  #shinyanimate::withAnim(), #animación shinyanimate
  aos::use_aos(), #animación en scroll
  includeScript("scripts.js"),
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
                           selected = "Iquique",
                           width = "100%"),
               
               #input$rubro
               selectInput(inputId = "rubro", 
                           label = "Rubro principal de su negocio o emprendimiento", 
                           choices = c("", rubros_sii),
                           width = "100%"),
               #input$subrubro
               selectInput(inputId = "subrubro", 
                           label = "Subrubro de su negocio o emprendimiento", 
                           choices = NULL,
                           width = "100%"),
               
               conditionalPanel("input.rubro == '' || input.subrubro == ''",
                                #id = "condicional_rubro_elegido",
                                em("Por favor, rellene todos los campos")
               )
           )
    ) %>% aos(animation = "fade-down", duration = "2000", delay = 0),
    
  ),
  
  espaciador(),
  
  #—----
  #resultados ----
  
  fluidRow(
    column(12,
           
           h3("Resumen") %>% aos(animation = "fade-down", duration = "1000"),
           br(),
           
           conditionalPanel("input.rubro != '' && input.subrubro != ''",
                            id = "condicional_rubro_elegido",
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
                            htmlOutput("parrafo5")%>% aos(animation = "fade-down", delay = "400")
           ),
    ),
    
  ),
  
  espaciador(),
  
  #—----
  # EMPRESAS ----
  fluidRow(
    column(12,
           h2("Empresas") %>% aos(animation = "fade-down", duration = "1000"),
           br(),
           
           #textos ----
           conditionalPanel("input.rubro != '' && input.subrubro != ''",
                            id = "condicional_rubro_elegido",
                            #párrafo empresas/rubros y tramos
                            htmlOutput("t_empresas_rubro_1")%>% aos(animation = "fade-down", delay = "0"),
                            br(),br()
           ),
           #cantidad de empresas
           #cuántas del rubro hay en la región
            #porcentaje de las empresas  
           #cuántas del rubro hay en la comuna
            #porcentaje de las empresas  
           
           #como se divide el rubro (porcentaje empresas por subrubros)
           
           #principales actividades del rubro/subrubro
           
           #casitas empresas ----
           h3("Tamaño de las empresas") %>%
             aos(animation = "fade-down", delay = "0"),
           br(),
           
           conditionalPanel("input.rubro != '' && input.subrubro != ''",
                            id = "condicional_rubro_elegido",
                            #gráfico de logos de empresas en tres filas
                            plotOutput("g_empresas_comuna", height = "200px") %>% 
                              aos(animation = "fade-down", delay = "0")
           ),
    )),
  
  espaciador(),
  
  #crecimiento del subrubro ----
  fluidRow(
    column(12,
           h3("Crecimiento de las empresas del subrubro") %>%
             aos(animation = "fade-down", delay = "0"),
           br(),
           
           conditionalPanel("input.rubro != '' && input.subrubro != ''",
                            id = "condicional_rubro_elegido",
                            div(
                              p("En comparación con", 
                                style = "display:inline-block; vertical-align: middle;"),
                              div(
                                selectInput(inputId = "comparacion_crecimiento", 
                                            label = NULL,
                                            choices = c("el año pasado" = 1,
                                                        "cinco años atrás" = 5,
                                                        "diez años atrás" = 10),
                                            width = "100%"),
                                style = "display:inline-block; width: 160px; margin-bottom: 0px; margin-top: 0px; margin-left: 8px; margin-right: 8px; vertical-align: middle;"),
                              
                              div(
                                htmlOutput("crecimiento_subrubros_empresas"),
                                style = "display:inline-block; vertical-align: middle; margin-top: 0px; margin-bottom: 18px;"),
                            ) %>% 
                              aos(animation = "fade-down", delay = "0")
           )
    )
  ),
  espaciador(),
  
  #grafico crecimiento empresas rubro ----
  fluidRow(
    column(12,
           h3("Evolución de las empresas del rubro") %>%
             aos(animation = "fade-down", delay = "0"),
           br(),
           
           conditionalPanel("input.rubro != '' && input.subrubro != ''",
                            id = "condicional_rubro_elegido",
                            
                            shinyWidgets::radioGroupButtons("selector_g_crecimiento_empresas_rubro",
                                                            label = NULL,
                                                            choices = c("Región", "Comuna"),
                                                            selected = "Región",
                                                            justified = TRUE,
                                                            width = "90%") %>%
                              aos(animation = "fade-down", delay = "100"),
                            
                            plotOutput("g_crecimiento_empresas_rubro", height = "300px") %>% 
                              aos(animation = "fade-down", delay = "200")
           ),
    )
  ),
  
  espaciador(),
  
  #grafico crecimiento empresas subrubro ----
  fluidRow(
    column(12,
           h3("Evolución de las empresas del subrubro") %>%
             aos(animation = "fade-down", delay = "0"),
           br(),
           
           conditionalPanel("input.rubro != '' && input.subrubro != ''",
                            id = "condicional_rubro_elegido",
                            
                            shinyWidgets::radioGroupButtons("selector_g_crecimiento_empresas_subrubro",
                                                            label = NULL,
                                                            choices = c("Región", "Comuna"),
                                                            selected = "Región",
                                                            justified = TRUE,
                                                            width = "90%") %>%
                              aos(animation = "fade-down", delay = "100"),
                            
                            plotOutput("g_crecimiento_empresas_subrubro", height = "300px") %>% 
                              aos(animation = "fade-down", delay = "200")
           ),
    )
  ),
  
  
  espaciador(),
  
  #mapa empresas rubro ----
  fluidRow(#style = "margin-right: 0px; padding-right: 0px; float: right; border: 0px; align-items: right;border-radius: 0px;",
    column(7,
           h3("Ubicación de las empresas") %>% 
             aos(animation = "fade-down", delay = "0"),
           br(),
           
           conditionalPanel("input.rubro != '' && input.subrubro != ''",
                            id = "condicional_rubro_elegido",
                            
                            shinyWidgets::radioGroupButtons("selector_m_iquique_empresas_rubro",
                                                            label = NULL,
                                                            choices = c("Rubro", "Subrubro"),
                                                            selected = "Rubro",
                                                            justified = TRUE,
                                                            width = "90%"),
                            #texto descriptivo
                            
                            #texto del ciiu que se muestra a partir del subrubro
                            conditionalPanel("input.selector_m_iquique_empresas_rubro == 'Subrubro'",
                                             id = "condicional_rubro_elegido",
                                             textOutput("subrubro_en_ciiu")
                            )
           )
    ),
    
    column(5, #style = "margin-right: 0px; padding-right: 0px; float: right; border: 0px; align-items: right;border-radius: 0px;",
           conditionalPanel("input.rubro != '' && input.subrubro != ''",
                            id = "condicional_rubro_elegido",
                            
                            plotOutput("m_iquique_empresas_rubro", height = "400px", width = "359px") %>% #width exacto para que no tenga borde blanco
                              aos(animation = "fade-down", delay = "0") %>%
                              shinycssloaders::withSpinner(proxy.height = "400px", color = color_oscuro, color.background = color_fondo) #%>%
                            #aos(animation = "fade-down"),
                            #poner hacia el lado derecho, ojalá fondo oscuro hacia esa esquina
           ) 
    )
  ),
  
  espaciador(),
  
  
  #grafico barras empresas subrubro ----
  fluidRow(
    column(12,
           h3("Empresas por subrubro") %>%
             aos(animation = "fade-down", delay = "0"),
           br(),
           
           conditionalPanel("input.rubro != '' && input.subrubro != ''",
                            id = "condicional_rubro_elegido",
                            
                            shinyWidgets::radioGroupButtons("selector_g_barras_empresas_subrubro",
                                                            label = NULL,
                                                            choices = c("Región", "Comuna"),
                                                            selected = "Región",
                                                            justified = TRUE,
                                                            width = "90%") %>%
                              aos(animation = "fade-down", delay = "100"),
                            
                            plotOutput("g_barras_empresas_subrubro", height = "300px") %>% 
                              aos(animation = "fade-down", delay = "200")
           ),
    )
  ),
  
  espaciador(),
  
  #—----
  # TRABAJADORES ----
  fluidRow(
    column(12,
           h2("Trabajadores") %>% aos(animation = "fade-down", duration = "1000"),
           br(),
           
           conditionalPanel("input.rubro != '' && input.subrubro != ''",
                            id = "condicional_rubro_elegido",
                            #gráfico de logos del género de trabajadores de la comuna
                            plotOutput("g_trabajadores_comuna", height = "50px") %>% 
                              aos(animation = "fade-down"),
                            br(),br(),
                            
                            #gráfico de logos del género de trabajadores de la comuna por el rubro
                            plotOutput("g_trabajadores_comuna_rubro", height = "50px") %>% 
                              aos(animation = "fade-down")
           )
    )
  ),
  
  espaciador(),
  
  #—----
  # VENTAS ----
  fluidRow(
    column(12,
           h2("Ventas") %>% aos(animation = "fade-down", duration = "1000"),
    )
  ),
  
  espaciador(),
  
)
)

#http://dataintelligence.cl/shiny/dataemprende_datos

library(dplyr)

shinyUI(fluidPage(
  aos::use_aos(), #animación en scroll
  includeScript("scripts.js"), #javascript de animación fade para conditional panels
  includeCSS("estilos.css"), #estilos css
  
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
  #resumen ----
  
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
                            htmlOutput("t_empresas_rubro_1") %>% aos(animation = "fade-down", delay = "0"),
                            br(), br(),
                            htmlOutput("t_empresas_rubro_2") %>% aos(animation = "fade-down", delay = "0"),
                            
           )
    )
  ),
  #cantidad de empresas
  #cuántas del rubro hay en la región OK
  #porcentaje de las empresas   OK 
  #cuántas del rubro hay en la comuna OK mapa
  #barras
  #porcentaje de las empresas  
  
  #como se divide el rubro (porcentaje empresas por subrubros)
  
  #principales actividades del rubro/subrubro
  
  espaciador_interior(),
  
  #mapa empresas comuna ----
  fluidRow(
    column(12,
           h3("Empresas del rubro por comuna"),
    ),       
    column(6,                 
           #mapa
           conditionalPanel("input.rubro != '' && input.subrubro != ''",
                            id = "condicional_rubro_elegido",
                            
                            #titulo del rubro/subrubro
                            htmlOutput("rubro_elegido_6") %>%
                              aos(animation = "fade-down", delay = "100"),
                            br(),
                            
                            plotOutput("m_empresas_comuna", height = "400px") %>% #width exacto para que no tenga borde blanco
                              aos(animation = "fade-down", delay = "0") %>%
                              shinycssloaders::withSpinner(proxy.height = "400px", color = color_oscuro, color.background = color_fondo)
           )
    ),
    
    #barras empresas comuna ----
    column(6,
           #barras con botonera
           conditionalPanel("input.rubro != '' && input.subrubro != ''",
                            id = "condicional_rubro_elegido",
                            #boronera
                            shinyWidgets::radioGroupButtons("selector_g_barras_empresas_rubro",
                                                            label = NULL,
                                                            choices = c("Rubro", "Subrubro"),
                                                            selected = "Rubro",
                                                            justified = TRUE,
                                                            width = "90%") %>%
                              aos(animation = "fade-down", delay = "100"),
                            
                            #titulo del rubro/subrubro
                            htmlOutput("rubro_subrubro_elegido_1") %>%
                              aos(animation = "fade-down", delay = "100"),
                            br(),
                            
                            #grafico de barras
                            plotOutput("g_barras_empresas_rubro_comuna", height = "300px") %>% 
                              aos(animation = "fade-down", delay = "200"),
           )
    )
  ),
  
  espaciador_interior(),
  
  
  #casitas empresas ----
  fluidRow(
    column(12,
  h3("Tamaño de las empresas en la comuna") %>%
    aos(animation = "fade-down", delay = "0"),
  br(),
  
  conditionalPanel("input.rubro != '' && input.subrubro != ''",
                   id = "condicional_rubro_elegido",
                   #gráfico de logos de empresas en tres filas
                   plotOutput("g_empresas_comuna", height = "200px") %>% 
                     aos(animation = "fade-down", delay = "0")
  ),
)
),


espaciador_interior(),

#texto crecimiento del subrubro ----
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
espaciador_interior(),


#grafico crecimiento empresas rubro ----
fluidRow(
  column(12,
         h3("Evolución de las empresas del rubro") %>%
           aos(animation = "fade-down", delay = "0"),
         br(),
         
         conditionalPanel("input.rubro != '' && input.subrubro != ''",
                          id = "condicional_rubro_elegido",
                          
                          #titulo del rubro elegido
                          htmlOutput("rubro_elegido_1") %>% 
                            aos(animation = "fade-down", delay = "0"),
                          br(),
                          
                          #botonera
                          shinyWidgets::radioGroupButtons("selector_g_crecimiento_empresas_rubro",
                                                          label = NULL,
                                                          choices = c("Región", "Comuna"),
                                                          selected = "Región",
                                                          justified = TRUE,
                                                          width = "90%") %>%
                            aos(animation = "fade-down", delay = "100"),
                          
                          #grafico de degradado
                          plotOutput("g_crecimiento_empresas_rubro", height = "300px") %>% 
                            aos(animation = "fade-down", delay = "200")
         ),
  )
),

espaciador_interior(),

#grafico crecimiento empresas subrubro ----
fluidRow(
  column(12,
         h3("Evolución de las empresas del subrubro") %>%
           aos(animation = "fade-down", delay = "0"),
         br(),
         
         conditionalPanel("input.rubro != '' && input.subrubro != ''",
                          id = "condicional_rubro_elegido",
                          
                          #titulo del subrubro
                          htmlOutput("subrubro_elegido_1") %>% 
                            aos(animation = "fade-down", delay = "0"),
                          br(),
                          
                          #botonera
                          shinyWidgets::radioGroupButtons("selector_g_crecimiento_empresas_subrubro",
                                                          label = NULL,
                                                          choices = c("Región", "Comuna"),
                                                          selected = "Región",
                                                          justified = TRUE,
                                                          width = "90%") %>%
                            aos(animation = "fade-down", delay = "100"),
                          
                          #gráfico de degradado
                          plotOutput("g_crecimiento_empresas_subrubro", height = "300px") %>% 
                            aos(animation = "fade-down", delay = "200")
         ),
  )
),


espaciador_interior(),

#mapa empresas rubro ----
fluidRow(#style = "margin-right: 0px; padding-right: 0px; float: right; border: 0px; align-items: right;border-radius: 0px;",
  column(7,
         h3("Ubicación de las empresas") %>% 
           aos(animation = "fade-down", delay = "0"),
         br(),
         
         conditionalPanel("input.rubro != '' && input.subrubro != ''",
                          id = "condicional_rubro_elegido",
                          
                          #botonera rubro/subrubro
                          shinyWidgets::radioGroupButtons("selector_m_iquique_empresas_rubro",
                                                          label = NULL,
                                                          choices = c("Rubro", "Subrubro"),
                                                          selected = "Rubro",
                                                          justified = TRUE,
                                                          width = "90%"),
                          
                          #texto del ciiu que se muestra a partir del subrubro#titulo del rubro/subrubro
                          htmlOutput("rubro_subrubro_elegido_2") %>%
                            aos(animation = "fade-down", delay = "100"),
                          br(),
         )
  ),
  
  column(5, 
         conditionalPanel("input.rubro != '' && input.subrubro != ''",
                          id = "condicional_rubro_elegido",
                          
                          #mapa
                          plotOutput("m_iquique_empresas_rubro", height = "400px", width = "359px") %>% #width exacto para que no tenga borde blanco
                            aos(animation = "fade-down", delay = 0) %>%
                            shinycssloaders::withSpinner(proxy.height = "400px", color = color_oscuro, color.background = color_fondo),
                          
                          #selector para el zoom
                          br(),
                          selectInput(inputId = "zoom_m_iquique_empresas_rubro", 
                                      label = "Acercamiento:",
                                      choices = c("Iquique y Alto Hospicio",
                                                  "Iquique norte",
                                                  "Iquique centro",
                                                  "Iquique sur",
                                                  "Alto Hospicio"),
                                      selected = "Iquique y Alto Hospicio",
                                      width = "100%") %>%
                            aos(animation = "fade-down", delay = 0),
                          br(),br(),
         ) 
  )
),

espaciador_interior(),


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
                          
                          plotOutput("g_barras_empresas_subrubro", height = "350px") %>% 
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
         
         #gráfico género ----
         conditionalPanel("input.rubro != '' && input.subrubro != ''",
                          id = "condicional_rubro_elegido",
                          
                          #gráfico de logos del género de trabajadores de la comuna
                          h3("Distribución de género promedio en la comuna"),
                          
                          plotOutput("g_trabajadores_comuna", height = "120px") %>% 
                            aos(animation = "fade-down"),
                          
                          espaciador_interior(),
                          
                          #gráfico de logos del género de trabajadores de la comuna por el rubro
                          h3("Distribución de género del rubro en la comuna"),
                          plotOutput("g_trabajadores_comuna_rubro", height = "120px") %>% 
                            aos(animation = "fade-down")
         )
  )
),

espaciador_interior(),


#grafico crecimiento rubro ----
fluidRow(
  column(12,
         h3("Evolución de los trabajadores del rubro") %>%
           aos(animation = "fade-down", delay = "0"),
         br(),
         
         conditionalPanel("input.rubro != '' && input.subrubro != ''",
                          id = "condicional_rubro_elegido",
                          
                          #titulo del rubro elegido
                          htmlOutput("rubro_elegido_3") %>% 
                            aos(animation = "fade-down", delay = "0"),
                          br(),
                          
                          #botonera
                          shinyWidgets::radioGroupButtons("selector_g_crecimiento_trabajadores_rubro",
                                                          label = NULL,
                                                          choices = c("Región", "Comuna"),
                                                          selected = "Región",
                                                          justified = TRUE,
                                                          width = "90%") %>%
                            aos(animation = "fade-down", delay = "100"),
                          
                          #grafico de degradado
                          plotOutput("g_crecimiento_trabajadores_rubro", height = "300px") %>% 
                            aos(animation = "fade-down", delay = "200")
         ),
  )
),

espaciador_interior(),

#grafico crecimiento subrubro ----
fluidRow(
  column(12,
         h3("Evolución de los trabajadores del subrubro") %>%
           aos(animation = "fade-down", delay = "0"),
         br(),
         
         conditionalPanel("input.rubro != '' && input.subrubro != ''",
                          id = "condicional_rubro_elegido",
                          
                          #titulo del rubro elegido
                          htmlOutput("subrubro_elegido_3") %>% 
                            aos(animation = "fade-down", delay = "0"),
                          br(),
                          
                          #botonera
                          shinyWidgets::radioGroupButtons("selector_g_crecimiento_trabajadores_subrubro",
                                                          label = NULL,
                                                          choices = c("Región", "Comuna"),
                                                          selected = "Región",
                                                          justified = TRUE,
                                                          width = "90%") %>%
                            aos(animation = "fade-down", delay = "100"),
                          
                          #grafico de degradado
                          plotOutput("g_crecimiento_trabajadores_subrubro", height = "300px") %>% 
                            aos(animation = "fade-down", delay = "200")
         ),
  )
),

espaciador_interior(),

#grafico dependencia rubro ----
fluidRow(
  column(12,
         h3("Trabajadores por dependencia") %>%
           aos(animation = "fade-down", delay = "0"),
         br(),
         #barras con botonera
         conditionalPanel("input.rubro != '' && input.subrubro != ''",
                          id = "condicional_rubro_elegido",
                          #boronera
                          shinyWidgets::radioGroupButtons("selector_g_trabajadores_dependencia",
                                                          label = NULL,
                                                          choices = c("Rubro", "Subrubro"),
                                                          selected = "Rubro",
                                                          justified = TRUE,
                                                          width = "90%") %>%
                            aos(animation = "fade-down", delay = "100"),
                          
                          #titulo del rubro/subrubro
                          htmlOutput("rubro_subrubro_elegido_3") %>%
                            aos(animation = "fade-down", delay = "100"),
                          br(),
                          
                          #grafico de barras
                          plotOutput("g_trabajadores_dependencia", height = "300px") %>% 
                            aos(animation = "fade-down", delay = "200"),
         )
  )
),

espaciador(),

#—----
# VENTAS ----
fluidRow(
  column(12,
         h2("Ventas") %>% aos(animation = "fade-down", duration = "1000"),
         br(),
  )
),


#grafico crecimiento rubro ----
fluidRow(
  column(12,
         h3("Evolución de ventas del rubro") %>%
           aos(animation = "fade-down", delay = "0"),
         br(),
         
         conditionalPanel("input.rubro != '' && input.subrubro != ''",
                          id = "condicional_rubro_elegido",
                          
                          #titulo del rubro elegido
                          htmlOutput("rubro_elegido_4") %>% 
                            aos(animation = "fade-down", delay = "0"),
                          br(),
                          
                          #botonera
                          shinyWidgets::radioGroupButtons("selector_g_crecimiento_ventas_rubro",
                                                          label = NULL,
                                                          choices = c("Región", "Comuna"),
                                                          selected = "Región",
                                                          justified = TRUE,
                                                          width = "90%") %>%
                            aos(animation = "fade-down", delay = "100"),
                          
                          #grafico de degradado
                          plotOutput("g_crecimiento_ventas_rubro", height = "300px") %>% 
                            aos(animation = "fade-down", delay = "200")
         ),
  )
),

espaciador_interior(),

#grafico crecimiento subrubro ----
fluidRow(
  column(12,
         h3("Evolución de ventas del subrubro") %>%
           aos(animation = "fade-down", delay = "0"),
         br(),
         
         conditionalPanel("input.rubro != '' && input.subrubro != ''",
                          id = "condicional_rubro_elegido",
                          
                          #titulo del rubro elegido
                          htmlOutput("subrubro_elegido_4") %>% 
                            aos(animation = "fade-down", delay = "0"),
                          br(),
                          
                          #botonera
                          shinyWidgets::radioGroupButtons("selector_g_crecimiento_ventas_subrubro",
                                                          label = NULL,
                                                          choices = c("Región", "Comuna"),
                                                          selected = "Región",
                                                          justified = TRUE,
                                                          width = "90%") %>%
                            aos(animation = "fade-down", delay = "100"),
                          
                          #grafico de degradado
                          plotOutput("g_crecimiento_ventas_subrubro", height = "300px") %>% 
                            aos(animation = "fade-down", delay = "200")
         ),
  )
),

espaciador_interior(),

#grafico horizontal rubros mayores ventas ----
#rubros con mayores ventas anuales promedio en la comuna elegida
fluidRow(
  column(12,
         h3("Rubros con mayores ventas") %>%
           aos(animation = "fade-down", delay = "0"),
         br(),
         
         conditionalPanel("input.rubro != '' && input.subrubro != ''",
                          id = "condicional_rubro_elegido",
                          
                          #titulo del rubro elegido
                          # htmlOutput("rubro_elegido_5") %>% 
                          #   aos(animation = "fade-down", delay = "0"),
                          em("Cifras en millones de pesos.")%>% 
                            aos(animation = "fade-down", delay = "0"),
                           br(), br(),
                          
                          #botonera
                          shinyWidgets::radioGroupButtons("selector_g_mayores_ventas_rubro",
                                                          label = NULL,
                                                          choices = c("Región", "Comuna"),
                                                          selected = "Región",
                                                          justified = TRUE,
                                                          width = "90%") %>%
                            aos(animation = "fade-down", delay = "100"),
                          
                          #grafico de degradado
                          plotOutput("g_mayores_ventas_rubro", height = "300px") %>% 
                            aos(animation = "fade-down", delay = "200")
         ),
  )
),

espaciador_interior(),

#grafico horizontal subrubros mayores ventas ----
fluidRow(
  column(12,
         h3("Sububros del rubro elegido con mayores ventas") %>%
           aos(animation = "fade-down", delay = "0"),
         br(),
         
         conditionalPanel("input.rubro != '' && input.subrubro != ''",
                          id = "condicional_rubro_elegido",
                          
                          #titulo del subrubro elegido
                          htmlOutput("rubro_elegido_5") %>% 
                            aos(animation = "fade-down", delay = "0"),
                          br(),
                          em("Cifras en millones de pesos.") %>% 
                            aos(animation = "fade-down", delay = "100"),
                          br(), br(),
                          
                          #botonera
                          shinyWidgets::radioGroupButtons("selector_g_mayores_ventas_subrubro",
                                                          label = NULL,
                                                          choices = c("Región", "Comuna"),
                                                          selected = "Región",
                                                          justified = TRUE,
                                                          width = "90%") %>%
                            aos(animation = "fade-down", delay = "100"),
                          
                          #grafico de degradado
                          plotOutput("g_mayores_ventas_subrubro", height = "300px") %>% 
                            aos(animation = "fade-down", delay = "200")
         ),
  )
),

espaciador(),



)
)

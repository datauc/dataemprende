#http://dataintelligence.cl/shiny/dataemprende

shinyUI(
  fluidPage(title = "DataEmprende Tarapacá", lang = "es",
            aos::use_aos(), #animación en scroll
            includeScript("scripts.js"), #javascript de animación fade para conditional panels
            includeCSS("estilos.css"), #estilos css
            shinyjs::useShinyjs(),
            tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"), #fontAwesome 5,7
            
            
            #header ----
            fluidRow(
              column(12, class = "fondo",
                     style = "padding-bottom: 30px;",
                     
                     div(
                       img(src = "logos.svg",
                           height = 120, 
                           style = "padding: 5px; margin-left: -15px;"), #%>% 
                       #aos(animation = "fade-down", duration = "1000"),
                       
                       br(),
                       
                       div(
                         h1("DataEmprende", style = "display:inline-block;"),
                         div("Tarapacá", class = "h1b", style = "display:inline-block;")), #%>% 
                       #aos(animation = "fade-down", duration = "2000", delay = 800),
                       
                       br(),
                       
                       p("Para asesorarte, necesitamos que respondas algunas preguntas..."), #%>% 
                       #aos(animation = "fade-down"),
                       
                       #espaciador(),
                       #espaciador_interior(),
                       br(), br(), br(), 
                       
                       p('“Proyecto financiado por el Gobierno Regional de Tarapacá a través del Fondo de Innovación para la Competitividad FIC 2017”',
                         style = "font-size: 85%; opacity: 0.6;"), 
                       #aos(animation = "fade-down", duration = "2000", delay = 1600),
                     ) %>% 
                       aos(animation = "fade-down", duration = "2000"),
              ),
              
            ),
            
            
            
            
            fluidRow(
              #sidebar ----
              column(4, #columna del sidebar, lado izquierdo
                     style = glue::glue("background-color: {color_medio_2}; 
                                        margin-top: 0px;"), #para que se pegue al header
                     
                     
                     desliza_para_continuar(),
                     
                     #espaciador(),
                     div(
                       br(), #br(),
                       #br(), br(),
                       #p(" ")
                     ),
                     
                     #div con los tres selectores
                     div(id = "selectores",
                         #class = "fade_in",
                         
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
                                     #choices = c(rubros_sii),
                                     width = "100%"),
                         #input$subrubro
                         selectInput(inputId = "subrubro", 
                                     label = "Subrubro de su negocio o emprendimiento", 
                                     choices = NULL,
                                     width = "100%"),
                         
                         #texto de aviso de campo incompleto
                         conditionalPanel("input.rubro == '' || input.subrubro == ''",
                                          id = "condicional_rubro_elegido",
                                          em("Por favor, rellene todos los campos")
                         )
                     ) %>% 
                       aos(animation = "fade-down", duration = "2000", delay = 400), #animación selectores
                     
                     #cuadro con degradado debajo de los selectores
                     div(
                       style = paste0("background: rgb(69,123,157);",
                                      "background: linear-gradient(0deg, rgba(69,123,157,1) 0%, rgba(57,113,150,1) 100%);",
                                      "margin-left: -15px;",
                                      "margin-right: -15px;",
                                      "margin-bottom: -20px;"), #espaciados porque la column tiene un margen interior
                       espaciador(),
                     )
              ), 
              
              #—----
              
              column(8, #columna del lado derecho, todo el contenido
                     
                     #el gran condicional: oculta toda la página si no se han seleccionado rubros
                     conditionalPanel(selector_rubro_no_vacio,
                                      id = "condicional_rubro_elegido",
                     
                     #pestañas ----
                     
                     #espaciador(),
                     div(
                       br(), br(),
                       #br(), br(),
                       #p(" ")
                     ),
                     
                     #espaciador_interior(), 
                     
                     tabsetPanel(id = "tabs", type="pills", 
                                 
                                 
                                 
                                 
                                 #RESUMEN
                                 # tabPanel(title=HTML("&nbsp;&nbsp;&nbsp;&nbsp;Resumen&nbsp;&nbsp;&nbsp;&nbsp;"), 
                                 #          
                                 #          #fotos(1),
                                 # ), #end pestaña
                                 
                                 
                                 
                                 
                                 #—----
                                 # EMPRESAS ----
                                 tabPanel(title=HTML("&nbsp;&nbsp;&nbsp;&nbsp;Empresas&nbsp;&nbsp;&nbsp;&nbsp;"), 
                                          
                                          #resumen ----
                                          fluidRow(
                                            column(12,
                                                   
                                                   desliza_para_continuar(),
                                                   
                                                   espaciador_interior(),
                                                   
                                                   div(class="fade_in",
                                                       h2("Resumen") #%>% 
                                                       #aos(animation = "fade-down", delay = "2000", duration = "1000"),
                                                   ),
                                                   
                                                   br(),
                                                   
                                                   conditionalPanel(selector_rubro_no_vacio,
                                                                    id = "condicional_rubro_elegido",
                                                                    
                                                                    div(class = "fade_in",
                                                                        #párrafo empresas/rubros y tramos
                                                                        htmlOutput("parrafo1") %>% 
                                                                          aos(animation = "fade-down", delay = "0") %>% 
                                                                          shinycssloaders::withSpinner(proxy.height = "400px", color = color_oscuro, color.background = color_fondo),
                                                                        br(),br(),
                                                                        
                                                                        #párrafo tramos/comuna, tramos/rubro
                                                                        htmlOutput("parrafo2") %>% 
                                                                          aos(animation = "fade-down", delay = "100") %>% 
                                                                          shinycssloaders::withSpinner(proxy.height = "400px", color = color_oscuro, color.background = color_fondo),
                                                                        br(),br(),
                                                                        
                                                                        htmlOutput("parrafo3") %>% 
                                                                          aos(animation = "fade-down", delay = "200") %>% 
                                                                          shinycssloaders::withSpinner(proxy.height = "400px", color = color_oscuro, color.background = color_fondo),
                                                                        br(),br(),
                                                                        
                                                                        htmlOutput("parrafo4") %>% 
                                                                          aos(animation = "fade-down", delay = "300") %>% 
                                                                          shinycssloaders::withSpinner(proxy.height = "400px", color = color_oscuro, color.background = color_fondo),
                                                                        br(),br(),
                                                                        
                                                                        htmlOutput("parrafo5") %>% 
                                                                          aos(animation = "fade-down", delay = "400") %>% 
                                                                          shinycssloaders::withSpinner(proxy.height = "400px", color = color_oscuro, color.background = color_fondo)
                                                                    )
                                                   ),
                                            ),
                                            
                                          ),
                                          
                                          espaciador_interior(),
                                          
                                          #empresas ----
                                          fluidRow(
                                            column(12,
                                                   
                                                   espaciador(),
                                                   
                                                   div(class = "fade_in",
                                                       h2("Empresas") #%>% 
                                                       #aos(animation = "fade-down", duration = "1000")
                                                   ),
                                                   
                                                   #br(),
                                                   espaciador_interior(),
                                                   
                                                   #textos ----
                                                   conditionalPanel(selector_rubro_no_vacio,
                                                                    id = "condicional_rubro_elegido",
                                                                    div(class = "fade_in",
                                                                        #párrafo empresas/rubros y tramos
                                                                        htmlOutput("t_empresas_rubro_1") %>% 
                                                                          aos(animation = "fade-down", delay = "0") %>% 
                                                                          shinycssloaders::withSpinner(proxy.height = "400px", color = color_oscuro, color.background = color_fondo),
                                                                        br(), br(),
                                                                        htmlOutput("t_empresas_rubro_2") %>% 
                                                                          aos(animation = "fade-down", delay = "0") %>% 
                                                                          shinycssloaders::withSpinner(proxy.height = "400px", color = color_oscuro, color.background = color_fondo),
                                                                    )
                                                   )
                                            )
                                          ),
                                          
                                          espaciador_interior(),
                                          
                                          
                                          
                                          
                                          #logos tamaño empresas ----
                                          fluidRow(
                                            column(12,
                                                   
                                                   
                                                   conditionalPanel(selector_rubro_no_vacio,
                                                                    id = "condicional_rubro_elegido",
                                                                    
                                                                    div(class = "fade_in",
                                                                        h3("Tamaño de las empresas en la comuna") %>%
                                                                          aos(animation = "fade-down", delay = "0"),
                                                                        br(),
                                                                        #gráfico de logos de empresas en tres filas
                                                                        plotOutput("g_empresas_comuna", height = "300px", width = "500px") %>% 
                                                                          aos(animation = "fade-down", delay = "0") %>% 
                                                                          shinycssloaders::withSpinner(proxy.height = "400px", color = color_oscuro, color.background = color_fondo)
                                                                    )
                                                   ),
                                            )
                                          ),
                                          
                                          espaciador_interior(),
                                          
                                          
                                          #mapa empresas comuna ----
                                          fluidRow(
                                            column(12,
                                                   h3("Empresas del rubro por comuna") %>% 
                                                     aos(animation = "fade-down", delay = "0"),
                                            ),       
                                            column(6,                 
                                                   #mapa
                                                   conditionalPanel(selector_rubro_no_vacio,
                                                                    id = "condicional_rubro_elegido",
                                                                    div(class = "fade_in",
                                                                        #titulo del rubro/subrubro
                                                                        htmlOutput("rubro_elegido_6") %>%
                                                                          aos(animation = "fade-down", delay = "100"),
                                                                        br(),
                                                                        br(),
                                                                        plotOutput("m_empresas_comuna", height = "400px") %>% #width exacto para que no tenga borde blanco
                                                                          aos(animation = "fade-down", delay = "0") %>%
                                                                          shinycssloaders::withSpinner(proxy.height = "400px", color = color_oscuro, color.background = color_fondo)
                                                                    ))
                                            ),
                                            
                                            #barras empresas comuna ----
                                            column(6,
                                                   #barras con botonera
                                                   conditionalPanel(selector_rubro_no_vacio,
                                                                    id = "condicional_rubro_elegido",
                                                                    div(class = "fade_in",
                                                                        br(), br(),
                                                                        #botonera
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
                                                                          aos(animation = "fade-down", delay = "200") %>% 
                                                                          shinycssloaders::withSpinner(proxy.height = "400px", color = color_oscuro, color.background = color_fondo)
                                                                    )
                                                   )
                                            )
                                          ),
                                          
                                          espaciador_interior(),
                                          
                                          #gráfico área tramos comuna ----
                                          fluidRow(
                                            column(12,
                                                   h3("Evolución del tamaño de las empresas") %>%
                                                     aos(animation = "fade-down", delay = "0"),
                                                   br(),
                                                   
                                                   conditionalPanel(selector_rubro_no_vacio,
                                                                    id = "condicional_rubro_elegido",
                                                                    
                                                                    #botonera
                                                                    shinyWidgets::radioGroupButtons("selector_g_empresas_area_comuna",
                                                                                                    label = NULL,
                                                                                                    choices = c("Comuna", "Región"),
                                                                                                    selected = "Región",
                                                                                                    justified = TRUE,
                                                                                                    width = "90%") %>%
                                                                      aos(animation = "fade-down", delay = "100"),
                                                                    
                                                                    #titulo de comuna elegida
                                                                    conditionalPanel("input.selector_g_empresas_area_comuna =='Comuna'",
                                                                                     htmlOutput("comuna_elegida_1") %>% 
                                                                                       aos(animation = "fade-down", delay = "0")
                                                                    ),
                                                                    
                                                                    br(),
                                                                    em("Excluyendo grandes empresas.") %>%
                                                                      aos(animation = "fade-down", delay = "100"),
                                                                    
                                                                    #gráfico de logos de empresas en tres filas
                                                                    plotOutput("g_empresas_area_comuna", height = "400px") %>% 
                                                                      aos(animation = "fade-down", delay = "200")
                                                   ),
                                            )
                                          ),
                                          
                                          espaciador_interior(),
                                          
                                          #gráfico área tramos rubro ----
                                          fluidRow(
                                            column(12,
                                                   h3("Evolución del tamaño de las empresas") %>%
                                                     aos(animation = "fade-down", delay = "0"),
                                                   br(),
                                                   
                                                   conditionalPanel(selector_rubro_no_vacio,
                                                                    id = "condicional_rubro_elegido",
                                                                    
                                                                    #botonera
                                                                    shinyWidgets::radioGroupButtons("selector_g_empresas_area_rubro",
                                                                                                    label = NULL,
                                                                                                    choices = c("Rubro", "Subrubro"),
                                                                                                    selected = "Rubro",
                                                                                                    justified = TRUE,
                                                                                                    width = "90%") %>%
                                                                      aos(animation = "fade-down", delay = "100"),
                                                                    
                                                                    
                                                                    #titulo del rubro
                                                                    htmlOutput("rubro_subrubro_elegido_6") %>% 
                                                                      aos(animation = "fade-down", delay = "0"),
                                                                    br(),
                                                                    
                                                                    em("Excluyendo grandes empresas.") %>%
                                                                      aos(animation = "fade-down", delay = "100"),
                                                                    
                                                                    #gráfico de logos de empresas en tres filas
                                                                    plotOutput("g_empresas_area_rubro", height = "400px") %>% 
                                                                      aos(animation = "fade-down", delay = "200")
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
                                                   
                                                   conditionalPanel(selector_rubro_no_vacio,
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
                                                   
                                                   conditionalPanel(selector_rubro_no_vacio,
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
                                                   
                                                   conditionalPanel(selector_rubro_no_vacio,
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
                                            column(12,
                                                   h3("Ubicación de las empresas") %>%
                                                     aos(animation = "fade-down", delay = "0"),
                                                   br(),
                                                   
                                                   conditionalPanel(selector_rubro_no_vacio,
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
                                                                    #)
                                                                    #),
                                                                    
                                                                    # column(5, 
                                                                    #        conditionalPanel(selector_rubro_no_vacio,
                                                                    #                         id = "condicional_rubro_elegido",
                                                                    
                                                                    #mapa
                                                                    plotOutput("m_iquique_empresas_rubro", 
                                                                               height = "500px"#, 
                                                                               #width = "359px"
                                                                    ) %>% #width exacto para que no tenga borde blanco
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
                                                     aos(animation = "fade-down", delay = 0),
                                                   br(),
                                                   
                                                   conditionalPanel(selector_rubro_no_vacio,
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
                                          
                                          #fotos(2),
                                          
                                          espaciador()
                                          
                                 ),
                                 
                                 #—----
                                 # TRABAJADORES ----
                                 tabPanel(title=HTML("&nbsp;&nbsp;&nbsp;&nbsp;Trabajadores&nbsp;&nbsp;&nbsp;&nbsp;"), 
                                          
                                          fluidRow(
                                            column(12,
                                                   
                                                   desliza_para_continuar(),
                                                   
                                                   espaciador_interior(),
                                                   
                                                   div(class="fade_in",
                                                       h2("Trabajadores")
                                                   ),
                                                   
                                                   # h2("Trabajadores") %>% 
                                                   #   aos(animation = "fade-down", duration = "1000"),
                                                   
                                                   #br(),
                                                   espaciador_interior(),
                                                   
                                                   #gráfico género ----
                                                   conditionalPanel(selector_rubro_no_vacio,
                                                                    id = "condicional_rubro_elegido",
                                                                    
                                                                    #gráfico de logos del género de trabajadores de la comuna
                                                                    h3("Distribución de género promedio en la comuna") %>%
                                                                      aos(animation = "fade-down", delay = "0"),
                                                                    
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
                                          
                                          #grafico crecimiento genero ----
                                          fluidRow(
                                            column(12,
                                                   h3("Evolución de trabajadores por género") %>%
                                                     aos(animation = "fade-down", delay = "0"),
                                                   br(),
                                                   
                                                   conditionalPanel(selector_rubro_no_vacio,
                                                                    id = "condicional_rubro_elegido",
                                                                    
                                                                    #titulo del rubro elegido
                                                                    htmlOutput("rubro_subrubro_elegido_4") %>% 
                                                                      aos(animation = "fade-down", delay = "0"),
                                                                    br(),
                                                                    
                                                                    #botonera
                                                                    shinyWidgets::radioGroupButtons("selector_rubro_g_trabajadores_crecimiento_genero",
                                                                                                    label = NULL,
                                                                                                    choices = c("Rubro", "Subrubro"),
                                                                                                    selected = "Rubro",
                                                                                                    justified = TRUE,
                                                                                                    width = "90%") %>%
                                                                      aos(animation = "fade-down", delay = "100"),
                                                                    # br(),
                                                                    # shinyWidgets::radioGroupButtons("selector_genero_g_trabajadores_crecimiento_genero",
                                                                    #                                 label = NULL,
                                                                    #                                 choices = c("Mujeres", "Hombres"),
                                                                    #                                 selected = "Mujeres",
                                                                    #                                 justified = TRUE,
                                                                    #                                 width = "90%") %>%
                                                                    #   aos(animation = "fade-down", delay = "200"),
                                                                    
                                                                    #grafico de degradado
                                                                    plotOutput("g_crecimiento_trabajadores_genero", height = "300px") %>% 
                                                                      aos(animation = "fade-down", delay = "200")
                                                   ),
                                            )
                                          ),
                                          
                                          espaciador_interior(),
                                          
                                          
                                          #grafico crecimiento rubro ----
                                          fluidRow(
                                            column(12,
                                                   h3("Evolución de los trabajadores del rubro") %>%
                                                     aos(animation = "fade-down", delay = "0"),
                                                   br(),
                                                   
                                                   conditionalPanel(selector_rubro_no_vacio,
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
                                                   
                                                   conditionalPanel(selector_rubro_no_vacio,
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
                                                   conditionalPanel(selector_rubro_no_vacio,
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
                                          
                                          espaciador_interior(),
                                          
                                          #grafico crecimiento dependencia ----
                                          fluidRow(
                                            column(12,
                                                   h3("Evolución de trabajadores por dependencia") %>%
                                                     aos(animation = "fade-down", delay = "0"),
                                                   br(),
                                                   
                                                   conditionalPanel(selector_rubro_no_vacio,
                                                                    id = "condicional_rubro_elegido",
                                                                    
                                                                    #titulo del rubro elegido
                                                                    htmlOutput("rubro_subrubro_elegido_5") %>% 
                                                                      aos(animation = "fade-down", delay = "0"),
                                                                    br(),
                                                                    
                                                                    #botonera
                                                                    shinyWidgets::radioGroupButtons("selector_rubro_g_trabajadores_crecimiento_dependencia",
                                                                                                    label = NULL,
                                                                                                    choices = c("Rubro", "Subrubro"),
                                                                                                    selected = "Rubro",
                                                                                                    justified = TRUE,
                                                                                                    width = "90%") %>%
                                                                      aos(animation = "fade-down", delay = "100"),
                                                                    
                                                                    #grafico de degradado
                                                                    plotOutput("g_crecimiento_trabajadores_dependencia", height = "300px") %>% 
                                                                      aos(animation = "fade-down", delay = "200")
                                                   ),
                                            )
                                          ),
                                          
                                          #espaciador_interior(),
                                          
                                          
                                          
                                          #espaciador_interior(),
                                          
                                          espaciador(),
                                          #fotos(3),
                                 ),
                                          
                                          #—----
                                          # VENTAS ----
                                 tabPanel(title=HTML("&nbsp;&nbsp;&nbsp;&nbsp;Ventas&nbsp;&nbsp;&nbsp;&nbsp;"), 
                                          
                                          fluidRow(
                                            column(12,
                                                   
                                                   desliza_para_continuar(),
                                                   
                                                   espaciador_interior(),
                                                   
                                                   div(class="fade_in",
                                                       h2("Ventas")
                                                   ),
                                                   
                                                   # h2("Trabajadores") %>% 
                                                   #   aos(animation = "fade-down", duration = "1000"),
                                                   
                                                   #br(),
                                                   espaciador_interior(),
                                          
                                                   # h2("Ventas") %>% aos(animation = "fade-down", duration = "1000"),
                                                   # br(),
                                            )
                                          ),
                                          
                                          
                                          #grafico crecimiento rubro ----
                                          fluidRow(
                                            column(12,
                                                   h3("Evolución de ventas del rubro") %>%
                                                     aos(animation = "fade-down", delay = "0"),
                                                   br(),
                                                   
                                                   conditionalPanel(selector_rubro_no_vacio,
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
                                                   
                                                   conditionalPanel(selector_rubro_no_vacio,
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
                                                   
                                                   conditionalPanel(selector_rubro_no_vacio,
                                                                    id = "condicional_rubro_elegido",
                                                                    
                                                                    #titulo del rubro elegido
                                                                    # htmlOutput("rubro_elegido_5") %>% 
                                                                    #   aos(animation = "fade-down", delay = "0"),
                                                                    em("Cifras en millones de pesos.") %>% 
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
                                                   
                                                   conditionalPanel(selector_rubro_no_vacio,
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
                                          
                                          #espaciador(),
                                          #fotos(4),
                                          
                                          espaciador(),
                                          
                                 ), #end pestaña
                                 
                                 #—----
                                 #OFERTA ----
                                 tabPanel(title=HTML("&nbsp;&nbsp;&nbsp;&nbsp;Precios&nbsp;&nbsp;&nbsp;&nbsp;"), 
                                          
                                          desliza_para_continuar(),
                                          
                                          espaciador_interior(),
                                          
                                          div(class = "fade_in",
                                              #YAPO.cl ----
                                              h2("Portal de compraventa Yapo.cl") %>% 
                                                aos(animation = "fade-down", duration = "1000"),
                                              br(),
                                              
                                              #productos categoria ----
                                              h3("Productos en venta por categoría") %>%
                                                aos(animation = "fade-down", delay = "0"),
                                              
                                              br(),
                                              
                                              plotOutput("yapo_productos_categoria", height = "400px") %>% 
                                                aos(animation = "fade-down", delay = "100"),
                                              
                                              espaciador_interior(),
                                              
                                              
                                              
                                              #precios categoría ----
                                              h3("Precios por categoría") %>%
                                                aos(animation = "fade-down", delay = "0"),
                                              #diferentes tendencias
                                              
                                              plotOutput("yapo_precios_categoria", height = "500px") %>% 
                                                aos(animation = "fade-down", delay = "100"),
                                              
                                              espaciador_interior(),
                                              
                                              #tendencias productos ----
                                              h3("Tendencias de publicación de productos") %>%
                                                aos(animation = "fade-down", delay = "0"),
                                              
                                              br(),
                                              #selector categorias
                                              selectInput(inputId = "yapo_categorias_2", 
                                                          label = "Seleccione una categoría de productos:", 
                                                          choices = categorias_yapo,
                                                          width = "100%"),
                                              br(),
                                              
                                              plotOutput("yapo_tendencias_productos", height = "300px") %>% 
                                                aos(animation = "fade-down", delay = "100"),
                                              
                                              espaciador_interior(),
                                              
                                              #productos más vendidos ----
                                              h3("Productos más vendidos") %>%
                                                aos(animation = "fade-down", delay = "0"),
                                              
                                              br(),
                                              #selector categorias
                                              selectInput(inputId = "yapo_categorias_3", 
                                                          label = "Seleccione una categoría de productos:", 
                                                          choices = categorias_yapo,
                                                          width = "100%"),
                                              br(),
                                              
                                              plotOutput("yapo_productos_mas_vendidos", height = "400px") %>% 
                                                aos(animation = "fade-down", delay = "100"),
                                              
                                              espaciador_interior(),
                                              
                                              
                                              
                                              #horas del día ----
                                              h3("Horario de las publicaciones") %>%
                                                aos(animation = "fade-down", delay = "0"),
                                              
                                              br(),
                                              #selector categorias
                                              selectInput(inputId = "yapo_categorias_4", 
                                                          label = "Seleccione una categoría de productos:", 
                                                          choices = categorias_yapo,
                                                          width = "100%"),
                                              br(),
                                              
                                              plotOutput("yapo_horas_del_dia", height = "300px") %>% 
                                                aos(animation = "fade-down", delay = "100"),
                                              
                                              espaciador(),
                                              
                                              
                                              
                                              
                                              #—----
                                              
                                              #portalinmobiliario.cl ----
                                              h2("Portal de arriendo de inmuebles PortalInmobiliario.cl") %>% 
                                                aos(animation = "fade-down", duration = "1000"),
                                              br(),
                                              
                                              #precio por tipo ----
                                              h3("Precio de arriendos por tipo de inmueble") %>%
                                                aos(animation = "fade-down", delay = "0"),
                                              br(),
                                              
                                              em("Cifras en millones de pesos.") %>% 
                                                aos(animation = "fade-down", delay = "0"),
                                              
                                              plotOutput("portal_precio_tipo", height = "400px") %>% 
                                                aos(animation = "fade-down", delay = "100"),
                                              
                                              espaciador_interior(),
                                              
                                              #precio por metros ----
                                              h3("Precio de arriendos por cantidad de metros cuadrados") %>%
                                                aos(animation = "fade-down", delay = "0"),
                                              br(),
                                              
                                              em("Cifras en millones de pesos.") %>% 
                                                aos(animation = "fade-down", delay = "0"),
                                              
                                              plotOutput("portal_precio_metros", height = "350px") %>% 
                                                aos(animation = "fade-down", delay = "100"),
                                              
                                              espaciador_interior(),
                                              
                                              #relacion precio metros ----
                                              h3("Relación entre precio de arriendos y metros cuadrados") %>%
                                                aos(animation = "fade-down", delay = "0"),
                                              br(),
                                              
                                              em("Cifras en millones de pesos.") %>% 
                                                aos(animation = "fade-down", delay = "0"),
                                              
                                              plotOutput("portal_relacion_precio_metros", height = "500px") %>% 
                                                aos(animation = "fade-down", delay = "100"),
                                              
                                              espaciador_interior(),
                                              
                                              #precio dormitorios ----
                                              h3("Precios de arriendos por cantidad de dormitorios") %>%
                                                aos(animation = "fade-down", delay = "0"),
                                              br(),
                                              
                                              em("Cifras en millones de pesos.") %>% 
                                                aos(animation = "fade-down", delay = "0"),
                                              
                                              plotOutput("portal_precio_dormitorios", height = "400px") %>% 
                                                aos(animation = "fade-down", delay = "100"),
                                              
                                              espaciador_interior(),
                                              
                                              
                                              
                                              #—----
                                              ## Mapa Claudio 
                                              # fluidRow(
                                              #   column(12,
                                              #          h2("Mapa") %>% aos(animation = "fade-down", duration = "1000"),
                                              #          br(),
                                              #   )
                                              # ),
                                              # fluidRow(
                                              #   
                                              #   column(12,
                                              #          
                                              #          div(id = "selectores",
                                              #              #input$comuna
                                              #              selectInput(inputId = "comuna2", 
                                              #                          label = "Comuna donde se ubica su negocio o emprendimiento", 
                                              #                          choices = comunas_sii2,
                                              #                          selected = "Iquique",
                                              #                          width = "100%"),
                                              #              
                                              #              #input$rubro
                                              #              selectInput(inputId = "rubro2", 
                                              #                          label = "Rubro principal de su negocio o emprendimiento", 
                                              #                          choices = NULL,
                                              #                          width = "100%"),
                                              #              #input$subrubro
                                              #              selectInput(inputId = "srubro2", 
                                              #                          label = "Subrubro de su negocio o emprendimiento", 
                                              #                          choices = NULL,
                                              #                          width = "100%"),
                                              #              
                                              #              conditionalPanel("input.rubro2 == '' || input.subrubro2 == ''",
                                              #                               #id = "condicional_rubro_elegido",
                                              #                               em("Por favor, rellene todos los campos")
                                              #              )
                                              #          )
                                              #   ) %>% aos(animation = "fade-down", duration = "2000", delay = 0),
                                              #   
                                              # ),
                                              # fluidRow(
                                              #   column(12,leafletOutput("mymap"))
                                              # ),
                                          )
                                 ), #fin pestaña
                                 
                                 # #ESTUDIO
                                 # tabPanel(title=HTML("&nbsp;&nbsp;&nbsp;&nbsp;Estudios&nbsp;&nbsp;&nbsp;&nbsp;"), 
                                 #          
                                 #          espaciador(),
                                 #          
                                 #          # #enlace como link que abre el archivo
                                 #          # p("sssss"),
                                 #          # tags$a(href="Estudio ofertas laborales/Estudio-ofertas-laborales.html", target="_blank", "Link"),
                                 #          
                                 #          #br(),
                                 #          
                                 #          tags$iframe(src = "Estudio ofertas laborales/Estudio-ofertas-laborales.html", style = "width: 100%; height: 600px; border:0"),
                                 #          #div(
                                 #          #includeHTML("www/Estudio ofertas laborales/Estudio-ofertas-laborales.html")
                                 #          #),
                                 #          #includeHTML("/home/bastian/Dataemprende/Estudio ofertas laborales/Estudio-ofertas-laborales.html"),
                                 #          
                                 #          espaciador()
                                 # ),
                                 
                                 
                                 #CURSOS ----
                                 
                                 tabPanel(title=HTML("&nbsp;&nbsp;&nbsp;&nbsp;Capacitación&nbsp;&nbsp;&nbsp;&nbsp;"), 
                                          
                                          fluidRow(
                                            column(12,
                                                   desliza_para_continuar(),
                                                   espaciador_interior(),
                                                   div(class="fade_in",
                                                       h2("Capacitación") ),
                                                   espaciador_interior()
                                            )
                                          ),
                                          
                                          fluidRow(
                                            column(12,
                                            #texto explicativo
                                            p("Dataemprende ofrece cursos personalizados de capacitación en transformación digital para ayudarle con la digitalización de su negocio o empresa. Respondiendo las siguientes preguntas, le recomendaremos una malla personalizada de los cursos que le son más apropiados considerando sus conocimientos, recursos, y características de su negocio.") |> 
                                              aos(animation = "fade-down", delay = "100"),
                                            espaciador_interior(),
                                            
                                            
                                            #selectores ----
                                            h4("¿Cuál es el nivel de digitalización de su pyme?") |> 
                                              aos(animation = "fade-down", delay="100"),
                                            shinyWidgets::radioGroupButtons(
                                              inputId = "v_nivel", label = NULL,
                                              choiceNames = c("nulo", "bajo", "medio", "alto", "muy alto"),
                                              choiceValues = 1:5,
                                              selected = NA,
                                              width = 600, justified = T, size = "lg") |> 
                                              aos(animation = "fade-down", delay="100"),
                                            espaciador_interior(),
                                            
                                            h4("¿Le interesa digitalizar su pyme en el corto o mediano plazo?") |> 
                                              aos(animation = "fade-down", delay="100"),
                                            shinyWidgets::radioGroupButtons(
                                              inputId = "v_deseo", label = NULL,
                                              choiceNames = c("sí", "no"),
                                              choiceValues = c(1, 0),
                                              selected = NA,
                                              width = 600, justified = T, size = "lg") |> 
                                              aos(animation = "fade-down", delay="100"),
                                            espaciador_interior(),
                                            
                                            h4("¿Cuál es el nivel actual de manejo de tecnologías digitales en su pyme?") |> 
                                              aos(animation = "fade-down", delay="100"),
                                            shinyWidgets::radioGroupButtons(
                                              inputId = "v_conoc", label = NULL,
                                              choiceNames = c("bajo", "medio", "alto"),
                                              choiceValues = 1:3,
                                              selected = NA,
                                              width = 600, justified = T, size = "lg") |> 
                                              aos(animation = "fade-down", delay="100"),
                                            espaciador_interior(),
                                            
                                            h4("¿Su pyme cuenta con los recursos (económicos, financieros) para digitalizarse?") |> 
                                              aos(animation = "fade-down", delay="100"),
                                            shinyWidgets::radioGroupButtons(
                                              inputId = "v_recur", label = NULL,
                                              choiceNames = c("sí", "no"),
                                              choiceValues = c(1, 0),
                                              selected = NA,
                                              width = 600, justified = T, size = "lg") |> 
                                              aos(animation = "fade-down", delay="100"),
                                            espaciador_interior(),
                                            
                                            h4("¿Cómo se compara su pyme en términos de digitalización con respecto a su competencia?") |> 
                                              aos(animation = "fade-down", delay="100"),
                                            shinyWidgets::radioGroupButtons(
                                              inputId = "v_compe", label = NULL,
                                              choiceNames = c("peor", "igual", "mejor"),
                                              choiceValues = 1:3,
                                              selected = NA,
                                              width = 600, justified = T, size = "lg") |> 
                                              aos(animation = "fade-down", delay="100"),
                                            espaciador_interior(),
                                            
                                            
                                            #alerta
                                            div(id='cursos_alerta',
                                                             em("Por favor, rellene todos los campos para obtener sus resultados")
                                            ),
                                            
                                            #resultados ----
                                            div(id="cursos_resultado",
                                                div(class="fade_in",
                                                    h2("Recomendación de cursos")
                                                ),
                                                
                                                espaciador_interior(),
                                                
                                                p("A continuación se presentan los cursos que recomendamos...") |> 
                                                  aos(animation = "fade-down", delay = "100"),
                                                
                                                #cantidad de cursos
                                                
                                                espaciador_interior(),
                                                
                                                fluidRow(
                                                  column(5,
                                                         uiOutput("cursos_resultado_botones") |> shinycssloaders::withSpinner(proxy.height = "200px", color = color_oscuro, color.background = color_fondo)
                                                  ),
                                                  column(7,
                                                         #descripción del curso seleccionado
                                                         div(style = glue::glue("color: {color_blanco}; background: {color_oscuro}; border-radius: 5px; border: none; height: 100%; padding: 12px;"),
                                                             
                                                             div(#style = "padding: 20px; padding-top: 0px;",
                                                               
                                                               div(id = "btn_curso_vacio",
                                                                   espaciador_interior(),
                                                                   em("Seleccione uno de los cursos recomendados para información más detallada."),
                                                                   espaciador_interior()
                                                               ),
                                                             h3(textOutput("btn_curso_titulo"), style = "margin-left: 12px;"),
                                                             br(),
                                                             
                                                             div(textOutput("btn_curso_descripcion"), style = "margin-left: 12px;"),
                                                             br()
                                                             )
                                                         )
                                                  )
                                                )
                                            )
                                            
                                            )),
                                          
                                          espaciador()
                                 ), #end tab cursos
                                 
                                 #—----
                                 
                                 
                                 #DESCARGAS ----
                                 tabPanel(title=HTML("&nbsp;&nbsp;&nbsp;&nbsp;Documentos&nbsp;&nbsp;&nbsp;&nbsp;"), 
                                          
                                          espaciador_interior(),
                                          
                                          div(class = "fade_in",
                                              
                                              h2("Descarga de datos y estudios") %>% 
                                                aos(animation = "fade-down", duration = "1000"),
                                              br(),
                                              
                                              p("En esta sección podrás descargar documentos de estudio relacionados al proyecto DataEmprende.") %>% 
                                                aos(animation = "fade-down", delay = 0),
                                              br(), br(),
                                              
                                              
                                              #descarga 1
                                              
                                              h3("Taller descargable de Fondos Concursables") %>% 
                                                aos(animation = "fade-down", delay=0),
                                              br(),
                                              
                                              # p("Este estudio trata sobre...") %>% 
                                              #   aos(animation = "fade-down", delay = 0),
                                              # br(),
                                              
                                              downloadButton(outputId = "descarga_taller", 
                                                             label = HTML("&nbsp;&nbsp;Descargar estudio"), 
                                                             icon = icon("file-download"),
                                                             style = "text-decoration: none !important;") %>% 
                                                aos(animation = "fade-down", delay=0), 
                                              
                                              
                                              espaciador_interior(),
                                              
                                              
                                              #descarga 2
                                              h3("Cuadro comparativo del régimen general y simplificado") %>% 
                                                aos(animation = "fade-down", delay=0),
                                              br(),
                                              
                                              # p("Este estudio trata sobre...") %>% 
                                              #   aos(animation = "fade-down", delay = 0),
                                              # br(),
                                              
                                              downloadButton(outputId = "descarga_cuadro_regimen", 
                                                             label = HTML("&nbsp;&nbsp;Descargar estudio"), 
                                                             icon = icon("file-download"),
                                                             style = "text-decoration: none !important;") %>% 
                                                aos(animation = "fade-down", delay=0), 
                                              #CuadroComparativodelRegimenGeneralySimplificado.pdf
                                              
                                              
                                              
                                              espaciador_interior(),
                                              
                                              
                                              #descarga 3
                                              h3("Cuadro Comparativo de Empresas y Sociedades") %>% 
                                                aos(animation = "fade-down", delay=0),
                                              br(),
                                              
                                              # p("Este estudio trata sobre...") %>% 
                                              #   aos(animation = "fade-down", delay = 0),
                                              # br(),
                                              
                                              downloadButton(outputId = "descarga_cuadro_empresas", 
                                                             label = HTML("&nbsp;&nbsp;Descargar estudio"), 
                                                             icon = icon("file-download"),
                                                             style = "text-decoration: none !important;") %>% 
                                                aos(animation = "fade-down", delay=0), 
                                              #CuadroComparativodeEmpresasySociedades.pdf
                                              
                                              
                                              espaciador_interior(),
                                              
                                              #estudio de ofertas laborales ----
                                              h3("Estudio de ofertas laborales de la región de Tarapacá") %>% 
                                                aos(animation = "fade-down", delay=0),
                                              br(),
                                              
                                              #frame
                                              tags$iframe(src = "Estudio ofertas laborales/Estudio-ofertas-laborales.html", style = "width: 100%; height: 600px; border:0"),
                                              
                                              #link
                                              tags$a(href="Estudio ofertas laborales/Estudio-ofertas-laborales.html", target="_blank", "(Presione aquí para abrirlo en una nueva pestaña)"),
                                              
                                              br(),
                                              
                                              # downloadButton(outputId = "descarga_ofertas_laborales", 
                                              #                label = HTML("&nbsp;&nbsp;Descargar estudio"), 
                                              #                icon = icon("file-download"),
                                              #                style = "text-decoration: none !important;") %>% 
                                              #   aos(animation = "fade-down", delay=0), 
                                              
                                              
                                              #espaciador_interior(),
                                              
                                              espaciador()
                                          )
                                 ), #fin pestaña
                                 
                                 #PROYECTOS ----
                                 tabPanel(title=HTML("&nbsp;&nbsp;&nbsp;&nbsp;Proyectos&nbsp;&nbsp;&nbsp;&nbsp;"), 
                                          
                                          espaciador(),
                                          
                                          div(class = "fade_in",
                                              
                                              h2("Conoce más proyectos") %>% 
                                                aos(animation = "fade-down", duration = "1000"),
                                              br(),
                                              
                                              
                                              p("Visita estos links y conoce nuestros productos y actividades, de los que puedes participar gratuitamente."),
                                              br(),br(),
                                              
                                              
                                              tags$a(href = "https://www.tarapaca.mat.uc.cl/", "Tarapacá UC",
                                                     target="_blank", class = "enlace", style="display:inline"),
                                              p("Visita nuestra página y conoce todos los proyectos que hemos ejecutado."),
                                              br(),
                                              
                                              tags$a(href = "https://www.acamedia.cl/", "Acamedia",
                                                     target="_blank", class = "enlace", style="display:inline"),
                                              p("Plataforma de capacitación, producto principal de nuestro proyecto TarapaTech, donde podrás acceder a videos tutoriales que serán liberados por temporadas."),
                                              br(),
                                              
                                              tags$a(href = "https://www.galeriatarapatech.cl/", "Galería Tarapatech",
                                                     target="_blank", class = "enlace", style="display:inline"),
                                              p("Conoce Galería TarapaTech, plataforma de difusión de las pymes y emprendimientos de Tarapacá, puedes inscribirte gratuitamente y pontenciarte con nosotros."),
                                              br(),
                                              
                                              tags$a(href = "https://www.tarapatech.cl/", "Tarapatech",
                                                     target="_blank", class = "enlace", style="display:inline"),
                                              p("Conoce la ruta que hemos desarrollado para potenciar los emprendimientos y pymes de Tarapacá."),
                                              br(),
                                              
                                              
                                              espaciador()
                                          )
                                          
                                 ) #fin pestaña
                     ) #fin tabset
                     ) #fin condicional
                     
              ) #fin columna
            ), #fin fluidRow principal
            
            
            #footer ----
            fluidRow(
              column(12,
                     align = "center",
                     style = paste0("background: rgb(69,123,157);",
                                    "background: linear-gradient(180deg, rgba(69,123,157,1) 0%, rgba(57,113,150,1) 100%);"
                                    #"margin-left: -20px;",
                                    #"margin-right: -20px;",
                                    #"margin-bottom: -20px;"
                     ),
                     
                     div(
                       br(),
                       p("Proyecto FIC", 
                         style = "font-family: 'Dosis', sans-serif;
                       margin-bottom: 6px; font-size: 20px; font-weight: 600;"),
                       
                       p('“Proyecto financiado por el Gobierno Regional de Tarapacá a través del Fondo de Innovación para la Competitividad FIC 2017”'),
                       br(),
                       
                       p("Ejecuta",
                         style = "font-family: 'Dosis', sans-serif;
                       margin-bottom: -50px; font-size: 20px; font-weight: 600;"),
                       
                       br(),
                       img(src = "logos.svg", height = 160),
                       img(src = "logo-uc-blanco.svg", height = 100)
                     ),
                     
                     
                     
                     #p("Plataforma desarrollada por el equipo DATA UC, usando R Shiny"),
                     #p("Investigación y desarrollo - equipo DATA UC y TARAPACÁ UC"),
                     
                     # div(style = "display: inline-block;", 
                     #     em("Desarrollo: ")
                     # ),
                     # div(style = "display: inline-block;",
                     #     tags$a(href = "http://bastian.olea.biz",
                     #            "Bastián Olea Herrera,",
                     #            target="_blank",
                     #            style = paste0("color:", color_claro, ";")
                     #     ),
                     # ),
                     # 
                     # br(),
                     # 
                     # div(style = "display: inline-block;", 
                     #     em("Dirección: ")),
                     # div(style = "display: inline-block;",
                     #     tags$a(href = "https://www.linkedin.com/in/alexis-alvear-leyton/",
                     #            "Alexis Alvear Leyton",
                     #            target="_blank",
                     #            style = paste0("color:", color_claro, ";")
                     #     ),
                     # ),
                     # br(), br(),
                     # 
                     # div(tags$a(href = "https://github.com/datauc/dataemprende",
                     #            "Código de fuente disponible en GitHub",
                     #            target="_blank",
                     #            style = paste0("color:", color_claro, ";")
                     # ),
                     # ),
                     # 
                     # br(),
                     # 
                     # p("Facultad de Matemáticas"),
                     # p("Universidad Católica de Chile"),
                     # tags$a(img(
                     #   #src = "logodatauc.png",
                     #   src = "logo-datauc-blanco.svg",
                     #   width = 200, style = "padding: 10px"
                     # ),
                     # href = "http://www.mat.uc.cl",
                     # target="_blank"
                     # ),
                     br(),
              )
            ) #fin footer
  )) #fin fluidPage y shinyUI

#Bastián Olea Herrera (@bastimapache)
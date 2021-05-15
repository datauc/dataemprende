shinyUI(fluidPage(
  #shinyanimate::withAnim(), #animación shinyanimate
  aos::use_aos(), #animación en scroll
  includeCSS("estilos.css"), #estilos css
  
  # Application title
  titlePanel("Dataemprende"),
  
  fluidRow(
    column(12,
           h1("Bienvenido") %>% aos(animation = "fade-down", duration = "2000"),
           br(),
           p("Para asesorarte, necesitamos que respondas algunas preguntas...") %>% 
             aos(animation = "fade-down", duration = "2000", delay = 400),
           
           br(), br(), br(), br(), br(), br(),
           br(), br(), br(), br(), br(), br(),
    )
  ),
  
  #preguntas ----
  
  fluidRow(
    #conditionalPanel(condition = "input.continuar1 > 0",
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
           br(), br(), br(), br(), br(), br(),
           br(), br(), br(), br(), br(), br(),
           
    )
    ) %>% aos(animation = "fade-down", duration = "2000", delay = 400),
  ),
  
  
  #resultados ----
  
  fluidRow(
    column(12,
           textOutput("parrafo1")%>% aos(animation = "fade-up", delay = "200"),
           br(),br(),
           textOutput("parrafo2")%>% aos(animation = "fade-up", delay = "300"),
           br(),br(),
           textOutput("parrafo3")%>% aos(animation = "fade-up", delay = "400"),
           br(),br(),
           textOutput("parrafo4")%>% aos(animation = "fade-up", delay = "500"),
           br(),br(),
           textOutput("parrafo5")%>% aos(animation = "fade-up", delay = "600"),
           br(),br(),
           br(),br(),
    ),
    
  ),
  
  # empresas ----
  fluidRow(
    column(12,
           h2("Empresas") %>% aos(animation = "fade-down", duration = "1000"),
           br(),
           plotOutput("empresas_comuna", height = "200px") %>% 
             aos(animation = "fade-up"),
           
    )
  ),
  
  br(),br(),
  br(),br(),
  
  # trabajadores ----
  fluidRow(
    column(12,
           h2("Trabajadores") %>% aos(animation = "fade-down", duration = "1000"),
           br(),
           
           #gráfico de logos del género de trabajadores de la comuna
           plotOutput("g_trabajadores_comuna", height = "50px") %>% 
             aos(animation = "fade-up"),
           br(),br(),
           
           #gráfico de logos del género de trabajadores de la comuna por el rubro
           plotOutput("g_trabajadores_comuna_rubro", height = "50px") %>% 
             aos(animation = "fade-up"),
    )
  ),
  
  br(),br(),
  br(),br(),
  
  # ventas ----
  fluidRow(
    column(12,
           h2("Ventas") %>% aos(animation = "fade-down", duration = "1000"),
    )
  ),
  
  br(),br(),
  br(),br(),
  
)
)
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Dataemprende"),
  
  fluidRow(
    column(12,
           h2("Bienvenido"),
           p("Para asesorarte, necesitamos que respondas algunas preguntas...")
    )
  ),
  
  #preguntas ----
  
  fluidRow(
    column(12,
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
                       width = "100%")
           
    )
  ),
  
  
  #resultados ----
  
  fluidRow(
    column(12,
           textOutput("parrafo1"),
           br(),
           textOutput("parrafo2"),
           br(),
           textOutput("parrafo3"),
           br(),
           textOutput("parrafo4"),
           br(),
           textOutput("parrafo5"),
           br()
    ),
    
  ),
  
)
)
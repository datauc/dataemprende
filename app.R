library(shiny)

# UI ----
ui <- fluidPage(

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
                   textOutput("parrafo1")
            ),
        
        ),

        )

#—----

# SERVER ----
server <- function(input, output, session) {

    #filtrar selector de subrubros
    observeEvent(input$rubro, {
        subrubros_filtrados <- subrubros_sii %>%
            filter(rubro == input$rubro) %>%
            select(subrubro) %>%
            pull()
        
        updateSelectInput(session, "subrubro",
                          choices = subrubros_filtrados)
    })
    
    #texto ----
    #texto párrafo 1
    output$parrafo1 <- reactive({
        t <- paste(
            ####poner un if else "ninguno" en rubros y subrubros sin gente 
            ####separar esto en varios reactive para que sea más liviano
            
            "En la región de Tarapacá, existen", 
            filter(datos$empresas_rubros, rubro == input$rubro) %>% pull() %>% puntos(), 
            "empresas dedicadas a su mismo rubro.",
            #De estas, un P1% son pequeñas empresas.
            "En la comuna de", paste0(input$comuna, ","), 
            filter(datos$empresas_comunas, comuna == input$comuna) %>% pull() %>% puntos(), 
            "empresas se dedican a su rubro.",
            #", un P2% de estas siendo pequeñas empresas."
            "\n", 
            "En total,",
            filter(datos$trabajadores_rubros, rubro == input$rubro) %>% pull() %>% puntos(), 
            "personas trabajan en el rubro de",
            paste(tolower(input$rubro)),
            "\n",
            
            #"un T4% de estos trabajadores son mujeres
            #y un T5 son trabajadores a honorarios. 
            "En la comuna donde se ubica su negocio,",
            filter(datos$trabajadores_comuna_rubro, comuna == input$comuna, rubro == input$rubro) %>% pull() %>% puntos(),
            "trabajadores se desempeñan en su subro, y",
            filter(datos$trabajadores_comuna_subrubro, comuna == input$comuna, subrubro == input$subrubro) %>% pull() %>% puntos(),
            "en su subrubro."
            
            #El rubro input$rubro obtiene ventas por V1 millones de pesos anuales en la región. 
            #En la comuna de input$comuna, este monto es de V2 millones; es decir, un V3% de las ventas del rubro.
        )  
        return(t)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

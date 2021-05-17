shinyServer(function(input, output, session) {
    
    #observe(startAnim(session, 'selectores', 'bounceInRight'))

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
    ####poner un if else "ninguno" en rubros y subrubros sin gente 
    ####separar esto en varios reactive para que sea más liviano
    
    #texto párrafo 1
    #párrafo empresas/rubros y tramos ----
    output$parrafo1 <- reactive({
        t <- HTML(
            "En la región de Tarapacá existen",
            cifra(22.047),
            "empresas.", 
            cifra(filter(datos$empresas_rubros, rubro == input$rubro) %>% pull() %>% puntos() %>% ninguna()), 
            "de ellas son empresas dedicadas a su mismo rubro, equivalentes al",
            cifra((filter(datos$empresas_rubros, rubro == input$rubro)$n/22047) %>% porcentaje() %>% paste0(".")),
            
            "A nivel regional, un",
            cifra(filter(datos$tramos_region, tramo == "Pequeña")$porcentaje %>% porcentaje()),
            "de las empresas son pequeñas empresas, y un",
            cifra(filter(datos$tramos_region, tramo == "Micro")$porcentaje %>% porcentaje()),
            "microempresas."
        )
        return(t)
            })
    
    #párrafo tramos/comuna, tramos/rubro ----
    output$parrafo2 <- reactive({
        t <- HTML(
            "En la comuna de", paste0(input$comuna, ","), 
            "un",
            cifra(filter(datos$tramos_comuna, comuna == input$comuna, tramo == "Pequeña")$porcentaje %>% porcentaje()),
            "de las empresas son pequeñas, y",
            cifra(filter(datos$tramos_comuna, comuna == input$comuna, tramo == "Micro")$porcentaje %>% porcentaje()),
            "son microempresas.",
            cifra(filter(datos$empresas_comunas, comuna == input$comuna) %>% pull() %>% puntos() %>% ninguna()),
            "empresas se dedican a su rubro.",
            "A nivel nacional, un",
            cifra(filter(datos$tramos_rubro, rubro == input$rubro, tramo == "Pequeña")$porcentaje %>% porcentaje()),
            "de las empresas del rubro",
            #paste(tolower(input$rubro)),
            "son pequeñas, mientras que un",
            cifra(filter(datos$tramos_rubro, rubro == input$rubro, tramo == "Micro")$porcentaje %>% porcentaje()),
            "corresponden a microempresas."
        )
        return(t)
    })
            
    #párrafo trabajadores/rubro ----
    output$parrafo3 <- reactive({
        t <- HTML(
            "En total,",
            cifra(filter(datos$trabajadores_rubros, rubro == input$rubro) %>% pull() %>% puntos() %>% ninguna()),
            "personas trabajan en el rubro de",
            paste(tolower(input$rubro)) %>% paste0(".")
        )
        return(t)
    })
    
    #párrafos trabajadores/rubro y subrubro ----
    output$parrafo4 <- reactive({
        trabajadores_comuna_rubro <- filter(datos$trabajadores_comuna_rubro, comuna == input$comuna, rubro == input$rubro) %>% pull()
        trabajadores_comuna_subrubro <- filter(datos$trabajadores_comuna_subrubro, comuna == input$comuna, subrubro == input$subrubro) %>% pull()
        
        t <- "En la comuna donde se ubica su negocio"
        
            if (isTruthy(trabajadores_comuna_rubro) == TRUE) {
                t <- paste(paste0(t, ","), 
                           cifra(trabajadores_comuna_rubro %>% puntos() %>% ninguna()),
                           "trabajadores se desempeñan en su rubro, y",
                           cifra(trabajadores_comuna_subrubro %>% puntos() %>% ninguna(palabra = "ninguno")),
                           "en su subrubro.")
            } else {
                t <- paste(t, "no hay trabajadores que se desempeñen en este rubro.")
            }

        return(HTML(t))
    })
    
    #párrafos trabajadores/género ----
    output$parrafo5 <- reactive({
        #género en porcentajes
        porcentaje_trabajadores_hombres_comuna <- filter(datos$trabajadores_genero_comunas, comuna == input$comuna)$masculino_p
        porcentaje_trabajadores_mujeres_comuna <- filter(datos$trabajadores_genero_comunas, comuna == input$comuna)$femenino_p
        
        porcentaje_trabajadores_hombres_rubro_region <- filter(datos$trabajadores_genero_rubros, rubro == input$rubro)$masculino_p
        porcentaje_trabajadores_mujeres_rubro_region <- filter(datos$trabajadores_genero_rubros, rubro == input$rubro)$femenino_p
        
        porcentaje_trabajadores_hombres_rubro_comuna <- filter(datos$trabajadores_genero_rubros_comunas, rubro == input$rubro, comuna == input$comuna)$masculino_p 
        porcentaje_trabajadores_mujeres_rubro_comuna <- filter(datos$trabajadores_genero_rubros_comunas, rubro == input$rubro, comuna == input$comuna)$femenino_p 
        
        t <- paste(
            #género por comuna
            "En su comuna, el porcentaje de trabajadores hombres y mujeres es de",
            cifra(porcentaje_trabajadores_hombres_comuna %>% porcentaje()),
            "y",
            cifra(porcentaje_trabajadores_mujeres_comuna %>% porcentaje()),
            "respectivamente.", 
            
            #género por region y rubro
            "A nivel regional, la distribución de hombres y mujeres en su rubro es de",
            cifra(porcentaje_trabajadores_hombres_rubro_region %>% porcentaje()),
            "y",
            cifra(porcentaje_trabajadores_mujeres_rubro_region %>% porcentaje())
        )
         
        #género por comuna y rubro
        if (isTruthy(porcentaje_trabajadores_hombres_rubro_comuna) == TRUE) {
            t <- paste(paste0(t, ";"), 
                       "mientras que, específicamente, en la comuna de",
                       paste(input$comuna),
                       "la distribución de género es de",
                       cifra(porcentaje_trabajadores_hombres_rubro_comuna %>% porcentaje()),
                       "hombres y",
                       cifra(porcentaje_trabajadores_mujeres_rubro_comuna %>% porcentaje()),
                       "mujeres."
            )
        } else {
            t <- paste(paste0(t, ";"),
                       "sin embargo, en la comuna de", input$comuna, "no hay trabajadores del rubro",
                       tolower(input$rubro) %>% paste0(".")
            )
        }
        return(HTML(t))
    })
    
    
    
    #empresas ----
    
    #gráfico de logos de empresas en tres filas
    output$g_empresas_comuna <- renderPlot({
        p <- graficar_empresas(input$comuna)
        return(p)
    })
    
    
    #trabajadores ----
    
    #gráfico de logos del género de trabajadores de la comuna
    output$g_trabajadores_comuna <- renderPlot({
        
        porcentaje_trabajadores_hombres_comuna <- filter(datos$trabajadores_genero_comunas, comuna == input$comuna)$masculino_p
        porcentaje_trabajadores_mujeres_comuna <- filter(datos$trabajadores_genero_comunas, comuna == input$comuna)$femenino_p
        
        p <- graficar_genero(porcentaje_trabajadores_hombres_comuna,
                             porcentaje_trabajadores_mujeres_comuna) +
            scale_color_manual(values = c("lightblue", "pink"))
        
        return(p)
    })
    
    #gráfico de logos del género de trabajadores de la comuna por el rubro
    output$g_trabajadores_comuna_rubro <- renderPlot({
        
        porcentaje_trabajadores_hombres_rubro_comuna <- filter(datos$trabajadores_genero_rubros_comunas, rubro == input$rubro, comuna == input$comuna)$masculino_p 
        porcentaje_trabajadores_mujeres_rubro_comuna <- filter(datos$trabajadores_genero_rubros_comunas, rubro == input$rubro, comuna == input$comuna)$femenino_p 
        
        p <- graficar_genero(porcentaje_trabajadores_hombres_rubro_comuna,
                             porcentaje_trabajadores_mujeres_rubro_comuna) +
            scale_color_manual(values = c("lightblue", "pink"))
        
        return(p)
    })

    
    })
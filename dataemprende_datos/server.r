library(dplyr)

shinyServer(function(input, output, session) {

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
        t <- paste(
            "En la región de Tarapacá existen 22.047 empresas.", 
            filter(datos$empresas_rubros, rubro == input$rubro) %>% pull() %>% puntos() %>% ninguna(), 
            "de ellas son empresas dedicadas a su mismo rubro, equivalentes al",
            (filter(datos$empresas_rubros, rubro == input$rubro)$n/22047) %>% scales::percent(decimal.mark = ",", accuracy = 0.1) %>% paste0("."),
            
            "A nivel regional, un",
            filter(datos$tramos_region, tramo == "Pequeña")$porcentaje %>% scales::percent(decimal.mark = ",", accuracy = 0.1),
            "de las empresas son pequeñas empresas, y un",
            filter(datos$tramos_region, tramo == "Micro")$porcentaje %>% scales::percent(decimal.mark = ",", accuracy = 0.1),
            "microempresas."
        )
        return(t)
            })
    
    #párrafo tramos/comuna, tramos/rubro ----
    output$parrafo2 <- reactive({
        t <- paste(
            "En la comuna de", paste0(input$comuna, ","), 
            "un",
            filter(datos$tramos_comuna, comuna == input$comuna, tramo == "Pequeña")$porcentaje %>% scales::percent(decimal.mark = ",", accuracy = 0.1),
            "de las empresas son pequeñas, y",
            filter(datos$tramos_comuna, comuna == input$comuna, tramo == "Micro")$porcentaje %>% scales::percent(decimal.mark = ",", accuracy = 0.1),
            "son microempresas.",
            filter(datos$empresas_comunas, comuna == input$comuna) %>% pull() %>% puntos() %>% ninguna(), 
            "empresas se dedican a su rubro.",
            "A nivel nacional, un",
            filter(datos$tramos_rubro, rubro == input$rubro, tramo == "Pequeña")$porcentaje %>% scales::percent(decimal.mark = ",", accuracy = 0.1),
            "de las empresas del rubro",
            #paste(tolower(input$rubro)),
            "son pequeñas, mientras que un",
            filter(datos$tramos_rubro, rubro == input$rubro, tramo == "Micro")$porcentaje %>% scales::percent(decimal.mark = ",", accuracy = 0.1),
            "corresponden a microempresas."
        )
        return(t)
    })
            
    #párrafo trabajadores/rubro ----
    output$parrafo3 <- reactive({
        t <- paste(
            "En total,",
            filter(datos$trabajadores_rubros, rubro == input$rubro) %>% pull() %>% puntos() %>% ninguna(), 
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
                           trabajadores_comuna_rubro %>% puntos() %>% ninguna(),
                           "trabajadores se desempeñan en su rubro, y",
                           trabajadores_comuna_subrubro %>% puntos() %>% ninguna(palabra = "ninguno"),
                           "en su subrubro.")
            } else {
                t <- paste(t, "no hay trabajadores que se desempeñen en este rubro.")
            }

        return(t)
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
            porcentaje_trabajadores_hombres_comuna %>% scales::percent(decimal.mark = ",", accuracy = 0.1),
            "y",
            porcentaje_trabajadores_mujeres_comuna %>% scales::percent(decimal.mark = ",", accuracy = 0.1),
            "respectivamente.", 
            
            #género por region y rubro
            "A nivel regional, la distribución de hombres y mujeres en su rubro es de",
            porcentaje_trabajadores_hombres_rubro_region %>% scales::percent(decimal.mark = ",", accuracy = 0.1),
            "y",
            porcentaje_trabajadores_mujeres_rubro_region %>% scales::percent(decimal.mark = ",", accuracy = 0.1)
        )
         
        #género por comuna y rubro
        if (isTruthy(porcentaje_trabajadores_hombres_rubro_comuna) == TRUE) {
            t <- paste(paste0(t, ";"), 
                       "mientras que, específicamente, en la comuna de",
                       paste(input$comuna),
                       "la distribución de género es de",
                       porcentaje_trabajadores_hombres_rubro_comuna %>% scales::percent(decimal.mark = ",", accuracy = 0.1),
                       "hombres y",
                       porcentaje_trabajadores_mujeres_rubro_comuna %>% scales::percent(decimal.mark = ",", accuracy = 0.1),
                       "mujeres."
            )
        } else {
            t <- paste(paste0(t, ";"),
                       "sin embargo, en la comuna de", input$comuna, "no hay trabajadores del rubro",
                       tolower(input$rubro) %>% paste0(".")
            )
        }
        return(t)
    })
}

)
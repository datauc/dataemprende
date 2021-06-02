shinyServer(function(input, output, session) {
    
    options(shiny.sanitize.errors = FALSE)
    options(OutDec= ",") #decimales con coma
    
    #filtrar selector de subrubros
    observeEvent(input$rubro, {
      req(input$rubro != "")
      # #entrega subrubros ordenados por cantidad de empresas
      # #tictoc::tic()
      # subrubros_filtrados <- datos$empresas_subrubros %>%
      #   filter(rubro == input$rubro) %>%
      #   arrange(desc(n)) %>% 
      #   select(subrubro) %>%
      #   pull()
      # #tictoc::toc()
      
      #lo mismo pero preprocesado
      subrubros_filtrados <- subrubros_sii$subrubro[subrubros_sii$rubro == input$rubro]
        
      cat(fill=T, "observeEvent(input$rubro)")
        updateSelectInput(session, "subrubro",
                          choices = subrubros_filtrados)
    }) 
    
    #texto ----
    ####poner un if else "ninguno" en rubros y subrubros sin gente 
    ####separar esto en varios reactive para que sea más liviano
    
    #texto párrafo 1
    #párrafo empresas/rubros y tramos ----
    output$parrafo1 <- reactive({
        req(input$rubro != "")
        
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
        cat(fill=T, "output$parrafo1")
        return(t)
    })
    
    #párrafo tramos/comuna, tramos/rubro ----
    output$parrafo2 <- reactive({
        req(input$rubro != "")
        
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
        cat(fill=T, "output$parrafo2")
        return(t)
    })
    
    #párrafo trabajadores/rubro ----
    output$parrafo3 <- reactive({
        req(input$rubro != "")
        
        t <- HTML(
            "En total,",
            cifra(filter(datos$trabajadores_rubros, rubro == input$rubro) %>% pull() %>% puntos() %>% ninguna()),
            "personas trabajan en el rubro de",
            paste(tolower(input$rubro)) %>% paste0(".")
        )
        cat(fill=T, "output$parrafo3")
        return(t)
    })
    
    #párrafos trabajadores/rubro y subrubro ----
    output$parrafo4 <- reactive({
        req(input$rubro != "",
            input$subrubro != "")
        
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
        cat(fill=T, "output$parrafo4")
        return(HTML(t))
    })
    
    #párrafos trabajadores/género ----
    output$parrafo5 <- reactive({
        req(input$rubro != "")
        
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
        cat(fill=T, "output$parrafo5")
        return(HTML(t))
    })
    
    
    #—----
    
    #EMPRESAS ----
    
    #texto empresas rubro ----
    output$t_empresas_rubro_1 <- reactive({
        req(input$rubro != "")
        
        t <- HTML(
            "En la región existen",
            cifra(filter(datos$empresas_rubros, rubro == input$rubro) %>% pull() %>% puntos() %>% ninguna()), 
            "empresas dedicadas a su mismo rubro, equivalentes al",
            cifra((filter(datos$empresas_rubros, rubro == input$rubro)$n/22047) %>% porcentaje()),
            "del total de empresas de Tarapacá."
        )
        return(t)
    })
    
    #texto empresas rubro ----
    output$t_empresas_rubro_2 <- reactive({
        req(input$rubro != "")
        
        t <- HTML(
            "El",
            cifra(datos$empresas_rubros_comuna %>%
                filter(comuna == input$comuna,
                       rubro == input$rubro) %>% select(porcentaje) %>% pull() %>% porcentaje()),
            "de las empresas de su rubro se encuentra ubicada en su comuna."
        )
        return(t)
    })
        
    
    #mapa empresas comuna ----
    output$rubro_elegido_6 <- reactive({
      req(input$rubro != "",
          input$subrubro != "")
      HTML(cifra(input$rubro))
    })
    
    output$m_empresas_comuna <- renderPlot({
        req(input$rubro != "")
        
        m <- datos$empresas_rubros_comuna %>%
            filter(rubro == input$rubro) %>%
            graficar_mapa_comunas(variable = "empresas")
        cat(fill=T, "output$m_empresas_comuna")
        return(m)
    }, res = 100) #%>%
        #bindCache(input$rubro)
    
    
    
    output$rubro_subrubro_elegido_1 <- reactive({
        req(input$rubro != "",
            input$subrubro != "")
        
        if (input$selector_g_barras_empresas_rubro == "Rubro") {
            h <- HTML(cifra(input$rubro)) }
        else {
            h <- HTML(cifra(input$subrubro))
        }
        return(h)
    })
    
    #grafico horizontal empresas comuna ----
    output$g_barras_empresas_rubro_comuna <- renderPlot({
        req(input$rubro != "")
        
        if (input$selector_g_barras_empresas_rubro == "Rubro") {
            d <- datos$empresas_rubros_comuna %>%
                ungroup() %>%
                filter(rubro == input$rubro)
            
        } else if (input$selector_g_barras_empresas_rubro == "Subrubro") {
            d <- datos$empresas_subrubros_comuna %>%
                ungroup() %>%
                filter(subrubro == input$subrubro)
        }
        
        p <- graficar_barras_horizontales(d, variable_categorica = "comuna", slice=7, str_wrap=30, str_trunc=50)
        return(p)
    }, res = 100)
    
    
    #tamaños de empresas ----
    #gráfico de logos de empresas en tres filas
    output$g_empresas_comuna <- renderPlot({
        p <- graficar_empresas(input$comuna)
        return(p)
    })
    

    #crecimiento del subrubro ----
    #textos que comparan cantidad de empresas hace 1, 5 o 10 años y muestran porcentaje de cambio
    output$crecimiento_subrubros_empresas <- reactive({
        req(input$rubro != "")
        
        #hace 1 año
        if (input$comparacion_crecimiento == 1){
            t <- HTML("el subrubro ha crecido en un",
                      cifra((datos$crecimiento_subrubros_region %>% filter(subrubro == input$subrubro))$crecimiento_1 %>% porcentaje()),
                      "a nivel regional.")
        }
        #hace 5 años
        else if (input$comparacion_crecimiento == 5) {
            t <- HTML("el subrubro ha crecido en un",
                      cifra((datos$crecimiento_subrubros_region %>% filter(subrubro == input$subrubro))$crecimiento_5 %>% porcentaje()),
                      "a nivel regional.")
        }
        #hace 10 años
        else if (input$comparacion_crecimiento == 10) {
            t <- HTML("el subrubro ha crecido en un",
                      cifra((datos$crecimiento_subrubros_region %>% filter(subrubro == input$subrubro))$crecimiento_10 %>% porcentaje()),
                      "a nivel regional.")
        }
      cat(fill=T, "output$crecimiento_subrubros_empresas")
        return(t)
    })
    
    output$rubro_elegido_1 <- reactive({
        req(input$rubro != "",
            input$subrubro != "")
        HTML(cifra(input$rubro))
    })
    
    output$subrubro_elegido_1 <- reactive({
        req(input$rubro != "",
            input$subrubro != "")
        HTML(cifra(input$subrubro))
    })
    
    #grafico crecimiento rubro region/comuna----
    #gráfico de líneas con degradado
    output$g_crecimiento_empresas_rubro <- renderPlot({
        req(input$rubro != "")
        #lógica para la botonera de comuna o región:
        
        #filtrar por comuna si se elige comuna en el selector
        if (input$selector_g_crecimiento_empresas_rubro == "Comuna") {
            d <- datos$empresas_año_rubro_comuna %>%
                filter(rubro == input$rubro,
                       comuna == input$comuna)
        } #si se selecciona region, usar datos sin comuna precalculados
        else { 
            d <- datos$empresas_año_rubro_region %>%
                filter(rubro == input$rubro)
        }
        #graficar
        p <- d %>%
            graficar_lineas_degradado()
        return(p)
    }, res = 100) #%>%
        # bindCache(input$selector_g_cantidad_empresas,
        #           input$rubro,
        #           input$comuna)
    
    
    #grafico crecimiento subrubro region/comuna----
    #gráfico de líneas con degradado
    output$g_crecimiento_empresas_subrubro <- renderPlot({
        req(input$rubro != "",
            input$subrubro != "")
        #lógica para la botonera de comuna o región:
        
        #filtrar por comuna si se elige comuna en el selector
        if (input$selector_g_crecimiento_empresas_subrubro == "Comuna") {
            d <- datos$empresas_año_subrubro_comuna %>%
                filter(subrubro == input$subrubro,
                       comuna == input$comuna)
        } #si se selecciona region, usar datos sin comuna precalculados
        else { 
            d <- datos$empresas_año_subrubro_region %>%
                filter(subrubro == input$subrubro)
        }
        #graficar
        p <- d %>%
            graficar_lineas_degradado(variable = "subrubro")
        return(p)
    }, res = 100)
    
    
    #hacer el cruce entre el subrubro y el ciiu que le corresponde para filtrar el mapa de puntos de empresas
    subrubro_en_ciiu <- reactive({
        cruce_ciiuu_sii %>%
            filter(subrubro == input$subrubro) %>%
            select(glosa_division) %>%
            pull()
    }) #%>%
        #bindCache(input$subrubro)
    
    
    
    
    #mapa empresas rubro ----
    #mapa de puntos de empresas
    d_iquique_empresas_rubro <- reactive({
        req(input$rubro != "")
        
        #condición para graficar puntos del rubro entero, o del ciuu que corresponde al surubro elegido
        #subsubro
        if (input$selector_m_iquique_empresas_rubro == "Subrubro") {
            d <- puntos_empresas %>% 
                filter(rubro == input$rubro,
                       subrubro == input$subrubro,
                       glosa_division == subrubro_en_ciiu()) #usar función que ya hizo la correspondencia entre ciuu y subrubro
        } #rubro, sin filtrar subrubro
        else {
            d <- puntos_empresas %>% 
                filter(rubro == input$rubro)
        }
    }) %>%
        bindCache(input$selector_m_iquique_empresas_rubro, 
                  input$zoom_m_iquique_empresas_rubro,
                  input$rubro, input$subrubro)
    
    #mapa
    output$m_iquique_empresas_rubro <- renderPlot({
      req(input$rubro != "")
        #nivel de zoom
        if (input$zoom_m_iquique_empresas_rubro == "Iquique y Alto Hospicio") {
            #iquique y alto hospicio
            mover_x_elegido = 0; mover_y_elegido = 0; zoom_elegido = 0
            
        } else if (input$zoom_m_iquique_empresas_rubro == "Iquique norte") {
            # # #centrar en iquique arriba
            mover_x_elegido = -0.029; mover_y_elegido = 0.03; zoom_elegido = 0.039
            
        } else if (input$zoom_m_iquique_empresas_rubro == "Iquique centro") {
            #centrar en iquique al medio
            mover_x_elegido = -0.02; mover_y_elegido = 0; zoom_elegido = 0.039
            
        } else if (input$zoom_m_iquique_empresas_rubro == "Iquique sur") {
            #centrar en iquique abajo
            mover_x_elegido = -0.014; mover_y_elegido = -0.016; zoom_elegido = 0.039
            
        } else if (input$zoom_m_iquique_empresas_rubro == "Alto Hospicio") {
            #centrar en alto hospicio
            mover_x_elegido = 0.021; mover_y_elegido = -0.022; zoom_elegido = 0.025
        }
        
        #graficar
        p <- d_iquique_empresas_rubro() %>%
            graficar_mapa_rubros(mover_x = mover_x_elegido,
                                 mover_y = mover_y_elegido,
                                 zoom = zoom_elegido)  
        cat(fill=T, "output$m_iquique_empresas_rubro")
        return(p)
    }, res = 100) %>%
        bindCache(input$selector_m_iquique_empresas_rubro, #selector rubro/subrubro
                  input$zoom_m_iquique_empresas_rubro, #zoom
                  input$rubro, input$subrubro)
    
    output$rubro_subrubro_elegido_2 <- reactive({
        req(input$rubro != "",
            input$subrubro != "")
        
        if (input$selector_m_iquique_empresas_rubro == "Rubro") {
            h <- HTML(cifra(input$rubro)) }
        else {
            #output de texto del ciiu correspondiente
            h <- HTML(cifra(subrubro_en_ciiu()))
        }
        return(h)
    })
    
    
    #grafico horizontal empresas subrubro ----
    output$g_barras_empresas_subrubro <- renderPlot({
        req(input$rubro != "")
        
        if (input$selector_g_barras_empresas_subrubro == "Región") {
            d <- datos$empresas_año_subrubro_region %>%
                ungroup() %>%
                filter(rubro == input$rubro,
                       año == max(año)) %>%
                select(-año, -rubro)
            
        } else if (input$selector_g_barras_empresas_subrubro == "Comuna") {
            d <- datos$empresas_año_subrubro_comuna %>%
                ungroup() %>%
                filter(rubro == input$rubro,
                       comuna == input$comuna,
                       año == max(año)) %>%
                select(-año, -rubro, -comuna)
        }

        p <- graficar_barras_horizontales(d, slice=6, str_wrap=30, str_trunc=50)
        return(p)
    }, res = 100)
    
    
    #—----
    #TRABAJADORES ----
    
    output$rubro_elegido_3 <- reactive({
        req(input$rubro != "",
            input$subrubro != "")
        HTML(cifra(input$rubro))
    })
    
    output$subrubro_elegido_3 <- reactive({
        req(input$rubro != "",
            input$subrubro != "")
        HTML(cifra(input$subrubro))
    })
    
    #grafico crecimiento rubro region/comuna----
    #gráfico de líneas con degradado
    output$g_crecimiento_trabajadores_rubro <- renderPlot({
        req(input$rubro != "")
        #lógica para la botonera de comuna o región:
        
        #filtrar por comuna si se elige comuna en el selector
        if (input$selector_g_crecimiento_trabajadores_rubro == "Comuna") {
            d <- datos$trabajadores_año_subrubro_comuna %>%
                filter(rubro == input$rubro,
                       comuna == input$comuna) %>%
                group_by(rubro, comuna, año) %>%
                summarize(trabajadores = sum(trabajadores, na.rm=T), .groups = "drop")
            
        } #si se selecciona region, usar datos sin comuna precalculados
        else { 
            d <- datos$trabajadores_año_subrubro_region %>%
                filter(rubro == input$rubro) %>%
                group_by(rubro, año) %>%
                summarize(trabajadores = sum(trabajadores, na.rm=T), .groups = "drop")
        }
        #graficar
        p <- d %>%
            graficar_lineas_degradado(variable = "rubro", variable_y_elegida="trabajadores", 
                                      texto_y = "Cantidad de trabajadores",
                                      numero_largo=1.5)
        cat(fill=T, "output$g_crecimiento_trabajadores_rubro")
        return(p)
    }, res = 100) #%>%
    # bindCache(input$selector_g_crecimiento_trabajadores_rubro,
    #           input$rubro,
    #           input$comuna)
    
    
    
    #grafico crecimiento subrubro region/comuna----
    #gráfico de líneas con degradado
    output$g_crecimiento_trabajadores_subrubro <- renderPlot({
        req(input$rubro != "",
            input$subrubro != "")
        #lógica para la botonera de comuna o región:
        
        #filtrar por comuna si se elige comuna en el selector
        if (input$selector_g_crecimiento_trabajadores_subrubro == "Comuna") {
            d <- datos$trabajadores_año_subrubro_comuna %>%
                filter(subrubro == input$subrubro,
                       comuna == input$comuna) 
            
        } #si se selecciona region, usar datos sin comuna precalculados
        else { 
            d <- datos$trabajadores_año_subrubro_region %>%
                filter(subrubro == input$subrubro) 
        }
        #graficar
        p <- d %>%
            graficar_lineas_degradado(variable = "subrubro", variable_y_elegida="trabajadores", 
                                      texto_y = "Cantidad de trabajadores",
                                      numero_largo=1.5)
        return(p)
    }, res = 100) #%>%
    # bindCache(input$selector_g_crecimiento_trabajadores_subrubro,
    #           input$rubro,
    #           input$comuna)
    
    
    
    #grafico genero ----
    #gráfico de logos del género de trabajadores de la comuna
    output$g_trabajadores_comuna <- renderPlot({
        
        porcentaje_trabajadores_hombres_comuna <- filter(datos$trabajadores_genero_comunas, comuna == input$comuna)$masculino_p
        porcentaje_trabajadores_mujeres_comuna <- filter(datos$trabajadores_genero_comunas, comuna == input$comuna)$femenino_p
        
        p <- graficar_genero(porcentaje_trabajadores_hombres_comuna,
                             porcentaje_trabajadores_mujeres_comuna) +
            scale_color_manual(values = c("lightblue", "pink"))
        
        return(p)
    }, res = 100)
    
    #gráfico de logos del género de trabajadores de la comuna por el rubro
    output$g_trabajadores_comuna_rubro <- renderPlot({
        req(input$rubro != "")
        
        porcentaje_trabajadores_hombres_rubro_comuna <- filter(datos$trabajadores_genero_rubros_comunas, rubro == input$rubro, comuna == input$comuna)$masculino_p 
        porcentaje_trabajadores_mujeres_rubro_comuna <- filter(datos$trabajadores_genero_rubros_comunas, rubro == input$rubro, comuna == input$comuna)$femenino_p 
        
        p <- graficar_genero(porcentaje_trabajadores_hombres_rubro_comuna,
                             porcentaje_trabajadores_mujeres_rubro_comuna) +
            scale_color_manual(values = c("lightblue", "pink"))
        
        return(p)
    }, res = 100)
  
    
    #grafico dependencia rubro ----
    output$rubro_subrubro_elegido_3 <- reactive({
      req(input$rubro != "",
          input$subrubro != "")
      
      if (input$selector_g_trabajadores_dependencia == "Rubro") {
        h <- HTML(cifra(input$rubro)) }
      else {
        h <- HTML(cifra(input$subrubro))
      }
      return(h)
    })
    
    output$g_trabajadores_dependencia <- renderPlot({
      req(input$rubro != "",
          input$subrubro != "")
      
      if (input$selector_g_trabajadores_dependencia == "Rubro") {
      d <- datos$trabajadores_dependencia_rubro %>%
        filter(rubro == input$rubro)
      
      } else if (input$selector_g_trabajadores_dependencia == "Subrubro") {
        d <- datos$trabajadores_dependencia_subrubro %>%
          filter(subrubro == input$subrubro)
      }
      
      p <- graficar_circular(d, variable_categorica = "dependencia", variable_numerica = "trabajadores")
      return(p)
    }, res = 100, bg = color_fondo)
    
    
    

    #(!)mayores trabajadores ----
    #en comuna
    #mayores trabajadores mujeres

    #mapa leaflet ----
    ### Mapa leaflet: Actualizacón Selector de Rubro.
    observeEvent(input$comuna2,{
        select_rubro2 = with(empresas_mapa_sii,
                             glosa_seccion[nom_comuna==input$comuna2]) %>% 
            unique() %>% sort()
        updateSelectInput(session,"rubro2",
                          choices = select_rubro2 )
    })
    ### Mapa leaflet: Actualicación Selector de Subrubro.
    observeEvent(input$rubro2,{
        select_srubro2 = with(empresas_mapa_sii,
                              glosa_division[nom_comuna==input$comuna2 &
                                                 glosa_seccion == input$rubro2]) %>%
            unique() %>% sort()
        updateSelectInput(session,"srubro2",
                          choices = select_srubro2)
    })
    
    ### Mapa leaflet: Creación Mapa leaflet
    output$mymap <- renderLeaflet({
        
        Empresas_aux = empresas_mapa_sii %>% 
            filter(nom_comuna ==input$comuna2,
                   glosa_seccion == input$rubro2,
                   glosa_division == input$srubro2)
        
        indx= which(comunas_sii2==input$comuna2)
        
        
        leaflet() %>%
            setView(lng = com_lng[indx],
                    lat = com_lat[indx],
                    zoom = c(13,15,12,14,14)[indx]) %>% 
            addProviderTiles(providers$CartoDB.Positron) %>% 
            addCircleMarkers(lng = Empresas_aux$longitud, 
                             lat = Empresas_aux$latitud,
                             radius = 4,color = "#6082b6",stroke = FALSE,fillOpacity = 0.7)    
    })
    
    
    #—----
    
    #VENTAS ----  
    
    output$rubro_elegido_4 <- reactive({
        req(input$rubro != "",
            input$subrubro != "")
        HTML(cifra(input$rubro))
    })
    
    output$subrubro_elegido_4 <- reactive({
        req(input$rubro != "",
            input$subrubro != "")
        HTML(cifra(input$subrubro))
    })
    
    #grafico crecimiento rubro region/comuna----
    #gráfico de líneas con degradado
    output$g_crecimiento_ventas_rubro <- renderPlot({
        req(input$rubro != "")
        #lógica para la botonera de comuna o región:
        
        #filtrar por comuna si se elige comuna en el selector
        if (input$selector_g_crecimiento_ventas_rubro == "Comuna") {
            d <- datos$ventas_año_subrubro_comuna %>% 
                filter(rubro == input$rubro,
                       comuna == input$comuna) %>%
                group_by(año, rubro) %>%
                summarize(ventas_anuales = sum(ventas_anuales, na.rm = T),
                          empresas = sum(empresas), .groups = "drop") %>%
                mutate(ventas_promedio = ventas_anuales/empresas,
                       ventas_promedio = round(ventas_promedio/1000000, 3))
            
        } #si se selecciona region, usar datos sin comuna precalculados
        else { 
            d <- datos$ventas_año_subrubro_region %>% 
                filter(rubro == input$rubro) %>%
                group_by(año, rubro) %>%
                summarize(ventas_anuales = sum(ventas_anuales, na.rm = T),
                          empresas = sum(empresas), .groups = "drop") %>%
                mutate(ventas_promedio = ventas_anuales/empresas,
                       ventas_promedio = round(ventas_promedio/1000000, 3))
        }
        #graficar
        p <- d %>%
            graficar_lineas_degradado(variable = "rubro", variable_y_elegida="ventas_promedio", 
                                      texto_y = "Ventas (en millones de pesos)",
                                      numero_largo=1.5)
        cat(fill=T, "output$g_crecimiento_ventas_rubro")
        return(p)
    }, res = 100) #%>%
    # bindCache(input$selector_g_crecimiento_trabajadores_rubro,
    #           input$rubro,
    #           input$comuna)
    
    
    
    #grafico crecimiento subrubro region/comuna----
    #gráfico de líneas con degradado
    output$g_crecimiento_ventas_subrubro <- renderPlot({
        req(input$rubro != "",
            input$subrubro != "")
        #lógica para la botonera de comuna o región:
        
        #filtrar por comuna si se elige comuna en el selector
        if (input$selector_g_crecimiento_ventas_subrubro == "Comuna") {
            d <- datos$ventas_año_subrubro_comuna %>% 
                filter(subrubro == input$subrubro,
                       comuna == input$comuna) %>%
                ungroup() %>%
                mutate(ventas_promedio = ventas_anuales/empresas,
                       ventas_promedio = round(ventas_promedio/1000000, 3))
            
        } #si se selecciona region, usar datos sin comuna precalculados
        else { 
            d <- datos$ventas_año_subrubro_region %>% 
                filter(subrubro == input$subrubro) %>%
                ungroup() %>%
                mutate(ventas_promedio = ventas_anuales/empresas,
                       ventas_promedio = round(ventas_promedio/1000000, 3))
        }
        #graficar
        p <- d %>%
            graficar_lineas_degradado(variable = "subrubro", variable_y_elegida="ventas_promedio", 
                                      texto_y = "Ventas (en millones de pesos)",
                                      numero_largo=1.5)
        return(p)
    }, res = 100) #%>%
    # bindCache(input$selector_g_crecimiento_trabajadores_subrubro,
    #           input$rubro,
    #           input$comuna)
    
    
    output$rubro_elegido_5 <- reactive({
      req(input$rubro != "",
          input$subrubro != "")
      HTML(cifra(input$rubro))
    })
    
    output$subrubro_elegido_5 <- reactive({
      req(input$rubro != "",
          input$subrubro != "")
      HTML(cifra(input$subrubro))
    })
    #mayores ventas rubro ----
    
    #rubros con mayores ventas anuales promedio en la región/comuna elegida
    output$g_mayores_ventas_rubro <- renderPlot({
      req(input$rubro != "")
      
      #región
      if (input$selector_g_mayores_ventas_rubro == "Región") {
        d <- datos$ventas_año_subrubro_comuna %>%
          filter(año == 2019) %>%
          group_by(rubro) %>%
          summarize(ventas_anuales = sum(ventas_anuales, na.rm=T),
                    empresas = sum(empresas), .groups = "drop") %>%
          mutate(ventas_promedio = ventas_anuales/empresas,
                 ventas_promedio = round(ventas_promedio/1000000, 1))
      }
      #comuna
      else {
        d <- datos$ventas_año_subrubro_comuna %>%
          filter(año == 2019,
                 comuna == input$comuna) %>%
          group_by(comuna, rubro) %>%
          summarize(ventas_anuales = sum(ventas_anuales, na.rm=T),
                    empresas = sum(empresas), .groups = "drop") %>%
          mutate(ventas_promedio = ventas_anuales/empresas,
                 ventas_promedio = round(ventas_promedio/1000000, 1))
      }
      p <- graficar_barras_horizontales(d, variable_categorica = "rubro", 
                                        variable_numerica = "ventas_promedio", 
                                        slice=6, str_wrap=25, str_trunc=50)
      return(p)
    }, res = 100)
    
    #mayores ventas subrubro ----
    #subrubros con mayores ventas anuales promedio en la región/comuna elegida
    output$g_mayores_ventas_subrubro <- renderPlot({
      req(input$rubro != "")
      
      #región
      if (input$selector_g_mayores_ventas_subrubro == "Región") {
        d <- datos$ventas_año_subrubro_comuna %>%
          filter(año == 2019,
                 rubro == input$rubro) %>%
          group_by(subrubro) %>%
          summarize(ventas_anuales = sum(ventas_anuales, na.rm=T),
                    empresas = sum(empresas), .groups = "drop") %>%
          mutate(ventas_promedio = ventas_anuales/empresas,
                 ventas_promedio = round(ventas_promedio/1000000, 1))
      }
      #comuna
      else {
        d <- datos$ventas_año_subrubro_comuna %>%
          filter(año == 2019,
                 comuna == input$comuna,
                 rubro == input$rubro) %>%
          group_by(comuna, subrubro) %>%
          summarize(ventas_anuales = sum(ventas_anuales, na.rm=T),
                    empresas = sum(empresas), .groups = "drop") %>%
          mutate(ventas_promedio = ventas_anuales/empresas,
                 ventas_promedio = round(ventas_promedio/1000000, 1))
      }
      p <- graficar_barras_horizontales(d, variable_categorica = "subrubro", 
                                        variable_numerica = "ventas_promedio", 
                                        slice=6, str_wrap=25, str_trunc=50)
      cat(fill=T, "output$g_mayores_ventas_subrubro")
      return(p)
    }, res = 100)
    
    
})

#Bastián Olea Herrera (@bastimapache)
shinyServer(function(input, output, session) {
  options(shiny.sanitize.errors = FALSE)
  options(OutDec= ",") #decimales con coma
  
  #el gran condicional
  shinyjs::hide("condicional_rubro_elegido")  
  reactive({
    req(input$rubro)
  if (input$rubro != "") { shinyjs::show("condicional_rubro_elegido") }
    }) |> bindEvent(input$rubro)

  
  #filtrar selector de subrubros
  observeEvent(input$rubro, {
    req(input$rubro != "")
    subrubros_filtrados <- subrubros_sii$subrubro[subrubros_sii$rubro == input$rubro]
    updateSelectInput(session, "subrubro", choices = subrubros_filtrados)
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
  # output$rubro_elegido_6 <- reactive({
  #   req(input$rubro != "",
  #       input$subrubro != "")
  #   HTML(cifra(input$rubro))
  # })
  
  datos_m_empresas_comuna <- reactive({
    req(input$rubro != "",
        input$subrubro != "",
        datos_mapa_regional$lugares)
    
    d <- datos$empresas_rubros_comuna %>%
      filter(rubro == input$rubro)
    return(d)
  }) %>% bindCache(input$rubro)
  
  output$m_empresas_comuna <- renderPlot({
    req(input$rubro != "",
        input$subrubro != "",
        datos_mapa_regional$lugares,
        datos_m_empresas_comuna(),
        nrow(datos_m_empresas_comuna()) > 1)
    
    # m <- datos$empresas_rubros_comuna %>%
    #     filter(rubro == input$rubro) %>%
    m <- graficar_mapa_comunas(datos_m_empresas_comuna(), variable = "empresas")
    cat(fill=T, "output$m_empresas_comuna")
    return(m)
  }, res = 100, bg = color_fondo) #%>%
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
  
  # #mapa leaflet ----
  # ### Mapa leaflet: Actualizacón Selector de Rubro.
  # observeEvent(input$comuna2,{
  #   select_rubro2 = with(empresas_mapa_sii,
  #                        glosa_seccion[nom_comuna==input$comuna2]) %>% 
  #     unique() %>% sort()
  #   updateSelectInput(session,"rubro2",
  #                     choices = select_rubro2 )
  # })
  # ### Mapa leaflet: Actualicación Selector de Subrubro.
  # observeEvent(input$rubro2,{
  #   select_srubro2 = with(empresas_mapa_sii,
  #                         glosa_division[nom_comuna==input$comuna2 &
  #                                          glosa_seccion == input$rubro2]) %>%
  #     unique() %>% sort()
  #   updateSelectInput(session,"srubro2",
  #                     choices = select_srubro2)
  # })
  # 
  # ### Mapa leaflet: Creación Mapa leaflet
  # output$mymap <- renderLeaflet({
  #   
  #   Empresas_aux = empresas_mapa_sii %>% 
  #     filter(nom_comuna ==input$comuna2,
  #            glosa_seccion == input$rubro2,
  #            glosa_division == input$srubro2)
  #   
  #   indx= which(comunas_sii2==input$comuna2)
  #   
  #   
  #   leaflet() %>%
  #     setView(lng = com_lng[indx],
  #             lat = com_lat[indx],
  #             zoom = c(13,15,12,14,14)[indx]) %>% 
  #     addProviderTiles(providers$CartoDB.Positron) %>% 
  #     addCircleMarkers(lng = Empresas_aux$longitud, 
  #                      lat = Empresas_aux$latitud,
  #                      radius = 4,color = "#6082b6",stroke = FALSE,fillOpacity = 0.7)    
  # })
  
  
  #grafico horizontal empresas comuna ----
  output$g_barras_empresas_rubro_comuna <- renderPlot({
    req(input$rubro != "",
        input$subrubro != "")
    
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
  
  
  #logos tamaños de empresas ----
  #gráfico de logos de empresas en tres filas
  output$g_empresas_comuna <- renderPlot({
    p <- graficar_empresas(input$comuna)
    return(p)
  })
  
  
  #grafico area tramos comuna ----
  output$comuna_elegida_1 <- reactive({
    req(input$rubro != "",
        input$subrubro != "")
    HTML(cifra(input$comuna))
  })
  
  output$g_empresas_area_comuna <- renderPlot({
    req(input$rubro != "",
        input$subrubro != "")
    
    #comuna o región
    if (input$selector_g_empresas_area_comuna == "Comuna") {
      d <- datos$tramos_comuna_13 %>%
        #excluir grandes empresas
        filter(tramo4 != "Grande") %>%
        droplevels() %>%
        #filtrar comuna
        filter(comuna == input$comuna)
      
    } else if (input$selector_g_empresas_area_comuna == "Región") {
      d <- datos$tramos_region_13 %>%
        #excluir grandes empresas
        filter(tramo4 != "Grande") %>%
        droplevels()
    }
    p <- graficar_area_aditiva(d)
    return(p)
  }, res=100, bg = color_fondo) #%>% 
  #bindCache(input$selector_g_empresas_area_comuna,
  #          input$comuna)
  
  
  #grafico area tramos rubro ----
  output$rubro_subrubro_elegido_6 <- reactive({
    req(input$rubro != "",
        input$subrubro != "")
    
    if (input$selector_g_empresas_area_rubro == "Rubro") {
      h <- HTML(cifra(input$rubro)) }
    else {
      h <- HTML(cifra(input$subrubro))
    }
    return(h)
  })
  
  output$g_empresas_area_rubro <- renderPlot({
    req(input$rubro != "",
        input$subrubro != "")
    
    #comuna o región
    if (input$selector_g_empresas_area_rubro == "Rubro") {
      d <- datos$tramos_rubro_13 %>%
        #excluir grandes empresas
        filter(tramo4 != "Grande") %>%
        droplevels() %>%
        filter(rubro == input$rubro)
      
    } else if (input$selector_g_empresas_area_rubro == "Subrubro") {
      d <- datos$tramos_subrubro_13 %>%
        #excluir grandes empresas
        filter(tramo4 != "Grande") %>%
        droplevels() %>%
        filter(subrubro == input$subrubro)
    }
    p <- graficar_area_aditiva(d)
    return(p)
  }, res=100, bg = color_fondo) %>% 
    bindCache(input$selector_g_empresas_area_rubro,
              input$rubro,
              input$subrubro)
  
  
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
        #filter(rubro == rubros_sii[10])
        filter(rubro == input$rubro)
    }
  }) #%>%
  # bindCache(input$selector_m_iquique_empresas_rubro, 
  #           input$zoom_m_iquique_empresas_rubro,
  #           input$rubro, input$subrubro)
  
  #mapa
  output$m_iquique_empresas_rubro <- renderPlot({
    req(input$rubro != "",
        input$subrubro != "",
        nrow(d_iquique_empresas_rubro()) > 1)
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
  }, res = 100, bg = color_fondo) #%>%
  # bindCache(input$selector_m_iquique_empresas_rubro, #selector rubro/subrubro
  #           input$zoom_m_iquique_empresas_rubro, #zoom
  #           input$rubro, input$subrubro)
  
  
  
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
  
  
  #grafico crecimiento género ----
  output$rubro_subrubro_elegido_4 <- reactive({
    req(input$rubro != "",
        input$subrubro != "")
    
    if (input$selector_rubro_g_trabajadores_crecimiento_genero == "Rubro") {
      h <- HTML(cifra(input$rubro)) }
    else {
      h <- HTML(cifra(input$subrubro))
    }
    return(h)
  })
  
  output$g_crecimiento_trabajadores_genero <- renderPlot({
    req(input$rubro != "",
        input$subrubro != "")
    # #variable de género
    # genero_elegido <- switch(input$selector_genero_g_trabajadores_crecimiento_genero,
    #                  "Mujeres" = "femenino",
    #                  "Hombres" = "masculino")
    
    #condicional por rubro o subrubro
    if (input$selector_rubro_g_trabajadores_crecimiento_genero == "Rubro") {
      d <- datos$trabajadores_año_genero_rubro %>%
        filter(rubro == input$rubro)
      #género == genero_elegido)
    } else if (input$selector_rubro_g_trabajadores_crecimiento_genero == "Subrubro") {
      d <- datos$trabajadores_año_genero_subrubro %>%
        filter(subrubro == input$subrubro)
      #género == genero_elegido)
    }
    #graficar
    #p <- graficar_lineas_degradado(d, variable_y_elegida = "trabajadores", numero_largo = 1.5)
    p <- graficar_lineas_comparadas_degradado(d, variable_y = "trabajadores",
                                              numero_largo=1.5,
                                              texto_y = "Trabajadores por género",
                                              variable = "rubro",
                                              variable_categorica_elegida = "género")
    return(p)
  }, res=100)
  
  
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
  
  
  #grafico crecimiento dependencia ----
  output$rubro_subrubro_elegido_5 <- reactive({
    req(input$rubro != "",
        input$subrubro != "")
    
    if (input$selector_rubro_g_trabajadores_crecimiento_dependencia == "Rubro") {
      h <- HTML(cifra(input$rubro)) }
    else {
      h <- HTML(cifra(input$subrubro))
    }
    return(h)
  })
  
  output$g_crecimiento_trabajadores_dependencia <- renderPlot({
    req(input$rubro != "",
        input$subrubro != "")
    
    #condicional por rubro o subrubro
    if (input$selector_rubro_g_trabajadores_crecimiento_dependencia == "Rubro") {
      d <- datos$trabajadores_año_dependencia_rubro %>%
        filter(rubro == input$rubro)
      
    } else if (input$selector_rubro_g_trabajadores_crecimiento_dependencia == "Subrubro") {
      d <- datos$trabajadores_año_dependencia_subrubro %>%
        filter(subrubro == input$subrubro)
    }
    #graficar
    p <- graficar_lineas_comparadas_degradado(d, variable_y = "trabajadores",
                                              numero_largo=1.5,
                                              texto_y = "Trabajadores por dependencia",
                                              variable = "rubro",
                                              variable_categorica_elegida = "dependencia")
    return(p)
  }, res=100)
  
  
  
  #(!)mayores trabajadores ----
  #en comuna
  #mayores trabajadores mujeres
  
  
  
  
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
  
  
  #—----
  
  #YAPO ----
  
  #productos categoria ----
  output$yapo_productos_categoria <- renderPlot({
    req(base_yapo_procesada$resumen)
    
    p <- base_yapo_procesada$resumen %>% 
      select(categoria, cantidad) %>% 
      graficar_barras_horizontales(variable_categorica = "categoria", 
                                   variable_numerica = "cantidad", slice = 15)
    return(p)
  }, res = 100)  
  
  
  #precios categoría ----
  output$yapo_precios_categoria <- renderPlot({
    req(base_yapo_procesada$resumen)
    
    p <- base_yapo_procesada$resumen %>% 
      select(-cantidad) %>% 
      #filter(categoria == categorias[8]) %>% 
      ggplot() +
      geom_segment(col = color_claro, size = 1, 
                   aes(xend = 0, x = precio_promedio, 
                       y = forcats::fct_relevel(categoria, rev(categorias_yapo)), yend = forcats::fct_relevel(categoria, rev(categorias_yapo)))) +
      geom_point(size = 4, col = color_blanco, 
                 aes(x = precio_promedio, y = forcats::fct_relevel(categoria, rev(categorias_yapo)))) +
      geom_point(size = 2.5, col = color_claro, 
                 aes(x = precio_promedio, y = forcats::fct_relevel(categoria, rev(categorias_yapo)))) +
      #geom_text(aes(label = paste0("  $", stringr::str_trim(format(round(precio_promedio, -2), big.mark = ".", decimal.mark = ","))), 
      geom_text(size = 4, family = "Dosis ExtraLight SemiBold", col = color_blanco,
                aes(label = precio_promedio %>% round(-2) %>% 
                      format(big.mark = ".", decimal.mark = ",") %>% 
                      stringr::str_trim() %>% paste0("   $", .),
                    x = precio_promedio, 
                    y = forcats::fct_relevel(categoria, rev(categorias_yapo))),
                hjust = 0) +
      theme_minimal() +
      scale_x_continuous(expand = expansion(c(0, 0.5))) +
      coord_cartesian(clip="off") +
      theme_void() +
      theme(axis.text.y = element_text(hjust = 1, color = color_negro, family = "Montserrat", size = 9, margin = margin(r = 10))) +
      theme(plot.background = element_rect(fill = color_fondo, color = color_fondo))
    return(p)
  }, res = 100)  
  
  
  #tendencias productos ----
  output$yapo_tendencias_productos <- renderPlot({
    req(base_yapo_procesada$entera)
    
    p <- base_yapo_procesada$entera %>%
      group_by(categoria, fecha) %>% 
      summarize(cantidad = n()) %>% 
      #filtrar categoría
      filter(categoria == input$yapo_categorias_2) %>% 
      #últimos x meses
      filter(fecha > max(fecha) - months(3)) %>% 
      #suavizar datos
      mutate(cantidad = zoo::rollmean(cantidad, k = 6, fill = NA)) %>% 
      #graficar
      graficar_lineas_degradado_reg()
    return(p)
  }, res = 100)  
  
  #productos más vendidos ----
  output$yapo_productos_mas_vendidos <- renderPlot({
    req(base_yapo_procesada$palabras)
    
    p <- base_yapo_procesada$palabras %>%
      group_by(categoria) %>%
      count(palabra) %>%
      arrange(categoria, desc(n)) %>%
      slice(1:15) %>%
      mutate(id = 1:n()) %>%
      filter(categoria == input$yapo_categorias_3) %>% 
      mutate(palabra =stringr::str_to_sentence(palabra)) %>% 
      graficar_barras_horizontales(variable_categorica = "palabra",
                                   variable_numerica = "n")
    return(p)
  }, res = 100)  
  
  
  #horas del día ----
  output$yapo_horas_del_dia <- renderPlot({
    req(base_yapo_procesada$entera)
    
    p <- base_yapo_procesada$entera %>% 
      filter(fecha >= max(fecha) - months(1)) %>% #último mes
      filter(categoria == input$yapo_categorias_4) %>% 
      mutate(horas = lubridate::hour(hora),
             horas = replace(horas, horas == 0, 24)) %>% 
      group_by(horas) %>% 
      summarize(cantidad = n()) %>% 
      mutate(punto_maximo = max(cantidad),
             punto_minimo = min(cantidad)) %>% 
      #mutate(horas_etiqueta = paste(horas, "hrs.")) %>%
      ggplot(aes(horas, cantidad)) +
      #noche
      annotate(geom = "rect", xmin = 0, xmax = 12, ymin = -Inf, ymax = Inf,
               fill = color_oscuro, alpha = 0.3) +
      #día
      # annotate(geom = "rect", xmin = 12, xmax = 23, ymin = -Inf, ymax = Inf,
      #           fill = color_claro, alpha = 0) +
      #laboral
      #annotate(geom = "rect", xmin = 8, xmax = 18, ymin = -Inf, ymax = Inf,
      #          fill = color_medio, alpha = 0.2) +
      geom_vline(xintercept = c(8, 18), col = color_claro, size = 1, alpha = 0.3) +
      #línea
      geom_smooth(fill = color_claro, size = 0, alpha = 0.3) +
      geom_line(col = color_claro, size = 1, alpha = 0.8, stat = "smooth", method = "loess", lineend = "round") +
      #puntos
      #geom_point(col = color_blanco, size = 5) +
      geom_point(col = color_blanco, size = 3) +
      #íconos
      geom_text(label = "\uF185", col = color_claro, alpha = 0.7, aes(x = 21+0.5, y = punto_maximo*0.1),
                size = 12, family = 'FontAwesome', show.legend=F, inherit.aes = F, check_overlap = T, hjust = 0.5) +
      geom_text(label = "\uF186", col = color_claro, alpha = 0.7, aes(x = 3-0.5, y = punto_maximo*0.9),
                size = 12, family = 'FontAwesome', show.legend=F, inherit.aes = F, check_overlap = T, hjust = 0.5) +
      scale_x_continuous(labels = function(x) paste(x, "hrs."), 
                         breaks = c(1, 6, 12, 18, 23), expand = expansion(add = c(0, 1.5))) +
      coord_cartesian(clip = "off") +
      theme_void() +
      theme(axis.text.y = element_text(hjust = 1, color = color_negro, family = "Montserrat", size = 9, margin = margin(r = 5)),
            axis.text.x = element_text(hjust = 0.5, color = color_negro, family = "Montserrat", size = 9, margin = margin(t = 5, b=2))) +
      theme(plot.background = element_rect(fill = color_fondo, color = color_fondo))
    return(p)
  }, res = 100)  
  
  #—----
  
  #PORTAL ----
  
  #precio por tipo ----
  output$portal_precio_tipo <- renderPlot({
    req(base_portal)
    
    p <- base_portal %>% 
      group_by(tipo) %>% 
      summarize(promedio = median(precio),
                min = min(precio),
                max = max(precio),
                q = list(quantile(precio))) %>% #calcular cuartiles
      tidyr::unnest_wider(q) %>% 
      #graficar
      ggplot(aes(y = tipo)) +
      #cuartiles
      geom_segment(col = color_claro, size=1.4, lineend = "round", aes(xend = `75%`, x = `25%`, yend = tipo)) +
      #promedio
      geom_point(aes(x = promedio), size = 6, col = color_blanco) +
      geom_point(aes(x = promedio), size = 4, col = color_claro) +
      #texto
      #encima
      geom_text(aes(x = promedio, label = paste(round(promedio/1000000,1), "M")), nudge_y = 0.2,
                col = color_blanco, size = 5, angle = 0, hjust = 0.5, vjust = 0, family = "Dosis ExtraLight SemiBold") +
      #minimo
      geom_text(aes(x = `25%`, label = paste(round(`25%`/1000000,1), " ")),
                col = color_claro, size = 3.5, angle = 0, hjust = 1.1, vjust = 0.5, family = "Dosis ExtraLight SemiBold") +
      #maximo
      geom_text(aes(x = `75%`, label = paste(" ", round(`75%`/1000000,1))),
                col = color_claro, size = 3.5, angle = 0, hjust = -0.1, vjust = 0.5, family = "Dosis ExtraLight SemiBold") +
      theme_void() +
      coord_cartesian(clip = "off") +
      scale_x_continuous(labels = function (x) paste(x/1000000, "M"), 
                         expand = expansion(mult = c(0.2, 0.2)),
                         breaks = c(0, 1000000, 2000000, 3000000, 4000000, 5000000)) +
      theme(axis.text.y = element_text(hjust = 1, color = color_negro, family = "Montserrat", size = 9, margin = margin(r = 5)),
            axis.text.x = element_text(hjust = 0.5, color = color_negro, family = "Montserrat", size = 9, margin = margin(t = 5, b=2))) +
      theme(plot.background = element_rect(fill = color_fondo, color = color_fondo)) +
      theme(panel.grid.major.x = element_line(color = color_medio_2, size = 0.7))
    
    return(p)
  }, res = 100)  
  
  
  #precio por metros ----
  output$portal_precio_metros <- renderPlot({
    req(base_portal)
    
    p <- base_portal %>% 
      group_by(metros_cat) %>%
      summarize(promedio = mean(precio),
                min = min(precio),
                max = max(precio),
                n = n()) %>% 
      mutate(promedio = round(promedio/1000000, 2)) %>% 
      graficar_barras_horizontales(variable_categorica = "metros_cat",
                                   variable_numerica = "promedio")
    
    return(p)
  }, res = 100)  
  
  
  #relacion precio metros ----
  output$portal_relacion_precio_metros <- renderPlot({
    req(base_portal)
    
    p <- base_portal %>% 
      ggplot(aes(as.numeric(metros), precio/1000000, col=tipo)) +
      geom_point(size = 3, alpha = 0.9) +
      xlim(0, 500) +
      ylim(0, 6) +
      scale_color_manual(values = colores_azules_3_grupos_2[c(1, 4, 6, 8)]) +
      theme_void() +
      labs(y = "Precio de arriendo (en millones)",
           x = "Metros cuadrados") +
      theme(axis.text.y = element_text(hjust = 1, color = color_negro, family = "Montserrat", size = 9, margin = margin(r = 5)),
            axis.text.x = element_text(hjust = 0.5, color = color_negro, family = "Montserrat", size = 9, margin = margin(t = 5, b=2))) +
      theme(axis.title.y = element_text(family = "Montserrat", color = color_oscuro, angle=90, margin=margin(r=6)),
            axis.title.x = element_text(family = "Montserrat", color = color_oscuro, angle=0, margin=margin(t=6, b=4))) +
      theme(plot.background = element_rect(fill = color_fondo, color = color_fondo)) +
      theme(panel.grid.major.x = element_line(color = color_medio_2, size = 0.7),
            panel.grid.major.y = element_line(color = color_medio_2, size = 0.7)) +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(family = "Montserrat", color = color_negro, margin = margin(r=16))) +
      guides(color = guide_legend(override.aes = list(size = 4)))
    
    return(p)
  }, res = 100)  
  
  
  #precio por dormitorios ----
  output$portal_precio_dormitorios <- renderPlot({
    req(base_portal)
    
    p <- base_portal %>% 
      filter(!is.na(dormitorios)) %>% 
      mutate(dormitorios_cat = case_when(dormitorios == 1 ~ "1 dormitorio",
                                         dormitorios >= 10 ~"10+ dormitorios",
                                         TRUE ~ paste(dormitorios, "dormitorios"))) %>% 
      mutate(dormitorios_cat = as.factor(dormitorios_cat),
             dormitorios_cat = forcats::fct_relevel(dormitorios_cat, "10+ dormitorios", after = Inf),
             dormitorios_cat = forcats::fct_rev(dormitorios_cat)) %>% 
      group_by(dormitorios_cat) %>% 
      summarize(promedio = mean(precio),
                min = min(precio),
                max = max(precio)) %>% 
      mutate(promedio = round(promedio/1000000, 2)) %>% 
      graficar_barras_horizontales(variable_categorica = "dormitorios_cat",
                                   variable_numerica = "promedio", rank = F)
    
    return(p)
  }, res = 100)  
  
  
  #—----
  
  #CURSOS ----
  shinyjs::hide("cursos_resultado")
  
  #estado de los selectores: es false si quedan por responder, true si se responden todos
  estado_selectores_cursos <- reactive({
    if (is.null(input$v_compe) | 
        is.null(input$v_nivel) |
        is.null(input$v_deseo) |
        is.null(input$v_conoc) |
        is.null(input$v_recur) |
        is.null(input$v_compe)) {
      
      estado <- FALSE 
    } else { estado <- TRUE }
    return(estado)
  })
  
  #ocultar alerta
  observe({
    if (estado_selectores_cursos() == TRUE) { 
      #ocultar alerta y mostrar resultados cuando ya se respondió
      shinyjs::hide("cursos_alerta") 
      shinyjs::show("cursos_resultado")
    } else {
      #mostrar alerta
      shinyjs::show("cursos_alerta") 
      shinyjs::hide("cursos_resultado")
    }
  }) |> 
    bindEvent(estado_selectores_cursos())
  
  
  #recomendaciones ----
  parametros <- reactiveValues(tmp=NULL)
  
  cursos <- reactive({
  #observe({
    if (estado_selectores_cursos() == TRUE) { 
    #print("de panita")
        reco <- recomendacion(v_nivel = input$v_nivel, 
                      v_deseo = input$v_deseo, 
                      v_conoc = input$v_conoc, 
                      v_recur = input$v_recur, 
                      v_compe = input$v_compe,
                      v_rubro = input$rubro)
        Sys.sleep(3.5)
        return(reco)
    }
    }) |> 
    bindEvent(estado_selectores_cursos())
  
  #para pruebas:
  #   cursos <- function() {
  #     recomendacion(nombre = "gimnasio local",
  #                 v_nivel = 2, v_deseo = 1, v_conoc = 1, v_recur = 1, v_compe = 1,
  #                 v_rubro = "Actividades artísticas, de entretenimiento y recreativas")
  #   }
  # cursos()
  
  #observe({ print(cursos()) })
  
  
  #resultado----
  # #output$dynamic_ui_p0_clave <- renderUI({
  # 
  # botonera <- function(datos) {
  #   #crear x elementos donde .x es el numero de boletín
  #   
  #   message("ejecutando botonera")
  #   
  #   cursos_recomendados <- datos
    
  output$cursos_resultado_botones <- renderUI({
      ui_elems <- purrr::map(as.character(cursos()), ~{
        
        #una tabla que haga match con los cursos para darle título, nivel, descripción e ícono
        #x_id <-  janitor::make_clean_names(.x, case = "snake")
        
        #x_curso <- .x
        
        #titulo
        x_curso <- cursos_tabla |> 
          dplyr::filter(id_curso == .x) |> 
          pull(titulo)
        
        # x_nivel <- cursos_tabla |> 
        #   dplyr::filter(id_curso == .x) |> 
        #   pull(nivel)
        
        x_orden <- cursos() |> tibble() |> rename(curso = 1) |> mutate(n = 1:n()) |> 
          filter(curso == .x) |> 
          pull(n)
        
        #botones
        output <- column(12, style = "margin:0px; padding:0px;",
                         actionButton(inputId = paste0("btn_curso_", .x),
                                      class = "boton_dinamico", #para el hover
                                      style = glue::glue("width: 100%;
                                                         /*height: 60px;*/
                                                         padding: 6px;
                                                         margin-left: auto;
                                                         margin-right: auto;
                                                         margin-bottom: 20px;
                                                         background: {color_claro};
                                                         border-radius: 5px;
                                                         border: none;
                                                         color: white;"),
                                      label = div(style = "width: 100%; padding: 6px; text-align: left; white-space: initial;",
                                                  #numero
                                                  div(style = "max-width: 30px; height: 40px;
                                                               font-size: 30px; font-family: 'Dosis', sans-serif; font-weight: 800;
                                                               padding: 6px; padding-top: 10px;
                                                               position:absolute;
                                                               margin: 0px;
                                                               margin-left: 0px;
                                                               top: 0; right: 1;",
                                                      div(x_orden, style = "color: white; opacity: 1; text-align: right !important;")
                                                  ),
                                                  #titulo
                                                  div(style = "/*white-space: nowrap;*/
                                                               font-size: 100%;
                                                               margin-left: 36px;
                                                               width: 80%;
                                                               font-weight: bold; margin-bottom: -10px;",
                                                      p(x_curso)
                                                  )
                                      )
                         )
        )
        return(output)
      })
      return(tagList(ui_elems))
    }) 
    
    #crear reactives correspondientes a los botones
    #al apretar cualquier botón dinámico, muestra panel de info y oculta los botones
    #y además crea una variable para acceder al botón que se apretó
    observe({
      purrr::map(as.character(cursos()), ~{
        
        #x_id <-  janitor::make_clean_names(.x, case = "snake")
        #x_id <-  janitor::make_clean_names(cursos()[4], case = "snake") #para pruebas
        
        observeEvent(input[[paste0("btn_curso_", .x)]], {

          #crear texto que tiene el boletín del botón apretado
          #output$boton_apretado <- renderText({ .x })

          print(.x)
          
          #crear variable que tiene el boletín del botón apretado
          #parametros <- list() #para pruebas
          parametros$btn_curso_elegido <- .x #se puede usar en otros reactives

          shinyjs::hide("btn_curso_vacio")
          
          #Sys.sleep(0.2)
          #shinyjs::hide("botones_dinamicos_p2") #ocultar botones
          #updateTabsetPanel(session, "tabs_invisibles", selected = "Proyectos de ley")
          # updateTabsetPanel(session, "tabs_visibles", selected = "Proyectos de ley")
          # shinyjs::hide("filtros_p2") #ocultar barra superior de filtros
          # shinyjs::hide("cargar_mas_p2")
          # shinyjs::show("panel_p2") #mostrar panel
        })
      })
    })
    
    
    #textos ----
    output$btn_curso_titulo <- renderText({
      req(parametros$btn_curso_elegido)
      
      curso_elegido <- as.character(parametros$btn_curso_elegido)
      
      cursos_tabla |> 
        dplyr::filter(id_curso == curso_elegido) |> 
        pull(titulo) |> 
        toupper()
    })
    
    output$btn_curso_descripcion <- renderText({
      req(parametros$btn_curso_elegido)
      
      curso_elegido <- as.character(parametros$btn_curso_elegido)
      
      cursos_tabla |> 
        dplyr::filter(id_curso == curso_elegido) |> 
        pull(descripcion)
      })
    
    
    #message("ok botonera")
    #return(render)
  #} #fin funcion botonera
  
  # #output
  # output$cursos_resultado <- reactive({ 
  #   if (estado_selectores_cursos() == TRUE) { 
  #     message("rendering botonera")
  #   b <- botonera(datos = cursos())
  #   return(b) 
  #   }
  # })
  
  
  
  #—----
  #ESTUDIOS ----
  
  # output$descarga_estudio_ofertas_laborales <- downloadHandler(
  #   filename = "Estudio-ofertas-laborales.html",
  #   content = function(file) {
  #     file.copy("www/Estudio ofertas laborales/Estudio-ofertas-laborales.html", file)
  #   }, 
  #   contentType = "text/html")
  
  
  #DESCARGAS ----
  
  output$descarga_taller <- downloadHandler(
    filename = "FondosConcursables.pdf",
    content = function(file) {
      file.copy("www/descargas/FondosConcursables.pdf", file)
    }, 
    contentType = "application/pdf")
  
  output$descarga_cuadro_regimen <- downloadHandler(
    filename = "CuadroComparativodelRegimenGeneralySimplificado.pdf",
    content = function(file) {
      file.copy("www/descargas/CuadroComparativodelRegimenGeneralySimplificado.pdf", file)
    }, 
    contentType = "application/pdf")
  
  output$descarga_cuadro_empresas <- downloadHandler(
    filename = "CuadroComparativodeEmpresasySociedades.pdf",
    content = function(file) {
      file.copy("www/descargas/CuadroComparativodeEmpresasySociedades.pdf", file)
    }, 
    contentType = "application/pdf")
  
})

#Bastián Olea Herrera (@bastimapache)
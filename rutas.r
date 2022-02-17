library(dplyr)
source("dataemprende/variables.r")

# Brechas posibles ----
# En torno a la posibilidad de transformación digital o digitalización de pymes, se hipotetizan 5 posibles tipos de brechas en los que calificarían. Cada brecha puede tener un nivel asociado, con un polo negativo y otro positivo.
# 
# 1. **Deseo e interés**
#  - pymes sin interés o necesidad inmediata de digitalización
#  - pymes en rubros que no conciben posibilidad de digitalización
#
# 2. **Necesidad formativa**
#  - pymes carentes de conocimientos básicos para digitalizarse
#  - pymes que no identifican la necesidad de digitalizarse
#
# 3. **Necesidad de capital**
#  - pymes sin acceso a recursos humanos, económicos o financieros para digitalizarse
#
# 4. **Necesidad competitiva**
#  - pymes rezagadas respecto de sus competidores
#  - pymes que podrían aumentar sus ventas por medio de digitalización
#
# 5. **Nivel de digitalización**
#   - pymes que pueden/necesitan avanzar en su nivel de digitalización


#Operacionalización ----
# ¿cuál es el nivel de digitalización de su pyme? #(nivel de digitalización)
#   nulo/bajo/medio/alto/vanguardista
# 
# ¿le interesa digitalizar su pyme en el corto o mediano plazo? #(deseo e interés)
#   sí/no
#   
# ¿cuál es el nivel de manejo de tecnologías digitales en su pyme? #(necesidad formativa)
#   bajo/medio/alto
# 
# ¿cuenta con recursos (económicos, técnicos o humanos) para digitalizarse? #(necesidad de capital)
#   sí/no
#   
# ¿cómo se compara su pyme en términos de digitalización con respecto a su competencia? #(necesidad competitiva)
#   peor/igual/mejor


#Variables ----

#(nivel de digitalización)
v_nivel = 4     # 1 2 3 4 5

#(deseo e interés)
v_deseo = 1     # 0 1

#(necesidad formativa)
v_conoc = 2     # 1 2 3 

#(necesidad de capital)
v_recur = 0     # 0 1

#(necesidad competitiva)
v_compe = 1     # 1 2 3

#rubros
v_rubro = rubros_sii

# Rutas ----
# 1. **Rutas respecto a disposición** _(deseo e interés)_
# - cursos de evangelización
# - cursos de introducción
#
# 2. **Rutas respecto a nivel de formación** _(necesidad formativa)_
# - cursos introductorios
# - cursos temáticos
# - cursos de herramientas básicas
#
# 3. **Rutas respecto a capacidad** de transformación _(necesidad de capital)_
# - cursos de postulación a proyectos
# - cursos de planificación
#
# 4. **Rutas respecto a intereses de su nicho** _(necesidad competitiva)_
# - cursos de nivelación con competencia
# - cursos de innovación
#
# 5. **Rutas respecto a nivel de digitalización**
# - cursos de herramientas avanzadas



cursos <- vector()
cursos <- list()

recomendacion <- function(nombre = "ejemplo", v_deseo, v_conoc, v_recur, v_compe, v_nivel, v_rubro = "ninguno") {
  # 1. Rutas respecto a disposición (deseo e interés)
  c_deseo <- case_when(v_deseo == 0 ~ c("evangelización"),
                       #v_nivel <= 3 & v_deseo == 1 ~ c("fidelización")
  )
  
  # 2. Rutas respecto a nivel de formación (necesidad formativa)
  c_conoc <- case_when(v_conoc == 1 ~ c("introducción", "aplicación"),
                       v_conoc == 2 ~ c("aplicación", NA_character_)
                       #v_deseo == 1 & v_conoc >= 2 ~ c("temáticos") #depende del rubro
  ) 
  
  # 3. Rutas respecto a capacidad de transformación (necesidad de capital)
  c_recur <- case_when(v_nivel < 4 & v_recur == 0 ~ c("postulación a proyectos"),
                       #v_recur == 1 ~ c("planificación")
  )
  
  # 4. Rutas respecto a intereses de su nicho (necesidad competitiva)
  c_compe <- case_when(v_compe == 1 ~ c("evangelización"),
                       v_compe == 2 ~ c("innovación"),
                       #v_compe == 3 ~ c("innovación")
  )
  
  # 5. Rutas respecto a nivel de digitalización
  c_nivel <- case_when(v_nivel == 1 ~ c("evangelización", "introducción"),
                       v_nivel == 2 ~ c("introducción", "herramientas básicas"),
                       v_nivel == 3 ~ c("herramientas básicas", "herramientas intermedias"),
                       v_nivel == 4 ~ c("herramientas intermedias", "herramientas avanzadas"),
                       v_nivel == 5 ~ c("herramientas avanzadas", NA_character_)
  )
  
  # Rutas combinatorias
  c_recur_nivel <- case_when(v_recur == 1 & v_nivel >= 3 ~ c("innovación") ) #con recursos y nivel (tiene posibilidad de innovar)
  
  c_deseo_compe <- case_when(v_deseo == 0 & v_compe >= 2 ~ c("evangelización") ) #sin interés y aún así mejor que la competencia (convencerle de que con tecnologías puede mejorar más)
  
  #rubros de venta pero sin nivel
  #(convencer de digitalizarse para ventas online, marketing digital)
  
  if (v_rubro == "ninguno") {
    c_temat <- NA_character_
  } else {
    c_temat <- case_when(v_nivel <= 4 & v_rubro == "Actividades artísticas, de entretenimiento y recreativas" ~    c("temático 1: marketing digital", NA_character_, NA_character_),
                         v_nivel <= 4 & v_rubro == "Actividades profesionales, científicas y técnicas" ~           c("temático 3: análisis de datos", NA_character_, NA_character_),
                         v_nivel <= 4 & v_rubro == "Otras actividades de servicios" ~                              c("temático 2: ventas online", "temático 4: herramientas financieras", NA_character_),
                         v_nivel <= 4 & v_rubro == "Actividades de alojamiento y de servicio de comidas" ~         c("temático 1: marketing digital", "temático 4: herramientas financieras", NA_character_),
                         v_nivel <= 4 & v_rubro == "Comercio al por mayor y al por menor, reparación de vehículos automotores y motocicletas" ~ c("temático 1: marketing digital", "temático 2: ventas online", "temático 4: herramientas financieras"),
                         v_nivel <= 4 & v_rubro == "Transporte y almacenamiento" ~                                 c("temático 5: logística", "temático 4: herramientas financieras", NA_character_)
    )
  }
  
  #concatenar cursos
  cursos <- c(c_deseo,
              c_conoc,
              c_recur,
              c_compe,
              c_nivel,
              #
              c_recur_nivel,
              c_deseo_compe,
              #
              c_temat) |> 
    #limpieza
    unique() |> na.exclude() |> as.vector()
  
  #parches
  #if ("cursos de innovación" %in% cursos) { cursos <- cursos[! cursos %in% c("cursos de evangelización")] }
  #if ("cursos de innovación" %in% cursos) { cursos <- cursos[! cursos %in% c("cursos de evangelización")] }
  
  #texto
  message("Negocio: ", toupper(nombre))
  message("Nivel: ", case_when(v_nivel == 1 ~ "nulo", v_nivel == 2 ~ "bajo", v_nivel == 3 ~ "medio", v_nivel == 4 ~ "alto", v_nivel == 5 ~ "vanguardista"))
  message(case_when(v_deseo == 0 ~ "Sin interés", v_deseo == 1 ~ "Con interés"), " en digitalizarse")
  message("Manejo ", case_when(v_conoc == 1 ~ "bajo", v_conoc == 2 ~ "medio", v_conoc == 3 ~ "alto"), " de tecnologías")
  message(case_when(v_recur == 0 ~ "Sin recursos", v_recur == 1 ~ "Con recursos"), " para digitalizarse")
  message("Se considera ", case_when(v_compe == 1 ~ "peor", v_compe == 2 ~ "igual de", v_compe == 3 ~ "mejor"), " digitalizado que su competencia")
  if (v_rubro != "ninguno") { message("Rubro: ", stringr::str_trunc(v_rubro, 40)) }
  message("Recomendación de ", length(cursos), " cursos: ", paste(cursos, collapse = ", "), "\n")
  return(cursos)
  
}

# Obtener ----

({
  recomendacion(nombre = "pequeño almacén", 
                v_nivel = 1, v_deseo = 0, v_conoc = 1, v_recur = 0, v_compe = 1, 
                v_rubro = "Comercio al por mayor y al por menor, reparación de vehículos automotores y motocicletas")
  
  recomendacion(nombre = "food truck con pos", 
                v_nivel = 3, v_deseo = 1, v_conoc = 2, v_recur = 0, v_compe = 3, 
                v_rubro = "Actividades de alojamiento y de servicio de comidas")
  
  recomendacion(nombre = "empresa que vende online", 
                v_nivel = 3, v_deseo = 1, v_conoc = 3, v_recur = 1, v_compe = 3, 
                v_rubro = "Comercio al por mayor y al por menor, reparación de vehículos automotores y motocicletas")
  
  recomendacion(nombre = "fintech", 
                v_nivel = 5, v_deseo = 1, v_conoc = 3, v_recur = 1, v_compe = 3, 
                v_rubro = "Otras actividades de servicios")
  
  recomendacion(nombre = "peluquería con instagram", 
                v_nivel = 3, v_deseo = 0, v_conoc = 2, v_recur = 1, v_compe = 2, 
                v_rubro = "Otras actividades de servicios")
  
  recomendacion(nombre = "manicurista a domicilio", 
                v_nivel = 2, v_deseo = 0, v_conoc = 2, v_recur = 0, v_compe = 3, 
                v_rubro = "Otras actividades de servicios")
  
  recomendacion(nombre = "gimnasio local", 
                v_nivel = 2, v_deseo = 1, v_conoc = 1, v_recur = 1, v_compe = 1, 
                v_rubro = "Actividades artísticas, de entretenimiento y recreativas")
  
  recomendacion(nombre = "empresa de transportes", 
                v_nivel = 3, v_deseo = 1, v_conoc = 2, v_recur = 1, v_compe = 2, 
                v_rubro = "Transporte y almacenamiento")
})

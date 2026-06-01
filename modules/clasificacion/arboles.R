# =====================================================
#  Árboles de decisión
# =====================================================

# -------------------------------
# TEORIA
# -------------------------------

Arboles_Teoria_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Única llamada necesaria para activar MathJax en toda la página
    withMathJax(),
    
    tags$div(
      style = "padding: 20px; background-color: #fcfdfe;",
      
      # =====================================
      # CABECERA
      # =====================================
      h2(
        "Árboles de Clasificación",
        style = "font-weight: 800; color: #1a365d; margin-bottom: 5px;"
      ),
      
      p(
        "Modelos predictivos basados en el aprendizaje supervisado que descubren de forma recursiva reglas de asociación estructuradas en flujos lógicos.",
        style = "color: #64748b; font-size: 1.1rem; margin-bottom: 30px;"
      ),
      
      # =====================================
      # TARJETAS PRINCIPALES
      # =====================================
      bslib::layout_column_wrap(
        width = 1/3, # Tres columnas perfectamente alineadas
        heights_equal = "row",
        
        # ---------------------------------
        # 1. ¿QUÉ ES UN ÁRBOL?
        # ---------------------------------
        bslib::card(
          bslib::card_header(
            tags$b("1. Estructura y Flujo"),
            style = "background: #e0e7ff;"
          ),
          bslib::card_body(
            p("Técnica que segmenta jerárquicamente la muestra traduciendo las etiquetas conocidas de \\(Y\\) según sus atributos predictivos (Breiman et al., 2017). Se compone de:"),
            tags$ul(
              style = "margin-top: 10px;",
              tags$li(tags$b("Nodos y Ramas:"), " El nodo raíz (superior) e intermedios denotan pruebas lógicas sobre los atributos; las ramas bifurcan sus resultados.", style = "margin-bottom: 6px;"),
              tags$li(tags$b("Hojas:"), " Nodos terminales que contienen la clase o categoría final asignada al individuo.")
            )
          )
        ),
        
        # ---------------------------------
        # 2. INDUCCIÓN E HISTORIA
        # ---------------------------------
        bslib::card(
          bslib::card_header(
            tags$b("2. Algoritmos Fundamentales"),
            style = "background: #dcfce7;"
          ),
          bslib::card_body(
            p("Desarrollados de manera independiente entre finales de los 70 y principios de los 80, dieron pie al grueso de la inducción moderna (Han et al., 2011):"),
            tags$ul(
              style = "margin-top: 10px;",
              tags$li(tags$b("Línea ID3 / C4.5:"), " Diseñados por J. Ross Quinlan, basados en el particionamiento multi-rama a través de la ganancia de información.", style = "margin-bottom: 6px;"),
              tags$li(tags$b("Algoritmo CART:"), " Propuesto por Breiman, enfocado explícitamente en la generación iterativa de árboles de decisión binarios.")
            )
          )
        ),
        
        # ---------------------------------
        # 3. ATRIBUTOS CRÍTICOS
        # ---------------------------------
        bslib::card(
          bslib::card_header(
            tags$b("3. Ventajas y Rendimiento"),
            style = "background: #fef3c7;"
          ),
          bslib::card_body(
            p("Esta metodología goza de una amplia adopción en minería de datos debido a sus propiedades intrínsecas (Han et al., 2011):"),
            tags$ul(
              style = "margin-top: 10px;",
              tags$li(tags$b("Cero caja negra:"), " Proporciona gráficos e interpretaciones intuitivas, idóneas para hallar patrones ocultos.", style = "margin-bottom: 6px;"),
              tags$li(tags$b("Sin configuración:"), " No exige calibrar parámetros previos estrictos para entrenar sus reglas iniciales.", style = "margin-bottom: 6px;"),
              tags$li(tags$b("Robustez:"), " Destaca por su fiabilidad y precisión en entornos multivariantes.")
            )
          )
        )
      ), # Fin del layout_column_wrap
      
      br(),
      
      # =====================================
      # CRITERIOS DE PARTICIÓN
      # =====================================
      bslib::card(
        style = "border: 1px solid #cbd5e1; background: #f8fafc;",
        bslib::card_body(
          h4(icon("chart-pie"), "Criterios de Partición y Medidas de Impureza", style = "color: #1e40af; margin-bottom: 15px;"),
          p("Para realizar las divisiones lógicas, el algoritmo busca aislar las clases de forma que cada subnodo sea lo más homogéneo ('puro') posible, apoyándose en dos indicadores analíticos principales:"),
          
          bslib::layout_column_wrap(
            width = 1/2,
            style = "gap: 20px; margin-top: 15px;",
            
            # Subcolumna: Entropía
            tags$div(
              style = "border: 1px solid #e2e8f0; padding: 15px; border-radius: 8px; background: white;",
              tags$h5(tags$b("Entropía y Ganancia de Información"), style = "color: #1a365d; margin-bottom: 10px;"),
              p("La entropía mide el grado de incertidumbre o desorden del subconjunto de datos \\(\\mathbf{S}\\):"),
              p("$$H(\\mathbf{S}) = -\\sum_{i=1}^{c} p_i \\log_2 p_i$$"),
              p("Donde \\(c\\) es el total de clases e \\(p_i\\) es la proporción del grupo \\(i\\). A partir de ella, se evalúa un atributo \\(A\\) buscando el que ", tags$b("maximice la ganancia"), " de reducción de desorden:"),
              p("$$\\text{Gain}(\\mathbf{S}, A) = H(\\mathbf{S}) - \\sum_{v \\in \\text{Valores}(A)} \\frac{|\\mathbf{S}_v|}{|\\mathbf{S}|} H(\\mathbf{S}_v)$$")
            ),
            
            # Subcolumna: Gini
            tags$div(
              style = "border: 1px solid #e2e8f0; padding: 15px; border-radius: 8px; background: white;",
              tags$h5(tags$b("Índice de Gini"), style = "color: #1a365d; margin-bottom: 10px;"),
              p("Utilizado de forma predeterminada por el algoritmo CART, mide directamente la impureza o grado de mezcla de clases del nodo:"),
              p("$$\\text{Gini}(\\mathbf{S}) = 1 - \\sum_{i=1}^{c} p_i^2$$"),
              p("Este índice arroja valores mínimos (cercanos a 0) si la inmensa mayoría de las observaciones pertenecen a una única categoría. Aquí, el algoritmo selecciona el atributo que ", tags$b("minimice el índice de Gini"), ", forzando particiones lo más puras posibles.")
            )
          )
        )
      ),
      
      br(),
      
      # =====================================
      # DINÁMICA DE CONSTRUCCIÓN Y PARADA
      # =====================================
      bslib::card(
        style = "border: 1px solid #cbd5e1; background: #f8fafc;",
        bslib::card_body(
          h4(icon("gears"), "Construcción Recursiva y Condiciones de Parada", style = "color: #1e40af; margin-bottom: 15px;"),
          p("El proceso sigue un enfoque recursivo descendente que arranca desde el nodo raíz con la totalidad de la muestra y divide los datos de forma iterativa según la tipología de las variables:"),
          tags$ul(
            tags$li(tags$b("Variables categóricas:"), " Se abren simultáneamente tantas ramas físicas como valores distintos posea el atributo evaluado.", style = "margin-bottom: 6px;"),
            tags$li(tags$b("Variables continuas:"), " Se calcula un punto de corte óptimo que segmenta los datos en dos subgrupos, originando particiones binarias.")
          ),
          
          hr(style = "border-top: 1px solid #cbd5e1; margin: 15px 0;"),
          
          h5("Condiciones de Parada Estructurales:", style = "color: #1a365d; font-weight: bold; margin-bottom: 10px;"),
          tags$div(
            style = "display: flex; flex-direction: column; gap: 10px; margin-left: 10px;",
            tags$div(style = "border-left: 4px solid #10b981; padding-left: 12px;", "El subnodo alcanza un estado completamente puro (todos sus individuos pertenecen ya a la misma clase de \\(Y\\))."),
            tags$div(style = "border-left: 4px solid #3b82f6; padding-left: 12px;", "Se agotan los atributos predictivos disponibles en la muestra para continuar segmentando."),
            tags$div(style = "border-left: 4px solid #f59e0b; padding-left: 12px;", "Se alcanzan los límites prefijados por el investigador, tales como la profundidad máxima permitida o el número mínimo de observaciones por nodo.")
          )
        )
      ),
      
      br(),
      
      # =====================================
      # PODA (PRUNING)
      # =====================================
      bslib::card(
        style = "border: 1px solid #cbd5e1; background: #f8fafc;",
        bslib::card_body(
          h4(icon("scissors"), "Técnicas de Poda (Pruning) contra el Sobreajuste", style = "color: #1e40af; margin-bottom: 15px;"),
          p("Permitir que el árbol se ramifique sin restricciones suele inducir al sobreajuste (overfitting): el modelo memoriza el ruido de entrenamiento, mermando su capacidad de generalizar ante nuevos individuos (Han et al., 2011). Para mitigar este problema se aplican dos aproximaciones:"),
          tags$ul(
            tags$li(tags$b("Poda previa (Pre-pruning):"), " Detiene el algoritmo de forma temprana antes de que la estructura gane una complejidad excesiva.", style = "margin-bottom: 6px;"),
            tags$li(tags$b("Poda posterior (Post-pruning):"), " Colapsa ramas y simplifica nodos una vez que el árbol ha alcanzado su máximo crecimiento.")
          ),
          
          hr(style = "border-top: 1px solid #cbd5e1; margin: 20px 0;"),
          
          tags$div(
            style = "border-left: 4px solid #ef4444; background: #fef2f2; padding: 12px; border-radius: 0 8px 8px 0;",
            tags$h5(tags$b("Criterio de Coste-Complejidad en CART:"), style = "color: #991b1b; margin-bottom: 8px;"),
            p("Busca un compromiso matemático óptimo que equilibre el error de clasificación con el número de hojas mediante la penalización por el parámetro \\(\\alpha\\):"),
            p("$$R_\\alpha(T) = R(T) + \\alpha |T|$$"),
            
            p(style = "font-size: 0.9rem; margin-bottom: 0;", 
              "Donde \\(R(T)\\) representa el error empírico del árbol \\(T\\), \\(|T|\\) es el conteo de nodos terminales y \\(\\alpha\\) es el factor regulador. Valores elevados de \\(\\alpha\\) barren la estructura en favor de modelos altamente interpretables y robustos.")
          ) 
        ) 
      ) 
    ) 
  ) 
} 

Arboles_Teoria_Server <- function(id){
  moduleServer(id, function(input, output, session){ })
}



# -------------------------------
# ANALISIS
# -------------------------------
Arboles_Analisis_UI <- function(id){
  
  ns <- NS(id)
  
  tagList(
    
    h3("Árboles de Decisión - Aplicación práctica"),
    
    br(),
    
    fluidRow(
      
      # =====================================================
      # PANEL IZQUIERDO
      # =====================================================
      
      column(
        
        width = 3,
        
        wellPanel(
          
          h4("Configuración del modelo"),
          
          uiOutput(ns("target_ui")),
          
          br(),
          
          uiOutput(ns("predictors_ui")),
          
          br(),
          
          sliderInput(
            ns("maxdepth"),
            "Profundidad máxima:",
            min = 1,
            max = 5,
            value = 3,
            step = 1
          ),
          
          br(),
          
          selectInput(
            ns("palette"),
            "Paleta de colores:",
            choices = c(
              "Azules" = "Blues",
              "Verdes" = "Greens",
              "Rojo-Amarillo-Verde" = "RdYlGn",
              "Púrpuras" = "Purples"
            ),
            selected = "RdYlGn"
          ),
          
          br(),
          
          actionButton(
            ns("run_tree"),
            "Ejecutar modelo",
            class = "btn-primary btn-block"
          )
          
        )
      ),
      
      # =====================================================
      # PANEL DERECHO
      # =====================================================
      
      column(
        
        width = 9,
        
        tabsetPanel(
          
          tabPanel(
            
            "Árbol",
            
            br(),
            
            plotOutput(
              ns("tree_plot"),
              height = "750px"
            )
            
          ),
          
          tabPanel(
            
            "Importancia de Variables",
            
            br(),
            
            plotOutput(
              ns("importance_plot"),
              height = "500px"
            )
            
          ),
          
          tabPanel(
            
            "Reglas de Decisión",
            
            br(),
            
            verbatimTextOutput(ns("tree_rules"))
            
          ),
          
          tabPanel(
            
            "Datos",
            
            br(),
            
            h4("Dataset original"),
            tableOutput(ns("dataset")),
            
            br(),
            
            h4("Dataset estandarizado"),
            tableOutput(ns("dataset_std"))
            
          )
          
        )
      )
    )
  )
}
# -------------------------------
# 2. CONTROLADOR (SERVER)
# -------------------------------
Arboles_Analisis_Server <- function(id, datos, datos_ejemplo = NULL){
  
  moduleServer(id, function(input, output, session){
    
    # =====================================================
    # 1. DATOS BASE
    # =====================================================
    
    datos_base <- reactive({
      
      df <- if(!is.null(datos()) && nrow(datos()) > 0){
        datos()
      } else {
        datos_ejemplo
      }
      
      req(df)
      
      # Conversión inteligente tipo BreastCancer
      df[] <- lapply(df, function(x){
        
        if(is.factor(x) || is.character(x)){
          
          as_n <- suppressWarnings(as.numeric(as.character(x)))
          
          if(sum(!is.na(as_n)) > (length(x)*0.8)){
            as_n
          } else {
            x
          }
          
        } else {
          x
        }
      })
      
      na.omit(df)
    })
    
    
    # =====================================================
    # 2. SELECTORES
    # =====================================================
    
    output$target_ui <- renderUI({
      
      selectInput(
        session$ns("target_var"),
        "Variable Objetivo (Y):",
        choices = names(datos_base()),
        selected = names(datos_base())[ncol(datos_base())]
      )
      
    })
    
    
    output$predictors_ui <- renderUI({
      
      req(input$target_var)
      
      opciones_x <- setdiff(names(datos_base()), input$target_var)
      
      selectizeInput(
        session$ns("predictor_vars"),
        "Variables Independientes (X):",
        choices = opciones_x,
        multiple = TRUE,
        selected = opciones_x[1:min(2, length(opciones_x))],
        
        options = list(
          plugins = list("remove_button"),
          create = TRUE,
          persist = FALSE
        )
      )
    })
    
    
    # =====================================================
    # 3. DATOS ESTANDARIZADOS
    # =====================================================
    
    datos_std <- reactive({
      
      req(input$target_var)
      
      df <- datos_base()
      
      num_cols <- sapply(df, is.numeric)
      
      # NO escalamos la Y
      num_cols[input$target_var] <- FALSE
      
      if(sum(num_cols) > 0){
        
        df[, num_cols] <- scale(df[, num_cols, drop = FALSE])
        
      }
      
      df
    })
    
    
    # =====================================================
    # 4. TABLAS
    # =====================================================
    
    output$dataset <- renderTable({
      head(datos_base(), 6)
    })
    
    output$dataset_std <- renderTable({
      head(datos_std(), 6)
    })
    
    
    # =====================================================
    # 5. MODELO
    # =====================================================
    
    modelo_tree <- eventReactive(input$run_tree, {
      
      req(input$target_var,
          input$predictor_vars)
      
      df <- datos_std()
      
      # Clasificación automática
      if(length(unique(df[[input$target_var]])) <= 5){
        
        df[[input$target_var]] <- as.factor(df[[input$target_var]])
        
      }
      
      formula_tree <- as.formula(
        paste(
          input$target_var,
          "~",
          paste(input$predictor_vars,
                collapse = " + ")
        )
      )
      
      tipo_nodo <- ifelse(
        is.factor(df[[input$target_var]]),
        "class",
        "anova"
      )
        
        rpart(
          formula = formula_tree,
          data = df,
          method = tipo_nodo,
          
          control = rpart.control(
            maxdepth = input$maxdepth,
            cp = 0.01
          )
        )
      
    })
    
    
    # =====================================================
    # 6. ÁRBOL
    # =====================================================
    
    output$tree_plot <- renderPlot({
      
      req(modelo_tree())
      
      mod <- modelo_tree()
      
      validate(
        need(
          !is.null(mod$splits),
          "El árbol generado es demasiado simple."
        )
      )
      
      extra_dinamico <- if(mod$method == "class") 104 else 1
      
      rpart.plot(
        
        mod,
        
        type = 4,
        extra = extra_dinamico,
        
        under = TRUE,
        
        box.palette = input$palette,
        
        shadow.col = "#E0E0E0",
        
        nn = TRUE,
        fallen.leaves = TRUE,
        branch.lty = 3,
        
        main = paste(
          "Árbol de Decisión:",
          input$target_var
        )
      )
      
    })
    
    
    # =====================================================
    # 7. IMPORTANCIA VARIABLES
    # =====================================================
    
    output$importance_plot <- renderPlot({
      
      req(modelo_tree())
      
      mod <- modelo_tree()
      
      imp <- mod$variable.importance
      
      validate(
        need(
          !is.null(imp),
          "No hay suficiente información."
        )
      )
      
      df_imp <- data.frame(
        
        Variable = names(imp),
        Importancia = as.numeric(imp)
        
      )
      
      ggplot(
        df_imp,
        aes(
          x = reorder(Variable, Importancia),
          y = Importancia
        )
      ) +
        
        geom_bar(
          stat = "identity",
          fill = "#2563eb",
          alpha = 0.85,
          width = 0.6
        ) +
        
        coord_flip() +
        
        theme_minimal(base_size = 14) +
        
        labs(
          title = "Importancia de Variables",
          x = NULL,
          y = "Importancia"
        )
      
    })
    
    
    # =====================================================
    # 8. REGLAS
    # =====================================================
    
    output$tree_rules <- renderPrint({
      
      req(modelo_tree())
      
      cat("--- Reglas del árbol ---\n\n")
      
      rpart.rules(modelo_tree())
      
    })
    
  })
  
}

# -------------------------------
# AUTOEVALUACION
# -------------------------------
Arboles_Auto_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Encabezado estilizado
    h3("Autoevaluación", 
       style = "color: #1a446c; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; font-weight: 600; margin-top: 40px; margin-bottom: 25px; border-bottom: 2px solid #f4f6f9; padding-bottom: 10px;"),
    
    # 1. Bloque dinámico donde se imprimen las preguntas del AF (ahora van primero)
    uiOutput(ns("preguntas")),
    
    br(),
    
    # 2. Barra de acciones y puntuación 
    card(
      class = "shadow-sm mb-4 border-0",
      style = "background-color: #fdfdfd;",
      card_body(
        class = "d-flex justify-content-between align-items-center flex-wrap gap-3 py-3",
        div(
          class = "d-flex gap-2",
          actionButton(ns("ver"), "👁️ Ver respuestas", class = "btn-primary"),
          actionButton(ns("shuffle"), "🔀 Reordenar test", class = "btn-outline-primary")
        ),
        uiOutput(ns("score"))
      )
    ),
    
    br(),
    
    # 3. Formulario colapsable para agregar preguntas (queda al final del todo)
    accordion(
      open = FALSE,
      class = "shadow-sm border-0",
      accordion_panel(
        title = "➕ Gestión: Añadir pregunta personalizada de Análisis Factorial",
        icon = icon("gear"),
        fluidRow(
          column(width = 9, textInput(ns("nueva_pregunta"), "Enunciado de la pregunta")),
          column(width = 3, selectInput(ns("correcta"), "Asignar correcta", 
                                        choices = c("Opción 1", "Opción 2", "Opción 3", "Opción 4")))
        ),
        fluidRow(
          column(width = 3, textInput(ns("op1"), "Opción 1")),
          column(width = 3, textInput(ns("op2"), "Opción 2")),
          column(width = 3, textInput(ns("op3"), "Opción 3")),
          column(width = 3, textInput(ns("op4"), "Opción 4"))
        ),
        actionButton(ns("add"), "Guardar pregunta", class = "btn-success btn-sm mt-2")
      )
    )
  )
}

Arboles_Auto_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    preguntas_base <- list(
      list(
        texto = "¿Qué representa la matriz A?",
        opciones = c("a) Matriz de datos originales", "b) Matriz de cargas factoriales", "c) Matriz de covarianzas", "d) Matriz identidad"),
        correcta = "b) Matriz de cargas factoriales"
      ),
      list(
        texto = "¿Cuál es el objetivo principal del análisis factorial?",
        opciones = c("a) Reducir dimensionalidad explicando la covarianza entre variables", "b) Clasificar observaciones", "c) Calcular medias", "d) Eliminar variables"),
        correcta = "a) Reducir dimensionalidad explicando la covarianza entre variables"
      ),
      list(
        texto = "¿Qué son los factores comunes?",
        opciones = c("a) Variables observadas", "b) Factores no observados que explican la correlación", "c) Errores aleatorios", "d) Variables dependientes"),
        correcta = "b) Factores no observados que explican la correlación"
      ),
      list(
        texto = "¿En qué se diferencian los factores comunes de los únicos?",
        opciones = c("a) Los únicos son observables", "b) Los comunes explican covarianza y los únicos son específicos/errores", "c) No hay diferencia", "d) Los comunes son aleatorios"),
        correcta = "b) Los comunes explican covarianza y los únicos son específicos/errores"
      ),
      list(
        texto = "¿Por qué se rota la matriz factorial?",
        opciones = c("a) Para aumentar la interpretabilidad", "b) Para reducir variables", "c) Para calcular medias", "d) Para eliminar factores"),
        correcta = "a) Para aumentar la interpretabilidad"
      ),
      list(
        texto = "Si los factores están incorrelados, ¿qué tipo de rotación se usa?",
        opciones = c("a) Oblícua", "b) Varimax (ortogonal)", "c) Promax", "d) Quartimax"),
        correcta = "b) Varimax (ortogonal)"
      ),
      list(
        texto = "¿Qué significa la comunalidad de una variable?",
        opciones = c("a) Varianza total de la variable", "b) Proporción de varianza explicada por los factores", "c) Media de la variable", "d) Error de medición"),
        correcta = "b) Proporción de varianza explicada por los factores"
      ),
      list(
        texto = "¿Qué diferencia hay entre AF exploratorio y confirmatorio?",
        opciones = c("a) No hay diferencia", "b) El exploratorio busca estructura y el confirmatorio la valida", "c) Uno usa PCA y otro no", "d) Uno es supervisado"),
        correcta = "b) El exploratorio busca estructura y el confirmatorio la valida"
      ),
      list(
        texto = "¿Qué indica una carga factorial alta?",
        opciones = c("a) Baja relación con el factor", "b) Alta relación entre variable y factor", "c) Error alto", "d) Independencia"),
        correcta = "b) Alta relación entre variable y factor"
      ),
      list(
        texto = "¿Qué criterio se usa para decidir el número de factores?",
        opciones = c("a) Regla de Kaiser (eigenvalores > 1)", "b) Media de variables", "c) Tamaño de la muestra", "d) Varianza mínima"),
        correcta = "a) Regla de Kaiser (eigenvalores > 1)"
      ),
      
      # 5 PREGUNTAS NUEVAS PARA COMPLETAR LAS 15
      list(
        texto = "¿Qué mide el índice KMO (Kaiser-Meyer-Olkin)?",
        opciones = c("a) La adecuación muestral para realizar un análisis factorial", "b) El número exacto de factores a extraer", "c) La distancia euclídea entre los individuos", "d) El sesgo de la rotación oblícua"),
        correcta = "a) La adecuación muestral para realizar un análisis factorial"
      ),
      list(
        texto = "¿Qué evalúa la prueba de esfericidad de Bartlett?",
        opciones = c("a) Si la matriz de correlación es significativamente diferente de una matriz identidad", "b) Si las observaciones siguen una distribución lineal perfecta", "c) La varianza compartida por factores únicos", "d) La normalidad multivariante del dataset"),
        correcta = "a) Si la matriz de correlación es significativamente diferente de una matriz identidad"
      ),
      list(
        texto = "Si dos factores se encuentran correlacionados entre sí, ¿qué tipo de rotación es la adecuada?",
        opciones = c("a) Rotación Ortogonal (como Varimax)", "b) Rotación Oblícua (como Promax u Oblimin)", "c) Ninguna, no se pueden rotar factores correlacionados", "d) Rotación por Componentes Principales"),
        correcta = "b) Rotación Oblícua (como Promax u Oblimin)"
      ),
      list(
        texto = "¿Qué compone la varianza total de una variable en el Análisis Factorial?",
        opciones = c("a) La comunalidad únicamente", "b) La suma de la comunalidad y la varianza única (especificidad + error)", "c) El número total de factores multiplicado por las medias", "d) Los autovalores mayores que uno"),
        correcta = "b) La suma de la comunalidad y la varianza única (especificidad + error)"
      ),
      list(
        texto = "En el Análisis Factorial, ¿qué se asume típicamente sobre los errores o factores únicos?",
        opciones = c("a) Que están altamente correlacionados entre sí", "b) Que no están correlacionados entre sí ni con los factores comunes", "c) Que son idénticos a las comunalidades", "d) Que explican la estructura compartida de las variables"),
        correcta = "b) Que no están correlacionados entre sí ni con los factores comunes"
      )
    )
    
    preguntas_usuario <- reactiveVal(list())
    
    observeEvent(input$add, {
      req(input$nueva_pregunta, input$op1, input$op2, input$op3, input$op4)
      nueva <- list(
        texto = input$nueva_pregunta,
        opciones = c(input$op1, input$op2, input$op3, input$op4),
        correcta = c(input$op1, input$op2, input$op3, input$op4)[match(input$correcta, c("Opción 1", "Opción 2", "Opción 3", "Opción 4"))]
      )
      preguntas_usuario(c(preguntas_usuario(), list(nueva)))
    })
    
    todas_preguntas <- reactive({
      c(preguntas_base, preguntas_usuario())
    })
    
    preguntas_ordenadas <- reactiveVal(NULL)
    
    # CORREGIDO: Añadido isolate() para evitar el bucle infinito reactivo inicial
    observe({
      lista_actual <- todas_preguntas()
      lista_enriquecida <- lapply(seq_along(lista_actual), function(idx) {
        p <- lista_actual[[idx]]
        p$id_unico <- paste0("af_q_", gsub("[^a-zA-Z0-9]", "", p$texto))
        p
      })
      
      # Generamos la muestra aleatoria de 10 preguntas de manera aislada al arrancar
      if (is.null(isolate(preguntas_ordenadas()))) {
        n_mostrar <- min(10, length(lista_enriquecida))
        muestra_inicial <- sample(lista_enriquecida, n_mostrar)
        preguntas_ordenadas(muestra_inicial)
      }
    })
    
    # Reordenación: toma 10 preguntas distintas del banco total de AF y mezcla opciones
    observeEvent(input$shuffle, {
      lista_actual <- todas_preguntas()
      lista_enriquecida <- lapply(seq_along(lista_actual), function(idx) {
        p <- lista_actual[[idx]]
        p$id_unico <- paste0("af_q_", gsub("[^a-zA-Z0-9]", "", p$texto))
        p
      })
      
      n_mostrar <- min(10, length(lista_enriquecida))
      nuevas <- sample(lista_enriquecida, n_mostrar)
      
      nuevas <- lapply(nuevas, function(p) {
        p$opciones <- sample(p$opciones)
        p
      })
      preguntas_ordenadas(nuevas)
    })
    
    # RENDER ÚNICO: Estructura de tarjetas con feedback integrado por ID inmutable
    output$preguntas <- renderUI({
      req(preguntas_ordenadas())
      
      tagList(
        lapply(seq_along(preguntas_ordenadas()), function(i) {
          pregunta_actual <- preguntas_ordenadas()[[i]]
          id_input <- pregunta_actual$id_unico
          
          feedback_ui <- NULL
          if (input$ver) {
            user_ans <- input[[id_input]]
            correct_ans <- pregunta_actual$correcta
            
            if (!is.null(user_ans) && user_ans == correct_ans) {
              feedback_ui <- div(class = "text-success mt-2 font-weight-bold", "✔️ ¡Correcto!")
            } else {
              feedback_ui <- div(class = "text-danger mt-2", paste0("❌ Incorrecto. Respuesta correcta: ", correct_ans))
            }
          }
          
          card(
            class = "mb-3 shadow-sm",
            card_header(tags$strong(paste0("Pregunta ", i))),
            card_body(
              radioButtons(
                session$ns(id_input),
                pregunta_actual$texto,
                choices = pregunta_actual$opciones,
                selected = input[[id_input]], 
                width = "100%"
              ),
              feedback_ui
            )
          )
        })
      )
    })
    
    # Caja de puntuación global dinámica basada en las 10 activas
    output$score <- renderUI({
      req(input$ver)
      total <- length(preguntas_ordenadas())
      
      aciertos <- sum(sapply(preguntas_ordenadas(), function(p) {
        res <- input[[p$id_unico]]
        !is.null(res) && res == p$correcta
      }), na.rm = TRUE)
      
      porcentaje <- (aciertos / total) * 100
      clase_color <- if(porcentaje >= 70) "alert-success" else "alert-warning"
      
      div(
        class = paste("alert mb-0 py-2 px-4", clase_color),
        tags$strong(paste0("Puntuación: ", aciertos, " / ", total, " (", round(porcentaje), "%)"))
      )
    })
  })
}
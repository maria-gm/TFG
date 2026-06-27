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
    
    #--------------------------------------------------
    # TÍTULO (igual estilo que logística)
    #--------------------------------------------------
    h3(
      "Análisis",
      style = "color: #1a446c;
               font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif;
               font-weight: 600;
               margin-top: 40px;
               margin-bottom: 20px;
               border-bottom: 2px solid #f4f6f9;
               padding-bottom: 10px;"
    ),
    
    fluidRow(
      
      #--------------------------------------------------
      # PANEL LATERAL (IZQUIERDA)
      #--------------------------------------------------
      column(
        4,
        
        wellPanel(
          h4("Configuración Árbol de Decisión"),
          p("Ajuste los parámetros del modelo CART (Classification and Regression Trees)."),
          
          hr(),
          
          uiOutput(ns("target_ui")),
          uiOutput(ns("predictors_ui")),
          
          hr(),
          
          sliderInput(
            ns("maxdepth"),
            "Profundidad máxima",
            min = 1,
            max = 30,
            value = 5
          ),
          
          sliderInput(
            ns("cp"),
            "Parámetro de complejidad (cp)",
            min = 0.0001,
            max = 0.1,
            value = 0.01,
            step = 0.001
          ),
          
          hr(),
          
          uiOutput(ns("ui_dl_tree")),
          
          helpText("El árbol se recalcula automáticamente al modificar los parámetros.")
        )
      ),
      
      #--------------------------------------------------
      # PANEL PRINCIPAL (DERECHA)
      #--------------------------------------------------
      column(
        8,
        
        tabsetPanel(
          id = ns("tabs_tree"),
          
          #-------------------------
          # 1. DATOS
          #-------------------------
          tabPanel(
            "1. Datos",
            
            br(),
            
            p(
              "Se muestran los datos originales y la versión estandarizada utilizada en el modelo.",
              style = "color: #7f8c8d; font-style: italic; margin-bottom: 20px;"
            ),
            
            h4("Dataset original", style = "color: #2c3e50; font-weight: 500;"),
            DT::DTOutput(ns("dataset")),
            
            br(), hr(), br(),
            
            h4("Dataset estandarizado", style = "color: #2c3e50; font-weight: 500;"),
            DT::DTOutput(ns("dataset_std"))
          ),
          
          #-------------------------
          # 2. ÁRBOL
          #-------------------------
          tabPanel(
            "2. Árbol",
            
            br(),
            
            h4("Estructura del árbol", style = "color: #2c3e50; font-weight: 500;"),
            plotOutput(ns("tree_plot"), height = "650px"),
            
            br(),
            
            h4("Interpretación", style = "color: #2c3e50; font-weight: 500;"),
            verbatimTextOutput(ns("interp_arbol"))
          ),
          
          #-------------------------
          # 3. MÉTRICAS
          #-------------------------
          tabPanel(
            "3. Métricas",
            
            br(),
            
            h4("Rendimiento del modelo", style = "color: #2c3e50; font-weight: 500;"),
            DT::DTOutput(ns("tabla_metricas")),
            
            br(),
            
            h4("Interpretación", style = "color: #2c3e50; font-weight: 500;"),
            verbatimTextOutput(ns("interp_metricas"))
          ),
          
          #-------------------------
          # 4. IMPORTANCIA
          #-------------------------
          tabPanel(
            "4. Importancia",
            
            br(),
            
            h4("Importancia de variables", style = "color: #2c3e50; font-weight: 500;"),
            plotOutput(ns("importance_plot"), height = "450px"),
            
            br(),
            
            verbatimTextOutput(ns("interp_importancia"))
          ),
          
          #-------------------------
          # 5. REGLAS
          #-------------------------
          tabPanel(
            "5. Reglas",
            
            br(),
            
            h4("Reglas del árbol", style = "color: #2c3e50; font-weight: 500;"),
            verbatimTextOutput(ns("tree_rules")),
            
            br(),
            
            verbatimTextOutput(ns("interp_reglas"))
          ),
          
          #-------------------------
          # 6. CP
          #-------------------------
          tabPanel(
            "6. Selección de cp",
            
            br(),
            
            h4("Complejidad del modelo", style = "color: #2c3e50; font-weight: 500;"),
            plotOutput(ns("cp_selector"), height = "450px"),
            
            br(),
            
            verbatimTextOutput(ns("interp_cp"))
          )
        )
      )
    )
  )
}

Arboles_Analisis_Server <- function(id, datos, datos_ejemplo = NULL) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    backtick <- function(x) paste0("`", x, "`")
    
    #--------------------------------------------------
    # 1. DATOS
    #--------------------------------------------------
    datos_base <- reactive({
      df <- if (!is.null(datos()) && nrow(datos()) > 0) datos() else datos_ejemplo
      req(df)
      
      df[] <- lapply(df, function(x) {
        if (is.factor(x) || is.character(x)) {
          as_n <- suppressWarnings(as.numeric(as.character(x)))
          if (sum(!is.na(as_n)) > 0.8 * length(x)) as_n else x
        } else x
      })
      
      na.omit(df)
    })
    
    #--------------------------------------------------
    # 2. UI SELECTORES
    #--------------------------------------------------
    output$target_ui <- renderUI({
      req(datos_base())
      selectInput(ns("target_var"),
                  "Variable objetivo (Y):",
                  choices = names(datos_base()))
    })
    
    output$predictors_ui <- renderUI({
      req(input$target_var)
      opts <- setdiff(names(datos_base()), input$target_var)
      
      selectizeInput(ns("predictor_vars"),
                     "Variables predictoras:",
                     choices = opts,
                     multiple = TRUE,
                     selected = opts[1:min(4, length(opts))])
    })
    
    #--------------------------------------------------
    # 3. DATASET
    #--------------------------------------------------
    datos_std <- reactive({
      req(input$target_var)
      df <- datos_base()
      
      num <- sapply(df, is.numeric)
      num[input$target_var] <- FALSE
      
      if (any(num)) df[, num] <- scale(df[, num, drop = FALSE])
      df
    })
    
    output$dataset <- DT::renderDT(datos_base())
    output$dataset_std <- DT::renderDT(datos_std())
    
    #--------------------------------------------------
    # 4. MODELO (con COMPLEJIDAD cp)
    #--------------------------------------------------
    modelo_tree <- reactive({
      
      req(input$target_var, input$predictor_vars)
      
      df <- datos_std()
      
      df[[input$target_var]] <- if (is.numeric(df[[input$target_var]]))
        df[[input$target_var]]
      else
        as.factor(df[[input$target_var]])
      
      formula <- as.formula(
        paste(
          backtick(input$target_var),
          "~",
          paste(sapply(input$predictor_vars, backtick), collapse = " + ")
        )
      )
      
      method <- if (is.factor(df[[input$target_var]])) "class" else "anova"
      
      rpart::rpart(
        formula,
        data = df,
        method = method,
        control = rpart::rpart.control(
          maxdepth = input$maxdepth,
          cp = input$cp,
          minsplit = 2,
          minbucket = 1
        )
      )
    })
    
    #--------------------------------------------------
    # 6. ÁRBOL
    #--------------------------------------------------
    output$tree_plot <- renderPlot({
      
      req(modelo_tree())
      
      rpart.plot::rpart.plot(
        modelo_tree(),
        type = 2,
        extra = 101,
        box.palette = "RdYlGn",
        shadow.col = "gray"
      )
    })
    
    output$interp_arbol <- renderText({
      paste(
        "El árbol divide el espacio de variables en regiones homogéneas.",
        "La complejidad del modelo está controlada por cp y la profundidad máxima."
      )
    })
    
    #--------------------------------------------------
    # 7. MÉTRICAS (como tu modelo logístico)
    #--------------------------------------------------
    output$tabla_metricas <- DT::renderDT({
      
      req(modelo_tree())
      
      preds <- predict(modelo_tree(), type = "class")
      
      reales <- as.factor(datos_base()[[input$target_var]])
      
      matriz <- table(Real = reales, Predicho = preds)
      
      TP <- matriz[2,2]
      TN <- matriz[1,1]
      FP <- matriz[1,2]
      FN <- matriz[2,1]
      
      accuracy <- (TP + TN) / sum(matriz)
      precision <- ifelse((TP + FP) == 0, NA, TP/(TP+FP))
      recall <- ifelse((TP + FN) == 0, NA, TP/(TP+FN))
      f1 <- ifelse(is.na(precision) | is.na(recall),
                   NA,
                   2 * precision * recall / (precision + recall))
      
      df_m <- data.frame(
        Métrica = c("Accuracy", "Precision", "Recall", "F1-score"),
        Valor = round(c(accuracy, precision, recall, f1), 4)
      )
      
      DT::datatable(df_m, options = list(paging = FALSE), rownames = FALSE)
    })
    
    output$interp_metricas <- renderText({
      paste(
        "Accuracy mide el rendimiento global.",
        "Recall es crítico en cáncer de mama (evitar falsos negativos).",
        "Precision mide fiabilidad de predicción de malignidad.",
        "F1 balancea ambos."
      )
    })
    
    #--------------------------------------------------
    # 8. SELECCIÓN DE COMPLEJIDAD (cp)
    #--------------------------------------------------
    output$cp_selector <- renderPlot({
      
      req(modelo_tree())
      plotcp(modelo_tree(),
             main = "Selección del parámetro de complejidad (cp)")
    })
    
    output$interp_cp <- renderText({
      paste0(
        "SELECCIÓN DE COMPLEJIDAD (cp):\n\n",
        "Este gráfico muestra cómo cambia el error con la complejidad del árbol.\n",
        "El objetivo es elegir el punto donde el error deja de disminuir significativamente.\n",
        "Valores bajos de cp generan árboles más complejos y mayor riesgo de sobreajuste."
      )
    })
    
    #--------------------------------------------------
    # 9. IMPORTANCIA
    #--------------------------------------------------
    output$importance_plot <- renderPlot({
      
      imp <- modelo_tree()$variable.importance
      
      if (is.null(imp)) {
        plot.new()
        text(0.5, 0.5, "Sin importancia disponible")
      } else {
        barplot(sort(imp),
                horiz = TRUE,
                col = "steelblue")
      }
    })
    
    output$interp_importancia <- renderText({
      "Las variables se ordenan según su contribución a la reducción de impureza."
    })
    
    #--------------------------------------------------
    # 10. REGLAS
    #--------------------------------------------------
    output$tree_rules <- renderPrint({
      
      mod <- modelo_tree()
      
      hojas <- as.numeric(rownames(mod$frame[mod$frame$var == "<leaf>", , drop = FALSE]))
      
      reglas <- rpart::path.rpart(mod, nodes = hojas, print.it = FALSE)
      
      for (i in seq_along(reglas)) {
        cat("Regla", i, ":\n")
        cat(paste(reglas[[i]][-1], collapse = " AND "), "\n\n")
      }
    })
    
    output$interp_reglas <- renderText({
      "Las reglas IF-THEN permiten interpretar el modelo de forma transparente."
    })
    
  })
}

# -------------------------------
# AUTOEVALUACION
# -------------------------------

Arboles_Auto_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # ─── SOLUCIÓN REFORZADA PARA EL ANCHO DE LOS RADIO BUTTONS ───
    tags$head(
      tags$style(HTML("
        /* Ataca directamente a todas las variaciones de radio buttons de Shiny */
        .shiny-input-radiogroup, 
        .shiny-input-radiogroup .shiny-options-group,
        .shiny-input-radiogroup .form-check,
        .shiny-input-radiogroup .radio {
          width: 100% !important;
          max-width: 100% !important;
          display: block !important;
        }
        
        /* Asegura que la etiqueta y el texto ocupen todo el espacio del card */
        .shiny-input-radiogroup label,
        .shiny-input-radiogroup .form-check-label {
          width: 100% !important;
          max-width: 100% !important;
          display: inline-block !important;
          white-space: normal !important; /* Permite saltos lógicos, no prematuros */
          word-break: break-word !important;
        }
        
        /* Ajuste por si el flexbox de Bootstrap 5 está encogiendo el texto */
        .form-check {
          display: flex !important;
          align-items: center !important;
          gap: 0.5rem;
        }
      "))
    ),
    
    h3("Autoevaluación", 
       style = "color: #1a446c; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; font-weight: 600; margin-top: 40px; margin-bottom: 20px; border-bottom: 2px solid #f4f6f9; padding-bottom: 10px;"),
    
    uiOutput(ns("preguntas")),
    
    br(),
    
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
    
    accordion(
      open = FALSE,
      class = "shadow-sm border-0",
      accordion_panel(
        title = "➕ Gestión: Añadir pregunta personalizada de los árboles de decisión",
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
        
        actionButton(ns("add"), "Guardar pregunta en el banco de los árboles de decisión", class = "btn-success btn-sm mt-2")
      )
    )
  )
}

Arboles_Auto_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    mostrar_respuestas <- reactiveVal(FALSE)
    
    observeEvent(input$ver, {
      mostrar_respuestas(!mostrar_respuestas())
      
      updateActionButton(
        session,
        "ver",
        label = if (mostrar_respuestas()) "🙈 Ocultar respuestas" else "👁️ Ver respuestas"
      )
    })
    
    preguntas_base <- list(
      list(
        texto = "¿Qué son los árboles de decisión?",
        opciones = c(
          "Modelos predictivos que generan reglas jerárquicas para clasificar o predecir",
          "Técnicas de reducción de dimensionalidad",
          "Métodos para calcular correlaciones",
          "Modelos basados exclusivamente en regresión lineal"
        ),
        correcta = "Modelos predictivos que generan reglas jerárquicas para clasificar o predecir"
      ),
      
      list(
        texto = "¿Cuándo se utilizan los árboles de decisión?",
        opciones = c(
          "Cuando se desea clasificar individuos o realizar predicciones",
          "Únicamente para calcular medias",
          "Solo para reducir variables",
          "Exclusivamente para análisis factorial"
        ),
        correcta = "Cuando se desea clasificar individuos o realizar predicciones"
      ),
      
      list(
        texto = "¿Qué utilizan los árboles de decisión para clasificar a los individuos?",
        opciones = c(
          "Reglas de decisión basadas en los atributos predictivos",
          "Únicamente la variable dependiente",
          "Correlaciones entre observaciones",
          "Promedios de cada grupo"
        ),
        correcta = "Reglas de decisión basadas en los atributos predictivos"
      ),
      
      list(
        texto = "¿Con qué tipo de variables pueden trabajar los árboles de decisión?",
        opciones = c(
          "Variables categóricas y continuas",
          "Solo variables categóricas",
          "Solo variables numéricas",
          "Únicamente variables binarias"
        ),
        correcta = "Variables categóricas y continuas"
      ),
      
      list(
        texto = "¿Qué puede indicar que las métricas de ajuste del modelo sean extremadamente altas?",
        opciones = c(
          "Posible sobreajuste del modelo",
          "Falta de información en los datos",
          "Error en la variable objetivo",
          "Ausencia de variables predictoras"
        ),
        correcta = "Posible sobreajuste del modelo"
      ),
      
      list(
        texto = "En el dataset Breast Cancer utilizado en la aplicación, ¿qué variable suele aparecer como una de las más representativas para clasificar tumores?",
        opciones = c(
          "Radius_mean",
          "ID",
          "Diagnóstico",
          "Número de observación"
        ),
        correcta = "Radius_mean"
      ),
      
      list(
        texto = "¿Qué representa el accuracy de un árbol de decisión?",
        opciones = c(
          "La proporción de observaciones clasificadas correctamente",
          "El número total de variables del modelo",
          "La profundidad máxima del árbol",
          "La cantidad de nodos terminales"
        ),
        correcta = "La proporción de observaciones clasificadas correctamente"
      ),
      
      list(
        texto = "¿Qué es el sobreajuste (overfitting)?",
        opciones = c(
          "Cuando el modelo aprende el ruido de los datos y pierde capacidad de generalización",
          "Cuando el árbol tiene pocos nodos",
          "Cuando se eliminan variables predictoras",
          "Cuando el accuracy es bajo"
        ),
        correcta = "Cuando el modelo aprende el ruido de los datos y pierde capacidad de generalización"
      ),
      
      list(
        texto = "¿Qué mide la importancia de variables en un árbol de decisión?",
        opciones = c(
          "La contribución de cada predictor a la reducción de impureza",
          "La media de cada variable",
          "La correlación entre observaciones",
          "La cantidad de datos faltantes"
        ),
        correcta = "La contribución de cada predictor a la reducción de impureza"
      ),
      
      list(
        texto = "¿Qué representa una hoja en un árbol de decisión?",
        opciones = c(
          "La clasificación o predicción final",
          "La variable más importante",
          "La raíz del árbol",
          "Un dato perdido"
        ),
        correcta = "La clasificación o predicción final"
      ),
      
      list(
        texto = "¿Qué criterio utiliza habitualmente el algoritmo CART para seleccionar divisiones?",
        opciones = c(
          "Índice de Gini",
          "Coeficiente de correlación",
          "Media aritmética",
          "Distancia euclídea"
        ),
        correcta = "Índice de Gini"
      ),
      
      list(
        texto = "¿Qué mide la entropía en un árbol de decisión?",
        opciones = c(
          "El grado de incertidumbre o desorden de un nodo",
          "La profundidad del árbol",
          "El número de observaciones",
          "La precisión global del modelo"
        ),
        correcta = "El grado de incertidumbre o desorden de un nodo"
      ),
      
      list(
        texto = "Según la interpretación mostrada en la aplicación, ¿qué significa que una variable aparezca en primer lugar en el gráfico de importancia?",
        opciones = c(
          "Que es la que más contribuye a las divisiones relevantes del árbol",
          "Que tiene más valores perdidos",
          "Que fue introducida primero en la base de datos",
          "Que es la variable objetivo"
        ),
        correcta = "Que es la que más contribuye a las divisiones relevantes del árbol"
      ),
      
      list(
        texto = "¿Qué representan las reglas de decisión mostradas en la pestaña de reglas?",
        opciones = c(
          "Condiciones lógicas IF-THEN desde la raíz hasta una hoja",
          "Correlaciones entre variables",
          "Promedios de cada grupo",
          "Valores faltantes detectados"
        ),
        correcta = "Condiciones lógicas IF-THEN desde la raíz hasta una hoja"
      ),
      
      list(
        texto = "¿Cuál es una de las principales ventajas de los árboles de decisión frente a modelos de caja negra?",
        opciones = c(
          "Permiten interpretar fácilmente el proceso de decisión",
          "Siempre tienen mayor precisión",
          "No requieren datos",
          "Eliminan automáticamente el ruido"
        ),
        correcta = "Permiten interpretar fácilmente el proceso de decisión"
      )
    )
    
    preguntas_usuario <- reactiveVal(list())
    
    observeEvent(input$add, {
      req(input$nueva_pregunta, input$op1, input$op2, input$op3, input$op4)
      
      nueva <- list(
        texto = input$nueva_pregunta,
        opciones = c(input$op1, input$op2, input$op3, input$op4),
        correcta = c(input$op1, input$op2, input$op3, input$op4)[
          match(input$correcta, c("Opción 1", "Opción 2", "Opción 3", "Opción 4"))
        ]
      )
      
      preguntas_usuario(c(preguntas_usuario(), list(nueva)))
    })
    
    todas_preguntas <- reactive({
      c(preguntas_base, preguntas_usuario())
    })
    
    preguntas_ordenadas <- reactiveVal(NULL)
    
    observe({
      lista_enriquecida <- lapply(todas_preguntas(), function(p) {
        p$id_unico <- paste0("af_q_", gsub("[^a-zA-Z0-9]", "", p$texto))
        p
      })
      
      if (is.null(isolate(preguntas_ordenadas()))) {
        preguntas_ordenadas(sample(lista_enriquecida, min(10, length(lista_enriquecida))))
      }
    })
    
    observeEvent(input$shuffle, {
      lista_enriquecida <- lapply(todas_preguntas(), function(p) {
        p$id_unico <- paste0("af_q_", gsub("[^a-zA-Z0-9]", "", p$texto))
        p
      })
      
      nuevas <- sample(lista_enriquecida, min(10, length(lista_enriquecida)))
      
      nuevas <- lapply(nuevas, function(p) {
        p$opciones <- sample(p$opciones)
        p
      })
      
      preguntas_ordenadas(nuevas)
    })
    
    output$preguntas <- renderUI({
      req(preguntas_ordenadas())
      
      tagList(
        lapply(seq_along(preguntas_ordenadas()), function(i) {
          
          pregunta <- preguntas_ordenadas()[[i]]
          id_input <- pregunta$id_unico
          
          feedback_ui <- NULL
          
          if (isTRUE(mostrar_respuestas())) {
            
            user_ans <- input[[id_input]]
            correct <- pregunta$correcta
            
            if (!is.null(user_ans) && user_ans == correct) {
              feedback_ui <- div(
                class = "text-success mt-2 font-weight-bold",
                "✔️ ¡Correcto!"
              )
            } else {
              feedback_ui <- div(
                class = "text-danger mt-2",
                paste0("❌ Incorrecto. Respuesta correcta: ", correct)
              )
            }
          }
          
          card(
            class = "mb-3 shadow-sm",
            card_header(tags$strong(paste0("Pregunta ", i))),
            card_body(
              radioButtons(
                session$ns(id_input),
                pregunta$texto,
                choices = pregunta$opciones,
                selected = input[[id_input]]
              ),
              feedback_ui
            )
          )
        })
      )
    })
    
    output$score <- renderUI({
      req(mostrar_respuestas())
      
      total <- length(preguntas_ordenadas())
      
      aciertos <- sum(sapply(preguntas_ordenadas(), function(p) {
        res <- input[[p$id_unico]]
        !is.null(res) && res == p$correcta
      }))
      
      porcentaje <- aciertos / total * 100
      
      div(
        class = if (porcentaje >= 70) "alert alert-success" else "alert alert-warning",
        strong(paste0("Puntuación: ", aciertos, " / ", total, " (", round(porcentaje), "%)"))
      )
    })
  })
}
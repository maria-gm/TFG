# =====================================================
#  Árboles de decisión
# =====================================================

# -------------------------------
# TEORIA
# -------------------------------
# =========================================================
# ÁRBOLES DE CLASIFICACIÓN - EDICIÓN MEMORIA PREMIUM
# =========================================================
Arboles_Teoria_UI <- function(id) {
  ns <- NS(id)
  
  # Estilos CSS personalizados para homogeneizar con el diseño premium
  custom_css <- "
    .theory-card {
      border: 1px solid #e2e8f0;
      border-radius: 12px;
      box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.05);
    }
    .equation-container {
      background-color: #f8fafc;
      padding: 16px;
      border-radius: 10px;
      margin: 18px 0;
      border: 1px solid #e2e8f0;
      text-align: center;
    }
    .metric-definition {
      border-left: 4px solid #10b981;
      padding-left: 16px;
      margin-bottom: 20px;
    }
  "
  
  tagList(
    withMathJax(),
    tags$head(tags$style(HTML(custom_css))),
    
    tags$div(
      style = "padding: 30px; background-color: #fcfdfe;",
      
      # =====================================
      # CABECERA ESTILO PREMIUM
      # =====================================
      tags$div(
        style = "margin-bottom: 25px;",
        h1(
          "Árboles de Clasificación",
          style = "font-weight: 800; color: #1e3a8a; margin-bottom: 6px; font-size: 2.5rem;"
        ),
        p(
          "Modelos predictivos basados en el aprendizaje supervisado que descubren de forma recursiva reglas de asociación estructuradas en flujos lógicos.",
          style = "color: #64748b; font-size: 1.15rem; margin-bottom: 25px;"
        )
      ),
      
      # =====================================
      # BLOQUES INTRODUCTORIOS (ESTILO image_3552b6)
      # =====================================
      bslib::layout_column_wrap(
        width = 1/3,
        heights_equal = "row",
        style = "margin-bottom: 35px;",
        
        # Bloque 1: Estructura y Flujo
        bslib::card(
          style = "border: 1px solid #cbd5e1; border-radius: 8px; overflow: hidden; background: #ffffff; box-shadow: 0 2px 4px rgba(0,0,0,0.02);",
          bslib::card_header(
            "1. Estructura y Flujo",
            style = "background-color: #e0e7ff; color: #1e1b4b; font-weight: 700; font-size: 1.1rem; padding: 12px 16px;"
          ),
          bslib::card_body(
            style = "padding: 20px; line-height: 1.6; color: #334155;",
            p("Técnica que segmenta jerárquicamente la muestra traduciendo las etiquetas conocidas de \\(Y\\) según sus atributos predictivos (Breiman et al., 2017). Se compone de:"),
            tags$ul(
              style = "padding-left: 15px; margin-top: 10px; list-style-type: none;",
              tags$li(style = "margin-bottom: 8px;", HTML("• <b>Nodos y Ramas:</b> El nodo raíz (superior) e intermedios denotan pruebas lógicas sobre los atributos; las ramas bifurcan sus resultados.")),
              tags$li(style = "margin-bottom: 0;", HTML("• <b>Hojas:</b> Nodos terminales que contienen la clase o categoría final asignada al individuo."))
            )
          )
        ),
        
        # Bloque 2: Algoritmos Fundamentales
        bslib::card(
          style = "border: 1px solid #cbd5e1; border-radius: 8px; overflow: hidden; background: #ffffff; box-shadow: 0 2px 4px rgba(0,0,0,0.02);",
          bslib::card_header(
            "2. Algoritmos Fundamentales",
            style = "background-color: #dcfce7; color: #064e3b; font-weight: 700; font-size: 1.1rem; padding: 12px 16px;"
          ),
          bslib::card_body(
            style = "padding: 20px; line-height: 1.6; color: #334155;",
            p("Desarrollados de manera independiente entre finales de los 70 y principios de los 80, dieron pie al grueso de la inducción moderna (Han et al., 2011):"),
            tags$ul(
              style = "padding-left: 15px; margin-top: 10px; list-style-type: none;",
              tags$li(style = "margin-bottom: 8px;", HTML("• <b>Línea ID3 / C4.5:</b> Diseñados por J. Ross Quinlan, basados en el particionamiento multi-rama a través de la ganancia de información.")),
              tags$li(style = "margin-bottom: 0;", HTML("• <b>Algoritmo CART:</b> Propuesto por Breiman, enfocado explícitamente en la generación iterativa de árboles de decisión binarios."))
            )
          )
        ),
        
        # Bloque 3: Ventajas y Rendimiento
        bslib::card(
          style = "border: 1px solid #cbd5e1; border-radius: 8px; overflow: hidden; background: #ffffff; box-shadow: 0 2px 4px rgba(0,0,0,0.02);",
          bslib::card_header(
            "3. Ventajas y Rendimiento",
            style = "background-color: #fef9c3; color: #713f12; font-weight: 700; font-size: 1.1rem; padding: 12px 16px;"
          ),
          bslib::card_body(
            style = "padding: 20px; line-height: 1.6; color: #334155;",
            p("Esta metodología goza de una amplia adopción en minería de datos debido a sus propiedades intrínsecas (Han et al., 2011):"),
            tags$ul(
              style = "padding-left: 15px; margin-top: 10px; list-style-type: none;",
              tags$li(style = "margin-bottom: 6px;", HTML("• <b>Cero caja negra:</b> Proporciona gráficos e interpretaciones intuitivas, idóneas para hallar patrones ocultos.")),
              tags$li(style = "margin-bottom: 6px;", HTML("• <b>Sin configuración:</b> No exige calibrar parámetros previos estrictos para entrenar sus reglas iniciales.")),
              tags$li(style = "margin-bottom: 0;", HTML("• <b>Robustez:</b> Destaca por su fiabilidad y precisión en entornos multivariantes."))
            )
          )
        )
      ),
      
      # =====================================
      # CRITERIOS DE PARTICIÓN
      # =====================================
      h4(icon("chart-pie"), "Criterios de Partición y Medidas de Impureza", style = "color: #1e40af; margin-bottom: 12px; font-weight: 700;"),
      bslib::card(
        class = "theory-card", style = "margin-bottom: 30px;",
        bslib::card_body(
          p("Para realizar las divisiones lógicas, el algoritmo busca aislar las clases de forma que cada subnodo sea lo más homogéneo ('puro') posible, apoyándose en dos indicadores analíticos principales:"),
          
          bslib::layout_column_wrap(
            width = 1/2,
            style = "gap: 20px; margin-top: 15px;",
            
            tags$div(
              style = "border: 1px solid #e2e8f0; padding: 15px; border-radius: 8px; background: white;",
              tags$h5(tags$b("Entropía y Ganancia de Información"), style = "color: #1e3a8a; margin-bottom: 10px;"),
              p("La entropía mide el grado de incertidumbre o desorden del subconjunto de datos \\(\\mathbf{S}\\):"),
              tags$div(class = "equation-container", HTML("$$H(\\mathbf{S}) = -\\sum_{i=1}^{c} p_i \\log_2 p_i$$")),
              p(HTML("Donde \\(c\\) es el total de clases e \\(p_i\\) es la proporción del grupo \\(i\\). A partir de ella, se evalúa un atributo \\(A\\) buscando el que <b>maximice la ganancia</b> de reducción de desorden:")),
              tags$div(class = "equation-container", HTML("$$\\text{Gain}(\\mathbf{S}, A) = H(\\mathbf{S}) - \\sum_{v \\in \\text{Valores}(A)} \\frac{|\\mathbf{S}_v|}{|\\mathbf{S}|} H(\\mathbf{S}_v)$$"))
            ),
            
            tags$div(
              style = "border: 1px solid #e2e8f0; padding: 15px; border-radius: 8px; background: white;",
              tags$h5(tags$b("Índice de Gini"), style = "color: #1e3a8a; margin-bottom: 10px;"),
              p("Utilizado de forma predeterminada por el algoritmo CART, mide directamente la impureza o grado de mezcla de clases del nodo:"),
              tags$div(class = "equation-container", HTML("$$\\text{Gini}(\\mathbf{S}) = 1 - \\sum_{i=1}^{c} p_i^2$$")),
              p(HTML("Este índice arroja valores mínimos (cercanos a 0) si la inmensa mayoría de las observaciones pertenecen a una única categoría. Aquí, el algoritmo selecciona el atributo que <b>minimice el índice de Gini</b>, forzando particiones lo más puras posibles."))
            )
          )
        )
      ),
      
      # =====================================
      # DINÁMICA DE CONSTRUCCIÓN
      # =====================================
      h4(icon("gears"), "Construcción Recursiva y Condiciones de Parada", style = "color: #1e40af; margin-bottom: 12px; font-weight: 700;"),
      bslib::card(
        class = "theory-card", style = "margin-bottom: 30px;",
        bslib::card_body(
          p("El proceso sigue un enfoque recursivo descendente que arranca desde el nodo raíz con la totalidad de la muestra y divide los datos de forma iterativa según la tipología de las variables:"),
          tags$ul(
            style = "padding-left: 20px;",
            tags$li(style = "margin-bottom: 6px;", HTML("<b>Variables categóricas:</b> Se abren simultáneamente tantas ramas físicas como valores distintos posea el atributo evaluado.")),
            tags$li(style = "margin-bottom: 15px;", HTML("<b>Variables continuas:</b> Se calcula un punto de corte óptimo que segmenta los datos en dos subgrupos, originando particiones binarias."))
          ),
          h5("Condiciones de Parada Estructurales:", style = "color: #1e3a8a; font-weight: bold; margin-bottom: 10px;"),
          tags$div(
            style = "display: flex; flex-direction: column; gap: 10px; margin-left: 10px;",
            tags$div(style = "border-left: 4px solid #10b981; padding-left: 12px;", "El subnodo alcanza un estado completamente puro (todos sus individuos pertenecen ya a la misma clase de \\(Y\\))."),
            tags$div(style = "border-left: 4px solid #3b82f6; padding-left: 12px;", "Se agotan los atributos predictivos disponibles en la muestra para continuar segmentando."),
            tags$div(style = "border-left: 4px solid #f59e0b; padding-left: 12px;", "Se alcanzan los límites prefijados por el investigador, tales como la profundidad máxima permitida o el número mínimo de observaciones por nodo.")
          )
        )
      ),
      
      # =====================================
      # PODA (PRUNING) - TEXTO EXACTO DE MEMORIA
      # =====================================
      h4(icon("scissors"), "Técnicas de Poda (Pruning) contra el Sobreajuste", style = "color: #1e40af; margin-bottom: 12px; font-weight: 700;"),
      bslib::card(
        class = "theory-card", style = "margin-bottom: 30px;",
        bslib::card_body(
          p("Permitir que el árbol se ramifique sin restricciones suele inducir al sobreajuste (overfitting): el modelo memoriza el ruido de entrenamiento, mermando su capacidad de generalizar ante nuevos individuos (Han et al., 2011). Para mitigar este problema se aplican dos aproximaciones:"),
          tags$ul(
            style = "padding-left: 20px;",
            tags$li(style = "margin-bottom: 6px;", HTML("<b>Poda previa (Pre-pruning):</b> Detiene el algoritmo de forma temprana antes de que la estructura gane una complejidad excesiva.")),
            tags$li(style = "margin-bottom: 15px;", HTML("<b>Poda posterior (Post-pruning):</b> Colapsa ramas y simplifica nodos una vez que el árbol ha alcanzado su máximo crecimiento."))
          ),
          
          hr(style = "border-top: 1px solid #cbd5e1; margin: 20px 0;"),
          
          # Sección literal de obtención de complejidad (Fiel a image_3561de)
          p(HTML("El parámetro \\(\\alpha\\) no se fija de forma arbitraria, sino que se estima mediante procedimientos de validación. De manera que, para cada valor de \\(\\alpha\\), el algoritmo selecciona un subárbol \\(T_\\alpha\\). De entre todos los posibles subárboles, el óptimo es el que minimiza la función de coste-complejidad dentro del árbol máximo inicial \\(T_{\\text{max}}\\):")),
          tags$div(class = "equation-container", HTML("$$T_\\alpha = \\arg \\min_{(T \\subseteq T_{\\text{max}})} [R(T) + \\alpha|T|]$$")),
          p(HTML("En la práctica, se genera una secuencia de valores de \\(\\alpha\\) asociados a distintos subárboles obtenidos mediante poda por complejidad-coste y, posteriormente, se evalúa el rendimiento de cada uno de ellos sobre un conjunto de validación o mediante validación cruzada. El valor óptimo de \\(\\alpha\\) es aquel que minimiza el error de generalización (por ejemplo, tasa de error media en validación cruzada), logrando así el mejor equilibrio entre ajuste del modelo y simplicidad. Este procedimiento permite no solo evitar el sobreajuste sino garantizar una estructura con una capacidad óptima de generalización a nuevos datos."))
        )
      ),
      
      # =====================================
      # APARTADO 3.5.4: MÉTRICAS (Fiel a image_3546d8)
      # =====================================
      h4(icon("chart-simple"), "3.5.4 Métricas de evaluación", style = "color: #1e40af; margin-bottom: 12px; font-weight: 700;"),
      bslib::card(
        class = "theory-card",
        bslib::card_body(
          p("Una vez que se ha ajustado un modelo de clasificación, es necesario evaluar su capacidad predictiva para averiguar si generaliza correctamente nuevos individuos. (James et al., 2013)"),
          p("Esta evaluación se realiza mediante diferentes métricas que miden el rendimiento de los diferentes modelos y que se calculan a partir de las predicciones obtenidas sobre el conjunto de datos de evaluación."),
          p(HTML("Una de las métricas más utilizadas es la exactitud o <i>accuracy</i>, que mide la proporción de observaciones correctamente clasificadas respecto al total de observaciones evaluadas. Se define como:")),
          tags$div(class = "equation-container", HTML("$$\\text{Accuracy} = \\frac{\\text{TP} + \\text{TN}}{\\text{TP} + \\text{TN} + \\text{FP} + \\text{FN}}$$")),
          p(HTML("donde TP (<i>True Positives</i>) representa los verdaderos positivos, TN (<i>True Negatives</i>) los verdaderos negativos, FP (<i>False Positives</i>) los falsos positivos y FN (<i>False Negatives</i>) los falsos negativos.")),
          p(HTML("Un valor de <i>accuracy</i> próximo a 1 indica un elevado porcentaje de clasificaciones correctas, mientras que valores más bajos reflejan un peor desempeño del modelo.")),
          p(HTML("No obstante, esta métrica puede resultar engañosa cuando las clases están desbalanceadas, ya que un modelo podría obtener una alta exactitud simplemente prediciendo siempre la clase mayoritaria. Por este motivo, suele complementarse con otras métricas como la precisión (<i>precision</i>), la sensibilidad (<i>recall</i>) y la medida F1.")),
          
          tags$div(
            style = "margin-top: 20px; display: flex; flex-direction: column; gap: 5px;",
            tags$div(
              class = "metric-definition",
              p(HTML("La precisión se define como la proporción de predicciones positivas correctas entre todas las predicciones positivas realizadas:")),
              tags$div(class = "equation-container", HTML("$$\\text{Precision} = \\frac{\\text{TP}}{\\text{TP} + \\text{FP}}$$"))
            ),
            tags$div(
              class = "metric-definition",
              p(HTML("La sensibilidad o <i>recall</i> mide la capacidad del modelo para identificar correctamente los casos positivos:")),
              tags$div(class = "equation-container", HTML("$$\\text{Recall} = \\frac{\\text{TP}}{\\text{TP} + \\text{FN}}$$"))
            ),
            tags$div(
              class = "metric-definition",
              p(HTML("Por último, la medida F1 combines ambas métricas en una única medida, calculada como la media armónica entre precisión y sensibilidad:")),
              tags$div(class = "equation-container", HTML("$$\\text{F1} = 2 \\cdot \\frac{\\text{Precision} \\cdot \\text{Recall}}{\\text{Precision} + \\text{Recall}}$$"))
            )
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
    h3("Análisis", 
       style = "color: #1a446c; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; font-weight: 600; margin-top: 40px; margin-bottom: 20px; border-bottom: 2px solid #f4f6f9; padding-bottom: 10px;"),
    
    fluidRow(
      # PANEL LATERAL (IZQUIERDA)
      column(4,
             wellPanel(
               h4("Configuración Árbol de Decisión"),
               p("Ajuste los parámetros del modelo CART (Classification and Regression Trees)."),
               hr(),
               uiOutput(ns("target_ui")),
               uiOutput(ns("predictors_ui")),
               hr(),
               sliderInput(ns("maxdepth"), "Profundidad máxima:", min = 1, max = 10, value = 5),
               sliderInput(ns("cp"), "Parámetro de complejidad (cp):", min = 0.0001, max = 0.1, value = 0.01, step = 0.001),
               hr(),
               helpText("Nota: Se eliminan filas con valores faltantes automáticamente.")
             )
      ),
      
      # PANEL PRINCIPAL (DERECHA)
      column(8,
             uiOutput(ns("mensaje_error_ui")),
             
             tabsetPanel(
               id = ns("tabs_tree"),
               
               # PESTAÑA 1: DATOS
               tabPanel("1. Datos", 
                        br(),
                        p("Se muestran los datos originales y la versión estandarizada utilizada en el modelo.", 
                          style = "color: #7f8c8d; font-style: italic; margin-bottom: 20px;"),
                        h4("Dataset original", style = "color: #2c3e50; font-weight: 500;"), 
                        DT::DTOutput(ns("dataset")),
                        br(), hr(), br(),
                        h4("Dataset estandarizado (Z-scores)", style = "color: #2c3e50; font-weight: 500;"), 
                        DT::DTOutput(ns("dataset_std"))
               ),
               # PESTAÑA 2: SELECCIÓN CP
               tabPanel("2. Selección de cp", 
                        br(),
                        h4("Complejidad del modelo", style = "color: #2c3e50; font-weight: 500;"), 
                        plotOutput(ns("cp_selector"), height = "450px"),
                        br(),
                        verbatimTextOutput(ns("interp_cp"))
               ),
               
               # PESTAÑA 3: ÁRBOL
               tabPanel("3. Árbol", 
                        br(),
                        h4("Estructura del árbol", style = "color: #2c3e50; font-weight: 500;"), 
                        plotOutput(ns("tree_plot"), height = "650px"),
                        br(),
                        h4("Interpretación", style = "color: #2c3e50; font-weight: 500;"), 
                        verbatimTextOutput(ns("interp_arbol"))
               ),
               
               # PESTAÑA 4: MÉTRICAS
               tabPanel("4. Métricas", 
                        br(),
                        h4("Rendimiento del modelo", style = "color: #2c3e50; font-weight: 500;"), 
                        DT::DTOutput(ns("tabla_metricas")),
                        br(),
                        h4("Interpretación", style = "color: #2c3e50; font-weight: 500;"), 
                        verbatimTextOutput(ns("interp_metricas"))
               ),
               
               # PESTAÑA 5: IMPORTANCIA
               tabPanel("5. Importancia", 
                        br(),
                        h4("Importancia de variables", style = "color: #2c3e50; font-weight: 500;"), 
                        plotOutput(ns("importance_plot"), height = "450px"),
                        br(),
                        verbatimTextOutput(ns("interp_importancia"))
               ),
               
               # PESTAÑA 6: REGLAS
               tabPanel("6. Reglas", 
                        br(),
                        h4("Reglas del árbol", style = "color: #2c3e50; font-weight: 500;"), 
                        verbatimTextOutput(ns("tree_rules")),
                        br(),
                        verbatimTextOutput(ns("interp_reglas"))
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
    
    # --- 1. PROCESAMIENTO GLOBAL Y VALIDACIONES ---
    datos_preprocesados <- reactive({
      df <- if (!is.null(datos()) && nrow(datos()) > 0) datos() else datos_ejemplo
      
      crear_banner_error <- function(mensaje) {
        div(
          style = "background-color: #fdf2f2; color: #9b1c1c; border: 1px solid #fde8e8; padding: 20px; margin-bottom: 25px; border-radius: 6px;",
          tags$span(style = "font-weight: bold; font-size: 16px;", tags$i(class = "fa fa-exclamation-triangle"), " No es posible realizar el análisis."),
          br(),
          tags$span(style = "font-style: italic; color: #4b5563; font-size: 14px;", "Información: El modelo CART requiere un conjunto mínimo de observaciones estructuradas."),
          br(), br(),
          tags$span(style = "font-weight: 500;", mensaje)
        )
      }
      
      if (is.null(df)) {
        return(list(valido = FALSE, ui_error = crear_banner_error("No se han detectado datos en el sistema.")))
      }
      
      df[] <- lapply(df, function(x) {
        if (is.factor(x) || is.character(x)) {
          as_n <- suppressWarnings(as.numeric(as.character(x)))
          if (sum(!is.na(as_n)) > 0.8 * length(x)) as_n else x
        } else x
      })
      
      df_limpio <- na.omit(df)
      if (nrow(df_limpio) < 15) {
        return(list(valido = FALSE, ui_error = crear_banner_error("Se requieren al menos 15 observaciones completas (sin valores omitidos) para entrenar el algoritmo.")))
      }
      
      if (ncol(df_limpio) < 2) {
        return(list(valido = FALSE, ui_error = crear_banner_error("El dataset debe contar con al menos una variable dependiente y una predictora.")))
      }
      
      return(list(valido = TRUE, base = df_limpio))
    })
    
    output$mensaje_error_ui <- renderUI({
      prep <- datos_preprocesados()
      if (!prep$valido) return(prep$ui_error)
      return(NULL)
    })
    
    datos_base <- reactive({ req(datos_preprocesados()$valido); datos_preprocesados()$base })
    
    datos_std <- reactive({
      req(input$target_var)
      df <- datos_base()
      num <- sapply(df, is.numeric)
      num[input$target_var] <- FALSE
      if (any(num)) df[, num] <- scale(df[, num, drop = FALSE])
      df
    })
    
    #--------------------------------------------------
    # 2. UI SELECTORES
    #--------------------------------------------------
    output$target_ui <- renderUI({
      req(datos_base())
      df <- datos_base()
      choices_vars <- names(df)
      
      # Forzar la variable predeterminada a "Class" si existe en el dataset
      seleccionada <- if("Class" %in% choices_vars) "Class" else choices_vars[1]
      
      selectInput(ns("target_var"),
                  "Variable objetivo (Y):",
                  choices = choices_vars,
                  selected = seleccionada)
    })
    
    output$predictors_ui <- renderUI({
      req(input$target_var)
      opts <- setdiff(names(datos_base()), c(input$target_var, "Id"))
      
      # Selección por defecto: Las dos primeras variables predictoras disponibles
      seleccion_defecto <- opts[1:min(2, length(opts))]
      
      selectizeInput(ns("predictor_vars"),
                     "Variables predictoras:",
                     choices = opts,
                     multiple = TRUE,
                     selected = seleccion_defecto,
                     options = list(plugins = list('remove_button'), persist = FALSE))
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
        "Recall es crítico en cáncer de mama (para evitar falsos negativos).",
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
      "Las variables se ordenan según su importancia para predecir la clase de la variable dependiente."
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
          "Técnicas de reducción de dimensionalidad",
          "Métodos para calcular correlaciones",
          "Modelos predictivos que generan reglas jerárquicas para clasificar o predecir",
          "Modelos basados exclusivamente en regresión lineal"
        ),
        correcta = "Modelos predictivos que generan reglas jerárquicas para clasificar o predecir"
      ),
      list(
        texto = "¿Cuándo se utilizan los árboles de decisión?",
        opciones = c(
          "Únicamente para calcular medias",
          "Cuando se desea clasificar individuos o realizar predicciones",
          "Solo para reducir variables",
          "Exclusivamente para análisis factorial"
        ),
        correcta = "Cuando se desea clasificar individuos o realizar predicciones"
      ),
      list(
        texto = "¿Qué utilizan los árboles de decisión para clasificar a los individuos?",
        opciones = c(
          "Únicamente la variable dependiente",
          "Correlaciones entre observaciones",
          "Promedios de cada grupo",
          "Reglas de decisión basadas en los atributos predictivos"
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
          "Falta de información en los datos",
          "Posible sobreajuste del modelo",
          "Error en la variable objetivo",
          "Ausencia de variables predictoras"
        ),
        correcta = "Posible sobreajuste del modelo"
      ),
      list(
        texto = "En el dataset Breast Cancer utilizado en la aplicación, ¿qué variable suele aparecer como una de las más representativas para clasificar tumores?",
        opciones = c(
          "ID",
          "Diagnóstico",
          "Radius_mean",
          "Número de observación"
        ),
        correcta = "Radius_mean"
      ),
      list(
        texto = "¿Qué representa el accuracy de un árbol de decisión?",
        opciones = c(
          "El número total de variables del modelo",
          "La proporción de observaciones clasificadas correctamente",
          "La profundidad máxima del árbol",
          "La cantidad de nodos terminales"
        ),
        correcta = "La proporción de observaciones clasificadas correctamente"
      ),
      list(
        texto = "¿Qué es el sobreajuste (overfitting)?",
        opciones = c(
          "Cuando el árbol tiene pocos nodos",
          "Cuando se eliminan variables predictoras",
          "Cuando el modelado aprende el ruido de los datos y pierde capacidad de generalización",
          "Cuando el accuracy es bajo"
        ),
        correcta = "Cuando el modelo aprende el ruido de los datos y pierde capacidad de generalización"
      ),
      list(
        texto = "¿Qué mide la importancia de variables en un árbol de decisión?",
        opciones = c(
          "La media de cada variable",
          "La contribución de cada predictor a la reducción de impureza",
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
          "Coeficiente de correlación",
          "Media aritmética",
          "Índice de Gini",
          "Distancia euclídea"
        ),
        correcta = "Índice de Gini"
      ),
      list(
        texto = "¿Qué mide la entropía en un árbol de decisión?",
        opciones = c(
          "La profundidad del árbol",
          "El número de observaciones",
          "La precisión global del modelo",
          "El grado de incertidumbre o desorden de un nodo"
        ),
        correcta = "El grado de incertidumbre o desorden de un nodo"
      ),
      list(
        texto = "Según la interpretación mostrada en la aplicación, ¿qué significa que una variable aparezca en primer lugar en el gráfico de importancia?",
        opciones = c(
          "Que tiene más valores perdidos",
          "Que fue introducida primero en la base de datos",
          "Que es la variable objetivo",
          "Que es la que más contribuye a las divisiones relevantes del árbol"
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
          "Siempre tienen mayor precisión",
          "No requieren datos",
          "Permiten interpretar fácilmente el proceso de decisión",
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
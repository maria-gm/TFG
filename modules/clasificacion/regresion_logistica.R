# ==========================
# REGRESIÓN LOGÍSTICA 
# ==========================

Regresion_logistica_Teoria_UI <- function(id) {
  ns <- NS(id)  
  
  # Estilos CSS  
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
    .confusion-matrix-table {
      border-collapse: separate;
      border-spacing: 0;
      width: 100%;
      border-radius: 8px;
      overflow: hidden;
      border: 1px solid #cbd5e1;
    }
    .confusion-matrix-table th, .confusion-matrix-table td {
      border: 1px solid #cbd5e1;
      padding: 12px;
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
      # CABECERA  
      # =====================================
      tags$div(
        style = "margin-bottom: 25px;",
        h1(
          "Regresión Logística Binaria",
          style = "font-weight: 800; color: #1e3a8a; margin-bottom: 6px; font-size: 2.5rem;"
        ),
        p(
          "Técnica de clasificación multivariante basada en el modelado de la probabilidad de ocurrencia de un fenómeno dicotómico.",
          style = "color: #64748b; font-size: 1.15rem; margin-bottom: 25px;"
        )
      ),
      
      # =====================================
      # BLOQUES  
      # =====================================
      bslib::layout_column_wrap(
        width = 1/3,
        heights_equal = "row",
        style = "margin-bottom: 35px;",
        
        # Bloque 1: Planteamiento 
        bslib::card(
          style = "border: 1px solid #cbd5e1; border-radius: 8px; overflow: hidden; background: #ffffff; box-shadow: 0 2px 4px rgba(0,0,0,0.02);",
          bslib::card_header(
            "1. Planteamiento Probabilístico",
            style = "background-color: #e0e7ff; color: #1e1b4b; font-weight: 700; font-size: 1.1rem; padding: 12px 16px;"
          ),
          bslib::card_body(
            style = "padding: 20px; line-height: 1.6; color: #334155;",
            p("La probabilidad de éxito de la variable dependiente dicotómica se define formalmente mediante la siguiente función logística:"),
            tags$div(
              style = "background: #f8fafc; padding: 10px; border-radius: 6px; text-align: center; margin: 10px 0; border: 1px solid #e2e8f0; font-size: 0.9rem;",
              HTML("$$P(Y = 1 \\mid X = x) = \\frac{1}{1 + e^{-(\\beta_0 + \\beta_1 x_1 + \\dots + \\beta_p x_p)}}$$")
            ),
            p("Garantiza que las estimaciones resultantes queden estrictamente acotadas en el rango probabilístico [0, 1].")
          )
        ),
        
        # Bloque 2: Elementos del Modelo
        bslib::card(
          style = "border: 1px solid #cbd5e1; border-radius: 8px; overflow: hidden; background: #ffffff; box-shadow: 0 2px 4px rgba(0,0,0,0.02);",
          bslib::card_header(
            "2. Elementos del Modelo",
            style = "background-color: #dcfce7; color: #064e3b; font-weight: 700; font-size: 1.1rem; padding: 12px 16px;"
          ),
          bslib::card_body(
            style = "padding: 20px; line-height: 1.6; color: #334155;",
            tags$ul(
              style = "padding-left: 15px; margin: 0; list-style-type: none;",
              tags$li(style = "margin-bottom: 8px;", HTML("• <b>\\(Y\\)</b> es la variable dependiente.")),
              tags$li(style = "margin-bottom: 8px;", HTML("• Las \\(p\\) variables explicativas se designan por <b>\\(x_1, x_2, \\dots, x_p\\)</b>.")),
              tags$li(style = "margin-bottom: 8px;", HTML("• <b>\\(\\beta_0, \\beta_1, \\dots, \\beta_p\\)</b> son los parámetros del modelo.")),
              tags$li(style = "margin-bottom: 0;", HTML("• <b>\\(e\\)</b> es la constante de Euler (\\(e \\approx 2,718\\))."))
            )
          )
        ),
        
        # Bloque 3: Escenario de Aplicación
        bslib::card(
          style = "border: 1px solid #cbd5e1; border-radius: 8px; overflow: hidden; background: #ffffff; box-shadow: 0 2px 4px rgba(0,0,0,0.02);",
          bslib::card_header(
            "3. Escenario de Aplicación",
            style = "background-color: #fef9c3; color: #713f12; font-weight: 700; font-size: 1.1rem; padding: 12px 16px;"
          ),
          bslib::card_body(
            style = "padding: 20px; line-height: 1.6; color: #334155;",
            p(HTML("Si la variable respuesta es de 2 categorías se trata de una <b>regresión logística binaria</b>.")),
            p("El problema de la clasificación multiclase (más de 2 categorías) no se incluye en esta sección; en su lugar se aborda mediante el Análisis Discriminante Lineal, el cual presenta un enfoque distinto y supone normalidad y homocedasticidad en los datos.")
          )
        )
      ),
      
      # =====================================
      # ESTIMACIÓN DE PARÁMETROS
      # =====================================
      h4(icon("gears"), "Estimación de parámetros: Máxima Verosimilitul", style = "color: #1e40af; margin-bottom: 12px; font-weight: 700;"),
      bslib::card(
        class = "theory-card", style = "margin-bottom: 30px;",
        bslib::card_body(
          p("A diferencia del modelo de regresión lineal, los parámetros \\(\\boldsymbol{\\beta}\\) no se obtienen por mínimos cuadrados ordinarios, sino mediante el método de Máxima Verosimilitud (MLE). Este método busca los valores de \\(\\boldsymbol{\\beta}\\) que maximizan la probabilidad de observar los datos muestrales obtenidos."),
          p("Para \\(N\\) observaciones, la función de log-verosimilitud se define como:"),
          tags$div(class = "equation-container", HTML("$$\\ell(\\boldsymbol{\\beta}) = \\sum_{i=1}^{N} \\left[ y_i \\log(p(x_i; \\boldsymbol{\\beta})) + (1 - y_i) \\log(1 - p(x_i; \\boldsymbol{\\beta})) \\right]$$")),
          p(HTML("Donde \\(\\mathbf{x}_i = (1, x_{i1}, \\dots, x_{ip})^T\\), \\(y_i\\) es la respuesta observada (0 o 1) y \\(p(x_i; \\boldsymbol{\\beta})\\) es la probabilidad de que \\(x_i\\) pertenezca a \\(Y = 1\\) dado el valor de \\(x_i\\) y el parámetro \\(\\boldsymbol{\\beta}\\). Esta expresión puede simplificarse como:")),
          tags$div(class = "equation-container", HTML("$$\\ell(\\boldsymbol{\\beta}) = \\sum_{i=1}^{N} \\left[ y_i \\boldsymbol{\\beta}^T \\mathbf{x}_i - \\log(1 + e^{\\boldsymbol{\\beta}^T \\mathbf{x}_i}) \\right]$$")),
          p("Para maximizar esta función, se deriva y se iguala a 0:"),
          tags$div(class = "equation-container", HTML("$$\\frac{\\partial \\ell(\\boldsymbol{\\beta})}{\\partial \\boldsymbol{\\beta}} = \\sum_{i=1}^{N} \\mathbf{x}_i (y_i - p(x_i; \\boldsymbol{\\beta})) = 0$$")),
          p(HTML("Como resultado, se genera un sistema de \\(p+1\\) ecuaciones no lineales. Dado que la primera componente de \\(\\mathbf{x}_i\\) es la intersección, la primera ecuación implica que el número esperado de eventos coincide con el observado:")),
          tags$div(class = "equation-container", HTML("$$\\sum_{i=1}^{N} y_i = \\sum_{i=1}^{N} p(x_i; \\boldsymbol{\\beta})$$")),
          p("Este sistema se resuelve de manera iterativa mediante algoritmos numéricos como el de Newton-Raphson.")
        )
      ),
      
      # =====================================
      # TRANSFORMACIÓN LOGIT
      # =====================================
      h4(icon("arrow-right-arrow-left"), "Transformación Logit e Interpretación", style = "color: #1e40af; margin-bottom: 12px; font-weight: 700;"),
      bslib::card(
        class = "theory-card", style = "margin-bottom: 30px;",
        bslib::card_body(
          p("En cuanto a la interpretación de los coeficientes, el modelo de regresión logística se fundamenta en la transformación logit de la probabilidad \\(p\\) de que ocurra el evento de interés, expresándose como:"),
          tags$div(class = "equation-container", HTML("$$\\log \\left( \\frac{p}{1 - p} \\right) = \\beta_0 + \\beta_1 X_1 + \\beta_2 X_2 + \\dots + \\beta_p X_p$$")),
          p(HTML("Donde el cociente \\(\\frac{p}{1 - p}\\) representa las <i>odds</i> (razón de ventajas). Es decir, el cociente entre la probabilidad de que ocurra el evento de interés y la probabilidad de que no ocurra, siendo su logaritmo natural lineal respecto a los parámetros del modelo.")),
          p(HTML("Los coeficientes \\(\\beta_i\\) estiman el cambio en el <i>logit</i> por cada unidad de incremento en \\(X_i\\), de modo que su interpretación directa no es intuitiva en la escala original. Por ello, se emplean las odds ratios (OR) o razones de posibilidades, los cuales se obtienen aplicando la función exponencial:")),
          tags$div(class = "equation-container", HTML("$$\\text{OR}_i = e^{\\beta_i}$$")),
          p(HTML("donde \\(\\text{OR}_i\\) representa el cambio multiplicativo de las odds asociado al incremento unitario en la variable \\(X_i\\), manteniendo constantes el resto de las variables. Valores superiores a 1 indican un aumento de las <i>odds</i> de que ocurra el evento, mientras que valores inferiores a 1 indican una disminución. Por otro lado, si la razón es igual a 1, los cambios en la variable independiente no tienen efecto sobre las odds de ocurrencia del evento."))
        )
      ),
      
      # =====================================
      # EVALUACIÓN: CURVA ROC Y AUC
      # =====================================
      h4(icon("chart-area"), "Evaluación del Modelo: Curvas ROC y AUC", style = "color: #1e40af; margin-bottom: 12px; font-weight: 700;"),
      bslib::card(
        class = "theory-card", style = "margin-bottom: 30px;",
        bslib::card_body(
          p("Una vez estimados los parámetros, es necesario evaluar la capacidad predictiva del modelo. Para ello, una de las medidas más utilizadas es la curva ROC (Receiver Operating Characteristic)."),
          p("Esta representa la relación entre la tasa de verdaderos positivos (true positive rate, TPR) y la tasa de falsos positivos (false positive rate, FPR) para distintos umbrales de clasificación. La tasa de verdaderos positivos se conoce como sensibilidad y representa la proporción de casos positivos correctamente clasificados. Por su parte, la tasa de falsos positivos es FPR = 1 - Especificidad, siendo la especificidad la tasa de verdaderos negativos (true negative rate, TNR), es decir, la proporción de casos negativos correctamente identificados por el modelo. Matemáticamente:"),
          tags$div(class = "equation-container", HTML("$$\\text{Sensibilidad} = \\frac{\\text{VP}}{\\text{VP} + \\text{FN}}, \\quad \\text{Especificidad} = \\frac{\\text{VN}}{\\text{VN} + \\text{FP}}$$")),
          p(HTML("siendo <i>VP</i> el número de verdaderos positivos, <i>VN</i> el número de verdaderos negativos, <i>FP</i> el número de falsos positivos y <i>FN</i> el número de falsos negativos.")),
          
          # Tabla Matriz de Confusión 
          tags$div(
            style = "margin: 20px auto; max-width: 550px;",
            tags$table(
              class = "confusion-matrix-table",
              tags$tr(
                tags$th(style = "border: none; background: transparent;"),
                tags$th(colspan = "2", style = "background: #1e3a8a; color: white; font-weight: bold; padding: 8px;", "Clase Real")
              ),
              tags$tr(
                tags$th(style = "background: #1e3a8a; color: white; font-weight: bold; padding: 8px; font-size:0.9rem;", "Predicción"),
                tags$th(style = "background: #f1f5f9; color: #1e3a8a; font-weight: bold; width: 140px; padding: 8px;", "Positivo (1)"),
                tags$th(style = "background: #f1f5f9; color: #1e3a8a; font-weight: bold; width: 140px; padding: 8px;", "Negativo (0)")
              ),
              tags$tr(
                tags$td(style = "background: #f8fafc; font-weight: bold; padding: 10px;", "Positivo (1)"),
                tags$td(style = "background: #ffffff; padding: 10px;", HTML("<b>VP</b>")),
                tags$td(style = "background: #fff1f2; padding: 10px;", HTML("<span style='color:#be123c;'><b>FP</b></span>"))
              ),
              tags$tr(
                tags$td(style = "background: #f8fafc; font-weight: bold; padding: 10px;", "Negativo (0)"),
                tags$td(style = "background: #fff1f2; padding: 10px;", HTML("<span style='color:#be123c;'><b>FN</b></span>")),
                tags$td(style = "background: #ffffff; padding: 10px;", HTML("<b>VN</b>"))
              )
            )
          ),
          
          p("De este modo, la curva ROC permite analizar la relación entre la capacidad del modelo para detectar correctamente los casos positivos y el número de falsas clasificaciones positivas, independientemente del umbral de clasificación elegido."),
          p("En cuanto a la calidad global de la clasificación, suele resumirse mediante el Área Bajo la Curva (AUC), definida como:"),
          tags$div(class = "equation-container", HTML("$$\\text{AUC} = \\int_{0}^{1} \\text{ROC}(x) \\, dx$$")),
          p("El valor del AUC toma valores entre 0 y 1. Un valor cercano a 0.5 indica una capacidad predictiva similar al azar, mientras que valores próximos a 1 reflejan una excelente capacidad de discriminación entre ambas clases.")
        )
      ),
      
      # =====================================
      # MÉTRICAS
      # =====================================
      h4(icon("chart-simple"), "3.5.4 Métricas de evaluación", style = "color: #1e40af; margin-bottom: 12px; font-weight: 700;"),
      bslib::card(
        class = "theory-card",
        bslib::card_body(
          p("Una vez que se ha ajustado un modelo de clasificación, es necesario evaluar su capacidad predictiva para averiguar si generaliza correctamente nuevos individuos. Esta evaluación se realiza mediante diferentes métricas que miden el rendimiento de los diferentes modelos y que se calculan a partir de las predicciones obtenidas sobre el conjunto de datos de evaluación."),
          p(HTML("Una de las métricas más utilizadas es la exactitud o <i>accuracy</i>, que mide la proporción de observaciones correctamente clasificadas respecto al total de observaciones evaluadas. Se define como:")),
          tags$div(class = "equation-container", HTML("$$\\text{Accuracy} = \\frac{\\text{TP} + \\text{TN}}{\\text{TP} + \\text{TN} + \\text{FP} + \\text{FN}}$$")),
          p(HTML("donde <b>TP</b> (<i>True Positives</i>) represents los verdaderos positivos, <b>TN</b> (<i>True Negatives</i>) los verdaderos negativos, <b>FP</b> (<i>False Positives</i>) los falsos positivos y <b>FN</b> (<i>False Negatives</i>) los falsos negativos. Un valor de <i>accuracy</i> próximo a 1 indica un elevado porcentaje de clasificaciones correctas, mientras que valores más bajos reflejan un peor desempeño del modelo.")),
          p(HTML("No obstante, esta métrica puede resultar engañosa cuando las clases están desbalanceadas, ya que un modelo podría obtener una alta exactitud simplemente prediciendo siempre la clase mayoritaria. Por este motivo, suele complementarse con otras métricas como la precisión (<i>precision</i>), la sensibilidad (<i>recall</i>) y la medida F1.")),
          
          tags$div(
            style = "margin-top: 20px; display: flex; flex-direction: column; gap: 5px;",
            tags$div(
              class = "metric-definition",
              p(HTML("La <b>precisión</b> se define como la proporción de predicciones positivas correctas entre todas las predicciones positivas realizadas:")),
              tags$div(class = "equation-container", HTML("$$\\text{Precision} = \\frac{\\text{TP}}{\\text{TP} + \\text{FP}}$$"))
            ),
            tags$div(
              class = "metric-definition",
              p(HTML("La <b>sensibilidad o <i>recall</i></b> mide la capacidad del modelo para identificar correctamente los casos positivos:")),
              tags$div(class = "equation-container", HTML("$$\\text{Recall} = \\frac{\\text{TP}}{\\text{TP} + \\text{FN}}$$"))
            ),
            tags$div(
              class = "metric-definition",
              p(HTML("Por último, la <b>medida F1</b> combina ambas métricas en una única medida, calculada como la media armónica entre precisión y sensibilidad:")),
              tags$div(class = "equation-container", HTML("$$\\text{F1} = 2 \\cdot \\frac{\\text{Precision} \\cdot \\text{Recall}}{\\text{Precision} + \\text{Recall}}$$"))
            )
          )
        )
      )
    )
  )
}
Regresion_logistica_Teoria_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
  })
}

# -------------------------------
# ANALISIS
# -------------------------------
Regresion_logistica_Analisis_UI <- function(id){
  ns <- NS(id)
  tagList(
    # Título 
    h3("Análisis", 
       style = "color: #1a446c; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; font-weight: 600; margin-top: 40px; margin-bottom: 20px; border-bottom: 2px solid #f4f6f9; padding-bottom: 10px;"),
    
    fluidRow(
      #--------------------------------------------------
      # PANEL LATERAL 
      #--------------------------------------------------
      column(4,
             wellPanel(
               h4("Configuración Logística"),
               p("Establezca los parámetros de clasificación binaria mediante Máxima Verosimilitud."),
               hr(),
               uiOutput(ns("ui_var_dep")),
               uiOutput(ns("ui_var_indep")),
               hr(),
               helpText("La regresión logística predice la probabilidad condicional de pertenecer a una categoría específica (Evento)."),
               helpText("Nota: Se eliminan filas con valores faltantes automáticamente.")
             )
      ),
      
      #--------------------------------------------------
      # PANEL PRINCIPAL (A LA DERECHA)
      #--------------------------------------------------
      column(8,
             # Banner de error
             uiOutput(ns("alerta_logistica")),
             br(),
             
             tabsetPanel(
               id = ns("tabs_log"),
               
               # PESTAÑA 1: DATOS
               tabPanel("1. Datos", 
                        br(),
                        p("Información: El modelo binomial requiere una variable dependiente de naturaleza estrictamente dicotómica.", 
                          style = "color: #7f8c8d; font-style: italic; margin-bottom: 25px;"),
                        h4("Dataset original completo", style = "color: #2c3e50; font-weight: 500;"), 
                        DT::DTOutput(ns("tabla_original")),
                        br(), hr(), br(),
                        h4("Preparación Logística (Codificación factorizada 0/1)", style = "color: #2c3e50; font-weight: 500;"), 
                        DT::DTOutput(ns("tabla_preparada"))
               ),
               
               # PESTAÑA 2: RESULTADOS
               tabPanel("2. Resultados del Modelo", 
                        br(),
                        h4("Coeficientes (Log-Odds)", style = "color: #2c3e50; font-weight: 500;"), 
                        DT::DTOutput(ns("tabla_coefs")),
                        br(),
                        h4("Odds Ratios (Interpretación Explicativa Real)", style = "color: #2c3e50; font-weight: 500;"), 
                        DT::DTOutput(ns("tabla_odds")),
                        br(),
                        h4("Métricas de evaluación", style = "color: #2c3e50; font-weight: 500;"), 
                        DT::DTOutput(ns("tabla_metricas")),
                        br(),
                        h4("Interpretación de Resultados", style = "color: #2c3e50; font-weight: 500;"), 
                        verbatimTextOutput(ns("interp_resultados_log"))
               ),
               
               # PESTAÑA 3: CLASIFICACIÓN
               tabPanel("3. Clasificación y Curva ROC", 
                        br(),
                        fluidRow(
                          column(6, 
                                 h4("Matriz de Confusión", style = "color: #2c3e50; font-weight: 500;"), 
                                 DT::DTOutput(ns("matriz_confusion"))),
                          column(6, 
                                 h4("Curva ROC", style = "color: #2c3e50; font-weight: 500;"), 
                                 plotOutput(ns("plot_roc")))
                        ),
                        br(),
                        h4("Interpretación del Rendimiento", style = "color: #2c3e50; font-weight: 500;"), 
                        verbatimTextOutput(ns("interp_rendimiento_log"))
               ),
               
               # PESTAÑA 4: PREDICCIÓN
               tabPanel("4. Predicción",
                        br(),
                        h4("Análisis de las Probabilidades Predichas", style = "color: #2c3e50; font-weight: 500;"), 
                        plotOutput(ns("plot_pred_log"), height = "450px"),
                        br(),
                        h4("Interpretación de la Capacidad Predictiva", style = "color: #2c3e50; font-weight: 500;"), 
                        verbatimTextOutput(ns("interp_pred_log"))
               )
             )
      )
    )
  )
}

Regresion_logistica_Analisis_Server <- function(id, datos, datos_ejemplo = NULL){
  moduleServer(id, function(input, output, session){
    
    # ==========================================================================
    # FUNCIONES DE VALIDACIÓN 
    # ==========================================================================
    validar_datos_logistica <- function(df) {
      if (is.null(df) || nrow(df) == 0) return("No se han cargado datos.")
      
      vars_binarias <- names(df)[sapply(df, function(x) length(unique(x[!is.na(x)])) == 2)]
      
      if (length(vars_binarias) == 0 && !("Class" %in% names(df))) {
        return("El dataset no contiene ninguna variable dicotómica (con exactamente 2 categorías) apta como variable dependiente (Y).")
      }
      
      if (nrow(df[complete.cases(df), ]) < 10) {
        return("No hay suficientes observaciones completas (mínimo sugerido: 10 filas sin valores faltantes) para ajustar el modelo.")
      }
      
      return(NULL) 
    }
    
    mensaje_error_analisis <- function(mensaje) {
      div(
        style = "background-color: #fdf2f2; color: #9b1c1c; border: 1px solid #fde8e8; padding: 20px; margin-bottom: 25px; border-radius: 6px;",
        tags$span(style = "font-weight: bold; font-size: 16px;", tags$i(class = "fa fa-exclamation-triangle"), " No es posible realizar el análisis."),
        br(),
        tags$span(style = "font-style: italic; color: #4b5563; font-size: 14px;", "Información: El modelo Binomial requiere una variable de respuesta binaria/dicotómica (0/1 o dos factores)."),
        br(), br(),
        tags$span(style = "font-weight: 500;", mensaje)
      )
    }
    
    # ==========================================================================
    # LOGICA DE VALIDACIÓN
    # ==========================================================================
    evaluacion_logistica <- reactive({
      df <- if(!is.null(datos()) && is.data.frame(datos())) datos() else datos_ejemplo
      if(is.list(df) && !is.data.frame(df)) df <- df$Regresion_multiple 
      
      error_msg <- validar_datos_logistica(df)
      if (!is.null(error_msg)) return(list(valido = FALSE, mensaje = error_msg))
      return(list(valido = TRUE, datos = df))
    })
    
    # Renderizado del error 
    output$alerta_logistica <- renderUI({
      eval <- evaluacion_logistica()
      if (!eval$valido) mensaje_error_analisis(eval$mensaje) else NULL
    })
    
    # --- 1. DATOS BASE ---
    datos_base <- reactive({
      req(evaluacion_logistica()$valido)
      df <- evaluacion_logistica()$datos
      as.data.frame(df)[complete.cases(df), , drop = FALSE]
    })
    
    # --- 2. SELECTORES ---
    output$ui_var_dep <- renderUI({
      req(evaluacion_logistica()$valido)
      df <- datos_base()
      vars <- names(df)[sapply(df, function(x) length(unique(x)) == 2)]
      
      # Si "Class" está en el dataset, forzar a que sea la seleccionada por defecto
      seleccionada <- if("Class" %in% names(df)) "Class" else if(length(vars) > 0) vars[1] else NULL
      if(length(vars) == 0 && "Class" %in% names(df)) vars <- "Class"
      
      selectInput(session$ns("var_dep"), "Variable a Predecir (Y):", choices = vars, selected = seleccionada)
    })
    
    output$ui_var_indep <- renderUI({
      req(evaluacion_logistica()$valido)
      req(input$var_dep)
      df <- datos_base()
      cols <- setdiff(names(df), c(input$var_dep, "Id"))
      
      # Filtrar las columnas que sean numéricas 
      cols_numericas <- cols[sapply(df[, cols, drop = FALSE], is.numeric)]
      
      # Si  no detecta numéricas, permitir cols  para no romper la app
      if(length(cols_numericas) == 0) cols_numericas <- cols
      
      # Selección por defecto de las dos primeras variables numéricas
      seleccion_defecto <- cols_numericas[1:min(2, length(cols_numericas))]
      
      selectizeInput(session$ns("var_indep"), "Variables Predictoras (X):", 
                     choices = cols, multiple = TRUE, 
                     selected = seleccion_defecto,
                     options = list(plugins = list('remove_button'), persist = FALSE))
    })
    
    # --- 3. PREPARACIÓN  ---
    datos_log <- reactive({
      req(evaluacion_logistica()$valido)
      req(input$var_dep, input$var_indep)
      df <- datos_base()
      
      # Forzar variable dependiente a Factor binario 
      df[[input$var_dep]] <- as.factor(df[[input$var_dep]])
      levels(df[[input$var_dep]]) <- c(0, 1) 
      
      # Limpieza y conversión  a numéricas ordinarles para las variables independientes 
      for(col in input$var_indep) {
        if(is.factor(df[[col]]) || is.character(df[[col]])) {
          df[[col]] <- as.numeric(as.character(df[[col]]))
        }
      }
      
      df
    })
    
    # --- 4. MODELO  ---
    modelo_log <- reactive({
      req(evaluacion_logistica()$valido)
      req(input$var_dep, input$var_indep)
      req(length(input$var_indep) >= 1)
      formula_str <- paste(input$var_dep, "~", paste(input$var_indep, collapse = "+"))
      glm(as.formula(formula_str), data = datos_log(), family = binomial)
    })
    
    # --- 5. OUTPUTS DE TABLAS ---
    output$tabla_original <- DT::renderDT({
      req(evaluacion_logistica()$valido)
      DT::datatable(
        datos_base(), 
        options = list(paging = FALSE, scrollY = "400px", scrollX = TRUE, autoWidth = TRUE),
        class = 'cell-border stripe hover compact'
      ) %>% 
        DT::formatRound(columns = names(datos_base())[sapply(datos_base(), is.numeric)], digits = 3)
    })
    
    output$tabla_preparada <- DT::renderDT({
      req(evaluacion_logistica()$valido)
      req(datos_log(), input$var_dep, input$var_indep)
      df_prep <- datos_log()[, c(input$var_dep, input$var_indep), drop = FALSE]
      
      DT::datatable(
        df_prep, 
        options = list(paging = FALSE, scrollY = "400px", scrollX = TRUE, autoWidth = TRUE),
        class = 'cell-border stripe hover compact'
      ) %>% 
        DT::formatRound(columns = names(df_prep)[sapply(df_prep, is.numeric)], digits = 3)
    })
    
    output$tabla_coefs <- DT::renderDT({
      req(evaluacion_logistica()$valido)
      req(modelo_log())
      df_c <- broom::tidy(modelo_log())
      colnames(df_c) <- c("Término", "Estimación (Log-Odds)", "Error Estándar", "Estadístico z", "p-valor")
      
      DT::datatable(df_c, options = list(paging = FALSE, scrollX = TRUE), class = 'cell-border compact') %>%
        DT::formatRound(columns = 2:5, digits = 4)
    })
    
    output$tabla_odds <- DT::renderDT({
      req(evaluacion_logistica()$valido)
      req(modelo_log())
      or <- exp(coef(modelo_log()))
      ci <- exp(suppressMessages(confint.default(modelo_log())))
      
      df_or <- data.frame(
        Variable = names(or), 
        Odds_Ratio = or, 
        IC_Inferior_2.5 = ci[,1], 
        IC_Superior_97.5 = ci[,2]
      )
      
      DT::datatable(df_or, options = list(paging = FALSE, scrollX = TRUE), class = 'cell-border compact') %>%
        DT::formatRound(columns = 2:4, digits = 3)
    })
    
    output$tabla_metricas <- DT::renderDT({
      req(evaluacion_logistica()$valido)
      req(modelo_log())
      
      probs <- predict(modelo_log(), type="response")
      clases <- ifelse(probs>0.5,1,0)
      reales <- as.numeric(as.character(datos_log()[[input$var_dep]]))
      
      matriz <- table(Real=reales, Predicho=clases)
      
      TP <- if(nrow(matriz) > 1 && ncol(matriz) > 1) matriz[2,2] else 0
      TN <- matriz[1,1]
      FP <- if(ncol(matriz) > 1) matriz[1,2] else 0
      FN <- if(nrow(matriz) > 1) matriz[2,1] else 0
      
      accuracy <- (TP+TN)/sum(matriz)
      precision <- ifelse((TP+FP)==0, NA, TP/(TP+FP))
      recall <- ifelse((TP+FN)==0, NA, TP/(TP+FN))
      
      f1 <- ifelse(is.na(precision) | is.na(recall) | (precision+recall)==0,
                   NA, 2*precision*recall/(precision+recall))
      
      df_m <- data.frame(
        Métrica=c("Accuracy", "Precision", "Recall", "F1-score"),
        Valor=round(c(accuracy, precision, recall, f1),4)
      )
      
      DT::datatable(df_m, options=list(paging=FALSE, scrollX=TRUE), rownames=FALSE)
    })
    
    output$interp_resultados_log <- renderText({
      req(evaluacion_logistica()$valido)
      req(input$var_dep)
      paste0(
        "Los coeficientes de Log-Odds indican el cambio en el logaritmo de la probabilidad a favor ante un aumento del regresor.\n",
        "Los Odds Ratios (OR) superiores a 1 indican que el predictor incrementa la probabilidad del suceso (Efecto Positivo).\n\n",
        "Ejemplo práctico (Dataset 'BreastCancer'): Al modelar el diagnóstico del tumor (Y: Class) para predecir la probabilidad ",
        "de que el tejido sea maligno ('malignant', codificado como Evento 1) a partir de los rasgos celulares, descubrirás ",
        "que variables como 'Cell.Size' (tamaño celular) o 'Cl.thickness' muestran Odds Ratios significativos y mayores que 1. ",
        "Esto demuestra estadísticamente que un incremento en la irregularidad del tamaño celular eleva exponencialmente las posibilidades de malignidad."
      )
    })
    
    # --- 6. CLASIFICACIÓN Y ROC ---
    output$matriz_confusion <- DT::renderDT({
      req(evaluacion_logistica()$valido)
      req(modelo_log())
      preds <- predict(modelo_log(), type = "response")
      clases <- ifelse(preds > 0.5, 1, 0)
      
      matriz <- as.data.frame.matrix(table(Real = datos_log()[[input$var_dep]], Predicho = clases))
      DT::datatable(matriz, options = list(paging = FALSE, searching = FALSE), class = 'cell-border compact', rownames = TRUE)
    })
    
    output$plot_roc <- renderPlot({
      req(evaluacion_logistica()$valido)
      req(modelo_log())
      preds <- predict(modelo_log(), type = "response")
      res_roc <- pROC::roc(datos_log()[[input$var_dep]], preds)
      pROC::plot.roc(res_roc, main = paste("Curva ROC - AUC:", round(res_roc$auc, 3)), 
                     col = "#1a446c", lwd = 4, print.auc = TRUE)
    })
    
    output$interp_rendimiento_log <- renderText({
      req(evaluacion_logistica()$valido)
      req(modelo_log())
      paste0(
        "La matriz de confusión cuantifica los aciertos y los errores de clasificación utilizando un umbral estándar de corte p = 0.5.\n",
        "La curva ROC evalúa la capacidad de discriminación del modelo sobre todos los umbrales posibles (un AUC de 1 indica clasificación perfecta).\n\n",
        "Ejemplo práctico (Dataset 'BreastCancer'): Dado que las características citológicas extraídas son predictores biológicos muy robustos, ",
        "la matriz de confusión registrará una tasa de falsos positivos y falsos negativos bajísima. Esto se traduce en una curva ROC impecable ",
        "con un Área Bajo la Curva (AUC) que suele superar el 0.95, demostrando el altísimo poder de clasificación clínica de la aplicación de R Shiny."
      )
    })
    
    # --- 7. PESTAÑA 4: PREDICCIÓN ---
    output$plot_pred_log <- renderPlot({
      req(evaluacion_logistica()$valido)
      req(modelo_log(), input$var_dep)
      
      log_odds <- predict(modelo_log(), type = "link")
      probabilidades <- predict(modelo_log(), type = "response")
      
      df_plot <- data.frame(
        Log_Odds = log_odds,
        Probabilidad = probabilidades,
        Real = factor(datos_log()[[input$var_dep]], labels = c("Benigno (0)", "Maligno (1)"))
      )
      
      df_plot <- df_plot[order(df_plot$Log_Odds), ]
      
      ggplot(df_plot, aes(x = Log_Odds, y = Probabilidad)) +
        geom_line(color = "#1a446c", size = 1.2, alpha = 0.9, name = "Función Logística") +
        geom_point(aes(color = Real), size = 2.5, alpha = 0.5, position = position_jitter(height = 0.02, width = 0)) +
        theme_minimal() +
        labs(title = "Curva Ajustada del Modelo Regresión Logística",
             subtitle = "Representación de la función sigmoide sobre el índice de riesgo",
             x = "Índice de Riesgo Estimado (Log-Odds)",
             y = "Probabilidad Asignada de Malignidad P(Y=1|X)",
             color = "Diagnóstico Real") +
        scale_color_manual(values = c("#2ecc71", "#e74c3c")) +
        scale_y_continuous(limits = c(-0.05, 1.05), breaks = seq(0, 1, by = 0.25)) +
        theme(legend.position = "bottom")
    })
    
    output$interp_pred_log <- renderText({
      req(evaluacion_logistica()$valido)
      req(modelo_log())
      paste0(
        "La gráfica sigmoidal ilustra la probabilidad predicha frente al log-odds del modelo.\n",
        "Un buen ajuste muestra una separación clara de los puntos reales (0 y 1) en los extremos de la curva en forma de S."
      )
    })
  })
}
# -------------------------------
# AUTOEVALUACION
# -------------------------------
Regresion_logistica_Auto_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
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
        title = "➕ Gestión: Añadir pregunta personalizada de la regresión logística",
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
        
        actionButton(ns("add"), "Guardar pregunta en el banco de la regresión logística", class = "btn-success btn-sm mt-2")
      )
    )
  )
}
Regresion_logistica_Auto_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    mostrar_respuestas <- reactiveVal(FALSE)
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
        texto = "¿Cuál es el objetivo principal de la regresión logística?",
        opciones = c(
          "Reducir la dimensionalidad",
          "Clasificar observaciones en categorías",
          "Agrupar individuos similares",
          "Estimar componentes principales"
        ),
        correcta = "Clasificar observaciones en categorías"
      ),
      
      list(
        texto = "¿Qué tipo de variable respuesta utiliza la regresión logística binaria?",
        opciones = c(
          "Una variable continua",
          "Una variable ordinal",
          "Una variable dicotómica",
          "Una variable de conteo"
        ),
        correcta = "Una variable dicotómica"
      ),
      
      list(
        texto = "¿Qué función de enlace emplea la regresión logística?",
        opciones = c(
          "Logit",
          "Identidad",
          "Raíz cuadrada",
          "Logaritmo natural"
        ),
        correcta = "Logit"
      ),
      
      list(
        texto = "¿Qué representa una probabilidad estimada de 0.85 para un individuo?",
        opciones = c(
          "Que pertenece a la clase positiva con un 85% de probabilidad",
          "Que el modelo acierta el 85% de las veces",
          "Que el error del modelo es del 15%",
          "Que el individuo tiene un 85% de las variables correctas"
        ),
        correcta = "Que pertenece a la clase positiva con un 85% de probabilidad"
      ),
      
      list(
        texto = "Si un coeficiente β es positivo, ¿qué ocurre al aumentar esa variable?",
        opciones = c(
          "Disminuye la probabilidad del evento",
          "Aumenta la probabilidad del evento",
          "No cambia la probabilidad",
          "El modelo deja de ser válido"
        ),
        correcta = "Aumenta la probabilidad del evento"
      ),
      
      list(
        texto = "¿Qué indica un Odds Ratio igual a 1?",
        opciones = c(
          "No existe efecto de la variable",
          "La variable duplica el riesgo",
          "La variable reduce el riesgo a la mitad",
          "Existe multicolinealidad"
        ),
        correcta = "No existe efecto de la variable"
      ),
      
      list(
        texto = "¿Qué medida se utiliza habitualmente para evaluar la capacidad discriminante del modelo?",
        opciones = c(
          "Coeficiente de variación",
          "Área bajo la curva ROC (AUC)",
          "Coeficiente de correlación",
          "Media de los residuos"
        ),
        correcta = "Área bajo la curva ROC (AUC)"
      ),
      
      list(
        texto = "En una matriz de confusión, ¿qué representa un falso positivo?",
        opciones = c(
          "Un caso positivo clasificado como negativo",
          "Un caso negativo clasificado como positivo",
          "Un positivo correctamente clasificado",
          "Un negativo correctamente clasificado"
        ),
        correcta = "Un caso negativo clasificado como positivo"
      ),
      
      list(
        texto = "¿Qué ocurre si se disminuye mucho el umbral de clasificación (por ejemplo de 0.5 a 0.2)?",
        opciones = c(
          "Se clasifican más observaciones como positivas",
          "Se clasifican menos observaciones como positivas",
          "No cambia ninguna predicción",
          "Desaparecen los falsos negativos"
        ),
        correcta = "Se clasifican más observaciones como positivas"
      ),
      
      list(
        texto = "Supón que un modelo presenta una sensibilidad muy alta pero una especificidad baja. ¿Qué significa?",
        opciones = c(
          "Detecta bien los positivos pero confunde muchos negativos",
          "Detecta mal los positivos",
          "Clasifica perfectamente todas las observaciones",
          "Existe sobreajuste garantizado"
        ),
        correcta = "Detecta bien los positivos pero confunde muchos negativos"
      ),
      
      list(
        texto = "¿Qué problema puede provocar que varias variables explicativas estén muy correlacionadas?",
        opciones = c(
          "Multicolinealidad",
          "Sobreajuste del umbral",
          "Heterocedasticidad",
          "Autocorrelación temporal"
        ),
        correcta = "Multicolinealidad"
      ),
      
      list(
        texto = "Si el AUC obtenido es 0.50, ¿cómo se interpreta?",
        opciones = c(
          "El modelo clasifica casi perfectamente",
          "El modelo no discrimina mejor que el azar",
          "Existe sobreajuste",
          "Los datos contienen errores"
        ),
        correcta = "El modelo no discrimina mejor que el azar"
      ),
      
      list(
        texto = "Al aumentar el número de verdaderos positivos sin modificar el resto de resultados, ¿qué medida mejora directamente?",
        opciones = c(
          "Precisión (Accuracy)",
          "Sensibilidad",
          "Error cuadrático medio",
          "Coeficiente de determinación"
        ),
        correcta = "Sensibilidad"
      ),
      
      list(
        texto = "Un paciente obtiene una probabilidad estimada de enfermedad del 0.74 y el umbral de clasificación es 0.5. ¿Cómo será clasificado?",
        opciones = c(
          "Como caso negativo",
          "No puede clasificarse",
          "Como caso positivo",
          "Depende del tamaño muestral"
        ),
        correcta = "Como caso positivo"
      ),
      
      list(
        texto = "¿Cuál de las siguientes afirmaciones es correcta?",
        opciones = c(
          "La regresión logística predice directamente valores continuos.",
          "Solo puede utilizar una variable explicativa.",
          "Puede utilizar variables continuas y categóricas como predictores.",
          "Solo funciona cuando todas las variables siguen una distribución normal."
        ),
        correcta = "Puede utilizar variables continuas y categóricas como predictores"
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
    
    preguntas <- reactive({
      c(preguntas_base, preguntas_usuario())
    })
    
    preguntas_ordenadas <- reactiveVal(NULL)
    
    observe({
      lista_actual <- preguntas()
      
      lista_enriquecida <- lapply(lista_actual, function(p) {
        p$id_unico <- paste0("q_", gsub("[^a-zA-Z0-9]", "", p$texto))
        p
      })
      
      if (is.null(isolate(preguntas_ordenadas()))) {
        preguntas_ordenadas(sample(lista_enriquecida, min(10, length(lista_enriquecida))))
      }
    })
    
    observeEvent(input$shuffle, {
      lista_actual <- preguntas()
      
      lista_enriquecida <- lapply(lista_actual, function(p) {
        p$id_unico <- paste0("q_", gsub("[^a-zA-Z0-9]", "", p$texto))
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
              feedback_ui <- div(class = "text-success mt-2 font-weight-bold", "✔️ ¡Correcto!")
            } else {
              feedback_ui <- div(class = "text-danger mt-2",
                                 paste0("❌ Incorrecto. Respuesta: ", correct))
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

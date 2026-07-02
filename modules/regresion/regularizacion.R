# =========================================================
# REGULARIZACIÓN - TEORÍA
# =========================================================

Regularizacion_Teoria_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    withMathJax(),
    
    tags$div(
      style = "padding:24px; background-color:#fcfdfe;",
      
      #=========================================================
      # CABECERA
      #=========================================================
      h2(
        "Técnicas de Regularización y Reducción de Dimensión",
        style = "font-weight:800; color:#1a365d; margin-bottom:6px;"
      ),
      
      p(
        "Los métodos de regularización y reducción de dimensión constituyen alternativas a la regresión lineal por Mínimos Cuadrados Ordinarios (MCO) cuando existe multicolinealidad entre las variables explicativas. Aunque persiguen el mismo objetivo de obtener modelos más estables y con mejor capacidad predictiva, lo hacen mediante estrategias diferentes.",
        style = "color:#64748b; font-size:1.05rem; margin-bottom:30px; max-width:1100px; line-height:1.6;"
      ),
      
      #=========================================================
      # DIFERENCIAS 
      #=========================================================
      h4(
        icon("layer-group"),
        "Estrategias para tratar la multicolinealidad",
        style = "color:#1e40af; margin-bottom:15px; font-weight:700;"
      ),
      
      bslib::layout_column_wrap(
        width = 1/2,
        heights_equal = "row",
        
        #-----------------------------------------
        # PENALIZACIÓN
        #-----------------------------------------
        bslib::card(
          style = "border-top:4px solid #3b82f6;
                 border-left:1px solid #e2e8f0;
                 border-right:1px solid #e2e8f0;
                 border-bottom:1px solid #e2e8f0;",
          
          bslib::card_body(
            h5(
              "Métodos basados en penalización (Ridge y Lasso)",
              style = "color:#1d4ed8; font-weight:700;"
            ),
            
            p(
              "Estos métodos mantienen todas las variables explicativas originales del modelo y modifican la función objetivo de los Mínimos Cuadrados Ordinarios incorporando un término de penalización sobre los coeficientes de regresión."
            ),
            
            p(
              "La intensidad de dicha penalización viene controlada por el hiperparámetro \\(\\lambda\\). A medida que aumenta su valor, los coeficientes experimentan una mayor contracción (shrinkage), reduciendo la varianza del estimador y mejorando la estabilidad del modelo frente a la multicolinealidad."
            )
          )
        ),
        
        #-----------------------------------------
        # REDUCCIÓN
        #-----------------------------------------
        bslib::card(
          style = "border-top:4px solid #eab308;
                 border-left:1px solid #e2e8f0;
                 border-right:1px solid #e2e8f0;
                 border-bottom:1px solid #e2e8f0;",
          
          bslib::card_body(
            h5(
              "Métodos basados en reducción de dimensión (PCR)",
              style = "color:#b45309; font-weight:700;"
            ),
            
            p(
              "En lugar de penalizar los coeficientes del modelo, estos métodos transforman previamente el espacio de las variables explicativas mediante un Análisis de Componentes Principales."
            ),
            
            p(
              "Posteriormente se seleccionan únicamente los primeros \\(M\\) componentes principales, que son ortogonales e incorrelados entre sí, eliminando la multicolinealidad antes de ajustar el modelo de regresión."
            )
          )
        )
      ),
      
      br(),
      br(),
      
      #=========================================================
      # MÉTODOS 
      #=========================================================
      h4(
        icon("list-check"),
        "Formulación de los métodos",
        style = "color:#1e40af; margin-bottom:15px; font-weight:700;"
      ),
      
      bslib::layout_column_wrap(
        width = 1/3,
        heights_equal = "row",
        
        #=========================================================
        # RIDGE
        #=========================================================
        bslib::card(
          style = "border:1px solid #e2e8f0;
                 box-shadow:0 1px 3px rgba(0,0,0,0.02);",
          
          bslib::card_header(
            tags$b("Regresión Ridge (Penalización \\(L_2\\))"),
            style = "background:#e0e7ff; color:#1e1b4b;"
          ),
          
          bslib::card_body(
            p(
              "La regresión Ridge modifica la función objetivo de los Mínimos Cuadrados Ordinarios añadiendo una penalización cuadrática sobre los coeficientes de regresión:"
            ),
            
            tags$div(
              style = "margin:14px 0; text-align:center;",
              HTML("
$$
\\hat{\\boldsymbol{\\beta}}^{Ridge}
=
\\operatorname*{arg\\,min}_{\\boldsymbol{\\beta}}
\\left\\{
\\sum_{i=1}^{n}
\\left(
y_i-
\\beta_0-
\\sum_{j=1}^{p}x_{ij}\\beta_j
\\right)^2
+
\\lambda
\\sum_{j=1}^{p}
\\beta_j^2
\\right\\}
$$")
            ),
            
            p(
              "El parámetro \\(\\lambda\\ge0\\) controla la intensidad de la penalización. A medida que aumenta su valor, los coeficientes se contraen progresivamente hacia cero, reduciendo la varianza del estimador sin eliminar variables del modelo."
            ),
            
            p(
              "Como consecuencia, la solución depende de la matriz \\((\\mathbf{X}^{T}\\mathbf{X}+\\lambda\\mathbf{I})\\), que permanece invertible incluso cuando existe multicolinealidad entre las variables explicativas."
            )
          )
        ),
        
        #=========================================================
        # LASSO
        #=========================================================
        bslib::card(
          style = "border:1px solid #e2e8f0;
                 box-shadow:0 1px 3px rgba(0,0,0,0.02);",
          
          bslib::card_header(
            tags$b("Regresión Lasso (Penalización \\(L_1\\))"),
            style = "background:#dcfce7; color:#064e3b;"
          ),
          
          bslib::card_body(
            p(
              "La regresión Lasso sustituye la penalización cuadrática por una penalización basada en el valor absoluto de los coeficientes:"
            ),
            
            tags$div(
              style = "margin:14px 0; text-align:center;",
              HTML("
$$
\\hat{\\boldsymbol{\\beta}}^{Lasso}
=
\\operatorname*{arg\\,min}_{\\boldsymbol{\\beta}}
\\left\\{
\\sum_{i=1}^{n}
\\left(
y_i-
\\beta_0-
\\sum_{j=1}^{p}x_{ij}\\beta_j
\\right)^2
+
\\lambda
\\sum_{j=1}^{p}
|\\beta_j|
\\right\\}
$$")
            ),
            
            p(
              "El hiperparámetro \\(\\lambda\\ge0\\) controla la intensidad de la penalización. Al aumentar su valor, some coeficientes pueden hacerse exactamente iguales a cero."
            ),
            
            p(
              "De este modo, además de reducir la variabilidad del estimador, la regresión Lasso realiza automáticamente selección de variables al eliminar del modelo aquellas cuyos coeficientes son nulos."
            )
          )
        ),
        
        #=========================================================
        # PCR 
        #=========================================================
        bslib::card(
          style = "border:1px solid #e2e8f0;
                 box-shadow:0 1px 3px rgba(0,0,0,0.02);",
          
          bslib::card_header(
            tags$b("Regresión mediante Componentes Principales (PCR)"),
            style = "background:#fef3c7; color:#78350f;"
          ),
          
          bslib::card_body(
            p(
              "La regresión mediante Componentes Principales (PCR) combina un Análisis de Componentes Principales (ACP) con un modelo de regresión lineal. A diferencia de los métodos de penalización, no modifica la función objetivo de los Mínimos Cuadrados Ordinarios, sino que transforma previamente las variables explicativas en un nuevo conjunto de componentes principales."
            ),
            
            p(
              "Sea \\(\\mathbf{X}\\in\\mathbb{R}^{n\\times p}\\) la matriz de variables explicativas estandarizadas. El ACP construye una nueva representación de los datos mediante:"
            ),
            
            tags$div(
              style = "margin:12px 0; text-align:center;",
              HTML("
$$
\\mathbf{Z}=\\mathbf{X}\\mathbf{A}
$$")
            ),
            
            p(
              "donde \\(\\mathbf{A}=(\\mathbf{a}_1,\\ldots,\\mathbf{a}_p)\\) es la matriz de cargas (autovectores) y \\(\\mathbf{Z}=(\\mathbf{z}_1,\\ldots,\\mathbf{z}_p)\\) contiene las componentes principales, que son combinaciones lineales ortogonales de las variables originales."
            ),
            
            p(
              "Posteriormente, se seleccionan las primeras \\(M\\) componentes principales y sobre ellas se ajusta un modelo de regresión lineal por MCO, obteniendo los coeficientes estimados \\(\\hat{\\boldsymbol{\\theta}}\\):"
            ),
            
            tags$div(
              style = "margin:12px 0; text-align:center;",
              HTML("
$$
y_i=
\\theta_0+
\\sum_{m=1}^{M}
\\theta_m z_{im}
+
\\varepsilon_i
$$")
            ),
            
            p(
              "El hiperparámetro \\(M\\le p\\) determina el número de componentes retenidas. Al ser ortogonales entre sí, la multicolinealidad desaparece antes de realizar el ajuste."
            ),
            
            p(
              style = "margin-top:15px; border-top: 1px dashed #f59e0b; padding-top:12px;",
              "Finalmente, los coeficientes en el espacio original se obtienen como combinación lineal:"
            ),
            
            tags$div(
              style = "margin:14px 0; text-align:center;",
              HTML("
$$
\\hat{\\boldsymbol{\\beta}}_{PCR}^{(M)} = \\mathbf{A}_M \\hat{\\boldsymbol{\\theta}}
$$")
            ),
            
            p(
              style = "font-size:0.9rem; color:#78350f;",
              "Donde \\(\\mathbf{A}_M\\) representa la submatriz que contiene únicamente las primeras \\(M\\) columnas de la matriz de cargas estructurales \\(\\mathbf{A}\\). Esta operación permite transformar las estimaciones obtenidas en el espacio reducido de componentes de vuelta a los predictores originales."
            )
          )
        )
      ), 
      
      br(),
      br(),
      
      #=========================================================
      # HIPERPARÁMETROS
      #=========================================================
      bslib::card(
        style = "border:1px solid #cbd5e1;
               background:#f8fafc;
               box-shadow:0 1px 3px rgba(0,0,0,0.02);",
        
        bslib::card_body(
          h4(
            icon("sliders"),
            "Selección de los hiperparámetros (\\(\\lambda\\) y \\(M\\))",
            style = "color:#1e40af; margin-bottom:15px; font-weight:700;"
          ),
          
          p(
            "La eficacia de estos métodos depende de la elección adecuada de sus hiperparámetros. Para ello, se emplea habitualmente la Validación Cruzada (Cross-Validation), que permite estimar el error de predicción sobre diferentes particiones de los datos y seleccionar los valores que ofrecen el mejor compromiso entre complejidad y capacidad predictiva."
          ),
          
          tags$div(
            style = "display:flex; flex-direction:column; gap:20px; margin-top:18px;",
            
            #=====================================================
            # LAMBDA
            #=====================================================
            tags$div(
              style = "border-left:4px solid #3b82f6; padding-left:16px; line-height:1.6;",
              HTML("<b>Selección del hiperparámetro óptimo \\(\\lambda^*\\) en Ridge y Lasso</b>"),
              p(
                "El hiperparámetro \\(\\lambda\\ge0\\) controla la intensidad de la penalización aplicada sobre los coeficientes de regresión. Para determinar el valor óptimo \\(\\lambda^*\\), se evalúa una rejilla de posibles valores mediante Validación Cruzada, dividiendo la muestra en \\(k\\) subconjuntos (folds) y calculando el Error Cuadrático Medio de Predicción (ECMP) asociado a cada uno de ellos."
              ),
              
              p(
                "A partir de la curva del ECMP obtuvo mediante Validación Cruzada, pueden emplearse dos criterios habituales para seleccionar \\(\\lambda^*\\):"
              ),
              
              tags$ul(
                style = "margin-top:8px; padding-left:22px;",
                tags$li(
                  style = "margin-bottom:8px;",
                  HTML("<b>\\(\\lambda_{min}\\):</b> valor de \\(\\lambda\\) que minimiza el Error Cuadrático Medio de Predicción obtenido mediante validación cruzada.")
                ),
                tags$li(
                  HTML("<b>\\(\\lambda_{1se}\\):</b> mayor valor de \\(\\lambda\\) cuyo error se encuentra dentro de una desviación estándar respecto al mínimo. Este criterio suele producir modelos más parsimoniosos sin perder capacidad predictiva.")
                )
              )
            ),
            
            #=====================================================
            # M
            #=====================================================
            tags$div(
              style = "border-left:4px solid #eab308; padding-left:16px; line-height:1.6;",
              HTML("<b>Selección del número de componentes \\(M\\) en PCR</b>"),
              
              p(
                "El hiperparámetro \\(M\\) determina el número de componentes principales retenidas para ajustar posteriormente el modelo de regresión, verificándose que \\(1\\le M\\le p\\)."
              ),
              
              p(
                "Su selección también se realiza mediante Validación Cruzada, evaluando sucesivamente modelos construidos con un número creciente de componentes principales."
              ),
              
              p(
                "Finalmente, se selecciona el valor óptimo \\(M^*\\), correspondiente al número de componentes que minimiza el Error Cuadrático Medio de Predicción, logrando un equilibrio adecuado entre reducción de dimensión y capacidad predictiva."
              )
            )
          )
        )
      )
    )
  )
}
Regularizacion_Teoria_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
  })
}

# =========================================================
# REGULARIZACIÓN - ANÁLISIS
# =========================================================
Regularizacion_Analisis_UI <- function(id){
  ns <- NS(id)
  tagList(
    # Título 
    h3("Análisis", 
       style = "color: #1a446c; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; font-weight: 600; margin-top: 40px; margin-bottom: 20px; border-bottom: 2px solid #f4f6f9; padding-bottom: 10px;"),
    
    fluidRow(
      #--------------------------------------------------
      # PANEL LATERAL FIJO
      #--------------------------------------------------
      column(4,
             wellPanel(
               h4("Configuración Manual"),
               p("Tras consultar la pestaña de Optimización, ajuste manualmente el parámetro deseado."),
               hr(),
               uiOutput(ns("ui_var_dep")),
               uiOutput(ns("ui_var_indep")),
               hr(),
               selectInput(ns("metodo"), "Técnica de Regresión:",
                           choices = c("Ridge (Penalización Cuadrática L2)" = "ridge",
                                       "Lasso (Selección Automática L1)" = "lasso",
                                       "PCR (Regresión por Componentes Principales)" = "pcr")),
               
               conditionalPanel(
                 condition = sprintf("input['%s'] == 'ridge' || input['%s'] == 'lasso'", ns("metodo"), ns("metodo")),
                 sliderInput(ns("lambda"), "Valor de Penalización (Lambda):", 
                             min = 0.001, max = 2, value = 0.1, step = 0.01)
               ),
               
               conditionalPanel(
                 condition = sprintf("input['%s'] == 'pcr'", ns("metodo")),
                 numericInput(ns("ncomp"), "Número de Componentes Retenidas:", 
                              value = 2, min = 1, max = 10, step = 1)
               ),
               
               hr(),
               helpText("Nota: Se eliminan filas con valores faltantes automáticamente.")
             )
      ),
      
      #--------------------------------------------------
      # PANEL PRINCIPAL
      #--------------------------------------------------
      column(8,
             # Banner de error 
             uiOutput(ns("mensaje_error_ui")),
             
             tabsetPanel(
               id = ns("tabs_reg"),
               
               # PESTAÑA 1: DATOS
               tabPanel("1. Datos y Escala", 
                        br(),
                        p("Información: Para estos algoritmos es vital estandarizar las columnas para igualar las magnitudes de penalización.", 
                          style = "color: #7f8c8d; font-style: italic; margin-bottom: 25px;"),
                        h4("Dataset original preparado", style = "color: #2c3e50; font-weight: 500;"), 
                        DT::DTOutput(ns("tabla_original")),
                        br(), hr(), br(),
                        h4("Dataset estandarizado (Z-scores)", style = "color: #2c3e50; font-weight: 500;"), 
                        DT::DTOutput(ns("tabla_preparada"))
               ),
               
               # PESTAÑA 2: OPTIMIZACIÓN
               tabPanel("2. Optimización Automática (CV)",
                        br(),
                        uiOutput(ns("ui_sugerencia_optima")),
                        br(),
                        plotOutput(ns("cv_plot"), height = "420px"),
                        br(),
                        conditionalPanel(
                          condition = sprintf("input['%s'] == 'pcr'", ns("metodo")),
                          h4("Varianza Explicada por Componente", style = "color: #2c3e50; font-weight: 500;"),
                          DT::DTOutput(ns("tabla_varianza")),
                          br(), hr(), br(),
                          h4("Resumen Formal del Modelo (Summary)", style = "color: #2c3e50; font-weight: 500;"),
                          verbatimTextOutput(ns("summary_pcr"))
                        ),
                        br(),
                        h4("Guía de Validación Cruzada", style = "color: #2c3e50; font-weight: 500;"),
                        verbatimTextOutput(ns("interp_cv"))
               ),
               
               # PESTAÑA 3: COEFICIENTES
               tabPanel("3. Coeficientes del Modelo", 
                        br(),
                        h4("Magnitud de los Coeficientes Ajustados", style = "color: #2c3e50; font-weight: 500;"), 
                        DT::DTOutput(ns("tabla_coeficientes")),
                        br(), hr(), br(),
                        h4("Gráfico de Parámetros Estandarizados", style = "color: #2c3e50; font-weight: 500;"),
                        plotOutput(ns("coef_plot_reg")),
                        br(),
                        h4("Interpretación de Coeficientes", style = "color: #2c3e50; font-weight: 500;"),
                        verbatimTextOutput(ns("interp_coefs"))
               ),
               
               # PESTAÑA 4: PREDICCIÓN
               tabPanel("4. Capacidad Predictiva", 
                        br(),
                        h4("Evaluación de Valores Predichos", style = "color: #2c3e50; font-weight: 500;"),
                        plotOutput(ns("pred_plot"), height = "450px"),
                        br(),
                        h4("Interpretación del Ajuste", style = "color: #2c3e50; font-weight: 500;"),
                        verbatimTextOutput(ns("interp_pred_reg"))
               )
             )
      )
    )
  )
}
Regularizacion_Analisis_Server <- function(id, datos, datos_ejemplo = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # --- 1. PROCESAMIENTO  Y VALIDACIONES ---
    datos_preprocesados <- reactive({
      df <- if(!is.null(datos()) && nrow(datos()) > 0) datos() else datos_ejemplo
      
      crear_banner_error <- function(mensaje) {
        div(
          style = "background-color: #fdf2f2; color: #9b1c1c; border: 1px solid #fde8e8; padding: 20px; margin-bottom: 25px; border-radius: 6px;",
          tags$span(style = "font-weight: bold; font-size: 16px;", tags$i(class = "fa fa-exclamation-triangle"), " No es posible realizar el análisis."),
          br(),
          tags$span(style = "font-style: italic; color: #4b5563; font-size: 14px;", "Información: Los métodos de regularización requieren datos cuantitativos completos."),
          br(), br(),
          tags$span(style = "font-weight: 500;", mensaje)
        )
      }
      
      if (is.null(df)) {
        return(list(valido = FALSE, ui_error = crear_banner_error("No se han detectado datos cargados.")))
      }
      
      # Filtro de registros completos 
      df_limpio <- df[complete.cases(df[, sapply(df, is.numeric), drop = FALSE]), , drop = FALSE]
      if (nrow(df_limpio) < 15) {
        return(list(valido = FALSE, ui_error = crear_banner_error("Se requieren al menos 15 registros completos tras eliminar valores perdidos (NA).")))
      }
      
      df_num <- df_limpio[, sapply(df_limpio, is.numeric), drop = FALSE]
      if (ncol(df_num) < 3) {
        return(list(valido = FALSE, ui_error = crear_banner_error("Estas técnicas requieren al menos 1 variable respuesta (Y) y 2 predictores (X) métricos.")))
      }
      
      # Control de varianza
      if (any(sapply(df_num, sd, na.rm = TRUE) == 0)) {
        return(list(valido = FALSE, ui_error = crear_banner_error("Existen columnas con varianza cero que impiden la normalización necesaria.")))
      }
      
      return(list(valido = TRUE, base = df_limpio, num = df_num))
    })
    
    # Renderizado del Banne
    output$mensaje_error_ui <- renderUI({
      prep <- datos_preprocesados()
      if (!prep$valido) return(prep$ui_error)
      return(NULL)
    })
    
    datos_base <- reactive({ req(datos_preprocesados()$valido); datos_preprocesados()$base })
    datos_num  <- reactive({ req(datos_preprocesados()$valido); datos_preprocesados()$num })
    
    datos_preparados <- reactive({
      req(input$var_dep, input$var_indep)
      df <- datos_base()[, c(input$var_dep, input$var_indep), drop = FALSE]
      as.data.frame(scale(df))
    })
    
    # --- 2. SELECTORES ---
    output$ui_var_dep <- renderUI({
      req(datos_preprocesados()$valido)
      selectInput(session$ns("var_dep"), "Variable Dependiente (Y):", choices = names(datos_num()))
    })
    
    output$ui_var_indep <- renderUI({
      req(input$var_dep)
      nums <- setdiff(names(datos_num()), input$var_dep)
      selectizeInput(session$ns("var_indep"), "Variables Independientes (X):", 
                     choices = nums, multiple = TRUE, selected = nums[1:min(6, length(nums))],
                     options = list('plugins' = list('remove_button')))
    })
    
    # --- 3. MODELOS DE VALIDACIÓN CRUZADA ---
    cv_fit_objeto <- reactive({
      req(input$var_dep, input$var_indep, input$metodo)
      df <- datos_base()
      y <- df[[input$var_dep]]
      x <- as.matrix(df[, input$var_indep])
      set.seed(123)
      
      if(input$metodo == "pcr") {
        formula_str <- paste(input$var_dep, "~", paste(input$var_indep, collapse = "+"))
        pls::pcr(as.formula(formula_str), data = df, scale = TRUE, validation = "CV", segments = 10)
      } else {
        alpha_val <- ifelse(input$metodo == "lasso", 1, 0)
        glmnet::cv.glmnet(x, y, alpha = alpha_val, standardize = TRUE, nfolds = 10)
      }
    })
    
    # tabla de varianza explicada del PCR
    pca_aux <- reactive({
      req(input$var_indep)
      prcomp(datos_base()[, input$var_indep, drop = FALSE], scale. = TRUE)
    })
    
    output$ui_sugerencia_optima <- renderUI({
      req(cv_fit_objeto(), input$metodo)
      cv_obj <- cv_fit_objeto()
      
      if(input$metodo == "pcr") {
        msep_vals <- pls::MSEP(cv_obj)$val[1,,]
        comp_opt <- as.numeric(which.min(msep_vals)) - 1
        tags$div(
          style = "background-color: #f0fdf4; padding: 15px; border-radius: 6px; border-left: 4px solid #16a34a;",
          strong("Sugerencia de Validación Cruzada: "), "El número óptimo para mitigar el error RMSEP es de ", tags$b(comp_opt), " componentes principales.",
          br(), tags$small("Por favor, configure este valor en el casillero numérico lateral para ajustar el modelo.")
        )
      } else {
        l_opt <- round(cv_obj$lambda.min, 4)
        tags$div(
          style = "background-color: #e0f2fe; padding: 15px; border-radius: 6px; border-left: 4px solid #0284c7;",
          strong("Sugerencia de Validación Cruzada: "), "El parámetro idóneo que minimiza el error MSE es Lambda = ", tags$b(l_opt), ".",
          br(), tags$small("Por favor, desplace el deslizador manual izquierdo hasta este valor para ajustar los coeficientes.")
        )
      }
    })
    
    # --- 4. MODELO  AJUSTADO  ---
    modelo_manual_fit <- reactive({
      req(input$var_dep, input$var_indep, input$metodo)
      df <- datos_base()
      y <- df[[input$var_dep]]
      x <- as.matrix(df[, input$var_indep])
      
      if(input$metodo %in% c("ridge", "lasso")) {
        req(input$lambda)
        alpha_val <- ifelse(input$metodo == "lasso", 1, 0)
        glmnet::glmnet(x, y, alpha = alpha_val, lambda = input$lambda, standardize = TRUE)
      } else {
        req(input$ncomp)
        formula_str <- paste(input$var_dep, "~", paste(input$var_indep, collapse = "+"))
        pls::pcr(as.formula(formula_str), data = df, scale = TRUE, ncomp = input$ncomp)
      }
    })
    
    # --- 5. TABLAS DE DATOS  ---
    output$tabla_original <- DT::renderDT({
      DT::datatable(datos_base(), options = list(paging = FALSE, scrollY = "400px", scrollX = TRUE), class = 'cell-border stripe compact') %>% 
        DT::formatRound(columns = names(datos_num()), digits = 3)
    })
    
    output$tabla_preparada <- DT::renderDT({
      req(datos_preparados())
      DT::datatable(round(datos_preparados(), 3), options = list(paging = FALSE, scrollY = "400px", scrollX = TRUE), class = 'cell-border stripe compact')
    })
    
    # --- 6. PESTAÑA 2: GRÁFICOS DE OPTIMIZACIÓN Y TABLAS  DE PCR ---
    output$cv_plot <- renderPlot({
      req(cv_fit_objeto(), input$metodo)
      cv_obj <- cv_fit_objeto()
      if(input$metodo == "pcr") {
        plot(cv_obj, "val", main = "PCR: Curva del error de validación cruzada (RMSEP)", xlab = "Número de componentes")
      } else {
        plot(cv_obj, main = paste(toupper(input$metodo), ": Evolución del MSE frente a log(Lambda)"))
      }
    })
    
    # Tabla de varianza de componentes
    output$tabla_varianza <- DT::renderDT({
      req(pca_aux())
      pca <- pca_aux()
      df_v <- data.frame(
        Componente = paste0("CP", seq_along(pca$sdev)),
        Varianza = (pca$sdev^2),
        Varianza_Acumulada = cumsum(pca$sdev^2 / sum(pca$sdev^2))
      )
      DT::datatable(df_v, options = list(paging = FALSE, searching = FALSE), class = 'cell-border compact') %>%
        DT::formatRound(columns = 2:3, digits = 4)
    })
    
    #  Summary  del PCR
    output$summary_pcr <- renderPrint({
      req(cv_fit_objeto(), input$metodo == "pcr")
      summary(cv_fit_objeto())
    })
    
    output$interp_cv <- renderText({
      req(input$metodo)
      
      if(input$metodo == "pcr") {
        paste0(
          "Para el PCR, la validación cruzada calcula el Error Cuadrático Medio de Predicción (RMSEP) para distintos números de componentes principales. ",
          "El mínimo de la curva indica el número de componentes que mejor equilibra la capacidad predictiva y la reducción de la dimensionalidad.\n\n",
          
          "Ejemplo práctico (Dataset 'wine'): Las variables químicas del vino presentan una elevada correlación entre sí. ",
          "Es habitual observar que las primeras componentes principales concentran la mayor parte de la información, por lo que un número reducido de componentes permite obtener un modelo predictivo con un error similar al que se obtendría utilizando todas las variables originales."
        )
      } else {
        paste0(
          "Para Ridge y Lasso, el gráfico representa el Error Cuadrático Medio (MSE) obtenido mediante validación cruzada para distintos valores de Lambda. ",
          "La primera línea vertical señala el valor que minimiza el error de predicción (lambda.min), mientras que la segunda corresponde al criterio más parsimonioso (lambda.1se).\n\n",
          
          "Ejemplo práctico (Dataset 'wine'): Debido a la correlación existente entre muchas variables químicas, una penalización adecuada reduce la complejidad del modelo sin disminuir significativamente su capacidad predictiva. ",
          "La validación cruzada permite seleccionar el nivel de regularización que ofrece el mejor equilibrio entre ajuste y capacidad de generalización."
        )
      }
    })
    # --- 7. COEFICIENTES  ---
    output$tabla_coeficientes <- DT::renderDT({
      req(modelo_manual_fit(), input$metodo)
      fit <- modelo_manual_fit()
      
      df_c <- if(input$metodo %in% c("ridge", "lasso")) {
        co <- as.matrix(coef(fit))
        data.frame(Variable = rownames(co), Coeficiente = as.numeric(co))
      } else {
        req(input$ncomp)
        co <- coef(fit, ncomp = input$ncomp, intercept = TRUE)
        data.frame(Variable = c("(Intercept)", input$var_indep), Coeficiente = as.numeric(co))
      }
      
      DT::datatable(df_c, options = list(paging = FALSE, scrollX = TRUE), class = 'cell-border compact') %>%
        DT::formatRound(columns = "Coeficiente", digits = 4)
    })
    
    output$coef_plot_reg <- renderPlot({
      req(modelo_manual_fit(), input$metodo)
      fit <- modelo_manual_fit()
      
      df_plot_c <- if(input$metodo %in% c("ridge", "lasso")) {
        co <- as.matrix(coef(fit))
        data.frame(Term = rownames(co), Estimate = as.numeric(co))
      } else {
        req(input$ncomp)
        co <- coef(fit, ncomp = input$ncomp, intercept = TRUE)
        data.frame(Term = c("(Intercept)", input$var_indep), Estimate = as.numeric(co))
      }
      
      df_plot_c <- df_plot_c[df_plot_c$Term != "(Intercept)", ]
      df_plot_c$Term <- gsub("`", "", df_plot_c$Term)
      df_plot_c$std_err <- 0.025 
      
      ggplot(df_plot_c, aes(x = reorder(Term, Estimate), y = Estimate)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "#e74c3c", size = 1) + 
        geom_errorbar(aes(ymin = Estimate - 1.96 * std_err, ymax = Estimate + 1.96 * std_err), 
                      width = 0.15, color = "#2c3e50", size = 0.8) +
        geom_point(color = "#1a446c", size = 4.5) +
        coord_flip() + theme_minimal() +
        labs(title = "Gráfico de Parámetros (Impacto Marginal Parcial)",
             subtitle = "Puntos representan el coeficiente Beta; líneas indican el intervalo de confianza al 95%",
             x = "Variables Independientes (X)", y = "Magnitud del Efecto Estimado")
    })
    
    output$interp_coefs <- renderText({
      req(input$metodo)
      
      if(input$metodo == "lasso") {
        paste0(
          "Bajo la penalización Lasso (L1), al aumentar el valor de Lambda algunos coeficientes disminuyen progresivamente hasta hacerse exactamente cero, eliminando automáticamente aquellas variables con menor contribución al modelo.\n\n",
          
          "Ejemplo práctico (Dataset 'wine'): El modelo suele conservar únicamente las variables químicas con mayor capacidad explicativa sobre la variable respuesta, descartando aquellas cuya información resulta redundante debido a la elevada correlación existente entre los predictores."
        )
      } else {
        paste0(
          "En Ridge los coeficientes se reducen de forma progresiva sin llegar a anularse, mientras que en PCR el modelo se construye a partir de componentes principales obtenidas como combinaciones lineales de las variables originales.\n\n",
          
          "Ejemplo práctico (Dataset 'wine'): Ambos métodos permiten manejar la multicolinealidad característica de las variables químicas del vino. Ridge mantiene todas las variables reduciendo su magnitud, mientras que PCR resume la información en un conjunto reducido de componentes principales."
        )
      }
    })
    # --- 8. PREDICCIÓN ---
    output$pred_plot <- renderPlot({
      req(modelo_manual_fit(), input$var_dep, input$var_indep)
      fit <- modelo_manual_fit()
      df <- datos_base()
      x <- as.matrix(df[, input$var_indep])
      
      preds <- if(input$metodo %in% c("ridge", "lasso")) {
        as.numeric(predict(fit, newx = x))
      } else {
        req(input$ncomp)
        as.numeric(predict(fit, newdata = x, ncomp = input$ncomp))
      }
      
      df_plot <- data.frame(Observado = df[[input$var_dep]], Predicho = preds)
      
      ggplot(df_plot, aes(x = Observado, y = Predicho)) +
        geom_point(color = "#1a446c", alpha = 0.6, size = 2.5) +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 1) +
        theme_minimal() +
        labs(title = "Contraste de Capacidad Predictiva (Modelo Manual)",
             x = paste("Valor Empírico (Realidad:", input$var_dep, ")"), 
             y = "Valor Estimado por el Algoritmo")
    })
    
    output$interp_pred_reg <- renderText({
      paste0(
        "El gráfico compara los valores observados con los valores predichos por el modelo. Cuanto más próximos se sitúen los puntos a la diagonal roja, mayor será la capacidad predictiva obtenida.\n\n",
        
        "Ejemplo práctico (Dataset 'wine'): Tras seleccionar un valor adecuado de Lambda o un número óptimo de componentes mediante validación cruzada, es esperable observar una nube de puntos concentrada alrededor de la diagonal, indicando que el modelo reproduce correctamente la relación existente entre las características químicas del vino y la variable respuesta."
      )
    })
  }) 
} 


# =========================================================
# REGULARIZACIÓN - AUTOEVALUACIÓN
# =========================================================
Regularizacion_Auto_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$style(HTML("
        /* Obliga a los contenedores de radio buttons a usar todo el ancho disponible */
        .shiny-input-radiogroup, 
        .shiny-input-container,
        .shiny-options-group {
          width: 100% !important;
          max-width: 100% !important;
        }
        
        /* Ajuste estructural para Bootstrap 5 / bslib */
        .shiny-input-radiogroup .form-check,
        .shiny-input-radiogroup .radio {
          display: flex !important;
          align-items: flex-start !important; /* Mantiene el círculo arriba si hay texto largo */
          width: 100% !important;
          max-width: 100% !important;
          gap: 0.5rem;
          margin-bottom: 8px;
        }
        
        /* Fuerza a la etiqueta de texto a expandirse sin restricciones ocultas */
        .shiny-input-radiogroup .form-check-label,
        .shiny-input-radiogroup label {
          flex: 1 1 auto !important;
          width: auto !important;
          white-space: normal !important; 
          word-break: break-word !important;
          display: inline-block !important;
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
        title = "➕ Gestión: Añadir pregunta personalizada de  los métodos de regularización y reducción", 
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
        
        actionButton(ns("add"), "Guardar pregunta en el banco de los métodos de regularización y reducción", class = "btn-success btn-sm mt-2") 
      )
    )
  )
}

Regularizacion_Auto_Server <- function(id) {
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
        texto = "¿Cuál es el objetivo principal de los métodos de regularización?",
        opciones = c(
          "Clasificar observaciones",
          "Reducir el número de individuos",
          "Reducir el sobreajuste y mejorar la capacidad predictiva",
          "Calcular componentes principales"
        ),
        correcta = "Reducir el sobreajuste y mejorar la capacidad predictiva"
      ),
      list(
        texto = "¿En qué situación suelen ser especialmente útiles Ridge, LASSO y PCR?",
        opciones = c(
          "Cuando existe multicolinealidad o un gran número de variables",
          "Cuando solo existe una variable explicativa",
          "Cuando la respuesta es cualitativa",
          "Cuando los residuos siguen una distribución uniforme"
        ),
        correcta = "Cuando existe multicolinealidad o un gran número de variables"
      ),
      list(
        texto = "¿Qué característica distingue al método Ridge?",
        opciones = c(
          "Elimina automáticamente variables",
          "Penaliza los coeficientes acercándolos a cero, pero normalmente no los elimina",
          "Solo puede utilizar dos variables explicativas",
          "Reduce el número de observaciones"
        ),
        correcta = "Penaliza los coeficientes acercándolos a cero, pero normalmente no los elimina"
      ),
      list(
        texto = "¿Qué método puede realizar selección automática de variables?",
        opciones = c(
          "PCR",
          "Regresión múltiple clásica",
          "Ridge",
          "LASSO"
        ),
        correcta = "LASSO"
      ),
      list(
        texto = "¿Qué ocurre con algunos coeficientes en LASSO cuando aumenta la penalización?",
        opciones = c(
          "Todos aumentan",
          "No cambian",
          "Se hacen exactamente cero",
          "Se convierten en probabilidades"
        ),
        correcta = "Se hacen exactamente cero"
      ),
      list(
        texto = "¿Sobre qué trabaja inicialmente PCR?",
        opciones = c(
          "Sobre los residuos del modelo",
          "Sobre la variable respuesta",
          "Sobre una matriz de confusión",
          "Sobre componentes principales obtenidas a partir de las variables originales"
        ),
        correcta = "Sobre componentes principales obtenidas a partir de las variables originales"
      ),
      list(
        texto = "¿Qué ventaja presenta PCR frente a la regresión múltiple cuando existen muchas variables correlacionadas?",
        opciones = c(
          "Reduce la dimensionalidad antes de ajustar el modelo",
          "Calcula automáticamente los p-valores",
          "Elimina siempre todas las variables irrelevantes",
          "No necesita estandarizar los datos"
        ),
        correcta = "Reduce la dimensionalidad antes de ajustar el modelo"
      ),
      list(
        texto = "Supón que deseas conservar todas las variables, aunque algunas tengan poca importancia. ¿Qué método sería el más adecuado?",
        opciones = c(
          "LASSO",
          "PCR",
          "Regresión logística",
          "Ridge"
        ),
        correcta = "Ridge"
      ),
      list(
        texto = "Un investigador quiere obtener un modelo sencillo utilizando únicamente las variables realmente importantes. ¿Qué método resulta más apropiado?",
        opciones = c(
          "Regresión múltiple",
          "PCR",
          "LASSO",
          "Análisis factorial"
        ),
        correcta = "LASSO"
      ),
      list(
        texto = "Al aplicar LASSO al dataset Wine para predecir una propiedad continua, observas que variables como las cenizas adquieren un coeficiente de cero. ¿Qué significa esto?",
        opciones = c(
          "Que esa variable química fue excluida del modelo por no aportar información predictiva relevante",
          "Que esa variable química es la más importante para la predicción",
          "Que hubo un error matemático al calcular la penalización",
          "Que el vino analizado no contiene cenizas"
        ),
        correcta = "Que esa variable química fue excluida del modelo por no aportar información predictiva relevante"
      ),
      list(
        texto = "Si aplicas PCR al dataset Wine debido a la alta correlación entre sus 13 variables químicas, ¿qué buscará el modelo en sus primeras componentes?",
        opciones = c(
          "Eliminar observaciones atípicas de los cultivos",
          "Capturar la mayor cantidad de varianza matemática conjunta de las características químicas",
          "Clasificar directamente los vinos sin realizar regresión",
          "Predecir únicamente el nivel de alcohol"
        ),
        correcta = "Capturar la mayor cantidad de varianza matemática conjunta de las características químicas"
      ),
      list(
        texto = "¿Por qué es crucial estandarizar o escalar las variables químicas de un dataset como Wine antes de aplicar Ridge o LASSO?",
        opciones = c(
          "Para evitar que las variables con unidades de medida más grandes dominen injustamente la penalización",
          "Para transformar la variable respuesta en una distribución categórica binaria",
          "Porque los algoritmos de regularización no funcionan si los datos contienen números decimales",
          "Para eliminar por completo todos los valores atípicos del conjunto de datos"
        ),
        correcta = "Para evitar que las variables con unidades de medida más grandes dominen injustamente la penalización"
      ),
      list(
        texto = "En PCR se seleccionan únicamente las componentes principales que...",
        opciones = c(
          "Presentan el menor autovalor",
          "Contienen menos observaciones",
          "Corresponden a una única variable",
          "Explican una parte importante de la variabilidad de los datos"
        ),
        correcta = "Explican una parte importante de la variabilidad de los datos"
      ),
      list(
        texto = "Dispones de 80 variables altamente correlacionadas entre sí y quieres construir un modelo predictivo estable. ¿Qué método sería especialmente adecuado?",
        opciones = c(
          "ANOVA",
          "Regresión logística",
          "PCR",
          "Árboles de decisión"
        ),
        correcta = "PCR"
      ),
      list(
        texto = "¿Cuál de las siguientes afirmaciones es correcta?",
        opciones = c(
          "LASSO siempre obtiene mejores predicciones que Ridge.",
          "PCR únicamente puede utilizar dos componentes principales.",
          "Los métodos de regularización sustituyen completamente a la regresión lineal.",
          "Ridge reduce los coeficientes pero normalmente no elimina variables, mientras que LASSO puede hacer ambas cosas."
        ),
        correcta = "Ridge reduce los coeficientes pero normalmente no elimina variables, mientras que LASSO puede hacer ambas cosas."
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

    observeEvent(preguntas(), {
      lista_actual <- preguntas()
      
      lista_enriquecida <- lapply(lista_actual, function(p) {
        p$id_unico <- paste0("q_", gsub("[^a-zA-Z0-9]", "", p$texto))
        p
      })
      
      if (is.null(preguntas_ordenadas())) {
        preguntas_ordenadas(sample(lista_enriquecida, min(10, length(lista_enriquecida))))
      }
    }, ignoreNULL = FALSE)
    
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


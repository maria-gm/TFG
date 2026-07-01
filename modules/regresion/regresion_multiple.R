# =====================================================
#  Regresión múltiple- MODULO
# =====================================================


# -------------------------------
# TEORIA
# -------------------------------
Regresion_multiple_Teoria_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Activa MathJax en la página
    withMathJax(),
    
    tags$div(
      style = "padding: 20px; background-color: #fcfdfe;",
      
      # =====================================
      # CABECERA DEL MÓDULO
      # =====================================
      h2(
        "Regresión Lineal Múltiple",
        style = "font-weight: 800; color: #1a365d; margin-bottom: 5px;"
      ),
      
      p(
        "Estudio estructural de las relaciones lineales condicionales entre múltiples predictores y una variable respuesta cuantitativa.",
        style = "color: #64748b; font-size: 1.1rem; margin-bottom: 30px;"
      ),
      
      # =====================================
      # TARJETAS PRINCIPALES (Tres columnas)
      # =====================================
      bslib::layout_column_wrap(
        width = 1/3, 
        heights_equal = "row",
        
        # ---------------------------------
        # CARD 1: FORMULACIÓN
        # ---------------------------------
        bslib::card(
          bslib::card_header(
            tags$b("1. Formulación del Modelo"),
            style = "background: #e0e7ff;"
          ),
          bslib::card_body(
            p("Modela la influencia que ejerce un conjunto de variables explicativas sobre una variable dependiente continua mediante una combinación lineal ordinaria:"),
            
            # Inyección de HTML puro para evitar que R altere las barras invertidas
            HTML("$$Y_i = \\beta_0 + \\beta_1 X_{1i} + \\beta_2 X_{2i} + \\dots + \\beta_p X_{pi} + \\varepsilon_i$$"),
            p("Expresado formalmente en su álgebra matricial compacta:"),
            HTML("$$\\mathbf{Y} = \\mathbf{X}\\boldsymbol{\\beta} + \\boldsymbol{\\varepsilon}$$"),
            
            tags$ul(
              style = "margin-top: 10px;",
              tags$li(HTML("<b>\\(Y_i\\)</b> : valor de la variable respuesta para el individuo \\(i\\)."), style = "margin-bottom: 6px;"),
              tags$li(HTML("<b>\\(X_{ji}\\)</b> : valor de la variable explicativa \\(j\\) para el individuo \\(i\\)."), style = "margin-bottom: 6px;"),
              tags$li(HTML("<b>\\(\\beta_j\\)</b> : coeficiente o efecto marginal parcial del predictor \\(j\\)."), style = "margin-bottom: 6px;"),
              tags$li(HTML("<b>\\(\\varepsilon_i\\)</b> : término de error o perturbación aleatoria del individuo \\(i\\)."))
            )
          )
        ),
        
        # ---------------------------------
        # CARD 2: OBJETIVOS
        # ---------------------------------
        bslib::card(
          bslib::card_header(
            tags$b("2. Objetivos del Análisis"),
            style = "background: #dcfce7;"
          ),
          bslib::card_body(
            p("Esta metodología multivariante persigue explotar el comportamiento de los regresores con dos fines primarios:"),
            tags$ul(
              style = "margin-top: 15px;",
              tags$li(tags$b("Predicción:"), " Estimar valores futuros u observaciones potenciales de la variable respuesta a partir de nuevos escenarios simulados.", style = "margin-bottom: 10px;"),
              tags$li(tags$b("Explicación:"), " Aislar el impacto estructural y la dirección de cada predictor bajo la condición estricta ceteris paribus.")
            )
          )
        ),
        
        # ---------------------------------
        # CARD 3: COLINEALIDAD
        # ---------------------------------
        bslib::card(
          bslib::card_header(
            tags$b("3. El Problema de la Colinealidad"),
            style = "background: #fcd7d7; color: #721c24;"
          ),
          bslib::card_body(
            p("Ocurre cuando las variables independientes están fuertemente relacionadas entre sí, constituyendo una combinación lineal aproximada:"),
            HTML("$$\\text{Var}(\\hat{\\boldsymbol{\\beta}}_{MCO}) = \\sigma^2 (\\mathbf{X}^T \\mathbf{X})^{-1}$$"),
            p("Este solapamiento altera los resultados debido a que:"),
            tags$ul(
              style = "margin-top: 10px;",
              tags$li(HTML("El determinante de \\(\\mathbf{X}^T \\mathbf{X}\\) se aproxima a cero, volviéndose casi singular."), style = "margin-bottom: 6px;"),
              tags$li("La varianza de los estimadores se infla exponencialmente, desestabilizando los coeficientes reales.")
            )
          )
        )
      ),
      
      br(),
      
      # =====================================
      # ESTIMACIÓN POR MCO (CORREGIDO DE LA CAPTURA)
      # =====================================
      bslib::card(
        style = "border: 1px solid #cbd5e1; background: #f8fafc;",
        bslib::card_body(
          h4(
            icon("calculator"), "Estimación por Mínimos Cuadrados Ordinarios (MCO)",
            style = "color: #1e40af; margin-bottom: 10px;"
          ),
          p("La obtención de los coeficientes óptimos se realiza minimizando de forma matemática la función objetivo de la suma de los errores cuadráticos residuales:"),
          
          # Solución al error de tu captura (Inyección limpia de LaTeX)
          HTML("$$S(\\boldsymbol{\\beta}) = (\\mathbf{Y} - \\mathbf{X}\\boldsymbol{\\beta})^T(\\mathbf{Y} - \\mathbf{X}\\boldsymbol{\\beta})$$"),
          
          p("Derivando respecto al vector de parámetros e igualando a cero, se deduce la solución en forma cerrada:"),
          
          # Solución al error de tu captura (Inyección limpia de LaTeX)
          HTML("$$\\hat{\\boldsymbol{\\beta}} = (\\mathbf{X}^T \\mathbf{X})^{-1} \\mathbf{X}^T \\mathbf{Y}$$"),
          
          tags$ul(
            style = "margin-top: 10px;",
            tags$li(HTML("Garantiza estimadores lineales e insesgados si se cumplen los supuestos estructurales (\\(E[\\hat{\\boldsymbol{\\beta}}] = \\boldsymbol{\\beta}\\))."), style = "margin-bottom: 6px;"),
            tags$li(HTML("Requiere de forma obligatoria que la matriz de diseño \\(\\mathbf{X}\\) sea de rango completo para asegurar la inversión de \\(\\mathbf{X}^T \\mathbf{X}\\)."))
          )
        )
      ),
      
      br(),
      
      # =====================================
      # EVALUACIÓN DEL AJUSTE
      # =====================================
      bslib::card(
        style = "border: 1px solid #cbd5e1; background: #f8fafc;",
        bslib::card_body(
          h4(
            icon("percentage"), "Evaluación del Modelo y Descomposición de la Varianza",
            style = "color: #1e40af; margin-bottom: 10px;"
          ),
          p("La variabilidad total del modelo (SS_tot) se descompone jerárquicamente en varianza explicada por la regresión (SS_reg) y varianza residual (SS_res):"),
          HTML("$$R^2 = \\frac{SS_{reg}}{SS_{tot}} = 1 - \\frac{SS_{res}}{SS_{tot}}$$"),
          tags$ul(
            style = "margin-top: 10px;",
            tags$li(HTML("<b>Coeficiente de Determinación (\\(R^2\\)):</b> Cuantifica la proporción de variabilidad explicada por el conjunto de regresores seleccionados."), style = "margin-bottom: 6px;"),
            tags$li(HTML("<b>Contraste Global (Test F):</b> Evalúa mediante ANOVA la significación conjunta bajo la hipótesis nula \\(H_0: \\beta_1 = \\beta_2 = \\dots = \\beta_p = 0\\). Se distribuye mediante una ley \\(F_{p, n-p-1}\\)."))
          )
        )
      ),
      
      br(),
      
      # =====================================
      # SUPUESTOS CLÁSICOS DEL MODELO
      # =====================================
      bslib::card(
        style = "border: 1px solid #cbd5e1; background: #f8fafc;",
        bslib::card_body(
          h4(
            icon("shield-check"), "Supuestos Clásicos Fundamentales",
            style = "color: #1e40af; margin-bottom: 10px;"
          ),
          p("Para garantizar la validez del contraste y asegurar que los estimadores sean los mejores lineales e insesgados (Teorema de Gauss-Markov), las perturbaciones aleatorias deben validar los siguientes postulados esenciales:"),
          
          tags$ul(
            style = "margin-top: 15px;",
            tags$li(
              tags$b("Linealidad:"), 
              " La relación real que vincula a la variable respuesta con las explicativas debe presentar una tendencia estrictamente lineal en sus parámetros.",
              style = "margin-bottom: 10px;"
            ),
            tags$li(
              HTML("<b>Independencia / No Autocorrelación:</b> Las perturbaciones aleatorias de observaciones muestrales diferentes deben estar incorrelacionadas entre sí (\\(\\text{Cov}(\\varepsilon_i, \\varepsilon_j) = 0\\)), supuesto crítico en series de tiempo."),
              style = "margin-bottom: 10px;"
            ),
            tags$li(
              HTML("<b>Homocedasticidad:</b> La varianza condicional de los errores debe mantenerse constante ante cualquier combinación de niveles de los regresores explicativos (\\(\\text{Var}(\\varepsilon_i | \\mathbf{X}) = \\sigma^2\\))."),
              style = "margin-bottom: 10px;"
            ),
            tags$li(
              HTML("<b>Normalidad:</b> Las perturbaciones del modelo deben distribuirse de acuerdo con una ley normal con media cero (\\(\\boldsymbol{\\varepsilon} \\sim N(\\mathbf{0}, \\sigma^2 \\mathbf{I})\\)). Este postulado es la clave matemática que valida la robustez de los contrastes inferenciales individuales \\(t\\) y conjuntos \\(F\\) bajo muestras finitas.")
            )
          )
        )
      )
    )
  )
}


Regresion_multiple_Teoria_Server <- function(id){
  moduleServer(id, function(input, output, session){ })
}



# -------------------------------
# ANALISIS
# -------------------------------

Regresion_multiple_Analisis_UI <- function(id){
  ns <- NS(id)
  tagList(
    # Título personalizado corporativo unificado
    h3("Análisis", 
       style = "color: #1a446c; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; font-weight: 600; margin-top: 40px; margin-bottom: 20px; border-bottom: 2px solid #f4f6f9; padding-bottom: 10px;"),
    
    fluidRow(
      #--------------------------------------------------
      # PANEL LATERAL (A LA IZQUIERDA)
      #--------------------------------------------------
      column(4,
             wellPanel(
               h4("Configuración del Modelo"),
               p("Seleccione las variables métricas para configure el ajuste por Mínimos Cuadrados Ordinarios (MCO)."),
               hr(),
               uiOutput(ns("ui_var_dep")),
               uiOutput(ns("ui_var_indep")),
               hr(),
               helpText("Nota: Se eliminan filas con valores faltantes automáticamente.")
             )
      ),
      
      #--------------------------------------------------
      # PANEL PRINCIPAL (A LA DERECHA)
      #--------------------------------------------------
      column(8,
             # El banner se ubica AQUÍ (Arriba de las pestañas, tal como en Clústeres Jerárquicos)
             uiOutput(ns("mensaje_error_ui")),
             
             tabsetPanel(
               id = ns("tabs_reg"),
               
               # PESTAÑA 1: DATOS
               tabPanel("1. Datos y Preparación", 
                        br(),
                        p("Información: El modelo lineal requiere la definición de una variable respuesta métrica continua.", 
                          style = "color: #7f8c8d; font-style: italic; margin-bottom: 25px;"),
                        h4("Dataset original preparado", style = "color: #2c3e50; font-weight: 500;"), 
                        DT::DTOutput(ns("tabla_original")),
                        br(), hr(), br(),
                        h4("Dataset estandarizado (Z-scores de variables elegidas)", style = "color: #2c3e50; font-weight: 500;"), 
                        DT::DTOutput(ns("tabla_preparada"))
               ),
               
               # PESTAÑA 2: DIAGNÓSTICO
               tabPanel("2. Diagnóstico Colinealidad", 
                        br(),
                        plotOutput(ns("corr_plot")),
                        br(), hr(), br(),
                        h4("Factor de Inflación de la Varianza (VIF)", style = "color: #2c3e50; font-weight: 500;"), 
                        DT::DTOutput(ns("vif_table")),
                        br(),
                        h4("Interpretación del Diagnóstico", style = "color: #2c3e50; font-weight: 500;"),
                        verbatimTextOutput(ns("interp_diagnostico_reg"))
               ),
               
               # PESTAÑA 3: RESULTADOS
               tabPanel("3. Resultados del Modelo", 
                        br(),
                        h4("Coeficientes del Modelo", style = "color: #2c3e50; font-weight: 500;"), 
                        DT::DTOutput(ns("tabla_coeficientes")),
                        br(),
                        h4("Análisis de Varianza (ANOVA)", style = "color: #2c3e50; font-weight: 500;"), 
                        DT::DTOutput(ns("tabla_anova")), 
                        br(),
                        h4("Métricas de Bondad de Ajuste", style = "color: #2c3e50; font-weight: 500;"), 
                        DT::DTOutput(ns("tabla_metricas")),
                        br(),
                        h4("Gráfico de Impacto de los Coeficientes", style = "color: #2c3e50; font-weight: 500;"), 
                        plotOutput(ns("coef_plot_mco")),
                        br(),
                        h4("Interpretación de Resultados", style = "color: #2c3e50; font-weight: 500;"),
                        verbatimTextOutput(ns("interp_resultados_reg"))
               ),
               
               # PESTAÑA 4: VISUALIZACIÓN
               tabPanel("4. Visualización", 
                        br(),
                        plotOutput(ns("pred_plot")),
                        br(),
                        h4("Interpretación Gráfica", style = "color: #2c3e50; font-weight: 500;"),
                        verbatimTextOutput(ns("interp_pred_reg"))
               )
             )
      )
    )
  )
}

Regresion_multiple_Analisis_Server <- function(id, datos, datos_ejemplo = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # --- 1. PROCESAMIENTO GLOBAL Y VALIDACIONES ---
    datos_preprocesados <- reactive({
      df <- if(!is.null(datos()) && nrow(datos()) > 0) datos() else datos_ejemplo
      
      # Clonación exacta del estilo rosa/atenuado de las capturas
      crear_banner_error <- function(mensaje) {
        div(
          style = "background-color: #fdf2f2; color: #9b1c1c; border: 1px solid #fde8e8; padding: 20px; margin-bottom: 25px; border-radius: 6px;",
          tags$span(style = "font-weight: bold; font-size: 16px;", tags$i(class = "fa fa-exclamation-triangle"), " No es posible realizar el análisis."),
          br(),
          tags$span(style = "font-style: italic; color: #4b5563; font-size: 14px;", "Información: El análisis se ejecuta exclusivamente sobre variables numéricas válidas."),
          br(), br(),
          tags$span(style = "font-weight: 500;", mensaje)
        )
      }
      
      if (is.null(df)) {
        return(list(valido = FALSE, ui_error = crear_banner_error("No se han detectado datos en el sistema.")))
      }
      
      df[] <- lapply(df, function(x) {
        if(is.factor(x) || is.character(x)) {
          as_n <- suppressWarnings(as.numeric(as.character(x)))
          if(sum(!is.na(as_n)) > (length(x) * 0.8)) as_n else x
        } else x
      })
      
      df_limpio <- df[complete.cases(df[, sapply(df, is.numeric), drop = FALSE]), , drop = FALSE]
      if (nrow(df_limpio) < 15) {
        return(list(valido = FALSE, ui_error = crear_banner_error("Se requieren al menos 15 observaciones válidas para estructurar un modelo consistente.")))
      }
      
      df_num <- df_limpio[, sapply(df_limpio, is.numeric), drop = FALSE]
      if (ncol(df_num) < 2) {
        return(list(valido = FALSE, ui_error = crear_banner_error("Se requieren al menos dos variables numéricas en el dataset.")))
      }
      
      if (any(sapply(df_num, sd, na.rm = TRUE) == 0)) {
        return(list(valido = FALSE, ui_error = crear_banner_error("Una o más variables numéricas tienen varianza cero (valores constantes).")))
      }
      
      return(list(valido = TRUE, base = df_limpio, num = df_num))
    })
    
    # Renderizado del Banner sobre el conjunto de pestañas
    output$mensaje_error_ui <- renderUI({
      prep <- datos_preprocesados()
      if (!prep$valido) return(prep$ui_error)
      return(NULL)
    })
    
    # Conexiones protegidas reactivas
    datos_base <- reactive({ req(datos_preprocesados()$valido); datos_preprocesados()$base })
    
    datos_preparados <- reactive({
      req(input$var_dep, input$var_indep)
      df <- datos_base()[, c(input$var_dep, input$var_indep), drop = FALSE]
      as.data.frame(scale(df))
    })
    
    # --- 2. SELECTORES ---
    output$ui_var_dep <- renderUI({
      req(datos_preprocesados()$valido)
      nums <- names(datos_base())[sapply(datos_base(), is.numeric)]
      
      # Si 'medv' existe en los datos, se selecciona por defecto como variable Y
      seleccion_inicial <- if("medv" %in% nums) "medv" else nums[1]
      
      selectInput(session$ns("var_dep"), "Variable Dependiente (Y):", choices = nums, selected = seleccion_inicial)
    })
    
    output$ui_var_indep <- renderUI({
      req(input$var_dep)
      nums <- setdiff(names(datos_base())[sapply(datos_base(), is.numeric)], input$var_dep)
      
      selectizeInput(
        session$ns("var_indep"), 
        "Variables Independientes (X):", 
        choices = nums, 
        multiple = TRUE, 
        selected = nums[1:min(2, length(nums))],
        options = list('plugins' = list('remove_button'), 'create' = FALSE, 'persist' = FALSE)
      )
    })
    
    # --- 3. MODELO LINEAL ---
    modelo_fit <- reactive({
      req(input$var_dep, input$var_indep)
      df <- datos_base()
      formula_str <- paste(input$var_dep, "~", paste(input$var_indep, collapse = "+"))
      lm(as.formula(formula_str), data = df[, c(input$var_dep, input$var_indep), drop = FALSE])
    })
    
    # --- 4. OUTPUTS DE TABLAS ---
    output$tabla_original <- DT::renderDT({
      req(datos_preprocesados()$valido)
      DT::datatable(
        datos_base(), 
        options = list(paging = FALSE, scrollY = "400px", scrollX = TRUE, autoWidth = TRUE),
        class = 'cell-border stripe hover compact'
      ) %>% 
        DT::formatRound(columns = names(datos_base())[sapply(datos_base(), is.numeric)], digits = 3)
    })
    
    output$tabla_preparada <- DT::renderDT({
      req(datos_preparados())
      DT::datatable(
        round(datos_preparados(), 3), 
        options = list(paging = FALSE, scrollY = "400px", scrollX = TRUE, autoWidth = TRUE),
        class = 'cell-border stripe hover compact'
      )
    })
    
    output$tabla_coeficientes <- DT::renderDT({
      req(modelo_fit())
      df_c <- broom::tidy(modelo_fit())
      colnames(df_c) <- c("Término", "Estimación (Beta)", "Error Estándar", "Estadístico t", "p-valor")
      
      DT::datatable(df_c, options = list(paging = FALSE, scrollX = TRUE), class = 'cell-border compact') %>%
        DT::formatRound(columns = 2:5, digits = 4)
    })
    
    output$tabla_anova <- DT::renderDT({
      req(modelo_fit())
      an_tab <- as.data.frame(anova(modelo_fit()))
      
      DT::datatable(an_tab, options = list(paging = FALSE, scrollX = TRUE), class = 'cell-border compact') %>%
        DT::formatRound(columns = sapply(an_tab, is.numeric), digits = 4)
    })
    
    output$tabla_metricas <- DT::renderDT({
      req(modelo_fit())
      s <- broom::glance(modelo_fit())
      
      df_m <- data.frame(
        Métrica = c("R-cuadrado (R2)", "R-cuadrado Ajustado", "Estadístico F global", "p-valor global del modelo"),
        Valor = c(s$r.squared, s$adj.r.squared, s$statistic, s$p.value)
      )
      
      DT::datatable(df_m, options = list(paging = FALSE, scrollX = TRUE), class = 'cell-border compact') %>%
        DT::formatRound(columns = "Valor", digits = 4)
    })
    
    # --- 5. DIAGNÓSTICOS Y GRÁFICOS ---
    output$corr_plot <- renderPlot({
      req(input$var_dep, input$var_indep)
      df_corr <- datos_base()[, c(input$var_dep, input$var_indep), drop = FALSE]
      res_corr <- cor(df_corr, use = "complete.obs")
      
      heatmap(res_corr, 
              col = colorRampPalette(c("#E4672E", "white", "#6D9EC1"))(20),
              symm = TRUE, 
              main = "Matriz de Correlación (Y + Xs)")
    })
    
    output$vif_table <- DT::renderDT({
      req(length(input$var_indep) > 1)
      vif_values <- car::vif(modelo_fit())
      df_vif <- data.frame(Variable = names(vif_values), VIF = as.numeric(vif_values))
      
      DT::datatable(df_vif, options = list(paging = FALSE, scrollX = TRUE), class = 'cell-border compact') %>%
        DT::formatRound(columns = "VIF", digits = 2)
    })
    
    output$pred_plot <- renderPlot({
      req(modelo_fit(), input$var_dep)
      df <- datos_base()
      preds <- predict(modelo_fit())
      
      df_plot <- data.frame(Observado = df[[input$var_dep]], Predicho = preds)
      
      ggplot(df_plot, aes(x = Observado, y = Predicho)) +
        geom_point(color = "#1a446c", alpha = 0.6, size = 2.5) +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 1) +
        theme_minimal() +
        labs(title = "Valores Observados vs. Valores Predichos",
             x = paste("Realidad (Y:", input$var_dep, ")"),
             y = "Predicción estimada por el modelo MCO")
    })
    
    output$coef_plot_mco <- renderPlot({
      req(modelo_fit())
      df_c <- broom::tidy(modelo_fit())
      df_c <- df_c[df_c$term != "(Intercept)", ]
      
      ggplot(df_c, aes(x = reorder(term, estimate), y = estimate)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
        geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), 
                      width = 0.2, color = "#2c3e50", size = 0.8) +
        geom_point(color = "#1a446c", size = 4) +
        coord_flip() +
        theme_minimal() +
        labs(title = "Gráfico de Parámetros (Impacto Marginal Parcial)",
             subtitle = "Puntos representan el coeficiente Beta; líneas indican el intervalo de confianza al 95%",
             x = "Variables Independientes (X)",
             y = "Magnitud del Efecto Estimado")
    })
    
    # --- 6. INTERPRETACIONES ---
    output$interp_diagnostico_reg <- renderText({
      "Análisis de colinealidad mediante el factor de inflación de la varianza (VIF) y la matriz de correlaciones simultáneas."
    })
    
    output$interp_resultados_reg <- renderText({
      req(input$var_indep)
      paste0(
        "Los coeficientes describen el peso marginal parcial estimado para cada regresor del modelo estadístico de Mínimos Cuadrados Ordinarios.\n\n",
        "Ejemplo práctico (Dataset 'Boston Housing'): Al modelar el valor mediano de la vivienda (medv), descubrirás que variables como el número medio de habitaciones ",
        "(rm) exhiben un p-valor de contraste t extremadamente bajo (<0.05), lo que rechaza la hipótesis nula e indica que por ",
        "cada habitación adicional en la vivienda, el valor condicional esperado del inmueble se incrementa de forma lineal y significativa, manteniendo constantes el resto de factores (como la tasa de criminalidad 'crim')."
      )
    })
    
    output$interp_pred_reg <- renderText({
      paste0(
        "El gráfico de dispersión contrasta el valor empírico frente a la estimación. La línea discontinua roja representa la predicción perfecta (1:1).\n\n",
        "Ejemplo práctico (Dataset 'Boston Housing'): Cuanto más estrecha y concentrada sea la nube de puntos en torno a la diagonal de referencia, ",
        "mayor será el R-cuadrado del modelo, demostrando la fidelidad explicativa de las características estructurales y demográficas de los distritos de Boston evaluados sobre el valor final de la vivienda (medv)."
      )
    })
    
  }) 
}

# -------------------------------
# AUTOEVALUACION
# -------------------------------
Regresion_multiple_Auto_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # ─── SOLUCIÓN REFORZADA DEFINTIVA PARA RESPUESTAS EN UNA LÍNEA ───
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
        title = "➕ Gestión: Añadir pregunta personalizada de  la regresión múltiple", # Corrigo texto PCA -> DBSCAN
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
        
        actionButton(ns("add"), "Guardar pregunta en el banco de la regresión múltiple", class = "btn-success btn-sm mt-2") # Corrigo texto PCA -> DBSCAN
      )
    )
  )
}

Regresion_multiple_Auto_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # CORRECCIÓN 1: Habías duplicado e inicializado dos veces consecutivas este valor reactivo
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
        texto = "¿Cuál es el objetivo principal de la regresión lineal múltiple?",
        opciones = c(
          "Clasificar observaciones",
          "Reducir la dimensionalidad",
          "Predecir una variable respuesta continua a partir de varias variables explicativas",
          "Agrupar individuos similares"
        ),
        correcta = "Predecir una variable respuesta continua a partir de varias variables explicativas"
      ),
      list(
        texto = "En un modelo de regresión con el dataset Boston Housing, la variable 'rm' (número de habitaciones) muestra una estimación Beta de 3.8099 y un p-valor de 0.0000. ¿Cómo se interpreta este resultado?",
        opciones = c(
          "Que por cada habitación adicional el precio medio de la vivienda tiende a subir en 3.81 unidades (miles de dólares), siendo una variable altamente significativa",
          "Que el número de habitaciones disminuye el valor de los hogares en Boston",
          "Que la variable 'rm' no aporta información útil y debe descartarse de inmediato",
          "Que existe un error de multicolinealidad provocado exclusivamente por las habitaciones"
        ),
        correcta = "Que por cada habitación adicional el precio medio de la vivienda tiende a subir en 3.81 unidades (miles de dólares), siendo una variable altamente significativa"
      ),
      list(
        texto = "En un análisis de regresión para predecir el precio de la vivienda en Boston, el R-cuadrado Ajustado registra un valor de 0.7338. ¿Qué significa exactamente este dato?",
        opciones = c(
          "Que el modelo comete un error promedio de 73.38 dólares en cada predicción inmobiliaria",
          "Que el 73.38% de la variabilidad del precio de las viviendas queda explicada por el modelo, habiendo penalizado la inclusión de predictores irrelevantes",
          "Que el modelo solo realiza predicciones correctas para el 73.38% de los datos y falla en el resto",
          "Que la probabilidad de que el modelo sea erróneo en la población general es exactamente de 0.7338"
        ),
        correcta = "Que el 73.38% de la variabilidad del precio de las viviendas queda explicada por el modelo, habiendo penalizado la inclusión de predictores irrelevantes"
      ),
      list(
        texto = "¿Qué indica un p-valor pequeño asociado a un coeficiente?",
        opciones = c(
          "Que la variable aporta información al modelo",
          "Que existe multicolinealidad",
          "Que el modelo es incorrecto",
          "Que el coeficiente vale exactamente cero"
        ),
        correcta = "Que la variable aporta información al modelo"
      ),
      list(
        texto = "Al evaluar globalmente un modelo para el precio de la vivienda con el dataset Boston Housing, se obtiene un p-valor global de 0.0000 y un Estadístico F global de 108.0767. ¿Qué conclusión se extrae de estas métricas?",
        opciones = c(
          "Que el modelo en su conjunto es estadísticamente significativo para predecir el precio de la vivienda",
          "Que todas las variables independientes tienen exactamente el mismo impacto en el modelo",
          "Que existe un problema insalvable de falta de datos o variables vacías",
          "Que los residuos del modelo siguen una distribución perfectamente uniforme alrededor del precio"
        ),
        correcta = "Que el modelo en su conjunto es estadísticamente significativo para predecir el precio de la vivienda"
      ),
      list(
        texto = "Empleando como variable dependiente medv y las demás como predictoras, las líneas horizontales del gráfico de coeficientes representan los intervalos de confianza al 95%. Si observas las variables 'age' e 'indus', estas líneas cruzan la línea vertical discontinua del cero. ¿Qué significa esto visualmente?",
        opciones = c(
          "Que los coeficientes de estas variables son infinitamente grandes",
          "Que existe un sesgo de heterocedasticidad en la medición de ambas características",
          "Que no se puede rechazar que su efecto real sea cero, confirmando visualmente que no son estadísticamente significativas",
          "Que son los dos predictores que más aumentan el precio estimado de los inmuebles"
        ),
        correcta = "Que no se puede rechazar que su efecto real sea cero, confirmando visualmente que no son estadísticamente significativas"
      ),
      list(
        texto = "Empleando como variable dependiente medv y las demás como predictoras, si la variable 'crim' (tasa de criminalidad per cápita) presents un coeficiente negativo y significativo, ¿qué conclusión práctica se extrae?",
        opciones = c(
          "Que los barrios con mayor tasa de criminalidad tienden a registrar precios de vivienda más bajos, manteniendo el resto constante",
          "Que un incremento en el precio de la vivienda provoca que aumente la delincuencia en la zona",
          "Que la delincuencia no guarda ninguna relación estadística con el valor de los inmuebles",
          "Que el modelo ha fallado porque los coeficientes de delincuencia siempre deben ser positivos"
        ),
        correcta = "Que los barrios con mayor tasa de criminalidad tienden a registrar precios de vivienda más bajos, manteniendo el resto constante"
      ),
      list(
        texto = "Analizando un modelo de regresión múltiple para el precio de la vivienda con los datos de Boston Housing, ¿cuál de las siguientes afirmaciones sobre las variables 'age' e 'indus' es estadísticamente correcta si presentan p-valores de 0.9582 y 0.7383 respectivamente?",
        opciones = c(
          "Ambas variables distorsionan la estimación del intercepto",
          "Son las dos variables que mayor impacto predictivo aportan sobre el valor medio de la vivienda",
          "Ambas variables presentan p-valores muy altos, lo que indica que no son estadísticamente significativas en presencia de las demás",
          "Sus coeficientes demuestran que la antigüedad de la casa duplica el valor del suelo comercial"
        ),
        correcta = "Ambas variables presentan p-valores muy altos, lo que indica que no son estadísticamente significativas en presencia de las demás"
      ),
      list(
        texto = "Considera como variable dependiente medv y las demás como predictoras con los datos de Boston Housing. En el gráfico de impacto de coeficientes, la variable 'nox' (concentración de óxidos de nitrógeno) se sitúa muy a la izquierda, reflejando una magnitud estimada cercana a -17.76. ¿Qué interpretación tiene esto?",
        opciones = c(
          "Muestra de forma clara que 'nox' es la variable con el impacto marginal negativo más fuerte sobre la predicción del precio al aumentar una unidad su concentración",
          "Significa que es la variable con menor importancia y menor relevancia matemática del modelo",
          "Indica que su intervalo de confianza al 95% es inestable y contiene errores numéricos",
          "Demuestra que la variable tiene una correlación lineal positiva directa con el precio de los hogares"
        ),
        correcta = "Muestra de forma clara que 'nox' es la variable con el impacto marginal negativo más fuerte sobre la predicción del precio al aumentar una unidad su concentración"
      ),
      list(
        texto = "Un investigador incorpora una nueva variable y el R² aumenta ligeramente, pero el R² ajustado disminuye. ¿Qué indica esto?",
        opciones = c(
          "La nueva variable probablemente no aporta información útil",
          "El modelo ha mejorado claramente",
          "Los residuos son normales",
          "El modelo ya no puede utilizarse"
        ),
        correcta = "La nueva variable probablemente no aporta información útil"
      ),
      list(
        texto = "Empleando como variable dependiente medv y las demás como predictoras con los datos de Boston Housing, ¿cuál es la variable predictora que registra el impacto negativo más significativo (con un estadístico t de -10.3471) sobre el valor de las viviendas?",
        opciones = c(
          "tax (impuestos)",
          "crim (tasa de criminalidad)",
          "ptratio (proporción alumno-profesor)",
          "lstat (porcentaje de población de bajo estatus socioeconómico)"
        ),
        correcta = "lstat (porcentaje de población de bajo estatus socioeconómico)"
      ),
      list(
        texto = "¿Cuál de las siguientes afirmaciones describe mejor el problema de la multicolinealidad?",
        opciones = c(
          "Ocurre cuando los residuos del modelo tienen varianzas que cambian según el valor ajustado",
          "Se presenta cuando dos o más variables predictoras están altamente correlacionadas entre sí",
          "Indica que el modelo no ha sido capaz de predecir correctamente la variable respuesta",
          "Significa que la variable dependiente sigue una escala cualitativa o binaria"
        ),
        correcta = "Se presenta cuando dos o más variables predictoras están altamente correlacionadas entre sí"
      ),
      list(
        texto = "Si la variable respuesta o dependiente cambia en su escala numérica (por ejemplo, de metros a kilómetros), ¿qué ocurre con el coeficiente de determinación (R²)?",
        opciones = c(
          "El R² disminuye proporcionalmente al cambio de la unidad",
          "El R² aumenta de forma exponencial debido al cambio de escala",
          "El R² permanece exactamente igual, ya que es una medida adimensional de bondad de ajuste",
          "El modelo se vuelve inválido e impide calcular el valor de R²"
        ),
        correcta = "El R² permanece exactamente igual, ya que es una medida adimensional de bondad de ajuste"
      ),
      list(
        texto = "¿Qué problema matemático de estimación directa suele generar una multicolinealidad severa en un modelo de regresión lineal múltiple?",
        opciones = c(
          "Provoca que las estimaciones de los coeficientes Beta sean muy inestables y aumente drásticamente su error estándar",
          "Hace que los p-valores globales del estadístico F siempre se aproximen a uno",
          "Fuerza a que todos los residuos sigan una distribución asimétrica no lineal",
          "Sustituye de forma automática los coeficientes numéricos continuos por variables categóricas"),
        correcta = "Provoca que las estimaciones de los coeficientes Beta sean muy inestables y aumente drásticamente su error estándar"),
      list(texto = "¿Cuál es la consecuencia directa de omitir una variable explicativa relevante que está correlacionada con las variables ya incluidas en el modelo?",
           opciones = c(
             "Los coeficientes estimados de las variables incluidas absorberán el efecto de la omitida, generando estimaciones sesgadas","El estadístico F global del modelo pasará a valer exactamente cero de inmediato",
             "El R-cuadrado del modelo aumentará falsamente hasta alcanzar la perfección",
             "La matriz de covarianzas se vuelve estrictamente diagonal e independiente"),
           correcta = "Los coeficientes estimados de las variables incluidas absorberán el efecto de la omitida, generando estimaciones sesgadas"))
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
    
    # CORRECCIÓN 3: Cambiado observe() por observeEvent(preguntas(), ...).
    # Al usar observe() plano con isolate(), Shiny entraba en bucles infinitos de renderizado
    # o no actualizaba correctamente el set inicial de preguntas al iniciar el módulo.
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

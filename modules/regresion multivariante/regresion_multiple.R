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
               p("Seleccione las variables métricas para estructurar el ajuste por Mínimos Cuadrados Ordinarios (MCO)."),
               hr(),
               uiOutput(ns("ui_var_dep")),
               uiOutput(ns("ui_var_indep")),
               hr(),
               helpText("Nota: El análisis se ejecuta automáticamente al cambiar cualquier parámetro."),
               helpText("Nota: Se eliminan filas con valores faltantes automáticamente.")
             )
      ),
      
      #--------------------------------------------------
      # PANEL PRINCIPAL (A LA DERECHA)
      #--------------------------------------------------
      column(8,
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
                        h4("Coeficientes del Modelo"), DT::DTOutput(ns("tabla_coeficientes")),
                        br(),
                        h4("Gráfico de Impacto de los Coeficientes"), plotOutput(ns("coef_plot_mco")), # AÑADIDO
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
    
    # --- 1. PROCESAMIENTO DE DATOS ---
    datos_base <- reactive({
      df <- if(!is.null(datos()) && nrow(datos()) > 0) datos() else datos_ejemplo
      req(df)
      df[] <- lapply(df, function(x) {
        if(is.factor(x) || is.character(x)) {
          as_n <- suppressWarnings(as.numeric(as.character(x)))
          if(sum(!is.na(as_n)) > (length(x)*0.8)) as_n else x
        } else x
      })
      df[complete.cases(df[, sapply(df, is.numeric)]), ]
    })
    
    datos_preparados <- reactive({
      req(input$var_dep, input$var_indep)
      df <- datos_base()[, c(input$var_dep, input$var_indep), drop = FALSE]
      as.data.frame(scale(df))
    })
    
    # --- 2. SELECTORES ---
    output$ui_var_dep <- renderUI({
      nums <- names(datos_base())[sapply(datos_base(), is.numeric)]
      selectInput(session$ns("var_dep"), "Variable Dependiente (Y):", choices = nums)
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
        options = list(
          'plugins' = list('remove_button'),
          'create' = TRUE,
          'persist' = FALSE
        )
      )
    })
    
    # --- 3. MODELO LINEAL COMPLETAMENTE REACTIVO (MCO) ---
    modelo_fit <- reactive({
      req(input$var_dep, input$var_indep)
      df <- datos_base()
      formula_str <- paste(input$var_dep, "~", paste(input$var_indep, collapse = "+"))
      lm(as.formula(formula_str), data = df[, c(input$var_dep, input$var_indep)])
    })
    
    # --- 4. OUTPUTS TABLAS ---
    output$tabla_original <- DT::renderDT({
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
    
    # --- 5. DIAGNÓSTICO: COLINEALIDAD (MANTENIENDO EL HEATMAP CLÁSICO DE TU MEMORIA) ---
    output$corr_plot <- renderPlot({
      req(input$var_dep, input$var_indep)
      
      # Calculamos la correlación de las variables elegidas en tiempo real
      df_corr <- datos_base()[, c(input$var_dep, input$var_indep)]
      res_corr <- cor(df_corr, use = "complete.obs")
      
      # Heatmap clásico idéntico a la Figura 2 de tu TFG
      heatmap(res_corr, 
              col = colorRampPalette(c("#E4672E", "white", "#6D9EC1"))(20),
              symm = TRUE, 
              main = "Matriz de Correlación (Y + Xs)")
    })
    
    output$vif_table <- DT::renderDT({
      req(length(input$var_indep) > 1)
      vif_values <- car::vif(modelo_fit())
      df_vif <- data.frame(Variable = names(vif_values), VIF = as.numeric(vif_values))
      
      # Tabla compacta con scroll lateral sin paginación verde
      DT::datatable(df_vif, options = list(paging = FALSE, scrollX = TRUE), class = 'cell-border compact') %>%
        DT::formatRound(columns = "VIF", digits = 2)
    })
    
    
    # --- 6. VISUALIZACIÓN (REACTIVA) ---
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
      
      # Quitamos el intercepto para poder comparar la magnitud real de las variables independientes
      df_c <- df_c[df_c$term != "(Intercept)", ]
      
      # Construimos el gráfico de parámetros con intervalos de confianza del 95%
      ggplot(df_c, aes(x = reorder(term, estimate), y = estimate)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) + # Línea crítica del cero
        geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), 
                      width = 0.2, color = "#2c3e50", size = 0.8) +
        geom_point(color = "#1a446c", size = 4) +
        coord_flip() + # Volteamos el gráfico para leer los nombres horizontalmente
        theme_minimal() +
        labs(title = "Gráfico de Parámetros (Impacto Marginal Parcial)",
             subtitle = "Puntos representan el coeficiente Beta; líneas indican el intervalo de confianza al 95%",
             x = "Variables Independientes (X)",
             y = "Magnitud del Efecto Estimado")
    })
    
    output$interp_resultados_reg <- renderText({
      req(input$var_indep)
      paste0(
        "Los coeficientes describen el peso marginal parcial estimado para cada regresor del modelo estadístico de Mínimos Cuadrados Ordinarios.\n\n",
        "Ejemplo práctico (Dataset 'penguins'): Al modelar la masa corporal, descubrirás que variables como el largo ",
        "de la aleta exhiben un p-valor de contraste t extremadamente bajo (<0.05), lo que rechaza la hipótesis nula e indica que por ",
        "cada milímetro adicional de aleta, el peso condicional esperado del pingüino se incrementa de forma lineal y significativa."
      )
    })
    
    output$interp_pred_reg <- renderText({
      paste0(
        "El gráfico de dispersión contrasta el valor empírico frente a la estimación. La línea discontinua roja representa la predicción perfecta (1:1).\n\n",
        "Ejemplo práctico (Dataset 'penguins'): Cuanto más estrecha y concentrada sea la nube de puntos en torno a la diagonal de referencia, ",
        "mayor será el R-cuadrado del modelo, demostrando la fidelidad explicativa de las características físicas de los pingüinos analizados."
        )
    })
    
  }) # Cierre de moduleServer
} # Cierre de Regresion_multiple_Analisis_Server

# -------------------------------
# AUTOEVALUACION
# -------------------------------

Regresion_multiple_Auto_UI <- function(id){
  ns <- NS(id)
  
  tagList(
    
    h3("Autoevaluación"),
    
    uiOutput(ns("preguntas")),
    
    br(),
    
    actionButton(ns("ver"), "Ver respuestas"),
    
    br(), br(),
    
    uiOutput(ns("respuestas"))
  )
}
Regresion_multiple_Auto_Server <- function(id){
  
  moduleServer(id, function(input, output, session){
    
    # =========================================
    # 📚 PREGUNTAS (EDITA AQUÍ)
    # =========================================
    
    preguntas <- list(
      list(
        texto = "¿?",
        opciones = c("a)","b)","c)", "d)"),
        correcta = "c)"
      ),
      list(
        texto = "¿?",
        opciones = c("a)","b)","c)", "d)"),
        correcta = "a)"
      ),
      list(
        texto = "¿?",
        opciones = c("a)","b)","c)", "d)"),
        correcta = "b)"
      ),
      list(
        texto = "¿?",
        opciones = c("a)","b)","c)", "d)"),
        correcta = ""
      ),
      list(
        texto = "¿?",
        opciones = c("a)","b)","c)", "d)"),
        correcta = "d)"
      ),
      list(
        texto = "¿?",
        opciones = c("a)","b)","c)", "d)"),
        correcta = "c)"
      ),
      list(
        texto = "¿?",
        opciones = c("a)","b)","c)", "d)"),
        correcta = "d)"
      ),
      list(
        texto = "¿?",
        opciones = c("a)","b)","c)", "d"),
        correcta = "a)"
      ),
      list(
        texto = "¿?",
        opciones = c("a)","b)","c)", "d"),
        correcta = "c)"
      ),
      list(
        texto = "¿?",
        opciones = c("a)","b)","c)", "d"),
        correcta = "b)"
      )
    )
    
    # =========================================
    # UI DINÁMICA
    # =========================================
    
    output$preguntas <- renderUI({
      
      tagList(
        lapply(seq_along(preguntas), function(i){
          
          radioButtons(
            inputId = session$ns(paste0("q", i)),
            label = paste0(i, ". ", preguntas[[i]]$texto),
            choices = preguntas[[i]]$opciones
          )
        })
      )
    })
    
    # =========================================
    # RESPUESTAS
    # =========================================
    
    output$respuestas <- renderUI({
      
      req(input$ver)
      
      tagList(
        lapply(seq_along(preguntas), function(i){
          
          respuesta_usuario <- input[[paste0("q", i)]]
          correcta <- preguntas[[i]]$correcta
          
          if(is.null(respuesta_usuario)){
            return(NULL)
          }
          
          if(respuesta_usuario == correcta){
            p(strong(paste0("✔ Pregunta ", i, ": Correcto")))
          } else {
            p(strong(paste0("✘ Pregunta ", i, ": Incorrecto. Respuesta correcta: ", correcta)))
          }
        })
      )
    })
    
  })
}

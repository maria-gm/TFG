# =====================================================
#  Regresión múltiple- MODULO
# =====================================================


# -------------------------------
# TEORIA
# -------------------------------

Regresion_logistica_Teoria_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Única llamada necesaria para activar MathJax en la página
    withMathJax(),
    
    tags$div(
      style = "padding: 20px; background-color: #fcfdfe;",
      
      # =====================================
      # CABECERA
      # =====================================
      h2(
        "Regresión Logística",
        style = "font-weight: 800; color: #1a365d; margin-bottom: 5px;"
      ),
      
      p(
        "Modelización de variables dependientes categóricas binarias a partir de un conjunto de predictores.",
        style = "color: #64748b; font-size: 1.1rem; margin-bottom: 30px;"
      ),
      
      # =====================================
      # TARJETAS PRINCIPALES
      # =====================================
      bslib::layout_column_wrap(
        width = 1/3, # Tres columnas perfectas
        heights_equal = "row",
        
        # ---------------------------------
        # FUNDAMENTO
        # ---------------------------------
        bslib::card(
          bslib::card_header(
            tags$b("1. Fundamento matemático"),
            style = "background: #e0e7ff;"
          ),
          bslib::card_body(
            p("Modela la probabilidad condicionada de que la variable respuesta pertenezca a la categoría de interés \\(Y = 1\\) mediante la función logística:"),
            p("$$p(x_i; \\boldsymbol{\\beta}) = \\frac{e^{\\beta_0 + \\beta_1X_{1i} + \\dots + \\beta_pX_{pi}}}{1 + e^{\\beta_0 + \\beta_1X_{1i} + \\dots + \\beta_pX_{pi}}}$$"),
            p("Garantiza así que las estimaciones se mantengan estrictamente dentro del rango de probabilidades \\([0, 1]\\).")
          )
        ),
        
        # ---------------------------------
        # OBJETIVOS
        # ---------------------------------
        bslib::card(
          bslib::card_header(
            tags$b("2. Objetivos"),
            style = "background: #dcfce7;"
          ),
          bslib::card_body(
            p("Este modelo de regresión multivariante persigue resolver problemas de clasificación y asociación buscando:"),
            tags$ul(
              style = "margin-top: 15px;",
              tags$li("Comprobar hipótesis o relaciones causales entre variables.", style = "margin-bottom: 10px;"),
              tags$li("Estudiar la probabilidad de ocurrencia de un fenómeno binario.", style = "margin-bottom: 10px;"),
              tags$li("Encontrar el modelo con el mejor ajuste descriptivo y que sea altamente interpretable.", style = "margin-bottom: 10px;"),
              tags$li("Clasificar nuevas observaciones estimando su probabilidad de pertenencia a un grupo.")
            )
          )
        ),
        
        # ---------------------------------
        # CUÁNDO USARLO
        # ---------------------------------
        bslib::card(
          bslib::card_header(
            tags$b("3. ¿Cuándo usarlo?"),
            style = "background: #fef3c7;"
          ),
          bslib::card_body(
            p("Se recomienda aplicar una regresión logística cuando tu base de datos presente:"),
            tags$ul(
              style = "margin-top: 15px;",
              tags$li("Una variable resultado binaria o dicotómica (0 o 1).", style = "margin-bottom: 10px;"),
              tags$li("Variables predictoras que pueden ser cuantitativas y/o categóricas.", style = "margin-bottom: 10px;"),
              tags$li("Necesidad de analizar efectos marginales combinados sobre una probabilidad.", style = "margin-bottom: 10px;"),
              tags$li("Casos donde no se cumplan los supuestos de continuidad o linealidad directa exigidos por la regresión lineal ordinaria.")
            )
          )
        )
      ),
      
      br(),
      
      # =====================================
      # FORMULACIÓN DEL MODELO Y LOGIT
      # =====================================
      bslib::card(
        style = "border: 1px solid #cbd5e1; background: #f8fafc;",
        bslib::card_body(
          h4(
            icon("calculator"), "Transformación Logit y Odds Ratio",
            style = "color: #1e40af; margin-bottom: 10px;"
          ),
          p("La transformación logit mapea la probabilidad a una escala lineal continua e infinita. Aplicando el logaritmo de las odds (posibilidades o ventajas), el modelo se linealiza de la siguiente forma:"),
          p("$$\\log\\left(\\frac{p(x_i)}{1 - p(x_i)}\\right) = \\beta_0 + \\beta_1X_{1i} + \\dots + \\beta_pX_{pi}$$"),
          tags$ul(
            style = "margin-top: 10px;",
            tags$li("La variable respuesta modelada de forma lineal no es directamente la probabilidad, sino el logaritmo de la ventaja.", style = "margin-bottom: 6px;"),
            tags$li("A diferencia del modelo lineal ordinario, la regresión logística difiere notablemente tanto en su estructura no lineal como en la naturaleza de sus supuestos.")
          )
        )
      ),
      
      br(),
      
      # =====================================
      # ESTIMACIÓN DE PARÁMETROS
      # =====================================
      bslib::card(
        style = "border: 1px solid #cbd5e1; background: #f8fafc;",
        bslib::card_body(
          h4(
            icon("gears"), "Estimación de parámetros: Máxima Verosimilitud",
            style = "color: #1e40af; margin-bottom: 10px;"
          ),
          p("A diferencia del modelo de regresión lineal simple o múltiple, los parámetros \\(\\boldsymbol{\\beta}\\) no se obtienen por el método de mínimos cuadrados ordinarios. En su lugar, se recurre al método de Máxima Verosimilitud (MLE), el cual optimiza y maximiza la función de log-verosimilitud para las \\(N\\) observaciones:"),
          p("$$\\ell(\\boldsymbol{\\beta}) = \\sum_{i=1}^{N} \\left\\{ y_i \\log[p(x_i; \\boldsymbol{\\beta})] + (1 - y_i) \\log[1 - p(x_i; \\boldsymbol{\\beta})] \\right\\}$$"),
          tags$ul(
            style = "margin-top: 10px;",
            tags$li("\\(y_i\\) : representa la respuesta binaria real observada (0 o 1) para cada individuo.", style = "margin-bottom: 6px;"),
            tags$li("\\(p(x_i; \\boldsymbol{\\beta})\\) : es la probabilidad de éxito condicionada al valor de los predictores \\(x_i\\) y controlada por el vector de parámetros estocásticos.", style = "margin-bottom: 6px;"),
            tags$li("El estimador busca los coeficientes numéricos precisos que maximicen la probabilidad matemática de observar la muestra exacta que ha sido recopilada.")
          )
        )
      ), 
      
      br(),
      
      # =====================================
      # EVALUACIÓN E INTERPRETACIÓN
      # =====================================
      bslib::card(
        style = "border: 1px solid #cbd5e1; background: #f8fafc;",
        bslib::card_body(
          h4(
            icon("magnifying-glass"), "Interpretación y Evaluación del Modelo",
            style = "color: #1e40af; margin-bottom: 15px;"
          ),
          p("La interpretación práctica de los coeficientes estimados y la validación predictiva final constituyen fases críticas del análisis multivariante:"),
          
          tags$div(
            style = "display: flex; flex-direction: column; gap: 15px; margin-top: 15px;",
            
            # --------------------------------
            # ODDS RATIOS
            # --------------------------------
            tags$div(
              style = "border-left: 4px solid #3b82f6; padding-left: 12px;",
              tags$b("Odds Ratios (OR): "),
              "La interpretación intuitiva de un coeficiente se realiza mediante su exponencial (\\(e^{\\beta_j}\\)). " ,
              "Representa el cambio multiplicativo en las ventajas por cada incremento unitario en la variable predictora, manteniendo constantes el resto. " ,
              "Valores superiores a 1 reflejan un aumento de la probabilidad del evento, mientras que valores inferiores a 1 implican una disminución."
            ),
            
            # --------------------------------
            # EVALUACIÓN PREDICTIVA (AUC)
            # --------------------------------
            tags$div(
              style = "border-left: 4px solid #10b981; padding-left: 12px;",
              tags$b("Capacidad de Discriminación (Métrica AUC): "),
              "La calidad general del clasificador se evalúa habitualmente calculando el Área Bajo la Curva (AUC). " ,
              "El índice resultante oscila entre 0 y 1. Un valor cercano a 0.5 denota una capacidad puramente aleatoria (similar al azar), mientras que los valores próximos a 1 reflejan una excelente capacidad predictiva para separar con éxito ambas clases."
            )
          )
        )
      )
    )
  )
}

Regresion_logistica_Teoria_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Módulo de servidor lógicamente vacío para visualizaciones estáticas teóricas
  })
}
# -------------------------------
# ANALISIS
# -------------------------------
Regresion_logistica_Analisis_UI <- function(id){
  ns <- NS(id)
  tagList(
    # Título personalizado corporativo unificado con el de RLM
    h3("Análisis", 
       style = "color: #1a446c; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; font-weight: 600; margin-top: 40px; margin-bottom: 20px; border-bottom: 2px solid #f4f6f9; padding-bottom: 10px;"),
    
    fluidRow(
      #--------------------------------------------------
      # PANEL LATERAL (A LA IZQUIERDA)
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
               helpText("Nota: El análisis se ejecuta automáticamente al cambiar cualquier parámetro."),
               helpText("Nota: Se eliminan filas con valores faltantes automáticamente.")
             )
      ),
      
      #--------------------------------------------------
      # PANEL PRINCIPAL (A LA DERECHA)
      #--------------------------------------------------
      column(8,
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
                        h4("Métricas de Información y Error", style = "color: #2c3e50; font-weight: 500;"), 
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
               
               # PESTAÑA 4: PREDICCIÓN (AÑADIDO EN TAB DISTINTO)
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
    
    # --- 1. DATOS BASE ---
    datos_base <- reactive({
      df <- if(!is.null(datos()) && is.data.frame(datos())) datos() else datos_ejemplo
      if(is.list(df) && !is.data.frame(df)) df <- df$Regresion_multiple 
      req(df)
      as.data.frame(df)[complete.cases(df), , drop = FALSE]
    })
    
    # --- 2. SELECTORES ---
    output$ui_var_dep <- renderUI({
      req(datos_base())
      df <- datos_base()
      vars <- names(df)[sapply(df, function(x) length(unique(x)) == 2)]
      if(length(vars) == 0 && "Class" %in% names(df)) vars <- "Class"
      selectInput(session$ns("var_dep"), "Variable a Predecir (Y):", choices = vars)
    })
    
    output$ui_var_indep <- renderUI({
      req(input$var_dep)
      df <- datos_base()
      cols <- setdiff(names(df), c(input$var_dep, "Id"))
      selectizeInput(session$ns("var_indep"), "Variables Predictoras (X):", 
                     choices = cols, multiple = TRUE, 
                     selected = cols[1:min(5, length(cols))][sapply(df[, cols], is.numeric)],
                     options = list(plugins = list('remove_button')))
    })
    
    # --- 3. PREPARACIÓN LOGÍSTICA ---
    datos_log <- reactive({
      req(input$var_dep, input$var_indep)
      df <- datos_base()
      df[[input$var_dep]] <- as.factor(df[[input$var_dep]])
      levels(df[[input$var_dep]]) <- c(0, 1) 
      df
    })
    
    # --- 4. MODELO COMPLETAMENTE REACTIVO ---
    modelo_log <- reactive({
      req(input$var_dep, input$var_indep)
      formula_str <- paste(input$var_dep, "~", paste(input$var_indep, collapse = "+"))
      glm(as.formula(formula_str), data = datos_log(), family = binomial)
    })
    
    # --- 5. OUTPUTS DE TABLAS (CON SCROLL INTERNO NATIVO) ---
    output$tabla_original <- DT::renderDT({
      DT::datatable(
        datos_base(), 
        options = list(paging = FALSE, scrollY = "400px", scrollX = TRUE, autoWidth = TRUE),
        class = 'cell-border stripe hover compact'
      ) %>% 
        DT::formatRound(columns = names(datos_base())[sapply(datos_base(), is.numeric)], digits = 3)
    })
    
    output$tabla_preparada <- DT::renderDT({
      req(datos_log())
      df_prep <- datos_log()[, c(input$var_dep, input$var_indep), drop = FALSE]
      
      DT::datatable(
        df_prep, 
        options = list(paging = FALSE, scrollY = "400px", scrollX = TRUE, autoWidth = TRUE),
        class = 'cell-border stripe hover compact'
      ) %>% 
        DT::formatRound(columns = names(df_prep)[sapply(df_prep, is.numeric)], digits = 3)
    })
    
    output$tabla_coefs <- DT::renderDT({
      req(modelo_log())
      df_c <- broom::tidy(modelo_log())
      colnames(df_c) <- c("Término", "Estimación (Log-Odds)", "Error Estándar", "Estadístico z", "p-valor")
      
      DT::datatable(df_c, options = list(paging = FALSE, scrollX = TRUE), class = 'cell-border compact') %>%
        DT::formatRound(columns = 2:5, digits = 4)
    })
    
    output$tabla_odds <- DT::renderDT({
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
      req(modelo_log())
      m <- modelo_log()
      df_m <- data.frame(
        Métrica = c("Criterio AIC (Menor denota mejor ajuste)", "Null Deviance (Varianza sin predictores)", "Residual Deviance (Varianza del modelo)"),
        Valor = c(m$aic, m$null.deviance, m$deviance)
      )
      
      DT::datatable(df_m, options = list(paging = FALSE, scrollX = TRUE), class = 'cell-border compact') %>%
        DT::formatRound(columns = "Valor", digits = 4)
    })
    
    output$interp_resultados_log <- renderText({
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
      req(modelo_log())
      preds <- predict(modelo_log(), type = "response")
      clases <- ifelse(preds > 0.5, 1, 0)
      
      matriz <- as.data.frame.matrix(table(Real = datos_log()[[input$var_dep]], Predicho = clases))
      
      DT::datatable(matriz, options = list(paging = FALSE, searching = FALSE), class = 'cell-border compact', rownames = TRUE)
    })
    
    output$plot_roc <- renderPlot({
      req(modelo_log())
      preds <- predict(modelo_log(), type = "response")
      res_roc <- pROC::roc(datos_log()[[input$var_dep]], preds)
      pROC::plot.roc(res_roc, main = paste("Curva ROC - AUC:", round(res_roc$auc, 3)), 
                     col = "#1a446c", lwd = 4, print.auc = TRUE)
    })
    
    output$interp_rendimiento_log <- renderText({
      req(modelo_log())
      paste0(
        "La matriz de confusión cuantifica los aciertos y los errores de clasificación utilizando un umbral estándar de corte p = 0.5.\n",
        "La curva ROC evalúa la capacidad de discriminación del modelo sobre todos los umbrales posibles (un AUC de 1 indica clasificación perfecta).\n\n",
        "Ejemplo práctico (Dataset 'BreastCancer'): Dado que las características citológicas extraídas son predictores biológicos muy robustos, ",
        "la matriz de confusión registrará una tasa de falsos positivos y falsos negativos bajísima. Esto se traduce en una curva ROC impecable ",
        "con un Área Bajo la Curva (AUC) que suele superar el 0.95, demostrando el altísimo poder de clasificación clínica de la aplicación de R Shiny."
      )
    })
    # --- 7. PESTAÑA 4: PREDICCIÓN (CURVA LOGÍSTICA DE LÍNEAS - AJUSTE SIGMOIDE) ---
    output$plot_pred_log <- renderPlot({
      req(modelo_log(), input$var_dep)
      
      # Extraemos los Log-Odds (eje X) y las Probabilidades (eje Y)
      log_odds <- predict(modelo_log(), type = "link")
      probabilidades <- predict(modelo_log(), type = "response")
      
      df_plot <- data.frame(
        Log_Odds = log_odds,
        Probabilidad = probabilidades,
        Real = factor(datos_log()[[input$var_dep]], labels = c("Benigno (0)", "Maligno (1)"))
      )
      
      # Ordenamos por Log-Odds para que la línea se dibuje de izquierda a derecha sin cruzarse
      df_plot <- df_plot[order(df_plot$Log_Odds), ]
      
      ggplot(df_plot, aes(x = Log_Odds, y = Probabilidad)) +
        # Añadimos la curva sigmoide teórica ajustada por tu modelo
        geom_line(color = "#1a446c", size = 1.2, alpha = 0.9, name = "Función Logística") +
        # Pintamos los pacientes reales sobre la curva para ver dónde caen
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
    

  })
}


# -------------------------------
# AUTOEVALUACION
# -------------------------------

Regresion_logistica_Auto_UI <- function(id){
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
Regresion_logistica_Auto_Server <- function(id){
  
  moduleServer(id, function(input, output, session){
    
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
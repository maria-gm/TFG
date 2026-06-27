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
      
      probs <- predict(modelo_log(), type="response")
      
      clases <- ifelse(probs>0.5,1,0)
      
      reales <- as.numeric(as.character(datos_log()[[input$var_dep]]))
      
      matriz <- table(Real=reales,
                      Predicho=clases)
      
      TP <- matriz[2,2]
      TN <- matriz[1,1]
      FP <- matriz[1,2]
      FN <- matriz[2,1]
      
      accuracy <- (TP+TN)/sum(matriz)
      
      precision <- ifelse((TP+FP)==0,
                          NA,
                          TP/(TP+FP))
      
      recall <- ifelse((TP+FN)==0,
                       NA,
                       TP/(TP+FN))
      
      f1 <- ifelse(is.na(precision) |
                     is.na(recall) |
                     (precision+recall)==0,
                   NA,
                   2*precision*recall/(precision+recall))
      
      df_m <- data.frame(
        Métrica=c(
          "Accuracy",
          "Precision",
          "Recall",
          "F1-score"
        ),
        Valor=round(c(
          accuracy,
          precision,
          recall,
          f1
        ),4)
      )
      
      DT::datatable(
        df_m,
        options=list(
          paging=FALSE,
          scrollX=TRUE
        ),
        rownames=FALSE
      )
      
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
Regresion_logistica_Auto_UI <- function(id) {
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

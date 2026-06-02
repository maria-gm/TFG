# =========================================================
# REGULARIZACIÓN - TEORÍA
# =========================================================

Regularizacion_Teoria_UI <- function(id){
  ns <- NS(id)
  
  tagList(
    
    h3("Regresión por regularización - Teoría"),
    
    br(),
    
    p("Las técnicas de regularización se utilizan para mejorar el comportamiento de los modelos de regresión cuando existen problemas de multicolinealidad o sobreajuste."),
    
    p("Estos métodos incorporan una penalización sobre el tamaño de los coeficientes del modelo, reduciendo así la complejidad y mejorando la capacidad predictiva."),
    
    h4("Ridge"),
    
    p("La regresión Ridge añade una penalización de tipo L2, reduciendo el valor de los coeficientes sin llegar a eliminarlos completamente."),
    
    h4("Lasso"),
    
    p("La regresión Lasso incorpora una penalización L1, capaz de reducir algunos coeficientes exactamente a cero, realizando selección automática de variables."),
    
    h4("Parámetro Lambda"),
    
    p("El parámetro lambda controla la intensidad de la penalización. Valores pequeños producen modelos similares a la regresión clásica, mientras que valores grandes simplifican el modelo.")
    
  )
}

Regularizacion_Teoria_Server <- function(id){
  
  moduleServer(id, function(input, output, session){ })
}



# =========================================================
# REGULARIZACIÓN - ANÁLISIS
# =========================================================
Regularizacion_Analisis_UI <- function(id){
  ns <- NS(id)
  tagList(
    h3("Autoevaluación", 
       style = "color: #1a446c; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; font-weight: 600; margin-top: 40px; margin-bottom: 20px; border-bottom: 2px solid #f4f6f9; padding-bottom: 10px;"),
    
    fluidRow(
      #--------------------------------------------------
      # PANEL LATERAL
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
               helpText("Nota: El análisis responde automáticamente en tiempo real."),
               helpText("Nota: Se eliminan filas con valores faltantes automáticamente.")
             )
      ),
      
      #--------------------------------------------------
      # PANEL PRINCIPAL
      #--------------------------------------------------
      column(8,
             tabsetPanel(
               id = ns("tabs_reg"),
               
               # PESTAÑA 1: DATOS
               tabPanel("1. Datos y Escala", 
                        br(),
                        p("Información: Para estos algoritmos es vital estandarizar las columnas para igualar las magnitudes de penalización.", style = "color: #7f8c8d; font-style: italic; margin-bottom: 25px;"),
                        h4("Dataset original preparado", style = "color: #2c3e50; font-weight: 500;"), 
                        DT::DTOutput(ns("tabla_original")),
                        br(), hr(), br(),
                        h4("Dataset estandarizado (Z-scores)", style = "color: #2c3e50; font-weight: 500;"), 
                        DT::DTOutput(ns("tabla_preparada"))
               ),
               
               # PESTAÑA 2: OPTIMIZACIÓN POR CV (RESCATADO Y AMPLIADO)
               tabPanel("2. Optimización Automática (CV)",
                        br(),
                        uiOutput(ns("ui_sugerencia_optima")),
                        br(),
                        plotOutput(ns("cv_plot"), height = "420px"),
                        br(),
                        # Contenedores dinámicos exclusivos de PCR rescatados de tu script
                        conditionalPanel(
                          condition = sprintf("input['%s'] == 'pcr'", ns("metodo")),
                          h4("Tabla de Varianza Explicada por Componente", style = "color: #2c3e50; font-weight: 500;"),
                          DT::DTOutput(ns("tabla_varianza")),
                          br(), hr(), br(),
                          h4("Resumen Formal del Modelo PCR (Summary)", style = "color: #2c3e50; font-weight: 500;"),
                          verbatimTextOutput(ns("summary_pcr"))
                        ),
                        br(),
                        h4("Guía de Validación Cruzada", style = "color: #2c3e50; font-weight: 500;"),
                        verbatimTextOutput(ns("interp_cv"))
               ),
               
               # PESTAÑA 3: COEFICIENTES (TU CAPTURA DE PUNTOS E INTERVALOS)
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
    
    # --- 1. PROCESAMIENTO DE DATOS ---
    datos_base <- reactive({
      df <- if(!is.null(datos()) && nrow(datos()) > 0) datos() else datos_ejemplo
      req(df)
      df[complete.cases(df[, sapply(df, is.numeric)]), ]
    })
    
    datos_num <- reactive({
      df <- datos_base()
      df_n <- df[, sapply(df, is.numeric), drop = FALSE]
      cols_validas <- sapply(df_n, function(x) length(unique(x)) > 15)
      if(sum(cols_validas) < 2) return(df_n)
      df_n[, cols_validas, drop = FALSE]
    })
    
    datos_preparados <- reactive({
      req(input$var_dep, input$var_indep)
      df <- datos_base()[, c(input$var_dep, input$var_indep), drop = FALSE]
      as.data.frame(scale(df))
    })
    
    # --- 2. SELECTORES ---
    output$ui_var_dep <- renderUI({
      nums <- names(datos_num())
      selectInput(session$ns("var_dep"), "Variable Dependiente (Y):", choices = nums)
    })
    
    output$ui_var_indep <- renderUI({
      req(input$var_dep)
      nums <- setdiff(names(datos_num()), input$var_dep)
      selectizeInput(
        session$ns("var_indep"), "Variables Independientes (X):", 
        choices = nums, multiple = TRUE, selected = nums[1:min(6, length(nums))],
        options = list('plugins' = list('remove_button'))
      )
    })
    
    # --- 3. MODELOS DE VALIDACIÓN CRUZADA (AUTOMÁTICOS) ---
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
    
    # PCA Auxiliar para la tabla de varianza explicada del PCR
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
    
    # --- 4. MODELO MANUAL AJUSTADO AL DESLIZADOR LATERAL ---
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
    
    # --- 5. TABLAS DE DATOS NATIVAS CON SCROLL ---
    output$tabla_original <- DT::renderDT({
      DT::datatable(datos_base(), options = list(paging = FALSE, scrollY = "400px", scrollX = TRUE), class = 'cell-border stripe compact') %>% 
        DT::formatRound(columns = names(datos_num()), digits = 3)
    })
    
    output$tabla_preparada <- DT::renderDT({
      req(datos_preparados())
      DT::datatable(round(datos_preparados(), 3), options = list(paging = FALSE, scrollY = "400px", scrollX = TRUE), class = 'cell-border stripe compact')
    })
    
    # --- 6. PESTAÑA 2: GRÁFICOS DE OPTIMIZACIÓN Y TABLAS RESCATADAS DE PCR ---
    output$cv_plot <- renderPlot({
      req(cv_fit_objeto(), input$metodo)
      cv_obj <- cv_fit_objeto()
      if(input$metodo == "pcr") {
        plot(cv_obj, "val", main = "PCR: Curva del error de validación cruzada (RMSEP)", xlab = "Número de componentes")
      } else {
        plot(cv_obj, main = paste(toupper(input$metodo), ": Evolución del MSE frente a log(Lambda)"))
      }
    })
    
    # Rescatado e integrado de tu script original: Tabla de varianza de componentes
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
    
    # Rescatado e integrado de tu script original: Summary formal del PCR
    output$summary_pcr <- renderPrint({
      req(cv_fit_objeto(), input$metodo == "pcr")
      summary(cv_fit_objeto())
    })
    
    output$interp_cv <- renderText({
      req(input$metodo)
      if(input$metodo == "pcr") {
        paste0("Para el PCR, la validación cruzada calcula el error de predicción RMSEP.\n",
               "El codo o punto más bajo del gráfico indica cuántas componentes retienen la información real eliminando el ruido.\n\n",
               "Ejemplo práctico (wines/Boston): Verás que el error disminuye drásticamente al añadir la primera y segunda componente, ",
               "estabilizándose rápidamente. Esto demuestra que con pocas componentes capturamos la variabilidad informativa de la matriz.")
      } else {
        paste0("Para Ridge/Lasso, el gráfico traza el Error Cuadrático Medio (MSE) condicionado a log(Lambda).\n",
               "La primera línea discontinua marca el lambda de mínimo error (lambda.min) y la segunda el criterio parsimonioso (lambda.1se).\n\n",
               "Ejemplo práctico (wines/Boston): El gráfico te guiará para saber qué nivel de penalización equilibra la balanza de sesgo y varianza.")
      }
    })
    
    # --- 7. PESTAÑA 3: COEFICIENTES (TU CAPTURA DE PUNTOS E INTERVALOS) ---
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
        paste0("Bajo la penalización Lasso (L1), al aumentar el Lambda verás cómo las barras de coeficientes de variables ",
               "irrelevantes colapsan y pasan a valer exactamente CERO, desapareciendo del impacto.\n\n",
               "Ejemplo práctico: Verás que Lasso conserva los descriptores clave pero elimina los redundantes, depurando el modelo.")
      } else {
        paste0("En Ridge y PCR, las variables se contraen o proyectan, pero todas permanecen en el modelo lineal sin anularse por completo.\n\n",
               "Ejemplo práctico: Permite ver el impacto relativo estandarizado de cada regresor libre de colinealidad.")
      }
    })
    
    # --- 8. PESTAÑA 4: PREDICCIÓN ---
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
        "Muestra la precisión del ajuste del modelo configurado por el usuario.\n\n",
        "Ejemplo práctico: Si el Lambda o número de componentes se ha optimizado correctamente con el paso 2, ",
        "los puntos se ceñirán con gran precisión geométrica en torno a la diagonal roja."
      )
    })
    
  }) # Cierre de moduleServer
} # Cierre de Regularizacion_Analisis_Server



# =========================================================
# REGULARIZACIÓN - AUTOEVALUACIÓN
# =========================================================

Regularizacion_Auto_UI <- function(id){
  
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


Regularizacion_Auto_Server <- function(id){
  
  moduleServer(id, function(input, output, session){
    
    preguntas <- list(
      
      list(
        texto = "¿Cuál es el objetivo principal de la regularización?",
        opciones = c(
          "a) Incrementar el número de variables",
          "b) Reducir el sobreajuste",
          "c) Eliminar observaciones",
          "d) Crear nuevas variables"
        ),
        correcta = "b) Reducir el sobreajuste"
      ),
      
      list(
        texto = "¿Qué penalización utiliza Ridge?",
        opciones = c(
          "a) L1",
          "b) L2",
          "c) Logarítmica",
          "d) Exponencial"
        ),
        correcta = "b) L2"
      ),
      
      list(
        texto = "¿Qué característica distingue a Lasso?",
        opciones = c(
          "a) No penaliza coeficientes",
          "b) Elimina variables automáticamente",
          "c) Incrementa el error",
          "d) Usa componentes principales"
        ),
        correcta = "b) Elimina variables automáticamente"
      )
    )
    
    
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
            
            p(strong(
              paste0("✔ Pregunta ", i, ": Correcto")
            ))
            
          } else {
            
            p(strong(
              paste0(
                "✘ Pregunta ",
                i,
                ": Incorrecto. Respuesta correcta: ",
                correcta
              )
            ))
          }
        })
      )
    })
    
  })
}
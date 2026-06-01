# =====================================================
#  Análisis discriminante lineal
# =====================================================


# -------------------------------
# TEORIA
# -------------------------------

LDA_Teoria_UI <- function(id) {
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
        "Análisis Discriminante Lineal (LDA)",
        style = "font-weight: 800; color: #1a365d; margin-bottom: 5px;"
      ),
      
      p(
        "Técnica de clasificación multivariante basada en el modelado de la distribución de los predictores condicionada a cada clase.",
        style = "color: #64748b; font-size: 1.1rem; margin-bottom: 30px;"
      ),
      
      # =====================================
      # TARJETAS PRINCIPALES
      # =====================================
      bslib::layout_column_wrap(
        width = 1/3, # Tres columnas perfectamente niveladas
        heights_equal = "row",
        
        # ---------------------------------
        # 1. ENFOQUE DEL LDA
        # ---------------------------------
        bslib::card(
          bslib::card_header(
            tags$b("1. Planteamiento Probabilístico"),
            style = "background: #e0e7ff;"
          ),
          bslib::card_body(
            p("Constituye una alternativa robusta a la regresión logística (James et al., 2013). En lugar de modelar la probabilidad directa de la clase, el LDA modela la distribución de los predictores en cada grupo:"),
            p("$$\\text{Enfoque: } P(\\mathbf{X}=\\mathbf{x} \\mid Y=k)$$"),
            p("A partir de esta densidad y mediante el teorema de Bayes, se deduce la probabilidad a posteriori de pertenencia.")
          )
        ),
        
        # ---------------------------------
        # 2. HIPÓTESIS DEL MODELO
        # ---------------------------------
        bslib::card(
          bslib::card_header(
            tags$b("2. Supuestos Fundamentales"),
            style = "background: #dcfce7;"
          ),
          bslib::card_body(
            p("Para estimar las funciones de densidad, el LDA se fundamenta estrictamente en dos hipótesis de partida:"),
            tags$ul(
              style = "margin-top: 10px;",
              tags$li(tags$b("Normalidad:"), " Las variables predictoras condicionadas a cada clase siguen una distribución normal multivariante.", style = "margin-bottom: 6px;"),
              tags$li(tags$b("Homocedasticidad:"), " Todas las clases comparten la misma estructura de dispersión, asumiendo una única matriz de covarianzas común: \\(\\mathbf{\\Sigma}_1 = \\dots = \\mathbf{\\Sigma}_K = \\mathbf{\\Sigma}\\).")
            )
          )
        ),
        
        # ---------------------------------
        # 3. CRITERIO DE PREFERENCIA
        # ---------------------------------
        bslib::card(
          bslib::card_header(
            tags$b("3. Ventajas frente a la Logística"),
            style = "background: #fef3c7;"
          ),
          bslib::card_body(
            p("Proporciona estimaciones de parámetros significativamente más estables en los siguientes escenarios críticos:"),
            tags$ul(
              style = "margin-top: 10px;",
              tags$li("Categorías de respuesta netamente separadas.", style = "margin-bottom: 6px;"),
              tags$li("Tamaño de muestra \\(n\\) reducido con predictores aproximadamente normales.", style = "margin-bottom: 6px;"),
              tags$li("Variables respuesta múltiples o no binarias (\\(K > 2\\)).")
            )
          )
        )
      ), # Fin del layout_column_wrap
      
      br(),
      
      # =====================================
      # CASO UNIVARIANTE VS MULTIVARIANTE
      # =====================================
      bslib::layout_column_wrap(
        width = 1/2, # Dos columnas para contrastar univariante y multivariante
        heights_equal = "row",
        
        # ---------------------------------
        # ESTIMACIÓN CASO UNIVARIANTE
        # ---------------------------------
        bslib::card(
          style = "border: 1px solid #cbd5e1; background: #f8fafc;",
          bslib::card_body(
            h4(icon("calculator"), "Caso Univariante (Un solo predictor)", style = "color: #1e40af; margin-bottom: 15px;"),
            p("Considerando un único predictor continuo \\(X\\) con varianza común \\(\\sigma^2\\), la probabilidad a posteriori se calcula mediante el teorema de Bayes a partir de densidades normales gaussianas (Hastie et al., 2009):"),
            p("$$P(Y=k \\mid X=x) = \\frac{\\pi_k \\frac{1}{\\sqrt{2\\pi}\\sigma} \\exp\\left(-\\frac{(x-\\mu_k)^2}{2\\sigma^2}\\right)}{\\sum_{l=1}^{K} \\pi_l \\frac{1}{\\sqrt{2\\pi}\\sigma} \\exp\\left(-\\frac{(x-\\mu_l)^2}{2\\sigma^2}\\right)}$$"),
            p("Donde \\(\\pi_k\\) representa la probabilidad a priori de la clase y \\(\\mu_k\\) es la media escalar del predictor en dicha clase.")
          )
        ),
        
        # ---------------------------------
        # ESTIMACIÓN CASO MULTIVARIANTE
        # ---------------------------------
        bslib::card(
          style = "border: 1px solid #cbd5e1; background: #f8fafc;",
          bslib::card_body(
            h4(icon("matrix-org"), "Caso Multivariante (Múltiples predictores)", style = "color: #1e40af; margin-bottom: 15px;"),
            p("Cuando se dispone de un vector de \\(p\\) predictores continuos \\(\\mathbf{x}\\), se asume que cada clase sigue una función de densidad normal multivariante \\(f_k(\\mathbf{x}) \\sim N(\\boldsymbol{\\mu}_k, \\mathbf{\\Sigma})\\):"),
            p("$$f_k(\\mathbf{x}) = \\frac{1}{(2\\pi)^{p/2} |\\mathbf{\\Sigma}|^{1/2}} \\exp\\left( -\\frac{1}{2} (\\mathbf{x} - \\boldsymbol{\\mu}_k)^T \\mathbf{\\Sigma}^{-1} (\\mathbf{x} - \\boldsymbol{\\mu}_k) \\right)$$"),
            p("Donde \\(\\boldsymbol{\\mu}_k\\) es el vector de medias del grupo \\(k\\), \\(|\\mathbf{\\Sigma}|\\) denota el determinante de la matriz de covarianzas común y \\(\\mathbf{\\Sigma}^{-1}\\) es su inversa.")
          )
        )
      ),
      
      br(),
      
      # =====================================
      # REGLAS DE DECISIÓN Y CLASIFICADOR
      # =====================================
      bslib::card(
        style = "border: 1px solid #cbd5e1; background: #f8fafc;",
        bslib::card_body(
          h4(icon("arrow-right-to-bracket"), "Funciones Discriminantes Lineales y Regla de Decisión", style = "color: #1e40af; margin-bottom: 15px;"),
          p("Al simplificar analíticamente el logaritmo de las probabilidades para maximizar la asignación y cancelar los términos cuadráticos comunes, se derivan las funciones discriminantes lineales para ambos casos:"),
          
          tags$div(
            style = "display: flex; flex-direction: column; gap: 15px; margin-top: 15px; margin-bottom: 20px;",
            tags$div(
              style = "border-left: 4px solid #10b981; padding-left: 12px;",
              tags$b("Función Discriminante Univariante (Escalar):"),
              p("$$\\delta_k(x) = x \\cdot \\frac{\\mu_k}{\\sigma^2} - \\frac{\\mu_k^2}{2\\sigma^2} + \\log(\\pi_k)$$")
            ),
            tags$div(
              style = "border-left: 4px solid #8b5cf6; padding-left: 12px;",
              tags$b("Función Discriminante Multivariante (Matricial):"),
              p("$$\\delta_k(\\mathbf{x}) = \\mathbf{x}^T \\mathbf{\\Sigma}^{-1} \\boldsymbol{\\mu}_k - \\frac{1}{2} \\boldsymbol{\\mu}_k^T \\mathbf{\\Sigma}^{-1} \\boldsymbol{\\mu}_k + \\log(\\pi_k)$$")
            )
          ),
          
          hr(style = "border-top: 1px solid #cbd5e1; margin: 20px 0;"),
          
          tags$div(
            style = "border-left: 4px solid #3b82f6; padding-left: 12px; background: #f0f9ff; padding: 12px; border-radius: 0 8px 8px 0;",
            tags$b(icon("circle-check"), "Regla General del Clasificador:"), 
            " Tanto en el escenario univariante como en el multivariante, el modelo asignará la nueva observación al grupo que optimice su respectiva función lineal:",
            p("$$\\text{Clase asignada} = \\arg\\max_k \\delta_k(\\mathbf{x})$$")
          )
        )
      )
    )
  )
}

LDA_Teoria_Server <- function(id){
  moduleServer(id, function(input, output, session){ })
}



# -------------------------------
# ANALISIS
# -------------------------------

# =====================================================
# LDA - UI
# =====================================================

LDA_Analisis_UI <- function(id){
  
  ns <- NS(id)
  
  tagList(
    
    h3("Análisis Discriminante Lineal (LDA)"),
    
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
          
          checkboxInput(
            ns("scale_data"),
            "Estandarizar variables",
            value = TRUE
          ),
          
          br(),
          
          actionButton(
            ns("run_lda"),
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
          
          # =====================================================
          # TAB 1
          # =====================================================
          
          tabPanel(
            
            "Espacio Discriminante",
            
            br(),
            
            plotOutput(
              ns("lda_plot"),
              height = "650px"
            )
            
          ),
          
          # =====================================================
          # TAB 2
          # =====================================================
          
          tabPanel(
            
            "Clasificación",
            
            br(),
            
            h4("Matriz de Confusión"),
            
            tableOutput(ns("conf_matrix"))
            
          ),
          
          # =====================================================
          # TAB 3
          # =====================================================
          
          tabPanel(
            
            "Coeficientes",
            
            br(),
            
            tableOutput(ns("coef_table"))
            
          ),
          
          # =====================================================
          # TAB 4
          # =====================================================
          
          tabPanel(
            
            "Métricas",
            
            br(),
            
            tableOutput(ns("metricas"))
            
          ),
          
          # =====================================================
          # TAB 5
          # =====================================================
          
          tabPanel(
            
            "Datos",
            
            br(),
            
            h4("Dataset Original"),
            
            tableOutput(ns("dataset")),
            
            br(),
            
            h4("Dataset Preparado"),
            
            tableOutput(ns("dataset_std"))
            
          )
          
        )
        
      )
      
    )
    
  )
  
}



# =====================================================
# LDA - SERVER
# =====================================================

LDA_Analisis_Server <- function(id, datos, datos_ejemplo = NULL){
  
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
      
      # Conversión inteligente
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
        "Variable Objetivo (Grupos):",
        choices = names(datos_base()),
        selected = names(datos_base())[ncol(datos_base())]
      )
      
    })
    
    
    output$predictors_ui <- renderUI({
      
      req(input$target_var)
      
      nums <- names(datos_base())[sapply(datos_base(), is.numeric)]
      
      opciones_x <- setdiff(nums, input$target_var)
      
      selectizeInput(
        
        session$ns("predictor_vars"),
        
        "Variables Predictoras:",
        
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
    # 3. DATOS PREPARADOS
    # =====================================================
    
    datos_std <- reactive({
      
      req(input$target_var,
          input$predictor_vars)
      
      df <- datos_base()[,
                         c(input$target_var,
                           input$predictor_vars)]
      
      df[[input$target_var]] <- as.factor(df[[input$target_var]])
      
      if(input$scale_data){
        
        df[, input$predictor_vars] <- scale(
          df[, input$predictor_vars]
        )
        
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
    # 5. MODELO LDA
    # =====================================================
    
    modelo_lda <- eventReactive(input$run_lda, {
      
      req(input$target_var,
          input$predictor_vars)
      
      df <- datos_std()
      
      formula_lda <- as.formula(
        
        paste(
          input$target_var,
          "~",
          paste(input$predictor_vars,
                collapse = " + ")
        )
        
      )
      
      MASS::lda(
        formula_lda,
        data = df
      )
      
    })
    
    
    
    # =====================================================
    # 6. PREDICCIONES
    # =====================================================
    
    predicciones <- reactive({
      
      req(modelo_lda())
      
      pred <- predict(modelo_lda())
      
      data.frame(
        
        Real = datos_std()[[input$target_var]],
        
        Predicho = pred$class,
        
        pred$x
        
      )
      
    })
    
    
    
    # =====================================================
    # 7. VISUALIZACIÓN
    # =====================================================
    output$lda_plot <- renderPlot({
      
      req(predicciones())
      
      df_plot <- predicciones()
      
      validate(
        need(
          "LD2" %in% colnames(df_plot),
          "El modelo solo tiene una dimensión discriminante."
        )
      )
      
      # =====================================================
      # CENTROIDES
      # =====================================================
      
      centroides <- aggregate(
        cbind(LD1, LD2) ~ Real,
        data = df_plot,
        mean
      )
      
      # =====================================================
      # GRÁFICO LDA
      # =====================================================
      
      ggplot(
        
        df_plot,
        
        aes(
          x = LD1,
          y = LD2,
          color = Real
        )
        
      ) +
        
        # -------------------------------------------------
      # ZONAS DISCRIMINANTES
      # -------------------------------------------------
      
      stat_ellipse(
        aes(fill = Real),
        geom = "polygon",
        alpha = 0.15,
        color = NA
      ) +
        
        # -------------------------------------------------
      # PUNTOS
      # -------------------------------------------------
      
      geom_point(
        size = 3,
        alpha = 0.8
      ) +
        
        # -------------------------------------------------
      # CENTROIDES
      # -------------------------------------------------
      
      geom_point(
        data = centroides,
        
        aes(
          x = LD1,
          y = LD2
        ),
        
        shape = 4,
        size = 6,
        stroke = 2,
        color = "black"
      ) +
        
        # -------------------------------------------------
      # ETIQUETAS CENTROIDES
      # -------------------------------------------------
      
      geom_text(
        
        data = centroides,
        
        aes(
          x = LD1,
          y = LD2,
          label = Real
        ),
        
        color = "black",
        
        vjust = -1,
        
        fontface = "bold",
        
        inherit.aes = FALSE
      ) +
        
        # -------------------------------------------------
      # TEMA
      # -------------------------------------------------
      
      theme_minimal(base_size = 14) +
        
        labs(
          
          title = "Espacio Discriminante",
          
          subtitle = "Representación de las funciones discriminantes",
          
          x = "LD1",
          
          y = "LD2",
          
          color = "Grupo",
          
          fill = "Grupo"
          
        ) +
        
        theme(
          
          plot.title = element_text(
            face = "bold"
          ),
          
          legend.position = "bottom"
          
        )
      
    })   

    
    
    # =====================================================
    # 8. MATRIZ CONFUSIÓN
    # =====================================================
    
    output$conf_matrix <- renderTable({
      
      req(predicciones())
      
      table(
        
        Real = predicciones()$Real,
        
        Predicho = predicciones()$Predicho
        
      )
      
    })
    
    
    
    # =====================================================
    # 9. MÉTRICAS
    # =====================================================
    
    output$metricas <- renderTable({
      
      req(predicciones())
      
      accuracy <- mean(
        predicciones()$Real ==
          predicciones()$Predicho
      )
      
      data.frame(
        
        Métrica = "Accuracy",
        
        Valor = round(accuracy, 4)
        
      )
      
    })
    
    
    
    # =====================================================
    # 10. COEFICIENTES
    # =====================================================
    
    output$coef_table <- renderTable({
      
      req(modelo_lda())
      
      round(
        as.data.frame(modelo_lda()$scaling),
        4
      )
      
    },
    rownames = TRUE)
    
  })
  
}


# -------------------------------
# AUTOEVALUACION
# -------------------------------

LDA_Auto_UI <- function(id) {
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

LDA_Auto_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # BANCO EXTENDIDO A 15 PREGUNTAS DE ANÁLISIS FACTORIAL (AF)
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


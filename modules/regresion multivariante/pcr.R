# =========================================================
# PCR - TEORÍA
# =========================================================

PCR_Teoria_UI <- function(id){
  
  ns <- NS(id)
  
  tagList(
    
    h3("PCR (Principal Component Regression) - Teoría"),
    
    br(),
    
    p("La regresión por componentes principales (PCR) combina el análisis de componentes principales (PCA) con la regresión lineal."),
    
    p("El objetivo principal es reducir la dimensionalidad del conjunto de variables explicativas antes de ajustar el modelo de regresión."),
    
    h4("Funcionamiento"),
    
    p("En primer lugar, se aplica PCA sobre las variables predictoras para obtener nuevas variables denominadas componentes principales."),
    
    p("Posteriormente, estas componentes se utilizan como variables explicativas en el modelo de regresión."),
    
    h4("Ventajas"),
    
    tags$ul(
      tags$li("Reduce problemas de multicolinealidad."),
      tags$li("Disminuye la dimensionalidad."),
      tags$li("Mejora la estabilidad del modelo.")
    ),
    
    h4("Limitaciones"),
    
    tags$ul(
      tags$li("Las componentes principales pueden resultar difíciles de interpretar."),
      tags$li("Parte de la información original puede perderse.")
    )
    
  )
}

PCR_Teoria_Server <- function(id){
  
  moduleServer(id, function(input, output, session){ })
}



# =========================================================
# PCR - ANÁLISIS
# =========================================================

PCR_Analisis_UI <- function(id){
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      
      column(
        4,
        
        wellPanel(
          
          h4("Configuración del Modelo"),
          
          uiOutput(ns("ui_var_dep")),
          
          uiOutput(ns("ui_var_indep")),
          
          hr(),
          
          sliderInput(
            ns("ncomp"),
            "Número de componentes principales:",
            min = 1,
            max = 10,
            value = 2,
            step = 1
          ),
          
          helpText("Las componentes principales sustituyen a las variables originales."),
          
          actionButton(
            ns("run_pcr"),
            "Ejecutar análisis",
            class = "btn-primary",
            width = "100%"
          )
        )
      ),
      
      column(
        8,
        
        tabsetPanel(
          
          tabPanel(
            "1. Datos",
            
            br(),
            
            h4("Dataset original"),
            tableOutput(ns("tabla_original")),
            
            h4("Dataset estandarizado"),
            tableOutput(ns("tabla_preparada"))
          ),
          
          tabPanel(
            "2. PCA",
            
            plotOutput(ns("varianza_plot")),
            
            tableOutput(ns("tabla_varianza"))
          ),
          
          tabPanel(
            "3. Resultados PCR",
            
            br(),
            
            h4("Coeficientes del modelo"),
            tableOutput(ns("tabla_coeficientes")),
            
            h4("Resumen del modelo"),
            verbatimTextOutput(ns("summary_pcr"))
          ),
          
          tabPanel(
            "4. Visualización",
            
            plotOutput(ns("pred_plot"))
          )
        )
      )
    )
  )
}



# =========================================================
# PCR - SERVER
# =========================================================

PCR_Analisis_Server <- function(id, datos, datos_ejemplo = NULL){
  
  moduleServer(id, function(input, output, session){
    
    # -----------------------------------------------------
    # DATOS
    # -----------------------------------------------------
    
    datos_base <- reactive({
      
      df <- if(!is.null(datos()) && nrow(datos()) > 0){
        datos()
      } else {
        datos_ejemplo
      }
      
      req(df)
      
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
      
      df[complete.cases(df[, sapply(df, is.numeric)]), ]
    })
    
    
    # -----------------------------------------------------
    # SELECTORES
    # -----------------------------------------------------
    
    output$ui_var_dep <- renderUI({
      
      nums <- names(datos_base())[sapply(datos_base(), is.numeric)]
      
      selectInput(
        session$ns("var_dep"),
        "Variable dependiente:",
        choices = nums
      )
    })
    
    
    output$ui_var_indep <- renderUI({
      
      req(input$var_dep)
      
      nums <- setdiff(
        names(datos_base())[sapply(datos_base(), is.numeric)],
        input$var_dep
      )
      
      selectizeInput(
        session$ns("var_indep"),
        "Variables independientes:",
        choices = nums,
        multiple = TRUE,
        selected = nums[1:min(4, length(nums))],
        options = list(
          plugins = list("remove_button")
        )
      )
    })
    
    
    # -----------------------------------------------------
    # DATOS PREPARADOS
    # -----------------------------------------------------
    
    datos_preparados <- reactive({
      
      req(input$var_dep, input$var_indep)
      
      df <- datos_base()[, c(input$var_dep, input$var_indep)]
      
      as.data.frame(scale(df))
    })
    
    
    # -----------------------------------------------------
    # MODELO PCR
    # -----------------------------------------------------
    
    modelo_pcr <- eventReactive(input$run_pcr,{
      
      req(input$var_dep, input$var_indep)
      
      formula_pcr <- as.formula(
        paste(input$var_dep, "~", paste(input$var_indep, collapse = "+"))
      )
      
      pls::pcr(
        formula_pcr,
        data = datos_base(),
        scale = TRUE,
        ncomp = input$ncomp
      )
    })
    
    
    # -----------------------------------------------------
    # PCA AUXILIAR
    # -----------------------------------------------------
    
    pca_aux <- reactive({
      
      req(input$var_indep)
      
      prcomp(
        datos_base()[, input$var_indep],
        scale. = TRUE
      )
    })
    
    
    # -----------------------------------------------------
    # TABLAS
    # -----------------------------------------------------
    
    output$tabla_original <- renderTable({
      head(datos_base(), 6)
    })
    
    output$tabla_preparada <- renderTable({
      head(datos_preparados(), 6)
    })
    
    
    output$tabla_coeficientes <- renderTable({
      
      req(modelo_pcr())
      
      co <- coef(modelo_pcr(), ncomp = input$ncomp)
      
      data.frame(
        Termino = rownames(co),
        Estimacion = as.numeric(co)
      )
      
    }, digits = 4)
    
    
    output$tabla_varianza <- renderTable({
      
      req(pca_aux())
      
      pca <- pca_aux()
      
      data.frame(
        Componente = paste0("CP", seq_along(pca$sdev)),
        Varianza = (pca$sdev^2),
        Varianza_Acumulada = cumsum(pca$sdev^2 / sum(pca$sdev^2))
      )
      
    }, digits = 4)
    
    
    # -----------------------------------------------------
    # RESUMEN
    # -----------------------------------------------------
    
    output$summary_pcr <- renderPrint({
      
      req(modelo_pcr())
      
      summary(modelo_pcr())
    })
    
    
    # -----------------------------------------------------
    # GRÁFICO VARIANZA
    # -----------------------------------------------------
    
    output$varianza_plot <- renderPlot({
      
      req(pca_aux())
      
      pca <- pca_aux()
      
      var_exp <- pca$sdev^2 / sum(pca$sdev^2)
      
      df_plot <- data.frame(
        Componente = seq_along(var_exp),
        Varianza = var_exp
      )
      
      ggplot(df_plot, aes(x = Componente, y = Varianza)) +
        geom_col(fill = "#6D9EC1") +
        geom_line() +
        geom_point(size = 2) +
        theme_minimal() +
        labs(
          title = "Varianza explicada por componente",
          x = "Componente principal",
          y = "Proporción de varianza"
        )
    })
    
    
    # -----------------------------------------------------
    # PREDICCIONES
    # -----------------------------------------------------
    
    output$pred_plot <- renderPlot({
      
      req(modelo_pcr())
      
      fit <- modelo_pcr()
      
      y_real <- datos_base()[[input$var_dep]]
      
      y_pred <- predict(
        fit,
        ncomp = input$ncomp
      )
      
      df_res <- data.frame(
        Real = y_real,
        Pred = as.numeric(y_pred)
      )
      
      ggplot(df_res, aes(x = Real, y = Pred)) +
        geom_point(color = "#2c3e50", alpha = 0.6, size = 2) +
        geom_abline(
          slope = 1,
          intercept = 0,
          color = "#e74c3c",
          linetype = "dashed"
        ) +
        theme_minimal() +
        labs(
          title = "Predicción vs realidad",
          x = "Valores reales",
          y = "Valores predichos"
        )
    })
    
  })
}



# =========================================================
# PCR - AUTOEVALUACIÓN
# =========================================================

PCR_Auto_UI <- function(id){
  
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


PCR_Auto_Server <- function(id){
  
  moduleServer(id, function(input, output, session){
    
    preguntas <- list(
      
      list(
        texto = "¿Qué significa PCR?",
        opciones = c(
          "a) Principal Component Regression",
          "b) Partial Correlation Regression",
          "c) Predictive Component Reduction",
          "d) Principal Covariance Regression"
        ),
        correcta = "a) Principal Component Regression"
      ),
      
      list(
        texto = "¿Qué técnica se aplica antes de la regresión en PCR?",
        opciones = c(
          "a) Clustering",
          "b) LDA",
          "c) PCA",
          "d) Árboles"
        ),
        correcta = "c) PCA"
      ),
      
      list(
        texto = "¿Cuál es una ventaja principal de PCR?",
        opciones = c(
          "a) Incrementar la dimensionalidad",
          "b) Reducir multicolinealidad",
          "c) Eliminar observaciones",
          "d) Evitar el escalado"
        ),
        correcta = "b) Reducir multicolinealidad"
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
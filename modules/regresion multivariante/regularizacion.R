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
    
    fluidRow(
      
      column(
        4,
        
        wellPanel(
          
          h4("Configuración del Modelo"),
          
          uiOutput(ns("ui_var_dep")),
          
          uiOutput(ns("ui_var_indep")),
          
          hr(),
          
          selectInput(
            ns("metodo"),
            "Método de Regularización:",
            choices = c(
              "Ridge (L2)" = "ridge",
              "Lasso (L1)" = "lasso"
            )
          ),
          
          sliderInput(
            ns("lambda"),
            "Valor de Penalización (Lambda):",
            min = 0,
            max = 5,
            value = 0.1,
            step = 0.05
          ),
          
          helpText("Valores altos de lambda aumentan la penalización."),
          
          actionButton(
            ns("run_reg"),
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
            "2. Colinealidad",
            
            plotOutput(ns("corr_plot")),
            
            tableOutput(ns("vif_table"))
          ),
          
          tabPanel(
            "3. Resultados",
            
            br(),
            
            uiOutput(ns("alerta_lambda")),
            
            h4("Coeficientes"),
            tableOutput(ns("tabla_coeficientes"))
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
# REGULARIZACIÓN - SERVER
# =========================================================

Regularizacion_Analisis_Server <- function(id, datos, datos_ejemplo = NULL){
  
  moduleServer(id, function(input, output, session){
    
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
        selected = nums[1:min(3, length(nums))],
        options = list(
          plugins = list("remove_button")
        )
      )
    })
    
    
    datos_preparados <- reactive({
      
      req(input$var_dep, input$var_indep)
      
      df <- datos_base()[, c(input$var_dep, input$var_indep)]
      
      as.data.frame(scale(df))
    })
    
    
    modelo_fit <- eventReactive(input$run_reg,{
      
      req(input$var_dep, input$var_indep)
      
      df <- datos_base()
      
      y <- df[[input$var_dep]]
      
      x <- as.matrix(df[, input$var_indep])
      
      alpha <- if(input$metodo == "ridge") 0 else 1
      
      glmnet::glmnet(
        x,
        y,
        alpha = alpha,
        lambda = input$lambda
      )
    })
    
    
    output$tabla_original <- renderTable({
      head(datos_base(), 6)
    })
    
    output$tabla_preparada <- renderTable({
      head(datos_preparados(), 6)
    })
    
    
    output$alerta_lambda <- renderUI({
      
      tagList(
        h5("Interpretación de Lambda"),
        
        p("Lambda regula la intensidad de la penalización."),
        
        p("En Ridge los coeficientes se reducen progresivamente."),
        
        p("En Lasso algunos coeficientes pueden convertirse en cero.")
      )
    })
    
    
    output$tabla_coeficientes <- renderTable({
      
      req(modelo_fit())
      
      co <- as.matrix(coef(modelo_fit()))
      
      data.frame(
        Termino = rownames(co),
        Estimacion = as.numeric(co)
      )
      
    }, digits = 4)
    
    
    output$corr_plot <- renderPlot({
      
      req(input$var_dep, input$var_indep)
      
      df_corr <- datos_base()[, c(input$var_dep, input$var_indep)]
      
      res_corr <- cor(df_corr, use = "complete.obs")
      
      heatmap(
        res_corr,
        col = colorRampPalette(c("#E4672E", "white", "#6D9EC1"))(20),
        symm = TRUE,
        main = "Matriz de correlación"
      )
    })
    
    
    output$vif_table <- renderTable({
      
      req(length(input$var_indep) > 1)
      
      formula_vif <- as.formula(
        paste(input$var_dep, "~", paste(input$var_indep, collapse = "+"))
      )
      
      fit <- lm(formula_vif, data = datos_base())
      
      vif_values <- car::vif(fit)
      
      data.frame(
        Variable = names(vif_values),
        VIF = as.numeric(vif_values)
      )
      
    }, digits = 2)
    
    
    output$pred_plot <- renderPlot({
      
      req(modelo_fit())
      
      fit <- modelo_fit()
      
      y_real <- datos_base()[[input$var_dep]]
      
      y_pred <- predict(
        fit,
        newx = as.matrix(datos_base()[, input$var_indep])
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
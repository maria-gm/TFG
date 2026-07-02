# =====================================================================
#  REGRESIÓN - SELECTOR MAESTRO DINÁMICO (ACTUALIZADO CON PCR)
# =====================================================================

Regresion_UI <- function(id){
  ns <- NS(id)
  
  tagList(
    selectInput(
      ns("tecnica"),
      "Seleccione técnica de regresión:",
      choices = c(
        "Visión General de la Familia" = "GENERAL",
        "Regresión múltiple" = "MULTIPLE",
        "Métodos de regularización y reducción" = "REGULARIZACION" 
      ),
      selected = "GENERAL"
    ),
    br(),
    uiOutput(ns("interfaz_maestra_dinamica"))
  )
}

Regresion_Server <- function(id, datos, datos_ejemplo = NULL){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    datos_ok <- reactive({
      if (!is.null(datos())) datos() else NULL
    })
    
    # =====================================================
    # GRÁFICOS CONCEPTUALES
    # =====================================================
    output$plot_mini_multiple <- renderPlot({
      par(mar = c(2, 2, 1.5, 1))
      set.seed(789)
      x <- runif(40, 1, 10)
      y <- 2 + 1.5 * x + rnorm(40, sd = 2)
      plot(x, y, pch = 16, col = "#cbd5e1", xlab = "", ylab = "", axes = FALSE, main = "Predicción Continua (Y)", col.main = "#1a365d", cex.main = 0.9)
      box(col = "#e2e8f0")
      mod <- lm(y ~ x)
      abline(mod, col = "#3b82f6", lwd = 3)
      yhat <- predict(mod)
      segments(x, y, x, yhat, col = rgb(239/255, 68/255, 68/255, 0.4))
    })
        output$plot_mini_regularizacion <- renderPlot({
      par(mar = c(3.2, 1.5, 2, 1.5), mgp = c(1.2, 0.3, 0))
      
      lambda_seq <- seq(0, 5, length.out = 100)
      
      plot(1, type = "n", xlab = "", ylab = "", 
           xlim = c(-0.2, 5.2), ylim = c(-2.5, 3.5), axes = FALSE, 
           main = "Regularización vs. Reducción", col.main = "#b45309", cex.main = 0.85)
      
      abline(h = 0, lty = "dotted", col = "#cbd5e1", lwd = 1.5)
      
      box(col = "#e2e8f0")
      
      lines(lambda_seq, 2.8 * exp(-0.4 * lambda_seq), col = "#f59e0b", lwd = 2.5)   
      lines(lambda_seq, 1.2 * exp(-0.25 * lambda_seq), col = "#b45309", lwd = 2.5) 
      lines(lambda_seq, -1.8 * exp(-0.5 * lambda_seq), col = "#7c2d12", lwd = 2.5) 
      
      abline(v = 2.5, col = "#3b82f6", lty = "dashed", lwd = 2)
      

      text(2.5, 2.9, "Corte PCR\n(Nº Componentes)", col = "#1d4ed8", font = 2, cex = 0.65, adj = 0.5)
      
      text(0.1, 2.0, "Coeficientes\n(Ridge/Lasso)", col = "#b45309", font = 2, cex = 0.65, adj = 0)
      

      axis(1, at = c(0.6, 4.4), labels = c("Modelo Complejo", "Modelo Simple"), 
           col = "transparent", col.axis = "#64748b", cex.axis = 0.75, padj = 0.2)
    })
    
    # =====================================================
    # INTERFAZ 
    # =====================================================
    output$interfaz_maestra_dinamica <- renderUI({
      req(input$tecnica)
      
      if (input$tecnica == "GENERAL") {
        return(
          tags$div(
            style = "padding: 10px;",
            h3("La Familia de Modelos de Regresión", style = "font-weight: 700; color: #1a365d;"),
            p("Técnicas de aprendizaje supervisado para modelar, predecir e inferir la relación entre variables explicativas y una variable respuesta:", style = "color: #64748b;"),
            br(),
            bslib::layout_column_wrap(
              width = 1/3,
              heights_equal = "row",
              bslib::card(
                style = "border-top: 4px solid #3b82f6;",
                bslib::card_header(tags$b("Regresión Múltiple")),
                bslib::card_body(p("Modela una variable continua mediante múltiples variables predictoras, minimizando la suma de los errores al cuadrado."), plotOutput(ns("plot_mini_multiple"), height = "135px"))
              ),

              bslib::card(
                style = "border-top: 4px solid #f59e0b;",
                bslib::card_header(tags$b("Métodos de Regularización y Reducción")),
                bslib::card_body(p("Técnicas que restringen los coeficientes (Ridge/Lasso) o simplifican las variables (PCR) para corregir la alta correlación entre los datos."), plotOutput(ns("plot_mini_regularizacion"), height = "135px"))
              )
            )
          )
        )
      }
      
      if (input$tecnica == "MULTIPLE") {
        Regresion_multiple_Teoria_Server("regresion_multiple_teoria")
        Regresion_multiple_Analisis_Server("regresion_multiple_analisis", datos = reactive({ if(!is.null(datos_ok())) datos_ok() else datos_ejemplo$Regresion_multiple }), datos_ejemplo = datos_ejemplo)
        Regresion_multiple_Auto_Server("regresion_multiple_auto")
        return(tabsetPanel(tabPanel("Teoría", Regresion_multiple_Teoria_UI(ns("regresion_multiple_teoria"))), tabPanel("Análisis", Regresion_multiple_Analisis_UI(ns("regresion_multiple_analisis"))), tabPanel("Autoevaluación", Regresion_multiple_Auto_UI(ns("regresion_multiple_auto")))))
      }
      
      
      if (input$tecnica == "REGULARIZACION") {
        Regularizacion_Teoria_Server("regularizacion_teoria")
        Regularizacion_Analisis_Server("regularizacion_analisis", datos = reactive({ if(!is.null(datos_ok())) datos_ok() else datos_ejemplo$Regularizacion }), datos_ejemplo = datos_ejemplo)
        Regularizacion_Auto_Server("regularizacion_auto")
        return(tabsetPanel(tabPanel("Teoría", Regularizacion_Teoria_UI(ns("regularizacion_teoria"))), tabPanel("Análisis", Regularizacion_Analisis_UI(ns("regularizacion_analisis"))), tabPanel("Autoevaluación", Regularizacion_Auto_UI(ns("regularizacion_auto")))))
      }
    })
    
    observeEvent(session$userData$reset_regresion, { updateSelectInput(session, "tecnica", selected = "GENERAL") }, ignoreInit = TRUE)
  })
}


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
        "Regresión logística" = "LOGISTICA",
        "Métodos de regularización y reducción" = "REGULARIZACION" # <-- Nombre cambiado
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
    # RENDERIZACIÓN DE MINIGRÁFICOS CONCEPTUALES
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
    
    output$plot_mini_logistica <- renderPlot({
      par(mar = c(2, 2, 1.5, 1))
      set.seed(101)
      x <- runif(50, -4, 4)
      p_real <- 1 / (1 + exp(-2 * x))
      y <- rbinom(50, 1, p_real)
      plot(x, y, pch = 21, bg = ifelse(y==1, "#a7f3d0", "#fecaca"), col = ifelse(y==1, "#047857", "#b91c1c"), xlab = "", ylab = "", axes = FALSE, main = "Probabilidad Binaria (0 / 1)", col.main = "#047857", cex.main = 0.9)
      box(col = "#e2e8f0")
      curve(1 / (1 + exp(-2 * x)), add = TRUE, col = "#10b981", lwd = 3)
      abline(h = 0.5, lty = 3, col = "#64748b")
    })
    
    # Gráfico que incluye la traza o corte característico de PCR (Reducción por componentes)
    output$plot_mini_regularizacion <- renderPlot({
      par(mar = c(2, 2, 2, 1))
      lambda_seq <- seq(0, 5, length.out = 50)
      plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 5), ylim = c(-2, 3), axes = FALSE, main = "Encogimiento y Selección (PCR/Lasso)", col.main = "#b45309", cex.main = 0.9)
      box(col = "#e2e8f0")
      lines(lambda_seq, 3 * exp(-lambda_seq), col = "#f59e0b", lwd = 2.5)
      lines(lambda_seq, -1.8 * exp(-1.5 * lambda_seq), col = "#d97706", lwd = 2)
      lines(lambda_seq, 1.2 * exp(-0.8 * lambda_seq), col = "#b45309", lwd = 2)
      # Línea vertical indicativa que emula la selección discreta de componentes en PCR
      abline(v = 2.2, col = "#3b82f6", linetype = 2, lwd = 2)
      text(2.4, 2.5, "Corte PCR", col = "#1e3a8a", font = 2, cex = 0.8, adj = 0)
      abline(h = 0, lty = "dashed", col = "#94a3b8")
    })
    
    # =====================================================
    # INTERFAZ MAESTRA ATÓMICA
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
                bslib::card_body(p("Modela una variable respuesta continua mediante múltiples predictores cuantitativos minimizando los residuos cuadrados ordinarios."), plotOutput(ns("plot_mini_multiple"), height = "135px"))
              ),
              bslib::card(
                style = "border-top: 4px solid #10b981;",
                bslib::card_header(tags$b("Regresión Logística")),
                bslib::card_body(p("Predice la probabilidad de ocurrencia de un suceso binario o categórico evaluando el logaritmo de la ventaja."), plotOutput(ns("plot_mini_logistica"), height = "135px"))
              ),
              bslib::card(
                style = "border-top: 4px solid #f59e0b;",
                bslib::card_header(tags$b("Métodos de Regularización y Reducción")),
                bslib::card_body(p("Introduce restricciones geométricas (Ridge/Lasso) o proyecciones en subespacios (PCR) para combatir la multicolinealidad."), plotOutput(ns("plot_mini_regularizacion"), height = "135px"))
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
      
      if (input$tecnica == "LOGISTICA") {
        Regresion_logistica_Teoria_Server("regresion_logistica_teoria")
        Regresion_logistica_Analisis_Server("regresion_logistica_analisis", datos = reactive({ if(!is.null(datos_ok())) datos_ok() else datos_ejemplo$Regresion_logistica }), datos_ejemplo = datos_ejemplo)
        Regresion_logistica_Auto_Server("regresion_logistica_auto")
        return(tabsetPanel(tabPanel("Teoría", Regresion_logistica_Teoria_UI(ns("regresion_logistica_teoria"))), tabPanel("Análisis", Regresion_logistica_Analisis_UI(ns("regresion_logistica_analisis"))), tabPanel("Autoevaluación", Regresion_logistica_Auto_UI(ns("regresion_logistica_auto")))))
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


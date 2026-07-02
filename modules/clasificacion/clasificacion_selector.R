# =====================================================================
#  CLASIFICACIÓN - SELECTOR MAESTRO DINÁMICO (CON TABS CONDICIONALES)
# =====================================================================

Clasificacion_UI <- function(id){
  ns <- NS(id)
  
  tagList(
    selectInput(
      ns("tecnica"),
      "Seleccione técnica de clasificación:",
      choices = c(
        "Visión General de la Familia" = "GENERAL",
        "Regresión logística"="LOGISTICA",
        "LDA (Análisis Discriminante Lineal)" = "LDA",
        "Árboles de decisión" = "ARBOLES"
      ),
      selected = "GENERAL"
    ),
    br(),
    uiOutput(ns("interfaz_maestra_dinamica"))
  )
}

Clasificacion_Server <- function(id, datos, datos_ejemplo = NULL){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    datos_ok <- reactive({
      if (!is.null(datos())) datos() else NULL
    })
    
    # =====================================================
    # RENDERIZACIÓN DE MINIGRÁFICOS CONCEPTUALES
    # =====================================================
    # Regresión logística
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
    # 1. Gráfico de LDA (Dos campanas de Gauss con la frontera de decisión lineal óptima)
    output$plot_mini_lda <- renderPlot({
      par(mar = c(2, 2, 1.5, 1))
      curve(dnorm(x, mean = -1.5, sd = 1), from = -5, to = 5, col = "#3b82f6", lwd = 2.5, axes = FALSE, xlab = "", ylab = "", main = "Separación Lineal Óptima", col.main = "#1a365d", cex.main = 0.9)
      box(col = "#e2e8f0")
      curve(dnorm(x, mean = 1.5, sd = 1), from = -5, to = 5, col = "#10b981", lwd = 2.5, add = TRUE)
      # Frontera divisoria equidistante
      abline(v = 0, col = "#ef4444", lwd = 2, lty = 2)
      text(-2.5, 0.15, "Clase A", col = "#1e3a8a", font = 2, cex = 0.8)
      text(2.5, 0.15, "Clase B", col = "#047857", font = 2, cex = 0.8)
    })
    
    # 2. Gráfico de Árboles (Estructura jerárquica de decisión Sí/No)
    output$plot_mini_arboles <- renderPlot({
      par(mar = c(1, 1, 1.5, 1))
      plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 10), ylim = c(0, 10), axes = FALSE, main = "Particiones de Espacio", col.main = "#047857", cex.main = 0.9)
      box(col = "#e2e8f0")
      # Nodo Raíz
      rect(3.5, 8, 6.5, 9.8, border = "#3b82f6", col = "#eff6ff", lwd = 2)
      text(5, 8.9, "¿Variable X > 5?", col = "#1e3a8a", font = 2, cex = 0.8)
      # Ramas
      arrows(4, 8, 2, 5, length = 0.08, col = "#64748b")
      arrows(6, 8, 8, 5, length = 0.08, col = "#64748b")
      text(2.5, 6.8, "Sí", col = "#047857", cex = 0.8)
      text(7.5, 6.8, "No", col = "#b91c1c", cex = 0.8)
      # Nodos Hoja
      rect(0.5, 3.2, 3.5, 5, border = "#10b981", col = "#ecfdf5")
      text(2, 4.1, "Clase Verde", col = "#047857", cex = 0.8)
      rect(6.5, 3.2, 9.5, 5, border = "#ef4444", col = "#fdf2f2")
      text(8, 4.1, "Clase Roja", col = "#b91c1c", cex = 0.8)
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
            h3("La Familia de Técnicas de Clasificación", style = "font-weight: 700; color: #1a365d;"),
            p("Algoritmos de aprendizaje supervisado que clasifican elementos en categorías específicas a partir de sus características o variables.", style = "color: #64748b;"),
            br(),
            bslib::layout_column_wrap(
              width = 1/3, 
              heights_equal = "row",
              bslib::card(
                style = "border-top: 4px solid #3b82f6;",
                bslib::card_header(tags$b("Regresión Logística")),
                bslib::card_body(p("Modela la probabilidad de que una observación pertenezca a una categoría (usualmente binaria) mediante una función sigmoide."), plotOutput(ns("plot_mini_logistica"), height = "135px"))
              ),
              bslib::card(
                style = "border-top: 4px solid #3b82f6;",
                bslib::card_header(tags$b("LDA (Análisis Discriminante Lineal)")),
                bslib::card_body(p("Método estadístico que asume datos con varianza constante para encontrar la función lineal óptima que maximiza la separación entre clases."), plotOutput(ns("plot_mini_lda"), height = "135px"))
              ),
              bslib::card(
                style = "border-top: 4px solid #10b981;",
                bslib::card_header(tags$b("Árboles de Decisión")),
                bslib::card_body(p("Modelo basado en reglas condicionales que divide los datos de forma sucesiva buscando la máxima homogeneidad en cada grupo."), plotOutput(ns("plot_mini_arboles"), height = "135px"))
              )
            )
          )
        )
      }
      
      # Sincronización exacta con tus nombres de funciones reales (`LDA_teoria` y `Arboles_teoria`)

      if (input$tecnica == "LOGISTICA") {
        Regresion_logistica_Teoria_Server("regresion_logistica_teoria")
        Regresion_logistica_Analisis_Server("regresion_logistica_analisis", datos = reactive({ if(!is.null(datos_ok())) datos_ok() else datos_ejemplo$Regresion_logistica }), datos_ejemplo = datos_ejemplo)
        Regresion_logistica_Auto_Server("regresion_logistica_auto")
        return(tabsetPanel(tabPanel("Teoría", Regresion_logistica_Teoria_UI(ns("regresion_logistica_teoria"))), tabPanel("Análisis", Regresion_logistica_Analisis_UI(ns("regresion_logistica_analisis"))), tabPanel("Autoevaluación", Regresion_logistica_Auto_UI(ns("regresion_logistica_auto")))))
      }
      if (input$tecnica == "LDA") {
        LDA_Teoria_Server("LDA_teoria")
        LDA_Analisis_Server("LDA_analisis", datos = reactive({ if(!is.null(datos_ok())) datos_ok() else datos_ejemplo$LDA }), datos_ejemplo = datos_ejemplo)
        LDA_Auto_Server("LDA_auto")
        return(tabsetPanel(tabPanel("Teoría", LDA_Teoria_UI(ns("LDA_teoria"))), tabPanel("Análisis", LDA_Analisis_UI(ns("LDA_analisis"))), tabPanel("Autoevaluación", LDA_Auto_UI(ns("LDA_auto")))))
      }
      
      if (input$tecnica == "ARBOLES") {
        Arboles_Teoria_Server("Arboles_teoria")
        Arboles_Analisis_Server("Arboles_analisis", datos = reactive({ if(!is.null(datos_ok())) datos_ok() else datos_ejemplo$Arboles }), datos_ejemplo = datos_ejemplo)
        Arboles_Auto_Server("Arboles_auto")
        return(tabsetPanel(tabPanel("Teoría", Arboles_Teoria_UI(ns("Arboles_teoria"))), tabPanel("Análisis", Arboles_Analisis_UI(ns("Arboles_analisis"))), tabPanel("Autoevaluación", Arboles_Auto_UI(ns("Arboles_auto")))))
      }
    })
    
    observeEvent(session$userData$reset_clasificacion, { updateSelectInput(session, "tecnica", selected = "GENERAL") }, ignoreInit = TRUE)
  })
}

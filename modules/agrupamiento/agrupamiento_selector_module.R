# =====================================================================
#  AGRUPAMIENTO - SELECTOR 
# =====================================================================

Agrupamiento_UI <- function(id){
  ns <- NS(id)
  
  tagList(
    selectInput(
      ns("tecnica"),
      "Seleccione algoritmo de agrupamiento:",
      choices = c(
        "Visión General de la Familia" = "GENERAL", 
        "Clústeres Jerárquicos" = "Jerarquicos", 
        "K-Means" = "K_means", 
        "DBSCAN" = "DBSCAN"
      ),
      selected = "GENERAL"
    ),
    br(),
    uiOutput(ns("interfaz_maestra_dinamica"))
  )
}

Agrupamiento_Server <- function(id, datos, datos_ejemplo = NULL){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    datos_ok <- reactive({
      if (!is.null(datos())) datos() else NULL
    })
    
    # =====================================================
    # 1. INSTANCIACIÓN FIJA DE SERVIDORES (FUERA DE RENDERUI)
    # =====================================================
    # Se inicializan una sola vez y quedan a la escucha de forma limpia e independiente
    
    # Servidores Jerárquicos
    Jerarquicos_Teoria_Server("jerarquico_teoria")
    Jerarquicos_Analisis_Server("jerarquico_analisis", 
                                datos = reactive({ if (!is.null(datos_ok())) datos_ok() else datos_ejemplo$Jerarquicos }), 
                                datos_ejemplo = datos_ejemplo)
    Jerarquicos_Auto_Server("jerarquico_auto")
    
    # Servidores K-Means
    K_means_Teoria_Server("kmeans_teoria")
    K_means_Analisis_Server("kmeans_analisis", 
                            datos = reactive({ if (!is.null(datos_ok())) datos_ok() else datos_ejemplo$K_means }), 
                            datos_ejemplo = datos_ejemplo)
    K_means_Auto_Server("kmeans_auto")
    
    # Servidores DBSCAN
    DBSCAN_Teoria_Server("dbscan_teoria")
    DBSCAN_Analisis_Server("dbscan_analisis", 
                           datos = reactive({ if (!is.null(datos_ok())) datos_ok() else datos_ejemplo$DBSCAN }), 
                           datos_ejemplo = datos_ejemplo)
    DBSCAN_Auto_Server("dbscan_auto")
    
    
    # =====================================================
    # 2. RENDERIZACIÓN DE MINIGRÁFICOS CONCEPTUALES
    # =====================================================
    output$plot_mini_jerarquico <- renderPlot({
      par(mar = c(2, 2, 1.5, 1))
      plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 10), ylim = c(0, 10), axes = FALSE, main = "Dendrograma (Árbol)", col.main = "#1e3a8a", cex.main = 0.9)
      box(col = "#e2e8f0")
      lines(c(2, 2, 4, 4), c(0, 4, 4, 0), col = "#3b82f6", lwd = 2)
      lines(c(6, 6, 8, 8), c(0, 6, 6, 0), col = "#3b82f6", lwd = 2)
      lines(c(3, 3, 7, 7), c(4, 9, 9, 6), col = "#93c5fd", lwd = 2)
      lines(c(5, 5), c(9, 10), col = "#1e40af", lwd = 2)
      points(c(2, 4, 6, 8), c(0, 0, 0, 0), pch = 21, bg = "white", col = "#1e3a8a", cex = 1.2)
    })
    
    output$plot_mini_kmeans <- renderPlot({
      par(mar = c(2, 2, 1.5, 1))
      set.seed(123)
      g1_x <- rnorm(30, mean = 2, sd = 0.5); g1_y <- rnorm(30, mean = 2, sd = 0.5)
      g2_x <- rnorm(30, mean = 6, sd = 0.5); g2_y <- rnorm(30, mean = 6, sd = 0.5)
      plot(c(g1_x, g2_x), c(g1_y, g2_y), col = c(rep("#a7f3d0", 30), rep("#93c5fd", 30)), pch = 16, xlab = "", ylab = "", axes = FALSE, main = "Partición por Proximidad", col.main = "#047857", cex.main = 0.9)
      box(col = "#e2e8f0")
      points(2, 2, pch = 4, col = "#10b981", lwd = 4, cex = 1.8)
      points(6, 6, pch = 4, col = "#3b82f6", lwd = 4, cex = 1.8)
    })
    
    output$plot_mini_dbscan <- renderPlot({
      par(mar = c(2, 2, 1.5, 1))
      set.seed(456)
      cx <- seq(1, 5, length.out = 40); cy <- sin(cx) + rnorm(40, sd = 0.15)
      rx <- c(1.5, 4.2, 5.5); ry <- c(3.5, -2.1, 1.2)
      plot(c(cx, rx), c(cy, ry), col = c(rep("#fde047", 40), rep("#64748b", 3)), pch = 16, xlab = "", ylab = "", axes = FALSE, main = "Densidad y Ruido", col.main = "#b45309", cex.main = 0.9)
      box(col = "#e2e8f0")
      symbols(c(2.5, 4), c(sin(2.5), sin(4)), circles = c(0.4, 0.4), inches = FALSE, add = TRUE, fg = "#f59e0b", lty = 2)
    })
    
    # =====================================================
    # 3. INTERFAZ DINÁMICA (SÓLO INTERCAMBIA HTML / VISTAS)
    # =====================================================
    output$interfaz_maestra_dinamica <- renderUI({
      req(input$tecnica)
      
      # 1. VISIÓN GENERAL DE LA FAMILIA
      if (input$tecnica == "GENERAL") {
        return(
          tags$div(
            style = "padding: 10px;",
            h3("La Familia de Algoritmos de Agrupamiento (Clustering)", style = "font-weight: 700; color: #1a365d;"),
            p("Técnicas de aprendizaje no supervisado para identificar agrupaciones naturales y patrones ocultos en tus datos:", style = "color: #64748b;"),
            br(),
            bslib::layout_column_wrap(
              width = 1/3,
              heights_equal = "row",
              bslib::card(
                style = "border-top: 4px solid #3b82f6;",
                bslib::card_header(tags$b("Clústeres Jerárquicos")),
                bslib::card_body(p("Construye una jerarquía de grupos de forma ascendente reflejada en un árbol."), plotOutput(ns("plot_mini_jerarquico"), height = "135px"))
              ),
              bslib::card(
                style = "border-top: 4px solid #10b981;",
                bslib::card_header(tags$b("Algoritmo K-Means")),
                bslib::card_body(p("Divide los datos particionando el espacio en K grupos esféricos."), plotOutput(ns("plot_mini_kmeans"), height = "135px"))
              ),
              bslib::card(
                style = "border-top: 4px solid #f59e0b;",
                bslib::card_header(tags$b("Algoritmo DBSCAN")),
                bslib::card_body(p("Agrupa por regiones de alta densidad conectada, aislando el ruido."), plotOutput(ns("plot_mini_dbscan"), height = "135px"))
              )
            )
          )
        )
      }
      
      # 2. CLÚSTERES JERÁRQUICOS
      if (input$tecnica == "Jerarquicos") {
        return(
          tabsetPanel(
            tabPanel("Teoría", Jerarquicos_Teoria_UI(ns("jerarquico_teoria"))),
            tabPanel("Análisis", Jerarquicos_Analisis_UI(ns("jerarquico_analisis"))),
            tabPanel("Autoevaluación", Jerarquicos_Auto_UI(ns("jerarquico_auto")))
          )
        )
      }
      
      # 3. K-MEANS
      if (input$tecnica == "K_means") {
        return(
          tabsetPanel(
            tabPanel("Teoría", K_means_Teoria_UI(ns("kmeans_teoria"))),
            tabPanel("Análisis", K_means_Analisis_UI(ns("kmeans_analisis"))),
            tabPanel("Autoevaluación", K_means_Auto_UI(ns("kmeans_auto")))
          )
        )
      }
      
      # 4. DBSCAN
      if (input$tecnica == "DBSCAN") {
        return(
          tabsetPanel(
            tabPanel("Teoría", DBSCAN_Teoria_UI(ns("dbscan_teoria"))),
            tabPanel("Análisis", DBSCAN_Analisis_UI(ns("dbscan_analisis"))),
            tabPanel("Autoevaluación", DBSCAN_Auto_UI(ns("dbscan_auto")))
          )
        )
      }
    })
    
    observeEvent(session$userData$reset_agrupamiento, {
      updateSelectInput(session, "tecnica", selected = "GENERAL")
    }, ignoreInit = TRUE)
  })
}

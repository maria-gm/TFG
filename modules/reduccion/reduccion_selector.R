# =====================================================
#  REDUCCIÓN - SELECTOR GENERAL CON TABS CONDICIONALES
# =====================================================

Reduccion_UI <- function(id){
  ns <- NS(id)
  
  tagList(
    selectInput(
      ns("tecnica"),
      "Seleccione técnica:",
      choices = c("Visión General de la Familia" = "GENERAL", "PCA", "AF", "BIPLOT"),
      selected = "GENERAL"
    ),
    br(),
    # Este contenedor inyectará de forma dinámica la interfaz correcta
    uiOutput(ns("interfaz_dinamica_ui"))
  )
}

Reduccion_Server <- function(id, datos, datos_ejemplo = NULL){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    datos_ok <- reactive({
      if (!is.null(datos())) datos() else NULL
    })
    
    # =====================================================
    # GRÁFICOS  PARA LA PORTADA
    # =====================================================
    output$plot_mini_pca <- renderPlot({
      par(mar = c(2, 2, 1.5, 1))
      set.seed(42)
      x <- rnorm(80)
      y <- 0.7 * x + rnorm(80, sd = 0.4)
      plot(x, y, pch = 16, col = "#cbd5e1", xlab = "", ylab = "", axes = FALSE, main = "Componentes principales", col.main = "#1e3a8a", cex.main = 0.9)
      box(col = "#e2e8f0")
      abline(a = 0, b = 0.7, col = "#3b82f6", lwd = 3)
      abline(a = 0, b = -1/0.7, col = "#93c5fd", lwd = 2)
    })
    
    output$plot_mini_af <- renderPlot({
      par(mar = c(1, 1, 1.5, 1))
      plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 10), ylim = c(0, 10), axes = FALSE, main = "Variables Latentes (Factores) ", col.main = "#065f46", cex.main = 0.9)
      box(col = "#e2e8f0")
      symbols(5, 8, circles = 1.2, inches = FALSE, add = TRUE, fg = "#10b981", bg = "#e6f4ea", lwd = 2)
      text(5, 8, "Factor F", col = "#065f46", font = 2, cex = 0.9)
      rect(c(1, 4, 7), 2, c(3, 6, 9), 4, border = "#a7f3d0", col = "#f0fdf4", lwd = 1.5)
      text(c(2, 5, 8), 3, c("X1", "X2", "X3"), col = "#047857")
      arrows(c(4.2, 5, 5.8), 6.6, c(2.2, 5, 7.8), 4.2, length = 0.1, col = "#10b981", lwd = 2)
    })
    
    output$plot_mini_biplot <- renderPlot({
      par(mar = c(2, 2, 1.5, 1))
      set.seed(12)
      px <- rnorm(30, mean = 2, sd = 1)
      py <- rnorm(30, mean = 2, sd = 1)
      plot(px, py, pch = 20, col = rgb(0.6, 0.6, 0.6, 0.5), xlab = "", ylab = "", axes = FALSE, xlim = c(-1, 5), ylim = c(-1, 5), main = "Individuos y variables", col.main = "#92400e", cex.main = 0.9)
      box(col = "#e2e8f0")
      arrows(0, 0, c(3.5, 1, 4), c(1.5, 3.8, 3.5), length = 0.08, col = "#f59e0b", lwd = 2.5)
      text(c(3.8, 1, 4.3), c(1.5, 4.1, 3.5), c("V1", "V2", "V3"), col = "#78350f", font = 2, cex = 0.8)
    })
    
    # =====================================================
    # ARQUITECTURA DINÁMICA DE LA INTERFAZ
    # =====================================================
    output$interfaz_dinamica_ui <- renderUI({
      req(input$tecnica)
      
      # CASO A: Si selecciona la visión general, NO se generan pestañas
      if (input$tecnica == "GENERAL") {
        return(
          tags$div(
            style = "padding: 10px;",
            h3("La Familia de Reducción de Dimensionalidad", style = "font-weight: 700; color: #1a365d;"),
            p("Técnicas que disminuyen el número de variables en un conjunto de datos, conservando la información esencial para mejorar la capacidad de generalización. ", style = "color: #64748b;"),
            br(),
            bslib::layout_column_wrap(
              width = 1/3,
              heights_equal = "row",
              bslib::card(
                style = "border-top: 4px solid #3b82f6;",
                bslib::card_header(tags$b("PCA (Componentes Principales)")),
                bslib::card_body(
                  p("El PCA reduce la dimensionalidad de los datos transformándolos en un nuevo conjunto de ejes ortogonales ordenados por la cantidad de varianza que capturan."),
                  plotOutput(ns("plot_mini_pca"), height = "140px")
                )
              ),
              bslib::card(
                style = "border-top: 4px solid #10b981;",
                bslib::card_header(tags$b("AF (Análisis Factorial)")),
                bslib::card_body(
                  p("Agrupa las variables que están estrechamente relacionadas en dimensiones subyacentes más simples, llamadas factores"),
                  plotOutput(ns("plot_mini_af"), height = "140px")
                )
              ),
              bslib::card(
                style = "border-top: 4px solid #f59e0b;",
                bslib::card_header(tags$b("BIPLOT (Visualización)")),
                bslib::card_body(
                  p("Realiza una representación  simultánea de individuos y variables en un espacio de baja dimensionalidad"),
                  plotOutput(ns("plot_mini_biplot"), height = "140px")
                )
              )
            )
          )
        )
      }
      
      # CASO B: Si se selecciona una técnica, pintamos el contenedor estructurado con las pestañas
      tabsetPanel(
        tabPanel("Teoría", uiOutput(ns("teoria_ui"))),
        tabPanel("Análisis", uiOutput(ns("analisis_ui"))),
        tabPanel("Autoevaluación", uiOutput(ns("auto_ui")))
      )
    })
    
    # =====================================================
    # CONTROLADOR DE EVENTOS Y ENRUTAMIENTO DE SUB-MÓDULOS
    # =====================================================
    observeEvent(input$tecnica, {
      req(input$tecnica)
      
      if (input$tecnica == "GENERAL") return()
      
      if (input$tecnica == "PCA") {
        output$teoria_ui   <- renderUI({ PCA_Teoria_UI(ns("pca_teoria")) })
        output$analisis_ui <- renderUI({ PCA_Analisis_UI(ns("pca_analisis")) })
        output$auto_ui     <- renderUI({ PCA_Auto_UI(ns("pca_auto")) })
        
        PCA_Teoria_Server("pca_teoria")
        PCA_Analisis_Server("pca_analisis", 
                            datos = reactive({ if (!is.null(datos_ok())) datos_ok() else datos_ejemplo$PCA }), 
                            datos_ejemplo = datos_ejemplo)
        PCA_Auto_Server("pca_auto")
      }
      
      if (input$tecnica == "AF") {
        output$teoria_ui   <- renderUI({ AF_Teoria_UI(ns("af_teoria")) })
        output$analisis_ui <- renderUI({ AF_Analisis_UI(ns("af_analisis")) })
        output$auto_ui     <- renderUI({ AF_Auto_UI(ns("af_auto")) })
        
        AF_Teoria_Server("af_teoria")
        AF_Analisis_Server("af_analisis", 
                           datos = reactive({ if (!is.null(datos_ok())) datos_ok() else datos_ejemplo$AF }), 
                           datos_ejemplo = datos_ejemplo)
        AF_Auto_Server("af_auto")
      }
      
      if (input$tecnica == "BIPLOT") {
        output$teoria_ui   <- renderUI({ BIPLOT_Teoria_UI(ns("biplot_teoria")) })
        output$analisis_ui <- renderUI({ BIPLOT_Analisis_UI(ns("biplot_analisis")) })
        output$auto_ui     <- renderUI({ BIPLOT_Auto_UI(ns("biplot_auto")) })
        
        BIPLOT_Teoria_Server("biplot_teoria")
        BIPLOT_Analisis_Server("biplot_analisis", 
                               datos = reactive({ if (!is.null(datos_ok())) datos_ok() else datos_ejemplo$BIPLOT }), 
                               datos_ejemplo = datos_ejemplo)
        BIPLOT_Auto_Server("biplot_auto")
      }
    }, ignoreInit = FALSE)
    
    # Reset al pulsar el menú lateral izquierdo principal de la app
    observeEvent(session$userData$reset_reduccion, {
      updateSelectInput(session, "tecnica", selected = "GENERAL")
    }, ignoreInit = TRUE)
  })
}
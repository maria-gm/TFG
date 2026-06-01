# =====================================================
# AGRUPAMIENTO - SELECTOR FINAL
# =====================================================

Agrupamiento_UI <- function(id){
  ns <- NS(id)
  
  tagList(
    h2("Técnicas de agrupamiento"),
    
    selectInput(
      ns("tecnica"),
      "Selecciona la técnica",
      choices = c("K-means", "Jerarquicos", "DBSCAN"),
      selected = "K-means"
    ),
    
    br(),
    
    tabsetPanel(
      tabPanel("Teoría", uiOutput(ns("teoria_ui"))),
      tabPanel("Análisis", uiOutput(ns("analisis_ui"))),
      tabPanel("Autoevaluación", uiOutput(ns("auto_ui")))
  ))
}

Agrupamiento_Server <- function(id, datos, datos_ejemplo = NULL){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    renderContenido <- function(tecnica){
      
      # ==========================
      # k_means
      # ==========================
      if(tecnica == "K-means"){
        # UI
        output$teoria_ui  <- renderUI({ K_means_Teoria_UI(ns("k_means_teoria")) })
        output$analisis_ui <- renderUI({ K_means_Analisis_UI(ns("k_means_analisis")) })
        output$auto_ui    <- renderUI({ K_means_Auto_UI(ns("k_means_auto")) })
        
        # Servidores
        K_means_Teoria_Server("k_means_teoria")
        K_means_Analisis_Server("k_means_analisis", datos = reactive({
          if(!is.null(datos())) datos() else datos_ejemplo$K_means
        }), datos_ejemplo = datos_ejemplo)
        K_means_Auto_Server("k_means_auto")
      }
      
      # ==========================
      # Jerarquicos
      # ==========================
      if(tecnica == "Jerarquicos"){
        # UI
        output$teoria_ui  <- renderUI({ Jerarquicos_Teoria_UI(ns("Jerarquicos_teoria")) })
        output$analisis_ui <- renderUI({ Jerarquicos_Analisis_UI(ns("Jerarquicos_analisis")) })
        output$auto_ui    <- renderUI({ Jerarquicos_Auto_UI(ns("Jerarquicos_auto")) })
        
        # Servidores
        Jerarquicos_Teoria_Server("Jerarquicos_teoria")
        Jerarquicos_Analisis_Server("Jerarquicos_analisis", datos = reactive({
          if(!is.null(datos())) datos() else datos_ejemplo$Jerarquicos
        }), datos_ejemplo = datos_ejemplo)
        Jerarquicos_Auto_Server("Jerarquicos_auto")
      }
      # ==========================
      # DBSCAN
      # ==========================
      if(tecnica == "DBSCAN"){
        # UI
        output$teoria_ui  <- renderUI({ DBSCAN_Teoria_UI(ns("DBSCAN_teoria")) })
        output$analisis_ui <- renderUI({ DBSCAN_Analisis_UI(ns("DBSCAN_analisis")) })
        output$auto_ui    <- renderUI({ DBSCAN_Auto_UI(ns("DBSCAN_auto")) })
        
        # Servidores
        DBSCAN_Teoria_Server("DBSCAN_teoria")
        DBSCAN_Analisis_Server("DBSCAN_analisis", datos = reactive({
          if(!is.null(datos())) datos() else datos_ejemplo$DBSCAN
        }), datos_ejemplo = datos_ejemplo)
        DBSCAN_Auto_Server("DBSCAN_auto")
      }
    }
    
    # ------------------------------
    # Inicializar con técnica por defecto
    # ------------------------------
    renderContenido("K-means")
    
    # ------------------------------
    # Observador para cambios de técnica
    # ------------------------------
    observeEvent(input$tecnica, {
      req(input$tecnica)
      renderContenido(input$tecnica)
    })
    
  })
}
    
    
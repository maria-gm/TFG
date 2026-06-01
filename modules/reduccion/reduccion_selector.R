# =====================================================
#  REDUCCIÓN - SELECTOR GENERAL
# =====================================================

Reduccion_UI <- function(id){
  ns <- NS(id)
  
  tagList(
    selectInput(ns("tecnica"),
                "Seleccione técnica:",
                choices = c("PCA", "AF", "BIPLOT"),
                selected = "PCA"),
    br(),
    tabsetPanel(
      tabPanel("Teoría", uiOutput(ns("teoria_ui"))),
      tabPanel("Análisis", uiOutput(ns("analisis_ui"))),
      tabPanel("Autoevaluación", uiOutput(ns("auto_ui")))
    )
  )
}


Reduccion_Server <- function(id, datos, datos_ejemplo = NULL){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    renderContenido <- function(tecnica){
      
      # ==========================
      # PCA
      # ==========================
      if(tecnica == "PCA"){
        # UI
        output$teoria_ui  <- renderUI({ PCA_Teoria_UI(ns("pca_teoria")) })
        output$analisis_ui <- renderUI({ PCA_Analisis_UI(ns("pca_analisis")) })
        output$auto_ui    <- renderUI({ PCA_Auto_UI(ns("pca_auto")) })
        
        # Servidores
        PCA_Teoria_Server("pca_teoria")
        PCA_Analisis_Server("pca_analisis", datos = reactive({
          if(!is.null(datos())) datos() else datos_ejemplo$PCA
        }), datos_ejemplo = datos_ejemplo)
        PCA_Auto_Server("pca_auto")
      }
      
      # ==========================
      # AF
      # ==========================
      if(tecnica == "AF"){
        # UI
        output$teoria_ui  <- renderUI({ AF_Teoria_UI(ns("af_teoria")) })
        output$analisis_ui <- renderUI({ AF_Analisis_UI(ns("af_analisis")) })
        output$auto_ui    <- renderUI({ AF_Auto_UI(ns("af_auto")) })
        
        # Servidores
        AF_Teoria_Server("af_teoria")
        AF_Analisis_Server("af_analisis", datos = reactive({
          if(!is.null(datos())) datos() else datos_ejemplo$AF
        }), datos_ejemplo = datos_ejemplo)
        AF_Auto_Server("af_auto")
      }
      # ==========================
      # BIPLOT
      # ==========================
      if(tecnica == "BIPLOT"){
        # UI
        output$teoria_ui  <- renderUI({ BIPLOT_Teoria_UI(ns("biplot_teoria")) })
        output$analisis_ui <- renderUI({ BIPLOT_Analisis_UI(ns("biplot_analisis")) })
        output$auto_ui    <- renderUI({ BIPLOT_Auto_UI(ns("biplot_auto")) })
        
        # Servidores
        BIPLOT_Teoria_Server("biplot_teoria")
        BIPLOT_Analisis_Server("biplot_analisis", datos = reactive({
          if(!is.null(datos())) datos() else datos_ejemplo$BIPLOT
        }), datos_ejemplo = datos_ejemplo)
        BIPLOT_Auto_Server("biplot_auto")
      }
    }
    
    # ------------------------------
    # Inicializar con técnica por defecto
    # ------------------------------
    renderContenido("PCA")
    
    # ------------------------------
    # Observador para cambios de técnica
    # ------------------------------
    observeEvent(input$tecnica, {
      req(input$tecnica)
      renderContenido(input$tecnica)
    })
    
  })
}
# =====================================================
# Clasificacion - SELECTOR 
# =====================================================
Clasificacion_UI <- function(id){
  ns <- NS(id)
  
  tagList(
    h2("Técnicas de clasificación"),
    
    selectInput(
      ns("tecnica"),
      "Selecciona la técnica",
      choices = c("LDA", "Árboles de decisión"),
      selected = "LDA"
    ),
    
    br(),
    
    tabsetPanel(
      tabPanel("Teoría", uiOutput(ns("teoria_ui"))),
      tabPanel("Análisis", uiOutput(ns("analisis_ui"))),
      tabPanel("Autoevaluación", uiOutput(ns("auto_ui")))
    ))
}

Clasificacion_Server <- function(id, datos, datos_ejemplo = NULL){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    renderContenido <- function(tecnica){
      
      # ==========================
      # LDA
      # ==========================
      if(tecnica == "LDA"){
        # UI
        output$teoria_ui  <- renderUI({ LDA_Teoria_UI(ns("LDA_teoria")) })
        output$analisis_ui <- renderUI({ LDA_Analisis_UI(ns("LDA_analisis")) })
        output$auto_ui    <- renderUI({ LDA_Auto_UI(ns("LDA_auto")) })
        
        # Servidores
        LDA_Teoria_Server("LDA_teoria")
        LDA_Analisis_Server("LDA_analisis", datos = reactive({
          if(!is.null(datos())) datos() else datos_ejemplo$LDA
        }), datos_ejemplo = datos_ejemplo)
        LDA_Auto_Server("LDA_auto")
      }
      
      # ==========================
      # Árboles de decisión
      # ==========================
      if(tecnica == "Árboles de decisión"){
        # UI
        output$teoria_ui  <- renderUI({ Arboles_Teoria_UI(ns("Arboles_teoria")) })
        output$analisis_ui <- renderUI({ Arboles_Analisis_UI(ns("Arboles_analisis")) })
        output$auto_ui    <- renderUI({ Arboles_Auto_UI(ns("Arboles_auto")) })
        
        # Servidores
        Arboles_Teoria_Server("Arboles_teoria")
        Arboles_Analisis_Server("Arboles_analisis", datos = reactive({
          if(!is.null(datos())) datos() else datos_ejemplo$Arboles
        }), datos_ejemplo = datos_ejemplo)
        Arboles_Auto_Server("Arboles_auto")
      }
      
      
      
    }
    
    # ------------------------------
    # Inicializar con técnica por defecto
    # ------------------------------
    renderContenido("LDA")
    
    # ------------------------------
    # Observador para cambios de técnica
    # ------------------------------
    observeEvent(input$tecnica, {
      req(input$tecnica)
      renderContenido(input$tecnica)
    })
    
  })
}

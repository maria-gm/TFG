# =====================================================
# REGRESION - SELECTOR 
# =====================================================

Regresion_UI <- function(id){
  ns <- NS(id)
  
  tagList(
    h2("Técnicas de regresión"),
    
    selectInput(
      ns("tecnica"),
      "Selecciona la técnica",
      choices = c(
        "Regresión múltiple",
        "Regresión logística",
        "Métodos de regularización",
        "PCR"
      ),
      selected = "Regresión múltiple"
    ),
    
    br(),
    
    tabsetPanel(
      tabPanel("Teoría", uiOutput(ns("teoria_ui"))),
      tabPanel("Análisis", uiOutput(ns("analisis_ui"))),
      tabPanel("Autoevaluación", uiOutput(ns("auto_ui")))
    )
  )
}

Regresion_Server <- function(id, datos, datos_ejemplo = NULL){
  
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    
    renderContenido <- function(tecnica){
      
      # =====================================================
      # REGRESIÓN MÚLTIPLE
      # =====================================================
      
      if(tecnica == "Regresión múltiple"){
        
        # UI
        output$teoria_ui <- renderUI({
          Regresion_multiple_Teoria_UI(ns("regresion_multiple_teoria"))
        })
        
        output$analisis_ui <- renderUI({
          Regresion_multiple_Analisis_UI(ns("regresion_multiple_analisis"))
        })
        
        output$auto_ui <- renderUI({
          Regresion_multiple_Auto_UI(ns("regresion_multiple_auto"))
        })
        
        # SERVERS
        Regresion_multiple_Teoria_Server(
          "regresion_multiple_teoria"
        )
        
        Regresion_multiple_Analisis_Server(
          "regresion_multiple_analisis",
          datos = reactive({
            if(!is.null(datos())){
              datos()
            } else {
              datos_ejemplo$Regresion_multiple
            }
          }),
          datos_ejemplo = datos_ejemplo
        )
        
        Regresion_multiple_Auto_Server(
          "regresion_multiple_auto"
        )
      }
      
      
      # =====================================================
      # REGRESIÓN LOGÍSTICA
      # =====================================================
      
      if(tecnica == "Regresión logística"){
        
        # UI
        output$teoria_ui <- renderUI({
          Regresion_logistica_Teoria_UI(ns("regresion_logistica_teoria"))
        })
        
        output$analisis_ui <- renderUI({
          Regresion_logistica_Analisis_UI(ns("regresion_logistica_analisis"))
        })
        
        output$auto_ui <- renderUI({
          Regresion_logistica_Auto_UI(ns("regresion_logistica_auto"))
        })
        
        # SERVERS
        Regresion_logistica_Teoria_Server(
          "regresion_logistica_teoria"
        )
        
        Regresion_logistica_Analisis_Server(
          "regresion_logistica_analisis",
          datos = reactive({
            if(!is.null(datos())){
              datos()
            } else {
              datos_ejemplo$Regresion_logistica
            }
          }),
          datos_ejemplo = datos_ejemplo
        )
        
        Regresion_logistica_Auto_Server(
          "regresion_logistica_auto"
        )
      }
      
      
      # =====================================================
      # REGULARIZACIÓN
      # =====================================================
      
      if(tecnica == "Métodos de regularización"){
        
        # UI
        output$teoria_ui <- renderUI({
          Regularizacion_Teoria_UI(ns("regularizacion_teoria"))
        })
        
        output$analisis_ui <- renderUI({
          Regularizacion_Analisis_UI(ns("regularizacion_analisis"))
        })
        
        output$auto_ui <- renderUI({
          Regularizacion_Auto_UI(ns("regularizacion_auto"))
        })
        
        # SERVERS
        Regularizacion_Teoria_Server(
          "regularizacion_teoria"
        )
        
        Regularizacion_Analisis_Server(
          "regularizacion_analisis",
          datos = reactive({
            if(!is.null(datos())){
              datos()
            } else {
              datos_ejemplo$Regularizacion
            }
          }),
          datos_ejemplo = datos_ejemplo
        )
        
        Regularizacion_Auto_Server(
          "regularizacion_auto"
        )
      }
      
      
      # =====================================================
      # PCR
      # =====================================================
      
      if(tecnica == "PCR"){
        
        # UI
        output$teoria_ui <- renderUI({
          PCR_Teoria_UI(ns("pcr_teoria"))
        })
        
        output$analisis_ui <- renderUI({
          PCR_Analisis_UI(ns("pcr_analisis"))
        })
        
        output$auto_ui <- renderUI({
          PCR_Auto_UI(ns("pcr_auto"))
        })
        
        # SERVERS
        PCR_Teoria_Server(
          "pcr_teoria"
        )
        
        PCR_Analisis_Server(
          "pcr_analisis",
          datos = reactive({
            if(!is.null(datos())){
              datos()
            } else {
              datos_ejemplo$PCR
            }
          }),
          datos_ejemplo = datos_ejemplo
        )
        
        PCR_Auto_Server(
          "pcr_auto"
        )
      }
      
    }
    
    
    # =====================================================
    # TÉCNICA INICIAL
    # =====================================================
    
    renderContenido("Regresión múltiple")
    
    
    # =====================================================
    # CAMBIO DE TÉCNICA
    # =====================================================
    
    observeEvent(input$tecnica, {
      
      req(input$tecnica)
      
      renderContenido(input$tecnica)
      
    })
    
  })
}


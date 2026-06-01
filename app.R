# =====================================================
#  MÓDULOS
# =====================================================
source("global.R")
source("modules/reduccion/reduccion_selector.R")
source("modules/reduccion/pca_module.R")
source("modules/reduccion/af_module.R")
source("modules/reduccion/biplot_module.R")
source("modules/agrupamiento/clusteres_jerarquicos_module.R")
source("modules/agrupamiento/k_means_module.R")
source("modules/agrupamiento/dbscan_module.R")
source("modules/agrupamiento/agrupamiento_selector_module.R")
source("modules/regresion multivariante/regresion_selector.R")
source("modules/regresion multivariante/regresion_multiple.R")
source("modules/regresion multivariante/regresion_logistica.R")
source("modules/regresion multivariante/pcr.R")
source("modules/regresion multivariante/regularizacion.R")
source("modules/clasificacion/clasificacion_selector.R")
source("modules/clasificacion/lda.R")
source("modules/clasificacion/arboles.R")


# =====================================================
# MENU PRINCIPAL APP 
# =====================================================

Menu_UI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      style = "display: flex; flex-wrap: wrap; margin-top: 10px; gap: 0;",
      
      # COLUMNA IZQUIERDA: EXPLICACIÓN DE LA APP
      column(5, style = "display: flex; margin-bottom: 15px;",
             card(
               style = "height: 100%; border-left: 5px solid #2C3E50; background-color: #ffffff; padding: 18px; width: 100%;",
               card_header(strong("📘 Guía de Inicio Rápido"), style = "background-color: #f8f9fa; border: none; font-size: 1.05rem;"),
               
               div(style = "margin-top: 15px;",
                   h5("¿Qué es esta herramienta?", style = "color: #2C3E50; font-weight: bold; font-size: 1.05rem;"),
                   p("Es una plataforma interactiva diseñada para aplicar técnicas estadísticas multivariantes complejas de forma visual y automatizada, sin necesidad de escribir código ni configurar entornos de programación.", style = "font-size: 0.95rem; color: #555; line-height: 1.5;"),
                   
                   h5("¿Cómo empezar?", style = "color: #2C3E50; font-weight: bold; margin-top: 25px; font-size: 1.05rem;"),
                   tags$ol(style = "font-size: 0.92rem; padding-left: 18px; color: #555; line-height: 1.6;",
                           tags$li(strong("Explora:"), " Navega por las pestañas superiores para ver gráficos y análisis automáticos calculados con el dataset activo."),
                           tags$li(strong("Prueba ejemplos:"), " A la derecha dispones de los datasets oficiales de la app para descargarlos y revisarlos localmente."),
                           tags$li(strong("Analiza tus datos:"), " Usa el panel inferior derecho para cargar tu propio archivo .csv y actualizar toda la plataforma de forma global.")
                   )
               )
             )
      ),
      
      # COLUMNA DERECHA: REPOSITORIO + ACCIÓN DE SUBIDA
      column(7, style = "display: flex; flex-direction: column; justify-content: space-between; margin-bottom: 15px;",
             
             # Bloque de Ejemplo
             div(
               h5(icon("download"), " Repositorio de Datasets de Ejemplo", style = "margin-bottom: 12px; font-weight: bold; color: #2C3E50; font-size: 1.05rem;"),
               
               tags$div(
                 style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(170px, 1fr)); gap: 12px; align-items: stretch; width: 100%; margin-bottom: 15px;",
                 
                 # Tarjeta Wine
                 card(
                   style = "margin: 0; padding: 12px; border-top: 3px solid #722F37; height: 100%; display: flex; flex-direction: column; justify-content: flex-start;",
                   div(
                     strong("🍷 Wine Dataset", style = "color: #722F37; font-size: 0.92rem; display: block;"),
                     div(style = "margin: 6px 0;",
                         span("178 filas", style = "background-color: #FDF2F4; color: #722F37; padding: 2px 6px; border-radius: 4px; font-size: 0.75rem; font-weight: bold; margin-right: 4px;"),
                         span("13 vars", style = "background-color: #FDF2F4; color: #722F37; padding: 2px 6px; border-radius: 4px; font-size: 0.75rem; font-weight: bold;")
                     ),
                     p("Contiene el análisis químico de tres tipos de vinos cultivados por diferentes productores en una región específica de Italia.", style = "font-size: 0.85rem; line-height: 1.3; color: #555; margin-bottom: 8px;"),
                     p(em("🎯 Ideal para reducción de dimensiones (PCA) y clasificación.") , style = "font-size: 0.82rem; color: #722F37; margin: 0;")
                   ),
                   downloadButton(ns("download_wine"), "Descargar", style = "background-color: #722F37; border-color: #722F37; color: white; font-size: 0.8rem; padding: 6px; width: 100%; margin-top: auto;")
                 ),
                 
                 # Tarjeta Penguins
                 card(
                   style = "margin: 0; padding: 12px; border-top: 3px solid #2B6CB0; height: 100%; display: flex; flex-direction: column; justify-content: flex-start;",
                   div(
                     strong("🐧 Penguins Dataset", style = "color: #2B6CB0; font-size: 0.92rem; display: block;"),
                     div(style = "margin: 6px 0;",
                         span("344 filas", style = "background-color: #EBF8FF; color: #2B6CB0; padding: 2px 6px; border-radius: 4px; font-size: 0.75rem; font-weight: bold; margin-right: 4px;"),
                         span("8 vars", style = "background-color: #EBF8FF; color: #2B6CB0; padding: 2px 6px; border-radius: 4px; font-size: 0.75rem; font-weight: bold;")
                     ),
                     p("Recoge mediciones morfológicas (pico, aletas, masa corporal) de tres especies de pingüinos en el archipiélago de Palmer, Antártida.", style = "font-size: 0.85rem; line-height: 1.3; color: #555; margin-bottom: 8px;"),
                     p(em("🎯 Perfecto para técnicas de clustering y regresiones múltiples."), style = "font-size: 0.82rem; color: #2B6CB0; margin: 0;")
                   ),
                   downloadButton(ns("download_penguins"), "Descargar", style = "background-color: #2B6CB0; border-color: #2B6CB0; color: white; font-size: 0.8rem; padding: 6px; width: 100%; margin-top: auto;")
                 ),
                 
                 # Tarjeta Breast Cancer
                 card(
                   style = "margin: 0; padding: 12px; border-top: 3px solid #B83280; height: 100%; display: flex; flex-direction: column; justify-content: flex-start;",
                   div(
                     strong("🧬 Breast Cancer", style = "color: #B83280; font-size: 0.92rem; display: block;"),
                     div(style = "margin: 6px 0;",
                         span("569 filas", style = "background-color: #FFF5F7; color: #B83280; padding: 2px 6px; border-radius: 4px; font-size: 0.75rem; font-weight: bold; margin-right: 4px;"),
                         span("30 vars", style = "background-color: #FFF5F7; color: #B83280; padding: 2px 6px; border-radius: 4px; font-size: 0.75rem; font-weight: bold;")
                     ),
                     p("Características geométricas calculadas a partir de imágenes digitales de una aspiración con aguja fina de masas mamarias.", style = "font-size: 0.85rem; line-height: 1.3; color: #555; margin-bottom: 8px;"),
                     p(em("🎯 Recomendado para modelos de clasificación binaria supervisada."), style = "font-size: 0.82rem; color: #B83280; margin: 0;")
                   ),
                   downloadButton(ns("download_bc"), "Descargar", style = "background-color: #B83280; border-color: #B83280; color: white; font-size: 0.8rem; padding: 6px; width: 100%; margin-top: auto;")
                 )
               )
             ),
             
             # Bloque de Carga
             card(
               style = "background-color: #ffffff; border: 1px solid #e2e8f0; border-top: 3px solid #2C3E50; margin: 0; padding: 15px; box-shadow: 0 1px 3px rgba(0,0,0,0.02);",
               fluidRow(
                 style = "display: flex; align-items: center; flex-wrap: wrap;",
                 
                 column(6, style = "margin-bottom: 5px; margin-top: 5px;",
                        strong("🔄 ¿Quieres analizar tus propios datos?", style = "color: #2C3E50; font-size: 0.95rem; display: block; margin-bottom: 4px;"),
                        p("Sube un archivo CSV estructurado para sustituir el dataset activo globalmente en la app.", style = "font-size: 0.85rem; color: #64748b; margin: 0;")
                 ),
                 
                 column(6, style = "display: flex; justify-content: flex-end; align-items: center; gap: 12px; margin-top: 5px; margin-bottom: 5px;",
                        # Envoltorio CSS para fileInput que elimina el margen inferior por defecto de Shiny
                        tags$div(
                          style = "margin-bottom: 0; width: 65%;",
                          fileInput(ns("upload_csv"), NULL, accept = ".csv", buttonLabel = "Subir CSV...", placeholder = "Ninguno", width = "100%")
                        ),
                        # Botón corregido con estilos unificados y clase nativa de Bootstrap
                        actionButton(
                          ns("reset_data"), 
                          "Restablecer originales", 
                          class = "btn-outline-danger btn-sm", 
                          style = "height: 38px; white-space: nowrap; margin-top: 6px; font-weight: 500;"
                        )
                 )
               )
             )
             
      )
    ),
    
    br(),
    
    # =================================================
    # NUEVA SECCIÓN: FAMILIAS DE TÉCNICAS MULTIVARIANTES
    # =================================================
    fluidRow(
      style = "margin-top: 10px; padding: 0 15px;",
      h5(icon("gears"), " Familias de Técnicas Estadísticas Disponibles", style = "margin-bottom: 15px; font-weight: bold; color: #2C3E50; font-size: 1.05rem;"),
      
      tags$div(
        style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(220px, 1fr)); gap: 15px; align-items: stretch; width: 100%;",
        
        # Familia 1: Reducción de Dimensión
        card(
          style = "margin: 0; padding: 15px; border-top: 4px solid #1abc9c; background-color: #ffffff; height: 100%;",
          strong("📊 Reducción de Dimensión", style = "color: #1abc9c; font-size: 1rem; display: block; margin-bottom: 8px;"),
          p(strong("Problema:"), " Simplificar conjuntos de datos masivos con demasiadas variables entrelazadas que saturan los análisis geométricos y provocan redundancia.", style = "font-size: 0.88rem; color: #555; margin-bottom: 6px;"),
          p(strong("Variables:"), " Exclusivamente variables cuantitativas (numéricas continuas) altamente correlacionadas entre sí.", style = "font-size: 0.88rem; color: #555; margin-bottom: 0;")
        ),
        
        # Familia 2: Agrupamiento
        card(
          style = "margin: 0; padding: 15px; border-top: 4px solid #2ecc71; background-color: #ffffff; height: 100%;",
          strong("🧩 Agrupamiento (Clustering)", style = "color: #2ecc71; font-size: 1rem; display: block; margin-bottom: 8px;"),
          p(strong("Problema:"), " Hallar de forma automatizada particiones, segmentos u observaciones homogéneas sin disponer de una clasificación teórica o etiqueta previa (Aprendizaje no supervisado).", style = "font-size: 0.88rem; color: #555; margin-bottom: 6px;"),
          p(strong("Variables:"), " Predictores cuantitativos continuos utilizados para calcular distancias o densidades de vecindad.", style = "font-size: 0.88rem; color: #555; margin-bottom: 0;")
        ),
        
        # Familia 3: Regresión
        card(
          style = "margin: 0; padding: 15px; border-top: 4px solid #3498db; background-color: #ffffff; height: 100%;",
          strong("📈 Regresión", style = "color: #3498db; font-size: 1rem; display: block; margin-bottom: 8px;"),
          p(strong("Problema:"), " Estimar tendencias analíticas, modelar dependencias estructurales y predecir numéricamente el valor de un fenómeno en base a un histórico (Aprendizaje supervisado).", style = "font-size: 0.88rem; color: #555; margin-bottom: 6px;"),
          p(strong("Variables:"), " Una variable respuesta (Y) numérica continua ligada a un conjunto de predictores (X) preferentemente cuantitativos.", style = "font-size: 0.88rem; color: #555; margin-bottom: 0;")
        ),
        
        # Familia 4: Clasificación
        card(
          style = "margin: 0; padding: 15px; border-top: 4px solid #e67e22; background-color: #ffffff; height: 100%;",
          strong(icon("tag"), " Clasificación", style = "color: #e67e22; font-size: 1rem; display: block; margin-bottom: 8px;"),
          p(strong("Problema:"), " Construir fronteras de decisión y reglas discriminantes automáticas capaces de asignar nuevas muestras dentro de categorías conocidas (Aprendizaje supervisado).", style = "font-size: 0.88rem; color: #555; margin-bottom: 6px;"),
          p(strong("Variables:"), " Una variable respuesta (Y) estrictamente cualitativa (categórica/binaria) junto a variables independientes (X) cuantitativas o categóricas.", style = "font-size: 0.88rem; color: #555; margin-bottom: 0;")
        )
      ) 
    ) 
  ) 
} 

          
Menu_Server <- function(id){
  
  moduleServer(id, function(input, output, session){
    
    datos_usuario <- reactiveVal(NULL)
    
    observeEvent(input$upload_csv,{
      req(input$upload_csv)
      
      Sys.sleep(0.8)
      
      df <- read.csv(input$upload_csv$datapath, header = TRUE)
      datos_usuario(df)
    })
    
    observeEvent(input$reset_data,{
      datos_usuario(NULL)
    })
    # DESCARGAS
    output$download_wine <- downloadHandler(
      filename = function() {"wine.csv"},
      content = function(file) { write.csv(wine, file, row.names = FALSE) }
    )
    
    output$download_penguins <- downloadHandler(
      filename = function() {"penguins.csv"},
      content = function(file) { write.csv(penguins, file, row.names = FALSE) }
    )
    
    output$download_bc <- downloadHandler(
      filename = function() {"breast_cancer.csv"},
      content = function(file) { write.csv(breast_cancer, file, row.names = FALSE) }
    )
    
    output$tabla <- renderTable({
      req(datos_usuario())
      head(datos_usuario())
    })
    
    return(datos_usuario)
    
  })}


# =====================================================
# UI PRINCIPAL
# =====================================================
ui <- fluidPage(
  
  # TEMA GLOBAL
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2C3E50",
    base_font = font_google("Inter")
  ),
  
  # FONDO GENERAL
  div(
    style = "background-color:#f4f6f9; min-height:100vh;",
    
    # =====================================================
    # HEADER 
    # =====================================================
    div(
      style = "background: linear-gradient(135deg, #2C3E50, #4CA1AF);
               color:white; padding:30px; border-radius:10px; margin-bottom:20px;",
      
      h1("Herramienta de Análisis Multivariante"),
      p("Explora, analiza y visualiza datos mediante técnicas estadísticas avanzadas")
    ),
    
    # =====================================================
    # TABS MODERNOS
    # =====================================================
    navset_card_tab(
      
      nav_panel(
        "📘 Menú", 
        
        card(
          style = "margin-top: 10px; min-height: 580px; background-color: #ffffff;",
          Menu_UI("intro") 
        )
      ),
      
      nav_panel(
        "📊 Reducción de Dimensión",
        
        card(
          full_screen = TRUE,
          Reduccion_UI("red_dim")
        )
      ),
      
      nav_panel(
        "🧩 Agrupamiento",
        
        card(
          full_screen = TRUE,
          Agrupamiento_UI("agrup_mod")
        )
      ),
      
      nav_panel(
        "📈 Regresión",
        
        card(
          full_screen = TRUE,
          Regresion_UI("regr_mod")
        )
      ),
      
      nav_panel(
        title = "Clasificación",
        icon = icon("tag"),
        
        card(
          full_screen = TRUE,
          Clasificacion_UI("clas_mod")
        ) 
      ),
      br(),
      br(),
      
      div(
        style = "
    background: linear-gradient(135deg, #2C3E50, #4CA1AF);
    padding: 45px 40px;
    border-radius: 16px 16px 0 0;
    margin-top: 30px;
    color: white;
  ",
        
        fluidRow(
          
          column(
            8,
            
            h2(
              "Universidad de Salamanca",
              style = "
          font-size: 2rem;
          font-weight: 700;
          margin-bottom: 8px;
          color: white;
        "
            ),
            
            p(
              "Trabajo de Fin de Grado en Estadística",
              style = "
          color: #cbd5e1;
          font-size: 1rem;
          margin-bottom: 25px;
        "
            ),
            
            h4(
              "Aplicación web interactiva para el análisis de datos multivariantes mediante técnicas de aprendizaje automático",
              style = "
          color: white;
          font-size: 1.25rem;
          line-height: 1.5;
          font-weight: 600;
          margin-bottom: 18px;
          max-width: 850px;
        "
            ),
            
            p(
              "Plataforma desarrollada mediante R y Shiny para la aplicación visual e interactiva de técnicas estadísticas multivariantes y modelos de aprendizaje automático.",
              style = "
          color: #94a3b8;
          font-size: 0.96rem;
          line-height: 1.7;
          max-width: 780px;
        "
            )
          ),
          
          column(
            4,
            
            div(
              style = "
          height: 100%;
          display: flex;
          flex-direction: column;
          justify-content: center;
          align-items: flex-end;
          text-align: right;
        ",
              
              img(
                src = "logo.png",
                style = "
    width: 120px;
    opacity: 0.9;
    margin-bottom: 18px;
    filter: brightness(1.1);
  "
              ),
              
              p(
                "María Gordo Martín",
                style = "
            margin: 0;
            color: white;
            font-size: 1rem;
            font-weight: 600;
          "
              ),
              
              p(
                "Curso 2025-2026",
                style = "
            margin-top: 4px;
            color: #94a3b8;
            font-size: 0.9rem;
          "
              )
            )
          )
        )
      )
    )
  )
)

# =====================================================
# SERVER PRINCIPAL
# =====================================================
server <- function(input, output, session){
  
  # Dataset usuario
  datos_usuario <- Menu_Server("intro") # CORREGIDO: Vinculado a Menu_Server
  
  # Dataset activo
  dataset_activo <- reactive({
    if(!is.null(datos_usuario())){
      return(datos_usuario())
    }
    NULL
  })
  
  # =========================
  # REDUCCIÓN
  # =========================
  Reduccion_Server(
    "red_dim",
    datos = dataset_activo,
    datos_ejemplo = datos_ejemplo
  )
  
  # =========================
  # AGRUPAMIENTO
  # =========================
  Agrupamiento_Server(
    "agrup_mod", 
    datos = dataset_activo,
    datos_ejemplo = datos_ejemplo
  )
  
  # =========================
  # REGRESIÓN
  # =========================
  Regresion_Server(
    "regr_mod", 
    datos = dataset_activo,
    datos_ejemplo = datos_ejemplo
  )
  
  # =========================
  # CLASIFICACIÓN
  # =========================
  Clasificacion_Server(
    "clas_mod", 
    datos = dataset_activo,
    datos_ejemplo = datos_ejemplo
  )
  
}

# =====================================================
# LANZAR APP
# =====================================================
shinyApp(ui, server)

  
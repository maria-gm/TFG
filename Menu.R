# =====================================================
# MENU PRINCIPAL APP 
# =====================================================
Menu_UI <- function(id){
  ns <- NS(id)
  
  tagList(
    # =====================================================
    # BLOQUE SUPERIOR: INTRODUCCIÓN Y EJEMPLOS (ALTURA ALINEADA)
    # =====================================================
    div(
      style = "display: flex; flex-wrap: wrap; margin-top: 10px; gap: 20px;",
      
      # COLUMNA IZQUIERDA: GUÍA DE INICIO (CRECE NATURALMENTE)
      div(
        style = "flex: 5; min-width: 300px; display: flex;",
        card(
          style = "height: 100%; border-left: 5px solid #2C3E50; background-color: #ffffff; padding: 18px; width: 100%; display: flex; flex-direction: column; justify-content: space-between;",
          card_header(strong("📘 Guía de Inicio Rápido"), style = "background-color: #f8f9fa; border: none; font-size: 1.05rem;"),
          
          div(style = "margin-top: 12px; flex-grow: 1;",
              h5("¿Qué es esta herramienta?", style = "color: #2C3E50; font-weight: bold; font-size: 1.02rem;"),
              p("Es una plataforma interactiva diseñada para aplicar técnicas estadísticas multivariantes complejas de forma visual y automatizada, sin necesidad de escribir código ni configurar entornos de programación.", style = "font-size: 0.92rem; color: #555; line-height: 1.4; margin-bottom: 15px;"),
              
              h5("¿Cómo empezar?", style = "color: #2C3E50; font-weight: bold; font-size: 1.02rem;"),
              tags$ol(style = "font-size: 0.90rem; padding-left: 16px; color: #555; line-height: 1.5; margin-bottom: 0;",
                      tags$li(strong("Explora:"), " Navega por las pestañas superiores para ver gráficos y análisis automáticos calculados con el dataset activo."),
                      tags$li(strong("Prueba ejemplos:"), " A la derecha dispones de los datasets oficiales de la app para descargarlos y revisarlos localmente."),
                      tags$li(strong("Analiza tus datos:"), " Usa el panel inferior para cargar tu propio archivo .csv y actualizar toda la plataforma de forma global.")
              )
          )
        )
      ),
      
      # COLUMNA DERECHA: CUADRÍCULA 2X2 DE DATASETS
      div(
        style = "flex: 7; min-width: 400px; display: flex; flex-direction: column;",
        h5(icon("download"), " Repositorio de Datasets de Ejemplo", style = "margin-bottom: 15px; font-weight: bold; color: #2C3E50; font-size: 1.05rem;"),
        
        tags$div(
          style = "display: grid; grid-template-columns: repeat(2, 1fr); gap: 15px; width: 100%; flex-grow: 1;",
          
          # 1. Tarjeta Wine
          card(
            style = "margin: 0; padding: 14px; border-top: 3px solid #722F37; height: 100%; display: flex; flex-direction: column; justify-content: space-between;",
            div(
              strong("🍷 Wine Dataset", style = "color: #722F37; font-size: 0.95rem; display: block;"),
              div(style = "margin: 4px 0;",
                  span("178 filas", style = "background-color: #FDF2F4; color: #722F37; padding: 2px 6px; border-radius: 4px; font-size: 0.72rem; font-weight: bold; margin-right: 4px;"),
                  span("14 vars", style = "background-color: #FDF2F4; color: #722F37; padding: 2px 6px; border-radius: 4px; font-size: 0.72rem; font-weight: bold;")
              ),
              p("Análisis químico de tres tipos de vinos cultivados en una región específica de Italia.", style = "font-size: 0.82rem; line-height: 1.3; color: #555; margin-bottom: 8px;")
            ),
            downloadButton(ns("download_wine"), "Descargar", style = "background-color: #722F37; border-color: #722F37; color: white; font-size: 0.78rem; padding: 4px 8px; margin-top: auto; width: 100%;")
          ),
          
          # 2. Tarjeta Penguins
          card(
            style = "margin: 0; padding: 14px; border-top: 3px solid #2B6CB0; height: 100%; display: flex; flex-direction: column; justify-content: space-between;",
            div(
              strong("🐧 Penguins Dataset", style = "color: #2B6CB0; font-size: 0.95rem; display: block;"),
              div(style = "margin: 4px 0;",
                  span("344 filas", style = "background-color: #EBF8FF; color: #2B6CB0; padding: 2px 6px; border-radius: 4px; font-size: 0.72rem; font-weight: bold; margin-right: 4px;"),
                  span("8 vars", style = "background-color: #EBF8FF; color: #2B6CB0; padding: 2px 6px; border-radius: 4px; font-size: 0.72rem; font-weight: bold;")
              ),
              p("Mediciones morfológicas de tres especies de pingüinos en el archipiélago de Palmer, Antártida.", style = "font-size: 0.82rem; line-height: 1.3; color: #555; margin-bottom: 8px;")
            ),
            downloadButton(ns("download_penguins"), "Descargar", style = "background-color: #2B6CB0; border-color: #2B6CB0; color: white; font-size: 0.78rem; padding: 4px 8px; margin-top: auto; width: 100%;")
          ),
          
          # 3. Tarjeta Boston
          card(
            style = "margin: 0; padding: 14px; border-top: 3px solid #D69E2E; height: 100%; display: flex; flex-direction: column; justify-content: space-between;",
            div(
              strong("🏢 Boston Housing", style = "color: #D69E2E; font-size: 0.95rem; display: block;"),
              div(style = "margin: 4px 0;",
                  span("506 filas", style = "background-color: #FEF3C7; color: #D69E2E; padding: 2px 6px; border-radius: 4px; font-size: 0.72rem; font-weight: bold; margin-right: 4px;"),
                  span("14 vars", style = "background-color: #FEF3C7; color: #D69E2E; padding: 2px 6px; border-radius: 4px; font-size: 0.72rem; font-weight: bold;")
              ),
              p("Datos de viviendas en Boston. Incluye criminalidad, contaminación y valores medianos comerciales.", style = "font-size: 0.82rem; line-height: 1.3; color: #555; margin-bottom: 8px;")
            ),
            downloadButton(ns("download_boston"), "Descargar", style = "background-color: #D69E2E; border-color: #D69E2E; color: white; font-size: 0.78rem; padding: 4px 8px; margin-top: auto; width: 100%;")
          ),
          
          # 4. Tarjeta Breast Cancer
          card(
            style = "margin: 0; padding: 14px; border-top: 3px solid #D53F8C; height: 100%; display: flex; flex-direction: column; justify-content: space-between;",
            div(
              strong("🧬 Breast Cancer", style = "color: #D53F8C; font-size: 0.95rem; display: block;"),
              div(style = "margin: 4px 0;",
                  span("699 filas", style = "background-color: #FFFBEB; color: #D53F8C; padding: 2px 6px; border-radius: 4px; font-size: 0.72rem; font-weight: bold; margin-right: 4px;"),
                  span("11 vars", style = "background-color: #FFFBEB; color: #D53F8C; padding: 2px 6px; border-radius: 4px; font-size: 0.72rem; font-weight: bold;")
              ),
              p("Datos clínicos de biopsias de tejidos mamarios de Wisconsin con descriptores de forma celular.", style = "font-size: 0.82rem; line-height: 1.3; color: #555; margin-bottom: 8px;")
            ),
            downloadButton(ns("download_cancer"), "Descargar", style = "background-color: #D53F8C; border-color: #D53F8C; color: white; font-size: 0.78rem; padding: 4px 8px; margin-top: auto; width: 100%;")
          )
        )
      )
    ),
    
    # =====================================================
    # BLOQUE INFERIOR: SINCRO CON SELECCIÓN DE DICCIONARIO Y CARGA
    # =====================================================
    hr(style = "margin: 20px 0;"),
    fluidRow(
      style = "display: flex; flex-wrap: wrap; gap: 20px;",
      
      # DICCIONARIO DE VARIABLES (ALTURA FIJA Y SCROLL INTERNO)
      div(
        style = "flex: 1; min-width: 300px; display: flex;",
        card(
          style = "width: 100%; height: 380px; display: flex; flex-direction: column; padding: 14px;",
          card_header(strong("📖 Diccionario de Variables"), style = "background-color: #f8f9fa; border: none; margin-bottom: 10px;"),
          
          # Selector reactivo vinculado directamente con input$dataset_info_select del Server
          selectInput(ns("dataset_info_select"), "Selecciona un Dataset para inspeccionar:",
                      choices = c("Wine" = "wine", 
                                  "Penguins" = "penguins", 
                                  "Boston Housing" = "boston", 
                                  "Breast Cancer" = "cancer"),
                      selected = "wine", width = "100%"),
          
          # Salida reactiva del diccionario que gestiona el switch() del Server
          div(
            style = "overflow-y: auto; flex-grow: 1; padding: 8px; border: 1px solid #e3e6f0; border-radius: 4px; background-color: #fafafa;",
            uiOutput(ns("informacion_dataset_ui"))
          )
        )
      ),
      
      # PANEL DE CARGA DE ARCHIVOS Y VISTA PREVIA
      div(
        style = "flex: 1; min-width: 300px; display: flex;",
        card(
          style = "width: 100%; min-height: 380px; padding: 14px; display: flex; flex-direction: column; justify-content: flex-start;",
          card_header(strong("📤 Cargar Dataset Propio (.csv)"), style = "background-color: #f8f9fa; border: none; margin-bottom: 10px;"),
          
          # Input de subida de archivo vinculado a input$file_usuario
          fileInput(ns("file_usuario"), "Selecciona un archivo CSV local:",
                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"),
                    buttonLabel = "Examinar...", placeholder = "Ningún archivo seleccionado", width = "100%"),
          
          # Botón de reset vinculado a input$reset_datos
          actionButton(ns("reset_datos"), "Restablecer Análisis", 
                       icon = icon("sync"), 
                       class = "btn-warning", 
                       style = "width: 100%; color: #212529; font-weight: bold; margin-bottom: 10px;"),
          # Contenedor dinámico donde se renderiza la vista previa (output$vista_previa_panel)
        uiOutput(ns("vista_previa_panel"))))))}

# =====================================================
# LÓGICA DEL SERVER PARA LA CONSULTA DENTRO DEL MÓDULO MENÚ
# =====================================================
Menu_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Contenedor reactivo interno para el dataset subido
    dataset_usuario_dinamico <- reactiveVal(NULL)
    
    # Renderizado del diccionario de variables
    output$informacion_dataset_ui <- renderUI({
      req(input$dataset_info_select)
      
      switch(input$dataset_info_select,
             
             # =====================================================
             # WINE
             # =====================================================
             
             "wine" = tags$div(
               
               style = "font-size: 0.88rem; color: #444;",
               
               tags$ul(
                 style = "padding-left: 15px; line-height: 1.5;",
                 
                 tags$li(strong("Type:"), " Tipo de vino (1, 2 o 3)."),
                 tags$li(strong("Alcohol:"), " Graduación alcohólica."),
                 tags$li(strong("Malic:"), " Concentración de ácido málico."),
                 tags$li(strong("Ash:"), " Contenido de cenizas."),
                 tags$li(strong("Alcalinity:"), " Alcalinidad de las cenizas."),
                 tags$li(strong("Magnesium:"), " Concentración de magnesio."),
                 tags$li(strong("Phenols:"), " Fenoles totales."),
                 tags$li(strong("Flavanoids:"), " Flavonoides."),
                 tags$li(strong("Nonflavanoids:"), " Fenoles no flavonoides."),
                 tags$li(strong("Proanthocyanins:"), " Proantocianidinas."),
                 tags$li(strong("Color:"), " Intensidad del color."),
                 tags$li(strong("Hue:"), " Matiz del color."),
                 tags$li(strong("OD:"), " Absorbancia OD280/OD315."),
                 tags$li(strong("Proline:"), " Concentración de prolina.")
               )
             ),
             
             # =====================================================
             # PENGUINS
             # =====================================================
             
             "penguins" = tags$div(
               
               style = "font-size: 0.88rem; color: #444;",
               
               tags$ul(
                 style = "padding-left: 15px; line-height: 1.5;",
                 
                 tags$li(strong("species:"), " Especie del pingüino (Adelie, Chinstrap o Gentoo)."),
                 tags$li(strong("island:"), " Isla donde fue observado."),
                 tags$li(strong("bill_length_mm:"), " Longitud del pico en milímetros."),
                 tags$li(strong("bill_depth_mm:"), " Profundidad del pico en milímetros."),
                 tags$li(strong("flipper_length_mm:"), " Longitud de la aleta en milímetros."),
                 tags$li(strong("body_mass_g:"), " Masa corporal en gramos."),
                 tags$li(strong("sex:"), " Sexo del individuo."),
                 tags$li(strong("year:"), " Año de observación.")
               )
             ),
             
             # =====================================================
             # BOSTON
             # =====================================================
             
             "boston" = tags$div(
               
               style = "font-size: 0.88rem; color: #444;",
               
               tags$ul(
                 style = "padding-left: 15px; line-height: 1.5;",
                 
                 tags$li(strong("crim:"), " Tasa de criminalidad per cápita."),
                 tags$li(strong("zn:"), " Proporción de suelo residencial para parcelas grandes."),
                 tags$li(strong("indus:"), " Proporción de suelo industrial."),
                 tags$li(strong("chas:"), " Proximidad al río Charles (0 = no, 1 = sí)."),
                 tags$li(strong("nox:"), " Concentración de óxidos de nitrógeno."),
                 tags$li(strong("rm:"), " Número medio de habitaciones por vivienda."),
                 tags$li(strong("age:"), " Porcentaje de viviendas construidas antes de 1940."),
                 tags$li(strong("dis:"), " Distancia ponderada a centros de empleo."),
                 tags$li(strong("rad:"), " Accesibilidad a autopistas radiales."),
                 tags$li(strong("tax:"), " Tasa del impuesto sobre la propiedad."),
                 tags$li(strong("ptratio:"), " Ratio alumno-profesor."),
                 tags$li(strong("black:"), " Índice demográfico del estudio original."),
                 tags$li(strong("lstat:"), " Porcentaje de población de nivel socioeconómico bajo."),
                 tags$li(strong("medv:"), " Valor mediano de la vivienda (variable respuesta).")
               )
             ),
             
             # =====================================================
             # BREAST CANCER
             # =====================================================
             
             "cancer" = tags$div(
               
               style = "font-size: 0.88rem; color: #444;",
               
               tags$ul(
                 style = "padding-left: 15px; line-height: 1.5;",
                 
                 tags$li(strong("Id:"), " Identificador de la muestra."),
                 tags$li(strong("Cl.thickness:"), " Grosor del agrupamiento celular."),
                 tags$li(strong("Cell.size:"), " Uniformidad del tamaño celular."),
                 tags$li(strong("Cell.shape:"), " Uniformidad de la forma celular."),
                 tags$li(strong("Marg.adhesion:"), " Adhesión marginal entre células."),
                 tags$li(strong("Epith.c.size:"), " Tamaño de células epiteliales."),
                 tags$li(strong("Bare.nuclei:"), " Número de núcleos desnudos."),
                 tags$li(strong("Bl.cromatin:"), " Cromatina blanda."),
                 tags$li(strong("Normal.nucleoli:"), " Presencia de nucléolos normales."),
                 tags$li(strong("Mitoses:"), " Actividad mitótica observada."),
                 tags$li(strong("Class:"), " Diagnóstico: benigno o maligno.")
               )
             )
      )
    })
      
     
    # Escuchar carga del archivo del usuario
    observeEvent(input$file_usuario, {
      req(input$file_usuario)
      df <- tryCatch({
        read.csv(input$file_usuario$datapath, stringsAsFactors = TRUE)
      }, error = function(e) {
        showNotification("Error al leer el archivo .csv", type = "error")
        return(NULL)
      })
      dataset_usuario_dinamico(df)
      showNotification("¡Dataset cargado con éxito!", type = "message")
    })
    
    # BOTÓN DE RESTABLECER ANALISIS: Deja la pantalla limpia como al inicio de la app
    observeEvent(input$reset_datos, {
      dataset_usuario_dinamico(NULL) # Limpia el dataset global de usuario
      
      # Forzamos la actualización para limpiar el selector del diccionario volviendo al primero
      updateSelectInput(session, "dataset_info_select", selected = "wine")
      
      showNotification("Pantalla restablecida. Utilizando análisis por defecto de la aplicación.", type = "warning")
    })
    
    # Panel de Vista Previa Dinámico
    output$vista_previa_panel <- renderUI({
      if (is.null(dataset_usuario_dinamico())) return(NULL)
      div(
        style = "margin-top: 20px; padding: 12px; background-color: #ffffff; border-radius: 6px; border: 1px dashed #2B6CB0;",
        h6(icon("table"), " Vista previa del archivo cargado:", style = "color: #2B6CB0; font-weight: bold;"),
        div(style = "overflow-x: auto; max-height: 200px;", tableOutput(session$ns("tabla_vista_previa")))
      )
    })
    
    output$tabla_vista_previa <- renderTable({
      req(dataset_usuario_dinamico())
      head(dataset_usuario_dinamico(), 5)
    }, striped = TRUE, bordered = TRUE, spacing = "s")
    
    # Controladores de Descarga (Incluido Breast Cancer)
    output$download_wine <- downloadHandler(
      filename = function() { "wine_dataset.csv" },
      content = function(file) { write.csv(datos_ejemplo$PCA, file, row.names = FALSE) }
    )
    output$download_penguins <- downloadHandler(
      filename = function() { "penguins_dataset.csv" },
      content = function(file) { write.csv(datos_ejemplo$Jerarquicos, file, row.names = FALSE) }
    )
    output$download_boston <- downloadHandler(
      filename = function() { "boston_housing_dataset.csv" },
      content = function(file) { write.csv(datos_ejemplo$Regresion_multiple, file, row.names = FALSE) }
    )
    output$download_cancer <- downloadHandler(
      filename = function() { "breast_cancer_dataset.csv" },
      content = function(file) { write.csv(datos_ejemplo$Regresion_logistica, file, row.names = FALSE) }
    )
    
    return(dataset_usuario_dinamico)
  })
}

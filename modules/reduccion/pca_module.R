
# =========================================
# PCA - TEORÍA
# =========================================

PCA_Teoria_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Para activar MathJax 
    withMathJax(),
    
    tags$div(
      style = "padding: 20px; background-color: #fcfdfe;",
      
      # =====================================
      # CABECERA
      # =====================================
      h2(
        "Análisis de Componentes Principales (ACP)",
        style = "font-weight: 800; color: #1a365d; margin-bottom: 5px;"
      ),
      
      p(
        "Técnica de reducción de dimensionalidad basada en la descomposición de la variabilidad total.",
        style = "color: #64748b; font-size: 1.1rem; margin-bottom: 30px;"
      ),
      
      # =====================================
      # TARJETAS PRINCIPALES
      # =====================================
      bslib::layout_column_wrap(
        width = 1/3, # Tres columnas 
        heights_equal = "row",
        
        # ---------------------------------
        # FUNDAMENTO
        # ---------------------------------
        bslib::card(
          bslib::card_header(
            tags$b("1. Fundamento matemático"),
            style = "background: #e0e7ff;"
          ),
          bslib::card_body(
            p("El ACP transforma las variables originales en nuevas variables incorreladas llamadas componentes principales:"),
            p("$$Y = XA$$"),
            tags$ul(
              style = "margin-top: 10px;",
              tags$li("\\(X\\) : matriz de datos original centrada y estandarizada.", style = "margin-bottom: 8px;"),
              tags$li("\\(A\\) : matriz de cargas o pesos (autovectores).", style = "margin-bottom: 8px;"),
              tags$li("\\(Y\\) : matriz de componentes principales.")
            )
          )
        ),
        
        # ---------------------------------
        # OBJETIVOS
        # ---------------------------------
        bslib::card(
          bslib::card_header(
            tags$b("2. Objetivos"),
            style = "background: #dcfce7;"
          ),
          bslib::card_body(
            p("Esta técnica persigue simplificar la estructura de los datos buscando:"),
            tags$ul(
              style = "margin-top: 15px;",
              tags$li("Reducir el número de variables originales.", style = "margin-bottom: 10px;"),
              tags$li("Conservar la máxima variabilidad (inercia) posible.", style = "margin-bottom: 10px;"),
              tags$li("Visualizar estructuras, agrupaciones y patrones ocultos.", style = "margin-bottom: 10px;"),
              tags$li("Eliminar la redundancia y multicolinealidad entre variables.")
            )
          )
        ),
        
        # ---------------------------------
        # CUÁNDO USARLO
        # ---------------------------------
        bslib::card(
          bslib::card_header(
            tags$b("3. ¿Cuándo usarlo?"),
            style = "background: #fef3c7;"
          ),
          bslib::card_body(
            p("Se recomienda aplicar un ACP cuando tu base de datos presente:"),
            tags$ul(
              style = "margin-top: 15px;",
              tags$li("Variables continuas altamente correlacionadas.", style = "margin-bottom: 10px;"),
              tags$li("Una gran cantidad de variables cuantitativas.", style = "margin-bottom: 10px;"),
              tags$li("Necesidad de una representación gráfica en baja dimensión.", style = "margin-bottom: 10px;"),
              tags$li("Utilidad para identificar patrones ocultos en los datos.")
            )
          )
        )
      ),
      
      br(),
      
      # =====================================
      # COMPONENTES PRINCIPALES
      # =====================================
      bslib::card(
        style = "border: 1px solid #cbd5e1; background: #f8fafc;",
        bslib::card_body(
          h4(
            icon("chart-line"), "Componentes principales",
            style = "color: #1e40af; margin-bottom: 10px;"
          ),
          p("Las componentes principales se construyen como combinaciones lineales de las variables originales maximizando la varianza explicada:"),
          p("$$Y_j = a_{1j}X_1 + a_{2j}X_2 + \\dots + a_{pj}X_p$$"),
          tags$ul(
            style = "margin-top: 10px;",
            tags$li("La primera componente explica la máxima varianza posible.", style = "margin-bottom: 6px;"),
            tags$li("Las siguientes explican porciones decrecientes de la variabilidad restante.", style = "margin-bottom: 6px;"),
            tags$li("Todas las componentes son ortogonales (perpendiculares e incorreladas) entre sí.")
          )
        )
      ),
      
      br(),
      
      # =====================================
      # DESCOMPOSICIÓN ESPECTRAL
      # =====================================
      bslib::card(
        style = "border: 1px solid #cbd5e1; background: #f8fafc;",
        bslib::card_body(
          h4(
            icon("table"), "Descomposición espectral",
            style = "color: #1e40af; margin-bottom: 10px;"
          ),
          p("El ACP se basa matemáticamente en la descomposición espectral de la matriz de covarianzas o de correlaciones:"),
          p("$$S = A \\Lambda A^T$$"),
          tags$ul(
            style = "margin-top: 10px;",
            tags$li("\\(S\\) : matriz de covarianzas o correlaciones.", style = "margin-bottom: 6px;"),
            tags$li("\\(A\\) : matriz ortogonal de autovectores (loadings).", style = "margin-bottom: 6px;"),
            tags$li("\\(\\Lambda\\) : matriz diagonal de autovalores (varianzas de las componentes).")
          )
        )
      ), 
      
      br(),
      
      # =====================================
      # VARIANZA EXPLICADA
      # =====================================
      bslib::card(
        style = "border: 1px solid #cbd5e1; background: #f8fafc;",
        bslib::card_body(
          h4(
            icon("percentage"), "Varianza explicada",
            style = "color: #1e40af; margin-bottom: 10px;"
          ),
          p("La importancia y retención de cada componente se evalúa de manera ordenada mediante sus autovalores:"),
          p("$$\\lambda_1 > \\lambda_2 > \\dots > \\lambda_p$$"),
          tags$ul(
            style = "margin-top: 10px;",
            tags$li("Autovalores grandes indican las componentes más relevantes del análisis.", style = "margin-bottom: 6px;"),
            tags$li("El gráfico de sedimentación (Scree Plot) ayuda a decidir el número óptimo de componentes.", style = "margin-bottom: 6px;"),
            tags$li("La varianza acumulada mide el porcentaje total de información original retenida.")
          )
        )
      ),
      
      br(),
      # =====================================
      # NÚMERO DE COMPONENTES 
      # =====================================
      bslib::card(
        style = "border: 1px solid #cbd5e1; background: #f8fafc;",
        bslib::card_body(
          h4(
            icon("arrow-down-short-wide"), "Selección del número de componentes",
            style = "color: #1e40af; margin-bottom: 10px;"
          ),
          p("Determinar cuántos ejes se deben retener es una decisión crítica. El investigador debe encontrar el menor número de dimensiones capaz de conservar la máxima información posible mediante criterios generales (Mavrou, 2015):"),
          
          tags$ul(
            style = "margin-top: 15px;",
            tags$li(
              tags$b("Criterio de Kaiser (Autovalores > 1):"), 
              " Se seleccionan únicamente las dimensiones cuyo autovalor es superior a la unidad (\\(\\lambda_j > 1\\)). Así, cada eje explica al menos tanta variabilidad como una variable original estandarizada.",
              style = "margin-bottom: 10px;"
            ),
            tags$li(
              tags$b("Porcentaje de varianza acumulada:"), 
              " Se fija a priori un porcentaje de variabilidad total que se desea explicar (habitualmente entre el 70% y el 80%). Se retiene el número mínimo de dimensiones necesarias para alcanzar o superar dicho umbral.",
              style = "margin-bottom: 10px;"
            ),
            tags$li(
              tags$b("Gráfico de sedimentación (Scree Plot):"), 
              " Propuesto por Cattell (1966), representa los autovalores en orden descendente. El número de componentes se determina observando el punto de inflexión donde la pendiente se estabiliza formando un 'codo'; las componentes anteriores a este punto se consideran las relevantes."
            )
          )
        )
      ),
      
      br(),
      
      # =====================================
      # INTERPRETACIÓN
      # =====================================
      bslib::card(
        style = "border: 1px solid #cbd5e1; background: #f8fafc;",
        bslib::card_body(
          h4(
            icon("magnifying-glass"), "Interpretación de las componentes",
            style = "color: #1e40af; margin-bottom: 15px;"
          ),
          p("La interpretación de las componentes principales se efectúa examinando la relación matemática con las variables originales:"),
          
          tags$div(
            style = "display: flex; flex-direction: column; gap: 15px; margin-top: 15px;",
            
            # --------------------------------
            # PESOS
            # --------------------------------
            tags$div(
              style = "border-left: 4px solid #3b82f6; padding-left: 12px;",
              tags$b("Pesos (Autovectores): "),
              "Indican los coeficientes numéricos de la combinación lineal. Definen la dirección de los nuevos ejes en el espacio."
            ),
            
            # --------------------------------
            # CARGAS
            # --------------------------------
            tags$div(
              style = "border-left: 4px solid #10b981; padding-left: 12px;",
              tags$b("Cargas factoriales (Loadings): "),
              "Representan las correlaciones lineales directas entre las variables originales y las componentes principales calculadas."
            )
          )
        )
      )
    )
  )
}

PCA_Teoria_Server <- function(id){
  
  moduleServer(id, function(input, output, session){
    
  })
  
}

# =========================================
# PCA - ANÁLISIS
# =========================================

PCA_Analisis_UI <- function(id){
  ns <- NS(id)
  tagList(
    # Título 
    h3("Análisis", 
       style = "color: #1a446c; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; font-weight: 600; margin-top: 40px; margin-bottom: 20px; border-bottom: 2px solid #f4f6f9; padding-bottom: 10px;"),
    
    fluidRow(
      #--------------------------------------------------
      # PANEL LATERAL (ESTRUCTURA FIJA)
      #--------------------------------------------------
      column(width = 3,
             wellPanel(
               h4("Configuración"),
               p("Seleccione las componentes principales que desea visualizar e interpretar."),
               hr(),
               # Controles dinámicos inyectados según la pestaña activa
               uiOutput(ns("panel_lateral")),
               hr(),
               helpText("Las cargas más altas indican mayor contribución a la componente."),
               br(),
               uiOutput(ns("ui_dl_pca")),
               br(),
               helpText("Nota: Se eliminan filas con valores faltantes automáticamente.")
             )
      ),
      
      #--------------------------------------------------
      # PANEL PRINCIPAL (DERECHA)
      #--------------------------------------------------
      column(width = 9,
             uiOutput(ns("mensaje_error_ui")),
             
             tabsetPanel(
               id = ns("tabs_pca"),
               
               # PESTAÑA 1: DATOS
               tabPanel("1. Datos", value = "datos",
                        br(),
                        p("Información: El análisis se ejecuta exclusivamente sobre las columnas de tipo numérico.", 
                          style = "color: #7f8c8d; font-style: italic; margin-bottom: 25px;"),
                        h4("Dataset original", style = "color: #2c3e50; font-weight: 500;"),
                        DT::DTOutput(ns("dataset")),
                        br(), hr(), br(),
                        h4("Dataset estandarizado (Z-scores)", style = "color: #2c3e50; font-weight: 500;"),
                        DT::DTOutput(ns("dataset_std"))
               ),
               
               # PESTAÑA 2: DIAGNÓSTICO
               tabPanel("2. Diagnóstico", value = "diagnostico",
                        br(),
                        wellPanel(
                          p("Se analiza la viabilidad factorial y la correlación entre variables antes de construir el PCA.")
                        ),
                        h4("Test de Esfericidad de Bartlett", style = "color: #2c3e50; font-weight: 500;"),
                        verbatimTextOutput(ns("bartlett_test")),
                        br(),
                        h4("Matriz de correlaciones", style = "color: #2c3e50; font-weight: 500;"),
                        plotOutput(ns("corrplot"), height = "500px")
               ),
               
               # PESTAÑA 3: SELECCIÓN DE COMPONENTES
               tabPanel("3. Selección de Componentes", value = "componentes",
                        br(),
                        wellPanel(
                          p("Scree plot (Gráfico de sedimentación) y análisis de la varianza explicada acumulada.")
                        ),
                        h4("Scree Plot", style = "color: #2c3e50; font-weight: 500;"),
                        plotOutput(ns("scree"), height = "450px"),
                        br(),
                        h4("Tabla de varianza explicada", style = "color: #2c3e50; font-weight: 500;"),
                        DT::DTOutput(ns("var_table")),
                        br(),
                        h4("Interpretación", style = "color: #2c3e50; font-weight: 500;"),
                        verbatimTextOutput(ns("interp_varianza"))
               ),
               
               # PESTAÑA 4: INDIVIDUOS
               tabPanel("4. Individuos", value = "individuos",
                        br(),
                        wellPanel(
                          p("Proyección de las observaciones en el nuevo espacio factorial mapeado.")
                        ),
                        h4("Mapa de Individuos (Puntuaciones Factoriales)", style = "color: #2c3e50; font-weight: 500;"),
                        plotOutput(ns("scores"), height = "600px"),
                        br(),
                        h4("Interpretación", style = "color: #2c3e50; font-weight: 500;"),
                        verbatimTextOutput(ns("interp_scores")),
                        br(),
                        h4("Puntuaciones de los individuos (Scores)", style = "color: #2c3e50; font-weight: 500;"),
                        DT::DTOutput(ns("scores_table"))
               ),
               
               # PESTAÑA 5: VARIABLES
               tabPanel("5. Variables", value = "variables",
                        br(),
                        wellPanel(
                          p("Interpretación de cargas factoriales (Loadings). Las celdas resaltadas superan la magnitud mínima de contribución definida.")
                        ),
                        h4("Tabla de cargas (Loadings)", style = "color: #2c3e50; font-weight: 500;"),
                        DT::DTOutput(ns("loadings_table")),
                        br(),
                        h4("Contribución porcentual de las variables", style = "color: #2c3e50; font-weight: 500;"),
                        DT::DTOutput(ns("loadings_importantes")),
                        br(),
                        h4("Interpretación", style = "color: #2c3e50; font-weight: 500;"),
                        verbatimTextOutput(ns("interp_loadings"))
               )
             )
      )
    )
  )
}

PCA_Analisis_Server <- function(id, datos, datos_ejemplo = NULL){
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    
    #--------------------------------------------------
    # 1. EVALUACIÓN Y PREPROCESADO 
    #--------------------------------------------------
    datos_preprocesados <- reactive({
      df <- if(!is.null(datos()) && nrow(datos()) > 0) datos() else datos_ejemplo
      
      crear_banner_error <- function(mensaje) {
        div(
          style = "background-color: #fdf2f2; color: #9b1c1c; border: 1px solid #fde8e8; padding: 20px; margin-bottom: 25px; border-radius: 6px;",
          tags$span(style = "font-weight: bold; font-size: 16px;", tags$i(class = "fa fa-exclamation-triangle"), " No es posible realizar el análisis."),
          br(),
          tags$span(style = "font-style: italic; color: #4b5563; font-size: 14px;", "Información: El Análisis de Componentes Principales (PCA) requiere variables métricas continuas intercorrelacionadas."),
          br(), br(),
          tags$span(style = "font-weight: 500;", mensaje)
        )
      }
      
      if (is.null(df)) {
        return(list(valido = FALSE, ui_error = crear_banner_error("No se han detectado datos en el sistema.")))
      }
      
      # Filtrar únicamente columnas numéricas
      df_num <- df[, sapply(df, is.numeric), drop = FALSE]
      
      # Omitir registros incompletos 
      df_clean <- na.omit(df_num)
      
      if (ncol(df_clean) < 2) {
        return(list(valido = FALSE, ui_error = crear_banner_error("Se necesitan al menos 2 columnas numéricas para justificar una reducción dimensional.")))
      }
      if (nrow(df_clean) < 15) {
        return(list(valido = FALSE, ui_error = crear_banner_error("Se requieren al menos 15 observaciones completas para validar la estructura dimensional.")))
      }
      
      return(list(valido = TRUE, base_limpia = df_clean))
    })
    
    output$mensaje_error_ui <- renderUI({
      prep <- datos_preprocesados()
      if (!prep$valido) return(prep$ui_error)
      return(NULL)
    })
    
    datos_num <- reactive({ req(datos_preprocesados()$valido); datos_preprocesados()$base_limpia })
    
    datos_std <- reactive({ req(datos_num()); scale(datos_num()) })
    
    pca_res <- reactive({ req(datos_num()); prcomp(datos_num(), scale. = TRUE) })
    
    #--------------------------------------------------
    # 2. SELECTORES LATERALES DINÁMICOS POR PESTAÑA
    #--------------------------------------------------
    output$panel_lateral <- renderUI({
      req(pca_res())
      max_comp <- ncol(pca_res()$x)
      
      elementos <- list(
        numericInput(
          ns("k_componentes"),
          "Número de componentes a retener:",
          value = min(2, max_comp),
          min = 1,
          max = max_comp,
          step = 1
        )
      )
      
      if (!is.null(input$tabs_pca) && input$tabs_pca == "individuos") {
        elementos <- c(elementos, list(
          hr(),
          h5("Gráfico de Individuos"),
          selectInput(
            ns("eje_x"),
            "Componente Eje X:",
            choices = 1:max_comp,
            selected = 1
          ),
          selectInput(
            ns("eje_y"),
            "Componente Eje Y:",
            choices = 1:max_comp,
            selected = min(2, max_comp)
          )
        ))
      }
      
      if (!is.null(input$tabs_pca) && input$tabs_pca == "variables") {
        elementos <- c(elementos, list(
          hr(),
          h5("Filtrado de Cargas (Loadings)"),
          sliderInput(
            ns("magnitud_carga"),
            "Magnitud mínima de contribución:",
            min = 0,
            max = 1,
            value = 0.3,
            step = 0.05
          )
        ))
      }
      
      tagList(elementos)
    })
    
    #--------------------------------------------------
    # 3. BOTÓN DE DESCARGA DINÁMICO
    #--------------------------------------------------
    output$ui_dl_pca <- renderUI({
      req(input$k_componentes)
      k <- round(input$k_componentes)
      
      downloadButton(
        ns("dl_pca"),
        paste0("Descargar las ", k, " componentes"),
        class = "btn-success",
        style = "width: 100%;"
      )
    })
    
    #--------------------------------------------------
    # 4. RENDERIZADO DE TABLAS Y GRÁFICOS
    #--------------------------------------------------
    output$dataset <- DT::renderDT({
      req(datos_num())
      
      DT::datatable(
        datos_num(), 
        options = list(paging = FALSE, scrollY = "400px", scrollX = TRUE, autoWidth = TRUE),
        class = 'cell-border stripe hover compact'
      ) %>% 
        DT::formatRound(columns = names(datos_num()), digits = 3)
    })
    
    output$dataset_std <- DT::renderDT({
      req(datos_std())
      
      DT::datatable(
        round(as.data.frame(datos_std()), 3), 
        options = list(paging = FALSE, scrollY = "400px", scrollX = TRUE, autoWidth = TRUE),
        class = 'cell-border stripe hover compact'
      )
    })
    
    output$bartlett_test <- renderPrint({
      req(datos_std())
      psych::cortest.bartlett(cor(datos_std()))
    })
    
    output$corrplot <- renderPlot({
      req(datos_std())
      corrplot::corrplot(
        cor(datos_std()),
        method = "color",
        type = "upper",
        tl.col = "black"
      )
    })
    
    output$scree <- renderPlot({
      req(pca_res(), input$k_componentes)
      k_sel <- round(input$k_componentes)
      
      factoextra::fviz_eig(
        pca_res(),
        addlabels = TRUE,
        barfill = "#34495e",
        barcolor = "#34495e"
      ) +
        ggplot2::labs(
          title = "Scree Plot",
          subtitle = paste0("Corte seleccionado en las ", k_sel, " primeras componentes")
        ) +
        theme_minimal()
    })
    
    output$var_table <- DT::renderDT({
      req(pca_res(), input$k_componentes)
      k <- round(input$k_componentes)
      
      eig <- pca_res()$sdev^2
      tabla <- data.frame(
        Componente = paste0("PC", 1:length(eig)),
        Eigenvalue = round(eig, 3),
        Varianza_Pct = round(eig/sum(eig)*100, 2),
        Acumulada_Pct = round(cumsum(eig/sum(eig)*100), 2)
      )
      tabla_filtrada <- tabla[1:k, , drop = FALSE]
      
      DT::datatable(
        tabla_filtrada,
        options = list(paging = FALSE, scrollY = "300px", scrollX = TRUE, autoWidth = TRUE),
        class = 'cell-border stripe hover compact'
      )
    })
    
    output$interp_varianza <- renderText({
      req(pca_res(), input$k_componentes)
      k <- round(input$k_componentes)
      
      eig <- pca_res()$sdev^2
      var_acum <- round(sum(eig[1:k]) / sum(eig) * 100, 2)
      
      paste0(
        "Las primeras ", k, " componentes explican conjuntamente el ", var_acum, "% de la variabilidad total de los datos.\n\n",
        "Ejemplo práctico (Dataset 'wines'): En ese conjunto de datos, las 3 primeras componentes suelen ser ",
        "suficientes para capturar más del 66% de la varianza total. Esto demuestra cómo el PCA permite resumir ",
        "las propiedades originales en pocas dimensiones sin perder demasiada información analítica."
      )
    })
    
    output$scores <- renderPlot({
      req(pca_res())
      comp_x <- if(!is.null(input$eje_x)) as.numeric(input$eje_x) else 1
      comp_y <- if(!is.null(input$eje_y)) as.numeric(input$eje_y) else min(2, ncol(pca_res()$x))
      
      factoextra::fviz_pca_ind(
        pca_res(),
        axes = c(comp_x, comp_y),
        col.ind = "#2c3e50",
        repel = TRUE
      ) +
        theme_minimal()
    })
    
    output$interp_scores <- renderText({
      req(input$k_componentes)
      k <- round(input$k_componentes)
      comp_x <- if(!is.null(input$eje_x)) input$eje_x else 1
      comp_y <- if(!is.null(input$eje_y)) input$eje_y else 2
      
      paste0(
        "El gráfico muestra el mapa de las observaciones cruzando la componente ", comp_x, " con la componente ", comp_y, ".\n",
        "Se están reteniendo un total de ", k, " componentes según los criterios de selección estructurados.\n\n",
        "Ejemplo práctico: Permite observar cómo las muestras se agrupan y separan de forma natural en clusters distintos dentro del espacio factorial mapeado."
      )
    })
    
    output$scores_table <- DT::renderDT({
      req(pca_res(), input$k_componentes)
      k <- round(input$k_componentes)
      df_scores <- as.data.frame(pca_res()$x[, 1:k, drop = FALSE])
      
      DT::datatable(
        round(df_scores, 3),
        options = list(paging = FALSE, scrollY = "400px", scrollX = TRUE, autoWidth = TRUE),
        class = 'cell-border stripe hover compact'
      )
    })
    
    output$loadings_table <- DT::renderDT({
      req(pca_res(), input$k_componentes)
      k <- round(input$k_componentes)
      umbral <- if(!is.null(input$magnitud_carga)) input$magnitud_carga else 0.3
      df_loadings <- as.data.frame(pca_res()$rotation[, 1:k, drop = FALSE])
      
      DT::datatable(
        round(df_loadings, 3),
        options = list(paging = FALSE, scrollY = "400px", scrollX = TRUE, autoWidth = TRUE),
        class = 'cell-border stripe hover compact'
      ) %>% 
        DT::formatStyle(
          columns = 1:k,
          backgroundColor = DT::styleInterval(
            cuts = c(-umbral, umbral), 
            values = c('#fcd7d7', 'transparent', '#d4edda')
          ),
          fontWeight = DT::styleInterval(
            cuts = c(-umbral, umbral), 
            values = c('bold', 'normal', 'bold')
          )
        )
    })
    
    output$loadings_importantes <- DT::renderDT({
      req(pca_res(), input$k_componentes)
      k <- round(input$k_componentes)
      df_contrib <- as.data.frame(factoextra::get_pca_var(pca_res())$contrib[, 1:k, drop = FALSE])
      
      DT::datatable(
        round(df_contrib, 2),
        options = list(paging = FALSE, scrollY = "400px", scrollX = TRUE, autoWidth = TRUE),
        class = 'cell-border stripe hover compact'
      )
    })
    
    output$interp_loadings <- renderText({
      req(input$k_componentes)
      k <- round(input$k_componentes)
      umbral <- if(!is.null(input$magnitud_carga)) input$magnitud_carga else 0.3
      
      paste0(
        "Se evalúan las cargas (loadings) asociadas a las ", k, " componentes seleccionadas.\n",
        "Se han resaltado en VERDE las cargas positivas significativas y en ROJO las negativas significativas (magnitud absoluta > ", umbral, ").\n\n",
        "Las variables con cargas más extremas son las que definen geométricamente el significado conceptual de cada componente."
      )
    })
    
    # Descarga de resultados estructurados
    output$dl_pca <- downloadHandler(
      filename = function() {
        paste("PCA_Resultados_", round(input$k_componentes), "_componentes.csv", sep = "")
      },
      content = function(file) {
        req(pca_res(), input$k_componentes)
        k <- round(input$k_componentes)
        componentes_exportar <- as.data.frame(pca_res()$x[, 1:k, drop = FALSE])
        write.csv(componentes_exportar, file, row.names = TRUE)
      }
    )
  })
}


  
# =========================================
# PCA - AUTOEVALUACIÓN
# =========================================
PCA_Auto_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$style(HTML("
        /* Ataca directamente a todas las variaciones de radio buttons de Shiny */
        .shiny-input-radiogroup, 
        .shiny-input-radiogroup .shiny-options-group,
        .shiny-input-radiogroup .form-check,
        .shiny-input-radiogroup .radio {
          width: 100% !important;
          max-width: 100% !important;
          display: block !important;
        }
        
        /* Asegura que la etiqueta y el texto ocupen todo el espacio del card */
        .shiny-input-radiogroup label,
        .shiny-input-radiogroup .form-check-label {
          width: 100% !important;
          max-width: 100% !important;
          display: inline-block !important;
          white-space: normal !important; /* Permite saltos lógicos, no prematuros */
          word-break: break-word !important;
        }
        
        /* Ajuste por si el flexbox de Bootstrap 5 está encogiendo el texto */
        .form-check {
          display: flex !important;
          align-items: center !important;
          gap: 0.5rem;
        }
      "))
    ),
    
    h3("Autoevaluación", 
       style = "color: #1a446c; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; font-weight: 600; margin-top: 40px; margin-bottom: 20px; border-bottom: 2px solid #f4f6f9; padding-bottom: 10px;"),
    
    uiOutput(ns("preguntas")),
    
    br(),
    
    card(
      class = "shadow-sm mb-4 border-0",
      style = "background-color: #fdfdfd;",
      card_body(
        class = "d-flex justify-content-between align-items-center flex-wrap gap-3 py-3",
        div(
          class = "d-flex gap-2",
          actionButton(ns("ver"), "👁️ Ver respuestas", class = "btn-primary"),
          actionButton(ns("shuffle"), "🔀 Reordenar test", class = "btn-outline-primary")
        ),
        uiOutput(ns("score"))
      )
    ),
    
    br(),
    
    accordion(
      open = FALSE,
      class = "shadow-sm border-0",
      accordion_panel(
        title = "➕ Gestión: Añadir pregunta personalizada de PCA" ,
        icon = icon("gear"),
        
        fluidRow(
          column(width = 9, textInput(ns("nueva_pregunta"), "Enunciado de la pregunta")),
          column(width = 3, selectInput(ns("correcta"), "Asignar correcta", 
                                        choices = c("Opción 1", "Opción 2", "Opción 3", "Opción 4")))
        ),
        
        fluidRow(
          column(width = 3, textInput(ns("op1"), "Opción 1")),
          column(width = 3, textInput(ns("op2"), "Opción 2")),
          column(width = 3, textInput(ns("op3"), "Opción 3")),
          column(width = 3, textInput(ns("op4"), "Opción 4"))
        ),
        
        actionButton(ns("add"), "Guardar pregunta en el banco de PCA", class = "btn-success btn-sm mt-2")
      )
    )
  )
}
PCA_Auto_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    mostrar_respuestas <- reactiveVal(FALSE)
    mostrar_respuestas <- reactiveVal(FALSE)
    
    observeEvent(input$ver, {
      mostrar_respuestas(!mostrar_respuestas())
      
      updateActionButton(
        session,
        "ver",
        label = if (mostrar_respuestas()) "🙈 Ocultar respuestas" else "👁️ Ver respuestas"
      )
    })
    preguntas_base <- list(
      list(texto = "¿Qué representa la primera componente principal?", opciones = c("Variable con mayor varianza", "Combinación lineal de máxima varianza", "Media", "Variable aleatoria"), correcta = "Combinación lineal de máxima varianza"),
      list(texto = "¿Cuál es el objetivo del PCA?", opciones = c("Clasificar", "Reducir dimensionalidad", "Predecir", "Agrupar"), correcta = "Reducir dimensionalidad"),
      list(texto = "¿Cuál es el método para elegir el número óptimo de componentes?", opciones = c("P-value", "Método del codo", "ANOVA", "Chi-cuadrado"), correcta = "Método del codo"),
      list(texto = "¿Qué representa la varianza explicada?", opciones = c("Error", "Información capturada", "Media", "Número variables"), correcta = "Información capturada"),
      list(texto = "¿Qué significa un Loading alto?", opciones = c("Baja importancia", "Alta contribución", "Ruido", "Outlier"), correcta = "Alta contribución"),
      list(texto = "¿Cuál es el paso previo a realizar el ACP?", opciones = c("Estandarizar las variables", "Eliminar variables no relevantes", "Ninguno", "Cambiar escala"), correcta = "Estandarizar las variables"),
      list(texto = "¿Las escalas diferentes afectan al resultado del PCA?", opciones = c("No", "Sí, sesgan los resultados", "Mejoran los resultados", "No cambia nada"), correcta = "Sí, sesgan los resultados"),
      list(texto = "¿Qué representan los valores propios?", opciones = c("Media", "Varianza explicada", "Correlación", "Cluster"), correcta = "Varianza explicada"),
      list(texto = "¿Qué implica que dos variables sean ortogonales?", opciones = c("Igualdad", "No correlación", "Dependencia", "Ruido"), correcta = "No correlación"),
      list(texto = "¿Cuándo no es recomendable usar PCA?", opciones = c("Cuando los datos tienen valores grandes", "Si las variables presentan relaciones no lineales fuertes", "Cuando el conjunto de datos presenta muchas variables", "Cuando los datos han sido preprocesados"), correcta = "Si las variables presentan relaciones no lineales fuertes"),
      list(texto = "¿Qué criterio se utiliza habitualmente si se trabaja con variables estandarizadas?", opciones = c("Seleccionar componentes con autovalores superiores a 1", "Elegir componentes con autovalores cercanos a 0", "Quedarse siempre con todas las componentes", "Eliminar las variables con correlación positiva"), correcta = "Seleccionar componentes con autovalores superiores a 1"),
      list(texto = "En el dataset 'wines', si la primera componente tiene loadings fuertemente negativos en 'Total Phenols' y 'Flavanoids', ¿qué indica una puntuación (score) muy baja en PC1 para un vino?", opciones = c("Que el vino tiene una alta concentración de compuestos fenólicos y flavonoides", "Que es un vino con bajo contenido alcohólico", "Que el vino carece de antioxidantes por completo", "Que la medición contiene un error técnico"), correcta = "Que el vino tiene una alta concentración de compuestos fenólicos y flavonoides"),
      list(texto = "Si al proyectar los vinos en el plano PC1-PC2 observas tres agrupaciones (clusters) perfectamente definidas y separadas, ¿cuál es la conclusión analítica correcta?", opciones = c("El PCA ha fallado al mezclar los datos", "Las propiedades químicas logran diferenciar claramente los tres tipos de vino", "Todos los vinos se comportan de manera idéntica", "El número de observaciones es insuficiente"), correcta = "Las propiedades químicas logran diferenciar claramente los tres tipos de vino"),
      list(texto = "Si las variables químicas 'Alcohol' e 'Intensity' tienen cargas (loadings) altas y positivas sobre la PC2 en el análisis de 'wines', ¿qué caracteriza a las muestras situadas en la parte más alta del eje vertical?", opciones = c("Son vinos suaves, ligeros y con poco color", "Son vinos con alta graduación alcohólica y color intenso", "Son vinos con niveles de acidez extremadamente altos", "Son muestras con valores perdidos"), correcta = "Son vinos con alta graduación alcohólica y color intenso"),
      list(texto = "Al revisar el Scree Plot del dataset 'wines',  ¿Qué decisión deberías tomas?", opciones = c("Retener las 3 primeras componentes", "Descartar todo el análisis por baja varianza", "Retener las 13 componentes originales obligatoriamente", "Utilizar únicamente la primera componente"), correcta = "Retener las 3 primeras componentes")
    )
    
    preguntas_usuario <- reactiveVal(list())
    
    observeEvent(input$add, {
      req(input$nueva_pregunta, input$op1, input$op2, input$op3, input$op4)
      
      nueva <- list(
        texto = input$nueva_pregunta,
        opciones = c(input$op1, input$op2, input$op3, input$op4),
        correcta = c(input$op1, input$op2, input$op3, input$op4)[
          match(input$correcta, c("Opción 1", "Opción 2", "Opción 3", "Opción 4"))
        ]
      )
      
      preguntas_usuario(c(preguntas_usuario(), list(nueva)))
    })
    
    preguntas <- reactive({
      c(preguntas_base, preguntas_usuario())
    })
    
    preguntas_ordenadas <- reactiveVal(NULL)
    
    observe({
      lista_actual <- preguntas()
      
      lista_enriquecida <- lapply(lista_actual, function(p) {
        p$id_unico <- paste0("q_", gsub("[^a-zA-Z0-9]", "", p$texto))
        p
      })
      
      if (is.null(isolate(preguntas_ordenadas()))) {
        preguntas_ordenadas(sample(lista_enriquecida, min(10, length(lista_enriquecida))))
      }
    })
    
    observeEvent(input$shuffle, {
      lista_actual <- preguntas()
      
      lista_enriquecida <- lapply(lista_actual, function(p) {
        p$id_unico <- paste0("q_", gsub("[^a-zA-Z0-9]", "", p$texto))
        p
      })
      
      nuevas <- sample(lista_enriquecida, min(10, length(lista_enriquecida)))
      
      nuevas <- lapply(nuevas, function(p) {
        p$opciones <- sample(p$opciones)
        p
      })
      
      preguntas_ordenadas(nuevas)
    })
    
    output$preguntas <- renderUI({
      req(preguntas_ordenadas())
      
      tagList(
        lapply(seq_along(preguntas_ordenadas()), function(i) {
          pregunta <- preguntas_ordenadas()[[i]]
          id_input <- pregunta$id_unico
          
          feedback_ui <- NULL
          
          if (isTRUE(mostrar_respuestas())) {
            user_ans <- input[[id_input]]
            correct <- pregunta$correcta
            
            if (!is.null(user_ans) && user_ans == correct) {
              feedback_ui <- div(class = "text-success mt-2 font-weight-bold", "✔️ ¡Correcto!")
            } else {
              feedback_ui <- div(class = "text-danger mt-2",
                                 paste0("❌ Incorrecto. Respuesta: ", correct))
            }
          }
          
          card(
            class = "mb-3 shadow-sm",
            card_header(tags$strong(paste0("Pregunta ", i))),
            card_body(
              radioButtons(
                session$ns(id_input),
                pregunta$texto,
                choices = pregunta$opciones,
                selected = input[[id_input]]
              ),
              feedback_ui
            )
          )
        })
      )
    })
    
    output$score <- renderUI({
      req(mostrar_respuestas())
      
      total <- length(preguntas_ordenadas())
      
      aciertos <- sum(sapply(preguntas_ordenadas(), function(p) {
        res <- input[[p$id_unico]]
        !is.null(res) && res == p$correcta
      }))
      
      porcentaje <- aciertos / total * 100
      
      div(
        class = if (porcentaje >= 70) "alert alert-success" else "alert alert-warning",
        strong(paste0("Puntuación: ", aciertos, " / ", total, " (", round(porcentaje), "%)"))
      )
    })
  })
}
 

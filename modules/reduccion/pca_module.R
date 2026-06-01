
# =========================================
# PCA MODULO
# =========================================

# =========================================
# PCA - TEORÍA
# =========================================

PCA_Teoria_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Única llamada necesaria para activar MathJax en la página
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
        width = 1/3, # Tres columnas perfectas
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
      # NÚMERO DE COMPONENTES (TFG)
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
    
    # Título personalizado con los estilos CSS solicitados
    h3("Análisis", 
       style = "color: #1a446c; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; font-weight: 600; margin-top: 40px; margin-bottom: 20px; border-bottom: 2px solid #f4f6f9; padding-bottom: 10px;"),
    
    fluidRow(
      
      #--------------------------------------------------
      # PANEL LATERAL
      #--------------------------------------------------
      
      column(
        width = 3,
        
        wellPanel(
          
          h4("Configuración"),
          
          p("Seleccione las componentes principales que desea visualizar e interpretar."),
          
          hr(),
          
          uiOutput(ns("panel_lateral")),
          
          hr(),
          
          helpText(
            "Las cargas más altas indican mayor contribución a la componente."
          ),
          
          br(),
          
          # Se cambia a uiOutput para actualizar dinámicamente el texto del botón
          uiOutput(ns("ui_dl_pca"))
        )
      ),
      
      #--------------------------------------------------
      # PANEL PRINCIPAL
      #--------------------------------------------------
      
      column(
        width = 9,
        
        tabsetPanel(
          id = ns("tabs_pca"), # Este ID nos permite saber qué pestaña está activa
          
          #================================================
          # DATOS
          #================================================
          
          tabPanel(
            "Datos",
            value = "datos",
            
            br(),
            
            # Texto estilizado y discreto en lugar del wellPanel gris
            p("Información: El análisis se ejecuta exclusivamente sobre las columnas de tipo numérico.", 
              style = "color: #7f8c8d; font-style: italic; margin-bottom: 25px;"),
            
            h4("Dataset original", style = "color: #2c3e50; font-weight: 500;"),
            DT::DTOutput(ns("dataset")),
            
            br(), hr(), br(),
            
            h4("Dataset estandarizado", style = "color: #2c3e50; font-weight: 500;"),
            DT::DTOutput(ns("dataset_std"))
          ),
          
          
          #================================================
          # DIAGNÓSTICO
          #================================================
          
          tabPanel(
            "Diagnóstico",
            value = "diagnostico",
            
            br(),
            
            wellPanel(
              p("Se analiza la correlación entre variables antes del PCA.")
            ),
            
            h4("Test de Bartlett"),
            verbatimTextOutput(ns("bartlett_test")),
            
            br(),
            
            h4("Matriz de correlaciones"),
            plotOutput(ns("corrplot"), height = "500px")
          ),
          
          #================================================
          # COMPONENTES
          #================================================
          
          tabPanel(
            "Selección de Componentes",
            value = "componentes",
            
            br(),
            
            wellPanel(
              p("Scree plot y varianza explicada.")
            ),
            
            h4("Scree Plot"),
            plotOutput(ns("scree"), height = "450px"),
            
            br(),
            
            h4("Varianza explicada"),
            DT::DTOutput(ns("var_table")),
            
            br(),
            
            h4("Interpretación"),
            verbatimTextOutput(ns("interp_varianza"))
          ),
          
          #================================================
          # INDIVIDUOS
          #================================================
          
          tabPanel(
            "Individuos",
            value = "individuos",
            
            br(),
            
            wellPanel(
              p("Proyección de las observaciones en el espacio factorial.")
            ),
            
            plotOutput(ns("scores"), height = "600px"),
            
            br(),
            
            h4("Interpretación"),
            verbatimTextOutput(ns("interp_scores")),
            
            br(),
            
            h4("Puntuaciones de los individuos"),
            DT::DTOutput(ns("scores_table"))
          ),
          
          #================================================
          # VARIABLES
          #================================================
          
          tabPanel(
            "Variables",
            value = "variables",
            
            br(),
            
            wellPanel(
              p("Interpretación de cargas factoriales. Las celdas resaltadas superan la magnitud mínima de contribución.")
            ),
            
            h4("Tabla de cargas"),
            DT::DTOutput(ns("loadings_table")),
            
            br(),
            
            h4("Variables más influyentes"),
            DT::DTOutput(ns("loadings_importantes")),
            
            br(),
            
            h4("Interpretación"),
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
    # DATOS
    #--------------------------------------------------
    
    datos_num <- reactive({
      df <- if(!is.null(datos()) && nrow(datos()) > 0)
        datos()
      else
        datos_ejemplo
      
      req(df)
      df_n <- df[, sapply(df, is.numeric), drop = FALSE]
      df_n[complete.cases(df_n), ]
    })
    
    datos_std <- reactive({
      req(datos_num())
      scale(datos_num())
    })
    
    #--------------------------------------------------
    # PCA
    #--------------------------------------------------
    pca_res <- reactive({
      req(datos_num())
      prcomp(datos_num(), scale. = TRUE)
    })
    
    #--------------------------------------------------
    # SELECTORES LATERALES DINÁMICOS (CONDICIONALES POR PESTAÑA)
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
    # RENDERIZADO DINÁMICO DEL BOTÓN DE DESCARGA
    #--------------------------------------------------
    output$ui_dl_pca <- renderUI({
      req(input$k_componentes)
      k <- round(input$k_componentes)
      
      downloadButton(
        ns("dl_pca"),
        paste0("Descargar las ", k, " componentes seleccionadas"),
        class = "btn-success",
        style = "width: 100%;"
      )
    })
    
    #--------------------------------------------------
    # TABLAS DATOS
    #--------------------------------------------------
    output$dataset <- DT::renderDT({
      df_base <- if(!is.null(datos()) && nrow(datos()) > 0) datos() else datos_ejemplo
      req(df_base)
      
      DT::datatable(
        df_base[complete.cases(df_base[, sapply(df_base, is.numeric)]), ], 
        options = list(paging = FALSE, scrollY = "400px", scrollX = TRUE, autoWidth = TRUE),
        class = 'cell-border stripe hover compact'
      ) %>% 
        DT::formatRound(columns = names(datos_num()), digits = 3)
    })
    output$dataset_std <- DT::renderDT({
      req(datos_std())
      
      DT::datatable(
        round(as.data.frame(datos_std()), 3), # Matriz numérica pura tipificada
        options = list(paging = FALSE, scrollY = "400px", scrollX = TRUE, autoWidth = TRUE),
        class = 'cell-border stripe hover compact'
      )
    })
    #--------------------------------------------------
    # DIAGNÓSTICO
    #--------------------------------------------------
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
    
    #--------------------------------------------------
    # SCREE PLOT
    #--------------------------------------------------
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
    #--------------------------------------------------
    # TABLA VARIANZA
    #--------------------------------------------------
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
        options = list(
          paging = FALSE,         # Sin botones inferiores
          scrollY = "300px",      # Un poco más baja por ser menos filas
          scrollX = TRUE,
          autoWidth = TRUE
        ),
        class = 'cell-border stripe hover compact'
      )
    })
    
    #--------------------------------------------------
    # INTERPRETACIÓN VARIANZA
    #--------------------------------------------------
    output$interp_varianza <- renderText({
      req(pca_res(), input$k_componentes)
      k <- round(input$k_componentes)
      
      eig <- pca_res()$sdev^2
      var_acum <- round(sum(eig[1:k]) / sum(eig) * 100, 2)
      
      paste0(
        "Las primeras ", k, " componentes explican conjuntamente el ", var_acum, "% de la variabilidad total de los datos.\n\n",
        "Ejemplo práctico (Dataset 'wines'): En ese conjunto de datos, las 2 primeras componentes suelen ser ",
        "suficientes para capturar más del 55% de la varianza total. Esto demuestra cómo el PCA permite resumir ",
        "las 13 propiedades químicas originales en solo dos dimensiones sin perder demasiada información analítica."
      )
    })
    #--------------------------------------------------
    # INDIVIDUOS
    #--------------------------------------------------
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
        "Ejemplo práctico (Dataset 'wines'): Al graficar la PC1 frente a la PC2, se puede observar cómo las muestras ",
        "de vino se agrupan y separan de forma natural en 3 clusters distintos en el espacio, los cuales corresponden ",
        "exactamente a los 3 tipos de cultivos o variedades de uva italiana analizadas."
      )
    })
    output$scores_table <- DT::renderDT({
      req(pca_res(), input$k_componentes)
      k <- round(input$k_componentes)
      
      df_scores <- as.data.frame(pca_res()$x[, 1:k, drop = FALSE])
      
      DT::datatable(
        round(df_scores, 3),
        options = list(
          paging = FALSE,         # Elimina la paginación verde
          scrollY = "400px",      # Scroll vertical interno
          scrollX = TRUE,
          autoWidth = TRUE
        ),
        class = 'cell-border stripe hover compact'
      )
    })
    
    #--------------------------------------------------
    # VARIABLES (Cargas bicolor: verde positivas, rojo negativas)
    #--------------------------------------------------
    output$loadings_table <- DT::renderDT({
      req(pca_res(), input$k_componentes)
      k <- round(input$k_componentes)
      
      umbral <- if(!is.null(input$magnitud_carga)) input$magnitud_carga else 0.3
      
      df_loadings <- as.data.frame(pca_res()$rotation[, 1:k, drop = FALSE])
      
      DT::datatable(
        round(df_loadings, 3),
        options = list(
          paging = FALSE,         # Quita botones de la captura
          scrollY = "400px",      # Añade barra vertical interna
          scrollX = TRUE,
          autoWidth = TRUE
        ),
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
        options = list(
          paging = FALSE,         # Quita botones
          scrollY = "400px",      # Añade barra vertical
          scrollX = TRUE,
          autoWidth = TRUE
        ),
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
        "Ejemplo práctico (Dataset 'wines'): En la columna PC1, variables de calidad polifenólica como 'Flavanoids' ",
        "y 'Phenols' destacan con valores altos superiores al umbral. Dependiendo de la orientación del eje de R, ",
        "aparecerán fuertemente en verde o rojo, indicando qué variables guían la separación horizontal del mapa. ",
        "En la PC2, suele destacar la fuerte influencia de la variable 'Alcohol'."
      )
    })
    #--------------------------------------------------
    # GESTOR DE DESCARGAS
    #--------------------------------------------------
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
        title = "➕ Gestión: Añadir pregunta personalizada de PCA",
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
      list(texto = "¿Qué porcentaje de varianza total acumulan todas las componentes principales calculadas?", opciones = c("El 50%", "El 100% de la varianza original", "Depende del número de observaciones", "Menos del 10%"), correcta = "El 100% de la varianza original"),
      list(texto = "¿Cómo se comportan las correlaciones entre las distintas componentes principales?", opciones = c("Son altamente correlacionadas", "Su correlación es exactamente cero", "Varían según el tamaño muestral", "Son idénticas a las correlaciones originales"), correcta = "Su correlación es exactamente cero"),
      list(texto = "Si la matriz de correlación es una matriz identidad, ¿qué indica respecto al PCA?", opciones = c("El PCA será sumamente efectivo", "No tiene sentido aplicar PCA porque las variables no están correlacionadas", "Todas las variables se reducirán a una única componente", "Aumentará el error de aproximación"), correcta = "No tiene sentido aplicar PCA porque las variables no están correlacionadas"),
      list(texto = "¿Qué gráfico muestra la importancia de cada componente principal?", opciones = c("Gráfico de dispersión 3D", "Gráfico de sedimentación (Scree plot)", "Histograma de frecuencias", "Diagrama de caja y bigotes"), correcta = "Gráfico de sedimentación (Scree plot)")
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
 

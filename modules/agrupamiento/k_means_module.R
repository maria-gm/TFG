# =====================================================
#   k_means - MODULO
# =====================================================

# -------------------------------
# TEORIA
# -------------------------------

K_means_Teoria_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    #  activar MathJax 
    withMathJax(),
    
    tags$div(
      style = "padding: 20px; background-color: #fcfdfe;",
      
      # =====================================
      # CABECERA
      # =====================================
      h2(
        "K-Means",
        style = "font-weight: 800; color: #1a365d; margin-bottom: 5px;"
      ),
      
      p(
        "Técnica de partición no jerárquica orientada a dividir los datos en grupos exhaustivos y mutuamente excluyentes.",
        style = "color: #64748b; font-size: 1.1rem; margin-bottom: 30px;"
      ),
      
      # =====================================
      # TARJETAS PRINCIPALES
      # =====================================
      bslib::layout_column_wrap(
        width = 1/3, # Tres columnas 
        heights_equal = "row",
        
        # ---------------------------------
        # 1. IDEA PRINCIPAL
        # ---------------------------------
        bslib::card(
          bslib::card_header(
            tags$b("1. Agrupamiento No Jerárquico"),
            style = "background: #e0e7ff;"
          ),
          bslib::card_body(
            p("A diferencia de las técnicas jerárquicas, este método divide un conjunto de \\(N\\) individuos con \\(D\\) variables en \\(K\\) clústeres exhaustivos y mutuamente excluyentes \\(\\mathbf{P} = \\{\\mathbf{P}_1, \\mathbf{P}_2, \\dots, \\mathbf{P}_K\\}\\):"),
            tags$ul(
              style = "margin-top: 10px;",
              tags$li("Agrupación total: \\(\\bigcup_{i=1}^{K} \\mathbf{P}_i = \\mathbf{X}\\) (agrupa a todos los individuos).", style = "margin-bottom: 6px;"),
              tags$li("Exclusión mutua: \\(\\mathbf{P}_i \\cap \\mathbf{P}_j = \\emptyset\\) para \\(1 \\le i \\neq j \\le K\\) (ningún elemento pertenece a múltiples grupos).")
            )
          )
        ),
        
        # ---------------------------------
        # 2. OPTIMIZACIÓN MATEMÁTICA
        # ---------------------------------
        bslib::card(
          bslib::card_header(
            tags$b("2. Función Criterio (SSE)"),
            style = "background: #dcfce7;"
          ),
          bslib::card_body(
            p("El algoritmo busca encontrar la partición óptima minimizando la variabilidad interna o Suma de los Errores Cuadráticos (SSE), conocida como el problema de clustering de mínimo SSE (MSSC):"),
            p("$$\\text{SSE} = \\sum_{i=1}^{K} \\sum_{\\mathbf{x}_j \\in \\mathbf{P}_i} \\|\\mathbf{x}_j - \\mathbf{c}_i\\|_2^2$$"),
            p("Donde \\(\\|\\cdot\\|_2\\) representa la norma euclídea y \\(\\mathbf{c}_i = \\frac{1}{|\\mathbf{P}_i|} \\sum_{\\mathbf{x}_j \\in \\mathbf{P}_i} \\mathbf{x}_j\\) es el centroide (media) del clúster.")
          )
        ),
        
        # ---------------------------------
        # 3. LIMITACIONES CRÍTICAS
        # ---------------------------------
        bslib::card(
          bslib::card_header(
            tags$b("3. Limitaciones Técnicas"),
            style = "background: #fef3c7;"
          ),
          bslib::card_body(
            p("Pese a su gran eficiencia y simplicidad, el investigador debe considerar las siguientes restricciones del modelo (Celebi et al., 2013):"),
            tags$ul(
              style = "margin-top: 10px;",
              tags$li("Parámetro fijo: Requiere que el número de grupos \\(K\\) se determine rigurosamente a priori.", style = "margin-bottom: 6px;"),
              tags$li("Sensibilidad local: Es altamente sensible a los valores atípicos (outliers) y a la inicialización arbitraria de los centroides.")
            )
          )
        )
      ), 
      
      br(),
      
      # =====================================
      # ALGORITMO ITERATIVO K-MEANS
      # =====================================
      bslib::card(
        style = "border: 1px solid #cbd5e1; background: #f8fafc;",
        bslib::card_body(
          h4(
            icon("gears"), "Algoritmo Iterativo K-Means (Celebi et al., 2013)",
            style = "color: #1e40af; margin-bottom: 15px;"
          ),
          p("A través de pruebas sucesivas e iterativas, el algoritmo reduce la varianza residual siguiendo cuatro fases secuenciales:"),
          
          tags$ul(
            style = "margin-top: 10px;",
            tags$li(
              tags$b("Fase 1: Inicialización de variables."), 
              " Se seleccionan arbitrariamente \\(K\\) puntos iniciales dentro del espacio de datos para actuar como los primeros centroides del sistema.",
              style = "margin-bottom: 10px;"
            ),
            tags$li(
              tags$b("Fase 2: Asignación de observaciones."), 
              " Cada individuo \\(\\mathbf{x}\\) es asignado al clúster cuyo centroide sea el más cercano en distancia euclídea al cuadrado, garantizando la mínima varianza:",
              p("$$\\mathbf{P}_i^{(t)} = \\left\\{ \\mathbf{x} : \\|\\mathbf{x} - \\mathbf{c}_i^{(t)}\\|_2^2 \\le \\|\\mathbf{x} - \\mathbf{c}_j^{(t)}\\|_2^2 \\quad \\forall j, \\, 1 \\le j \\le K \\right\\}$$"),
              style = "margin-bottom: 10px;"
            ),
            tags$li(
              tags$b("Fase 3: Actualización de centroides."), 
              " Tras reasignar todos los elementos de la muestra, se recalculan las coordenadas vectoriales de los centroides computando la media aritmética exacta de sus nuevos componentes:",
              p("$$\\mathbf{c}_i^{(t+1)} = \\frac{1}{|\\mathbf{P}_i^{(t)}|} \\sum_{\\mathbf{x}_j \\in \\mathbf{P}_i^{(t)}} \\mathbf{x}_j$$"),
              style = "margin-bottom: 10px;"
            ),
            tags$li(
              tags$b("Fase 4: Condición de convergencia."), 
              " Las fases 2 y 3 se repiten iterativamente hasta que el algoritmo converge, es decir, cuando la posición de los centroides en la iteración actual coincide exactamente con la anterior (\\(\\mathbf{c}_i^{(t+1)} = \\mathbf{c}_i^{(t)}\\))."
            )
          )
        )
      ),
      
      br(),
      
      # =====================================
      # INTERPRETACIÓN PRÁCTICA
      # =====================================
      bslib::card(
        style = "border: 1px solid #cbd5e1; background: #f8fafc;",
        bslib::card_body(
          h4(
            icon("circle-nodes"), "Interpretación de los Resultados",
            style = "color: #1e40af; margin-bottom: 12px;"
          ),
          p("El análisis práctico del clustering resultante se apoya en los siguientes fundamentos conceptuales:"),
          tags$ul(
            style = "margin-top: 10px;",
            tags$li(tags$b("Homogeneidad intragrupo:"), " Los individuos que comparten el mismo clúster presentan perfiles e índices de comportamiento altamente similares.", style = "margin-bottom: 6px;"),
            tags$li(tags$b("Representación grupal:"), " Los centroides finales calculados actúan como los perfiles vectoriales promedio de cada tipología detectada.", style = "margin-bottom: 6px;"),
            tags$li(tags$b("Separación intergrupo:"), " Clústeres con fronteras bien definidas e hiperplanos distantes indican una clasificación robusta y de alta calidad.")
          )
        )
      )
    )
  )
}


K_means_Teoria_Server <- function(id){
  
  moduleServer(id, function(input, output, session){
    
  })
  
}


# -------------------------------
# ANÁLISIS 
# -------------------------------
K_means_Analisis_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Título
    h3("Análisis", 
       style = "color: #1a446c; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; font-weight: 600; margin-top: 40px; margin-bottom: 20px; border-bottom: 2px solid #f4f6f9; padding-bottom: 10px;"),
    
    fluidRow(
      #--------------------------------------------------
      # PANEL LATERAL
      #--------------------------------------------------
      column(4,
             wellPanel(
               h4("Configuración"),
               p("Seleccione los parámetros para inicializar los centroides y definir el número de clústeres."),
               hr(),
               
               # --- ESTO SE VE SIEMPRE ---
               numericInput(ns("k"), "Número de clústeres (k):", 
                            value = 3, min = 2, max = 10),
               
               selectInput(ns("metodo"), "Base del cálculo:",
                           choices = c("Todas las variables numéricas" = "kmeans",
                                       "Solo componentes PCA" = "pca")),
               
               # --- ESTO SOLO SE VE EN LA PESTAÑA DE VISUALIZACIÓN ---
               conditionalPanel(
                 condition = sprintf("input['%s'] == '3. Visualización'", ns("tabs_kmeans")),
                 uiOutput(ns("var_categorica_ui"))
               ),
               
               hr(),
               helpText("Nota: Se eliminan filas con valores faltantes automáticamente."),
               br(),
               # Botón de descarga
               downloadButton(ns("dl_data"), "Descargar Clústeres (CSV)", class = "btn-success", style = "width: 100%;")
             )
      ),
      
      #--------------------------------------------------
      # PANEL PRINCIPAL
      #--------------------------------------------------
      column(8,
             # Contenedor dinámico para el banner de error rojo 
             uiOutput(ns("mensaje_error_ui")),
             
             tabsetPanel(
               id = ns("tabs_kmeans"),
               
               #================================================
               # PESTAÑA 1: DATOS
               #================================================
               tabPanel("1. Datos",
                        br(),
                        p("Información: El algoritmo K-means calcula los centroides basándose en las distancias euclídeas de las columnas numéricas normalizadas.", 
                          style = "color: #7f8c8d; font-style: italic; margin-bottom: 25px;"),
                        h4("Dataset original preparado", style = "color: #2c3e50; font-weight: 500;"),
                        DT::DTOutput(ns("tabla_datos")),
                        br(), hr(), br(),
                        h4("Dataset estandarizado", style = "color: #2c3e50; font-weight: 500;"),
                        DT::DTOutput(ns("tabla_scale"))
               ),
               
               #================================================
               # PESTAÑA 2: DIAGNÓSTICO
               #================================================
               tabPanel("2. Diagnóstico de 'k'",
                        br(),
                        fluidRow(
                          column(6, plotOutput(ns("elbow_plot"), height = "400px")),
                          column(6, plotOutput(ns("silhouette_plot"), height = "400px"))
                        ),
                        br(),
                        h4("Interpretación del Diagnóstico", style = "color: #2c3e50; font-weight: 500;"),
                        verbatimTextOutput(ns("interp_diagnostico"))
               ),
               
               #================================================
               # PESTAÑA 3: VISUALIZACIÓN
               #================================================
               tabPanel("3. Visualización",
                        br(),
                        plotOutput(ns("cluster_plot"), height = "500px"),
                        br(),
                        h4("Interpretación", style = "color: #2c3e50; font-weight: 500;"),
                        verbatimTextOutput(ns("interp_kmeans"))
               )
             )
      )
    )
  )
}
K_means_Analisis_Server <- function(id, datos, datos_ejemplo = NULL) {
  
  moduleServer(id, function(input, output, session) {
    
    # --- 1.  VALIDACIÓN Y PREPROCESADO GLOBAL ---
    datos_preprocesados <- reactive({
      df <- if(!is.null(datos()) && nrow(datos()) > 0) datos() else datos_ejemplo
      
      # Generador del banner de error rojo 
      crear_banner_error <- function(mensaje) {
        div(
          class = "alert alert-danger",
          style = "background-color: #f2dede; color: #a94442; border-color: #ebccd1; padding: 15px; margin-bottom: 20px; border: 1px solid transparent; border-radius: 4px; font-family: inherit;",
          tags$b(icon("triangle-exclamation"), " No es posible realizar el análisis."),
          br(),
          tags$span(style = "font-style: italic; color: #555555;", "Información: El análisis se ejecuta exclusivamente sobre las columnas de tipo numérico."),
          br(), br(),
          tags$b(mensaje)
        )
      }
      
      # Validaciones del dataset
      if (is.null(df)) {
        return(list(valido = FALSE, ui_error = crear_banner_error("No se han detectado datos cargados. Por favor, suba un archivo o seleccione un ejemplo.")))
      }
      if (nrow(df) < 10) {
        return(list(valido = FALSE, ui_error = crear_banner_error("Se requieren al menos 10 observaciones válidas para estructurar agrupaciones estables por K-means.")))
      }
      
      # Filtro de casos completos 
      df_limpio <- df[complete.cases(df), , drop = FALSE]
      if (nrow(df_limpio) < 10) {
        return(list(valido = FALSE, ui_error = crear_banner_error("El dataset no contiene suficientes filas completas (mínimo 10) tras eliminar registros con valores perdidos (NA).")))
      }
      
      #  filtro de columnas cuantitativas
      df_num <- df_limpio[, sapply(df_limpio, is.numeric), drop = FALSE]
      columnas_reales <- sapply(df_num, function(x) length(unique(x)) > 15)
      
      if(sum(columnas_reales) < 2) {
        if (ncol(df_num) < 2) {
          return(list(valido = FALSE, ui_error = crear_banner_error("Se requieren al menos dos variables numéricas en el conjunto de datos.")))
        }
      } else {
        df_num <- df_num[, columnas_reales, drop = FALSE]
      }
      
      # Control de varianza cero 
      sd_cols <- sapply(df_num, sd, na.rm = TRUE)
      if (any(sd_cols == 0)) {
        return(list(valido = FALSE, ui_error = crear_banner_error("Una o más variables cuantitativas seleccionadas tienen varianza cero (constantes) y no pueden estandarizarse.")))
      }
      
      #  cálculo de la matriz 
      rownames(df_limpio) <- 1:nrow(df_limpio)
      rownames(df_num) <- 1:nrow(df_num)
      df_scaled <- scale(df_num)
      
      return(list(
        valido = TRUE,
        base   = df_limpio,
        num    = df_num,
        scaled = df_scaled
      ))
    })
    
    # --- 2. RENDERIZADO DEL BANNER DE ERROR EN LA UI ---
    output$mensaje_error_ui <- renderUI({
      prep <- datos_preprocesados()
      if (!prep$valido) {
        return(prep$ui_error)
      }
      return(NULL) 
    })
    
    # --- 3. ENLACES REACTIVOS  ---
    datos_base   <- reactive({ req(datos_preprocesados()$valido); datos_preprocesados()$base })
    datos_num    <- reactive({ req(datos_preprocesados()$valido); datos_preprocesados()$num })
    datos_scaled <- reactive({ req(datos_preprocesados()$valido); datos_preprocesados()$scaled })
    
    pca_res <- reactive({
      req(datos_scaled())
      prcomp(datos_scaled())
    })
    
    res_kmeans <- reactive({
      req(input$k, datos_scaled())
      set.seed(42)
      if(input$metodo == "kmeans") {
        kmeans(datos_scaled(), centers = input$k, nstart = 25)
      } else {
        kmeans(pca_res()$x[, 1:2], centers = input$k, nstart = 25)
      }
    })
    
    df_final <- reactive({
      req(pca_res(), res_kmeans(), datos_base())
      
      df_viz <- as.data.frame(pca_res()$x[, 1:2])
      df_viz$cluster <- factor(res_kmeans()$cluster)
      
      if(!is.null(input$var_cat) && input$var_cat != "Ninguna" && input$var_cat != "") {
        var_real <- datos_base()[[input$var_cat]]
        if(length(var_real) == nrow(df_viz)) {
          df_viz$realidad <- as.factor(var_real)
        }
      }
      df_viz
    })
    
    # --- 4. OUTPUT: TABLAS ---
    output$tabla_datos <- DT::renderDT({
      req(datos_preprocesados()$valido)
      DT::datatable(
        datos_base(), 
        options = list(paging = FALSE, scrollY = "400px", scrollX = TRUE, autoWidth = TRUE),
        class = 'cell-border stripe hover compact'
      ) %>% 
        DT::formatRound(columns = names(datos_num()), digits = 3)
    })
    
    output$tabla_scale <- DT::renderDT({
      req(datos_scaled())
      DT::datatable(
        round(as.data.frame(datos_scaled()), 3), 
        options = list(paging = FALSE, scrollY = "400px", scrollX = TRUE, autoWidth = TRUE),
        class = 'cell-border stripe hover compact'
      )
    })
    
    # --- 5. OUTPUT: DIAGNÓSTICO ---
    output$elbow_plot <- renderPlot({
      req(datos_scaled())
      factoextra::fviz_nbclust(datos_scaled(), kmeans, method = "wss") + 
        theme_minimal() + labs(title="Optimización: Método del Codo")
    })
    
    output$silhouette_plot <- renderPlot({
      req(datos_scaled())
      factoextra::fviz_nbclust(datos_scaled(), kmeans, method = "silhouette") + 
        theme_minimal() + labs(title="Optimización: Coeficiente de Silueta")
    })
    
    output$interp_diagnostico <- renderText({
      req(datos_scaled())
      paste0(
        "Las métricas de diagnóstico estiman el número óptimo de clústeres evaluando la cohesión y separación matemática de los grupos.\n",
        "- Método del Codo (WSS): Busca el punto de inflexión donde añadir un clúster adicional no reduce drásticamente la varianza interna.\n",
        "- Coeficiente de Silueta: Maximiza el promedio de la silueta; el pico más alto representa la estructura de clústeres más limpia.\n\n",
        "Ejemplo práctico (Dataset 'penguins'): Al evaluar este conjunto de datos, notarás que tanto la curva del codo como el ",
        "gráfico de silueta muestran señales muy claras en k = 3. Esto confirma matemáticamente que dividir la morfología de las ",
        "observaciones en tres grandes bloques es la estructura más robusta y natural disponible."
      )
    })
    
    # --- 6. OUTPUT: VISUALIZACIÓN ---
    output$cluster_plot <- renderPlot({
      req(df_final())
      df <- df_final()
      p <- ggplot(df, aes(x = PC1, y = PC2))
      
      if(!is.null(input$var_cat) && input$var_cat != "Ninguna" && input$var_cat != "") {
        p <- p + geom_point(aes(color = realidad, shape = cluster), size = 3.5, alpha = 0.8) +
          labs(color = input$var_cat, shape = "Clúster asignado")
      } else {
        p <- p + geom_point(aes(color = cluster), size = 3, alpha = 0.7) +
          labs(color = "Clúster") +
          ggsci::scale_color_jco()
      }
      
      p + stat_ellipse(aes(fill = cluster), geom = "polygon", alpha = 0.1, color = "black", linetype = 2) +
        ggsci::scale_fill_jco() +
        guides(fill = "none") + 
        theme_minimal() +
        theme(legend.position = "right") +
        labs(title = paste("Resultado K-means (k =", input$k, ")"),
             subtitle = "Proyección sobre las dos primeras componentes principales")
    })   
    
    output$interp_kmeans <- renderText({
      req(input$k)
      paste0(
        "El gráfico proyecta la dispersión espacial de los datos. Las elipses delimitan el radio de asignación de los centroides estables.\n\n",
        "Ejemplo práctico (Dataset 'penguins'): Con k = 3, si activas la visualización de la 'Variable Real' seleccionando ",
        "'species', descubrirás que las agrupaciones matemáticas de K-means clonan casi a la perfección las familias reales. ",
        "El grupo correspondiente a los pingüinos Gentoo se aísla de forma inmediata a la derecha, mientras que los clústeres de ",
        "las especies Adelie y Chinstrap se localizan más cercanos debido a sutiles semejanzas en su tamaño corporal y del pico."
      )
    })
    
    # --- 7. ENLACES DE CONTROL ---
    output$var_categorica_ui <- renderUI({
      prep <- datos_preprocesados()
      req(prep$valido)
      df <- prep$base
      nom_cats <- names(df)[sapply(df, function(x) {
        is.factor(x) || is.character(x) || (is.numeric(x) && length(unique(x)) <= 5)
      })]
      
      selectInput(session$ns("var_cat"), "Variable Real (Color):", 
                  choices = c("Ninguna", nom_cats),
                  selected = "Ninguna")
    })
    
    output$dl_data <- downloadHandler(
      filename = function() { paste("Resultados_Clustering_Kmeans.csv", sep = "") },
      content = function(file) {
        req(datos_base(), res_kmeans())
        df_export <- datos_base()
        df_export$Cluster_Asignado <- res_kmeans()$cluster
        write.csv(df_export, file, row.names = TRUE)
      }
    )
    
  }) 
}
#-------------------------------
# AUTOEVALUACIÓN
#-------------------------------
K_means_Auto_UI <- function(id) {
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
        title = "➕ Gestión: Añadir pregunta personalizada de K-Means",
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
        
        actionButton(ns("add"), "Guardar pregunta en el banco del K-Means", class = "btn-success btn-sm mt-2")
      )
    )
  )
}
K_means_Auto_Server <- function(id) {
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
      list(
        texto = "¿Cuál es el objetivo principal del K-means?",
        opciones = c(
          "Calcular la matriz de cargas factoriales",
          "Agrupar observaciones minimizando la varianza dentro de cada cluster",
          "Predecir una variable continua dependiente",
          "Encontrar las componentes de máxima varianza"
        ),
        correcta = "Agrupar observaciones minimizando la varianza dentro de cada cluster"
      ),
      list(
        texto = "¿Qué representa la K en el algoritmo K-means?",
        opciones = c(
          "El número predefinido de clusters que se desean obtener",
          "El número de iteraciones máximas",
          "La distancia máxima permitida entre clusters",
          "El coeficiente de correlación de los datos"
        ),
        correcta = "El número predefinido de clusters que se desean obtener"
      ),
      list(
        texto = "¿Cómo se visualizan habitualmente los resultados de K-means con muchas variables?",
        opciones = c(
          "Mediante un dendrograma jerárquico",
          "Utilizando únicamente tablas de contingencia",
          "Proyectando los clusters en el plano de las dos primeras componentes principales (PCA)",
          "Mediante una matriz identidad de confusión"
        ),
        correcta = "Proyectando los clusters en el plano de las dos primeras componentes principales (PCA)"
      ),
      list(
        texto = "¿Cómo se suele obtener el número K óptimo de clusters?",
        opciones = c(
          "Calculando el P-valor de una prueba ANOVA",
          "Extrayendo únicamente los autovalores mayores que 1",
          "Usando el método del codo (Elbow method) o la anchura de silueta",
          "Probando todas las combinaciones posibles hasta infinito"
        ),
        correcta = "Usando el método del codo (Elbow method) o la anchura de silueta"
      ),
      list(
        texto = "¿Qué propiedad cumplen los puntos asignados a un mismo cluster en K-means?",
        opciones = c(
          "Presentan una correlación exactamente lineal con el origen",
          "Tienen todos la misma varianza e idénticos valores originales",
          "Son linealmente independientes de los centroides",
          "Están más cerca del centroide de su propio cluster que de cualquier otro centroide"
        ),
        correcta = "Están más cerca del centroide de su propio cluster que de cualquier otro centroide"
      ),
      list(
        texto = "¿Qué es el centroide en el contexto de K-means?",
        opciones = c(
          "El vector de medias de las variables para las observaciones asignadas a ese cluster",
          "El punto más alejado del conjunto de datos",
          "El error estándar residual acumulado",
          "El primer autovector calculado mediante SVD"
        ),
        correcta = "El vector de medias de las variables para las observaciones asignadas a ese cluster"
      ),
      list(
        texto = "¿Qué problema puede surgir debido a la inicialización aleatoria de los centroides en K-means?",
        opciones = c(
          "Que el algoritmo nunca llegue a converger",
          "Que las variables se estandaricen automáticamente de forma errónea",
          "Que los resultados dependan de la semilla inicial y se caiga en un mínimo local",
          "Que el número K cambie dinámicamente a mitad del proceso"
        ),
        correcta = "Que los resultados dependan de la semilla inicial y se caiga en un mínimo local"
      ),
      list(
        texto = "¿Por qué es fundamental estandarizar las variables antes de aplicar K-means?",
        opciones = c(
          "Para transformar todas las variables en categóricas",
          "Para evitar que las variables con magnitudes o escalas más grandes dominen el cálculo de las distancias",
          "Porque el algoritmo solo funciona con datos distribuidos de forma normal perfecta",
          "Para reducir la dimensionalidad eliminando variables"
        ),
        correcta = "Para evitar que las variables con magnitudes o escalas más grandes dominen el cálculo de las distancias"
      ),
      list(
        texto = "¿Cómo se comporta K-means ante la presencia de valores atípicos (outliers)?",
        opciones = c(
          "Es completamente inmune y los ignora de manera automática",
          "Los agrupa a todos en un cluster especial llamado K-medio",
          "Es muy sensible, ya que los outliers pueden distorsionar fuertemente la posición de los centroides",
          "Los transforma en la media general de los datos"
        ),
        correcta = "Es muy sensible, ya que los outliers pueden distorsionar fuertemente la posición de los centroides"
      ),
      list(
        texto = "¿Cuándo se detiene (converge) el proceso iterativo de K-means?",
        opciones = c(
          "Cuando las asignaciones de los puntos a los clusters ya no cambian o se llega al límite de iteraciones",
          "Cuando se alcanza el número de factores solicitado por Kaiser",
          "Cuando la varianza total explicada llega exactamente al 100%",
          "Cuando la distancia entre todos los centroides es igual a cero"
        ),
        correcta = "Cuando las asignaciones de los puntos a los clusters ya no cambian o se llega al límite de iteraciones"
      ),
      list(
        texto = "¿Para qué tipo de formas de clusters funciona mejor K-means?",
        opciones = c(
          "Clusters con formas alargadas o de luna contorneada",
          "Estructuras de datos puramente jerárquicas anidadas",
          "Agrupaciones basadas estrictamente en densidades locales lineales",
          "Clusters compactos y de forma aproximadamente esférica"
        ),
        correcta = "Clusters compactos y de forma aproximadamente esférica"
      ),
      list(
        texto = "¿Qué mide Inercia (Within-cluster sum-of-squares) devuelta por K-means?",
        opciones = c(
          "La distancia de separación que existe entre los centroides de los clusters",
          "La suma de las distancias al cuadrado de cada punto al centroide de su cluster asignado",
          "El número total de observaciones mal clasificadas en el dataset",
          "La correlación global promedio entre todas las variables del estudio"
        ),
        correcta = "La suma de las distancias al cuadrado de cada punto al centroide de su cluster asignado"
      ),
      list(
        texto = "Tras cruzar los 3 clusters obtenidos mediante K-means con la variable real 'species' de los pingüinos, observas que Adelie y Chinstrap se mezclan en un mismo cluster. ¿A qué se debe esto?",
        opciones = c(
          "A un error de convergencia del algoritmo que requiere aumentar drásticamente el número de iteraciones",
          "A que K-means obliga a que todos los clusters tengan exactamente el mismo número de observaciones por especie",
          "A que ambas especies comparten características físicas similares en tamaño y proporciones, haciendo que sus nubes de puntos se solapen",
          "A que los pingüinos Chinstrap son un subtipo genético directo de los pingüinos Gentoo"
        ),
        correcta = "A que ambas especies comparten características físicas similares en tamaño y proporciones, haciendo que sus nubes de puntos se solapen"
      ),
      list(
        texto = "Al interpretar los centroides finales de un K-means (K=2) en el dataset 'penguins', el Cluster 1 muestra medias muy altas de longitud de aleta y masa corporal en comparación con el Cluster 2. ¿Qué conclusión biológica es correcta?",
        opciones = c(
          "El Cluster 1 ha agrupado mayoritariamente a los pingüinos de la especie Gentoo, que son notablemente más grandes",
          "El Cluster 1 representa exclusivamente a los pingüinos hembra de cualquier especie",
          "El algoritmo ha separado a los pingüinos que viven en zonas cálidas de los que viven en zonas frías",
          "El Cluster 2 está compuesto únicamente por individuos jóvenes con picos hiperdesarrollados"
        ),
        correcta = "El Cluster 1 ha agrupado mayoritariamente a los pingüinos de la especie Gentoo, que son notablemente más grandes"
      ),
      list(
        texto = "Al graficar el resultado de K-means en el dataset 'penguins', ¿qué significa cada punto del gráfico?",
        opciones = c(
          "Cada punto representa la media total de una especie diferente",
          "Cada punto representa a un pingüino individual del dataset",
          "Cada punto representa una isla geográfica del archipiélago Palmer",
          "Cada punto representa una variable médica como el sexo o la edad"
        ),
        correcta = "Cada punto representa a un pingüino individual del dataset"
      )
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

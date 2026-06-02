# =====================================================
#   k_means - MODULO
# =====================================================


# -------------------------------
# TEORIA
# -------------------------------

K_means_Teoria_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Única llamada necesaria para activar MathJax en toda la página
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
        width = 1/3, # Tres columnas perfectamente niveladas
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
      ), # Fin del layout_column_wrap
      
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
    # Título personalizado idéntico al de los módulos anteriores
    h3("Análisis", 
       style = "color: #1a446c; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; font-weight: 600; margin-top: 40px; margin-bottom: 20px; border-bottom: 2px solid #f4f6f9; padding-bottom: 10px;"),
    
    fluidRow(
      #--------------------------------------------------
      # PANEL LATERAL (A LA IZQUIERDA - IDÉNTICO AL JERÁRQUICO)
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
               # Botón de descarga idéntico al del módulo jerárquico
               downloadButton(ns("dl_data"), "Descargar Clústeres (CSV)", class = "btn-success", style = "width: 100%;")
             )
      ),
      
      #--------------------------------------------------
      # PANEL PRINCIPAL (A LA DERECHA)
      #--------------------------------------------------
      column(8,
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
                        h4("Dataset estandarizado (Z-scores)", style = "color: #2c3e50; font-weight: 500;"),
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
    
    # 1. DATOS BASE (LIMPIEZA MAESTRA)
    datos_base <- reactive({
      df <- if(!is.null(datos()) && nrow(datos()) > 0) datos() else datos_ejemplo
      req(df)
      df[complete.cases(df), , drop = FALSE]
    })
    
    # 2. IDENTIFICACIÓN DE NUMÉRICOS
    datos_num <- reactive({
      df <- datos_base()
      df_num <- df[, sapply(df, is.numeric), drop = FALSE]
      columnas_reales <- sapply(df_num, function(x) length(unique(x)) > 15)
      
      if(sum(columnas_reales) == 0) return(df_num)
      df_num[, columnas_reales, drop = FALSE]
    })
    
    # 3. ESCALADO
    datos_scaled <- reactive({
      req(ncol(datos_num()) >= 2)
      d_num <- datos_num()
      d_num <- d_num[, sapply(d_num, sd) > 0, drop = FALSE]
      scale(d_num)
    })
    
    # 4. PCA
    pca_res <- reactive({
      req(datos_scaled())
      prcomp(datos_scaled())
    })
    
    # 5. EJECUCIÓN K-MEANS
    res_kmeans <- reactive({
      req(input$k, datos_scaled())
      set.seed(42)
      if(input$metodo == "kmeans") {
        kmeans(datos_scaled(), centers = input$k, nstart = 25)
      } else {
        kmeans(pca_res()$x[, 1:2], centers = input$k, nstart = 25)
      }
    })
    
    # 6. SELECTOR DINÁMICO
    output$var_categorica_ui <- renderUI({
      df <- datos_base()
      nom_cats <- names(df)[sapply(df, function(x) {
        is.factor(x) || is.character(x) || (is.numeric(x) && length(unique(x)) <= 5)
      })]
      
      selectInput(session$ns("var_cat"), "Variable Real (Color):", 
                  choices = c("Ninguna", nom_cats),
                  selected = "Ninguna")
    })
    
    # 7. UNIÓN DE RESULTADOS
    df_final <- reactive({
      req(pca_res(), res_kmeans())
      
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
    
    # --- OUTPUTS: TABLAS (ORIGINAL ENTERO VS NUMÉRICO PURO) ---
    output$tabla_datos <- DT::renderDT({
      DT::datatable(
        datos_base(), # Dataset original completo con categóricas
        options = list(paging = FALSE, scrollY = "400px", scrollX = TRUE, autoWidth = TRUE),
        class = 'cell-border stripe hover compact'
      ) %>% 
        DT::formatRound(columns = names(datos_num()), digits = 3)
    })
    
    output$tabla_scale <- DT::renderDT({
      req(datos_scaled())
      DT::datatable(
        round(as.data.frame(datos_scaled()), 3), # Matriz pura de números tipificados
        options = list(paging = FALSE, scrollY = "400px", scrollX = TRUE, autoWidth = TRUE),
        class = 'cell-border stripe hover compact'
      )
    })
    
    # --- OUTPUTS: DIAGNÓSTICO ---
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
    
    # --- OUTPUTS: VISUALIZACIÓN Y CONTROL ---
    output$cluster_plot <- renderPlot({
      validate(need(df_final(), "Sincronizando datos..."))
      
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
    
    # --- GESTOR DE DESCARGAS (IGUAL AL DE CLUSTERING JERÁRQUICO) ---
    output$dl_data <- downloadHandler(
      filename = function() {
        paste("Resultados_Clustering_Kmeans.csv", sep = "")
      },
      content = function(file) {
        req(datos_base(), res_kmeans())
        df_export <- datos_base()
        # Añade la columna de clústeres calculados antes de exportar
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
    # Encabezado estilizado
    h3("Autoevaluación", 
       style = "color: #1a446c; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; font-weight: 600; margin-top: 40px; margin-bottom: 25px; border-bottom: 2px solid #f4f6f9; padding-bottom: 10px;"),
    
    # 1. Bloque dinámico donde se imprimen las preguntas del AF (ahora van primero)
    uiOutput(ns("preguntas")),
    
    br(),
    
    # 2. Barra de acciones y puntuación 
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
    
    # 3. Formulario colapsable para agregar preguntas (queda al final del todo)
    accordion(
      open = FALSE,
      class = "shadow-sm border-0",
      accordion_panel(
        title = "➕ Gestión: Añadir pregunta personalizada de Análisis Factorial",
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
        actionButton(ns("add"), "Guardar pregunta", class = "btn-success btn-sm mt-2")
      )
    )
  )
}

K_means_Auto_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # BANCO CORREGIDO, COMPLETADO Y EXTENDIDO A 15 PREGUNTAS DE K-MEANS
    preguntas_base <- list(
      list(
        texto = "¿Cuál es el objetivo principal del K-means?",
        opciones = c("a) Agrupar observaciones minimizando la varianza dentro de cada cluster", "b) Calcular la matriz de cargas factoriales", "c) Predecir una variable continua dependiente", "d) Encontrar las componentes de máxima varianza"),
        correcta = "a) Agrupar observaciones minimizando la varianza dentro de cada cluster"
      ),
      list(
        texto = "¿Qué representa la 'K' en el algoritmo K-means?",
        opciones = c("a) El número de iteraciones máximas", "b) El número predefinido de clusters que se desean obtener", "c) La distancia máxima permitida entre clusters", "d) El coeficiente de correlación de los datos"),
        correcta = "b) El número predefinido de clusters que se desean obtener"
      ),
      list(
        texto = "¿Cómo se visualizan habitualmente los resultados de K-means con muchas variables?",
        opciones = c("a) Mediante un dendrograma jerárquico", "b) Proyectando los clusters en el plano de las dos primeras componentes principales (PCA)", "c) Utilizando únicamente tablas de contingencia", "d) Mediante una matriz identidad de confusión"),
        correcta = "b) Proyectando los clusters en el plano de las dos primeras componentes principales (PCA)"
      ),
      list(
        texto = "¿Cómo se suele obtener el número 'K' óptimo de clusters?",
        opciones = c("a) Usando el método del codo (Elbow method) o la anchura de silueta", "b) Calculando el P-valor de una prueba ANOVA", "c) Extrayendo únicamente los autovalores mayores que 1", "d) Probando todas las combinaciones posibles hasta infinito"),
        correcta = "a) Usando el método del codo (Elbow method) o la anchura de silueta"
      ),
      list(
        texto = "¿Qué propiedad cumplen los puntos asignados a un mismo cluster en K-means?",
        opciones = c("a) Están más cerca del centroide de su propio cluster que de cualquier otro centroide", "b) Presentan una correlación exactamente lineal con el origen", "c) Tienen todos la misma varianza e idénticos valores originales", "d) Son linealmente independientes de los centroides"),
        correcta = "a) Están más cerca del centroide de su propio cluster que de cualquier otro centroide"
      ),
      list(
        texto = "En un algoritmo de clustering basado en densidad como DBSCAN existe un punto núcleo 'p'. ¿Cómo se diferencian K-means y DBSCAN respecto a la noción de vecindad de un punto?",
        opciones = c("a) K-means asume clusters de formas arbitrarias basados en densidad y DBSCAN formas esféricas", "b) K-means asigna puntos al centroide más cercano (partición global) y DBSCAN se basa en vecindades locales de radio Épsilon", "c) No hay ninguna diferencia, ambos calculan centroides obligatoriamente", "d) K-means requiere que todos los puntos del cluster se encuentren dentro del radio Épsilon de 'p'"),
        correcta = "b) K-means asigna puntos al centroide más cercano (partición global) y DBSCAN se basa en vecindades locales de radio Épsilon"
      ),
      list(
        texto = "¿A qué tipo de familias de algoritmos pertenece K-means?",
        opciones = c("a) Algoritmos jerárquicos divisivos", "b) Algoritmos de particionamiento no jerárquico", "c) Algoritmos de aprendizaje supervisado", "d) Modelos probabilísticos factoriales"),
        correcta = "b) Algoritmos de particionamiento no jerárquico"
      ),
      list(
        texto = "¿Qué es el centroide en el contexto de K-means?",
        opciones = c("a) El punto más alejado del conjunto de datos", "b) El vector de medias de las variables para las observaciones asignadas a ese cluster", "c) El error estándar residual acumulado", "d) El primer autovector calculado mediante SVD"),
        correcta = "b) El vector de medias de las variables para las observaciones asignadas a ese cluster"
      ),
      list(
        texto = "¿Qué problema puede surgir debido a la inicialización aleatoria de los centroides en K-means?",
        opciones = c("a) Que el algoritmo nunca llegue a converger", "b) Que los resultados dependan de la semilla inicial y se caiga en un mínimo local", "c) Que las variables se estandaricen automáticamente de forma errónea", "d) Que el número K cambie dinámicamente a mitad del proceso"),
        correcta = "b) Que los resultados dependan de la semilla inicial y se caiga en un mínimo local"
      ),
      list(
        texto = "¿Por qué es fundamental estandarizar las variables antes de aplicar K-means?",
        opciones = c("a) Para evitar que las variables con magnitudes o escalas más grandes dominen el cálculo de las distancias", "b) Para transformar todas las variables en categóricas", "c) Porque el algoritmo solo funciona con datos distribuidos de forma normal perfecta", "d) Para reducir la dimensionalidad eliminando variables"),
        correcta = "a) Para evitar que las variables con magnitudes o escalas más grandes dominen el cálculo de las distancias"
      ),
      # 5 PREGUNTAS MÁS PARA COMPLETAR EL BANCO DE 15
      list(
        texto = "¿Qué métrica de distancia utiliza de forma estándar el algoritmo clásico de K-means?",
        opciones = c("a) Distancia de Manhattan", "b) Distancia Euclídea", "c) Distancia de Mahalanobis", "d) Distancia de Jaccard"),
        correcta = "b) Distancia Euclídea"
      ),
      list(
        texto = "¿Cómo se comporta K-means ante la presencia de valores atípicos (outliers)?",
        opciones = c("a) Es muy sensible, ya que los outliers pueden distorsionar fuertemente la posición de los centroides", "b) Es completamente inmune y los ignora de manera automática", "c) Los agrupa a todos en un cluster especial llamado 'K-medio'", "d) Los transforma en la media general de los datos"),
        correcta = "a) Es muy sensible, ya que los outliers pueden distorsionar fuertemente la posición de los centroides"
      ),
      list(
        texto = "¿Cuándo se detiene (converge) el proceso iterativo de K-means?",
        opciones = c("a) Cuando se alcanza el número de factores solicitado por Kaiser", "b) Cuando las asignaciones de los puntos a los clusters ya no cambian o se llega al límite de iteraciones", "c) Cuando la varianza total explicada llega exactamente al 100%", "d) Cuando la distancia entre todos los centroides es igual a cero"),
        correcta = "b) When las asignaciones de los puntos a los clusters ya no cambian o se llega al límite de iteraciones"
      ),
      list(
        texto = "¿Para qué tipo de formas de clusters funciona mejor K-means?",
        opciones = c("a) Clusters con formas alargadas o de luna contorneada", "b) Clusters compactos y de forma aproximadamente esférica", "c) Estructuras de datos puramente jerárquicas anidadas", "d) Agrupaciones basadas estrictamente en densidades locales lineales"),
        correcta = "b) Clusters compactos y de forma aproximadamente esférica"
      ),
      list(
        texto = "¿Qué mide la 'Inercia' (Within-cluster sum-of-squares) devuelta por K-means?",
        opciones = c("a) La suma de las distancias al cuadrado de cada punto al centroide de su cluster asignado", "b) La distancia de separación que existe entre los centroides de los clusters", "c) El número total de observaciones mal clasificadas en el dataset", "d) La correlación global promedio entre todas las variables del estudio"),
        correcta = "a) La suma de las distancias al cuadrado de cada punto al centroide de su cluster asignado"
      )
    )
    
    preguntas_usuario <- reactiveVal(list())
    
    observeEvent(input$add, {
      req(input$nueva_pregunta, input$op1, input$op2, input$op3, input$op4)
      nueva <- list(
        texto = input$nueva_pregunta,
        opciones = c(input$op1, input$op2, input$op3, input$op4),
        correcta = c(input$op1, input$op2, input$op3, input$op4)[match(input$correcta, c("Opción 1", "Opción 2", "Opción 3", "Opción 4"))]
      )
      preguntas_usuario(c(preguntas_usuario(), list(nueva)))
    })
    
    todas_preguntas <- reactive({
      c(preguntas_base, preguntas_usuario())
    })
    
    preguntas_ordenadas <- reactiveVal(NULL)
    
    # Selección inicial aleatoria de exactamente 10 preguntas (Aislada contra bucles)
    observe({
      lista_actual <- todas_preguntas()
      lista_enriquecida <- lapply(seq_along(lista_actual), function(idx) {
        p <- lista_actual[[idx]]
        # Prefijo km_q_ específico para la sección de K-means
        p$id_unico <- paste0("km_q_", gsub("[^a-zA-Z0-9]", "", p$texto))
        p
      })
      
      if (is.null(isolate(preguntas_ordenadas()))) {
        n_mostrar <- min(10, length(lista_enriquecida))
        muestra_inicial <- sample(lista_enriquecida, n_mostrar)
        preguntas_ordenadas(muestra_inicial)
      }
    })
    
    # Reordenación controlada de 10 preguntas con mezcla interna de alternativas
    observeEvent(input$shuffle, {
      lista_actual <- todas_preguntas()
      lista_enriquecida <- lapply(seq_along(lista_actual), function(idx) {
        p <- lista_actual[[idx]]
        p$id_unico <- paste0("km_q_", gsub("[^a-zA-Z0-9]", "", p$texto))
        p
      })
      
      n_mostrar <- min(10, length(lista_enriquecida))
      nuevas <- sample(lista_enriquecida, n_mostrar)
      
      nuevas <- lapply(nuevas, function(p) {
        p$opciones <- sample(p$opciones)
        p
      })
      preguntas_ordenadas(nuevas)
    })
    
    # RENDER ÚNICO: Tarjetas y feedback por ID inmutable (Sin alertas de no respondido)
    output$preguntas <- renderUI({
      req(preguntas_ordenadas())
      
      tagList(
        lapply(seq_along(preguntas_ordenadas()), function(i) {
          pregunta_actual <- preguntas_ordenadas()[[i]]
          id_input <- pregunta_actual$id_unico
          
          feedback_ui <- NULL
          if (input$ver) {
            user_ans <- input[[id_input]]
            correct_ans <- pregunta_actual$correcta
            
            if (!is.null(user_ans) && user_ans == correct_ans) {
              feedback_ui <- div(class = "text-success mt-2 font-weight-bold", "✔️ ¡Correcto!")
            } else {
              feedback_ui <- div(class = "text-danger mt-2", paste0("❌ Incorrecto. Respuesta correcta: ", correct_ans))
            }
          }
          
          card(
            class = "mb-3 shadow-sm",
            card_header(tags$strong(paste0("Pregunta ", i))),
            card_body(
              radioButtons(
                session$ns(id_input),
                pregunta_actual$texto,
                choices = pregunta_actual$opciones,
                selected = input[[id_input]], 
                width = "100%"
              ),
              feedback_ui
            )
          )
        })
      )
    })
    
    # Caja de puntuación global dinámica basada en las 10 activas
    output$score <- renderUI({
      req(input$ver)
      total <- length(preguntas_ordenadas())
      
      aciertos <- sum(sapply(preguntas_ordenadas(), function(p) {
        res <- input[[p$id_unico]]
        !is.null(res) && res == p$correcta
      }), na.rm = TRUE)
      
      porcentaje <- (aciertos / total) * 100
      clase_color <- if(porcentaje >= 70) "alert-success" else "alert-warning"
      
      div(
        class = paste("alert mb-0 py-2 px-4", clase_color),
        tags$strong(paste0("Puntuación: ", aciertos, " / ", total, " (", round(porcentaje), "%)"))
      )
    })
  })
}

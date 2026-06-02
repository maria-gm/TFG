# =====================================================
#   CLÚSTERES JERÁRQUICOS - MÓDULO 
# =====================================================

# -------------------------------
# TEORÍA
# -------------------------------
# -------------------------------
# TEORÍA - CLUSTERING JERÁRQUICO
# -------------------------------

Jerarquicos_Teoria_UI <- function(id) {
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
        "Clustering Jerárquico",
        style = "font-weight: 800; color: #1a365d; margin-bottom: 5px;"
      ),
      
      p(
        "Algoritmo de agrupamiento basado en niveles jerárquicos representados gráficamente mediante un dendrograma.",
        style = "color: #64748b; font-size: 1.1rem; margin-bottom: 30px;"
      ),
      
      # =====================================
      # TARJETAS PRINCIPALES
      # =====================================
      bslib::layout_column_wrap(
        width = 1/3, # Tres columnas niveladas
        heights_equal = "row",
        
        # ---------------------------------
        # 1. ¿QUÉ ES?
        # ---------------------------------
        bslib::card(
          bslib::card_header(
            tags$b("1. Concepto y Enfoques"),
            style = "background: #e0e7ff;"
          ),
          bslib::card_body(
            p("Algoritmo que organiza los datos en grupos jerárquicos divididos por niveles, presentando dos enfoques según su construcción:"),
            tags$ul(
              style = "margin-top: 10px;",
              tags$li(tags$b("Clústeres Aglomerativos:"), " Comienza con tantos grupos como observaciones. Fusiona sucesivamente los grupos más cercanos y actualiza las distancias hasta unificar todo en un único clúster.", style = "margin-bottom: 8px;"),
              tags$li(tags$b("Clústeres Divisivos:"), " Proceso inverso; inicia con un único grupo global que contiene a todos los individuos y se divide progresivamente.")
            )
          )
        ),
        
        # ---------------------------------
        # 2. MATRIZ DE DISTANCIAS
        # ---------------------------------
        bslib::card(
          bslib::card_header(
            tags$b("2. Matriz de Distancias"),
            style = "background: #dcfce7;"
          ),
          bslib::card_body(
            p("El algoritmo se apoya en disimilitudes midiendo la proximidad de las observaciones mediante una matriz simétrica de dimensión \\(N \\times N\\) (donde \\(N\\) representa el número total de individuos), calculada habitualmente con la distancia euclídea."),
            p("Para recalcular las distancias de forma eficiente entre los clústeres fusionados \\(P\\) y \\(Q\\) frente a un tercer grupo \\(R\\), se utiliza la fórmula de recurrencia de ", tags$b("Lance y Williams (Soto et al., 2006):")),
            p("$$d^2(R, P+Q) = \\delta_1 d^2(R,P) + \\delta_2 d^2(R,Q) + \\delta_3 d^2(P,Q) + \\delta_4 |d^2(R,P) - d^2(R,Q)|$$")
          )
        ),
        
        # ---------------------------------
        # 3. RESULTADO Y VALIDACIÓN
        # ---------------------------------
        bslib::card(
          bslib::card_header(
            tags$b("3. Jerarquía y Dendrograma"),
            style = "background: #fef3c7;"
          ),
          bslib::card_body(
            p("El proceso se resume en un dendrograma que permite cortar la jerarquía al nivel deseado tras seleccionar el número óptimo de grupos."),
            tags$ul(
              style = "margin-top: 10px;",
              tags$li(tags$b("Validación:"), " Se emplean métricas como el coeficiente de silueta o la regla del codo para justificar los cortes.", style = "margin-bottom: 8px;"),
              tags$li(tags$b("Complemento espacial:"), " Ante bases de datos saturadas, se recomienda proyectar las observaciones en un mapa bidimensional de Componentes Principales (PCA) combinando rigurosidad jerárquica con sensibilidad espacial.")
            )
          )
        )
      ), # Fin del layout_column_wrap
      
      br(),
      
      # =====================================
      # TABLA DE COEFICIENTES LANCE-WILLIAMS
      # =====================================
      bslib::card(
        style = "border: 1px solid #cbd5e1; background: #f8fafc;",
        bslib::card_body(
          h4(icon("table"), "Coeficientes de Lance y Williams por método", style = "color: #1e40af; margin-bottom: 12px;"),
          p("Los coeficientes ponderadores \\(\\delta_j\\) determinan las propiedades y la evolución métrica de cada enlace (Villardón, 2007):"),
          
          tags$table(
            class = "table table-sm table-bordered",
            style = "background: white; font-size: 0.9rem; text-align: center; margin-top: 15px;",
            tags$thead(
              style = "background: #f1f5f9; font-weight: bold;",
              tags$tr(
                tags$th("Método de Enlace"),
                tags$th("\\(\\delta_1\\)"),
                tags$th("\\(\\delta_2\\)"),
                tags$th("\\(\\delta_3\\)"),
                tags$th("\\(\\delta_4\\)")
              )
            ),
            tags$tbody(
              tags$tr(tags$td(tags$b("Vecino más próximo")), tags$td("\\(1/2\\)"), tags$td("\\(1/2\\)"), tags$td("\\(0\\)"), tags$td("\\(-1/2\\)")),
              tags$tr(tags$td(tags$b("Vecino más lejano")), tags$td("\\(1/2\\)"), tags$td("\\(1/2\\)"), tags$td("\\(0\\)"), tags$td("\\(1/2\\)")),
              tags$tr(tags$td(tags$b("Media")), tags$td("\\(\\frac{n_P}{n_P+n_Q}\\)"), tags$td("\\(\\frac{n_Q}{n_P+n_Q}\\)"), tags$td("\\(0\\)"), tags$td("\\(0\\)")),
              tags$tr(tags$td(tags$b("Centroide")), tags$td("\\(\\frac{n_P}{n_P+n_Q}\\)"), tags$td("\\(\\frac{n_Q}{n_P+n_Q}\\)"), tags$td("\\(-\\frac{n_P n_Q}{(n_P+n_Q)^2}\\)"), tags$td("\\(0\\)")),
              tags$tr(tags$td(tags$b("Mediana")), tags$td("\\(1/2\\)"), tags$td("\\(1/2\\)"), tags$td("\\(-1/4\\)"), tags$td("\\(0\\)")),
              tags$tr(tags$td(tags$b("Ward")), tags$td("\\(\\frac{n_R+n_P}{n_R+n_P+n_Q}\\)"), tags$td("\\(\\frac{n_R+n_Q}{n_R+n_P+n_Q}\\)"), tags$td("\\(-\\frac{n_R}{n_R+n_P+n_Q}\\)"), tags$td("\\(0\\)")),
              tags$tr(tags$td(tags$b("Flexible")), tags$td("\\(\\frac{1-\\beta}{2}\\)"), tags$td("\\(\\frac{1-\\beta}{2}\\)"), tags$td("\\(\\beta\\)"), tags$td("\\(0\\)"))
            )
          ),
          p(style = "font-size: 0.8rem; color: #64748b; margin-top: 5px;", "Donde \\(n_R, n_P, n_Q\\) son el número de individuos de cada grupo y \\(\\beta\\) es un parámetro arbitrario entre 0 y 1.")
        )
      ),
      
      br(),
      
      # =====================================
      # CRITERIOS DE ENLACE DETALLADOS
      # =====================================
      bslib::card(
        style = "border: 1px solid #cbd5e1; background: #f8fafc;",
        bslib::card_body(
          h4(icon("diagram-project"), "Criterios de Enlace (Soto et al., 2006)", style = "color: #1e40af; margin-bottom: 15px;"),
          
          tags$div(
            style = "display: flex; flex-direction: column; gap: 15px;",
            
            tags$div(
              style = "border-left: 4px solid #3b82f6; padding-left: 12px;",
              tags$b("Método del vecino más próximo: "),
              "Utiliza la distancia mínima entre observaciones pares de cada clúster. Es sensible para la detección de outliers, pero propende a formar grupos muy grandes por encadenamiento."
            ),
            tags$div(
              style = "border-left: 4px solid #ef4444; padding-left: 12px;",
              tags$b("Método del vecino más lejano: "),
              "Considera el máximo de las distancias entre las observaciones de los grupos. También ayuda a identificar outliers, generando soluciones con grupos pequeños y sumamente compactos."
            ),
            tags$div(
              style = "border-left: 4px solid #10b981; padding-left: 12px;",
              tags$b("Método de la media: "),
              "Calcula la distancia promedio entre todas las combinaciones de pares de observaciones. Genera grupos equilibrados (ni sobre-dimensionados ni muy pequeños) y ofrece buenos resúmenes gráficos."
            ),
            tags$div(
              style = "border-left: 4px solid #f59e0b; padding-left: 12px;",
              tags$b("Método del centroide: "),
              "Mide el distanciamiento entre vectores medios (centroides) en escala de intervalo. Si hay disparidad severa de tamaños, el nuevo centroide se absorberá casi por completo en el clúster de mayor volumen."
            ),
            tags$div(
              style = "border-left: 4px solid #8b5cf6; padding-left: 12px;",
              tags$b("Método de Ward: "),
              "Minimiza en cada paso la suma de cuadrados intragrupo basándose en la descomposición del ANOVA multivariante. Es altamente eficiente y construye conjuntos muy pequeños y homogéneos, aunque exhibe alta sensibilidad ante outliers."
            )
          )
        )
      )
    )
  )
}


Jerarquicos_Teoria_Server <- function(id){
  
  moduleServer(id, function(input, output, session){
    
  })
  
}



# -------------------------------
# Análisis
# -------------------------------
Jerarquicos_Analisis_UI <- function(id){
  ns <- NS(id)
  tagList(
    # Título personalizado idéntico a los módulos anteriores
    h3("Análisis", 
       style = "color: #1a446c; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; font-weight: 600; margin-top: 40px; margin-bottom: 20px; border-bottom: 2px solid #f4f6f9; padding-bottom: 10px;"),
    
    fluidRow(
      column(4,
             wellPanel(
               h4("Configuración"),
               p("Seleccione los parámetros para estructurar el árbol jerárquico y definir los clústeres."),
               hr(),
               
               # --- ESTO SE VE SIEMPRE ---
               selectInput(ns("metodo"), "Método de enlace (Lance-Williams)",
                           choices = c("Ward.D2" = "ward.D2", "Completo" = "complete", "Promedio" = "average", "Single" = "single"),
                           selected = "ward.D2"),
               numericInput(ns("k"), "Número de clústeres", value = 3, min = 2, max = 10),
               
               # --- ESTO SOLO SE VE EN PCA Y DENDROGRAMA ---
               conditionalPanel(
                 condition = sprintf("input['%s'] == '2. Proyección PCA' || input['%s'] == '3. Árbol (Dendrograma)'", ns("tabs"), ns("tabs")),
                 uiOutput(ns("ui_var_cat")),
                 checkboxInput(ns("mostrar_labels"), "Mostrar etiquetas (N < 50)", FALSE)
               ),
               
               hr(),
               downloadButton(ns("dl_data"), "Descargar Clústeres (CSV)", class = "btn-success", style = "width: 100%;")
             )
      ),
      column(8,
             tabsetPanel(id = ns("tabs"),
                         tabPanel("1. Datos", 
                                  br(),
                                  p("Información: El algoritmo calcula las distancias euclídeas a partir de las columnas numéricas normalizadas.", 
                                    style = "color: #7f8c8d; font-style: italic; margin-bottom: 25px;"),
                                  h4("Dataset original preparado", style = "color: #2c3e50; font-weight: 500;"),
                                  DT::DTOutput(ns("tabla_resumen")),
                                  br(), hr(), br(),
                                  h4("Dataset estandarizado (Z-scores)", style = "color: #2c3e50; font-weight: 500;"),
                                  DT::DTOutput(ns("tabla_scale"))),
                         
                         tabPanel("2. Proyección PCA", 
                                  br(),
                                  plotOutput(ns("pca_plot")),
                                  br(),
                                  h4("Interpretación", style = "color: #2c3e50; font-weight: 500;"),
                                  verbatimTextOutput(ns("interp_pca_cl"))),
                         
                         tabPanel("3. Árbol (Dendrograma)", 
                                  br(),
                                  plotOutput(ns("dendrograma")),
                                  br(),
                                  h4("Interpretación", style = "color: #2c3e50; font-weight: 500;"),
                                  verbatimTextOutput(ns("interp_dendro"))),
                         
                         tabPanel("4. Perfil de Variables", 
                                  br(),
                                  plotOutput(ns("boxplot_perfil")),
                                  br(),
                                  h4("Interpretación", style = "color: #2c3e50; font-weight: 500;"),
                                  verbatimTextOutput(ns("interp_perfil"))),
                         
                         tabPanel("5. Validación (Silueta)", 
                                  br(),
                                  plotOutput(ns("silhouette_map")),
                                  br(),
                                  plotOutput(ns("silhouette_dist")),
                                  br(),
                                  h4("Interpretación", style = "color: #2c3e50; font-weight: 500;"),
                                  verbatimTextOutput(ns("interp_silueta")))
             )
      )
    )
  )
}

# -------------------------------
# Server: Clustering Jerárquico
# -------------------------------
Jerarquicos_Analisis_Server <- function(id, datos, datos_ejemplo = NULL) {
  
  moduleServer(id, function(input, output, session) {
    
    # --- 1. DATOS BASE (LIMPIEZA TOTAL) ---
    datos_base <- reactive({
      df <- if(!is.null(datos()) && nrow(datos()) > 0) datos() else datos_ejemplo
      req(df)
      df[complete.cases(df), , drop = FALSE]
    })
    
    # --- 2. SELECCIÓN DE NUMÉRICOS ---
    datos_num <- reactive({
      df <- datos_base()
      df_n <- df[, sapply(df, is.numeric), drop = FALSE]
      cols_validas <- sapply(df_n, function(x) length(unique(x)) > 15)
      if(sum(cols_validas) < 2) return(df_n) 
      df_n[, cols_validas, drop = FALSE]
    })
    
    # --- 3. ESCALADO Y DISTANCIA ---
    datos_scaled <- reactive({ req(ncol(datos_num()) >= 2); scale(datos_num()) })
    
    matriz_distancia <- reactive({
      req(datos_scaled())
      d <- dist(datos_scaled(), method = "euclidean")
      if (input$metodo %in% c("centroid", "median")) d^2 else d
    })
    
    # --- 4. MODELO Y CLÚSTERES ---
    modelo_hc <- reactive({ req(matriz_distancia()); hclust(matriz_distancia(), method = input$metodo) })
    res_jerarquico <- reactive({ req(modelo_hc(), input$k); cutree(modelo_hc(), k = input$k) })
    
    # --- 5. PCA Y UNIÓN DE RESULTADOS ---
    pca_res_obj <- reactive({ req(datos_scaled()); prcomp(datos_scaled()) })
    
    df_final <- reactive({
      req(pca_res_obj(), res_jerarquico())
      df_viz <- as.data.frame(pca_res_obj()$x[, 1:2])
      df_viz$cluster <- factor(res_jerarquico())
      
      if(!is.null(input$var_cat) && input$var_cat != "Ninguna" && input$var_cat != "") {
        var_real <- datos_base()[[input$var_cat]]
        if(length(var_real) == nrow(df_viz)) {
          df_viz$realidad <- as.factor(var_real)
        }
      }
      df_viz
    })
    
    # --- 6. OUTPUT: TABLAS (ORIGINAL Y ESTANDARIZADA CON SCROLL INTERNO) ---
    # --- 6. OUTPUT: TABLAS (CORREGIDO: ORIGINAL COMPLETO VS NUMÉRICO PURO) ---
    output$tabla_resumen <- DT::renderDT({
      df_completo <- datos_base()
      req(df_completo)
      
      DT::datatable(
        # Muestra el dataset original entero con sus columnas categóricas de referencia
        df_completo, 
        options = list(paging = FALSE, scrollY = "400px", scrollX = TRUE, autoWidth = TRUE),
        class = 'cell-border stripe hover compact'
      ) %>%
        DT::formatRound(columns = names(datos_num()), digits = 3)
    })
    
    output$tabla_scale <- DT::renderDT({
      req(datos_scaled())
      
      DT::datatable(
        # Matriz pura de números tipificados (Z-scores) que ingresa al algoritmo
        round(as.data.frame(datos_scaled()), 3), 
        options = list(paging = FALSE, scrollY = "400px", scrollX = TRUE, autoWidth = TRUE),
        class = 'cell-border stripe hover compact'
      )
    })
    
    
    # --- 7. OUTPUT: PROYECCIÓN PCA (INTERPRETACIÓN FIJA EN WARD) ---
    output$pca_plot <- renderPlot({
      validate(need(df_final(), "Sincronizando dimensiones..."))
      df = df_final()
      p <- ggplot(df, aes(x = PC1, y = PC2))
      
      if(!is.null(input$var_cat) && input$var_cat != "Ninguna" && input$var_cat != "") {
        p <- p + geom_point(aes(color = realidad, shape = cluster), size = 3.5, alpha = 0.8) +
          labs(color = input$var_cat, shape = "Clúster")
      } else {
        p <- p + geom_point(aes(color = cluster), size = 3, alpha = 0.7) +
          labs(color = "Clúster") +
          ggsci::scale_color_jco()
      }
      
      p + stat_ellipse(aes(fill = cluster), geom = "polygon", alpha = 0.1, color = "black", linetype = 2, na.rm = TRUE) +
        ggsci::scale_fill_jco() +
        guides(fill = "none") + theme_minimal() +
        labs(title = "Comparación: Variable Real vs. Clustering")
    })
    
    output$interp_pca_cl <- renderText({
      req(input$k)
      paste0(
        "Nota analítica: Esta interpretación se fundamenta de manera estándar en el método de minimización de varianza interna de Ward.D2.\n",
        "La proyección sobre el plano principal del PCA permite constatar espacialmente la nitidez geométrica de los grupos.\n\n",
        "Ejemplo práctico (Dataset 'penguins'): Al contrastar el agrupamiento obtenido (k=3) frente a la variable categórica ",
        "'species', descubrirás que bajo el criterio de Ward las elipses delimitan de forma casi perfecta a los pingüinos Gentoo debido a sus marcadas ",
        "diferencias de peso y aleta, reflejando un solapamiento menor y natural entre las especies Adelie y Chinstrap."
      )
    })
    
    # --- 8. OUTPUT: DENDROGRAMA (COMENTARIOS DINÁMICOS POR MÉTODO DE ENLACE) ---
    output$dendrograma <- renderPlot({
      req(modelo_hc())
      alturas <- rev(modelo_hc()$height)
      h_corte <- (alturas[input$k] + alturas[input$k-1]) / 2
      
      factoextra::fviz_dend(modelo_hc(), k = input$k, cex = 0.5, lwd = 0.4, k_colors = "jco",
                            rect = TRUE, rect_fill = TRUE, 
                            show_labels = (input$mostrar_labels && nrow(datos_base()) <= 50)) +
        geom_hline(yintercept = h_corte, linetype = "dashed", color = "red") +
        theme_minimal() + theme(axis.text.x = element_blank())
    })
    
    output$interp_dendro <- renderText({
      req(input$k, input$metodo)
      
      msg_comun <- paste0(
        "El dendrograma representa el proceso de agrupación jerárquica. El corte en ", input$k, " grupos se indica mediante la línea discontinua roja.\n\n",
        "Guía según el método de enlace activo ('", input$metodo, "') aplicado a 'penguins':\n"
      )
      
      msg_especifico <- switch(input$metodo,
                               "ward.D2" = paste0(
                                 "- Método de Ward: Minimiza la varianza dentro de los clústeres. Genera un árbol con un brazo muy ",
                                 "limpio y elevado correspondiente a la especie Gentoo (muy pesados), y otro brazo equilibrado que ",
                                 "subdivide con precisión los grupos Adelie y Chinstrap buscando estructuras esféricas compactas."
                               ),
                               "complete" = paste0(
                                 "- Enlace Completo (Máxima Distancia): Agrupa basándose en los miembros más alejados. Tiende a crear ",
                                 "clústeres muy compactos y de diámetros similares. En 'penguins', esto fuerza agrupaciones estrictas ",
                                 "evitando que casos límite o pingüinos de tamaños ambiguos desestabilicen los tres grandes bloques biológicos."
                               ),
                               "average" = paste0(
                                 "- Enlace Promedio (UPGMA): Evalúa la distancia media entre todos los pares de objetos. Es menos sensible a ",
                                 "valores atípicos que el método completo. En 'penguins', dibuja ramas con saltos de altura más suaves y ",
                                 "proporcionales, reflejando transiciones morfológicas continuas entre los diferentes ejemplares."
                               ),
                               "single" = paste0(
                                 "- Enlace Simple (Mínima Distancia): Conecta grupos a través de sus vecinos más cercanos. Sufre del fenómeno ",
                                 "de encadenamiento ('chaining'). Verás que en 'penguins' el dendrograma toma forma de escalera con ramas ",
                                 "largas y desequilibradas, donde los individuos se van uniendo uno a uno a un solo gran clúster central."
                               )
      )
      
      return(paste0(msg_comun, msg_especifico))
    })
    
    # --- 9. OUTPUT: PERFIL DE VARIABLES (INTERPRETACIÓN FIJA EN WARD) ---
    output$boxplot_perfil <- renderPlot({
      req(res_jerarquico(), datos_num())
      df_long <- as.data.frame(datos_num()) %>%
        mutate(cluster = as.factor(res_jerarquico())) %>%
        tidyr::pivot_longer(cols = -cluster, names_to = "Variable", values_to = "Valor")
      
      ggplot(df_long, aes(x = cluster, y = Valor, fill = cluster)) +
        geom_boxplot(alpha = 0.6, outlier.size = 0.5) +
        facet_wrap(~Variable, scales = "free_y", ncol = 3) +
        theme_minimal() +
        ggsci::scale_fill_jco() +
        labs(title = "Distribución de variables por clúster")
    })
    
    output$interp_perfil <- renderText({
      req(input$k)
      paste0(
        "Nota analítica: Esta caracterización de perfiles describe la morfología típica de los grupos generados bajo el método Ward.D2.\n",
        "Los gráficos de cajas identifican los rasgos diferenciales que definen la naturaleza de cada grupo.\n\n",
        "Ejemplo práctico (Dataset 'penguins'): El clúster que captura a los pingüinos Gentoo destaca exponencialmente ",
        "por exhibir cajas desplazadas hacia valores muy elevados en las variables 'body_mass_g' (masa corporal) y ",
        "'flipper_length_mm' (largo de aleta), mientras que los otros dos grupos se fragmentan según las proporciones del pico."
      )
    })
    
    # --- 10. OUTPUT: VALIDACIÓN SILUETA (INTERPRETACIÓN FIJA EN WARD) ---
    output$silhouette_dist <- renderPlot({
      req(res_jerarquico(), matriz_distancia())
      sil <- cluster::silhouette(res_jerarquico(), matriz_distancia())
      df_sil <- as.data.frame(sil[, 1:3])
      df_sil$cluster <- as.factor(df_sil$cluster)
      
      ggplot(df_sil, aes(x = sil_width, y = cluster, fill = cluster)) +
        geom_violin(alpha = 0.4) + geom_boxplot(width = 0.1, color = "black") +
        geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
        theme_minimal() + 
        ggsci::scale_fill_jco() +
        labs(title = "Consistencia Interna (Silueta)")
    })
    
    output$silhouette_map <- renderPlot({
      req(res_jerarquico(), matriz_distancia(), pca_res_obj())
      sil <- cluster::silhouette(res_jerarquico(), matriz_distancia())
      df_viz <- as.data.frame(pca_res_obj()$x[, 1:2])
      df_viz$cluster <- factor(res_jerarquico())
      df_viz$sil_width <- sil[, "sil_width"]
      
      ggplot(df_viz, aes(x = PC1, y = PC2, color = cluster)) +
        geom_point(aes(alpha = sil_width, size = sil_width)) +
        scale_alpha_continuous(range = c(0.1, 1)) +
        scale_size_continuous(range = c(1, 4)) +
        theme_minimal() + 
        ggsci::scale_color_jco() +
        labs(title = "Mapa de Confianza")
    })
    
    output$interp_silueta <- renderText({
      req(input$k)
      paste0(
        "Nota analítica: La validación de la silueta cuantifica la consistencia del agrupamiento obtenido bajo el criterio de Ward.D2.\n",
        "Valores cercanos a 1 demuestran que los objetos están asignados al clúster correcto.\n\n",
        "Ejemplo práctico (Dataset 'penguins'): Notarás que el clúster esférico asignado a los Gentoo mantiene valores de silueta muy robustos y elevados. ","En contraposición, las observaciones ubicadas en la frontera difusa entre las especies Adelie y Chinstrap presentarán ","coeficientes moderados o negativos, evidenciando las zonas de solapamiento morfológico natural.")})
    # --- EXTRAS ---
    output$ui_var_cat <- renderUI({
      df <- datos_base()
    nom_cats <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x) || (is.numeric(x) && length(unique(x)) <= 5))]
    selectInput(session$ns("var_cat"), "Variable Real (Color):",choices = c("Ninguna", nom_cats), selected = "Ninguna")})
    output$dl_data <- downloadHandler(filename = function() {paste("Resultados_Clustering_Jerarquico.csv", sep = "")},content = function(file) {req(datos_base(), res_jerarquico())
      df_export <- datos_base()
      df_export$Cluster_Asignado <- res_jerarquico()
      write.csv(df_export, file, row.names = TRUE)
      })
    }) 
  } 
# -------------------------------
# Autoevaluación
# -------------------------------

Jerarquicos_Auto_UI <- function(id) {
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

Jerarquicos_Auto_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    preguntas_base <- list(
      list(
        texto = "¿Qué es un dendrograma?",
        opciones = c("Una representación gráfica en forma de árbol que ilustra el proceso de fusiones o divisiones de los clusters", "Una matriz que contiene los vectores de medias o centroides del mapa", "Un gráfico que muestra únicamente las cargas factoriales rotadas", "La línea de regresión que minimiza el error cuadrático medio"),
        correcta = "Una representation gráfica en forma de árbol que ilustra el proceso de fusiones o divisiones de los clusters"
      ),
      list(
        texto = "¿Cuál es el objetivo principal de los algoritmos de agrupamiento jerárquicos?",
        opciones = c("Construir una jerarquía o estructura de clusters anidados sin necesidad de fijar el número K a priori", "Calcular la combinación lineal de máxima varianza explicada", "Clasificar nuevas observaciones mediante fronteras de densidad locales", "Encontrar una matriz identidad que anule la multicolinealidad"),
        correcta = "Construir una jerarquía o estructura de clusters anidados sin necesidad de fijar el número K a priori"
      ),
      list(
        texto = "¿Qué distancia se utiliza comúnmente para medir la separación geométrica entre dos puntos individuales?",
        opciones = c("Distancia Euclídea", "Distancia Cophenética del árbol", "Distancia de enlace completo (Complete Linkage)", "Distancia de Mahalanobis corregida"),
        correcta = "Distancia Euclídea"
      ),
      list(
        texto = "¿En qué se diferencia el enfoque aglomerativo (bottom-up) del divisivo (top-down)?",
        opciones = c("El aglomerativo empieza con cada punto como un cluster individual y los une; el divisivo empieza con un único cluster global y lo divide", "El aglomerativo requiere centroides fijos y el divisivo utiliza radio Épsilon de densidad", "No hay diferencia real, ambos enfoques calculan exactamente el mismo dendrograma final", "Uno es un método supervisado y el otro no supervisado bayesiano"),
        correcta = "El aglomerativo empieza con cada punto como un cluster individual y los une; el divisivo empieza con un único cluster global y lo divide"
      ),
      list(
        texto = "¿Cómo define el método de enlace simple (Single Linkage) la distancia entre dos clusters?",
        opciones = c("Como la distancia mínima entre un punto de un cluster y un punto del otro cluster", "Como la distancia máxima entre los puntos más alejados de ambos clusters", "Como la distancia promedio entre todos los pares de puntos posibles", "Como la separación entre los vectores de medias o centroides de cada grupo"),
        correcta = "Como la distancia mínima entre un punto de un cluster y un punto del otro cluster"
      ),
      list(
        texto = "¿Qué caracteriza al método de enlace completo (Complete Linkage)?",
        opciones = c("Mide la distancia entre los dos puntos más alejados (distancia máxima) de ambos clusters", "Calcula la inercia interna aplicando la regla de Kaiser factorial", "Utiliza la distancia mínima entre vecinos para ignorar por completo el ruido", "Une los clusters basándose únicamente en el número total de observaciones"),
        correcta = "Mide la distancia entre los dos puntos más alejados (distancia máxima) de ambos clusters"
      ),
      list(
        texto = "¿Cuál es la propiedad fundamental del método de Ward?",
        opciones = c("Minimiza el incremento de la varianza total interna (suma de cuadrados dentro de los clusters) en cada fusión", "Une clusters basándose estrictamente en la densidad local de radio Épsilon", "Ignora los valores atípicos de la matriz original de forma automática", "Genera agrupaciones de formas alargadas y concéntricas"),
        correcta = "Minimiza el incremento de la varianza total interna (suma de cuadrados dentro de los clusters) en cada fusión"
      ),
      list(
        texto = "¿Qué indica la altura de las uniones verticales en un dendrograma?",
        opciones = c("La distancia o grado de disimilitud a la que se fusionan los clusters", "El número exacto de variables analizadas en el estudio multivariante", "El porcentaje de varianza capturado por la componente principal", "La cantidad de observaciones que componen el dataset usuario"),
        correcta = "La distancia o grado de disimilitud a la que se fusionan los clusters"
      ),
      list(
        texto = "¿Cómo decide el analista el número final de clusters al observar un dendrograma?",
        opciones = c("Realizando un corte horizontal en el nivel de altura donde las líneas verticales sean más largas", "Buscando el punto de inflexión exacto mediante el p-valor de Bartlett", "Seleccionando de forma obligatoria los autovalores superiores a uno", "El algoritmo corta automáticamente el árbol al llegar al 50% de inercia"),
        correcta = "Realizando un corte horizontal en el nivel de altura donde las líneas verticales sean más largas"
      ),
      list(
        texto = "¿Qué es la distancia cophenética en el clustering jerárquico?",
        opciones = c("La altura del dendrograma en la que dos observaciones se unen por primera vez en un mismo cluster", "La distancia euclídea original medida en la matriz de datos escalada", "La inercia remanente tras aplicar una rotación ortogonal Varimax", "El número de ramas en las que se divide el nodo raíz principal"),
        correcta = "La altura del dendrograma en la que dos observaciones se unen por primera vez en un mismo cluster"
      ),
      list(
        texto = "¿Qué mide el coeficiente de correlación cophenética?",
        opciones = c("El grado de fidelidad con el que el dendrograma preserva las distancias originales entre las observaciones", "La correlación lineal entre las dos primeras componentes de un PCA", "La adecuación de la muestra mediante el criterio KMO", "La probabilidad de que un punto sea clasificado como ruido espacial"),
        correcta = "El grado de fidelidad con el que el dendrograma preserva las distancias originales entre las observaciones"
      ),
      list(
        texto = "¿Por qué puede ser una desventaja el clustering jerárquico aglomerativo en conjuntos de datos muy grandes?",
        opciones = c("Porque su coste computacional en memoria y tiempo es elevado debido al cálculo de la matriz de distancias", "Porque requiere recalcular los centroides aleatorios en cada iteración", "Porque es un método supervisado que exige etiquetas previas", "Porque obliga a eliminar variables correlacionadas de forma drástica"),
        correcta = "Porque su coste computacional en memoria y tiempo es elevado debido al cálculo de la matriz de distancias"
      ),
      list(
        texto = "¿Qué efecto visual suele provocar el método de enlace simple (Single Linkage) en el dendrograma?",
        opciones = c("Efecto de encadenamiento (chaining), donde los puntos se unen uno a uno formando clusters alargados", "Clusters perfectamente esféricos del mismo tamaño y varianza", "Un corte simétrico ideal basado en la regla de Kaiser", "La eliminación por completo de las observaciones de la frontera"),
        correcta = "Efecto de encadenamiento (chaining), donde los puntos se unen uno a uno formando clusters alargados"
      ),
      list(
        texto = "Una vez que dos observaciones se han fusionado en un cluster en el método jerárquico aglomerativo, ¿se pueden separar en pasos posteriores?",
        opciones = c("No, las decisiones de fusión son definitivas e irreversibles a lo largo del algoritmo", "Sí, si la distancia euclídea del centroide se altera en la siguiente iteración", "Sí, el método de Ward permite reasignar puntos de forma dinámica", "Depende del número de iteraciones máximas configuradas en la UI"),
        correcta = "No, las decisiones de fusión son definitivas e irreversibles a lo largo del algoritmo"
      ),
      list(
        texto = "¿Cuál es el método de enlace que calcula la distancia entre clusters utilizando el promedio de todas las distancias entre sus componentes?",
        opciones = c("Enlace promedio (Average Linkage o UPGMA)", "Enlace completo (Complete Linkage)", "Método de particionamiento de K-means", "Enlace por densidad de radio Épsilon"),
        correcta = "Enlace promedio (Average Linkage o UPGMA)"
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
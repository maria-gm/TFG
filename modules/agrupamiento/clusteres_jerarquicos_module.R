# =====================================================
#   CLÚSTERES JERÁRQUICOS - MÓDULO 
# =====================================================

# -------------------------------
# TEORÍA
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
Jerarquicos_Auto_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # ─── SOLUCIÓN REFORZADA PARA EL ANCHO DE LOS RADIO BUTTONS ───
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
Jerarquicos_Analisis_Server <- function(id, datos, datos_ejemplo = NULL) {
  
  moduleServer(id, function(input, output, session) {
    
    # --- 1. DATOS BASE ---
    datos_base <- reactive({
      df <- if(!is.null(datos()) && nrow(datos()) > 0) datos() else datos_ejemplo
      req(df)
      df <- df[complete.cases(df), , drop = FALSE]
      req(nrow(df) > 0)
      rownames(df) <- 1:nrow(df)
      df
    })
    
    # --- 2. SELECCIÓN DE NUMÉRICOS ---
    datos_num <- reactive({
      df <- datos_base()
      df_n <- df[, sapply(df, is.numeric), drop = FALSE]
      cols_validas <- sapply(df_n, function(x) length(unique(x)) > 15)
      if(sum(cols_validas) < 2) {
        req(ncol(df_n) >= 2)
        return(df_n)
      } 
      df_n[, cols_validas, drop = FALSE]
    })
    
    # --- 3. ESCALADO Y DISTANCIA ---
    datos_scaled <- reactive({ 
      df_n <- datos_num()
      req(ncol(df_n) >= 2)
      sd_cols <- sapply(df_n, sd, na.rm = TRUE)
      req(all(sd_cols > 0)) 
      scale(df_n) 
    })
    
    matriz_distancia <- reactive({
      req(datos_scaled(), input$metodo)
      d <- dist(datos_scaled(), method = "euclidean")
      if (input$metodo %in% c("centroid", "median")) d^2 else d
    })
    
    # --- 4. MODELO Y CLÚSTERES ---
    modelo_hc <- reactive({ 
      req(matriz_distancia(), input$metodo)
      hclust(matriz_distancia(), method = input$metodo) 
    })
    
    res_jerarquico <- reactive({ 
      req(modelo_hc(), input$k)
      cutree(modelo_hc(), k = input$k) 
    })
    
    # --- 5. PCA Y UNIÓN DE RESULTADOS ---
    pca_res_obj <- reactive({ 
      req(datos_scaled()) 
      prcomp(datos_scaled()) 
    })
    
    df_final <- reactive({
      req(pca_res_obj(), res_jerarquico(), datos_base())
      n_pca <- nrow(pca_res_obj()$x)
      n_clusa <- length(res_jerarquico())
      n_base <- nrow(datos_base())
      req(n_pca == n_clusa, n_clusa == n_base)
      
      df_viz <- as.data.frame(pca_res_obj()$x[, 1:2])
      df_viz$cluster <- factor(res_jerarquico())
      
      if(!is.null(input$var_cat) && input$var_cat != "Ninguna" && input$var_cat != "") {
        var_real <- datos_base()[[input$var_cat]]
        if(!is.null(var_real) && length(var_real) == nrow(df_viz)) {
          df_viz$realidad <- as.factor(var_real)
        }
      }
      df_viz
    })
    
    # --- 6. OUTPUT: TABLAS ---
    output$tabla_resumen <- DT::renderDT({
      df_completo <- datos_base()
      req(df_completo)
      DT::datatable(
        df_completo, 
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
    
    # --- 7. OUTPUT: PROYECCIÓN PCA ---
    output$pca_plot <- renderPlot({
      req(df_final())
      df <- df_final()
      p <- ggplot(df, aes(x = PC1, y = PC2))
      
      if(!is.null(input$var_cat) && input$var_cat != "Ninguna" && input$var_cat != "" && !is.null(df$realidad)) {
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
        "Nota analítica: Esta interpretación se fundamenta en la proyección sobre el plano principal del PCA.\n",
        "Permite constatar de manera geométrica qué tan nítidas y separadas son las fronteras de los grupos.\n\n",
        "Caso 'penguins': Al seleccionar k=3 y contrastar con la variable real 'species', comprobará que las elipses ",
        "asignan con altísima precisión al grupo de los pingüinos Gentoo (caracterizados por mayor tamaño). Las especies ",
        "Adelie y Chinstrap compartirán un espacio más cercano en el plano de los componentes debido a similitudes morfológicas generalizadas."
      )
    })
    
    # --- 8. OUTPUT: DENDROGRAMA ---
    # --- 8. OUTPUT: DENDROGRAMA ---
    output$dendrograma <- renderPlot({
      req(modelo_hc(), input$k)
      dend <- as.dendrogram(modelo_hc())
      alturas <- rev(modelo_hc()$height)
      req(length(alturas) >= input$k)
      h_corte <- (alturas[input$k] + alturas[max(1, input$k-1)]) / 2
      
      dend_data <- ggdendro::dendro_data(dend, type = "rectangle")
      
      p <- ggplot(ggdendro::segment(dend_data)) + 
        geom_segment(aes(x = x, y = y, xend = xend, yend = yend), 
                     color = "#2c3e50", 
                     linewidth = 0.2) + 
        geom_hline(yintercept = h_corte, linetype = "dashed", color = "red", linewidth = 0.4) +
        theme_minimal() +
        labs(title = "Dendrograma de Agrupamiento Jerárquico", x = "", y = "Altura (Distancia)") +
        theme(
          axis.text.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()
        )
      
      if (input$mostrar_labels && nrow(datos_base()) <= 50) {
        p <- p + scale_x_continuous(breaks = dend_data$labels$x, labels = dend_data$labels$label) +
          theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))
      }
      p
    })
    
    output$interp_dendro <- renderText({
      req(input$k, input$metodo)
      
      # Selección dinámica de la frase resumen según el método activo de Lance-Williams
      frase_metodo <- switch(input$metodo,
                             "single"   = "El enlace Simple busca vecinos mínimos, provocando un efecto de encadenamiento continuo que tiende a aislar valores atípicos individuales en lugar de agrupar las especies de pingüinos de forma homogénea.",
                             "ward.D2"  = "El método de Ward maximiza la homogeneidad interna, estructurando el árbol en bifurcaciones altamente simétricas y equilibradas que segmentan con máxima precisión las diferencias físicas reales entre las especies.",
                             "complete" = "El enlace Completo prioriza la distancia máxima permitiendo identificar subgrupos muy compactos, aunque su estructura es altamente sensible a pingüinos con dimensiones anatómicas extremas o singulares.",
                             "average"  = "El enlace Promedio suaviza la varianza calculando distancias medias entre grupos, ofreciendo una escala de altura más chata y un punto intermedio que atenúa el encadenamiento sin llegar a la simetría perfecta de Ward.",
                             "Método de enlace no especificado."
      )
      
      # Estructura del diagnóstico textual completo enviado a la UI
      paste0(
        "Diagnóstico del Árbol (Algoritmo Jerárquico):\n",
        "El gráfico superior muestra cómo se fusionan las observaciones de abajo hacia arriba. ",
        "Usted seleccionó el método de enlace '", input$metodo, "' con una división en k = ", input$k, " grupos.\n\n",
        "Interpretación del método:\n", frase_metodo
      )
    })
    
    # --- 9. OUTPUT: PERFIL DE VARIABLES ---
    output$boxplot_perfil <- renderPlot({
      req(res_jerarquico(), datos_num())
      req(length(res_jerarquico()) == nrow(datos_num()))
      
      df_long <- as.data.frame(datos_num()) %>%
        dplyr::mutate(cluster = as.factor(res_jerarquico())) %>%
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
      # INTERPRETACIÓN DE LOS BOXPLOTS
      paste0(
        "Análisis de Variables por Grupo (¿Quién es quién?):\n",
        "Los gráficos de caja permiten mapear la identidad física y características biológicas de cada clúster asignado.\n\n",
        "Guía de lectura para 'penguins':\n",
        "- Clúster con valores máximos en 'body_mass_g' (masa corporal) y 'flipper_length_mm' (longitud de aleta): Representa inequívocamente a los individuos de la especie Gentoo.\n",
        "- Clúster con 'bill_length_mm' (longitud del pico) elevado pero 'bill_depth_mm' (grosor del pico) moderado: Suele asociarse fuertemente con la especie Chinstrap.\n",
        "- Clúster con picos notablemente más cortos y gruesos ('bill_depth_mm' alto): Corresponde al patrón anatómico estándar de la especie Adelie."
      )
    })
    
    # --- 10. OUTPUT: VALIDACIÓN SILUETA ---
    output$silhouette_dist <- renderPlot({
      req(res_jerarquico(), matriz_distancia())
      sil <- cluster::silhouette(res_jerarquico(), matriz_distancia())
      df_sil <- as.data.frame(sil[, 1:3])
      df_sil$cluster <- as.factor(df_sil$cluster)
      
      ggplot(df_sil, aes(x = sil_width, y = cluster, fill = cluster)) +
        geom_violin(alpha = 0.4) + geom_boxplot(width = 0.1, color = "black") +
        geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
        theme_minimal() + ggsci::scale_fill_jco() + labs(title = "Consistencia Interna (Silueta)")
    })
    
    output$silhouette_map <- renderPlot({
      req(res_jerarquico(), matriz_distancia(), pca_res_obj())
      sil <- cluster::silhouette(res_jerarquico(), matriz_distancia())
      df_viz <- as.data.frame(pca_res_obj()$x[, 1:2])
      req(nrow(df_viz) == length(res_jerarquico()))
      
      df_viz$cluster <- factor(res_jerarquico())
      df_viz$sil_width <- sil[, "sil_width"]
      
      ggplot(df_viz, aes(x = PC1, y = PC2, color = cluster)) +
        geom_point(aes(alpha = sil_width, size = sil_width)) +
        scale_alpha_continuous(range = c(0.1, 1)) + scale_size_continuous(range = c(1, 4)) +
        theme_minimal() + ggsci::scale_color_jco() + labs(title = "Mapa de Confianza")
    })
    
    output$interp_silueta <- renderText({ 
      req(input$k)
      paste0(
        "Métricas de Validación de Silueta:\n",
        "La silueta mide qué tan bien integrado está un objeto a su clúster asignado en comparación con los demás (rango de -1 a 1).\n\n",
        "Valores cercanos a 1 implican una asignación perfecta. Valores por debajo de 0 alertan de elementos artificialmente forzados ",
        "dentro de un grupo erróneo.\n\n",
        "En este dataset, el grupo correspondiente a Gentoo mostrará una silueta ancha y un 'mapa de confianza' con puntos grandes ",
        "y sólidos (alta cohesión). Por el contrario, las zonas de contacto limítrofes entre Adelie y Chinstrap mostrarán puntos ",
        "más pequeños y tenues, indicando la complejidad natural de separar estadísticamente estas dos especies en base a su morfología."
      )
    })
    
    # --- EXTRAS ---
    output$ui_var_cat <- renderUI({
      df <- datos_base()
      req(df)
      nom_cats <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x) || (is.numeric(x) && length(unique(x)) <= 5))]
      selectInput(session$ns("var_cat"), "Variable Real (Color):", choices = c("Ninguna", nom_cats), selected = "Ninguna")
    })
    
    output$dl_data <- downloadHandler(
      filename = function() { paste("Resultados_Clustering_Jerarquico.csv", sep = "") },
      content = function(file) {
        req(datos_base(), res_jerarquico())
        df_export <- datos_base()
        df_export$Cluster_Asignado <- res_jerarquico()
        write.csv(df_export, file, row.names = TRUE)
      }
    )
  }) 
}
# -------------------------------
# Autoevaluación
# -------------------------------
Jerarquicos_Auto_UI <- function(id) {
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
Jerarquicos_Auto_Server <- function(id) {
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
